# Nu ska vi lära oss grejer #1

library(tidyverse)
library(DBI)
library(odbc)

# ladda in data
connection <- DBI::dbConnect(odbc::odbc(), 
                             Driver = "SQL Server", 
                             Server = "analys.ltdalarna.se", 
                             Database = "Analys", 
                             Trusted_Connection = "True", 
                             Encoding = "windows-1252")

sql_vårdepisod <- "
SELECT [VårdepisodID]
      ,[TidpunktStart]
      ,[TidpunktSlut]
      ,[Vårdtid]
      ,[VårdepisodAvslutad]
      ,[PatientID]
      ,[Covid19Påvisad]
      --,[Covid19Misstänkt]
      --,[Covid19Huvuddiagnos]
      ,[UtplaceringstidIVA]
      ,[TidInskrivningTillFörstaUtplaceringIVA]
      ,[TidSistaUtplaceringIVAtillUtskrivning]
      ,[UtplaceringIVA]
      ,[UtplaceringIVAPågående]
      ,[Inskrivningssätt]
      ,[Utskrivningssätt]
      ,[Akut]
      ,[InskrivenFrånAnnanKlinik]
      ,[UtskrivenTillAnnanKlinik]
      ,[UtplaceringPågående]
  FROM [Analys].[ssis].[Slutenvård_Vårdepisod]
"

# ladda in data i R och spara som en tibble
grunddata_vårdepisod <- dbGetQuery(connection, sql_vårdepisod) %>% 
  as_tibble()



# se typer och exempel på alla kolumner i dataset
str(grunddata_vårdepisod)

# se fördelning och statistik per variabel
summary(grunddata_vårdepisod)


# %>% är en pipe som skickar vidare data till nästa processsteg

# select väljer ut vissa variabler
grunddata_vårdepisod %>%
  select(PatientID, TidpunktStart, TidpunktSlut) 

# mutate skapar nya variabler eller ändrar på befintliga
grunddata_vårdepisod %>%
  select(PatientID, TidpunktStart, TidpunktSlut) %>%
  mutate(PatientID = as.character(PatientID))


# skapa nytt dataset 
nytt_dataset <- 
    grunddata_vårdepisod %>%
    select(PatientID, TidpunktStart, TidpunktSlut)
  

# filtrera ut rader, en patient med alla dess vårdepisoder
grunddata_vårdepisod %>%
  filter(PatientID == "9221774653000355855")

# skapa datum-variabler från tid och datum, as.Date konverterar från POSIX till Date
grunddata_vårdepisod %>%
  select(PatientID, TidpunktStart, TidpunktSlut) %>%
  filter(PatientID == "9221774653000355855") %>%
  mutate(DatumStart = as.Date(TidpunktStart),
         DatumSlut = as.Date(TidpunktSlut)) %>%
  select(PatientID, DatumStart, DatumSlut)


# beräkna vårddagar
grunddata_vårdepisod %>%
  mutate(DatumStart = as.Date(TidpunktStart),
         DatumSlut = as.Date(TidpunktSlut)) %>%
  filter(PatientID == "9221774653000355855") %>%
  select(PatientID, DatumStart, DatumSlut) %>%
  mutate(Vårddagar = as.integer(difftime(DatumSlut, DatumStart, units = "days")))



# beräkna vårdtillfällen som lett till återinskrivning
vårdepisod <- grunddata_vårdepisod %>%
  mutate(PatientID = as.character(PatientID))%>%
  mutate(DatumStart = as.Date(TidpunktStart),
         DatumSlut = as.Date(TidpunktSlut)) %>%
  filter(PatientID == "9221774653000355855" | PatientID == "-4534178196391246530") %>%
  select(PatientID, DatumStart, DatumSlut) %>%
  mutate(Vårddagar = as.integer(difftime(DatumSlut, DatumStart, units = "days"))) %>%
  group_by(PatientID) %>%
  mutate(DatumStart.nästa = lead(DatumStart, order_by = DatumStart)) %>%
  arrange(PatientID, DatumStart) %>%
  # beräkna dagar från utskrivning till nästa inskrivning
  mutate(TidTillNästa = as.integer(difftime(DatumStart.nästa, DatumSlut, units = "days"))) %>%
  # beräkna dagar från utskrivning till dagens datum
  mutate(DagarFrånDatumSlutTillIdag = as.integer(difftime(Sys.Date(),DatumSlut, units = "days"))) %>%
  # om nästa inskrivning är inom 30 dagar eller att mer än 30 dagar gått från senaste utskrivning och ingen ny inskrivning finns, sätt 1
  mutate(Återinskrivning30 = ifelse(
    is.na(TidTillNästa) & DagarFrånDatumSlutTillIdag > 30, 
    0, 
    ifelse(TidTillNästa <= 30, 1, 0)))

vårdepisod %>%
  mutate(År = year(DatumSlut))%>%
  group_by(År) %>%
  summarise(Antal_observationer = n(), 
            Vårddagar.medel = mean(Vårddagar), 
            Vårddagar.summa = sum(Vårddagar))




vårdepisod %>%
  filter(Vårddagar > 1000)

vårdepisod %>%
  mutate(År = year(DatumSlut))%>%
  group_by(År, Vårddagar) %>%
  summarise(Antal_observationer = n()) %>%
  filter(Vårddagar < 15) %>%
  ggplot()+
  geom_line(aes(x = Vårddagar, y=Antal_observationer, color = factor(År)))



# patient -> ålder, kön, acg, hemort
# organisation -> division, klinik, avdelning
# 

sql_vårdtillfälle = "

WITH vårdtillfälle AS
(
SELECT 
	   [PatientID]
      ,[Inskrivningsdatum]
      ,[Utskrivningsdatum]
      ,[Inskrivningssätt]
      ,[Utskrivningssätt]      
      ,[VårdtillfälleID]
      ,[InskrivenVårdenhet]
      ,[InskrivenHsa]      
      ,[Akut]
      ,[Vårddagar_inskrivna]
      ,[Vårddagar_utskrivna]
      ,[DRG_kod]
      ,[DRG_vikt]      
      ,[HsaName_lvl2]
      ,[HsaName_lvl3]
      ,[HsaName_lvl4]
  FROM [Analys].[ssis].[Slutenvård_Vårdtillfälle]
),
patient AS
(
SELECT 
       [PatientID_BK]
      ,[Fodelsedatum]
      ,[Fodelsear]
      ,[Kon]
      ,[Lanskod_ID]      
      ,[Kommunkod_ID]      
      ,[Lan_Text]               
  FROM [Analys].[ssis].[Dim_PatInfo]
)
SELECT *
FROM patient
INNER JOIN vårdtillfälle ON vårdtillfälle.PatientID = patient.PatientID_BK

"

grunddata_vårdtillfälle <- dbGetQuery(connection, sql_vårdtillfälle) %>% 
  as_tibble()

grunddata_vårdtillfälle %>%
  mutate(Vårdålder = as.integer(as.double(difftime(Inskrivningsdatum,Fodelsedatum, units = "days"))/365.25)) %>%
  select(PatientID_BK, Fodelsedatum, Inskrivningsdatum, Vårdålder, HsaName_lvl2, HsaName_lvl3, HsaName_lvl4)        

