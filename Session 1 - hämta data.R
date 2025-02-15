# Nu ska vi l�ra oss grejer #1

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

sql_v�rdepisod <- "
SELECT [V�rdepisodID]
      ,[TidpunktStart]
      ,[TidpunktSlut]
      ,[V�rdtid]
      ,[V�rdepisodAvslutad]
      ,[PatientID]
      ,[Covid19P�visad]
      --,[Covid19Misst�nkt]
      --,[Covid19Huvuddiagnos]
      ,[UtplaceringstidIVA]
      ,[TidInskrivningTillF�rstaUtplaceringIVA]
      ,[TidSistaUtplaceringIVAtillUtskrivning]
      ,[UtplaceringIVA]
      ,[UtplaceringIVAP�g�ende]
      ,[Inskrivningss�tt]
      ,[Utskrivningss�tt]
      ,[Akut]
      ,[InskrivenFr�nAnnanKlinik]
      ,[UtskrivenTillAnnanKlinik]
      ,[UtplaceringP�g�ende]
  FROM [Analys].[ssis].[Slutenv�rd_V�rdepisod]
"

# ladda in data i R och spara som en tibble
grunddata_v�rdepisod <- dbGetQuery(connection, sql_v�rdepisod) %>% 
  as_tibble()



# se typer och exempel p� alla kolumner i dataset
str(grunddata_v�rdepisod)

# se f�rdelning och statistik per variabel
summary(grunddata_v�rdepisod)


# %>% �r en pipe som skickar vidare data till n�sta processsteg

# select v�ljer ut vissa variabler
grunddata_v�rdepisod %>%
  select(PatientID, TidpunktStart, TidpunktSlut) 

# mutate skapar nya variabler eller �ndrar p� befintliga
grunddata_v�rdepisod %>%
  select(PatientID, TidpunktStart, TidpunktSlut) %>%
  mutate(PatientID = as.character(PatientID))


# skapa nytt dataset 
nytt_dataset <- 
    grunddata_v�rdepisod %>%
    select(PatientID, TidpunktStart, TidpunktSlut)
  

# filtrera ut rader, en patient med alla dess v�rdepisoder
grunddata_v�rdepisod %>%
  filter(PatientID == "9221774653000355855")

# skapa datum-variabler fr�n tid och datum, as.Date konverterar fr�n POSIX till Date
grunddata_v�rdepisod %>%
  select(PatientID, TidpunktStart, TidpunktSlut) %>%
  filter(PatientID == "9221774653000355855") %>%
  mutate(DatumStart = as.Date(TidpunktStart),
         DatumSlut = as.Date(TidpunktSlut)) %>%
  select(PatientID, DatumStart, DatumSlut)


# ber�kna v�rddagar
grunddata_v�rdepisod %>%
  mutate(DatumStart = as.Date(TidpunktStart),
         DatumSlut = as.Date(TidpunktSlut)) %>%
  filter(PatientID == "9221774653000355855") %>%
  select(PatientID, DatumStart, DatumSlut) %>%
  mutate(V�rddagar = as.integer(difftime(DatumSlut, DatumStart, units = "days")))



# ber�kna v�rdtillf�llen som lett till �terinskrivning
v�rdepisod <- grunddata_v�rdepisod %>%
  mutate(PatientID = as.character(PatientID))%>%
  mutate(DatumStart = as.Date(TidpunktStart),
         DatumSlut = as.Date(TidpunktSlut)) %>%
  filter(PatientID == "9221774653000355855" | PatientID == "-4534178196391246530") %>%
  select(PatientID, DatumStart, DatumSlut) %>%
  mutate(V�rddagar = as.integer(difftime(DatumSlut, DatumStart, units = "days"))) %>%
  group_by(PatientID) %>%
  mutate(DatumStart.n�sta = lead(DatumStart, order_by = DatumStart)) %>%
  arrange(PatientID, DatumStart) %>%
  # ber�kna dagar fr�n utskrivning till n�sta inskrivning
  mutate(TidTillN�sta = as.integer(difftime(DatumStart.n�sta, DatumSlut, units = "days"))) %>%
  # ber�kna dagar fr�n utskrivning till dagens datum
  mutate(DagarFr�nDatumSlutTillIdag = as.integer(difftime(Sys.Date(),DatumSlut, units = "days"))) %>%
  # om n�sta inskrivning �r inom 30 dagar eller att mer �n 30 dagar g�tt fr�n senaste utskrivning och ingen ny inskrivning finns, s�tt 1
  mutate(�terinskrivning30 = ifelse(
    is.na(TidTillN�sta) & DagarFr�nDatumSlutTillIdag > 30, 
    0, 
    ifelse(TidTillN�sta <= 30, 1, 0)))

v�rdepisod %>%
  mutate(�r = year(DatumSlut))%>%
  group_by(�r) %>%
  summarise(Antal_observationer = n(), 
            V�rddagar.medel = mean(V�rddagar), 
            V�rddagar.summa = sum(V�rddagar))




v�rdepisod %>%
  filter(V�rddagar > 1000)

v�rdepisod %>%
  mutate(�r = year(DatumSlut))%>%
  group_by(�r, V�rddagar) %>%
  summarise(Antal_observationer = n()) %>%
  filter(V�rddagar < 15) %>%
  ggplot()+
  geom_line(aes(x = V�rddagar, y=Antal_observationer, color = factor(�r)))



# patient -> �lder, k�n, acg, hemort
# organisation -> division, klinik, avdelning
# 

sql_v�rdtillf�lle = "

WITH v�rdtillf�lle AS
(
SELECT 
	   [PatientID]
      ,[Inskrivningsdatum]
      ,[Utskrivningsdatum]
      ,[Inskrivningss�tt]
      ,[Utskrivningss�tt]      
      ,[V�rdtillf�lleID]
      ,[InskrivenV�rdenhet]
      ,[InskrivenHsa]      
      ,[Akut]
      ,[V�rddagar_inskrivna]
      ,[V�rddagar_utskrivna]
      ,[DRG_kod]
      ,[DRG_vikt]      
      ,[HsaName_lvl2]
      ,[HsaName_lvl3]
      ,[HsaName_lvl4]
  FROM [Analys].[ssis].[Slutenv�rd_V�rdtillf�lle]
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
INNER JOIN v�rdtillf�lle ON v�rdtillf�lle.PatientID = patient.PatientID_BK

"

grunddata_v�rdtillf�lle <- dbGetQuery(connection, sql_v�rdtillf�lle) %>% 
  as_tibble()

grunddata_v�rdtillf�lle %>%
  mutate(V�rd�lder = as.integer(as.double(difftime(Inskrivningsdatum,Fodelsedatum, units = "days"))/365.25)) %>%
  select(PatientID_BK, Fodelsedatum, Inskrivningsdatum, V�rd�lder, HsaName_lvl2, HsaName_lvl3, HsaName_lvl4)        

