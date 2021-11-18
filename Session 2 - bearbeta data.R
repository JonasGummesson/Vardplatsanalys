# Session 2 - 


# beräkna vårdtillfällen som lett till återinskrivning
vårdepisod <- grunddata_vårdepisod %>%
  mutate(PatientID = as.character(PatientID))%>%
  mutate(DatumStart = as.Date(TidpunktStart),
         DatumSlut = as.Date(TidpunktSlut)) %>%
  #filter(PatientID == "9221774653000355855" | PatientID == "-4534178196391246530") %>%
  select(PatientID, DatumStart, DatumSlut) %>%
  mutate(Vårddagar = as.integer(difftime(DatumSlut, DatumStart, units = "days"))) %>%
  group_by(PatientID) %>%
  mutate(DatumStart.nästa = lead(DatumStart, order_by = DatumStart)) %>%
  arrange(PatientID, DatumStart) %>%
  # beräkna dagar från utskrivning till nästa inskrivning
  mutate(TidTillNästa = as.integer(difftime(DatumStart.nästa, DatumSlut, units = "days")))


vårdepisod %>%
  group_by(TidTillNästa) %>%
  summarise(Antal = n()) %>%
  filter(TidTillNästa >= 0) %>%
  arrange(TidTillNästa) %>%
ggplot()+
  geom_bar(aes(x = TidTillNästa, y = Antal), stat = "identity")


ggplot(vårdepisod)+
  geom_bar(aes(x = TidTillNästa), stat = "count")
