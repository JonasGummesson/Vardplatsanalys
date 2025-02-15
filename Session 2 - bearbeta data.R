# Session 2 - 


# ber�kna v�rdtillf�llen som lett till �terinskrivning
v�rdepisod <- grunddata_v�rdepisod %>%
  mutate(PatientID = as.character(PatientID))%>%
  mutate(DatumStart = as.Date(TidpunktStart),
         DatumSlut = as.Date(TidpunktSlut)) %>%
  #filter(PatientID == "9221774653000355855" | PatientID == "-4534178196391246530") %>%
  select(PatientID, DatumStart, DatumSlut) %>%
  mutate(V�rddagar = as.integer(difftime(DatumSlut, DatumStart, units = "days"))) %>%
  group_by(PatientID) %>%
  mutate(DatumStart.n�sta = lead(DatumStart, order_by = DatumStart)) %>%
  arrange(PatientID, DatumStart) %>%
  # ber�kna dagar fr�n utskrivning till n�sta inskrivning
  mutate(TidTillN�sta = as.integer(difftime(DatumStart.n�sta, DatumSlut, units = "days")))


v�rdepisod %>%
  group_by(TidTillN�sta) %>%
  summarise(Antal = n()) %>%
  filter(TidTillN�sta >= 0) %>%
  arrange(TidTillN�sta) %>%
ggplot()+
  geom_bar(aes(x = TidTillN�sta, y = Antal), stat = "identity")


ggplot(v�rdepisod)+
  geom_bar(aes(x = TidTillN�sta), stat = "count")
