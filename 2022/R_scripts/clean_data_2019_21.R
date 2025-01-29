library(needs)

needs(tidyverse,
      ggplot2,
      readxl,
      glue,
      wbstats)

country_codes <- wb_cachelist$countries%>%
  select(ISO = iso3c,
         NAME = country)%>%
  mutate(NAME = str_replace_all(NAME, 
  c("Slovak Republic" = "Slovakia")))

costs <- read_csv("../raw_data/frontex_docs_converted/tabula-2019_Costs.csv")%>%
  bind_rows(read_csv("../raw_data/frontex_docs_converted/tabula-2020_Costs.csv"))%>%
  bind_rows(read_csv("../raw_data/frontex_docs_converted/tabula-2021_Costs.csv"))%>%
  rename(ROID = "RO-ID",
         DATE = Date,
         MSISO = "MS/SAC",
         FX_CONTRIB = "TOTAL PAID")%>%
  mutate(FX_CONTRIB = as.numeric(str_remove_all(FX_CONTRIB, "[, €]")))%>%
  filter(ROID != "RO-ID")

fx_staff <- read_csv("../raw_data/frontex_docs_converted/tabula-2019_Frontex rep..csv")%>%
  bind_rows(read_csv("../raw_data/frontex_docs_converted/tabula-2020_Frontex rep..csv"))%>%
  bind_rows(read_csv("../raw_data/frontex_docs_converted/tabula-2021_Frontex rep..csv"))%>%
  rename(ROID = "RO-ID",
         DATE = "RO Date",
         N_FX_STAFF = "Frontex Representative on Board")%>%
  filter(ROID != "RO-ID")%>%
  mutate(N_FX_STAFF = as.numeric(N_FX_STAFF))

staff <- read_csv("../raw_data/frontex_docs_converted/tabula-2019_Staff.csv")%>%
  bind_rows(read_csv("../raw_data/frontex_docs_converted/tabula-2020_Staff.csv"))%>%
  bind_rows(read_csv("../raw_data/frontex_docs_converted/tabula-2021_Staff.csv"))%>%
  select(-1)%>%
  rename(ROID = "RO-ID",
         DATE = "RO Date",
         MSNAME = MSSAC,
         N_ESC_LEAD = "Escort\rLeaders",
         N_ESC = Escorts,
         N_MEDS = "Medical Personnel", 
         N_OBS_MS = "Observers\r(MS)",
         N_OBS_TC = "Observers\r(TC)",
         N_MONITORS_NAT = "Monitors\rnational",
         N_MONITORS_POOL_MS = "Monitors from\rPool (MS)",
         N_MONITORS_FX = "Monitors from\rPool (FRO)",
         FRESO_AP_CAT_1 = "FRESO airport CAT\r1",
         FRESO_AP_CAT_2 = "FRESO airport CAT\r2",
         FRESO_HQ_CAT_1 = "FRESO HQ CAT\r1",
         FRESO_HQ_CAT_2 = "FRESO HQ\rCAT 2",
         FRESO_CAT_3 = "FRESO CAT\r3"
         )%>%
  filter(ROID != "RO-ID")%>%
  mutate(across(c(N_ESC_LEAD:N_MONITORS_FX), ~as.numeric(.)))%>%
  mutate(N_MONITORS = rowSums(.[14:16], na.rm = T),
         N_OBS = rowSums(.[12:13], na.rm = T))%>%
  mutate(N_ESC_OBS_MED = rowSums(.[grep("N_OBS$|N_ESC|N_MEDS", names(.))], na.rm = T))

dests <- read_csv("../raw_data/frontex_docs_converted/tabula-2019_TCNs, date, type.csv")%>%
  bind_rows(read_csv("../raw_data/frontex_docs_converted/tabula-2020_TCNs, date, type.csv",
                     col_types = "ccccccd"))%>%
  bind_rows(read_csv("../raw_data/frontex_docs_converted/tabula-2021_TCNs, date, type.csv"))%>%
  select(-3, -6)%>%
  rename(ROID = "RO-ID",
         DATE_OPTYPE = "Date of Departure type of operation",
         MSNAME = MSSAC,
         DEST = "Destination(s)",
         N_RETURNEES = "...7")%>%
  filter(!grepl("Total", ROID))%>%
  filter(MSNAME != "MSSAC")%>%
  filter(!is.na(N_RETURNEES))%>%
  mutate(DATE_OPTYPE = if_else(
    is.na(DATE_OPTYPE) & !is.na(ROID),
    substr(ROID, 10, 23),
    DATE_OPTYPE
  ))%>%
  mutate(ROID = if_else(
    nchar(ROID) > 8, 
    substr(ROID, 1, 8),
    ROID
  ))%>%
  mutate(DATE = substr(DATE_OPTYPE, 1, 10),
         OPTYPE = substr(DATE_OPTYPE, 12,14))%>%
  select(-DATE_OPTYPE)%>%
  fill(c(ROID, DATE, OPTYPE))%>%
  mutate(
    DEST = case_when(
      grepl("erzegovina", DEST) ~ "Bosnia and Herzegovina",
      grepl("Gambia", DEST) ~ "Gambia",
      grepl("Dominican", DEST) ~ "Dominican Republic",
      grepl("Kosovo", DEST) ~ "Kosovo",
      TRUE ~ DEST
    ))

by_op_2019_21 <- fx_staff
write_csv(by_op_2019_21, "../clean_data/by_op_2019_21.csv")

by_ms_2019_21 <- full_join(
  costs %>%
    left_join(country_codes %>% rename(MSNAME = NAME), by = c("MSISO" = "ISO")),
  staff %>%
    left_join(country_codes, by = c("MSNAME" = "NAME")),
  by = c("MSISO" = "ISO", "MSNAME", "ROID", "DATE")
)
write_csv(by_ms_2019_21, "../clean_data/by_ms_2019_21.csv")

by_dest_2019_21 <- dests
write_csv(by_dest_2019_21, "../clean_data/by_dest_2019_21.csv")
