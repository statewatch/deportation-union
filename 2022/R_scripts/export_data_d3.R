library(needs)

needs(tidyverse,
      ggplot2,
      readxl,
      glue,
      wbstats,
      lubridate)

## export for web
#################

files <- list.files("../clean_data")[grepl("csv", list.files("../clean_data")) & !grepl("_new", list.files("../clean_data"))]

names <- str_replace_all(files, "\\.csv", "")

for(i in 1:length(files)){
  assign(names[i], read_csv(glue("../clean_data/{files[i]}")))
}

# Operations per year 
# -------------------

OPTYPE_YEAR <- by_ms_2006_18%>%
  group_by(ID, DATE, OPTYPE)%>%
  summarize(N_RETURNEES = sum(N_RETURNEES, na.rm=T))%>%
  ungroup()%>%
  select(DATE, N_RETURNEES, OPTYPE)%>%
  mutate(DATE = as_date(DATE))%>%
  bind_rows(by_dest_2019_21%>%
              group_by(ROID, DATE, OPTYPE)%>%
              summarize(N_RETURNEES = sum(N_RETURNEES, na.rm = T))%>%
              select(-ROID)%>%
              mutate(DATE = as_date(DATE, format = "%d/%m/%Y")))%>%
  mutate(YEAR = year(DATE))%>%
  group_by(YEAR, OPTYPE)%>%
  summarize(`Number of deported people` = sum(N_RETURNEES, na.rm = T), 
            `Number of operations` = n())%>%
  mutate(OPTYPE = if_else(is.na(OPTYPE), "unknown", OPTYPE))%>%
  gather(-YEAR, -OPTYPE, key = KEY, value = value)%>%
  spread(key = OPTYPE, value = value)%>%
  mutate(across(c(CRO:unknown), ~replace_na(., 0)))

write_csv(OPTYPE_YEAR, "../clean_data/OPTYPE_YEAR_new.csv")


# Deported people per year, MS and DEST
# -------------------------------------

OPERATIONS_BY_DEST_MS <- by_dest_2006_18%>%
  select(ROWID, N_RETURNEES, DEST)%>%
  left_join(by_ms_2006_18%>%
              select(ROWID, MSNAME, DATE))%>%
  select(-ROWID)%>%
  mutate(DATE = as_date(DATE))%>%
  bind_rows(by_dest_2019_21 %>%
              select(N_RETURNEES, DEST, MSNAME, DATE)%>%
              mutate(DATE = as_date(DATE, format = "%d/%m/%Y")))

temp_order <- OPERATIONS_BY_DEST_MS %>%
  mutate(YEAR = year(DATE))%>%
  group_by(MSNAME, YEAR)%>%
  summarize(N_RETURNEES = sum(N_RETURNEES, na.rm = TRUE))%>%
  group_by(MSNAME)%>%
  summarize(total_over_years = sum(N_RETURNEES, na.rm = T))%>%
  filter(total_over_years > 0)%>%
  arrange(desc(total_over_years))

N_RETURNEES_YEARS_MSNAME <- OPERATIONS_BY_DEST_MS %>%
  mutate(YEAR = year(DATE))%>%
  group_by(MSNAME, YEAR)%>%
  summarize(DEST = "all destinations", N_RETURNEES = sum(N_RETURNEES, na.rm = TRUE)) %>%
  bind_rows(OPERATIONS_BY_DEST_MS %>%
              mutate(YEAR = year(DATE))%>%
              group_by(MSNAME, DEST, YEAR)%>%
              summarize(N_RETURNEES = sum(N_RETURNEES, na.rm = TRUE)))%>%
  # create each YEAR for each combo
  spread(key = YEAR, value = N_RETURNEES)%>%
  mutate(across(c(`2006`:`2021`), ~replace_na(.,0)))%>%
  gather(-MSNAME, -DEST, key = YEAR, value = N_RETURNEES)%>%
  filter(N_RETURNEES > 0)%>%
  spread(key = MSNAME, value = N_RETURNEES)%>%
  dplyr::mutate(across(c(`Austria`:`United Kingdom`), ~replace_na(.,0)))%>%
  select(append(append("YEAR", "DEST"), temp_order$MSNAME))%>%
  mutate(temp_order = case_when(
    DEST == "all destinations" ~ 1,
    TRUE ~ 0
  ))%>%
  arrange(desc(temp_order))%>%
  select(-temp_order)

write_csv(N_RETURNEES_YEARS_MSNAME, "../clean_data/N_RETURNEES_YEARS_MSNAME_new.csv")

# deportations from - to, all years total
# ---------------------------------------

N_RETURNEES_MSNAME_DEST <- N_RETURNEES_YEARS_MSNAME%>%
  filter(DEST != "all destinations")%>%
  gather(-YEAR, -DEST, key = MSNAME, value = N_RETURNEES)%>%
  # group all years together
  group_by(DEST, MSNAME)%>%
  summarize(N_RETURNEES = sum(N_RETURNEES, na.rm=T))%>%
  filter(N_RETURNEES != 0)

# TOP_10_MSSTATES <- N_RETURNEES_MSNAME_DEST%>%
#   group_by(MSNAME)%>%
#   filter(sum(N_RETURNEES) > 780)

TOP_15_MSSTATE <- N_RETURNEES_MSNAME_DEST %>%
  group_by(MSNAME)%>%
  summarize(N = sum(N_RETURNEES))%>%
  arrange(desc(N))%>%
  head(15)

TOP_15_DEST <- N_RETURNEES_MSNAME_DEST %>%
  group_by(DEST)%>%
  summarize(N = sum(N_RETURNEES))%>%
  arrange(desc(N))%>%
  head(15)

TOP_15_MSSTATES_DESTS <- N_RETURNEES_MSNAME_DEST %>%
  # they need to be sorted in the order that the nodes shall appear
  group_by(MSNAME)%>%
  mutate(N_MS = sum(N_RETURNEES))%>%
  group_by(DEST)%>%
  mutate(N_DEST = sum(N_RETURNEES))%>%
  filter(MSNAME %in% TOP_15_MSSTATE$MSNAME & DEST %in% TOP_15_DEST$DEST)%>%
  arrange(desc(N_DEST))%>%
  arrange(desc(N_MS))%>%
  select(-N_DEST, -N_MS)


# ROUTES_MIN_15 <- N_RETURNEES_MSNAME_DEST%>%
#   filter(N_RETURNEES >= 15)

write_csv(N_RETURNEES_MSNAME_DEST, "../clean_data/N_RETURNEES_MSNAME_DEST_new.csv")

write_csv(TOP_15_MSSTATES_DESTS, "../clean_data/N_RETURNEES_MSNAME_DEST_ROUTES_MIN_15_new.csv")


# FX_CONTRIB per YEAR and MS (total)
# ----------------------------------

OPERATIONS_BY_MS <- by_ms_2006_18 %>%
  filter(!is.na(MSNAME))%>%
  select(MSNAME, 
         FX_CONTRIB, 
         DATE)%>%
  mutate(DATE = as_date(DATE))%>%
  bind_rows(by_ms_2019_21 %>%
              select(MSNAME, FX_CONTRIB, DATE)%>%
              mutate(DATE = as_date(DATE, format="%d/%m/%Y")))%>%
  filter(!is.na(FX_CONTRIB) & FX_CONTRIB > 0)
  

temp_order <- OPERATIONS_BY_MS %>%
  filter(!is.na(MSNAME))%>%
  mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
  group_by(MSNAME)%>%
  summarize(total_over_years = sum(FX_CONTRIB, na.rm = T))%>%
  filter(total_over_years > 0)%>%
  arrange(desc(total_over_years))

FX_CONTRIB_YEARS_MSNAME_TOTAL <- OPERATIONS_BY_MS %>%
  filter(!is.na(MSNAME))%>%
  mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
  group_by(MSNAME, YEAR) %>%
  summarize(FX_CONTRIB = sum(FX_CONTRIB, na.rm = T))%>%
  # spread(key = YEAR, value = FX_CONTRIB)%>%
  # mutate(across(c(`2009`:`2021`), ~replace_na(.,0)))%>%
  # gather(-MSNAME, key = YEAR, value = FX_CONTRIB)%>%
  # filter(FX_CONTRIB > 0)%>%
  spread(key = MSNAME, value = FX_CONTRIB)%>%
  dplyr::mutate(across(c(`Albania`:`United Kingdom`), ~replace_na(.,0)))%>%
  select(append("YEAR", temp_order$MSNAME))%>%
  mutate(MSNAME = "all EU member states")

FX_CONTRIB_YEARS_MSNAME_STATES <- FX_CONTRIB_YEARS_MSNAME_TOTAL%>%
  select(-MSNAME)%>%
  gather(-YEAR, key = MSNAME, value = value)%>%
  mutate(key = MSNAME)%>%
  spread(key = key, value = value)%>%
  dplyr::mutate(across(c(`Albania`:`United Kingdom`), ~replace_na(.,0)))

FX_CONTRIB_YEARS_MSNAME <- bind_rows(FX_CONTRIB_YEARS_MSNAME_TOTAL,FX_CONTRIB_YEARS_MSNAME_STATES)%>%
  relocate(MSNAME, .before = "Germany")

write_csv(FX_CONTRIB_YEARS_MSNAME, "../clean_data/FX_CONTRIB_YEARS_MSNAME_new.csv")


# FX_CONTRIB BY PERSON AND DEST
# -----------------------------

## needs to be unique member states that are participating, 
## otherwise participating countries who are only deporting
## one or a few people from their country but recieve
## a lot of money distort the image.

## operations with one destination per MS
OPERATIONS_UNIQUE_DEST <- by_dest_2006_18 %>%
  left_join(by_ms_2006_18 %>%
              select(ROWID, ID))%>%
  group_by(ID)%>%
  mutate(n_dest = n())%>%
  filter(n_dest == 1)%>%
  select(ID, N_RETURNEES, DEST)%>%
  left_join(by_ms_2006_18 %>%
              select(ID, FX_CONTRIB))%>%
  bind_rows(
    by_dest_2019_21 %>%
      group_by(ROID)%>%
      mutate(n_dest = n())%>%
      filter(n_dest == 1)%>%
      select(ROID, DEST, N_RETURNEES)%>%
      left_join(by_ms_2019_21%>%
                  select(ROID, FX_CONTRIB))
  )%>%
  ## with unique combinations of OPERATION and DEST, there is not much data (starting 2018 only)
  # ## it looks like contribution amounts were only included starting 2009.
  # ## before 2009: exclude from averages
  # ## after 2009: if NA, assume there was 0€ frontex contribution
  # filter(!(as.numeric(substr(ROWID, 1, 4)) < 2009 & !is.na(ROWID)) &
  #          N_RETURNEES > 0) %>%
  mutate(FX_CONTRIB = replace_na(FX_CONTRIB, 0))%>%
  ## filter out entries without destination (all of them are entries with third countries as MS)
  filter(!is.na(DEST))

CONTRIB_PP_BY_DEST <- OPERATIONS_UNIQUE_DEST %>%
  group_by(DEST)%>%
  summarise(across(where(is.numeric), ~ sum(., na.rm=T)))%>%
  # add average across countries
  bind_rows(OPERATIONS_UNIQUE_DEST%>%
              group_by(DEST = "average across all destinations")%>%
              summarise(across(where(is.numeric), ~ sum(., na.rm=T))))%>%
  mutate(FX_CONTRIB_PP = FX_CONTRIB / N_RETURNEES)%>%
  arrange(desc(FX_CONTRIB_PP))%>%
  # add ISO-codes for X-axis labels from world bank data
  left_join(wb_cachelist$countries %>% 
              select(iso = iso2c, country_name = country)%>%
              mutate(country_name = str_replace_all(country_name, 
                                                    c("Congo, Dem\\. Rep\\." = "Congo DR",
                                                      "Gambia, The" = "Gambia",
                                                      "Egypt, Arab Rep\\." = "Egypt",
                                                      "Russian Federation" = "Russia"
                                                    ))),
            by = c("DEST" = "country_name"))%>%
  mutate(iso = case_when(DEST == "average across all destinations" ~ "", 
                         TRUE ~ iso))%>%
  # only include those with at least 10 returnees, otherwise maybe distorted
  filter(N_RETURNEES >= 10)%>%
  head(26)

write_csv(CONTRIB_PP_BY_DEST, "../clean_data/CONTRIB_PP_BY_DEST_new.csv")
