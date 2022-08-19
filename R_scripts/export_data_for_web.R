library(needs)

needs(tidyverse,
      ggplot2,
      readxl,
      glue,
      wbstats)


# load data
###########


OPERATIONS <- read_csv("../clean_data/OPERATIONS.csv")

OPERATIONS_BY_MS <- read_csv("../clean_data/OPERATIONS_BY_MS.csv",
                             col_types = cols(
                               DATE = col_date(format = ""),
                               ROID = col_character(),
                               ID = col_character(),
                               MSNAME = col_character(),
                               MSISO = col_character(),
                               N_RETURNEES = col_double(),
                               N_ESC_OBS_MED = col_double(),
                               N_ESC = col_double(),
                               N_ESC_LEAD = col_double(),
                               N_MEDS = col_double(),
                               N_OBS = col_double(),
                               N_MONITORS = col_double(),
                               FX_CONTRIB = col_double(),
                               OPTYPE = col_character(),
                               NOTES_COMMENTS = col_character(),
                               N_ESC_POOL = col_double(),
                               N_MONITORS_POOL = col_double()
                             ))

OPERATIONS_BY_DEST_MS <- read_csv("../clean_data/OPERATIONS_BY_DEST_MS.csv")

# export data for d3.js visualisations
######################################

# Operations per year 
####################

OPTYPE_YEAR <- OPERATIONS_BY_MS %>%
  mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
  group_by(YEAR, OPTYPE)%>%
  summarize(`Number of deported people` = sum(N_RETURNEES, na.rm = T), 
            `Number of operations` = n())%>%
  filter(!is.na(OPTYPE))%>%
  gather(-YEAR, -OPTYPE, key = KEY, value = value)%>%
  spread(key = OPTYPE, value = value)%>%
  mutate(across(c(CRO:NRO), ~replace_na(., 0)))
  
write_csv(OPTYPE_YEAR, "../clean_data/OPTYPE_YEAR.csv")

# Deported people per year, MS and DEST
#######################################

temp_order <- OPERATIONS_BY_DEST_MS %>%
  filter(!is.na(MSNAME) & !is.na(DEST))%>%
  select(MSNAME, 
         N_RETURNEES, 
         DEST, 
         DATE)%>%
  mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
  group_by(MSNAME, YEAR)%>%
  summarize(N_RETURNEES = sum(N_RETURNEES, na.rm = TRUE))%>%
  group_by(MSNAME)%>%
  summarize(total_over_years = sum(N_RETURNEES, na.rm = T))%>%
  filter(total_over_years > 0)%>%
  arrange(desc(total_over_years))


N_RETURNEES_YEARS_MSNAME <- OPERATIONS_BY_DEST_MS %>%
  filter(!is.na(MSNAME) & !is.na(DATE))%>%
  select(MSNAME, N_RETURNEES, DEST, DATE)%>%
  mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
  group_by(MSNAME, YEAR)%>%
  summarize(DEST = "all destinations", N_RETURNEES = sum(N_RETURNEES, na.rm = TRUE)) %>%
  bind_rows(OPERATIONS_BY_DEST_MS %>%
              filter(!is.na(MSNAME) & !is.na(DATE)& !is.na(DEST))%>%
              select(MSNAME, N_RETURNEES, DEST, DATE)%>%
              mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
              group_by(MSNAME, DEST, YEAR)%>%
              summarize(N_RETURNEES = sum(N_RETURNEES, na.rm = TRUE)))%>%
  # create each YEAR for each combo
  spread(key = YEAR, value = N_RETURNEES)%>%
  mutate(across(c(`2006`:`2020`), ~replace_na(.,0)))%>%
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


write_csv(N_RETURNEES_YEARS_MSNAME, "../clean_data/N_RETURNEES_YEARS_MSNAME.csv")

# deportations from - to, all years total
#########################################

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

write_csv(N_RETURNEES_MSNAME_DEST, "../clean_data/N_RETURNEES_MSNAME_DEST.csv")

write_csv(TOP_15_MSSTATES_DESTS, "../clean_data/N_RETURNEES_MSNAME_DEST_ROUTES_MIN_15.csv")

# FX_CONTRIB per YEAR and MS (total)
####################################

temp_order <- OPERATIONS_BY_MS %>%
  filter(!is.na(MSNAME))%>%
  select(MSNAME, 
         N_RETURNEES, 
         FX_CONTRIB, 
         DATE)%>%
  mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
  group_by(MSNAME)%>%
  summarize(total_over_years = sum(FX_CONTRIB, na.rm = T))%>%
  filter(total_over_years > 0)%>%
  arrange(desc(total_over_years))

FX_CONTRIB_YEARS_MSNAME_TOTAL <- OPERATIONS_BY_MS %>%
  select(MSNAME, 
         N_RETURNEES, 
         FX_CONTRIB, 
         DATE)%>%
  filter(!is.na(MSNAME))%>%
  mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
  group_by(MSNAME, YEAR) %>%
  summarize(FX_CONTRIB = sum(FX_CONTRIB, na.rm = T))%>%
  spread(key = YEAR, value = FX_CONTRIB)%>%
  mutate(across(c(`2006`:`2020`), ~replace_na(.,0)))%>%
  gather(-MSNAME, key = YEAR, value = FX_CONTRIB)%>%
  filter(FX_CONTRIB > 0)%>%
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

write_csv(FX_CONTRIB_YEARS_MSNAME, "../clean_data/FX_CONTRIB_YEARS_MSNAME.csv")
  
  
# # FX_CONTRIB per YEAR and MS (per person)
# #########################################
# 
# FX_CONTRIB_PP_YEARS_MSNAME <- OPERATIONS_BY_MS %>%
#   select(MSNAME, 
#          N_RETURNEES, 
#          FX_CONTRIB, 
#          DATE)%>%
#   filter(!is.na(MSNAME))%>%
#   mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
#   group_by(YEAR) %>%
#   summarize(N_RETURNEES = sum(N_RETURNEES, na.rm = TRUE),
#             FX_CONTRIB = sum(FX_CONTRIB, na.rm = T))%>%
#   # has to be returnees to make per person calc
#   filter(N_RETURNEES > 0)%>%
#   mutate(FX_CONTRIB_PP = FX_CONTRIB / N_RETURNEES)%>%
#   select(-N_RETURNEES, -FX_CONTRIB)%>%
#   mutate(MSNAME = "Average across member states")%>%
#   bind_rows(OPERATIONS_BY_MS %>%
#               select(MSNAME, 
#                      N_RETURNEES, 
#                      FX_CONTRIB, 
#                      DATE)%>%
#               filter(!is.na(MSNAME))%>%
#               mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
#               group_by(MSNAME, YEAR) %>%
#               summarize(N_RETURNEES = sum(N_RETURNEES, na.rm = TRUE),
#                         FX_CONTRIB = sum(FX_CONTRIB, na.rm = T))%>%
#               # has to be returnees to make per person calc
#               filter(N_RETURNEES > 0)%>%
#               mutate(FX_CONTRIB_PP = FX_CONTRIB / N_RETURNEES)%>%
#               select(-N_RETURNEES, -FX_CONTRIB))%>%
#   spread(key = YEAR, value = FX_CONTRIB_PP)%>%
#   mutate(across(c(`2006`:`2020`), ~replace_na(.,0)))%>%
#   gather(-MSNAME, key = YEAR, value = FX_CONTRIB_PP)%>%
#   filter(FX_CONTRIB_PP > 0)%>%
#   spread(key = MSNAME, value = FX_CONTRIB_PP)%>%
#   dplyr::mutate(across(c(`Austria`:`United Kingdom`), ~replace_na(.,0)))%>%
#   # move average col to end so it gets drawn last (on top of all others)%>%
#   relocate(`Average across member states`, .after = `United Kingdom`)%>%
#   # unneccessary but otherwise frontend logic gets confused
#   mutate(KEY = "frontex_contribution_per_person")
# 
# write_csv(FX_CONTRIB_PP_YEARS_MSNAME, "../clean_data/FX_CONTRIB_PP_YEARS_MSNAME.csv")


# FX_CONTRIB BY PERSON AND DEST
################################


# get operations with unique destinations
OPERATIONS_UNIQUE_DEST <- OPERATIONS_BY_DEST_MS %>%
  select(ID, DEST)%>%
  unique()%>%
  group_by(ID)%>%
  mutate(n = n())%>%
  filter(n == 1)%>%
  select(-n)%>%
  # join operation details
  left_join(OPERATIONS, by = "ID")

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

write_csv(CONTRIB_PP_BY_DEST, "../clean_data/CONTRIB_PP_BY_DEST.csv")

