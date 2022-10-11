library(needs)

needs(tidyverse,
      ggplot2,
      readxl,
      glue,
      wbstats,
      lubridate)

## export for web
#################

files <- list.files("../clean_data")[grepl("csv", list.files("../clean_data"))]

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
  select(-N_ESC, -N_ESC_LEAD)%>%
  left_join(by_ms_2006_18%>%
              select(ROWID, MSNAME, DATE))%>%
  select(-ROWID)%>%
  bind_rows(dests_2019_20%>%
              left_join(lookup_date_id_type)%>%
              select(-ROID, -MSISO, -OPTYPE))%>%
  bind_rows(returnees_2020_2021_split%>%
              filter(substr(DATE, 1, 4) != "2020")%>%
              select(-ROID, -MSTYPE, -OPTYPE))

temp_order <- OPERATIONS_BY_DEST_MS %>%
  mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
  group_by(MSNAME, YEAR)%>%
  summarize(N_RETURNEES = sum(N_RETURNEES, na.rm = TRUE))%>%
  group_by(MSNAME)%>%
  summarize(total_over_years = sum(N_RETURNEES, na.rm = T))%>%
  filter(total_over_years > 0)%>%
  arrange(desc(total_over_years))

N_RETURNEES_YEARS_MSNAME <- OPERATIONS_BY_DEST_MS %>%
  mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
  group_by(MSNAME, YEAR)%>%
  summarize(DEST = "all destinations", N_RETURNEES = sum(N_RETURNEES, na.rm = TRUE)) %>%
  bind_rows(OPERATIONS_BY_DEST_MS %>%
              mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
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
  bind_rows(costs_2019 %>%
              left_join(lookup_date_id_type)%>%
              select(-OPTYPE, -ROID, -MSISO))%>%
  ## NOTE: contributions are equally divided across
  ## member states (no better data available yet)
  bind_rows(costs_2020_2021%>%
              ## filter out VRD operations
              filter(!grepl("VRD", ROID))%>%
              left_join(esc_monitors_2020_21 %>%
                          select(ROID, DATE, MSNAME))%>%
              group_by(ROID)%>%
              mutate(n_ms = n())%>%
              ungroup()%>%
              mutate(FX_CONTRIB = FX_CONTRIB / n_ms)%>%
              select(-ROID, -n_ms))%>%
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

# OPERATIONS_BY_DEST <- by_dest_2006_18%>%
#   select(ROWID, DEST, N_RETURNEES)%>%
#   left_join(by_ms_2006_18%>%
#               mutate(FX_CONTRIB_PP = FX_CONTRIB / N_RETURNEES)%>%
#               select(ROWID, MSNAME, DATE, FX_CONTRIB_PP))%>%
#   rename(ID = ROWID)%>%
#   bind_rows(costs_2019 %>%
#               ## contributions to countries that helped with an operation
#               ## but didn't deport anyone from their country are not taken into account
#               left_join(dests_2019_20%>%
#                           group_by(ROID, MSNAME)%>%
#                           summarize(N_RETURNEES = sum(N_RETURNEES, na.rm=T)))%>%
#               mutate(FX_CONTRIB_PP = FX_CONTRIB / N_RETURNEES)%>%
#               select(-N_RETURNEES)%>%
#               left_join(dests_2019_20)%>%
#               left_join(fx_staff_2019_20 %>% select(ROID, DATE))%>%
#               select(ID = ROID, DEST, N_RETURNEES, MSNAME, DATE, FX_CONTRIB_PP))%>%
#   bind_rows(test <- costs_2020_2021%>%
#               left_join(returnees_2020_2021 %>%
#                           group_by(ROID)%>%
#                           summarize(N_RETURNEES = sum(N_RETURNEES, na.rm=T)))%>%
#               mutate(FX_CONTRIB_PP = FX_CONTRIB / N_RETURNEES)%>%
#               select(-N_RETURNEES, -FX_CONTRIB)%>%
#               left_join(returnees_2020_2021_split %>%
#                           select(ROID, DATE, MSNAME, N_RETURNEES, DEST))%>%
#               rename(ID = ROID))%>%
#   ## it looks like contribution amounts were only included starting 2009.
#   ## before 2009: exclude from averages
#   ## after 2009: if NA, assume there was 0€ frontex contribution
#   filter(as.numeric(substr(DATE, 1, 4)) >= 2009 & 
#            N_RETURNEES > 0) %>%
#   mutate(FX_CONTRIB_PP = replace_na(FX_CONTRIB_PP, 0))
#             
# 
# CONTRIB_PP_BY_DEST <- OPERATIONS_BY_DEST %>%
#   group_by(DEST)%>%
#   summarise(FX_CONTRIB_PP = sum(FX_CONTRIB_PP * N_RETURNEES) / sum (N_RETURNEES))%>%
#   # add average across countries
#   bind_rows(OPERATIONS_BY_DEST%>%
#               group_by(DEST = "average across all destinations")%>%
#               summarise(FX_CONTRIB_PP = sum(FX_CONTRIB_PP * N_RETURNEES) / sum (N_RETURNEES)))%>%
#   arrange(desc(FX_CONTRIB_PP))%>%
#   # add ISO-codes for X-axis labels from world bank data
#   left_join(wb_cachelist$countries %>% 
#               select(iso = iso2c, country_name = country)%>%
#               mutate(country_name = str_replace_all(country_name, 
#                                                     c("Congo, Dem\\. Rep\\." = "Congo DR",
#                                                       "Gambia, The" = "Gambia",
#                                                       "Egypt, Arab Rep\\." = "Egypt",
#                                                       "Russian Federation" = "Russia"
#                                                     ))),
#             by = c("DEST" = "country_name"))%>%
#   mutate(iso = case_when(DEST == "average across all destinations" ~ "", 
#                          TRUE ~ iso))%>%
#   # only include those with at least 10 returnees, otherwise maybe distorted
#   filter(N_RETURNEES >= 10)%>%
#   head(26)


# # get operations with unique destinations

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
  bind_rows(dests_2019_20%>%
              group_by(ROID)%>%
              mutate(n_dest = n())%>%
              filter(n_dest == 1)%>%
              select(ROID, N_RETURNEES, DEST)%>%
              left_join(costs_2019 %>%
                          select(ROID, FX_CONTRIB))%>%
              ## throw out rows that are not in 2019 costs data
              filter(!is.na(FX_CONTRIB)))%>%
  bind_rows(returnees_2020_2021_split %>%
              group_by(ROID)%>%
              mutate(n_dest = n())%>%
              filter(n_dest == 1)%>%
              select(ROID, N_RETURNEES, DEST)%>%
              left_join(costs_2020_2021 %>%
                          select(ROID, FX_CONTRIB))
              )%>%
  ## with unique combinations of OPERATION and DEST, there is not much data (starting 2018 only)
  # ## it looks like contribution amounts were only included starting 2009.
  # ## before 2009: exclude from averages
  # ## after 2009: if NA, assume there was 0€ frontex contribution
  # filter(!(as.numeric(substr(ROWID, 1, 4)) < 2009 & !is.na(ROWID)) &
  #          N_RETURNEES > 0) %>%
  mutate(FX_CONTRIB = replace_na(FX_CONTRIB, 0))

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


# # put it all into consistent shape 
# ##################################
# 
# OPERATIONS_BY_DEST_MS <- by_dest_2006_18 %>%
#   # add some info from higher level
#   left_join(by_ms_2006_18 %>%
#               ## ROWID is unique to MS
#               select(ROWID, ID, ROID, MSNAME, MSISO, DATE)%>%
#               unique())%>%
#   select(ROID, MSNAME, MSISO, DEST, N_RETURNEES, ID, DATE)%>%
#   # bind data from 2019, 2020
#   bind_rows(dests_2019_20%>%
#               left_join(lookup_date_id_type%>%select(-OPTYPE))%>%
#               mutate(ID = ROID)
#   )%>%
#   # bind data from 2020, 2021
#   bind_rows(returnees_2020_2021%>%
#               ## only take 2021 numbers, because for 2020 the other data is better
#               ## (accurate distribution of returnees, not half-half method)
#               filter(substr(DATE, 1, 4) != "2020")%>%
#               dplyr::select(-OPTYPE, -MSTYPE)%>%
#               mutate(ID = ROID,
#                      MSNAME = str_replace_all(MSNAME, "The Netherlands", "Netherlands"))%>%
#               left_join(country_codes)
#   )%>%
#   ## filter out doubles for 2020
#   unique()%>%
#   ## test if any double entries left
#   group_by(ID, MSISO, DEST)%>%
#   mutate(n = n())
# 
# write_csv(OPERATIONS_BY_DEST_MS, "../clean_data/OPERATIONS_BY_DEST_MS_new.csv")
#
# OPERATIONS_BY_DEST_MS <- by_dest_2006_18 %>%
#   # add some info from higher level
#   left_join(by_ms_2006_18 %>%
#               select(ROWID, ID, ROID, MSNAME, MSISO, DATE)%>%
#               unique())%>%
#   select(ROID, MSNAME, MSISO, DEST, N_RETURNEES, N_ESC, N_ESC_LEAD, ROWID, ID, DATE)%>%
#   # bind data from 2019, 2020
#   bind_rows(dests_2019_20%>%
#               left_join(lookup_date_id_type%>%select(-OPTYPE))%>%
#               mutate(ID = ROID,
#                      ROWID = NA_character_,
#                      N_ESC = as.numeric(NA),
#                      N_ESC_LEAD = as.numeric(NA))
#   )%>%
#   # bind data from 2020, 2021
#   bind_rows(returnees_2020_2021_split%>%
#                             ## only take 2021 numbers, because for 2020 the other data is better
#                             ## (accurate distribution of returnees, not half-half method)
#                             filter(substr(DATE, 1, 4) != "2020")%>%
#                             dplyr::select(-OPTYPE, -MSTYPE)%>%
#                             mutate(ID = ROID,
#                                    MSNAME = str_replace_all(MSNAME, "The Netherlands", "Netherlands"))%>%
#                             left_join(country_codes)
#               )%>%
#   ## filter out doubles for 2020
#   unique()
# 
# write_csv(OPERATIONS_BY_DEST_MS, "../clean_data/OPERATIONS_BY_DEST_MS.csv")
# 
# OPERATIONS_BY_MS <-
#   #
#   # 2006 - 2018
#   #
#   # aggregate MS_DEST data
#   by_dest_2006_18 %>%
#   group_by(ROWID) %>%
#   summarise(N_RETURNEES = sum(N_RETURNEES, na.rm = T),
#             N_ESC = sum(N_ESC, na.rm = T),
#             N_ESC_LEAD = sum(N_ESC_LEAD, na.rm = T))%>%
#   # join with ms data
#   full_join(by_ms_2006_18,
#             by = "ROWID")%>%
#   # decide which values to take: take the number of deported people that is bigger
#   # (usually there was a faulty 0)
#   mutate(across(c(`N_RETURNEES.x`,`N_RETURNEES.y`), ~replace_na(., 0)))%>%
#   mutate(N_RETURNEES = case_when(
#     `N_RETURNEES.x` >=  `N_RETURNEES.y` ~ `N_RETURNEES.x`,
#     `N_RETURNEES.y` >  `N_RETURNEES.x` ~ `N_RETURNEES.y`
#   ))%>%
#   mutate(`N_ESC_OBS_MED.x` = N_ESC + N_ESC_LEAD + N_MEDS + N_OBS)%>%
#   mutate(N_ESC_OBS_MED = case_when(
#     `N_ESC_OBS_MED.x` >  N_ESC_OBS_MED ~ `N_ESC_OBS_MED.x`,
#     TRUE ~ N_ESC_OBS_MED
#   ))%>%
#   select(-c(`N_RETURNEES.x`,`N_RETURNEES.y`, `N_ESC_OBS_MED.x`))%>%
#   # aggregate per ID and country
#   # there is one case, where two deportations from germany to kosovo leave the same day, with the same
#   # number, but two different codes. will match their "ROIDs" / Codes here manually so they appear as
#   # the same operation
#   mutate(ROID = case_when(
#     ROID %in% c("RO-2016-112", "RO-2016-113") ~ "RO-2016-112/113",
#     TRUE ~ ROID
#   ))%>%
#   group_by(ID, DATE, OPTYPE, ROID, MSNAME, MSISO)%>% #
#   summarise(across(c(N_RETURNEES,
#                      N_ESC_OBS_MED,
#                      N_ESC,
#                      N_ESC_LEAD,
#                      N_MEDS,
#                      N_OBS,
#                      N_MONITORS,
#                      FX_CONTRIB), ~ sum(., na.rm = T)),
#             NOTES_COMMENTS = first(`Notes/comments`))%>%
#   ungroup()%>%
#   # if the number of escorts etc is unclear, turn zeros into NAs
#   mutate(across(c(N_ESC, N_ESC_LEAD), ~case_when(
#     is.na(N_ESC_OBS_MED - N_MEDS - N_OBS) ~ as.numeric(NA),
#     TRUE ~ .
#   )))%>%
#   select(DATE,
#          ROID,
#          ID,
#          MSNAME,
#          MSISO,
#          N_RETURNEES,
#          N_ESC_OBS_MED,
#          N_ESC,
#          N_ESC_LEAD,
#          N_MEDS,
#          N_OBS,
#          N_MONITORS,
#          FX_CONTRIB,
#          OPTYPE,
#          NOTES_COMMENTS
#   )%>%
#   filter(!(is.na(DATE) & N_RETURNEES == 0))%>%
#   mutate(across(where(is.numeric), ~case_when(
#     . == 0 ~ as.numeric(NA),
#     TRUE ~ .
#   )))%>%
#   #
#   # 2019 - 2020
#   #
#   bind_rows(
#     left_join(monitors_2019_20, escorts_2019_20, by = c("ROID", "MSISO", "MSNAME"))%>%
#       left_join(costs_2019, by = c("ROID", "MSISO", "MSNAME"))%>%
#       left_join(lookup_date_id_type, by = "ROID")%>%
#       mutate(ID = ROID)%>%
#       left_join(
#         dests_2019_20 %>%
#           group_by(ROID, MSISO)%>%
#           summarize(N_RETURNEES = sum(N_RETURNEES, na.rm=T))%>%
#           ungroup(),
#         by = c("ROID", "MSISO")
#       )%>%
#       rowwise()%>%
#       mutate(N_ESC_OBS_MED = sum(c(N_ESC, N_ESC_LEAD, N_ESC_POOL, N_OBS, N_MONITORS, N_MONITORS_POOL), na.rm = T))%>%
#       select(DATE,
#              ROID,
#              ID,
#              MSNAME,
#              MSISO,
#              N_RETURNEES,
#              N_ESC_OBS_MED,
#              N_ESC,
#              N_ESC_LEAD,
#              N_ESC_POOL,
#              N_OBS,
#              N_MONITORS,
#              N_MONITORS_POOL,
#              FX_CONTRIB,
#              OPTYPE
#       )
#   )%>%
#   #
#   # 2020 - 2021
#   #
#   bind_rows(returnees_2020_2021_split%>%
#               ## only add in data on escorts if the updated data from 2021/2022 is disaggregated by destination
#               left_join(esc_monitors_2020_21)%>%
#               dplyr::select(-MSTYPE, -DEST)%>%
#               left_join(costs_2020_2021)%>%
#               ## in the below sum, +N_OBS should be added once disaggregated data is available
#               mutate(N_ESC_OBS_MED = N_ESC_LEAD+N_ESC)%>%
#               mutate(ID = ROID,
#                      MSNAME = str_replace_all(MSNAME, "The Netherlands", "Netherlands"))%>%
#               left_join(country_codes))%>%
#   mutate(across(c(N_OBS, N_ESC_POOL, N_MONITORS_POOL), ~ifelse(is.na(.), 0, .)))%>%
#   ## filter out doubles
#   unique()
# 
# test <- returnees_2020_2021_split%>%
#   ## only add in data on escorts if the updated data from 2021/2022 is disaggregated by destination
#   left_join(esc_monitors_2020_21)%>%
#   dplyr::select(-MSTYPE, -DEST)%>%
#   left_join(costs_2020_2021)%>%
#   ## in the below sum, +N_OBS should be added once disaggregated data is available
#   mutate(N_ESC_OBS_MED = N_ESC_LEAD+N_ESC)%>%
#   mutate(ID = ROID,
#          MSNAME = str_replace_all(MSNAME, "The Netherlands", "Netherlands"))%>%
#   left_join(country_codes)
# 
# ## some operations are present in old but not new data
# ## because all operations with inconclusive dests_2019_20 have been filtered out
# operations_missing_newdata <- OPERATIONS_BY_MS %>%
#   filter(substr(DATE, 1, 4) == "2020")%>%
#   filter(!(ROID %in% test$ROID))
# 
# differing_data <- OPERATIONS_BY_MS %>% group_by(ID, MSNAME)%>%mutate(n=n()) %>% filter (n> 1)
# write_csv(differing_data, "../clean_data/DIFFERING_DATA_OPS_MS_2020.csv")
# 
# write_csv(OPERATIONS_BY_MS, "../clean_data/OPERATIONS_BY_MS.csv")
# 
# # should be zero:
# # OPERATIONS_BY_MS %>% group_by(MSISO, ID)%>%mutate(n=n())%>%filter(n>1)
# 
# OPERATIONS <- OPERATIONS_BY_MS %>%
#   ungroup()%>%
#   # aggregate MS data by ID (internally used "operation id")
#   group_by(ID, OPTYPE)%>%
#   summarize(across(c(N_MEDS,
#                      N_OBS,
#                      N_MONITORS,
#                      N_MONITORS_POOL,
#                      FX_CONTRIB,
#                      N_RETURNEES,
#                      N_ESC,
#                      N_ESC_LEAD,
#                      N_ESC_POOL,
#                      N_ESC_OBS_MED
#                      ),
#             ~ sum(.x, na.rm=T))
#   )  %>%
#   #
#   # add N_FX_STAFF
#   # for 2006 - 2018: BY_OP
#   # 2019 + 2020: fxstaff
#   #
#   full_join(bind_rows(by_op_2006_18 %>% select(-DATE),
#                       fx_staff_2019_20%>%
#                         select(ID = ROID,
#                                N_FX_STAFF)),
#             by = c("ID"))%>%
#   mutate(across(where(is.numeric), ~case_when(
#     . == 0 ~ as.numeric(NA),
#     TRUE ~ .
#   ))) %>%
#   select(ID,
#          OPTYPE,
#          N_RETURNEES,
#          N_FX_STAFF,
#          N_ESC_OBS_MED,
#          N_ESC,
#          N_ESC_LEAD,
#          N_MEDS,
#          N_OBS,
#          N_MONITORS,
#          N_MONITORS_POOL,
#          FX_CONTRIB
#   )%>%
#   #
#   # 2020 and 2021
#   #
#   bind_rows(returnees_2020_2021%>%
#               group_by(ROID)%>%
#               summarize(N_RETURNEES = sum(N_RETURNEES, na.rm=T))%>%
#               full_join(dests_2020_21 %>%select(ROID, OPTYPE)%>%unique())%>%
#               full_join(esc_monitors_2020_21%>%
#                           group_by(ROID)%>%
#                           summarize(across(where(is.numeric), ~ sum(., na.rm=T))))%>%
#               full_join(costs_2020_2021)%>%
#               full_join(obs_fxstaff_2020_21)%>%
#               rename(ID = ROID)%>%
#               select(-DATE)%>%
#               mutate(N_ESC_OBS_MED = N_ESC_LEAD+N_ESC+N_OBS)
#               )%>%
#   mutate(across(c(N_ESC_LEAD, N_MEDS, N_OBS, N_MONITORS, N_MONITORS_POOL), ~ifelse(is.na(.), 0, .)))%>%
#   unique()
# 
# differing_data <- OPERATIONS %>% group_by(ID)%>%mutate(n=n()) %>% filter (n> 1)
# write_csv(differing_data, "../clean_data/DIFFERING_DATA_OPS_2020.csv")
# 
# write_csv(OPERATIONS, "../clean_data/OPERATIONS.csv")

