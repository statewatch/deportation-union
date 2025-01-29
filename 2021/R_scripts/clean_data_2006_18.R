library(needs)

needs(tidyverse,
      ggplot2,
      readxl,
      glue)


country_codes <- read_excel("../raw_data/country codes.xlsx")%>%
  mutate(across(1:4, ~trimws(.))) %>%
  select(-vorwahl, -iso2)%>%
  rename("MSISO" = "iso3",
         "MSNAME" = "name")

# READ AND WRANGLE DATA FROM SPREADSHEET
# (YEARS 2006 - 2018)
########################################

for(year in c(2018:2006)){ # year <- 2016
  
  filepath <- "../raw_data/deportation-union-data.xlsx"
  
  sheetnr <- case_when(year == 2018 ~ 12,
                       year == 2017 ~ 13,
                       year == 2016 ~ 14,
                       year == 2015 ~ 15,
                       year == 2014 ~ 16,
                       year == 2013 ~ 17,
                       year == 2012 ~ 18,
                       year == 2011 ~ 19,
                       year == 2010 ~ 20,
                       year == 2009 ~ 21,
                       year == 2008 ~ 22,
                       year == 2007 ~ 23,
                       year == 2006 ~ 24)
  
  df <- read_excel(filepath,
                   sheet = sheetnr,
                   col_types = c(
                     "numeric",
                     "date",
                     "text",
                     "text",
                     "text",
                     "text",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "numeric",
                     "text",
                     "text",
                     "numeric",
                     "text",
                     "numeric",
                     "text",
                     "text"
                   ))%>%
    rename(
      DATE = Date,
      MSNAME = "Member States",
      N_RETURNEES = Returnees,
      N_RETURNEES = Returnees,
      N_FX_STAFF = "Frontex staff",
      N_ESC_OBS_MED = "Escorts, observers, escort leaders, medical staff",
      N_MEDS = "Medical personnel",
      N_OBS = Observers,
      N_MONITORS = Monitors,
      FX_CONTRIB = "Frontex contribution",
      ROID = Code,
      OPTYPE = Type
    )%>%
    select(-c(
      Departure, # <- almost never information there
      Stopover, # <- almost never information there
      "Charter cost", # <- almost never information there
    ))%>%
    ## create numbers per operation (not disaggregated by member states)
    mutate(ID = str_c(as.character(year), "_", as.character(Number)))%>%
    select(-Number)%>%
  ## fix spelling mistakes
  mutate(MSNAME = case_when(
    grepl("Fronte", MSNAME, ignore.case = T) ~ "Frontex",
    grepl("UK", MSNAME) ~ "United Kingdom",
    grepl("taly", MSNAME, ignore.case = T) ~ "Italy",
    grepl("embourg", MSNAME, ignore.case = T) ~ "Luxembourg",
    grepl("observer", MSNAME, ignore.case = T) ~ str_remove(MSNAME, " \\(observer\\)"),
    grepl("ombudsman", MSNAME, ignore.case = T) ~ str_remove(MSNAME, " \\(Ombudsman\\)"),
    grepl("N\\/A", MSNAME) ~ NA_character_,
    TRUE ~ MSNAME
  ))%>%
    left_join(country_codes, by = "MSNAME")%>%
    mutate(ROWID = str_c(year,"_",as.character(row_number())))%>%
    # fix typo
    mutate(OPTYPE = toupper(OPTYPE))%>%
    mutate(N_ESC_OBS_MED_check = rowSums(.[12:19], na.rm = T))%>%
    mutate(N_ESC_OBS_MED = case_when(
      N_ESC_OBS_MED_check > N_ESC_OBS_MED ~ N_ESC_OBS_MED_check,
      TRUE ~ N_ESC_OBS_MED
    ))%>%
    mutate(check_ESC_OBS_MED_by_dest = case_when(
      N_ESC_OBS_MED_check == N_ESC_OBS_MED ~ TRUE,
      TRUE ~ FALSE
    ))%>%
    select(-N_ESC_OBS_MED_check)

  by_ms <- df %>%
    select(- `Destination (1)`,
           - `Destination (2)`,
           - `Destination (3)`,
           - `Returnees (1)`,
           - `Returnees (2)`,
           - `Returnees (3)`,
           - `Escort leaders (1)`,
           - `Escort leaders (2)`,
           - `Escort leaders (3)`,
           - `Escorts (1)`,
           - `Escorts (2)`,
           - `Escorts (3)`,
           - check_ESC_OBS_MED_by_dest
    )%>%
    # for cases where date is not available,
    # set dec 31st of that year as date (so it can still be aggregated by year correctly)
    mutate(DATE = case_when(is.na(DATE)==T ~ as.Date(str_c(as.character(year), "-12-31")),
                            TRUE ~ as.Date(DATE)))
  
  by_dest <- df %>%
    # create dataframe of numbers of deported people
    select(`Returnees (1)`,
           `Returnees (2)`,
           `Returnees (3)`,
           ROWID
    )%>%
    gather(-ROWID, key=key, value=N_RETURNEES)%>%
    mutate(key = str_replace_all(key,"Returnees ", ""))%>% # <- the number in brackets in the original, e.g. returnees (3)
    filter(!is.na(N_RETURNEES)) %>%
    # create and join dataframe of dests_2019_20
    full_join(df %>%
                select(`Destination (1)`,
                       `Destination (2)`,
                       `Destination (3)`,
                       ROWID
                )%>%
                gather(-ROWID, key=key, value=DEST)%>%
                mutate(key = str_replace_all(key,"Destination ", ""))%>%# <- the number in brackets in the original, e.g. returnees (3)
                filter(!is.na(DEST))%>%
                mutate(
                  DEST = case_when(
                    grepl("erzegovina", DEST) ~ "Bosnia and Herzegovina",
                    grepl("Congo", DEST) ~ "Congo DR",
                    grepl("Gambia", DEST) ~ "Gambia",
                    grepl("Dominican", DEST) ~ "Dominican Republic",
                    grepl("Kosovo", DEST) ~ "Kosovo",
                    DEST == "x" ~ NA_character_,
                    TRUE ~ DEST
                  )
                ))%>%
    # create and join dataframe of escorts
    full_join(df %>%
                select( `Escorts (1)`,
                        `Escorts (2)`,
                        `Escorts (3)`,
                        ROWID,
                        check_ESC_OBS_MED_by_dest
                )%>%
                gather(-ROWID, -check_ESC_OBS_MED_by_dest, key=key, value=N_ESC)%>%
                mutate(key = str_replace_all(key,"Escorts ", ""))%>% # <- the number in brackets in the original, e.g. returnees (3)
                filter(!is.na(N_ESC)), 
              by=c("ROWID", "key"))%>%
    # create and join dataframe of escort leaders
    full_join(df %>%
                select(`Escort leaders (1)`,
                       `Escort leaders (2)`,
                       `Escort leaders (3)`,
                       ROWID
                )%>%
                gather(-ROWID, key=key, value=N_ESC_LEAD)%>%
                mutate(key = str_replace_all(key,"Escort leaders ", ""))%>% # <- the number in brackets in the original, e.g. returnees (3)
                filter(!is.na(N_ESC_LEAD)), 
              by=c("ROWID", "key"))%>%
    select(-key) %>%
    # ## filter out rows with no data
    filter(!(is.na(N_RETURNEES) & is.na(DEST) & is.na(N_ESC) & is.na(N_ESC_LEAD)))
    
  ifelse(year == 2018,
         by_ms_2006_18 <- by_ms,
         by_ms_2006_18 <- bind_rows(by_ms_2006_18, by_ms))
  
  ifelse(year == 2018,
         by_dest_2006_18 <- by_dest,
         by_dest_2006_18 <- bind_rows(by_dest_2006_18, by_dest))
}

rm(df, by_ms, by_dest, filepath, year, sheetnr)

write_csv(by_dest_2006_18, "../clean_data/by_dest_2006_18.csv")
write_csv(by_ms_2006_18, "../clean_data/by_ms_2006_18.csv")

