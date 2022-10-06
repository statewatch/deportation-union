library(needs)

needs(tidyverse,
      ggplot2,
      readxl,
      glue,
      wbstats)


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
    ## attempt to create numbers per operation (not disaggregated by member states)
    mutate(ID = str_c(as.character(year), "_", as.character(Number)))%>%
    select(-Number)%>%
    ## check whether number or ROID are better measures
    # group_by(Number)%>%
    # mutate(n_in_number = n())%>%
    # group_by(ROID)%>%
    # mutate(n_in_roid = n())%>%
    # ungroup()%>%
    # # if same ROID is used for more rows than Number, use Number as "ROID"
    # mutate(ID = case_when(
    #   n_in_roid > n_in_number ~ Number,
    #   # in some exceptions prefer Number, when ROID is used across years:
    #   (ROID %in% c("2013/ROS/48", 
  #                "2014/ROS/07", 
  #                "2013/ROS/43",
  #                "2013/ROS/44",
  #                "2013/ROS/45",
  #                "2013/ROS/47",
  #                "2014/ROS/10")) == T ~ Number,
  #   TRUE ~ ROID
  # ))%>%
  # select(-n_in_roid, -n_in_number, -Number)%>%
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
    mutate(OPTYPE = toupper(OPTYPE))
  
  # frontex staff - cannot be disaggregated by member state etc
  by_op <- df %>%
    select(ID, DATE, N_FX_STAFF)%>%
    filter(!is.na(N_FX_STAFF) & N_FX_STAFF > 0)
  
  # data that is only available per operation and member state,
  # not disaggregated by dests_2019_20
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
           - `Escorts (3)`
    )%>%
    # for cases where date is not available,
    # set dec 31st of that year as date (so it can still be aggregated by year correctly)
    mutate(DATE = case_when(is.na(DATE)==T ~ as.Date(str_c(as.character(year), "-12-31")),
                            TRUE ~ as.Date(DATE)))%>%
    # filter out frontex data rows (they were mainly there for N_FX_STAFF attribution)
    # unless:
    # - money went from frontex to frontex
    # - they provided medical personnel, escorts or observers
    # - there was no member state involved in the operation
    group_by(ID)%>%
    mutate(N_MS_IN_OP = n())%>%
    ungroup()%>%
    # filter(!(MSISO == "FX" & 
    #            is.na(FX_CONTRIB) & 
    #            is.na(N_MEDS) & 
    #            N_MS_IN_OP > 1 & 
    #            is.na(N_OBS) & 
    #            is.na(N_MONITORS)))%>%
    select(-N_MS_IN_OP)
  
  # maybe add / take some data from "by_ms" to an "by_op" file, e.g. the number of monitors. tbd
  
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
                        ROWID
                )%>%
                gather(-ROWID, key=key, value=N_ESC)%>%
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
    select(-key)%>%
    # filter out data rows with no specified destination; often times mistakes in the data unfortunately.
    filter(!is.na(DEST))%>%
    ## filter out rows with no data attached to destination
    filter(!(is.na(N_RETURNEES) & is.na(N_ESC) & is.na(N_ESC_LEAD)))
  
  ifelse(year == 2018,
         by_ms_2006_18 <- by_ms,
         by_ms_2006_18 <- bind_rows(by_ms_2006_18, by_ms))
  
  ifelse(year == 2018,
         by_dest_2006_18 <- by_dest,
         by_dest_2006_18 <- bind_rows(by_dest_2006_18, by_dest))
  
  ifelse(year == 2018,
         by_op_2006_18 <- by_op,
         by_op_2006_18 <- bind_rows(by_op_2006_18, by_op))
}

rm(df, by_op, by_ms, by_dest, filepath, year, sheetnr)


# READ AND WRANGLE DATA FROM CONVERTED PDFS
# (YEARS 2019 AND 2020)
# PDF OBTAINED IN DEC 2021
###########################################

lookup_date_id_type <- read_excel("../raw_data/frontex_docs_converted/2019_DESTIN. per MS.xlsx") %>%
  select(DATE = "Date of Departure",
         ROID = "Operation ID")%>%
  left_join(read_excel("../raw_data/frontex_docs_converted/2019_DEST._MONIT._TYPE_FXSTAFF.xlsx", sheet = 6)%>%
              select(OPTYPE = `TYPE OF OPERATION`,
                     ROID),
            by = "ROID")%>%
  bind_rows(read_excel("../raw_data/frontex_docs_converted/2020_DESTIN. per MS.xlsx") %>%
              select(DATE = "Date of Departure",
                     ROID = "Operation ID")%>%
              left_join(read_excel("../raw_data/frontex_docs_converted/2020_DEST._MONIT._TYPE_FXSTAFF.xlsx", sheet = 5)%>%
                          select(OPTYPE = `TYPE OF OPERATION`,
                                 ROID),
                        by = "ROID"))%>%
  mutate(DATE = as.Date(str_c(substr(DATE, 7,11),"/", substr(DATE, 4,5), "/", substr(DATE, 1,2))))%>%
  unique() #%>%
## filter out RO-01516 because it will be added with an updated date in data below
# filter(ROID != "RO-01516")

# by ROID, MS and DEST
dests_2019_20 <- read_excel("../raw_data/frontex_docs_converted/2019_DESTIN. per MS.xlsx") %>%
  select(ROID = "Operation ID",
         MSNAME = MS,
         DEST = Destination,
         N_RETURNEES = "Number of TCNs")%>%
  mutate(MSNAME = str_replace_all(MSNAME, "The Netherlands", "Netherlands"))%>%
  left_join(country_codes, by="MSNAME")%>%
  bind_rows(read_excel("../raw_data/frontex_docs_converted/2020_DESTIN. per MS.xlsx") %>%
              select(ROID = "Operation ID",
                     MSNAME = MS,
                     DEST = Destination,
                     N_RETURNEES = "Number of TCNs")%>%
              mutate(MSNAME = str_replace_all(MSNAME, "The Netherlands", "Netherlands"))%>%
              left_join(country_codes, by="MSNAME"))%>%
  mutate(
    DEST = case_when(
      grepl("erzegovina", DEST) ~ "Bosnia and Herzegovina",
      grepl("Congo", DEST) ~ "Congo DR",
      grepl("Gambia", DEST) ~ "Gambia",
      grepl("Dominican", DEST) ~ "Dominican Republic",
      grepl("Kosovo", DEST) ~ "Kosovo",
      TRUE ~ DEST
    ))

# by ROID and MS
# OBS: VALUES FROM SEP 2020 ONWARDS MISSING
costs_2019 <- read_excel("../raw_data/frontex_docs_converted/2019_COSTS per MS.xlsx")%>%
  select(ROID = "RO",
         # OPTYPE = Type, better data from 2019_DEST._MONIT._TYPE_FXSTAFF.xlsx
         MSISO = `MS/SAC/TC/Frontex`,
         FX_CONTRIB = "TOTAL PAID")%>%
  left_join(country_codes, by="MSISO")%>%
  ## not adding costs for 2020 as the year's dataset is incomplete.
  ## use data from later request
  # bind_rows(
  #   read_excel("../raw_data/frontex_docs_converted/2020_COSTS per MS.xlsx")%>%
  #     select(ROID = "RO",
  #            # OPTYPE = Type, better data from 2019_DEST._MONIT._TYPE_FXSTAFF.xlsx
  #            MSISO = `MS/SAC/TC/Frontex`,
  #            FX_CONTRIB = "TOTAL PAID")%>%
  #     left_join(country_codes, by="MSISO")
  # )%>%
  mutate(FX_CONTRIB = as.numeric(str_remove_all(FX_CONTRIB, "[,€]")))

# by ROID
fx_staff_2019_20<- read_excel("../raw_data/frontex_docs_converted/2019_DEST._MONIT._TYPE_FXSTAFF.xlsx", sheet = 6)%>%
  select(OPTYPE = `TYPE OF OPERATION`, 
         ROID, 
         N_FX_STAFF = `FRONTEX STAFF ON BOARD`)%>%
  bind_rows(
    read_excel("../raw_data/frontex_docs_converted/2020_DEST._MONIT._TYPE_FXSTAFF.xlsx", sheet = 5)%>%
      select(OPTYPE = `TYPE OF OPERATION`, 
             ROID, 
             N_FX_STAFF = `FRONTEX STAFF ON BOARD`)
  )%>%
  left_join(lookup_date_id_type, by = c("ROID", "OPTYPE"))

# by ROID and MS
escorts_2019_20 <- read_excel("../raw_data/frontex_docs_converted/2019_ESCORTS_OBSERVERS.xlsx", sheet = 10) %>%
  select(ROID = "RO Title",
         MSNAME ="MS/SAC",
         N_ESC ="Escorts",
         N_ESC_LEAD = "Escort Leaders",
         N_ESC_POOL = "Escorts from Pool",
         N_OBS = "Observers"
  )%>%
  left_join(country_codes, by="MSNAME") %>%
  # some country-date-RO combinations have multiple rows
  # maybe for different dests_2019_20 (not present in data)?
  # need to be aggregated
  group_by(ROID, MSISO, MSNAME)%>%
  summarize(across(c(N_ESC, N_ESC_LEAD, N_ESC_POOL, N_OBS), ~sum(., na.rm=T)))%>%
  mutate(UNIQUE_ID=str_c(ROID, MSISO))%>%
  bind_rows(
    read_excel("../raw_data/frontex_docs_converted/2020_ESCORTS_OBSERVERS.xlsx", sheet = 7) %>%
      select(ROID = "RO Title",
             MSNAME ="MSSAC",
             N_ESC ="Escorts",
             N_ESC_LEAD = "Escort Leaders",
             N_ESC_POOL = "Escorts from Pool",
             N_OBS = "Observers"
      )%>%
      left_join(country_codes, by="MSNAME") %>%
      # some country-date-RO combinations have multiple rows
      # maybe for different dests_2019_20, which they don't tell us?
      # need to be aggregated
      group_by(ROID, MSISO, MSNAME)%>%
      summarize(across(c(N_ESC, N_ESC_LEAD, N_ESC_POOL, N_OBS), ~sum(., na.rm=T)))%>%
      mutate(UNIQUE_ID=str_c(ROID, MSISO))
  )

# by ROID and MS
monitors_2019_20 <- 
  # 2019
  # national monitors
  read_excel("../raw_data/frontex_docs_converted/2019_NAT.MONITORS per MS.xlsx")%>%
  select(ROID = "RO Title",
         MSNAME = MSSAC,
         N_MONITORS = "Monitors  national")%>%
  left_join(country_codes, by="MSNAME")%>%
  # some country-date-RO combinations have multiple rows
  # maybe for different dests_2019_20 (not present in data)?
  # need to be aggregated
  group_by(ROID, MSISO, MSNAME)%>%
  summarize(N_MONITORS = sum(N_MONITORS, na.rm = T))%>%
  # pool monitors
  left_join(read_excel("../raw_data/frontex_docs_converted/2019_DEST._MONIT._TYPE_FXSTAFF.xlsx", sheet = 6)%>%
              select(ROID, 
                     N_MONITORS_POOL = `MONITORS ON BOARD (from MS or/and from Frontex pool)`)%>%
              separate(N_MONITORS_POOL, c("1", "2", "3", "4", "5", "6"), sep = ",")%>%
              gather(-ROID, key = key, value = N_MONITORS_POOL)%>%
              select(-key)%>%
              filter(grepl("pool", N_MONITORS_POOL))%>%
              mutate(MSISO = str_extract(N_MONITORS_POOL, "[A-Z]+"))%>%
              mutate(N_MONITORS_POOL = as.numeric(str_extract(N_MONITORS_POOL, "[0-9]+")))%>%
              left_join(country_codes, by="MSISO"),
            by = c("ROID", "MSNAME", "MSISO")
  )%>%
  # 2020
  # national
  bind_rows(
    read_excel("../raw_data/frontex_docs_converted/2020_NAT.MONITORS per MS.xlsx")%>%
      select(ROID = "RO Title",
             MSNAME = MSSAC,
             N_MONITORS = "Monitors  national")%>%
      left_join(country_codes, by="MSNAME")%>%
      # some country-date-RO combinations have multiple rows
      # maybe for different dests_2019_20 (not present in data)?
      # need to be aggregated
      group_by(ROID, MSISO, MSNAME)%>%
      summarize(N_MONITORS = sum(N_MONITORS, na.rm = T))%>%
      # pool monitors
      left_join(pool_mons_2019 <- read_excel("../raw_data/frontex_docs_converted/2020_DEST._MONIT._TYPE_FXSTAFF.xlsx", sheet = 5)%>%
                  select(ROID, 
                         N_MONITORS_POOL = `MONITORS ON BOARD (from MS or/and from Frontex pool)`)%>%
                  separate(N_MONITORS_POOL, c("1", "2", "3", "4", "5", "6"), sep = ",")%>%
                  gather(-ROID, key = key, value = N_MONITORS_POOL)%>%
                  select(-key)%>%
                  filter(grepl("pool", N_MONITORS_POOL))%>%
                  mutate(MSISO = str_extract(N_MONITORS_POOL, "[A-Z]+"))%>%
                  mutate(N_MONITORS_POOL = as.numeric(str_extract(N_MONITORS_POOL, "[0-9]+")))%>%
                  # mutate(FROM_POOL = TRUE)%>%
                  left_join(country_codes, by="MSISO"),
                by = c("ROID", "MSNAME", "MSISO")
      )
  )

####
# export specific (more detailed than others) file formats for 2019 and 2020 here
# from here on data will only be used in ways that is possible across all years 
####

## by roid, MS and dest

write_csv(dests_2019_20, "../clean_data/2019_20_BY_ID_MS_DEST.csv")

## by roid and MS

monitors_escorts_2019_20 <- full_join(
  escorts_2019_20,
  monitors_2019_20
)

write_csv(monitors_escorts_2019_20, "../clean_data/2019_20_BY_ID_MS.csv")

write_csv(costs_2019, "../clean_data/2019_COSTS_BY_ID_MS.csv")

## by roid

write_csv(fx_staff_2019_20, "../clean_data/2019_20_BY_ID.csv")



# READ AND WRANGLE DATA FROM CONVERTED PDF
# (YEARS 2020 and 2021)
# PDF OBTAINED IN AUG 2022
###########################################

dests_2020_21_orig <- read_csv("../raw_data/frontex_docs_converted/tabula-PAD-2022-00265-destinations2020.csv",
                               col_names = c("DATE", "OPTYPE", "DEST", "ROID"))%>%
  bind_rows(read_csv("../raw_data/frontex_docs_converted/tabula-PAD-2022-00265-destinations2021.csv",
                     col_names = c("DATE", "OPTYPE", "DEST", "ROID")))%>%
  ## RO-01516 has differing dates, see readme. make consistent
  mutate(DATE = ifelse(ROID == "RO-01516", "14/10/2020", DATE))%>%
  mutate(DATE = as.Date(str_c(substr(DATE, 7,11),"/", substr(DATE, 4,5), "/", substr(DATE, 1,2))))%>%
  unique()%>%
  filter(ROID != "ROID")%>%
  mutate(DEST = str_replace_all(DEST, "Viet Nam", "Vietnam"))%>%
  ## filter out voluntary return operations for now, as they are only present from 2020 onwards
  filter(OPTYPE != "VRD")

## for now (until we obtain better data:)
## evenly distribute returnees across destination

dests_2020_21 <- dests_2020_21_orig %>%
  mutate(DEST = str_remove_all(DEST, " & Pakistan \\(Pakistan cancelled\\)"))%>%
  separate(DEST, c("DEST_1", "DEST_2"), sep = " & ")%>%
  gather(DEST_1, DEST_2, key = key, value = DEST)%>%
  select(-key)%>%
  filter(!is.na(DEST))%>%
  group_by(ROID)%>%
  mutate(N_DEST = n())%>%
  ungroup()%>%
  mutate(DEST = str_remove_all(DEST, "\\*"))

costs_2020_2021 <- read_csv("../raw_data/frontex_docs_converted/tabula-PAD-2022-00265-costs2020.csv")%>%
  bind_rows(read_csv("../raw_data/frontex_docs_converted/tabula-PAD-2022-00265-costs2021.csv"))%>%
  dplyr::select(DATE = 1, FX_CONTRIB = 3)%>%
  mutate(ROID = substr(DATE, 12, 99))%>%
  filter(FX_CONTRIB != "TOTAL PAID")%>%
  ## RO-01516 has differing dates, see readme. make consistent
  mutate(DATE = ifelse(ROID == "RO-01516", "14/10/2020", DATE))%>%
  mutate(DATE = as.Date(str_c(substr(DATE, 7,10),"/", substr(DATE, 4,5), "/", substr(DATE, 1,2))))%>%
  mutate(FX_CONTRIB = as.numeric(str_remove_all(FX_CONTRIB, "[ ,€]")))

returnees_2020_2021 <- read_csv("../raw_data/frontex_docs_converted/tabula-PAD-2022-00265-deportees2020.csv")%>%
  bind_rows(read_csv("../raw_data/frontex_docs_converted/tabula-PAD-2022-00265-deportees2021.csv"))%>%
  rename(DATE = 1, ROID = 2, MSNAME = 3, MSTYPE = 4, N_RETURNEES = 5)%>%
  filter(grepl("RO", ROID))%>%
  ## RO-01516 has differing dates, see readme. make consistent
  mutate(DATE = ifelse(ROID == "RO-01516", "14/10/2020", DATE))%>%
  mutate(N_RETURNEES = as.numeric(N_RETURNEES),
         DATE = as.Date(str_c(substr(DATE, 7,10),"/", substr(DATE, 4,5), "/", substr(DATE, 1,2))))

esc_monitors_2020_21 <- read_csv("../raw_data/frontex_docs_converted/tabula-PAD-2022-00265-escortsmonitors2020.csv")%>%
  rename(ROID = 1,
         DATE = 2,
         MSNAME = 3,
         MSTYPE = 4,
         N_ESC_LEAD = 5,
         N_ESC = 6,
         N_MONITORS_POOL = 7,
         N_MONITORS = 8)%>%
  bind_rows(read_csv("../raw_data/frontex_docs_converted/tabula-PAD-2022-00265-escortsmonitors2021.csv")%>%
              rename(ROID = 1,
                     DATE = 2,
                     MSNAME = 3,
                     MSTYPE = 4,
                     N_ESC_LEAD = 5,
                     N_ESC = 6,
                     N_MONITORS_POOL = 7,
                     N_MONITORS = 8))%>%
  ## RO-01516 has differing dates, see readme. make consistent
  mutate(DATE = ifelse(ROID == "RO-01516", "14/10/2020", DATE))%>%
  mutate(DATE = as.Date(str_c(substr(DATE, 7,10),"/", substr(DATE, 4,5), "/", substr(DATE, 1,2))))

obs_fxstaff_2020_21 <- read_csv("../raw_data/frontex_docs_converted/tabula-PAD-2022-00265-observers_fxstaff2020.csv")%>%
  bind_rows(read_csv("../raw_data/frontex_docs_converted/tabula-PAD-2022-00265-observers_fxstaff2021.csv"))%>%
  rename(DATE = 2, N_OBS = 3, N_FX_STAFF = 4)%>%
  filter(ROID != "ROID")%>%
  ## RO-01516 has differing dates, see readme. make consistent
  mutate(DATE = ifelse(ROID == "RO-01516", "14/10/2020", DATE))%>%
  mutate(DATE = as.Date(str_c(substr(DATE, 7,10),"/", substr(DATE, 4,5), "/", substr(DATE, 1,2))))%>%
  mutate(across(c(N_OBS, N_FX_STAFF), ~ as.numeric(.)))


## export for web
#################

returnees_2020_2021_split <- returnees_2020_2021 %>%
  ## spread across destinations
  full_join(dests_2020_21)%>%
  mutate(N_RETURNEES = N_RETURNEES / N_DEST)%>%
  select(-N_DEST)

# Operations per year 
# -------------------

OPTYPE_YEAR <- by_ms_2006_18%>%
  select(DATE, N_RETURNEES, OPTYPE)%>%
  bind_rows(dests_2019_20 %>%
              group_by(ROID)%>%
              summarize(N_RETURNEES = sum(N_RETURNEES, na.rm = T))%>%
              left_join(fx_staff_2019_20 %>%
                          select(-N_FX_STAFF))%>%
              select(-ROID)
            )%>%
  bind_rows(returnees_2020_2021%>%
              left_join(dests_2020_21 %>%
                          select(DATE, ROID, OPTYPE)%>%
                          unique())%>%
              ## only take 2021 data to avoid doubles
              filter(substr(DATE, 1, 4) != "2020")%>%
              group_by(DATE, OPTYPE)%>%
              summarise(N_RETURNEES = sum(N_RETURNEES, na.rm = T)))%>%
  mutate(YEAR = as.numeric(substr(DATE, 1, 4)))%>%
  group_by(YEAR, OPTYPE)%>%
  summarize(`Number of deported people` = sum(N_RETURNEES, na.rm = T), 
            `Number of operations` = n())%>%
  filter(!is.na(OPTYPE))%>%
  gather(-YEAR, -OPTYPE, key = KEY, value = value)%>%
  spread(key = OPTYPE, value = value)%>%
  mutate(across(c(CRO:NRO), ~replace_na(., 0)))

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

