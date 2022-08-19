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

for(year in c(2018:2006)){
  
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
    # attempt to create numbers per operation (not disaggregated by member states)
    mutate(ID = str_c(as.character(year), "_", as.character(Number)))%>%
    select(-Number)%>%
    # check whether number or ROID are better measures
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
    # fix spelling mistakes
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
  # not disaggregated by destinations
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
    filter(!(MSISO == "FX" & 
               is.na(FX_CONTRIB) & 
               is.na(N_MEDS) & 
               N_MS_IN_OP > 1 & 
               is.na(N_OBS) & 
               is.na(N_MONITORS)))%>%
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
    # create and join dataframe of destinations
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
                ),
       by=c("ROWID", "key"))%>%
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
    filter(!is.na(DEST))
  
  ifelse(year == 2018,
         all_by_ms <- by_ms,
         all_by_ms <- bind_rows(all_by_ms, by_ms))
  
  ifelse(year == 2018,
         all_by_dest_ms <- by_dest,
         all_by_dest_ms <- bind_rows(all_by_dest_ms, by_dest))
  
  ifelse(year == 2018,
         all_by_op <- by_op,
         all_by_op <- bind_rows(all_by_op, by_op))
}

rm(df, by_op, by_ms, by_dest, filepath, year, sheetnr)


# READ AND WRANGLE DATA FROM CONVERTED PDFS
# (YEARS 2019 AND 2020)
###########################################

# total: 330 operations
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
  unique()

# by ROID, MS and DEST
destinations <- read_excel("../raw_data/frontex_docs_converted/2019_DESTIN. per MS.xlsx") %>%
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
# OBS: VALUES FROM SEP ONWARDS MISSING
costs <- read_excel("../raw_data/frontex_docs_converted/2019_COSTS per MS.xlsx")%>%
  select(ROID = "RO",
         # OPTYPE = Type, better data from 2019_DEST._MONIT._TYPE_FXSTAFF.xlsx
         MSISO = `MS/SAC/TC/Frontex`,
         FX_CONTRIB = "TOTAL PAID")%>%
  left_join(country_codes, by="MSISO")%>%
  bind_rows(
    read_excel("../raw_data/frontex_docs_converted/2020_COSTS per MS.xlsx")%>%
      select(ROID = "RO",
             # OPTYPE = Type, better data from 2019_DEST._MONIT._TYPE_FXSTAFF.xlsx
             MSISO = `MS/SAC/TC/Frontex`,
             FX_CONTRIB = "TOTAL PAID")%>%
      left_join(country_codes, by="MSISO")
  )%>%
  mutate(FX_CONTRIB = as.numeric(str_remove_all(FX_CONTRIB, "[,€]")))

# by ROID
fx_staff<- read_excel("../raw_data/frontex_docs_converted/2019_DEST._MONIT._TYPE_FXSTAFF.xlsx", sheet = 6)%>%
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

# by ROID or ROID and MS? unclear how to connect
escorts <- read_excel("../raw_data/frontex_docs_converted/2019_ESCORTS_OBSERVERS.xlsx", sheet = 10) %>%
  select(ROID = "RO Title",
         MSNAME ="MS/SAC",
         N_ESC ="Escorts",
         N_ESC_LEAD = "Escort Leaders",
         N_ESC_POOL = "Escorts from Pool",
         N_OBS = "Observers"
  )%>%
  left_join(country_codes, by="MSNAME") %>%
  # some country-date-RO combinations have multiple rows
  # maybe for different destinations, which they don't tell us?
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
      # maybe for different destinations, which they don't tell us?
      # need to be aggregated
      group_by(ROID, MSISO, MSNAME)%>%
      summarize(across(c(N_ESC, N_ESC_LEAD, N_ESC_POOL, N_OBS), ~sum(., na.rm=T)))%>%
      mutate(UNIQUE_ID=str_c(ROID, MSISO))
  )

# by ROID or ROID and MS? unclear how to connect
monitors <- 
  # 2019
  # national monitors
  read_excel("../raw_data/frontex_docs_converted/2019_NAT.MONITORS per MS.xlsx")%>%
  select(ROID = "RO Title",
         MSNAME = MSSAC,
         N_MONITORS = "Monitors  national")%>%
  left_join(country_codes, by="MSNAME")%>%
  # some country-date-RO combinations have multiple rows
  # maybe for different destinations, which they don't tell us?
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
      # maybe for different destinations, which they don't tell us?
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
# possibility to export specific (more detailed than others) file formats for 2019 and 2020 here
# from here on data will only be used in ways that is possible across all years 
####


# put it all into consistent shape 
##################################

OPERATIONS_BY_DEST_MS <- all_by_dest_ms %>%
  # add some info from higher level
  left_join(all_by_ms %>%
              select(ROWID, ID, ROID, MSNAME, MSISO, DATE)%>%
              unique())%>%
  select(ROID, MSNAME, MSISO, DEST, N_RETURNEES, N_ESC, N_ESC_LEAD, ROWID, ID, DATE)%>%
  # bind data from 2019, 2020
  bind_rows(destinations%>%
              left_join(lookup_date_id_type%>%select(-OPTYPE))%>%
              mutate(ID = ROID,
                     ROWID = NA_character_,
                     N_ESC = as.numeric(NA),
                     N_ESC_LEAD = as.numeric(NA))
  )

write_csv(OPERATIONS_BY_DEST_MS, "../clean_data/OPERATIONS_BY_DEST_MS.csv")

OPERATIONS_BY_MS <- 
  #
  # 2006 - 2018
  #
  # aggregate MS_DEST data
  all_by_dest_ms %>%
  group_by(ROWID) %>%
  summarise(N_RETURNEES = sum(N_RETURNEES, na.rm = T),
            N_ESC = sum(N_ESC, na.rm = T),
            N_ESC_LEAD = sum(N_ESC_LEAD, na.rm = T))%>%
  # join with ms data
  full_join(all_by_ms,
            by = "ROWID")%>%
  # decide which values to take: take the number of deported people that is bigger
  # (usually there was a faulty 0)
  mutate(across(c(`N_RETURNEES.x`,`N_RETURNEES.y`), ~replace_na(., 0)))%>%
  mutate(N_RETURNEES = case_when(
    `N_RETURNEES.x` >=  `N_RETURNEES.y` ~ `N_RETURNEES.x`,
    `N_RETURNEES.y` >  `N_RETURNEES.x` ~ `N_RETURNEES.y`
  ))%>% 
  mutate(`N_ESC_OBS_MED.x` = N_ESC + N_ESC_LEAD + N_MEDS + N_OBS)%>%
  mutate(N_ESC_OBS_MED = case_when(
    `N_ESC_OBS_MED.x` >  N_ESC_OBS_MED ~ `N_ESC_OBS_MED.x`,
    TRUE ~ N_ESC_OBS_MED
  ))%>% 
  select(-c(`N_RETURNEES.x`,`N_RETURNEES.y`, `N_ESC_OBS_MED.x`))%>%
  # aggregate per ID and country
  # there is one case, where two deportations from germany to kosovo leave the same day, with the same
  # number, but two different codes. will match their "ROIDs" / Codes here manually so they appear as
  # the same operation
  mutate(ROID = case_when(
    ROID %in% c("RO-2016-112", "RO-2016-113") ~ "RO-2016-112/113",
    TRUE ~ ROID
  ))%>%
  group_by(ID, DATE, OPTYPE, ROID, MSNAME, MSISO)%>% #  
  summarise(across(c(N_RETURNEES,
                     N_ESC_OBS_MED,
                     N_ESC,
                     N_ESC_LEAD,
                     N_MEDS,
                     N_OBS,
                     N_MONITORS,
                     FX_CONTRIB), ~ sum(., na.rm = T)),
            NOTES_COMMENTS = first(`Notes/comments`))%>%
  ungroup()%>%
  # if the number of escorts etc is unclear, turn zeros into NAs
  mutate(across(c(N_ESC, N_ESC_LEAD), ~case_when(
    is.na(N_ESC_OBS_MED - N_MEDS - N_OBS) ~ as.numeric(NA),
    TRUE ~ .
  )))%>%
  select(DATE,
         ROID,
         ID,
         MSNAME,
         MSISO,
         N_RETURNEES,
         N_ESC_OBS_MED,
         N_ESC,
         N_ESC_LEAD,
         N_MEDS,
         N_OBS,
         N_MONITORS,
         FX_CONTRIB,
         OPTYPE,
         NOTES_COMMENTS
  )%>%
  filter(!(is.na(DATE) & N_RETURNEES == 0))%>%
  mutate(across(where(is.numeric), ~case_when(
    . == 0 ~ as.numeric(NA),
    TRUE ~ .
  )))%>%
  #
  # 2019 - 2020
  # 
  bind_rows(
    left_join(monitors, escorts, by = c("ROID", "MSISO", "MSNAME"))%>%
      left_join(costs, by = c("ROID", "MSISO", "MSNAME"))%>%
      left_join(lookup_date_id_type, by = "ROID")%>%
      mutate(ID = ROID)%>%
      left_join(
        destinations %>%
          group_by(ROID, MSISO)%>%
          summarize(N_RETURNEES = sum(N_RETURNEES, na.rm=T))%>%
          ungroup(),
        by = c("ROID", "MSISO")
      )%>%
      rowwise()%>%
      mutate(N_ESC_OBS_MED = sum(c(N_ESC, N_ESC_LEAD, N_ESC_POOL, N_OBS, N_MONITORS, N_MONITORS_POOL), na.rm = T))%>%
      select(DATE, 
             ROID, 
             ID, 
             MSNAME, 
             MSISO, 
             N_RETURNEES, 
             N_ESC_OBS_MED, 
             N_ESC, 
             N_ESC_LEAD, 
             N_ESC_POOL, 
             N_OBS, 
             N_MONITORS,
             N_MONITORS_POOL,
             FX_CONTRIB,
             OPTYPE
      )
  )

write_csv(OPERATIONS_BY_MS, "../clean_data/OPERATIONS_BY_MS.csv")


# should be zero:
# OPERATIONS_BY_MS %>% group_by(MSISO, ID)%>%mutate(n=n())%>%filter(n>1)

OPERATIONS <- OPERATIONS_BY_MS %>%
  ungroup()%>%
  # aggregate MS data by ID (internally used "operation id")
  group_by(ID, OPTYPE)%>%
  summarize(across(c(N_MEDS,
                     N_OBS,
                     N_MONITORS,
                     N_MONITORS_POOL,
                     FX_CONTRIB,
                     N_RETURNEES,
                     N_ESC,
                     N_ESC_LEAD,
                     N_ESC_POOL,
                     N_ESC_OBS_MED
                     ),
            ~ sum(.x, na.rm=T))
  )  %>%
  # 
  # add N_FX_STAFF
  # for 2006 - 2018: BY_OP
  # 2019 + 2020: fxstaff
  # 
  full_join(bind_rows(all_by_op %>% select(-DATE),
                      fx_staff%>%
                        select(ID = ROID,
                               N_FX_STAFF)),
            by = c("ID"))%>%
  mutate(across(where(is.numeric), ~case_when(
    . == 0 ~ as.numeric(NA),
    TRUE ~ .
  ))) %>%
  select(ID,
         OPTYPE,
         N_RETURNEES,
         N_FX_STAFF,
         N_ESC_OBS_MED,
         N_ESC,
         N_ESC_LEAD,
         N_MEDS,
         N_OBS,
         N_MONITORS,
         FX_CONTRIB
  )

write_csv(OPERATIONS, "../clean_data/OPERATIONS.csv")

