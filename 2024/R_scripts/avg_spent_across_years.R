library(needs)

needs(tidyverse,
      ggplot2)

source("clean_data.R")

per_deportation <- costs_operations %>%
  group_by(year = substr(DATE, 1, 4))%>%
  summarize(EUR = sum(EUR),
            N_RETURNEES = sum(N_RETURNEES))%>%
  mutate(pp = round(EUR/N_RETURNEES , 2))%>%
  rename(eur_total = EUR,
         number_returned = N_RETURNEES,
         avg_eur_per_person = pp)

write_csv(per_deportation, "../clean_data/average_per_year.csv")

per_deportation_country <- costs_operations %>%
  group_by(year = substr(DATE, 1, 4), DEST)%>%
  summarize(EUR = sum(EUR),
            N_RETURNEES = sum(N_RETURNEES))%>%
  mutate(pp = round(EUR/N_RETURNEES, 2))%>%
  rename(eur_total = EUR,
         destination = DEST,
         number_returned = N_RETURNEES,
         avg_eur_per_person = pp)%>%
  arrange(destination, year)

write_csv(per_deportation_country, "../clean_data/average_per_year_dest.csv")

share_notcounted <- returnees %>%
  group_by(ROID, DEST, DATE)%>%
  summarize(N_RETURNEES = sum(N_RETURNEES, na.rm=T))%>%
  group_by(ROID, DATE)%>%
  mutate(n_dest = n())%>%
  group_by(year = substr(DATE, 1, 4))%>%
  summarize(doubles = sum(n_dest == 2),
            total = n())%>%
  mutate(not_counted = round(doubles/total),2)

write_csv(share_notcounted, "../clean_data/averages_omitted.csv")
