library(needs)

needs(tidyverse,
      ggplot2)

source("clean_data.R")

per_deportation <- costs_operations %>%
  group_by(year = substr(DATE, 1, 4))%>%
  summarize(EUR = sum(EUR),
            N_RETURNEES = sum(N_RETURNEES))%>%
  mutate(pp = EUR/N_RETURNEES)

returnees %>%
  group_by(ROID, DEST, DATE)%>%
  summarize(N_RETURNEES = sum(N_RETURNEES, na.rm=T))%>%
  group_by(ROID, DATE)%>%
  mutate(n_dest = n())%>%
  group_by(year = substr(DATE, 1, 4))%>%
  summarize(doubles = sum(n_dest == 2),
            total = n())%>%
  mutate(not_counted = doubles/total)
