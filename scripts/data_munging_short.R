library(tidyverse)
library(rgdal)
library(ggplot2)
library(rgeos)

# data cleaning 
  
  # read in dataset 
  dat <- read.csv("it_nyc_2008_2017_short.csv")
  
  # extract census tract 
  dat$tract_short <-stringr::str_remove_all(string = dat$Census_Tract.x, pattern = "[A-Za-z]")
  dat$tract_short <- trimws(dat$tract_short)
  
  # create a new column that combines the tract with the borocode (1, 2,3,4,5)- this creates a UID bc 
  # the same tract names can appear in different boros
  # 1- MN, 2- BX, 3 - BK, 4 - QN, 5 - SI 
  dat <- dat %>% 
    mutate(tract_uid = case_when(
      County.x == "New York County" ~ paste(1, tract_short, sep = ""),
      County.x == "Bronx County" ~ paste(2, tract_short, sep = ""),
      County.x == "Kings County" ~ paste(3, tract_short, sep = ""),
      County.x == "Queens County" ~ paste(4, tract_short, sep = ""),
      County.x == "Richmond County" ~ paste(5, tract_short, sep ="")
    ))
  # length(unique(dat$tract_uid))

# shapefile cleaning
  
  # read in shapefile 
  census_tracts <- readOGR(dsn = "Census_Tracts_2010")
  # create a uid, bc some census tracts are repeated for boros 
  census_tracts@data$uid <- paste(census_tracts@data$boro_code, census_tracts@data$ctlabel, sep = "")
  # create a vector that contains the combination of nta names and uid 
  ntaname <-census_tracts@data %>% distinct(ntaname, uid)

# join together to get NTA name in short dataset 
  dat<-left_join(dat, ntaname, by = c("tract_uid" = "uid"))

# write to csv 

write_csv(dat, "it_nyc_2008_2017_short_nta.csv")
