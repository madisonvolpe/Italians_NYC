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
  #length(unique(dat$tract_uid))

# shapefile cleaning
  
  # read in shapefile 
  census_tracts <- readOGR(dsn = "Census_Tracts_2010")
  # create a uid, bc some census tracts are repeated for boros 
  census_tracts@data$uid <- paste(census_tracts@data$boro_code, census_tracts@data$ctlabel, sep = "")
  # create a vector that contains the combination of nta names and uid 
  ntaname <-census_tracts@data %>% distinct(ntaname, uid)

# join together to get NTA name in short dataset 
  dat<-left_join(dat, ntaname, by = c("tract_uid" = "uid"))

# write wide version to csv (with all variables)
write_csv(dat, "it_nyc_2008_2017_short_nta.csv")

  # convert to long ( with only some variables)
  dat_long <- select(dat, NAME.x,Census_Tract.x, County.x, State.x,TotalPop_12, TR_Italian_12,pro_2012,
                   TotalPop_17, TR_Italian_17, pro_2017, tract_short, tract_uid, ntaname)

  # reshape data to work for Tableau
  dat_long <- data.table::melt(setDT(dat_long),
     measure.vars = list(c(5,8), c(6,9), c(7,10)), variable.name = "Year", value.name = c("Population", "Italian","Prop"))

  dat_long$Year <- ifelse(dat_long$Year == 1, 2012, 2017)

# write long version to csv
write_csv(dat_long, "it_nyc_2008_2017_LongData.csv")
