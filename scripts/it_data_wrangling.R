library(censusapi)
library(plyr)
library(tidyverse)

Sys.setenv(CENSUS_KEY= '6266a93443753a9bc219aaa0a4f501ab40ab164f')
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_KEY")

# list all available apis 
APIs <- listCensusApis()

  # state codes: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMSDataDict13.txt
  # list of variables to call in API 
      # main = https://www.census.gov/data/developers/data-sets/acs-5year.2013.html
      # 2005-2009 - https://api.census.gov/data/2009/acs5/variables.html (2005-2009)
      # 2006-2010 - https://api.census.gov/data/2010/acs/acs5/variables.html
      # 2007-2011 - https://api.census.gov/data/2011/acs/acs5/variables.html
      # 2008-2012 - https://api.census.gov/data/2012/acs/acs5/variables.html
      # 2013-2017 - https://api.census.gov/data/2017/acs/acs5/variables.html

# Variables to use
      # NAME - Name of Census Tract
      # B01003_001E - Total Population of Census Tract
      # B04004_051E - People Reporting Single Ancestry (Full)
      # B04005_051E - People Reporting Multiple Ancestry (Partial)
      # B04006_051E - People Reporting Ancestry (Total)

# Things to know 
      # You can compare non overlapping ACS datasets
      # Can also compare census estimates to ACS, but can only use 1980,1990, and 2000 census bc
          # it was a short form census in 2010
      # Comparing ACS to 2000 Census may be harder than expected :( 

# Statistical signficance of ACS Estimates
  # - https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch07.pdf
      #  Statistical significance means that there is strong statistical evidence 
      #  that a true difference exists within the full population
      #  SE =MOE/1.645
      #  1. Calculate the SEs for both estimates 
      #  2. Square the resulting SE for each estimate
      #  3. Sum the squared SEs
      #  4. Calculate the square root of the sum of the squared SE
      #  5. Divde the difference between the two ACS estimates by the square root of the sum of the squared SEs
      #  6. Compare abs value of this calculation to the critical value for level of confidence. 
      #  7. If abs value is greater than crit value than the difference between the two estimates is stat significant
      #   abs(est1 - est2/sqrt(SE(x1)^2 +SE(x2)^2)) > Zcl
      

get_italian_ny_est <- function(acs_nm, yr){
  est <- getCensus(name = acs_nm,
            vintage = yr,
            vars = c("NAME", 
                     "B01003_001E", "B01003_001M",
                     "B04006_051E", "B04006_051M"),
            region = "tract:*",
            regionin = "state:36")
  return(est)
}

change_names <- function(x,y){
  colhds<- c("TotalPop", "TotalPopMOE", "TR_Italian", "TR_ItalianMOE")
  names(x)[5:8] <- colhds
  names(x)[5:8] <- paste(names(x)[5:8], y, sep = "_")
  return(x)
}

create_census_cols <- function(x){
  list_names <- stringr::str_split(string = x[,"NAME"], ",")
  list_namesdf <- lapply(list_names, function(k) data.frame(as.list(k)))
  list_namesdf <- lapply(list_namesdf, setNames, nm = c("Census_Tract", "County", "State"))
  list_namesdf <- plyr::rbind.fill(list_namesdf)
  x <- cbind(x, list_namesdf)
  x[9:11] <- apply(x[9:11], 2, as.character)
  x[9:11] <- apply(x[9:11], 2, trimws)
  return(x)
}
  
  
# get estimates   
est_2008_2012 <- get_italian_ny_est("acs/acs5", 2012)
est_2013_2017 <- get_italian_ny_est("acs/acs5", 2017)

# change column name estimates 
est_2008_2012 <- change_names(est_2008_2012, "12")
est_2013_2017 <- change_names(est_2013_2017, "17")

# create new columns 
est_2008_2012 <- create_census_cols(est_2008_2012)
est_2013_2017 <- create_census_cols(est_2013_2017)
  
# filter both datasets for NYC counties 
est_2008_2012 <- est_2008_2012 %>%
                  filter(County %in% c("Bronx County", "Kings County",
                                             "New York County", "Queens County",
                                             "Richmond County"))
est_2013_2017 <- est_2013_2017 %>%
                  filter(County %in% c("Bronx County", "Kings County",
                                       "New York County", "Queens County",
                                       "Richmond County"))

# join two datasets together by state, county, tract 
est_2008_2017 <-left_join(est_2008_2012, est_2013_2017, by = c("state", "county", "tract"))
  
  # delete duplicate columns
  est_2008_2017<-est_2008_2017[,-c(12,17:19)]

# write full dataset to csv 
  write_csv(est_2008_2017, "it_nyc_2008_2017.csv")



