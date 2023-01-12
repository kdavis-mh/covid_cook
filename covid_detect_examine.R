# set directory
setwd("~/anaconda3/envs/notebook/analysis/2022/0410_coviddetct")

# load libraries
library(tidyverse)
#library('readxl')
#library("RPostgreSQL")
options("scipen" = 10)

# OK, let's take a look at this measurement
#df <- read_csv("1_data/NWSS_Public_SARS-CoV-2_Wastewater_Metric_Data.csv")
# just grab illinois
#dfill <- df %>% filter(reporting_jurisdiction == "Illinois")
# now just cook county
#dfcook <- dfill %>% filter(county_names=="Cook")

# But wait, there's an API

#https://data.cdc.gov/resource/2ew6-ywp6.csv
#https://github.com/Chicago/RSocrata

#install.packages("RSocrata")
library(RSocrata)

gets <- 1

for (get in gets) {
  dfapi <- read.socrata("https://data.cdc.gov/resource/2ew6-ywp6.csv?wwtp_jurisdiction=Illinois")
  print(paste("gotten"))
}

# save this
write_csv(dfapi,"2_output/illinois_2022_04_18.csv",na="")

# now just cook county
dfcook <- dfapi %>% filter(county_names=="Cook")

#how many treatment plants? each one gets a unique number
dfplants <- as.data.frame( unique(dfcook$wwtp_id) )

# looks like they test in different places. How many?
dfsites <- as.data.frame( unique(dfcook$key_plot_id) )

#let's get a sense of it. count plants and the sample locations
dflocals <- dfcook %>% group_by(wwtp_id,sample_location) %>% 
  summarize(count=n())

# so one site - 635 - mostly samples before the treatment plant, otherwise at the treatment plant
# but these are less than the sites, so there are multiple sites going on before and at plants

# does sample_location_specify help?
dfspec <- dfcook %>% group_by(wwtp_id,sample_location,sample_location_specify) %>% 
  summarize(count=n())
# we get the same number as the key plot id. So that's the one to use

# one thing needed - a sewershed map

# let's get details of our locations
# and then add what we can about the location using the cdc map


dfinfo <- data.frame(wwtp_id = integer(),
                     reporting_jurisdiction = character(),
                     key_plot_id = character(),
                     population_served = integer(),
                     population_served = as.Date(character()),
                     stringsAsFactors = FALSE)


for (site in dfsites$`unique(dfcook$key_plot_id)`) {
  dfsel <- dfcook %>% select("wwtp_id","reporting_jurisdiction","key_plot_id","population_served","first_sample_date") %>% 
    filter(key_plot_id == site)
  # just want to check if population is consistent
  print(paste(site,unique(dfsel$population_served)))
  dfsel <- dfsel %>% slice(1)
  dfinfo <- rbind(dfinfo,dfsel)
}

write_csv(dfinfo,"2_output/sites_info.csv",na="")

# ok, really need a sewershed map

# looks like everyone was collecting data after 1/24, so let's filter on that

dfrecent <- dfcook %>% filter(date_end > "2022-01-24")

dfsitescount <- dfrecent %>% group_by(key_plot_id) %>% 
  summarize(count=n())

# so multiple samples a day in the main file, but only one per day via the API

# but a few have less than the others
# let's look at those

df982 <- dfrecent %>% filter(key_plot_id=="NWSS_il_982_Treatment plant_raw wastewater")
df982ch <- df982 %>% group_by(date_end) %>% 
  summarize(count=n())
# two samples a day

df640 <- dfrecent %>% filter(key_plot_id=="NWSS_il_640_Treatment plant_raw wastewater")
df640ch <- df640 %>% group_by(date_end) %>% 
  summarize(count=n())
# four samples a day

# compare to other sites
df675 <- dfrecent %>% filter(key_plot_id=="NWSS_il_675_Before treatment plant_17_raw wastewater")
# let's look at this one
write_csv(df675,"2_output/df675.csv",na="")
df675ch <- df675 %>% group_by(date_end) %>% 
  summarize(count=n())


df641 <- dfrecent %>% filter(key_plot_id=="NWSS_il_641_Treatment plant_raw wastewater")
df641ch <- df641 %>% group_by(date_end) %>% 
  summarize(count=n())

# both 6 samples a day, but with fewer most recent two days. some tests take longer?

# so are all sites usually sampled at least once a day? 
# Why don't we see that with the data via the API?

# and they don't seem to vary much in terms of results each day
# let's see what that looks like

#dflook <- dfrecent %>% filter(key_plot_id=="CDC_il_595_Treatment plant_raw wastewater")


# OK, let's see what we get

dftry <- dfrecent %>% group_by(key_plot_id,date_end) %>% 
  summarize(samples=n(),
            population= max(population_served),
            ptc_15d = median(ptc_15d,na.rm = TRUE),
            detect_prop_15d = median(detect_prop_15d,na.rm = TRUE),
            percentile = median(percentile,na.rm = TRUE)
            )

dftry$change <- ifelse(dftry$ptc_15d < 0, "decreasing", 
                  ifelse(dftry$ptc_15d > 0, "increasing","no change"))

write_csv(dftry,"2_output/dftry.csv",na="")


