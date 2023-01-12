# load libraries
library(tidyverse)
options("scipen" = 10)

# setwd("~/anaconda3/envs/notebook/analysis/2022/0410_coviddetct")
#https://data.cdc.gov/resource/2ew6-ywp6.csv
#https://github.com/Chicago/RSocrata

#install.packages("RSocrata")
library(RSocrata)

# little shortcut, if you have a bunch of commands you want to run
# within a script, put them in a for-loop with a single loop (runs one time)
gets <- 1
for (get in gets) {
  
  dfapi <- read.socrata("https://data.cdc.gov/resource/2ew6-ywp6.csv?wwtp_jurisdiction=Illinois")
  print(paste("gotten"))
  
  # filter for just cook county
  dfrecent <- dfapi %>% filter(date_end > "2022-01-24") %>% filter(county_names=="Cook")
  
  # count reports by site
  dfsitescount <- dfrecent %>% group_by(key_plot_id) %>% 
    summarize(count=n())
  
  
  # there's usually just one report in this dataset via API
  # but let's calculate medians just in case
  
  dftry <- dfrecent %>% group_by(key_plot_id,date_end) %>% 
    summarize(samples=n(),
              population= max(population_served),
              ptc_15d = median(ptc_15d,na.rm = TRUE),
              detect_prop_15d = median(detect_prop_15d,na.rm = TRUE),
              percentile = median(percentile,na.rm = TRUE)
    )
  
  # add plain language for the change
  dftry$change <- ifelse(dftry$ptc_15d < 0, "decreasing", 
                         ifelse(dftry$ptc_15d > 0, "INCREASING",
                         ifelse(dftry$ptc_15d == 0,"no change","missing")))
  dftry$change[is.na(dftry$change)] = "missing"

  
  # Get a count for each day of increasing, decreasing and no change
  # then save out the latest day with some columns suitable for readability
  
  # first filter for bad values
  dfres <- dftry %>% filter(!percentile=="999") %>% arrange(date_end)
  
  # now let's figure out counts by increasing or decreasing
  dates <- unique(dfres$date_end)
  spots <- unique(dfres$key_plot_id)
  
  dfch <- as.data.frame( table(dfres$change) )
  dfch$day <- "total"
  dfch <- pivot_wider(dfch,id_cols = c("day"),
                       names_from = "Var1",
                       values_from = c("Freq") )
  # count for each day
  for (day in dates) {
    print(paste(day))
    dfday <- dfres %>% filter(date_end == day)
    check <- as.data.frame( table(dfday$change) )
    check$day <- day
    check <- pivot_wider(check,id_cols = c("day"),
                        names_from = "Var1",
                        values_from = c("Freq") )
    dfch <- bind_rows(dfch,check)
    
  }
  
  # clean this up a bit
  dfch <- dfch %>% filter(!day=="total")
  colnames(dfch) <- c("day","decreasing","increasing","missing","no_change")
  dfch$total <- rowSums(dfch[2:5],na.rm=TRUE)
  # save that out
  url <-  paste("2_output/status_",day,".csv",sep="")
  write_csv(dfch,url,na="")

  # get latest data
  dflatest <- dfres %>% filter(date_end==day)
  
  # I want to get some plain language stuff out of the key_plot_id
  cdcspots <- c("CDC_il_682_Treatment plant_raw wastewater","CDC_il_683_Treatment plant_raw wastewater")
  
  dfcdc <- dflatest %>% filter(key_plot_id %in% cdcspots)
  dfcdc$id <- str_sub(dfcdc$key_plot_id,1,10)
  dfcdc$spot <- str_sub(dfcdc$key_plot_id,start=12)
  dfcdc$spot <- str_replace(dfcdc$spot,"_"," ")
  
  dfill <- dflatest %>% filter(!key_plot_id %in% cdcspots)
  dfill$id <- str_sub(dfill$key_plot_id,1,11)
  dfill$spot <- str_sub(dfill$key_plot_id,start=13)
  # these underscores are really annoying
  dfill$spot <- str_replace(dfill$spot,"_"," ")
  dfill$spot <- str_replace(dfill$spot,"plant_","plant ")
  dfill$spot <- str_replace(dfill$spot,"_raw"," raw")
  
  dflat <- rbind(dfcdc,dfill)
  dflat <- dflat %>%arrange(change,population)
  
  url2 <-  paste("2_output/latest_",day,".csv",sep="")
  write_csv(dflat,url2,na="")
  
  # and save the ILL data
  # save this
  url3 <-  paste("2_output/illinois/illinois_",day,".csv",sep="")
  write_csv(dfapi,url3,na="")
  print(paste("all done"))
}


# let's do a visual comparison of increasing vs decreasing

# first we need to pivot longer
colnames(dfch)

dfplot <- pivot_longer(dfch,
                       cols=c('decreasing','increasing','missing','no_change'),
                       names_to = 'status',
                       values_to = 'change'
                      )
dfplot$change <- replace_na(dfplot$change,0)


# let's load our theme 
source("3_notes/theme_mh_large.R")

summary(dfplot)

dfplot$day <- as.Date(dfplot$day)

plot <- ggplot(dfplot) +
  aes(x = day, 
      y = change,
      group = status,
      color=status) +
  geom_line(stat="identity", 
            size = 1)

plot

# now plot
plot <- ggplot(dfplot) +
  aes(x = day, 
      y = change,
      group = status,
      color=status) +
  geom_line(stat="identity", 
            size = 1) +
  # customizing our labels
  labs(title = paste("Number of sites reporting increase in COVID vs. decrease, as of ",day,sep=""),
       caption = "Source: CDC wastewater data",
       x = NULL,
       y = NULL,
       fill = NULL) +
  theme_mh_large() + # this is our theme, or styles
  theme(panel.grid.major.x = element_blank(),
        axis.ticks.x.bottom = element_line(size = .1),
        axis.ticks.length.x = unit(-.07, "cm"),
        legend.position = "top") +
  scale_colour_manual( values =c("#237a82", "#ae1b1f", "#7f919b", "#b4c8df"), name="" ) +
  scale_fill_manual(values = c("#237a82", "#ae1b1f", "#7f919b", "#b4c8df"), name="") +
  scale_x_date(date_breaks = "2 months")
  
plot

width <- 8
height <- 4
dev.new(width = width, height = height, unit = "in", noRStudioGD =T)
plot
ploturl <-  paste("2_output/plot_",day,".jpg",sep="")
ggsave(ploturl, plot, width = width, height = height)
