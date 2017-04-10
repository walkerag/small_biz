###############################################
#Analyze US County-Level Elections Data
###############################################

rm(list=ls())
gc()

library(maptools)
library(sf)
library(tidyverse)
library(stringr)
library(ggmap)
require(gpclib)
library(knitr)
library(markdown)
library(reshape)
library(htmlTable)
library(formattable)
library(DT)
library(pander)
library(ggrepel)

#######################
#ELECTION DATA MAPS
#######################

#Read in election data
elections<-read.csv("/Users/walkerag/Documents/small_biz/US_County_Level_Presidential_Results_08-16.csv"
                    ,stringsAsFactors = FALSE)

#Read in area shape file:
area <- readShapePoly("/Users/walkerag/Documents/small_biz/cb_2016_us_county_500k.shp")

#Create percentage variables
elections$dem_2008_perc<-elections$dem_2008/elections$total_2008
elections$dem_2012_perc<-elections$dem_2012/elections$total_2012
elections$dem_2016_perc<-elections$dem_2016/elections$total_2016
elections$gop_2008_perc<-elections$gop_2008/elections$total_2008
elections$gop_2012_perc<-elections$gop_2012/elections$total_2012
elections$gop_2016_perc<-elections$gop_2016/elections$total_2016
elections$gop_12_16_change<-elections$gop_2016_perc-elections$gop_2012_perc

#Remove non-mainland area
area <- area[!area$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69", "64", "68", "70", "74",
                                  "81", "84", "86", "87", "89", "71", "76", "95", "79"),]

area_map <- fortify(area, region="GEOID")

#Pad FIPS to 5 characters
elections$fips_code<-str_pad(elections$fips_code, 5, pad = "0")

#Change Shannon (now Oglala) county ZIP
#Name change seems to have resulted in a new FIPS...
elections[elections$fips_code=="46113","fips_code"]<-"46102"
#https://en.wikipedia.org/wiki/Oglala_Lakota_County,_South_Dakota

#Plot election results
elections.plot<-subset(elections,select=c(fips_code,gop_2016_perc))
ggplot() +
  geom_map(data=area_map, map=area_map,
           aes(x=long, y=lat, map_id=id, group=group),size=0.15) +
  geom_map(data=elections.plot, map=area_map, aes_string(map_id="fips_code", fill=elections.plot$gop_2016_perc),
           size=0.15) +
  scale_fill_gradient2(low = muted("blue"), mid = "white",high = muted("red"), midpoint = 0.5, space = "Lab",
                       na.value = "grey50", guide = "colourbar",name="GOP Vote %") +
  coord_map('polyconic') +
  labs(title="2016 GOP Vote Percentage by County") + 
  theme(
    plot.title = element_text(hjust = 0.5,family="Trebuchet MS"),
    legend.position=c(0.9,0.3),
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    text = element_text(size = 13)
  )
