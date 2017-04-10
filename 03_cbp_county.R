###############################################
#Analyze US County-Level Elections Data and Census Business Patterns Data
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
library(extrafont)
library(sf)
library(scales)

#Load election data
elections<-read.csv("/Users/walkerag/Documents/small_biz/US_County_Level_Presidential_Results_08-16.csv"
                    ,stringsAsFactors = FALSE)

#Load business panel data
dat<-read.csv("/Users/walkerag/Documents/small_biz/BP_2014_00A3_with_ann.csv",stringsAsFactors = FALSE)
dat<-dat[-1,]

#Number of digits in NAICS
dat$NAICS.characters<-nchar(dat$NAICS.id)

#Remove extraneous columns
dat<-subset(dat,select=-c(GEO.id,YEAR.id))

#Summarize by employer size category
dat %>% group_by(EMPSZES.id,EMPSZES.display.label) %>% summarise(total=n())

#Create simpler employer size label for plots
dat[dat$EMPSZES.id=="212","EMPSIZE"]<-"1 to 4"
dat[dat$EMPSZES.id=="220","EMPSIZE"]<-"5 to 9"
dat[dat$EMPSZES.id=="230","EMPSIZE"]<-"10 to 19"
dat[dat$EMPSZES.id=="241","EMPSIZE"]<-"20 to 49"
dat[dat$EMPSZES.id=="242","EMPSIZE"]<-"50 to 99"
dat[dat$EMPSZES.id=="251","EMPSIZE"]<-"100 to 249"
dat[dat$EMPSZES.id=="252","EMPSIZE"]<-"250 to 499"
dat[dat$EMPSZES.id=="254","EMPSIZE"]<-"500 to 999"
dat[dat$EMPSZES.id=="260","EMPSIZE"]<-"1000+"
dat[dat$EMPSZES.id=="001","EMPSIZE"]<-"All"
dat$EMPSIZE<-factor(dat$EMPSIZE,levels=c("1 to 4","5 to 9","10 to 19","20 to 49","50 to 99","100 to 249","250 to 499","500 to 999","1000+","All"))

#Restrict to employer size breakdown 
dat<-dat[dat$EMPSZES.id!="001",]

#Sector rollup only 
dat<-dat[dat$NAICS.id=="00",]

dat$ESTAB_NUM<-as.numeric(dat$ESTAB)
summary(dat$ESTAB_NUM)

#QA
dat %>% group_by(EMPSIZE) %>% summarise(total=n(),biz=sum(ESTAB_NUM))

#Estimated employees, based on interpolation/extrapolation
dat[dat$EMPSZES.id=="212","EST_EMP"]<-2.5
dat[dat$EMPSZES.id=="220","EST_EMP"]<-7
dat[dat$EMPSZES.id=="230","EST_EMP"]<-14.5
dat[dat$EMPSZES.id=="241","EST_EMP"]<-34.5
dat[dat$EMPSZES.id=="242","EST_EMP"]<-74.5
dat[dat$EMPSZES.id=="251","EST_EMP"]<-174.5
dat[dat$EMPSZES.id=="252","EST_EMP"]<-374.5
dat[dat$EMPSZES.id=="254","EST_EMP"]<-749.5
dat[dat$EMPSZES.id=="260","EST_EMP"]<-2000

dat$EST_EMP_MULT<-dat$ESTAB_NUM*dat$EST_EMP

head(dat)

#Summarize employees per firm at the county level
county_summ<-dat %>% group_by(GEO.id2,GEO.display.label) %>% summarise(
  employee_per_company=sum(EST_EMP_MULT)/sum(ESTAB_NUM)
)

#Create election variables
#Haven't done much analysis with these yet- could be interesting
elections$dem_2008_perc<-elections$dem_2008/elections$total_2008
elections$dem_2012_perc<-elections$dem_2012/elections$total_2012
elections$dem_2016_perc<-elections$dem_2016/elections$total_2016
elections$gop_2008_perc<-elections$gop_2008/elections$total_2008
elections$gop_2012_perc<-elections$gop_2012/elections$total_2012
elections$gop_2016_perc<-elections$gop_2016/elections$total_2016
elections$gop_12_16_change<-elections$gop_2016_perc-elections$gop_2012_perc

#Pad FIPS to 5 characters
elections$GEO.id2<-str_pad(elections$fips_code, 5, pad = "0")

#Change Shannon (now Oglala) county ZIP
#Name change seems to have resulted in a new FIPS...
elections[elections$GEO.id2=="46113","GEO.id2"]<-"46102"
#https://en.wikipedia.org/wiki/Oglala_Lakota_County,_South_Dakota

#Merge county business data to elections data
comb<-merge(county_summ,elections,by='GEO.id2')

#Log votes for clearer plotting
comb$log_total_votes<-log(comb$total_2016)

#GOP Vote % vs Log(Total Votes)
ggplot(comb, aes(x=log(total_2016), y=gop_2016_perc)) +
  geom_point(shape=19,alpha = 0.65) +    # Use hollow circles
  geom_smooth(method=lm,color="red") +   
  xlab("Log(Total 2016 Votes)") +
  ylab("GOP Vote Percentage") +
  ggtitle(paste0("GOP Vote Percentage by Log(Total Votes)")) +
  scale_y_continuous(labels=scales::percent,limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  theme(text = element_text(size = 13,family="Trebuchet MS"))

#Employees per Firm vs Log(Total Votes)
ggplot(comb, aes(x=log(total_2016), y=employee_per_company)) +
  geom_point(shape=19,alpha = 0.65) +    # Use hollow circles
  geom_smooth(method=lm,color="darkgreen") +   
  xlab("Log(Total 2016 Votes)") +
  ylab("Employees per Firm") +
  ggtitle(paste0("Employees per Firm by Log(Total Votes)")) +
  theme(text = element_text(size = 13,family="Trebuchet MS"))

#Flag winner of states
comb$Winner<-'Republican'
comb[comb$dem_2016_perc>comb$gop_2016_perc,"Winner"]<-"Democrat"

#Employees per Firm vs Log(Total Votes), by state winner
ggplot(data=comb, aes(x=log_total_votes, y=employee_per_company,color=Winner)) +
  geom_point(shape=19,alpha = 0.35) +
  xlab("Log(Total 2016 Votes)") +
  ylab("Employees per Firm") +
  ggtitle("Estimated Employees per Firm by Log(Total Votes), Election Winner") +
  theme(text = element_text(size = 13,family="Trebuchet MS")) + 
  theme(legend.title.align=0.5) +
  geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE,    # Don't add shaded confidence region
              fullrange=TRUE,show_guide=FALSE) +
  scale_y_continuous(limits=c(0,70),breaks=c(0,15,30,45,60)) +
  scale_color_manual(values=c("dodgerblue2", "firebrick1"))




