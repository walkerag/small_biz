###############################################
#Analyze US-level Census Business Patterns Data
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

#Codes from census metadata
#a	0 to 19 employees
#b	20 to 99 employees
#c	100 to 249 employees
#D	Withheld to avoid disclosing data for individual companies; data are included in higher level totals
#e	250 to 499 employees
#f	500 to 999 employees
#g	1,000 to 2,499 employees
#h	2,500 to 4,999 employees
#i	5,000 to 9,999 employees
#j	10,000 to 24,999 employees
#k	25,000 to 49,999 employees
#l	50,000 to 99,999 employees
#m	100,000 employees or more
#N	Not available or not comparable
#S	Withheld because estimate did not meet publication standards

#Census business panel data: country level
dat<-read.csv("/Users/walkerag/Documents/small_biz/BP_2014_00A2_with_ann.csv",stringsAsFactors = FALSE)

#First record contains descriptions, remove
dat<-dat[-1,]

#Data includes summaries by company type (corporation,partnership etc.)
unique(dat$LFO.display.label)
#Keep "all establishment" rollup only
dat<-dat[dat$LFO.id=="001",]

#Remove extraneous columns
dat<-subset(dat,select=-c(LFO.id,LFO.display.label,GEO.id2,GEO.id,GEO.display.label,YEAR.id))

#Record number of digits in NAICS code for later use
dat$NAICS.characters<-nchar(dat$NAICS.id)

#Summarize by employer size category
dat %>% group_by(EMPSZES.id,EMPSZES.display.label) %>% summarise(total=n())

#Create simpler employer size label for plotting
dat[dat$EMPSZES.id=="212","EMPSIZE"]<-"1 to 4"
dat[dat$EMPSZES.id=="220","EMPSIZE"]<-"5 to 9"
dat[dat$EMPSZES.id=="230","EMPSIZE"]<-"10 to 19"
dat[dat$EMPSZES.id=="241","EMPSIZE"]<-"20 to 49"
dat[dat$EMPSZES.id=="242","EMPSIZE"]<-"50 to 99"
dat[dat$EMPSZES.id=="251","EMPSIZE"]<-"100 to 249"
dat[dat$EMPSZES.id=="252","EMPSIZE"]<-"250 to 499"
dat[dat$EMPSZES.id=="254","EMPSIZE"]<-"500 to 999"
dat[dat$EMPSZES.id=="260","EMPSIZE"]<-"1000+"
dat$EMPSIZE<-factor(dat$EMPSIZE,levels=c("1 to 4","5 to 9","10 to 19","20 to 49","50 to 99","100 to 249","250 to 499","500 to 999","1000+"))

#Only keep data broken down by employer size
dat<-dat[dat$EMPSZES.id!="001",]

#Check for missing data, and flag based on category

#Data withheld due to sample size
dat$PAYANN_WITHHELD<-ifelse(dat$PAYANN=="D",1,0)
dat$ESTAB_WITHHELD<-ifelse(dat$ESTAB=="D",1,0)
dat$EMP_WITHHELD<-ifelse(dat$EMP=="D",1,0)

#Data unavailable or not comparable
dat$PAYANN_UNAVAILABLE<-ifelse(dat$PAYANN=="N",1,0)
dat$ESTAB_UNAVAILABLE<-ifelse(dat$ESTAB=="N",1,0)
dat$EMP_UNAVAILABLE<-ifelse(dat$EMP=="N",1,0)

#Data did not meet publication standards
dat$PAYANN_PROB<-ifelse(dat$PAYANN=="S",1,0)
dat$ESTAB_PROB<-ifelse(dat$ESTAB=="S",1,0)
dat$EMP_PROB<-ifelse(dat$EMP=="S",1,0)

dat$EMP_MISSING<-ifelse(dat$EMP %in% c("N", "b", "D","a","c","S","f","e","g","h","i","j","k","l"),1,0)
sum(dat$EMP_MISSING)

sum(dat$PAYANN_WITHHELD)
sum(dat$ESTAB_WITHHELD)
sum(dat$EMP_WITHHELD)
sum(dat$PAYANN_UNAVAILABLE)
sum(dat$ESTAB_UNAVAILABLE)
sum(dat$EMP_UNAVAILABLE)
sum(dat$PAYANN_PROB)
sum(dat$ESTAB_PROB)
sum(dat$EMP_PROB)

#Flag if field missing for any reason
dat$MISSING<-ifelse((dat$PAYANN_WITHHELD + dat$ESTAB_WITHHELD + dat$EMP_WITHHELD + dat$PAYANN_UNAVAILABLE + dat$ESTAB_UNAVAILABLE + dat$EMP_UNAVAILABLE + dat$PAYANN_PROB + dat$ESTAB_PROB + dat$EMP_PROB + dat$EMP_MISSING)>0,1,0)
sum(dat$MISSING)
#2213

#Interpolate employee size where necessary using codes provided
dat$EMP_INTERPOLATED<-as.numeric(dat$EMP)
dat[dat$EMP=="a","EMP_INTERPOLATED"]<-9.5
dat[dat$EMP=="b","EMP_INTERPOLATED"]<-59.5
dat[dat$EMP=="c","EMP_INTERPOLATED"]<-174.5
dat[dat$EMP=="e","EMP_INTERPOLATED"]<-374.5
dat[dat$EMP=="f","EMP_INTERPOLATED"]<-749.5
dat[dat$EMP=="g","EMP_INTERPOLATED"]<-1749.5
dat[dat$EMP=="h","EMP_INTERPOLATED"]<-3749.5
dat[dat$EMP=="i","EMP_INTERPOLATED"]<-7499.5
dat[dat$EMP=="j","EMP_INTERPOLATED"]<-17499.5
dat[dat$EMP=="k","EMP_INTERPOLATED"]<-37499.5
dat[dat$EMP=="l","EMP_INTERPOLATED"]<-74999.5
dat[dat$EMP=="m","EMP_INTERPOLATED"]<-125000

head(dat)

#################################
#Total US, all industries, broken down by employer size
#################################

#Keep all industry rollup only
total<-dat[dat$NAICS.id=="00",]

sum(total$MISSING)
#No missing data issues at this level of aggregation

#Make establishment count variable numeric
total$ESTAB_NUM<-as.numeric(total$ESTAB)
summary(total$ESTAB_NUM)

#Make annual pay numeric
total$PAYANN_NUM<-as.numeric(total$PAYANN)
summary(total$ESTAB_NUM)

#Percentage variables for employee count, payroll total, and establishment count
total$EMP_PERC<-total$EMP_INTERPOLATED/sum(total$EMP_INTERPOLATED)
total$PAYANN_PERC<-total$PAYANN_NUM/sum(total$PAYANN_NUM)
total$ESTAB_PERC<-total$ESTAB_NUM/sum(total$ESTAB_NUM)
head(total)

#Plot percentage totals by business size
total.pre<-subset(total,select=c(EMPSIZE,EMP_PERC,ESTAB_PERC,PAYANN_PERC))
total.plot <- melt(total.pre, id=c("EMPSIZE"))
head(total.plot)

#Create a nice metric variable for legend
total.plot[total.plot$variable=='EMP_PERC',"Metric"]<-"% of US Employees"
total.plot[total.plot$variable=='ESTAB_PERC',"Metric"]<-"% of US Businesses"
total.plot[total.plot$variable=='PAYANN_PERC',"Metric"]<-"% of US Payroll"

#TOTAL BAR CHART
ggplot(data=total.plot, aes(x=EMPSIZE, y=value, fill=Metric)) +
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  xlab("Number of Employees in Firm") +
  ylab("% of US Total") +
  ggtitle("% of Total US Employees, Businesses, and Payroll by Firm Size") +
  scale_y_continuous(labels=scales::percent,limits=c(0,0.6),breaks=c(0.1,0.2,0.3,0.4,0.5,0.6)) +
  theme(text = element_text(size = 13,family="Trebuchet MS")) + 
  theme(legend.title.align=0.5) 

#Prep data for table output
total.table<-total.pre
names(total.table)<-c("Employer Size","% of Employees","% of Companies","% of Pay")
total.table[,2]<-percent(total.table[,2])
total.table[,3]<-percent(total.table[,3])
total.table[,4]<-percent(total.table[,4])
formattable(total.table)

#Cleanup
rm(total.pre)
rm(total.plot)
rm(total.table)
rm(total)

#################################
#THREE DIGIT NAICS SUMMARY
#################################

#Remove total for all sectors
#Only three digit NAICS code
three<-dat[dat$NAICS.id!="00" & dat$NAICS.characters==3,]

three.plot<-subset(three,select=c(NAICS.display.label,EMPSIZE,EMP_INTERPOLATED))
head(three.plot)

#Create simpler business size categories
three.plot$Business_Size<-'Large'
three.plot[three.plot$EMPSIZE %in% c("20 to 49", "50 to 99", "100 to 249"),"Business_Size"]<-'Medium'
three.plot[three.plot$EMPSIZE %in% c("1 to 4", "5 to 9", "10 to 19"),"Business_Size"]<-'Small'

#Total employees by industry/business size
three_summ<-three.plot %>% group_by(NAICS.display.label, Business_Size) %>% summarise(
  total=n()
  ,total_employees=sum(EMP_INTERPOLATED)
)

#Percentage totals
three_summ <- three_summ %>% group_by(NAICS.display.label) %>% mutate(
  total_industry_employees=sum(total_employees)
  ,perc_industry_employees=total_employees/total_industry_employees
)

#Check NAs
three_summ[is.na(three_summ$total_employees),]

#Remove NA value industry
three_summ<-three_summ[three_summ$NAICS.display.label!="Nursing and residential care facilities",]
sum(three_summ$total_employees)

#Split out for plotting
three_small<-three_summ[three_summ$Business_Size=="Small",]
three_med<-three_summ[three_summ$Business_Size=="Medium",]
three_large<-three_summ[three_summ$Business_Size=="Large",]

#Small firms
ggplot(data=three_small, aes(x=perc_industry_employees, y=total_employees)) +
  geom_point(shape=19,alpha = 0.8,color="dodgerblue1") +
  xlab("% of Total Industry Employees") +
  ylab("Industry Employees in Small Firms") +
  ggtitle("Small (<20 Worker) Firms") +
  theme(text = element_text(size = 13,family="Trebuchet MS"))  + 
  geom_text_repel(data=filter(three_small, total_employees>2000000), aes(label=NAICS.display.label)) +
  scale_x_continuous(labels=scales::percent,limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                     ,limits=c(0,8000000),breaks=c(0,2000000,4000000,6000000,8000000))

#Mid-size firms
ggplot(data=three_med, aes(x=perc_industry_employees, y=total_employees)) +
  geom_point(shape=19,alpha = 0.8,color="darkorange") +
  xlab("% of Total Industry Employees") +
  ylab("Industry Employees in Mid-Size Firms") +
  ggtitle("Mid-Size (20-250 Worker) Firms") +
  theme(text = element_text(size = 13,family="Trebuchet MS"))  + 
  geom_text_repel(data=filter(three_med, total_employees>2000000), aes(label=NAICS.display.label)) +
  scale_x_continuous(labels=scales::percent,limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                     ,limits=c(0,8000000),breaks=c(0,2000000,4000000,6000000,8000000))

#Large firms
ggplot(data=three_large, aes(x=perc_industry_employees, y=total_employees)) +
  geom_point(shape=19,alpha = 0.8,color="darkgreen") +
  xlab("% of Total Industry Employees") +
  ylab("Industry Employees in Large Firms") +
  ggtitle("Large (>250 Worker) Firms") +
  theme(text = element_text(size = 13,family="Trebuchet MS"))  + 
  geom_text_repel(data=filter(three_large, total_employees>2000000), aes(label=NAICS.display.label)) +
  scale_x_continuous(labels=scales::percent,limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                     ,limits=c(0,8000000),breaks=c(0,2000000,4000000,6000000,8000000))

