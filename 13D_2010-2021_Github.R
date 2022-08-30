# setup
library(plm)
library(dplyr)
#library(plyr) do not use plyr with dplyr
library(tidyverse)
library(readr)
library(data.table)
library(DataCombine)
library(functional)
library(stringr)
library(slider)
library(ggplot2)
library(zoo)
library(sandwich)
library(lubridate)
require(foreign)
require(lmtest)
library(sandwich)
#library(scales)

library(data.table)
getwd()


#read 13d 
setwd("C:/Users/user/Desktop/13d13g/records-2010-2021")
# First, identify the files you want:
fileDT = data.table(fn = list.files(pattern="csv$"))
#Read in the files as a list column:
fileDT[, contents := .(lapply(fn, fread))]
#Stack the 13DGs
dg13 <- fileDT[, rbindlist(setNames(contents, fn), idcol="filename")]
rm(fileDT)
dg13<-dg13[,-c(1,14)]

#only identify 13D
d13<-subset(dg13, substring(dg13$form_type,1,6)  == "SC 13D")
#observations drop from 324,718 to 62,336

#add descriptions for 13d
filers<-fread("C:/Users/user/Desktop/13d13g/filers.csv")
filers<-filers[,-c(3,4,5,6,7,8,10,12)]
colnames(filers)[1]<-"filer_id"
colnames(filers)[2]<-"filer_name"

d13<-merge(d13, filers, by='filer_id', all.x = TRUE)
rm(filers, dg13)

#I don't think reporting people isn ot necessary at this point.
#reportingpeople<-fread("C:/Users/user/Desktop/13d13g/reporting_people-2021.csv")

#get stock id file
stocks<-read.csv("C:/Users/user/Desktop/13d13g/stocks.csv")
colnames(stocks)[2]<-'stock name'
d13<-merge(x=d13, y=stocks, by.x='stock_id',by.y='id', all.x = TRUE)

rm(stocks)
##################################
#Read Investor Types
Investor_Types<-read.csv("C:/Users/user/Desktop/Prof Juha/Investor types 2022-07-15.csv")

#I see there are many firms without CIK. I will remove firms without CIK for now.
Investor_Types2 <- Investor_Types[!(is.na(Investor_Types$CIK)), ]
#Data shrinks from 92502 -> 48742 (43760)
Investor_Types<-Investor_Types2
rm(Investor_Types2)

# merge 13d to Investor Types
Complete_13D<-merge(x=d13, y=Investor_Types, by.x='cik', by.y='CIK')

rm(d13,Investor_Types)

#############################################
#make a new column to identify which are hedge funds
#13D
Complete_13D <- Complete_13D %>%
  mutate(HedgeFund = case_when(if_any(in_commercial_hedge_fund_databases:has_private_hedge_funds, ~ .x == 1) ~ "Yes", TRUE ~ "No"))
sum(Complete_13D$HedgeFund=='Yes')
#13,963 out of 62,223 observations in 13DG are Hedgefunds

#trim data
Complete_13D<-Complete_13D[,-c(24)]

#let's save
getwd()
setwd("C:/Users/user/Desktop/WhaleWisdom")
write.csv(Complete_13D,"C:/Users/user/Desktop/WhaleWisdom/13D 2010-2021.csv", row.names = FALSE )




























#############################################











#13D/G
#step1. merge 13dg to 13dgfiler
step1<-merge(x=dg13, y=dg13filers, by.x='filer_id', by.y='filer_id')

#step2.merge 13dg to dg13stocks
dg13<-merge(x=step1, y=dg13stocks, by.x='stock_id', by.y='stock_id')
rm(step1,dg13filers,dg13stocks)
gc()













#read description files for 13f
setwd("C:/Users/user/Desktop/13d13g/records-2010-2021")
temp = list.files(pattern="*.csv")
list2env(lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
                read.csv), envir = .GlobalEnv)





f13stocks<-stocks
f13filers<-filers
f13filings<-filings
rm(filers, filings, stocks)

#read 13f in 2021
setwd("C:/Users/user/Desktop/13f/21")
# First, identify the files you want:
fileDT = data.table(fn = list.files(pattern="csv$"))
#Read in the files as a list column:
fileDT[, contents := .(lapply(fn, fread))]
#Stack the quarterly 13f
f13 <- fileDT[, rbindlist(setNames(contents, fn), idcol="filename")]
rm(fileDT,other_managers,filing_report_sigs,stock_deletions,quarters,temp)

#read description files for 13dg
dg13filers<-read.csv("C:/Users/user/Desktop/13d13g/filers.csv")
dg13stocks<-read.csv("C:/Users/user/Desktop/13d13g/stocks.csv")
#read 13d/g
dg13<-read.csv("C:/Users/user/Desktop/13d13g/records-2021.csv")
gc()

#clean 13f
f13<-f13[,-c(1,2,5)]
f13<-as.data.table(f13)
colnames(f13)[3]<-"stock_name"
head(f13)
class(f13)

f13filings<-f13filings[,-c(4,8,9,10,11,12)]
f13filings<-as.data.table(f13filings)
class(f13filings)

f13filers<-f13filers[,-c(3,4,5,6,7,8,10,12)]
colnames(f13filers)[1]<-"filer_id"
colnames(f13filers)[2]<-"filer_name"
f13filers<-as.data.table(f13filers)
class(f13filers)

f13stocks<-f13stocks[,-c(4)]
colnames(f13stocks)[1]<-"stock_id"
colnames(f13stocks)[2]<-"stock_name"
f13stocks<-as.data.table(f13stocks)
class(f13stocks)

#clean 13dg
dg13<-dg13[,-c(1,6,7)]
dg13<-as.data.table(dg13)
class(dg13)

dg13filers<-dg13filers[,-c(3,4,5,6,7,8,10,12)]
colnames(dg13filers)[1]<-"filer_id"
colnames(dg13filers)[2]<-"filer_name"
dg13filers<-as.data.table(dg13filers)
class(dg13filers)

dg13stocks<-dg13stocks[,-c(4)]
colnames(dg13stocks)[1]<-"stock_id"
colnames(dg13stocks)[2]<-"stock_name"
dg13stocks<-as.data.table(dg13stocks)
class(dg13stocks)

#############################################
#13F
#step1. merge 13f to '13ffilings' to get the filer ID. (filings contain filer id)
step1<-merge(x=f13, y=f13filings, by.x ='filing_id',by.y='id')

#step2. merge step1 to 13ffilers
step2<-merge(x=step1, y=f13filers, by.x='filer_id', by.y='filer_id')

#step3. merge step2 to f13stocks
f13<-merge(x=step2, y=f13stocks, by.x='stock_id', by.y='stock_id')

rm(f13filers,f13filings,f13stocks,step1,step2)
gc()

#############################################
#13D/G
#step1. merge 13dg to 13dgfiler
step1<-merge(x=dg13, y=dg13filers, by.x='filer_id', by.y='filer_id')

#step2.merge 13dg to dg13stocks
dg13<-merge(x=step1, y=dg13stocks, by.x='stock_id', by.y='stock_id')
rm(step1,dg13filers,dg13stocks)
gc()

#############################################
#Both 13F and 13D/G contains CIK and filer ID. From both variables, we can know who is filing.
#Also we can link 13F and 13D/G to Investor Type data prof. Juha sent.

#Read Investor Types
Investor_Types<-read.csv("C:/Users/user/Desktop/Prof Juha/Investor types 2022-07-15.csv")

#I see there are mnay firms without CIK. I will remove firms without CIK for now.
Investor_Types2 <- Investor_Types[!(is.na(Investor_Types$CIK)), ]
#Data shrinks from 92502 -> 48742 (43760)
Investor_Types<-Investor_Types2
rm(Investor_Types2)

#step4. merge 13f to Investor Types
Complete_13F<-merge(x=f13, y=Investor_Types, by.x='cik', by.y='CIK')
#13F data shrinks from 10084564 -> 10055179 (-0.2%)
gc()

#step5. merge 13dg to Investor Types
Complete_13DG<-merge(x=dg13, y=Investor_Types, by.x='cik', by.y='CIK')
#13DG srhinks from 21901 -> 21881 (only 20 obs)

rm(f13, dg13,Investor_Types)

#############################################
#make a new column to identify which are hedge funds

#13F
library(dplyr)
head(Complete_13F)
Complete_13F <- Complete_13F %>%
  mutate(HedgeFund = case_when(if_any(in_commercial_hedge_fund_databases:has_private_hedge_funds, ~ .x == 1) ~ "Yes", TRUE ~ "No"))
sum(Complete_13F$HedgeFund=='Yes')
#332465 out of 10055179 observations in 13F are Hedgefunds

#13D/G
Complete_13DG <- Complete_13DG %>%
  mutate(HedgeFund = case_when(if_any(in_commercial_hedge_fund_databases:has_private_hedge_funds, ~ .x == 1) ~ "Yes", TRUE ~ "No"))
sum(Complete_13DG$HedgeFund=='Yes')
#8998 out of 21881 observations in 13DG are Hedgefunds


#let's save
getwd()
setwd("C:/Users/user/Desktop/WhaleWisdom")
write.csv(Complete_13F,"C:/Users/user/Desktop/WhaleWisdom/2021_13F_Investor.csv", row.names = FALSE )
write.csv(Complete_13DG,"C:/Users/user/Desktop/WhaleWisdom/2021_13DG_Investor.csv", row.names = FALSE )


#############################################


