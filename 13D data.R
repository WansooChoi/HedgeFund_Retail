# setup
#library(plm)
library(dplyr)
#library(purrr)
#library(plyr) do not use plyr with dplyr
library(tidyverse)
#library(tidyr)
#library(readr)
library(data.table)
# library(DataCombine)
# library(functional)
# library(stringr)
# library(slider)
library(ggplot2)
#library(zoo)
#library(sandwich)
library(lubridate)
# require(foreign)
# require(lmtest)
# library(epiDisplay)
# library(vtable)
# library(skimr)
#library(scales)

#########################################
d13<-fread("C:/Users/user/Desktop/WhaleWisdom/13D 2010-2021.csv")
#colnames(d13)[21]<-"filer name"
names(d13)[names(d13) == 'cusip_number'] <- 'CUSIP'
d13<- d13%>%
  mutate(CUSIP=substr(CUSIP,1,8))
head(d13)

d13<-subset(d13,select=c(stock_id, filer_id, form_type, CUSIP, filed_as_of_date, event_date, HedgeFund))

#GET MARKET CAP FROM MILLI
MilliCRSP<-MilliCRSP%>%
  group_by(PERMNO)%>%
  mutate(MarketCap=PRC*SHROUT)

#use only things we need
MilliCRSP<-subset(MilliCRSP,select=c(PERMNO, DATE, TICKER, PRC, RET, CUSIP, MarketCap, mroibtrd, mroibvol))

#MERGE 13D AND MILLICRSP
d13<-d13%>%
  mutate(DATE=format(d13$event_date, "%Y%m%d"))

#make it numeric values
d13<-d13%>%
  mutate(DATE=as.numeric(d13$DATE))
#########################################
MilliCRSP13D$event_date[MilliCRSP13D$form_type == 'SC 13D/A'] <- NA

D13only<-d13[!d13$form_type== 'SC 13D/A']
              



