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
MilliCRSP<-fread("C:/Users/user/Desktop/WhaleWisdom/MilliCRSP 2010-2021.csv")
#colnames(d13)[21]<-"filer name"
names(d13)[names(d13) == 'cusip_number'] <- 'CUSIP'
d13<- d13%>%
  mutate(CUSIP=substr(CUSIP,1,8))
head(d13)

d13<-subset(d13,select=c(stock_id, filer_id, form_type, CUSIP, filed_as_of_date, event_date, aggregate_shares,HedgeFund))                     
d13<-d13[!d13$form_type== 'SC 13D/A']

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

#BETTER TO USE JOIN FUNCTIONS FROM DPLYR THAN THE MERGE. 
MilliCRSP13D<-left_join(MilliCRSP,d13,by=c('CUSIP','DATE'))

MilliCRSP13D<-MilliCRSP13D%>%
  mutate(event_date=format(event_date, "%Y%m%d"))
MilliCRSP13D<-MilliCRSP13D%>%
  mutate(event_date=as.Date(event_date, "%Y%m%d"))

MilliCRSP13D<-MilliCRSP13D%>%
  mutate(DATE=as.Date(as.character(DATE),format="%Y%m%d"))


MilliCRSP13D<-subset(MilliCRSP13D,select=c(PERMNO, DATE,RET,CUSIP,stock_id, filer_id, event_date,MarketCap,aggregate_shares, mroibvol, HedgeFund))
MilliCRSP13D<-MilliCRSP13D[!duplicated(MilliCRSP13D),]

###################################################################################################
####################SAVE DATA AND RUN WITH FRESH WINDOW FOR RAM##########################
###################################################################################################
write.csv(MilliCRSP13D,"C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/MilliCRSP13D.csv", row.names = FALSE )
#MilliCRSP13D3<-fread("C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/MilliCRSP13D3.csv")



