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


# d13<-fread("C:/Users/user/Desktop/WhaleWisdom/13D 2010-2021.csv")
# MilliCRSP<-fread("C:/Users/user/Desktop/WhaleWisdom/MilliCRSP 2010-2021.csv")
# 
# ########################################################################################
# names(d13)[names(d13) == 'cusip_number'] <- 'CUSIP'
# d13<- d13%>%
#   mutate(CUSIP=substr(CUSIP,1,8))
# head(d13)
# 
# d13<-subset(d13,select=c(stock_id, filer_id, form_type, CUSIP, filed_as_of_date, event_date, HedgeFund))
# 
# #GET MARKET CAP FROM MILLI
# MilliCRSP<-MilliCRSP%>%
#   group_by(PERMNO)%>%
#   mutate(MarketCap=PRC*SHROUT)
# 
# #use only things we need
# MilliCRSP<-subset(MilliCRSP,select=c(PERMNO, DATE, TICKER, PRC, RET, CUSIP, MarketCap, mroibtrd, mroibvol))
# 
# #MERGE 13D AND MILLICRSP
# d13<-d13%>%
#   mutate(DATE=format(d13$event_date, "%Y%m%d"))
# 
# #make it numeric values
# d13<-d13%>%
#   mutate(DATE=as.numeric(d13$DATE))
# 
# #BETTER TO USE JOIN FUNCTIONS FROM DPLYR THAN THE MERGE. 
# MilliCRSP13D<-left_join(MilliCRSP,d13,by=c('CUSIP','DATE'))
# #WOW THIS IS WAY MUCH FASTER THAN MERGE FUNCTION !!!!
# 
# head(MilliCRSP13D)
# n_distinct(MilliCRSP13D$CUSIP)
# n_distinct(MilliCRSP$CUSIP)
# n_distinct(d13$CUSIP)
# 
# #Frequency Table
# tab1(MilliCRSP13D$HedgeFund, sort.group = "decreasing", cum.percent = TRUE)
# #only 16204/10912193 observations is announced in 13D by non-hedgefunds and 7661/10912193 is announced in 13D by hedgefunds
# #makes sense because 13D announcement is annual.
# 
# #Make a list of firms that are reported 13D (both hedgefund and non-hedgefund)
# Announced13D<-subset(MilliCRSP13D, HedgeFund=="Yes" | HedgeFund=="No")
# tab1(Announced13D$HedgeFund, sort.group = "decreasing", cum.percent = TRUE)
# #good.
# 
# #see how data looks like when we only use 13D (first report) i.e. without 13D/A (amendment)
# tab1(Announced13D$form_type, sort.group = "decreasing", cum.percent = TRUE)
# #There are 4993 13D (first report) and 18872 13D/A (amendment report)
# 
# MilliCRSP13D2<-MilliCRSP13D%>%
#   mutate(event_date=format(event_date, "%Y%m%d"))
# MilliCRSP13D2<-MilliCRSP13D2%>%
#   mutate(event_date=as.Date(event_date, "%Y%m%d"))
# 
# MilliCRSP13D2<-MilliCRSP13D2%>%
#   mutate(DATE=as.Date(as.character(DATE),format="%Y%m%d"))
# 
# rm(Announced13D,d13,MilliCRSP,MilliCRSP13D)
# gc()
# 
# #if formtype is 13D/A, then event_date is NA.
# MilliCRSP13D2$event_date[MilliCRSP13D2$form_type == 'SC 13D/A'] <- NA
# 
# #check if it worked by deleting rows if event_date is NA
# test<-subset(MilliCRSP13D2,!is.na(event_date))
# #looks good
# rm(test)
# 
# #we no longer use returns
# #MilliCRSP13D2$DATE<-as.character(MilliCRSP13D2$DATE)
# # #want to calculate buy and hold return -20 ~ +20 days from event date.
# # #choose rows with no NA in event date and only show ID and event date
# # MilliCRSP13D2<-subset(MilliCRSP13D2,select=c(DATE,PERMNO,event_date,PRC,mroibvol,MarketCap))
# # MilliCRSP13D2 <- MilliCRSP13D2 %>% mutate(across(c(DATE,event_date), ~as.Date(.x,"%Y%m%d")))
# # MilliCRSP13D2<-MilliCRSP13D2[!duplicated(MilliCRSP13D2),]
# # setDT(MilliCRSP13D2)
# # 
# # events = unique(MilliCRSP13D2[!is.na(event_date),.(PERMNO,event_date)])
# # 
# # #helper column
# # events[, eDate:=event_date]
# # 
# # #makes new column(temporary) lower and upper boundary
# # MilliCRSP13D2[, `:=`(s=DATE-20, e=DATE+20)]
# # 
# # #non-equi match
# # bhr = events[MilliCRSP13D2, on=.(PERMNO, event_date>=s, event_date<=e), nomatch=0]
# # 
# # #Generate the BuyHoldReturn column, by ID and EventDate
# # bhr = bhr[, .(DATE, BuyHoldReturn_I=c(NA, PRC[-1]/PRC[1] -1)), by = .(PERMNO,eDate)]
# # 
# # #merge back to get the ful data
# # bhr = bhr[MilliCRSP13D2,on=.(PERMNO,DATE),.(PERMNO,DATE,PRC,event_date=i.event_date,BuyHoldReturn_I,mroibvol,MarketCap)]
# # 
# # bhr2<-bhr%>%
# #   filter(!is.na(BuyHoldReturn_I))
# # 
# # #until here is good
# #   
# # bhr<-bhr%>%
# #   mutate(absmroibvol=abs(mroibvol))
# 
# #rm(Announced13D,d13,events,MilliCRSP,MilliCRSP13D,MilliCRSP13D2)
# ########################################################################################
# #in order to plot, make all the sequences visible 
# gc()
# 
# MilliCRSP13D3<-subset(MilliCRSP13D2,select=c(PERMNO, DATE, event_date,absmroibvol))
# MilliCRSP13D3<-MilliCRSP13D3[!duplicated(MilliCRSP13D3),]
# rm(MilliCRSP13D2)
# gc()

###################################################################################################
####################SAVE DATA AND RUN WITH FRESH WINDOW FOR RAM##########################
###################################################################################################
#write.csv(MilliCRSP13D3,"C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/MilliCRSP13D3.csv", row.names = FALSE )
MilliCRSP13D3<-fread("C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/MilliCRSP13D3.csv")


test<-subset(MilliCRSP13D3,!is.na(event_date))
#rm(test)


out <- MilliCRSP13D3 %>%
  mutate(rn = row_number()) %>% 
  filter(complete.cases(event_date)) %>% 
  rowwise %>%
  mutate(order = list(-20:20), rn = list(rn + order)) %>%
  ungroup %>%
  unnest(where(is.list)) %>% 
  mutate(across(c("DATE", "event_date", "absmroibvol"),
                ~ MilliCRSP13D3[[cur_column()]][rn])) %>% 
  select(-rn) %>% 
  mutate(event_date = case_when(order == 0 ~ event_date))


out<-out%>%
  mutate(group_number=rep(1:4015, each=41))







  
  



