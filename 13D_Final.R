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
library(zoo)
#library(sandwich)
library(lubridate)
# require(foreign)
# require(lmtest)
library(epiDisplay)
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
# MilliCRSP13D<-fread("C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/MilliCRSP13D.csv")
# MilliCRSP13D<-MilliCRSP13D[!duplicated(MilliCRSP13D),]
# 
# test<-subset(MilliCRSP13D,!is.na(event_date))
# #rm(test)
# head(MilliCRSP13D3)
# 
# out <- MilliCRSP13D %>%
#   mutate(rn = row_number()) %>% 
#   filter(complete.cases(event_date)) %>% 
#   rowwise %>%
#   mutate(order = list(-14:14), rn = list(rn + order)) %>%
#   ungroup %>%
#   unnest(where(is.list)) %>% 
#   mutate(across(c("PERMNO", "DATE","RET","CUSIP", "event_date","MarketCap", "mroibvol","stock_id","aggregate_shares","filer_id", "HedgeFund"),
#                 ~ MilliCRSP13D[[cur_column()]][rn])) %>% 
#   select(-rn) %>% 
#   mutate(event_date = case_when(order == 0 ~ event_date))
# 
# 
# # out2 <- MilliCRSP13D3 %>%
# #   mutate(rn = row_number()) %>% 
# #   filter(complete.cases(event_date)) %>% 
# #   rowwise %>%
# #   mutate(order = list(-3:3), rn = list(rn + order)) %>%
# #   ungroup %>%
# #   unnest(where(is.list)) %>% 
# #   mutate(across(c("DATE", "event_date", "mroibvol"),
# #                 ~ MilliCRSP13D3[[cur_column()]][rn])) %>% 
# #   select(-rn) %>% 
# #   mutate(event_date = case_when(order == 0 ~ event_date))
# 
# out<-out%>%
#   mutate(group_number=rep(1:nrow(test), each=29))
# 
# write.csv(out,"C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/Sep17_13D.csv", row.names = FALSE )

#Simply Begin from here
output<-fread("C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/Sep17_13D.csv")

length(unique(output$group_number))
#there are 4634 13D events in this table.

#sum order imbalances of previous -14 and post 14 days around event date. only leave the -1:1 dates.
output <- output %>% 
  mutate(order_id = ifelse(order == 0, NA, order < 0)) %>%
  group_by(`group_number`, order_id) %>%
  mutate(sum_imbalance = ifelse(order == 0, NA, sum(mroibvol)))%>%
  filter(order %in% -1:1)

#for ease of computation next step, fill up the NA values on -1 and +1 rows from the eventdate.
output <- output %>% 
  group_by(group_number)%>%
  fill(c(stock_id, filer_id, event_date, aggregate_shares, HedgeFund), .direction = 'downup')

#remove rows on the eventdates
output<-output%>%
  drop_na(sum_imbalance)

#make a new column that indicates if imbalance is larger after the event date
output<-setDT(output)[, status := sum_imbalance[order_id=="FALSE"]>sum_imbalance[order_id=="TRUE"], by = group_number]

#make a new table called Himbalance(Higher Imbalance) that contains 13D event that is related to increase in retail order imbalance
Himbalance<-output%>%
  filter(status=="TRUE")

length(unique(Himbalance$group_number))
#there are 2198 

#make a new table called Limbalance(Lower Imbalance) that contains 13D event that is related to increase in retail order imbalance
Limbalance<-output%>%
  filter(status=="FALSE")

length(unique(Limbalance$group_number))
#there are 2436

#see what characteristics that had higher imbalance have
tab1(Himbalance$HedgeFund, sort.group = "decreasing", cum.percent = TRUE)
#Among Himbalance 1518=(3036/2) are not hedge fund. 680=(1360/2) are hedge fund.

tab1(Limbalance$HedgeFund, sort.group = "decreasing", cum.percent = TRUE)
#Among Limbalance 1667(3334/2) are not hedge fund. 769=(1538/2) are hedge fund.

1518/(1518+680)
#Among the 13D events that resulted in increased OIB, 69% of the funds are not hedgefund.
#31% are hedge fund. (Note that 68.7% of the events are not hedgefund so the ratio tells us it didn't matter whether it was hedge fund or not.)
1667/(1667+769)
#Among the 13D events that resulted in decreased OIB, 68.4% of the funds are not hedgefund.
#likewise, the ratio tells us it didn't matter whether it was hedge fund or not.

#compare AGG shares on average
mean(Himbalance$aggregate_shares)
mean(Limbalance$aggregate_shares)
#On average, Himbalance has higher aggregate shares

#compare Size on average
mean(Himbalance$MarketCap)
mean(Limbalance$MarketCap)
#on average, Himbalance has higher marketcap companies.(makes sense higher firms will be followed more)

#does hedge fund deals more with high marketcap firms?
Hedgefund<-output%>%
  filter(HedgeFund=="Yes")
length(unique(Hedgefund$group_number))
#1544 out of 4634 13D events are from hedge funds

mean(Hedgefund$MarketCap)
#firms that Hedgefund bought has marketcap similar to Limbalance. so Hedge fund doesn't necessarily deal with large firms.

###what are well known hedgefunds?
#how many different filers are there for the hedge fund 13D filing
length(unique(Hedgefund$filer_id))
#among the 1544 hedge fund 13D filing, there are 381 unique filers.

#########################################################STRANGE THING HAPPENED HERE#############################################
#import list of large hedgefund firms along with the filer Id 
Famous<-fread("C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/ListofHF.csv")

Famouslist<-pull(Famous,filer_id)
FamousHedgefund<-Hedgefund%>%
  filter(filer_id %in% Famouslist)
length(unique(FamousHedgefund$filer_id))
#only 2 hedge funds among 18?

FamousHimbalance<-Himbalance%>%
  filter(filer_id %in% Famouslist)
length(unique(FamousHimbalance$filer_id))

Famouslimbalance<-Limbalance%>%
  filter(filer_id %in% Famouslist)
length(unique(Famouslimbalance$filer_id))






 check<-output%>%
  filter(filer_id %in% Famouslist)
length(unique(check$filer_id))
#only 6 here. Apparently, 4 out of 6 is hedge fund but not regarded as hedgefund here.

#d13 file had all the filer_id in the list. why do I not all of those filings over here?
#let's see what those firms bought
d13<-fread("C:/Users/user/Desktop/WhaleWisdom/13D 2010-2021.csv")
FamousHedgefund13D<-d13%>%
  filter(filer_id %in% Famouslist)
length(unique(FamousHedgefund13D$filer_id))
#ok 18 here.
FamousHedgefund13D<-FamousHedgefund13D%>%
  filter(form_type=="SC 13D")
length(unique(FamousHedgefund13D$filer_id))
verify<-subset(FamousHedgefund13D,select=c(filer_id,filer_name,HedgeFund))
# 17 HERE.


#########################################################STRANGE THING HAPPENED#############################################









test<-output[1:58,]
a<-which(is.na(test$event_date) == FALSE)-14
b<-which(is.na(test$event_date) == FALSE)-1
c<-which(is.na(test$event_date) == FALSE)+1
d<-which(is.na(test$event_date) == FALSE)+14

test2<-test%>%
  group_by(group_number)%>%
  mutate(sumvalue=ifelse(order<0, sum(mroibvol[a:b], ifelse(order>0, sum(mroibvol[c:d],NA)))))

         
rowwise()%>%
         
         
         
         
         
test<-output[1:29,]
test
test$sums <- rollsumr(test$mroibvol, k = 3, fill = NA)
rowSums(test$mroibvol[event_date:167])








which(is.na(test$event_date) == FALSE)

a<-which(is.na(test$event_date) == FALSE)-14
b<-which(is.na(test$event_date) == FALSE)-0
c<-which(is.na(test$event_date) == FALSE)+14
test2<-test%>%
  mutate(previous)=rowSums(test$mroibvol[a:b]
  



rm(res)



