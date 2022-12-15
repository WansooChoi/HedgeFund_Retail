# setup
#library(plm)
#library(plyr) do not use plyr with dplyr
library(dplyr)
#library(purrr)
#library(tidyr)
library(tidyverse)
#library(sqldf)
#library(readr)
library(data.table)

# library(DataCombine)
# library(functional)
# library(stringr)
#library(slider)
library(ggplot2)
library(zoo)
#library(sandwich)
#library(lubridate)
# require(foreign)
# require(lmtest)
library(epiDisplay)
# library(vtable)
# library(skimr)
#library(scales)

#
#load following table
#SetofDataBeforeBuyHoldReturn


########################################################################################################################################################################
##NOV17 modification
sample<-subset(MilliCRSP13D_ACT,select=c(PERMNO,DATE,RET))
sample$RET<-as.numeric((sample$RET))
sample$DATE<-as.numeric((sample$DATE))

#####subtract Fama French mkt return.
FF<-fread("C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/FamaFrench.csv")
#date range 20100104 ~ 20211223
which(FF == 20100104, arr.ind=TRUE)
which(FF == 20211231, arr.ind=TRUE)
FF<-FF[22128:25148,]
head(FF)
tail(FF)
FF<-FF[,c(1,2,5)]
FF$`Mkt-RF`<-FF$`Mkt-RF`/100
FF$RF<-FF$RF/100
FF<-FF%>%mutate(Mkt=`Mkt-RF`+RF)
FF<-FF[,c(1,4)]
head(FF)
tail(FF)
FF$DATE<-as.numeric((FF$DATE))

#merge maret return in sample
sample[FF, on = c('DATE'), Mkt:=i.Mkt]
head(sample)

#get excess return
sample<-sample%>%mutate(mMkt=-Mkt)
sample<-sample%>%
  mutate(excret=rowSums(sample[,c(3,5)],na.rm = TRUE))
head(sample)

sample<-sample%>%mutate(logret=log(excret+1))
head(sample)
sample<-subset(sample,select = c(1,2,3,7))

rm(MilliCRSP13D_ACT_onlyNA,MilliCRSP13D_ACT_No_NA_HF,MilliCRSP13D_ACT_No_NA,MilliCRSP13D_ACT,FF)
#sample<-sample[,paste0("bhr",-14) := sum(colSums(sample[,5:(20-14)]))]

for(i in 15:-14) {
  sample<-sample[, paste0("dm",i) := shift(logret, i)]
}

for(i in -14:14) {
  sample[[paste0("bhr",i)]] <- rowSums(sample[, 5:(i+20)], na.rm = TRUE)
}

#### remember to change back to exp()-1
sample<-subset(sample,select = -c(5:34))
head(sample)

sample[,5:33]<-exp(sample[,5:33])-1

head(sample)
#now we have expanded the excess returns pre post 14 days for evey day.
#merge this back to Millicrsp13D_ACT_NO_NA
#load basefile first and then load sample file
rm(MilliCRSP13D_ACT_onlyNA,MilliCRSP13D_ACT_No_NA_HF,MilliCRSP13D_ACT)

setDT(MilliCRSP13D_ACT_No_NA)
setDT(sample)
MilliCRSP13D_ACT_No_NA$RET<-as.numeric(MilliCRSP13D_ACT_No_NA$RET)
MilliCRSP13D_ACT_No_NA<-MilliCRSP13D_ACT_No_NA%>%
  drop_na(RET)

MilliCRSP13D_ACT_No_NA[sample, on = .(PERMNO,DATE), names(sample)[5:33] := mget(paste0("i.", names(sample)[5:33]))]
head(MilliCRSP13D_ACT_No_NA)

rm(sample)

####PLOT####
plotsample<-MilliCRSP13D_ACT_No_NA[,24:52]
head(plotsample)
plotsample<-colMeans(plotsample) 
plotsample<-data.table(plotsample)
plotsample<-plotsample%>%
  mutate(order=-14:14)
head(plotsample)
colnames(plotsample)[1] = "average buy and hold abnormal return"

ggplot(data=plotsample, aes(x=order, y=`average buy and hold abnormal return`, group=1)) +
  geom_line()+
  geom_point()

















#ignore here#ignore here#ignore here#ignore here#ignore here#ignore here#ignore here
#trim the data
# Below need tidyr and dplyr exclusively
# head(MilliCRSP13D_ACT)
# 
# 
# list <- MilliCRSP13D_ACT %>%
#   filter(complete.cases(event_number)) %>%
#   rowwise %>%
#   mutate(order = list(-14:14), range = list(DATE + order))%>%
#   ungroup %>%
#   unnest(where(is.list)) %>%
#   subset(select=c(event_number,order,range))
# names(list)[names(list) == 'range'] <- 'DATE'
# 
# list<-list%>%mutate(helper="1")
# 
# MilliCRSP13D_ACT_test<-left_join(MilliCRSP13D_ACT,list,by=c('event_number','DATE'))
# MilliCRSP13D_ACT_test_No_NA<-MilliCRSP13D_ACT_test%>%
#   drop_na(helper)
#ignore here#ignore here#ignore here#ignore here#ignore here#ignore here
















#4968 13-d events confirmed.
144072/29

Milli_everything<-Milli_everything%>%
  mutate(group_number=rep(1:nrow(MilliCRSP13D_ACT_No_NA), each=29))
################################################################################################



#write.csv(Milli_everything_trimmed,"C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/Milli_everything_trimmed_OCT8.csv", row.names = FALSE )

#basic data table is formed. 
#now I need to calculate 
#1. buy and hold return  2. buy and hold excess return  3. sum before and after event_date order imbalance
########################################################################################
########################            RUN BELOW            ###############################
# ########################################################################################
# out<-fread("C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/Milli_everything_trimmed_OCT8.csv")
# 
# ########################################################################################
# # calculate buy and hold return.
# 
# #original out is pre 14:14 chopped data
# #out<-fread("C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/MilliCRSP13D_ACT_test_OCT8.csv")
# 
# out<-Milli_everything
# setDT(out)
# 
# events = unique(out[!is.na(event_date),.(PERMNO,event_date)])
# 
# #helper column
# events[, eDate:=event_date]
# 
# #makes new column(temporary) lower and upper boundary
# out[, `:=`(s=DATE-20, e=DATE+20)]
# 
# #non-equi match
# bhr = events[out, on=.(PERMNO, event_date>=s, event_date<=e), nomatch=0]
# 
# #Generate the BuyHoldReturn column, by ID and EventDate
# bhr = bhr[, .(DATE, BuyHoldReturn_I=c(NA, PRC[-1]/PRC[1] -1)), by = .(PERMNO,eDate)]
# 
# #merge back to get the full data
# #bhr = bhr[out,on=.(PERMNO,DATE),.(PERMNO, DATE, PRC, CUSIP, MarketCap, mroibtrd, mroibvol, stock_id, filer_id, form_type, event_date=i.event_date,BuyHoldReturn_I, aggregate_shares, HedgeFund, order)]
# out<-subset(out,select=-c(s,e))
# out<-left_join(out,bhr,by=c('PERMNO', 'DATE'))
# 
# out<-out[!duplicated(out),]
# out



















































#13D
names(d13)[names(d13) == 'cusip_number'] <- 'CUSIP'
d13<- d13%>%
  mutate(CUSIP=substr(CUSIP,1,8))
head(d13)

d13<-subset(d13,select=c(stock_id, filer_id, form_type, CUSIP, filed_as_of_date, event_date,aggregate_shares, HedgeFund))
d13<-d13[d13$form_type != 'SC 13D/A', ]

#MERGE 13D AND MILLICRSP
#first unify all the date related variables into yyyymmdd format
d13<-d13%>%
  mutate(DATE=format(d13$event_date, "%Y%m%d"))%>%
  mutate(filed_as_of_date=format(d13$filed_as_of_date, "%Y%m%d"))%>%
  mutate(event_date=format(d13$event_date, "%Y%m%d"))

d13<-d13%>%
  mutate(DATE=as.numeric(d13$DATE))%>%
  mutate(filed_as_of_date=as.numeric(d13$filed_as_of_date))%>%
  mutate(event_date=as.numeric(d13$event_date))

#MilliCRSP_ACT already has yyyymmdd format for all date related variables. only need to numerize it.
MilliCRSP_Act<-MilliCRSP_Act%>%
  mutate(DATE=as.numeric(MilliCRSP_Act$DATE))%>%
  mutate(filed_as_of_date=as.numeric(MilliCRSP_Act$filed_as_of_date))%>%
  mutate(event_date=as.numeric(MilliCRSP_Act$event_date))








#BETTER TO USE JOIN FUNCTIONS FROM DPLYR THAN THE MERGE.
# MilliCRSP13D_by<-left_join(MilliCRSP_Act,d13,by=c('CUSIP','DATE'))
# head(MilliCRSP13D_by)
# 
# MilliCRSP13D<-MilliCRSP_Act %>% left_join(d13)
# head(MilliCRSP13D)
# 
# MilliCRSP13D_by_No_NA1<-MilliCRSP13D_by%>%
#   drop_na(event_date.x)
# MilliCRSP13D_by_No_NA2<-MilliCRSP13D_by%>%
#   drop_na(event_date.y)

MilliCRSP13D<-left_join(MilliCRSP,d13,by=c('CUSIP','DATE'))
MilliCRSP13D_No_NA<-MilliCRSP13D%>%
  drop_na(event_date)
head(MilliCRSP13D_No_NA)


#write.csv(MilliCRSP13D,"C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/MilliCRSP13D_OCT2.csv", row.names = FALSE )

########################################################################################
########################            RUN BELOW            ###############################
########################################################################################

MilliCRSP13D<-fread("C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/MilliCRSP13D_OCT2.csv")

MilliCRSP13D2<-MilliCRSP13D%>%
  mutate(event_date.x=as.Date(as.character(event_date.x),format="%Y%m%d"))

# MilliCRSP13D2<-MilliCRSP13D%>%
#   mutate(DATE=format(as.Date(event_date.y,format="%Y%m%d")))

#format(as.Date(as.character(event_date.y),format="%Y%m%d"))


MilliCRSP13D2 <- MilliCRSP13D2%>%
  mutate(event_date=coalesce(event_date.x,event_date.y))


MilliCRSP13D2_Act_No_NA<-MilliCRSP13D2%>%
  drop_na(event_date.x)



coalesce(y, z)

df = data.frame(n, s, b) %>% 
  unite(x, c(n, s), sep = " ", remove = FALSE)




MilliCRSP13D<-left_join(MilliCRSP %>% group_by(PERMNO, DATE) %>% mutate(id = row_number()),
                        d13 %>% group_by(DATE) %>% mutate(id = row_number()), 
                        by = c("CUIP", "DATE"))




MilliCRSP13D<-MilliCRSP13D%>%
  mutate(event_date=format(event_date, "%Y%m%d"))
MilliCRSP13D<-MilliCRSP13D%>%
  mutate(event_date=as.Date(event_date, "%Y%m%d"))

MilliCRSP13D<-MilliCRSP13D%>%
  mutate(DATE=as.Date(as.character(DATE),format="%Y%m%d"))

rm(d13,MilliCRSP)
# gc()
#
# #if formtype is 13D/A, then event_date is NA.
MilliCRSP13D$event_date[MilliCRSP13D$form_type == 'SC 13D/A'] <- NA
#write.csv(MilliCRSP13D,"C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/MilliCRSP13D_OCT1.csv", row.names = FALSE )

MilliCRSP13D<-fread("C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/MilliCRSP13D_OCT1.csv")
MilliCRSP13D<-subset(MilliCRSP13D,select = -c(TICKER, RET,form_type, aggregate_shares,filer_id, filed_as_of_date))
MilliCRSP13D<-MilliCRSP13D[!duplicated(MilliCRSP13D),]

# Below need tidyr and dplyr exclusively
out <- MilliCRSP13D %>%
  mutate(rn = row_number()) %>%
  filter(complete.cases(event_date)) %>%
  rowwise %>%
  mutate(order = list(-14:14), rn = list(rn + order)) %>%
  ungroup %>%
  unnest(where(is.list)) %>%
  mutate(across(c("PERMNO", "DATE", "PRC", "CUSIP", "MarketCap", "mroibtrd", "mroibvol", "stock_id","event_date", "HedgeFund"),
                ~ MilliCRSP13D[[cur_column()]][rn])) %>%
  select(-rn) %>%
  mutate(event_date = case_when(order == 0 ~ event_date))

# #write.csv(out,"C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/out.csv", row.names = FALSE )

out<-fread("C:/Users/user/Desktop/HedgeFund_Retail_GitDeskTop/out.csv")

#Returnss
#want to calculate buy and hold return -20 ~ +20 days from event date.
#choose rows with no NA in event date and only show ID and event date
out<-subset(out,select=-c(form_type))
setDT(out)

events = unique(out[!is.na(event_date),.(PERMNO,event_date)])

#helper column
events[, eDate:=event_date]

#makes new column(temporary) lower and upper boundary
out[, `:=`(s=DATE-30, e=DATE+30)]

#non-equi match
bhr = events[out, on=.(PERMNO, event_date>=s, event_date<=e), nomatch=0]

#Generate the BuyHoldReturn column, by ID and EventDate
bhr = bhr[, .(DATE, BuyHoldReturn_I=c(NA, PRC[-1]/PRC[1] -1)), by = .(PERMNO,eDate)]

#merge back to get the ful data
bhr = bhr[out,on=.(PERMNO,DATE),.(PERMNO, DATE, PRC, CUSIP, MarketCap, mroibtrd, mroibvol, stock_id, filer_id, form_type, event_date=i.event_date,BuyHoldReturn_I, aggregate_shares, HedgeFund, order)]
out<-subset(out,select=-c(s,e))
out<-left_join(out,bhr,by=c('PERMNO', 'DATE'))
out<-out[!duplicated(out),]

#until here is good


















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






f