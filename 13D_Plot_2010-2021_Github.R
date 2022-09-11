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
library(epiDisplay)
library(vtable)
library(skimr)
#library(scales)

#ok github good to go.
#I will use github desk top instead of git or git in r because
#github desktop is easiest at this moment

d13<-fread("C:/Users/user/Desktop/WhaleWisdom/13D 2010-2021.csv")
MilliCRSP<-fread("C:/Users/user/Desktop/WhaleWisdom/MilliCRSP 2010-2021.csv")

########################################################################################
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

#BETTER TO USE JOIN FUNCTIONS FROM DPLYR THAN THE MERGE. 
MilliCRSP13D<-left_join(MilliCRSP,d13,by=c('CUSIP','DATE'))
#WOW THIS IS WAY MUCH FASTER THAN MERGE FUNCTION !!!!

head(MilliCRSP13D)
n_distinct(MilliCRSP13D$CUSIP)
n_distinct(MilliCRSP$CUSIP)
n_distinct(d13$CUSIP)

#Frequency Table
tab1(MilliCRSP13D$HedgeFund, sort.group = "decreasing", cum.percent = TRUE)
#only 16204/10912193 observations is announced in 13D by non-hedgefunds and 7661/10912193 is announced in 13D by hedgefunds
#makes sense because 13D announcement is annual.

#Make a list of firms that are reported 13D (both hedgefund and non-hedgefund)
Announced13D<-subset(MilliCRSP13D, HedgeFund=="Yes" | HedgeFund=="No")
tab1(Announced13D$HedgeFund, sort.group = "decreasing", cum.percent = TRUE)
#good.

#see how data looks like when we only use 13D (first report) i.e. without 13D/A (amendment)
tab1(Announced13D$form_type, sort.group = "decreasing", cum.percent = TRUE)
#There are 4993 13D (first report) and 18872 13D/A (amendment report)

#ignore 13D/A by deleting values for eventdate when formtype= SC 13D/A
MilliCRSP13D$DATE<-as.character(MilliCRSP13D$DATE)
MilliCRSP13D<-MilliCRSP13D%>%
  mutate(DATE=format(DATE, "%Y%m%d"))
MilliCRSP13D<-MilliCRSP13D%>%
  mutate(event_date=format(event_date, "%Y%m%d"))

# MilliCRSP13D$form_type<-as.character(MilliCRSP13D$form_type)
MilliCRSP13D2<-MilliCRSP13D%>%
  mutate(event_date=ifelse(form_type=="SC 13D/A",NA,event_date))
#check if it worked by deleting rows if event_date is NA
test<-subset(MilliCRSP13D2,!is.na(event_date))
#looks good
rm(test)

#want to calculate buy and hold return -20 ~ +20 days from event date.
#choose rows with no NA in event date and only show ID and event date
MilliCRSP13D2<-subset(MilliCRSP13D2,select=c(DATE,PERMNO,event_date,PRC,mroibvol,MarketCap))
MilliCRSP13D2 <- MilliCRSP13D2 %>% mutate(across(c(DATE,event_date), ~as.Date(.x,"%Y%m%d")))

setDT(MilliCRSP13D2)

events = unique(MilliCRSP13D2[!is.na(event_date),.(PERMNO,event_date)])

#helper column
events[, eDate:=event_date]

#makes new column(temporary) lower and upper boundary
MilliCRSP13D2[, `:=`(s=DATE-20, e=DATE+20)]

#non-equi match
bhr = events[MilliCRSP13D2, on=.(PERMNO, event_date>=s, event_date<=e), nomatch=0]

#Generate the BuyHoldReturn column, by ID and EventDate
bhr = bhr[, .(DATE, BuyHoldReturn_Mkt=c(NA, PRC[-1]/PRC[1] -1)), by = .(PERMNO,eDate)]

#merge back to get the ful data
bhr = bhr[MilliCRSP13D2,on=.(PERMNO,DATE),.(PERMNO,DATE,PRC,event_date=i.event_date,BuyHoldReturn_Mkt,mroibvol,MarketCap)]

bhr2<-bhr%>%
  filter(!is.na(BuyHoldReturn_Mkt))

#until here is good


########################################################################
#Market return to calculate excess market
#weighted price
bhr<-bhr2
#rm(bhr2,Announced13D,MilliCRSP13D, d13,MilliCRSP,events)
bhr<-bhr%>%
  mutate(wprice=weighted.mean(PRC,MarketCap))




#non-equi match
imb = events[MilliCRSP13D2, on=.(PERMNO, event_date>=s, event_date<=e), nomatch=0]

#Generate the BuyHoldReturn column, by ID and EventDate
imb = imb[, .(DATE, Imbalance=c(NA, mroibvol)), by = .(PERMNO,eDate)]

#merge back to get the full data
bhr = bhr[MilliCRSP13D2,on=.(PERMNO,DATE),.(PERMNO,DATE,PRC,event_date=i.event_date,BuyHoldReturn_Mkt)]

imb = imb[, .(DATE, BuyHoldReturn_Mkt=c(NA, PRC[-1]/PRC[1] -1)), by = .(PERMNO,eDate)]







##########################################################################################
#choose rows with no NA in event date and only show ID and event date
events = unique(MilliCRSP13D2[!is.na(event_date),.(PERMNO,event_date)])
events2 = unique(MilliCRSP13D2[!is.na(event_date)])

#helper column
events[, eDate:=event_date]

#makes new column(temporary) lower and upper boundary
MilliCRSP13D2[, `:=`(s=DATE-6, e=DATE+6)]

#non-equi match
bhr = events[MilliCRSP13D2, on=.(PERMNO, event_date>=s, event_date<=e), nomatch=0]

#Generate the BuyHoldReturn column, by ID and EventDate
bhr = bhr[, .(DATE, BuyHoldReturn_Mkt=c(NA, PRC[-1]/PRC[1] -1)), by = .(PERMNO,eDate)]

#merge back to get the full data
bhr = bhr[MilliCRSP13D2,on=.(PERMNO,DATE),.(PERMNO,DATE,PRC,event_date=i.event_date,BuyHoldReturn_Mkt)]

############################################################################################################


setDT(MilliCRSP13D)
MilliCRSP13D[,(c("DATE", "event_date")):=lapply(.SD, as.Date), .SDcols=c("DATE", "event_date")]


MilliCRSP13D2<-MilliCRSP13D[,!c("event_date")][unique(MilliCRSP13D[!is.na(event_date), .(PERMNO, event_date)]), on="PERMNO", allow.cartesian=T][
  abs(event_date-DATE)<=35][,BuyHoldReturn:=c(NA,PRC[-1]/PRC[1]-1), .(PERMNO)][
    MilliCRSP13D[,.(PERMNO,DATE)], on=.(PERMNO,DATE)]












length(unique(MilliCRSP$PERMNO))
length(unique(MilliCRSP13D$PERMNO))
length(unique(MilliCRSP13D2$PERMNO))
test<-na.omit(MilliCRSP13D2)
length(unique(test$PERMNO))

#calculate buy hold return for value-weight market around event date
MilliCRSP13D3<-subset(MilliCRSP13D,select=c(DATE,PERMNO,PRC,event_date,MarketCap))
MilliCRSP13D3<-left_join(
  dplyr::select(MilliCRSP13D3,PERMNO, DATE), 
  inner_join(MilliCRSP13D3 %>% dplyr::select(-event_date),filter(MilliCRSP13D3,!is.na(event_date)) %>% distinct(PERMNO, event_date), by="PERMNO") %>%
    filter(abs(event_date-DATE)<=40) %>% 
    group_by(PERMNO, event_date) %>% 
    mutate(BuyHoldReturn_m = c(NA,weighted.mean(PRC[-1]/PRC[1]-1),MarketCap,na.rm=TRUE)),
  by=c("PERMNO", "DATE")
)



CleanSample <- CLEAN13DHF %>%
  group_by(DATE) %>%
  summarize(imbal_vw = weighted.mean(mroibvol,MarketCap, na.rm=TRUE), 
            ret_vw=weighted.mean(ExcRet,MarketCap, na.rm=TRUE))%>%
  ungroup()

df<-df%>%
  group_by(DATE)%>%
  mutate(wret=weighted.mean(RET,MarketCap, na.rm=TRUE))



setDT(MilliCRSP13D)
MilliCRSP13D[,(c("DATE", "event_date")):=lapply(.SD, as.Date), .SDcols=c("DATE", "event_date")]
MilliCRSP13D2<-MilliCRSP13D[,!c("event_date")][unique(MilliCRSP13D[!is.na(event_date), .(PERMNO, event_date)]), on="PERMNO", allow.cartesian=T][
  abs(event_date-DATE)<=35][,BuyHoldReturn:=c(NA,PRC[-1]/PRC[1]-1), .(PERMNO)][
    MilliCRSP13D[,.(PERMNO,DATE)], on=.(PERMNO,DATE)]








####################################################################################################################
MilliCRSP13D2 |> group_by(PERMNO) |> mutate( x = PRC/lag(PRC) - 1 ,
                              y = which(DATE == event_date) - 1:n() ,
                              BuyHoldReturn = case_when(between(y , -5 , 5) ~ x , TRUE ~ NA_real_))



MilliCRSP13D %>%
  group_by(PERMNO) %>%
  mutate(x = PRC/lag(PRC) - 1, y = which(DATE == event_date) - 1:n() , BuyHoldReturn = case_when(between(y , -5 , 5) ~ x , TRUE ~ NA_real_)) 
# %>%
#   select(-x , -y)



MilliCRSP13D |> group_by(PERMNO) |> mutate(x = PRC/lag(PRC)-1, y = which(DATE == event_date)-1:n(), BuyHoldReturn = case_when(between(y , -5 , 5) ~ x, TRUE ~ NA_real_))
#|> select(-x , -y)

MilliCRSP13D <- MilliCRSP13D %>%
  group_by(PERMNO) %>% 
  mutate(x = PRC/lag(PRC)-1, y = which(DATE == event_date)-1:n(), BuyHoldReturn = case_when(between(y , -5 , 5) ~ x, TRUE ~ NA_real_)) %>%
  select(-x , -y)




MilliCRSP13D<-MilliCRSP13D%>%
  group_by(PERMNO)%>%
  mutate(BHret=ifelse(DATE==event_date,ifelse(ExcRet>0,"Winner","Loser"),"NA"))
#want to calculate buy and hold return of the value-weight market






df<-MilliCRSP

df<-df%>%
  mutate(DATE=as.numeric(DATE),RET=as.numeric(RET))

head(df)

df<-df%>%
  group_by(DATE)%>%
  mutate(wret=weighted.mean(RET,MarketCap, na.rm=TRUE))

#leftjoin this to MillCRSP13D
head(df)
df<-subset(df,select=c(PERMNO, DATE, wret))
rm(MilliCRSP)
MilliCRSP13D<-left_join(MilliCRSP13D,df,by=c('PERMNO','DATE'))

#now we have the weighted average market return.
head(MilliCRSP13D)

#also calculate excess return
MilliCRSP13D<-MilliCRSP13D%>%
  mutate(RET=as.numeric(RET))%>%
  mutate(ExcRet=RET-wret)

#want to know the statistics of excess return
ExcessReturns<-as.data.table(MilliCRSP13D$ExcRet)
st(MilliCRSP13D, vars = 'ExcRet')
summary(ExcessReturns)
skim(ExcessReturns)

#also want to know the statistics of size and add new column that indicates large medium small
threshold<-as.data.frame(quantile(MilliCRSP13D$MarketCap, c(.33, .66, .99)) )
MilliCRSP13D<-MilliCRSP13D%>%
  mutate(Size=ifelse(MarketCap < threshold[1,], "Small", ifelse(MarketCap < threshold[2,],"Medium","Large")))

MilliCRSP13D<-subset(MilliCRSP13D,select=-c(MarketCap))

#see who has positive alpha (if excess return is higher than zero on the event date)
MilliCRSP13D$DATE<-as.character(MilliCRSP13D$DATE)
MilliCRSP13D<-MilliCRSP13D%>%
  mutate(DATE=as.Date(DATE, "%Y%m%d"))

MilliCRSP13D<-MilliCRSP13D%>%
  mutate(event_date=as.Date(event_date, "%Y%m%d"))

###############################################

MilliCRSP13D<-MilliCRSP13D%>%
  mutate(Winners=ifelse(DATE==event_date,ifelse(ExcRet>0,"Winner","Loser"),"NA"))

rm(Announced13D,d13,df,ExcessReturns,ReturnStat,threshold)

Winners<-MilliCRSP13D%>%
  filter(Winners=="Winner")
Winners<-subset(Winners,select = c())

#distribution of marketcapital of the firm with positive alpha
tab1(Winners$Size, sort.group = "decreasing", cum.percent = TRUE)
tab1(MilliCRSP13D$Size, sort.group = "decreasing", cum.percent = TRUE)
#better returns for small firms.

#distribution of Hedgefund of the firm with positive alpha
tab1(Winners$HedgeFund, sort.group = "decreasing", cum.percent = TRUE)
tab1(MilliCRSP13D$HedgeFund, sort.group = "decreasing", cum.percent = TRUE)
#Hedge fund that won -> No =11,821  Yes= 4,835
#Hedge fund in 13D -> No=22,212  Yes=9,411
11821/22212
4835/9411
#about 53% of non-hedge funds and 51% of hedge funds made positive alpha

tab1(Winners$form_type, sort.group = "decreasing", cum.percent = TRUE)
#Among the ones with positive alpha, there are more amendments (13D/A) 
#than the new notice 12,137 vs 4,519
tab1(MilliCRSP13D$form_type, sort.group = "decreasing", cum.percent = TRUE)
#Among the entire data, 7,971 are new notice (13D) and 23,652 are amendments (13D/A)
12137/23652
4519/7971
#51% of amendments have positive excess return and 56.7% of new notice have positive excess return








.#figure out way to plot efficienty...
#######################################try plotting averages############################################
#####################################################################################################################
#####################################################################################################################

EventDate<-mean(na.omit(CLEAN13DHF$D13_InfoReleaseDate))
#average 13D info release date is "2021-03-01"

CleanSample <- CLEAN13DHF %>%
  group_by(DATE) %>%
  summarize(imbal_vw = weighted.mean(mroibvol,MarketCap, na.rm=TRUE), 
            ret_vw=weighted.mean(ExcRet,MarketCap, na.rm=TRUE))%>%
  ungroup()

CleanSample$DATE<-as.character(CleanSample$DATE)
CleanSample<-CleanSample%>%
  mutate(DATE=as.Date(DATE, "%Y%m%d"))

which(CleanSample$DATE=="2021-03-01")-20
CleanSample2<-CleanSample[(which(CleanSample$DATE=="2021-03-01")-15):(which(CleanSample$DATE=="2021-03-01")+15),]
CleanSample2%>%
  ggplot(aes(difftime(DATE, EventDate,units = "days"))) + geom_line(aes(y=imbal_vw, colour = "Order Imbalance")) + geom_line(aes(y=ret_vw, colour = "Return")) + ggtitle(paste("Average Order Imbalance and Return (All Firms)")) + xlab("Date") + ylab("")
setwd("C:/Users/user/Desktop/WhaleWisdom/Plots")
ggsave(filename=paste("20220823 Average Order Imbalance and Return (13D).jpg"), height=7,width=20,dpi = 320)


#######################################plot averages with small firms############################################
#####################################################################################################################
#####################################################################################################################

#define small? let's just use smallest 40%...






















############################################TEST############################################

CleanSample<-subset(CleanSample, PERMNO  == List$PERMNO[1])
CleanSample$DATE<-as.character(CleanSample$DATE)
CleanSample<-CleanSample%>%
  mutate(DATE=as.Date(DATE, "%Y%m%d"))
na.omit(CleanSample$D13_InfoReleaseDate)+15
na.omit(CleanSample$D13_InfoReleaseDate)-15













WholeList<-as.data.table(unique(CLEAN13DHF2$PERMNO))
colnames(WholeList)[1]<-"PERMNO"
#1:x where x is desired number of graphs
List<-WholeList[1:10,]
setwd("C:/Users/user/Desktop/WhaleWisdom/Plots")

#
for (i in 1:length(List$PERMNO)){
  CleanSample<-subset(CLEAN13DHF2, PERMNO  == List$PERMNO[i])
  CleanSample$DATE<-as.character(CleanSample$DATE)
  CleanSample<-CleanSample%>%
    mutate(DATE=as.Date(DATE, "%Y%m%d"))
  #ggplot1
  Plot<-ggplot(CleanSample, aes(DATE)) + geom_line(aes(y=mroibvol, colour = "Order Imbalance")) + geom_line(aes(y=ret_mkt_m, colour = "Return")) + ggtitle(paste("Order Imbalance for PERMNO" ,List$PERMNO[i])) + xlab("Date") + ylab("")
  rect1 <- data.frame(xmin=as.Date(na.omit(CleanSample$D13_InfoReleaseDate), "%Y-%m-%d"), xmax=as.Date(na.omit(CleanSample$D13_InfoReleaseDate)+15, "%Y-%m-%d"), ymin=-Inf, ymax=Inf)
  rect<-rbind(rect1)
  
  Plot + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                   color="grey20",
                   alpha=0.5,
                   inherit.aes = FALSE) 
  
  ggsave(filename=paste("Order Imbalance for PERMNO",List$PERMNO[i],".jpg"), height=15,width=40,dpi = 320)
  rm(CleanSample,Plot,rect,rect1)
}


