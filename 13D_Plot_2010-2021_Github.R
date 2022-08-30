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
#library(scales)
#what happens if I do this?
d13<- d13[1,2]
d13<-d13[1:10,]
d13
#final change

library(data.table)
getwd()
d13<-fread("C:/Users/user/Desktop/WhaleWisdom/13D 2010-2021.csv")
MilliCRSP<-fread("C:/Users/user/Desktop/WhaleWisdom/MilliCRSP 2010-2021.csv")

########################################################################################
#colnames(d13)[21]<-"filer name"
colnames(d13)[6]<-"CUSIP"
d13<- d13%>%
  mutate(CUSIP=substr(CUSIP,1,8))
head(d13)

d13<-subset(d13,select=c(stock_id, filer_id, form_type, CUSIP, filed_as_of_date, event_date, HedgeFund))

#GET MARKET CAP FROM MILLI
MilliCRSP<-MilliCRSP%>%
  group_by(PERMNO)%>%
  mutate(MarketCap=PRC*SHROUT)

MilliCRSP<-subset(MilliCRSP,select=c(PERMNO, DATE, TICKER, RET, CUSIP, MarketCap))

#n_distinct(d13$form_type)
#sapply(d13, n_distinct)
#unique(d13$form_type)
#subset()
#substring(dg13$form_type,1,6)
#D13<-subset(dg13, substring(dg13$form_type,1,6)  == "SC 13D")
#D13light<-D13[,c(2,3,4,5,6,7,8,9,14,15,16,17,21,28)]

#change date format
#colnames(D13light)[5]<-"D13_InfoReleaseDate"

d13<-d13%>%
  mutate(DATE=format(d13$event_date, "%Y%m%d"))

#make it numeric values
d13<-d13%>%
  mutate(DATE=as.numeric(d13$DATE))

#merge 13d and crsp by CUSIP and DATE
#for now I am only interested in size and return
# MilliCRSP<-MilliCRSP[,c(1,2,3,4,6,7,11)]
# 
# MilliCRSP<-MilliCRSP%>%
#   group_by(PERMNO)%>%
#   mutate(MarketCap=PRC*SHROUT)

# MilliCRSP<-MilliCRSP[,c(1,2,3,4,5,7,8)]

#MilliCRSP13D<-merge(MilliCRSP,d13,by=c('CUSIP','DATE'),all.x = TRUE)

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

#Make a list of firms that are announced in 13D
Announced13D<-subset(MilliCRSP13D, HedgeFund=="Yes" | HedgeFund=="No")
tab1(Announced13D$HedgeFund, sort.group = "decreasing", cum.percent = TRUE)
#good.

##########################################################################################
##########################################################################################
#I think we don't need the list...

#List13D<-subset(Announced13D,select=c(PERMNO,DATE,HedgeFund))
#List13D<-List13D[!duplicated(List13D), ]
#names(List13D)[names(List13D) == 'HedgeFund'] <- 'ChosenByHedgeFund'

#Hmmm There are many firms that has been announced by both Hedge funds and non-hedge funds
#let's just think about hedge fund only
#later I can use year to match the two table. so that I can know what year it was announced.
# List13DHF<-subset(List13D, HedgeFund=="Yes")
# List13DHF<-List13DHF[!duplicated(List13DHF), ]
# rm(List13D2)
# List13DHF<-List13DHF[,c(1,5)]
# colnames(List13DHF)[2]<-"ChosenByHedgeFund"

#want to calculate buy and hold return of the value-weight market
df<-MilliCRSP

df<-df%>%
  mutate(DATE=as.numeric(DATE),RET=as.numeric(RET))

head(df)

df<-df%>%
  group_by(DATE)%>%
  mutate(wret=weighted.mean(RET,MarketCap, na.rm=TRUE))

#check if it was successful.
# see<- df %>% 
#   arrange(DATE)
# head(see)
# rm(see)
#leftjoin this to MillCRSP13D
head(df)
df<-subset(df,select=c(PERMNO, DATE, wret))
rm(MilliCRSP)
MilliCRSP13D<-left_join(MilliCRSP13D,df,by=c('PERMNO','DATE'))

#now we have the weighted average market return.
head(MilliCRSP13D)

#hahaha just testing github repository blah blah blahhhh
#
makesomenoise<-c(1,2,3.4)

#left join. add 13D HF list to Millicrsp13D
#MilliCRSP13D<-left_join(MilliCRSP13D,List13D,by=c('PERMNO','DATE'))
tab1(MilliCRSP13D$ChosenByHedgeFund, sort.group = "decreasing", cum.percent = TRUE)
CLEAN13DHF<-subset(MilliCRSP13DHF, ChosenByHedgeFund=="Yes")

#I need excess return and turnover... to see what Barry, Brav, Jian found.
#or just see imbalance first.
colnames(CLEAN13DHF)[11]<-"mrbtrd"
colnames(CLEAN13DHF)[12]<-"mrbvol"
colnames(CLEAN13DHF)[13]<-"mrstrd"
colnames(CLEAN13DHF)[14]<-"mrsvol"
head(CLEAN13DHF)

#calcualte new mroibtrd and mroibvol
CLEAN13DHF<-CLEAN13DHF%>%
  mutate(mroibtrd=(mrbtrd-mrstrd)/(mrbtrd+mrstrd))

CLEAN13DHF<-CLEAN13DHF%>%
  mutate(mroibvol=(mrbvol-mrsvol)/(mrbvol+mrsvol))

CLEAN13DHF<-CLEAN13DHF[,-c(11,12,13,14)]

CLEAN13DHF2<-CLEAN13DHF[,c(1,2,3,4,7,9,10,14,24,25,26,27)]

#also calculate excess return
CLEAN13DHF3<-CLEAN13DHF2%>%
  mutate(RET=as.numeric(RET))%>%
  mutate(ExcRet=RET-wret)

CLEAN13DHF<-CLEAN13DHF3
rm(CLEAN13DHF2,CLEAN13DHF3)


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


