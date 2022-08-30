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

MilliCRSPLINK<-fread("C:/Users/user/Desktop/WhaleBJZZ/MilliCRSPLink 10-21.csv")
COMPUSTAT<-fread("C:/Users/user/Desktop/WhaleBJZZ/COMPUSTAT 10-21.csv")
CRSP<-fread("C:/Users/user/Desktop/WhaleBJZZ/CRSP 10-21.csv")
Milli<-fread("C:/Users/user/Desktop/WhaleBJZZ/Milli 10-21_No 09.csv")
CRSPMONTHLY<-fread("C:/Users/user/Desktop/WhaleBJZZ/CRSPMONTHLY 10-21.csv")

#COMPUSTATFINANCIAL<-fread("C:/Users/user/Desktop/WhaleBJZZ/COMPUSTATFINANCIAL 14-21.csv")
##f13<-fread("C:/Users/user/Desktop/WhaleWisdom/2021_13F_Investor.csv")
##dg13<-fread("C:/Users/user/Desktop/WhaleWisdom/2021_13DG_Investor.csv")

#Milli stands for Millisecond Data by WRDS

############################COMPUSTAT, CRSPMONTHLY MERGE#############################
###########################TO GET MONTHLY BOOK TO MARKET#############################
#####CRSPMONTHLY trimming
#shrcd in 10 11
CRSPMONTHLY<-subset(CRSPMONTHLY, SHRCD  == 11 |SHRCD  == 10)

#remove price under $1
CRSPMONTHLY2 <- subset(CRSPMONTHLY, !abs(PRC) <= 1)

#absolute price
CRSPMONTHLY3<-CRSPMONTHLY2%>%
  mutate(PRC=abs(PRC))

#delete unnecessary tables
CRSPMONTHLY<-CRSPMONTHLY3
rm(CRSPMONTHLY2)
rm(CRSPMONTHLY3)

#get monthly date
CRSPMONTHLY<- CRSPMONTHLY%>%
  mutate(MONTHLYDATE=substr(date,1,6))
head(CRSPMONTHLY)

#get yearly date
CRSPMONTHLY<- CRSPMONTHLY%>%
  mutate(YEARLYDATE=substr(date,1,4))
head(CRSPMONTHLY)

#####COMPUSTAT trimming
colnames(COMPUSTAT)[2] <-"date"

COMPUSTAT<- COMPUSTAT%>%
  mutate(CUSIP=substr(cusip,1,8))
head(COMPUSTAT)

#monthly date
COMPUSTAT<- COMPUSTAT%>%
  mutate(MONTHLYDATE=substr(date,1,6))
head(COMPUSTAT)

#yearly date
COMPUSTAT<- COMPUSTAT%>%
  mutate(YEARLYDATE=substr(date,1,4))
head(COMPUSTAT)

# #####COMPUSTATFINANCIAL trimming
# head(COMPUSTATFINANCIAL)
# COMPUSTATFINANCIAL<-COMPUSTATFINANCIAL[,-c(3,4)]
# 
# colnames(COMPUSTATFINANCIAL)[3] <-"date"
# colnames(COMPUSTATFINANCIAL)[2] <-"PERMNO"
# 
# # COMPUSTATFINANCIAL<- COMPUSTATFINANCIAL%>%
# #   mutate(CUSIP=substr(cusip,1,8))
# #head(COMPUSTATFINANCIAL)
# 
# COMPUSTATFINANCIAL<- COMPUSTATFINANCIAL%>%
#   mutate(MONTHLYDATE =substr(date,1,6))
# head(COMPUSTATFINANCIAL)

# ###merge COMPUSTAT and CRSP by trimmed eight digit cusip and monthly date
# #CRSPMONTHCOMPUSTATFINANCIAL<-merge(CRSPMONTHLY,COMPUSTATFINANCIAL, by=c('PERMNO','date'))
# CRSPMONTHCOMPUSTATFINANCIAL<-merge(CRSPMONTHLY,COMPUSTATFINANCIAL, by=c('PERMNO','MONTHLYDATE'))
# #it is better to match by PERMNO than by CUSIP
# 
# head(CRSPMONTHCOMPUSTATFINANCIAL)
# CRSPMONTHCOMPUSTATFINANCIAL2<-na.omit(CRSPMONTHCOMPUSTATFINANCIAL)
# CRSPMONTHCOMPUSTATFINANCIAL2<-CRSPMONTHCOMPUSTATFINANCIAL2[,-c(4,5,7,8,9,10,11,12,15,16)]
# CRSPMONTHCOMPUSTATFINANCIAL<-CRSPMONTHCOMPUSTATFINANCIAL2
# rm(CRSPMONTHCOMPUSTATFINANCIAL2)


#######merge COMPUSTAT and CRSP by trimmed eight digit cusip and monthly date#######
head(CRSPMONTHLY)
head(COMPUSTAT)
#GET RID OF DUPLICATES
COMPUSTAT<-COMPUSTAT[,-c(1,2,3,4,5,6,7,9,10,12,13,15)]
#IF I DON'T GET RID OF THOSE COLUMNS, IT IS HARD TO FIND DUPLICATES
COMPUSTAT<-na.omit(COMPUSTAT)
COMPUSTAT<-COMPUSTAT3[!duplicated(COMPUSTAT)]

#left join compustat yearly data to crsp monthly.
CRSPMONTHCOMPU<-merge(CRSPMONTHLY,COMPUSTAT, by=c('CUSIP','YEARLYDATE'), all.x = TRUE)
#there are still a litte duplicates (same cusip, yearlydate, and ticker, but with multiple bookvalue)
#It takes too much time to identify. let's skip at this moment.


#calcualte book to market value
CRSPMONTHCOMPU<- CRSPMONTHCOMPU%>%
  mutate(BookToMarket=bkvlps/PRC)

head(CRSPMONTHCOMPU)

#take log of book to market value
CRSPMONTHCOMPU<- CRSPMONTHCOMPU%>%
  mutate(LOGBookToMarket=log(BookToMarket))
head(CRSPMONTHCOMPU)

CRSPMONTHCOMPU<- CRSPMONTHCOMPU%>%
  mutate(LAGLOGBookToMarket=lag(LOGBookToMarket))
head(CRSPMONTHCOMPU)

CRSPMONTHCOMPU<-CRSPMONTHCOMPU[,-c(5,6,13,14)]

########################################################################
##########################CLEAN CRSP####################################

#shrcd in 10 11
CRSP<-subset(CRSP, SHRCD  == 11 |SHRCD  == 10)

#2 remove price under $1 
CRSP <- subset(CRSP, !abs(PRC) <= 1)

#absolute price
CRSP<-CRSP%>%
  mutate(PRC=abs(PRC))

######################################################################
######################################################################
############################Daily Return##############################
head(CRSP)

CRSP<-CRSP%>%
  group_by(PERMNO)%>%
  mutate(DailyReturn=(PRC)/(lag(PRC))-1)
head(CRSP)

#calculate weekly Rolling compounded return
CRSP<-CRSP%>%
  group_by(PERMNO)%>%
  mutate(WeeklyCompoundReturn=((PRC)/(lag(PRC,n=5)))^(1/5)-1)
head(CRSP)

#####################################################################
#####################################################################
############calculate monthly volatility of daily returns############

#get monthly date
CRSP<- CRSP%>%
  mutate(DATE2=substr(date,1,6))

#get rid of NA in Daily Return
CRSP<-CRSP %>% drop_na(DailyReturn)

#calculate volatility based on month date #
CRSP<- CRSP%>%
  group_by(DATE2,PERMNO)%>%
  mutate(Volatility=sd(DailyReturn))
head(CRSP3)

#split volatility for a second to lag monthly
Volatility<-CRSP[,c(1,5,13,14)]
head(Volatility)
colnames(Volatility)[2] <-"TICKER"
Volatility<-Volatility%>% distinct(PERMNO, TICKER, DATE2, Volatility, .keep_all = TRUE)

Volatility<- Volatility%>%
  group_by(PERMNO)%>%
  mutate(LAGVolatility=lag(Volatility))
Volatility<-Volatility[,-c(2,4)]

#re-merge to add lagged volatiltiy
CRSP2<-merge(CRSP,Volatility, by=c('PERMNO','DATE2'))
#CRSP5<-merge(CRSP3,Volatility, by=c('PERMNO','DATE2'),all.x = TRUE)
#CRSP4, CRSP5 results are the same, meaning there are duplicates.

#CRSP3<-CRSP2[!duplicated(CRSP2)] can't run this. not enought memory.

CRSP2<- CRSP2 %>% 
  arrange(date)%>%
  arrange(PERMNO)

head(CRSP2)
CRSP2<-CRSP2[,-c(2,4,5,14)]

CRSP<-CRSP2  
rm(CRSP2)
rm(Volatility)


#####################################################################################
#####################################################################################
#####################################CLEAN MILLI#####################################
#if Na in Bs_ratio_retail, then also NA in other related important variables.
#drop NA in bs_ratio_retail_vol
Milli2<-Milli %>% drop_na(bs_ratio_retail_vol)

#drop NA in bs_ratio_retail_num
Milli3<-Milli2 %>% drop_na(bs_ratio_retail_num)

Milli<-Milli3
rm(Milli2)
rm(Milli3)


########################################################################################
#calculate Order imbalance before matching.

#############################ORDER IMBALANCE MEASURE#################################
#####################################################################################
#check if bs_ratio_retail_vol is equal to (buy-sell)/(buy+sell)
#the given mroibtrd and mroibvol does not match with table I
#mean(MilliCRSP$mroibtrd)
#mean(MilliCRSP$mroibvol)

head(Milli)
colnames(Milli)[5]<-"mrbtrd"
colnames(Milli)[6]<-"mrbvol"
colnames(Milli)[7]<-"mrstrd"
colnames(Milli)[8]<-"mrsvol"

#calcualte new mroibtrd and mroibvol
Milli2<-Milli%>%
  mutate(mroibtrd=(mrbtrd-mrstrd)/(mrbtrd+mrstrd))
#mean(Milli2$mroibtrd) worry about this later after merge.

Milli2<-Milli2%>%
  mutate(mroibvol=(mrbvol-mrsvol)/(mrbvol+mrsvol))
#mean(Milli2$mroibvol)

#manual calculation is not the same but quite similar. 0.006 off for both.
#maybe due to different number of observation 5281472(mine) 4628957(Paper)

Milli<-Milli2
rm(Milli2)


# # #THOUGH ROLLING MEAN WAS WRONG SO I WAS GOING TO USE BELOW FUNCTION TO ONLY USE 5TH ROWS
# #only save every 5th data point
# Nth.only<-function(dataframe, n)dataframe[(seq(n,to=nrow(dataframe),by=n)),]
# MilliCRSP3<-Nth.only(MilliCRSP2, 5)
# head(MilliCRSP3)
########################################################################################



#######################match CRSP and Milli via MilliCRSPLINK########################
#####################################################################################

#merge CRSP with MilliCRSPLINK via PERMNO and date
head(MilliCRSPLINK)
head(CRSP)
CRSP2<-merge(MilliCRSPLINK,CRSP, by=c('PERMNO','date'))


head(CRSP)
head(Milli)
#merge Milli with MilliCRSPLINK via SYM_ROOT and date
colnames(MilliCRSPLINK) <- c('SYM_ROOT', 'DATE', 'SYM_SUFFIX ', 'PERMNO', 'CUSIP', 'NCUSIP', 'match_lvl' )
Milli2<-merge(MilliCRSPLINK,Milli, by=c('SYM_ROOT','DATE'))

#match CRSP and Milli via PERMNO
head(CRSP2)
colnames(CRSP2)[2] <-"DATE"
MilliCRSP<-merge(CRSP2,Milli2, by=c('PERMNO','DATE'))

rm(MilliCRSPLINK)
rm(Milli)
rm(CRSP)
rm(Milli2)
rm(CRSP2)

head(MilliCRSP)
MilliCRSP <- MilliCRSP[ ,-c(3,4,5,6,7,9,17,18,20,21,22,24,25,26,27)]
head(MilliCRSP)

#make order imbalance weekly (five day average imbalance)do this after merge...
head(MilliCRSP)
MilliCRSP2<-MilliCRSP%>%
  group_by(PERMNO)%>%
  mutate(WeeklyImbalanceTrade=rollmean(mroibtrd,k=5,fill=NA))

MilliCRSP2<-MilliCRSP2%>%
  group_by(PERMNO)%>%
  mutate(WeeklyImbalanceVolume=rollmean(mroibvol,k=5,fill=NA))

MilliCRSP<-MilliCRSP2
rm(MilliCRSP2)


##############################INDEPENDENT VARIABLE###################################
#####################################################################################

#calculate monthly returns ####modified July 1
CRSPMONTHLY<-CRSPMONTHLY%>%
  group_by(PERMNO)%>%
  mutate(LAGMonthlyReturn=lag(PRC,n=1)/(lag(PRC,n=2))-1)
head(CRSPMONTHLY)

#calculate previous 6 month return ####modified July 1
CRSPMONTHLY<-CRSPMONTHLY%>%
  group_by(PERMNO)%>%
  mutate(SixMonthReturn=(lag(PRC,n=2)/(lag(PRC,n=8)))^(1/6)-1)
head(CRSPMONTHLY)

################control variables#########################
############calculate market cap############
CRSPMONTHLY<-CRSPMONTHLY%>%
  group_by(PERMNO)%>%
  mutate(MonthlyMarketCap=PRC*SHROUT)
#take log
CRSPMONTHLY<-CRSPMONTHLY%>%
  mutate(LOGMonthlyMarketCap=log(MonthlyMarketCap))

CRSPMONTHLY<-CRSPMONTHLY%>%
  mutate(LAGLOGMonthlyMarketCap=lag(LOGMonthlyMarketCap))

CRSPMONTHLY<-CRSPMONTHLY[,-c(15,16)]

############calculate turnover############
CRSPMONTHLY<-CRSPMONTHLY%>%
  group_by(PERMNO)%>%
  mutate(MonthlyTurnover=VOL/SHROUT)

CRSPMONTHLY<-CRSPMONTHLY%>%
  mutate(LAGMonthlyTurnover=lag(MonthlyTurnover))

CRSPMONTHLY<-CRSPMONTHLY[,-c(16)]


#####################################################################################
#####################################################################################
################################DEPENDENT VARIABLE###################################

#distinguish CRSP return and trim a bit
head(MilliCRSP)
colnames(MilliCRSP)[6] <-"CRSPRETURN"

#merge CRSP Monthly and MilliCRSP
head(MilliCRSP)
head(CRSPMONTHLY)
colnames(CRSPMONTHLY)[2] <-"DATE"

######merge monthly data to daily data######
head(MilliCRSP)
MilliCRSP<- MilliCRSP%>%
  mutate(MONTHLYDATE=substr(DATE,1,6))
head(MilliCRSP)
head(CRSPMONTHLY)
MilliCRSP_Mth_Day<-merge(MilliCRSP,CRSPMONTHLY, by=c('PERMNO','MONTHLYDATE'))
head(MilliCRSP_Mth_Day)
colnames(MilliCRSP_Mth_Day)[3] <-"DAILYDATE"
colnames(MilliCRSP_Mth_Day)[27] <-"CRSP MONTHLY RETURN"
#order by DAILYDATE
MilliCRSP_Mth_Day<- MilliCRSP_Mth_Day %>% 
  arrange(DAILYDATE)%>%
  arrange(PERMNO)


#Lag Imbalance and weekly return
MilliCRSP_Mth_Day<-MilliCRSP_Mth_Day%>%
  group_by(PERMNO)%>%
  mutate(LAGWeeklyImbalanceTrade=lag(WeeklyImbalanceTrade, n=5))

MilliCRSP_Mth_Day<-MilliCRSP_Mth_Day%>%
  group_by(PERMNO)%>%
  mutate(LAGWeeklyImbalanceVolume=lag(WeeklyImbalanceVolume, n=5))

MilliCRSP_Mth_Day<-MilliCRSP_Mth_Day%>%
  group_by(PERMNO)%>%
  mutate(LAGWeeklyCompoundReturn =lag(WeeklyCompoundReturn))

# MilliCRSP_Mth_Day<-MilliCRSP_Mth_Day%>%
#   group_by(PERMNO)%>%
#   mutate(LAGMonthlyReturn =lag(MonthlyReturn))


################add book to market #########################
CRSPMONTHCOMPU<-CRSPMONTHCOMPU[,-c(1,2,6,7,8,9,11,12)]
CRSPMONTHCOMPU<-CRSPMONTHCOMPU[,-c(2,3)]

head(MilliCRSP_Mth_Day)
MilliCRSP_Mth_Day <- MilliCRSP_Mth_Day[ ,-c(20,21,22,23,24,25,26,28,29)]

MilliCRSPCOMPU<-merge(MilliCRSP_Mth_Day,CRSPMONTHCOMPU, by=c('PERMNO','MONTHLYDATE'))

MilliCRSPCOMPU<- MilliCRSPCOMPU %>% 
  arrange(DAILYDATE)%>%
  arrange(PERMNO)

head(MilliCRSPCOMPU)

#write.csv(MilliCRSP,"C:/Users/user/Desktop/WhaleWisdom/MilliCRSP 2010-2021.csv", row.names = FALSE )
rm(COMPUSTAT)
rm(MilliCRSP)
rm(CRSPMONTHLY)
rm(MilliCRSP_Mth_Day)
rm(CRSPMONTHCOMPU)
gc()

################################################################################################
################Fama Macbeth Manual Regression without book to market #########################
#get rid of duplicated data, NA, and minus infinity
head(MilliCRSPCOMPU)
MilliCRSPCOMPU <- data.table(MilliCRSPCOMPU)
MilliCRSPCOMPU<-MilliCRSPCOMPU[!duplicated(MilliCRSPCOMPU), ]
MilliCRSPCOMPU<-na.omit(MilliCRSPCOMPU)
MilliCRSPCOMPU<-MilliCRSPCOMPU[!LAGLOGBookToMarket=="-Inf"]

#write.csv(MilliCRSPCOMPU,"C:/Users/user/Desktop/WhaleWisdom/MilliCRSPCOMPU 2010-2021.csv", row.names = FALSE )
################
#Fama Macbeth 
reg.1 <- MilliCRSPCOMPU[,as.list(coef(lm(WeeklyImbalanceVolume ~ LAGWeeklyImbalanceVolume + LAGWeeklyCompoundReturn
                                          +LAGMonthlyReturn + SixMonthReturn + LAGMonthlyTurnover + LAGVolatility + LAGLOGMonthlyMarketCap + LAGLOGBookToMarket))), by=DAILYDATE]
################
#Get coefficients
reg.2<-na.omit(reg.1)
second.step.coefficients <- apply(reg.2[, -1], 2, mean)
second.step.coefficients

################
#Newey-West SE with six lag
second.step.NW.sigma.sq <- apply(reg.2[, -1], 2, 
                                 function(x) sqrt(NeweyWest(lm(x ~ 1), 
                                                            lag = 6, prewhite = FALSE)['(Intercept)',       
                                                                                       '(Intercept)']))
second.step.NW.sigma.sq

#t-statistics with lag 6
t.statistics.NW.lag.6 <- second.step.coefficients / second.step.NW.sigma.sq
t.statistics.NW.lag.6

################
#count how many days used in the regression
Days<-unique(MilliCRSPCOMPU$DAILYDATE)
Days<-as.data.frame(Days)
Days<-count(Days)

Number_of_stocks<-setNames(count(MilliCRSPCOMPU, "DAILYDATE"), c("Date", "Number of Stocks"))

#stocks per day = #obs / # of days
StockperDay<-Number_of_stocks$`Number of Stocks`/Days

################################################################################################
################################################################################################
################Fama Macbeth Manual Regression with book to market #########################
#currently pmg function not working becuase of some NA remains in the regression, which I could remove it
#in manual regression but not able to do that in the package.

# fpmg <- pmg(WeeklyImbalanceVolume ~ LAGWeeklyImbalanceVolume + LAGWeeklyCompoundReturn
#             +LAGMonthlyReturn + SixMonthReturn + LAGMonthlyTurnover + LAGVolatility + LAGLOGMonthlyMarketCap + LAGLOGBookToMarket, MilliCRSPCOMPU5, na.omit, index=c("DAILYDATE"))
# summary(fpmg)
# coeftest(fpmg)
# 

