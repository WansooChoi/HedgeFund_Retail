library(dplyr)
library(data.table)
# df<-fread("C:/Users/user/Desktop/Example.csv")
# df2<-fread("C:/Users/user/Desktop/Example2.csv")
# df3<-fread("C:/Users/user/Desktop/Example3.csv")
# df4<-fread("C:/Users/user/Desktop/Example4.csv")
df<-fread("C:/Users/user/Desktop/Example5.csv")
##############################################################################################################
#choose rows with no NA in event date and only show ID and event date
events = unique(df[!is.na(EventDate),.(ID,EventDate)])
#.(ID,EventDate) means to display the two column.

#helper column
#:= is defined for use in j only. It adds or updates or removes column(s) by reference. 
#It makes no copies of any part of memory at all.
events[, eDate:=EventDate]

#makes new column(temporary) lower and upper boundary
df[, `:=`(s=Date-6, e=Date+6)]

#non-equi match
bhr = events[df, on=.(ID, EventDate>=s, EventDate<=e), nomatch=0]

#Generate the BuyHoldReturn column, by ID and EventDate
bhr2 = bhr[, .(Date, BuyHoldReturn=c(NA, Price[-1]/Price[1] -1)), by = .(ID,eDate)]

#merge back to get the full data
bhr3 = bhr2[df,on=.(ID,Date),.(ID,Date,Price,EventDate=i.EventDate,BuyHoldReturn)]
##############################################################################################################
#choose rows with no NA in event date and only show ID and event date
events = unique(df[!is.na(EventDate),.(ID,EventDate)])
#.(ID,EventDate) means to display the two column.

#helper column
#:= is defined for use in j only. It adds or updates or removes column(s) by reference. 
#It makes no copies of any part of memory at all.
events[, eDate:=EventDate]

#makes new column(temporary) lower and upper boundary
df[, `:=`(s=Date-6, e=Date+6)]

#non-equi match
bhr = events[df, on=.(ID, EventDate>=s, EventDate<=e), nomatch=0]

#Generate the BuyHoldReturn column, by ID and EventDate
bhr2 = bhr[, .(Date, BuyHoldReturnM1=c(NA, (Price[-1]/Price[1] -1)*MarketCap[-1])), by = .(ID,eDate)]
bhr3 = bhr2[, .(Date, BuyHoldReturnM2=c(NA, (Price[-1]/Price[1] -1)*MarketCap[-1])), by = .(ID[1],eDate)]

#merge back to get the full data
bhr3 = bhr2[df,on=.(ID,Date),.(ID,Date,Price,EventDate=i.EventDate,BuyHoldReturn)]
##############################################################################################################



df <- df %>% mutate(across(c(Date,EventDate), ~as.Date(.x)))


whatthis1<-filter(df,!is.na(EventDate))
whatthis2<-left_join(df,inner_join(df %>% dplyr::select(-EventDate),filter(df,!is.na(EventDate))))


df2<-left_join(df,inner_join(df %>% dplyr::select(-EventDate),filter(df,!is.na(EventDate)) %>% distinct(ID, EventDate), by="ID") %>%
    filter(abs(EventDate-Date)<=6) %>% 
    group_by(ID, EventDate) %>% 
    mutate(BuyHoldReturn = c(NA,Price[-1]/Price[1]-1)),
  by=c("ID", "Date")
)

 

weighted.mean((PRC[-1]/PRC[1]-1)

##############################################################################################################
setDT(df)
df[,(c("Date", "EventDate")):=lapply(.SD, as.Date), .SDcols=c("Date", "EventDate")]
df2<-df[,!c("EventDate")][unique(df[!is.na(EventDate), .(ID, EventDate)]), on="ID", allow.cartesian=T][
  abs(EventDate-Date)<=6][,BuyHoldReturn:=c(NA,Price[-1]/Price[1]-1), .(ID,EventDate)][
    df[,.(ID,Date)], on=.(ID,Date)
  ]
