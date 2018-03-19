# Load Workspace
load("data/ISDB.ISTRIPS.RData")
load("data/ISDB.ISFISHSETS.RData")
load("data/ISDB.ISSETPROFILE_WIDE.RData")

# Load Packages
library(dplyr)
library(lubridate)

# Use left join to add TRIP_ID to ISSETPROFILE_WIDE
ISSETPROFILE_WIDE<-left_join(ISSETPROFILE_WIDE, ISFISHSETS%>%select(FISHSET_ID, TRIP_ID))
ISSETPROFILE_WIDE$DATE_TIME1[year(ISSETPROFILE_WIDE$DATE_TIME1)==9999]<-NA
ISSETPROFILE_WIDE$DATE_TIME2[year(ISSETPROFILE_WIDE$DATE_TIME2)==9999]<-NA
ISSETPROFILE_WIDE$DATE_TIME3[year(ISSETPROFILE_WIDE$DATE_TIME3)==9999]<-NA
ISSETPROFILE_WIDE$DATE_TIME4[year(ISSETPROFILE_WIDE$DATE_TIME4)==9999]<-NA

# Group by TRIP_ID and apply error checking functions.
a<-unique(ISSETPROFILE_WIDE$TRIP_ID)[1]
testdata<-ISSETPROFILE_WIDE%>%
  filter(TRIP_ID == a)%>%
  select(TRIP_ID, SET_NO,DATE_TIME1, DATE_TIME2, DATE_TIME3, DATE_TIME4)
testdata<-testdata[order(testdata$SET_NO),]
testdata

total<-0
checked<-0
for(i in unique(ISSETPROFILE_WIDE$TRIP_ID)[1:1000])
{
  total=total+1
  testdata<-ISSETPROFILE_WIDE%>%filter(TRIP_ID==i)
  checkNAs<-testdata%>%summarise(d1=sum(!is.na(DATE_TIME1)),
                                 d2=sum(!is.na(DATE_TIME2)),
                                 d3=sum(!is.na(DATE_TIME3)),
                                 d4=sum(!is.na(DATE_TIME4)))
  if(checkNAs$d2>0 & checkNAs$d3>0)
  {
    checked=checked+1
    testdata<-testdata[order(testdata$SET_NO),]
    
    print(paste("Trip ID:", i))
    # Step 1: Are all datetime3 > datetime2?
    if(length(which(testdata$DATE_TIME3<testdata$DATE_TIME2))>0)
      print(which(testdata$DATE_TIME3<testdata$DATE_TIME2))
    
    # Step 2: Are all datetime2 > datetime3-1
    if(length(which(testdata$DATE_TIME2[-1]<testdata$DATE_TIME3[1:nrow(testdata)-1]))>0)
      print(which(testdata$DATE_TIME2[-1]<testdata$DATE_TIME3[1:nrow(testdata)-1]))
  }
}
paste("Checked", checked, "of", total)




difftime(testdata$DATE_TIME2[-1], testdata$DATE_TIME3[1:nrow(testdata)-1])
