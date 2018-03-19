# ---- Load Workspace and Packages ----
load("data/ISDB.ISTRIPS.RData")
load("data/ISDB.ISFISHSETS.RData")
load("data/ISDB.ISSETPROFILE_WIDE.RData")
library(dplyr)
library(lubridate)

# ---- Data Manipulation ----
# Use left join to add TRIP_ID to ISSETPROFILE_WIDE
ISSETPROFILE_WIDE<-left_join(ISSETPROFILE_WIDE, ISFISHSETS%>%select(FISHSET_ID, TRIP_ID))
ISSETPROFILE_WIDE$DATE_TIME1[year(ISSETPROFILE_WIDE$DATE_TIME1)==9999]<-NA
ISSETPROFILE_WIDE$DATE_TIME2[year(ISSETPROFILE_WIDE$DATE_TIME2)==9999]<-NA
ISSETPROFILE_WIDE$DATE_TIME3[year(ISSETPROFILE_WIDE$DATE_TIME3)==9999]<-NA
ISSETPROFILE_WIDE$DATE_TIME4[year(ISSETPROFILE_WIDE$DATE_TIME4)==9999]<-NA

# ---- Create date_ref ----
# Set up table describing datetime structure to reduce wasted time in loop
date_ref<-ISSETPROFILE_WIDE%>%
  group_by(TRIP_ID)%>%
  summarise(d1=sum(!is.na(DATE_TIME1)),
            d2=sum(!is.na(DATE_TIME2)),
            d3=sum(!is.na(DATE_TIME3)),
            d4=sum(!is.na(DATE_TIME4)))

# ---- Type 1: Available d2 & d3 ----
trips_23<-(date_ref%>%
             filter(d1==0 & d4==0 & d2>0 & d3>0)%>%
             select(TRIP_ID)%>%data.frame())$TRIP_ID

# Set up error reports
error_type1<-c() # if datetime2 < datetime3
error_type2<-c() # if set n < set n-1
error1_details<-list()
error2_details<-list()

# Optional code for estimating efficiency
total<-0 # count total trips considered
starttime<-now()

# Check for errors and generate error reports
for(i in unique(trips_23)[1:1000])
{
  total=total+1
  testdata<-ISSETPROFILE_WIDE%>%filter(TRIP_ID==i)
  testdata<-testdata[order(testdata$SET_NO),]
  
  # Step 1: Are all datetime3 > datetime2?
  if(length(which(testdata$DATE_TIME3<testdata$DATE_TIME2))>0)
  {
    error_type1<-c(error_type1,i)
    error1_details<-append(error1_details, list(c(testdata$SET_NO[which(testdata$DATE_TIME3<testdata$DATE_TIME2)])))
    names(error1_details)[length(error1_details)]<-i
  }
  
  # Step 2: Are all datetime2 > datetime3-1
  if(length(which(testdata$DATE_TIME2[-1]<testdata$DATE_TIME3[1:nrow(testdata)-1]))>0)
  {
    error_type2<-c(error_type2, i)
    error2_details<-append(error2_details, list(c(testdata$SET_NO[which(testdata$DATE_TIME2[-1]<testdata$DATE_TIME3[1:nrow(testdata)-1])])))
    names(error2_details)[length(error2_details)]<-i
  }
}

# Optional code for estimating efficiency
endtime<-now()
paste("Time elapsed:", round(difftime(endtime, starttime, unit='sec'),2), "seconds")
paste("Checks per time:", round(total/as.numeric(difftime(endtime, starttime, unit='sec')),2), "checks per second")
paste("Time per check:", round(as.numeric(difftime(endtime, starttime, unit='sec'))/total,2), "seconds per check")
# WAY FASTER! [0.12 seconds per check to 0.03 seconds per check; from 1 hour to 8 minutes]

# View error reports
error_type1
error1_details
error_type2
error2_details








# ---- Type 2: Available d1 & d4 ----
# Create function used for both 23 and 14 structures
trips_14<-(date_ref%>%
             filter(d1>0 & d4>0 & d2==0 & d3==0)%>%
             select(TRIP_ID)%>%data.frame())$TRIP_ID

# Set up error reports
error_type1<-c() # if datetime2 < datetime3
error_type2<-c() # if set n < set n-1
error1_details<-list()
error2_details<-list()

# Optional code for estimating efficiency
total<-0 # count total trips considered
starttime<-now()

# Check for errors and generate error reports
for(i in unique(trips_14)[1:1000])
{
  total=total+1
  testdata<-ISSETPROFILE_WIDE%>%filter(TRIP_ID==i)
  testdata<-testdata[order(testdata$SET_NO),]
  
  # Step 1: Are all datetime1 > datetime4?
  if(length(which(testdata$DATE_TIME4<testdata$DATE_TIME1))>0)
  {
    error_type1<-c(error_type1,i)
    error1_details<-append(error1_details, list(c(testdata$SET_NO[which(testdata$DATE_TIME4<testdata$DATE_TIME1)])))
    names(error1_details)[length(error1_details)]<-i
  }
  
  # Step 2: Are all datetime1 > datetime4-1
  if(length(which(testdata$DATE_TIME1[-1]<testdata$DATE_TIME4[1:nrow(testdata)-1]))>0)
  {
    error_type2<-c(error_type2, i)
    error2_details<-append(error2_details, list(c(testdata$SET_NO[which(testdata$DATE_TIME1[-1]<testdata$DATE_TIME4[1:nrow(testdata)-1])])))
    names(error2_details)[length(error2_details)]<-i
  }
}

# Optional code for estimating efficiency
endtime<-now()
paste("Time elapsed:", round(difftime(endtime, starttime, unit='sec'),2), "seconds")
paste("Checks per time:", round(total/as.numeric(difftime(endtime, starttime, unit='sec')),2), "checks per second")
paste("Time per check:", round(as.numeric(difftime(endtime, starttime, unit='sec'))/total,2), "seconds per check")

# View error reports
error_type1
error1_details
error_type2
error2_details

# ---- Type 4: Available d1, d2, d3, d4 ----
trips_1234<-(date_ref%>%
            filter(d1>0 & d4>0 & d2>0 & d3>0)%>%
            select(TRIP_ID)%>%data.frame())$TRIP_ID

# Set up error reports
error_type1<-c() # if datetime2 < datetime3
error_type2<-c() # if set n < set n-1
error1_details<-list()
error2_details<-list()

# Optional code for estimating efficiency
total<-0 # count total trips considered
starttime<-now()

# Check for errors and generate error reports
for(i in unique(trips_1234)[1:10])
{
  total=total+1
  testdata<-ISSETPROFILE_WIDE%>%filter(TRIP_ID==i)
  testdata<-testdata[order(testdata$SET_NO),]
  
  # Step 1: Are all datetime1 > datetime2 > datetime3 > datetime4?
  if(length(which(testdata$DATE_TIME2<testdata$DATE_TIME1 |
                  testdata$DATE_TIME3<testdata$DATE_TIME2 |
                  testdata$DATE_TIME4<testdata$DATE_TIME3))>0)
  {
    error_type1<-c(error_type1,i)
    error1_details<-append(error1_details, list(c(testdata$SET_NO[which(testdata$DATE_TIME2<testdata$DATE_TIME1 |
                                                                          testdata$DATE_TIME3<testdata$DATE_TIME2 |
                                                                          testdata$DATE_TIME4<testdata$DATE_TIME3)])))
    names(error1_details)[length(error1_details)]<-i
  }
  
  # Step 2: Are all datetime1 > datetime4-1
  if(length(which(testdata$DATE_TIME1[-1]<testdata$DATE_TIME4[1:nrow(testdata)-1]))>0)
  {
    error_type2<-c(error_type2, i)
    error2_details<-append(error2_details, list(c(testdata$SET_NO[which(testdata$DATE_TIME1[-1]<testdata$DATE_TIME4[1:nrow(testdata)-1])])))
    names(error2_details)[length(error2_details)]<-i
  }
}

# Optional code for estimating efficiency
endtime<-now()
paste("Time elapsed:", round(difftime(endtime, starttime, unit='sec'),2), "seconds")
paste("Checks per time:", round(total/as.numeric(difftime(endtime, starttime, unit='sec')),2), "checks per second")
paste("Time per check:", round(as.numeric(difftime(endtime, starttime, unit='sec'))/total,2), "seconds per check")

# View error reports
error_type1
error1_details
error_type2
error2_details

# test: yes, this is accurate
ISSETPROFILE_WIDE%>%filter(TRIP_ID==8802)%>%
  select(SET_NO, DATE_TIME1, DATE_TIME2, DATE_TIME3, DATE_TIME4)

# ---- Type 3: No Available datetime information ----
trips_0<-(date_ref%>%
             filter(d1==0 & d4==0 & d2==0 & d3==0)%>%
             select(TRIP_ID)%>%data.frame())$TRIP_ID





# ---- Pieces for later ----
difftime(testdata$DATE_TIME2[-1], testdata$DATE_TIME3[1:nrow(testdata)-1])
