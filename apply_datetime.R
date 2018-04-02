# ---- Load Workspace and Packages ----
load("data/ISDB.ISFISHSETS.RData")
load("data/ISDB.ISSETPROFILE_WIDE.RData")
library(dplyr)
library(lubridate)
library(tidyr)
source("datetime_functions.R")

# ---- Data Manipulation ----
# Use left join to add TRIP_ID to ISSETPROFILE_WIDE
ISSETPROFILE_WIDE<-left_join(ISSETPROFILE_WIDE, ISFISHSETS%>%select(FISHSET_ID, TRIP_ID))
# Convert missing dates to NA
ISSETPROFILE_WIDE$DATE_TIME1[year(ISSETPROFILE_WIDE$DATE_TIME1)==9999]<-NA
ISSETPROFILE_WIDE$DATE_TIME2[year(ISSETPROFILE_WIDE$DATE_TIME2)==9999]<-NA
ISSETPROFILE_WIDE$DATE_TIME3[year(ISSETPROFILE_WIDE$DATE_TIME3)==9999]<-NA
ISSETPROFILE_WIDE$DATE_TIME4[year(ISSETPROFILE_WIDE$DATE_TIME4)==9999]<-NA

# ---- Apply Datetime Functions ----
spt<-now()
datetime_ref()
datetime_all(print_all=FALSE)
ept<-now()
ept-spt

# ---- Write Error Reports ----
write.csv(errors_seq_1, "error-reports/errors_seq_1.csv", row.names=FALSE)
write.csv(errors_seq_2, "error-reports/errors_seq_2.csv", row.names=FALSE)
write.csv(errors_sten_2, "error-reports/errors_sten_2.csv", row.names=FALSE)
write.csv(errors_seq_34, "error-reports/errors_seq_34.csv", row.names=FALSE)
write.csv(errors_withinsetseq_34, "error-reports/errors_withinsetseq_34.csv", row.names=FALSE)




# ---- Summary Statistics ----
# How many trips per datetime variable type?
table1<-dt_ref_sum%>%
  group_by(n)%>%
  summarise(count=n())%>%
  data.frame()
table1

# Of type 1, how many errors?
nrow(errors_seq_1)
  # how many trips?
length(unique(errors_seq_1$TRIP_ID))
  # How many errors of each type?
table2<-errors_seq_1%>%
  group_by(error_details)%>%
  summarise(count=n())%>%
  data.frame()
table2

# Of type 2, how many errors in sequence?
nrow(errors_seq_2)
  # How many trips?
length(unique(errors_seq_2$TRIP_ID))
  # How many sequence errors of each type?
table3<-errors_seq_2%>%
  group_by(error_details)%>%
  summarise(count=n(),
            trips=length(unique(TRIP_ID)))%>%
  data.frame()
table3

