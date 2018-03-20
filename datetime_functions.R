# All functions here require {dplyr}

# ---- Function: datetime_ref() ----
# Produces: dt_ref (tibble) and dt_ref_sum (dataframe)
# Optional Argument: unique TRIP_ID to check (x)
# Requires: ISSETPROFILE_WIDE with TRIP_ID appended and 1999-01-01 dates converted to NA
datetime_ref<-function(x=unique(ISSETPROFILE_WIDE$TRIP_ID))
{
  dt_ref<<-ISSETPROFILE_WIDE%>%
    filter(TRIP_ID %in% x)%>%
    group_by(TRIP_ID)%>%
    summarise(d1=sum(!is.na(DATE_TIME1)),
              d2=sum(!is.na(DATE_TIME2)),
              d3=sum(!is.na(DATE_TIME3)),
              d4=sum(!is.na(DATE_TIME4)))
  dt_ref_sum<<-dt_ref%>%
    data.frame()%>%gather("type","value",2:5)%>%
    mutate(newvalue=ifelse(value>0,1,0))%>%
    select(TRIP_ID, type, newvalue)%>%
    spread(type, newvalue)%>%
    mutate(n=d1+d2+d3+d4)
}

# ---- Function: datetime_0() ----
# Produces: vector of trips with no datetime information
datetime_0<-function() {return(unique(dt_ref_sum$TRIP_ID[dt_ref_sum$n==0]))}

# ---- Function: datetime_1() ----
# Checks: Sets with single datetime available
# Produces: (a) trips where sequence of set dates is unordered (data frame called errors_seq_1)
# Optionally returns this data frame if print_errors=TRUE
datetime_1<-function(print_errors=FALSE)
{
  trips<-unique(dt_ref_sum$TRIP_ID[dt_ref_sum$n==1])
  
  errors_seq_1<<-ISSETPROFILE_WIDE%>%
    filter(TRIP_ID %in% trips)%>%
    group_by(TRIP_ID)%>%
    arrange(SET_NO)%>%
    gather("datetime","value",c(DATE_TIME1, DATE_TIME2, DATE_TIME3, DATE_TIME4))%>%
    filter(!is.na(value))%>%
    summarise(sequence_error=is.unsorted(value, strictly=TRUE),error_details=ifelse(length(unique(value))==1,"Single date for all sets","Multiple dates"))%>%
    filter(sequence_error==TRUE)%>%
    data.frame()

  if(print_errors==TRUE){return(errors_seq_1)}
}

# ---- Function: datetime_2() ----
# Checks: Sets with 2 datetimes available
# Produces: (a) sets where end datetime < start datetime (data frame called errors_sten_2)
#           (b) sets where start datetime < end datime of previous set (data frame called errors_seq_2)
# Optionally returns these data frames as a list if print_errors=TRUE
datetime_2<-function(print_errors=FALSE)
{
  trips<-unique(dt_ref_sum$TRIP_ID[dt_ref_sum$n==2])
  
  ref1<-ISSETPROFILE_WIDE%>%
    filter(TRIP_ID %in% trips)%>%
    group_by(TRIP_ID)%>%
    gather("datetime","value",c(DATE_TIME1, DATE_TIME2, DATE_TIME3, DATE_TIME4))%>%
    arrange(TRIP_ID, SET_NO)%>%
    group_by(TRIP_ID, SET_NO)%>%
    summarise(start=first(value[!is.na(value)]),end=nth(value[!is.na(value)],2))%>%
    data.frame()

  # Identify sets with end datetime before start datetime or missing values
  errors_sten_2<<-ref1%>%
    filter(start>end | is.na(start) | is.na(end))%>%
    mutate(error_details=ifelse(start>end & !is.na(start) & !is.na(end),"End datetime < start datetime","Missing start or end datetime"))
  
  # Identify sets with start datetime before end of previous set end datetime
  errors_seq_2<<-ref1%>%
    group_by(TRIP_ID)%>%
    filter(start<lag(end) | is.na(start) | is.na(lag(end)))%>%
    filter(SET_NO>1)%>%
    mutate(error_details=ifelse(start<lag(end) & !is.na(start) & !is.na(lag(end)),"End datetime < start datetime","Missing start or previous end datetime"))%>%
    select(TRIP_ID, SET_NO, error_details)%>%
    data.frame()
  
  if(print_errors==TRUE){return(list(errors_sten_2, errors_seq_2))}
}

# ---- Function: datetime_34() ----
# Checks: Sets with 3 or 4 datetimes available
# Produces: (a) sets where end datetime < start datetime (data frame called errors_sten_34)
#           (b) sets where start datetime < end datime of previous set (data frame called errors_seq_34)
#           (c) sets where available within-set dates are not sequential (data frame called errors_withinsetseq_34)
# Optionally returns these data frames as a list if print_errors=TRUE
datetime_34<-function(print_errors=FALSE)
{
  trips<-unique(dt_ref_sum$TRIP_ID[dt_ref_sum$n %in% c(3,4)])
  
  ref1<-ISSETPROFILE_WIDE%>%
    filter(TRIP_ID %in% trips)%>%
    group_by(TRIP_ID)%>%
    gather("datetime","value",c(DATE_TIME1, DATE_TIME2, DATE_TIME3, DATE_TIME4))%>%
    select(TRIP_ID, SET_NO,datetime, value)
  ref2<-ref1%>%
    arrange(TRIP_ID, SET_NO)%>%
    group_by(TRIP_ID, SET_NO)%>%
    summarise(start=first(value[!is.na(value)]),end=last(value[!is.na(value)]))%>%
    data.frame()
  
  # Identify sets with end datetime before start datetime or missing values
  errors_sten_34<<-ref2%>%
    filter(start>end | is.na(start) | is.na(end))%>%
    mutate(error_details=ifelse(start>end & !is.na(start) & !is.na(end),"End datetime < start datetime","Missing start or end datetime"))
  
  # Identify sets with start datetime before end of previous set end datetime
  errors_seq_34<<-ref2%>%
    group_by(TRIP_ID)%>%
    filter(start<lag(end) | is.na(start) | is.na(lag(end)))%>%
    filter(SET_NO>1)%>%
    mutate(error_details=ifelse(start<lag(end) & !is.na(start) & !is.na(lag(end)),"End datetime < start datetime","Missing start or previous end datetime"))%>%
    select(TRIP_ID, SET_NO, error_details)%>%
    data.frame()
  
  # Identify sets with sequence of datetimes unsorted within the set
  errors_withinsetseq_34<<-ref1%>%
    arrange(SET_NO)%>%
    group_by(TRIP_ID, SET_NO)%>%
    filter(SET_NO==1)%>%
    filter(!is.na(value))%>%
    summarise(sequence_error=is.unsorted(value, strictly=TRUE),error_details=ifelse(length(unique(value))==1,"Single date for all sets","Multiple dates"))%>%
    filter(sequence_error==TRUE)%>%
    data.frame()
    
  if(print_errors==TRUE){return(list(errors_sten_2, errors_seq_2))}
}

# ---- Run all datetime functions ----
datetime_all<-function(x=TRUE)
{
  datetime_0()
  datetime_1(print_errors=x)
  datetime_2(print_errors=x)
  datetime_34(print_errors=x)
}

datetime_0()
