#  title: "Function Script for BT Target FOrecasting"
#author: "Giollamhir"
# date: "June 28, 2017"
# ------


#Function to bring in BT population from ad hoc Adobe report
# OPERATIONAL:    
#####   Reformat dates to m/d/y DATE BEFORE importing. 
#####   Reformat numbers to numbers, with no comma separators or decimal points, BEFORE importing. 
#####   Adobe export formats are crap.
#####  Delete extraeous top rows and delete "totals" row before importing


getBTpop <- function(f) {
  
  tempset <- read.csv(file=f, skip = 1, header = F, stringsAsFactors=FALSE)
  headers = read.csv(file=f, skip = 0, header = F, nrows = 1, as.is = T)
  colnames(tempset)= headers
  tempset$Item  <- (as.Date(parse_date_time(tempset$Item, orders="mdy")))  
  # DATE FORMAT REMINDERS: 
  # Running as.date reformats to %Y/%m/%d format
  
  return(tempset)
  
}

#  IMPRESSION INGESTION STEP 1, normal ingestion (running on processed campaign reports from ad server)
#OPERATIONAL all impression files have to have impressions column converted to number format before ingestion

impIngest <- function(x) {
  tempset <- read.csv(file= x, stringsAsFactors=FALSE)
  tempset <- subset(tempset, select = c("Date","Impressions")) #cleanup unnecessary columns 
  tempset$Date  <- (as.Date(tempset$Date, "%m/%d/%Y"))  #sets format
  # Reminder: Running as.date reformats to %Y/%m/%d format
  # collapse all data to on sum of impressions per date 
  tempset2 <- tempset %>% group_by(Date) %>% dplyr::summarize(Imps = sum(Impressions))
  tempset <- tempset2
  
  

  return(tempset)
}  

#  IMPRESSION INGESTION STEP 2, normal ingestion
#For some reason, collapsing by date was not compatble with these proceses continuing in the same function 
# so I had to continue processing in a second, separate function: 

impProcess <- function(x,s,c,b) {
  tempset <- x
  #add Segment column and Campaign Column 
  tempset <- cbind(Camp = c, tempset)  
  tempset <- cbind(Seg = s, tempset)  
  
  # tempset$Impressions <- as.numeric(as.character(tempset$Impressions))
  
  #add the relevant BT population from the bt data
  tempset$BTpop <- b
  
  #add weekday column
  tempset$DoW <- weekdays(as.Date(tempset$Date, "%m/%d/%Y")) #weekdays() function is from lubridate library
  
  return(tempset)
}  

#Write the above into a function
#Graphing function taking inputs data frame, segment value string, and  subtitle text string
#For graphine Impressiosn per BTpop of a segment (with any number of campaigns)
chartSeg <- function(df,s,st) {  
  
  chart_title <- paste("Daily Impressions Per BT Population,June 2017, Segment", s)
  chart_subtitle <- st
 p <- ggplot(df, aes(x=BTpop, y=Imps, group=Camp, col=Camp)) +
    geom_point(size=0.8) + 
    labs(title = chart_title,
         subtitle = chart_subtitle,
         # caption = "Caption!",
         x = "BT Segment Population", y = "Impressions") +
    geom_smooth(se=FALSE) +
    geom_smooth(aes(group=Camp, col=Camp), se=FALSE, method="lm", size=0.65)
 
 return(p)
}

# chart as above but with a scale at the bottom threshold common to these segments
chartSegMinScale <- function(df,s,st) {  
  
  chart_title <- paste("Daily Impressions Per BT Population,June 2017, Segment", s)
  chart_subtitle <- st
  p <- ggplot(df, aes(x=BTpop, y=Imps, group=Camp, col=Camp)) +
    geom_point(size=0.8) + 
    labs(title = chart_title,
         subtitle = chart_subtitle,
         # caption = "Caption!",
         x = "BT Segment Population", y = "Impressions") +
    geom_smooth(se=FALSE) +
    geom_smooth(aes(group=Camp, col=Camp), se=FALSE, method="lm", size=0.65) + 
    scale_x_continuous(limits = c(0, 3500))  +
    scale_y_continuous(limits = c(0, 1250)) 
  
  return(p)
}

chartSegMaxScale <- function(df,s,st) {  
  
  chart_title <- paste("Daily Impressions Per BT Population,June 2017, Segment", s)
  chart_subtitle <- st
  p <- ggplot(df, aes(x=BTpop, y=Imps, group=Camp, col=Camp)) +
    geom_point(size=0.8) + 
    labs(title = chart_title,
         subtitle = chart_subtitle,
         # caption = "Caption!",
         x = "BT Segment Population", y = "Impressions") +
    geom_smooth(se=FALSE) +
    geom_smooth(aes(group=Camp, col=Camp), se=FALSE, method="lm", size=0.65) + 
    scale_x_continuous(limits = c(0, 8750))  +
    scale_y_continuous(limits = c(0, 3500)) 
  
  return(p)
}

# Returns graph similar to above but faceted by Day of the Week
chartSegDoW <- function(df,s,st) {  
  chart_title <- paste("Impressions Per BT Population,June 2017 Campaign", s)
  chart_subtitle <- st
  p <- ggplot(df, aes(x=BTpop, y=Imps, group=Camp, col=Camp)) +
    geom_line() +
    geom_point() + 
    labs(title = chart_title,
         subtitle = chart_subtitle,
         # caption = "Caption!",
         x = "BT Segment Population", y = "Impressions") +
    geom_smooth() +
    geom_smooth(aes(group=Camp), se=FALSE, method="lm", color="black") +
    facet_wrap(~DoW)  
  
  return(p)
}



# Reads Segment population data files 
# so test segment population can be used to predict potential impressions

newInput <- function(filename){  #filename is the input file for the segment, m is the linear model for the segment
  #first read the input file
  t <- read.csv(file=filename)
  t$Date  <- (as.Date(parse_date_time(t$Date, orders="mdy"))) 
  #add DoW
  t$DoW <- weekdays(as.Date(t$Date, "%Y/%m/%d"))
  
  df <- subset(t, select = c(BTpop,DoW))
  
  return(df)
  
}

# custom function to create linear models

bt_model <- function(df) {
  #df$DoW <- weekdays(as.Date(tempset$Date, "%m/%d/%Y")) #weekdays() function is from lubridate library
  lm(Imps ~ BTpop, data = df)
}


#  function to cycle through impression actifity dataframe and summarize total forecast for each campaign for the total time period matching observations/inputs

sumForecastData <- function(df) {
  n <- as.integer(round((sum(df)), digits = 0))
  return(n) 
}



# ESTIMATION FUNCTION
#create function for calculating an estimate based on coefficients in comparable segments
#takes inputs: 
# file name to read BT segment size for the segment to be estimated
# and n where n is the coefficient multiplier to use on the segment population 

roughEst <- function(f,n) {
  d <- read.csv(f)
  dest <- round(((sum(d$BTpop))/nrow(d))*30*n,0)
  sprintf("nrow(d) is", nrow(d), "Impression estimate is %s",dest)
  
  sprintf("Impression estimate is %s",dest)
  return(dest)
}