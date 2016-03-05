setwd("~/DSTribune/Stories/PorterRanch")


#SITE
#http://www.arb.ca.gov/aqmis2/display.php?year=2015&mon=3&day=3&param=CH4&units=007&statistic=DAVG&order=basin%2Ccounty_name%2Cname&county_name=--COUNTY--&basin=SC-South+Coast&latitude=--PART+OF+STATE--&o3switch=new&hours=all&ptype=aqd&report=7DAY&btnsubmit=Update+Display

#QUICK DOWNLOAD

Molecules = c("CH4","NO2","NO","CO")

#Start Date - Get year, month, day   in 7 day increments

#methane
download.file("http://www.arb.ca.gov/aqmis2/display.php?download=y&year=2015&mon=3&day=4&param=CH4&units=007&statistic=DAVG&order=basin%2Ccounty_name%2Cname&county_name=--COUNTY--&basin=SC-South+Coast&latitude=--PART+OF+STATE--&o3switch=new&hours=all&ptype=aqd&report=7DAY&btnsubmit=Update+Display",destfile="test10")


#NO2
download.file("http://www.arb.ca.gov/aqmis2/display.php?download=y&year=2015&mon=3&day=4&param=NO2&units=007&statistic=DAVG&order=basin%2Ccounty_name%2Cname&county_name=--COUNTY--&basin=SC-South+Coast&latitude=--PART+OF+STATE--&o3switch=new&hours=all&ptype=aqd&report=7DAY&btnsubmit=Update+Display",destfile="test10")

#NO
#http://www.arb.ca.gov/aqmis2/display.php?download=y&year=2015&mon=3&day=3&param=NO&units=007&statistic=DAVG&order=basin%2Ccounty_name%2Cname&county_name=--COUNTY--&basin=SC-South+Coast&latitude=--PART+OF+STATE--&o3switch=new&hours=all&ptype=aqd&report=7DAY&btnsubmit=Update+Display

#CO
#http://www.arb.ca.gov/aqmis2/display.php?download=y&year=2015&mon=3&day=3&param=CO&units=007&statistic=DAVG&order=basin%2Ccounty_name%2Cname&county_name=--COUNTY--&basin=SC-South+Coast&latitude=--PART+OF+STATE--&o3switch=new&hours=all&ptype=aqd&report=7DAY&btnsubmit=Update+Display






#http://www.arb.ca.gov/aqmis2/display.php?download=y&param=CH4&units=007&year=2016&report=SITELIST&o3area=&o3pa8=&county_name=--COUNTY--&latitude=S-Southern&basin=--AIR+BASIN--&order=basin,county_name,s.name&ptype=aqd
download.file(paste("http://www.arb.ca.gov/aqmis2/display.php?download=y&param=CH4&units=007&year=2016&month=01&report=SITELIST&o3area=&o3pa8=&county_name=--COUNTY--&latitude=S-Southern&basin=--AIR+BASIN--&order=basin,county_name,s.name&ptype=aqd",sep=""),destfile = "Methane3.csv")



#Methane Cycle - https://environmentofearth.wordpress.com/2009/09/13/methane-cycle-in-atmosphere/
#Methane #Param Code: 4300

#Oxidation of methane and formation of formaldehyde  #Param Code: 43502
#Byproducts include NO -> NO2:   NO2: 42602    NO:  42601

#Oxidation of formaldehyde and formation of carbon monoxide #Param Code: Audit Levels 42101

#Oxidation of carbon monoxide and formation of carbon dioxide   


#LatLongBin
#Methane
download.file(paste("https://aqs.epa.gov/api/rawData?user=",user,"&pw=", pass, "&format=DMCSV&param=4300&bdate=",startDate,"&edate=",endDate,"&minlat=",minLat,"&maxlat=",maxLat,"&minlon=",minLong,"&maxlon=",maxLong,sep=""),destfile = "Methane.csv")

#Formaldehyde
download.file(paste("https://aqs.epa.gov/api/rawData?user=",user,"&pw=", pass, "&format=DMCSV&param=43502&bdate=",startDate,"&edate=",endDate,"&minlat=",minLat,"&maxlat=",maxLat,"&minlon=",minLong,"&maxlon=",maxLong,sep=""),destfile = "Formaldehyde.csv")

#NO2
download.file(paste("https://aqs.epa.gov/api/rawData?user=",user,"&pw=", pass, "&format=DMCSV&param=42602&bdate=",startDate,"&edate=",endDate,"&minlat=",minLat,"&maxlat=",maxLat,"&minlon=",minLong,"&maxlon=",maxLong,sep=""),destfile = "NO2.csv")

#NO
download.file(paste("https://aqs.epa.gov/api/rawData?user=",user,"&pw=", pass, "&format=DMCSV&param=42601&bdate=",startDate,"&edate=",endDate,"&minlat=",minLat,"&maxlat=",maxLat,"&minlon=",minLong,"&maxlon=",maxLong,sep=""),destfile = "NO.csv")

#CO
download.file(paste("https://aqs.epa.gov/api/rawData?user=",user,"&pw=", pass, "&format=DMCSV&param=42101&bdate=",startDate,"&edate=",endDate,"&minlat=",minLat,"&maxlat=",maxLat,"&minlon=",minLong,"&maxlon=",maxLong,sep=""),destfile = "CO.csv")



setwd("~/DSTribune/Stories/PorterRanch")
library(ggplot2)
library(dplyr)

NO2 <- read.csv("NO2.csv")
NO <- read.csv("NO.csv")
fmld <- read.csv("Formaldehyde.csv")

NO2$DateTime <- paste(NO2$Date.GMT, NO$X24.Hour.GMT, sep=" ")
NO2$DateTime <-   as.POSIXct(NO2$DateTime, format="%Y-%m-%d %H:%M",tz = "GMT") 
NO2$YearMonthDay <- as.factor(format(NO2$DateTime,'%Y-%m-%d'))

  NO2Daily <- NO2 %>%
              group_by(YearMonthDay) %>%
                  summarise(Avg = mean(Sample.Measurement, na.rm = TRUE),
                            Max = max(Sample.Measurement, na.rm = TRUE),
                            Median = median(Sample.Measurement, na.rm = TRUE),
                            totalSamples = n(),
                            stdError = sd(Sample.Measurement, na.rm = TRUE))
  
  NO2Daily$DateTime <- as.POSIXct(NO2Daily$YearMonthDay, format="%Y-%m-%d",tz = "GMT") 

ggplot(data=NO2Daily, aes(x = DateTime, y = Max)) + geom_line()

