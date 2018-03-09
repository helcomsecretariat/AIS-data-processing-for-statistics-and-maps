#####################################################################
# 
# R Script to  pre-process AIS data 
# Developped by Florent NICOLAS, HELCOM Secretariat - 2017
# R Version 3.4.3
#
# Input data: yearly file of AIS signals / all messages
# Required data: shape file defining the limits of the Baltic Sea and the list of Maritime Identification Digits 
# Ouput data: monthly files of pre-processed AIS data and quality assessment
#
# !! The current script is processing the year XXXX To produce the same data for the year XXXX, replace XXXX by XXXX (CTRL + A and CTRL + F)
#
#####################################################################

# packages needed
install.packages("sp")
install.packages("rgdal")
install.packages("maps")
install.packages("maps")
install.packages("rworldmap")
install.packages("RgoogleMaps")
install.packages("plyr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("data.table")
install.packages("lubridate")
install.packages("zoo")
install.packages("geosphere")
install.packages("base")

# packages needed
library("sp")
library("rgdal")
install.packages("maps")
library("maps")
library("rworldmap")
library("RgoogleMaps")
library("plyr")
library("dplyr")
library("tidyr")
library("data.table")
library("lubridate")
library("zoo")
library("geosphere")
library("base")

##### the otput  are produced in the directory E:/test_division/division_finale2013/
# this one has to be changed manually (CTRL+F) to adjust the script to your directories


# Steps 1, 2 and 3 : Division mother file / cleaning the divisions / selection of relevant parameters ------------------------------------ 
rm(list = ls()) # clean environment

# start time to have time time needed for producing the AIS monthly files:
start.time <- Sys.time()

# import the polygons defining the Baltic Sea
exit_baltic <-readOGR("//10.10.10.210/e$/AIS_on_github/Baltic Sea limits","Limits_Baltic_Sea_AIS")

#Divide file in smaller text files and cleaning / selecting relevant data
conn=file("E:/helcom-log-2013.csv", open="r")

###########################################################################################################################################
# Enter the number of records (rows) per divisions:
n_rows= as.numeric(1000000)
###########################################################################################################################################

#read the first XXX rows:
data<-read.csv(conn, nrows=n_rows, head=T, fill=T,na.strings=c(""," ", "null", "NA"))
#add column name to the first file
colnames(data)<-c( "timestamp_pretty","timestamp","targetType","mmsi","msgid","posacc","lat","long","sog","cog","draught","name","dimBow",
                   "dimPort","dimStarboard","dimStern","shipTypeCargoTypeCode","shipType","shipCargo","destination","eta","imo","callsign")

dir.create(file.path("E:/test_division/division_finale2013/"), showWarnings=F)

i=1
while (length(data)>0) {
  #unique name file for each divisions:
  fn=paste("E:/test_division/division_finale2013/division_ais_2013.", i,sep=",", ".csv" )
  out_con=file(fn, open="w")
  #Remove the double quote sign in the division
  data$name <-  gsub("\"","",data$name)  
  data$destination <-  gsub("\"","",data$destination) 
  data$callsign <-  gsub("\"","",data$callsign) 
  data$msgid <-  gsub("\"","",data$msgid )
  data$targetType <-  gsub("\"","",data$targetType ) 
  data$mmsi <-  gsub("\"","",data$mmsi ) 
  data$lat <-  gsub("\"","",data$lat )
  data$long <-  gsub("\"","",data$long )
  data$sog <-  gsub("\"","",data$sog )
  data$cog <-  gsub("\"","",data$cog )
  data$shipType <-  gsub("\"","",data$shipType )
  data$dimBow <-  gsub("\"","",data$dimBow )
  data$draught <-  gsub("\"","",data$draught )
  data$dimPort <-  gsub("\"","",data$dimPort )
  data$dimStarboard <-  gsub("\"","",data$dimStarboard )
  data$dimStern <-  gsub("\"","",data$dimStern )
  data$imo <-  gsub("\"","",data$imo )
  
  #cleaning AIS data begins here
  
  #remove signals that are not from 2013
  data$year <- substr(data$timestamp_pretty,7,10) 
  data<-subset(data,data$year==2013)
  
  #remove duplication
  all_data_with_duplication<-nrow(data)
  data <- data[!duplicated(data), ]
  all_data_without_duplication<-nrow(data)
  
  all_AIS<-nrow(data)
  
  ###  
  # 1. Select the message ID
  data$msgid<-as.factor(data$msgid)
  data<-subset(data, msgid == 1 | msgid == 2 | msgid == 3 | msgid == 18 | msgid == 19 )
  
  Messages_for_shipping<-nrow(data)
  
  #### 
  # 2. MMSI and imo
  data$mmsi<-as.numeric(as.character(data$mmsi))
  data <- subset(data,  mmsi > 99999999 & mmsi < 999999999)
  wrongMMSI= c(000000000,111111111,222222222,333333333,444444444,555555555, 666666666, 777777777, 888888888,
               999999999,123456789,0,12345, 1193046) ############################################################# list to be updated
  wrongMMSI<-as.numeric(wrongMMSI)
  data<-subset(data,  mmsi!="wrongMMSI")
  
  data$imo<-as.numeric(as.character(data$imo))
  
  
  not_clean_imo<-sum(is.na(data$imo))
  data$imo[is.na(data$imo)] <- NA
  #for too big IMO numbers:
  data$imo[data$imo < 999999] <- NA
  #for too small IMO numbers:
  data$imo[data$imo >9999999] <- NA
  clean_imo<-sum(is.na(data$imo))
  
  wrong_imo<- clean_imo-not_clean_imo
  MMSI_registered<-nrow(data)
  
  # remove special characters from strings
  data$msgid <- gsub("([\\])","", data$msgid)
  data$msgid <- gsub("[][!#$%()*.:;<=>@^_`|~.{}]", "", data$msgid)
  
  data$targetType <- gsub("([\\])","", data$targetType)
  data$targetType <- gsub("[][!#$%()*.:;<=>@^_`|~.{}]", "", data$targetType)
  
  data$posacc <- gsub("([\\])","", data$posacc)
  data$posacc <- gsub("[][!#$%()*:;<=>@^_`|~.{}]", "", data$posacc)
  
  data$dimBow <- gsub("([\\])","", data$dimBow)
  data$dimBow <- gsub("[][!#$%()*:;<=>@^_`|~{}]", "", data$dimBow)
  
  data$dimPort <- gsub("([\\])","", data$dimPort)
  data$dimPort <- gsub("[][!#$%()*:;<=>@^_`|.{}]", "", data$dimPort)
  
  data$draught <- gsub("([\\])","", data$draught)
  data$draught <- gsub("[][!#$%()*:;<=>@^_`|~{}]", "", data$draught)
  
  data$dimStarboard <- gsub("([\\])","", data$dimStarboard)
  data$dimStarboard <- gsub("[][!#$%()*:;<=>@^_`|~{}]", "", data$dimStarboard)
  
  data$dimStern <- gsub("([\\])","", data$dimStern)
  data$dimStern <- gsub("[][!#$%()*:;<=>@^_`|~{}]", "", data$dimStern)
  
  ###
  # 3. add MID and flag
  data$MID <- substr(data$mmsi,1,3)
  MID <- read.csv("//hcvhost01/data/MID.csv", sep=";", quote="")
  data<-merge(data, MID, by="MID")
  
  #remove special characters
  data$country <- gsub("([\\])","", data$country)
  data$country <- gsub("[][!#$%()*.:;<=>@^_`|~.{}]", "", data$country)
  
  ###
  # 4. Addition of the columns month and week
  # addition on the month number in new column called month
  data$month <- substr(data$timestamp_pretty,4,5) 
  data$month <- as.numeric(data$month)
  
  # addition of column week
  library("ISOweek")
  data$date <- substr((as.factor(data$timestamp_pretty)),1,10)
  data$date <- as.Date(data$date,format = "%d/%m/%Y")
  data$week<-ISOweek(data$date)
  data$week<-substr((as.factor(data$week)),7,8)
  
  
  ###
  # 5. Selection of Baltic Sea
  library(ggplot2)
  library(sp)
  library(rgdal)
  
  ###
  # Prepare lat/long
  data <- subset(data, !(is.na(lat) ))
  data <- subset(data, !(is.na(long) ))
  data$lat<-as.numeric(as.character(data$lat))
  data$long<-as.numeric(as.character(data$long))
  data <- subset(data,  lat >= -90 & lat < 90) 
  data <- subset(data,  long > -180 & long < 180)
  
  coordinates(data) <- c("long", "lat")
  
  
  # to confirm the same reference system
  proj4string(data) <- proj4string(exit_baltic)
  
  # combine is.na() with over() to do the containment test; note that we
  # need to "demote" parks to a SpatialPolygons object first
  InsideBalticSea <- !is.na(over(data, as(exit_baltic, "SpatialPolygons")))
  mean(InsideBalticSea)
  
  data <- as.data.frame(data)
  data$InsideBalticSea <- InsideBalticSea
  
  InsideBalticSea <- nrow(subset(data, InsideBalticSea == 1))
  OutsideBalticSea <- nrow(subset(data, InsideBalticSea == 0))  
  
  
  #plot to check
  #data<- subset(data, data$into_baltic==TRUE)  
  #newmap <- getMap(resolution = "low")
  #plot(newmap, xlim = c(5.39, 30), ylim = c(52, 65.5), asp = 1, col="grey")
  #points(data$long, data$lat, col = "red", cex = 1)
  
  
  #old function:: 
  #definition of Baltic Sea
  # read coordinates of the polygon around the Baltic Sea
  # baltic <- read.csv("E:/test_division/DeleteAreaOutsideBaltic_Coordinates.txt", sep=";")
  # long=c(baltic $POINT_X)
  # lat=c(baltic $POINT_Y)
  # create df with 2 digits
  # baltic<- data.frame( lat, long)
  # baltic$lat <-  round(lat, digits = 2)
  # baltic$long <- round(long, digits = 2)
  # data$long<-as.numeric(as.character(data$long))
  # data$lat<-as.numeric(as.character(data$lat))
  
  # point in polygon
  # data$InsideBalticSea <- factor(point.in.polygon(data$long, data$lat, baltic$long, baltic$lat))
  #InsideBalticSea <- nrow(subset(data, InsideBalticSea == 1))
  #OutsideBalticSea <- nrow(subset(data, InsideBalticSea == 0))
  
  ###
  # 6. SOG
  data$sog <- as.numeric(as.character(data$sog))
  data <- subset(data,  sog >= 0 & sog < 800 )
  #summary(data$sog)
  
  clean_SOG<-nrow(data)
  
  ###
  # 7. COG
  data$cog <- as.numeric(as.character(data$cog))
  data <- subset(data,  cog >= 0 & cog < 360 )
  #summary(data$cog)
  
  clean_COG<-nrow(data)  
  
  ########## STEP III
  #subsetting the data
  
  #### 
  # 1.  IMO
  #data <- subset(data, !(is.na(imo) ))
  data$imo <- as.numeric(as.character(data$imo))
  #data <- subset(data,  imo > 999999 & imo < 9999999)
  #summary(data$imo)
  Imo_and_non_imo<-nrow(data)
  
  ####
  # 2. Baltic Sea
  data<-subset(data, InsideBalticSea == 1)
  InsideBalticSea<-nrow(data)
  
  # Selection of relevant information and export of data in csv files
  
  
  # as.characters and Selection of parameters
  data$dimBow<-as.character(data$dimBow)
  data$dimPort<-as.character(data$dimPort)
  data$dimStarboard<-as.character(data$dimStarboard)
  data$dimStern<-as.character(data$dimStern)
  data<- data[,c("timestamp_pretty","timestamp","msgid","targetType","mmsi", "lat","long", "posacc", "sog", "cog", "shipType", "dimBow", "draught","dimPort","dimStarboard","dimStern","month", "week", "imo", "country", "name", "callsign" )] 

  #stats report
  signals<-c(all_data_with_duplication,all_data_without_duplication ,Messages_for_shipping,MMSI_registered,wrong_imo,Imo_and_non_imo,clean_SOG,clean_COG, InsideBalticSea,OutsideBalticSea)
  parameters<-c("all_data_with_duplication","all_data_without_duplication","Messages_for_shipping","MMSI_registered","wrong_imo","Imo_and_non_imo","clean_SOG","clean_COG", "InsideBalticSea","OutsideBalticSea")
  stats<-data.frame(parameters,signals)
  
  
  # end cleaning AIS data
  
  #write tables
  stats_directory=paste("E:/test_division/division_finale2013/stats_ais_2013_", i, sep=",", ".csv" )
  write.table(stats, stats_directory, sep=",", col.names = T, row.names=F, quote=F)
  
  
  
  #### filter the signals / month: 1 file in a monthly folder
  
  data_2013_january<-data[data$month == 1, ]
  data_2013_january <- subset(data_2013_january, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2013/january"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2013_january)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2013/january/data_2013_januaryfinal", sep="_",i, ".csv")
    write.table (data_2013_january, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
  
  
  data_2013_february<-data[data$month == 2, ]
  data_2013_february <- subset(data_2013_february, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2013/february"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2013_february)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2013/february/data_2013_februaryfinal", sep="_",i, ".csv")
    write.table (data_2013_february, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
  
  
  ###march 03
  data_2013_march<-data[data$month == 3, ]
  data_2013_march <- subset(data_2013_march, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2013/march"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2013_march)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2013/march/data_2013_marchfinal", sep="_",i, ".csv")
    write.table (data_2013_march, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
  
  
  ###april 04
  data_2013_april<-data[data$month == 4, ]
  dir.create(file.path("E:/test_division/division_finale2013/april"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2013_april)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2013/april/data_2013_aprilfinal", sep="_",i, ".csv")
    write.table (data_2013_april, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
  
  
  ###may 05
  data_2013_may<-data[data$month == 5, ]
  data_2013_may <- subset(data_2013_may, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2013/may"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2013_may)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2013/may/data_2013_mayfinal", sep="_",i, ".csv")
    write.table (data_2013_may, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
  
  
  
  ###june 06
  data_2013_june<-data[data$month == 6, ]
  data_2013_june <- subset(data_2013_june, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2013/june"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2013_june)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2013/june/data_2013_junefinal", sep="_",i, ".csv")
    write.table (data_2013_june, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
  
  
  ###july 07
  data_2013_july<-data[data$month == 7, ]
  data_2013_july <- subset(data_2013_july, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2013/july"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2013_july)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2013/july/data_2013_julyfinal", sep="_",i, ".csv")
    write.table (data_2013_july, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
  
  
  ###august 08
  data_2013_august<-data[data$month == 8, ]
  data_2013_august <- subset(data_2013_august, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2013/august"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2013_august)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2013/august/data_2013_augustfinal", sep="_",i, ".csv")
    write.table (data_2013_august, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
  
  
  
  
  ###september 09
  data_2013_september<-data[data$month == 9, ]
  data_2013_september <- subset(data_2013_september, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2013/september"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2013_september)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2013/september/data_2013_septemberfinal", sep="_",i, ".csv")
    write.table (data_2013_september, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
  
  
  
  ###october 10
  data_2013_october<-data[data$month == 10, ]
  data_2013_october <- subset(data_2013_october, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2013/october"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2013_october)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2013/october/data_2013_octoberfinal", sep="_",i, ".csv")
    write.table (data_2013_october, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
  
  
  ###november 11
  data_2013_november<-data[data$month == 11, ]
  data_2013_november <- subset(data_2013_november, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2013/november"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2013_november)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2013/november/data_2013_novemberfinal", sep="_",i, ".csv")
    write.table (data_2013_november, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
  
  
  ###december 12
  data_2013_december<-data[data$month == 12, ]
  data_2013_december <- subset(data_2013_december, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2013/december"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2013_december)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2013/december/data_2013_decemberfinal", sep="_",i, ".csv")
    write.table (data_2013_december, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
  
  
  ###   going to the next division 
  
  
  
  #ais_directory=paste("E:/test_division/division_finale2013/division_ais_2013.", i,sep=",", ".csv" )
  #write.table(data, ais_directory, row.names=F, sep=",", col.names=T)  
  #close(out_con)
  
  #read the next files of n_rows rows
  data<-read.csv(conn, nrows=n_rows,fill=T,na.strings=c(""," ", "null", "NA"))
  #add column names for these divisions
  colnames(data)<-c( "timestamp_pretty","timestamp","targetType","mmsi","msgid","posacc","lat","long","sog","cog","draught","name","dimBow",
                     "dimPort","dimStarboard","dimStern","shipTypeCargoTypeCode","shipType","shipCargo","destination","eta","imo","callsign")
  i=i+1
}
close(conn)

#mail notification
library(mail)
##sendmail("your_email@domain.domain", "2013 is divided and CLEAN with new method", "2013 is divided and CLEAN", password="rmail")






# Steps 4: Sorting and merging the division into monthly files -----------------------------------------------

###January 01 Merging all files in each month_folder to have 1 file per month
rm(list = ls()[!ls() %in% c("start.time")])
setwd("E:/test_division/division_finale2013/january")
fileList <- list.files(pattern="data_2013_januaryfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(as.character(data$imo))
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
#for too big IMO numbers:
data$imo[data$imo < 999999] <- NA
#for too small IMO numbers:
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("january")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2013/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2013/file_list_months_weeks/duplication_january.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2013/file_list_months_weeks/ais_january_2013.csv")
write.table (data, directory_final, row.names=F, sep=",")

library(mail)
###sendmail("florent.nicolas@helcom.fi", "AIS january  2013 is merged", " AIS january 2013 is merged", password="rmail")
 

###february 02 Merging all files in each month_folder to have 1 file per month
rm(list = ls()[!ls() %in% c("start.time")])
setwd("E:/test_division/division_finale2013/february")
fileList <- list.files(pattern="data_2013_februaryfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
#for too big IMO numbers:
data$imo[data$imo < 999999] <- NA
#for too small IMO numbers:
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("february")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2013/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2013/file_list_months_weeks/duplication_february.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2013/file_list_months_weeks/ais_february_2013.csv")
write.table (data, directory_final, row.names=F, sep=",")

###march 03 Merging all files in each month_folder to have 1 file per month
rm(list = ls()[!ls() %in% c("start.time")])
setwd("E:/test_division/division_finale2013/march")
fileList <- list.files(pattern="data_2013_marchfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
#for too big IMO numbers:
data$imo[data$imo < 999999] <- NA
#for too small IMO numbers:
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("march")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2013/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2013/file_list_months_weeks/duplication_march.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2013/file_list_months_weeks/ais_march_2013.csv")
write.table (data, directory_final, row.names=F, sep=",")

library(mail)
###sendmail("florent.nicolas@helcom.fi", "AIS march  2013 is merged", " AIS march 2013 is merged", password="rmail")

###april 04 Merging all files in each month_folder to have 1 file per month
rm(list = ls()[!ls() %in% c("start.time")])
setwd("E:/test_division/division_finale2013/april")
fileList <- list.files(pattern="data_2013_aprilfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
#for too big IMO numbers:
data$imo[data$imo < 999999] <- NA
#for too small IMO numbers:
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("april")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2013/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2013/file_list_months_weeks/duplication_april.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2013/file_list_months_weeks/ais_april_2013.csv")
write.table (data, directory_final, row.names=F, sep=",")


###may 05 Merging all files in each month_folder to have 1 file per month
rm(list = ls()[!ls() %in% c("start.time")])
setwd("E:/test_division/division_finale2013/may")
fileList <- list.files(pattern="data_2013_mayfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
#for too big IMO numbers:
data$imo[data$imo < 999999] <- NA
#for too small IMO numbers:
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("may")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2013/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2013/file_list_months_weeks/duplication_may.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2013/file_list_months_weeks/ais_may_2013.csv")
write.table (data, directory_final, row.names=F, sep=",")

library(mail)
###sendmail("florent.nicolas@helcom.fi", "AIS may  2013 is merged", " AIS may 2013 is merged", password="rmail")
 


###june 06 Merging all files in each month_folder to have 1 file per month
rm(list = ls()[!ls() %in% c("start.time")])
setwd("E:/test_division/division_finale2013/june")
fileList <- list.files(pattern="data_2013_junefinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
#for too big IMO numbers:
data$imo[data$imo < 999999] <- NA
#for too small IMO numbers:
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("june")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2013/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2013/file_list_months_weeks/duplication_june.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2013/file_list_months_weeks/ais_june_2013.csv")
write.table (data, directory_final, row.names=F, sep=",")



###july 07 Merging all files in each month_folder to have 1 file per month
rm(list = ls()[!ls() %in% c("start.time")])
setwd("E:/test_division/division_finale2013/july")
fileList <- list.files(pattern="data_2013_julyfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
#for too big IMO numbers:
data$imo[data$imo < 999999] <- NA
#for too small IMO numbers:
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("july")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2013/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2013/file_list_months_weeks/duplication_july.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2013/file_list_months_weeks/ais_july_2013.csv")
write.table (data, directory_final, row.names=F, sep=",")



###august 08
rm(list = ls()[!ls() %in% c("start.time")])
setwd("E:/test_division/division_finale2013/august")
fileList <- list.files(pattern="data_2013_augustfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
#for too big IMO numbers:
data$imo[data$imo < 999999] <- NA
#for too small IMO numbers:
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("august")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2013/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2013/file_list_months_weeks/duplication_august.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2013/file_list_months_weeks/ais_august_2013.csv")
write.table (data, directory_final, row.names=F, sep=",")

library(mail)
###sendmail("florent.nicolas@helcom.fi", "AIS august  2013 is merged", " AIS august 2013 is merged", password="rmail")
 


###september 09 Merging all files in each month_folder to have 1 file per month
rm(list = ls()[!ls() %in% c("start.time")])
setwd("E:/test_division/division_finale2013/september")
fileList <- list.files(pattern="data_2013_septemberfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
#for too big IMO numbers:
data$imo[data$imo < 999999] <- NA
#for too small IMO numbers:
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("september")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2013/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2013/file_list_months_weeks/duplication_september.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2013/file_list_months_weeks/ais_september_2013.csv")
write.table (data, directory_final, row.names=F, sep=",")




###october 10 Merging all files in each month_folder to have 1 file per month
rm(list = ls()[!ls() %in% c("start.time")])
setwd("E:/test_division/division_finale2013/october")
fileList <- list.files(pattern="data_2013_octoberfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
#for too big IMO numbers:
data$imo[data$imo < 999999] <- NA
#for too small IMO numbers:
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("october")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2013/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2013/file_list_months_weeks/duplication_october.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2013/file_list_months_weeks/ais_october_2013.csv")
write.table (data, directory_final, row.names=F, sep=",")

###sendmail("florent.nicolas@helcom.fi", "AIS october  2013 is merged", " AIS october 2013 is merged", password="rmail")
 

###november 11 Merging all files in each month_folder to have 1 file per month
rm(list = ls()[!ls() %in% c("start.time")])
setwd("E:/test_division/division_finale2013/november")
fileList <- list.files(pattern="data_2013_novemberfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
#for too big IMO numbers:
data$imo[data$imo < 999999] <- NA
#for too small IMO numbers:
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("november")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2013/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2013/file_list_months_weeks/duplication_november.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2013/file_list_months_weeks/ais_november_2013.csv")
write.table (data, directory_final, row.names=F, sep=",")

 


###december 12 Merging all files in each month_folder to have 1 file per month
rm(list = ls()[!ls() %in% c("start.time")])
setwd("E:/test_division/division_finale2013/december")
fileList <- list.files(pattern="data_2013_decemberfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
#for too big IMO numbers:
data$imo[data$imo < 999999] <- NA
#for too small IMO numbers:
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("december")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2013/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2013/file_list_months_weeks/duplication_december.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2013/file_list_months_weeks/ais_december_2013.csv")
write.table (data, directory_final, row.names=F, sep=",")

library(mail)
###sendmail("florent.nicolas@helcom.fi", "AIS december  2013 is merged", " AIS december 2013 is merged", password="rmail")
rm(list = ls()[!ls() %in% c("start.time")])






# Step 5: division of monthly files in weekly files (optional) -----------------------

### table per week new test (14082013):
# taking lot of time to generate, not efficient


#rm(list = ls())
#setwd("E:/test_division/division_finale2013/file_list_months_weeks")
#fileList <- list.files(pattern="scope_ais_.*\\.csv", recursive=FALSE)
#for (i in 1:52){
# for (file in fileList){
#data_2013<-read.table(file, header=T, fill=T, sep=",")
#data_2013<-data_2013[data_2013$week == i, ]
#nrow<-as.numeric(nrow(data_2013))  
#if(nrow>0){
#data_2013 <- data_2013[order(data_2013$timestamp_pretty) , ]
#dir.create(file.path("E:/test_division/division_finale2013/file_list_months_weeks"), showWarnings=F)

#time in UTC GMT seconds in the filename to avoir overwritting
#install.packages("stringr")
#a<-Sys.time()
#library(stringr)
#a<-str_replace_all(a, fixed(" "), "_")
#a<-str_replace_all(a, fixed(":"), "_")
#a<-str_replace_all(a, fixed("-"), "_")

#directory_divisions=paste("E:/test_division/division_finale2013/file_list_months_weeks/data_2013_scope_week",i, sep="_", a, ".csv")
#write.table (data_2013, directory_divisions, row.names=F, sep=",")
#} else {
#}  } }  

#library(mail)
###sendmail("florent.nicolas@helcom.fi", "AIS monthly and weekly files ready", "The final step is done: weekly files are ready (I hope so...). weekly and monthly files WITHOUT DUPLICATION are available", password="rmail")

# Empty the environment and close R ---------------------------------------

# delete temp files (divisions)
setwd("E:/test_division/division_finale2013/")
#fileList <- list.files(pattern="division_ais_2013.*\\.csv", recursive=FALSE)
#for (file in fileList) {
#unlink(file)
#}


# time taken to process
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken 
str(time.taken)
unit <- attr(time.taken, "units")
time.taken <- paste(time.taken,unit,"")

start.time
end.time
time.taken

#save time taken
year <- 2013

dir.create(file.path("E:/test_division/time to process"), showWarnings=F)
directory_time.taken <- paste("E:/test_division/time to process/time_to_pre_process_" , year, sep="", ".csv" )
write.table(time.taken, directory_time.taken, sep=";", col.names = F, row.names=F)


#empty temp memory and close R
#rm(list = ls())
#rm(list = ls())
#quit(save = "yes")