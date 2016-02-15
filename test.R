

############ Script to process AIS data for the year 2010, HELCOM Secretariat (www.helcom.fi - 2016)
# to process another year, replace 2010 by the year you want (CTRL+A to select all the script and CTRL+F to replace )


############ the input is the decoded AIS data in csv (= AIS mother file).
############ the outputs are monthly files (step 4) or weekly files (step 5 if needed)


# Steps 1, 2 and 3 : Division mother file / cleaning the divisions / selection of relevant parameters ------------------------------------ 

#Divide file in smaller text files and cleaning / selecting relevant data

rm(list = ls())
conn=file("//HCVHOST01/data/Data DMA/helcom-log-2010.csv", open="r")
#read the first 1 000 000 rows:
data<-read.csv(conn, nrows=1000000, head=T, fill=T,na.strings=c(""," ", "null", "NA"))
#add column name to the first file
colnames(data)<-c( "timestamp_pretty","timestamp","targetType","mmsi","msgid","posacc","lat","long","sog","cog","draught","name","dimBow",
                   "dimPort","dimStarboard","dimStern","shipTypeCargoTypeCode","shipType","shipCargo","destination","eta","imo","callsign")
i=1
while (length(data)>0) {
  #unique name file for each divisions:
  fn=paste("E:/test_division/division_finale2010/division_ais_2010.", i,sep=",", ".csv" )
  out_con=file(fn, open="w")
  #Remove the double quote sign for the divisions
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
  
  #remove signals that are not from 2010
  data$year <- substr(data$timestamp_pretty,7,10) 
  data<-subset(data,data$year==2010)
  
  #remove duplication
  all_data_with_duplication<-nrow(data)
  data <- data[!duplicated(data), ]
  all_data_without_duplication<-nrow(data)
  
  all_AIS<-nrow(data)
  
  ####  
  # 1. Select the message ID
  data$msgid<-as.factor(data$msgid)
  data<-subset(data, msgid == 1 | msgid == 2 | msgid == 3 | msgid == 18 | msgid == 19 )
  
  Messages_for_shipping<-nrow(data)
  
  #### 
  # 2. MMSI and imo
  data$mmsi<-as.numeric(as.character(data$mmsi))
  data <- subset(data,  mmsi > 99999999 & mmsi < 999999999)
  wrongMMSI= c(000000000,111111111,222222222,333333333,444444444,555555555, 666666666, 777777777, 888888888,
               999999999,123456789,0,12345, 1193046) 
  wrongMMSI<-as.numeric(wrongMMSI)
  data<-subset(data,  mmsi!="wrongMMSI")
  
  data$imo<-as.numeric( data$imo)
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
  
  # addition of column week
  library("ISOweek")
  data$date <- substr((as.factor(data$timestamp_pretty)),1,10)
  data$date <- as.Date(data$date,format = "%d/%m/%Y")
  library("ISOweek")
  data$week<-ISOweek(data$date)
  data$week<-substr((as.factor(data$week)),7,8)
  
  
  ###
  # 5. Selection of Baltic Sea
  library(ggplot2)
  library(sp)
  library(rgdal)
  
  #definition of Baltic Sea
  # read coordinates of the polygon around the Baltic Sea
  baltic <- read.csv("E:/test_division/DeleteAreaOutsideBaltic_Coordinates.txt", sep=";")
  long=c(baltic $POINT_X)
  lat=c(baltic $POINT_Y)
  # create df with 2 digits
  baltic<- data.frame( lat, long)
  baltic$lat <-  round(lat, digits = 2)
  baltic$long <- round(long, digits = 2)
  data$long<-as.numeric(as.character(data$long))
  data$lat<-as.numeric(as.character(data$lat))
  
  # point in polygon
  data$InsideBalticSea <- factor(point.in.polygon(data$long, data$lat, baltic$long, baltic$lat))
  #InsideBalticSea <- nrow(subset(data, InsideBalticSea == 1))
  OutsideBalticSea <- nrow(subset(data, InsideBalticSea == 0))
  
  ###
  # 6. SOG
  data$sog <- as.numeric(data$sog)
  data <- subset(data,  sog >= 0 & sog < 800 )
  #summary(data$sog)
  
  clean_SOG<-nrow(data)
  
  ###
  # 7. COG
  data$cog <- as.numeric(data$cog)
  data <- subset(data,  cog >= 0 & cog < 360 )
  #summary(data$cog)
  
  clean_COG<-nrow(data)  
  
  ###
  # 8. lat/long
  data <- subset(data, !(is.na(lat) ))
  data <- subset(data, !(is.na(long) ))
  data$lat<-as.numeric(as.character(data$lat))
  data$long<-as.numeric(as.character(data$long))
  
  ########## STEP III
  #subsetting the data
  
  #### 
  # 1.  IMO
  #data <- subset(data, !(is.na(imo) ))
  data$imo <- as.numeric(as.character(data$imo))
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
  data<- data[,c("timestamp_pretty","timestamp","msgid","targetType","mmsi", "lat","long", "posacc", "sog", "cog", "shipType", "dimBow", "draught","dimPort","dimStarboard","dimStern","month", "week", "imo", "country")] 
  
  #stats report
  signals<-c(all_data_with_duplication,all_data_without_duplication ,Messages_for_shipping,MMSI_registered,wrong_imo,Imo_and_non_imo,clean_SOG,clean_COG, InsideBalticSea,OutsideBalticSea)
  parameters<-c("all_data_with_duplication","all_data_without_duplication","Messages_for_shipping","MMSI_registered","wrong_imo","Imo_and_non_imo","clean_SOG","clean_COG", "InsideBalticSea","OutsideBalticSea")
  stats<-data.frame(parameters,signals)
  
  # end cleaning AIS data
  
  #write tables
  stats_directory=paste("E:/test_division/division_finale2010/stats_ais_2010_", i, sep=",", ".csv" )
  write.table(stats, stats_directory, sep=",", col.names = T, row.names=F, quote=F)
  
  ais_directory=paste("E:/test_division/division_finale2010/division_ais_2010.", i,sep=",", ".csv" )
  write.table(data, ais_directory, row.names=F, sep=",", col.names=T)  
  close(out_con)
  
  #read the next files of 1000000 rows
  data<-read.csv(conn, nrows=1000000,fill=T,na.strings=c(""," ", "null", "NA"))
  #add column names for these divisions
  colnames(data)<-c( "timestamp_pretty","timestamp","targetType","mmsi","msgid","posacc","lat","long","sog","cog","draught","name","dimBow",
                     "dimPort","dimStarboard","dimStern","shipTypeCargoTypeCode","shipType","shipCargo","destination","eta","imo","callsign")
  i=i+1
}
close(conn)

#mail notification
library(mail)
sendmail("florent.nicolas@helcom.fi", "2010 is divided and CLEAN with new method", "2010 is divided and CLEAN", password="rmail")

# Steps 4: Sorting and merging the division into monthly files -----------------------------------------------



########## SCRIPT to produce monthly and weekly files, year 2010

###january 01
# 1. sorting by month in different folder: 1 folrm(list = ls())
setwd("E:/test_division/division_finale2010/")
fileList <- list.files(pattern="division_ais_2010.*\\.csv", recursive=FALSE)
i=1
for (file in fileList) {
  data2010<-read.table(file, header=T, fill=T, sep=",")
  data_2010_january<-data2010[data2010$month == 1, ]
  dir.create(file.path("E:/test_division/division_finale2010/january"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2010_january)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2010/january/data_2010_januaryfinal", sep="_",i, ".csv")
    write.table (data_2010_january, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
}


#2. Merging all files in each month_folder to have 1 file per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/january")
fileList <- list.files(pattern="data_2010_januaryfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
data$imo[data$imo < 999999] <- NA
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("january")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2010/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2010/file_list_months_weeks/duplication_january.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2010/file_list_months_weeks/ais_january_2010.csv")
write.table (data, directory_final, row.names=F, sep=",")

###february 02
# 1. sorting by month in different folder: 1 folder per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/")
fileList <- list.files(pattern="division_ais_2010.*\\.csv", recursive=FALSE)
i=1
for (file in fileList) {
  data2010<-read.table(file, header=T, fill=T, sep=",")
  data_2010_february<-data2010[data2010$month == 2, ]
  data_2010_february <- subset(data_2010_february, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2010/february"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2010_february)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2010/february/data_2010_februaryfinal", sep="_",i, ".csv")
    write.table (data_2010_february, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
}

#2. Merging all files in each month_folder to have 1 file per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/february")
fileList <- list.files(pattern="data_2010_februaryfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
data$imo[data$imo < 999999] <- NA
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("february")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2010/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2010/file_list_months_weeks/duplication_february.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2010/file_list_months_weeks/ais_february_2010.csv")
write.table (data, directory_final, row.names=F, sep=",")

###march 03
# 1. sorting by month in different folder: 1 folder per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/")
fileList <- list.files(pattern="division_ais_2010.*\\.csv", recursive=FALSE)
i=1
for (file in fileList) {
  data2010<-read.table(file, header=T, fill=T, sep=",")
  data_2010_march<-data2010[data2010$month == 3, ]
  data_2010_march <- subset(data_2010_march, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2010/march"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2010_march)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2010/march/data_2010_marchfinal", sep="_",i, ".csv")
    write.table (data_2010_march, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
}

#2. Merging all files in each month_folder to have 1 file per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/march")
fileList <- list.files(pattern="data_2010_marchfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
data$imo[data$imo < 999999] <- NA
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("march")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2010/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2010/file_list_months_weeks/duplication_march.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2010/file_list_months_weeks/ais_march_2010.csv")
write.table (data, directory_final, row.names=F, sep=",")

###april 04
# 1. sorting by month in different folder: 1 folder per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/")
fileList <- list.files(pattern="division_ais_2010.*\\.csv", recursive=FALSE)
i=1
for (file in fileList) {
  data2010<-read.table(file, header=T, fill=T, sep=",")
  data_2010_april<-data2010[data2010$month == 4, ]
  dir.create(file.path("E:/test_division/division_finale2010/april"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2010_april)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2010/april/data_2010_aprilfinal", sep="_",i, ".csv")
    write.table (data_2010_april, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
}

#2. Merging all files in each month_folder to have 1 file per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/april")
fileList <- list.files(pattern="data_2010_aprilfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
data$imo[data$imo < 999999] <- NA
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("april")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2010/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2010/file_list_months_weeks/duplication_april.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2010/file_list_months_weeks/ais_april_2010.csv")
write.table (data, directory_final, row.names=F, sep=",")

###may 05
# 1. sorting by month in different folder: 1 folder per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/")
fileList <- list.files(pattern="division_ais_2010.*\\.csv", recursive=FALSE)
i=1
for (file in fileList) {
  data2010<-read.table(file, header=T, fill=T, sep=",")
  data_2010_may<-data2010[data2010$month == 5, ]
  data_2010_may <- subset(data_2010_may, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2010/may"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2010_may)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2010/may/data_2010_mayfinal", sep="_",i, ".csv")
    write.table (data_2010_may, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
}

#2. Merging all files in each month_folder to have 1 file per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/may")
fileList <- list.files(pattern="data_2010_mayfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
data$imo[data$imo < 999999] <- NA
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("may")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2010/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2010/file_list_months_weeks/duplication_may.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2010/file_list_months_weeks/ais_may_2010.csv")


###june 06
# 1. sorting by month in different folder: 1 folder per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/")
fileList <- list.files(pattern="division_ais_2010.*\\.csv", recursive=FALSE)
i=1
for (file in fileList) {
  data2010<-read.table(file, header=T, fill=T, sep=",")
  data_2010_june<-data2010[data2010$month == 6, ]
  data_2010_june <- subset(data_2010_june, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2010/june"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2010_june)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2010/june/data_2010_junefinal", sep="_",i, ".csv")
    write.table (data_2010_june, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
}

#2. Merging all files in each month_folder to have 1 file per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/june")
fileList <- list.files(pattern="data_2010_junefinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
data$imo[data$imo < 999999] <- NA
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("june")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2010/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2010/file_list_months_weeks/duplication_june.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2010/file_list_months_weeks/ais_june_2010.csv")
write.table (data, directory_final, row.names=F, sep=",")


###july 07
# 1. sorting by month in different folder: 1 folder per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/")
fileList <- list.files(pattern="division_ais_2010.*\\.csv", recursive=FALSE)
i=1
for (file in fileList) {
  data2010<-read.table(file, header=T, fill=T, sep=",")
  data_2010_july<-data2010[data2010$month == 7, ]
  data_2010_july <- subset(data_2010_july, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2010/july"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2010_july)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2010/july/data_2010_julyfinal", sep="_",i, ".csv")
    write.table (data_2010_july, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
}

#2. Merging all files in each month_folder to have 1 file per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/july")
fileList <- list.files(pattern="data_2010_julyfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
data$imo[data$imo < 999999] <- NA
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("july")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2010/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2010/file_list_months_weeks/duplication_july.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2010/file_list_months_weeks/ais_july_2010.csv")
write.table (data, directory_final, row.names=F, sep=",")

rm(list = ls())


###august 08
# 1. sorting by month in different folder: 1 folder per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/")
fileList <- list.files(pattern="division_ais_2010.*\\.csv", recursive=FALSE)
i=1
for (file in fileList) {
  data2010<-read.table(file, header=T, fill=T, sep=",")
  data_2010_august<-data2010[data2010$month == 8, ]
  data_2010_august <- subset(data_2010_august, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2010/august"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2010_august)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2010/august/data_2010_augustfinal", sep="_",i, ".csv")
    write.table (data_2010_august, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
}

#2. Merging all files in each month_folder to have 1 file per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/august")
fileList <- list.files(pattern="data_2010_augustfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
data$imo[data$imo < 999999] <- NA
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("august")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2010/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2010/file_list_months_weeks/duplication_august.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2010/file_list_months_weeks/ais_august_2010.csv")
write.table (data, directory_final, row.names=F, sep=",")



###september 09
# 1. sorting by month in different folder: 1 folder per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/")
fileList <- list.files(pattern="division_ais_2010.*\\.csv", recursive=FALSE)
i=1
for (file in fileList) {
  data2010<-read.table(file, header=T, fill=T, sep=",")
  data_2010_september<-data2010[data2010$month == 9, ]
  data_2010_september <- subset(data_2010_september, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2010/september"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2010_september)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2010/september/data_2010_septemberfinal", sep="_",i, ".csv")
    write.table (data_2010_september, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
}

#2. Merging all files in each month_folder to have 1 file per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/september")
fileList <- list.files(pattern="data_2010_septemberfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
data$imo[data$imo < 999999] <- NA
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("september")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2010/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2010/file_list_months_weeks/duplication_september.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2010/file_list_months_weeks/ais_september_2010.csv")
write.table (data, directory_final, row.names=F, sep=",")
rm(list = ls())


###october 10
# 1. sorting by month in different folder: 1 folder per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/")
fileList <- list.files(pattern="division_ais_2010.*\\.csv", recursive=FALSE)
i=1
for (file in fileList) {
  data2010<-read.table(file, header=T, fill=T, sep=",")
  data_2010_october<-data2010[data2010$month == 10, ]
  data_2010_october <- subset(data_2010_october, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2010/october"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2010_october)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2010/october/data_2010_octoberfinal", sep="_",i, ".csv")
    write.table (data_2010_october, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
}

#2. Merging all files in each month_folder to have 1 file per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/october")
fileList <- list.files(pattern="data_2010_octoberfinal*", recursive=FALSE)
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
dir.create(file.path("E:/test_division/division_finale2010/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2010/file_list_months_weeks/duplication_october.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2010/file_list_months_weeks/ais_october_2010.csv")
write.table (data, directory_final, row.names=F, sep=",")

###november 11
# 1. sorting by month in different folder: 1 folder per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/")
fileList <- list.files(pattern="division_ais_2010.*\\.csv", recursive=FALSE)
i=1
for (file in fileList) {
  data2010<-read.table(file, header=T, fill=T, sep=",")
  data_2010_november<-data2010[data2010$month == 11, ]
  data_2010_november <- subset(data_2010_november, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2010/november"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2010_november)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2010/november/data_2010_novemberfinal", sep="_",i, ".csv")
    write.table (data_2010_november, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
}

#2. Merging all files in each month_folder to have 1 file per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/november")
fileList <- list.files(pattern="data_2010_novemberfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
data$imo[data$imo < 999999] <- NA
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("november")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2010/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2010/file_list_months_weeks/duplication_november.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2010/file_list_months_weeks/ais_november_2010.csv")
write.table (data, directory_final, row.names=F, sep=",")
rm(list = ls())


###december 12
# 1. sorting by month in different folder: 1 folder per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/")
fileList <- list.files(pattern="division_ais_2010.*\\.csv", recursive=FALSE)
i=1
for (file in fileList) {
  data2010<-read.table(file, header=T, fill=T, sep=",")
  data_2010_december<-data2010[data2010$month == 12, ]
  data_2010_december <- subset(data_2010_december, !(is.na(timestamp_pretty) ))
  dir.create(file.path("E:/test_division/division_finale2010/december"), showWarnings=F)
  nrow<-as.numeric(nrow(data_2010_december)) 
  if(nrow>0){ 
    directory_divisions=paste("E:/test_division/division_finale2010/december/data_2010_decemberfinal", sep="_",i, ".csv")
    write.table (data_2010_december, directory_divisions, row.names=F, sep=",")
    i=i+1  
  } else {
  }
}

#2. Merging all files in each month_folder to have 1 file per month
rm(list = ls())
setwd("E:/test_division/division_finale2010/december")
fileList <- list.files(pattern="data_2010_decemberfinal*", recursive=FALSE)
library(plyr)
data <- ldply(fileList, read.table, header=T, sep = ",", fill=T)
data <- subset(data, !(is.na(timestamp_pretty) ))

data$imo<-as.numeric(data$imo)
not_clean_imo<-sum(is.na(data$imo))
data$imo[is.na(data$imo)] <- NA
data$imo[data$imo < 999999] <- NA
data$imo[data$imo >9999999] <- NA

rows_data_duplicate<-nrow(data)
data <- unique(data)
rows_no_duplication<-nrow(data)
difference<-(rows_data_duplicate-rows_no_duplication)
month<-c("december")
duplication<-data.frame(rows_data_duplicate,rows_no_duplication,difference, month, row.names = NULL)
dir.create(file.path("E:/test_division/division_finale2010/file_list_months_weeks"), showWarnings=F)
directory_stats_duplications=paste("E:/test_division/division_finale2010/file_list_months_weeks/duplication_december.csv")
write.table (duplication, directory_stats_duplications , row.names=F, sep=";")

data$timestamp_pretty<-as.character(data$timestamp_pretty)
data$shipType<-as.character(data$shipType)

directory_final=paste("E:/test_division/division_finale2010/file_list_months_weeks/ais_december_2010.csv")
write.table (data, directory_final, row.names=F, sep=",")



# Step 5: division of monthly files in weekly files (optional) -----------------------

### table per week new test (14082015):
# taking lot of time to generate, not efficient


rm(list = ls())
setwd("E:/test_division/division_finale2010/file_list_months_weeks")
fileList <- list.files(pattern="scope_ais_.*\\.csv", recursive=FALSE)
for (i in 1:52){
 for (file in fileList){
data_2010<-read.table(file, header=T, fill=T, sep=",")
data_2010<-data_2010[data_2010$week == i, ]
nrow<-as.numeric(nrow(data_2010))  
if(nrow>0){
data_2010 <- data_2010[order(data_2010$timestamp_pretty) , ]
dir.create(file.path("E:/test_division/division_finale2010/file_list_months_weeks"), showWarnings=F)

time in UTC GMT seconds in the filename to avoir overwritting
install.packages("stringr")
a<-Sys.time()
library(stringr)
a<-str_replace_all(a, fixed(" "), "_")
a<-str_replace_all(a, fixed(":"), "_")
a<-str_replace_all(a, fixed("-"), "_")

directory_divisions=paste("E:/test_division/division_finale2010/file_list_months_weeks/data_2010_scope_week",i, sep="_", a, ".csv")
write.table (data_2010, directory_divisions, row.names=F, sep=",")
} else {
}  } }  


# Empty the environment and close R ---------------------------------------

# delete temp files (divisions)
setwd("E:/test_division/division_finale2010/")
fileList <- list.files(pattern="division_ais_2010.*\\.csv", recursive=FALSE)

for (file in fileList) {
  unlink(file)
}

#empty temp memory 
rm(list = ls())
rm(list = ls())
