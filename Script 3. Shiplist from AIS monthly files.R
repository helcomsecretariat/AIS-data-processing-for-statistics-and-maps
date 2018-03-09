#####################################################################
# 
# R Script to produce yearly ship list
# Developped by Florent NICOLAS, HELCOM Secretariat - 2017
# R Version 3.4.3
#
# Input data :  AIS monthly files for the year XXXX (script 1)
# Ouput data:  Shiplist for the year XXXX with upgraded ship information from a commercial provider
#
# You have to replace (CTR A + CTR F) the year 2013 with the year wanted
#
#####################################################################

rm(list = ls())


# Step 1: create ship list of ships from AIS monthly files ------------------------------------

###### file list to select monthly files AIS data
setwd("E:/test_division/division_finale2013/file_list_months_weeks")
#setwd("E:/test_division/division_finale2013/180seconds/file_list_months_weeks")
fileList <- list.files(pattern="ais_.*\\.csv", recursive=FALSE )

i=1
for (file in fileList) {
  
  ###### read the AIS table
  data<-read.table(file, header=T, fill=T, sep=",")  
  data$imo<-as.numeric(as.character(data$imo))
  data$mmsi<-as.numeric(as.character(data$mmsi))
  
  ######apply your script to each of the AIS tables (monthly tableS)
  
  ##chose variables
  data <- data[,c("mmsi","imo","targetType", "shipType","dimBow","dimPort","dimStarboard","draught","dimStern", "callsign", "country", "name")]
  
  #remove same signals
  data <- data[!duplicated(data),]
  
   ###### write (= export) the  12 tables
  dir.create(file.path("E:/test_division/division_finale2013/ship_list/"), showWarnings=F)
  dir.create(file.path("E:/test_division/division_finale2013/ship_list/ship_list_temp/"), showWarnings=F)
  directory_PRF_2013=paste("E:/test_division/division_finale2013/ship_list/ship_list_temp/AIS_shipList_", i,sep=",", ".csv")
  write.table (data, directory_PRF_2013, row.names=F, sep=";")
  i=i+1
}


#######  to merge the 12 files to have one single file_list
rm(list = ls())
rm(list = ls())
#set your working directory where the 12 files are
setwd("E:/test_division/division_finale2013/ship_list/ship_list_temp/")

#create the file list with the patter small_ship_list,....
fileList <- list.files(pattern="AIS_shipList_,*", recursive=FALSE)
library(plyr)

# ldply to merge the files:
data <- ldply(fileList, read.table, header=T, sep = ";", fill=T)

#remove duplication in the files:
data <- data[!duplicated(data),]

#data <- unique(data)

#remove the duplication if the dimensions of the ship are different but same mmsi
# we keep the signal with the IMO number (if available) and the bigger draught 
data$mmsi<-as.numeric(as.character(data$mmsi))
data$imo<-as.numeric(as.character(data$imo))
data$draught<-as.numeric(as.character(data$draught))
data <- data[order(-data$mmsi,-data$imo,-data$draught),] 

# separate imo and non imo
data_non_imo <- subset(data, is.na(data$imo)) 
data_imo <- subset(data, !is.na(data$imo))

# remove duplicated imo
data_imo<-  data_imo[!duplicated(data_imo$imo),]

# remerge imo and non imo
data <- rbind(data_non_imo,data_imo)

#t <- subset(data, data$imo == 7432202)


#write final shipList_2013
directory_final=paste("E:/test_division/division_finale2013/ship_list/ship_list_2013_final.csv")
write.table (data, directory_final, row.names=F, sep=";", quote=T)

#write final shipList_2013 to be merged with VF
directory_final=paste("E:/ship_list/temp_AIS/ship_list_2013_final.csv")
write.table (data, directory_final, row.names=F, sep=";", quote=T)

# send mail notification and empty environment  ------------------------------------
library(mail)
#sendmail("florent.nicolas@helcom.fi", "Shiplist 2013 done", " Shiplist 2013 done", password="rmail")
rm(list = ls())
rm(list = ls())
rm(list = ls())



# Step 2: FINAL TABLE 2 : merging ship list AIS with ship provider information ------------------------------------
rm(list = ls())
rm(list = ls())

setwd("E:/ship_list/temp_AIS")
#ship_list_2013 <- read.csv("ship_list_2013_final.csv", sep=";")
ship_list_2013 <- read.csv("E:/ship_list/temp_AIS/ship_list_2013_final.csv", sep=";")

ship_list_2013$year <- 2013

#add country (following the MID from the MMSI)
# 3. add MID and flag
#ship_list_2013$MID <- substr(ship_list_2013$mmsi,1,3)
#MID <- read.csv("//hcvhost01/data/MID.csv", sep=";", quote="")
#ship_list_2013<-merge(ship_list_2013, MID, by="MID")
#remove special characters
#ship_list_2013$country <- gsub("([\\])","", ship_list_2013$country)
#ship_list_2013$country <- gsub("[][!#$%()*.:;<=>@^_`|~.{}]", "", ship_list_2013$country)

ship_list <- read.csv("HELCOM-Maritime-Database_ALL.csv", sep=";", na.strings =c("", "--", "NA") )
ship_list[ship_list == 0] <- NA
# rename column for tonnage
colnames(ship_list)[which(colnames(ship_list) == 'GROSS.TONNAGE')] <- 'VF_GROSS_TONNAGE'
colnames(ship_list)[which(colnames(ship_list) == 'NET.TONNAGE')] <- 'VF_NET_TONNAGE'

#merging to have final shiplist
year_2013  <-  merge(ship_list_2013,ship_list,by.x='imo',by.y='imo',all.x=TRUE)

no_year<-as.numeric(nrow(subset(year_2013,(is.na(year) ))))
#year_2013 <-subset(year_2013, imo == "6814128" | imo == "1234567"  | imo ==  "5338555" | imo ==  "5104253" )

#add lenght and width to the AIS information
year_2013$dimStern <- as.numeric(year_2013$dimStern)
year_2013$dimBow <- as.numeric(year_2013$dimBow)
year_2013$dimPort <- as.numeric(year_2013$dimPort)
year_2013$dimStarboard <- as.numeric(year_2013$dimStarboard)
year_2013$draught <- as.numeric(year_2013$draught)

year_2013$length_AIS <- year_2013$dimBow +  year_2013$dimStern
year_2013$width_AIS <- year_2013$dimPort + year_2013$dimStarboard

year_2013$length_AIS [year_2013$length_AIS  == 0 & is.numeric(year_2013$length_AIS )] <- NA
year_2013$width_AIS [year_2013$width_AIS  == 0 & is.numeric(year_2013$width_AIS )] <- NA
year_2013$draught [year_2013$draught  == 0 & is.numeric(year_2013$draught )] <- NA

#replace the shiptype, dimensions, etc.  if == NA from VF
year_2013$shipType_final<- ifelse(is.na(year_2013$VF_SHIP_TYPE), as.character(year_2013$shipType), as.character(year_2013$VF_SHIP_TYPE))
#year_2013$shipType_final<-as.factor(year_2013$shipType_final)
year_2013$name_final<- ifelse(is.na(year_2013$VF_NAME), as.character(year_2013$name), as.character(year_2013$VF_NAME))

year_2013$length_final <- ifelse(is.na(year_2013$VF_LENGTH), as.numeric(year_2013$length_AIS), as.numeric(year_2013$VF_LENGTH))
year_2013$width_final <- ifelse(is.na(year_2013$VF_WIDTH), as.numeric(year_2013$width_AIS),as.numeric(year_2013$VF_WIDTH))
year_2013$draught_final <- ifelse(is.na(year_2013$VF_DRAUGHT), as.numeric(year_2013$draught), as.numeric(year_2013$VF_DRAUGHT))

# select relevant parameters
#year_2013<- year_2013[,c("imo","mmsi","callsign","country","targetType","year","name_final","length_final","width_final","draught_final", "shipType_final")] 

# add the HELCOM gross shiptype and detail shiptype
gross_detail_shiptype <- read.csv("HELCOM_gross_detail_shiptypes.csv", sep=";")
colnames(gross_detail_shiptype)[1] <- "shipType_final"
colnames(gross_detail_shiptype)[2] <- "HELCOM_Gross_ShipType"
colnames(gross_detail_shiptype)[3] <- "HELCOM_Detail_ShipType"



# merge
library(plyr)
year_2013 <- join(year_2013, gross_detail_shiptype, by = "shipType_final")
#remove white spaces and change upper case (only first letter is uppercase) and the slash for Gross_ShipType
year_2013$HELCOM_Gross_ShipType <- gsub(" ", "", year_2013$HELCOM_Gross_ShipType, fixed = TRUE)
year_2013$HELCOM_Gross_ShipType <- gsub("/", "", year_2013$HELCOM_Gross_ShipType, fixed = TRUE)
year_2013$HELCOM_Detail_ShipType <- gsub(" ", "", year_2013$HELCOM_Detail_ShipType, fixed = TRUE)

year_2013$HELCOM_Gross_ShipType <- tolower(year_2013$HELCOM_Gross_ShipType)
year_2013$HELCOM_Detail_ShipType <- tolower(year_2013$HELCOM_Detail_ShipType)
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
year_2013$HELCOM_Gross_ShipType <- proper(year_2013$HELCOM_Gross_ShipType)
year_2013$HELCOM_Detail_ShipType <- proper(year_2013$HELCOM_Detail_ShipType)

# test data (imo without shiptype)
no_gross<-  year_2013[is.na(year_2013$HELCOM_Gross_ShipType),]
summary(no_gross$imo)
no_detail<-  year_2013[is.na(year_2013$HELCOM_Detail_ShipType),]
summary(no_detail$imo)

# rename column for tonnage
colnames(year_2013)[which(colnames(year_2013) == 'VF_GROSS_TONNAGE')] <- 'Gross_tonnage'
colnames(year_2013)[which(colnames(year_2013) == 'VF_NET_TONNAGE')] <- 'Net_tonnage'

#### export data
# select relevant parameters
year_2013<- year_2013[,c("imo","mmsi","callsign","targetType","country","year","name_final","length_final","width_final","draught_final", "Gross_tonnage","Net_tonnage", "HELCOM_Gross_ShipType","HELCOM_Detail_ShipType")] 

# replace NANA with NA
year_2013$HELCOM_Gross_ShipType<- as.character(year_2013$HELCOM_Gross_ShipType)
year_2013$HELCOM_Detail_ShipType<- as.character(year_2013$HELCOM_Detail_ShipType)
year_2013$HELCOM_Gross_ShipType[year_2013$HELCOM_Gross_ShipType == "NANA"] <- "Unknown"
year_2013$HELCOM_Detail_ShipType[year_2013$HELCOM_Detail_ShipType == "NANA"] <- "Unknown"

# replace Vehiclecarrierrorocargo to ROROcargo
year_2013$HELCOM_Gross_ShipType[year_2013$HELCOM_Gross_ShipType == "Vehiclecarrierrorocargo"] <- "Rorocargo"

t <- subset(year_2013, year_2013$imo == 7432202)



# remove duplicated imo numbers (due to the last merging to dimensions and shiptypes):
# we keep the signal with the IMO number (if available) and the bigger draught 
# separate imo and non imo
year_2013_non_imo <- subset(year_2013, is.na(year_2013$imo)) 
year_2013_imo <- subset(year_2013, !is.na(year_2013$imo))

# sort by imo and draught
#year_2013_imo$imo <- as.numeric(year_2013_imo$imo)
year_2013_imo <- year_2013_imo[order(-year_2013_imo$imo,-year_2013_imo$draught),] 

# remove duplicated imo
year_2013_imo<-  year_2013_imo[!duplicated(year_2013_imo$imo),]

# remerge imo and non imo
year_2013_all <- rbind(year_2013_imo,year_2013_non_imo)







#write table and dbf to produce density maps
write.table(year_2013_all, "E:/ship_list/shiplist_2013_final.csv", row.names=F, sep=";")


# send mail notification and empty environment  ------------------------------------
library(mail)
#sendmail("your_email@domain.domain", "Shiplist 2013 merged with VF done", "Shiplist 2013 merged with VF done", password="rmail")

#to do a final check of the shiptypes
year_2013$HELCOM_Gross_ShipType <- as.factor(year_2013$HELCOM_Gross_ShipType)
summary(year_2013$HELCOM_Gross_ShipType)

year_2013$HELCOM_Detail_ShipType <- as.factor(year_2013$HELCOM_Detail_ShipType)
summary(year_2013$HELCOM_Detail_ShipType)

rm(list = ls())
rm(list = ls())
rm(list = ls())





 

