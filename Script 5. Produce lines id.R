#####################################################################
# 
# R Script to produce trips_id to generate density maps
# Developped by Florent NICOLAS, HELCOM Secretariat - 2017
# R Version 3.4.3
#
# Input data :  AIS monthly files for the year XXXX
# Ouput data:  Monthly files with trip_id for each AIS signals (E:/DensityMaps_V3/YEAR/01_trips/tracks_YEAR_)
#
# Fill in row 19 the year you want to process
#
#####################################################################

rm(list = ls())
 


#### year to process:
year <- 2011


# packages needed
library("sp")
library("rgdal")
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
library("foreign")

#options(scipen = 999)


# time for producing the files:
start.time <- Sys.time()

# 1. input data: AIS monthly files with a filelist


working.dir <- paste("E:/test_division/division_finale", year, "/file_list_months_weeks", sep="")
#working.dir <- paste("D:/HELCOM AIS data/division_finale", year,"/file_list_months_weeks", sep="" )
#setwd("D:/HELCOM AIS data/division_finale2016/file_list_months_weeks") #if data on D drive
setwd(working.dir) # auto setwd with year as variable
fileList <- list.files(pattern="ais_.*\\.csv", recursive=FALSE)

for (file in fileList) {
  
data <-read.csv(file, header=T, fill=T) 
  
  ### test
#data <- read.csv("E:/test_division/division_finale2016/file_list_months_weeks/ais_march_2016.csv", header=T, fill=T) 

  #################  
  #################  
  #################
  #################                           PREPARE DATA
  #################  
  ################# 
  ################# 
  ################# 
  
# 2. prepare the timestamp
  options(scipen = 999)
  #data$timestamp <- strptime(data$timestamp_pretty, "%d/%m/%Y %H:%M:%S")
  data$timestamp <- as.numeric(data$timestamp)
 
  
# 3. Extract the month of the monthly file that is processing  
  Sys.setlocale("LC_TIME", "C")
  date <- strptime(data$timestamp_pretty, "%d/%m/%Y %H:%M:%S")
  month <- tolower(unique(months(as.Date(date))))
  
# 4. removing wrong mmsi 111111111 and wrong imo
  data <- subset(data,data$mmsi != "111111111")
  data <- subset(data,data$imo != "1111111")
  data <- subset(data,data$imo != "2222222") 
  data <- subset(data,data$imo != "3333333") 
  data <- subset(data,data$imo != "4444444") 
  data <- subset(data,data$imo != "5555555") 
  data <- subset(data,data$imo != "6666666") 
  data <- subset(data,data$imo != "7777777") 
  data <- subset(data,data$imo != "8888888") 
  data <- subset(data,data$imo != "9999999") 

# 5. select only IMO ships and relevant parameters
  data<-subset(data, (!is.na(imo)))
  data<- data[,c("timestamp_pretty", "timestamp", "imo", "sog", "cog", "lat","long")] 
  data$imo<- as.numeric(as.character(data$imo))
  
  
# 6. sort data by ship and time
  data <- data[order(data$imo, data$timestamp),] 
  
  #################  
  #################  
  #################
  #################                           PRODUCE LINES ID
  #################  
  ################# 
  ################# 
  ################# 
  
#7. remove signals that are outside Baltic Sea (outside the exists)
  data$long <- as.numeric(as.character(data$long))
  data$lat <- as.numeric(as.character(data$lat))
  coordinates(data) <- c("long", "lat")                                         # coordinate system
  outside_exits_polygons <-readOGR("W:/Florentdrafts","Outside_Exits")
  #plot(outside_exits_polygons)
  proj4string(data) <- proj4string(outside_exits_polygons)                      # to confirm the same reference system
  
  outside_exist_signals <- !is.na(over(data, as(outside_exits_polygons, "SpatialPolygons")))     # overlap shape file and data
  mean(outside_exist_signals) 
  

  data<-as.data.frame(data)                                                      # build data frame
  data$outside_exist_signals <-outside_exist_signals *1
  data <- subset(data,outside_exist_signals==0 )                                 # select only signals that not in the polygons (so inside the Baltic Sea)
  
  
  ######################### ######################### #########################   A. Definition of the signals
  ######################### ######################### ######################### 
  
  
# 8. definition of signals in ports and add port name
  coordinates(data) <- c("long", "lat")
  ports_polygons <-readOGR("W:/PROJECTS/Maritime Assessment 2016/Ports","Ports_V3")
  #plot(ports_polygons)

  proj4string(data) <- proj4string(ports_polygons)                            # to confirm the same reference system
  inside.port <- !is.na(over(data, as(ports_polygons, "SpatialPolygons")))    # overlapping shape file and data
  mean(inside.port)
  inside.port2 <- over(data, ports_polygons[,"port"])                         # get the name of the port
  
  
# 9. add column to assess if the signal is in the exit polygons of the Baltic Sea (to be sure that exiting / entering the Baltic Sea is creating a new trip)
  exit_baltic <-readOGR("W:/Florentdrafts","Exit_Baltic_Sea")
  proj4string(data) <- proj4string(exit_baltic)                               # to confirm the same reference system
  into_exit_baltic <- !is.na(over(data, as(exit_baltic, "SpatialPolygons")))
  mean(into_exit_baltic)
  data$into_exit_baltic <-into_exit_baltic *1
  #t<- subset(data, data$into_exit_baltic ==1)
  data<-as.data.frame(data)    #build data frame
  data$inside.port <-inside.port *1
  data$trips_data2 <- data$inside.port
  data$trips_data2[data$into_exit_baltic== 1] <- 1
  
  
# 10. for the signals in the port, if SOG < 0.5 KN (0.926 km/h) then SOG = 0
  data$sog <- as.numeric(data$sog)
  data$sog <- ifelse( data$sog < 0.5 & data$inside.port == "1" , "0", data$sog)
  data$sog <- as.numeric(data$sog)
  summary(data$sog)
  summary(data$inside.port)
  
# 11. Def of STOPS: if the SOG is = 0 and the ship is in the port polygon, so it is a stop
  data$inside.port <-inside.port *1
  data$Stops <- NA
  summary(data$sog)
  data$Stops <- ifelse( data$sog== 0 & data$inside.port == "1", "1", "0")
  data$Stops <- as.numeric(data$Stops)
  
# 12. add name of the port
  data$Port <- inside.port2$port
  
  
# 13. Def of Trips: if signal outside the ports = trips
  data$Trips <- NA
  data$Trips <- ifelse(  data$inside.port == "0", "1", "0")
  data$Stops <- as.numeric(as.character(data$Stops))
  data$Trips <- as.numeric(as.character(data$Trips))
  data <- data[order(data$imo, data$timestamp),] 
  
 
  ######################### ######################### #########################   B. Definition of the signals
  ######################### ######################### ######################### 
# 14. Counting number of trips WORKING, replace Trips_rle by trips_id
  data$Trips_rle <- setDT(data)[, v1 := if(all(!trips_data2)) c(TRUE, imo[-1]!= imo[-.N]) 
                                else rep(FALSE, .N), .(grp = rleid(trips_data2))][,cumsum(v1)*(!trips_data2)]
  

# 15. Counting the number of inside ports
  data$inside.port.rle <- setDT(data)[, v1 := if(all(!Trips_rle)) c(TRUE, imo[-1]!= imo[-.N]) 
                                      else rep(FALSE, .N), .(grp = rleid(Trips_rle))][,cumsum(v1)*(!Trips_rle)]
  

# 16. remove signals that are not in the ports polygons
  data_inside_ports <- subset(data, data$into_exit_baltic !=1)                    # Remove signals that are into the exit/enter polygones of the Baltic Sea (not needed)
  data_inside_ports <- subset(data_inside_ports,data_inside_ports$inside.port.rle != 0)
  
# 17. for each sequences (rle) of signals in the ports polygons, keep the signal in the middle
  data_inside_ports_middle <- setDT(data_inside_ports)[, if(.N >1) .SD[ceiling(.N/2)] else .SD ,inside.port.rle]
  data_inside_ports_middle$visit <- 1
  
# 18. for each sequences (rle) of signals in the ports polygons, keep the first signal in the polygon and the last one
  data_inside_ports_first_last <- data_inside_ports %>%
    group_by(inside.port.rle) %>%
    #arrange(stopSequence) %>%
    filter(row_number()==1 | row_number()==n()) %>%
    filter(n()>1) #remove if there is only one signal per inside.port.rle
  data_inside_ports_first_last$visit <- 1
  data_inside_ports_first_last <- as.data.table(data_inside_ports_first_last)
  
# 19. duration of the trips
  duration_trips <- filter(data, Trips_rle !=0) %>% 
  unite(dates, timestamp_pretty, sep = " ") %>% #create date object
  mutate(dates = as.POSIXct(strptime(dates, format = "%d/%m/%Y %H:%M:%S"))) %>%
  group_by(Trips_rle) %>%  
  filter(dates == min(dates) | dates == max(dates)) %>%                          #select rows with min and max dates
  summarise(minTime = min(dates),
               maxTime = max(dates),
                duration_minutes = round(difftime(max(dates), min(dates), units="mins")),
                 Ship = imo[1])
  
# 20. keep only the trips_rle from data that > 10 minutes
  duration_trips <- filter(duration_trips, duration_minutes >10)    
  trips_rle_to_keep <-   as.factor(duration_trips$Trips_rle)
  data <- data[data$Trips_rle %in% duration_trips$Trips_rle,]                    # keep only the trips_rle with duration > 10 minutes and data$Trips_rle are null (signals the ports)
  data$visit <- 0                                                                #create column visit with 0 as unique value
  
# 21. merge the points in polygons (the middle points), the trip > 10 mintues and the first and last point in each polygons for each rle
  data <- rbind(data,data_inside_ports_middle,data_inside_ports_first_last, fill=T)
  
# 22. sort the data per ship and time 
  data <- data[order(data$imo, data$timestamp),] 
  data <- subset(data, data$into_exit_baltic == 0)                                # remove signals that are in the exit / enter to avoid mistake (not needed)
  
# 23. prepare table 
  data <- as.data.frame(data)
  data <- data[,c("timestamp_pretty", "timestamp", "imo","lat",  "long", "Trips_rle", "visit")] # select relevant parameters
  names(data)[names(data)=="Trips_rle"] <- "trip_id" #change name col
  names(data)[names(data)=="timestamp_pretty"] <- "date_time"  #change name col
  data$trip_id<- as.numeric(as.character(data$trip_id))
  data$timestamp<- as.numeric(as.character(data$timestamp))
  
# 24. for the stops, replace the Trips_rle = 0 with the value of the previous trip: we don´ t want stops, they have to be considered as trips
  setDT(data)[, trip_id := na.locf(na.locf(replace(trip_id, trip_id==0, NA), 
                                           na.rm=FALSE), fromLast=TRUE), by = imo]
  
  
# 25. remove the trips that stay during this month in the port (when trip_id ==0)
  data <- subset(data, data$trip_id != 0)
  
#  remove duplicated signals
  #data <- data[!duplicated(data), ]
  
  ######################### ######################### #########################   C. Correction of the distance between signals
  ######################### ######################### ######################### 
  
# 26. first round of calculation distance to remove points that are too far away
  data <- data[order(data$imo, -data$timestamp),]                               #re-order the data to get distance between points
  distance <- data %>%
  group_by(trip_id) %>%
  mutate(longlead = lead(long), latlead = lead(lat)) %>%
      #na.omit() %>%
       rowwise() %>%
        mutate(dist = distCosine(c(long,lat), c(longlead, latlead)))
  
  data$distance <- distance$dist /1000                                           # distance in km
  data <- data[order(data$imo, data$timestamp),]                                # order by ship and time
  
# 27. remove signals if distance > 30 km
  data <- subset(data,data$distance < 30)
  
# 28.  second round of distance to recalculate the distance between points
  data <- data[order(data$imo, -data$timestamp),]                              #re-order the data to get distance between points
  distance <- data %>%
  group_by(trip_id) %>%
  mutate(longlead = lead(long), latlead = lead(lat)) %>%
      #na.omit() %>%
       rowwise() %>%
        mutate(dist = distCosine(c(long,lat), c(longlead, latlead)))
         data$distance <- distance$dist /1000
          data <- data[order(data$imo, data$timestamp),] 
  
# 29. create new trip ID when signals are too far away and remove when distance =
  data <- subset(data, data$distance != "NaN")                                          #to remove the errors of the distance
  data$new_trip_id <- data$trip_id + cumsum(data$distance > 30)
    #data$new_trip_id_2 <- with(data, cumsum(distance > 30| !duplicated(trip_id)))      # new function test
  summary(data$new_trip_id)
  #data <- subset(data, data$distance > 0 )  # remove when distance = 0 to lighten the files
  
  ######################### ######################### #########################   D. Preparing to write table and write
  ######################### ######################### ######################### 
  
# 30. get the max value of trip id  and n rows
  max_trip <- as.numeric(max(data$new_trip_id))
  n_rows <- nrow(data)  
  
# 31. re-order data 
  data <- data[order(data$imo, data$date_time),] 
    # NOT NEEDED: delete duplicated signals (ships in the same trip at the same positions, i.e. during visits at ports)
      #data <- setkey(data)
       #data <- trips_id[!(duplicated(trips_id[c("imo","lat","long","trip_id")]) | duplicated(trips_id[c("imo","lat","long","trip_id")], fromLast = TRUE)), ]
         options(scipen = 999) 

#  32. Export the final monthly files events
dir.create(file.path("E:/DensityMaps_V3/"), showWarnings=F)
dir.to.create <- paste("E:/DensityMaps_V3/", year, sep="") #level 1 in directory using year as variable
dir.create(file.path(dir.to.create), showWarnings=F)
dir.to.create2 <- paste(dir.to.create,"/01_trips", sep="") # dir.to.create level 2 for lines_id using year as variable
dir.create(file.path(dir.to.create2), showWarnings=F)
dir.to.write <- paste(dir.to.create2, "/tracks_", year,"_", sep="")
dir.to.create3 <- paste(dir.to.create,"/02_lines", sep="") # dir.to.create level 2 for lines (for py script) using year as variable
dir.create(file.path(dir.to.create3), showWarnings=F)
dir.to.write <- paste(dir.to.create2, "/tracks_", year,"_", sep="")
directory=paste(dir.to.write, month,"_",max_trip,"_", n_rows,  sep="", ".csv" )
write.table(data, directory, sep=";", col.names = T, row.names=F)
}



  ######################### ######################### #########################   E. Sort the files again
  ######################### ######################### ######################### 
# Last step: to sort the files by time once again. 
# due to RAM issues, it is possible that the final files of trips_id are not sorted properly. 
# this script will sort again the 12 files already written

rm(list = ls()[!ls() %in% c("start.time", "year","dir.to.create")])
# input data
working.dir <- paste(dir.to.create,"/01_trips",sep="")
setwd(working.dir)
patter.directory <- paste("tracks_", year,".*\\.csv", sep="")
fileList <- list.files(pattern=patter.directory , recursive=FALSE)
#fileList <- list.files(pattern="tracks_2016.*\\.csv", recursive=FALSE) #old direcory - when year is not a variable
for (file in fileList) {
    data <-read.csv(file,  sep=";") 
#test  
#data <- read.csv("E:/DensityMaps_V3/2016/01_trips/january/tracks_2016_january_44156_11047760.csv", sep=";", nrows = 2000)
  data <- data[order(data$imo, data$date_time),] 
  
# remove duplicated signals issue / signals popping from other port
  data <- data[!duplicated(data[,c("timestamp", "imo")], fromLast=T),]
  
  # get the max value of trip id  for the name of the file 
  max_trip <- as.numeric(max(data$new_trip_id))
  # get the  N rows
  n_rows <- nrow(data)
    # get the month of the monthly file that is processing  forthe name of the file
  Sys.setlocale("LC_TIME", "C")
  date <- strptime(data$date_time, "%d/%m/%Y %H:%M:%S")
  month <- tolower(unique(months(as.Date(date))))
  options(scipen = 999) 
  # Export the final monthly files events
  final.directory <- paste(working.dir, "/tracks_", year, "_", sep="")
  directory=paste(final.directory, month,"_",max_trip,"_", n_rows,  sep="", ".csv" )
  write.table(data, directory, sep=";", col.names = T, row.names=F)
  
}
library(mail)
#sendmail("florent.nicolas@helcom.fi", "tracks for 2016 ready", "tracks for 2016 ready", password="rmail")

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
directory_time.taken <- paste(working.dir, "/time.taken_" , year, sep="", ".csv" )
write.table(time.taken, directory_time.taken, sep=";", col.names = F, row.names=F)


## extra steps to prepare the work for the .py script
# 1. create directories
dir.to.create <- paste("E:/DensityMaps_V3/", year, sep="")
dir.to.create_lines_ST <- paste(dir.to.create, "/03_lines_by_shiptype", sep="")
dir.create(file.path(dir.to.create_lines_ST), showWarnings=F)
dir.to.create_rasters <- paste(dir.to.create, "/04_rasters", sep="")
dir.create(file.path(dir.to.create_rasters), showWarnings=F)

# create .dbf shiplist for imo ships only
shiplist <- paste("E:/ship_list/shiplist_", year, "_final.csv", sep="")
shiplist <- read.csv(shiplist, header=T, fill=T, sep=";") 
shiplist <- subset(shiplist, !(is.na(shiplist$imo)))
shiplist <- subset(shiplist,  shiplist$imo < "9999999")
summary(shiplist$imo)
shiplist <- subset(shiplist, !(is.na(shiplist$imo)))

directory <- paste("E:/DensityMaps_V3/ship_list_dbf/imo/", year,".dbf", sep="")
write.dbf(shiplist,directory,factor2char=T)

#rm(list = ls())
#rm(list = ls())

  
