#####################################################################
# 
# R Script to  generate events statistics using AIS data (trips, stops, enters, exits, etc.)
# Developped by Florent NICOLAS, HELCOM Secretariat - 2017
# R Version 3.4.3
#
# Input data :  AIS monthly files 
# Ouput data:  Events yearly files (E:/Events_V2/Events_YEAR.csv)
#
# !! you have to enter the year that you want to process in the field below (row 17)
#
#####################################################################

rm(list = ls())
rm(list = ls())

year = 2007

# packages needed

# to undo (from 6/12)
#install.packages("maps")

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

# time for producing the files:
start.time <- Sys.time()

#prepare directory for exporting the outcome
dir.to.create <-  "E:/Events_V2/"
dir.create(file.path(dir.to.create), showWarnings=F)

dir.to.create <- paste(dir.to.create,year,sep="")
dir.create(file.path(dir.to.create), showWarnings=F)

dir.to.create <- paste(dir.to.create,"/", "monthly", sep="")
dir.create(file.path(dir.to.create), showWarnings=F)


# 1. input data: AIS monthly files with a filelist
working.dir <- paste("E:/test_division/division_finale", year, "/file_list_months_weeks", sep="")
#setwd("D:/HELCOM AIS data/division_finale2016/file_list_months_weeks") #if data on D drive
setwd(working.dir) # auto setwd with year as variable
fileList <- list.files(pattern="ais_.*\\.csv", recursive=FALSE)

  for (file in fileList) {
  df <- read.table(file, header=T, fill=T, sep=",", na.strings=c(""," ","null", "NA"))   
  
      ###### TEST ONLY                                      
  #df <- read.csv("D:/HELCOM AIS data/division_finale2014/file_list_months_weeks/ais_june_2014.csv", header=T, fill=T, nrows=100000)   
  
  ########## add column names (parameters) and select relevant parameters FOR YEARS 2016, 2016 and 2016 ONLY 
  
  
  #colnames(df)<-c( "timestamp_pretty","timestamp","msgid","targetType","mmsi","lat","long","posacc","sog","cog","shipType","dimBow",
  # "dimport","dimStarboard","dimStern","shipTypeCargoTypeCode","shipType","destination","imo","eta")
  #################  
  #################  
  #################
  #################                           PREPARE DATA
  #################  
  ################# 
  ################# 
  ################# 
  
  df<- df[,c("timestamp_pretty","timestamp","mmsi", "lat","long", "sog", "cog",  "imo")]
  #df = df[-1,]
  
# 2. prepare the timestamp  
  Sys.setlocale("LC_TIME", "C")
  date <- strptime(df$timestamp_pretty, "%d/%m/%Y %H:%M:%S") 
  month <- unique(months(as.Date(date)))
  
# 3. removing wrong mmsi 111111111 and wrong imo
  df <- subset(df,df$mmsi != "111111111")
  df <- subset(df,df$imo != "1111111")
  df <- subset(df,df$imo != "2222222") 
  df <- subset(df,df$imo != "3333333") 
  df <- subset(df,df$imo != "4444444") 
  df <- subset(df,df$imo != "5555555") 
  df <- subset(df,df$imo != "6666666") 
  df <- subset(df,df$imo != "7777777") 
  df <- subset(df,df$imo != "8888888") 
  df <- subset(df,df$imo != "9999999") 
  #for too big IMO numbers:
  df$imo <- as.numeric(as.character(df$imo))
  df$imo[df$imo < 999999] <- NA
  #for too small IMO numbers:
  df$imo[df$imo >9999999] <- NA
  
# 4. select only IMO ships and relevant parameters
  df<-subset(df, (!is.na(imo)))
  df<- df[,c("timestamp_pretty","imo", "sog", "cog", "lat","long")] 
  df$imo<- as.numeric(df$imo)
  
# 5. sort data by ship and time
  df <- df[order(df$imo, df$timestamp_pretty),] 
 
# 6 . positions as numerical values
  df$lat <- as.numeric(as.character(df$lat))
  df$long <- as.numeric(as.character(df$long))

  #################  
  #################  
  #################
  #################                           GENERATE IN/OUT EVENTS
  #################  
  ################# 
  ################# 
  #################   
  
                        # df is data
                        # df is data 
                        # df is data
  data <- df
  
  ######################### ######################### #########################   A. Definition of the signals inside the polygons 
  ######################### ######################### #########################
  
# 7. definition of signals in exits and add port name
  coordinates(data) <- c("long", "lat")
  exit_baltic <-readOGR("W:/Florentdrafts","Exit_Baltic_Sea")
  #plot(exit_baltic)
  proj4string(data) <- proj4string(exit_baltic)                                     # to confirm the same reference system
  into_exit_baltic <- !is.na(over(data, as(exit_baltic, "SpatialPolygons")))        # combine is.na() with over() to do the containment test; note that we 
  mean(into_exit_baltic)
  data2<-as.data.frame(data)                                                        # build data frame called data2
  data2$into_exit_baltic <-into_exit_baltic *1
  into_exit_baltic <- over(data, exit_baltic[,"Name"])                              # get the name of the exit: Skagen / Kiel Canal / Lappeenranta / Neva
  data2$Name_exit <- NA
  data2$Name_exit <- into_exit_baltic$Name                                          # add the name
  summary(data2$Name_exit)
  
  #t <- subset(data2, data2$into_exit_baltic==TRUE)
  #newmap <- getMap(resolution = "low")
  #plot(newmap, xlim = c(5.39, 30), ylim = c(52, 65.5), asp = 1, col="grey")         # plot to check
  #points(t$long, t$lat, col = "red", cex = 1)
  
  summary(data2$into_exit_baltic)
 
  data2$opp_into_exit_baltic <- ifelse(data2$into_exit_baltic== 1,0,1)             # if not in the baltic, then outside or vice versa
  
  #t <- subset(data2, data2$opp_into_exit_baltic==0)
  #newmap <- getMap(resolution = "low")
  #plot(newmap, xlim = c(5.39, 30), ylim = c(52, 65.5), asp = 1, col="grey")
  #points(t$long, t$lat, col = "red", cex = 1)                                      # plot to check
  
# 8 . identify signals outside the Baltic Sea to be make sure to create new enter / exit if the ship is using the same location
  coordinates(data2) <- c("long", "lat")
  outside_exits_polygons <-readOGR("W:/Florentdrafts","Outside_Exits")
  #plot(outside_exits_polygons)
  proj4string(data2) <- proj4string(outside_exits_polygons)                       # to confirm the same reference system
  
  outside_exist_signals <- !is.na(over(data2, as(outside_exits_polygons, "SpatialPolygons")))    # combine is.na() with over() to do the containment test; note that we
  mean(outside_exist_signals)
  
  data2<-as.data.frame(data2)                                                     # build data frame
  data2$outside_exist_signals <-outside_exist_signals *1                          # id the signals
  

  #data <- subset(data,outside_exist_signals==0 )                                 # select only signals that not in the polygons (so inside the Baltic Sea)
  
  
  ######################### ######################### #########################   B. Skagen
  ######################### ######################### ######################### 
  data <- subset(data2,data2$Name_exit == "Skagen" | (is.na(Name_exit)) )
  data <- data[order(data$imo, data$timestamp_pretty),] 
  summary(data$Name_exit)
  #newmap <- getMap(resolution = "low")
  #plot(newmap, xlim = c(5.39, 30), ylim = c(52, 65.5), asp = 1, col="grey")
  #points(data$long, data$lat, col = "red", cex = 1)
  
# 9. Count of passage in Skagen
  data$Passage <- setDT(data)[, v1 := if(all(!opp_into_exit_baltic)) c(TRUE, imo[-1]!= imo[-.N]) 
                              else rep(FALSE, .N), .(grp = rleid(opp_into_exit_baltic))][,cumsum(v1)*(!opp_into_exit_baltic)]
  
# 10. bearing N == 0 or South == 90
  data$cog2 <- NA
  data$cog<-as.numeric(data$cog)
  data$cog2 <- ifelse(80<data$cog & data$cog<260,  "180", "0")
  data$cog2<- as.numeric(data$cog2)
  
  #check_passage <- subset(data, data$Passage == 2 )
  #mean(check_passage$cog2)
  
  detach(package:plyr)
  detach(package:dplyr)
  detach(package:tidyr)
  #detach(package:lubridate)
  library(plyr)
  library(tidyr)
  library(dplyr) 
  
# 11. Computer information about the passages in Skagen (time, cog, etc.)
  data_passage <- filter(data, Passage != 0) %>% # drop rows with Stop == 0 
    unite(dates, timestamp_pretty, sep = " ") %>% #create date object
    mutate(dates = as.POSIXct(strptime(dates, format = "%d/%m/%Y %H:%M:%S"))) %>%
    group_by(Passage) %>% # for each stop
    filter(dates == min(dates) | dates == max(dates)) %>% #select rows with min and max dates
    dplyr::summarise(minTime = min(dates),
                     maxTime = max(dates),
                     duration_minutes = round(difftime(max(dates), min(dates), units="mins")),
                     mean_COG= mean(cog),
                     mean_COG2= mean(cog2),
                     Ship = imo[1])
  
# 12. Definition if exit or enter
  #summary(data_passage$mean_COG)
  #entering_Skagen <- subset(data_passage, data_passage$mean_COG < 260 & data_passage$mean_COG > 80  )
  #exiting_Skagen <- subset(data_passage, data_passage$mean_COG > 260 | data_passage$mean_COG < 80  )
  entering_Skagen <- subset(data_passage, data_passage$mean_COG2 ==180  )
  exiting_Skagen <- subset(data_passage, data_passage$mean_COG2 ==0  )
  
  rows_entering <- as.numeric(nrow(entering_Skagen))  
  if (rows_entering !=0) entering_Skagen$event <- "enter"               #add column event = enter or exit if there are observations
  
  rows_exiting <- as.numeric(nrow(exiting_Skagen)) 
  if (rows_exiting !=0)  exiting_Skagen$event <- "exit"                 #add column event = enter or exit if there are observations
    
# 13. merge all passages
  data_Skagen <- rbind(entering_Skagen,exiting_Skagen)
  
# 14. add location name if there are observations
  rows_Skagen <- as.numeric(nrow(data_Skagen))  
  if (rows_Skagen !=0) data_Skagen$name <- "Skagen"
  
  ######################### ######################### #########################   C. Kiel
  ######################### ######################### #########################
  
  data <- subset(data2,data2$Name_exit == "Kiel" | (is.na(Name_exit)) )
  summary(data$Name_exit)
  data <- data[order(data$imo, data$timestamp_pretty),] 
  
# 15. Count of passage in Kiel
  data$Passage <- setDT(data)[, v1 := if(all(!opp_into_exit_baltic)) c(TRUE, imo[-1]!= imo[-.N]) 
                              else rep(FALSE, .N), .(grp = rleid(opp_into_exit_baltic))][,cumsum(v1)*(!opp_into_exit_baltic)]
  
# 16. bearing N == 0 or South == 90
  data$cog2 <- NA
  data$cog<-as.numeric(data$cog)
  data$cog2 <- ifelse(180<data$cog ,  "270", "90")
  data$cog2<- as.numeric(data$cog2)
  
  detach(package:plyr)
  detach(package:dplyr)
  detach(package:tidyr)
  #detach(package:lubridate)
  
  library(plyr)
  library(tidyr)
  library(dplyr)
# 17. Computer information about the passages in Kiel (time, cog, etc.)
  data_passage <- filter(data, Passage != 0) %>% # drop rows with Stop == 0 
  unite(dates, timestamp_pretty, sep = " ") %>% #create date object
  mutate(dates = as.POSIXct(strptime(dates, format = "%d/%m/%Y %H:%M:%S"))) %>%
  group_by(Passage) %>% # for each stop
  filter(dates == min(dates) | dates == max(dates)) %>% #select rows with min and max dates
  summarise(minTime = min(dates),
              maxTime = max(dates),
              duration_minutes = round(difftime(max(dates), min(dates), units="mins")),
              mean_COG= mean(cog),
              mean_COG2= mean(cog2),
              Ship = imo[1])
  
# 18. Definition if exit or enter
  #summary(data_passage$mean_COG)
  exiting_Kiel <- subset(data_passage, data_passage$mean_COG2 ==270 )
  entering_Kiel <- subset(data_passage, data_passage$mean_COG2 == 90 )
  
  rows_entering <- as.numeric(nrow(entering_Kiel))                                  #add column event = enter or exit if there are observations
  if (rows_entering !=0) entering_Kiel$event <- "enter"
  
  rows_exiting <- as.numeric(nrow(exiting_Kiel)) 
  if (rows_exiting !=0)  exiting_Kiel$event <- "exit"
  
# 19. merge all passages
  data_Kiel <- rbind(entering_Kiel,exiting_Kiel)
  
# 20. add location name if there are observations
  rows_Kiel <- as.numeric(nrow(data_Kiel))  
  if (rows_Kiel !=0) data_Kiel$name <- "Kiel"
  
  
  ######################### ######################### #########################   D. Neva River
  ######################### ######################### ######################### 
  
  data <- subset(data2,data2$Name_exit == "Neva_River" | (is.na(Name_exit)) )
  summary(data$Name_exit)
  data <- data[order(data$imo, data$timestamp_pretty),] 
  
 
# 21. Count of passage in Neva
  data$Passage <- setDT(data)[, v1 := if(all(!opp_into_exit_baltic)) c(TRUE, imo[-1]!= imo[-.N]) 
                              else rep(FALSE, .N), .(grp = rleid(opp_into_exit_baltic))][,cumsum(v1)*(!opp_into_exit_baltic)]

# 22. bearing N == 0 or South == 90
  data$cog2 <- NA
  data$cog<-as.numeric(data$cog)
  data$cog2 <- ifelse(270>data$cog & data$cog>90,  "180", "0")
  data$cog2<- as.numeric(data$cog2)
  summary(data$cog2)

  
  detach(package:plyr)
  detach(package:dplyr)
  detach(package:tidyr)
  #detach(package:lubridate)
  library(plyr)
  library(tidyr)
  library(dplyr) 

# 23. Computer information about the passages in Skagen (time, cog, etc.)  
  data_passage <- filter(data, Passage != 0) %>% # drop rows with Stop == 0 
    unite(dates, timestamp_pretty, sep = " ") %>% #create date object
    mutate(dates = as.POSIXct(strptime(dates, format = "%d/%m/%Y %H:%M:%S"))) %>%
    group_by(Passage) %>% # for each stop
    filter(dates == min(dates) | dates == max(dates)) %>% #select rows with min and max dates
    dplyr::summarise(minTime = min(dates),
                     maxTime = max(dates),
                     duration_minutes = round(difftime(max(dates), min(dates), units="mins")),
                     mean_COG= mean(cog),
                     mean_COG2= mean(cog2),
                     Ship = imo[1])
  
# 24. Definition if exit or enter
  entering_Neva <- subset(data_passage, data_passage$mean_COG2 ==0  )
  exiting_Neva <- subset(data_passage, data_passage$mean_COG2 ==180  )
  
  rows_entering <- as.numeric(nrow(entering_Neva))                           
  if (rows_entering !=0) entering_Neva$event <- "enter"
  
  rows_exiting <- as.numeric(nrow(exiting_Neva)) 
  if (rows_exiting !=0)  exiting_Neva$event <- "exit"
  
# 25. merge all passages 
  data_Neva <- rbind(entering_Neva,exiting_Neva)                            
  
# 26. add location name if there are observations
  rows_Neva <- as.numeric(nrow(data_Neva))  
  if (rows_Neva !=0) data_Neva$name <- "Neva"
  
  
  ######################### ######################### #########################   E. Lappeenranta
  ######################### ######################### ######################### 
  
  data <- subset(data2,data2$Name_exit == "Lappeenranta" | (is.na(Name_exit)) )
  summary(data$Name_exit)
  data <- data[order(data$imo, data$timestamp_pretty),] 
  
# 27. Count of passage in Lappeenranta
  data$Passage <- setDT(data)[, v1 := if(all(!opp_into_exit_baltic)) c(TRUE, imo[-1]!= imo[-.N]) 
                              else rep(FALSE, .N), .(grp = rleid(opp_into_exit_baltic))][,cumsum(v1)*(!opp_into_exit_baltic)]
  
 
# 28. bearing N == 0 or South == 90
  data$cog2 <- NA
  data$cog<-as.numeric(data$cog)
  data$cog2 <- ifelse(270<data$cog & data$cog>90,  "0", "180")
  data$cog2<- as.numeric(data$cog2)
  summary(data$cog2)
  #check_passage <- subset(data, data$Passage == 2 )
  #mean(check_passage$cog2)
  
  detach(package:plyr)
  detach(package:dplyr)
  detach(package:tidyr)
  #detach(package:lubridate)
  library(plyr)
  library(tidyr)
  library(dplyr) 
  
# 29. Computer information about the passages in Lappeenranta (time, cog, etc.)
  data_passage <- filter(data, Passage != 0) %>% # drop rows with Stop == 0 
    unite(dates, timestamp_pretty, sep = " ") %>% #create date object
    mutate(dates = as.POSIXct(strptime(dates, format = "%d/%m/%Y %H:%M:%S"))) %>%
    group_by(Passage) %>% # for each stop
    filter(dates == min(dates) | dates == max(dates)) %>% #select rows with min and max dates
    dplyr::summarise(minTime = min(dates),
                     maxTime = max(dates),
                     duration_minutes = round(difftime(max(dates), min(dates), units="mins")),
                     mean_COG= mean(cog),
                     mean_COG2= mean(cog2),
                     Ship = imo[1])
  
# 30. Definition if exit or enter
  entering_Lappeenranta <- subset(data_passage, data_passage$mean_COG2 ==180  )
  exiting_Lappeenranta <- subset(data_passage, data_passage$mean_COG2 ==0  )
  entering_Lappeenranta <- subset(data_passage, data_passage$mean_COG2 ==0  )
  exiting_Lappeenranta <- subset(data_passage, data_passage$mean_COG2 ==180  )
  

  rows_entering <- as.numeric(nrow(entering_Lappeenranta))                               #add column event = enter or exit if there are observations                      
  if (rows_entering !=0) entering_Lappeenranta$event <- "enter"
  
  rows_exiting <- as.numeric(nrow(exiting_Lappeenranta)) 
  if (rows_exiting !=0)  exiting_Lappeenranta$event <- "exit"
  
# 31. merge all passages
  data_Lappeenranta <- rbind(entering_Lappeenranta,exiting_Lappeenranta)
  
# 32. add location name if there are observations
  rows_Lappeenranta <- as.numeric(nrow(data_Lappeenranta))  
  if (rows_Lappeenranta !=0) data_Lappeenranta$name <- "Lappeenranta"
  
  
  
  
  
  
  ######################### ######################### #########################   F. Goteborg
  ######################### ######################### ######################### 
 
  data <- subset(data2,data2$Name_exit == "Goteborg" | (is.na(Name_exit)) )
  summary(data$Name_exit)
  data <- data[order(data$imo, data$timestamp_pretty),] 
  
  #newmap <- getMap(resolution = "low")
  #plot(newmap, xlim = c(5.39, 30), ylim = c(52, 65.5), asp = 1, col="grey")
  #points(data$long, data$lat, col = "red", cex = 1)

# 33. Count of passage in Goteborg
  data$Passage <- setDT(data)[, v1 := if(all(!opp_into_exit_baltic)) c(TRUE, imo[-1]!= imo[-.N]) 
                              else rep(FALSE, .N), .(grp = rleid(opp_into_exit_baltic))][,cumsum(v1)*(!opp_into_exit_baltic)]
  
# 34. bearing N == 0 or South == 90
  data$cog2 <- NA
  data$cog<-as.numeric(data$cog)
  data$cog2 <- ifelse(270>data$cog & 90<data$cog,  "180", "0")
  data$cog2<- as.numeric(data$cog2)
  summary(data$cog2)
  
  detach(package:plyr)
  detach(package:dplyr)
  detach(package:tidyr)
  #detach(package:lubridate)
  library(plyr)
  library(tidyr)
  library(dplyr) 
  
# 35. Computer information about the passages in Goteborg (time, cog, etc.)
  data_passage <- filter(data, Passage != 0) %>% # drop rows with Stop == 0 
    unite(dates, timestamp_pretty, sep = " ") %>% #create date object
    mutate(dates = as.POSIXct(strptime(dates, format = "%d/%m/%Y %H:%M:%S"))) %>%
    group_by(Passage) %>% # for each stop
    filter(dates == min(dates) | dates == max(dates)) %>% #select rows with min and max dates
    dplyr::summarise(minTime = min(dates),
                     maxTime = max(dates),
                     duration_minutes = round(difftime(max(dates), min(dates), units="mins")),
                     mean_COG= mean(cog),
                     mean_COG2= mean(cog2),
                     Ship = imo[1])
  
# 36. Definition if exit or enter
  entering_Goteborg <- subset(data_passage, data_passage$mean_COG2 ==180  )
  exiting_Goteborg <- subset(data_passage, data_passage$mean_COG2 ==0  )
  
  rows_entering <- as.numeric(nrow(entering_Goteborg))  
  if (rows_entering !=0) entering_Goteborg$event <- "enter"                        #add column event = enter or exit if there are observations
         
  
  rows_exiting <- as.numeric(nrow(exiting_Goteborg)) 
  if (rows_exiting !=0)  exiting_Goteborg$event <- "exit"
  
# 37. merge all passages
  data_Goteborg <- rbind(entering_Goteborg,exiting_Goteborg)
  
# 38. add location name if there are observations
  rows_goteborg <- as.numeric(nrow(data_Goteborg))  
  if (rows_goteborg !=0) data_Goteborg$name <- "Goteborg"
  
  
# 39. Merge Skagen, Kiel, Neva_river, Lappeenranta and Goteborg
  total_in_out <- rbind(data_Skagen,data_Kiel)
  total_in_out <- rbind(total_in_out,data_Neva)
  total_in_out <- rbind(total_in_out,data_Lappeenranta)
  total_in_out <- rbind(total_in_out,data_Goteborg)
  colnames(total_in_out)[4] <- "duration.minutes"
  total_in_out$duration.minutes <- NA
  colnames(total_in_out)[9] <- "Location"
  total_in_out$duration.hours <- round(total_in_out$duration.minutes/60,digits=2)
  
  total_in_out$Location <- as.factor(total_in_out$Location)
  summary(total_in_out$Location)
  
  
  ls()
  library(gdata)
  
  keep(df,total_in_out,date, exit_baltic, month, year, start.time, sure = TRUE)
  
  
  
  #################  
  #################  
  #################
  #################                           trips_stops
  #################  
  ################# 
  ################# 
  ################# 
  
# 40. remove signals outside Skagen and inside Kiel
  data <- df
  coordinates(data) <- c("long", "lat")
  outside_exits_polygons <-readOGR("W:/Florentdrafts","Outside_Exits")
  #plot(outside_exits_polygons)
  
# 41. confirm the same reference system
  proj4string(data) <- proj4string(outside_exits_polygons)
  
# 42. overlap outside / exits and AIS data
  outside_exist_signals <- !is.na(over(data, as(outside_exits_polygons, "SpatialPolygons")))
  mean(outside_exist_signals)
  
# 43. build data frame
  data<-as.data.frame(data)
  data$outside_exist_signals <-outside_exist_signals *1                           # adjust format
  
# 44. select only signals that are not in the polygons (so inside the Baltic Sea)
  data <- subset(data,outside_exist_signals==0 )  
  
  ######################### ######################### #########################   A. Definition of the signals
  ######################### ######################### ######################### 
  
# 45. definition of signals in ports and add port name
  coordinates(data) <- c("long", "lat")
  ports_polygons <-readOGR("W:/PROJECTS/Maritime Assessment 2016/Ports","Ports_V3")
  #plot(ports_polygons)
  proj4string(data) <- proj4string(ports_polygons) # confirm the same reference system
  
  inside.port <- !is.na(over(data, as(ports_polygons, "SpatialPolygons"))) # combine is.na() with over() to do the containment test
  mean(inside.port)
  inside.port2 <- over(data, ports_polygons[,"port"])
  
  
# 46. add column to assess if the signal is in the exit polygons of the Baltic Sea 
  #(to be sure that exiting / entering the Baltic Sea is creating a new trip)
  exit_baltic <-readOGR("W:/Florentdrafts","Exit_Baltic_Sea")
  #plot(exit_baltic)
  proj4string(data) <- proj4string(exit_baltic)                              # to confirm the same reference system
  into_exit_baltic <- !is.na(over(data, as(exit_baltic, "SpatialPolygons")))
  mean(into_exit_baltic)
  data$into_exit_baltic <-into_exit_baltic *1
  
  data2<-as.data.frame(data)                                                 #build data frame
  data2$inside.port <-inside.port *1
  #data2$outside.port[data2$inside.port== 1] <- 0
  #data2$outside.port[data2$inside.port== 0] <- 1
  data2$trips_data2 <- data2$inside.port
  data2$trips_data2[data2$into_exit_baltic== 1] <- 1
  
  
# 47. for the signals in the port, if SOG < 0.5 KN (0.926 km/h) then SOG = 0
  summary(data2$sog)
  data2$sog <- data2$sog
  data2$sog <- as.numeric(data2$sog)
  data2$sog <- ifelse( data2$sog < 0.5 & data2$inside.port == "1" , "0", data2$sog)
  data2$sog <- as.numeric(data2$sog)
  summary(data2$sog)
  mean(data2$inside.port)
  
# 48. Def of STOPS: if the SOG is = 0 and the ship is in the port polygon, so it is a stop
  data2$inside.port <-inside.port *1
  #summary(data2$inside.port)
  data2$Stops <- NA
  summary(data2$sog)
  data2$Stops <- ifelse( data2$sog== 0 & data2$inside.port == "1", "1", "0")
  data2$Stops <- as.numeric(data2$Stops)
  mean(data2$Stops)
  
# 49. add name of the port
  data2$Port <- inside.port2$port
  data2$Stops<-as.numeric(data2$Stops)
  data2$Stops<-as.factor(data2$Stops)
  
  #plot to check
  #t <- subset(data2, data2$Stops==TRUE)
  #newmap <- getMap(resolution = "low")
  #plot(newmap, xlim = c(5.39, 30), ylim = c(52, 65.5), asp = 1, col="grey")
  #points(t$long, t$lat, col = "red", cex = 1)
  
  #select only signals in the ports and plot
  #t <- subset(data2, data2$Stops==1)
  #newmap <- getMap(resolution = "low")
  #plot(newmap, xlim = c(5.39, 30), ylim = c(52, 65.5), asp = 1)
  #points(t$long, t$lat, col = "red", cex = 1)
  
  # only sog=0 
  #data<-subset(data, data$sog==0)
  # only in ports_selected
  #data<-subset(data, data$port==1)
  
# 50. Def of Trips: if signal outside the ports = trips and rename dataframe
  data2$Trips <- NA
  data2$Trips <- ifelse(  data2$inside.port == "0", "1", "0")
  #If not stop, then trip:
  #data$Trips[data$Stops== 1] <- 0
  #data$Trips[data$Stops== 0] <- 1
  #data$Trips <- as.factor(data$Trips)
  
  data <- data2 # rename data2 in data 
  
  #plot to check the stops 
  #t <- subset(data, data$Stops==1)
  #newmap <- getMap(resolution = "low")
  #plot(newmap, xlim = c(5.39, 30), ylim = c(52, 65.5), asp = 1, col="grey")
  #points(t$long, t$lat, col = "red", cex = 1)
  data$Stops <- as.numeric(as.character(data$Stops))
  data$Trips <- as.numeric(as.character(data$Trips))
 
# 51. check the new variables and order by ship and time
  summary(data$Trips)
  summary(data$Stops)
  #summary(data$in_port)
  data <- data[order(data$imo, data$timestamp_pretty),] 
  
  #imo_test <- subset(data, data$imo == 207015000)
  #newmap <- getMap(resolution = "low")
  #plot(newmap, xlim = c(5.39, 30), ylim = c(52, 65.5), asp = 1, col="grey")
  #points(imo_test$long, imo_test$lat, col = "red", cex = 1)
  
  
  ########################## ######################## #########################    B. PRODUCE EVENTS STOPS
  ######################### ######################### ######################### 
  
# 52. counting number of stops
  #data$Stops2 <- setDT(data)[, v1 := if(all(!Trips)) c(TRUE, imo[-1]!= imo[-.N])   #not working, taking into account the passages in the ports without stopping  
  #else rep(FALSE, .N), .(grp = rleid(Trips))][,cumsum(v1)*(!Trips)]
  data$Stops2 <- as.numeric(ifelse(  data$Stop == "1", "0", "1"))
  data$Stops2 <- setDT(data)[, v1 := if(all(!Stops2)) c(TRUE, imo[-1]!= imo[-.N]) 
                            else rep(FALSE, .N), .(grp = rleid(Stops2))][,cumsum(v1)*(!Stops2)]
  
# 53. correction of strange trips: if signals are only in ports: it has to create a new stop if the port is different
  setDT(data)[, `:=`(Stops2 = as.character(Stops2), Idindx = rleid(Port))]
  indx <- unique(data, by = "Idindx")[, counter := (1:.N) - 1L, by = Stops2]
  data[indx[counter > 0], Stops2 := paste(Stops2, i.counter, sep = "-"), on = "Idindx"]
  
  data$Idindx[data$Trips== 1] <- 0
  data$Stops2 <- data$Idindx
  data$Stops2[data$Stops== 0] <- 0
  summary(data$Stops2)
  
# 54. duration of the stops
  data_stops <- filter(data, Stops2 != 0) %>%                                       # drop rows with Stop == 0 
    unite(dates, timestamp_pretty, sep = " ") %>% #create date object
    mutate(dates = as.POSIXct(strptime(dates, format = "%d/%m/%Y %H:%M:%S"))) %>%
    group_by(Idindx) %>% # for each stop
    filter(dates == min(dates) | dates == max(dates)) %>%                           #select rows with min and max dates
    summarise(minTime = min(dates),
              maxTime = max(dates),
              duration_minutes = round(difftime(max(dates), min(dates), units="mins")),
              Ship = imo[1],
              Port = Port[1])
  
# 55. calcul of Lat Long of the stops (average)
    cal <- function(x, y){  # A custom function for calculating the mean Lat/Long          
    latToRadians <- x * pi / 180
    longToRadians <- y * pi / 180
    
    x_cartesian <- cos(latToRadians) * cos(longToRadians)
    y_cartesian <- cos(latToRadians) * sin(longToRadians)
    z_cartesian <- sin(latToRadians)
    
    aveX <- sum(x_cartesian) / length(x_cartesian)
    aveY <- sum(y_cartesian) / length(y_cartesian)
    aveZ <- sum(z_cartesian) / length(z_cartesian)
    
    hyp <- sqrt(aveX * aveX + aveY * aveY)
    lat <- atan2(aveZ, hyp)
    long <- atan2(aveY, aveX)
    
    latMean <- lat * 180 / pi
    longMean <- long * 180 / pi
    
    return(as.data.frame(cbind(latMean, longMean)))
  } 
  
  #plot the stops
  #newmap <- getMap(resolution = "low")
  #plot(newmap, xlim = c(5.39, 30), ylim = c(52, 65.5), asp = 1, col="grey")
  #points(t$long , t$lat , col = "blue", cex = 1)
  data$Stops <- as.numeric(data$Stops)
  LatLong <- data %>% # create the LatLong of the stop (data)
    filter(Stops2 != 0) %>%
    group_by(Idindx) %>%
    do(cal(.$lat, .$long))
  
  #plot LatLong
  #newmap <- getMap(resolution = "low")
  #plot(newmap, xlim = c(5.39, 30), ylim = c(52, 65.5), asp = 1, col="grey")
  #points(LatLong$longMean, LatLong$latMean, col = "red", cex = 1)
  LatLong <- as.data.table(LatLong)
  data_stops <- as.data.table(data_stops)

# 56. Join durations of the stops and LatLong   
  data_stops <- full_join(data_stops, LatLong, by="Idindx" )
  

# 57. filter the stops more than 10 min
  data_stops <- filter(data_stops, duration_minutes >10)
  
  ######################### ######################### #########################  C. TRIPS
  ######################### ######################### ######################### 
 
# 58. Counting number of trips 
  data$Trips_rle <- setDT(data)[, v1 := if(all(!trips_data2)) c(TRUE, imo[-1]!= imo[-.N]) 
                                else rep(FALSE, .N), .(grp = rleid(trips_data2))][,cumsum(v1)*(!trips_data2)]
  
  # old versions of rle, does not work
  #data$Trips_rle <- setDT(data)[, v1 := if(all(!Stops)) c(TRUE, imo[-1]!= imo[-.N]) 
  #else rep(FALSE, .N), .(grp = rleid(Stops))][,cumsum(v1)*(!Stops)]
  
  #data$Trips_rle <- setDT(data)[, v1 := if(all(!inside.port)) c(TRUE, imo[-1]!= imo[-.N]) 
  #else rep(FALSE, .N), .(grp = rleid(inside.port))][,cumsum(v1)*(!inside.port)]
  
  #plot the trips to check
  #t <- subset(data, data$Trips_rle > 0)
  #library("RgoogleMaps")
  #newmap <- getMap(resolution = "low")
  #plot(newmap, xlim = c(5.39, 30), ylim = c(52, 65.5), asp = 1, col="grey")
  #points(t$long, t$lat, col = "blue", cex = 1)
  
# 59. Duration of the trips
  duration_trips <- filter(data, Trips_rle !=0) %>% 
    unite(dates, timestamp_pretty, sep = " ") %>%                                   # create date object
    mutate(dates = as.POSIXct(strptime(dates, format = "%d/%m/%Y %H:%M:%S"))) %>%
    group_by(Trips_rle) %>%  
    filter(dates == min(dates) | dates == max(dates)) %>%                           # select rows with min and max dates
    summarise(minTime = min(dates),
              maxTime = max(dates),
              duration_minutes = round(difftime(max(dates), min(dates), units="mins")),
              Ship = imo[1])
  
# 60. Duration of the trips
  data_for_distance <- data                                                         # create data for distance only
  data_for_distance <- as.data.frame(data_for_distance)                             # as data frame
  
# 61. select relevant parameters
  data_for_distance <- data_for_distance[,c("timestamp_pretty", "imo","lat",  "long", "Trips_rle")] 
  
# 62. remove the stops to keep only the trips
  data_for_distance <- subset(data_for_distance, data_for_distance$Trips_rle !=0)
  
  
  #data_for_distance <- ddply(data_for_distance,.(Trips_rle),function(x){          # remove unique Trips_rle
  #if(nrow(x)==1){
  #return(NULL)}
  #else{
  #return(x)
  #}
  #})
  
# 63. Distance sailed in meters
  library(geosphere)
  library(base)
  data_for_distance$Trips_rle <- as.numeric(data_for_distance$Trips_rle)
  distance <- data_for_distance %>%
    group_by(Trips_rle) %>%
    mutate(longlead = lead(long), latlead = lead(lat)) %>%
    na.omit() %>%
    rowwise() %>%
    mutate(dist = distCosine(c(long,lat), c(longlead, latlead)))
  
# 64. Cumulative sum of distance sailed in meters
  options(scipen = 50)
  #t <- subset(distance, distance$dist.sailed == "NaN")
  distance <- transform(distance, dist.sailed = ave(dist, Trips_rle, FUN = cumsum))
  distance <- distance[,c("Trips_rle", "dist", "dist.sailed")] 
  distance <- distance[,c("Trips_rle",  "dist.sailed")] 
  
# 65. Keep longest cumulative distance for each Ids (trips)
  distance.sum <- distance %>% 
    group_by(Trips_rle) %>%
    slice(which.max(dist.sailed)) 
  
# 66. Join duration and distance of the trip 
  trips <- full_join(distance.sum, duration_trips, by="Trips_rle" )
  #filter the stops more than 10 min
  trips <- filter(trips, duration_minutes >10)
  
# 67. prepare the final tables (stops and then trips) for merging 
  colnames(data_stops)[4] <- "duration.minutes"                                # for stops rename the column number 4
  data_stops$event <- "stop"
  colnames(data_stops)[6] <- "Location"                                        # for stops rename the column number 6
  #data_stops <- data_stops[,c("Idindx", "minTime","maxTime","duration.minutes", "Ship", "Location", "event")] 
  
  trips$event <- "trip"                                                        # for trips  
  colnames(trips)[1] <- "Trips.rle"
  colnames(trips)[5] <- "duration.minutes"

# 68. Join stops and trips in same table 
  total_trips_stops <- rbind.fill(data_stops, trips)                           # join                  
  total_trips_stops <- total_trips_stops[,c("Ship", "minTime","maxTime","duration.minutes", "event", "dist.sailed", "Location", "Trips.rle", "Idindx")] # select relevant parameters
  
  total_trips_stops$duration.hours <- round(total_trips_stops$duration.minutes/60,digits=2)  # duration in hours
  total_trips_stops <- total_trips_stops[order(total_trips_stops$Ship, total_trips_stops$minTime),]  # order by ship and time of event

# 69. Merge all final data together (total_trips_stops and total_in_out from the first step)
  total <- rbind.fill( total_in_out, total_trips_stops)
  total <- total[!duplicated(total), ]                                        # remove duplicates
  total <- total[,c("Ship", "minTime","maxTime","duration.minutes", "event", "dist.sailed", "Location","Trips.rle", "Idindx", "mean_COG",
                    "mean_COG2")]                                             # select relevant parameters
  total <- total[order(total$Ship, total$minTime),]                           # order by ship and time of event
  
# 70. merge the consecutive trips together: ships can not have consecutive trips without stops or exit.
  library(dplyr)
  total_final <- total %>% 
    group_by(Ship) %>% 
    mutate(new_id = data.table::rleid(event)) %>% 
    group_by(event, new_id, Ship) %>% 
    mutate(duration.minutes = ifelse(event %in% c('trip', 'stop'), sum(duration.minutes), duration.minutes), maxTime = tail(maxTime, 1)) %>% 
    mutate(dist.sailed = ifelse(event == 'trip', sum(dist.sailed), dist.sailed), dist.sailed = tail(dist.sailed, 1)) %>% 
    filter(!duplicated(new_id)) %>% 
    select(-new_id)
  
# 71. Export the final monthly files events
  dir.create(file.path("E:/Events_V2/"), showWarnings=F)
  dir.to.create1 <- paste("E:/Events_V2/", year, sep="") #level 1 in directory using year as variable
  dir.create(file.path(dir.to.create1), showWarnings=F)
  dir.to.create2 <- paste(dir.to.create1,"/monthly", sep="") # dir.to.create level 2 using year as variable
  dir.create(file.path(dir.to.create2), showWarnings=F)
  
  directory=paste(dir.to.create2, "/Events_", year, "_", month,  sep="", ".csv" )
  write.table(total_final, directory, sep=";", col.names = T, row.names=F)
  
}

#library(mail)
#sendmail("florent.nicolas@helcom.fi", "AIS monthly events for 2016 ready", "AIS monthly events for 2016 ready", password="rmail")

#################  
#################  
#################
#################                           WRITE YEARLY FILES
#################  
################# 
################# 
################# 

rm(list = ls()[!ls() %in% c("start.time", "year","dir.to.create2", "time")])
#library(plyr)
#library(dplyr)

# 72. Read monthly events files using file list
setwd(dir.to.create2)
#filelist with pattern of the monthly events files
pattern <- paste("Events_", year, "_.*\\.csv", sep="")
fileList <- list.files(pattern=pattern, recursive=FALSE)  

# 73. Merging the monthly files
df <- ldply(fileList, read.table, header=T, sep = ";", fill=T)

# 74. Order by ship and time
df <- df[order(df$Ship, df$minTime),]

# 75. select relevant parameters and change their formats
df <- df[,c("Ship", "minTime","maxTime","duration.minutes", "event", "dist.sailed", "Location","Trips.rle", "mean_COG")] 
#data<- subset(df, df$Ship==209276000)

#change formats of some parameters
df$minTime <-  as.POSIXct(as.character(df$minTime), format = "%Y-%m-%d %H:%M:%S")  # change their formats
df <- df[order(df$Ship, df$minTime),]                                # sort again by ship and time
df$duration.minutes <- as.numeric(df$duration.minutes)
df$Ship <- as.factor(df$Ship)

# 76. Merge events that are divided between 2 consecutive months
library(dplyr)
total_final <- df %>% 
  group_by(Ship) %>% 
  mutate(new_id = data.table::rleid(event)) %>% 
  group_by(event, new_id, Ship) %>% 
  mutate(duration.minutes = ifelse(event %in% c('trip', 'stop'), sum(duration.minutes), duration.minutes), maxTime = tail(maxTime, 1)) %>% 
  mutate(dist.sailed = ifelse(event == 'trip', sum(dist.sailed), dist.sailed), dist.sailed = tail(dist.sailed, 1)) %>% 
  filter(!duplicated(new_id)) %>% 
  select(-new_id)

# 77. select again relevant parameters (not needed)
total_final <- total_final[,c("Ship", "minTime","maxTime","duration.minutes", "event", "dist.sailed", "Location","Trips.rle", "mean_COG")] 
 

# 78. writing the yearly event file

directory <- paste("E:/Events_V2/Events_", year, ".csv", sep="")
write.table(total_final, directory, sep=";", col.names = T, row.names=F)


#################  
#################  
#################
#################                           PRODUCE SUMMARY OF THE EVENTS
#################  
################# 
################# 
################# 

# 79. Prepare data
data_stops <- subset(total_final, total_final$event=="stop")                          # for stops
dt <- data.table(data_stops)
data_stops_summary <- dt[,list(duration.minutes=sum(duration.minutes)),by= Ship]
data_stops_summary$stop <- 1

data_trips <- subset(total_final, total_final$event=="trip")                          # for trips
dt <- data.table(data_trips)

# 80. Produce summary (SUM of distance for trips and duration for trips and stops)
data_trips_summary <- dt[,list(distance=sum(dist.sailed),duration.minutes=sum(duration.minutes)),by=Ship]   # for trips
data_trips_summary$trip <- 1

data_exit_enter <- subset(total_final, total_final$event=="exit" |total_final$event=="enter" )    # for enters and exits

# 81. Prepare ship related information
shiplist <- paste("E:/ship_list/shiplist_", year,"_final.csv", sep="")
shiptype <- read.csv(shiplist, sep=";")
shiptype <- shiptype[,c("imo", "country","length_final","width_final", "draught_final", "Gross_tonnage", "Net_tonnage", "HELCOM_Gross_ShipType", "HELCOM_Detail_ShipType")] 
colnames(shiptype)[1] <- "Ship"

data_stops_summary$Ship <- as.factor(data_stops_summary$Ship)
data_trips_summary$Ship <- as.factor(data_trips_summary$Ship)
data_exit_enter$Ship <- as.factor(data_exit_enter$Ship)
shiptype$Ship <- as.factor(shiptype$Ship)

# 82. Add ship related information
summary_stops <- merge(data_stops_summary,shiptype, by= "Ship" )
summary_trips <- merge(data_trips_summary,shiptype, by= "Ship" )
summary_exit_enter <- merge(data_exit_enter,shiptype, by= "Ship" )


# 83. writing summary tables
directory=paste("E:/Events_V2/Summary_stops_", year,".csv", sep="")
write.table(summary_stops, directory, sep=";", col.names = T, row.names=F)

directory=paste("E:/Events_V2/Summary_trips_", year,".csv", sep="")
write.table(summary_trips, directory, sep=";", col.names = T, row.names=F)

directory=paste("E:/Events_V2/Summary_exit_enter_", year,".csv", sep="")
write.table(summary_exit_enter, directory, sep=";", col.names = T, row.names=F)

#library(mail)
#sendmail("florent.nicolas@helcom.fi", "AIS Events for 2016 ready", " AIS Events for 2016 ready", password="rmail")


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
directory_time.taken=paste("E:/Events_V2/time.taken_for_events_", year, sep="", ".csv" )
write.table(time.taken, directory_time.taken, sep=";", col.names = F, row.names=F)

