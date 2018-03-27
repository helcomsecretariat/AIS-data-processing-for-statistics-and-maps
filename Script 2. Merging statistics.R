##### Script 2
# Developped by Florent NICOLAS, HELCOM Secretariat - 2017
# R Version 3.4.3



##Merging descriptive statistics  for the year 2013
##For another year, replace 2013 by the year wanted

# Part 1: merging the monthly statistics (total and duplicated signals_ to be done only after the monthly files are done)
# Part 2: merging the statistics for the year 2013 (to be done when the divisions are ready)


# Part 1 : Merging the monthly statistics files (Number of signals and duplication) ------------------------------------ 
rm(list = ls())
setwd("E:/test_division/division_finale2013/file_list_months_weeks")

fileList <- list.files(pattern="duplication_*", recursive=FALSE)
library(plyr)
duplication_2013 <- ldply(fileList, read.table, header=T, sep = ";", fill=T)

directory_stats_duplications=paste("E:/test_division/division_finale2013/file_list_months_weeks/signals_2013.csv")
#directory_stats_duplications=paste("E:/test_division/division_finale2013/180seconds/file_list_months_weeks/signals_2013.csv")

write.table (duplication_2013, directory_stats_duplications , row.names=F, sep=";", quote=F)
library(mail)
#sendmail("your_email@domain.domain", "monthly statistics files 2013 (Number of signals and duplication) are merged", "monthly statistics files 2013 (Number of signals and duplication) are merged", password="rmail")
rm(list = ls())


# Part 2 : Merging the statistics for the year 2013 ------------------------------------ 
rm(list = ls())
setwd("E:/test_division/division_finale2013/")
fileList <- list.files(pattern="stats_.*\\,.csv", recursive=TRUE)
lenghtlist<-as.numeric(length(fileList))
for (file in fileList) {
  
  library(plyr)
  data <- ldply(fileList, read.table, header=T, sep = ",")
  #data$Errors<-as.numeric(data$Errors)
  dir.create(file.path("E:/test_division/division_finale2013/statistics_2013"), showWarnings=F)
  directory_final=paste("E:/test_division/division_finale2013/statistics_2013/temp_statistics_2013", ".csv")
  write.table (data, directory_final, row.names=F, sep=";", quote=F)
  
}

temp <- read.csv("E:/test_division/division_finale2013/statistics_2013/temp_statistics_2013 .csv", sep=";")
temp$signals<-as.numeric(temp$signals)
test_merge_statistics <- ddply(temp, c("parameters"), summarise,
                               signals    = sum(signals))

write.table (test_merge_statistics, "E:/test_division/division_finale2013/statistics_2013/statistics_2013.csv", row.names=F, sep=";")

library(mail)
#sendmail("your_email@domain.domain", "all statistics 2013 merged", "all statistics 2013 merged", password="rmail")

rm(list = ls())
rm(list = ls())

