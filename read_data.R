setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbe")

# load packages
library(stringr)

# read in data
data_drainage <- read.table("gerinneabfluss_1981_2010_sub.txt", header = TRUE)
data_airtmp <- read.table("airtmp_1981_2010_weight_weight_sub.txt", header = TRUE)
data_glorad <- read.table("glorad_1981_2010_weight_weight_sub.txt", header = TRUE)
data_groundwaterdepth <- read.table("groundwaterdepth_1981_2010_weight_weight_sub.txt", header = TRUE)
data_precip <- read.table("precip_1981_2010_weight_weight_sub.txt", header = TRUE)
data_qinfiltration <- read.table("qinfiltartionfirstlayer_1981_2010_weight_weight_sub.txt", header = TRUE)
data_relhum <- read.table("relhum_1981_2010_weight_weight_sub.txt", header = TRUE)
data_snowstorage <- read.table("snowstorage_1981_2010_weight_weight_sub.txt", header = TRUE)
data_soilwaterrootzone <- read.table("soilwaterrootzone_1981_2010_weight_weight_sub.txt", header = TRUE)
data_soilwaterunsatzone <- read.table("soilwaterunsatzone_1981_2010_weight_weight_sub.txt", header = TRUE)

# rename columnnames for better identification
data_drainage_named <- data_drainage
colnames(data_drainage_named)[colnames(data_drainage_named) %in% str_subset(colnames(data_drainage_named), "X")] <-
  paste0("drainage_", colnames(data_drainage_named)[colnames(data_drainage_named) %in% str_subset(colnames(data_drainage_named), "X")]) 

data_airtmp_named <- data_airtmp
colnames(data_airtmp_named)[colnames(data_airtmp_named) %in% str_subset(colnames(data_airtmp_named), "X")] <-
  paste0("airtmp_", colnames(data_airtmp_named)[colnames(data_airtmp_named) %in% str_subset(colnames(data_airtmp_named), "X")]) 

data_glorad_named <- data_glorad
colnames(data_glorad_named)[colnames(data_glorad_named) %in% str_subset(colnames(data_glorad_named), "X")] <-
  paste0("glorad_", colnames(data_glorad_named)[colnames(data_glorad_named) %in% str_subset(colnames(data_glorad_named), "X")]) 

data_groundwaterdepth_named <- data_groundwaterdepth
colnames(data_groundwaterdepth_named)[colnames(data_groundwaterdepth_named) %in% str_subset(colnames(data_groundwaterdepth_named), "X")] <-
  paste0("groundwaterdepth_", colnames(data_groundwaterdepth_named)[colnames(data_groundwaterdepth_named) %in% str_subset(colnames(data_groundwaterdepth_named), "X")]) 

data_precip_named <- data_precip
colnames(data_precip_named)[colnames(data_precip_named) %in% str_subset(colnames(data_precip_named), "X")] <-
  paste0("precip_", colnames(data_precip_named)[colnames(data_precip_named) %in% str_subset(colnames(data_precip_named), "X")]) 

data_qinfiltration_named <- data_qinfiltration
colnames(data_qinfiltration_named)[colnames(data_qinfiltration_named) %in% str_subset(colnames(data_qinfiltration_named), "X")] <-
  paste0("qinfiltration_", colnames(data_qinfiltration_named)[colnames(data_qinfiltration_named) %in% str_subset(colnames(data_qinfiltration_named), "X")]) 

data_relhum_named <- data_relhum
colnames(data_relhum_named)[colnames(data_relhum_named) %in% str_subset(colnames(data_relhum_named), "X")] <-
  paste0("relhum_", colnames(data_relhum_named)[colnames(data_relhum_named) %in% str_subset(colnames(data_relhum_named), "X")]) 

data_snowstorage_named <- data_snowstorage
colnames(data_snowstorage_named)[colnames(data_snowstorage_named) %in% str_subset(colnames(data_snowstorage_named), "X")] <-
  paste0("snowstorage_", colnames(data_snowstorage_named)[colnames(data_snowstorage_named) %in% str_subset(colnames(data_snowstorage_named), "X")]) 

data_soilwaterrootzone_named <- data_soilwaterrootzone
colnames(data_soilwaterrootzone_named)[colnames(data_soilwaterrootzone_named) %in% str_subset(colnames(data_soilwaterrootzone_named), "X")] <-
  paste0("soilwaterrootzone_", colnames(data_soilwaterrootzone_named)[colnames(data_soilwaterrootzone_named) %in% str_subset(colnames(data_soilwaterrootzone_named), "X")]) 

data_soilwaterunsatzone_named <- data_soilwaterunsatzone
colnames(data_soilwaterunsatzone_named)[colnames(data_soilwaterunsatzone_named) %in% str_subset(colnames(data_soilwaterunsatzone_named), "X")] <-
  paste0("soilwaterunsatzone_", colnames(data_soilwaterunsatzone_named)[colnames(data_soilwaterunsatzone_named) %in% str_subset(colnames(data_soilwaterunsatzone_named), "X")]) 

# merge data
data_merged <- merge(data_drainage_named, data_airtmp_named, by.x = c("YY", "MM", "DD", "HH"), 
                   by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged <- merge(data_merged, data_glorad_named, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged <- merge(data_merged, data_groundwaterdepth_named, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged <- merge(data_merged, data_precip_named, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged <- merge(data_merged, data_qinfiltration_named, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged <- merge(data_merged, data_relhum_named, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged <- merge(data_merged, data_snowstorage_named, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged <- merge(data_merged, data_soilwaterrootzone_named, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged <- merge(data_merged, data_soilwaterunsatzone_named, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)

data_merged <- data_merged[order(data_merged$YY, data_merged$MM, data_merged$DD, data_merged$HH),]
# add row ind
data_merged$ind <- 1:nrow(data_merged)
# saveRDS(data_merged, "data_merged.rds")
