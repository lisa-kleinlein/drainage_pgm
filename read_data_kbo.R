setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbo")

# load packages
library(stringr)

# read in data
data_drainage_kbo <- read.table("gerinneabfluss_1981_2010_sub.txt", header = TRUE)
data_airtmp_kbo <- read.table("airtmp_1981_2010_weight_weight_sub.txt", header = TRUE)
data_glorad_kbo <- read.table("glorad_1981_2010_weight_weight_sub.txt", header = TRUE)
data_groundwaterdepth_kbo <- read.table("groundwaterdepth_1981_2010_weight_weight_sub.txt", header = TRUE)
data_precip_kbo <- read.table("precip_1981_2010_weight_weight_sub.txt", header = TRUE)
data_qinfiltration_kbo <- read.table("qinfiltartionfirstlayer_1981_2010_weight_weight_sub.txt", header = TRUE)
data_relhum_kbo <- read.table("relhum_1981_2010_weight_weight_sub.txt", header = TRUE)
data_snowstorage_kbo <- read.table("snowstorage_1981_2010_weight_weight_sub.txt", header = TRUE)
data_soilwaterrootzone_kbo <- read.table("soilwaterrootzone_1981_2010_weight_weight_sub.txt", header = TRUE)
data_soilwaterunsatzone_kbo <- read.table("soilwaterunsatzone_1981_2010_weight_weight_sub.txt", header = TRUE)

# long version
data_drainage_named_kbo <- data_drainage_kbo
colnames(data_drainage_named_kbo)[colnames(data_drainage_named_kbo) %in% str_subset(colnames(data_drainage_named_kbo), "X")] <-
  paste0("drainage_", colnames(data_drainage_named_kbo)[colnames(data_drainage_named_kbo) %in% str_subset(colnames(data_drainage_named_kbo), "X")]) 

data_airtmp_named_kbo <- data_airtmp_kbo
colnames(data_airtmp_named_kbo)[colnames(data_airtmp_named_kbo) %in% str_subset(colnames(data_airtmp_named_kbo), "X")] <-
  paste0("airtmp_", colnames(data_airtmp_named_kbo)[colnames(data_airtmp_named_kbo) %in% str_subset(colnames(data_airtmp_named_kbo), "X")]) 

data_glorad_named_kbo <- data_glorad_kbo
colnames(data_glorad_named_kbo)[colnames(data_glorad_named_kbo) %in% str_subset(colnames(data_glorad_named_kbo), "X")] <-
  paste0("glorad_", colnames(data_glorad_named_kbo)[colnames(data_glorad_named_kbo) %in% str_subset(colnames(data_glorad_named_kbo), "X")]) 

data_groundwaterdepth_named_kbo <- data_groundwaterdepth_kbo
colnames(data_groundwaterdepth_named_kbo)[colnames(data_groundwaterdepth_named_kbo) %in% str_subset(colnames(data_groundwaterdepth_named_kbo), "X")] <-
  paste0("groundwaterdepth_", colnames(data_groundwaterdepth_named_kbo)[colnames(data_groundwaterdepth_named_kbo) %in% str_subset(colnames(data_groundwaterdepth_named_kbo), "X")]) 

data_precip_named_kbo <- data_precip_kbo
colnames(data_precip_named_kbo)[colnames(data_precip_named_kbo) %in% str_subset(colnames(data_precip_named_kbo), "X")] <-
  paste0("precip_", colnames(data_precip_named_kbo)[colnames(data_precip_named_kbo) %in% str_subset(colnames(data_precip_named_kbo), "X")]) 

data_qinfiltration_named_kbo <- data_qinfiltration_kbo
colnames(data_qinfiltration_named_kbo)[colnames(data_qinfiltration_named_kbo) %in% str_subset(colnames(data_qinfiltration_named_kbo), "X")] <-
  paste0("qinfiltration_", colnames(data_qinfiltration_named_kbo)[colnames(data_qinfiltration_named_kbo) %in% str_subset(colnames(data_qinfiltration_named_kbo), "X")]) 

data_relhum_named_kbo <- data_relhum_kbo
colnames(data_relhum_named_kbo)[colnames(data_relhum_named_kbo) %in% str_subset(colnames(data_relhum_named_kbo), "X")] <-
  paste0("relhum_", colnames(data_relhum_named_kbo)[colnames(data_relhum_named_kbo) %in% str_subset(colnames(data_relhum_named_kbo), "X")]) 

data_snowstorage_named_kbo <- data_snowstorage_kbo
colnames(data_snowstorage_named_kbo)[colnames(data_snowstorage_named_kbo) %in% str_subset(colnames(data_snowstorage_named_kbo), "X")] <-
  paste0("snowstorage_", colnames(data_snowstorage_named_kbo)[colnames(data_snowstorage_named_kbo) %in% str_subset(colnames(data_snowstorage_named_kbo), "X")]) 

data_soilwaterrootzone_named_kbo <- data_soilwaterrootzone_kbo
colnames(data_soilwaterrootzone_named_kbo)[colnames(data_soilwaterrootzone_named_kbo) %in% str_subset(colnames(data_soilwaterrootzone_named_kbo), "X")] <-
  paste0("soilwaterrootzone_", colnames(data_soilwaterrootzone_named_kbo)[colnames(data_soilwaterrootzone_named_kbo) %in% str_subset(colnames(data_soilwaterrootzone_named_kbo), "X")]) 

data_soilwaterunsatzone_named_kbo <- data_soilwaterunsatzone_kbo
colnames(data_soilwaterunsatzone_named_kbo)[colnames(data_soilwaterunsatzone_named_kbo) %in% str_subset(colnames(data_soilwaterunsatzone_named_kbo), "X")] <-
  paste0("soilwaterunsatzone_", colnames(data_soilwaterunsatzone_named_kbo)[colnames(data_soilwaterunsatzone_named_kbo) %in% str_subset(colnames(data_soilwaterunsatzone_named_kbo), "X")]) 

# merge data
data_merged_kbo <- merge(data_drainage_named_kbo, data_airtmp_named_kbo, by.x = c("YY", "MM", "DD", "HH"), 
                         by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbo <- merge(data_merged_kbo, data_glorad_named_kbo, by.x = c("YY", "MM", "DD", "HH"), 
                         by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbo <- merge(data_merged_kbo, data_groundwaterdepth_named_kbo, by.x = c("YY", "MM", "DD", "HH"), 
                         by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbo <- merge(data_merged_kbo, data_precip_named_kbo, by.x = c("YY", "MM", "DD", "HH"), 
                         by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbo <- merge(data_merged_kbo, data_qinfiltration_named_kbo, by.x = c("YY", "MM", "DD", "HH"), 
                         by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbo <- merge(data_merged_kbo, data_relhum_named_kbo, by.x = c("YY", "MM", "DD", "HH"), 
                         by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbo <- merge(data_merged_kbo, data_snowstorage_named_kbo, by.x = c("YY", "MM", "DD", "HH"), 
                         by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbo <- merge(data_merged_kbo, data_soilwaterrootzone_named_kbo, by.x = c("YY", "MM", "DD", "HH"), 
                         by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbo <- merge(data_merged_kbo, data_soilwaterunsatzone_named_kbo, by.x = c("YY", "MM", "DD", "HH"), 
                         by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)

data_merged_kbo <- data_merged_kbo[order(data_merged_kbo$YY, data_merged_kbo$MM, data_merged_kbo$DD, data_merged_kbo$HH),]
# add row ind
data_merged_kbo$ind <- 1:nrow(data_merged_kbo)
# saveRDS(data_merged_kbo, "data_merged_kbo.rds")
