setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbj")

# load packages
library(stringr)

# read in data
data_drainage_kbj <- read.table("gerinneabfluss_1981_2010_sub.txt", header = TRUE)
data_airtmp_kbj <- read.table("airtmp_1981_2010_weight_weight_sub.txt", header = TRUE)
data_glorad_kbj <- read.table("glorad_1981_2010_weight_weight_sub.txt", header = TRUE)
data_groundwaterdepth_kbj <- read.table("groundwaterdepth_1981_2010_weight_weight_sub.txt", header = TRUE)
data_precip_kbj <- read.table("precip_1981_2010_weight_weight_sub.txt", header = TRUE)
data_qinfiltration_kbj <- read.table("qinfiltartionfirstlayer_1981_2010_weight_weight_sub.txt", header = TRUE)
data_relhum_kbj <- read.table("relhum_1981_2010_weight_weight_sub.txt", header = TRUE)
data_snowstorage_kbj <- read.table("snowstorage_1981_2010_weight_weight_sub.txt", header = TRUE)
data_soilwaterrootzone_kbj <- read.table("soilwaterrootzone_1981_2010_weight_weight_sub.txt", header = TRUE)
data_soilwaterunsatzone_kbj <- read.table("soilwaterunsatzone_1981_2010_weight_weight_sub.txt", header = TRUE)

# long version
data_drainage_named_kbj <- data_drainage_kbj
colnames(data_drainage_named_kbj)[colnames(data_drainage_named_kbj) %in% str_subset(colnames(data_drainage_named_kbj), "X")] <-
  paste0("drainage_", colnames(data_drainage_named_kbj)[colnames(data_drainage_named_kbj) %in% str_subset(colnames(data_drainage_named_kbj), "X")]) 

data_airtmp_named_kbj <- data_airtmp_kbj
colnames(data_airtmp_named_kbj)[colnames(data_airtmp_named_kbj) %in% str_subset(colnames(data_airtmp_named_kbj), "X")] <-
  paste0("airtmp_", colnames(data_airtmp_named_kbj)[colnames(data_airtmp_named_kbj) %in% str_subset(colnames(data_airtmp_named_kbj), "X")]) 

data_glorad_named_kbj <- data_glorad_kbj
colnames(data_glorad_named_kbj)[colnames(data_glorad_named_kbj) %in% str_subset(colnames(data_glorad_named_kbj), "X")] <-
  paste0("glorad_", colnames(data_glorad_named_kbj)[colnames(data_glorad_named_kbj) %in% str_subset(colnames(data_glorad_named_kbj), "X")]) 

data_groundwaterdepth_named_kbj <- data_groundwaterdepth_kbj
colnames(data_groundwaterdepth_named_kbj)[colnames(data_groundwaterdepth_named_kbj) %in% str_subset(colnames(data_groundwaterdepth_named_kbj), "X")] <-
  paste0("groundwaterdepth_", colnames(data_groundwaterdepth_named_kbj)[colnames(data_groundwaterdepth_named_kbj) %in% str_subset(colnames(data_groundwaterdepth_named_kbj), "X")]) 

data_precip_named_kbj <- data_precip_kbj
colnames(data_precip_named_kbj)[colnames(data_precip_named_kbj) %in% str_subset(colnames(data_precip_named_kbj), "X")] <-
  paste0("precip_", colnames(data_precip_named_kbj)[colnames(data_precip_named_kbj) %in% str_subset(colnames(data_precip_named_kbj), "X")]) 

data_qinfiltration_named_kbj <- data_qinfiltration_kbj
colnames(data_qinfiltration_named_kbj)[colnames(data_qinfiltration_named_kbj) %in% str_subset(colnames(data_qinfiltration_named_kbj), "X")] <-
  paste0("qinfiltration_", colnames(data_qinfiltration_named_kbj)[colnames(data_qinfiltration_named_kbj) %in% str_subset(colnames(data_qinfiltration_named_kbj), "X")]) 

data_relhum_named_kbj <- data_relhum_kbj
colnames(data_relhum_named_kbj)[colnames(data_relhum_named_kbj) %in% str_subset(colnames(data_relhum_named_kbj), "X")] <-
  paste0("relhum_", colnames(data_relhum_named_kbj)[colnames(data_relhum_named_kbj) %in% str_subset(colnames(data_relhum_named_kbj), "X")]) 

data_snowstorage_named_kbj <- data_snowstorage_kbj
colnames(data_snowstorage_named_kbj)[colnames(data_snowstorage_named_kbj) %in% str_subset(colnames(data_snowstorage_named_kbj), "X")] <-
  paste0("snowstorage_", colnames(data_snowstorage_named_kbj)[colnames(data_snowstorage_named_kbj) %in% str_subset(colnames(data_snowstorage_named_kbj), "X")]) 

data_soilwaterrootzone_named_kbj <- data_soilwaterrootzone_kbj
colnames(data_soilwaterrootzone_named_kbj)[colnames(data_soilwaterrootzone_named_kbj) %in% str_subset(colnames(data_soilwaterrootzone_named_kbj), "X")] <-
  paste0("soilwaterrootzone_", colnames(data_soilwaterrootzone_named_kbj)[colnames(data_soilwaterrootzone_named_kbj) %in% str_subset(colnames(data_soilwaterrootzone_named_kbj), "X")]) 

data_soilwaterunsatzone_named_kbj <- data_soilwaterunsatzone_kbj
colnames(data_soilwaterunsatzone_named_kbj)[colnames(data_soilwaterunsatzone_named_kbj) %in% str_subset(colnames(data_soilwaterunsatzone_named_kbj), "X")] <-
  paste0("soilwaterunsatzone_", colnames(data_soilwaterunsatzone_named_kbj)[colnames(data_soilwaterunsatzone_named_kbj) %in% str_subset(colnames(data_soilwaterunsatzone_named_kbj), "X")]) 

# merge data
data_merged_kbj <- merge(data_drainage_named_kbj, data_airtmp_named_kbj, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbj <- merge(data_merged_kbj, data_glorad_named_kbj, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbj <- merge(data_merged_kbj, data_groundwaterdepth_named_kbj, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbj <- merge(data_merged_kbj, data_precip_named_kbj, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbj <- merge(data_merged_kbj, data_qinfiltration_named_kbj, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbj <- merge(data_merged_kbj, data_relhum_named_kbj, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbj <- merge(data_merged_kbj, data_snowstorage_named_kbj, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbj <- merge(data_merged_kbj, data_soilwaterrootzone_named_kbj, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)
data_merged_kbj <- merge(data_merged_kbj, data_soilwaterunsatzone_named_kbj, by.x = c("YY", "MM", "DD", "HH"), 
                     by.y = c("YY", "MM", "DD", "HH"), all.x = TRUE, all.y = TRUE)

saveRDS(data_merged_kbj, "data_merged_kbj.rds")
