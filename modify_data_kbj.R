setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbj")

# load packages
library(zoo)
library(dplyr)
library(stringr)

data_merged_kbj <- readRDS("data_merged_kbj.rds")

# data with new columns that contain rolling means of predictors
data_merged_rol_kbj <- data_merged_kbj

variablenames <- c("drainage", "airtmp", "glorad", "groundwaterdepth", "precip", "qinfiltration",
                   "relhum", "snowstorage", "soilwaterrootzone", "soilwaterunsatzone")

for (i in variablenames) {
  for (j in colnames(data_merged_rol_kbj)[str_detect(colnames(data_merged_rol_kbj), i)]) {
    new_columnname <- paste0("rol_", j)
    if (i %in% c("airtmp", "glorad", "precip", "qinfiltration", "relhum")) {
      data_merged_rol_kbj <- data_merged_rol_kbj %>%
        mutate(!!new_columnname := rollapply(data_merged_rol_kbj[, j], 8 * 7, mean, align = 'right', fill = NA))
    }
    else if (i == "snowstorage") {
      data_merged_rol_kbj <- data_merged_rol_kbj %>%
        mutate(!!new_columnname := rollapply(data_merged_rol_kbj[, j], 8 * 30, mean, align = 'right', fill = NA))
    } else if (i %in% c("soilwaterrootzone", "soilwaterunsatzone")) {
      data_merged_rol_kbj <- data_merged_rol_kbj %>%
        mutate(!!new_columnname := rollapply(data_merged_rol_kbj[, j], 8 * 60, mean, align = 'right', fill = NA))
    } else if (i == "groundwaterdepth") {
      data_merged_rol_kbj <- data_merged_rol_kbj %>%
        mutate(!!new_columnname := rollapply(data_merged_rol_kbj[, j], 8 * 90, mean, align = 'right', fill = NA))
    }
    
  }
}

saveRDS(data_merged_rol_kbj, "data_merged_rol_kbj.rds")
