setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbo")

# load packages
library(zoo)
library(dplyr)
library(stringr)

data_merged_kbo <- readRDS("data_merged_kbo.rds")

# data with new columns that contain rolling means of predictors
data_merged_rol_kbo <- data_merged_kbo

variablenames <- c("drainage", "airtmp", "glorad", "groundwaterdepth", "precip", "qinfiltration",
                   "relhum", "snowstorage", "soilwaterrootzone", "soilwaterunsatzone")

for (i in variablenames) {
  for (j in colnames(data_merged_rol_kbo)[str_detect(colnames(data_merged_rol_kbo), i)]) {
    new_columnname <- paste0("rol_", j)
    if (i %in% c("airtmp", "glorad", "precip", "qinfiltration", "relhum")) {
      data_merged_rol_kbo <- data_merged_rol_kbo %>%
        mutate(!!new_columnname := rollapply(data_merged_rol_kbo[, j], 8 * 7, mean, align = 'right', fill = NA))
    }
    else if (i == "snowstorage") {
      data_merged_rol_kbo <- data_merged_rol_kbo %>%
        mutate(!!new_columnname := rollapply(data_merged_rol_kbo[, j], 8 * 30, mean, align = 'right', fill = NA))
    } else if (i %in% c("soilwaterrootzone", "soilwaterunsatzone")) {
      data_merged_rol_kbo <- data_merged_rol_kbo %>%
        mutate(!!new_columnname := rollapply(data_merged_rol_kbo[, j], 8 * 60, mean, align = 'right', fill = NA))
    } else if (i == "groundwaterdepth") {
      data_merged_rol_kbo <- data_merged_rol_kbo %>%
        mutate(!!new_columnname := rollapply(data_merged_rol_kbo[, j], 8 * 90, mean, align = 'right', fill = NA))
    }
    
  }
}

saveRDS(data_merged_rol_kbo, "data_merged_rol_kbo.rds")
