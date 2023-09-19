setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbe")

data_merged <- readRDS("data_merged.rds")

# data with new columns that contain rolling means of predictors
data_merged_rol <- data_merged

# short version?
# variablenames <- c("drainage", "airtmp", "glorad", "groundwaterdepth", "precip", "qinfiltration",
#                    "relhum", "snowstorage", "soilwaterrootzone", "soilwaterunsatzone")
# 
# library(dplyr)
# library(zoo)
# 
# for (i in variablenames) {
#   for (j in colnames(data_merged_rol)[str_detect(colnames(data_merged_rol), i)]) {
#     if (i %in% c("airtmp", "glorad", "precip", "relhum")) {
#       new_columnname <- paste0("rol_", j)
#       data_merged_rol <- data_merged_rol %>%
#         mutate(!!new_columnname := rollapply(j, 8 * 7, mean, align = 'right', fill = NA))
#     } 
#     # else if (i %in% c("snowstorage")) {
#     #   data_merged_rol <- data_merged_rol %>%
#     #     mutate(paste0("rol_", j) = rollapply(j, 8 * 30, mean, align = 'right', fill = NA))
#     # } else if (i %in% c("soilwaterrootzone", "soilwaterunsatzone")) {
#     #   data_merged_rol <- data_merged_rol %>%
#     #     mutate(paste0("rol_", j) = rollapply(j, 8 * 60, mean, align = 'right', fill = NA))
#     # }
#     
#   }
# }
# 
# 
# for (i in variablenames) {
#   for (j in colnames(data_merged_rol)[str_detect(colnames(data_merged_rol), i)]) {
#     if (i %in% c("airtmp", "glorad", "precip", "relhum")) {
#       new_columnname <- paste0("rol_", j)
#       data_merged_rol <- data_merged_rol %>%
#         mutate(!!new_columnname := rollapply(j, 8 * 7, mean, align = 'right', fill = NA))
#     } 
#     # else if (i %in% c("snowstorage")) {
#     #   data_merged_rol <- data_merged_rol %>%
#     #     mutate(paste0("rol_", j) = rollapply(j, 8 * 30, mean, align = 'right', fill = NA))
#     # } else if (i %in% c("soilwaterrootzone", "soilwaterunsatzone")) {
#     #   data_merged_rol <- data_merged_rol %>%
#     #     mutate(paste0("rol_", j) = rollapply(j, 8 * 60, mean, align = 'right', fill = NA))
#     # }
#     
#   }
# }
# 
# for (j in colnames(data_merged_rol)[str_detect(colnames(data_merged_rol), "airtmp")]) {
#   data_merged_rol[, paste0("rol_", j)] <- NA
#   for (k in colnames(data_merged_rol)[str_detect(colnames(data_merged_rol), paste0("rol_", "airtmp"))]) {
#     for (r in 1:nrow(data_merged_rol)) {
#       if (r >= 8 * 7) {
#         data_merged_rol[r, k] <- mean(data_merged_rol[(r - 8 * 7 + 1):r, j])
#       }
#     }
#   }
# }

# long version
data_merged_rol$rol_airtmp_X10302 <- rollapply(data_merged_rol$airtmp_X10302, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_airtmp_X10303 <- rollapply(data_merged_rol$airtmp_X10303, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_airtmp_X10304 <- rollapply(data_merged_rol$airtmp_X10304, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_airtmp_X10321 <- rollapply(data_merged_rol$airtmp_X10321, 8 * 7, mean, align = 'right', fill = NA)

data_merged_rol$rol_glorad_X10302 <- rollapply(data_merged_rol$glorad_X10302, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_glorad_X10303 <- rollapply(data_merged_rol$glorad_X10303, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_glorad_X10304 <- rollapply(data_merged_rol$glorad_X10304, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_glorad_X10321 <- rollapply(data_merged_rol$glorad_X10321, 8 * 7, mean, align = 'right', fill = NA)

data_merged_rol$rol_groundwaterdepth_X10302 <- rollapply(data_merged_rol$groundwaterdepth_X10302, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_groundwaterdepth_X10303 <- rollapply(data_merged_rol$groundwaterdepth_X10303, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_groundwaterdepth_X10304 <- rollapply(data_merged_rol$groundwaterdepth_X10304, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_groundwaterdepth_X10321 <- rollapply(data_merged_rol$groundwaterdepth_X10321, 8 * 7, mean, align = 'right', fill = NA)

data_merged_rol$rol_precip_X10302 <- rollapply(data_merged_rol$precip_X10302, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_precip_X10303 <- rollapply(data_merged_rol$precip_X10303, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_precip_X10304 <- rollapply(data_merged_rol$precip_X10304, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_precip_X10321 <- rollapply(data_merged_rol$precip_X10321, 8 * 7, mean, align = 'right', fill = NA)

data_merged_rol$rol_qinfiltration_X10302 <- rollapply(data_merged_rol$qinfiltration_X10302, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_qinfiltration_X10303 <- rollapply(data_merged_rol$qinfiltration_X10303, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_qinfiltration_X10304 <- rollapply(data_merged_rol$qinfiltration_X10304, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_qinfiltration_X10321 <- rollapply(data_merged_rol$qinfiltration_X10321, 8 * 7, mean, align = 'right', fill = NA)

data_merged_rol$rol_relhum_X10302 <- rollapply(data_merged_rol$relhum_X10302, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_relhum_X10303 <- rollapply(data_merged_rol$relhum_X10303, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_relhum_X10304 <- rollapply(data_merged_rol$relhum_X10304, 8 * 7, mean, align = 'right', fill = NA)
data_merged_rol$rol_relhum_X10321 <- rollapply(data_merged_rol$relhum_X10321, 8 * 7, mean, align = 'right', fill = NA)

data_merged_rol$rol_snowstorage_X10302 <- rollapply(data_merged_rol$snowstorage_X10302, 8 * 30, mean, align = 'right', fill = NA)
data_merged_rol$rol_snowstorage_X10303 <- rollapply(data_merged_rol$snowstorage_X10303, 8 * 30, mean, align = 'right', fill = NA)
data_merged_rol$rol_snowstorage_X10304 <- rollapply(data_merged_rol$snowstorage_X10304, 8 * 30, mean, align = 'right', fill = NA)
data_merged_rol$rol_snowstorage_X10321 <- rollapply(data_merged_rol$snowstorage_X10321, 8 * 30, mean, align = 'right', fill = NA)

data_merged_rol$rol_soilwaterrootzone_X10302 <- rollapply(data_merged_rol$soilwaterrootzone_X10302, 8 * 60, mean, align = 'right', fill = NA)
data_merged_rol$rol_soilwaterrootzone_X10303 <- rollapply(data_merged_rol$soilwaterrootzone_X10303, 8 * 60, mean, align = 'right', fill = NA)
data_merged_rol$rol_soilwaterrootzone_X10304 <- rollapply(data_merged_rol$soilwaterrootzone_X10304, 8 * 60, mean, align = 'right', fill = NA)
data_merged_rol$rol_soilwaterrootzone_X10321 <- rollapply(data_merged_rol$soilwaterrootzone_X10321, 8 * 60, mean, align = 'right', fill = NA)

data_merged_rol$rol_soilwaterunsatzone_X10302 <- rollapply(data_merged_rol$soilwaterunsatzone_X10302, 8 * 60, mean, align = 'right', fill = NA)
data_merged_rol$rol_soilwaterunsatzone_X10303 <- rollapply(data_merged_rol$soilwaterunsatzone_X10303, 8 * 60, mean, align = 'right', fill = NA)
data_merged_rol$rol_soilwaterunsatzone_X10304 <- rollapply(data_merged_rol$soilwaterunsatzone_X10304, 8 * 60, mean, align = 'right', fill = NA)
data_merged_rol$rol_soilwaterunsatzone_X10321 <- rollapply(data_merged_rol$soilwaterunsatzone_X10321, 8 * 60, mean, align = 'right', fill = NA)

saveRDS(data_merged_rol, "data_merged_rol.rds")
