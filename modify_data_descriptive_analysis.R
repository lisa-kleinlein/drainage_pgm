setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbe")

library(dplyr)
library(tidyr)

# load data
data_merged_rol <- readRDS("data_merged_rol.rds")
# summer data set
data_merged_rol_winter <- data_merged_rol[data_merged_rol$MM < 5 | data_merged_rol$MM > 10, ]
data_merged_rol_summer <- data_merged_rol[data_merged_rol$MM >= 5 & data_merged_rol$MM <= 10, ]



### data preperation
## winter
# drainage
data_table_min_drainage_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(min_drainage_X10302 = min(drainage_X10302, na.rm = TRUE),
            min_drainage_X10321 = min(drainage_X10321, na.rm = TRUE),
            min_drainage_X10303 = min(drainage_X10303, na.rm = TRUE),
            min_drainage_X10304 = min(drainage_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_drainage_X10302", "min_drainage_X10321", "min_drainage_X10303", "min_drainage_X10304"),
               names_to = "catchment",
               names_prefix = "min_drainage_",
               values_to = "min_drainage")

data_table_mean_drainage_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(mean_drainage_X10302 = mean(drainage_X10302, na.rm = TRUE),
            mean_drainage_X10321 = mean(drainage_X10321, na.rm = TRUE),
            mean_drainage_X10303 = mean(drainage_X10303, na.rm = TRUE),
            mean_drainage_X10304 = mean(drainage_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_drainage_X10302", "mean_drainage_X10321", "mean_drainage_X10303", "mean_drainage_X10304"),
               names_to = "catchment",
               names_prefix = "mean_drainage_",
               values_to = "mean_drainage")

data_table_max_drainage_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(max_drainage_X10302 = max(drainage_X10302, na.rm = TRUE),
            max_drainage_X10321 = max(drainage_X10321, na.rm = TRUE),
            max_drainage_X10303 = max(drainage_X10303, na.rm = TRUE),
            max_drainage_X10304 = max(drainage_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_drainage_X10302", "max_drainage_X10321", "max_drainage_X10303", "max_drainage_X10304"),
               names_to = "catchment",
               names_prefix = "max_drainage_",
               values_to = "max_drainage")


# airtmp
data_table_min_airtmp_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(min_airtmp_X10302 = min(rol_airtmp_X10302, na.rm = TRUE),
            min_airtmp_X10321 = min(rol_airtmp_X10321, na.rm = TRUE),
            min_airtmp_X10303 = min(rol_airtmp_X10303, na.rm = TRUE),
            min_airtmp_X10304 = min(rol_airtmp_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_airtmp_X10302", "min_airtmp_X10321", "min_airtmp_X10303", "min_airtmp_X10304"),
               names_to = "catchment",
               names_prefix = "min_airtmp_",
               values_to = "min_airtmp")

data_table_mean_airtmp_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(mean_airtmp_X10302 = mean(rol_airtmp_X10302, na.rm = TRUE),
            mean_airtmp_X10321 = mean(rol_airtmp_X10321, na.rm = TRUE),
            mean_airtmp_X10303 = mean(rol_airtmp_X10303, na.rm = TRUE),
            mean_airtmp_X10304 = mean(rol_airtmp_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_airtmp_X10302", "mean_airtmp_X10321", "mean_airtmp_X10303", "mean_airtmp_X10304"),
               names_to = "catchment",
               names_prefix = "mean_airtmp_",
               values_to = "mean_airtmp")

data_table_max_airtmp_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(max_airtmp_X10302 = max(rol_airtmp_X10302, na.rm = TRUE),
            max_airtmp_X10321 = max(rol_airtmp_X10321, na.rm = TRUE),
            max_airtmp_X10303 = max(rol_airtmp_X10303, na.rm = TRUE),
            max_airtmp_X10304 = max(rol_airtmp_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_airtmp_X10302", "max_airtmp_X10321", "max_airtmp_X10303", "max_airtmp_X10304"),
               names_to = "catchment",
               names_prefix = "max_airtmp_",
               values_to = "max_airtmp")

# glorad
data_table_min_glorad_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(min_glorad_X10302 = min(rol_glorad_X10302, na.rm = TRUE),
            min_glorad_X10321 = min(rol_glorad_X10321, na.rm = TRUE),
            min_glorad_X10303 = min(rol_glorad_X10303, na.rm = TRUE),
            min_glorad_X10304 = min(rol_glorad_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_glorad_X10302", "min_glorad_X10321", "min_glorad_X10303", "min_glorad_X10304"),
               names_to = "catchment",
               names_prefix = "min_glorad_",
               values_to = "min_glorad")

data_table_mean_glorad_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(mean_glorad_X10302 = mean(rol_glorad_X10302, na.rm = TRUE),
            mean_glorad_X10321 = mean(rol_glorad_X10321, na.rm = TRUE),
            mean_glorad_X10303 = mean(rol_glorad_X10303, na.rm = TRUE),
            mean_glorad_X10304 = mean(rol_glorad_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_glorad_X10302", "mean_glorad_X10321", "mean_glorad_X10303", "mean_glorad_X10304"),
               names_to = "catchment",
               names_prefix = "mean_glorad_",
               values_to = "mean_glorad")

data_table_max_glorad_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(max_glorad_X10302 = max(rol_glorad_X10302, na.rm = TRUE),
            max_glorad_X10321 = max(rol_glorad_X10321, na.rm = TRUE),
            max_glorad_X10303 = max(rol_glorad_X10303, na.rm = TRUE),
            max_glorad_X10304 = max(rol_glorad_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_glorad_X10302", "max_glorad_X10321", "max_glorad_X10303", "max_glorad_X10304"),
               names_to = "catchment",
               names_prefix = "max_glorad_",
               values_to = "max_glorad")

# groundwaterdepth
data_table_min_groundwaterdepth_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(min_groundwaterdepth_X10302 = min(rol_groundwaterdepth_X10302, na.rm = TRUE),
            min_groundwaterdepth_X10321 = min(rol_groundwaterdepth_X10321, na.rm = TRUE),
            min_groundwaterdepth_X10303 = min(rol_groundwaterdepth_X10303, na.rm = TRUE),
            min_groundwaterdepth_X10304 = min(rol_groundwaterdepth_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_groundwaterdepth_X10302", "min_groundwaterdepth_X10321", "min_groundwaterdepth_X10303", "min_groundwaterdepth_X10304"),
               names_to = "catchment",
               names_prefix = "min_groundwaterdepth_",
               values_to = "min_groundwaterdepth")

data_table_mean_groundwaterdepth_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(mean_groundwaterdepth_X10302 = mean(rol_groundwaterdepth_X10302, na.rm = TRUE),
            mean_groundwaterdepth_X10321 = mean(rol_groundwaterdepth_X10321, na.rm = TRUE),
            mean_groundwaterdepth_X10303 = mean(rol_groundwaterdepth_X10303, na.rm = TRUE),
            mean_groundwaterdepth_X10304 = mean(rol_groundwaterdepth_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_groundwaterdepth_X10302", "mean_groundwaterdepth_X10321", "mean_groundwaterdepth_X10303", "mean_groundwaterdepth_X10304"),
               names_to = "catchment",
               names_prefix = "mean_groundwaterdepth_",
               values_to = "mean_groundwaterdepth")

data_table_max_groundwaterdepth_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(max_groundwaterdepth_X10302 = max(rol_groundwaterdepth_X10302, na.rm = TRUE),
            max_groundwaterdepth_X10321 = max(rol_groundwaterdepth_X10321, na.rm = TRUE),
            max_groundwaterdepth_X10303 = max(rol_groundwaterdepth_X10303, na.rm = TRUE),
            max_groundwaterdepth_X10304 = max(rol_groundwaterdepth_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_groundwaterdepth_X10302", "max_groundwaterdepth_X10321", "max_groundwaterdepth_X10303", "max_groundwaterdepth_X10304"),
               names_to = "catchment",
               names_prefix = "max_groundwaterdepth_",
               values_to = "max_groundwaterdepth")


# precip
data_table_min_precip_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(min_precip_X10302 = min(rol_precip_X10302, na.rm = TRUE),
            min_precip_X10321 = min(rol_precip_X10321, na.rm = TRUE),
            min_precip_X10303 = min(rol_precip_X10303, na.rm = TRUE),
            min_precip_X10304 = min(rol_precip_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_precip_X10302", "min_precip_X10321", "min_precip_X10303", "min_precip_X10304"),
               names_to = "catchment",
               names_prefix = "min_precip_",
               values_to = "min_precip")

data_table_mean_precip_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(mean_precip_X10302 = mean(rol_precip_X10302, na.rm = TRUE),
            mean_precip_X10321 = mean(rol_precip_X10321, na.rm = TRUE),
            mean_precip_X10303 = mean(rol_precip_X10303, na.rm = TRUE),
            mean_precip_X10304 = mean(rol_precip_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_precip_X10302", "mean_precip_X10321", "mean_precip_X10303", "mean_precip_X10304"),
               names_to = "catchment",
               names_prefix = "mean_precip_",
               values_to = "mean_precip")

data_table_max_precip_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(max_precip_X10302 = max(rol_precip_X10302, na.rm = TRUE),
            max_precip_X10321 = max(rol_precip_X10321, na.rm = TRUE),
            max_precip_X10303 = max(rol_precip_X10303, na.rm = TRUE),
            max_precip_X10304 = max(rol_precip_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_precip_X10302", "max_precip_X10321", "max_precip_X10303", "max_precip_X10304"),
               names_to = "catchment",
               names_prefix = "max_precip_",
               values_to = "max_precip")

# qinfiltration
data_table_min_qinfiltration_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(min_qinfiltration_X10302 = min(rol_qinfiltration_X10302, na.rm = TRUE),
            min_qinfiltration_X10321 = min(rol_qinfiltration_X10321, na.rm = TRUE),
            min_qinfiltration_X10303 = min(rol_qinfiltration_X10303, na.rm = TRUE),
            min_qinfiltration_X10304 = min(rol_qinfiltration_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_qinfiltration_X10302", "min_qinfiltration_X10321", "min_qinfiltration_X10303", "min_qinfiltration_X10304"),
               names_to = "catchment",
               names_prefix = "min_qinfiltration_",
               values_to = "min_qinfiltration")

data_table_mean_qinfiltration_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(mean_qinfiltration_X10302 = mean(rol_qinfiltration_X10302, na.rm = TRUE),
            mean_qinfiltration_X10321 = mean(rol_qinfiltration_X10321, na.rm = TRUE),
            mean_qinfiltration_X10303 = mean(rol_qinfiltration_X10303, na.rm = TRUE),
            mean_qinfiltration_X10304 = mean(rol_qinfiltration_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_qinfiltration_X10302", "mean_qinfiltration_X10321", "mean_qinfiltration_X10303", "mean_qinfiltration_X10304"),
               names_to = "catchment",
               names_prefix = "mean_qinfiltration_",
               values_to = "mean_qinfiltration")

data_table_max_qinfiltration_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(max_qinfiltration_X10302 = max(rol_qinfiltration_X10302, na.rm = TRUE),
            max_qinfiltration_X10321 = max(rol_qinfiltration_X10321, na.rm = TRUE),
            max_qinfiltration_X10303 = max(rol_qinfiltration_X10303, na.rm = TRUE),
            max_qinfiltration_X10304 = max(rol_qinfiltration_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_qinfiltration_X10302", "max_qinfiltration_X10321", "max_qinfiltration_X10303", "max_qinfiltration_X10304"),
               names_to = "catchment",
               names_prefix = "max_qinfiltration_",
               values_to = "max_qinfiltration")

# relhum
data_table_min_relhum_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(min_relhum_X10302 = min(rol_relhum_X10302, na.rm = TRUE),
            min_relhum_X10321 = min(rol_relhum_X10321, na.rm = TRUE),
            min_relhum_X10303 = min(rol_relhum_X10303, na.rm = TRUE),
            min_relhum_X10304 = min(rol_relhum_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_relhum_X10302", "min_relhum_X10321", "min_relhum_X10303", "min_relhum_X10304"),
               names_to = "catchment",
               names_prefix = "min_relhum_",
               values_to = "min_relhum")

data_table_mean_relhum_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(mean_relhum_X10302 = mean(rol_relhum_X10302, na.rm = TRUE),
            mean_relhum_X10321 = mean(rol_relhum_X10321, na.rm = TRUE),
            mean_relhum_X10303 = mean(rol_relhum_X10303, na.rm = TRUE),
            mean_relhum_X10304 = mean(rol_relhum_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_relhum_X10302", "mean_relhum_X10321", "mean_relhum_X10303", "mean_relhum_X10304"),
               names_to = "catchment",
               names_prefix = "mean_relhum_",
               values_to = "mean_relhum")

data_table_max_relhum_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(max_relhum_X10302 = max(rol_relhum_X10302, na.rm = TRUE),
            max_relhum_X10321 = max(rol_relhum_X10321, na.rm = TRUE),
            max_relhum_X10303 = max(rol_relhum_X10303, na.rm = TRUE),
            max_relhum_X10304 = max(rol_relhum_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_relhum_X10302", "max_relhum_X10321", "max_relhum_X10303", "max_relhum_X10304"),
               names_to = "catchment",
               names_prefix = "max_relhum_",
               values_to = "max_relhum")

# snowstorage
data_table_min_snowstorage_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(min_snowstorage_X10302 = min(rol_snowstorage_X10302, na.rm = TRUE),
            min_snowstorage_X10321 = min(rol_snowstorage_X10321, na.rm = TRUE),
            min_snowstorage_X10303 = min(rol_snowstorage_X10303, na.rm = TRUE),
            min_snowstorage_X10304 = min(rol_snowstorage_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_snowstorage_X10302", "min_snowstorage_X10321", "min_snowstorage_X10303", "min_snowstorage_X10304"),
               names_to = "catchment",
               names_prefix = "min_snowstorage_",
               values_to = "min_snowstorage")

data_table_mean_snowstorage_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(mean_snowstorage_X10302 = mean(rol_snowstorage_X10302, na.rm = TRUE),
            mean_snowstorage_X10321 = mean(rol_snowstorage_X10321, na.rm = TRUE),
            mean_snowstorage_X10303 = mean(rol_snowstorage_X10303, na.rm = TRUE),
            mean_snowstorage_X10304 = mean(rol_snowstorage_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_snowstorage_X10302", "mean_snowstorage_X10321", "mean_snowstorage_X10303", "mean_snowstorage_X10304"),
               names_to = "catchment",
               names_prefix = "mean_snowstorage_",
               values_to = "mean_snowstorage")

data_table_max_snowstorage_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(max_snowstorage_X10302 = max(rol_snowstorage_X10302, na.rm = TRUE),
            max_snowstorage_X10321 = max(rol_snowstorage_X10321, na.rm = TRUE),
            max_snowstorage_X10303 = max(rol_snowstorage_X10303, na.rm = TRUE),
            max_snowstorage_X10304 = max(rol_snowstorage_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_snowstorage_X10302", "max_snowstorage_X10321", "max_snowstorage_X10303", "max_snowstorage_X10304"),
               names_to = "catchment",
               names_prefix = "max_snowstorage_",
               values_to = "max_snowstorage")

# soilwaterrootzone
data_table_min_soilwaterrootzone_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(min_soilwaterrootzone_X10302 = min(rol_soilwaterrootzone_X10302, na.rm = TRUE),
            min_soilwaterrootzone_X10321 = min(rol_soilwaterrootzone_X10321, na.rm = TRUE),
            min_soilwaterrootzone_X10303 = min(rol_soilwaterrootzone_X10303, na.rm = TRUE),
            min_soilwaterrootzone_X10304 = min(rol_soilwaterrootzone_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_soilwaterrootzone_X10302", "min_soilwaterrootzone_X10321", "min_soilwaterrootzone_X10303", "min_soilwaterrootzone_X10304"),
               names_to = "catchment",
               names_prefix = "min_soilwaterrootzone_",
               values_to = "min_soilwaterrootzone")

data_table_mean_soilwaterrootzone_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(mean_soilwaterrootzone_X10302 = mean(rol_soilwaterrootzone_X10302, na.rm = TRUE),
            mean_soilwaterrootzone_X10321 = mean(rol_soilwaterrootzone_X10321, na.rm = TRUE),
            mean_soilwaterrootzone_X10303 = mean(rol_soilwaterrootzone_X10303, na.rm = TRUE),
            mean_soilwaterrootzone_X10304 = mean(rol_soilwaterrootzone_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_soilwaterrootzone_X10302", "mean_soilwaterrootzone_X10321", "mean_soilwaterrootzone_X10303", "mean_soilwaterrootzone_X10304"),
               names_to = "catchment",
               names_prefix = "mean_soilwaterrootzone_",
               values_to = "mean_soilwaterrootzone")

data_table_max_soilwaterrootzone_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(max_soilwaterrootzone_X10302 = max(rol_soilwaterrootzone_X10302, na.rm = TRUE),
            max_soilwaterrootzone_X10321 = max(rol_soilwaterrootzone_X10321, na.rm = TRUE),
            max_soilwaterrootzone_X10303 = max(rol_soilwaterrootzone_X10303, na.rm = TRUE),
            max_soilwaterrootzone_X10304 = max(rol_soilwaterrootzone_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_soilwaterrootzone_X10302", "max_soilwaterrootzone_X10321", "max_soilwaterrootzone_X10303", "max_soilwaterrootzone_X10304"),
               names_to = "catchment",
               names_prefix = "max_soilwaterrootzone_",
               values_to = "max_soilwaterrootzone")

# soilwaterunsatzone
data_table_min_soilwaterunsatzone_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(min_soilwaterunsatzone_X10302 = min(rol_soilwaterunsatzone_X10302, na.rm = TRUE),
            min_soilwaterunsatzone_X10321 = min(rol_soilwaterunsatzone_X10321, na.rm = TRUE),
            min_soilwaterunsatzone_X10303 = min(rol_soilwaterunsatzone_X10303, na.rm = TRUE),
            min_soilwaterunsatzone_X10304 = min(rol_soilwaterunsatzone_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_soilwaterunsatzone_X10302", "min_soilwaterunsatzone_X10321", "min_soilwaterunsatzone_X10303", "min_soilwaterunsatzone_X10304"),
               names_to = "catchment",
               names_prefix = "min_soilwaterunsatzone_",
               values_to = "min_soilwaterunsatzone")

data_table_mean_soilwaterunsatzone_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(mean_soilwaterunsatzone_X10302 = mean(rol_soilwaterunsatzone_X10302, na.rm = TRUE),
            mean_soilwaterunsatzone_X10321 = mean(rol_soilwaterunsatzone_X10321, na.rm = TRUE),
            mean_soilwaterunsatzone_X10303 = mean(rol_soilwaterunsatzone_X10303, na.rm = TRUE),
            mean_soilwaterunsatzone_X10304 = mean(rol_soilwaterunsatzone_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_soilwaterunsatzone_X10302", "mean_soilwaterunsatzone_X10321", "mean_soilwaterunsatzone_X10303", "mean_soilwaterunsatzone_X10304"),
               names_to = "catchment",
               names_prefix = "mean_soilwaterunsatzone_",
               values_to = "mean_soilwaterunsatzone")

data_table_max_soilwaterunsatzone_winter <- data_merged_rol_winter %>%
  group_by(YY) %>%
  summarise(max_soilwaterunsatzone_X10302 = max(rol_soilwaterunsatzone_X10302, na.rm = TRUE),
            max_soilwaterunsatzone_X10321 = max(rol_soilwaterunsatzone_X10321, na.rm = TRUE),
            max_soilwaterunsatzone_X10303 = max(rol_soilwaterunsatzone_X10303, na.rm = TRUE),
            max_soilwaterunsatzone_X10304 = max(rol_soilwaterunsatzone_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_soilwaterunsatzone_X10302", "max_soilwaterunsatzone_X10321", "max_soilwaterunsatzone_X10303", "max_soilwaterunsatzone_X10304"),
               names_to = "catchment",
               names_prefix = "max_soilwaterunsatzone_",
               values_to = "max_soilwaterunsatzone")

data_table_winter <- merge(data_table_min_drainage_winter, data_table_mean_drainage_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_max_drainage_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_min_airtmp_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_mean_airtmp_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_max_airtmp_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_min_glorad_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_mean_glorad_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_max_glorad_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_min_groundwaterdepth_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_mean_groundwaterdepth_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_max_groundwaterdepth_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_min_precip_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_mean_precip_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_max_precip_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_min_qinfiltration_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_mean_qinfiltration_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_max_qinfiltration_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_min_relhum_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_mean_relhum_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_max_relhum_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_min_snowstorage_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_mean_snowstorage_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_max_snowstorage_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_min_soilwaterrootzone_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_mean_soilwaterrootzone_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_max_soilwaterrootzone_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_min_soilwaterunsatzone_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_mean_soilwaterunsatzone_winter, by = c("catchment", "YY"))
data_table_winter <- merge(data_table_winter, data_table_max_soilwaterunsatzone_winter, by = c("catchment", "YY"))

data_table_winter <- cbind(data_table_winter, halfyear = rep("winter", nrow(data_table_winter)))





## summer
# drainage
data_table_min_drainage_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(min_drainage_X10302 = min(drainage_X10302, na.rm = TRUE),
            min_drainage_X10321 = min(drainage_X10321, na.rm = TRUE),
            min_drainage_X10303 = min(drainage_X10303, na.rm = TRUE),
            min_drainage_X10304 = min(drainage_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_drainage_X10302", "min_drainage_X10321", "min_drainage_X10303", "min_drainage_X10304"),
               names_to = "catchment",
               names_prefix = "min_drainage_",
               values_to = "min_drainage")

data_table_mean_drainage_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(mean_drainage_X10302 = mean(drainage_X10302, na.rm = TRUE),
            mean_drainage_X10321 = mean(drainage_X10321, na.rm = TRUE),
            mean_drainage_X10303 = mean(drainage_X10303, na.rm = TRUE),
            mean_drainage_X10304 = mean(drainage_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_drainage_X10302", "mean_drainage_X10321", "mean_drainage_X10303", "mean_drainage_X10304"),
               names_to = "catchment",
               names_prefix = "mean_drainage_",
               values_to = "mean_drainage")

data_table_max_drainage_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(max_drainage_X10302 = max(drainage_X10302, na.rm = TRUE),
            max_drainage_X10321 = max(drainage_X10321, na.rm = TRUE),
            max_drainage_X10303 = max(drainage_X10303, na.rm = TRUE),
            max_drainage_X10304 = max(drainage_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_drainage_X10302", "max_drainage_X10321", "max_drainage_X10303", "max_drainage_X10304"),
               names_to = "catchment",
               names_prefix = "max_drainage_",
               values_to = "max_drainage")


# airtmp
data_table_min_airtmp_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(min_airtmp_X10302 = min(rol_airtmp_X10302, na.rm = TRUE),
            min_airtmp_X10321 = min(rol_airtmp_X10321, na.rm = TRUE),
            min_airtmp_X10303 = min(rol_airtmp_X10303, na.rm = TRUE),
            min_airtmp_X10304 = min(rol_airtmp_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_airtmp_X10302", "min_airtmp_X10321", "min_airtmp_X10303", "min_airtmp_X10304"),
               names_to = "catchment",
               names_prefix = "min_airtmp_",
               values_to = "min_airtmp")

data_table_mean_airtmp_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(mean_airtmp_X10302 = mean(rol_airtmp_X10302, na.rm = TRUE),
            mean_airtmp_X10321 = mean(rol_airtmp_X10321, na.rm = TRUE),
            mean_airtmp_X10303 = mean(rol_airtmp_X10303, na.rm = TRUE),
            mean_airtmp_X10304 = mean(rol_airtmp_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_airtmp_X10302", "mean_airtmp_X10321", "mean_airtmp_X10303", "mean_airtmp_X10304"),
               names_to = "catchment",
               names_prefix = "mean_airtmp_",
               values_to = "mean_airtmp")

data_table_max_airtmp_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(max_airtmp_X10302 = max(rol_airtmp_X10302, na.rm = TRUE),
            max_airtmp_X10321 = max(rol_airtmp_X10321, na.rm = TRUE),
            max_airtmp_X10303 = max(rol_airtmp_X10303, na.rm = TRUE),
            max_airtmp_X10304 = max(rol_airtmp_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_airtmp_X10302", "max_airtmp_X10321", "max_airtmp_X10303", "max_airtmp_X10304"),
               names_to = "catchment",
               names_prefix = "max_airtmp_",
               values_to = "max_airtmp")

# glorad
data_table_min_glorad_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(min_glorad_X10302 = min(rol_glorad_X10302, na.rm = TRUE),
            min_glorad_X10321 = min(rol_glorad_X10321, na.rm = TRUE),
            min_glorad_X10303 = min(rol_glorad_X10303, na.rm = TRUE),
            min_glorad_X10304 = min(rol_glorad_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_glorad_X10302", "min_glorad_X10321", "min_glorad_X10303", "min_glorad_X10304"),
               names_to = "catchment",
               names_prefix = "min_glorad_",
               values_to = "min_glorad")

data_table_mean_glorad_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(mean_glorad_X10302 = mean(rol_glorad_X10302, na.rm = TRUE),
            mean_glorad_X10321 = mean(rol_glorad_X10321, na.rm = TRUE),
            mean_glorad_X10303 = mean(rol_glorad_X10303, na.rm = TRUE),
            mean_glorad_X10304 = mean(rol_glorad_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_glorad_X10302", "mean_glorad_X10321", "mean_glorad_X10303", "mean_glorad_X10304"),
               names_to = "catchment",
               names_prefix = "mean_glorad_",
               values_to = "mean_glorad")

data_table_max_glorad_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(max_glorad_X10302 = max(rol_glorad_X10302, na.rm = TRUE),
            max_glorad_X10321 = max(rol_glorad_X10321, na.rm = TRUE),
            max_glorad_X10303 = max(rol_glorad_X10303, na.rm = TRUE),
            max_glorad_X10304 = max(rol_glorad_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_glorad_X10302", "max_glorad_X10321", "max_glorad_X10303", "max_glorad_X10304"),
               names_to = "catchment",
               names_prefix = "max_glorad_",
               values_to = "max_glorad")

# groundwaterdepth
data_table_min_groundwaterdepth_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(min_groundwaterdepth_X10302 = min(rol_groundwaterdepth_X10302, na.rm = TRUE),
            min_groundwaterdepth_X10321 = min(rol_groundwaterdepth_X10321, na.rm = TRUE),
            min_groundwaterdepth_X10303 = min(rol_groundwaterdepth_X10303, na.rm = TRUE),
            min_groundwaterdepth_X10304 = min(rol_groundwaterdepth_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_groundwaterdepth_X10302", "min_groundwaterdepth_X10321", "min_groundwaterdepth_X10303", "min_groundwaterdepth_X10304"),
               names_to = "catchment",
               names_prefix = "min_groundwaterdepth_",
               values_to = "min_groundwaterdepth")

data_table_mean_groundwaterdepth_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(mean_groundwaterdepth_X10302 = mean(rol_groundwaterdepth_X10302, na.rm = TRUE),
            mean_groundwaterdepth_X10321 = mean(rol_groundwaterdepth_X10321, na.rm = TRUE),
            mean_groundwaterdepth_X10303 = mean(rol_groundwaterdepth_X10303, na.rm = TRUE),
            mean_groundwaterdepth_X10304 = mean(rol_groundwaterdepth_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_groundwaterdepth_X10302", "mean_groundwaterdepth_X10321", "mean_groundwaterdepth_X10303", "mean_groundwaterdepth_X10304"),
               names_to = "catchment",
               names_prefix = "mean_groundwaterdepth_",
               values_to = "mean_groundwaterdepth")

data_table_max_groundwaterdepth_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(max_groundwaterdepth_X10302 = max(rol_groundwaterdepth_X10302, na.rm = TRUE),
            max_groundwaterdepth_X10321 = max(rol_groundwaterdepth_X10321, na.rm = TRUE),
            max_groundwaterdepth_X10303 = max(rol_groundwaterdepth_X10303, na.rm = TRUE),
            max_groundwaterdepth_X10304 = max(rol_groundwaterdepth_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_groundwaterdepth_X10302", "max_groundwaterdepth_X10321", "max_groundwaterdepth_X10303", "max_groundwaterdepth_X10304"),
               names_to = "catchment",
               names_prefix = "max_groundwaterdepth_",
               values_to = "max_groundwaterdepth")


# precip
data_table_min_precip_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(min_precip_X10302 = min(rol_precip_X10302, na.rm = TRUE),
            min_precip_X10321 = min(rol_precip_X10321, na.rm = TRUE),
            min_precip_X10303 = min(rol_precip_X10303, na.rm = TRUE),
            min_precip_X10304 = min(rol_precip_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_precip_X10302", "min_precip_X10321", "min_precip_X10303", "min_precip_X10304"),
               names_to = "catchment",
               names_prefix = "min_precip_",
               values_to = "min_precip")

data_table_mean_precip_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(mean_precip_X10302 = mean(rol_precip_X10302, na.rm = TRUE),
            mean_precip_X10321 = mean(rol_precip_X10321, na.rm = TRUE),
            mean_precip_X10303 = mean(rol_precip_X10303, na.rm = TRUE),
            mean_precip_X10304 = mean(rol_precip_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_precip_X10302", "mean_precip_X10321", "mean_precip_X10303", "mean_precip_X10304"),
               names_to = "catchment",
               names_prefix = "mean_precip_",
               values_to = "mean_precip")

data_table_max_precip_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(max_precip_X10302 = max(rol_precip_X10302, na.rm = TRUE),
            max_precip_X10321 = max(rol_precip_X10321, na.rm = TRUE),
            max_precip_X10303 = max(rol_precip_X10303, na.rm = TRUE),
            max_precip_X10304 = max(rol_precip_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_precip_X10302", "max_precip_X10321", "max_precip_X10303", "max_precip_X10304"),
               names_to = "catchment",
               names_prefix = "max_precip_",
               values_to = "max_precip")

# qinfiltration
data_table_min_qinfiltration_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(min_qinfiltration_X10302 = min(rol_qinfiltration_X10302, na.rm = TRUE),
            min_qinfiltration_X10321 = min(rol_qinfiltration_X10321, na.rm = TRUE),
            min_qinfiltration_X10303 = min(rol_qinfiltration_X10303, na.rm = TRUE),
            min_qinfiltration_X10304 = min(rol_qinfiltration_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_qinfiltration_X10302", "min_qinfiltration_X10321", "min_qinfiltration_X10303", "min_qinfiltration_X10304"),
               names_to = "catchment",
               names_prefix = "min_qinfiltration_",
               values_to = "min_qinfiltration")

data_table_mean_qinfiltration_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(mean_qinfiltration_X10302 = mean(rol_qinfiltration_X10302, na.rm = TRUE),
            mean_qinfiltration_X10321 = mean(rol_qinfiltration_X10321, na.rm = TRUE),
            mean_qinfiltration_X10303 = mean(rol_qinfiltration_X10303, na.rm = TRUE),
            mean_qinfiltration_X10304 = mean(rol_qinfiltration_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_qinfiltration_X10302", "mean_qinfiltration_X10321", "mean_qinfiltration_X10303", "mean_qinfiltration_X10304"),
               names_to = "catchment",
               names_prefix = "mean_qinfiltration_",
               values_to = "mean_qinfiltration")

data_table_max_qinfiltration_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(max_qinfiltration_X10302 = max(rol_qinfiltration_X10302, na.rm = TRUE),
            max_qinfiltration_X10321 = max(rol_qinfiltration_X10321, na.rm = TRUE),
            max_qinfiltration_X10303 = max(rol_qinfiltration_X10303, na.rm = TRUE),
            max_qinfiltration_X10304 = max(rol_qinfiltration_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_qinfiltration_X10302", "max_qinfiltration_X10321", "max_qinfiltration_X10303", "max_qinfiltration_X10304"),
               names_to = "catchment",
               names_prefix = "max_qinfiltration_",
               values_to = "max_qinfiltration")

# relhum
data_table_min_relhum_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(min_relhum_X10302 = min(rol_relhum_X10302, na.rm = TRUE),
            min_relhum_X10321 = min(rol_relhum_X10321, na.rm = TRUE),
            min_relhum_X10303 = min(rol_relhum_X10303, na.rm = TRUE),
            min_relhum_X10304 = min(rol_relhum_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_relhum_X10302", "min_relhum_X10321", "min_relhum_X10303", "min_relhum_X10304"),
               names_to = "catchment",
               names_prefix = "min_relhum_",
               values_to = "min_relhum")

data_table_mean_relhum_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(mean_relhum_X10302 = mean(rol_relhum_X10302, na.rm = TRUE),
            mean_relhum_X10321 = mean(rol_relhum_X10321, na.rm = TRUE),
            mean_relhum_X10303 = mean(rol_relhum_X10303, na.rm = TRUE),
            mean_relhum_X10304 = mean(rol_relhum_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_relhum_X10302", "mean_relhum_X10321", "mean_relhum_X10303", "mean_relhum_X10304"),
               names_to = "catchment",
               names_prefix = "mean_relhum_",
               values_to = "mean_relhum")

data_table_max_relhum_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(max_relhum_X10302 = max(rol_relhum_X10302, na.rm = TRUE),
            max_relhum_X10321 = max(rol_relhum_X10321, na.rm = TRUE),
            max_relhum_X10303 = max(rol_relhum_X10303, na.rm = TRUE),
            max_relhum_X10304 = max(rol_relhum_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_relhum_X10302", "max_relhum_X10321", "max_relhum_X10303", "max_relhum_X10304"),
               names_to = "catchment",
               names_prefix = "max_relhum_",
               values_to = "max_relhum")

# snowstorage
data_table_min_snowstorage_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(min_snowstorage_X10302 = min(rol_snowstorage_X10302, na.rm = TRUE),
            min_snowstorage_X10321 = min(rol_snowstorage_X10321, na.rm = TRUE),
            min_snowstorage_X10303 = min(rol_snowstorage_X10303, na.rm = TRUE),
            min_snowstorage_X10304 = min(rol_snowstorage_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_snowstorage_X10302", "min_snowstorage_X10321", "min_snowstorage_X10303", "min_snowstorage_X10304"),
               names_to = "catchment",
               names_prefix = "min_snowstorage_",
               values_to = "min_snowstorage")

data_table_mean_snowstorage_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(mean_snowstorage_X10302 = mean(rol_snowstorage_X10302, na.rm = TRUE),
            mean_snowstorage_X10321 = mean(rol_snowstorage_X10321, na.rm = TRUE),
            mean_snowstorage_X10303 = mean(rol_snowstorage_X10303, na.rm = TRUE),
            mean_snowstorage_X10304 = mean(rol_snowstorage_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_snowstorage_X10302", "mean_snowstorage_X10321", "mean_snowstorage_X10303", "mean_snowstorage_X10304"),
               names_to = "catchment",
               names_prefix = "mean_snowstorage_",
               values_to = "mean_snowstorage")

data_table_max_snowstorage_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(max_snowstorage_X10302 = max(rol_snowstorage_X10302, na.rm = TRUE),
            max_snowstorage_X10321 = max(rol_snowstorage_X10321, na.rm = TRUE),
            max_snowstorage_X10303 = max(rol_snowstorage_X10303, na.rm = TRUE),
            max_snowstorage_X10304 = max(rol_snowstorage_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_snowstorage_X10302", "max_snowstorage_X10321", "max_snowstorage_X10303", "max_snowstorage_X10304"),
               names_to = "catchment",
               names_prefix = "max_snowstorage_",
               values_to = "max_snowstorage")

# soilwaterrootzone
data_table_min_soilwaterrootzone_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(min_soilwaterrootzone_X10302 = min(rol_soilwaterrootzone_X10302, na.rm = TRUE),
            min_soilwaterrootzone_X10321 = min(rol_soilwaterrootzone_X10321, na.rm = TRUE),
            min_soilwaterrootzone_X10303 = min(rol_soilwaterrootzone_X10303, na.rm = TRUE),
            min_soilwaterrootzone_X10304 = min(rol_soilwaterrootzone_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_soilwaterrootzone_X10302", "min_soilwaterrootzone_X10321", "min_soilwaterrootzone_X10303", "min_soilwaterrootzone_X10304"),
               names_to = "catchment",
               names_prefix = "min_soilwaterrootzone_",
               values_to = "min_soilwaterrootzone")

data_table_mean_soilwaterrootzone_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(mean_soilwaterrootzone_X10302 = mean(rol_soilwaterrootzone_X10302, na.rm = TRUE),
            mean_soilwaterrootzone_X10321 = mean(rol_soilwaterrootzone_X10321, na.rm = TRUE),
            mean_soilwaterrootzone_X10303 = mean(rol_soilwaterrootzone_X10303, na.rm = TRUE),
            mean_soilwaterrootzone_X10304 = mean(rol_soilwaterrootzone_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_soilwaterrootzone_X10302", "mean_soilwaterrootzone_X10321", "mean_soilwaterrootzone_X10303", "mean_soilwaterrootzone_X10304"),
               names_to = "catchment",
               names_prefix = "mean_soilwaterrootzone_",
               values_to = "mean_soilwaterrootzone")

data_table_max_soilwaterrootzone_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(max_soilwaterrootzone_X10302 = max(rol_soilwaterrootzone_X10302, na.rm = TRUE),
            max_soilwaterrootzone_X10321 = max(rol_soilwaterrootzone_X10321, na.rm = TRUE),
            max_soilwaterrootzone_X10303 = max(rol_soilwaterrootzone_X10303, na.rm = TRUE),
            max_soilwaterrootzone_X10304 = max(rol_soilwaterrootzone_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_soilwaterrootzone_X10302", "max_soilwaterrootzone_X10321", "max_soilwaterrootzone_X10303", "max_soilwaterrootzone_X10304"),
               names_to = "catchment",
               names_prefix = "max_soilwaterrootzone_",
               values_to = "max_soilwaterrootzone")

# soilwaterunsatzone
data_table_min_soilwaterunsatzone_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(min_soilwaterunsatzone_X10302 = min(rol_soilwaterunsatzone_X10302, na.rm = TRUE),
            min_soilwaterunsatzone_X10321 = min(rol_soilwaterunsatzone_X10321, na.rm = TRUE),
            min_soilwaterunsatzone_X10303 = min(rol_soilwaterunsatzone_X10303, na.rm = TRUE),
            min_soilwaterunsatzone_X10304 = min(rol_soilwaterunsatzone_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("min_soilwaterunsatzone_X10302", "min_soilwaterunsatzone_X10321", "min_soilwaterunsatzone_X10303", "min_soilwaterunsatzone_X10304"),
               names_to = "catchment",
               names_prefix = "min_soilwaterunsatzone_",
               values_to = "min_soilwaterunsatzone")

data_table_mean_soilwaterunsatzone_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(mean_soilwaterunsatzone_X10302 = mean(rol_soilwaterunsatzone_X10302, na.rm = TRUE),
            mean_soilwaterunsatzone_X10321 = mean(rol_soilwaterunsatzone_X10321, na.rm = TRUE),
            mean_soilwaterunsatzone_X10303 = mean(rol_soilwaterunsatzone_X10303, na.rm = TRUE),
            mean_soilwaterunsatzone_X10304 = mean(rol_soilwaterunsatzone_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("mean_soilwaterunsatzone_X10302", "mean_soilwaterunsatzone_X10321", "mean_soilwaterunsatzone_X10303", "mean_soilwaterunsatzone_X10304"),
               names_to = "catchment",
               names_prefix = "mean_soilwaterunsatzone_",
               values_to = "mean_soilwaterunsatzone")

data_table_max_soilwaterunsatzone_summer <- data_merged_rol_summer %>%
  group_by(YY) %>%
  summarise(max_soilwaterunsatzone_X10302 = max(rol_soilwaterunsatzone_X10302, na.rm = TRUE),
            max_soilwaterunsatzone_X10321 = max(rol_soilwaterunsatzone_X10321, na.rm = TRUE),
            max_soilwaterunsatzone_X10303 = max(rol_soilwaterunsatzone_X10303, na.rm = TRUE),
            max_soilwaterunsatzone_X10304 = max(rol_soilwaterunsatzone_X10304, na.rm = TRUE)) %>%
  pivot_longer(col = c("max_soilwaterunsatzone_X10302", "max_soilwaterunsatzone_X10321", "max_soilwaterunsatzone_X10303", "max_soilwaterunsatzone_X10304"),
               names_to = "catchment",
               names_prefix = "max_soilwaterunsatzone_",
               values_to = "max_soilwaterunsatzone")





data_table_summer <- merge(data_table_min_drainage_summer, data_table_mean_drainage_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_max_drainage_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_min_airtmp_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_mean_airtmp_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_max_airtmp_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_min_glorad_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_mean_glorad_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_max_glorad_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_min_groundwaterdepth_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_mean_groundwaterdepth_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_max_groundwaterdepth_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_min_precip_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_mean_precip_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_max_precip_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_min_qinfiltration_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_mean_qinfiltration_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_max_qinfiltration_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_min_relhum_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_mean_relhum_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_max_relhum_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_min_snowstorage_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_mean_snowstorage_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_max_snowstorage_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_min_soilwaterrootzone_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_mean_soilwaterrootzone_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_max_soilwaterrootzone_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_min_soilwaterunsatzone_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_mean_soilwaterunsatzone_summer, by = c("catchment", "YY"))
data_table_summer <- merge(data_table_summer, data_table_max_soilwaterunsatzone_summer, by = c("catchment", "YY"))

data_table_summer <- cbind(data_table_summer, halfyear = rep("summer", nrow(data_table_summer)))

data_table <- rbind(data_table_winter, data_table_summer)


# remove tables that are not needed
rm(data_table_min_drainage_winter)
rm(data_table_mean_drainage_winter)
rm(data_table_max_drainage_winter)
rm(data_table_min_airtmp_winter)
rm(data_table_mean_airtmp_winter)
rm(data_table_max_airtmp_winter)
rm(data_table_min_glorad_winter)
rm(data_table_mean_glorad_winter)
rm(data_table_max_glorad_winter)
rm(data_table_min_groundwaterdepth_winter)
rm(data_table_mean_groundwaterdepth_winter)
rm(data_table_max_groundwaterdepth_winter)
rm(data_table_min_precip_winter)
rm(data_table_mean_precip_winter)
rm(data_table_max_precip_winter)
rm(data_table_min_qinfiltration_winter)
rm(data_table_mean_qinfiltration_winter)
rm(data_table_max_qinfiltration_winter)
rm(data_table_min_relhum_winter)
rm(data_table_mean_relhum_winter)
rm(data_table_max_relhum_winter)
rm(data_table_min_snowstorage_winter)
rm(data_table_mean_snowstorage_winter)
rm(data_table_max_snowstorage_winter)
rm(data_table_min_soilwaterrootzone_winter)
rm(data_table_mean_soilwaterrootzone_winter)
rm(data_table_max_soilwaterrootzone_winter)
rm(data_table_min_soilwaterunsatzone_winter)
rm(data_table_mean_soilwaterunsatzone_winter)
rm(data_table_max_soilwaterunsatzone_winter)
rm(data_table_winter)

rm(data_table_min_drainage_summer)
rm(data_table_mean_drainage_summer)
rm(data_table_max_drainage_summer)
rm(data_table_min_airtmp_summer)
rm(data_table_mean_airtmp_summer)
rm(data_table_max_airtmp_summer)
rm(data_table_min_glorad_summer)
rm(data_table_mean_glorad_summer)
rm(data_table_max_glorad_summer)
rm(data_table_min_groundwaterdepth_summer)
rm(data_table_mean_groundwaterdepth_summer)
rm(data_table_max_groundwaterdepth_summer)
rm(data_table_min_precip_summer)
rm(data_table_mean_precip_summer)
rm(data_table_max_precip_summer)
rm(data_table_min_qinfiltration_summer)
rm(data_table_mean_qinfiltration_summer)
rm(data_table_max_qinfiltration_summer)
rm(data_table_min_relhum_summer)
rm(data_table_mean_relhum_summer)
rm(data_table_max_relhum_summer)
rm(data_table_min_snowstorage_summer)
rm(data_table_mean_snowstorage_summer)
rm(data_table_max_snowstorage_summer)
rm(data_table_min_soilwaterrootzone_summer)
rm(data_table_mean_soilwaterrootzone_summer)
rm(data_table_max_soilwaterrootzone_summer)
rm(data_table_min_soilwaterunsatzone_summer)
rm(data_table_mean_soilwaterunsatzone_summer)
rm(data_table_max_soilwaterunsatzone_summer)
rm(data_table_summer)
