#X10302 = Muenchen
#X10303 = BadToelz
#X10304 = Mittenwald
#X10321 = Schlehdorf

# load packages
library(mgcv)
library(zoo)
source("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/functions.R")

# load data
setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbe")
data_merged_rol <- readRDS("data_merged_rol.rds")
setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbj")
data_merged_rol_kbj <- readRDS("data_merged_rol_kbj.rds")


# calculate NM7Q per catchment and hydrological half-year
for (i in c("X10302", "X10303", "X10304", "X10321")) {
  for (j in c("winter", "summer")) {
    min_values_per_year <- numeric()
    for (k in 1981:2010) {
      if (j == "winter") {
        if (k == 1981) {
          values <- data_merged_rol[data_merged_rol$YY == k & data_merged_rol$MM <= 4, paste0("drainage_", i)]
          rol_values <- rollapply(values, 8 * 7, mean, align = 'right', fill = NA)
          min_values_per_year[length(min_values_per_year) + 1] <- min(rol_values, na.rm = TRUE)
        } else {
          values <- data_merged_rol[data_merged_rol$YY == (k - 1) & data_merged_rol$MM >= 11, paste0("drainage_", i)]
          values <- c(values, data_merged_rol[data_merged_rol$YY == k & data_merged_rol$MM <= 4, paste0("drainage_", i)])
          rol_values <- rollapply(values, 8 * 7, mean, align = 'right', fill = NA)
          min_values_per_year[length(min_values_per_year) + 1] <- min(rol_values, na.rm = TRUE)
        }
      } else {
        values <- data_merged_rol[data_merged_rol$YY == k &
                                    data_merged_rol$MM >= 5 & data_merged_rol$MM <= 10, paste0("drainage_", i)]
        rol_values <- rollapply(values, 8 * 7, mean, align = 'right', fill = NA)
        min_values_per_year[length(min_values_per_year) + 1] <- min(rol_values, na.rm = TRUE)
      }
      
    }
    assign(paste0("NM7Q_", i, "_", j), mean(min_values_per_year))
  }
}

# winter data set
data_merged_rol_winter <- data_merged_rol[data_merged_rol$MM < 5 | data_merged_rol$MM > 10, ]
data_merged_rol_winter_kbj <- data_merged_rol_kbj[data_merged_rol_kbj$MM < 5 | data_merged_rol_kbj$MM > 10, ]

data_merged_rol_winter$YY <- as.numeric(data_merged_rol_winter$YY)
data_merged_rol_winter_kbj$YY <- as.numeric(data_merged_rol_winter_kbj$YY)

# summer data set
data_merged_rol_summer <- data_merged_rol[data_merged_rol$MM >= 5 & data_merged_rol$MM <= 10, ]
data_merged_rol_summer_kbj <- data_merged_rol_kbj[data_merged_rol_kbj$MM >= 5 & data_merged_rol_kbj$MM <= 10, ]

data_merged_rol_summer$YY <- as.numeric(data_merged_rol_summer$YY)
data_merged_rol_summer_kbj$YY <- as.numeric(data_merged_rol_summer_kbj$YY)



# winter
model_winter_ps_intera_04 <- bam(formula = drainage_X10304 ~ YY +
                                   rol_snowstorage_X10304:rol_soilwaterunsatzone_X10304 +
                                   rol_snowstorage_X10304:rol_soilwaterrootzone_X10304 +
                                   s(rol_airtmp_X10304, bs = 'ps') + s(rol_glorad_X10304, bs = 'ps') + s(rol_groundwaterdepth_X10304, bs = 'ps') +
                                   s(rol_precip_X10304, bs = 'ps') + s(rol_qinfiltration_X10304, bs = 'ps') + s(rol_relhum_X10304, bs = 'ps') +
                                   s(rol_snowstorage_X10304, bs = 'ps') + s(rol_soilwaterrootzone_X10304, bs = 'ps') + s(rol_soilwaterunsatzone_X10304, bs = 'ps'),
                                 data = data_merged_rol_winter, family = gaussian(link = "log"))

model_winter_ps_intera_21 <- bam(formula = drainage_X10321 ~ YY +
                                   rol_snowstorage_X10321:rol_qinfiltration_X10321 +
                                   rol_qinfiltration_X10321:rol_soilwaterunsatzone_X10321 +
                                   s(rol_airtmp_X10321, bs = 'ps') + s(rol_glorad_X10321, bs = 'ps') + s(rol_groundwaterdepth_X10321, bs = 'ps') +
                                   s(rol_precip_X10321, bs = 'ps') + s(rol_qinfiltration_X10321, bs = 'ps') + s(rol_relhum_X10321, bs = 'ps') +
                                   s(rol_snowstorage_X10321, bs = 'ps') + s(rol_soilwaterrootzone_X10321, bs = 'ps') + s(rol_soilwaterunsatzone_X10321, bs = 'ps'),
                                 data = data_merged_rol_winter, family = Gamma(link = "log"))

model_winter_ps_intera_03 <- bam(formula = drainage_X10303 ~ drainage_X10304 + YY +
                                   rol_qinfiltration_X10303:rol_snowstorage_X10303 +
                                   rol_qinfiltration_X10303:rol_precip_X10303 +
                                   s(rol_airtmp_X10303, bs = 'ps') + s(rol_glorad_X10303, bs = 'ps') + s(rol_groundwaterdepth_X10303, bs = 'ps') +
                                   s(rol_precip_X10303, bs = 'ps') + s(rol_qinfiltration_X10303, bs = 'ps') + s(rol_relhum_X10303, bs = 'ps') +
                                   s(rol_snowstorage_X10303, bs = 'ps') + s(rol_soilwaterrootzone_X10303, bs = 'ps') + s(rol_soilwaterunsatzone_X10303, bs = 'ps'),
                                 data = data_merged_rol_winter, family = Gamma(link = "log"))

model_winter_ps_intera_02 <- bam(formula = drainage_X10302 ~ drainage_X10303 + drainage_X10321 + YY +
                                   rol_soilwaterrootzone_X10302:rol_airtmp_X10302 +
                                   rol_snowstorage_X10302:rol_groundwaterdepth_X10302 +
                                   s(rol_airtmp_X10302, bs = 'ps') + s(rol_glorad_X10302, bs = 'ps') + s(rol_groundwaterdepth_X10302, bs = 'ps') +
                                   s(rol_precip_X10302, bs = 'ps') + s(rol_qinfiltration_X10302, bs = 'ps') + s(rol_relhum_X10302, bs = 'ps') +
                                   s(rol_snowstorage_X10302, bs = 'ps') + s(rol_soilwaterrootzone_X10302, bs = 'ps') + s(rol_soilwaterunsatzone_X10302, bs = 'ps'),
                                 data = data_merged_rol_winter, family = Gamma(link = "log"))



# summer
model_summer_ps_intera_04 <- bam(formula = drainage_X10304 ~ YY +
                                   rol_glorad_X10304:rol_snowstorage_X10304 +
                                   rol_glorad_X10304:rol_qinfiltration_X10304 +
                                   s(rol_airtmp_X10304, bs = 'ps') + s(rol_glorad_X10304, bs = 'ps') + s(rol_groundwaterdepth_X10304, bs = 'ps') +
                                   s(rol_precip_X10304, bs = 'ps') + s(rol_qinfiltration_X10304, bs = 'ps') + s(rol_relhum_X10304, bs = 'ps') +
                                   s(rol_snowstorage_X10304, bs = 'ps') + s(rol_soilwaterrootzone_X10304, bs = 'ps') + s(rol_soilwaterunsatzone_X10304, bs = 'ps'),
                                 data = data_merged_rol_summer, family = Gamma(link = "log"))

model_summer_ps_intera_21 <- bam(formula = drainage_X10321 ~ YY +
                                   rol_snowstorage_X10321:rol_glorad_X10321 +
                                   rol_snowstorage_X10321:rol_qinfiltration_X10321 +
                                   s(rol_airtmp_X10321, bs = 'ps') + s(rol_glorad_X10321, bs = 'ps') + s(rol_groundwaterdepth_X10321, bs = 'ps') +
                                   s(rol_precip_X10321, bs = 'ps') + s(rol_qinfiltration_X10321, bs = 'ps') + s(rol_relhum_X10321, bs = 'ps') +
                                   s(rol_snowstorage_X10321, bs = 'ps') + s(rol_soilwaterrootzone_X10321, bs = 'ps') + s(rol_soilwaterunsatzone_X10321, bs = 'ps'),
                                 data = data_merged_rol_summer, family = Gamma(link = "log"))

model_summer_ps_intera_03 <- bam(formula = drainage_X10303 ~ drainage_X10304 + YY +
                                   rol_relhum_X10303:rol_soilwaterunsatzone_X10303 +
                                   rol_snowstorage_X10303:rol_precip_X10303 +
                                   s(rol_airtmp_X10303, bs = 'ps') + s(rol_glorad_X10303, bs = 'ps') + s(rol_groundwaterdepth_X10303, bs = 'ps') +
                                   s(rol_precip_X10303, bs = 'ps') + s(rol_qinfiltration_X10303, bs = 'ps') + s(rol_relhum_X10303, bs = 'ps') +
                                   s(rol_snowstorage_X10303, bs = 'ps') + s(rol_soilwaterrootzone_X10303, bs = 'ps') + s(rol_soilwaterunsatzone_X10303, bs = 'ps'),
                                 data = data_merged_rol_summer, family = Gamma(link = "log"))

model_summer_ps_intera_02 <- bam(formula = drainage_X10302 ~ drainage_X10303 + drainage_X10321 + YY +
                                   rol_precip_X10302:rol_snowstorage_X10302 +
                                   rol_relhum_X10302:rol_soilwaterunsatzone_X10302  +
                                   s(rol_airtmp_X10302, bs = 'ps') + s(rol_glorad_X10302, bs = 'ps') + s(rol_groundwaterdepth_X10302, bs = 'ps') +
                                   s(rol_precip_X10302, bs = 'ps') + s(rol_qinfiltration_X10302, bs = 'ps') + s(rol_relhum_X10302, bs = 'ps') +
                                   s(rol_snowstorage_X10302, bs = 'ps') + s(rol_soilwaterrootzone_X10302, bs = 'ps') + s(rol_soilwaterunsatzone_X10302, bs = 'ps'),
                                 data = data_merged_rol_summer, family = Gamma(link = "log"))




