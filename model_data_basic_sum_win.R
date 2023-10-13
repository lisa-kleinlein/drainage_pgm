#drainage_X10302 = Muenchen
#drainage_X10303 = BadToelz
#drainage_X10304 = Mittenwald
#drainage_X10321 = Schlehdorf


setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbe")

# load packages
library(zoo)
library(bnlearn)
library(Rgraphviz)
library(ggplot2)
source("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/functions.R")


# load data
data_merged_rol <- readRDS("data_merged_rol.rds")


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



data_merged_rol$YY <- as.numeric(data_merged_rol$YY)
# Bayesian network
variables_needed <- c("drainage_X10302", "drainage_X10303", "drainage_X10304", "drainage_X10321",
                      "rol_airtmp_X10302", "rol_airtmp_X10303", "rol_airtmp_X10304", "rol_airtmp_X10321",
                      "rol_glorad_X10302", "rol_glorad_X10303", "rol_glorad_X10304", "rol_glorad_X10321",
                      "rol_groundwaterdepth_X10302", "rol_groundwaterdepth_X10303", "rol_groundwaterdepth_X10304", "rol_groundwaterdepth_X10321",
                      "rol_precip_X10302", "rol_precip_X10303", "rol_precip_X10304", "rol_precip_X10321",
                      "rol_qinfiltration_X10302", "rol_qinfiltration_X10303", "rol_qinfiltration_X10304", "rol_qinfiltration_X10321",
                      "rol_relhum_X10302", "rol_relhum_X10303", "rol_relhum_X10304", "rol_relhum_X10321",
                      "rol_snowstorage_X10302", "rol_snowstorage_X10303", "rol_snowstorage_X10304", "rol_snowstorage_X10321",
                      "rol_soilwaterrootzone_X10302", "rol_soilwaterrootzone_X10303", "rol_soilwaterrootzone_X10304", "rol_soilwaterrootzone_X10321",
                      "rol_soilwaterunsatzone_X10302", "rol_soilwaterunsatzone_X10303", "rol_soilwaterunsatzone_X10304", "rol_soilwaterunsatzone_X10321",
                      "YY")

# Building network
network <- empty.graph(variables_needed)

network <- set.arc(network, "rol_airtmp_X10302", "drainage_X10302")
network <- set.arc(network, "rol_glorad_X10302", "drainage_X10302")
network <- set.arc(network, "rol_groundwaterdepth_X10302", "drainage_X10302")
network <- set.arc(network, "rol_precip_X10302", "drainage_X10302")
network <- set.arc(network, "rol_qinfiltration_X10302", "drainage_X10302")
network <- set.arc(network, "rol_relhum_X10302", "drainage_X10302")
network <- set.arc(network, "rol_snowstorage_X10302", "drainage_X10302")
network <- set.arc(network, "rol_soilwaterrootzone_X10302", "drainage_X10302")
network <- set.arc(network, "rol_soilwaterunsatzone_X10302", "drainage_X10302")
network <- set.arc(network, "rol_airtmp_X10303", "drainage_X10303")
network <- set.arc(network, "rol_glorad_X10303", "drainage_X10303")
network <- set.arc(network, "rol_groundwaterdepth_X10303", "drainage_X10303")
network <- set.arc(network, "rol_precip_X10303", "drainage_X10303")
network <- set.arc(network, "rol_qinfiltration_X10303", "drainage_X10303")
network <- set.arc(network, "rol_relhum_X10303", "drainage_X10303")
network <- set.arc(network, "rol_snowstorage_X10303", "drainage_X10303")
network <- set.arc(network, "rol_soilwaterrootzone_X10303", "drainage_X10303")
network <- set.arc(network, "rol_soilwaterunsatzone_X10303", "drainage_X10303")
network <- set.arc(network, "rol_airtmp_X10304", "drainage_X10304")
network <- set.arc(network, "rol_glorad_X10304", "drainage_X10304")
network <- set.arc(network, "rol_groundwaterdepth_X10304", "drainage_X10304")
network <- set.arc(network, "rol_precip_X10304", "drainage_X10304")
network <- set.arc(network, "rol_qinfiltration_X10304", "drainage_X10304")
network <- set.arc(network, "rol_relhum_X10304", "drainage_X10304")
network <- set.arc(network, "rol_snowstorage_X10304", "drainage_X10304")
network <- set.arc(network, "rol_soilwaterrootzone_X10304", "drainage_X10304")
network <- set.arc(network, "rol_soilwaterunsatzone_X10304", "drainage_X10304")
network <- set.arc(network, "rol_airtmp_X10321", "drainage_X10321")
network <- set.arc(network, "rol_glorad_X10321", "drainage_X10321")
network <- set.arc(network, "rol_groundwaterdepth_X10321", "drainage_X10321")
network <- set.arc(network, "rol_precip_X10321", "drainage_X10321")
network <- set.arc(network, "rol_qinfiltration_X10321", "drainage_X10321")
network <- set.arc(network, "rol_relhum_X10321", "drainage_X10321")
network <- set.arc(network, "rol_snowstorage_X10321", "drainage_X10321")
network <- set.arc(network, "rol_soilwaterrootzone_X10321", "drainage_X10321")
network <- set.arc(network, "rol_soilwaterunsatzone_X10321", "drainage_X10321")
network <- set.arc(network, "YY", "drainage_X10302")
network <- set.arc(network, "YY", "drainage_X10303")
network <- set.arc(network, "YY", "drainage_X10304")
network <- set.arc(network, "YY", "drainage_X10321")
network <- set.arc(network, "drainage_X10304", "drainage_X10303")
network <- set.arc(network, "drainage_X10303", "drainage_X10302")
network <- set.arc(network, "drainage_X10321", "drainage_X10302")


# separated according to hydrological half-years
# create data sets
data_merged_rol_summer <- data_merged_rol[data_merged_rol$MM >= 5 & data_merged_rol$MM <= 10, ]
data_merged_rol_winter <- data_merged_rol[data_merged_rol$MM < 5 | data_merged_rol$MM > 10, ]

# fitting bayesian network for winter (parameters are estimated via maximum likelihood (method = "mle-g"))
fitted_network_winter <- bn.fit(network, data = data_merged_rol_winter[, variables_needed])

## model fit
bn_model_diag_qq_lowlevel(bn.fit = fitted_network_winter, data = data_merged_rol_winter, halfyear = "winter")
bn_model_diag_predic(bn.fit = fitted_network_winter, data = data_merged_rol_winter)










# fitting bayesian network for summer (parameters are estimated via maximum likelihood (method = "mle-g"))
fitted_network_summer <- bn.fit(network, data = data_merged_rol_summer[, variables_needed])

## model fit
bn_model_diag_qq_lowlevel(bn.fit = fitted_network_summer, data = data_merged_rol_summer, halfyear = "summer")
bn_model_diag_predic(bn.fit = fitted_network_summer, data = data_merged_rol_summer)








