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
library(dplyr)

# load data
data_merged_rol <- readRDS("data_merged_rol.rds")
data_merged_rol_spline <- data_merged_rol


data_merged_rol_spline$YY <- as.numeric(data_merged_rol_spline$YY)
# Interaction
# spline_knots <- function(value, l, k) {
#   if (is.na(value)) {
#     NA
#   } else if (value >= k) {
#     (value - k)^l
#   } else {
#     0
#   }
# }

data_merged_rol_spline <- data_merged_rol_spline %>%
  mutate(rol_airtmp_2_X10304 = rol_airtmp_X10304^2,
         rol_airtmp_3_X10304 = rol_airtmp_X10304^3)
         #rol_airtmp_3_k1_X10304 = spline_knots(rol_airtmp_X10304, 3, -5),
         #rol_airtmp_3_k2_X10304 = spline_knots(rol_airtmp_X10304, 3, 5))
data_merged_rol_spline$rol_airtmp_3_k1_X10304 <- NA
for (i in 1:nrow(data_merged_rol_spline)) {
  if (is.na(data_merged_rol_spline$rol_airtmp_X10304[i])) {
    data_merged_rol_spline$rol_airtmp_3_k1_X10304[i] <- NA
  } else if (data_merged_rol_spline$rol_airtmp_X10304[i] >= -5) {
    data_merged_rol_spline$rol_airtmp_3_k1_X10304[i] <- (data_merged_rol_spline$rol_airtmp_X10304[i] + 5)^3
  } else {
    data_merged_rol_spline$rol_airtmp_3_k1_X10304[i] <- 0
  }
}
data_merged_rol_spline$rol_airtmp_3_k2_X10304 <- NA
for (i in 1:nrow(data_merged_rol_spline)) {
  if (is.na(data_merged_rol_spline$rol_airtmp_X10304[i])) {
    data_merged_rol_spline$rol_airtmp_3_k2_X10304[i] <- NA
  } else if (data_merged_rol_spline$rol_airtmp_X10304[i] >= 5) {
    data_merged_rol_spline$rol_airtmp_3_k2_X10304[i] <- (data_merged_rol_spline$rol_airtmp_X10304[i] - 5)^3
  } else {
    data_merged_rol_spline$rol_airtmp_3_k2_X10304[i] <- 0
  }
}


# Bayesian network
variables_needed_spline <- c("drainage_X10302", "drainage_X10303", "drainage_X10304", "drainage_X10321",
                            "rol_airtmp_X10302", "rol_airtmp_X10303", "rol_airtmp_X10304", "rol_airtmp_X10321",
                            "rol_glorad_X10302", "rol_glorad_X10303", "rol_glorad_X10304", "rol_glorad_X10321",
                            "rol_groundwaterdepth_X10302", "rol_groundwaterdepth_X10303", "rol_groundwaterdepth_X10304", "rol_groundwaterdepth_X10321",
                            "rol_precip_X10302", "rol_precip_X10303", "rol_precip_X10304", "rol_precip_X10321",
                            "rol_qinfiltration_X10302", "rol_qinfiltration_X10303", "rol_qinfiltration_X10304", "rol_qinfiltration_X10321",
                            "rol_relhum_X10302", "rol_relhum_X10303", "rol_relhum_X10304", "rol_relhum_X10321",
                            "rol_snowstorage_X10302", "rol_snowstorage_X10303", "rol_snowstorage_X10304", "rol_snowstorage_X10321",
                            "rol_soilwaterrootzone_X10302", "rol_soilwaterrootzone_X10303", "rol_soilwaterrootzone_X10304", "rol_soilwaterrootzone_X10321",
                            "rol_soilwaterunsatzone_X10302", "rol_soilwaterunsatzone_X10303", "rol_soilwaterunsatzone_X10304", "rol_soilwaterunsatzone_X10321",
                            "YY",
                            "rol_airtmp_2_X10304", "rol_airtmp_3_X10304", "rol_airtmp_3_k1_X10304", "rol_airtmp_3_k2_X10304")

# Building network
network_spline <- empty.graph(variables_needed_spline)

network_spline <- set.arc(network_spline, "rol_airtmp_X10302", "drainage_X10302")
network_spline <- set.arc(network_spline, "rol_glorad_X10302", "drainage_X10302")
network_spline <- set.arc(network_spline, "rol_groundwaterdepth_X10302", "drainage_X10302")
network_spline <- set.arc(network_spline, "rol_precip_X10302", "drainage_X10302")
network_spline <- set.arc(network_spline, "rol_qinfiltration_X10302", "drainage_X10302")
network_spline <- set.arc(network_spline, "rol_relhum_X10302", "drainage_X10302")
network_spline <- set.arc(network_spline, "rol_snowstorage_X10302", "drainage_X10302")
network_spline <- set.arc(network_spline, "rol_soilwaterrootzone_X10302", "drainage_X10302")
network_spline <- set.arc(network_spline, "rol_soilwaterunsatzone_X10302", "drainage_X10302")
network_spline <- set.arc(network_spline, "rol_airtmp_X10303", "drainage_X10303")
network_spline <- set.arc(network_spline, "rol_glorad_X10303", "drainage_X10303")
network_spline <- set.arc(network_spline, "rol_groundwaterdepth_X10303", "drainage_X10303")
network_spline <- set.arc(network_spline, "rol_precip_X10303", "drainage_X10303")
network_spline <- set.arc(network_spline, "rol_qinfiltration_X10303", "drainage_X10303")
network_spline <- set.arc(network_spline, "rol_relhum_X10303", "drainage_X10303")
network_spline <- set.arc(network_spline, "rol_snowstorage_X10303", "drainage_X10303")
network_spline <- set.arc(network_spline, "rol_soilwaterrootzone_X10303", "drainage_X10303")
network_spline <- set.arc(network_spline, "rol_soilwaterunsatzone_X10303", "drainage_X10303")
network_spline <- set.arc(network_spline, "rol_airtmp_X10304", "drainage_X10304")
network_spline <- set.arc(network_spline, "rol_glorad_X10304", "drainage_X10304")
network_spline <- set.arc(network_spline, "rol_groundwaterdepth_X10304", "drainage_X10304")
network_spline <- set.arc(network_spline, "rol_precip_X10304", "drainage_X10304")
network_spline <- set.arc(network_spline, "rol_qinfiltration_X10304", "drainage_X10304")
network_spline <- set.arc(network_spline, "rol_relhum_X10304", "drainage_X10304")
network_spline <- set.arc(network_spline, "rol_snowstorage_X10304", "drainage_X10304")
network_spline <- set.arc(network_spline, "rol_soilwaterrootzone_X10304", "drainage_X10304")
network_spline <- set.arc(network_spline, "rol_soilwaterunsatzone_X10304", "drainage_X10304")
network_spline <- set.arc(network_spline, "rol_airtmp_X10321", "drainage_X10321")
network_spline <- set.arc(network_spline, "rol_glorad_X10321", "drainage_X10321")
network_spline <- set.arc(network_spline, "rol_groundwaterdepth_X10321", "drainage_X10321")
network_spline <- set.arc(network_spline, "rol_precip_X10321", "drainage_X10321")
network_spline <- set.arc(network_spline, "rol_qinfiltration_X10321", "drainage_X10321")
network_spline <- set.arc(network_spline, "rol_relhum_X10321", "drainage_X10321")
network_spline <- set.arc(network_spline, "rol_snowstorage_X10321", "drainage_X10321")
network_spline <- set.arc(network_spline, "rol_soilwaterrootzone_X10321", "drainage_X10321")
network_spline <- set.arc(network_spline, "rol_soilwaterunsatzone_X10321", "drainage_X10321")
network_spline <- set.arc(network_spline, "YY", "drainage_X10302")
network_spline <- set.arc(network_spline, "YY", "drainage_X10303")
network_spline <- set.arc(network_spline, "YY", "drainage_X10304")
network_spline <- set.arc(network_spline, "YY", "drainage_X10321")
network_spline <- set.arc(network_spline, "drainage_X10304", "drainage_X10303")
network_spline <- set.arc(network_spline, "drainage_X10303", "drainage_X10302")
network_spline <- set.arc(network_spline, "drainage_X10321", "drainage_X10302")
# Splines
network_spline <- set.arc(network_spline, "rol_airtmp_2_X10304", "drainage_X10304")
network_spline <- set.arc(network_spline, "rol_airtmp_3_X10304", "drainage_X10304")
network_spline <- set.arc(network_spline, "rol_airtmp_3_k1_X10304", "drainage_X10304")
network_spline <- set.arc(network_spline, "rol_airtmp_3_k2_X10304", "drainage_X10304")

# fitting bayesian network (parameters are estimated via maximum likelihood (method = "mle-g"))
fitted_network_spline <- bn.fit(network_spline, data = data_merged_rol_spline[, variables_needed_spline])
AIC(fitted_network_spline, data = na.omit(data_merged_rol_spline[, variables_needed_spline]))

# visualization
plot_obj_spline <- graphviz.plot(fitted_network_spline)
plot(plot_obj_spline)



# ?splineDesign
# splines::splineDesign()