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
data_merged_rol_inter <- data_merged_rol


data_merged_rol_inter$YY <- as.numeric(data_merged_rol_inter$YY)
# Interaction
data_merged_rol_inter <- data_merged_rol_inter %>%
  mutate(rol_airtmp_precip_X10304 = rol_airtmp_X10304 * rol_precip_X10304,
         rol_glorad_snowstorage_X10304 = rol_glorad_X10304 * rol_snowstorage_X10304)

# Bayesian network
variables_needed_inter <- c("drainage_X10302", "drainage_X10303", "drainage_X10304", "drainage_X10321",
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
                        "rol_airtmp_precip_X10304", "rol_glorad_snowstorage_X10304")

# Building network
network_inter <- empty.graph(variables_needed_inter)

network_inter <- set.arc(network_inter, "rol_airtmp_X10302", "drainage_X10302")
network_inter <- set.arc(network_inter, "rol_glorad_X10302", "drainage_X10302")
network_inter <- set.arc(network_inter, "rol_groundwaterdepth_X10302", "drainage_X10302")
network_inter <- set.arc(network_inter, "rol_precip_X10302", "drainage_X10302")
network_inter <- set.arc(network_inter, "rol_qinfiltration_X10302", "drainage_X10302")
network_inter <- set.arc(network_inter, "rol_relhum_X10302", "drainage_X10302")
network_inter <- set.arc(network_inter, "rol_snowstorage_X10302", "drainage_X10302")
network_inter <- set.arc(network_inter, "rol_soilwaterrootzone_X10302", "drainage_X10302")
network_inter <- set.arc(network_inter, "rol_soilwaterunsatzone_X10302", "drainage_X10302")
network_inter <- set.arc(network_inter, "rol_airtmp_X10303", "drainage_X10303")
network_inter <- set.arc(network_inter, "rol_glorad_X10303", "drainage_X10303")
network_inter <- set.arc(network_inter, "rol_groundwaterdepth_X10303", "drainage_X10303")
network_inter <- set.arc(network_inter, "rol_precip_X10303", "drainage_X10303")
network_inter <- set.arc(network_inter, "rol_qinfiltration_X10303", "drainage_X10303")
network_inter <- set.arc(network_inter, "rol_relhum_X10303", "drainage_X10303")
network_inter <- set.arc(network_inter, "rol_snowstorage_X10303", "drainage_X10303")
network_inter <- set.arc(network_inter, "rol_soilwaterrootzone_X10303", "drainage_X10303")
network_inter <- set.arc(network_inter, "rol_soilwaterunsatzone_X10303", "drainage_X10303")
network_inter <- set.arc(network_inter, "rol_airtmp_X10304", "drainage_X10304")
network_inter <- set.arc(network_inter, "rol_glorad_X10304", "drainage_X10304")
network_inter <- set.arc(network_inter, "rol_groundwaterdepth_X10304", "drainage_X10304")
network_inter <- set.arc(network_inter, "rol_precip_X10304", "drainage_X10304")
network_inter <- set.arc(network_inter, "rol_qinfiltration_X10304", "drainage_X10304")
network_inter <- set.arc(network_inter, "rol_relhum_X10304", "drainage_X10304")
network_inter <- set.arc(network_inter, "rol_snowstorage_X10304", "drainage_X10304")
network_inter <- set.arc(network_inter, "rol_soilwaterrootzone_X10304", "drainage_X10304")
network_inter <- set.arc(network_inter, "rol_soilwaterunsatzone_X10304", "drainage_X10304")
network_inter <- set.arc(network_inter, "rol_airtmp_X10321", "drainage_X10321")
network_inter <- set.arc(network_inter, "rol_glorad_X10321", "drainage_X10321")
network_inter <- set.arc(network_inter, "rol_groundwaterdepth_X10321", "drainage_X10321")
network_inter <- set.arc(network_inter, "rol_precip_X10321", "drainage_X10321")
network_inter <- set.arc(network_inter, "rol_qinfiltration_X10321", "drainage_X10321")
network_inter <- set.arc(network_inter, "rol_relhum_X10321", "drainage_X10321")
network_inter <- set.arc(network_inter, "rol_snowstorage_X10321", "drainage_X10321")
network_inter <- set.arc(network_inter, "rol_soilwaterrootzone_X10321", "drainage_X10321")
network_inter <- set.arc(network_inter, "rol_soilwaterunsatzone_X10321", "drainage_X10321")
network_inter <- set.arc(network_inter, "YY", "drainage_X10302")
network_inter <- set.arc(network_inter, "YY", "drainage_X10303")
network_inter <- set.arc(network_inter, "YY", "drainage_X10304")
network_inter <- set.arc(network_inter, "YY", "drainage_X10321")
network_inter <- set.arc(network_inter, "drainage_X10304", "drainage_X10303")
network_inter <- set.arc(network_inter, "drainage_X10303", "drainage_X10302")
network_inter <- set.arc(network_inter, "drainage_X10321", "drainage_X10302")
# Interactions
network_inter <- set.arc(network_inter, "rol_airtmp_precip_X10304", "drainage_X10304")
network_inter <- set.arc(network_inter, "rol_glorad_snowstorage_X10304", "drainage_X10304")

# fitting bayesian network (parameters are estimated via maximum likelihood (method = "mle-g"))
fitted_network_inter <- bn.fit(network_inter, data = data_merged_rol_inter[, variables_needed_inter])
AIC(fitted_network_inter, data = na.omit(data_merged_rol_inter[, variables_needed_inter]))

# visualization
plot_obj_inter <- graphviz.plot(fitted_network_inter)
plot(plot_obj_inter)
