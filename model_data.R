setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbe")

# load packages
library(bnlearn)
library(Rgraphviz)

# load data
data_merged_rol <- readRDS("data_merged_rol.rds")

# bayes network
variables_needed <- c("drainage_X10302", "drainage_X10303", "drainage_X10304", "drainage_X10321",
                      "rol_airtmp_X10302", "rol_airtmp_X10303", "rol_airtmp_X10304", "rol_airtmp_X10321",
                      "rol_glorad_X10302", "rol_glorad_X10303", "rol_glorad_X10304", "rol_glorad_X10321",
                      "rol_groundwaterdepth_X10302", "rol_groundwaterdepth_X10303", "rol_groundwaterdepth_X10304", "rol_groundwaterdepth_X10321",
                      "rol_precip_X10302", "rol_precip_X10303", "rol_precip_X10304", "rol_precip_X10321",
                      "rol_qinfiltration_X10302", "rol_qinfiltration_X10303", "rol_qinfiltration_X10304", "rol_qinfiltration_X10321",
                      "rol_relhum_X10302", "rol_relhum_X10303", "rol_relhum_X10304", "rol_relhum_X10321",
                      "rol_snowstorage_X10302", "rol_snowstorage_X10303", "rol_snowstorage_X10304", "rol_snowstorage_X10321",
                      "rol_soilwaterrootzone_X10302", "rol_soilwaterrootzone_X10303", "rol_soilwaterrootzone_X10304", "rol_soilwaterrootzone_X10321",
                      "rol_soilwaterunsatzone_X10302", "rol_soilwaterunsatzone_X10303", "rol_soilwaterunsatzone_X10304", "rol_soilwaterunsatzone_X10321")

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
network <- set.arc(network, "drainage_X10304", "drainage_X10303")
network <- set.arc(network, "drainage_X10303", "drainage_X10302")
network <- set.arc(network, "drainage_X10321", "drainage_X10302")

fitted_network <- bn.fit(network, data = data_merged_rol[, variables_needed])

# visualization
plot_obj <- graphviz.plot(fitted_network)
plot(plot_obj)
