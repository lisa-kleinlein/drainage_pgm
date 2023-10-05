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

# fitting bayesian network (parameters are estimated via maximum likelihood (method = "mle-g"))
fitted_network <- bn.fit(network, data = data_merged_rol[, variables_needed])
# model fit?
# bn.fit.qqplot(fitted_network)
# bn.fit.xyplot(fitted_network)
# bn.fit.histogram(fitted_network)

# visualization
plot_obj <- graphviz.plot(fitted_network)
plot(plot_obj)




# examine normal distribution assumption
i <- 11
ggplot(data_merged_rol, aes(x = get(variables_needed[i]))) +
  geom_density() +
  labs(x = variables_needed[i])




# information criteria?
# BIC(fitted_network, data = na.omit(data_merged_rol[, variables_needed]))
# AIC(fitted_network, data = na.omit(data_merged_rol[, variables_needed]))



# measuring point X10302
predicted_values_X10302 <- predict(fitted_network, node = "drainage_X10302", data = data_merged_rol[, variables_needed], method = "bayes-lw")
plot(data_merged_rol$drainage_X10302, predicted_values_X10302,
     xlim = c(0, 800), ylim = c(0, 800),
     xlab = "values from data", ylab = "predictions", main = "drainage, Munich")
# rather overestimates values

fitted_network$drainage_X10302
residuals_X10302 <- data_merged_rol$drainage_X10302 - predicted_values_X10302
boxplot(residuals_X10302)
plot(data_merged_rol$drainage_X10302, residuals_X10302)


# measuring point X10303
predicted_values_X10303 <- predict(fitted_network, node = "drainage_X10303", data = data_merged_rol[, variables_needed], method = "bayes-lw")
plot(data_merged_rol$drainage_X10303, predicted_values_X10303, xlim = c(0, 350), ylim = c(0, 350))
# rather underestimates values

fitted_network$drainage_X10303
residuals_X10303 <- data_merged_rol$drainage_X10303 - predicted_values_X10303
boxplot(residuals_X10303)
plot(data_merged_rol$drainage_X10303, residuals_X10303)


# measuring point X10304
predicted_values_X10304 <- predict(fitted_network, node = "drainage_X10304", data = data_merged_rol[, variables_needed], method = "bayes-lw")
plot(data_merged_rol$drainage_X10304, predicted_values_X10304, xlim = c(0, 130), ylim = c(0, 130))
# rather underestimates values

fitted_network$drainage_X10304
residuals_X10304 <- data_merged_rol$drainage_X10304 - predicted_values_X10304
boxplot(residuals_X10304)
plot(data_merged_rol$drainage_X10304, residuals_X10304)


# measuring point X10321
predicted_values_X10321 <- predict(fitted_network, node = "drainage_X10321", data = data_merged_rol[, variables_needed], method = "bayes-lw")
plot(data_merged_rol$drainage_X10321, predicted_values_X10321, xlim = c(0, 300), ylim = c(0, 300))
# rather underestimates values

fitted_network$drainage_X10321
residuals_X10321 <- data_merged_rol$drainage_X10321 - predicted_values_X10321
boxplot(residuals_X10321)
plot(data_merged_rol$drainage_X10321, residuals_X10321)
