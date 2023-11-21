#drainage_X10302 = Muenchen
#drainage_X10303 = BadToelz
#drainage_X10304 = Mittenwald
#drainage_X10321 = Schlehdorf

# load packages
library(zoo)
library(bnlearn)
library(Rgraphviz)
library(ggplot2)
library(mgcv)
library(interactions)
source("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/functions.R")

# load data
setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbe")
data_merged_rol <- readRDS("data_merged_rol.rds")
setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbj")
data_merged_rol_kbj <- readRDS("data_merged_rol_kbj.rds")
# summer data set
data_merged_rol_summer <- data_merged_rol[data_merged_rol$MM >= 5 & data_merged_rol$MM <= 10, ]
data_merged_rol_summer_kbj <- data_merged_rol_kbj[data_merged_rol_kbj$MM >= 5 & data_merged_rol_kbj$MM <= 10, ]

data_merged_rol_summer$YY <- as.numeric(data_merged_rol_summer$YY)
data_merged_rol_summer_kbj$YY <- as.numeric(data_merged_rol_summer_kbj$YY)

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





#### GAMs

### Mittenwald
formula_04 <- drainage_X10304 ~ YY +
  s(rol_airtmp_X10304, bs = "ps") + s(rol_glorad_X10304, bs = "ps") + s(rol_groundwaterdepth_X10304, bs = "ps") +
  s(rol_precip_X10304, bs = "ps") + s(rol_qinfiltration_X10304, bs = "ps") + s(rol_relhum_X10304, bs = "ps") +
  s(rol_snowstorage_X10304, bs = "ps") + s(rol_soilwaterrootzone_X10304, bs = "ps") + s(rol_soilwaterunsatzone_X10304, bs = "ps")
## Verteilungsannahme: gauss
# link: log
model_summer_ps_04_1 <- bam(formula = formula_04, data = data_merged_rol_summer,
                            family = gaussian(link = "log"))
summary(model_summer_ps_04_1)
# model diagnosis
data_res_summer_ps_04_1_kbj <- data_obs_res_kbj(model = model_summer_ps_04_1, data_kbj = data_merged_rol_summer_kbj, catchment = "X10304")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_04_1_kbj, halfyear = "summer", catchment = "X10304",
                 title = "Mittenwald, gaussian, log link, summer (kbj)")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_04_1_kbj, halfyear = "summer", catchment = "X10304",
                 title = "Mittenwald, gaussian, log link, summer (kbj)", ylim_l = -100, ylim_u = 50)
mse_summer_ps_04_1_kbj <- res_mse_kbj(data_obs_res_kbj = data_res_summer_ps_04_1_kbj, halfyear = "summer", catchment = "X10304")



## Verteilungsannahme: gamma
# link: log
model_summer_ps_04_2 <- bam(formula = formula_04, data = data_merged_rol_summer,
                            family = Gamma(link = "log"))
summary(model_summer_ps_04_2)
# model diagnosis
data_res_summer_ps_04_2_kbj <- data_obs_res_kbj(model = model_summer_ps_04_2, data_kbj = data_merged_rol_summer_kbj, catchment = "X10304")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_04_2_kbj, halfyear = "summer", catchment = "X10304",
                 title = "Mittenwald, gamma, log link, summer (kbj)")
mse_summer_ps_04_2_kbj <- res_mse_kbj(data_obs_res_kbj = data_res_summer_ps_04_2_kbj, halfyear = "summer", catchment = "X10304")

### due to mse for member kbj for low drainage values: choose gamma, link: log


# adding interactions
vars_04 <- c("rol_airtmp_X10304", "rol_glorad_X10304", "rol_groundwaterdepth_X10304", "rol_precip_X10304",
             "rol_qinfiltration_X10304", "rol_relhum_X10304", "rol_snowstorage_X10304", "rol_soilwaterrootzone_X10304", "rol_soilwaterunsatzone_X10304")
# 1. interaction
formula_chr_04 <- "drainage_X10304 ~ YY +
  s(rol_airtmp_X10304, bs = 'ps') + s(rol_glorad_X10304, bs = 'ps') + s(rol_groundwaterdepth_X10304, bs = 'ps') +
  s(rol_precip_X10304, bs = 'ps') + s(rol_qinfiltration_X10304, bs = 'ps') + s(rol_relhum_X10304, bs = 'ps') +
  s(rol_snowstorage_X10304, bs = 'ps') + s(rol_soilwaterrootzone_X10304, bs = 'ps') + s(rol_soilwaterunsatzone_X10304, bs = 'ps')"
find_best_interac(vars = vars_04, formula_chr = formula_chr_04, data = data_merged_rol_summer,
                  family = Gamma(link = "log"), data_kbj = data_merged_rol_summer_kbj, catchment = "X10304", halfyear = "summer")

# 2. interaction
formula_chr_04 <- "drainage_X10304 ~ YY +
  rol_glorad_X10304:rol_snowstorage_X10304 +
  s(rol_airtmp_X10304, bs = 'ps') + s(rol_glorad_X10304, bs = 'ps') + s(rol_groundwaterdepth_X10304, bs = 'ps') +
  s(rol_precip_X10304, bs = 'ps') + s(rol_qinfiltration_X10304, bs = 'ps') + s(rol_relhum_X10304, bs = 'ps') +
  s(rol_snowstorage_X10304, bs = 'ps') + s(rol_soilwaterrootzone_X10304, bs = 'ps') + s(rol_soilwaterunsatzone_X10304, bs = 'ps')"
find_best_interac(vars = vars_04, formula_chr = formula_chr_04, data = data_merged_rol_summer,
                  family = Gamma(link = "log"), data_kbj = data_merged_rol_summer_kbj, catchment = "X10304", halfyear = "summer")


formula_chr_04 <- "drainage_X10304 ~ YY +
  rol_glorad_X10304:rol_snowstorage_X10304 +
  rol_glorad_X10304:rol_qinfiltration_X10304 +
  s(rol_airtmp_X10304, bs = 'ps') + s(rol_glorad_X10304, bs = 'ps') + s(rol_groundwaterdepth_X10304, bs = 'ps') +
  s(rol_precip_X10304, bs = 'ps') + s(rol_qinfiltration_X10304, bs = 'ps') + s(rol_relhum_X10304, bs = 'ps') +
  s(rol_snowstorage_X10304, bs = 'ps') + s(rol_soilwaterrootzone_X10304, bs = 'ps') + s(rol_soilwaterunsatzone_X10304, bs = 'ps')"
model_summer_ps_intera_04 <- bam(formula = as.formula(formula_chr_04), data = data_merged_rol_summer,
                                 family = Gamma(link = "log"))

data_res_summer_ps_intera_04_kbj <- data_obs_res_kbj(model = model_summer_ps_intera_04, data_kbj = data_merged_rol_summer_kbj, catchment = "X10304")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_intera_04_kbj, halfyear = "summer", catchment = "X10304",
                 title = "Mittenwald, gamma, log link, summer (kbj) with interactions", ylim_l = -100, ylim_u = 100)
mse_summer_ps_intera_04_kbj <- res_mse_kbj(data_obs_res_kbj = data_res_summer_ps_intera_04_kbj, halfyear = "summer", catchment = "X10304")


plot(model_summer_ps_intera_04)
interact_plot(model = model_summer_ps_intera_04, pred = rol_glorad_X10304, modx = rol_snowstorage_X10304,
              interval = TRUE, int.type = "confidence")
interact_plot(model = model_summer_ps_intera_04, pred = rol_snowstorage_X10304, modx = rol_glorad_X10304,
              interval = TRUE, int.type = "confidence")

interact_plot(model = model_summer_ps_intera_04, pred = rol_glorad_X10304, modx = rol_qinfiltration_X10304,
              interval = TRUE, int.type = "confidence")
interact_plot(model = model_summer_ps_intera_04, pred = rol_qinfiltration_X10304, modx = rol_glorad_X10304,
              interval = TRUE, int.type = "confidence")







### Schlehdorf
formula_21 <- drainage_X10321 ~ YY +
  s(rol_airtmp_X10321, bs = "ps") + s(rol_glorad_X10321, bs = "ps") + s(rol_groundwaterdepth_X10321, bs = "ps") +
  s(rol_precip_X10321, bs = "ps") + s(rol_qinfiltration_X10321, bs = "ps") + s(rol_relhum_X10321, bs = "ps") +
  s(rol_snowstorage_X10321, bs = "ps") + s(rol_soilwaterrootzone_X10321, bs = "ps") + s(rol_soilwaterunsatzone_X10321, bs = "ps")
## Verteilungsannahme: gauss
# link: log
model_summer_ps_21_1 <- bam(formula = formula_21, data = data_merged_rol_summer,
                            family = gaussian(link = "log"))
summary(model_summer_ps_21_1)
# model diagnosis
data_res_summer_ps_21_1_kbj <- data_obs_res_kbj(model = model_summer_ps_21_1, data_kbj = data_merged_rol_summer_kbj, catchment = "X10321")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_21_1_kbj, halfyear = "summer", catchment = "X10321",
                 title = "Schlehdorf, gaussian, log link, summer (kbj)")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_21_1_kbj, halfyear = "summer", catchment = "X10321",
                 title = "Schlehdorf, gaussian, log link, summer (kbj)", ylim_l = -100, ylim_u = 100)
mse_summer_ps_21_1_kbj <- res_mse_kbj(data_obs_res_kbj = data_res_summer_ps_21_1_kbj, halfyear = "summer", catchment = "X10321")


## Verteilungsannahme: gamma
# link: log
model_summer_ps_21_2 <- bam(formula = formula_21, data = data_merged_rol_summer,
                            family = Gamma(link = "log"))
summary(model_summer_ps_21_2)
# model diagnosis
data_res_summer_ps_21_2_kbj <- data_obs_res_kbj(model = model_summer_ps_21_2, data_kbj = data_merged_rol_summer_kbj, catchment = "X10321")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_21_2_kbj, halfyear = "summer", catchment = "X10321",
                 title = "Schlehdorf, gamma, log link, summer (kbj)", all = TRUE, ylim_l = -100, ylim_u = 100)
mse_summer_ps_21_2_kbj <- res_mse_kbj(data_obs_res_kbj = data_res_summer_ps_21_2_kbj, halfyear = "summer", catchment = "X10321")

### due to mse for member kbj for low drainage values: choose gamma, link: log

# adding interactions
vars_21 <- c("rol_airtmp_X10321", "rol_glorad_X10321", "rol_groundwaterdepth_X10321", "rol_precip_X10321",
             "rol_qinfiltration_X10321", "rol_relhum_X10321", "rol_snowstorage_X10321", "rol_soilwaterrootzone_X10321", "rol_soilwaterunsatzone_X10321")
# 1. interaction
formula_chr_21 <- "drainage_X10321 ~ YY +
  s(rol_airtmp_X10321, bs = 'ps') + s(rol_glorad_X10321, bs = 'ps') + s(rol_groundwaterdepth_X10321, bs = 'ps') +
  s(rol_precip_X10321, bs = 'ps') + s(rol_qinfiltration_X10321, bs = 'ps') + s(rol_relhum_X10321, bs = 'ps') +
  s(rol_snowstorage_X10321, bs = 'ps') + s(rol_soilwaterrootzone_X10321, bs = 'ps') + s(rol_soilwaterunsatzone_X10321, bs = 'ps')"
find_best_interac(vars = vars_21, formula_chr = formula_chr_21, data = data_merged_rol_summer,
                  family = Gamma(link = "log"), data_kbj = data_merged_rol_summer_kbj, catchment = "X10321", halfyear = "summer")

# 2. interaction
formula_chr_21 <- "drainage_X10321 ~ YY +
  rol_snowstorage_X10321:rol_glorad_X10321 +
  s(rol_airtmp_X10321, bs = 'ps') + s(rol_glorad_X10321, bs = 'ps') + s(rol_groundwaterdepth_X10321, bs = 'ps') +
  s(rol_precip_X10321, bs = 'ps') + s(rol_qinfiltration_X10321, bs = 'ps') + s(rol_relhum_X10321, bs = 'ps') +
  s(rol_snowstorage_X10321, bs = 'ps') + s(rol_soilwaterrootzone_X10321, bs = 'ps') + s(rol_soilwaterunsatzone_X10321, bs = 'ps')"
find_best_interac(vars = vars_21, formula_chr = formula_chr_21, data = data_merged_rol_summer,
                  family = Gamma(link = "log"), data_kbj = data_merged_rol_summer_kbj, catchment = "X10321", halfyear = "summer")


formula_chr_21 <- "drainage_X10321 ~ YY +
  rol_snowstorage_X10321:rol_glorad_X10321 +
  rol_snowstorage_X10321:rol_qinfiltration_X10321 +
  s(rol_airtmp_X10321, bs = 'ps') + s(rol_glorad_X10321, bs = 'ps') + s(rol_groundwaterdepth_X10321, bs = 'ps') +
  s(rol_precip_X10321, bs = 'ps') + s(rol_qinfiltration_X10321, bs = 'ps') + s(rol_relhum_X10321, bs = 'ps') +
  s(rol_snowstorage_X10321, bs = 'ps') + s(rol_soilwaterrootzone_X10321, bs = 'ps') + s(rol_soilwaterunsatzone_X10321, bs = 'ps')"
model_summer_ps_intera_21 <- bam(formula = as.formula(formula_chr_21), data = data_merged_rol_summer,
                                 family = Gamma(link = "log"))

summary(model_summer_ps_intera_21)

data_res_summer_ps_intera_21_kbj <- data_obs_res_kbj(model = model_summer_ps_intera_21, data_kbj = data_merged_rol_summer_kbj, catchment = "X10321")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_intera_21_kbj, halfyear = "summer", catchment = "X10321",
                 title = "Schlehdorf, gamma, log link, summer (kbj) with interactions", ylim_l = -100, ylim_u = 100)
mse_summer_ps_intera_21_kbj <- res_mse_kbj(data_obs_res_kbj = data_res_summer_ps_intera_21_kbj, halfyear = "summer", catchment = "X10321")


plot(model_summer_ps_intera_21)
interact_plot(model = model_summer_ps_intera_21, pred = rol_snowstorage_X10321, modx = rol_glorad_X10321,
              interval = TRUE, int.type = "confidence")
interact_plot(model = model_summer_ps_intera_21, pred = rol_glorad_X10321, modx = rol_snowstorage_X10321,
              interval = TRUE, int.type = "confidence")

interact_plot(model = model_summer_ps_intera_21, pred = rol_snowstorage_X10321, modx = rol_qinfiltration_X10321,
              interval = TRUE, int.type = "confidence")
interact_plot(model = model_summer_ps_intera_21, pred = rol_qinfiltration_X10321, modx = rol_snowstorage_X10321,
              interval = TRUE, int.type = "confidence")









### Bad Tölz
formula_03 <- drainage_X10303 ~ drainage_X10304 + YY +
  s(rol_airtmp_X10303, bs = "ps") + s(rol_glorad_X10303, bs = "ps") + s(rol_groundwaterdepth_X10303, bs = "ps") +
  s(rol_precip_X10303, bs = "ps") + s(rol_qinfiltration_X10303, bs = "ps") + s(rol_relhum_X10303, bs = "ps") +
  s(rol_snowstorage_X10303, bs = "ps") + s(rol_soilwaterrootzone_X10303, bs = "ps") + s(rol_soilwaterunsatzone_X10303, bs = "ps")
## Verteilungsannahme: gauss
# link: log
model_summer_ps_03_1 <- bam(formula = formula_03, data = data_merged_rol_summer,
                            family = gaussian(link = "log"))
summary(model_summer_ps_03_1)
# model diagnosis
data_res_summer_ps_03_1_kbj <- data_obs_res_kbj(model = model_summer_ps_03_1, data_kbj = data_merged_rol_summer_kbj, catchment = "X10303")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_03_1_kbj, halfyear = "summer", catchment = "X10303",
                 title = "Bad Tölz, gaussian, log link, summer (kbj)")
mse_summer_ps_03_1_kbj <- res_mse_kbj(data_obs_res_kbj = data_res_summer_ps_03_1_kbj, halfyear = "summer", catchment = "X10303")


## Verteilungsannahme: gamma
# link: log
model_summer_ps_03_2 <- bam(formula = formula_03, data = data_merged_rol_summer,
                            family = Gamma(link = "log"))
summary(model_summer_ps_03_2)
# model diagnosis
data_res_summer_ps_03_2_kbj <- data_obs_res_kbj(model = model_summer_ps_03_2, data_kbj = data_merged_rol_summer_kbj, catchment = "X10303")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_03_2_kbj, halfyear = "summer", catchment = "X10303",
                 title = "Bad Tölz, gamma, log link, summer (kbj)")
mse_summer_ps_03_2_kbj <- res_mse_kbj(data_obs_res_kbj = data_res_summer_ps_03_2_kbj, halfyear = "summer", catchment = "X10303")

### due to mse for low drainage values: choose gamma, link: log

# adding interactions
vars_03 <- c("rol_airtmp_X10303", "rol_glorad_X10303", "rol_groundwaterdepth_X10303", "rol_precip_X10303",
             "rol_qinfiltration_X10303", "rol_relhum_X10303", "rol_snowstorage_X10303", "rol_soilwaterrootzone_X10303", "rol_soilwaterunsatzone_X10303")
# 1. interaction
formula_chr_03 <- "drainage_X10303 ~ drainage_X10304 + YY +
  s(rol_airtmp_X10303, bs = 'ps') + s(rol_glorad_X10303, bs = 'ps') + s(rol_groundwaterdepth_X10303, bs = 'ps') +
  s(rol_precip_X10303, bs = 'ps') + s(rol_qinfiltration_X10303, bs = 'ps') + s(rol_relhum_X10303, bs = 'ps') +
  s(rol_snowstorage_X10303, bs = 'ps') + s(rol_soilwaterrootzone_X10303, bs = 'ps') + s(rol_soilwaterunsatzone_X10303, bs = 'ps')"
find_best_interac(vars = vars_03, formula_chr = formula_chr_03, data = data_merged_rol_summer,
                  family = Gamma(link = "log"), data_kbj = data_merged_rol_summer_kbj, catchment = "X10303", halfyear = "summer")

# 2. interaction
formula_chr_03 <- "drainage_X10303 ~ drainage_X10304 + YY +
  rol_relhum_X10303:rol_soilwaterunsatzone_X10303 +
  s(rol_airtmp_X10303, bs = 'ps') + s(rol_glorad_X10303, bs = 'ps') + s(rol_groundwaterdepth_X10303, bs = 'ps') +
  s(rol_precip_X10303, bs = 'ps') + s(rol_qinfiltration_X10303, bs = 'ps') + s(rol_relhum_X10303, bs = 'ps') +
  s(rol_snowstorage_X10303, bs = 'ps') + s(rol_soilwaterrootzone_X10303, bs = 'ps') + s(rol_soilwaterunsatzone_X10303, bs = 'ps')"
find_best_interac(vars = vars_03, formula_chr = formula_chr_03, data = data_merged_rol_summer,
                  family = Gamma(link = "log"), data_kbj = data_merged_rol_summer_kbj, catchment = "X10303", halfyear = "summer")

formula_chr_03 <- "drainage_X10303 ~ drainage_X10304 + YY +
  rol_relhum_X10303:rol_soilwaterunsatzone_X10303 +
  rol_snowstorage_X10303:rol_precip_X10303 +
  s(rol_airtmp_X10303, bs = 'ps') + s(rol_glorad_X10303, bs = 'ps') + s(rol_groundwaterdepth_X10303, bs = 'ps') +
  s(rol_precip_X10303, bs = 'ps') + s(rol_qinfiltration_X10303, bs = 'ps') + s(rol_relhum_X10303, bs = 'ps') +
  s(rol_snowstorage_X10303, bs = 'ps') + s(rol_soilwaterrootzone_X10303, bs = 'ps') + s(rol_soilwaterunsatzone_X10303, bs = 'ps')"
model_summer_ps_intera_03 <- bam(formula = as.formula(formula_chr_03), data = data_merged_rol_summer,
                                 family = Gamma(link = "log"))
summary(model_summer_ps_intera_03)
data_res_summer_ps_intera_03_kbj <- data_obs_res_kbj(model = model_summer_ps_intera_03, data_kbj = data_merged_rol_summer_kbj, catchment = "X10303")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_intera_03_kbj, halfyear = "summer", catchment = "X10303",
                 title = "Bad Tölz, gamma, log link, summer (kbj) with interactions")
mse_summer_ps_intera_03_kbj <- res_mse_kbj(data_obs_res_kbj = data_res_summer_ps_intera_03_kbj, halfyear = "summer", catchment = "X10303")


plot(model_summer_ps_intera_03)
interact_plot(model = model_summer_ps_intera_03, pred = rol_relhum_X10303, modx = rol_soilwaterunsatzone_X10303,
              interval = TRUE, int.type = "confidence")
interact_plot(model = model_summer_ps_intera_03, pred = rol_soilwaterunsatzone_X10303, modx = rol_relhum_X10303,
              interval = TRUE, int.type = "confidence")
interact_plot(model = model_summer_ps_intera_03, pred = rol_snowstorage_X10303, modx = rol_precip_X10303,
              interval = TRUE, int.type = "confidence")
interact_plot(model = model_summer_ps_intera_03, pred = rol_precip_X10303, modx = rol_snowstorage_X10303,
              interval = TRUE, int.type = "confidence")
















### Munich
formula_02 <- drainage_X10302 ~ drainage_X10303 + drainage_X10321 + YY +
  s(rol_airtmp_X10302, bs = "ps") + s(rol_glorad_X10302, bs = "ps") + s(rol_groundwaterdepth_X10302, bs = "ps") +
  s(rol_precip_X10302, bs = "ps") + s(rol_qinfiltration_X10302, bs = "ps") + s(rol_relhum_X10302, bs = "ps") +
  s(rol_snowstorage_X10302, bs = "ps") + s(rol_soilwaterrootzone_X10302, bs = "ps") + s(rol_soilwaterunsatzone_X10302, bs = "ps")
## Verteilungsannahme: gauss
# link: log
model_summer_ps_02_1 <- bam(formula = formula_02, data = data_merged_rol_summer,
                            family = gaussian(link = "log"))
summary(model_summer_ps_02_1)
# model diagnosis
data_res_summer_ps_02_1_kbj <- data_obs_res_kbj(model = model_summer_ps_02_1, data_kbj = data_merged_rol_summer_kbj, catchment = "X10302")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_02_1_kbj, halfyear = "summer", catchment = "X10302",
                 title = "Munich, gaussian, log link, summer (kbj)")
mse_summer_ps_02_1_kbj <- res_mse_kbj(data_obs_res_kbj = data_res_summer_ps_02_1_kbj, halfyear = "summer", catchment = "X10302")


## Verteilungsannahme: gamma
# link: log
model_summer_ps_02_2 <- bam(formula = formula_02, data = data_merged_rol_summer,
                            family = Gamma(link = "log"))
summary(model_summer_ps_02_2)
# model diagnosis
data_res_summer_ps_02_2_kbj <- data_obs_res_kbj(model = model_summer_ps_02_2, data_kbj = data_merged_rol_summer_kbj, catchment = "X10302")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_02_2_kbj, halfyear = "summer", catchment = "X10302",
                 title = "Munich, gamma, log link, summer (kbj)")
mse_summer_ps_02_2_kbj <- res_mse_kbj(data_obs_res_kbj = data_res_summer_ps_02_2_kbj, halfyear = "summer", catchment = "X10302")


### due to mse for low drainage values: choose gamma, link: log



# adding interactions
vars_02 <- c("rol_airtmp_X10302", "rol_glorad_X10302", "rol_groundwaterdepth_X10302", "rol_precip_X10302",
             "rol_qinfiltration_X10302", "rol_relhum_X10302", "rol_snowstorage_X10302", "rol_soilwaterrootzone_X10302", "rol_soilwaterunsatzone_X10302")
# 1. interaction
formula_chr_02 <- "drainage_X10302 ~ drainage_X10303 + drainage_X10321 + YY +
  s(rol_airtmp_X10302, bs = 'ps') + s(rol_glorad_X10302, bs = 'ps') + s(rol_groundwaterdepth_X10302, bs = 'ps') +
  s(rol_precip_X10302, bs = 'ps') + s(rol_qinfiltration_X10302, bs = 'ps') + s(rol_relhum_X10302, bs = 'ps') +
  s(rol_snowstorage_X10302, bs = 'ps') + s(rol_soilwaterrootzone_X10302, bs = 'ps') + s(rol_soilwaterunsatzone_X10302, bs = 'ps')"
find_best_interac(vars = vars_02, formula_chr = formula_chr_02, data = data_merged_rol_summer,
                  family = Gamma(link = "log"), data_kbj = data_merged_rol_summer_kbj, catchment = "X10302", halfyear = "summer")

# 2. interaction
formula_chr_02 <- "drainage_X10302 ~ drainage_X10303 + drainage_X10321 + YY +
  rol_precip_X10302:rol_snowstorage_X10302 +
  s(rol_airtmp_X10302, bs = 'ps') + s(rol_glorad_X10302, bs = 'ps') + s(rol_groundwaterdepth_X10302, bs = 'ps') +
  s(rol_precip_X10302, bs = 'ps') + s(rol_qinfiltration_X10302, bs = 'ps') + s(rol_relhum_X10302, bs = 'ps') +
  s(rol_snowstorage_X10302, bs = 'ps') + s(rol_soilwaterrootzone_X10302, bs = 'ps') + s(rol_soilwaterunsatzone_X10302, bs = 'ps')"
find_best_interac(vars = vars_02, formula_chr = formula_chr_02, data = data_merged_rol_summer,
                  family = Gamma(link = "log"), data_kbj = data_merged_rol_summer_kbj, catchment = "X10302", halfyear = "summer")


formula_chr_02 <- "drainage_X10302 ~ drainage_X10303 + drainage_X10321 + YY +
  rol_precip_X10302:rol_snowstorage_X10302 +
  rol_relhum_X10302:rol_soilwaterunsatzone_X10302  +
  s(rol_airtmp_X10302, bs = 'ps') + s(rol_glorad_X10302, bs = 'ps') + s(rol_groundwaterdepth_X10302, bs = 'ps') +
  s(rol_precip_X10302, bs = 'ps') + s(rol_qinfiltration_X10302, bs = 'ps') + s(rol_relhum_X10302, bs = 'ps') +
  s(rol_snowstorage_X10302, bs = 'ps') + s(rol_soilwaterrootzone_X10302, bs = 'ps') + s(rol_soilwaterunsatzone_X10302, bs = 'ps')"
model_summer_ps_intera_02 <- bam(formula = as.formula(formula_chr_02), data = data_merged_rol_summer,
                                 family = Gamma(link = "log"))
summary(model_summer_ps_intera_02)
data_res_summer_ps_intera_02_kbj <- data_obs_res_kbj(model = model_summer_ps_intera_02, data_kbj = data_merged_rol_summer_kbj, catchment = "X10302")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_intera_02_kbj, halfyear = "summer", catchment = "X10302",
                 title = "Munich, gamma, log link, summer (kbj) with interactions")
plot_obs_res_kbj(data_obs_res_kbj = data_res_summer_ps_intera_02_kbj, halfyear = "summer", catchment = "X10302",
                 title = "Munich, gamma, log link, summer (kbj) with interactions", ylim_l = -200, ylim_u = 200)
mse_summer_ps_intera_02_kbj <- res_mse_kbj(data_obs_res_kbj = data_res_summer_ps_intera_02_kbj, halfyear = "summer", catchment = "X10302")


plot(model_summer_ps_intera_02)
interact_plot(model = model_summer_ps_intera_02, pred = rol_precip_X10302, modx = rol_snowstorage_X10302,
              interval = TRUE, int.type = "confidence")
interact_plot(model = model_summer_ps_intera_02, pred = rol_snowstorage_X10302, modx = rol_precip_X10302,
              interval = TRUE, int.type = "confidence")
interact_plot(model = model_summer_ps_intera_02, pred = rol_relhum_X10302, modx = rol_soilwaterunsatzone_X10302,
              interval = TRUE, int.type = "confidence")
interact_plot(model = model_summer_ps_intera_02, pred = rol_soilwaterunsatzone_X10302, modx = rol_relhum_X10302,
              interval = TRUE, int.type = "confidence")
