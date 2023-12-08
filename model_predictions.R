source("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/model_chosen.R")
source("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/functions.R")

setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbo")

data_merged_rol_kbo <- readRDS("data_merged_rol_kbo.rds")
data_merged_rol_winter_kbo <- data_merged_rol_kbo[data_merged_rol_kbo$MM < 5 | data_merged_rol_kbo$MM > 10, ]
data_merged_rol_summer_kbo <- data_merged_rol_kbo[data_merged_rol_kbo$MM >= 5 & data_merged_rol_kbo$MM <= 10, ]

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


# Winter, Mittenwald
data_res_winter_ps_intera_04_kbo <- data_obs_res_kbo(model = model_winter_ps_intera_04, data_kbo = data_merged_rol_winter_kbo, catchment = "X10304")
plot_obs_pred_kbo(data_obs_res_kbo = data_res_winter_ps_intera_04_kbo, halfyear = "winter", catchment = "X10304",
                  title = "", all = TRUE, xlim_l = 0, xlim_u = 80, ylim_l = 0, ylim_u = 80)
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/predictions/pred_winter_X10304.pdf"), width = 6.5, height = 6)
mse_winter_ps_intera_04_kbo <- res_mse_kbo(data_obs_res_kbo = data_res_winter_ps_intera_04_kbo, halfyear = "winter", catchment = "X10304")
mse_all_winter_ps_intera_04_kbo <- res_mse_all_kbo(data_obs_res_kbo = data_res_winter_ps_intera_04_kbo, halfyear = "winter", catchment = "X10304")

# Winter, Schlehdorf
data_res_winter_ps_intera_21_kbo <- data_obs_res_kbo(model = model_winter_ps_intera_21, data_kbo = data_merged_rol_winter_kbo, catchment = "X10321")
plot_obs_pred_kbo(data_obs_res_kbo = data_res_winter_ps_intera_21_kbo, halfyear = "winter", catchment = "X10321",
                  title = "", all = TRUE, xlim_l = 0, xlim_u = 130, ylim_l = 0, ylim_u = 130)
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/predictions/pred_winter_X10321.pdf"), width = 6.5, height = 6)
mse_winter_ps_intera_21_kbo <- res_mse_kbo(data_obs_res_kbo = data_res_winter_ps_intera_21_kbo, halfyear = "winter", catchment = "X10321")
mse_all_winter_ps_intera_21_kbo <- res_mse_all_kbo(data_obs_res_kbo = data_res_winter_ps_intera_21_kbo, halfyear = "winter", catchment = "X10321")

# Winter, Bad Tölz
data_res_winter_ps_intera_03_kbo <- data_obs_res_kbo(model = model_winter_ps_intera_03, data_kbo = data_merged_rol_winter_kbo, catchment = "X10303")
plot_obs_pred_kbo(data_obs_res_kbo = data_res_winter_ps_intera_03_kbo, halfyear = "winter", catchment = "X10303",
                  title = "", all = TRUE, xlim_l = 0, xlim_u = 210, ylim_l = 0, ylim_u = 210)
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/predictions/pred_winter_X10303.pdf"), width = 6.5, height = 6)
mse_winter_ps_intera_03_kbo <- res_mse_kbo(data_obs_res_kbo = data_res_winter_ps_intera_03_kbo, halfyear = "winter", catchment = "X10303")
mse_all_winter_ps_intera_03_kbo <- res_mse_all_kbo(data_obs_res_kbo = data_res_winter_ps_intera_03_kbo, halfyear = "winter", catchment = "X10303")

# Winter, Munich
data_res_winter_ps_intera_02_kbo <- data_obs_res_kbo(model = model_winter_ps_intera_02, data_kbo = data_merged_rol_winter_kbo, catchment = "X10302")
plot_obs_pred_kbo(data_obs_res_kbo = data_res_winter_ps_intera_02_kbo, halfyear = "winter", catchment = "X10302",
                  title = "", all = TRUE, xlim_l = 0, xlim_u = 940, ylim_l = 0, ylim_u = 940)
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/predictions/pred_winter_X10302.pdf"), width = 6.5, height = 6)
plot_obs_pred_kbo(data_obs_res_kbo = data_res_winter_ps_intera_02_kbo, halfyear = "winter", catchment = "X10302",
                  title = "", all = TRUE, xlim_l = 0, xlim_u = 450, ylim_l = 0, ylim_u = 450)
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/predictions/pred_winter_X10302_cut.pdf"), width = 6.5, height = 6)
mse_winter_ps_intera_02_kbo <- res_mse_kbo(data_obs_res_kbo = data_res_winter_ps_intera_02_kbo, halfyear = "winter", catchment = "X10302")
mse_all_winter_ps_intera_02_kbo <- res_mse_all_kbo(data_obs_res_kbo = data_res_winter_ps_intera_02_kbo, halfyear = "winter", catchment = "X10302")



# Summer, Mittenwald
data_res_summer_ps_intera_04_kbo <- data_obs_res_kbo(model = model_summer_ps_intera_04, data_kbo = data_merged_rol_summer_kbo, catchment = "X10304")
plot_obs_pred_kbo(data_obs_res_kbo = data_res_summer_ps_intera_04_kbo, halfyear = "summer", catchment = "X10304",
                  title = "", all = TRUE, xlim_l = 0,xlim_u = 180, ylim_l = 0, ylim_u = 180)
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/predictions/pred_summer_X10304.pdf"), width = 6.5, height = 6)
mse_summer_ps_intera_04_kbo <- res_mse_kbo(data_obs_res_kbo = data_res_summer_ps_intera_04_kbo, halfyear = "summer", catchment = "X10304")
mse_all_summer_ps_intera_04_kbo <- res_mse_all_kbo(data_obs_res_kbo = data_res_summer_ps_intera_04_kbo, halfyear = "summer", catchment = "X10304")

# Summer, Schlehdorf
data_res_summer_ps_intera_21_kbo <- data_obs_res_kbo(model = model_summer_ps_intera_21, data_kbo = data_merged_rol_summer_kbo, catchment = "X10321")
plot_obs_pred_kbo(data_obs_res_kbo = data_res_summer_ps_intera_21_kbo, halfyear = "summer", catchment = "X10321",
                  title = "", all = TRUE, xlim_l = 0, xlim_u = 1100, ylim_l = 0, ylim_u = 1100)
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/predictions/pred_summer_X10321.pdf"), width = 6.5, height = 6)
plot_obs_pred_kbo(data_obs_res_kbo = data_res_summer_ps_intera_21_kbo, halfyear = "summer", catchment = "X10321",
                  title = "", all = TRUE, xlim_l = 0, xlim_u = 280, ylim_l = 0, ylim_u = 280)
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/predictions/pred_summer_X10321_cut.pdf"), width = 6.5, height = 6)
mse_summer_ps_intera_21_kbo <- res_mse_kbo(data_obs_res_kbo = data_res_summer_ps_intera_21_kbo, halfyear = "summer", catchment = "X10321")
mse_all_summer_ps_intera_21_kbo <- res_mse_all_kbo(data_obs_res_kbo = data_res_summer_ps_intera_21_kbo, halfyear = "summer", catchment = "X10321")

# Summer, Bad Tölz
data_res_summer_ps_intera_03_kbo <- data_obs_res_kbo(model = model_summer_ps_intera_03, data_kbo = data_merged_rol_summer_kbo, catchment = "X10303")
plot_obs_pred_kbo(data_obs_res_kbo = data_res_summer_ps_intera_03_kbo, halfyear = "summer", catchment = "X10303",
                  title = "", all = TRUE, xlim_l = 0, xlim_u = 430, ylim_l = 0, ylim_u = 3150)
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/predictions/pred_summer_X10303.pdf"), width = 6.5, height = 6)
plot_obs_pred_kbo(data_obs_res_kbo = data_res_summer_ps_intera_03_kbo, halfyear = "summer", catchment = "X10303",
                  title = "", all = TRUE, xlim_l = 0, xlim_u = 400, ylim_l = 0, ylim_u = 400)
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/predictions/pred_summer_X10303_cut.pdf"), width = 6.5, height = 6)
mse_summer_ps_intera_03_kbo <- res_mse_kbo(data_obs_res_kbo = data_res_summer_ps_intera_03_kbo, halfyear = "summer", catchment = "X10303")
mse_all_summer_ps_intera_03_kbo <- res_mse_all_kbo(data_obs_res_kbo = data_res_summer_ps_intera_03_kbo, halfyear = "summer", catchment = "X10303")

# Summer, Munich
data_res_summer_ps_intera_02_kbo <- data_obs_res_kbo(model = model_summer_ps_intera_02, data_kbo = data_merged_rol_summer_kbo, catchment = "X10302")
plot_obs_pred_kbo(data_obs_res_kbo = data_res_summer_ps_intera_02_kbo, halfyear = "summer", catchment = "X10302",
                  title = "", all = TRUE, xlim_l = 0, xlim_u = 800, ylim_l = 0, ylim_u = 2000)
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/predictions/pred_summer_X10302.pdf"), width = 6.5, height = 6)
plot_obs_pred_kbo(data_obs_res_kbo = data_res_summer_ps_intera_02_kbo, halfyear = "summer", catchment = "X10302",
                  title = "", all = TRUE, xlim_l = 0, xlim_u = 800, ylim_l = 0, ylim_u = 800)
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/predictions/pred_summer_X10302_cut.pdf"), width = 6.5, height = 6)
mse_summer_ps_intera_02_kbo <- res_mse_kbo(data_obs_res_kbo = data_res_summer_ps_intera_02_kbo, halfyear = "summer", catchment = "X10302")
mse_all_summer_ps_intera_02_kbo <- res_mse_all_kbo(data_obs_res_kbo = data_res_summer_ps_intera_02_kbo, halfyear = "summer", catchment = "X10302")

