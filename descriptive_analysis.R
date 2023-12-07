setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbe")

# load packages
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(raster)
source("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/modify_data_descriptive_analysis.R")


#### univariate description
### tables
vars <- c("drainage", "rol_airtmp", "rol_glorad", "rol_groundwaterdepth", "rol_precip",
          "rol_qinfiltration", "rol_relhum", "rol_snowstorage", "rol_soilwaterrootzone", "rol_soilwaterunsatzone")
for(i in vars) {
  assign(paste0("descr_winter_", i), data.frame(first_col = rep(NA, 4)))
  assign(paste0("descr_summer_", i), data.frame(first_col = rep(NA, 4)))
}

add_descr_cols_winter <- function(data, var) {
  rownames(data) <- c("X10304", "X10321", "X10303", "X10302")
  data_tmp <- cbind(data, min = unlist(lapply(data_merged_rol_winter[, c(paste0(var, "_X10304"), paste0(var, "_X10321"), paste0(var, "_X10303"), paste0(var, "_X10302"))], min, na.rm = TRUE)))
  data_tmp <- cbind(data_tmp, mean = unlist(lapply(data_merged_rol_winter[, c(paste0(var, "_X10304"), paste0(var, "_X10321"), paste0(var, "_X10303"), paste0(var, "_X10302"))], mean, na.rm = TRUE)))
  data_tmp <- cbind(data_tmp, median = unlist(lapply(data_merged_rol_winter[, c(paste0(var, "_X10304"), paste0(var, "_X10321"), paste0(var, "_X10303"), paste0(var, "_X10302"))], median, na.rm = TRUE)))
  data_tmp <- cbind(data_tmp, max = unlist(lapply(data_merged_rol_winter[, c(paste0(var, "_X10304"), paste0(var, "_X10321"), paste0(var, "_X10303"), paste0(var, "_X10302"))], max, na.rm = TRUE)))
  data_tmp <- cbind(data_tmp, sd = unlist(lapply(data_merged_rol_winter[, c(paste0(var, "_X10304"), paste0(var, "_X10321"), paste0(var, "_X10303"), paste0(var, "_X10302"))], sd, na.rm = TRUE)))
  print(data_tmp[, -1])
}

add_descr_cols_summer <- function(data, var) {
  rownames(data) <- c("X10304", "X10321", "X10303", "X10302")
  data_tmp <- cbind(data, min = unlist(lapply(data_merged_rol_summer[, c(paste0(var, "_X10304"), paste0(var, "_X10321"), paste0(var, "_X10303"), paste0(var, "_X10302"))], min, na.rm = TRUE)))
  data_tmp <- cbind(data_tmp, mean = unlist(lapply(data_merged_rol_summer[, c(paste0(var, "_X10304"), paste0(var, "_X10321"), paste0(var, "_X10303"), paste0(var, "_X10302"))], mean, na.rm = TRUE)))
  data_tmp <- cbind(data_tmp, median = unlist(lapply(data_merged_rol_summer[, c(paste0(var, "_X10304"), paste0(var, "_X10321"), paste0(var, "_X10303"), paste0(var, "_X10302"))], median, na.rm = TRUE)))
  data_tmp <- cbind(data_tmp, max = unlist(lapply(data_merged_rol_summer[, c(paste0(var, "_X10304"), paste0(var, "_X10321"), paste0(var, "_X10303"), paste0(var, "_X10302"))], max, na.rm = TRUE)))
  data_tmp <- cbind(data_tmp, sd = unlist(lapply(data_merged_rol_summer[, c(paste0(var, "_X10304"), paste0(var, "_X10321"), paste0(var, "_X10303"), paste0(var, "_X10302"))], sd, na.rm = TRUE)))
  print(data_tmp[, -1])
}

descr_winter_drainage <- add_descr_cols_winter(descr_winter_drainage, "drainage")
descr_winter_rol_airtmp <- add_descr_cols_winter(descr_winter_rol_airtmp, "rol_airtmp")
descr_winter_rol_glorad <- add_descr_cols_winter(descr_winter_rol_glorad, "rol_glorad")
descr_winter_rol_groundwaterdepth <- add_descr_cols_winter(descr_winter_rol_groundwaterdepth, "rol_groundwaterdepth")
descr_winter_rol_precip <- add_descr_cols_winter(descr_winter_rol_precip, "rol_precip")
descr_winter_rol_qinfiltration <- add_descr_cols_winter(descr_winter_rol_qinfiltration, "rol_qinfiltration")
descr_winter_rol_relhum <- add_descr_cols_winter(descr_winter_rol_relhum, "rol_relhum")
descr_winter_rol_snowstorage <- add_descr_cols_winter(descr_winter_rol_snowstorage, "rol_snowstorage")
descr_winter_rol_soilwaterrootzone <- add_descr_cols_winter(descr_winter_rol_soilwaterrootzone, "rol_soilwaterrootzone")
descr_winter_rol_soilwaterunsatzone <- add_descr_cols_winter(descr_winter_rol_soilwaterunsatzone, "rol_soilwaterunsatzone")

descr_summer_drainage <- add_descr_cols_summer(descr_summer_drainage, "drainage")
descr_summer_rol_airtmp <- add_descr_cols_summer(descr_summer_rol_airtmp, "rol_airtmp")
descr_summer_rol_glorad <- add_descr_cols_summer(descr_summer_rol_glorad, "rol_glorad")
descr_summer_rol_groundwaterdepth <- add_descr_cols_summer(descr_summer_rol_groundwaterdepth, "rol_groundwaterdepth")
descr_summer_rol_precip <- add_descr_cols_summer(descr_summer_rol_precip, "rol_precip")
descr_summer_rol_qinfiltration <- add_descr_cols_summer(descr_summer_rol_qinfiltration, "rol_qinfiltration")
descr_summer_rol_relhum <- add_descr_cols_summer(descr_summer_rol_relhum, "rol_relhum")
descr_summer_rol_snowstorage <- add_descr_cols_summer(descr_summer_rol_snowstorage, "rol_snowstorage")
descr_summer_rol_soilwaterrootzone <- add_descr_cols_summer(descr_summer_rol_soilwaterrootzone, "rol_soilwaterrootzone")
descr_summer_rol_soilwaterunsatzone <- add_descr_cols_summer(descr_summer_rol_soilwaterunsatzone, "rol_soilwaterunsatzone")


### plots
# min drainage
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = min_drainage, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = min_drainage, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "minimum drainage",
       x = "year") +
  theme_bw() +
  ylim(0, 165) +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/drainage_min.pdf"), width = 17, height = 9)

# mean drainage
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = mean_drainage, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = mean_drainage, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "mean drainage",
       x = "year") +
  theme_bw() +
  ylim(0, 165) +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/drainage_mean.pdf"), width = 17, height = 9)

# max drainage
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = max_drainage, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = max_drainage, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "maximum drainage",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/drainage_max.pdf"), width = 17, height = 9)

# min airtmp
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = min_airtmp, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = min_airtmp, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "minimum airtmp",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/airtmp_min.pdf"), width = 17, height = 9)

# mean airtmp
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = mean_airtmp, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = mean_airtmp, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "mean airtmp",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/airtmp_mean.pdf"), width = 17, height = 9)

# max airtmp
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = max_airtmp, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = max_airtmp, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "maximum airtmp",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/airtmp_max.pdf"), width = 17, height = 9)


# min glorad
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = min_glorad, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = min_glorad, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "minimum glorad",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/glorad_min.pdf"), width = 17, height = 9)

# mean glorad
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = mean_glorad, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = mean_glorad, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "mean glorad",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/glorad_mean.pdf"), width = 17, height = 9)

# max glorad
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = max_glorad, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = max_glorad, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "maximum glorad",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/glorad_max.pdf"), width = 17, height = 9)


# min groundwaterdepth
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = min_groundwaterdepth, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = min_groundwaterdepth, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "minimum grdepth",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/groundwaterdepth_min.pdf"), width = 17, height = 9)

# mean groundwaterdepth
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = mean_groundwaterdepth, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = mean_groundwaterdepth, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "mean grdepth",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/groundwaterdepth_mean.pdf"), width = 17, height = 9)

# max groundwaterdepth
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = max_groundwaterdepth, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = max_groundwaterdepth, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "maximum grdepth",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/groundwaterdepth_max.pdf"), width = 17, height = 9)


# min precip
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = min_precip, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = min_precip, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "minimum precip",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/precip_min.pdf"), width = 17, height = 9)

# mean precip
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = mean_precip, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = mean_precip, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "mean precip",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/precip_mean.pdf"), width = 17, height = 9)

# max precip
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = max_precip, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = max_precip, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "maximum precip",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/precip_max.pdf"), width = 17, height = 9)


# min qinfiltration
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = min_qinfiltration, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = min_qinfiltration, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "minimum qinfil",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/qinfiltration_min.pdf"), width = 17, height = 9)

# mean qinfiltration
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = mean_qinfiltration, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = mean_qinfiltration, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "mean qinfil",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/qinfiltration_mean.pdf"), width = 17, height = 9)

# max qinfiltration
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = max_qinfiltration, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = max_qinfiltration, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "maximum qinfil",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/qinfiltration_max.pdf"), width = 17, height = 9)


# min relhum
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = min_relhum, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = min_relhum, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "minimum relhum",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/relhum_min.pdf"), width = 17, height = 9)

# mean relhum
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = mean_relhum, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = mean_relhum, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "mean relhum",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/relhum_mean.pdf"), width = 17, height = 9)

# max relhum
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = max_relhum, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = max_relhum, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "maximum relhum",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/relhum_max.pdf"), width = 17, height = 9)


# min snowstorage
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = min_snowstorage, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = min_snowstorage, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "minimum snow",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/snowstorage_min.pdf"), width = 17, height = 9)

# mean snowstorage
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = mean_snowstorage, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = mean_snowstorage, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "mean snow",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/snowstorage_mean.pdf"), width = 17, height = 9)

# max snowstorage
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = max_snowstorage, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = max_snowstorage, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "maximum snow",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/snowstorage_max.pdf"), width = 17, height = 9)


# min soilwaterrootzone
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = min_soilwaterrootzone, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = min_soilwaterrootzone, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "minimum soilroot",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/soilwaterrootzone_min.pdf"), width = 17, height = 9)

# mean soilwaterrootzone
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = mean_soilwaterrootzone, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = mean_soilwaterrootzone, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "mean soilroot",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/soilwaterrootzone_mean.pdf"), width = 17, height = 9)

# max soilwaterrootzone
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = max_soilwaterrootzone, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = max_soilwaterrootzone, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "maximum soilroot",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/soilwaterrootzone_max.pdf"), width = 17, height = 9)


# min soilwaterunsatzone
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = min_soilwaterunsatzone, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = min_soilwaterunsatzone, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "minimum soilunsat",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/soilwaterunsatzone_min.pdf"), width = 17, height = 9)

# mean soilwaterunsatzone
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = mean_soilwaterunsatzone, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = mean_soilwaterunsatzone, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "mean soilunsat",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/soilwaterunsatzone_mean.pdf"), width = 17, height = 9)

# max soilwaterunsatzone
ggplot(data = data_table) +
  geom_line(mapping = aes(x = YY, y = max_soilwaterunsatzone, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  geom_point(mapping = aes(x = YY, y = max_soilwaterunsatzone, color = factor(catchment, levels = c("X10304", "X10321", "X10303", "X10302")))) +
  labs(y = "maximum soilunsat",
       x = "year") +
  theme_bw() +
  theme(text = element_text(size = 30),
        panel.spacing = unit(2, "lines")) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  facet_wrap(~factor(halfyear, levels = c("winter", "summer")))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/univ/soilwaterunsatzone_max.pdf"), width = 17, height = 9)





#### bivariate
### copulas
library(copula)
library(rvinecopulib)
vars_04 <- c("rol_airtmp_X10304", "rol_glorad_X10304", "rol_groundwaterdepth_X10304", "rol_precip_X10304",
             "rol_qinfiltration_X10304", "rol_relhum_X10304", "rol_snowstorage_X10304", "rol_soilwaterrootzone_X10304", "rol_soilwaterunsatzone_X10304")
vars_21 <- c("rol_airtmp_X10321", "rol_glorad_X10321", "rol_groundwaterdepth_X10321", "rol_precip_X10321",
             "rol_qinfiltration_X10321", "rol_relhum_X10321", "rol_snowstorage_X10321", "rol_soilwaterrootzone_X10321", "rol_soilwaterunsatzone_X10321")
vars_03 <- c("rol_airtmp_X10303", "rol_glorad_X10303", "rol_groundwaterdepth_X10303", "rol_precip_X10303",
             "rol_qinfiltration_X10303", "rol_relhum_X10303", "rol_snowstorage_X10303", "rol_soilwaterrootzone_X10303", "rol_soilwaterunsatzone_X10303")
vars_02 <- c("rol_airtmp_X10302", "rol_glorad_X10302", "rol_groundwaterdepth_X10302", "rol_precip_X10302",
             "rol_qinfiltration_X10302", "rol_relhum_X10302", "rol_snowstorage_X10302", "rol_soilwaterrootzone_X10302", "rol_soilwaterunsatzone_X10302")

for (i in vars_04) {
  assign(paste0("copula_", i, "_winter"), bicop(data = pobs(data_merged_rol_winter[, c(i, "drainage_X10304")]), family_set = "archimedean"))
}
for (i in vars_21) {
  assign(paste0("copula_", i, "_winter"), bicop(data = pobs(data_merged_rol_winter[, c(i, "drainage_X10321")]), family_set = "archimedean"))
}
for (i in vars_03) {
  assign(paste0("copula_", i, "_winter"), bicop(data = pobs(data_merged_rol_winter[, c(i, "drainage_X10303")]), family_set = "archimedean"))
}
for (i in vars_02) {
  assign(paste0("copula_", i, "_winter"), bicop(data = pobs(data_merged_rol_winter[, c(i, "drainage_X10302")]), family_set = "archimedean"))
}

for (i in vars_04) {
  assign(paste0("copula_", i, "_summer"), bicop(data = pobs(data_merged_rol_summer[, c(i, "drainage_X10304")]), family_set = "archimedean"))
}
for (i in vars_21) {
  assign(paste0("copula_", i, "_summer"), bicop(data = pobs(data_merged_rol_summer[, c(i, "drainage_X10321")]), family_set = "archimedean"))
}
for (i in vars_03) {
  assign(paste0("copula_", i, "_summer"), bicop(data = pobs(data_merged_rol_summer[, c(i, "drainage_X10303")]), family_set = "archimedean"))
}
for (i in vars_02) {
  assign(paste0("copula_", i, "_summer"), bicop(data = pobs(data_merged_rol_summer[, c(i, "drainage_X10302")]), family_set = "archimedean"))
}

# summary and plots
# winter, X10304
summary(copula_rol_airtmp_X10304_winter)
get_ktau(copula_rol_airtmp_X10304_winter)
summary(copula_rol_glorad_X10304_winter)
get_ktau(copula_rol_glorad_X10304_winter)
summary(copula_rol_groundwaterdepth_X10304_winter)
get_ktau(copula_rol_groundwaterdepth_X10304_winter)
summary(copula_rol_precip_X10304_winter)
get_ktau(copula_rol_precip_X10304_winter)
summary(copula_rol_qinfiltration_X10304_winter)
get_ktau(copula_rol_qinfiltration_X10304_winter)
summary(copula_rol_relhum_X10304_winter)
get_ktau(copula_rol_relhum_X10304_winter)
summary(copula_rol_snowstorage_X10304_winter)
get_ktau(copula_rol_snowstorage_X10304_winter)
summary(copula_rol_soilwaterrootzone_X10304_winter)
get_ktau(copula_rol_soilwaterrootzone_X10304_winter)
summary(copula_rol_soilwaterunsatzone_X10304_winter)
get_ktau(copula_rol_soilwaterunsatzone_X10304_winter)

contour(copula_rol_airtmp_X10304_winter, xlab = "airtmp_MW", ylab = "drainage_MW")
contour(copula_rol_glorad_X10304_winter, xlab = "glorad_MW", ylab = "drainage_MW")
contour(copula_rol_groundwaterdepth_X10304_winter, xlab = "grdepth_MW", ylab = "drainage_MW")
contour(copula_rol_precip_X10304_winter, xlab = "qinfiltration_MW", ylab = "drainage_MW")
contour(copula_rol_qinfiltration_X10304_winter, xlab = "qinfiltration_MW", ylab = "drainage_MW")
contour(copula_rol_relhum_X10304_winter, xlab = "relhum_MW", ylab = "drainage_MW")
contour(copula_rol_snowstorage_X10304_winter, xlab = "snowstorage_MW", ylab = "drainage_MW")
contour(copula_rol_soilwaterrootzone_X10304_winter, xlab = "soilwaterrootzone_MW", ylab = "drainage_MW")
contour(copula_rol_soilwaterunsatzone_X10304_winter, xlab = "soilunsat_MW", ylab = "drainage_MW")


# winter, X10321
summary(copula_rol_airtmp_X10321_winter)
get_ktau(copula_rol_airtmp_X10321_winter)
summary(copula_rol_glorad_X10321_winter)
get_ktau(copula_rol_glorad_X10321_winter)
summary(copula_rol_groundwaterdepth_X10321_winter)
get_ktau(copula_rol_groundwaterdepth_X10321_winter)
summary(copula_rol_precip_X10321_winter)
get_ktau(copula_rol_precip_X10321_winter)
summary(copula_rol_qinfiltration_X10321_winter)
get_ktau(copula_rol_qinfiltration_X10321_winter)
summary(copula_rol_relhum_X10321_winter)
get_ktau(copula_rol_relhum_X10321_winter)
summary(copula_rol_snowstorage_X10321_winter)
get_ktau(copula_rol_snowstorage_X10321_winter)
summary(copula_rol_soilwaterrootzone_X10321_winter)
get_ktau(copula_rol_soilwaterrootzone_X10321_winter)
summary(copula_rol_soilwaterunsatzone_X10321_winter)
get_ktau(copula_rol_soilwaterunsatzone_X10321_winter)

contour(copula_rol_airtmp_X10321_winter, xlab = "airtmp_SD", ylab = "drainage_SD")
contour(copula_rol_glorad_X10321_winter, xlab = "glorad_SD", ylab = "drainage_SD")
contour(copula_rol_groundwaterdepth_X10321_winter, xlab = "grdepth_SD", ylab = "drainage_SD")
contour(copula_rol_precip_X10321_winter, xlab = "qinfiltration_SD", ylab = "drainage_SD")
contour(copula_rol_qinfiltration_X10321_winter, xlab = "qinfiltration_SD", ylab = "drainage_SD")
contour(copula_rol_relhum_X10321_winter, xlab = "relhum_SD", ylab = "drainage_SD")
contour(copula_rol_snowstorage_X10321_winter, xlab = "snowstorage_SD", ylab = "drainage_SD")
contour(copula_rol_soilwaterrootzone_X10321_winter, xlab = "soilwaterrootzone_SD", ylab = "drainage_SD")
contour(copula_rol_soilwaterunsatzone_X10321_winter, xlab = "soilunsat_SD", ylab = "drainage_SD")

# winter, X10303
summary(copula_rol_airtmp_X10303_winter)
get_ktau(copula_rol_airtmp_X10303_winter)
summary(copula_rol_glorad_X10303_winter)
get_ktau(copula_rol_glorad_X10303_winter)
summary(copula_rol_groundwaterdepth_X10303_winter)
get_ktau(copula_rol_groundwaterdepth_X10303_winter)
summary(copula_rol_precip_X10303_winter)
get_ktau(copula_rol_precip_X10303_winter)
summary(copula_rol_qinfiltration_X10303_winter)
get_ktau(copula_rol_qinfiltration_X10303_winter)
summary(copula_rol_relhum_X10303_winter)
get_ktau(copula_rol_relhum_X10303_winter)
summary(copula_rol_snowstorage_X10303_winter)
get_ktau(copula_rol_snowstorage_X10303_winter)
summary(copula_rol_soilwaterrootzone_X10303_winter)
get_ktau(copula_rol_soilwaterrootzone_X10303_winter)
summary(copula_rol_soilwaterunsatzone_X10303_winter)
get_ktau(copula_rol_soilwaterunsatzone_X10303_winter)

contour(copula_rol_airtmp_X10303_winter, xlab = "airtmp_BT", ylab = "drainage_BT")
contour(copula_rol_glorad_X10303_winter, xlab = "glorad_BT", ylab = "drainage_BT")
contour(copula_rol_groundwaterdepth_X10303_winter, xlab = "grdepth_BT", ylab = "drainage_BT")
contour(copula_rol_precip_X10303_winter, xlab = "qinfiltration_BT", ylab = "drainage_BT")
contour(copula_rol_qinfiltration_X10303_winter, xlab = "qinfiltration_BT", ylab = "drainage_BT")
contour(copula_rol_relhum_X10303_winter, xlab = "relhum_BT", ylab = "drainage_BT")
contour(copula_rol_snowstorage_X10303_winter, xlab = "snowstorage_BT", ylab = "drainage_BT")
contour(copula_rol_soilwaterrootzone_X10303_winter, xlab = "soilwaterrootzone_BT", ylab = "drainage_BT")
contour(copula_rol_soilwaterunsatzone_X10303_winter, xlab = "soilunsat_BT", ylab = "drainage_BT")


# winter, X10302
summary(copula_rol_airtmp_X10302_winter)
get_ktau(copula_rol_airtmp_X10302_winter)
summary(copula_rol_glorad_X10302_winter)
get_ktau(copula_rol_glorad_X10302_winter)
summary(copula_rol_groundwaterdepth_X10302_winter)
get_ktau(copula_rol_groundwaterdepth_X10302_winter)
summary(copula_rol_precip_X10302_winter)
get_ktau(copula_rol_precip_X10302_winter)
summary(copula_rol_qinfiltration_X10302_winter)
get_ktau(copula_rol_qinfiltration_X10302_winter)
summary(copula_rol_relhum_X10302_winter)
get_ktau(copula_rol_relhum_X10302_winter)
summary(copula_rol_snowstorage_X10302_winter)
get_ktau(copula_rol_snowstorage_X10302_winter)
summary(copula_rol_soilwaterrootzone_X10302_winter)
get_ktau(copula_rol_soilwaterrootzone_X10302_winter)
summary(copula_rol_soilwaterunsatzone_X10302_winter)
get_ktau(copula_rol_soilwaterunsatzone_X10302_winter)

contour(copula_rol_airtmp_X10302_winter, xlab = "airtmp_MU", ylab = "drainage_MU")
contour(copula_rol_glorad_X10302_winter, xlab = "glorad_MU", ylab = "drainage_MU")
contour(copula_rol_groundwaterdepth_X10302_winter, xlab = "grdepth_MU", ylab = "drainage_MU")
contour(copula_rol_precip_X10302_winter, xlab = "qinfiltration_MU", ylab = "drainage_MU")
contour(copula_rol_qinfiltration_X10302_winter, xlab = "qinfiltration_MU", ylab = "drainage_MU")
contour(copula_rol_relhum_X10302_winter, xlab = "relhum_MU", ylab = "drainage_MU")
contour(copula_rol_snowstorage_X10302_winter, xlab = "snowstorage_MU", ylab = "drainage_MU")
contour(copula_rol_soilwaterrootzone_X10302_winter, xlab = "soilwaterrootzone_MU", ylab = "drainage_MU")
contour(copula_rol_soilwaterunsatzone_X10302_winter, xlab = "soilunsat_MU", ylab = "drainage_MU")


# summer, X10304
summary(copula_rol_airtmp_X10304_summer)
get_ktau(copula_rol_airtmp_X10304_summer)
summary(copula_rol_glorad_X10304_summer)
get_ktau(copula_rol_glorad_X10304_summer)
summary(copula_rol_groundwaterdepth_X10304_summer)
get_ktau(copula_rol_groundwaterdepth_X10304_summer)
summary(copula_rol_precip_X10304_summer)
get_ktau(copula_rol_precip_X10304_summer)
summary(copula_rol_qinfiltration_X10304_summer)
get_ktau(copula_rol_qinfiltration_X10304_summer)
summary(copula_rol_relhum_X10304_summer)
get_ktau(copula_rol_relhum_X10304_summer)
summary(copula_rol_snowstorage_X10304_summer)
get_ktau(copula_rol_snowstorage_X10304_summer)
summary(copula_rol_soilwaterrootzone_X10304_summer)
get_ktau(copula_rol_soilwaterrootzone_X10304_summer)
summary(copula_rol_soilwaterunsatzone_X10304_summer)
get_ktau(copula_rol_soilwaterunsatzone_X10304_summer)

contour(copula_rol_airtmp_X10304_summer, xlab = "airtmp_MW", ylab = "drainage_MW")
contour(copula_rol_glorad_X10304_summer, xlab = "glorad_MW", ylab = "drainage_MW")
contour(copula_rol_groundwaterdepth_X10304_summer, xlab = "grdepth_MW", ylab = "drainage_MW")
contour(copula_rol_precip_X10304_summer, xlab = "qinfiltration_MW", ylab = "drainage_MW")
contour(copula_rol_qinfiltration_X10304_summer, xlab = "qinfiltration_MW", ylab = "drainage_MW")
contour(copula_rol_relhum_X10304_summer, xlab = "relhum_MW", ylab = "drainage_MW")
contour(copula_rol_snowstorage_X10304_summer, xlab = "snowstorage_MW", ylab = "drainage_MW")
contour(copula_rol_soilwaterrootzone_X10304_summer, xlab = "soilwaterrootzone_MW", ylab = "drainage_MW")
contour(copula_rol_soilwaterunsatzone_X10304_summer, xlab = "soilunsat_MW", ylab = "drainage_MW")


# summer, X10321
summary(copula_rol_airtmp_X10321_summer)
get_ktau(copula_rol_airtmp_X10321_summer)
summary(copula_rol_glorad_X10321_summer)
get_ktau(copula_rol_glorad_X10321_summer)
summary(copula_rol_groundwaterdepth_X10321_summer)
get_ktau(copula_rol_groundwaterdepth_X10321_summer)
summary(copula_rol_precip_X10321_summer)
get_ktau(copula_rol_precip_X10321_summer)
summary(copula_rol_qinfiltration_X10321_summer)
get_ktau(copula_rol_qinfiltration_X10321_summer)
summary(copula_rol_relhum_X10321_summer)
get_ktau(copula_rol_relhum_X10321_summer)
summary(copula_rol_snowstorage_X10321_summer)
get_ktau(copula_rol_snowstorage_X10321_summer)
summary(copula_rol_soilwaterrootzone_X10321_summer)
get_ktau(copula_rol_soilwaterrootzone_X10321_summer)
summary(copula_rol_soilwaterunsatzone_X10321_summer)
get_ktau(copula_rol_soilwaterunsatzone_X10321_summer)

contour(copula_rol_airtmp_X10321_summer, xlab = "airtmp_SD", ylab = "drainage_SD")
contour(copula_rol_glorad_X10321_summer, xlab = "glorad_SD", ylab = "drainage_SD")
contour(copula_rol_groundwaterdepth_X10321_summer, xlab = "grdepth_SD", ylab = "drainage_SD")
contour(copula_rol_precip_X10321_summer, xlab = "qinfiltration_SD", ylab = "drainage_SD")
contour(copula_rol_qinfiltration_X10321_summer, xlab = "qinfiltration_SD", ylab = "drainage_SD")
contour(copula_rol_relhum_X10321_summer, xlab = "relhum_SD", ylab = "drainage_SD")
contour(copula_rol_snowstorage_X10321_summer, xlab = "snowstorage_SD", ylab = "drainage_SD")
contour(copula_rol_soilwaterrootzone_X10321_summer, xlab = "soilwaterrootzone_SD", ylab = "drainage_SD")
contour(copula_rol_soilwaterunsatzone_X10321_summer, xlab = "soilunsat_SD", ylab = "drainage_SD")

# summer, X10303
summary(copula_rol_airtmp_X10303_summer)
get_ktau(copula_rol_airtmp_X10303_summer)
summary(copula_rol_glorad_X10303_summer)
get_ktau(copula_rol_glorad_X10303_summer)
summary(copula_rol_groundwaterdepth_X10303_summer)
get_ktau(copula_rol_groundwaterdepth_X10303_summer)
summary(copula_rol_precip_X10303_summer)
get_ktau(copula_rol_precip_X10303_summer)
summary(copula_rol_qinfiltration_X10303_summer)
get_ktau(copula_rol_qinfiltration_X10303_summer)
summary(copula_rol_relhum_X10303_summer)
get_ktau(copula_rol_relhum_X10303_summer)
summary(copula_rol_snowstorage_X10303_summer)
get_ktau(copula_rol_snowstorage_X10303_summer)
summary(copula_rol_soilwaterrootzone_X10303_summer)
get_ktau(copula_rol_soilwaterrootzone_X10303_summer)
summary(copula_rol_soilwaterunsatzone_X10303_summer)
get_ktau(copula_rol_soilwaterunsatzone_X10303_summer)

contour(copula_rol_airtmp_X10303_summer, xlab = "airtmp_BT", ylab = "drainage_BT")
contour(copula_rol_glorad_X10303_summer, xlab = "glorad_BT", ylab = "drainage_BT")
contour(copula_rol_groundwaterdepth_X10303_summer, xlab = "grdepth_BT", ylab = "drainage_BT")
contour(copula_rol_precip_X10303_summer, xlab = "qinfiltration_BT", ylab = "drainage_BT")
contour(copula_rol_qinfiltration_X10303_summer, xlab = "qinfiltration_BT", ylab = "drainage_BT")
contour(copula_rol_relhum_X10303_summer, xlab = "relhum_BT", ylab = "drainage_BT")
contour(copula_rol_snowstorage_X10303_summer, xlab = "snowstorage_BT", ylab = "drainage_BT")
contour(copula_rol_soilwaterrootzone_X10303_summer, xlab = "soilwaterrootzone_BT", ylab = "drainage_BT")
contour(copula_rol_soilwaterunsatzone_X10303_summer, xlab = "soilunsat_BT", ylab = "drainage_BT")


# summer, X10302
summary(copula_rol_airtmp_X10302_summer)
get_ktau(copula_rol_airtmp_X10302_summer)
summary(copula_rol_glorad_X10302_summer)
get_ktau(copula_rol_glorad_X10302_summer)
summary(copula_rol_groundwaterdepth_X10302_summer)
get_ktau(copula_rol_groundwaterdepth_X10302_summer)
summary(copula_rol_precip_X10302_summer)
get_ktau(copula_rol_precip_X10302_summer)
summary(copula_rol_qinfiltration_X10302_summer)
get_ktau(copula_rol_qinfiltration_X10302_summer)
summary(copula_rol_relhum_X10302_summer)
get_ktau(copula_rol_relhum_X10302_summer)
summary(copula_rol_snowstorage_X10302_summer)
get_ktau(copula_rol_snowstorage_X10302_summer)
summary(copula_rol_soilwaterrootzone_X10302_summer)
get_ktau(copula_rol_soilwaterrootzone_X10302_summer)
summary(copula_rol_soilwaterunsatzone_X10302_summer)
get_ktau(copula_rol_soilwaterunsatzone_X10302_summer)

contour(copula_rol_airtmp_X10302_summer, xlab = "airtmp_MU", ylab = "drainage_MU")
contour(copula_rol_glorad_X10302_summer, xlab = "glorad_MU", ylab = "drainage_MU")
contour(copula_rol_groundwaterdepth_X10302_summer, xlab = "grdepth_MU", ylab = "drainage_MU")
contour(copula_rol_precip_X10302_summer, xlab = "qinfiltration_MU", ylab = "drainage_MU")
contour(copula_rol_qinfiltration_X10302_summer, xlab = "qinfiltration_MU", ylab = "drainage_MU")
contour(copula_rol_relhum_X10302_summer, xlab = "relhum_MU", ylab = "drainage_MU")
contour(copula_rol_snowstorage_X10302_summer, xlab = "snowstorage_MU", ylab = "drainage_MU")
contour(copula_rol_soilwaterrootzone_X10302_summer, xlab = "soilwaterrootzone_MU", ylab = "drainage_MU")
contour(copula_rol_soilwaterunsatzone_X10302_summer, xlab = "soilunsat_MU", ylab = "drainage_MU")







### empirical tail dependence
## lower tail dependence
lower_tail_depend <- function(data = data_merged_rol_winter, var = "rol_airtmp_X10304", catchment = "X10304",
                              xlab = "", ylab = "", quant = 0.1, plot = FALSE) {
  data_tmp <- na.omit(data)
  quant_var <- quantile(data_tmp[, var], quant)
  quant_drain <- quantile(data_tmp[, paste0("drainage_", catchment)], quant)
  
  ind_var_tmp <- which(data_tmp[, var] <= quant_var)
  ind_var_drain_tmp <- which(data_tmp[ind_var_tmp, paste0("drainage_", catchment)] <= quant_drain)
  print(length(ind_var_drain_tmp) / length(ind_var_tmp))
  
  if (plot == TRUE) {
    plot_tmp <- ggplot(data_tmp, aes(x = get(var), y = get(paste0("drainage_", catchment)))) +
      geom_point(aes(col = data_tmp[, paste0("drainage_", catchment)] <= quant_drain & data_tmp[, var] <= quant_var),
                 show.legend = FALSE) +
      ggdensity::geom_hdr() +
      geom_vline(xintercept = quant_var) +
      scale_color_manual(values=c("white", "orange")) +
      labs(x = xlab, y = ylab) +
      theme_bw() +
      theme(axis.title = element_text(size = 20),
            axis.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20))
    print(plot_tmp)
  }
  length(ind_var_drain_tmp) / length(ind_var_tmp)
}

# generate tables
vars <- c("airtmp", "glorad", "groundwaterdepth", "precip", "qinfiltration",
          "relhum", "snowstorage", "soilwaterrootzone", "soilwaterunsatzone")
catchments <- c("X10304", "X10321", "X10303", "X10302")
for (i in vars) {
  vec_tmp_winter <- rep(0, 4)
  for (j in 1:length(catchments)) {
    vec_tmp_winter[j] <- lower_tail_depend(data = data_merged_rol_winter, var = paste0("rol_", i, "_", catchments[j]), catchment = catchments[j], quant = 0.1)
    assign(paste0("lower_winter_", i), vec_tmp_winter)
  }
}

for (i in vars) {
  vec_tmp_summer <- rep(0, 4)
  for (j in 1:length(catchments)) {
    vec_tmp_summer[j] <- lower_tail_depend(data = data_merged_rol_summer, var = paste0("rol_", i, "_", catchments[j]), catchment = catchments[j], quant = 0.1)
    assign(paste0("lower_summer_", i), vec_tmp_summer)
  }
}

lower_tail_depend_winter <- cbind(airtmp = lower_winter_airtmp, glorad = lower_winter_glorad,
                                  groundwaterdepth = lower_winter_groundwaterdepth, precip = lower_winter_precip,
                                  qinfiltration = lower_winter_qinfiltration, relhum = lower_winter_relhum,
                                  snowstorage = lower_winter_snowstorage, soilwaterrotzone = lower_winter_soilwaterrootzone,
                                  soilwaterunsatzone = lower_winter_soilwaterunsatzone)
rownames(lower_tail_depend_winter) <- catchments 

lower_tail_depend_summer <- cbind(airtmp = lower_summer_airtmp, glorad = lower_summer_glorad,
                                  groundwaterdepth = lower_summer_groundwaterdepth, precip = lower_summer_precip,
                                  qinfiltration = lower_summer_qinfiltration, relhum = lower_summer_relhum,
                                  snowstorage = lower_summer_snowstorage, soilwaterrotzone = lower_summer_soilwaterrootzone,
                                  soilwaterunsatzone = lower_summer_soilwaterunsatzone)
rownames(lower_tail_depend_summer) <- catchments 

# generate plots
vars <- c("airtmp", "glorad", "groundwaterdepth", "precip", "qinfiltration",
             "relhum", "snowstorage", "soilwaterrootzone", "soilwaterunsatzone")
vars_names <- c("airtmp", "glorad", "grdepth", "precip", "qinfil",
                "relhum", "snow", "soilroot", "soilunsat")
catchments <- c("X10304", "X10321", "X10303", "X10302")
catchments_names <- c("MW", "SD", "BT", "MU")

for (i in vars) {
  for (j in 1:length(catchments)) {
    lower_tail_depend(data = data_merged_rol_winter, var = paste0("rol_", i, "_", catchments[j]), catchment = catchments[j],
                      quant = 0.1, xlab = paste0(vars_names[which(vars == i)], ", ", catchments_names[j], ", wi"), ylab = paste0("drainage, ", catchments_names[j], ", wi"), plot = TRUE)
    ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/tail_depend/lower_winter_", i, "_", catchments[j], ".pdf"), width = 6, height = 4.5)
  }
}

for (i in vars) {
  for (j in 1:length(catchments)) {
    lower_tail_depend(data = data_merged_rol_summer, var = paste0("rol_", i, "_", catchments[j]), catchment = catchments[j],
                      quant = 0.1, xlab = paste0(vars_names[which(vars == i)], ", ", catchments_names[j], ", su"), ylab = paste0("drainage, ", catchments_names[j], ", su"), plot = TRUE)
    ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/tail_depend/lower_summer_", i, "_", catchments[j], ".pdf"), width = 6, height = 4.5)
  }
}

# plot for paper
ggplot(na.omit(data_merged_rol_winter), aes(x = rol_airtmp_X10303, y = drainage_X10303)) +
  geom_point(aes(col = na.omit(data_merged_rol_winter)[, "drainage_X10303"] <= quantile(na.omit(data_merged_rol_winter)[, "drainage_X10303"], 0.1)
                 & na.omit(data_merged_rol_winter)[, "rol_airtmp_X10303"] <= quantile(na.omit(data_merged_rol_winter)[, "rol_airtmp_X10303"], 0.1)),
             show.legend = FALSE) +
  ggdensity::geom_hdr() +
  geom_vline(xintercept = quantile(na.omit(data_merged_rol_winter)[, "rol_airtmp_X10303"], 0.1)) +
  scale_color_manual(values=c("white", "orange")) +
  ylim(0, 120) +
  labs(x = "airtmp, BT, wi", y = "drainage, BT, wi") +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/tail_depend/lower_winter_airtmp_X10303_cut.pdf"), width = 9, height = 6)
  


## upper tail dependence
upper_tail_depend <- function(data = data_merged_rol_winter, var = "rol_airtmp_X10304", catchment = "X10304",
                              xlab = "", ylab = "", quant = 0.9, plot = FALSE) {
  data_tmp <- na.omit(data)
  quant_var <- quantile(data_tmp[, var], quant)
  quant_drain <- quantile(data_tmp[, paste0("drainage_", catchment)], quant)
  
  ind_var_tmp <- which(data_tmp[, var] > quant_var)
  ind_var_drain_tmp <- which(data_tmp[ind_var_tmp, paste0("drainage_", catchment)] > quant_drain)
  print(length(ind_var_drain_tmp) / length(ind_var_tmp))
  
  if (plot == TRUE) {
    plot_tmp <- ggplot(data_tmp, aes(x = get(var), y = get(paste0("drainage_", catchment)))) +
      geom_point(aes(col = data_tmp[, paste0("drainage_", catchment)] > quant_drain & data_tmp[, var] > quant_var),
                 show.legend = FALSE) +
      ggdensity::geom_hdr() +
      geom_vline(xintercept = quant_var) +
      scale_color_manual(values=c("white", "orange")) +
      labs(x = xlab, y = ylab) +
      theme_bw() +
      theme(axis.title = element_text(size = 20),
            axis.text = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 20))
    print(plot_tmp)
  }
  length(ind_var_drain_tmp) / length(ind_var_tmp)
}

# generate tables
for (i in vars) {
  vec_tmp_winter <- rep(0, 4)
  for (j in 1:length(catchments)) {
    vec_tmp_winter[j] <- upper_tail_depend(data = data_merged_rol_winter, var = paste0("rol_", i, "_", catchments[j]), catchment = catchments[j], quant = 0.9)
    assign(paste0("upper_winter_", i), vec_tmp_winter)
  }
}

for (i in vars) {
  vec_tmp_summer <- rep(0, 4)
  for (j in 1:length(catchments)) {
    vec_tmp_summer[j] <- upper_tail_depend(data = data_merged_rol_summer, var = paste0("rol_", i, "_", catchments[j]), catchment = catchments[j], quant = 0.9)
    assign(paste0("upper_summer_", i), vec_tmp_summer)
  }
}

upper_tail_depend_winter <- cbind(airtmp = upper_winter_airtmp, glorad = upper_winter_glorad,
                                  groundwaterdepth = upper_winter_groundwaterdepth, precip = upper_winter_precip,
                                  qinfiltration = upper_winter_qinfiltration, relhum = upper_winter_relhum,
                                  snowstorage = upper_winter_snowstorage, soilwaterrotzone = upper_winter_soilwaterrootzone,
                                  soilwaterunsatzone = upper_winter_soilwaterunsatzone)
rownames(upper_tail_depend_winter) <- catchments 

upper_tail_depend_summer <- cbind(airtmp = upper_summer_airtmp, glorad = upper_summer_glorad,
                                  groundwaterdepth = upper_summer_groundwaterdepth, precip = upper_summer_precip,
                                  qinfiltration = upper_summer_qinfiltration, relhum = upper_summer_relhum,
                                  snowstorage = upper_summer_snowstorage, soilwaterrotzone = upper_summer_soilwaterrootzone,
                                  soilwaterunsatzone = upper_summer_soilwaterunsatzone)
rownames(upper_tail_depend_summer) <- catchments

# generate plots
vars <- c("airtmp", "glorad", "groundwaterdepth", "precip", "qinfiltration",
          "relhum", "snowstorage", "soilwaterrootzone", "soilwaterunsatzone")
vars_names <- c("airtmp", "glorad", "grdepth", "precip", "qinfil",
          "relhum", "snow", "soilroot", "soilunsat")
catchments <- c("X10304", "X10321", "X10303", "X10302")
catchments_names <- c("MW", "SD", "BT", "MU")

for (i in vars) {
  for (j in 1:length(catchments)) {
    upper_tail_depend(data = data_merged_rol_winter, var = paste0("rol_", i, "_", catchments[j]), catchment = catchments[j],
                      quant = 0.9, xlab = paste0(vars_names[which(vars == i)], ", ", catchments_names[j], ", wi"), ylab = paste0("drainage, ", catchments_names[j], ", wi"), plot = TRUE)
    ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/tail_depend/upper_winter_", i, "_", catchments[j], ".pdf"), width = 6, height = 4.5)
  }
}

for (i in vars) {
  for (j in 1:length(catchments)) {
    upper_tail_depend(data = data_merged_rol_summer, var = paste0("rol_", i, "_", catchments[j]), catchment = catchments[j],
                      quant = 0.9, xlab = paste0(vars_names[which(vars == i)], ", ", catchments_names[j], ", su"), ylab = paste0("drainage, ", catchments_names[j], ", su"), plot = TRUE)
    ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/tail_depend/upper_summer_", i, "_", catchments[j], ".pdf"), width = 6, height = 4.5)
  }
}


# plot for paper
ggplot(na.omit(data_merged_rol_winter), aes(x = rol_airtmp_X10303, y = drainage_X10303)) +
  geom_point(aes(col = na.omit(data_merged_rol_winter)[, "drainage_X10303"] > quantile(na.omit(data_merged_rol_winter)[, "drainage_X10303"], 0.9) &
                   na.omit(data_merged_rol_winter)[, "rol_airtmp_X10303"] > quantile(na.omit(data_merged_rol_winter)[, "rol_airtmp_X10303"], 0.9)),
             show.legend = FALSE) +
  ggdensity::geom_hdr() +
  geom_vline(xintercept = quantile(na.omit(data_merged_rol_winter)[, "rol_airtmp_X10303"], 0.9)) +
  scale_color_manual(values=c("white", "orange")) +
  ylim(0, 160) +
  labs(x = "airtmp, BT, wi", y = "drainage, BT, wi") +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/tail_depend/upper_winter_airtmp_X10303_cut.pdf"), width = 9, height = 6)


# catchment maps
# load shapefiles
shape_bavaria <- st_read("shapes/shapes/EZG/EZG_hydbav_orig_utm.shp")
shape_bavaria_selected <- shape_bavaria[shape_bavaria$NameString %in% c("PEG-Isar-Mittenwald", "PEG-Loisach-Schlehdorf", "PEG-Isar-Bad-Toelz", "PEG-Isar-Muenchen"), ]

shape_bavaria_admin <- st_read("shapes/shapes/Deu/Deu_adm1.shp") %>% filter(NAME_1 == "Bayern")

shape_river_minor <- st_read("shapes/shapes/Rivers_Minor/Rivers_hydBav_WGS84_MDK.shp")

shape_catchments <- st_read("shapes/shapes/Flusspegel/Flusspegel_WGS84.shp")
shape_catchments_selected <- shape_catchments[shape_catchments$Name %in% c("Mittenwald/Isar", "Schlehdorf", "Bad-Toelz-KW/Isar", "Muenchen"), ]

tif_bavaria <- raster("tifs/tifs/hydbav_500__dgm.tif")
tif_bavaria_spdf <- as(tif_bavaria, "SpatialPixelsDataFrame")
tif_bavaria <- as.data.frame(tif_bavaria_spdf)

one_polygon <- st_union(shape_bavaria$geometry)

ind_tif_bavaria <- logical(length = nrow(tif_bavaria))
for (i in 1:nrow(tif_bavaria)){
  point.x <- tif_bavaria[i, "x"]
  point.y <- tif_bavaria[i, "y"]
  if (point.in.polygon(point.x, point.y, one_polygon[[1]][[1]][, 1], one_polygon[[1]][[1]][, 2]) == 1) {
      ind_tif_bavaria[[i]] <- TRUE
  }
  print(i)
}

# write.csv(ind_tif_bavaria, "ind_tif_bavaria.csv")
tif_bavaria_selected <- tif_bavaria[ind_tif_bavaria, ]

# create maps
# full map
ggplot() +
  geom_sf(data = shape_bavaria) +
  geom_sf(data = shape_river_minor, col = "blue") +
  geom_sf(data = shape_catchments_selected, aes(color = factor(unique(shape_catchments_selected$Name), levels = c("Mittenwald/Isar", "Schlehdorf", "Bad-Toelz-KW/Isar", "Muenchen"))), size = 2) +
  geom_sf(data = shape_bavaria_admin, fill = "transparent", color = "red") +
  geom_raster(data = tif_bavaria_selected, aes(x = x, y = y, alpha = hydbav_500__dgm), show.legend = FALSE) +
  scale_alpha(name = "") +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(colour = "transparent"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/data/maps/map_full.pdf"))


# selected map
ggplot() + 
  geom_sf(data = shape_bavaria_selected, col = "black") +
  geom_sf(data = shape_river_minor, col = "blue") +
  geom_sf(data = shape_catchments_selected, aes(color = factor(unique(shape_catchments_selected$Name), levels = c("Mittenwald/Isar", "Schlehdorf", "Bad-Toelz-KW/Isar", "Muenchen"))), size = 3) +
  geom_raster(data = tif_bavaria_selected, aes(x = x, y = y, alpha = hydbav_500__dgm), show.legend = FALSE) +
  scale_color_manual(name = "catchment", c("Mittenwald", "Schlehdorf", "Bad Tölz",  "Munich"),
                     values = c("darkred", "chartreuse4", "darkcyan", "darkviolet")) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_line(colour = "transparent")) +
  coord_sf(xlim = c(615000, 738000),
           ylim = c(5230000, 5350000))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/data/maps/map_selected.pdf"))

