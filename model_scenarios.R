#X10302 = Muenchen
#X10303 = BadToelz
#X10304 = Mittenwald
#X10321 = Schlehdorf

# load packages
library(ggplot2)
source("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/functions.R")
source("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/model_chosen.R")


# load data
setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbe")
data_merged_rol <- readRDS("data_merged_rol.rds")
setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbj")
data_merged_rol_kbj <- readRDS("data_merged_rol_kbj.rds")

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


# airtmp
# manipulated data set
# winter
data_scenario_airtmp <- data_merged_rol
# Mittenwald
# compute parameters for new distribution
mean_winter_X10304_current <- mean(data_merged_rol_winter$airtmp_X10304)
var_winter_X10304_current <- var(data_merged_rol_winter$airtmp_X10304)
mean_winter_X10304_new <- mean_winter_X10304_current + 2.5
var_winter_X10304_new <- var_winter_X10304_current + 2.9

mean_summer_X10304_current <- mean(data_merged_rol_summer$airtmp_X10304)
var_summer_X10304_current <- var(data_merged_rol_summer$airtmp_X10304)
mean_summer_X10304_new <- mean_summer_X10304_current + 3.9
var_summer_X10304_new <- var_summer_X10304_current + 5

# Schlehdorf
mean_winter_X10321_current <- mean(data_merged_rol_winter$airtmp_X10321)
var_winter_X10321_current <- var(data_merged_rol_winter$airtmp_X10321)
mean_winter_X10321_new <- mean_winter_X10321_current + 2.4
var_winter_X10321_new <- var_winter_X10321_current + 1.4

mean_summer_X10321_current <- mean(data_merged_rol_summer$airtmp_X10321)
var_summer_X10321_current <- var(data_merged_rol_summer$airtmp_X10321)
mean_summer_X10321_new <- mean_summer_X10321_current + 3.8
var_summer_X10321_new <- var_summer_X10321_current + 5.3

# Bad Tölz
mean_winter_X10303_current <- mean(data_merged_rol_winter$airtmp_X10303)
var_winter_X10303_current <- var(data_merged_rol_winter$airtmp_X10303)
mean_winter_X10303_new <- mean_winter_X10303_current + 2.4
var_winter_X10303_new <- var_winter_X10303_current + 1

mean_summer_X10303_current <- mean(data_merged_rol_summer$airtmp_X10303)
var_summer_X10303_current <- var(data_merged_rol_summer$airtmp_X10303)
mean_summer_X10303_new <- mean_summer_X10303_current + 3.8
var_summer_X10303_new <- var_summer_X10303_current + 5.4

# Munich
mean_winter_X10302_current <- mean(data_merged_rol_winter$airtmp_X10302)
var_winter_X10302_current <- var(data_merged_rol_winter$airtmp_X10302)
mean_winter_X10302_new <- mean_winter_X10302_current + 2.6
var_winter_X10302_new <- var_winter_X10302_current - 4.1

mean_summer_X10302_current <- mean(data_merged_rol_summer$airtmp_X10302)
var_summer_X10302_current <- var(data_merged_rol_summer$airtmp_X10302)
mean_summer_X10302_new <- mean_summer_X10302_current + 3.6
var_summer_X10302_new <- var_summer_X10302_current + 6.3


# compute new values for non-rolling airtmp
# for (i in 1:nrow(data_merged_rol)) {
#   if (data_merged_rol[i, "MM"] %in% c(1:4, 11, 12)) {
#     # estimate quantile for non-rolling value
#     quant_temp_X10304 <- ecdf(data_merged_rol_winter$airtmp_X10304)(data_merged_rol$airtmp_X10304[i])
#     quant_temp_X10321 <- ecdf(data_merged_rol_winter$airtmp_X10321)(data_merged_rol$airtmp_X10321[i])
#     quant_temp_X10303 <- ecdf(data_merged_rol_winter$airtmp_X10303)(data_merged_rol$airtmp_X10303[i])
#     quant_temp_X10302 <- ecdf(data_merged_rol_winter$airtmp_X10302)(data_merged_rol$airtmp_X10302[i])
#     # take value for this quantile of the new distribution
#     data_scenario_airtmp$airtmp_X10304[i] <- ifelse(quant_temp_X10304 %in% c(0, 1), NA, qnorm(quant_temp_X10304, mean = mean_winter_X10304_new, sd = sqrt(var_winter_X10304_new)))
#     data_scenario_airtmp$airtmp_X10321[i] <- ifelse(quant_temp_X10321 %in% c(0, 1), NA, qnorm(quant_temp_X10321, mean = mean_winter_X10321_new, sd = sqrt(var_winter_X10321_new)))
#     data_scenario_airtmp$airtmp_X10303[i] <- ifelse(quant_temp_X10303 %in% c(0, 1), NA, qnorm(quant_temp_X10303, mean = mean_winter_X10303_new, sd = sqrt(var_winter_X10303_new)))
#     data_scenario_airtmp$airtmp_X10302[i] <- ifelse(quant_temp_X10302 %in% c(0, 1), NA, qnorm(quant_temp_X10302, mean = mean_winter_X10302_new, sd = sqrt(var_winter_X10302_new)))
#   } else {
#     # estimate quantile for non-rolling value
#     quant_temp_X10304 <- ecdf(data_merged_rol_summer$airtmp_X10304)(data_merged_rol$airtmp_X10304[i])
#     quant_temp_X10321 <- ecdf(data_merged_rol_summer$airtmp_X10321)(data_merged_rol$airtmp_X10321[i])
#     quant_temp_X10303 <- ecdf(data_merged_rol_summer$airtmp_X10303)(data_merged_rol$airtmp_X10303[i])
#     quant_temp_X10302 <- ecdf(data_merged_rol_summer$airtmp_X10302)(data_merged_rol$airtmp_X10302[i])
#     # take value for this quantile of the new distribution
#     data_scenario_airtmp$airtmp_X10304[i] <- ifelse(quant_temp_X10304 %in% c(0, 1), NA, qnorm(quant_temp_X10304, mean = mean_summer_X10304_new, sd = sqrt(var_summer_X10304_new)))
#     data_scenario_airtmp$airtmp_X10321[i] <- ifelse(quant_temp_X10321 %in% c(0, 1), NA, qnorm(quant_temp_X10321, mean = mean_summer_X10321_new, sd = sqrt(var_summer_X10321_new)))
#     data_scenario_airtmp$airtmp_X10303[i] <- ifelse(quant_temp_X10303 %in% c(0, 1), NA, qnorm(quant_temp_X10303, mean = mean_summer_X10303_new, sd = sqrt(var_summer_X10303_new)))
#     data_scenario_airtmp$airtmp_X10302[i] <- ifelse(quant_temp_X10302 %in% c(0, 1), NA, qnorm(quant_temp_X10302, mean = mean_summer_X10302_new, sd = sqrt(var_summer_X10302_new)))
#   }
# }
# 
# # create rolling variable
# data_scenario_airtmp <- data_scenario_airtmp %>%
#   mutate(rol_airtmp_X10304 = rollapply(airtmp_X10304, 8 * 7, mean, na.rm = TRUE, align = 'right', fill = NA),
#          rol_airtmp_X10321 = rollapply(airtmp_X10321, 8 * 7, mean, na.rm = TRUE, align = 'right', fill = NA),
#          rol_airtmp_X10303 = rollapply(airtmp_X10303, 8 * 7, mean, na.rm = TRUE, align = 'right', fill = NA),
#          rol_airtmp_X10302 = rollapply(airtmp_X10302, 8 * 7, mean, na.rm = TRUE, align = 'right', fill = NA))
#
# # winter and summer data set
# data_scenario_winter_airtmp <- data_scenario_airtmp[data_scenario_airtmp$MM < 5 | data_scenario_airtmp$MM > 10, ]
# data_scenario_summer_airtmp <- data_scenario_airtmp[data_scenario_airtmp$MM >= 5 & data_scenario_airtmp$MM <= 10, ]
# 
# saveRDS(data_scenario_airtmp, "data_scenario_airtmp.rds")
# saveRDS(data_scenario_winter_airtmp, "data_scenario_winter_airtmp.rds")
# saveRDS(data_scenario_summer_airtmp, "data_scenario_summer_airtmp.rds")

setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbe")
data_scenario_winter_airtmp <- readRDS("data_scenario_winter_airtmp.rds")
data_scenario_summer_airtmp <- readRDS("data_scenario_summer_airtmp.rds")

# Winter
# predict drainage 
data_scenario_winter_airtmp$drainage_X10304 <- predict(model_winter_ps_intera_04, newdata = data_scenario_winter_airtmp, type = "response")
data_scenario_winter_airtmp$drainage_X10321 <- predict(model_winter_ps_intera_21, newdata = data_scenario_winter_airtmp, type = "response")
data_scenario_winter_airtmp$drainage_X10303 <- predict(model_winter_ps_intera_03, newdata = data_scenario_winter_airtmp, type = "response")
data_scenario_winter_airtmp$drainage_X10302 <- predict(model_winter_ps_intera_02, newdata = data_scenario_winter_airtmp, type = "response")

# Mittenwald
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10304), ], aes(x = drainage_X10304)) +
  geom_density() +
  labs(x = "drainage, MW, wi") +
  xlim(0, 127) +
  ylim(0, 0.225) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_winter_airtmp, aes(x = drainage_X10304)) +
  geom_density() +
  labs(x = "drainage, MW, wi") +
  xlim(0, 127) +
  ylim(0, 0.225) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10304), "drainage_X10304"] <= NM7Q_X10304_winter)) /
  length(data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10304), "drainage_X10304"])

length(which(data_scenario_winter_airtmp[!is.na(data_scenario_winter_airtmp$drainage_X10304), "drainage_X10304"] <= NM7Q_X10304_winter)) /
  length(data_scenario_winter_airtmp[!is.na(data_scenario_winter_airtmp$drainage_X10304), "drainage_X10304"])

# Schlehdorf
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10321), ], aes(x = drainage_X10321)) +
  geom_density() +
  labs(x = "drainage, SD, wi") +
  xlim(0, 300) +
  ylim(0, 0.1) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_winter_airtmp, aes(x = drainage_X10321)) +
  geom_density() +
  labs(x = "drainage, SD, wi") +
  xlim(0, 300) +
  ylim(0, 0.1) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10321), "drainage_X10321"] <= NM7Q_X10321_winter)) /
  length(data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10321), "drainage_X10321"])

length(which(data_scenario_winter_airtmp[!is.na(data_scenario_winter_airtmp$drainage_X10321), "drainage_X10321"] <= NM7Q_X10321_winter)) /
  length(data_scenario_winter_airtmp[!is.na(data_scenario_winter_airtmp$drainage_X10321), "drainage_X10321"])

# Bad Tölz
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10303), ], aes(x = drainage_X10303)) +
  geom_density() +
  labs(x = "drainage, BT, wi") +
  xlim(0, 600) +
  ylim(0, 0.08) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_winter_airtmp, aes(x = drainage_X10303)) +
  geom_density() +
  labs(x = "drainage, BT, wi") +
  xlim(0, 600) +
  ylim(0, 0.08) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10303), "drainage_X10303"] <= NM7Q_X10303_winter)) /
  length(data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10303), "drainage_X10303"])

length(which(data_scenario_winter_airtmp[!is.na(data_scenario_winter_airtmp$drainage_X10303), "drainage_X10303"] <= NM7Q_X10303_winter)) /
  length(data_scenario_winter_airtmp[!is.na(data_scenario_winter_airtmp$drainage_X10303), "drainage_X10303"])


# Munich
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10302), ], aes(x = drainage_X10302)) +
  geom_density() +
  labs(x = "drainage, MU, wi") +
  xlim(0, 12500) +
  ylim(0, 0.035) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_winter_airtmp, aes(x = drainage_X10302)) +
  geom_density() +
  labs(x = "drainage, MU, wi") +
  xlim(0, 12500) +
  ylim(0, 0.035) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10302), "drainage_X10302"] <= NM7Q_X10302_winter)) /
  length(data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10302), "drainage_X10302"])

length(which(data_scenario_winter_airtmp[!is.na(data_scenario_winter_airtmp$drainage_X10302), "drainage_X10302"] <= NM7Q_X10302_winter)) /
  length(data_scenario_winter_airtmp[!is.na(data_scenario_winter_airtmp$drainage_X10302), "drainage_X10302"])


# x axis on log scale
# Mittenwald
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10304), ], aes(x = drainage_X10304)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10304_winter, col = "blue") +
  labs(x = "drainage, MW, wi") +
  scale_x_continuous(trans = "log10", limits = c(2.5, 126)) + 
  ylim(0, 3) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_winter_X10304_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_winter_airtmp, aes(x = drainage_X10304)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10304_winter, col = "blue") +
  labs(x = "drainage, MW, wi") +
  scale_x_continuous(trans = "log10", limits = c(2.5, 126)) + 
  ylim(0, 3) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_winter_X10304_after_log.pdf"), width = 6, height = 4.5)

# Schlehdorf
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10321), ], aes(x = drainage_X10321)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10321_winter, col = "blue") +
  labs(x = "drainage, SD, wi") +
  scale_x_continuous(trans = "log10", limits = c(5, 300)) +
  ylim(0, 3) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_winter_X10321_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_winter_airtmp, aes(x = drainage_X10321)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10321_winter, col = "blue") +
  labs(x = "drainage, SD, wi") +
  scale_x_continuous(trans = "log10", limits = c(5, 300)) + 
  ylim(0, 3) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_winter_X10321_after_log.pdf"), width = 6, height = 4.5)

# Bad Tölz
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10303), ], aes(x = drainage_X10303)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10303_winter, col = "blue") +
  labs(x = "drainage, BT, wi") +
  scale_x_continuous(trans = "log10", limits = c(12, 600)) +
  ylim(0, 4) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_winter_X10303_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_winter_airtmp, aes(x = drainage_X10303)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10303_winter, col = "blue") +
  labs(x = "drainage, BT, wi") +
  scale_x_continuous(trans = "log10", limits = c(12, 600)) +
  ylim(0, 4) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_winter_X10303_after_log.pdf"), width = 6, height = 4.5)


# Munich
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_airtmp$drainage_X10302), ], aes(x = drainage_X10302)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10302_winter, col = "blue") +
  labs(x = "drainage, MU, wi") +
  scale_x_continuous(trans = "log10", limits = c(30, 13000)) +
  ylim(0, 3.5) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_winter_X10302_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_winter_airtmp, aes(x = drainage_X10302)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10302_winter, col = "blue") +
  labs(x = "drainage, MU, wi") +
  scale_x_continuous(trans = "log10", limits = c(30, 13000)) +
  ylim(0, 3.5) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_winter_X10302_after_log.pdf"), width = 6, height = 4.5)




# Summer
# predict drainage 
data_scenario_summer_airtmp$drainage_X10304 <- predict(model_summer_ps_intera_04, newdata = data_scenario_summer_airtmp, type = "response")
data_scenario_summer_airtmp$drainage_X10321 <- predict(model_summer_ps_intera_21, newdata = data_scenario_summer_airtmp, type = "response")
data_scenario_summer_airtmp$drainage_X10303 <- predict(model_summer_ps_intera_03, newdata = data_scenario_summer_airtmp, type = "response")
data_scenario_summer_airtmp$drainage_X10302 <- predict(model_summer_ps_intera_02, newdata = data_scenario_summer_airtmp, type = "response")

# Mittenwald
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10304), ], aes(x = drainage_X10304)) +
  geom_density() +
  labs(x = "drainage, MW, su") +
  xlim(0, 127) +
  ylim(0, 0.08) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_summer_airtmp, aes(x = drainage_X10304)) +
  geom_density() +
  labs(x = "drainage, MW, su") +
  xlim(0, 127) +
  ylim(0, 0.08) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10304), "drainage_X10304"] <= NM7Q_X10304_summer)) /
  length(data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10304), "drainage_X10304"])

length(which(data_scenario_summer_airtmp[!is.na(data_scenario_summer_airtmp$drainage_X10304), "drainage_X10304"] <= NM7Q_X10304_summer)) /
  length(data_scenario_summer_airtmp[!is.na(data_scenario_summer_airtmp$drainage_X10304), "drainage_X10304"])

# Schlehdorf
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10321), ], aes(x = drainage_X10321)) +
  geom_density() +
  labs(x = "drainage, SD, su") +
  xlim(0, 300) +
  ylim(0, 0.045) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_summer_airtmp, aes(x = drainage_X10321)) +
  geom_density() +
  labs(x = "drainage, SD, su") +
  xlim(0, 300) +
  ylim(0, 0.045) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10321), "drainage_X10321"] <= NM7Q_X10321_summer)) /
  length(data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10321), "drainage_X10321"])

length(which(data_scenario_summer_airtmp[!is.na(data_scenario_summer_airtmp$drainage_X10321), "drainage_X10321"] <= NM7Q_X10321_summer)) /
  length(data_scenario_summer_airtmp[!is.na(data_scenario_summer_airtmp$drainage_X10321), "drainage_X10321"])

# Bad Tölz
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10303), ], aes(x = drainage_X10303)) +
  geom_density() +
  labs(x = "drainage, BT, su") +
  xlim(0, 540) +
  ylim(0, 0.06) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_summer_airtmp, aes(x = drainage_X10303)) +
  geom_density() +
  labs(x = "drainage, BT, su") +
  xlim(0, 540) +
  ylim(0, 0.06) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10303), "drainage_X10303"] <= NM7Q_X10303_summer)) /
  length(data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10303), "drainage_X10303"])

length(which(data_scenario_summer_airtmp[!is.na(data_scenario_summer_airtmp$drainage_X10303), "drainage_X10303"] <= NM7Q_X10303_summer)) /
  length(data_scenario_summer_airtmp[!is.na(data_scenario_summer_airtmp$drainage_X10303), "drainage_X10303"])


# Munich
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10302), ], aes(x = drainage_X10302)) +
  geom_density() +
  labs(x = "drainage, MU, su") +
  xlim(0, 15000) +
  ylim(0, 0.045) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_summer_airtmp, aes(x = drainage_X10302)) +
  geom_density() +
  labs(x = "drainage, MU, su") +
  xlim(0, 15000) +
  ylim(0, 0.045) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10302), "drainage_X10302"] <= NM7Q_X10302_summer)) /
  length(data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10302), "drainage_X10302"])

length(which(data_scenario_summer_airtmp[!is.na(data_scenario_summer_airtmp$drainage_X10302), "drainage_X10302"] <= NM7Q_X10302_summer)) /
  length(data_scenario_summer_airtmp[!is.na(data_scenario_summer_airtmp$drainage_X10302), "drainage_X10302"])



# x axis on log scale
# Mittenwald
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10304), ], aes(x = drainage_X10304)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10304_summer, col = "blue") +
  labs(x = "drainage, MW, su") +
  scale_x_continuous(trans = "log10", limits = c(3, 130)) + 
  ylim(0, 2) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_summer_X10304_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_summer_airtmp, aes(x = drainage_X10304)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10304_summer, col = "blue") +
  labs(x = "drainage, MW, su") +
  scale_x_continuous(trans = "log10", limits = c(3, 130)) + 
  ylim(0, 2) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_summer_X10304_after_log.pdf"), width = 6, height = 4.5)

# Schlehdorf
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10321), ], aes(x = drainage_X10321)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10321_summer, col = "blue") +
  labs(x = "drainage, SD, su") +
  scale_x_continuous(trans = "log10", limits = c(6, 300)) +
  ylim(0, 2.5) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_summer_X10321_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_summer_airtmp, aes(x = drainage_X10321)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10321_summer, col = "blue") +
  labs(x = "drainage, SD, su") +
  scale_x_continuous(trans = "log10", limits = c(6, 300)) + 
  ylim(0, 2.5) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_summer_X10321_after_log.pdf"), width = 6, height = 4.5)


# Bad Tölz
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10303), ], aes(x = drainage_X10303)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10303_summer, col = "blue") +
  labs(x = "drainage, BT, su") +
  scale_x_continuous(trans = "log10", limits = c(14, 550)) +
  ylim(0, 3.75) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_summer_X10303_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_summer_airtmp, aes(x = drainage_X10303)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10303_summer, col = "blue") +
  labs(x = "drainage, BT, su") +
  scale_x_continuous(trans = "log10", limits = c(14, 550)) +
  ylim(0, 3.75) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_summer_X10303_after_log.pdf"), width = 6, height = 4.5)


# Munich
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_airtmp$drainage_X10302), ], aes(x = drainage_X10302)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10302_summer, col = "blue") +
  labs(x = "drainage, MU, su") +
  scale_x_continuous(trans = "log10", limits = c(30, 15000)) +
  ylim(0, 3.5) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_summer_X10302_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_summer_airtmp, aes(x = drainage_X10302)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10302_summer, col = "blue") +
  labs(x = "drainage, MU, su") +
  scale_x_continuous(trans = "log10", limits = c(30, 15000)) +
  ylim(0, 3.5) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/airtmp_summer_X10302_after_log.pdf"), width = 6, height = 4.5)


# precipitation
# manipulated data set
# winter
data_scenario_precip <- data_merged_rol
# compute new values for non-rolling precip
# for (i in 1:nrow(data_merged_rol)) {
#   if (data_merged_rol[i, "MM"] %in% c(1:4, 11, 12)) {
#     data_scenario_precip$precip_X10304[i] <- data_scenario_precip$precip_X10304[i] * 1.1
#     data_scenario_precip$precip_X10321[i] <- data_scenario_precip$precip_X10321[i] * 1.1
#     data_scenario_precip$precip_X10303[i] <- data_scenario_precip$precip_X10303[i] * 1.09
#     data_scenario_precip$precip_X10302[i] <- data_scenario_precip$precip_X10302[i] * 1.1
#   } else {
#     data_scenario_precip$precip_X10304[i] <- data_scenario_precip$precip_X10304[i] * 1.07
#     data_scenario_precip$precip_X10321[i] <- data_scenario_precip$precip_X10321[i] * 1.03
#     data_scenario_precip$precip_X10303[i] <- data_scenario_precip$precip_X10303[i] * 1.03
#     data_scenario_precip$precip_X10302[i] <- data_scenario_precip$precip_X10302[i] * 1.05
#   }
# }
# # create rolling variable
# data_scenario_precip <- data_scenario_precip %>%
#   mutate(rol_precip_X10304 = rollapply(precip_X10304, 8 * 7, mean, na.rm = TRUE, align = 'right', fill = NA),
#          rol_precip_X10321 = rollapply(precip_X10321, 8 * 7, mean, na.rm = TRUE, align = 'right', fill = NA),
#          rol_precip_X10303 = rollapply(precip_X10303, 8 * 7, mean, na.rm = TRUE, align = 'right', fill = NA),
#          rol_precip_X10302 = rollapply(precip_X10302, 8 * 7, mean, na.rm = TRUE, align = 'right', fill = NA))
# # winter and summer data set
# data_scenario_winter_precip <- data_scenario_precip[data_scenario_precip$MM < 5 | data_scenario_precip$MM > 10, ]
# data_scenario_summer_precip <- data_scenario_precip[data_scenario_precip$MM >= 5 & data_scenario_precip$MM <= 10, ]
# 
# saveRDS(data_scenario_precip, "data_scenario_precip.rds")
# saveRDS(data_scenario_winter_precip, "data_scenario_winter_precip.rds")
# saveRDS(data_scenario_summer_precip, "data_scenario_summer_precip.rds")

setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbe")
data_scenario_winter_precip <- readRDS("data_scenario_winter_precip.rds")
data_scenario_summer_precip <- readRDS("data_scenario_summer_precip.rds")

# Winter
# predict drainage 
data_scenario_winter_precip$drainage_X10304 <- predict(model_winter_ps_intera_04, newdata = data_scenario_winter_precip, type = "response")
data_scenario_winter_precip$drainage_X10321 <- predict(model_winter_ps_intera_21, newdata = data_scenario_winter_precip, type = "response")
data_scenario_winter_precip$drainage_X10303 <- predict(model_winter_ps_intera_03, newdata = data_scenario_winter_precip, type = "response")
data_scenario_winter_precip$drainage_X10302 <- predict(model_winter_ps_intera_02, newdata = data_scenario_winter_precip, type = "response")


# Mittenwald
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10304), ], aes(x = drainage_X10304)) +
  geom_density() +
  labs(x = "drainage, MW, wi") +
  xlim(0, 230) +
  ylim(0, 0.23) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_winter_precip, aes(x = drainage_X10304)) +
  geom_density() +
  labs(x = "drainage, MW, wi") +
  xlim(0, 230) +
  ylim(0, 0.23) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10304), "drainage_X10304"] <= NM7Q_X10304_winter)) /
  length(data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10304), "drainage_X10304"])

length(which(data_scenario_winter_precip[!is.na(data_scenario_winter_precip$drainage_X10304), "drainage_X10304"] <= NM7Q_X10304_winter)) /
  length(data_scenario_winter_precip[!is.na(data_scenario_winter_precip$drainage_X10304), "drainage_X10304"])

# Schlehdorf
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10321), ], aes(x = drainage_X10321)) +
  geom_density() +
  labs(x = "drainage, SD, wi") +
  xlim(0, 545) +
  ylim(0, 0.1) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_winter_precip, aes(x = drainage_X10321)) +
  geom_density() +
  labs(x = "drainage, SD, wi") +
  xlim(0, 545) +
  ylim(0, 0.1) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10321), "drainage_X10321"] <= NM7Q_X10321_winter)) /
  length(data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10321), "drainage_X10321"])

length(which(data_scenario_winter_precip[!is.na(data_scenario_winter_precip$drainage_X10321), "drainage_X10321"] <= NM7Q_X10321_winter)) /
  length(data_scenario_winter_precip[!is.na(data_scenario_winter_precip$drainage_X10321), "drainage_X10321"])

# Bad Tölz
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10303), ], aes(x = drainage_X10303)) +
  geom_density() +
  labs(x = "drainage, BT, wi") +
  xlim(0, 8500) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_winter_precip, aes(x = drainage_X10303)) +
  geom_density() +
  labs(x = "drainage, BT, wi") +
  xlim(0, 8500) +
  ylim(0, 0.2) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10303), "drainage_X10303"] <= NM7Q_X10303_winter)) /
  length(data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10303), "drainage_X10303"])

length(which(data_scenario_winter_precip[!is.na(data_scenario_winter_precip$drainage_X10303), "drainage_X10303"] <= NM7Q_X10303_winter)) /
  length(data_scenario_winter_precip[!is.na(data_scenario_winter_precip$drainage_X10303), "drainage_X10303"])


# Munich
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10302), ], aes(x = drainage_X10302)) +
  geom_density() +
  labs(x = "drainage, MU, wi") +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_winter_precip, aes(x = drainage_X10302)) +
  geom_density() +
  labs(x = "drainage, MU, wi") +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10302), "drainage_X10302"] <= NM7Q_X10302_winter)) /
  length(data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10302), "drainage_X10302"])

length(which(data_scenario_winter_precip[!is.na(data_scenario_winter_precip$drainage_X10302), "drainage_X10302"] <= NM7Q_X10302_winter)) /
  length(data_scenario_winter_precip[!is.na(data_scenario_winter_precip$drainage_X10302), "drainage_X10302"])



# x axis on log scale
# Mittenwald
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10304), ], aes(x = drainage_X10304)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10304_winter, col = "blue") +
  labs(x = "drainage, MW, wi") +
  scale_x_continuous(trans = "log10", limits = c(2.5, 230)) + 
  ylim(0, 2.8) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_winter_X10304_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_winter_precip, aes(x = drainage_X10304)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10304_winter, col = "blue") +
  labs(x = "drainage, MW, wi") +
  scale_x_continuous(trans = "log10", limits = c(2.5, 230)) + 
  ylim(0, 2.8) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_winter_X10304_after_log.pdf"), width = 6, height = 4.5)

# Schlehdorf
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10321), ], aes(x = drainage_X10321)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10321_winter, col = "blue") +
  labs(x = "drainage, SD, wi") +
  scale_x_continuous(trans = "log10", limits = c(6, 550)) +
  ylim(0, 2.5) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_winter_X10321_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_winter_precip, aes(x = drainage_X10321)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10321_winter, col = "blue") +
  labs(x = "drainage, SD, wi") +
  scale_x_continuous(trans = "log10", limits = c(6, 550)) + 
  ylim(0, 2.5) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_winter_X10321_after_log.pdf"), width = 6, height = 4.5)

# Bad Tölz
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10303), ], aes(x = drainage_X10303)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10303_winter, col = "blue") +
  labs(x = "drainage, BT, wi") +
  scale_x_continuous(trans = "log10", limits = c(12, 8500)) +
  ylim(0, 3.75) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_winter_X10303_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_winter_precip, aes(x = drainage_X10303)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10303_winter, col = "blue") +
  labs(x = "drainage, BT, wi") +
  scale_x_continuous(trans = "log10", limits = c(12, 8500)) +
  ylim(0, 3.75) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_winter_X10303_after_log.pdf"), width = 6, height = 4.5)


# Munich
# before
ggplot(data = data_merged_rol_winter[!is.na(data_scenario_winter_precip$drainage_X10302), ], aes(x = drainage_X10302)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10302_winter, col = "blue") +
  labs(x = "drainage, MU, wi") +
  scale_x_continuous(trans = "log10", limits = c(10, 1e+28)) +
  ylim(0, 3.8) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_winter_X10302_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_winter_precip, aes(x = drainage_X10302)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10302_winter, col = "blue") +
  labs(x = "drainage, MU, wi") +
  scale_x_continuous(trans = "log10", limits = c(10, 1e+28)) +
  ylim(0, 3.8) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_winter_X10302_after_log.pdf"), width = 6, height = 4.5)



# Summer
# predict drainage 
data_scenario_summer_precip$drainage_X10304 <- predict(model_summer_ps_intera_04, newdata = data_scenario_summer_precip, type = "response")
data_scenario_summer_precip$drainage_X10321 <- predict(model_summer_ps_intera_21, newdata = data_scenario_summer_precip, type = "response")
data_scenario_summer_precip$drainage_X10303 <- predict(model_summer_ps_intera_03, newdata = data_scenario_summer_precip, type = "response")
data_scenario_summer_precip$drainage_X10302 <- predict(model_summer_ps_intera_02, newdata = data_scenario_summer_precip, type = "response")

# Mittenwald
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10304), ], aes(x = drainage_X10304)) +
  geom_density() +
  labs(x = "drainage, MW, su") +
  xlim(0, 127) +
  ylim(0, 0.072) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_summer_precip, aes(x = drainage_X10304)) +
  geom_density() +
  labs(x = "drainage, MW, su") +
  xlim(0, 127) +
  ylim(0, 0.072) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10304), "drainage_X10304"] <= NM7Q_X10304_summer)) /
  length(data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10304), "drainage_X10304"])

length(which(data_scenario_summer_precip[!is.na(data_scenario_summer_precip$drainage_X10304), "drainage_X10304"] <= NM7Q_X10304_summer)) /
  length(data_scenario_summer_precip[!is.na(data_scenario_summer_precip$drainage_X10304), "drainage_X10304"])

# Schlehdorf
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10321), ], aes(x = drainage_X10321)) +
  geom_density() +
  labs(x = "drainage, SD, su") +
  xlim(0, 345) +
  ylim(0, 0.046) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_summer_precip, aes(x = drainage_X10321)) +
  geom_density() +
  labs(x = "drainage, SD, su") +
  xlim(0, 345) +
  ylim(0, 0.046) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10321), "drainage_X10321"] <= NM7Q_X10321_summer)) /
  length(data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10321), "drainage_X10321"])

length(which(data_scenario_summer_precip[!is.na(data_scenario_summer_precip$drainage_X10321), "drainage_X10321"] <= NM7Q_X10321_summer)) /
  length(data_scenario_summer_precip[!is.na(data_scenario_summer_precip$drainage_X10321), "drainage_X10321"])

# Bad Tölz
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10303), ], aes(x = drainage_X10303)) +
  geom_density() +
  labs(x = "drainage, BT, su") +
  xlim(0, 460) +
  ylim(0, 0.06) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_summer_precip, aes(x = drainage_X10303)) +
  geom_density() +
  labs(x = "drainage, BT, su") +
  xlim(0, 460) +
  ylim(0, 0.06) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10303), "drainage_X10303"] <= NM7Q_X10303_summer)) /
  length(data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10303), "drainage_X10303"])

length(which(data_scenario_summer_precip[!is.na(data_scenario_summer_precip$drainage_X10303), "drainage_X10303"] <= NM7Q_X10303_summer)) /
  length(data_scenario_summer_precip[!is.na(data_scenario_summer_precip$drainage_X10303), "drainage_X10303"])


# Munich
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10302), ], aes(x = drainage_X10302)) +
  geom_density() +
  labs(x = "drainage, MU, su") +
  xlim(0, 12750) +
  ylim(0, 0.031) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
# after
ggplot(data = data_scenario_summer_precip, aes(x = drainage_X10302)) +
  geom_density() +
  labs(x = "drainage, MU, su") +
  xlim(0, 12750) +
  ylim(0, 0.031) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

length(which(data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10302), "drainage_X10302"] <= NM7Q_X10302_summer)) /
  length(data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10302), "drainage_X10302"])

length(which(data_scenario_summer_precip[!is.na(data_scenario_summer_precip$drainage_X10302), "drainage_X10302"] <= NM7Q_X10302_summer)) /
  length(data_scenario_summer_precip[!is.na(data_scenario_summer_precip$drainage_X10302), "drainage_X10302"])



# x axis on log scale
# Mittenwald
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10304), ], aes(x = drainage_X10304)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10304_summer, col = "blue") +
  labs(x = "drainage, MW, su") +
  scale_x_continuous(trans = "log10", limits = c(3.5, 128)) + 
  ylim(0, 1.9) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_summer_X10304_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_summer_precip, aes(x = drainage_X10304)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10304_summer, col = "blue") +
  labs(x = "drainage, MW, su") +
  scale_x_continuous(trans = "log10", limits = c(3.5, 128)) + 
  ylim(0, 1.9) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_summer_X10304_after_log.pdf"), width = 6, height = 4.5)

# Schlehdorf
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10321), ], aes(x = drainage_X10321)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10321_summer, col = "blue") +
  labs(x = "drainage, SD, su") +
  scale_x_continuous(trans = "log10", limits = c(6.5, 345)) +
  ylim(0, 2.3) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_summer_X10321_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_summer_precip, aes(x = drainage_X10321)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10321_summer, col = "blue") +
  labs(x = "drainage, SD, su") +
  scale_x_continuous(trans = "log10", limits = c(6.5, 345)) + 
  ylim(0, 2.3) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_summer_X10321_after_log.pdf"), width = 6, height = 4.5)


# Bad Tölz
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10303), ], aes(x = drainage_X10303)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10303_summer, col = "blue") +
  labs(x = "drainage, BT, su") +
  scale_x_continuous(trans = "log10", limits = c(14, 460)) +
  ylim(0, 3.75) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_summer_X10303_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_summer_precip, aes(x = drainage_X10303)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10303_summer, col = "blue") +
  labs(x = "drainage, BT, su") +
  scale_x_continuous(trans = "log10", limits = c(14, 460)) +
  ylim(0, 3.75) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_summer_X10303_after_log.pdf"), width = 6, height = 4.5)


# Munich
# before
ggplot(data = data_merged_rol_summer[!is.na(data_scenario_summer_precip$drainage_X10302), ], aes(x = drainage_X10302)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10302_summer, col = "blue") +
  labs(x = "drainage, MU, su") +
  scale_x_continuous(trans = "log10", limits = c(30, 12720)) +
  ylim(0, 3.1) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_summer_X10302_before_log.pdf"), width = 6, height = 4.5)

# after
ggplot(data = data_scenario_summer_precip, aes(x = drainage_X10302)) +
  geom_density() +
  geom_vline(xintercept = NM7Q_X10302_summer, col = "blue") +
  labs(x = "drainage, MU, su") +
  scale_x_continuous(trans = "log10", limits = c(30, 12720)) +
  ylim(0, 3.1) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/scenarios/precip_summer_X10302_after_log.pdf"), width = 6, height = 4.5)

