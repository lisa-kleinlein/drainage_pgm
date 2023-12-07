library(interactions)
library(ggplot2)
library(devEMF)
library(dplyr)

source("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/model_chosen.R")

# effect visualization
# airtmp
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10304_airtmp.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_04, ylim = c(-0.35, 0.75), select = 1, xlab = "MW, wi", ylab = "s(airtmp)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_airtmp_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10321_airtmp.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_21, ylim = c(-0.35, 0.75), select = 1, xlab = "SD, wi", ylab = "s(airtmp)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_airtmp_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10303_airtmp.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_03, ylim = c(-0.35, 0.75), select = 1, xlab = "BT, wi", ylab = "s(airtmp)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_airtmp_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10302_airtmp_soilroot.pdf", width = 6, height = 4)
quantiles_winter_X10302_airtmp <- quantile(data_merged_rol_winter$rol_airtmp_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_winter_X10302_airtmp) <- seq(0, 1, by = 0.1)
interact_plot(model = model_winter_ps_intera_02, pred = rol_airtmp_X10302, modx = rol_soilwaterrootzone_X10302,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MU, wi", legend.main = "soilroot, MU, wi", outcome.scale = "link") +
  ylim(3.9, 5) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_winter_X10302_airtmp)[-c(4, 6, 8)],
                                         breaks = quantiles_winter_X10302_airtmp[-c(4, 6, 8)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10304_airtmp.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_04, select = 1, ylim = c(-0.35, 0.75), xlab = "MW, su", ylab = "s(airtmp)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_airtmp_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10321_airtmp.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_21, select = 1, ylim = c(-0.35, 0.75), xlab = "SD, su", ylab = "s(airtmp)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_airtmp_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10303_airtmp.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_03, select = 1, ylim = c(-0.35, 0.75), xlab = "BT, su", ylab = "s(airtmp)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_airtmp_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10302_airtmp.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_02, select = 1, ylim = c(-0.35, 0.75), xlab = "MU, su", ylab = "s(airtmp)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_airtmp_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

# glorad
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10304_glorad.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_04, select = 2, ylim = c(-0.5, 0.8), xlab = "MW, wi", ylab = "s(glorad)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_glorad_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10321_glorad.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_21, select = 2, ylim = c(-0.5, 0.8), xlab = "SD, wi", ylab = "s(glorad)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_glorad_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10303_glorad.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_03, select = 2, ylim = c(-0.5, 0.8), xlab = "BT, wi", ylab = "s(glorad)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_glorad_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10302_glorad.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_02, select = 2, ylim = c(-0.5, 0.8), xlab = "MU, wi", ylab = "s(glorad)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_glorad_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10304_glorad_snow.pdf", width = 6, height = 4)
quantiles_summer_X10304_glorad <- quantile(data_merged_rol_summer$rol_glorad_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10304_glorad) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_04, pred = rol_glorad_X10304, modx = rol_snowstorage_X10304,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MW, su", legend.main = "snow, MW, su", outcome.scale = "link") +
  ylim(2.1, 3.4) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10304_glorad[]),
                                         breaks = quantiles_summer_X10304_glorad)) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10321_glorad_snow.pdf", width = 6, height = 4)
quantiles_summer_X10321_glorad <- quantile(data_merged_rol_summer$rol_glorad_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10321_glorad) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_21, pred = rol_glorad_X10321, modx = rol_snowstorage_X10321,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "SD, su", legend.main = "snow, SD, su", outcome.scale = "link") +
  ylim(2.3, 3.6) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10321_glorad),
                                         breaks = quantiles_summer_X10321_glorad)) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10303_glorad.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_03, select = 2, ylim = c(-0.5, 0.8), xlab = "BT, su", ylab = "s(glorad)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_glorad_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10302_glorad.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_02, select = 2, ylim = c(-0.5, 0.8), xlab = "MU, su", ylab = "s(glorad)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_glorad_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()


# groundwaterdepth
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10304_grdepth.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_04, select = 3, ylim = c(-0.3, 0.6), xlab = "MW, wi", ylab = "s(grdepth)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_groundwaterdepth_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10321_grdepth.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_21, select = 3, ylim = c(-0.3, 0.6), xlab = "SD, wi", ylab = "s(grdepth)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_groundwaterdepth_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10303_grdepth.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_03, select = 3, ylim = c(-0.3, 0.6), xlab = "BT, wi", ylab = "s(grdepth)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_groundwaterdepth_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10302_grdepth_snow.pdf", width = 6, height = 4)
quantiles_winter_X10302_grdepth <- quantile(data_merged_rol_winter$rol_groundwaterdepth_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_winter_X10302_grdepth) <- seq(0, 1, by = 0.1)
interact_plot(model = model_winter_ps_intera_02, pred = rol_groundwaterdepth_X10302, modx = rol_snowstorage_X10302,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MU, wi", legend.main = "snow, MU, wi", outcome.scale = "link") +
  ylim(3.8, 4.7) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_winter_X10302_grdepth)[-7],
                                         breaks = quantiles_winter_X10302_grdepth[-7])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10304_grdepth.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_04, select = 3, ylim = c(-0.3, 0.6), xlab = "MW, su", ylab = "s(grdepth)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_groundwaterdepth_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10321_grdepth.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_21, select = 3, ylim = c(-0.3, 0.6), xlab = "SD, su", ylab = "s(grdepth)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_groundwaterdepth_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10303_grdepth.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_03, select = 3, ylim = c(-0.3, 0.6), xlab = "BT, su", ylab = "s(grdepth)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_groundwaterdepth_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10302_grdepth.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_02, select = 3, ylim = c(-0.12, 0.25), xlab = "MU, su", ylab = "s(grdepth)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_groundwaterdepth_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()



# precipitation
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10304_precip.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_04, select = 4, ylim = c(-0.6, 4), xlab = "MW, wi", ylab = "s(precip)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_precip_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10321_precip.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_21, select = 4, ylim = c(-0.6, 4), xlab = "SD, wi", ylab = "s(precip)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_precip_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10303_precip_qinfil.pdf", width = 6, height = 4)
quantiles_winter_X10303_precip <- quantile(data_merged_rol_winter$rol_precip_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_winter_X10303_precip) <- seq(0, 1, by = 0.1)
interact_plot(model = model_winter_ps_intera_03, pred = rol_precip_X10303, modx = rol_qinfiltration_X10303,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "BT, wi", legend.main = "qinfil, BT, wi", outcome.scale = "link") +
  ylim(2.5, 7.1) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_winter_X10303_precip[-c(2, 4, 6)]),
                                         breaks = quantiles_winter_X10303_precip[-c(2, 4, 6)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10302_precip.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_02, select = 4, ylim = c(-0.6, 4), xlab = "MU, wi", ylab = "s(precip)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_precip_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10304_precip.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_04, select = 4, ylim = c(-0.6, 4), xlab = "MW, su", ylab = "s(precip)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_precip_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10321_precip.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_21, select = 4, ylim = c(-0.6, 4), xlab = "SD, su", ylab = "s(precip)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_precip_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10303_precip_snow.pdf", width = 6, height = 4)
quantiles_summer_X10303_precip <- quantile(data_merged_rol_summer$rol_precip_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10303_precip) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_03, pred = rol_precip_X10303, modx = rol_snowstorage_X10303,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "BT, su", legend.main = "snow, BT, su", outcome.scale = "link") +
  ylim(2.5, 7.1) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10303_precip[-c(2, 4, 6, 8)]),
                                         breaks = quantiles_summer_X10303_precip[-c(2, 4, 6, 8)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10302_precip_snow.pdf", width = 6, height = 4)
quantiles_summer_X10302_precip <- quantile(data_merged_rol_summer$rol_precip_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10302_precip) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_02, pred = rol_precip_X10302, modx = rol_snowstorage_X10302,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MU, su", legend.main = "snow, MU, su", outcome.scale = "link") +
  ylim(2.6, 7.2) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10302_precip[-c(2, 4, 6, 8)]),
                                         breaks = quantiles_summer_X10302_precip[-c(2, 4, 6, 8)])) +
  theme(legend.position = "bottom")
dev.off()




# qinfiltration
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10304_qinfil.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_04, select = 5, ylim = c(-3, 2), xlab = "MW, wi", ylab = "s(qinfil)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_qinfiltration_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10321_qinfil_snow.pdf", width = 6, height = 4)
quantiles_winter_X10321_qinfil <- quantile(data_merged_rol_winter$rol_qinfiltration_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_winter_X10321_qinfil) <- seq(0, 1, by = 0.1)
interact_plot(model = model_winter_ps_intera_21, pred = rol_qinfiltration_X10321, modx = rol_snowstorage_X10321,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "SD, wi", legend.main = "snow, SD, wi", outcome.scale = "link") +
  ylim(0.5, 5.5) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_winter_X10321_qinfil[-c(2, 3, 5)]),
                                         breaks = quantiles_winter_X10321_qinfil[-c(2, 3, 5)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10303_qinfil_snow.pdf", width = 6, height = 4)
quantiles_winter_X10303_qinfil <- quantile(data_merged_rol_winter$rol_qinfiltration_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_winter_X10303_qinfil) <- seq(0, 1, by = 0.1)
interact_plot(model = model_winter_ps_intera_03, pred = rol_qinfiltration_X10303, modx = rol_snowstorage_X10303,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "BT, wi", legend.main = "snow, BT, wi", outcome.scale = "link") +
  ylim(-1.2, 3.8) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_winter_X10303_qinfil[-c(2, 3, 5)]),
                                         breaks = quantiles_winter_X10303_qinfil[-c(2, 3, 5)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10302_qinfil.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_02, select = 5, ylim = c(-3, 2), xlab = "MU, wi", ylab = "s(qinfil)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_qinfiltration_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10304_qinfil_glorad.pdf", width = 6, height = 4)
quantiles_summer_X10304_qinfil <- quantile(data_merged_rol_summer$rol_qinfiltration_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10304_qinfil) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_04, pred = rol_qinfiltration_X10304, modx = rol_glorad_X10304,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MW, su", legend.main = "glorad, MW, su", outcome.scale = "link") +
  ylim(0.5, 5.5) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10304_qinfil[-c(5)]),
                                         breaks = quantiles_summer_X10304_qinfil[-c(5)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10321_qinfil_snow.pdf", width = 6, height = 4)
quantiles_summer_X10321_qinfil <- quantile(data_merged_rol_summer$rol_qinfiltration_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10321_qinfil) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_21, pred = rol_qinfiltration_X10321, modx = rol_snowstorage_X10321,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "SD, su", legend.main = "snow, SD, su", outcome.scale = "link") +
  ylim(-0.5, 4.5) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10321_qinfil[-c(5)]),
                                         breaks = quantiles_summer_X10321_qinfil[-c(5)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10303_qinfil.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_03, select = 5, ylim = c(-3, 2), xlab = "BT, su", ylab = "s(qinfil)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_qinfiltration_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10302_qinfil.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_02, select = 5, ylim = c(-3, 2), xlab = "MU, su", ylab = "s(qinfil)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_qinfiltration_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()



# relhum
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10304_relhum.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_04, select = 6, ylim = c(-0.25, 0.7), xlab = "MW, wi", ylab = "s(relhum)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_relhum_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10321_relhum.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_21, select = 6, ylim = c(-0.25, 0.7), xlab = "SD, wi", ylab = "s(relhum)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_relhum_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10303_relhum.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_03, select = 6, ylim = c(-0.25, 0.7), xlab = "BT, wi", ylab = "s(relhum)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_relhum_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10302_relhum.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_02, select = 6, ylim = c(-0.25, 0.7), xlab = "MU, wi", ylab = "s(relhum)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_relhum_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10304_relhum.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_04, select = 6, ylim = c(-0.25, 0.7), xlab = "MW, su", ylab = "s(relhum)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_relhum_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10321_relhum.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_21, select = 6, ylim = c(-0.25, 0.7), xlab = "SD, su", ylab = "s(relhum)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_relhum_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10303_relhum_soilunsat.pdf", width = 6, height = 4)
quantiles_summer_X10303_relhum <- quantile(data_merged_rol_summer$rol_relhum_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10303_relhum) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_03, pred = rol_relhum_X10303, modx = rol_soilwaterunsatzone_X10303,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "BT, su", legend.main = "soilunsat, BT, su", outcome.scale = "link") +
  ylim(3, 3.95) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10303_relhum[-c(4, 6, 8)]),
                                         breaks = quantiles_summer_X10303_relhum[-c(4, 6, 8)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10302_relhum_soilunsat.pdf", width = 6, height = 4)
quantiles_summer_X10302_relhum <- quantile(data_merged_rol_summer$rol_relhum_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10302_relhum) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_02, pred = rol_relhum_X10302, modx = rol_soilwaterunsatzone_X10302,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MU, su", legend.main = "soilunsat, MU, su", outcome.scale = "link") +
  ylim(4.15, 5.1) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10302_relhum[-c(4, 6, 8)]),
                                         breaks = quantiles_summer_X10302_relhum[-c(4, 6, 8)])) +
  theme(legend.position = "bottom")
dev.off()



# snowstorage
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10304_snow_soilunsat.pdf", width = 6, height = 4)
quantiles_winter_X10304_snow <- quantile(data_merged_rol_winter$rol_snowstorage_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_winter_X10304_snow) <- seq(0, 1, by = 0.1)
interact_plot(model = model_winter_ps_intera_04, pred = rol_snowstorage_X10304, modx = rol_soilwaterunsatzone_X10304,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MW, wi", legend.main = "soilunsat, MW, wi", outcome.scale = "link") +
  ylim(1.2, 2.7) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_winter_X10304_snow[-c(2)]),
                                         breaks = quantiles_winter_X10304_snow[-c(2)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10321_snow_qinfil.pdf", width = 6, height = 4)
quantiles_winter_X10321_snow <- quantile(data_merged_rol_winter$rol_snowstorage_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_winter_X10321_snow) <- seq(0, 1, by = 0.1)
interact_plot(model = model_winter_ps_intera_21, pred = rol_snowstorage_X10321, modx = rol_qinfiltration_X10321,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "SD, wi", legend.main = "qinfil, SD, wi", outcome.scale = "link") +
  ylim(2, 3.5) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_winter_X10321_snow[-c(2)]),
                                         breaks = quantiles_winter_X10321_snow[-c(2)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10303_snow_qinfil.pdf", width = 6, height = 4)
quantiles_winter_X10303_snow <- quantile(data_merged_rol_winter$rol_snowstorage_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_winter_X10303_snow) <- seq(0, 1, by = 0.1)
interact_plot(model = model_winter_ps_intera_03, pred = rol_snowstorage_X10303, modx = rol_qinfiltration_X10303,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "BT, wi", legend.main = "qinfil, BT, wi", outcome.scale = "link") +
  ylim(2.8, 4.3) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_winter_X10303_snow[-c(2)]),
                                         breaks = quantiles_winter_X10303_snow[-c(2)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10302_snow_grdepth.pdf", width = 6, height = 4)
quantiles_winter_X10302_snow <- quantile(data_merged_rol_winter$rol_snowstorage_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_winter_X10302_snow) <- seq(0, 1, by = 0.1)
interact_plot(model = model_winter_ps_intera_02, pred = rol_snowstorage_X10302, modx = rol_groundwaterdepth_X10302,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MU, wi", legend.main = "grdepth, MU, wi", outcome.scale = "link") +
  ylim(3.5, 5) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_winter_X10302_snow[-c(2, 3, 5)]),
                                         breaks = quantiles_winter_X10302_snow[-c(2, 3, 5)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10304_snow_glorad.pdf", width = 6, height = 4)
quantiles_summer_X10304_snow <- quantile(data_merged_rol_summer$rol_snowstorage_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10304_snow) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_04, pred = rol_snowstorage_X10304, modx = rol_glorad_X10304,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MW, su", legend.main = "glorad, MW, su", outcome.scale = "link") +
  ylim(2.25, 3.75) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10304_snow[-c(2, 3, 4, 5, 6, 7)]),
                                         breaks = quantiles_summer_X10304_snow[-c(2, 3, 4, 5, 6, 7)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10321_snow_glorad.pdf", width = 6, height = 4)
quantiles_summer_X10321_snow <- quantile(data_merged_rol_summer$rol_snowstorage_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10321_snow) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_21, pred = rol_snowstorage_X10321, modx = rol_glorad_X10321,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "SD, su", legend.main = "glorad, SD, su", outcome.scale = "link") +
  ylim(2.5, 4) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10321_snow[-c(2, 3, 4, 5, 6, 7)]),
                                         breaks = quantiles_summer_X10321_snow[-c(2, 3, 4, 5, 6, 7)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10303_snow_precip.pdf", width = 6, height = 4)
quantiles_summer_X10303_snow <- quantile(data_merged_rol_summer$rol_snowstorage_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10303_snow) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_03, pred = rol_snowstorage_X10303, modx = rol_precip_X10303,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "BT, su", legend.main = "precip, BT, su", outcome.scale = "link") +
  ylim(3.35, 4.85) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10303_snow[-c(2, 3, 4, 5, 6, 7)]),
                                         breaks = quantiles_summer_X10303_snow[-c(2, 3, 4, 5, 6, 7)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10302_snow_precip.pdf", width = 6, height = 4)
quantiles_summer_X10302_snow <- quantile(data_merged_rol_summer$rol_snowstorage_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10302_snow) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_02, pred = rol_snowstorage_X10302, modx = rol_precip_X10302,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MU, su", legend.main = "precip, MU, su", outcome.scale = "link") +
  ylim(3.8, 5.3) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10302_snow[-c(2, 3, 4, 5, 6, 7, 8, 9)]),
                                         breaks = quantiles_summer_X10302_snow[-c(2, 3, 4, 5, 6, 7, 8, 9)])) +
  theme(legend.position = "bottom")
dev.off()



# soilwaterrootzone
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10304_soilroot_snow.pdf", width = 6, height = 4)
quantiles_winter_X10304_soilroot <- quantile(data_merged_rol_winter$rol_soilwaterrootzone_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_winter_X10304_soilroot) <- seq(0, 1, by = 0.1)
interact_plot(model = model_winter_ps_intera_04, pred = rol_soilwaterrootzone_X10304, modx = rol_snowstorage_X10304,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MW, wi", legend.main = "snow, MW, wi", outcome.scale = "link") +
  ylim(1.2, 3.2) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_winter_X10304_soilroot),
                                         breaks = quantiles_winter_X10304_soilroot)) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10321_soilroot.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_21, select = 8, ylim = c(-0.8, 1.2), xlab = "SD, wi", ylab = "s(soilroot)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_soilwaterrootzone_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10303_soilroot.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_03, select = 8, ylim = c(-0.8, 1.2), xlab = "BT, wi", ylab = "s(soilroot)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_soilwaterrootzone_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10302_soilroot_airtmp.pdf", width = 6, height = 4)
quantiles_winter_X10302_soilroot <- quantile(data_merged_rol_winter$rol_soilwaterrootzone_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_winter_X10302_soilroot) <- seq(0, 1, by = 0.1)
interact_plot(model = model_winter_ps_intera_02, pred = rol_soilwaterrootzone_X10302, modx = rol_airtmp_X10302,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MU, wi", legend.main = "airtmp, MU, wi", outcome.scale = "link") +
  ylim(2.75, 4.75) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_winter_X10302_soilroot)[-c(3, 5, 6, 8, 9)],
                                         breaks = quantiles_winter_X10302_soilroot[-c(3, 5, 6, 8, 9)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10304_soilroot.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_04, select = 8, ylim = c(-0.8, 1.2), xlab = "MW, su", ylab = "s(soilroot)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_soilwaterrootzone_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10321_soilroot.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_21, select = 8, ylim = c(-0.8, 1.2), xlab = "SD, su", ylab = "s(soilroot)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_soilwaterrootzone_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10303_soilroot.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_03, select = 8, ylim = c(-0.8, 1.2), xlab = "BT, su", ylab = "s(soilroot)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_soilwaterrootzone_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10302_soilroot.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_02, select = 8, ylim = c(-0.8, 1.2), xlab = "MU, su", ylab = "s(soilroot)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_soilwaterrootzone_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()



# soilwaterunsatzone
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10304_soilunsat_snow.pdf", width = 6, height = 4)
quantiles_winter_X10304_soilunsat <- quantile(data_merged_rol_winter$rol_soilwaterunsatzone_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_winter_X10304_soilunsat) <- seq(0, 1, by = 0.1)
interact_plot(model = model_winter_ps_intera_04, pred = rol_soilwaterunsatzone_X10304, modx = rol_snowstorage_X10304,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MW, wi", legend.main = "snow, MW, wi", outcome.scale = "link") +
  ylim(1.3, 2.9) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_winter_X10304_soilunsat)[-c(4, 6, 8)],
                                         breaks = quantiles_winter_X10304_soilunsat[-c(4, 6, 8)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10321_soilunsat_qinfil.pdf", width = 6, height = 4)
quantiles_winter_X10321_soilunsat <- quantile(data_merged_rol_winter$rol_soilwaterunsatzone_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_winter_X10321_soilunsat) <- seq(0, 1, by = 0.1)
interact_plot(model = model_winter_ps_intera_21, pred = rol_soilwaterunsatzone_X10321, modx = rol_qinfiltration_X10321,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "SD, wi", legend.main = "qinfil, SD, wi", outcome.scale = "link") +
  ylim(1.65, 3.25) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_winter_X10321_soilunsat)[-c(4, 6, 8)],
                                         breaks = quantiles_winter_X10321_soilunsat[-c(4, 6, 8)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10303_soilunsat.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_03, select = 9, ylim = c(-0.9, 0.7), xlab = "BT, wi", ylab = "s(soilunsat)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_soilwaterunsatzone_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/winter_X10302_soilunsat.pdf", width = 6, height = 4)
plot(model_winter_ps_intera_02, select = 9, ylim = c(-0.9, 0.7), xlab = "MU, wi", ylab = "s(soilunsat)")
axis(side = 3, at = quantile(data_merged_rol_winter$rol_soilwaterunsatzone_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10304_soilunsat.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_04, select = 9, ylim = c(-0.9, 0.7), xlab = "MW, su", ylab = "s(soilunsat)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_soilwaterunsatzone_X10304, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10321_soilunsat.pdf", width = 6, height = 4)
plot(model_summer_ps_intera_21, select = 9, ylim = c(-0.9, 0.7), xlab = "SD, su", ylab = "s(soilunsat)")
axis(side = 3, at = quantile(data_merged_rol_summer$rol_soilwaterunsatzone_X10321, probs = seq(0, 1, by = 0.1), na.rm = TRUE),
     labels = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"), tick = TRUE)
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10303_soilunsat_relhum.pdf", width = 6, height = 4)
quantiles_summer_X10303_soilunsat <- quantile(data_merged_rol_summer$rol_soilwaterunsatzone_X10303, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10303_soilunsat) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_03, pred = rol_soilwaterunsatzone_X10303, modx = rol_relhum_X10303,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "BT, su", legend.main = "relhum, BT, su", outcome.scale = "link") +
  ylim(2.5, 4.1) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10303_soilunsat)[-c(6, 8)],
                                         breaks = quantiles_summer_X10303_soilunsat[-c(6, 8)])) +
  theme(legend.position = "bottom")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects2/summer_X10302_soilunsat_relhum.pdf", width = 6, height = 4)
quantiles_summer_X10302_soilunsat <- quantile(data_merged_rol_summer$rol_soilwaterunsatzone_X10302, probs = seq(0, 1, by = 0.1), na.rm = TRUE)
names(quantiles_summer_X10302_soilunsat) <- seq(0, 1, by = 0.1)
interact_plot(model = model_summer_ps_intera_02, pred = rol_soilwaterunsatzone_X10302, modx = rol_relhum_X10302,
              interval = TRUE, int.type = "confidence", y.label = "linear predictor\n(other variables at mean)", x.label = "MU, su", legend.main = "relhum, MU, su", outcome.scale = "link") +
  ylim(4, 5.6) +
  scale_x_continuous(sec.axis = dup_axis(name = "", labels = names(quantiles_summer_X10302_soilunsat)[-c(6, 8)],
                                         breaks = quantiles_summer_X10302_soilunsat[-c(6, 8)])) +
  theme(legend.position = "bottom")
dev.off()


