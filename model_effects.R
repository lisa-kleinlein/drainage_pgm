library(interactions)
library(ggplot2)
library(devEMF)

source("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/model_chosen.R")

# Winter, Mittenwald
model_winter_ps_intera_04$coefficients[2]
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10304_airtmp.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_04, select = 1, ylim = c(-0.2, 0.6), xlab = "airtmp_MW", ylab = "s(airtmp_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10304_glorad.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_04, select = 2, ylim = c(-0.5, 0.6), xlab = "glorad_MW", ylab = "s(glorad_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10304_grdepth.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_04, select = 3, ylim = c(-0.15, 0.6), xlab = "grdepth_MW", ylab = "s(grdepth_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10304_precip.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_04, select = 4, ylim = c(-0.1, 0.8), xlab = "precip_MW", ylab = "s(precip_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10304_qinfil.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_04, select = 5, ylim = c(-0.5, 1.6), xlab = "qinfil_MW", ylab = "s(qinfil_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10304_relhum.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_04, select = 6, ylim = c(-0.1, 0.7), xlab = "relhum_MW", ylab = "s(relhum_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10304_snow_soilunsat.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_04, pred = rol_snowstorage_X10304, modx = rol_soilwaterunsatzone_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_MW)", x.label = "snow_MW", legend.main = "soilunsat_MW", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10304_soilunsat_snow.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_04, pred = rol_soilwaterunsatzone_X10304, modx = rol_snowstorage_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(soilunsat_MW)", x.label = "soilunsat_MW", legend.main = "snow_MW", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10304_snow_soilroot.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_04, pred = rol_snowstorage_X10304, modx = rol_soilwaterrootzone_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_MW)", x.label = "snow_MW", legend.main = "soilroot_MW", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10304_soilroot_snow.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_04, pred = rol_soilwaterrootzone_X10304, modx = rol_snowstorage_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(soilroot_MW)", x.label = "soilroot_MW", legend.main = "snow_MW", outcome.scale = "link")
dev.off()



# Winter, Schlehdorf
model_winter_ps_intera_21$coefficients[2]
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10321_airtmp.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_21, select = 1, ylim = c(-0.1, 0.5), xlab = "airtmp_SD", ylab = "s(airtmp_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10321_glorad.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_21, select = 2, ylim = c(-0.3, 0.8), xlab = "glorad_SD", ylab = "s(glorad_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10321_grdepth.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_21, select = 3, ylim = c(-0.08, 0.2), xlab = "grdepth_SD", ylab = "s(grdepth_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10321_precip.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_21, select = 4, ylim = c(-0.5, 3.5), xlab = "precip_SD", ylab = "s(precip_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10321_relhum.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_21, select = 6, ylim = c(-0.2, 0.15), xlab = "relhum_SD", ylab = "s(relhum_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10321_soilroot.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_21, select = 8, ylim = c(-0.8, 1.2), xlab = "soilroot_SD", ylab = "s(soilroot_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10321_snow_qinfil.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_21, pred = rol_snowstorage_X10321, modx = rol_qinfiltration_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_SD)", x.label = "snow_SD", legend.main = "qinfil_SD", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10321_qinfil_snow.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_21, pred = rol_qinfiltration_X10321, modx = rol_snowstorage_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(qinfil_SD)", x.label = "qinfil_SD", legend.main = "snow_SD", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10321_qinfil_soilunsat.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_21, pred = rol_qinfiltration_X10321, modx = rol_soilwaterunsatzone_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(qinfil_SD)", x.label = "qinfil_SD", legend.main = "soilunsat_SD", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10321_soilunsat_qinfil.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_21, pred = rol_soilwaterunsatzone_X10321, modx = rol_qinfiltration_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(soilunsat_SD)", x.label = "soilunsat_SD", legend.main = "qinfil_SD", outcome.scale = "link")
dev.off()



# Winter, Bad Tölz
model_winter_ps_intera_03$coefficients[2]
model_winter_ps_intera_03$coefficients[3]
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10303_airtmp.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_03, select = 1, ylim = c(-0.1, 0.42), xlab = "airtmp_BT", ylab = "s(airtmp_BT)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10303_glorad.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_03, select = 2, ylim = c(-0.12, 0.12), xlab = "glorad_BT", ylab = "s(glorad_BT)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10303_grdepth.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_03, select = 3, ylim = c(-0.25, 0.2), xlab = "grdepth_BT", ylab = "s(grdepth_BT)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10303_relhum.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_03, select = 6, ylim = c(-0.2, 0.15), xlab = "relhum_BT", ylab = "s(relhum_BT)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10303_soilroot.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_03, select = 8, ylim = c(-0.35, 0.6), xlab = "soilroot_BT", ylab = "s(soilroot_BT)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10303_soilunsat.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_03, select = 9, ylim = c(-0.25, 0.07), xlab = "soilunsat_BT", ylab = "s(soilunsat_BT)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10303_qinfil_snow.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_03, pred = rol_qinfiltration_X10303, modx = rol_snowstorage_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(qinfil_BT)", x.label = "qinfil_BT", legend.main = "snow_BT", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10303_snow_qinfil.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_03, pred = rol_snowstorage_X10303, modx = rol_qinfiltration_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_BT)", x.label = "snow_BT", legend.main = "qinfil_BT", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10303_qinfil_precip.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_03, pred = rol_qinfiltration_X10303, modx = rol_precip_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(qinfil_BT)", x.label = "qinfil_BT", legend.main = "precip_BT", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10303_precip_qinfil.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_03, pred = rol_precip_X10303, modx = rol_qinfiltration_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(precip_BT)", x.label = "precip_BT", legend.main = "qinfil_BT", outcome.scale = "link")
dev.off()








# Winter, Munich
model_winter_ps_intera_02$coefficients[2]
model_winter_ps_intera_02$coefficients[3]
model_winter_ps_intera_02$coefficients[4]
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10302_glorad.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_02, select = 2, ylim = c(-0.1, 0.4), xlab = "glorad_MU", ylab = "s(glorad_MU)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10302_precip.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_02, select = 4, ylim = c(-0.1, 1), xlab = "precip_MU", ylab = "s(precip_MU)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10302_qinfil.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_02, select = 5, ylim = c(-0.7, 0.5), xlab = "qinfil_MU", ylab = "s(qinfil_MU)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10302_relhum.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_02, select = 6, ylim = c(-0.25, 0.1), xlab = "relhum_MU", ylab = "s(relhum_MU)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10302_soilunsat.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_02, select = 9, ylim = c(-0.5, 0.3), xlab = "soilunsat_MU", ylab = "s(soilunsat_MU)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10302_soilroot_airtmp.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_02, pred = rol_soilwaterrootzone_X10302, modx = rol_airtmp_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(soilroot_MU)", x.label = "soilroot_MU", legend.main = "airtmp_MU", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10302_airtmp_soilroot.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_02, pred = rol_airtmp_X10302, modx = rol_soilwaterrootzone_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(airtmp_MU)", x.label = "airtmp_MU", legend.main = "soilroot_MU", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10302_snow_grdepth.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_02, pred = rol_snowstorage_X10302, modx = rol_groundwaterdepth_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_MU)", x.label = "snow_MU", legend.main = "grdepth_MU", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/winter_X10302_grdepth_snow.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_02, pred = rol_groundwaterdepth_X10302, modx = rol_snowstorage_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(grdepth_MU)", x.label = "grdepth_MU", legend.main = "snow_MU", outcome.scale = "link")
dev.off()






# Summer, Mittenwald
model_summer_ps_intera_04$coefficients[2]
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10304_airtmp.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_04, select = 1, ylim = c(-0.15, 0.1), xlab = "airtmp_MW", ylab = "s(airtmp_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10304_grdepth.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_04, select = 3, ylim = c(-0.3, 0.35), xlab = "grdepth_MW", ylab = "s(grdepth_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10304_precip.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_04, select = 4, ylim = c(-0.6, 0.7), xlab = "precip_MW", ylab = "s(precip_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10304_relhum.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_04, select = 6, ylim = c(-0.08, 0.18), xlab = "relhum_MW", ylab = "s(relhum_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10304_soilroot.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_04, select = 8, ylim = c(-0.5, 0.35), xlab = "soilroot_MW", ylab = "s(soilroot_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10304_soilunsat.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_04, select = 9, ylim = c(-0.15, 0.15), xlab = "soilunsat_MW", ylab = "s(soilunsat_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10304_glorad_snow.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_04, pred = rol_glorad_X10304, modx = rol_snowstorage_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(glorad_MW)", x.label = "glorad_MW", legend.main = "snow_MW", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10304_snow_glorad.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_04, pred = rol_snowstorage_X10304, modx = rol_glorad_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_MW)", x.label = "snow_MW", legend.main = "glorad_MW", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10304_glorad_qinfil.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_04, pred = rol_glorad_X10304, modx = rol_qinfiltration_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(glorad_MW)", x.label = "glorad_MW", legend.main = "qinfil_MW", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10304_qinfil_glorad.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_04, pred = rol_qinfiltration_X10304, modx = rol_glorad_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(qinfil_MW)", x.label = "qinfil_MW", legend.main = "glorad_MW", outcome.scale = "link")
dev.off()


# Summer, Schlehdorf
model_summer_ps_intera_21$coefficients[2]
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10321_airtmp.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_21, select = 1, ylim = c(-0.15, 0.12), xlab = "airtmp_SD", ylab = "s(airtmp_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10321_grdepth.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_21, select = 3, ylim = c(-0.2, 0.12), xlab = "grdepth_SD", ylab = "s(grdepth_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10321_precip.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_21, select = 4, ylim = c(-0.2, 4), xlab = "precip_SD", ylab = "s(precip_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10321_relhum.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_21, select = 6, ylim = c(-0.12, 0.3), xlab = "relhum_SD", ylab = "s(relhum_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10321_soilroot.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_21, select = 8, ylim = c(-0.5, 0.27), xlab = "soilroot_SD", ylab = "s(soilroot_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10321_soilunsat.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_21, select = 9, ylim = c(-0.05, 0.2), xlab = "soilunsat_SD", ylab = "s(soilunsat_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10321_snow_glorad.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_21, pred = rol_snowstorage_X10321, modx = rol_glorad_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_SD)", x.label = "snow_SD", legend.main = "glorad_SD", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10321_glorad_snow.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_21, pred = rol_glorad_X10321, modx = rol_snowstorage_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(glorad_SD)", x.label = "glorad_SD", legend.main = "snow_SD", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10321_snow_qinfil.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_21, pred = rol_snowstorage_X10321, modx = rol_qinfiltration_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_SD)", x.label = "snow_SD", legend.main = "qinfil_SD", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10321_qinfil_snow.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_21, pred = rol_qinfiltration_X10321, modx = rol_snowstorage_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(qinfil_SD)", x.label = "qinfil_SD", legend.main = "snow_SD", outcome.scale = "link")
dev.off()





# Summer, Bad Tölz
model_summer_ps_intera_03$coefficients[2]
model_summer_ps_intera_03$coefficients[3]
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10303_airtmp.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_03, select = 1, ylim = c(-0.1, 0.18), xlab = "airtmp_BT", ylab = "s(airtmp_BT)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10303_glorad.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_03, select = 2, ylim = c(-0.18, 0.1), xlab = "glorad_BT", ylab = "s(glorad_BT)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10303_grdepth.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_03, select = 3, ylim = c(-0.07, 0.08), xlab = "grdepth_BT", ylab = "s(grdepth_BT)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10303_qinfil.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_03, select = 5, ylim = c(-2.55, 0.3), xlab = "qinfil_BT", ylab = "s(qinfil_BT)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10303_soilroot.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_03, select = 8, ylim = c(-0.5, 0.25), xlab = "soilroot_BT", ylab = "s(soilroot_BT)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10303_relhum_soilunsat.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_03, pred = rol_relhum_X10303, modx = rol_soilwaterunsatzone_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(relhum_BT)", x.label = "relhum_BT", legend.main = "soilunsat_BT", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10303_soilunsat_relhum.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_03, pred = rol_soilwaterunsatzone_X10303, modx = rol_relhum_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(soilunsat_BT)", x.label = "soilunsat_BT", legend.main = "relhum_BT", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10303_snow_precip.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_03, pred = rol_snowstorage_X10303, modx = rol_precip_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_BT)", x.label = "snow_BT", legend.main = "precip_BT", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10303_precip_snow.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_03, pred = rol_precip_X10303, modx = rol_snowstorage_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(precip_BT)", x.label = "precip_BT", legend.main = "snow_BT", outcome.scale = "link")
dev.off()




# Summer, Munich
model_summer_ps_intera_02$coefficients[2]
model_summer_ps_intera_02$coefficients[3]
model_summer_ps_intera_02$coefficients[4]
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10302_airtmp.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_02, select = 1, ylim = c(-0.12, 0.32), xlab = "airtmp_MU", ylab = "s(airtmp_MU)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10302_glorad.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_02, select = 2, ylim = c(-0.2, 0.25), xlab = "glorad_MU", ylab = "s(glorad_MU)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10302_grdepth.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_02, select = 3, ylim = c(-0.12, 0.25), xlab = "grdepth_MU", ylab = "s(grdepth_MU)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10302_qinfil.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_02, select = 5, ylim = c(-1.2, 0.7), xlab = "qinfil_MU", ylab = "s(qinfil_MU)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10302_soilroot.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_02, select = 8, ylim = c(-0.3, 0.2), xlab = "soilroot_MU", ylab = "s(soilroot_MU)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10302_precip_snow.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_02, pred = rol_precip_X10302, modx = rol_snowstorage_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(precip_MU)", x.label = "precip_MU", legend.main = "snow_MU", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10302_snow_precip.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_02, pred = rol_snowstorage_X10302, modx = rol_precip_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_MU)", x.label = "snow_MU", legend.main = "precip_MU", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10302_relhum_soilunsat.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_02, pred = rol_relhum_X10302, modx = rol_soilwaterunsatzone_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(relhum_MU)", x.label = "relhum_MU", legend.main = "soilunsat_MU", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/eval/effects/summer_X10302_soilunsat_relhum.pdf", width = 6, height = 4.5)
interact_plot(model = model_summer_ps_intera_02, pred = rol_soilwaterunsatzone_X10302, modx = rol_relhum_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(soilunsat_MU)", x.label = "soilunsat_MU", legend.main = "relhum_MU", outcome.scale = "link")
dev.off()



# Comparison of effects

# airtmp in summer
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/summer_X10304_airtmp_comp.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_04, select = 1, ylim = c(-0.15, 0.32), xlab = "airtmp_MW", ylab = "s(airtmp_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/summer_X10321_airtmp_comp.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_21, select = 1, ylim = c(-0.15, 0.32), xlab = "airtmp_SD", ylab = "s(airtmp_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/summer_X10303_airtmp_comp.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_03, select = 1, ylim = c(-0.15, 0.32), xlab = "airtmp_BT", ylab = "s(airtmp_BT)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/summer_X10302_airtmp_comp.pdf", width = 6, height = 4.5)
plot(model_summer_ps_intera_02, select = 1, ylim = c(-0.15, 0.32), xlab = "airtmp_MU", ylab = "s(airtmp_MU)")
dev.off()

# relhum in winter
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/winter_X10304_relhum_comp.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_04, select = 6, ylim = c(-0.25, 0.7), xlab = "relhum_MW", ylab = "s(relhum_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/winter_X10321_relhum_comp.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_21, select = 6, ylim = c(-0.25, 0.7), xlab = "relhum_SD", ylab = "s(relhum_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/winter_X10303_relhum_comp.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_03, select = 6, ylim = c(-0.25, 0.7), xlab = "relhum_BT", ylab = "s(relhum_BT)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/winter_X10302_relhum_comp.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_02, select = 6, ylim = c(-0.25, 0.7), xlab = "relhum_MU", ylab = "s(relhum_MU)")
dev.off()

# precip im winter
pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/winter_X10304_precip_comp.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_04, select = 4, ylim = c(-0.5, 3.5), xlab = "precip_MW", ylab = "s(precip_MW)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/winter_X10321_precip_comp.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_21, select = 4, ylim = c(-0.5, 3.5), xlab = "precip_SD", ylab = "s(precip_SD)")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/winter_X10303_precip_qinfil_comp.pdf", width = 6, height = 4.5)
interact_plot(model = model_winter_ps_intera_03, pred = rol_precip_X10303, modx = rol_qinfiltration_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(precip_BT)", x.label = "precip_BT", legend.main = "qinfil_BT", outcome.scale = "link")
dev.off()

pdf(file = "C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/winter_X10302_precip_comp.pdf", width = 6, height = 4.5)
plot(model_winter_ps_intera_02, select = 4, ylim = c(-0.5, 3.5), xlab = "precip_MU", ylab = "s(precip_MU)")
dev.off()



# effect visualization
# airtmp
plot(model_winter_ps_intera_04, select = 1, ylim = c(-0.2, 0.6), xlab = "airtmp_MW", ylab = "s(airtmp_MW)")
abline(v = c(-16.8, -11.5, 10.5, 10.8))
ecdf(data_merged_rol_winter$rol_airtmp_X10304)(c(-16.8, -11.5, 10.5, 10.8))

plot(model_winter_ps_intera_21, select = 1, ylim = c(-0.1, 0.5), xlab = "airtmp_SD", ylab = "s(airtmp_SD)")
abline(v = c(-16.3, -11))
ecdf(data_merged_rol_winter$rol_airtmp_X10321)(c(-16.3, -11))

plot(model_winter_ps_intera_03, select = 1, ylim = c(-0.1, 0.42), xlab = "airtmp_BT", ylab = "s(airtmp_BT)")
abline(v = c(-15.95, -12.3, 0, 5))
ecdf(data_merged_rol_winter$rol_airtmp_X10303)(c(-15.95, -12.3, 0, 5))

interact_plot(model = model_winter_ps_intera_02, pred = rol_airtmp_X10302, modx = rol_soilwaterrootzone_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(airtmp_MU)", x.label = "airtmp_MU", legend.main = "soilroot_MU", outcome.scale = "link") +
  geom_vline(xintercept = c(-2))
ecdf(data_merged_rol_winter$rol_airtmp_X10302)(-2)

plot(model_summer_ps_intera_04, select = 1, ylim = c(-0.15, 0.1), xlab = "airtmp_MW", ylab = "s(airtmp_MW)")
abline(v = c(1.3, 2.9, 9.5, 15))
ecdf(data_merged_rol_summer$rol_airtmp_X10304)(c(1.3, 2.9, 9.5, 15))

plot(model_summer_ps_intera_21, select = 1, ylim = c(-0.15, 0.12), xlab = "airtmp_SD", ylab = "s(airtmp_SD)")
abline(v = c(11, 17))
ecdf(data_merged_rol_summer$rol_airtmp_X10321)(c(11, 17))

plot(model_summer_ps_intera_03, select = 1, ylim = c(-0.1, 0.18), xlab = "airtmp_BT", ylab = "s(airtmp_BT)")
abline(v = c(2, 16.8))
ecdf(data_merged_rol_summer$rol_airtmp_X10303)(c(2, 16.8))

plot(model_summer_ps_intera_02, select = 1, ylim = c(-0.12, 0.32), xlab = "airtmp_MU", ylab = "s(airtmp_MU)")
abline(v = c(7.2, 21))
ecdf(data_merged_rol_summer$rol_airtmp_X10302)(c(7.2, 21))



data_effects_viz_airtmp <- data.frame(hight_catch_half = c(rep(1, 4), rep(2, 2), rep(3, 4), rep(4, 2),
                                                            rep(1, 4), rep(2, 2), rep(3, 2), rep(4, 2)),
                                      quantile = c(0.001, 0.008, 0.9997, 0.99991,
                                                   0.0004, 0.006,
                                                   0.0004, 0.003, 0.513, 0.874,
                                                   0, 0.169,
                                                   0.019, 0.048, 0.511, 0.969,
                                                   0.452, 0.963,
                                                   0.008, 0.952,
                                                   0.058, 0.985),
                                      airtmp = c(-16.8, -11.5, 10.5, 10.8,
                                                 -16.3, -11,
                                                 -15.95, -12.3, 0, 5,
                                                 -13.4194, -2,
                                                 1.3, 2.9, 9.5, 15,
                                                 11, 17,
                                                 2, 16.8,
                                                 7.2, 21),
                                      catch_half_numb = c(rep("MW_winter_1", 2), rep("MW_winter_2", 2),
                                                          rep("SD_winter_1", 2),
                                                          rep("BT_winter_1", 2), rep("BT_winter_2", 2),
                                                          rep("MU_winter_1", 2),
                                                          rep("MW_summer_1", 2), rep("MW_summer_2", 2),
                                                          rep("SD_summer_1", 2),
                                                          rep("BT_summer_1", 2),
                                                          rep("MU_summer_1", 2)),
                                      catch = c(rep("MW", 4), rep("SD", 2), rep("BT", 4), rep("MU", 2),
                                                rep("MW", 4), rep("SD", 2), rep("BT", 2), rep("MU", 2)),
                                      half = c(rep("winter", 12),
                                                rep("summer", 10)))
ggplot(data_effects_viz_airtmp, aes(x = quantile, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Quantile of respective airtmp",
       y = "Negative effect for minimal increase of airtmp") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/airtmp_quantiles.pdf"), width = 20, height = 9)

ggplot(data_effects_viz_airtmp, aes(x = airtmp, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Respective airtmp",
       y = "Negative effect for minimal increase of airtmp") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/airtmp_values.pdf"), width = 20, height = 9)

# with soilwaterrootzone at mean (Munich in winter)

# glorad
plot(model_winter_ps_intera_04, select = 2, ylim = c(-0.5, 0.6), xlab = "glorad_MW", ylab = "s(glorad_MW)")
abline(v = c(135, 185))
ecdf(data_merged_rol_winter$rol_glorad_X10304)(c(135, 185))

plot(model_winter_ps_intera_21, select = 2, ylim = c(-0.3, 0.8), xlab = "glorad_SD", ylab = "s(glorad_SD)")
abline(v = c(120, 170))
ecdf(data_merged_rol_winter$rol_glorad_X10321)(c(120, 170))

plot(model_winter_ps_intera_03, select = 2, ylim = c(-0.12, 0.12), xlab = "glorad_BT", ylab = "s(glorad_BT)")
abline(v = c(120, 165, 310, 380, 550, 650))
ecdf(data_merged_rol_winter$rol_glorad_X10303)(c(120, 165, 310, 380, 550, 650))

plot(model_winter_ps_intera_02, select = 2, ylim = c(-0.1, 0.4), xlab = "glorad_MU", ylab = "s(glorad_MU)")
abline(v = c(55, 140))
ecdf(data_merged_rol_winter$rol_glorad_X10302)(c(55, 140))

interact_plot(model = model_summer_ps_intera_04, pred = rol_glorad_X10304, modx = rol_snowstorage_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(glorad_MW)", x.label = "glorad_MW", legend.main = "snow_MW", outcome.scale = "link") +
  geom_vline(xintercept = c(820, 860))
ecdf(data_merged_rol_summer$rol_glorad_X10304)(c(820, 860))

interact_plot(model = model_summer_ps_intera_21, pred = rol_glorad_X10321, modx = rol_snowstorage_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(glorad_SD)", x.label = "glorad_SD", legend.main = "snow_SD", outcome.scale = "link")

plot(model_summer_ps_intera_02, select = 2, ylim = c(-0.2, 0.25), xlab = "glorad_MU", ylab = "s(glorad_MU)")

plot(model_summer_ps_intera_03, select = 2, ylim = c(-0.18, 0.1), xlab = "glorad_BT", ylab = "s(glorad_BT)")


data_effects_viz_glorad <- data.frame(hight_catch_half = c(rep(1, 2), rep(2, 2), rep(3, 6), rep(4, 2),
                                                            rep(1, 2), rep(2, 0), rep(3, 0), rep(4, 0)),
                                      quantile = c(0.189, 0.415,
                                                   0.212, 0.430,
                                                   0.178, 0.398, 0.674, 0.766, 0.931, 0.981,
                                                   0.001, 0.370,
                                                   0.978, 0.989),
                                      glorad = c(135, 185,
                                                 120, 170,
                                                 120, 165, 310, 380, 550, 650,
                                                 55, 140,
                                                 820, 860),
                                      catch_half_numb = c(rep("MW_winter_1", 2),
                                                          rep("SD_winter_1", 2),
                                                          rep("BT_winter_1", 2), rep("BT_winter_2", 2), rep("BT_winter_3", 2),
                                                          rep("MU_winter_1", 2),
                                                          rep("MW_summer_1", 2)),
                                      catch = c(rep("MW", 2), rep("SD", 2), rep("BT", 6), rep("MU", 2),
                                                rep("MW", 2), rep("SD", 0), rep("BT", 0), rep("MU", 0)),
                                      half = c(rep("winter", 12),
                                                rep("summer", 2)))

ggplot(data_effects_viz_glorad, aes(x = quantile, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Quantile of respective glorad",
       y = "Negative effect for minimal increase of glorad") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/glorad_quantiles.pdf"), width = 20, height = 9)

ggplot(data_effects_viz_glorad, aes(x = glorad, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Respective glorad",
       y = "Negative effect for minimal increase of glorad") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/glorad_values.pdf"), width = 20, height = 9)

# with snowstorage (Schlehdorf and Mittenwald in summer) and qinfiltration at mean (Mittenwald in summer) 


# groundwaterdepth
plot(model_winter_ps_intera_04, select = 3, ylim = c(-0.15, 0.6), xlab = "grdepth_MW", ylab = "s(grdepth_MW)")
abline(v = c(-10.055, -9.99, -9.77, -9.68, -9.38, -9.26, -8.75, -8.625))
ecdf(data_merged_rol_winter$rol_groundwaterdepth_X10304)(c(-10.055, -9.99, -9.77, -9.68, -9.38, -9.26, -8.75, -8.625))

plot(model_winter_ps_intera_21, select = 3, ylim = c(-0.08, 0.2), xlab = "grdepth_SD", ylab = "s(grdepth_SD)")
abline(v = c(-7.945, -7.92, -7.73, -7.67, -7.27, -7.225))
ecdf(data_merged_rol_winter$rol_groundwaterdepth_X10321)(c(-7.945, -7.92, -7.73, -7.67, -7.27, -7.225))

plot(model_winter_ps_intera_03, select = 3, ylim = c(-0.25, 0.2), xlab = "grdepth_BT", ylab = "s(grdepth_BT)")
abline(v = c(-8.05, -7.79))
ecdf(data_merged_rol_winter$rol_groundwaterdepth_X10303)(c(-8.05, -7.79))

interact_plot(model = model_winter_ps_intera_02, pred = rol_groundwaterdepth_X10302, modx = rol_snowstorage_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(grdepth_MU)", x.label = "grdepth_MU", legend.main = "snow_MU", outcome.scale = "link") +
  geom_vline(xintercept = c(-6.8, -6.79, -6.58, -6.52, -6.23, -6.13))
ecdf(data_merged_rol_winter$rol_groundwaterdepth_X10302)(c(-6.8, -6.79, -6.58, -6.52, -6.23, -6.13))

plot(model_summer_ps_intera_04, select = 3, ylim = c(-0.3, 0.35), xlab = "grdepth_MW", ylab = "s(grdepth_MW)")
abline(v = c(-9.35, -9.25, -8.82, -8.73))
ecdf(data_merged_rol_summer$rol_groundwaterdepth_X10304)(c(-9.35, -9.25, -8.82, -8.73))

plot(model_summer_ps_intera_21, select = 3, ylim = c(-0.2, 0.12), xlab = "grdepth_SD", ylab = "s(grdepth_SD)")
abline(v = c(-7.24, -7.17))
ecdf(data_merged_rol_summer$rol_groundwaterdepth_X10321)(c(-7.24, -7.17))

plot(model_summer_ps_intera_03, select = 3, ylim = c(-0.07, 0.08), xlab = "grdepth_BT", ylab = "s(grdepth_BT)")
abline(v = c(-8.125, -8.04, -7.33, -7.25))
ecdf(data_merged_rol_summer$rol_groundwaterdepth_X10303)(c(-8.125, -8.04, -7.33, -7.25))

plot(model_summer_ps_intera_02, select = 3, ylim = c(-0.12, 0.25), xlab = "grdepth_MU", ylab = "s(grdepth_MU)")
abline(v = c(-6.812, -6.59, -6.415, -6.35, -6.23, -6.2))
ecdf(data_merged_rol_summer$rol_groundwaterdepth_X10302)(c(-6.812, -6.59, -6.415, -6.35, -6.23, -6.2))

data_effects_viz_groundwaterdepth <- data.frame(hight_catch_half = c(rep(1, 8), rep(2, 6), rep(3, 2), rep(4, 6),
                                                            rep(1, 4), rep(2, 2), rep(3, 4), rep(4, 6)),
                                      quantile = c(0.003, 0.013, 0.188, 0.296, 0.745, 0.856, 0.985, 0.997,
                                                   0.015, 0.036, 0.347, 0.488, 0.973, 0.990,
                                                   0.078, 0.455,
                                                   0.022, 0.026, 0.167, 0.275, 0.853, 0.999,
                                                   0.639, 0.721, 0.968, 0.984,
                                                   0.918, 0.967,
                                                   0.011, 0.043, 0.929, 0.946,
                                                   0.027, 0.175, 0.479, 0.664, 0.888, 0.922),
                                      grdepth = c(-10.055, -9.99, -9.77, -9.68, -9.38, -9.26, -8.75, -8.625,
                                                  -7.945, -7.92, -7.73, -7.67, -7.27, -7.225,
                                                  -8.05, -7.79,
                                                  -6.8, -6.79, -6.58, -6.52, -6.23, -6.13,
                                                  -9.35, -9.25, -8.82, -8.73,
                                                  -7.24, -7.17,
                                                  -8.125, -8.04, -7.33, -7.25,
                                                  -6.812, -6.59, -6.415, -6.35, -6.23, -6.2),
                                      catch_half_numb = c(rep("MW_winter_1", 2), rep("MW_winter_2", 2), rep("MW_winter_3", 2), rep("MW_winter_4", 2),
                                                           rep("SD_winter_1", 2), rep("SD_winter_2", 2), rep("SD_winter_3", 2),
                                                           rep("BT_winter_1", 2),
                                                           rep("MU_winter_1", 2), rep("MU_winter_2", 2), rep("MU_winter_3", 2),
                                                           rep("MW_summer_1", 2), rep("MW_summer_2", 2),
                                                           rep("SD_summer_1", 2),
                                                           rep("BT_summer_1", 2), rep("BT_summer_2", 2),
                                                           rep("MU_summer_1", 2), rep("MU_summer_2", 2), rep("MU_summer_3", 2)),
                                      catch = c(rep("MW", 8), rep("SD", 6), rep("BT", 2), rep("MU", 6),
                                                rep("MW", 4), rep("SD", 2), rep("BT", 4), rep("MU", 6)),
                                      half = c(rep("winter", 22),
                                                rep("summer", 16)))

ggplot(data_effects_viz_groundwaterdepth, aes(x = quantile, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Quantile of respective grdepth",
       y = "Negative effect for minimal increase of grdepth") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/grdepth_quantiles.pdf"), width = 20, height = 9)

ggplot(data_effects_viz_groundwaterdepth, aes(x = grdepth, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Respective grdepth",
       y = "Negative effect for minimal increase of grdepth") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/grdepth_values.pdf"), width = 20, height = 9)

# with snowstorage at mean (Munich in winter)




# precipitation
plot(model_winter_ps_intera_04, select = 4, ylim = c(-0.1, 0.8), xlab = "precip_MW", ylab = "s(precip_MW)")
abline(v = c(0.38, 2.38, 2.65, 4.1, 4.18))
ecdf(data_merged_rol_winter$rol_precip_X10304)(c(0.38, 2.38, 2.65, 4.1, 4.18))

plot(model_winter_ps_intera_21, select = 4, ylim = c(-0.5, 3.5), xlab = "precip_SD", ylab = "s(precip_SD)")

interact_plot(model = model_winter_ps_intera_03, pred = rol_precip_X10303, modx = rol_qinfiltration_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(precip_BT)", x.label = "precip_BT", legend.main = "qinfil_BT", outcome.scale = "link") +
  geom_vline(xintercept = c(2.25, 2.7))
ecdf(data_merged_rol_winter$rol_precip_X10303)(c(2.25, 2.7))

plot(model_winter_ps_intera_02, select = 4, ylim = c(-0.1, 1), xlab = "precip_MU", ylab = "s(precip_MU)")

plot(model_summer_ps_intera_04, select = 4, ylim = c(-0.6, 0.7), xlab = "precip_MW", ylab = "s(precip_MW)")
abline(v = c(0.75, 3.67, 3.9))
ecdf(data_merged_rol_summer$rol_precip_X10304)(c(0.75, 3.67, 3.9))

plot(model_summer_ps_intera_21, select = 4, ylim = c(-0.2, 4), xlab = "precip_SD", ylab = "s(precip_SD)")
abline(v = 0.55)
ecdf(data_merged_rol_summer$rol_precip_X10321)(0.55)

interact_plot(model = model_summer_ps_intera_03, pred = rol_precip_X10303, modx = rol_snowstorage_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(precip_BT)", x.label = "precip_BT", legend.main = "snow_BT", outcome.scale = "link") +
  geom_vline(xintercept = c(0.15, 0.55))
ecdf(data_merged_rol_summer$rol_precip_X10303)(c(0.15, 0.55))

interact_plot(model = model_summer_ps_intera_02, pred = rol_precip_X10302, modx = rol_snowstorage_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(precip_MU)", x.label = "precip_MU", legend.main = "snow_MU", outcome.scale = "link")

data_effects_viz_precipitation <- data.frame(hight_catch_half = c(rep(1, 6), rep(2, 0), rep(3, 2), rep(4, 0),
                                                                   rep(1, 4), rep(2, 2), rep(3, 2), rep(4, 0)),
                                                quantile = c(0, 0.633, 0.994, 0.996, 0.999, 0.9992,
                                                             0.992, 0.996,
                                                             0, 0.747, 0.9998415, 0.9998641,
                                                             0, 0.515,
                                                             0.118, 0.482),
                                                precip = c(0, 0.38, 2.38, 2.65, 4.1, 4.18,
                                                           2.25, 2.7,
                                                           0, 0.75, 3.67, 3.9,
                                                           0, 0.55,
                                                           0.15, 0.55),
                                                catch_half_numb = c(rep("MW_winter_1", 2), rep("MW_winter_2", 2), rep("MW_winter_3", 2),
                                                                    rep("BT_winter_1", 2),
                                                                    rep("MW_summer_1", 2), rep("MW_summer_2", 2),
                                                                    rep("SD_summer_1", 2),
                                                                    rep("BT_summer_1", 2)),
                                                catch = c(rep("MW", 6), rep("SD", 0), rep("BT", 2), rep("MU", 0),
                                                          rep("MW", 4), rep("SD", 2), rep("BT", 2), rep("MU", 0)),
                                                half = c(rep("winter", 8),
                                                          rep("summer", 8)))

ggplot(data_effects_viz_precipitation, aes(x = quantile, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Quantile of respective precip",
       y = "Negative effect for minimal increase of precip") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/precip_quantiles.pdf"), width = 20, height = 9)

ggplot(data_effects_viz_precipitation, aes(x = precip, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Respective precip",
       y = "Negative effect for minimal increase of precip") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/precip_values.pdf"), width = 20, height = 9)

# with qinfiltration at mean (Bad Tölz in winter) and snowstorage at mean (Bad Tölz and Munich in summer)




# qinfiltration
plot(model_winter_ps_intera_04, select = 5, ylim = c(-0.5, 1.6), xlab = "qinfil_MW", ylab = "s(qinfil_MW)")
abline(v = c(2.3, 2.55))
ecdf(data_merged_rol_winter$rol_qinfiltration_X10304)(c(2.3, 2.55))

interact_plot(model = model_winter_ps_intera_21, pred = rol_qinfiltration_X10321, modx = rol_snowstorage_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(qinfil_SD)", x.label = "qinfil_SD", legend.main = "snow_SD", outcome.scale = "link") +
  geom_vline(xintercept = c(1.65, 2.1))
ecdf(data_merged_rol_winter$rol_qinfiltration_X10321)(c(1.65, 2.1))

interact_plot(model = model_winter_ps_intera_03, pred = rol_qinfiltration_X10303, modx = rol_snowstorage_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(qinfil_BT)", x.label = "qinfil_BT", legend.main = "snow_BT", outcome.scale = "link") +
  geom_vline(xintercept = c(1.35, 2.26))
ecdf(data_merged_rol_winter$rol_qinfiltration_X10303)(c(1.35, 2.26))

plot(model_winter_ps_intera_02, select = 5, ylim = c(-0.7, 0.5), xlab = "qinfil_MU", ylab = "s(qinfil_MU)")
abline(v = c(1.27, 1.57))
ecdf(data_merged_rol_winter$rol_qinfiltration_X10302)(c(1.27, 1.57))

interact_plot(model = model_summer_ps_intera_04, pred = rol_qinfiltration_X10304, modx = rol_glorad_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(qinfil_MW)", x.label = "qinfil_MW", legend.main = "glorad_MW", outcome.scale = "link")

interact_plot(model = model_summer_ps_intera_21, pred = rol_qinfiltration_X10321, modx = rol_snowstorage_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(qinfil_SD)", x.label = "qinfil_SD", legend.main = "snow_SD", outcome.scale = "link") +
  geom_vline(xintercept = c(0.78, 1.75))
ecdf(data_merged_rol_summer$rol_qinfiltration_X10321)(c(0.78, 1.75))

plot(model_summer_ps_intera_03, select = 5, ylim = c(-2.55, 0.3), xlab = "qinfil_BT", ylab = "s(qinfil_BT)")
abline(v = c(0.8, 1.3, 1.83, 1.97))
ecdf(data_merged_rol_summer$rol_qinfiltration_X10303)(c(0.8, 1.3, 1.83, 1.97))

plot(model_summer_ps_intera_02, select = 5, ylim = c(-1.2, 0.7), xlab = "qinfil_MU", ylab = "s(qinfil_MU)")
abline(v = 1.6)
ecdf(data_merged_rol_summer$rol_qinfiltration_X10302)(1.6)


data_effects_viz_qinfiltration <- data.frame(hight_catch_half = c(rep(1, 2), rep(2, 2), rep(3, 2), rep(4, 2),
                                                                   rep(1, 0), rep(2, 2), rep(3, 4), rep(4, 2)),
                                             quantile = c(0.9995851, 0.9999,
                                                          0.998, 0.9997,
                                                          0.994, 0.9999,
                                                          0.992, 0.997,
                                                          0.810, 0.999,
                                                          0.831, 0.986, 0.99964, 0.9999,
                                                          0, 0.998),
                                             qinfil = c(2.3, 2.55,
                                                        1.65, 2.1,
                                                        1.35, 2.26,
                                                        1.27, 1.57,
                                                        0.78, 1.75,
                                                        0.8, 1.3, 1.83, 1.97,
                                                        0.0000063, 1.6),
                                             catch_half_numb = c(rep("MW_winter_1", 2),
                                                                 rep("SD_winter_1", 2),
                                                                 rep("BT_winter_1", 2),
                                                                 rep("MU_winter_1", 2),
                                                                 rep("SD_summer_1", 2),
                                                                 rep("BT_summer_1", 2), rep("BT_summer_2", 2),
                                                                 rep("MU_summer_1", 2)),
                                             catch = c(rep("MW", 2), rep("SD", 2), rep("BT", 2), rep("MU", 2),
                                                       rep("MW", 0), rep("SD", 2), rep("BT", 4), rep("MU", 2)),
                                             half = c(rep("winter", 8),
                                                       rep("summer", 8)))

ggplot(data_effects_viz_qinfiltration, aes(x = quantile, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Quantile of respective qinfil",
       y = "Negative effect for minimal increase of qinfil") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/qinfil_quantiles.pdf"), width = 20, height = 9)

ggplot(data_effects_viz_qinfiltration, aes(x = qinfil, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Respective qinfil",
       y = "Negative effect for minimal increase of qinfil") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/qinfil_values.pdf"), width = 20, height = 9)

# with snowstorage (Schlehdorf and Bad Tölz in winter, Schlehdorf in summer),
# soilwaterunsatzone at mean (Schlehdorf in winter),
# precipitation at mean (Bad Tölz in winter),
# glorad at mean (Mittenwald in summer)


# relhum
plot(model_winter_ps_intera_04, select = 6, ylim = c(-0.1, 0.7), xlab = "relhum_MW", ylab = "s(relhum_MW)")
abline(v = c(0.67, 0.72, 0.83, 0.86))
ecdf(data_merged_rol_winter$rol_relhum_X10304)(c(0.67, 0.72, 0.83, 0.86))

plot(model_winter_ps_intera_21, select = 6, ylim = c(-0.2, 0.15), xlab = "relhum_SD", ylab = "s(relhum_SD)")
abline(v = c(0.56, 0.62, 0.9, 0.965))
ecdf(data_merged_rol_winter$rol_relhum_X10321)(c(0.56, 0.62, 0.9, 0.965))

plot(model_winter_ps_intera_03, select = 6, ylim = c(-0.2, 0.15), xlab = "relhum_BT", ylab = "s(relhum_BT)")
abline(v = c(0.9, 0.945))
ecdf(data_merged_rol_winter$rol_relhum_X10303)(c(0.9, 0.945))

plot(model_winter_ps_intera_02, select = 6, ylim = c(-0.25, 0.1), xlab = "relhum_MU", ylab = "s(relhum_MU)")
abline(v = c(0.572, 0.582, 0.947, 0.976))
ecdf(data_merged_rol_winter$rol_relhum_X10302)(c(0.572, 0.582, 0.947, 0.976))

plot(model_summer_ps_intera_04, select = 6, ylim = c(-0.08, 0.18), xlab = "relhum_MW", ylab = "s(relhum_MW)")
abline(v = c(0.51, 0.57, 0.92, 0.955))
ecdf(data_merged_rol_summer$rol_relhum_X10302)(c(0.51, 0.57, 0.92, 0.955))

plot(model_summer_ps_intera_21, select = 6, ylim = c(-0.12, 0.3), xlab = "relhum_SD", ylab = "s(relhum_SD)")
abline(v = c(0.52, 0.59))
ecdf(data_merged_rol_summer$rol_relhum_X10321)(c(0.52, 0.59))

interact_plot(model = model_summer_ps_intera_03, pred = rol_relhum_X10303, modx = rol_soilwaterunsatzone_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(relhum_BT)", x.label = "relhum_BT", legend.main = "soilunsat_BT", outcome.scale = "link") +
  geom_vline(xintercept = c(0.515, 0.57, 0.89, 0.91))
ecdf(data_merged_rol_summer$rol_relhum_X10303)(c(0.515, 0.57, 0.89, 0.91))

interact_plot(model = model_summer_ps_intera_02, pred = rol_relhum_X10302, modx = rol_soilwaterunsatzone_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(relhum_MU)", x.label = "relhum_MU", legend.main = "soilunsat_MU", outcome.scale = "link") +
  geom_vline(xintercept = c(0.515, 0.565, 0.865, 0.87))
ecdf(data_merged_rol_summer$rol_relhum_X10302)(c(0.515, 0.565, 0.865, 0.87))



data_effects_viz_relhum <- data.frame(hight_catch_half = c(rep(1, 4), rep(2, 4), rep(3, 2), rep(4, 4),
                                                           rep(1, 4), rep(2, 2), rep(3, 4), rep(4, 4)),
                                      quantile = c(0.153, 0.288, 0.762, 0.878,
                                                   0.010, 0.050, 0.970, 0.999,
                                                   0.950, 0.994,
                                                   0.0004, 0.001, 0.985, 0.9998,
                                                   0.001, 0.003, 0.999, 1,
                                                   0.001, 0.008,
                                                   0.001, 0.004, 0.957, 0.984,
                                                   0.001, 0.003, 0.928, 0.940),
                                      relhum = c(0.67, 0.72, 0.83, 0.86,
                                                 0.56, 0.62, 0.9, 0.965,
                                                 0.9, 0.945,
                                                 0.572, 0.582, 0.947, 0.976,
                                                 0.51, 0.57, 0.92, 0.955,
                                                 0.52, 0.59,
                                                 0.515, 0.57, 0.89, 0.91,
                                                 0.515, 0.565, 0.865, 0.87),
                                      catch_half_numb = c(rep("MW_winter_1", 2), rep("MW_winter_2", 2),
                                                          rep("SD_winter_1", 2), rep("SD_winter_2", 2),
                                                          rep("BT_winter_1", 2),
                                                          rep("MU_winter_1", 2), rep("MU_winter_2", 2),
                                                          rep("MW_summer_1", 2), rep("MW_summer_2", 2),
                                                          rep("SD_summer_1", 2),
                                                          rep("BT_summer_1", 2), rep("BT_summer_2", 2),
                                                          rep("MU_summer_1", 2), rep("MU_summer_2", 2)),
                                      catch = c(rep("MW", 4), rep("SD", 4), rep("BT", 2), rep("MU", 4),
                                                rep("MW", 4), rep("SD", 2), rep("BT", 4), rep("MU", 4)),
                                      half = c(rep("winter", 14),
                                                rep("summer", 14)))

ggplot(data_effects_viz_relhum, aes(x = quantile, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Quantile of respective relhum",
       y = "Negative effect for minimal increase of relhum") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/relhum_quantiles.pdf"), width = 20, height = 9)

ggplot(data_effects_viz_relhum, aes(x = relhum, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Respective relhum",
       y = "Negative effect for minimal increase of relhum") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/relhum_values.pdf"), width = 20, height = 9)

# with soilwaterunsatzone at mean (Bad Tölz und Munich in summer)





# snowstorage
interact_plot(model = model_winter_ps_intera_04, pred = rol_snowstorage_X10304, modx = rol_soilwaterunsatzone_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_MW)", x.label = "snow_MW", legend.main = "soilunsat_MW", outcome.scale = "link") +
  geom_vline(xintercept = c(50, 80))
ecdf(data_merged_rol_winter$rol_snowstorage_X10304)(c(50, 80))

interact_plot(model = model_winter_ps_intera_21, pred = rol_snowstorage_X10321, modx = rol_qinfiltration_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_SD)", x.label = "snow_SD", legend.main = "qinfil_SD", outcome.scale = "link") +
  geom_vline(xintercept = c(40, 70, 280, 320, 440, 445))
ecdf(data_merged_rol_winter$rol_snowstorage_X10321)(c(40, 70, 280, 320, 440, 445))

interact_plot(model = model_winter_ps_intera_03, pred = rol_snowstorage_X10303, modx = rol_qinfiltration_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_BT)", x.label = "snow_BT", legend.main = "qinfil_BT", outcome.scale = "link") +
  geom_vline(xintercept = c(290, 320))
ecdf(data_merged_rol_winter$rol_snowstorage_X10303)(c(290, 320))

interact_plot(model = model_winter_ps_intera_02, pred = rol_snowstorage_X10302, modx = rol_groundwaterdepth_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_MU)", x.label = "snow_MU", legend.main = "grdepth_MU", outcome.scale = "link") +
  geom_vline(xintercept = c(1, 15))
ecdf(data_merged_rol_winter$rol_snowstorage_X10302)(c(1, 15))

interact_plot(model = model_summer_ps_intera_04, pred = rol_snowstorage_X10304, modx = rol_glorad_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_MW)", x.label = "snow_MW", legend.main = "glorad_MW", outcome.scale = "link") +
  geom_vline(xintercept = c(455, 490))
ecdf(data_merged_rol_summer$rol_snowstorage_X10304)(c(455, 490))

interact_plot(model = model_summer_ps_intera_21, pred = rol_snowstorage_X10321, modx = rol_glorad_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_SD)", x.label = "snow_SD", legend.main = "glorad_SD", outcome.scale = "link") +
  geom_vline(xintercept = c(145, 165, 270, 305))
ecdf(data_merged_rol_summer$rol_snowstorage_X10321)(c(145, 165, 270, 305))

interact_plot(model = model_summer_ps_intera_03, pred = rol_snowstorage_X10303, modx = rol_precip_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_BT)", x.label = "snow_BT", legend.main = "precip_BT", outcome.scale = "link") +
  geom_vline(xintercept = c(0, 25, 330, 350))
ecdf(data_merged_rol_summer$rol_snowstorage_X10303)(c(0, 25, 330, 350))

interact_plot(model = model_summer_ps_intera_02, pred = rol_snowstorage_X10302, modx = rol_precip_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(snow_MU)", x.label = "snow_MU", legend.main = "precip_MU", outcome.scale = "link") +
  geom_vline(xintercept = c(0.5, 3.2, 5.1, 5.9))
ecdf(data_merged_rol_summer$rol_snowstorage_X10302)(c(0.5, 3.2, 5.1, 5.9))

data_effects_viz_snowstorage <- data.frame(hight_catch_half = c(rep(1, 2), rep(2, 6), rep(3, 2), rep(4, 2),
                                                            rep(1, 2), rep(2, 4), rep(3, 4), rep(4, 4)),
                                      quantile = c(0.339, 0.426,
                                                   0.359, 0.481, 0.944, 0.971, 0.998, 0.99956,
                                                   0.935, 0.952,
                                                   0.247, 0.891,
                                                   0.996, 0.997,
                                                   0.963, 0.972,0.996, 0.999,
                                                   0.005, 0.805, 0.998, 0.9995,
                                                   0.916, 0.975, 0.990, 0.994),
                                      snow = c(50, 80,
                                               40, 70, 280, 320, 440, 445,
                                               290, 320,
                                               1, 15,
                                               455, 490,
                                               145, 165, 270, 305,
                                               0, 25, 330, 350,
                                               0.5, 3.2, 5.1, 5.9),
                                      catch_half_numb = c(rep("MW_winter_1", 2),
                                                           rep("SD_winter_1", 2), rep("SD_winter_2", 2), rep("SD_winter_3", 2),
                                                           rep("BT_winter_1", 2),
                                                           rep("MU_winter_1", 2),
                                                           rep("MW_summer_1", 2),
                                                           rep("SD_summer_1", 2), rep("SD_summer_2", 2),
                                                           rep("BT_summer_1", 2), rep("BT_summer_2", 2),
                                                           rep("MU_summer_1", 2), rep("MU_summer_2", 2)),
                                      catch = c(rep("MW", 2), rep("SD", 6), rep("BT", 2), rep("MU", 2),
                                                rep("MW", 2), rep("SD", 4), rep("BT", 4), rep("MU", 4)),
                                      half = c(rep("winter", 12),
                                                rep("summer", 14)))

ggplot(data_effects_viz_snowstorage, aes(x = quantile, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Quantile of respective snow",
       y = "Negative effect for minimal increase of snow") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/snow_quantiles.pdf"), width = 20, height = 9)

ggplot(data_effects_viz_snowstorage, aes(x = snow, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Respective snow",
       y = "Negative effect for minimal increase of snow") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/snow_values.pdf"), width = 20, height = 9)

# with soilwaterunsatzone at mean (Mittenwald in winter)
# qinfiltration at mean (Schlehdorf, Bad Tölz in winter and Schlehdorf in summer)
# groundwaterdepth at mean (Munich in winter)
# glorad at mean (Mittenwald, Schlehdorf in summer)
# precipitation at mean (Bad Tölz, Munich in summer)


# soilwaterrootzone
interact_plot(model = model_winter_ps_intera_04, pred = rol_soilwaterrootzone_X10304, modx = rol_snowstorage_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(soilroot_MW)", x.label = "soilroot_MW", legend.main = "snow_MW", outcome.scale = "link") +
  geom_vline(xintercept = c(0.341, 0.3445, 0.393, 0.3959))
ecdf(data_merged_rol_winter$rol_soilwaterrootzone_X10304)(c(0.341, 0.3445, 0.393, 0.3959))

plot(model_winter_ps_intera_21, select = 8, ylim = c(-0.8, 1.2), xlab = "soilroot_SD", ylab = "s(soilroot_SD)")

plot(model_winter_ps_intera_03, select = 8, ylim = c(-0.35, 0.6), xlab = "soilroot_BT", ylab = "s(soilroot_BT)")

interact_plot(model = model_winter_ps_intera_02, pred = rol_soilwaterrootzone_X10302, modx = rol_airtmp_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(soilroot_MU)", x.label = "soilroot_MU", legend.main = "airtmp_MU", outcome.scale = "link") +
  geom_vline(xintercept = c(0.399, 0.415))
ecdf(data_merged_rol_winter$rol_soilwaterrootzone_X10302)(c(0.399, 0.415))

plot(model_summer_ps_intera_04, select = 8, ylim = c(-0.5, 0.35), xlab = "soilroot_MW", ylab = "s(soilroot_MW)")

plot(model_summer_ps_intera_21, select = 8, ylim = c(-0.5, 0.27), xlab = "soilroot_SD", ylab = "s(soilroot_SD)")

plot(model_summer_ps_intera_03, select = 8, ylim = c(-0.5, 0.25), xlab = "soilroot_BT", ylab = "s(soilroot_BT)")

plot(model_summer_ps_intera_02, select = 8, ylim = c(-0.3, 0.2), xlab = "soilroot_MU", ylab = "s(soilroot_MU)")
abline(v = c(0.404, 0.408))
ecdf(data_merged_rol_summer$rol_soilwaterrootzone_X10302)(c(0.404, 0.408))


data_effects_viz_soilwaterrootzone <- data.frame(hight_catch_half = c(rep(1, 4), rep(2, 0), rep(3, 0), rep(4, 2),
                                                                 rep(1, 0), rep(2, 0), rep(3, 0), rep(4, 2)),
                                           quantile = c(0.009, 0.014, 0.994, 1,
                                                        0.009, 0.032,
                                                        0.106, 0.138),
                                           soilroot = c(0.341, 0.3445, 0.393, 0.3959,
                                                        0.399, 0.415,
                                                        0.404, 0.408),
                                           catch_half_numb = c(rep("MW_winter_1", 2), rep("MW_winter_2", 2),
                                                               rep("MU_winter_1", 2),
                                                               rep("MU_summer_1", 2)),
                                           catch = c(rep("MW", 4), rep("SD", 0), rep("BT", 0), rep("MU", 2),
                                                     rep("MW", 0), rep("SD", 0), rep("BT", 0), rep("MU", 2)),
                                           half = c(rep("winter", 6),
                                                     rep("summer", 2)))


ggplot(data_effects_viz_soilwaterrootzone, aes(x = quantile, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Quantile of respective soilroot",
       y = "Negative effect for minimal increase of soilroot") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/soilroot_quantiles.pdf"), width = 20, height = 9)

ggplot(data_effects_viz_soilwaterrootzone, aes(x = soilroot, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Respective soilroot",
       y = "Negative effect for minimal increase of soilroot") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/soilroot_values.pdf"), width = 20, height = 9)

# with snowstorage at mean (Mittenwald in winter),
# airtmp at mean (Munich in winter)


# soilwaterunsatzone
interact_plot(model = model_winter_ps_intera_04, pred = rol_soilwaterunsatzone_X10304, modx = rol_snowstorage_X10304,
              interval = TRUE, int.type = "confidence", y.label = "s(soilunsat_MW)", x.label = "soilunsat_MW", legend.main = "snow_MW", outcome.scale = "link") +
  geom_vline(xintercept = c(6032, 6050, 6102, 6157))
ecdf(data_merged_rol_winter$rol_soilwaterunsatzone_X10304)(c(6032, 6050, 6102, 6157))

interact_plot(model = model_winter_ps_intera_21, pred = rol_soilwaterunsatzone_X10321, modx = rol_qinfiltration_X10321,
              interval = TRUE, int.type = "confidence", y.label = "s(soilunsat_SD)", x.label = "soilunsat_SD", legend.main = "qinfil_SD", outcome.scale = "link") +
  geom_vline(xintercept = c(6474, 6483, 6513, 6521, 6545, 6570, 6589, 6599))
ecdf(data_merged_rol_winter$rol_soilwaterunsatzone_X10321)(c(6474, 6483, 6513, 6521, 6545, 6570, 6589, 6599))

plot(model_winter_ps_intera_03, select = 9, ylim = c(-0.25, 0.07), xlab = "soilunsat_BT", ylab = "s(soilunsat_BT)")
abline(v = c(5860.5, 5898.5))
ecdf(data_merged_rol_winter$rol_soilwaterunsatzone_X10303)(c(5860.5, 5898.5))

plot(model_winter_ps_intera_02, select = 9, ylim = c(-0.5, 0.3), xlab = "soilunsat_MU", ylab = "s(soilunsat_MU)")
abline(v = c(6061.5, 6068.5, 6120, 6140, 6168, 6185))
ecdf(data_merged_rol_winter$rol_soilwaterunsatzone_X10302)(c(6061.5, 6068.5, 6120, 6140, 6168, 6185))

plot(model_summer_ps_intera_04, select = 9, ylim = c(-0.15, 0.15), xlab = "soilunsat_MW", ylab = "s(soilunsat_MW)")
abline(v = c(6023.5, 6043.5))
ecdf(data_merged_rol_summer$rol_soilwaterunsatzone_X10304)(c(6023.5, 6043.5))

plot(model_summer_ps_intera_21, select = 9, ylim = c(-0.05, 0.2), xlab = "soilunsat_SD", ylab = "s(soilunsat_SD)")
abline(v = c(6493.7, 6500, 6524.5, 6536))
ecdf(data_merged_rol_summer$rol_soilwaterunsatzone_X10321)(c(6493.7, 6500, 6524.5, 6536))

interact_plot(model = model_summer_ps_intera_03, pred = rol_soilwaterunsatzone_X10303, modx = rol_relhum_X10303,
              interval = TRUE, int.type = "confidence", y.label = "s(soilunsat_BT)", x.label = "soilunsat_BT", legend.main = "relhum_BT", outcome.scale = "link") +
  geom_vline(xintercept = c(5797.5, 5799.5, 5888.5, 5890.5))
ecdf(data_merged_rol_summer$rol_soilwaterunsatzone_X10303)(c(5797.5, 5799.5, 5888.5, 5890.5))

interact_plot(model = model_summer_ps_intera_02, pred = rol_soilwaterunsatzone_X10302, modx = rol_relhum_X10302,
              interval = TRUE, int.type = "confidence", y.label = "s(soilunsat_MU)", x.label = "soilunsat_MU", legend.main = "relhum_MU", outcome.scale = "link") +
  geom_vline(xintercept = c(6074, 6088, 6166, 6183))
ecdf(data_merged_rol_summer$rol_soilwaterunsatzone_X10302)(c(6074, 6088, 6166, 6183))


data_effects_viz_soilwaterunsatzone <- data.frame(hight_catch_half = c(rep(1, 4), rep(2, 8), rep(3, 2), rep(4, 6),
                                                                       rep(1, 2), rep(2, 4), rep(3, 4), rep(4, 4)),
                                                 quantile = c(0.0911, 0.155, 0.732, 0.985,
                                                              0.011, 0.019, 0.157, 0.238, 0.689, 0.913, 0.985, 0.999,
                                                              0.894, 0.996,
                                                              0.001, 0.006, 0.052, 0.181, 0.370, 0.604,
                                                              0.004, 0.016,
                                                              0.001, 0.004, 0.085, 0.168,
                                                              0.001, 0.002, 0.985, 0.992,
                                                              0.013, 0.025, 0.533, 0.720),
                                                 soilunsat = c(6032, 6050, 6102, 6157,
                                                               6474, 6483, 6513, 6521, 6545, 6570, 6589, 6599,
                                                               5860.5, 5898.5,
                                                               6061.5, 6068.5, 6120, 6140, 6168, 6185,
                                                               6023.5, 6043.5,
                                                               6493.7, 6500, 6524.5, 6536,
                                                               5797.5, 5799.5, 5888.5, 5890.5,
                                                               6074, 6088, 6166, 6183),
                                                 catch_half_numb = c(rep("MW_winter_1", 2), rep("MW_winter_2", 2),
                                                                     rep("SD_winter_1", 2), rep("SD_winter_2", 2), rep("SD_winter_3", 2), rep("SD_winter_4", 2),
                                                                     rep("BT_winter_1", 2),
                                                                     rep("MU_winter_1", 2), rep("MU_winter_2", 2), rep("MU_winter_3", 2),
                                                                     rep("MW_summer_1", 2),
                                                                     rep("SD_summer_1", 2), rep("SD_summer_2", 2),
                                                                     rep("BT_summer_1", 2), rep("BT_summer_2", 2),
                                                                     rep("MU_summer_1", 2), rep("MU_summer_2", 2)),
                                                 catch = c(rep("MW", 4), rep("SD", 8), rep("BT", 2), rep("MU", 6),
                                                           rep("MW", 2), rep("SD", 4), rep("BT", 4), rep("MU", 4)),
                                                 half = c(rep("winter", 20),
                                                           rep("summer", 14)))

ggplot(data_effects_viz_soilwaterunsatzone, aes(x = quantile, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Quantile of respective soilunsat",
       y = "Negative effect for minimal increase of soilunsat") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/soilunsat_quantiles.pdf"), width = 20, height = 9)

ggplot(data_effects_viz_soilwaterunsatzone, aes(x = soilunsat, y = hight_catch_half)) +
  geom_line(aes(group = catch_half_numb,
                color = factor(catch, levels = c("MU", "BT", "SD", "MW"))),
            linewidth = 2) +
  scale_color_manual(name = "catchment",
                     c("Munich", "Bad Tölz", "Schlehdorf", "Mittenwald"),
                     values = c("darkviolet", "darkcyan", "chartreuse4", "darkred")) +
  labs(x = "Respective soilunsat",
       y = "Negative effect for minimal increase of soilunsat") +
  facet_wrap(~factor(half, levels = c("winter", "summer"))) +
  scale_y_discrete(limits = c(1, 2, 3, 4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        strip.text = element_text(size = 20),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/modeling/effects/soilunsat_values.pdf"), width = 20, height = 9)

# with snowstorage at mean (Mittenwald in winter),
# qinfiltration at mean (Schlehdorf in winter)
# relhum at mean (Bad Tölz, Munich in summer)
