setwd("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/drainage_pgm/kbe")

# load packages
library(sf)
library(dplyr)
library(ggplot)
library(raster)




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
      labs(x = var, y = paste0("drainage_", catchment)) +
      scale_color_manual(values=c("white", "orange")) +
      #ylim(0, 150) +
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
catchments <- c("X10304", "X10321", "X10303", "X10302")
catchments_names <- c("MW", "SD", "BT", "MU")

for (i in vars) {
  for (j in 1:length(catchments)) {
    lower_tail_depend(data = data_merged_rol_winter, var = paste0("rol_", i, "_", catchments[j]), catchment = catchments[j],
                      quant = 0.1, xlab = paste0(i, "_", catchments_names[j]), ylab = paste0("drainage_", catchments_names[j]), plot = TRUE)
    ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/tail_depend/lower_winter_", i, "_", catchments[j], ".pdf"), width = 12, height = 9)
  }
}

for (i in vars) {
  for (j in 1:length(catchments)) {
    lower_tail_depend(data = data_merged_rol_summer, var = paste0("rol_", i, "_", catchments[j]), catchment = catchments[j],
                      quant = 0.1, xlab = paste0(i, "_", catchments_names[j]), ylab = paste0("drainage_", catchments_names[j]), plot = TRUE)
    ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/tail_depend/lower_summer_", i, "_", catchments[j], ".pdf"), width = 12, height = 9)
  }
}


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
      labs(x = var, y = paste0("drainage_", catchment)) +
      scale_color_manual(values=c("white", "orange")) +
      #ylim(0, 150) +
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
catchments <- c("X10304", "X10321", "X10303", "X10302")
catchments_names <- c("MW", "SD", "BT", "MU")

for (i in vars) {
  for (j in 1:length(catchments)) {
    upper_tail_depend(data = data_merged_rol_winter, var = paste0("rol_", i, "_", catchments[j]), catchment = catchments[j],
                      quant = 0.9, xlab = paste0(i, "_", catchments_names[j]), ylab = paste0("drainage_", catchments_names[j]), plot = TRUE)
    ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/tail_depend/upper_winter_", i, "_", catchments[j], ".pdf"), width = 12, height = 9)
  }
}

for (i in vars) {
  for (j in 1:length(catchments)) {
    upper_tail_depend(data = data_merged_rol_summer, var = paste0("rol_", i, "_", catchments[j]), catchment = catchments[j],
                      quant = 0.9, xlab = paste0(i, "_", catchments_names[j]), ylab = paste0("drainage_", catchments_names[j]), plot = TRUE)
    ggsave(paste0("C:/Users/lisak/OneDrive/Dokumente/Studium/7. Semester/Bachelorarbeit/plots/descriptive_analysis/tail_depend/upper_summer_", i, "_", catchments[j], ".pdf"), width = 12, height = 9)
  }
}




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

