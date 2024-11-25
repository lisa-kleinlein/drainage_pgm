# Evaluation of influences on the drainage of the river Isar and its inflows in successive catchments

## Research Question

How do climatological and hydrological variables influence river drainage in four partially consecutive catchments in Bavaria, particularly when explaining low drainage values?

## Data

Climex-II data from the Geography Department of the LMU, which is not publicly available.

Simulated data at 4 river gauges in Mittenwald, Bad Tölz and Munich (located at the Isar river) and Schlehdorf  (located at the Loisach river) between 1981 and 2010; 3-hourly values of drainage, air temperature, global radiation, groundwater depth, precipitation, infiltration, relative humidity, snow storage, soil water in the root zone and soil water in the unsaturated zone (influencing variables as rolling means for different number of days).

## Used Methods

* Descriptive Analysis:
  - Tables and graphics of variables' measures
  - Bivariate Copulas of influencing variables with respective drainage
  - Empirical lower and upper tail dependence for influencing variables and respective drainage
* Modelling:
  - Separate GAMs for catchments and hydrological halfyears for a given influence structure
    (Distribution assumption: Gaussian or Gamma, log link;
     Linear predictor: Linear effects for year and drainage of previous catchments, B-Splines for influencing variables, two linear interactions for influencing variables;
     Distribution assumption and linear interactions are chosen, which most improve the mean squared error (MSE) for drainage values below a specific threshold called NM7Q, on a second simulation series)
* Model evaluation:
  - Effect plots for influencing variables
  - Change in expected drainage for two specific scenarios concerning air temperature and precipitation
  - Pedictions of drainage on a third simulation series
  
## Explanation of files
* bn_visualizations.R: create graphics for Bayesian Network Graphs for influence structure and theory in publication
* descriptive_analysis.R: create tables and plots for variabels' measures, bivariate copulas and empirical tail dependence coefficients and plots
* functions.R: Auxiliary functions (e.g., for generating plots, calculating the MSE)
* model_chosen.R: fit the eight chosen models
* model_data_summer.R: determine the best model for each of the four catchments for the hydrological summer
* model_data_winter.R: determine the best model for each of the four catchments for the hydrological winter
* model_effects.R: create effect plots for influencing variables
* model_predictions.R: predict drainage for a second simulation series and plot predicted vs. observed plots 
* model_scenarios.R: predict drainage for two specific scenarios and plot the expected distribution of drainage
* modify_data.R, modify_data_kbj.R, modify_data_kbo.R: prepare data for the three different simulation series (e.g., create rolling mean variables)
* modify_data_descriptive_analysis.R: create datasets used for depictions in descriptive analysis
* read_data.R, read_data_kbj.R, read_data_kbo.R: read in data for the three different simulation series

     
     
