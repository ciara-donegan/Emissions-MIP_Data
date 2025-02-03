## Additional plots for paper

# Load required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(gridExtra)
library(grid)
library(ncdf4)

# Specify and navigate to the location of Emissions-MIP directory
emi_dir <- paste0("C:/Users/done231/OneDrive - PNNL/Documents/GitHub/Emissions-MIP_Data/Emissions-MIP_Data/")
setwd(paste0(emi_dir))

# Select sorting method (region or experiment)
sort_by <- "experiment"
exper <- "shp-60p-red"
diff_or_perdiff <- 'diff'

# Source functions from other file (must be after defining sort_by because of if statements)
source("code/plot_functions.R")

# Define colorblind-friendly palette colors and associate with models (in case a
# plot is missing a model, the color scheme will remain consistent)
cbPalette <- c("#c4c4c3", "#4477aa", "#228833", "#66ccee", "#ccbb44","#ee6677", "#aa3377")

model_colors <- c('CESM1' = cbPalette[1], 'GISS-E2.1' = cbPalette[2], 'CAM-ATRAS' = cbPalette[3], 'GEOS' = cbPalette[4], 'NorESM2' = cbPalette[5], 'GFDL-ESM4' = cbPalette[6], 'E3SM' = cbPalette[7])
model_symbols <- c("CESM1" = 15, "GISS-E2.1" = 15, "CAM-ATRAS" = 17,  "GEOS" = 17, "NorESM2" = 17, "GFDL-ESM4" = 19, "E3SM" = 15)

# Read in csv file specifying which models to exclude from the data
excluded_models <- read.csv(file = paste0(emi_dir, '/input/excluded_data.csv'), fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
excluded_models <- excluded_models %>% drop_na() #gets rid of any empty spaces

# Call function to read in data
summary_long <- read_in_data()

# Generate plots
title_font <- 13
axis_font <- 10
axis_title_font <- 12

#filters each species from summary_long
loadso4 <- filter_species(summary_long, "loadso4")
loadso2 <- filter_species(summary_long, "loadso2")

if (sort_by=="region") {
  #creates plots based on each species using the plot_species function
  loadso4_plot  <- plot_species(loadso4, region, value, 'Load of SO4', expression(Delta*~loadso4~(kg~m^-2)), region, model_colors, model_symbols)
  loadso2_plot <- plot_species(loadso2, region, value, 'Load of SO2', expression(Delta*~loadso2~(kg~m^-2)), region, model_colors, model_symbols)
}

if (sort_by=="experiment") {
  #creates plots based on each species using the plot_species function
  loadso4_plot  <- plot_species(loadso4, region, value, 'Load of SO4', expression(Delta*~loadso4~(kg~m^-2)), exper, model_colors, model_symbols)
  loadso2_plot  <- plot_species(loadso2, region, value, 'Load of SO2', expression(Delta*~loadso2~(kg~m^-2)), exper, model_colors, model_symbols)
}

# Save loadso2 and loadso4
loadso2_diff <- loadso2_plot
loadso4_diff <- loadso4_plot

loadso2_diff_df <- loadso2
loadso4_diff_df <- loadso4

## Regenerate plots using per-diff
diff_or_perdiff <- 'per-diff'
setwd(paste0(emi_dir))
source("code/plot_functions.R")

# Call function to read in data
summary_long <- read_in_data()

#filters each species from summary_long
loadso4 <- filter_species(summary_long, "loadso4")
loadso2 <- filter_species(summary_long, "loadso2")

if (sort_by=="region") {
  #creates plots based on each species using the plot_species function
  loadso4_plot  <- plot_species(loadso4, region, value, 'Load of SO4', "Percent Difference", region, model_colors, model_symbols)
  loadso2_plot <- plot_species(loadso2, region, value, 'Load of SO2', "Percent Difference", region, model_colors, model_symbols)
}

if (sort_by=="experiment") {
  #creates plots based on each species using the plot_species function
  loadso4_plot  <- plot_species(loadso4, region, value, 'Load of SO4', "Percent Difference", exper, model_colors, model_symbols)
  loadso2_plot  <- plot_species(loadso2, region, value, 'Load of SO2', "Percent Difference", exper, model_colors, model_symbols)
}

# Save loadso2 and loadso4
loadso2_perdiff <- loadso2_plot
loadso4_perdiff <- loadso4_plot

# Save so2/so4 plots together
sox_plots <- grid_arrange_shared_legend(loadso2_diff,loadso4_diff,loadso2_perdiff,loadso4_perdiff)
png(paste0(emi_dir,"/output/SOx_plots_shp-60p-red.png"))
grid.draw(sox_plots)
dev.off()

## Generate load plots with absolute amounts (kg over entire basin)
global_area <- get_basin_area("gridarea_global.nc")
sea_area <- get_basin_area("gridarea_sea.nc")
land_area <- global_area - sea_area # no gridarea_land file?
atlantic_area <- get_basin_area("gridarea_NH-atlantic.nc")
pacific_area <- get_basin_area("gridarea_NH-pacific.nc")
indian_area <- get_basin_area("gridarea_NH-indian.nc")
arctic_area <- get_basin_area("gridarea_arctic.nc")

# Filter out unneeded regions
filtered_regions <- c("global","land","sea","NH_atlantic","NH_pacific","NH_indian","arctic")
loadso2 <- loadso2_diff_df %>% filter(region %in% filtered_regions)
loadso4 <- loadso4_diff_df %>% filter(region %in% filtered_regions)

# Multiply by area of basin and 1e-9 (kg -> Tg), to get units in Tg over basin
loadso2$value[loadso2$region=="global"] <- loadso2$value[loadso2$region=="global"]*global_area*1e-9
loadso2$value[loadso2$region=="sea"] <- loadso2$value[loadso2$region=="sea"]*sea_area*1e-9
loadso2$value[loadso2$region=="land"] <- loadso2$value[loadso2$region=="land"]*land_area*1e-9
loadso2$value[loadso2$region=="NH_atlantic"] <- loadso2$value[loadso2$region=="NH_atlantic"]*atlantic_area*1e-9
loadso2$value[loadso2$region=="NH_pacific"] <- loadso2$value[loadso2$region=="NH_pacific"]*pacific_area*1e-9
loadso2$value[loadso2$region=="NH_indian"] <- loadso2$value[loadso2$region=="NH_indian"]*indian_area*1e-9
loadso2$value[loadso2$region=="arctic"] <- loadso2$value[loadso2$region=="arctic"]*arctic_area*1e-9

loadso4$value[loadso4$region=="global"] <- loadso4$value[loadso4$region=="global"]*global_area*1e-9
loadso4$value[loadso4$region=="sea"] <- loadso4$value[loadso4$region=="sea"]*sea_area*1e-9
loadso4$value[loadso4$region=="land"] <- loadso4$value[loadso4$region=="land"]*land_area*1e-9
loadso4$value[loadso4$region=="NH_atlantic"] <- loadso4$value[loadso4$region=="NH_atlantic"]*atlantic_area*1e-9
loadso4$value[loadso4$region=="NH_pacific"] <- loadso4$value[loadso4$region=="NH_pacific"]*pacific_area*1e-9
loadso4$value[loadso4$region=="NH_indian"] <- loadso4$value[loadso4$region=="NH_indian"]*indian_area*1e-9
loadso4$value[loadso4$region=="arctic"] <- loadso4$value[loadso4$region=="arctic"]*arctic_area*1e-9

# Multiply standard deviation by constants
loadso2$sd[loadso2$region=="global"] <- loadso2$sd[loadso2$region=="global"]*global_area*1e-9
loadso2$sd[loadso2$region=="sea"] <- loadso2$sd[loadso2$region=="sea"]*sea_area*1e-9
loadso2$sd[loadso2$region=="land"] <- loadso2$sd[loadso2$region=="land"]*land_area*1e-9
loadso2$sd[loadso2$region=="NH_atlantic"] <- loadso2$sd[loadso2$region=="NH_atlantic"]*atlantic_area*1e-9
loadso2$sd[loadso2$region=="NH_pacific"] <- loadso2$sd[loadso2$region=="NH_pacific"]*pacific_area*1e-9
loadso2$sd[loadso2$region=="NH_indian"] <- loadso2$sd[loadso2$region=="NH_indian"]*indian_area*1e-9
loadso2$sd[loadso2$region=="arctic"] <- loadso2$sd[loadso2$region=="arctic"]*arctic_area*1e-9

loadso4$sd[loadso4$region=="global"] <- loadso4$sd[loadso4$region=="global"]*global_area*1e-9
loadso4$sd[loadso4$region=="sea"] <- loadso4$sd[loadso4$region=="sea"]*sea_area*1e-9
loadso4$sd[loadso4$region=="land"] <- loadso4$sd[loadso4$region=="land"]*land_area*1e-9
loadso4$sd[loadso4$region=="NH_atlantic"] <- loadso4$sd[loadso4$region=="NH_atlantic"]*atlantic_area*1e-9
loadso4$sd[loadso4$region=="NH_pacific"] <- loadso4$sd[loadso4$region=="NH_pacific"]*pacific_area*1e-9
loadso4$sd[loadso4$region=="NH_indian"] <- loadso4$sd[loadso4$region=="NH_indian"]*indian_area*1e-9
loadso4$sd[loadso4$region=="arctic"] <- loadso4$sd[loadso4$region=="arctic"]*arctic_area*1e-9

## Plot
loadso2_basin_plot  <- plot_species(loadso2, region, value, 'SO2 Column Burden', expression(Delta*~loadso2~(Tg)), exper, model_colors, model_symbols)
loadso4_basin_plot  <- plot_species(loadso4, region, value, 'SO4 Column Burden', expression(Delta*~loadso4~(Tg)), exper, model_colors, model_symbols)
