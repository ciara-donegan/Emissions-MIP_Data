# ------------------------------------------------------------------------------
# Program Name: summary_plots_diff.R
# Authors: Hamza Ahsan
# Date Last Modified: November 2, 2021
# Program Purpose: Produces summary plots of the percent difference between the
# perturbations and the reference case averaged over all years
# Input Files: ~Emissions-MIP/input/
# Output Files: ~Emissions-MIP/output/
# TODO:
# ------------------------------------------------------------------------------

# Load required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(gridExtra)
library(grid)

# Specify location of Emissions-MIP directory
emi_dir <- paste0('C:/Users/such559/Documents/Emissions-MIP_Data')

setwd(paste0(emi_dir))

# Specify what you are sorting by and either the region (i.e., global, land, sea, arctic, NH-land, NH-sea, SH-land, SH-sea) or experiment (i.e., bc-no-season, high-so4, no-so4, reference, so2-at-height, so2-no-season)
#The command line would look like: rscript <rscript>.r <"experiment" or "region"> <specific experiment or region you are sorting by>
sorting <- commandArgs(trailingOnly = TRUE) #pulling region from command line
sort_by <- sorting[1]
if (sort_by == "region"){region <- sorting[2]}
if (sort_by == "experiment"){exper <- sorting[2]}

# Define colorblind-friendly palette colors and associate with models (in case a
# plot is missing a model, the color scheme will remain consistent)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#920000",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#490092")

model_colors <- c(CESM1 = cbPalette[1], E3SM = cbPalette[2], GISS = cbPalette[3],
                  CESM2 = cbPalette[4], MIROC = cbPalette[5], NorESM2 = cbPalette[6],
                  GFDL = cbPalette[7], OsloCTM3 = cbPalette[8], UKESM = cbPalette[9],
                  GEOS = cbPalette[10])

<<<<<<< HEAD
model_symbols <- c(CESM1 = 15, E3SM = 15, GISS = 17, CESM2 = 19, MIROC = 15,
=======
model_symbols <- c(CESM1 = 15, E3SM = 15, GISS = 17, CESM2 = 19, MIROC = 15, 
>>>>>>> main
                   NorESM2 = 17, GFDL = 19, OsloCTM3 = 19, UKESM = 15, GEOS = 17)

# ------------------------------------------------------------------------------
#reads in csv file specifying which models to exclude from the data
excluded_models <- read.csv(file = paste0(emi_dir, '/input', '/excluded_data.csv'), fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
excluded_models %>% drop_na() #gets rid of any empty spaces
<<<<<<< HEAD
#-------------------------------------------------------------------------------

# Setup directory for bc-no-seas percent difference data
setwd(paste0(emi_dir, '/input/', region, '/bc-no-season/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
bc_no_seas <- rbind(map(target_filename, read.csv))
bc_no_seas <- lapply(bc_no_seas, function(x) {x["unit"] <- NULL; x})
bc_no_seas <- bind_rows(bc_no_seas)

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 4) # four years
bc_no_seas$model <- rep_models

# Take the average over all years for each variable and calculate std dev
bc_no_seas_summary <- bc_no_seas %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(bc_no_seas = mean(value), bc_no_seas_sd = sd(value))

#---------------------------------------------------

# Setup directory for high-SO4 percent difference data
setwd(paste0(emi_dir, '/input/', region, '/high-SO4/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
high_so4 <- rbind(map(target_filename, read.csv))
high_so4 <- lapply(high_so4, function(x) {x["unit"] <- NULL; x})
high_so4 <- bind_rows(high_so4)

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 4) # four years
high_so4$model <- rep_models

# Take the average over all years for each variable and calculate std dev
high_so4_summary <- high_so4 %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(high_so4 = mean(value), high_so4_sd = sd(value))

#---------------------------------------------------

# Setup directory for no-SO4 percent difference data
setwd(paste0(emi_dir, '/input/', region, '/no-SO4/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
no_so4 <- rbind(map(target_filename, read.csv))
no_so4 <- lapply(no_so4, function(x) {x["unit"] <- NULL; x})
no_so4 <- bind_rows(no_so4)

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 4) # four years
no_so4$model <- rep_models

# Take the average over all years for each variable and calculate std dev
no_so4_summary <- no_so4 %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(no_so4 = mean(value), no_so4_sd = sd(value))

#---------------------------------------------------

# Setup directory for SO2-at-height percent difference data
setwd(paste0(emi_dir, '/input/', region, '/so2-at-height/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")

so2_at_hgt <- rbind(map(target_filename, read.csv))
so2_at_hgt <- lapply(so2_at_hgt, function(x) {x["unit"] <- NULL; x})
so2_at_hgt <- bind_rows(so2_at_hgt)

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 4) # four years
so2_at_hgt$model <- rep_models

# Take the average over all years for each variable and calculate std dev
so2_at_hgt_summary <- so2_at_hgt %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(so2_at_hgt = mean(value), so2_at_hgt_sd = sd(value))

#---------------------------------------------------

# Setup directory for SO2-no-season percent difference data
setwd(paste0(emi_dir, '/input/', region, '/so2-no-season/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
so2_no_seas <- rbind(map(target_filename, read.csv))
so2_no_seas <- lapply(so2_no_seas, function(x) {x["unit"] <- NULL; x})
so2_no_seas <- bind_rows(so2_no_seas)

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 4) # four years
so2_no_seas$model <- rep_models

# Take the average over all years for each variable and calculate std dev
so2_no_seas_summary <- so2_no_seas %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(so2_no_seas = mean(value), so2_no_seas_sd = sd(value))

#---------------------------------------------------

# Bind data together
summary_data <- list(bc_no_seas_summary,
                     high_so4_summary,
                     no_so4_summary,
                     so2_at_hgt_summary,
                     so2_no_seas_summary) %>%
  reduce(left_join, by = c("variable", "model"))

# Correct model names for CESM and CESM2
summary_data$model[which(summary_data$model == "CESM")] <- "CESM1"

# Change to long format
summary_long_exp <- summary_data %>%
  gather(experiment, value, -c(model,
                               variable,
                               bc_no_seas_sd,
                               high_so4_sd,
                               no_so4_sd,
                               so2_at_hgt_sd,
                               so2_no_seas_sd)) %>%
  select(variable, model, experiment, value) %>%
  drop_na()

summary_long_sd <- summary_data %>%
  gather(experiment, sd, -c(model,
                            variable,
                            bc_no_seas,
                            high_so4,
                            no_so4,
                            so2_at_hgt,
                            so2_no_seas)) %>%
  select(variable, model, experiment, sd) %>%
  drop_na()

=======
#-----------------------------------------------------------------------------
#extracts data for each perturbation experiment from csv files
data_accumulation <- function(emi_dir, reg_name, exper){
  
  # Setup directory for bc-no-seas percent difference data
  setwd(paste0(emi_dir,'/input/', reg_name,'/', exper, '/per-diff'))
  
  # Read in csv files and bind into single data frame
  target_filename <- list.files(getwd(), "*.csv")
  regional_data <- rbind(map(target_filename, read.csv))
  regional_data <- lapply(regional_data, function(x) {x["unit"] <- NULL; x})
  regional_data <- bind_rows(regional_data)
  
  # Extract model from file names (fifth segment) and bind to experiment data frame
  models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
  rep_models <- rep(models, each = 4) # four years
  regional_data$model <- rep_models
  
  # Invert sign of CESM2 wet deposition variables
  regional_data$value[which(regional_data$model == "CESM2" & regional_data$variable == "wetbc")] <- 
    -1 * regional_data$value[which(regional_data$model == "CESM2" & regional_data$variable == "wetbc")]
  regional_data$value[which(regional_data$model == "CESM2" & regional_data$variable == "wetso2")] <- 
    -1 * regional_data$value[which(regional_data$model == "CESM2" & regional_data$variable == "wetso2")]
  regional_data$value[which(regional_data$model == "CESM2" & regional_data$variable == "wetso4")] <- 
    -1 * regional_data$value[which(regional_data$model == "CESM2" & regional_data$variable == "wetso4")]
  
  # Take the average over all years for each variable
  regional_data_summary <- regional_data %>% dplyr::group_by(variable, model) %>%
    dplyr::summarise(regional_data = mean(value), regional_data_sd = sd(value))
  
  return(regional_data_summary)
}

if (sort_by == "region"){
  
  bc_no_seas_summary <- data_accumulation(emi_dir,region,"bc-no-season")
  high_so4_summary <- data_accumulation(emi_dir,region,"high-so4")
  no_so4_summary <- data_accumulation(emi_dir,region,"no-so4")
  so2_at_hgt_summary <- data_accumulation(emi_dir,region,"so2-at-height")
  so2_no_seas_summary <- data_accumulation(emi_dir,region,"so2-no-season")
  
  bc_no_seas_summary <- rename(bc_no_seas_summary, bc_no_seas = regional_data)
  high_so4_summary <- rename(high_so4_summary, high_so4 = regional_data)
  no_so4_summary <- rename(no_so4_summary, no_so4 = regional_data)
  so2_at_hgt_summary <- rename(so2_at_hgt_summary, so2_at_hgt = regional_data)
  so2_no_seas_summary <- rename(so2_no_seas_summary, so2_no_seas = regional_data)
  
  bc_no_seas_summary <- rename(bc_no_seas_summary, bc_no_seas_sd = regional_data_sd)
  high_so4_summary <- rename(high_so4_summary, high_so4_sd = regional_data_sd)
  no_so4_summary <- rename(no_so4_summary, no_so4_sd = regional_data_sd)
  so2_at_hgt_summary <- rename(so2_at_hgt_summary, so2_at_hgt_sd = regional_data_sd)
  so2_no_seas_summary <- rename(so2_no_seas_summary, so2_no_seas_sd = regional_data_sd)
  
  # Bind data together
  summary_data <- list(bc_no_seas_summary, high_so4_summary, no_so4_summary, so2_at_hgt_summary, so2_no_seas_summary) %>% reduce(left_join, by = c("variable", "model"))
  
  # Correct model names for CESM and CESM2
  summary_data$model[which(summary_data$model == "CESM")] <- "CESM1"
  
  # Change to long format
  summary_long_exp <- summary_data %>% 
    gather(experiment, value, -c(model, variable, bc_no_seas_sd, high_so4_sd, no_so4_sd, so2_at_hgt_sd, so2_no_seas_sd)) %>%
    select(variable, model, experiment, value) %>%
    drop_na()
  
  summary_long_sd <- summary_data %>% 
    gather(experiment, sd, -c(model, variable, bc_no_seas, high_so4, no_so4, so2_at_hgt, so2_no_seas)) %>%
    select(variable, model, experiment, sd) %>%
    drop_na()
  
>>>>>>> main
  summary_long_sd$experiment <- gsub("_sd", "", summary_long_sd$experiment)
  
  summary_long <- dplyr::left_join(summary_long_exp, summary_long_sd)
  
  #runs through each excluded model pair and filters them out of summary_long
  if(nrow(excluded_models) != 0) { #only runs if the data frame is not empty
    for (val in 1:nrow(excluded_models)) {
      summary_long <- filter(summary_long, experiment != excluded_models$Scenario[val] | model != excluded_models$Model[val] | variable != excluded_models$Variable[val])
    }
  }
  
}

if (sort_by == "experiment"){
  #read in data for each region
  arctic <- data_accumulation(emi_dir,'arctic',exper)
  global <- data_accumulation(emi_dir,'global',exper)
  land <- data_accumulation(emi_dir,'land',exper)
  NH_atlantic <- data_accumulation(emi_dir,'NH-atlantic',exper)
  NH_indian <- data_accumulation(emi_dir,'NH-indian',exper)
  NH_land <- data_accumulation(emi_dir,'NH-land',exper)
  NH_pacific <- data_accumulation(emi_dir,'NH-pacific',exper)
  NH_sea <- data_accumulation(emi_dir,'NH-sea',exper)
  sea <- data_accumulation(emi_dir,'sea',exper)
  SH_land <- data_accumulation(emi_dir,'SH-land',exper)
  SH_sea <- data_accumulation(emi_dir,'SH-sea',exper)
  
  #rename the mean and standard deviation results columns in each data frame
  arctic <- rename(arctic, arctic = regional_data)
  global <- rename(global, global = regional_data)
  land <- rename(land, land = regional_data)
  NH_atlantic <- rename(NH_atlantic, NH_atlantic = regional_data)
  NH_indian <- rename(NH_indian, NH_indian = regional_data)
  NH_land <- rename(NH_land, NH_land = regional_data)
  NH_pacific <- rename(NH_pacific, NH_pacific = regional_data)
  NH_sea <- rename(NH_sea, NH_sea = regional_data)
  sea <- rename(sea, sea = regional_data)
  SH_land <- rename(SH_land, SH_land = regional_data)
  SH_sea <- rename(SH_sea, SH_sea = regional_data)
  
  arctic <- rename(arctic, arctic_sd = regional_data_sd)
  global <- rename(global, global_sd = regional_data_sd)
  land <- rename(land, land_sd = regional_data_sd)
  NH_atlantic <- rename(NH_atlantic, NH_atlantic_sd = regional_data_sd)
  NH_indian <- rename(NH_indian, NH_indian_sd = regional_data_sd)
  NH_land <- rename(NH_land, NH_land_sd = regional_data_sd)
  NH_pacific <- rename(NH_pacific, NH_pacific_sd = regional_data_sd)
  NH_sea <- rename(NH_sea, NH_sea_sd = regional_data_sd)
  sea <- rename(sea, sea_sd = regional_data_sd)
  SH_land <- rename(SH_land, SH_land_sd = regional_data_sd)
  SH_sea <- rename(SH_sea, SH_sea_sd = regional_data_sd)
  
  # Bind data together
  summary_data <- list(arctic, global, land, NH_atlantic, NH_indian, NH_land, NH_pacific, NH_sea, sea, SH_land, SH_sea) %>% reduce(left_join, by = c("variable", "model"))
  
  # Correct model names for CESM and CESM2
  summary_data$model[which(summary_data$model == "CESM")] <- "CESM1"
  
  # Change to long format
  summary_long_exp <- summary_data %>% 
    gather(region, value, -c(model, variable, arctic_sd, global_sd, land_sd, NH_atlantic_sd, NH_indian_sd, NH_land_sd, NH_pacific_sd, NH_sea_sd, sea_sd, SH_land_sd, SH_sea_sd)) %>%
    select(variable, model, region, value) %>%
    drop_na()
  
  summary_long_sd <- summary_data %>% 
    gather(region, sd, -c(model, variable, arctic, global, land, NH_atlantic, NH_indian, NH_land, NH_pacific, NH_sea, sea, SH_land, SH_sea)) %>%
    select(variable, model, region, sd) %>%
    drop_na()
  
  summary_long_sd$region <- gsub("_sd", "", summary_long_sd$region)
  
  summary_long <- dplyr::left_join(summary_long_exp, summary_long_sd)
}

#Determines max and min values for the y axis
axes_max <- max(summary_long$value)
axes_min <- min(summary_long$value)

#Define the necessary variables for the following cumulative functions
rlut <- dplyr::filter(summary_long, variable == "rlut")
rsut <- dplyr::filter(summary_long, variable == "rsut")
rlutcs <- dplyr::filter(summary_long, variable == "rlutcs")
rsutcs <- dplyr::filter(summary_long, variable == "rsutcs")
drybc <- dplyr::filter(summary_long, variable == "drybc")
wetbc <- dplyr::filter(summary_long, variable == "wetbc")
dryso2 <- dplyr::filter(summary_long, variable == "dryso2")
wetso2 <- dplyr::filter(summary_long, variable == "wetso2")
dryso4 <- dplyr::filter(summary_long, variable == "dryso4")
wetso4 <- dplyr::filter(summary_long, variable == "wetso4")

# Define normal and clear-sky net radiative flux (sum of longwave and shortwave radiation)
net_rad <- dplyr::left_join(rlut, rsut, by = c("model", "experiment"))
net_rad <- dplyr::mutate(net_rad, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, experiment, value, sd))

net_rad_cs <- dplyr::left_join(rlutcs, rsutcs, by = c("model", "experiment"))
net_rad_cs <- dplyr::mutate(net_rad_cs, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, experiment, value, sd))

# Define total BC deposition rate (sum of dry and wet BC )
tot_bc <- dplyr::left_join(drybc, wetbc, by = c("model", "experiment"))
tot_bc <- dplyr::mutate(tot_bc, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, experiment, value, sd))

# Define total S deposition rate (sum of dry and wet SO2/SO4 )
dry_s <- dplyr::left_join(dryso2, dryso4, by = c("model", "experiment"))
dry_s <- dplyr::mutate(dry_s, value = (32.065/64.066)*value.x + (32.065/96.06)*value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, experiment, value, sd))

wet_s <- dplyr::left_join(wetso2, wetso4, by = c("model", "experiment"))
wet_s <- dplyr::mutate(wet_s, value = (32.065/64.066)*value.x + (32.065/96.06)*value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, experiment, value, sd))

tot_s <- dplyr::left_join(dry_s, wet_s, by = c("model", "experiment"))
tot_s <- dplyr::mutate(tot_s, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, experiment, value, sd))

#if the combined values are larger/smaller, they will replace the max or min value on the y axis of the output plots
if (max(tot_s$value) > axes_max){
  axes_max <- max(tot_s)
}

if (max(tot_bc$value) > axes_max){
  axes_max <- max(tot_bc)
}

if (min(tot_s$value) < axes_min){
  axes_min <- min(tot_s)
}

if (min(tot_bc$value) < axes_min){
  axes_min <- min(tot_bc)
}

if (max(net_rad$value) > axes_max){
  axes_max <- max(net_rad)
}

if (max(net_rad_cs$value) > axes_max){
  axes_max <- max(net_rad_cs)
}

if (min(net_rad$value) < axes_min){
  axes_min <- min(net_rad)
}

if (min(net_rad_cs$value) < axes_min){
  axes_min <- min(net_rad_cs)
}


# Generate plots
title_font <- 9.5
axis_font <- 9
axis_title_font <- 9

<<<<<<< HEAD
emibc <- dplyr::filter(summary_long, variable == "emibc")
emibc_plot <- ggplot(emibc, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('surface flux of BC - ', region), y=expression(Delta*~emibc)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(emibc$value))-max(abs(emibc$sd)), max(abs(emibc$value))+max(abs(emibc$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)


emiso2 <- dplyr::filter(summary_long, variable == "emiso2")
emiso2_plot <- ggplot(emiso2, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('surface flux of SO2 - ', region), y=expression(Delta*~emiso2)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(emiso2$value))-max(abs(emiso2$sd)), max(abs(emiso2$value))+max(abs(emiso2$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)



mmrbc <- dplyr::filter(summary_long, variable == "mmrbc")
mmrbc_plot <- ggplot(mmrbc, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('surface concentration of BC - ', region), y=expression(Delta*~mmrbc)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(mmrbc$value))-max(abs(mmrbc$sd)), max(abs(mmrbc$value))+max(abs(mmrbc$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)



mmrso4 <- dplyr::filter(summary_long, variable == "mmrso4")
mmrso4_plot <- ggplot(mmrso4, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('surface concentration of SO4 - ', region), y=expression(Delta*~mmrso4)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(mmrso4$value))-max(abs(mmrso4$sd)), max(abs(mmrso4$value))+max(abs(mmrso4$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)



so2 <- dplyr::filter(summary_long, variable == "so2")
so2_plot <- ggplot(so2, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('surface concentration of SO2 - ', region), y=expression(Delta*~so2)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(so2$value))-max(abs(so2$sd)), max(abs(so2$value))+max(abs(so2$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)



rlut <- dplyr::filter(summary_long, variable == "rlut")
rlut_plot <- ggplot(rlut, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('longwave flux at TOA - \n', region), y=expression(Delta*~rlut)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(rlut$value))-max(abs(rlut$sd)), max(abs(rlut$value))+max(abs(rlut$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)



rsut <- dplyr::filter(summary_long, variable == "rsut")
rsut_plot <- ggplot(rsut, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('shortwave flux at TOA - \n', region), y=expression(Delta*~rsut)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(rsut$value))-max(abs(rsut$sd)), max(abs(rsut$value))+max(abs(rsut$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)



rsdt <- dplyr::filter(summary_long, variable == "rsdt")
rsdt_plot <- ggplot(rsdt, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('incident shortwave flux \n at TOA - ', region), y=expression(Delta*~rsdt)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(rsdt$value))-max(abs(rsdt$sd)), max(abs(rsdt$value))+max(abs(rsdt$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)



rlutcs <- dplyr::filter(summary_long, variable == "rlutcs")
rlutcs_plot <- ggplot(rlutcs, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('clear-sky longwave flux \n at TOA - ', region), y=expression(Delta*~rlutcs)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(rlutcs$value))-max(abs(rlutcs$sd)), max(abs(rlutcs$value))+max(abs(rlutcs$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)



rsutcs <- dplyr::filter(summary_long, variable == "rsutcs")
rsutcs_plot <- ggplot(rsutcs, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('clear-sky shortwave flux \n at TOA - ', region), y=expression(Delta*~rsutcs)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(rsutcs$value))-max(abs(rsutcs$sd)), max(abs(rsutcs$value))+max(abs(rsutcs$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)

net_rad_plot <- ggplot(net_rad, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('net radiative flux at TOA - \n', region), y=expression(Delta*~(rlut~+~rsut))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(net_rad$value))-max(abs(net_rad$sd)), max(abs(net_rad$value))+max(abs(net_rad$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)



net_rad_cs_plot <- ggplot(net_rad_cs, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('clear-sky net radiative \n flux at TOA - ', region), y=expression(Delta*~(rlutcs~+~rsutcs))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(net_rad_cs$value))-max(abs(net_rad_cs$sd)), max(abs(net_rad_cs$value))+max(abs(net_rad_cs$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)



imp_cld_plot <- ggplot(imp_cld, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('implied cloud response at TOA - \n', region), y=expression(Delta*~(rlut~+~rsut~-~rlutcs~-~rsutcs))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(imp_cld$value))-max(abs(imp_cld$sd)), max(abs(imp_cld$value))+max(abs(imp_cld$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


drybc <- dplyr::filter(summary_long, variable == "drybc")
drybc_plot <- ggplot(drybc, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('dry deposition rate \n of BC - ', region), y=expression(Delta*~drybc)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(drybc$value))-max(abs(drybc$sd)), max(abs(drybc$value))+max(abs(drybc$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)

=======
#creates a function that filters species out of a database
filter_species <- function(database, species){
  species <- dplyr::filter(database, variable == species)
  return(species)
}
>>>>>>> main

#filters each species from summary_long
emibc <- filter_species(summary_long, "emibc")
emiso2 <- filter_species(summary_long, "emiso2")
mmrbc <- filter_species(summary_long, "mmrbc")
mmrso4 <- filter_species(summary_long, "mmrso4")
so2 <- filter_species(summary_long, "so2")
rlut <- filter_species(summary_long, "rlut")
rsut <- filter_species(summary_long, "rsut")
rsdt <- filter_species(summary_long, "rsdt")
rlutcs <- filter_species(summary_long, "rlutcs")
rsutcs <- filter_species(summary_long, "rsutcs")
drybc <- filter_species(summary_long, "drybc")
wetbc <- filter_species(summary_long, "wetbc")
dryso2 <- filter_species(summary_long, "dryso2")
wetso2 <- filter_species(summary_long, "wetso2")
dryso4 <- filter_species(summary_long, "dryso4")
wetso4 <- filter_species(summary_long, "wetso4")
od550aer <- filter_species(summary_long, "od550aer")
clt <- filter_species(summary_long, "clt")
cltc <- filter_species(summary_long, "cltc")

#Creates a function that creates plots for the data based on each species
if (sort_by == "region"){
  plot_species <- function(variable, x, y, title, units, region_or_exper, model_colors, model_symbols){
    species <- variable
    species_plot <- ggplot(species, aes(x = experiment, y = value, color = model, shape = model))+
      theme_bw()+
      labs(title=paste0(title,' - ', region_or_exper), y="Percent") +
      theme(plot.title = element_text(hjust = 0.5, size = title_font),
            axis.text = element_text(size = axis_font),
            axis.title = element_text(size = axis_title_font),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank()) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(species$value))-max(abs(species$sd)), max(abs(species$value))+max(abs(species$sd)))) +
      scale_colour_manual(values = model_colors) +
      scale_shape_manual(values = model_symbols) +
      geom_point( position=position_dodge(width = 0.4), size = 1.5) +
      geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)
    return(species_plot)  
  }
  #creates plots based on each species using the plot_species function
  emibc_plot <- plot_species(emibc, region, value, 'surface flux of BC', expression(Delta*~emibc), region, model_colors, model_symbols)
  emiso2_plot <- plot_species(emiso2, region, value, 'surface flux of SO2', expression(Delta*~emiso2), region, model_colors, model_symbols)
  mmrbc_plot <- plot_species(mmrbc, region, value, 'surface concentration of BC', expression(Delta*~mmrbc), region, model_colors, model_symbols)
  mmrso4_plot <- plot_species(mmrso4, region, value, 'surface concentration of SO4', expression(Delta*~mmrso4), region, model_colors, model_symbols)
  so2_plot <- plot_species(so2, region, value, 'surface concentration of SO2', expression(Delta*~so2), region, model_colors, model_symbols)
  rlut_plot <- plot_species(rlut, region, value, 'upwelling longwave flux \n at TOA', expression(Delta*~rlut), region, model_colors, model_symbols)
  rsut_plot <- plot_species(rsut, region, value, 'upwelling shortwave flux \n at TOA', expression(Delta*~rsut), region, model_colors, model_symbols)
  rsdt_plot <- plot_species(rsdt, region, value, 'incident shortwave flux \n at TOA', expression(Delta*~rsdt), region, model_colors, model_symbols)
  rlutcs_plot <- plot_species(rlutcs, region, value, 'upwelling clear-sky longwave \n flux at TOA', expression(Delta*~rlutcs), region, model_colors, model_symbols)
  rsutcs_plot <- plot_species(rsutcs, region, value, 'upwelling clear-sky shortwave \n flux at TOA', expression(Delta*~rsutcs), region, model_colors, model_symbols)
  drybc_plot <- plot_species(drybc, region, value, 'dry deposition rate \n of BC', expression(Delta*~drybc), region, model_colors, model_symbols)
  wetbc_plot <- plot_species(wetbc, region, value, 'wet deposition rate \n of BC', expression(Delta*~wetbc), region, model_colors, model_symbols)
  dryso2_plot <- plot_species(dryso2, region, value, 'dry deposition rate \n of so2', expression(Delta*~dryso2), region, model_colors, model_symbols)
  wetso2_plot <- plot_species(wetso2, region, value, 'wet deposition rate \n of so2', expression(Delta*~wetso2), region, model_colors, model_symbols)
  dryso4_plot <- plot_species(dryso4, region, value, 'dry deposition rate \n of so4', expression(Delta*~dryso4), region, model_colors, model_symbols) 
  wetso4_plot <- plot_species(wetso4, region, value, 'wet deposition rate \n of so4', expression(Delta*~wetso4), region, model_colors, model_symbols) 
  od550aer_plot <-  plot_species(od550aer, region, value, 'ambient aerosol optical \n thickness at 550nm', expression(Delta*~od550aer), region, model_colors, model_symbols) 
  clt_plot <- plot_species(clt, region, value, 'total cloud cover \n percentage', expression(Delta*~clt), region, model_colors, model_symbols) 
  cltc_plot <- plot_species(cltc, region, value, 'convective cloud cover \n percentage', expression(Delta*~cltc), region, model_colors, model_symbols)
}
if (sort_by == "experiment"){
  plot_species <- function(variable, x, y, title, units, region_or_exper, model_colors, model_symbols){
    species <- variable
    species_plot <- ggplot(species, aes(x = region, y = value, color = model, shape = model))+
      theme_bw()+
      labs(title=paste0(title,' - ', region_or_exper), y="Percent") +
      theme(plot.title = element_text(hjust = 0.5, size = title_font),
            axis.text = element_text(size = axis_font),
            axis.title = element_text(size = axis_title_font),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank()) +
      scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(species$value))-max(abs(species$sd)), max(abs(species$value))+max(abs(species$sd)))) +
      scale_colour_manual(values = model_colors) +
      scale_shape_manual(values = model_symbols) +
      geom_point( position=position_dodge(width = 0.4), size = 1.5) +
      geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)
    
    return(species_plot)  
  }
  #creates plots based on each species using the plot_species function
  emibc_plot <- plot_species(emibc, region, value, 'surface flux of BC', expression(Delta*~emibc), exper, model_colors, model_symbols)
  emiso2_plot <- plot_species(emiso2, region, value, 'surface flux of SO2', expression(Delta*~emiso2), exper, model_colors, model_symbols)
  mmrbc_plot <- plot_species(mmrbc, region, value, 'surface concentration of BC', expression(Delta*~mmrbc), exper, model_colors, model_symbols)
  mmrso4_plot <- plot_species(mmrso4, region, value, 'surface concentration of SO4', expression(Delta*~mmrso4), exper, model_colors, model_symbols)
  so2_plot <- plot_species(so2, region, value, 'surface concentration of SO2', expression(Delta*~so2), exper, model_colors, model_symbols)
  rlut_plot <- plot_species(rlut, region, value, 'upwelling longwave flux \n at TOA', expression(Delta*~rlut), exper, model_colors, model_symbols)
  rsut_plot <- plot_species(rsut, region, value, 'upwelling shortwave flux \n at TOA', expression(Delta*~rsut), exper, model_colors, model_symbols)
  rsdt_plot <- plot_species(rsdt, region, value, 'incident shortwave flux \n at TOA', expression(Delta*~rsdt), exper, model_colors, model_symbols)
  rlutcs_plot <- plot_species(rlutcs, region, value, 'upwelling clear-sky longwave \n flux at TOA', expression(Delta*~rlutcs), exper, model_colors, model_symbols)
  rsutcs_plot <- plot_species(rsutcs, region, value, 'upwelling clear-sky shortwave \n flux at TOA', expression(Delta*~rsutcs), exper, model_colors, model_symbols)
  drybc_plot <- plot_species(drybc, region, value, 'dry deposition rate \n of BC', expression(Delta*~drybc), exper, model_colors, model_symbols)
  wetbc_plot <- plot_species(wetbc, region, value, 'wet deposition rate \n of BC', expression(Delta*~wetbc), exper, model_colors, model_symbols)
  dryso2_plot <- plot_species(dryso2, region, value, 'dry deposition rate \n of so2', expression(Delta*~dryso2), exper, model_colors, model_symbols)
  wetso2_plot <- plot_species(wetso2, region, value, 'wet deposition rate \n of so2', expression(Delta*~wetso2), exper, model_colors, model_symbols)
  dryso4_plot <- plot_species(dryso4, region, value, 'dry deposition rate \n of so4', expression(Delta*~dryso4), exper, model_colors, model_symbols) 
  wetso4_plot <- plot_species(wetso4, region, value, 'wet deposition rate \n of so4', expression(Delta*~wetso4), exper, model_colors, model_symbols) 
  od550aer_plot <-  plot_species(od550aer, region, value, 'ambient aerosol optical \n thickness at 550nm', expression(Delta*~od550aer), exper, model_colors, model_symbols) 
  clt_plot <- plot_species(clt, region, value, 'total cloud cover \n percentage', expression(Delta*~clt), exper, model_colors, model_symbols) 
  cltc_plot <- plot_species(cltc, region, value, 'convective cloud cover \n percentage', expression(Delta*~cltc), exper, model_colors, model_symbols)
  
}

<<<<<<< HEAD
wetbc <- dplyr::filter(summary_long, variable == "wetbc")
wetbc_plot <- ggplot(wetbc, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('wet deposition rate \n of BC - ', region), y=expression(Delta*~wetbc)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(wetbc$value))-max(abs(wetbc$sd)), max(abs(wetbc$value))+max(abs(wetbc$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)
=======
# Define normal and clear-sky net radiative flux (sum of longwave and shortwave radiation)
if (sort_by == "region"){
  net_rad <- dplyr::left_join(rlut, rsut, by = c("model", "experiment"))
  net_rad <- dplyr::mutate(net_rad, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, experiment, value, sd))
  
  net_rad_cs <- dplyr::left_join(rlutcs, rsutcs, by = c("model", "experiment"))
  net_rad_cs <- dplyr::mutate(net_rad_cs, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, experiment, value, sd))
  
  #plots normal and clear sky net radiative flux using the plot_species function
  net_rad_plot <- plot_species(net_rad, region, value, 'net radiative flux \n at TOA', expression(Delta*~rlut~+~rsut), region, model_colors, model_symbols)
  net_rad_cs_plot <- plot_species(net_rad_cs, region, value, 'clear-sky net radiative flux \n at TOA', expression(Delta*~rlutcs~+~rsutcs), region, model_colors, model_symbols)
}
>>>>>>> main

if (sort_by == "experiment"){
  net_rad <- dplyr::left_join(rlut, rsut, by = c("model", "region"))
  net_rad <- dplyr::mutate(net_rad, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, region, value, sd))
  
  net_rad_cs <- dplyr::left_join(rlutcs, rsutcs, by = c("model", "region"))
  net_rad_cs <- dplyr::mutate(net_rad_cs, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, region, value, sd))
  
  #plots normal and clear sky net radiative flux using the plot_species function
  net_rad_plot <- plot_species(net_rad, region, value, 'net radiative flux \n at TOA', expression(Delta*~rlut~+~rsut), exper, model_colors, model_symbols)
  net_rad_cs_plot <- plot_species(net_rad_cs, region, value, 'clear-sky net radiative flux \n at TOA', expression(Delta*~rlutcs~+~rsutcs), exper, model_colors, model_symbols)
}

<<<<<<< HEAD

tot_bc_plot <- ggplot(tot_bc, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('total deposition rate \n of BC - ', region), y=expression(Delta*~(drybc~+~wetbc))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(tot_bc$value))-max(abs(tot_bc$sd)), max(abs(tot_bc$value))+max(abs(tot_bc$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)


dryso2 <- dplyr::filter(summary_long, variable == "dryso2")
dryso2_plot <- ggplot(dryso2, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('dry deposition rate \n of SO2 - ', region), y=expression(Delta*~dryso2)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(dryso2$value))-max(abs(dryso2$sd)), max(abs(dryso2$value))+max(abs(dryso2$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)


wetso2 <- dplyr::filter(summary_long, variable == "wetso2")
wetso2_plot <- ggplot(wetso2, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('wet deposition rate \n of SO2 - ', region), y=expression(Delta*~wetso2)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(wetso2$value))-max(abs(wetso2$sd)), max(abs(wetso2$value))+max(abs(wetso2$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)


dryso4 <- dplyr::filter(summary_long, variable == "dryso4")
dryso4_plot <- ggplot(dryso4, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('dry deposition rate \n of SO4 - ', region), y=expression(Delta*~dryso4)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(dryso4$value))-max(abs(dryso4$sd)), max(abs(dryso4$value))+max(abs(dryso4$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)


wetso4 <- dplyr::filter(summary_long, variable == "wetso4")
wetso4_plot <- ggplot(wetso4, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('wet deposition rate \n of SO4 - ', region), y=expression(Delta*~wetso4)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(wetso4$value))-max(abs(wetso4$sd)), max(abs(wetso4$value))+max(abs(wetso4$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)


tot_s_plot <- ggplot(tot_s, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('total deposition rate \n of S - ', region), y=expression(Delta*~(dryso2~+~wetso2)/2~+~(dryso4~+~wetso4)/3)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(tot_s$value))-max(abs(tot_s$sd)), max(abs(tot_s$value))+max(abs(tot_s$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)


od550aer <- dplyr::filter(summary_long, variable == "od550aer")
od550aer_plot <- ggplot(od550aer, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('ambient aerosol optical \n thickness at 550nm - ', region), y=expression(Delta*~od550aer)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(od550aer$value))-max(abs(od550aer$sd)), max(abs(od550aer$value))+max(abs(od550aer$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)


clt <- dplyr::filter(summary_long, variable == "clt")
clt_plot <- ggplot(clt, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('total cloud cover \n percentage - ', region), y=expression(Delta*~clt)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(clt$value))-max(abs(clt$sd)), max(abs(clt$value))+max(abs(clt$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)
=======
# Define total BC deposition rate (sum of dry and wet BC )
if (sort_by == "region"){
  tot_bc <- dplyr::left_join(drybc, wetbc, by = c("model", "experiment"))
  tot_bc <- dplyr::mutate(tot_bc, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, experiment, value, sd))
  
  tot_bc_plot <- plot_species(tot_bc, region, value, 'total deposition rate \n of BC', expression(Delta*~drybc~+~wetbc), region, model_colors, model_symbols)
}

if (sort_by == "experiment"){
  tot_bc <- dplyr::left_join(drybc, wetbc, by = c("model", "region"))
  tot_bc <- dplyr::mutate(tot_bc, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, region, value, sd))
  
  tot_bc_plot <- plot_species(tot_bc, region, value, 'total deposition rate \n of BC', expression(Delta*~drybc~+~wetbc), exper, model_colors, model_symbols)
}

# Define total S deposition rate (sum of dry and wet SO2/SO4 )
if (sort_by == "region"){
  dry_s <- dplyr::left_join(dryso2, dryso4, by = c("model", "experiment"))
  dry_s <- dplyr::mutate(dry_s, value = (32.065/64.066)*value.x + (32.065/96.06)*value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, experiment, value, sd))
  
  wet_s <- dplyr::left_join(wetso2, wetso4, by = c("model", "experiment"))
  wet_s <- dplyr::mutate(wet_s, value = (32.065/64.066)*value.x + (32.065/96.06)*value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, experiment, value, sd))
  
  tot_s <- dplyr::left_join(dry_s, wet_s, by = c("model", "experiment"))
  tot_s <- dplyr::mutate(tot_s, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, experiment, value, sd))
  
  tot_s_plot <- plot_species(tot_s, region, value, 'total deposition rate \n of S', expression(Delta*(~dryso2~+~wetso2)/2~+~(dryso4~+~wetso4)/3), region, model_colors, model_symbols)
}

if (sort_by == "experiment"){
  dry_s <- dplyr::left_join(dryso2, dryso4, by = c("model", "region"))
  dry_s <- dplyr::mutate(dry_s, value = (32.065/64.066)*value.x + (32.065/96.06)*value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, region, value, sd))
  
  wet_s <- dplyr::left_join(wetso2, wetso4, by = c("model", "region"))
  wet_s <- dplyr::mutate(wet_s, value = (32.065/64.066)*value.x + (32.065/96.06)*value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, region, value, sd))
  
  tot_s <- dplyr::left_join(dry_s, wet_s, by = c("model", "region"))
  tot_s <- dplyr::mutate(tot_s, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, region, value, sd))
  
  tot_s_plot <- plot_species(tot_s, region, value, 'total deposition rate \n of S', expression(Delta*(~dryso2~+~wetso2)/2~+~(dryso4~+~wetso4)/3), exper, model_colors, model_symbols)
}
>>>>>>> main

if (sort_by == "region"){
  # Define implied cloud response (net - clearsky) as a new variable to plot
  imp_cld <- dplyr::left_join(net_rad, net_rad_cs, by = c("model", "experiment"))
  imp_cld <- dplyr::mutate(imp_cld, value = value.x - value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, experiment, value, sd))
  
  imp_cld_plot <- plot_species(imp_cld, region, value, 'implied cloud response at TOA - \n', expression(Delta*~rlut~+~rsut~-~rlutcs~-~rsutcs), region, model_colors, model_symbols)
}

<<<<<<< HEAD
cltc <- dplyr::filter(summary_long, variable == "cltc")
cltc_plot <- ggplot(cltc, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title = paste0('convective cloud cover \n percentage - ', region), y=expression(Delta*~cltc)) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(cltc$value))-max(abs(cltc$sd)), max(abs(cltc$value))+max(abs(cltc$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)+
  ylim(axes_min,axes_max)
=======
if (sort_by == "experiment"){
  # Define implied cloud response (net - clearsky) as a new variable to plot
  imp_cld <- dplyr::left_join(net_rad, net_rad_cs, by = c("model", "region"))
  imp_cld <- dplyr::mutate(imp_cld, value = value.x - value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, region, value, sd))
  
  imp_cld_plot <- plot_species(imp_cld, region, value, 'implied cloud response at TOA - \n', expression(Delta*~rlut~+~rsut~-~rlutcs~-~rsutcs),exper, model_colors, model_symbols)
}
>>>>>>> main

# Function from stack exchange to generate a shared legend
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom",
                                     legend.title = element_blank(),
                                     legend.text = element_text(size = 9,
                                                                margin = margin(r = 10, unit = "pt"))))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - 1.5 * lheight, lheight), # the "1.5" adds room for title
    top = textGrob("Summary - absolute difference", gp = gpar(fontsize = 12)))
}

emissions_plot <- grid_arrange_shared_legend(emibc_plot,
                                             emiso2_plot,
                                             mmrbc_plot,
                                             mmrso4_plot,
                                             so2_plot)

forcing_plot <- grid_arrange_shared_legend(rlut_plot,
                                           rsut_plot,
                                           net_rad_plot,
                                           rsdt_plot,
                                           rlutcs_plot,
                                           rsutcs_plot,
                                           net_rad_cs_plot,
                                           imp_cld_plot)

cloud_plot <- grid_arrange_shared_legend(od550aer_plot,
                                         clt_plot,
                                         cltc_plot)

deposition_plot <- grid_arrange_shared_legend(drybc_plot,
                                              wetbc_plot,
                                              tot_bc_plot,
                                              dryso2_plot,
                                              wetso2_plot,
                                              dryso4_plot,
                                              wetso4_plot,
                                              tot_s_plot)

# Print plots
if (sort_by == 'region'){
  setwd(paste0('../../../../output/', region, '/summary'))
  
  pdf(paste0(region, '_summary_plots_per_diff.pdf'), height = 11, width = 8.5, paper = "letter")
}

if (sort_by == 'experiment'){
  setwd(paste0('../../../../output/', exper, '/summary'))
  
  pdf(paste0(exper, '_summary_plots_per_diff.pdf'), height = 11, width = 8.5, paper = "letter")
}

grid.draw(emissions_plot)
grid.newpage()
grid.draw(forcing_plot)
grid.newpage()
grid.draw(cloud_plot)
grid.newpage()
grid.draw(deposition_plot)
dev.off()