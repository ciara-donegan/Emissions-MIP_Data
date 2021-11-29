# ------------------------------------------------------------------------------
# Program Name: summary_plots_per_diff.R
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
emi_dir <- paste0('C:/Users/ahsa361/Documents/Emissions-MIP_Data')

# Specify region by command line
region <- commandArgs(trailingOnly = TRUE) #pulling region from command line
region <- region[1] #replaces regions with the first trailing string in the command line

# Specify region (i.e., global, land, sea, arctic, NH-land, NH-sea, SH-land, SH-sea,
# NH-pacific, NH-atlantic, NH-indian)
region <- "NH-indian"

# Define colorblind-friendly palette colors and associate with models (in case a
# plot is missing a model, the color scheme will remain consistent)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#920000",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#490092")

model_colors <- c(CESM1 = cbPalette[1], E3SM = cbPalette[2], GISS = cbPalette[3],
                  CESM2 = cbPalette[4], MIROC = cbPalette[5], NorESM2 = cbPalette[6],
                  GFDL = cbPalette[7], OsloCTM3 = cbPalette[8], UKESM = cbPalette[9],
                  GEOS = cbPalette[10])

model_symbols <- c(CESM1 = 15, E3SM = 15, GISS = 17, CESM2 = 19, MIROC = 15,
                   NorESM2 = 17, GFDL = 19, OsloCTM3 = 19, UKESM = 15, GEOS = 17)

# ------------------------------------------------------------------------------
# Reads in csv file specifying which models to exclude from the data
excluded_models <- read.csv(file = paste0(emi_dir, '/input/excluded_data.csv'), fileEncoding="UTF-8-BOM")
excluded_models %>% drop_na() #gets rid of any empty spaces
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

  summary_long_sd$experiment <- gsub("_sd", "", summary_long_sd$experiment)

  summary_long <- dplyr::left_join(summary_long_exp, summary_long_sd)

  summary_long_sd$experiment <- gsub("_sd", "", summary_long_sd$experiment)

  summary_long <- dplyr::left_join(summary_long_exp, summary_long_sd)

# Runs through each excluded model pair and filters them out of summary_long
if(nrow(excluded_models) != 0) { #only runs if the data frame is not empty
  for (val in 1:nrow(excluded_models)) {
    summary_long <- filter(summary_long, experiment != excluded_models$Scenario[val] | model != excluded_models$Model[val])
  }
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
    top = textGrob("Summary - percent difference", gp = gpar(fontsize = 12)))
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
setwd(paste0('../../../../output/', region, '/summary'))

pdf(paste0(region, '_summary_plots_per_diff.pdf'), height = 11, width = 8.5, paper = "letter")
grid.draw(emissions_plot)
grid.newpage()
grid.draw(forcing_plot)
grid.newpage()
grid.draw(cloud_plot)
grid.newpage()
grid.draw(deposition_plot)
dev.off()
