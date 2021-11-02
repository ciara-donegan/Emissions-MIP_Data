# ------------------------------------------------------------------------------
# Program Name: summary_plots_diff.R
# Authors: Hamza Ahsan
# Date Last Modified: October 28, 2021
# Program Purpose: Produces summary plots of the difference between the
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

# Specify region (i.e., global, land, sea, arctic, NH-land, NH-sea, SH-land, SH-sea,
# NH-pacific, NH-atlantic, NH-indian)
region <- "NH-indian"

# Define colorblind-friendly palette colors and associate with models (in case a  
# plot is missing a model, the color scheme will remain consistent)
cbPalette <- c("#0072B2", "#D55E00")

model_colors <- c('CESM1' = cbPalette[1], 'GISS' = cbPalette[2])
model_symbols <- c("CESM1" = 15, "GISS" = 17)

# ------------------------------------------------------------------------------

# Setup directory for shp-10p-red difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-10p-red/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_10p_red <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_10p_red$model <- rep_models

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar
# mass of SO2 and dividing by molar mass of air, invert sign of forcing variables 
# to be consistent with convention (i.e. positive value denotes a heating effect),
# then take the average over all years for each variable and calculate std dev
shp_10p_red_summary <- shp_10p_red %>% 
  dplyr::group_by(variable, model) %>%
  within(value <- ifelse(variable == "so2", 64.066 / 28.96, 1) * value) %>%
  within(value <- ifelse(variable %in% c("rlut", "rsut", "rlutcs", "rsutcs"), -1, 1) * value) %>%
  dplyr::summarise(shp_10p_red = mean(value), shp_10p_red_sd = sd(value))

#---------------------------------------------------

# Setup directory for shp-10p-red-1950 difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-10p-red-1950/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_10p_red_1950 <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_10p_red_1950$model <- rep_models

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar
# mass of SO2 and dividing by molar mass of air, invert sign of forcing variables 
# to be consistent with convention (i.e. positive value denotes a heating effect),
# then take the average over all years for each variable and calculate std dev
shp_10p_red_1950_summary <- shp_10p_red_1950 %>% 
  dplyr::group_by(variable, model) %>%
  within(value <- ifelse(variable == "so2", 64.066 / 28.96, 1) * value) %>%
  within(value <- ifelse(variable %in% c("rlut", "rsut", "rlutcs", "rsutcs"), -1, 1) * value) %>%
  dplyr::summarise(shp_10p_red_1950 = mean(value), shp_10p_red_1950_sd = sd(value))

#---------------------------------------------------

# Setup directory for shp-20p-red difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-20p-red/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_20p_red <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_20p_red$model <- rep_models

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar
# mass of SO2 and dividing by molar mass of air, invert sign of forcing variables 
# to be consistent with convention (i.e. positive value denotes a heating effect),
# then take the average over all years for each variable and calculate std dev
shp_20p_red_summary <- shp_20p_red %>% 
  dplyr::group_by(variable, model) %>%
  within(value <- ifelse(variable == "so2", 64.066 / 28.96, 1) * value) %>%
  within(value <- ifelse(variable %in% c("rlut", "rsut", "rlutcs", "rsutcs"), -1, 1) * value) %>%
  dplyr::summarise(shp_20p_red = mean(value), shp_20p_red_sd = sd(value))

#---------------------------------------------------

# Setup directory for shp-20p-red-1950 difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-20p-red-1950/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_20p_red_1950 <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_20p_red_1950$model <- rep_models

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar
# mass of SO2 and dividing by molar mass of air, invert sign of forcing variables 
# to be consistent with convention (i.e. positive value denotes a heating effect),
# then take the average over all years for each variable and calculate std dev
shp_20p_red_1950_summary <- shp_20p_red_1950 %>% 
  dplyr::group_by(variable, model) %>%
  within(value <- ifelse(variable == "so2", 64.066 / 28.96, 1) * value) %>%
  within(value <- ifelse(variable %in% c("rlut", "rsut", "rlutcs", "rsutcs"), -1, 1) * value) %>%
  dplyr::summarise(shp_20p_red_1950 = mean(value), shp_20p_red_1950_sd = sd(value))

#---------------------------------------------------

# Setup directory for shp-80p-red difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-80p-red/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_80p_red <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_80p_red$model <- rep_models

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar
# mass of SO2 and dividing by molar mass of air, invert sign of forcing variables 
# to be consistent with convention (i.e. positive value denotes a heating effect),
# then take the average over all years for each variable and calculate std dev
shp_80p_red_summary <- shp_80p_red %>% 
  dplyr::group_by(variable, model) %>%
  within(value <- ifelse(variable == "so2", 64.066 / 28.96, 1) * value) %>%
  within(value <- ifelse(variable %in% c("rlut", "rsut", "rlutcs", "rsutcs"), -1, 1) * value) %>%
  dplyr::summarise(shp_80p_red = mean(value), shp_80p_red_sd = sd(value))

#---------------------------------------------------

# Setup directory for shp-atl-shift difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-atl-shift/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_atl_shift <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_atl_shift$model <- rep_models

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar
# mass of SO2 and dividing by molar mass of air, invert sign of forcing variables 
# to be consistent with convention (i.e. positive value denotes a heating effect),
# then take the average over all years for each variable and calculate std dev
shp_atl_shift_summary <- shp_atl_shift %>% 
  dplyr::group_by(variable, model) %>%
  within(value <- ifelse(variable == "so2", 64.066 / 28.96, 1) * value) %>%
  within(value <- ifelse(variable %in% c("rlut", "rsut", "rlutcs", "rsutcs"), -1, 1) * value) %>%
  dplyr::summarise(shp_atl_shift = mean(value), shp_atl_shift_sd = sd(value))

#---------------------------------------------------

# Setup directory for shp-atl-shift-1950 difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-atl-shift-1950/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_atl_shift_1950 <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_atl_shift_1950$model <- rep_models

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar
# mass of SO2 and dividing by molar mass of air, invert sign of forcing variables 
# to be consistent with convention (i.e. positive value denotes a heating effect),
# then take the average over all years for each variable and calculate std dev
shp_atl_shift_1950_summary <- shp_atl_shift_1950 %>% 
  dplyr::group_by(variable, model) %>%
  within(value <- ifelse(variable == "so2", 64.066 / 28.96, 1) * value) %>%
  within(value <- ifelse(variable %in% c("rlut", "rsut", "rlutcs", "rsutcs"), -1, 1) * value) %>%
  dplyr::summarise(shp_atl_shift_1950 = mean(value), shp_atl_shift_1950_sd = sd(value))

#---------------------------------------------------

# Setup directory for shp-ind-shift difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-ind-shift/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_ind_shift <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_ind_shift$model <- rep_models

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar
# mass of SO2 and dividing by molar mass of air, invert sign of forcing variables 
# to be consistent with convention (i.e. positive value denotes a heating effect),
# then take the average over all years for each variable and calculate std dev
shp_ind_shift_summary <- shp_ind_shift %>% 
  dplyr::group_by(variable, model) %>%
  within(value <- ifelse(variable == "so2", 64.066 / 28.96, 1) * value) %>%
  within(value <- ifelse(variable %in% c("rlut", "rsut", "rlutcs", "rsutcs"), -1, 1) * value) %>%
  dplyr::summarise(shp_ind_shift = mean(value), shp_ind_shift_sd = sd(value))

#---------------------------------------------------

# Setup directory for shp-ind-shift-1950 difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-ind-shift-1950/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_ind_shift_1950 <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_ind_shift_1950$model <- rep_models

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar
# mass of SO2 and dividing by molar mass of air, invert sign of forcing variables 
# to be consistent with convention (i.e. positive value denotes a heating effect),
# then take the average over all years for each variable and calculate std dev
shp_ind_shift_1950_summary <- shp_ind_shift_1950 %>% 
  dplyr::group_by(variable, model) %>%
  within(value <- ifelse(variable == "so2", 64.066 / 28.96, 1) * value) %>%
  within(value <- ifelse(variable %in% c("rlut", "rsut", "rlutcs", "rsutcs"), -1, 1) * value) %>%
  dplyr::summarise(shp_ind_shift_1950 = mean(value), shp_ind_shift_1950_sd = sd(value))

#---------------------------------------------------

# Bind data together
summary_data <- list(shp_10p_red_summary, 
                     shp_10p_red_1950_summary, 
                     shp_20p_red_summary, 
                     shp_20p_red_1950_summary, 
                     shp_80p_red_summary, 
                     shp_atl_shift_summary, 
                     shp_atl_shift_1950_summary, 
                     shp_ind_shift_summary, 
                     shp_ind_shift_1950_summary) %>% 
    reduce(left_join, by = c("variable", "model"))

# Correct model names
summary_data$model[which(summary_data$model == "CESM")] <- "CESM1"

# Change to long format
summary_long_exp <- summary_data %>% 
  gather(experiment, value, -c(model, 
                               variable, 
                               shp_10p_red_sd, 
                               shp_10p_red_1950_sd, 
                               shp_20p_red_sd, 
                               shp_20p_red_1950_sd, 
                               shp_80p_red_sd, 
                               shp_atl_shift_sd,
                               shp_atl_shift_1950_sd, 
                               shp_ind_shift_sd, 
                               shp_ind_shift_1950_sd)) %>%
  select(variable, model, experiment, value) %>%
  drop_na()

summary_long_sd <- summary_data %>% 
  gather(experiment, sd, -c(model, 
                            variable, 
                            shp_10p_red, 
                            shp_10p_red_1950, 
                            shp_20p_red, 
                            shp_20p_red_1950, 
                            shp_80p_red, 
                            shp_atl_shift, 
                            shp_atl_shift_1950, 
                            shp_ind_shift, 
                            shp_ind_shift_1950)) %>%
  select(variable, model, experiment, sd) %>%
  drop_na()

summary_long_sd$experiment <- gsub("_sd", "", summary_long_sd$experiment)

summary_long <- dplyr::left_join(summary_long_exp, summary_long_sd)

# Generate plots
title_font <- 9.5
axis_font <- 9
axis_title_font <- 9

emibc <- dplyr::filter(summary_long, variable == "emibc")
emibc_plot <- ggplot(emibc, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('surface flux of BC - ', region), y=expression(Delta*~emibc~(kg~m^-2~s^-1))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(emibc$value))-max(abs(emibc$sd)), max(abs(emibc$value))+max(abs(emibc$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


emiso2 <- dplyr::filter(summary_long, variable == "emiso2")
emiso2_plot <- ggplot(emiso2, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('surface flux of SO2 - ', region), y=expression(Delta*~emiso2~(kg~m^-2~s^-1))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(emiso2$value))-max(abs(emiso2$sd)), max(abs(emiso2$value))+max(abs(emiso2$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


mmrbc <- dplyr::filter(summary_long, variable == "mmrbc")
mmrbc_plot <- ggplot(mmrbc, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('surface concentration of BC - ', region), y=expression(Delta*~mmrbc~(kg~kg^-1))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(mmrbc$value))-max(abs(mmrbc$sd)), max(abs(mmrbc$value))+max(abs(mmrbc$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


mmrso4 <- dplyr::filter(summary_long, variable == "mmrso4")
mmrso4_plot <- ggplot(mmrso4, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('surface concentration of SO4 - ', region), y=expression(Delta*~mmrso4~(kg~kg^-1))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(mmrso4$value))-max(abs(mmrso4$sd)), max(abs(mmrso4$value))+max(abs(mmrso4$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


so2 <- dplyr::filter(summary_long, variable == "so2")
so2_plot <- ggplot(so2, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('surface concentration of SO2 - ', region), y=expression(Delta*~so2~(kg~kg^-1))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(so2$value))-max(abs(so2$sd)), max(abs(so2$value))+max(abs(so2$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


rlut <- dplyr::filter(summary_long, variable == "rlut")
rlut_plot <- ggplot(rlut, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('longwave flux at TOA - \n', region), y=expression(Delta*~rlut~(W~m^-2))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(rlut$value))-max(abs(rlut$sd)), max(abs(rlut$value))+max(abs(rlut$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


rsut <- dplyr::filter(summary_long, variable == "rsut")
rsut_plot <- ggplot(rsut, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('shortwave flux at TOA - \n', region), y=expression(Delta*~rsut~(W~m^-2))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(rsut$value))-max(abs(rsut$sd)), max(abs(rsut$value))+max(abs(rsut$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


rsdt <- dplyr::filter(summary_long, variable == "rsdt")
rsdt_plot <- ggplot(rsdt, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('incident shortwave flux \n at TOA - ', region), y=expression(Delta*~rsdt~(W~m^-2))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(rsdt$value))-max(abs(rsdt$sd)), max(abs(rsdt$value))+max(abs(rsdt$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


rlutcs <- dplyr::filter(summary_long, variable == "rlutcs")
rlutcs_plot <- ggplot(rlutcs, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('clear-sky longwave flux \n at TOA - ', region), y=expression(Delta*~rlutcs~(W~m^-2))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(rlutcs$value))-max(abs(rlutcs$sd)), max(abs(rlutcs$value))+max(abs(rlutcs$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


rsutcs <- dplyr::filter(summary_long, variable == "rsutcs")
rsutcs_plot <- ggplot(rsutcs, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('clear-sky shortwave flux \n at TOA - ', region), y=expression(Delta*~rsutcs~(W~m^-2))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(rsutcs$value))-max(abs(rsutcs$sd)), max(abs(rsutcs$value))+max(abs(rsutcs$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


# Define normal and clear-sky net radiative flux (incident shortwave + incident longwave + shortwave + longwave, 
# but the incidents cancel out)
net_rad <- dplyr::left_join(rlut, rsut, by = c("model", "experiment"))
net_rad <- dplyr::mutate(net_rad, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, experiment, value, sd))

net_rad_cs <- dplyr::left_join(rlutcs, rsutcs, by = c("model", "experiment"))
net_rad_cs <- dplyr::mutate(net_rad_cs, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, experiment, value, sd))

# Define implied cloud response (net - clearsky) as a new variable to plot
imp_cld <- dplyr::left_join(net_rad, net_rad_cs, by = c("model", "experiment"))
imp_cld <- dplyr::mutate(imp_cld, value = value.x - value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, experiment, value, sd))

net_rad_plot <- ggplot(net_rad, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('net radiative flux at TOA - \n', region), y=expression(Delta*~(rlut~+~rsut)~(W~m^-2))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(net_rad$value))-max(abs(net_rad$sd)), max(abs(net_rad$value))+max(abs(net_rad$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


net_rad_cs_plot <- ggplot(net_rad_cs, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('clear-sky net radiative flux \n at TOA - ', region), y=expression(Delta*~(rlutcs~+~rsutcs)~(W~m^-2))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(net_rad_cs$value))-max(abs(net_rad_cs$sd)), max(abs(net_rad_cs$value))+max(abs(net_rad_cs$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


imp_cld_plot <- ggplot(imp_cld, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('implied cloud response at TOA - \n', region), y=expression(Delta*~(rlut~+~rsut~-~rlutcs~-~rsutcs)~(W~m^-2))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(imp_cld$value))-max(abs(imp_cld$sd)), max(abs(imp_cld$value))+max(abs(imp_cld$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


drybc <- dplyr::filter(summary_long, variable == "drybc")
drybc_plot <- ggplot(drybc, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('dry deposition rate \n of BC - ', region), y=expression(Delta*~drybc~(kg~m^-2~s^-1))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(drybc$value))-max(abs(drybc$sd)), max(abs(drybc$value))+max(abs(drybc$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


wetbc <- dplyr::filter(summary_long, variable == "wetbc")
wetbc_plot <- ggplot(wetbc, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('wet deposition rate \n of BC - ', region), y=expression(Delta*~wetbc~(kg~m^-2~s^-1))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(wetbc$value))-max(abs(wetbc$sd)), max(abs(wetbc$value))+max(abs(wetbc$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


# Define total BC deposition rate (sum of dry and wet BC )
tot_bc <- dplyr::left_join(drybc, wetbc, by = c("model", "experiment"))
tot_bc <- dplyr::mutate(tot_bc, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, experiment, value, sd))

tot_bc_plot <- ggplot(tot_bc, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('total deposition rate \n of BC - ', region), y=expression(Delta*~(drybc~+~wetbc)~(kg~m^-2~s^-1))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(tot_bc$value))-max(abs(tot_bc$sd)), max(abs(tot_bc$value))+max(abs(tot_bc$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


dryso2 <- dplyr::filter(summary_long, variable == "dryso2")
dryso2_plot <- ggplot(dryso2, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('dry deposition rate \n of so2 - ', region), y=expression(Delta*~dryso2~(kg~m^-2~s^-1))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(dryso2$value))-max(abs(dryso2$sd)), max(abs(dryso2$value))+max(abs(dryso2$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


wetso2 <- dplyr::filter(summary_long, variable == "wetso2")
wetso2_plot <- ggplot(wetso2, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('wet deposition rate \n of so2 - ', region), y=expression(Delta*~wetso2~(kg~m^-2~s^-1))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(wetso2$value))-max(abs(wetso2$sd)), max(abs(wetso2$value))+max(abs(wetso2$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


dryso4 <- dplyr::filter(summary_long, variable == "dryso4")
dryso4_plot <- ggplot(dryso4, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('dry deposition rate \n of so4 - ', region), y=expression(Delta*~dryso4~(kg~m^-2~s^-1))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(dryso4$value))-max(abs(dryso4$sd)), max(abs(dryso4$value))+max(abs(dryso4$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


wetso4 <- dplyr::filter(summary_long, variable == "wetso4")
wetso4_plot <- ggplot(wetso4, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('wet deposition rate \n of so4 - ', region), y=expression(Delta*~wetso4~(kg~m^-2~s^-1))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(wetso4$value))-max(abs(wetso4$sd)), max(abs(wetso4$value))+max(abs(wetso4$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


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

tot_s_plot <- ggplot(tot_s, aes(x = experiment, y = value, color = model, shape = model)) +
  theme_bw() +
  labs(title=paste0('total deposition rate \n of S - ', region), y=expression(atop(Delta*~(dryso2~+~wetso2)/2~+~(dryso4~+~wetso4)/3, (kg~m^-2~s^-1)))) +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(tot_s$value))-max(abs(tot_s$sd)), max(abs(tot_s$value))+max(abs(tot_s$sd)))) +
  scale_colour_manual(values = model_colors) +
  scale_shape_manual(values = model_symbols) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)

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

pdf(paste0(region, '_summary_plots_diff.pdf'), height = 11, width = 8.5, paper = "letter")
grid.draw(emissions_plot)
grid.newpage()
grid.draw(forcing_plot)
grid.newpage()
grid.draw(deposition_plot)
dev.off()
