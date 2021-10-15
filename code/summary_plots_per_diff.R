# ------------------------------------------------------------------------------
# Program Name: summary_plots_per_diff.R
# Authors: Hamza Ahsan
# Date Last Modified: Sept 3, 2021
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
emi_dir <- paste0('C:/Users/ahsa361/OneDrive - PNNL/Desktop/Emissions-MIP-Phase1b')

# Specify region (i.e., global, land, sea, arctic, NH-land, NH-sea, SH-land, SH-sea,
# NH-pacific, NH-atlantic)
region <- "NH-atlantic"

# Define default ggplot colors and associate with models (in case a plot is 
# missing a model, the color scheme will remain consistent)
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(5)

model_colors <- c("CESM1" = cols[1], "GISS" = cols[2], "GISS (SO2)" = cols[4])

# ------------------------------------------------------------------------------

# Setup directory for shp-10p-red percent difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-10p-red/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_10p_red <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_10p_red$model <- rep_models

# Take the average over all years for each variable
shp_10p_red_summary <- shp_10p_red %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(shp_10p_red = mean(value))

#---------------------------------------------------

# Setup directory for shp-10p-red-1950 percent difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-10p-red-1950/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_10p_red_1950 <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_10p_red_1950$model <- rep_models

# Take the average over all years for each variable
shp_10p_red_1950_summary <- shp_10p_red_1950 %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(shp_10p_red_1950 = mean(value))

#---------------------------------------------------

# Setup directory for shp-20p-red percent difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-20p-red/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_20p_red <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_20p_red$model <- rep_models

# Take the average over all years for each variable
shp_20p_red_summary <- shp_20p_red %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(shp_20p_red = mean(value))

#---------------------------------------------------

# Setup directory for shp-20p-red-1950 percent difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-20p-red-1950/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_20p_red_1950 <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_20p_red_1950$model <- rep_models

# Take the average over all years for each variable
shp_20p_red_1950_summary <- shp_20p_red_1950 %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(shp_20p_red_1950 = mean(value))

#---------------------------------------------------

# Setup directory for shp-80p-red percent difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-80p-red/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_80p_red <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_80p_red$model <- rep_models

# Take the average over all years for each variable
shp_80p_red_summary <- shp_80p_red %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(shp_80p_red = mean(value))

#---------------------------------------------------

# Setup directory for shp-atl-shift percent difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-atl-shift/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_atl_shift <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_atl_shift$model <- rep_models

# Take the average over all years for each variable
shp_atl_shift_summary <- shp_atl_shift %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(shp_atl_shift = mean(value))

#---------------------------------------------------

# Setup directory for shp-atl-shift-1950 percent difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-atl-shift-1950/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_atl_shift_1950 <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_atl_shift_1950$model <- rep_models

# Take the average over all years for each variable
shp_atl_shift_1950_summary <- shp_atl_shift_1950 %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(shp_atl_shift_1950 = mean(value))

#---------------------------------------------------

# Setup directory for shp-ind-shift percent difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-ind-shift/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_ind_shift <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_ind_shift$model <- rep_models

# Take the average over all years for each variable
shp_ind_shift_summary <- shp_ind_shift %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(shp_ind_shift = mean(value))

#---------------------------------------------------

# Setup directory for shp-ind-shift-1950 percent difference data
setwd(paste0(emi_dir, '/input/', region, '/shp-ind-shift-1950/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
shp_ind_shift_1950 <- bind_rows(map(target_filename, read.csv))

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
shp_ind_shift_1950$model <- rep_models

# Take the average over all years for each variable
shp_ind_shift_1950_summary <- shp_ind_shift_1950 %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(shp_ind_shift_1950 = mean(value))

#---------------------------------------------------

# Bind data together
summary_data <- list(shp_10p_red_summary, shp_10p_red_1950_summary, shp_20p_red_summary, shp_20p_red_1950_summary, shp_80p_red_summary, shp_atl_shift_summary, shp_atl_shift_1950_summary, shp_ind_shift_summary, shp_ind_shift_1950_summary) %>% reduce(left_join, by = c("variable", "model"))

# Correct model names
summary_data$model[which(summary_data$model == "CESM")] <- "CESM1"
summary_data$model[which(summary_data$model == "GISS_SO2")] <- "GISS (SO2)"

# Change to long format
summary_long <- summary_data %>% gather(experiment, value, -c(model, variable)) %>%
  drop_na()

# Generate plots
title_font <- 9.5
axis_font <- 9
axis_title_font <- 9

emibc <- dplyr::filter(summary_long, variable == "emibc")
emibc_plot <- ggplot(emibc, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('surface flux of BC - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(emibc$value)), max(abs(emibc$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


emiso2 <- dplyr::filter(summary_long, variable == "emiso2")
emiso2_plot <- ggplot(emiso2, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('surface flux of SO2 - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(emiso2$value)), max(abs(emiso2$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


mmrbc <- dplyr::filter(summary_long, variable == "mmrbc")
mmrbc_plot <- ggplot(mmrbc, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('surface concentration of BC - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(mmrbc$value)), max(abs(mmrbc$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


mmrso4 <- dplyr::filter(summary_long, variable == "mmrso4")
mmrso4_plot <- ggplot(mmrso4, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('surface concentration of SO4 - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(mmrso4$value)), max(abs(mmrso4$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


so2 <- dplyr::filter(summary_long, variable == "so2")
so2_plot <- ggplot(so2, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('surface concentration of SO2 - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(so2$value)), max(abs(so2$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


rlut <- dplyr::filter(summary_long, variable == "rlut")
rlut_plot <- ggplot(rlut, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('upwelling longwave flux \n at TOA - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(rlut$value)), max(abs(rlut$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


rsut <- dplyr::filter(summary_long, variable == "rsut")
rsut_plot <- ggplot(rsut, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('upwelling shortwave flux \n at TOA - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(rsut$value)), max(abs(rsut$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


rsdt <- dplyr::filter(summary_long, variable == "rsdt")
rsdt_plot <- ggplot(rsdt, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('incident shortwave flux \n at TOA - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(rsdt$value)), max(abs(rsdt$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


rlutcs <- dplyr::filter(summary_long, variable == "rlutcs")
rlutcs_plot <- ggplot(rlutcs, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('upwelling clear-sky longwave \n flux at TOA - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(rlutcs$value)), max(abs(rlutcs$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


rsutcs <- dplyr::filter(summary_long, variable == "rsutcs")
rsutcs_plot <- ggplot(rsutcs, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('upwelling clear-sky shortwave \n flux at TOA - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(rsutcs$value)), max(abs(rsutcs$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


# Define normal and clear-sky net radiative flux and  (sum of longwave and shortwave radiation)
net_rad <- dplyr::left_join(rlut, rsut, by = c("model", "experiment"))
net_rad <- dplyr::mutate(net_rad, value = value.x + value.y) %>%
  dplyr::select(c(model, experiment, value))

net_rad_cs <- dplyr::left_join(rlutcs, rsutcs, by = c("model", "experiment"))
net_rad_cs <- dplyr::mutate(net_rad_cs, value = value.x + value.y) %>%
  dplyr::select(c(model, experiment, value))


net_rad_plot <- ggplot(net_rad, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('net radiative flux at TOA - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(net_rad$value)), max(abs(net_rad$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


net_rad_cs_plot <- ggplot(net_rad_cs, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('clear-sky net radiative flux at TOA - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(net_rad_cs$value)), max(abs(net_rad_cs$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


drybc <- dplyr::filter(summary_long, variable == "drybc")
drybc_plot <- ggplot(drybc, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('dry deposition rate \n of BC - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(drybc$value)), max(abs(drybc$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


wetbc <- dplyr::filter(summary_long, variable == "wetbc")
wetbc_plot <- ggplot(wetbc, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('wet deposition rate \n of BC - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(wetbc$value)), max(abs(wetbc$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


# Define total BC deposition rate (sum of dry and wet BC )
tot_bc <- dplyr::left_join(drybc, wetbc, by = c("model", "experiment"))
tot_bc <- dplyr::mutate(tot_bc, value = value.x + value.y) %>%
  dplyr::select(c(model, experiment, value))
tot_bc_plot <- ggplot(tot_bc, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('total deposition rate \n of BC - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(tot_bc$value)), max(abs(tot_bc$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5)


dryso2 <- dplyr::filter(summary_long, variable == "dryso2")
dryso2_plot <- ggplot(dryso2, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('dry deposition rate \n of SO2 - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(dryso2$value)), max(abs(dryso2$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


wetso2 <- dplyr::filter(summary_long, variable == "wetso2")
wetso2_plot <- ggplot(wetso2, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('wet deposition rate \n of SO2 - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(wetso2$value)), max(abs(wetso2$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


dryso4 <- dplyr::filter(summary_long, variable == "dryso4")
dryso4_plot <- ggplot(dryso4, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('dry deposition rate \n of SO4 - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(dryso4$value)), max(abs(dryso4$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


wetso4 <- dplyr::filter(summary_long, variable == "wetso4")
wetso4_plot <- ggplot(wetso4, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title = paste0('wet deposition rate \n of SO4 - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(wetso4$value)), max(abs(wetso4$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width = 0.4), size = 1.5)


# Define total S deposition rate (sum of dry and wet SO2/SO4 )
dry_s <- dplyr::left_join(dryso2, dryso4, by = c("model", "experiment"))
dry_s <- dplyr::mutate(dry_s, value = value.x + value.y) %>%
  dplyr::select(c(model, experiment, value))

wet_s <- dplyr::left_join(wetso2, wetso4, by = c("model", "experiment"))
wet_s <- dplyr::mutate(wet_s, value = value.x + value.y) %>%
  dplyr::select(c(model, experiment, value))

tot_s <- dplyr::left_join(dry_s, wet_s, by = c("model", "experiment"))
tot_s <- dplyr::mutate(tot_s, value = value.x + value.y) %>%
  dplyr::select(c(model, experiment, value))
tot_s_plot <- ggplot(tot_s, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('total deposition rate \n of S - ', region), y="Percent") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(tot_s$value)), max(abs(tot_s$value)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5)


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
                                           net_rad_cs_plot)

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
grid.draw(deposition_plot)
dev.off()
