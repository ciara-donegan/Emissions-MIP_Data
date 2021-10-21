# ------------------------------------------------------------------------------
# Program Name: summary_plots_diff.R
# Authors: Hamza Ahsan
# Date Last Modified: September 29, 2021
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
emi_dir <- paste0('C:/Users/ahsa361/OneDrive - PNNL/Desktop/Emissions-MIP-Phase1a')

# Specify region (i.e., global, land, sea, arctic, NH-land, NH-sea, SH-land, 
# SH-sea, NH-atlantic, NH-pacific)
region <- "NH-pacific"

# Define default ggplot colors and associate with models (in case a plot is
# missing a model, the color scheme will remain consistent)
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(10)


model_colors <- c(CESM1 = cols[1], E3SM = cols[2], GISS = cols[3], CESM2 = cols[4],
                  MIROC = cols[5], NorESM2 = cols[6], GFDL = cols[7], OsloCTM3 = cols[8],
                  UKESM = cols[9], GEOS = cols[10])

# ------------------------------------------------------------------------------
#Load the csv file
excluded_models <- read.csv(file = paste0(emi_dir, '/input', '/excluded_data.csv'), fileEncoding="UTF-8-BOM")
excluded_models %>% drop_na() #gets rid of any empty spaces

#-------------------------------------------------------------------------------


# Setup directory for bc-no-seas difference data
setwd(paste0(emi_dir, '/input/', region, '/bc-no-season/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
bc_no_seas <- rbind(map(target_filename, read.csv))
bc_no_seas <- lapply(bc_no_seas, function(x) {x["unit"] <- NULL; x})
bc_no_seas <- bind_rows(bc_no_seas)

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 4) # four years
bc_no_seas$model <- rep_models

# Invert sign of CESM2 wet deposition variables
bc_no_seas$value[which(bc_no_seas$model == "CESM2" & bc_no_seas$variable == "wetbc")] <- 
  -1 * bc_no_seas$value[which(bc_no_seas$model == "CESM2" & bc_no_seas$variable == "wetbc")]
bc_no_seas$value[which(bc_no_seas$model == "CESM2" & bc_no_seas$variable == "wetso2")] <- 
  -1 * bc_no_seas$value[which(bc_no_seas$model == "CESM2" & bc_no_seas$variable == "wetso2")]
bc_no_seas$value[which(bc_no_seas$model == "CESM2" & bc_no_seas$variable == "wetso4")] <- 
  -1 * bc_no_seas$value[which(bc_no_seas$model == "CESM2" & bc_no_seas$variable == "wetso4")]

# Take the average over all years for each variable and calculate std. dev.
bc_no_seas_summary <- bc_no_seas %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(bc_no_seas = mean(value), bc_no_seas_sd = sd(value))

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar mass
# of SO2 and dividing by molar mass of air
bc_no_seas_summary$bc_no_seas[which(bc_no_seas_summary$variable == "so2")] <- 
  bc_no_seas_summary$bc_no_seas[which(bc_no_seas_summary$variable == "so2")] * 64.066 / 28.96

bc_no_seas_summary$bc_no_seas_sd[which(bc_no_seas_summary$variable == "so2")] <- 
  bc_no_seas_summary$bc_no_seas_sd[which(bc_no_seas_summary$variable == "so2")] * 64.066 / 28.96

#---------------------------------------------------

# Setup directory for high-SO4 difference data
setwd(paste0(emi_dir, '/input/', region, '/high-SO4/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
high_so4 <- rbind(map(target_filename, read.csv))
high_so4 <- lapply(high_so4, function(x) {x["unit"] <- NULL; x})
high_so4 <- bind_rows(high_so4)

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 4) # four years
high_so4$model <- rep_models

# Invert sign of CESM2 wet deposition variables
high_so4$value[which(high_so4$model == "CESM2" & high_so4$variable == "wetbc")] <- 
  -1 * high_so4$value[which(high_so4$model == "CESM2" & high_so4$variable == "wetbc")]
high_so4$value[which(high_so4$model == "CESM2" & high_so4$variable == "wetso2")] <- 
  -1 * high_so4$value[which(high_so4$model == "CESM2" & high_so4$variable == "wetso2")]
high_so4$value[which(high_so4$model == "CESM2" & high_so4$variable == "wetso4")] <- 
  -1 * high_so4$value[which(high_so4$model == "CESM2" & high_so4$variable == "wetso4")]

# Take the average over all years for each variable and calculate std. dev.
high_so4_summary <- high_so4 %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(high_so4 = mean(value), high_so4_sd = sd(value))

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar mass
# of SO2 and dividing by molar mass of air
high_so4_summary$high_so4[which(high_so4_summary$variable == "so2")] <- 
  high_so4_summary$high_so4[which(high_so4_summary$variable == "so2")] * 64.066 / 28.96

high_so4_summary$high_so4_sd[which(high_so4_summary$variable == "so2")] <- 
  high_so4_summary$high_so4_sd[which(high_so4_summary$variable == "so2")] * 64.066 / 28.96

#---------------------------------------------------

# Setup directory for no-SO4 difference data
setwd(paste0(emi_dir, '/input/', region, '/no-SO4/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
no_so4 <- rbind(map(target_filename, read.csv))
no_so4 <- lapply(no_so4, function(x) {x["unit"] <- NULL; x})
no_so4 <- bind_rows(no_so4)

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 4) # four years
no_so4$model <- rep_models

# Invert sign of CESM2 wet deposition variables
no_so4$value[which(no_so4$model == "CESM2" & no_so4$variable == "wetbc")] <- 
  -1 * no_so4$value[which(no_so4$model == "CESM2" & no_so4$variable == "wetbc")]
no_so4$value[which(no_so4$model == "CESM2" & no_so4$variable == "wetso2")] <- 
  -1 * no_so4$value[which(no_so4$model == "CESM2" & no_so4$variable == "wetso2")]
no_so4$value[which(no_so4$model == "CESM2" & no_so4$variable == "wetso4")] <- 
  -1 * no_so4$value[which(no_so4$model == "CESM2" & no_so4$variable == "wetso4")]

# Take the average over all years for each variable and calculate std. dev.
no_so4_summary <- no_so4 %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(no_so4 = mean(value), no_so4_sd = sd(value))

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar mass
# of SO2 and dividing by molar mass of air
no_so4_summary$no_so4[which(no_so4_summary$variable == "so2")] <- 
  no_so4_summary$no_so4[which(no_so4_summary$variable == "so2")] * 64.066 / 28.96

no_so4_summary$no_so4_sd[which(no_so4_summary$variable == "so2")] <- 
  no_so4_summary$no_so4_sd[which(no_so4_summary$variable == "so2")] * 64.066 / 28.96

#---------------------------------------------------

# Setup directory for SO2-at-height difference data
setwd(paste0(emi_dir, '/input/', region, '/so2-at-height/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
so2_at_hgt <- rbind(map(target_filename, read.csv))
so2_at_hgt <- lapply(so2_at_hgt, function(x) {x["unit"] <- NULL; x})
so2_at_hgt <- bind_rows(so2_at_hgt)

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 4) # four years
so2_at_hgt$model <- rep_models

# Take the average over all years for each variable and calculate std. dev.
so2_at_hgt_summary <- so2_at_hgt %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(so2_at_hgt = mean(value), so2_at_hgt_sd = sd(value))

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar mass
# of SO2 and dividing by molar mass of air
so2_at_hgt_summary$so2_at_hgt[which(so2_at_hgt_summary$variable == "so2")] <- 
  so2_at_hgt_summary$so2_at_hgt[which(so2_at_hgt_summary$variable == "so2")] * 64.066 / 28.96

so2_at_hgt_summary$so2_at_hgt_sd[which(so2_at_hgt_summary$variable == "so2")] <- 
  so2_at_hgt_summary$so2_at_hgt_sd[which(so2_at_hgt_summary$variable == "so2")] * 64.066 / 28.96

#---------------------------------------------------

# Setup directory for SO2-no-season difference data
setwd(paste0(emi_dir, '/input/', region, '/so2-no-season/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
so2_no_seas <- rbind(map(target_filename, read.csv))
so2_no_seas <- lapply(so2_no_seas, function(x) {x["unit"] <- NULL; x})
so2_no_seas <- bind_rows(so2_no_seas)

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 4) # four years
so2_no_seas$model <- rep_models

# Invert sign of CESM2 wet deposition variables
so2_no_seas$value[which(so2_no_seas$model == "CESM2" & so2_no_seas$variable == "wetbc")] <- 
  -1 * so2_no_seas$value[which(so2_no_seas$model == "CESM2" & so2_no_seas$variable == "wetbc")]
so2_no_seas$value[which(so2_no_seas$model == "CESM2" & so2_no_seas$variable == "wetso2")] <- 
  -1 * so2_no_seas$value[which(so2_no_seas$model == "CESM2" & so2_no_seas$variable == "wetso2")]
so2_no_seas$value[which(so2_no_seas$model == "CESM2" & so2_no_seas$variable == "wetso4")] <- 
  -1 * so2_no_seas$value[which(so2_no_seas$model == "CESM2" & so2_no_seas$variable == "wetso4")]

# Take the average over all years for each variable and calculate std. dev.
so2_no_seas_summary <- so2_no_seas %>% dplyr::group_by(variable, model) %>%
  dplyr::summarise(so2_no_seas = mean(value), so2_no_seas_sd = sd(value))

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar mass
# of SO2 and dividing by molar mass of air
so2_no_seas_summary$so2_no_seas[which(so2_no_seas_summary$variable == "so2")] <- 
  so2_no_seas_summary$so2_no_seas[which(so2_no_seas_summary$variable == "so2")] * 64.066 / 28.96

so2_no_seas_summary$so2_no_seas_sd[which(so2_no_seas_summary$variable == "so2")] <- 
  so2_no_seas_summary$so2_no_seas_sd[which(so2_no_seas_summary$variable == "so2")] * 64.066 / 28.96

#---------------------------------------------------

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

summary_long_sd$experiment <- gsub("_sd", "", summary_long_sd$experiment)

summary_long <- dplyr::left_join(summary_long_exp, summary_long_sd)

#runs through each excluded model pair and filters them out of summary_long
if(nrow(excluded_models) != 0) { #only runs if the data frame is not empty
  for (val in 1:nrow(excluded_models)) {
    summary_long <- filter(summary_long, experiment != excluded_models$Scenario[val] | model != excluded_models$Model[val])
  }
}


# Generate plots
title_font <- 9.5
axis_font <- 9
axis_title_font <- 9

emibc <- dplyr::filter(summary_long, variable == "emibc")
emibc_plot <- ggplot(emibc, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('surface flux of BC - ', region), y="emibc (kg m-2 s-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(emibc$value))-max(abs(emibc$sd)), max(abs(emibc$value))+max(abs(emibc$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


emiso2 <- dplyr::filter(summary_long, variable == "emiso2")
emiso2_plot <- ggplot(emiso2, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('surface flux of SO2 - ', region), y="emiso2 (kg m-2 s-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +

  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(emiso2$value))-max(abs(emiso2$sd)), max(abs(emiso2$value))+max(abs(emiso2$sd)))) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)
  

mmrbc <- dplyr::filter(summary_long, variable == "mmrbc")
mmrbc_plot <- ggplot(mmrbc, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('surface concentration of BC - ', region), y="mmrbc (kg kg-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(mmrbc$value))-max(abs(mmrbc$sd)), max(abs(mmrbc$value))+max(abs(mmrbc$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


mmrso4 <- dplyr::filter(summary_long, variable == "mmrso4")
mmrso4_plot <- ggplot(mmrso4, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('surface concentration of SO4 - ', region), y="mmrso4 (kg kg-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(mmrso4$value))-max(abs(mmrso4$sd)), max(abs(mmrso4$value))+max(abs(mmrso4$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


so2 <- dplyr::filter(summary_long, variable == "so2")
so2_plot <- ggplot(so2, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('surface concentration of SO2 - ', region), y="so2 (kg kg-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(so2$value))-max(abs(so2$sd)), max(abs(so2$value))+max(abs(so2$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


rlut <- dplyr::filter(summary_long, variable == "rlut")
rlut_plot <- ggplot(rlut, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('upwelling longwave flux \n at TOA - ', region), y="rlut (W m-2)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(rlut$value))-max(abs(rlut$sd)), max(abs(rlut$value))+max(abs(rlut$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


rsut <- dplyr::filter(summary_long, variable == "rsut")
rsut_plot <- ggplot(rsut, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('upwelling shortwave flux \n at TOA - ', region), y="rsut (W m-2)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(rsut$value))-max(abs(rsut$sd)), max(abs(rsut$value))+max(abs(rsut$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


rsdt <- dplyr::filter(summary_long, variable == "rsdt")
rsdt_plot <- ggplot(rsdt, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('incident shortwave flux \n at TOA - ', region), y="rsdt (W m-2)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(rsdt$value))-max(abs(rsdt$sd)), max(abs(rsdt$value))+max(abs(rsdt$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


rlutcs <- dplyr::filter(summary_long, variable == "rlutcs")
rlutcs_plot <- ggplot(rlutcs, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('upwelling clear-sky longwave \n flux at TOA - ', region), y="rlutcs (W m-2)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(rlutcs$value))-max(abs(rlutcs$sd)), max(abs(rlutcs$value))+max(abs(rlutcs$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


rsutcs <- dplyr::filter(summary_long, variable == "rsutcs")
rsutcs_plot <- ggplot(rsutcs, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('upwelling clear-sky shortwave \n flux at TOA - ', region), y="rsutcs (W m-2)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(rsutcs$value))-max(abs(rsutcs$sd)), max(abs(rsutcs$value))+max(abs(rsutcs$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


# Define normal and clear-sky net radiative flux (sum of longwave and shortwave radiation)
net_rad <- dplyr::left_join(rlut, rsut, by = c("model", "experiment"))
net_rad <- dplyr::mutate(net_rad, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, experiment, value, sd))

net_rad_cs <- dplyr::left_join(rlutcs, rsutcs, by = c("model", "experiment"))
net_rad_cs <- dplyr::mutate(net_rad_cs, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, experiment, value, sd))


net_rad_plot <- ggplot(net_rad, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('net radiative flux \n at TOA - ', region), y="rlut + rsut (W m-2)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(net_rad$value))-max(abs(net_rad$sd)), max(abs(net_rad$value))+max(abs(net_rad$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


net_rad_cs_plot <- ggplot(net_rad_cs, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('clear-sky net radiative flux \n at TOA - ', region), y="rlutcs + rsutcs (W m-2)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(net_rad_cs$value))-max(abs(net_rad_cs$sd)), max(abs(net_rad_cs$value))+max(abs(net_rad_cs$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


drybc <- dplyr::filter(summary_long, variable == "drybc")
drybc_plot <- ggplot(drybc, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('dry deposition rate \n of BC - ', region), y="drybc (kg m-2 s-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(drybc$value))-max(abs(drybc$sd)), max(abs(drybc$value))+max(abs(drybc$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


wetbc <- dplyr::filter(summary_long, variable == "wetbc")
wetbc_plot <- ggplot(wetbc, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('wet deposition rate \n of BC - ', region), y="wetbc (kg m-2 s-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(wetbc$value))-max(abs(wetbc$sd)), max(abs(wetbc$value))+max(abs(wetbc$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


# Define total BC deposition rate (sum of dry and wet BC )
tot_bc <- dplyr::left_join(drybc, wetbc, by = c("model", "experiment"))
tot_bc <- dplyr::mutate(tot_bc, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, experiment, value, sd))

tot_bc_plot <- ggplot(tot_bc, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('total deposition rate \n of BC - ', region), y="drybc + wetbc (kg m-2 s-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(tot_bc$value))-max(abs(tot_bc$sd)), max(abs(tot_bc$value))+max(abs(tot_bc$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


dryso2 <- dplyr::filter(summary_long, variable == "dryso2")
dryso2_plot <- ggplot(dryso2, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('dry deposition rate \n of so2 - ', region), y="dryso2 (kg m-2 s-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(dryso2$value))-max(abs(dryso2$sd)), max(abs(dryso2$value))+max(abs(dryso2$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


wetso2 <- dplyr::filter(summary_long, variable == "wetso2")
wetso2_plot <- ggplot(wetso2, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('wet deposition rate \n of so2 - ', region), y="wetso2 (kg m-2 s-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(wetso2$value))-max(abs(wetso2$sd)), max(abs(wetso2$value))+max(abs(wetso2$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


dryso4 <- dplyr::filter(summary_long, variable == "dryso4")
dryso4_plot <- ggplot(dryso4, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('dry deposition rate \n of so4 - ', region), y="dryso4 (kg m-2 s-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(dryso4$value))-max(abs(dryso4$sd)), max(abs(dryso4$value))+max(abs(dryso4$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


wetso4 <- dplyr::filter(summary_long, variable == "wetso4")
wetso4_plot <- ggplot(wetso4, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('wet deposition rate \n of so4 - ', region), y="wetso4 (kg m-2 s-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(wetso4$value))-max(abs(wetso4$sd)), max(abs(wetso4$value))+max(abs(wetso4$sd)))) +
  scale_colour_manual(values = model_colors) +
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

tot_s_plot <- ggplot(tot_s, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('total deposition rate \n of S - ', region), y="(dryso2 + wetso2)/2 + (dryso4 + wetso4)/3 (kg m-2 s-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(tot_s$value))-max(abs(tot_s$sd)), max(abs(tot_s$value))+max(abs(tot_s$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


od550aer <- dplyr::filter(summary_long, variable == "od550aer")
od550aer_plot <- ggplot(od550aer, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('ambient aerosol optical \n thickness at 550nm - ', region), y="od550aer") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(od550aer$value))-max(abs(od550aer$sd)), max(abs(od550aer$value))+max(abs(od550aer$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


clt <- dplyr::filter(summary_long, variable == "clt")
clt_plot <- ggplot(clt, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('total cloud cover \n percentage - ', region), y="clt (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(clt$value))-max(abs(clt$sd)), max(abs(clt$value))+max(abs(clt$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


cltc <- dplyr::filter(summary_long, variable == "cltc")
cltc_plot <- ggplot(cltc, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('convective cloud cover \n percentage - ', region), y="cltc (%)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(cltc$value))-max(abs(cltc$sd)), max(abs(cltc$value))+max(abs(cltc$sd)))) +
  scale_colour_manual(values = model_colors) +
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
                                          od550aer_plot,
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

pdf(paste0(region, '_summary_plots_diff.pdf'), height = 11, width = 8.5, paper = "letter")
grid.draw(emissions_plot)
grid.newpage()
grid.draw(forcing_plot)
grid.newpage()
grid.draw(deposition_plot)
dev.off()
