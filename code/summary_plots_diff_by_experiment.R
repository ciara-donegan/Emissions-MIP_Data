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

# Specify experiment (i.e., bc-no-seas, high-so4, no-so4, reference, so2-at-height, so2-no-seas)
exper <- c("bc-no-season")

#Creates a list of regions
regions <- c("arctic", "global", "land", "NH-atlantic", "NH-land", "NH-pacific", "NH-sea", "sea", "SH-land", "SH-sea")

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

# Define a function that accumulates data from a region for the specified experiment
data_accumulation <- function(emi_dir, reg_name, exper){
  
  setwd(paste0(emi_dir,'/input/', reg_name,'/',exper, '/diff'))
  
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
  
  # Take the average over all years for each variable and calculate std. dev.
  regional_data_summary <- regional_data %>% dplyr::group_by(variable, model) %>%
    dplyr::summarise(regional_data = mean(value), regional_data_sd = sd(value))
  
  # Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar mass
  # of SO2 and dividing by molar mass of air
  regional_data_summary$regional_data[which(regional_data_summary$variable == "so2")] <- 
    regional_data_summary$regional_data[which(regional_data_summary$variable == "so2")] * 64.066 / 28.96
  
  regional_data_summary$regional_data_sd[which(regional_data_summary$variable == "so2")] <- 
    regional_data_summary$regional_data_sd[which(regional_data_summary$variable == "so2")] * 64.066 / 28.96
  
  return(regional_data_summary)
}

#read in data for each region
arctic <- data_accumulation(emi_dir,'arctic',exper)
global <- data_accumulation(emi_dir,'global',exper)
land <- data_accumulation(emi_dir,'land',exper)
NH_atlantic <- data_accumulation(emi_dir,'NH-atlantic',exper)
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
NH_land <- rename(NH_land, NH_land_sd = regional_data_sd)
NH_pacific <- rename(NH_pacific, NH_pacific_sd = regional_data_sd)
NH_sea <- rename(NH_sea, NH_sea_sd = regional_data_sd)
sea <- rename(sea, sea_sd = regional_data_sd)
SH_land <- rename(SH_land, SH_land_sd = regional_data_sd)
SH_sea <- rename(SH_sea, SH_sea_sd = regional_data_sd)

# Bind data together
summary_data <- list(arctic, global, land, NH_atlantic, NH_land, NH_pacific, NH_sea, sea, SH_land, SH_sea) %>% reduce(left_join, by = c("variable", "model"))

# Correct model names for CESM and CESM2
summary_data$model[which(summary_data$model == "CESM")] <- "CESM1"

# Change to long format
summary_long_exp <- summary_data %>% 
  gather(region, value, -c(model, variable, arctic_sd, global_sd, land_sd, NH_atlantic_sd, NH_land_sd, NH_pacific_sd, NH_sea_sd, sea_sd, SH_land_sd, SH_sea_sd)) %>%
  select(variable, model, region, value) %>%
  drop_na()

summary_long_sd <- summary_data %>% 
  gather(region, sd, -c(model, variable, arctic, global, land, NH_atlantic, NH_land, NH_pacific, NH_sea, sea, SH_land, SH_sea)) %>%
  select(variable, model, region, sd) %>%
  drop_na()

summary_long_sd$region <- gsub("_sd", "", summary_long_sd$region)

summary_long <- dplyr::left_join(summary_long_exp, summary_long_sd)

# Generate plots
title_font <- 9.5
axis_font <- 9
axis_title_font <- 9

emibc <- dplyr::filter(summary_long, variable == "emibc")
emibc_plot <- ggplot(emibc, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('surface flux of BC - ', exper), y="emibc (kg m-2 s-1)") +
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
emiso2_plot <- ggplot(emiso2, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('surface flux of SO2 - ', exper), y="emiso2 (kg m-2 s-1)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(emiso2$value))-max(abs(emiso2$sd)), max(abs(emiso2$value))+max(abs(emiso2$sd)))) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


mmrbc <- dplyr::filter(summary_long, variable == "mmrbc")
mmrbc_plot <- ggplot(mmrbc, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('surface concentration of BC - ', exper), y="mmrbc (kg kg-1)") +
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
mmrso4_plot <- ggplot(mmrso4, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('surface concentration of SO4 - ', exper), y="mmrso4 (kg kg-1)") +
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
so2_plot <- ggplot(so2, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('surface concentration of SO2 - ', exper), y="so2 (kg kg-1)") +
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
rlut_plot <- ggplot(rlut, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('upwelling longwave flux \n at TOA - ', exper), y="rlut (W m-2)") +
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
rsut_plot <- ggplot(rsut, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('upwelling shortwave flux \n at TOA - ', exper), y="rsut (W m-2)") +
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
rsdt_plot <- ggplot(rsdt, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('incident shortwave flux \n at TOA - ', exper), y="rsdt (W m-2)") +
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
rlutcs_plot <- ggplot(rlutcs, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('upwelling clear-sky longwave \n flux at TOA - ', exper), y="rlutcs (W m-2)") +
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
rsutcs_plot <- ggplot(rsutcs, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('upwelling clear-sky shortwave \n flux at TOA - ', exper), y="rsutcs (W m-2)") +
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
net_rad <- dplyr::left_join(rlut, rsut, by = c("model", "region"))
net_rad <- dplyr::mutate(net_rad, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, region, value, sd))

net_rad_cs <- dplyr::left_join(rlutcs, rsutcs, by = c("model", "region"))
net_rad_cs <- dplyr::mutate(net_rad_cs, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, region, value, sd))


net_rad_plot <- ggplot(net_rad, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('net radiative flux \n at TOA - ', exper), y="rlut + rsut (W m-2)") +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank()) +
  scale_y_continuous(limits = c(-max(abs(net_rad$value))-max(abs(net_rad$sd)), max(abs(net_rad$value))+max(abs(net_rad$sd)))) +
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)


net_rad_cs_plot <- ggplot(net_rad_cs, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('clear-sky net radiative flux \n at TOA - ', exper), y="rlutcs + rsutcs (W m-2)") +
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
drybc_plot <- ggplot(drybc, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('dry deposition rate \n of BC - ', exper), y="drybc (kg m-2 s-1)") +
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
wetbc_plot <- ggplot(wetbc, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('wet deposition rate \n of BC - ', exper), y="wetbc (kg m-2 s-1)") +
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
tot_bc <- dplyr::left_join(drybc, wetbc, by = c("model", "region"))
tot_bc <- dplyr::mutate(tot_bc, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, region, value, sd))

tot_bc_plot <- ggplot(tot_bc, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('total deposition rate \n of BC - ', exper), y="drybc + wetbc (kg m-2 s-1)") +
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
dryso2_plot <- ggplot(dryso2, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('dry deposition rate \n of so2 - ', exper), y="dryso2 (kg m-2 s-1)") +
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
wetso2_plot <- ggplot(wetso2, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('wet deposition rate \n of so2 - ', exper), y="wetso2 (kg m-2 s-1)") +
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
dryso4_plot <- ggplot(dryso4, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('dry deposition rate \n of so4 - ', exper), y="dryso4 (kg m-2 s-1)") +
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
wetso4_plot <- ggplot(wetso4, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('wet deposition rate \n of so4 - ', exper), y="wetso4 (kg m-2 s-1)") +
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

tot_s_plot <- ggplot(tot_s, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('total deposition rate \n of S - ', exper), y="(dryso2 + wetso2)/2 + (dryso4 + wetso4)/3 (kg m-2 s-1)") +
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
od550aer_plot <- ggplot(od550aer, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('ambient aerosol optical \n thickness at 550nm - ', exper), y="od550aer") +
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
clt_plot <- ggplot(clt, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('total cloud cover \n percentage - ', exper), y="clt (%)") +
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
cltc_plot <- ggplot(cltc, aes(x = region, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('convective cloud cover \n percentage - ', exper), y="cltc (%)") +
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
setwd(paste0('../../../../output/', exper, '/summary'))

pdf(paste0(exper, '_summary_plots_diff.pdf'), height = 11, width = 8.5, paper = "letter")
grid.draw(emissions_plot)
grid.newpage()
grid.draw(forcing_plot)
grid.newpage()
grid.draw(deposition_plot)
dev.off()
