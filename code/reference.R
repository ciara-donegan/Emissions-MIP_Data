# ------------------------------------------------------------------------------
# Program Name: reference.R
# Authors: Hamza Ahsan
# Date Last Modified: September 21, 2023
# Program Purpose: Produces time series line plots of the reference case 
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
region <- "global"

# Define colorblind-friendly palette colors and associate with models (in case a  
# plot is missing a model, the color scheme will remain consistent)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#920000", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#490092", "#117733")

model_colors <- c('CESM' = cbPalette[1], 'E3SM' = cbPalette[2], 'GISS modelE' = cbPalette[3],
                  'CESM2' = cbPalette[4], 'MIROC-SPRINTARS' = cbPalette[5], 'NorESM2' = cbPalette[6],
                  'GFDL-ESM4' = cbPalette[7], 'OsloCTM3' = cbPalette[8], 'UKESM1' = cbPalette[9],
                  'GEOS' = cbPalette[10], 'CAM-ATRAS' = cbPalette[11])

# Setup directory for difference data
setwd(paste0(emi_dir, '/input/', region, '/reference'))

# ------------------------------------------------------------------------------

# Read in csv files and bind into single data frame. Remove 'unit' column to
# avoid error when binding different classes (i.e., factor and integer).
target_filename <- list.files(getwd(), "*.csv")

# Omit 1950 reference and duplicate reference files
target_filename <- target_filename[!grepl("AerChemMIP_reference|AerChemMIP_BW1950|CMIP_nudge-ref-1950|CMIP_nudge-ref", target_filename)]

experiment <- rbind(map(target_filename, read.csv))
experiment <- lapply(experiment, function(x) {x["unit"] <- NULL; x})
experiment <- bind_rows(experiment)

# Extract model from file names (fourth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[4])
rep_models <- rep(models, each = 5) # five years
experiment$model <- rep_models

# Correct model names
experiment$model[which(experiment$model == "CMIP6_AerChemMIP")] <- "GISS modelE"
experiment$model[which(experiment$model == "CMIP6_CMIP_CESM")] <- "CESM"
experiment$model[which(experiment$model == "CMIP6_CMIP_E3SM")] <- "E3SM"
experiment$model[which(experiment$model == "CMIP6_CMIP_CESM2")] <- "CESM2"
experiment$model[which(experiment$model == "CAM5")] <- "CAM-ATRAS"
experiment$model[which(experiment$model == "UKESM")] <- "UKESM1"
experiment$model[which(experiment$model == "MIROC")] <- "MIROC-SPRINTARS"
experiment$model[which(experiment$model == "GFDL")] <- "GFDL-ESM4"

# Convert DMS from volume mixing ratio to mass mixing ratio
experiment <- within(experiment, value <- ifelse(variable == "dms", 62.13 / 28.96, 1) * value)  %>%
  
  # Invert sign of forcing variables to be consistent with convention (i.e. positive
  # value denotes a heating effect)
  within(value <- ifelse(variable %in% c("rlut", "rsut", "rlutcs", "rsutcs"), -1, 1) * value)  %>%
  
  # Invert sign of CESM2 wet deposition variables (i.e., CESM2 wetbc, wetso2, wetso4)
  within(value <- ifelse(variable %in% c("wetbc", "wetso2", "wetso4") & model == "CESM2", -1, 1) * value) %>%
  
  # Convert from NH4HSO4 to SO4 mass
  within(value <- ifelse(variable %in% c("dryso4", "loadso4", "mmrso4", "wetso4") & model == "E3SM", 96/115, 1) * value) %>%
  within(value <- ifelse(variable %in% c("dryso4", "loadso4", "mmrso4", "wetso4") & model == "CESM", 96/115, 1) * value) %>%
  within(value <- ifelse(variable %in% c("dryso4", "loadso4", "mmrso4", "wetso4") & model == "MIROC-SPRINTARS", 96/115, 1) * value) %>%
  
  # Convert from H2SO4 to SO4 mass
  within(value <- ifelse(variable %in% c("dryso4", "loadso4", "mmrso4", "wetso4") & model == "NorESM2", 96/98, 1) * value)

# Rearrange data frame by years descending
experiment <- dplyr::arrange(experiment, year)

# Convert volume mixing ratio to mass mixing ratio by multiplying by molar mass
# of SO2 and dividing by molar mass of air
so2_experiment <- dplyr::filter(experiment, variable == 'so2') %>%
  dplyr::mutate(new_value = value * 64.066 / 28.96)

# Change units from mol/mol to kg/kg
so2_experiment$unit <- 'kg kg-1'

# Define remaining experiments
emibc_experiment    <- dplyr::filter(experiment, variable == 'emibc')
emiso2_experiment   <- dplyr::filter(experiment, variable == 'emiso2')
mmrbc_experiment    <- dplyr::filter(experiment, variable == 'mmrbc')
mmrso4_experiment   <- dplyr::filter(experiment, variable == 'mmrso4')
rlut_experiment     <- dplyr::filter(experiment, variable == 'rlut')
rlutcs_experiment   <- dplyr::filter(experiment, variable == 'rlutcs')
rsdt_experiment     <- dplyr::filter(experiment, variable == 'rsdt')
rsut_experiment     <- dplyr::filter(experiment, variable == 'rsut')
rsutcs_experiment   <- dplyr::filter(experiment, variable == 'rsutcs')
drybc_experiment    <- dplyr::filter(experiment, variable == 'drybc')
wetbc_experiment    <- dplyr::filter(experiment, variable == 'wetbc')
dryso2_experiment   <- dplyr::filter(experiment, variable == 'dryso2')
wetso2_experiment   <- dplyr::filter(experiment, variable == 'wetso2')
dryso4_experiment   <- dplyr::filter(experiment, variable == 'dryso4')
wetso4_experiment   <- dplyr::filter(experiment, variable == 'wetso4')
od550aer_experiment <- dplyr::filter(experiment, variable == 'od550aer')
clt_experiment      <- dplyr::filter(experiment, variable == 'clt')
cltc_experiment     <- dplyr::filter(experiment, variable == 'cltc')

# Define normal and clear-sky net radiative flux and  (sum of longwave and shortwave radiation)
net_rad <- dplyr::left_join(rlut_experiment, rsut_experiment, by = c("year", "model"))
net_rad <- dplyr::mutate(net_rad, value = value.x + value.y) %>%
  dplyr::select(c(year, model, value))

net_rad_cs <- dplyr::left_join(rlutcs_experiment, rsutcs_experiment, by = c("year", "model"))
net_rad_cs <- dplyr::mutate(net_rad_cs, value = value.x + value.y) %>%
  dplyr::select(c(year, model, value))

# Define implied cloud response (net - clearsky) as a new variable to plot
imp_cld <- dplyr::left_join(net_rad, net_rad_cs, by = c("year", "model"))
imp_cld <- dplyr::mutate(imp_cld, value = value.x - value.y) %>%
  dplyr::select(c(year, model, value))

# Define total BC deposition rate (sum of dry BC and wet BC deposition)
tot_bc <- dplyr::left_join(drybc_experiment, wetbc_experiment, by = c("year", "model"))
tot_bc <- dplyr::mutate(tot_bc, value = value.x + value.y) %>%
  dplyr::select(c(year, model, value))

#Define total S deposition rate (sum of dry SO2/SO4 and wet SO2/SO4 deposition)
dry_s <- dplyr::left_join(dryso2_experiment, dryso4_experiment, by = c("year", "model"))
dry_s <- dplyr::mutate(dry_s, value = (32.065/64.066)*value.x + (32.065/96.06)*value.y) %>%
  dplyr::select(c(year, model, value))

wet_s <- dplyr::left_join(wetso2_experiment, wetso4_experiment, by = c("year", "model"))
wet_s <- dplyr::mutate(wet_s, value = (32.065/64.066)*value.x + (32.065/96.06)*value.y) %>%
  dplyr::select(c(year, model, value))

tot_s <- dplyr::left_join(dry_s, wet_s, by = c("year", "model"))
tot_s <- dplyr::mutate(tot_s, value = value.x + value.y) %>%
  dplyr::select(c(year, model, value))

# Pre-define plot font sizes
title_font <- 7
axis_font <- 6
axis_title_font <- 7

# Generate plots
emibc_plot <- ggplot(emibc_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('surface flux of BC - \n', region), y=expression(emibc~(kg~m^-2~s^-1)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

emiso2_plot <- ggplot(emiso2_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('surface flux of SO2 - \n', region), y=expression(emiso2~(kg~m^-2~s^-1)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

mmrbc_plot <- ggplot(mmrbc_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('surface concentration \n of BC - ', region), y=expression(mmrbc~(kg~kg^-1)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

mmrso4_plot <- ggplot(mmrso4_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('surface concentration \n of SO4 - ', region), y=expression(mmrso4~(kg~kg^-1)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

rlut_plot <- ggplot(rlut_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('longwave flux at TOA - \n', region), y=expression(rlut~(W~m^-2)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

rlutcs_plot <- ggplot(rlutcs_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('clear-sky longwave \n flux at TOA - ', region), y=expression(rlutcs~(W~m^-2)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

rsut_plot <- ggplot(rsut_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('shortwave flux at TOA - \n', region), y=expression(rsut~(W~m^-2)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

rsutcs_plot <- ggplot(rsutcs_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('clear-sky shortwave \n flux at TOA - ', region), y=expression(rsutcs~(W~m^-2)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

rsdt_plot <- ggplot(rsdt_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('incident shortwave flux \n at TOA - ', region), y=expression(rsdt~(W~m^-2)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

net_rad_plot <- ggplot(net_rad, aes(x = year, y = value, color = model)) +
  labs(title=paste0('net radiative flux at \n TOA -', region), y=expression(rlut~+~rsut~(W~m^-2)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

net_rad_cs_plot <- ggplot(net_rad_cs, aes(x = year, y = value, color = model)) +
  labs(title=paste0('clear-sky net radiative \n flux at TOA - ', region), y=expression(rlutcs~+~rsutcs~(W~m^-2)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

imp_cld_plot <- ggplot(imp_cld, aes(x = year, y = value, color = model)) +
  labs(title=paste0('implied cloud response \n at TOA - ', region), y=expression(rlut~+~rsut~-~rlutcs~-~rsutcs~(W~m^-2)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

so2_plot <- ggplot(so2_experiment, aes(x = year, y = new_value, color = model)) +
  labs(title=paste0('surface concentration \n of SO2 - ', region), y=expression(so2~(kg~kg^-1)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

drybc_plot <- ggplot(drybc_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('dry deposition rate \n of BC - ', region), y=expression(drybc~(kg~m^-2~s^-1)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

wetbc_plot <- ggplot(wetbc_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('wet deposition rate \n of BC - ', region), y=expression(wetbc~(kg~m^-2~s^-1)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

tot_bc_plot <- ggplot(tot_bc, aes(x = year, y = value, color = model)) +
  labs(title=paste0('total deposition rate \n of BC - ', region), y=expression(drybc~+~wetbc~(kg~m^-2~s^-1)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

dryso2_plot <- ggplot(dryso2_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('dry deposition rate \n of SO2 - ', region), y=expression(dryso2~(kg~m^-2~s^-1)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

wetso2_plot <- ggplot(wetso2_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('wet deposition rate \n of SO2 - ', region), y=expression(wetso2~(kg~m^-2~s^-1)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

dryso4_plot <- ggplot(dryso4_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('dry deposition rate \n of SO4 - ', region), y=expression(dryso4~(kg~m^-2~s^-1)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

wetso4_plot <- ggplot(wetso4_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('wet deposition rate \n of SO4 - ', region), y=expression(wetso4~(kg~m^-2~s^-1)), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

tot_s_plot <- ggplot(tot_s, aes(x = year, y = value, color = model)) +
  labs(title=paste0('total deposition rate \n of S - ', region), y=expression(atop((dryso2~+~wetso2)/2~+~(dryso4~+~wetso4)/3, (kg~m^-2~s^-1))), x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

od550aer_plot <- ggplot(od550aer_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('ambient aerosol optical \n thickness at 550nm - ', region), y="od550aer", x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

clt_plot <- ggplot(clt_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('total cloud cover \n percentage - ', region), y="clt (%)", x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

cltc_plot <- ggplot(cltc_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('convective cloud cover \n percentage - ', region), y="cltc (%)", x="Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  geom_line()

# Function from stack exchange to generate a shared legend
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = "bottom", 
                                     legend.title = element_blank(),
                                     legend.text = element_text(size = 7,
                                                                margin = margin(r = 10, unit = "pt"))))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position = "none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - 1.5 * lheight, lheight), # the "1.5" adds room for title
    top = textGrob(paste0('Reference - absolute ', region, ' averages'), gp = gpar(fontsize = 12)))
}

final_plot <- grid_arrange_shared_legend(emibc_plot, 
                                         emiso2_plot, 
                                         mmrbc_plot, 
                                         mmrso4_plot, 
                                         so2_plot, 
                                         rlut_plot, 
                                         rsut_plot, 
                                         net_rad_plot, 
                                         rsdt_plot, 
                                         rlutcs_plot, 
                                         rsutcs_plot, 
                                         net_rad_cs_plot,
                                         imp_cld_plot,
                                         drybc_plot,
                                         wetbc_plot,
                                         tot_bc_plot,
                                         dryso2_plot,
                                         wetso2_plot,
                                         dryso4_plot,
                                         wetso4_plot,
                                         tot_s_plot,
                                         od550aer_plot,
                                         clt_plot,
                                         cltc_plot)

# Print plots
setwd(paste0('../../../output/', region, '/timeseries'))

# To save to file on A4 paper
ggsave(paste0(region, '_reference.pdf'), final_plot, width = 21, height = 29.7, units = "cm")
