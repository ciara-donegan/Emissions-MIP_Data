# ------------------------------------------------------------------------------
# Program Name: reference.R
# Authors: Hamza Ahsan
<<<<<<< HEAD
# Date Last Modified: Sept 2, 2021
=======
# Date Last Modified: November 2, 2021
>>>>>>> main
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
<<<<<<< HEAD
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
model_colors <- c('CESM1' = cols[1], 'CESM1-1950' = cols[1], 'GISS' = cols[2],
                  'GISS-1950' = cols[2])

# Define line types
model_lines <- c('CESM1' = "solid", 'CESM1-1950' = "dashed", 'GISS' = "solid",
                 'GISS-1950' = "dashed")
=======
emi_dir <- paste0('C:/Users/ahsa361/Documents/Emissions-MIP_Data')

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
>>>>>>> main

# Setup directory for difference data
setwd(paste0(emi_dir, '/input/', region, '/reference'))

# ------------------------------------------------------------------------------

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
experiment <- bind_rows(map(target_filename, read.csv))

# Reformat file name strings to correctly parst model name
target_filename <- stringr::str_replace_all(target_filename, "nudge-ref", "nudge_ref")
target_filename <- stringr::str_replace_all(target_filename, "ref-1950", "ref_1950")

# Extract model from file names (fourth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[4])
rep_models <- rep(models, each = 5) # five years
experiment$model <- rep_models

<<<<<<< HEAD
# Correct model names for CESM and CESM2
experiment$model[which(experiment$model == "AerChemMIP_BW1950")] <- "GISS-1950"
experiment$model[which(experiment$model == "AerChemMIP_reference")] <- "GISS"
experiment$model[which(experiment$model == "CMIP_nudge_ref")] <- "CESM1"
experiment$model[which(experiment$model == "CMIP_nudge_ref_1950")] <- "CESM1-1950"
=======
# Correct model names
experiment$model[which(experiment$model == "CMIP6_AerChemMIP")] <- "GISS"
experiment$model[which(experiment$model == "CMIP6_CMIP_CESM")] <- "CESM1"
experiment$model[which(experiment$model == "CMIP6_CMIP_E3SM")] <- "E3SM"
experiment$model[which(experiment$model == "CMIP6_CMIP_CESM2")] <- "CESM2"

# Invert sign of forcing variables to be consistent with convention (i.e. positive
# value denotes a heating effect)
experiment <- within(experiment, value <- ifelse(variable %in% c("rlut", "rsut", "rlutcs", "rsutcs"), -1, 1) * value)

# Invert sign of CESM2 wet deposition variables (i.e., CESM2 wetbc, wetso2, wetso4)
experiment <- within(experiment, value <- ifelse(variable %in% c("wetbc", "wetso2", "wetso4") & model == "CESM2", -1, 1) * value)
>>>>>>> main

# Rearrange data frame by years descending
experiment <- dplyr::arrange(experiment, year)

# Convert volume mixing ratio to mass mixing ratio by multiplying by molar mass
# of SO2 and dividing by molar mass of air
so2_experiment <- dplyr::filter(experiment, variable == 'so2') %>%
  dplyr::mutate(new_value = value * 64.066 / 28.96)

# Change units from mol/mol to kg/kg
so2_experiment$unit <- 'kg kg-1'

# Define other experiments
emibc_experiment <- dplyr::filter(experiment, variable == 'emibc')
emiso2_experiment <- dplyr::filter(experiment, variable == 'emiso2')
mmrbc_experiment <- dplyr::filter(experiment, variable == 'mmrbc')
mmrso4_experiment <- dplyr::filter(experiment, variable == 'mmrso4')
rlut_experiment <- dplyr::filter(experiment, variable == 'rlut')
rlutcs_experiment <- dplyr::filter(experiment, variable == 'rlutcs')
rsdt_experiment <- dplyr::filter(experiment, variable == 'rsdt')
rsut_experiment <- dplyr::filter(experiment, variable == 'rsut')
rsutcs_experiment <- dplyr::filter(experiment, variable == 'rsutcs')
drybc_experiment <- dplyr::filter(experiment, variable == 'drybc')
wetbc_experiment <- dplyr::filter(experiment, variable == 'wetbc')
dryso2_experiment <- dplyr::filter(experiment, variable == 'dryso2')
wetso2_experiment <- dplyr::filter(experiment, variable == 'wetso2')
dryso4_experiment <- dplyr::filter(experiment, variable == 'dryso4')
wetso4_experiment <- dplyr::filter(experiment, variable == 'wetso4')

# Define normal and clear-sky net radiative flux and  (sum of longwave and shortwave radiation)
net_rad <- dplyr::left_join(rlut_experiment, rsut_experiment, by = c("year", "unit", "model"))
net_rad <- dplyr::mutate(net_rad, value = value.x + value.y) %>%
  dplyr::select(c(year, unit, model, value))

net_rad_cs <- dplyr::left_join(rlutcs_experiment, rsutcs_experiment, by = c("year", "unit", "model"))
net_rad_cs <- dplyr::mutate(net_rad_cs, value = value.x + value.y) %>%
  dplyr::select(c(year, unit, model, value))

# Define implied cloud response (net - clearsky) as a new variable to plot
imp_cld <- dplyr::left_join(net_rad, net_rad_cs, by = c("year", "model"))
imp_cld <- dplyr::mutate(imp_cld, value = value.x - value.y) %>%
  dplyr::select(c(year, model, value))

# Define total BC deposition rate (sum of dry BC and wet BC deposition)
tot_bc <- dplyr::left_join(drybc_experiment, wetbc_experiment, by = c("year", "unit", "model"))
tot_bc <- dplyr::mutate(tot_bc, value = value.x + value.y) %>%
  dplyr::select(c(year, unit, model, value))

#Define total S deposition rate (sum of dry SO2/SO4 and wet SO2/SO4 deposition)
dry_s <- dplyr::left_join(dryso2_experiment, dryso4_experiment, by = c("year", "unit", "model"))
dry_s <- dplyr::mutate(dry_s, value = (32.065/64.066)*value.x + (32.065/96.06)*value.y) %>%
  dplyr::select(c(year, unit, model, value))

wet_s <- dplyr::left_join(wetso2_experiment, wetso4_experiment, by = c("year", "unit", "model"))
wet_s <- dplyr::mutate(wet_s, value = (32.065/64.066)*value.x + (32.065/96.06)*value.y) %>%
  dplyr::select(c(year, unit, model, value))

tot_s <- dplyr::left_join(dry_s, wet_s, by = c("year", "unit", "model"))
tot_s <- dplyr::mutate(tot_s, value = value.x + value.y) %>%
  dplyr::select(c(year, unit, model, value))


title_font <- 7
axis_font <- 6
axis_title_font <- 7

# Generate plots
<<<<<<< HEAD
emibc_plot <- ggplot() +
  geom_line(data = dplyr::filter(emibc_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(emibc_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('surface flux of BC - ', region), y="emibc (kg m-2 s-1)", x="Year") +
=======
emibc_plot <- ggplot(emibc_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('surface flux of BC - \n', region), y=expression(emibc~(kg~m^-2~s^-1)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD

emiso2_plot <- ggplot() +
  geom_line(data = dplyr::filter(emiso2_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(emiso2_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('surface flux of SO2 - ', region), y="emiso2 (kg m-2 s-1)", x="Year") +
=======
emiso2_plot <- ggplot(emiso2_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('surface flux of SO2 - \n', region), y=expression(emiso2~(kg~m^-2~s^-1)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
mmrbc_plot <- ggplot() +
  geom_line(data = dplyr::filter(mmrbc_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(mmrbc_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('surface concentration \n of BC - ', region), y="mmrbc (kg kg-1)", x="Year") +
=======
mmrbc_plot <- ggplot(mmrbc_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('surface concentration \n of BC - ', region), y=expression(mmrbc~(kg~kg^-1)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
mmrso4_plot <- ggplot() +
  geom_line(data = dplyr::filter(mmrso4_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(mmrso4_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('surface concentration \n of SO4 - ', region), y="mmrso4 (kg kg-1)", x="Year") +
=======
mmrso4_plot <- ggplot(mmrso4_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('surface concentration \n of SO4 - ', region), y=expression(mmrso4~(kg~kg^-1)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
rlut_plot <- ggplot() +
  geom_line(data = dplyr::filter(rlut_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(rlut_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('upwelling longwave flux \n at TOA - ', region), y="rlut (W m-2)", x="Year") +
=======
rlut_plot <- ggplot(rlut_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('longwave flux at TOA - \n', region), y=expression(rlut~(W~m^-2)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
rlutcs_plot <- ggplot() +
  geom_line(data = dplyr::filter(rlutcs_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(rlutcs_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('upwelling clear-sky longwave \n flux at TOA - ', region), y="rlutcs (W m-2)", x="Year") +
=======
rlutcs_plot <- ggplot(rlutcs_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('clear-sky longwave \n flux at TOA - ', region), y=expression(rlutcs~(W~m^-2)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
rsut_plot <- ggplot() +
  geom_line(data = dplyr::filter(rsut_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(rsut_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('upwelling shortwave flux \n at TOA - ', region), y="rsut (W m-2)", x="Year") +
=======
rsut_plot <- ggplot(rsut_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('shortwave flux at TOA - \n', region), y=expression(rsut~(W~m^-2)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
rsutcs_plot <- ggplot() +
  geom_line(data = dplyr::filter(rsutcs_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(rsutcs_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('upwelling clear-sky shortwave \n flux at TOA - ', region), y="rsutcs (W m-2)", x="Year") +
=======
rsutcs_plot <- ggplot(rsutcs_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('clear-sky shortwave \n flux at TOA - ', region), y=expression(rsutcs~(W~m^-2)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
rsdt_plot <- ggplot() +
  geom_line(data = dplyr::filter(rsdt_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(rsdt_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('incident shortwave flux \n at TOA - ', region), y="rsdt (W m-2)", x="Year") +
=======
rsdt_plot <- ggplot(rsdt_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('incident shortwave flux \n at TOA - ', region), y=expression(rsdt~(W~m^-2)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
net_rad_plot <- ggplot() +
  geom_line(data = dplyr::filter(net_rad, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(net_rad, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('net radiative flux at TOA - ', region), y="rlut + rsut (W m-2)", x="Year") +
=======
net_rad_plot <- ggplot(net_rad, aes(x = year, y = value, color = model)) +
  labs(title=paste0('net radiative flux at \n TOA -', region), y=expression(rlut~+~rsut~(W~m^-2)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
net_rad_cs_plot <- ggplot() +
  geom_line(data = dplyr::filter(net_rad_cs, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(net_rad_cs, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('clear-sky net radiative \n flux at TOA - ', region), y="rlutcs + rsutcs (W m-2)", x="Year") +
=======
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
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
so2_plot <- ggplot() +
  geom_line(data = dplyr::filter(so2_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(so2_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('surface concentration \n of SO2 - ', region), y="so2 (kg kg-1)", x="Year") +
=======
so2_plot <- ggplot(so2_experiment, aes(x = year, y = new_value, color = model)) +
  labs(title=paste0('surface concentration \n of SO2 - ', region), y=expression(so2~(kg~kg^-1)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
drybc_plot <- ggplot() +
  geom_line(data = dplyr::filter(drybc_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(drybc_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('dry deposition rate \n of BC - ', region), y="drybc (kg m-2 s-1)", x="Year") +
=======
drybc_plot <- ggplot(drybc_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('dry deposition rate \n of BC - ', region), y=expression(drybc~(kg~m^-2~s^-1)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
wetbc_plot <- ggplot() +
  geom_line(data = dplyr::filter(wetbc_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(wetbc_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('wet deposition rate \n of BC - ', region), y="wetbc (kg m-2 s-1)", x="Year") +
=======
wetbc_plot <- ggplot(wetbc_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('wet deposition rate \n of BC - ', region), y=expression(wetbc~(kg~m^-2~s^-1)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
tot_bc_plot <- ggplot() +
  geom_line(data = dplyr::filter(tot_bc, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(tot_bc, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('total deposition rate \n of BC - ', region), y="drybc + wetbc (kg m-2 s-1)", x="Year") +
=======
tot_bc_plot <- ggplot(tot_bc, aes(x = year, y = value, color = model)) +
  labs(title=paste0('total deposition rate \n of BC - ', region), y=expression(drybc~+~wetbc~(kg~m^-2~s^-1)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
dryso2_plot <- ggplot() +
  geom_line(data = dplyr::filter(dryso2_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(dryso2_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('dry deposition rate \n of SO2 - ', region), y="dryso2 (kg m-2 s-1)", x="Year") +
=======
dryso2_plot <- ggplot(dryso2_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('dry deposition rate \n of SO2 - ', region), y=expression(dryso2~(kg~m^-2~s^-1)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
wetso2_plot <- ggplot() +
  geom_line(data = dplyr::filter(wetso2_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(wetso2_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('wet deposition rate \n of SO2 - ', region), y="wetso2 (kg m-2 s-1)", x="Year") +
=======
wetso2_plot <- ggplot(wetso2_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('wet deposition rate \n of SO2 - ', region), y=expression(wetso2~(kg~m^-2~s^-1)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
dryso4_plot <- ggplot() +
  geom_line(data = dplyr::filter(dryso4_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(dryso4_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('dry deposition rate \n of SO4 - ', region), y="dryso4 (kg m-2 s-1)", x="Year") +
=======
dryso4_plot <- ggplot(dryso4_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('dry deposition rate \n of SO4 - ', region), y=expression(dryso4~(kg~m^-2~s^-1)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
wetso4_plot <- ggplot() +
  geom_line(data = dplyr::filter(wetso4_experiment, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(wetso4_experiment, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('wet deposition rate \n of SO4 - ', region), y="wetso4 (kg m-2 s-1)", x="Year") +
=======
wetso4_plot <- ggplot(wetso4_experiment, aes(x = year, y = value, color = model)) +
  labs(title=paste0('wet deposition rate \n of SO4 - ', region), y=expression(wetso4~(kg~m^-2~s^-1)), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

<<<<<<< HEAD
tot_s_plot <- ggplot() +
  geom_line(data = dplyr::filter(tot_s, model %in% c("GISS", "GISS-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  geom_line(data = dplyr::filter(tot_s, model %in% c("CESM1", "CESM1-1950")), aes(x = year, y = value, color = model, linetype= model)) +
  labs(title=paste0('total deposition rate \n of S - ', region), y="(dryso2 + wetso2)/2 + (dryso4 + wetso4)/3 (kg m-2 s-1)", x="Year") +
=======
tot_s_plot <- ggplot(tot_s, aes(x = year, y = value, color = model)) +
  labs(title=paste0('total deposition rate \n of S - ', region), y=expression(atop((dryso2~+~wetso2)/2~+~(dryso4~+~wetso4)/3, (kg~m^-2~s^-1))), x="Year") +
>>>>>>> main
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = title_font),
        axis.text = element_text(size = axis_font),
        axis.title = element_text(size = axis_title_font)) +
  scale_colour_manual(values = model_colors) +
  scale_linetype_manual(values = model_lines)

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
                                         tot_s_plot)

# Print plots
setwd(paste0('../../../output/', region, '/timeseries'))

# To save to file on A4 paper
ggsave(paste0(region, '_reference.pdf'), final_plot, width = 21, height = 29.7, units = "cm")
