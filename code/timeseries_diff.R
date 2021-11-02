# ------------------------------------------------------------------------------
# Program Name: timeseries_diff.R
# Authors: Hamza Ahsan
# Date Last Modified: October 31, 2021
# Program Purpose: Produces time series line plots of the difference between 
# the perturbations and the reference case
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
cbPalette <- c("#0072B2", "#D55E00")

model_colors <- c('CESM1' = cbPalette[1], 'GISS' = cbPalette[2])
model_symbols <- c("CESM1" = 15, "GISS" = 17)

# ------------------------------------------------------------------------------

# Iterate over the different perturbation experiments
perts <- c('shp-10p-red', 'shp-10p-red-1950', 'shp-20p-red', 'shp-20p-red-1950', 
               'shp-80p-red', 'shp-atl-shift', 'shp-atl-shift-1950', 'shp-ind-shift',
               'shp-ind-shift-1950')

for(pert in perts){
  # Specify location of difference data
  setwd(paste0(emi_dir, '/input/', region, '/', pert, '/diff'))
  
  # Read in csv files and bind into single data frame
  target_filename <- list.files(getwd(), "*.csv")
  experiment <- bind_rows(map(target_filename, read.csv))
  
  # Extract model from file names (fifth segment) and bind to experiment data frame
  models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
  rep_models <- rep(models, each = 5) # five years
  experiment$model <- rep_models
  
  # Correct model names
    experiment$model[which(experiment$model == "CESM")] <- "CESM1"

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
  emibc_plot <- ggplot(emibc_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('surface flux of BC - ', region), y="emibc (kg m-2 s-1)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = scales::scientific_format(digits = 2)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  emiso2_plot <- ggplot(emiso2_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('surface flux of SO2 - ', region), y="emiso2 (kg m-2 s-1)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = scales::scientific_format(digits = 2)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  mmrbc_plot <- ggplot(mmrbc_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('surface concentration \n of BC - ', region), y="mmrbc (kg kg-1)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  mmrso4_plot <- ggplot(mmrso4_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('surface concentration \n of SO4 - ', region), y="mmrso4 (kg kg-1)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  rlut_plot <- ggplot(rlut_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('upwelling longwave flux \n at TOA - ', region), y="rlut (W m-2)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  rlutcs_plot <- ggplot(rlutcs_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('upwelling clear-sky longwave \n flux at TOA - ', region), y="rlutcs (W m-2)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  rsut_plot <- ggplot(rsut_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('upwelling shortwave flux \n at TOA - ', region), y="rsut (W m-2)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  rsutcs_plot <- ggplot(rsutcs_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('upwelling clear-sky shortwave \n flux at TOA - ', region), y="rsutcs (W m-2)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  rsdt_plot <- ggplot(rsdt_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('incident shortwave flux \n at TOA - ', region), y="rsdt (W m-2)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  net_rad_plot <- ggplot(net_rad, aes(x = year, y = value, color = model)) +
    labs(title=paste0('net radiative flux at TOA - ', region), y="rlut + rsut (W m-2)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  net_rad_cs_plot <- ggplot(net_rad_cs, aes(x = year, y = value, color = model)) +
    labs(title=paste0('clear-sky net radiative \n flux at TOA - ', region), y="rlutcs + rsutcs (W m-2)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  so2_plot <- ggplot(so2_experiment, aes(x = year, y = new_value, color = model)) +
    labs(title=paste0('surface concentration \n of SO2 - ', region), y="so2 (kg kg-1)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  drybc_plot <- ggplot(drybc_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('dry deposition rate \n of BC - ', region), y="drybc (kg m-2 s-1)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = scales::scientific_format(digits = 2)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  wetbc_plot <- ggplot(wetbc_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('wet deposition rate \n of BC - ', region), y="wetbc (kg m-2 s-1)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = scales::scientific_format(digits = 2)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  tot_bc_plot <- ggplot(tot_bc, aes(x = year, y = value, color = model)) +
    labs(title=paste0('total deposition rate \n of BC - ', region), y="drybc + wetbc (kg m-2 s-1)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = scales::scientific_format(digits = 2)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  dryso2_plot <- ggplot(dryso2_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('dry deposition rate \n of SO2 - ', region), y="dryso2 (kg m-2 s-1)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = scales::scientific_format(digits = 2)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  wetso2_plot <- ggplot(wetso2_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('wet deposition rate \n of SO2 - ', region), y="wetso2 (kg m-2 s-1)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = scales::scientific_format(digits = 2)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  dryso4_plot <- ggplot(dryso4_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('dry deposition rate \n of SO4 - ', region), y="dryso4 (kg m-2 s-1)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = scales::scientific_format(digits = 2)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  wetso4_plot <- ggplot(wetso4_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('wet deposition rate \n of SO4 - ', region), y="wetso4 (kg m-2 s-1)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = scales::scientific_format(digits = 2)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  tot_s_plot <- ggplot(tot_s, aes(x = year, y = value, color = model)) +
    labs(title=paste0('total deposition rate \n of S - ', region), y="(dryso2 + wetso2)/2 + (dryso4 + wetso4)/3 (kg m-2 s-1)", x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = scales::scientific_format(digits = 2)) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  # Function from stack exchange to generate a shared legend
  grid_arrange_shared_legend <- function(...) {
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom", 
                                       legend.title = element_blank(),
                                       legend.text = element_text(size = 7,
                                                                  margin = margin(r = 10, unit = "pt"))))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    grid.arrange(
      do.call(arrangeGrob, lapply(plots, function(x)
        x + theme(legend.position="none"))),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - 1.5 * lheight, lheight), # the "1.5" adds room for title
      top = textGrob(paste0(pert, ': absolute difference'), gp = gpar(fontsize = 12)))
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
                                           drybc_plot,
                                           wetbc_plot,
                                           tot_bc_plot,
                                           dryso2_plot,
                                           wetso2_plot,
                                           dryso4_plot,
                                           wetso4_plot,
                                           tot_s_plot)
  
  # Print plots
  setwd(paste0('../../../../output/', region, '/timeseries'))
  
  # To save to file on A4 paper
  ggsave(paste0(region, '_', pert ,'_diff.pdf'), final_plot, width = 21, height = 29.7, units = "cm")
}
