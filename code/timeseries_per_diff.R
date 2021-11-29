# ------------------------------------------------------------------------------
# Program Name: timeseries_per_diff.R
# Authors: Hamza Ahsan
# Date Last Modified: November 5, 2021
# Program Purpose: Produces time series line plots of the percent difference
# between the perturbations and the reference case
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

# Specify region (i.e., global, land, sea, arctic, NH-land, NH-sea, SH-land, SH-sea)
region <- commandArgs(trailingOnly = TRUE) #pulling region from command line
region <- region[1] #replaces regions with the first trailing string in the command line

<<<<<<< HEAD
# Specify region (i.e., global, land, sea, arctic, NH-land, NH-sea, SH-land,
# SH-sea, NH-atlantic, NH-pacific)
region <- "NH-pacific"
=======
# Specify location of Emissions-MIP directory
emi_dir <- paste0('C:/Users/ahsa361/Documents/Emissions-MIP_Data')
>>>>>>> main

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

# ------------------------------------------------------------------------------
# Reads in csv file specifying which models to exclude from the data
excluded_models <- read.csv(file = paste0(emi_dir, '/input', '/excluded_data.csv'), fileEncoding="UTF-8-BOM")
excluded_models %>% drop_na() #gets rid of any empty spaces

#-------------------------------------------------------------------------------

# Iterate over the different perturbation experiments
perts <- c('bc-no-season', 'high-so4', 'no-so4', 'so2-at-height', 'so2-no-season')

for(pert in perts){
  # Specify location of difference data
  setwd(paste0(emi_dir, '/input/', region, '/', pert, '/per-diff'))

  # Read in csv files and bind into single data frame
  target_filename <- list.files(getwd(), "*.csv")
  experiment <- rbind(map(target_filename, read.csv))
  experiment <- lapply(experiment, function(x) {x["unit"] <- NULL; x})
  experiment <- bind_rows(experiment)

  # Extract model from file names (fifth segment) and bind to experiment data frame
  models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
  rep_models <- rep(models, each = 4) # four years
  experiment$model <- rep_models

  # Correct model names
  experiment$model[which(experiment$model == "CESM")] <- "CESM1"
<<<<<<< HEAD

  # Change any negative value to positive (i.e., CESM2 wetbc, wetso2, wetso4)
  # Invert sign of CESM2 wet deposition variables
  experiment$value[which(experiment$model == "CESM2" & experiment$variable == "wetbc")] <-
    -1 * experiment$value[which(experiment$model == "CESM2" & experiment$variable == "wetbc")]
  experiment$value[which(experiment$model == "CESM2" & experiment$variable == "wetso2")] <-
    -1 * experiment$value[which(experiment$model == "CESM2" & experiment$variable == "wetso2")]
  experiment$value[which(experiment$model == "CESM2" & experiment$variable == "wetso4")] <-
    -1 * experiment$value[which(experiment$model == "CESM2" & experiment$variable == "wetso4")]

=======
  
>>>>>>> main
  # Rearrange data frame by years descending
  experiment <- dplyr::arrange(experiment, year)

  #runs through each excluded model pair and filters them out of summary_long
  if(nrow(excluded_models) != 0) { #only runs if the data frame is not empty
    for (val in 1:nrow(excluded_models)) {
      experiment <- filter(experiment, pert != excluded_models$Scenario[val] | experiment$model != excluded_models$Model[val])
    }
  }

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
  so2_experiment      <- dplyr::filter(experiment, variable == 'so2')
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

  #find the min and max percentage differences
  axes_max <- max(mmrbc_experiment$value, mmrso4_experiment$value, so2_experiment$new_value,emibc_experiment$value,emiso2_experiment$value,drybc_experiment$value,wetbc_experiment$value,drybc_experiment$value,dryso2_experiment$value,wetso2_experiment$value,dryso4_experiment$value,wetso4_experiment$value,tot_bc$value,tot_s$value,rlut_experiment$value,rsut_experiment$value,net_rad$value,rsdt_experiment$value,rlutcs_experiment$value,rsutcs_experiment$value,net_rad_cs$value,od550aer_experiment$value,clt_experiment$value,cltc_experiment$value, na.rm = TRUE) 
  axes_min <- min(mmrbc_experiment$value, mmrso4_experiment$value, so2_experiment$new_value,emibc_experiment$value,emiso2_experiment$value,drybc_experiment$value,wetbc_experiment$value,drybc_experiment$value,dryso2_experiment$value,wetso2_experiment$value,dryso4_experiment$value,wetso4_experiment$value,tot_bc$value,tot_s$value,rlut_experiment$value,rsut_experiment$value,net_rad$value,rsdt_experiment$value,rlutcs_experiment$value,rsutcs_experiment$value,net_rad_cs$value,od550aer_experiment$value,clt_experiment$value,cltc_experiment$value, na.rm = TRUE)
  
  # Pre-define plot font sizes
  title_font <- 7
  axis_font <- 6
  axis_title_font <- 7

  # Generate plots
  emibc_plot <- ggplot(emibc_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('surface flux \n of BC - ', region), y=expression(Delta*~emibc), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  emiso2_plot <- ggplot(emiso2_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('surface flux \n of SO2 - ', region), y=expression(Delta*~emiso2), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  mmrbc_plot <- ggplot(mmrbc_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('surface concentration \n of BC - ', region), y=expression(Delta*~mmrbc), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  mmrso4_plot <- ggplot(mmrso4_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('surface concentration \n of SO4 - ', region), y=expression(Delta*~mmrso4), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  rlut_plot <- ggplot(rlut_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('longwave flux at TOA - \n', region), y=expression(Delta*~rlut), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  rlutcs_plot <- ggplot(rlutcs_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('clear-sky longwave flux \n at TOA - ', region), y=expression(Delta*~rlutcs), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  rsut_plot <- ggplot(rsut_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('shortwave flux at TOA - \n', region), y=expression(Delta*~rsut), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  rsutcs_plot <- ggplot(rsutcs_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('clear-sky shortwaveflux \n at TOA - ', region), y=expression(Delta*~rsutcs), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  rsdt_plot <- ggplot(rsdt_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('incident shortwave flux \n at TOA - ', region), y=expression(Delta*~rsdt), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  net_rad_plot <- ggplot(net_rad, aes(x = year, y = value, color = model)) +
    labs(title=paste0('net radiative flux \n at TOA - ', region), y=expression(Delta*~(rlut~+~rsut)), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  net_rad_cs_plot <- ggplot(net_rad_cs, aes(x = year, y = value, color = model)) +
    labs(title=paste0('clear-sky net radiative \n flux at TOA - ', region), y=expression(Delta*~(rlutcs~+~rsutcs)), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line()
  
  imp_cld_plot <- ggplot(imp_cld, aes(x = year, y = value, color = model)) +
    labs(title=paste0('implied cloud response \n at TOA - ', region), y=expression(Delta*~(rlut~+~rsut~-~rlutcs~-~rsutcs)), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  so2_plot <- ggplot(so2_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('surface concentration \n of SO2 - ', region), y=expression(Delta*~so2), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  drybc_plot <- ggplot(drybc_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('dry deposition rate \n of BC - ', region), y=expression(Delta*~drybc), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  wetbc_plot <- ggplot(wetbc_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('wet deposition rate \n of BC - ', region), y=expression(Delta*~wetbc), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  tot_bc_plot <- ggplot(tot_bc, aes(x = year, y = value, color = model)) +
    labs(title=paste0('total deposition rate \n of BC - ', region), y=expression(Delta*~(drybc~+~wetbc)), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  dryso2_plot <- ggplot(dryso2_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('dry deposition rate \n of SO2 - ', region), y=expression(Delta*~dryso2), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  wetso2_plot <- ggplot(wetso2_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('wet deposition rate \n of SO2 - ', region), y=expression(Delta*~wetso2), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  dryso4_plot <- ggplot(dryso4_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('dry deposition rate \n of SO4 - ', region), y=expression(Delta*~dryso4), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  wetso4_plot <- ggplot(wetso4_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('wet deposition rate \n of SO4 - ', region), y=expression(Delta*~wetso4), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  tot_s_plot <- ggplot(tot_s, aes(x = year, y = value, color = model)) +
    labs(title=paste0('total deposition rate \n of S - ', region), y=expression(Delta*~(dryso2~+~wetso2)/2~+~(dryso4~+~wetso4)/3), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  od550aer_plot <- ggplot(od550aer_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('ambient aerosol optical \n thickness at 550nm - ', region), y=expression(Delta*~od550aer), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)

  clt_plot <- ggplot(clt_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('total cloud cover \n percentage - ', region), y=expression(Delta*~clt), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


  cltc_plot <- ggplot(cltc_experiment, aes(x = year, y = value, color = model)) +
    labs(title=paste0('convective cloud cover \n percentage - ', region), y=expression(Delta*~cltc), x="Year") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_colour_manual(values = model_colors) +
    geom_line() +
    ylim(axes_min,axes_max)


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
      top = textGrob(paste0(pert, ': percent difference'), gp = gpar(fontsize = 12)))
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
  setwd(paste0('../../../../output/', region, '/timeseries'))

  # To save to file on A4 paper
  ggsave(paste0(region, '_', pert ,'_per_diff.pdf'), final_plot, width = 21, height = 29.7, units = "cm")
}
