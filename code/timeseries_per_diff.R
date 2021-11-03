# ------------------------------------------------------------------------------
# Program Name: timeseries_diff.R
# Authors: Hamza Ahsan
# Date Last Modified: September 29, 2021
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
emi_dir <- paste0('C:/Users/ahsa361/OneDrive - PNNL/Desktop/Emissions-MIP')

#specifies whether to sort the data by region or by experiment
sort_by <- "experiment"

# Specify region if sorting by experiment (i.e., global, land, sea, arctic, NH-land, NH-sea, SH-land, 
# SH-sea, NH-atlantic, NH-pacific)
region <- "NH-pacific"

#Specify perturbation if sorting by region (i.e. bc-no-season, high-so4, no-so4, reference, so2-at-height, so2-no-season)
pert <- "bc-no-season"
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
# Iterate over the different perturbation/regional experiments
if (sort_by == "region"){scenarios <- c('bc-no-season', 'high-so4', 'no-so4', 'so2-at-height', 'so2-no-season')}
if (sort_by == "experiment"){scenarios <- c("arctic", "global", "land", "NH-atlantic", "NH-land", "NH-pacific", "NH-sea", "sea", "SH-land", "SH-sea")}
#-------------------------------------------------------------------------------
#reads in csv file specifying which models to exclude from the data
excluded_models <- read.csv(file = paste0(emi_dir, '/input', '/excluded_data.csv'), fileEncoding="UTF-8-BOM")
excluded_models %>% drop_na() #gets rid of any empty spaces
#-------------------------------------------------------------------------------

for(scenario in scenarios){
  # Specify location of difference data
  if (sort_by == "region"){
    setwd(paste0(emi_dir, '/input/', region, '/', scenario, '/per-diff'))
  }
  if (sort_by == "experiment"){
    setwd(paste0(emi_dir, '/input/', scenario, '/', pert, '/per-diff'))
  }
  
  # Read in csv files and bind into single data frame
  target_filename <- list.files(getwd(), "*.csv")
  experiment <- rbind(map(target_filename, read.csv))
  experiment <- lapply(experiment, function(x) {x["unit"] <- NULL; x})
  experiment <- bind_rows(experiment)

  # Extract model from file names (fifth segment) and bind to experiment data frame
  models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
  rep_models <- rep(models, each = 4) # four years
  experiment$model <- rep_models

  # Correct model names for CESM and CESM2
  experiment$model[which(experiment$model == "CESM")] <- "CESM1"
  
  # Change any negative value to positive (i.e., CESM2 wetbc, wetso2, wetso4)
  # Invert sign of CESM2 wet deposition variables
  experiment$value[which(experiment$model == "CESM2" & experiment$variable == "wetbc")] <- 
    -1 * experiment$value[which(experiment$model == "CESM2" & experiment$variable == "wetbc")]
  experiment$value[which(experiment$model == "CESM2" & experiment$variable == "wetso2")] <- 
    -1 * experiment$value[which(experiment$model == "CESM2" & experiment$variable == "wetso2")]
  experiment$value[which(experiment$model == "CESM2" & experiment$variable == "wetso4")] <- 
    -1 * experiment$value[which(experiment$model == "CESM2" & experiment$variable == "wetso4")]
  
  # Rearrange data frame by years descending
  experiment <- dplyr::arrange(experiment, year)
  
  # Convert volume mixing ratio to mass mixing ratio by multiplying by molar mass
  # of SO2 and dividing by molar mass of air
  so2_experiment <- dplyr::filter(experiment, variable == 'so2') %>%
    dplyr::mutate(new_value = value * 64.066 / 28.96)
  
  # Change units from mol/mol to kg/kg
  so2_experiment$unit <- 'kg kg-1'
  
    #only runs excluded models if sorting by region
    if (sort_by == "region"){
      #runs through each excluded model pair and filters them out of experiment
      if(nrow(excluded_models) != 0) { #only runs if the data frame is not empty
        for (val in 1:nrow(excluded_models)) {
          experiment <- filter(experiment, pert != excluded_models$Scenario[val] | experiment$model != excluded_models$Model[val])
        }
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
  
  plot_species <- function(experiment, reg_or_exper,title, units){
    species_plot <- ggplot(experiment, aes(x = year, y = value, color = model)) +
      labs(title=paste0(title, ' - ', reg_or_exper), y=units, x="Year") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = title_font),
            axis.text = element_text(size = axis_font),
            axis.title = element_text(size = axis_title_font)) +
      scale_y_continuous(labels = scales::scientific_format(digits = 2)) +
      scale_colour_manual(values = model_colors) +
      geom_line()
    
    return(species_plot)
  }
  if (sort_by == "region"){
    #Generate plots
    emibc_plot <- plot_species(emibc_experiment, region, "surface flux \n of BC", "emibc (kg m-2 s-1)")
    emiso2_plot <- plot_species(emiso2_experiment, region, "surface flux \n of SO2","emibc (kg m-2 s-1)")
    mmrbc_plot <- plot_species(mmrbc_experiment, region, "surface concentration \n of BC","mmrbc (kg kg-1)")
    mmrso4_plot <- plot_species(mmrso4_experiment, region, "surface concentration \n of SO4","mmrso4 (kg kg-1)")
    rlut_plot <- plot_species(rlut_experiment, region, "upwelling longwave flux \n at TOA","rlut (W m-2)")
    rlutcs_plot <- plot_species(rlutcs_experiment, region, "upwelling clear-sky longwave \n flux at TOA","rlutcs (W m-2)")
    rsut_plot <- plot_species(rsut_experiment, region, "upwelling shortwave flux \n at TOA","rsut (W m-2)")
    rsutcs_plot <- plot_species(rsutcs_experiment, region, "upwelling clear-sky shortwave \n flux at TOA","rsutcs (W m-2)")
    rsdt_plot <- plot_species(rsdt_experiment, region, "incident shortwave flux \n at TOA","rsdt (W m-2)")
    net_rad_plot <- plot_species(net_rad, region, "net radiative flux \n at TOA","rlut + rsut (W m-2)")
    net_rad_cs_plot <- plot_species(net_rad_cs, region, "clear-sky net radiative \n flux at TOA","rlutcs + rsutcs (W m-2)")
    so2_plot <- plot_species(so2_experiment, region, "surface concentration \n of SO2","so2 (kg kg-1)")
    drybc_plot <- plot_species(drybc_experiment, region, "dry deposition rate \n of BC","drybc (kg m-2 s-1)")
    wetbc_plot <- plot_species(wetbc_experiment, region, "wet deposition rate \n of BC","wetbc (kg m-2 s-1)")
    tot_bc_plot <- plot_species(tot_bc, region, "total deposition rate \n of BC", "drybc + wetbc (kg m-2 s-1)")
    dryso2_plot <- plot_species(dryso2_experiment, region, "dry deposition rate \n of SO2", "dryso2 (kg m-2 s-1)")
    wetso2_plot <- plot_species(wetso2_experiment, region, "wet deposition rate \n of SO2", "wetso2 (kg m-2 s-1)")
    dryso4_plot <- plot_species(dryso4_experiment, region, "dry deposition rate \n of SO4", "dryso4 (kg m-2 s-1)")
    wetso4_plot <- plot_species(wetso4_experiment, region, "wet deposition rate \n of SO4", "wetso4 (kg m-2 s-1)")
    tot_s_plot <- plot_species(tot_s, region, "total deposition rate \n of S", "(dryso2 + wetso2)/2 + (dryso4 + wetso4)/3 (kg m-2 s-1)")
    od550aer_plot <- plot_species(od550aer_experiment, region, "ambient aerosol optical \n thickness at 550nm", "od550aer")
    clt_plot <- plot_species(clt_experiment, region, "total cloud cover \n percentage", "clt (%)")
    cltc_plot <- plot_species(cltc_experiment, region, "convective cloud cover \n percentage", "cltc (%)")
  }
  
  if (sort_by == "experiment"){
    #Generate plots
    emibc_plot <- plot_species(emibc_experiment, pert, "surface flux \n of BC", "emibc (kg m-2 s-1)")
    emiso2_plot <- plot_species(emiso2_experiment, pert, "surface flux \n of SO2","emibc (kg m-2 s-1)")
    mmrbc_plot <- plot_species(mmrbc_experiment, pert, "surface concentration \n of BC","mmrbc (kg kg-1)")
    mmrso4_plot <- plot_species(mmrso4_experiment, pert, "surface concentration \n of SO4","mmrso4 (kg kg-1)")
    rlut_plot <- plot_species(rlut_experiment, pert, "upwelling longwave flux \n at TOA","rlut (W m-2)")
    rlutcs_plot <- plot_species(rlutcs_experiment, pert, "upwelling clear-sky longwave \n flux at TOA","rlutcs (W m-2)")
    rsut_plot <- plot_species(rsut_experiment, pert, "upwelling shortwave flux \n at TOA","rsut (W m-2)")
    rsutcs_plot <- plot_species(rsutcs_experiment, pert, "upwelling clear-sky shortwave \n flux at TOA","rsutcs (W m-2)")
    rsdt_plot <- plot_species(rsdt_experiment, pert, "incident shortwave flux \n at TOA","rsdt (W m-2)")
    net_rad_plot <- plot_species(net_rad, pert, "net radiative flux \n at TOA","rlut + rsut (W m-2)")
    net_rad_cs_plot <- plot_species(net_rad_cs, pert, "clear-sky net radiative \n flux at TOA","rlutcs + rsutcs (W m-2)")
    so2_plot <- plot_species(so2_experiment, pert, "surface concentration \n of SO2","so2 (kg kg-1)")
    drybc_plot <- plot_species(drybc_experiment, pert, "dry deposition rate \n of BC","drybc (kg m-2 s-1)")
    wetbc_plot <- plot_species(wetbc_experiment, pert, "wet deposition rate \n of BC","wetbc (kg m-2 s-1)")
    tot_bc_plot <- plot_species(tot_bc, pert, "total deposition rate \n of BC", "drybc + wetbc (kg m-2 s-1)")
    dryso2_plot <- plot_species(dryso2_experiment, pert, "dry deposition rate \n of SO2", "dryso2 (kg m-2 s-1)")
    wetso2_plot <- plot_species(wetso2_experiment, pert, "wet deposition rate \n of SO2", "wetso2 (kg m-2 s-1)")
    dryso4_plot <- plot_species(dryso4_experiment, pert, "dry deposition rate \n of SO4", "dryso4 (kg m-2 s-1)")
    wetso4_plot <- plot_species(wetso4_experiment, pert, "wet deposition rate \n of SO4", "wetso4 (kg m-2 s-1)")
    tot_s_plot <- plot_species(tot_s, pert, "total deposition rate \n of S", "(dryso2 + wetso2)/2 + (dryso4 + wetso4)/3 (kg m-2 s-1)")
    od550aer_plot <- plot_species(od550aer_experiment, pert, "ambient aerosol optical \n thickness at 550nm", "od550aer")
    clt_plot <- plot_species(clt_experiment, pert, "total cloud cover \n percentage", "clt (%)")
    cltc_plot <- plot_species(cltc_experiment, pert, "convective cloud cover \n percentage", "cltc (%)")
  }

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
                                           tot_s_plot,
                                           od550aer_plot,
                                           clt_plot,
                                           cltc_plot)

  # Print plots
  if (sort_by == "region"){ 
    setwd(paste0('../../../../output/', region, '/timeseries'))
    # To save to file on A4 paper
    ggsave(paste0(region, '_', scenario ,'_diff.pdf'), final_plot, width = 21, height = 29.7, units = "cm")
  }
  if (sort_by == "experiment"){
    setwd(paste0('../../../../output/', pert, '/timeseries'))
    # To save to file on A4 paper
    ggsave(paste0(scenario, '_', pert ,'_diff.pdf'), final_plot, width = 21, height = 29.7, units = "cm")
  }
}

