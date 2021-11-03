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
emi_dir <- paste0('C:/Users/such559/Documents/Emissions-MIP_Data')

#choose to sort data by region or experiment
sort_by <- "region"

# Specify region (i.e., global, land, sea, arctic, NH-land, NH-sea, SH-land, 
# SH-sea, NH-atlantic, NH-pacific)
region <- "NH-pacific"

# Specify experiment (i.e., bc-no-season, high-so4, no-so4, reference, so2-at-height, so2-no-season)
exper <- c("bc-no-season")


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
  
  model_symbols <- c(CESM1 = 15, E3SM = 15, GISS = 17, CESM2 = 19, MIROC = 15, 
                     NorESM2 = 17, GFDL = 19, OsloCTM3 = 19, UKESM = 15, GEOS = 17)
  
  # ------------------------------------------------------------------------------
  #reads in csv file specifying which models to exclude from the data
  excluded_models <- read.csv(file = paste0(emi_dir, '/input', '/excluded_data.csv'), fileEncoding="UTF-8-BOM")
  excluded_models %>% drop_na() #gets rid of any empty spaces
  #-----------------------------------------------------------------------------
  #extracts data for each perturbation experiment from csv files
  data_accumulation <- function(emi_dir, reg_name, exper){
    
    setwd(paste0(emi_dir,'/input/', reg_name,'/', exper, '/diff'))
    
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
  
  #-----------------------------------------------------------------------------
  if (sort_by == "region"){
  
  bc_no_seas_summary <- data_accumulation(emi_dir,region,"bc-no-season")
  high_so4_summary <- data_accumulation(emi_dir,region,"high-so4")
  no_so4_summary <- data_accumulation(emi_dir,region,"no-so4")
  so2_at_hgt_summary <- data_accumulation(emi_dir,region,"so2-at-height")
  so2_no_seas_summary <- data_accumulation(emi_dir,region,"so2-no-season")
  
  bc_no_seas_summary <- rename(bc_no_seas_summary, bc_no_seas = regional_data)
  high_so4_summary <- rename(high_so4_summary, high_so4 = regional_data)
  no_so4_summary <- rename(no_so4_summary, no_so4 = regional_data)
  so2_at_hgt_summary <- rename(so2_at_hgt_summary, so2_at_hgt = regional_data)
  so2_no_seas_summary <- rename(so2_no_seas_summary, so2_no_seas = regional_data)
  
  bc_no_seas_summary <- rename(bc_no_seas_summary, bc_no_seas_sd = regional_data_sd)
  high_so4_summary <- rename(high_so4_summary, high_so4_sd = regional_data_sd)
  no_so4_summary <- rename(no_so4_summary, no_so4_sd = regional_data_sd)
  so2_at_hgt_summary <- rename(so2_at_hgt_summary, so2_at_hgt_sd = regional_data_sd)
  so2_no_seas_summary <- rename(so2_no_seas_summary, so2_no_seas_sd = regional_data_sd)
  
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
  
  }
  
  if (sort_by == "experiment"){
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
  }

# Generate plots
title_font <- 9.5
axis_font <- 9
axis_title_font <- 9

#creates a function that filters species out of a database
filter_species <- function(database, species){
  species <- dplyr::filter(database, variable == species)
  return(species)
}

#filters each species from summary_long
emibc <- filter_species(summary_long, "emibc")
emiso2 <- filter_species(summary_long, "emiso2")
mmrbc <- filter_species(summary_long, "mmrbc")
mmrso4 <- filter_species(summary_long, "mmrso4")
so2 <- filter_species(summary_long, "so2")
rlut <- filter_species(summary_long, "rlut")
rsut <- filter_species(summary_long, "rsut")
rsdt <- filter_species(summary_long, "rsdt")
rlutcs <- filter_species(summary_long, "rlutcs")
rsutcs <- filter_species(summary_long, "rsutcs")
drybc <- filter_species(summary_long, "drybc")
wetbc <- filter_species(summary_long, "wetbc")
dryso2 <- filter_species(summary_long, "dryso2")
wetso2 <- filter_species(summary_long, "wetso2")
dryso4 <- filter_species(summary_long, "dryso4")
wetso4 <- filter_species(summary_long, "wetso4")
od550aer <- filter_species(summary_long, "od550aer")
clt <- filter_species(summary_long, "clt")
cltc <- filter_species(summary_long, "cltc")

#Creates a function that creates plots for the data based on each species
if (sort_by == "region"){
plot_species <- function(variable, x, y, title, units, region_or_exper){
  species <- variable
  species_plot <- ggplot(species, aes(x = experiment, y = value, color = model))+
    theme_bw()+
    labs(title=paste0(title,' - ', region_or_exper), y=units) +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank()) +
  scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(species$value))-max(abs(species$sd)), max(abs(species$value))+max(abs(species$sd))))+
  scale_colour_manual(values = model_colors) +
  geom_point( position=position_dodge(width=0.4), size = 1.5) +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)
 
return(species_plot)  
}
}
if (sort_by == "experiment"){
  plot_species <- function(variable, x, y, title, units, region_or_exper){
    species <- variable
    species_plot <- ggplot(species, aes(x = region, y = value, color = model))+
      theme_bw()+
      labs(title=paste0(title,' - ', region_or_exper), y=units) +
      theme(plot.title = element_text(hjust = 0.5, size = title_font),
            axis.text = element_text(size = axis_font),
            axis.title = element_text(size = axis_title_font),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title.x = element_blank()) +
      scale_y_continuous(labels = scales::scientific_format(digits = 2), limits = c(-max(abs(species$value))-max(abs(species$sd)), max(abs(species$value))+max(abs(species$sd))))+
      scale_colour_manual(values = model_colors) +
      geom_point( position=position_dodge(width=0.4), size = 1.5) +
      geom_errorbar(aes(ymin=value-sd, ymax=value+sd), width=0.2, position=position_dodge(0.4), show.legend = F)
    
    return(species_plot)  
  }
}

#creates plots based on each species using the plot_species function
emibc_plot <- plot_species(emibc, region, value, 'surface flux of BC', 'emibc (kg m-2 s-1)', exper)
emiso2_plot <- plot_species(emiso2, region, value, 'surface flux of SO2', 'emiso2 (kg m-2 s-1)', exper)
mmrbc_plot <- plot_species(mmrbc, region, value, 'surface concentration of BC', 'mmrbc (kg kg-1)', exper)
mmrso4_plot <- plot_species(mmrso4, region, value, 'surface concentration of SO4', 'mmrso4 (kg kg-1)', exper)
so2_plot <- plot_species(so2, region, value, 'surface concentration of SO2', 'so2 (kg kg-1)', exper)
rlut_plot <- plot_species(rlut, region, value, 'upwelling longwave flux \n at TOA', 'rlut (W m-2)', exper)
rsut_plot <- plot_species(rsut, region, value, 'upwelling shortwave flux \n at TOA', 'rsut (W m-2)', exper)
rsdt_plot <- plot_species(rsdt, region, value, 'incident shortwave flux \n at TOA', 'rsdt (W m-2)', exper)
rlutcs_plot <- plot_species(rlutcs, region, value, 'upwelling clear-sky longwave \n flux at TOA', 'rlutcs (W m-2)', exper)
rsutcs_plot <- plot_species(rsutcs, region, value, 'upwelling clear-sky shortwave \n flux at TOA', 'rsutcs (W m-2)', exper)
drybc_plot <- plot_species(drybc, region, value, 'dry deposition rate \n of BC', 'drybc (kg m-2 s-1)', exper)
wetbc_plot <- plot_species(wetbc, region, value, 'wet deposition rate \n of BC', 'wetbc (kg m-2 s-1)', exper)
dryso2_plot <- plot_species(dryso2, region, value, 'dry deposition rate \n of so2', 'dryso2 (kg m-2 s-1)', exper)
wetso2_plot <- plot_species(wetso2, region, value, 'wet deposition rate \n of so2', 'wetso2 (kg m-2 s-1)', exper)
dryso4_plot <- plot_species(dryso4, region, value, 'dry deposition rate \n of so4', 'dryso4 (kg m-2 s-1)', exper) 
wetso4_plot <- plot_species(wetso4, region, value, 'wet deposition rate \n of so4', 'wetso4 (kg m-2 s-1)', exper) 
od550aer_plot <-  plot_species(od550aer, region, value, 'ambient aerosol optical \n thickness at 550nm', 'od550aer', exper) 
clt_plot <- plot_species(clt, region, value, 'total cloud cover \n percentage', 'clt (%)', exper) 
cltc_plot <- plot_species(cltc, region, value, 'convective cloud cover \n percentage', 'cltc (%)', exper) 

# Define normal and clear-sky net radiative flux (sum of longwave and shortwave radiation)
if (sort_by == "region"){
  net_rad <- dplyr::left_join(rlut, rsut, by = c("model", "experiment"))
  net_rad <- dplyr::mutate(net_rad, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, experiment, value, sd))
  
  net_rad_cs <- dplyr::left_join(rlutcs, rsutcs, by = c("model", "experiment"))
  net_rad_cs <- dplyr::mutate(net_rad_cs, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, experiment, value, sd))
}

if (sort_by == "experiment"){
  net_rad <- dplyr::left_join(rlut, rsut, by = c("model", "region"))
  net_rad <- dplyr::mutate(net_rad, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, region, value, sd))
  
  net_rad_cs <- dplyr::left_join(rlutcs, rsutcs, by = c("model", "region"))
  net_rad_cs <- dplyr::mutate(net_rad_cs, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, region, value, sd))
}

#plots normal and clear sky net radiative flux using the plot_species function
net_rad_plot <- plot_species(net_rad, region, value, 'net radiative flux \n at TOA', 'rlut + rsut (W m-2)', exper)
net_rad_cs_plot <- plot_species(net_rad_cs, region, value, 'clear-sky net radiative flux \n at TOA', 'rlutcs + rsutcs (W m-2)', exper)

# Define total BC deposition rate (sum of dry and wet BC )
if (sort_by == "region"){
  tot_bc <- dplyr::left_join(drybc, wetbc, by = c("model", "experiment"))
  tot_bc <- dplyr::mutate(tot_bc, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, experiment, value, sd))
}

if (sort_by == "experiment"){
  tot_bc <- dplyr::left_join(drybc, wetbc, by = c("model", "region"))
  tot_bc <- dplyr::mutate(tot_bc, value = value.x + value.y) %>%
    dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
    dplyr::select(c(model, region, value, sd))
}

tot_bc_plot <- plot_species(tot_bc, region, value, 'total deposition rate \n of BC', 'drybc + wetbc (kg m-2 s-1)', exper)


# Define total S deposition rate (sum of dry and wet SO2/SO4 )
if (sort_by == "region"){
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
}

if (sort_by == "experiment"){
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
}

tot_s_plot <- plot_species(tot_s, region, value, 'total deposition rate \n of S', '(dryso2 + wetso2)/2 + (dryso4 + wetso4)/3 (kg m-2 s-1)', exper)

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
if (sort_by == 'region'){
setwd(paste0('../../../../output/', region, '/summary'))

pdf(paste0(region, '_summary_plots_diff.pdf'), height = 11, width = 8.5, paper = "letter")
}

if (sort_by == 'experiment'){
  setwd(paste0('../../../../output/', exper, '/summary'))
  
  pdf(paste0(exper, '_summary_plots_diff.pdf'), height = 11, width = 8.5, paper = "letter")
}

grid.draw(emissions_plot)
grid.newpage()
grid.draw(forcing_plot)
grid.newpage()
grid.draw(deposition_plot)
dev.off()
