# ------------------------------------------------------------------------------
# Program Name: summary_plots_per_diff_model_layer.R
# Authors: Hamza Ahsan
# Date Last Modified: September 23, 2023
# Program Purpose: Produces summary plots of the percent difference between the
# so2-at-height and reference against model layer data
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

# Specify region (i.e., global, land, sea, arctic, NH-land, NH-sea, SH-land,
# SH-sea, NH-atlantic, NH-pacific)
region <- "global"

# Define colorblind-friendly palette colors and associate with models (in case a
# plot is missing a model, the color scheme will remain consistent)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#920000", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#490092", "#117733")

model_colors <- c('CESM' = cbPalette[1], 'E3SM' = cbPalette[2], 'GISS modelE' = cbPalette[3],
                  'CESM2' = cbPalette[4], 'MIROC-SPRINTARS' = cbPalette[5], 'NorESM2' = cbPalette[6],
                  'GFDL-ESM4' = cbPalette[7], 'OsloCTM3' = cbPalette[8], 'UKESM1' = cbPalette[9],
                  'GEOS' = cbPalette[10], 'CAM-ATRAS' = cbPalette[11])

model_symbols <- c('CESM' = 15, 'E3SM' = 15, 'GISS modelE' = 17, 'CESM2' = 19, 'MIROC-SPRINTARS' = 15,
                   'NorESM2' = 17, 'GFDL-ESM4' = 19, 'OsloCTM3' = 19, 'UKESM1' = 15, 'GEOS' = 17,
                   'CAM-ATRAS' = 17)

# ------------------------------------------------------------------------------

# Read in global SO2-at-height percent difference data
setwd(paste0(emi_dir,'/input/global/so2-at-height/per-diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
so2_at_hgt <- rbind(map(target_filename, read.csv))
so2_at_hgt <- lapply(so2_at_hgt, function(x) {x["unit"] <- NULL; x})
so2_at_hgt <- bind_rows(so2_at_hgt)

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # five years
so2_at_hgt$model <- rep_models

# Correct model names
so2_at_hgt$model[which(so2_at_hgt$model == "GISS")] <- "GISS modelE"
so2_at_hgt$model[which(so2_at_hgt$model == "CAM5")] <- "CAM-ATRAS"
so2_at_hgt$model[which(so2_at_hgt$model == "UKESM")] <- "UKESM1"
so2_at_hgt$model[which(so2_at_hgt$model == "MIROC")] <- "MIROC-SPRINTARS"
so2_at_hgt$model[which(so2_at_hgt$model == "GFDL")] <- "GFDL-ESM4"

# Take the average over all years for each variable and calculate std dev
so2_at_hgt_summary <- so2_at_hgt %>%
  dplyr::group_by(variable, model) %>%
  dplyr::summarise(so2_at_hgt = mean(value), so2_at_hgt_sd = sd(value)) %>%
  drop_na()

# Add missing variables for OsloCTM3 (zero delta, i.e., no difference from reference)
add_oslo <- data.frame(variable = c("rsdt", "rlut", "rlutcs", "clt", "cltc"), model = "OsloCTM3")

so2_at_hgt_summary <- bind_rows(so2_at_hgt_summary, add_oslo) %>%
  replace(is.na(.), 0)

# Model layer thickness data and number of layers below 400m
model <- c("CESM", "E3SM", "GISS modelE", "NorESM2", "GFDL-ESM4", "CESM2", "OsloCTM3", "GEOS", "UKESM1", "CAM-ATRAS", "MIROC-SPRINTARS")
first_layer <- c(124, 25, 170, 127, 35, 150, 17, 58, 20, 129, 21)
second_layer <- c(149, 54, 190, 152, 50, 150, 25, 131, 33, 154, 49)
third_layer <- c(173, 72, 220, 176, 75, 150, 36, 65, 47, 180, 71)
num_layer <- c(3, 6, 2, 2, 5, 2, 7, 4, 6, 2, 4)
model_hgt <- data.frame(model, first_layer, second_layer, third_layer, num_layer)

# Combine global bldep and model layer thickness with global SO2-at-height results
summary_data <- model_hgt %>%
  left_join(so2_at_hgt_summary, by = "model") %>%
  rename(value = so2_at_hgt) %>%
  rename(sd = so2_at_hgt_sd)

# Generate plots
title_font <- 9.5
axis_font <- 9
axis_title_font <- 9

# Function that filters species out of a database
filter_species <- function(database, species){
  species <- dplyr::filter(database, variable == species)
  return(species)
}

# Filters each species from summary_data
emibc <- filter_species(summary_data, "emibc")
emiso2 <- filter_species(summary_data, "emiso2")
mmrbc <- filter_species(summary_data, "mmrbc")
mmrso4 <- filter_species(summary_data, "mmrso4")
so2 <- filter_species(summary_data, "so2")
rlut <- filter_species(summary_data, "rlut")
rsut <- filter_species(summary_data, "rsut")
rsdt <- filter_species(summary_data, "rsdt")
rlutcs <- filter_species(summary_data, "rlutcs")
rsutcs <- filter_species(summary_data, "rsutcs")
drybc <- filter_species(summary_data, "drybc")
wetbc <- filter_species(summary_data, "wetbc")
dryso2 <- filter_species(summary_data, "dryso2")
wetso2 <- filter_species(summary_data, "wetso2")
dryso4 <- filter_species(summary_data, "dryso4")
wetso4 <- filter_species(summary_data, "wetso4")
od550aer <- filter_species(summary_data, "od550aer")
clt <- filter_species(summary_data, "clt")
cltc <- filter_species(summary_data, "cltc")
cl <- filter_species(summary_data, "cl")
clivi <- filter_species(summary_data, "clivi")
dms <- filter_species(summary_data, "dms")
loadbc <- filter_species(summary_data, "loadbc")
loadso2 <- filter_species(summary_data, "loadso2")
loadso4 <- filter_species(summary_data, "loadso4")

# Plotting function for first model layer
plot_species <- function(variable, y, title, units, region_or_exper, model_colors, model_symbols){
  species <- variable
  species_plot <- ggplot(species, aes(x = first_layer, y = value, color = model, shape = model))+
    theme_bw()+
    labs(title=paste0(title,' - ', region_or_exper), y=units, x="First Layer (m)") +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(species$value))-max(abs(species$sd)), max(abs(species$value))+max(abs(species$sd)))) +
    scale_colour_manual(values = model_colors) +
    scale_shape_manual(values = model_symbols) +
    geom_point( position=position_dodge(width=0.4), size = 1.5) +
    geom_errorbar(aes(ymin=value-sd, ymax=value+sd), size = 0.1, width = 0, position=position_dodge(0.4), show.legend = F)
  return(species_plot)
}

emibc_plot <- plot_species(emibc, value, 'surface flux of BC', expression(Delta*~emibc), region, model_colors, model_symbols)
emiso2_plot <- plot_species(emiso2, value, 'surface flux of SO2', expression(Delta*~emiso2), region, model_colors, model_symbols)
mmrbc_plot <- plot_species(mmrbc, value, 'surface concentration of BC', expression(Delta*~mmrbc), region, model_colors, model_symbols)
mmrso4_plot <- plot_species(mmrso4, value, 'surface concentration of SO4', expression(Delta*~mmrso4), region, model_colors, model_symbols)
so2_plot <- plot_species(so2, value, 'surface concentration of SO2', expression(Delta*~so2), region, model_colors, model_symbols)
rlut_plot <- plot_species(rlut, value, 'upwelling longwave flux \n at TOA', expression(Delta*~rlut), region, model_colors, model_symbols)
rsut_plot <- plot_species(rsut, value, 'upwelling shortwave flux \n at TOA', expression(Delta*~rsut), region, model_colors, model_symbols)
rsdt_plot <- plot_species(rsdt, value, 'incident shortwave flux \n at TOA', expression(Delta*~rsdt), region, model_colors, model_symbols)
rlutcs_plot <- plot_species(rlutcs, value, 'upwelling clear-sky longwave \n flux at TOA', expression(Delta*~rlutcs), region, model_colors, model_symbols)
rsutcs_plot <- plot_species(rsutcs, value, 'upwelling clear-sky shortwave \n flux at TOA', expression(Delta*~rsutcs), region, model_colors, model_symbols)
drybc_plot <- plot_species(drybc, value, 'dry deposition rate \n of BC', expression(Delta*~drybc), region, model_colors, model_symbols)
wetbc_plot <- plot_species(wetbc, value, 'wet deposition rate \n of BC', expression(Delta*~wetbc), region, model_colors, model_symbols)
dryso2_plot <- plot_species(dryso2, value, 'dry deposition rate \n of so2', expression(Delta*~dryso2), region, model_colors, model_symbols)
wetso2_plot <- plot_species(wetso2, value, 'wet deposition rate \n of so2', expression(Delta*~wetso2), region, model_colors, model_symbols)
dryso4_plot <- plot_species(dryso4, value, 'dry deposition rate \n of so4', expression(Delta*~dryso4), region, model_colors, model_symbols)
wetso4_plot <- plot_species(wetso4, value, 'wet deposition rate \n of so4', expression(Delta*~wetso4), region, model_colors, model_symbols)
od550aer_plot <- plot_species(od550aer, value, 'ambient aerosol optical \n thickness at 550nm', expression(Delta*~od550aer), region, model_colors, model_symbols)
clt_plot <- plot_species(clt, value, 'total cloud cover', expression(Delta*~clt), region, model_colors, model_symbols)
cltc_plot <- plot_species(cltc, value, 'convective cloud cover', expression(Delta*~cltc), region, model_colors, model_symbols)
cl_plot <- plot_species(cl, value, 'surface cloud cover', expression(Delta*~cl), region, model_colors, model_symbols)
clivi_plot <- plot_species(clivi, value, 'ice water path', 	expression(Delta*~clivi), region, model_colors, model_symbols)
dms_plot <- plot_species(dms, value, 'surface concentration of DMS', expression(Delta*~dms), region, model_colors, model_symbols)
loadbc_plot <- plot_species(loadbc, value, 'column mass burden of BC', expression(Delta*~loadbc), region, model_colors, model_symbols)
loadso2_plot <- plot_species(loadso2, value, 'column mass burden of SO2', expression(Delta*~loadso2), region, model_colors, model_symbols)
loadso4_plot <- plot_species(loadso4, value, 'column mass burden of SO4', expression(Delta*~loadso4), region, model_colors, model_symbols)


# Define normal and clear-sky net radiative flux (sum of longwave and shortwave radiation)
net_rad <- dplyr::left_join(rlut, rsut, by = c("model", "first_layer", "second_layer", "third_layer", "num_layer"))
net_rad <- dplyr::mutate(net_rad, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, value, sd, first_layer, second_layer, third_layer, num_layer))

net_rad_cs <- dplyr::left_join(rlutcs, rsutcs, by = c("model", "first_layer", "second_layer", "third_layer", "num_layer"))
net_rad_cs <- dplyr::mutate(net_rad_cs, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, value, sd, first_layer, second_layer, third_layer, num_layer))

#plots normal and clear sky net radiative flux using the plot_species function
net_rad_plot <- plot_species(net_rad, value, 'net radiative flux \n at TOA', expression(Delta*~rlut~+~rsut), region, model_colors, model_symbols)
net_rad_cs_plot <- plot_species(net_rad_cs, value, 'clear-sky net radiative flux \n at TOA', expression(Delta*~rlutcs~+~rsutcs), region, model_colors, model_symbols)

# Define total BC deposition rate (sum of dry and wet BC )
tot_bc <- dplyr::left_join(drybc, wetbc, by = c("model", "first_layer", "second_layer", "third_layer", "num_layer"))
tot_bc <- dplyr::mutate(tot_bc, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, value, sd, first_layer, second_layer, third_layer, num_layer))

tot_bc_plot <- plot_species(tot_bc, value, 'total deposition rate \n of BC', expression(Delta*~drybc~+~wetbc), region, model_colors, model_symbols)

# Define total S deposition rate (sum of dry and wet SO2/SO4 )
dry_s <- dplyr::left_join(dryso2, dryso4, by = c("model", "first_layer", "second_layer", "third_layer", "num_layer"))
dry_s <- dplyr::mutate(dry_s, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, value, sd, first_layer, second_layer, third_layer, num_layer))

wet_s <- dplyr::left_join(wetso2, wetso4, by = c("model", "first_layer", "second_layer", "third_layer", "num_layer"))
wet_s <- dplyr::mutate(wet_s, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, value, sd, first_layer, second_layer, third_layer, num_layer))

tot_s <- dplyr::left_join(dry_s, wet_s, by = c("model", "first_layer", "second_layer", "third_layer", "num_layer"))
tot_s <- dplyr::mutate(tot_s, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, value, sd, first_layer, second_layer, third_layer, num_layer))

tot_s_plot <- plot_species(tot_s, value, 'total deposition rate \n of S', expression(Delta*(~dryso2~+~wetso2)/2~+~(dryso4~+~wetso4)/3), region, model_colors, model_symbols)

# Define implied cloud response (net - clearsky) as a new variable to plot
imp_cld <- dplyr::left_join(net_rad, net_rad_cs, by = c("model", "first_layer", "second_layer", "third_layer", "num_layer"))
imp_cld <- dplyr::mutate(imp_cld, value = value.x - value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, value, sd, first_layer, second_layer, third_layer, num_layer))

imp_cld_plot <- plot_species(imp_cld, value, 'implied cloud response at \n TOA', expression(Delta*~rlut~+~rsut~-~rlutcs~-~rsutcs), region, model_colors, model_symbols)

# Define total sulfate deposition (dryso4 + wetso4) for sulfate lifetime
tot_so4 <- dplyr::left_join(dryso4, wetso4, by = c("model", "first_layer", "second_layer", "third_layer", "num_layer"))
tot_so4 <- dplyr::mutate(tot_so4, value = value.x + value.y) %>%
  dplyr::mutate(sd = sqrt(sd.x^2 + sd.y^2)) %>%
  dplyr::select(c(model, value, sd, first_layer, second_layer, third_layer, num_layer))

# Define sulfate lifetime (loadso4/(dryso4+wetso4))
so4_lifetime <- dplyr::left_join(loadso4, tot_so4, by = c("model", "first_layer", "second_layer", "third_layer", "num_layer"))
so4_lifetime <- dplyr::mutate(so4_lifetime, value = (value.x/value.y)) %>%
  dplyr::mutate(sd = value*sqrt((sd.x/value.x)^2 + (sd.y/value.y)^2)) %>%
  dplyr::filter_if(~is.numeric(.), all_vars(!is.infinite(.))) %>%
  dplyr::select(c(model, value, sd, first_layer, second_layer, third_layer, num_layer))

so4_lifetime_plot <- plot_species(so4_lifetime, value, 'SO4 lifetime', expression(Delta*~loadso4/(dryso4~+~wetso4)), region, model_colors, model_symbols)

# Define SO2 timescale (loadso2/emiso2)
so2_timescale <- dplyr::left_join(loadso2, emiso2, by = c("model", "first_layer", "second_layer", "third_layer", "num_layer"))
so2_timescale <- dplyr::mutate(so2_timescale, value = (value.x/value.y)) %>%
  dplyr::mutate(sd = value*sqrt((sd.x/value.x)^2 + (sd.y/value.y)^2)) %>%
  dplyr::filter_if(~is.numeric(.), all_vars(!is.infinite(.))) %>%
  dplyr::select(c(model, value, sd, first_layer, second_layer, third_layer, num_layer))

so2_timescale_plot <- plot_species(so2_timescale, value, 'SO2 timescale', expression(Delta*~loadso2/emiso2), region, model_colors, model_symbols)


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
    top = textGrob("1st Model Layer Summary: SO2-at-height (percent difference)", gp = gpar(fontsize = 12)))
}

emissions_plot <- grid_arrange_shared_legend(emibc_plot,
                                             emiso2_plot,
                                             mmrbc_plot,
                                             mmrso4_plot,
                                             so2_plot,
                                             dms_plot)

column_plot <- grid_arrange_shared_legend(loadso4_plot,
                                          loadso2_plot,
                                          loadbc_plot,
                                          so4_lifetime_plot,
                                          so2_timescale_plot)

forcing_plot <- grid_arrange_shared_legend(rsut_plot,
                                           rlut_plot,
                                           net_rad_plot,
                                           rsdt_plot,
                                           rlutcs_plot,
                                           rsutcs_plot,
                                           net_rad_cs_plot,
                                           imp_cld_plot)

cloud_plot <- grid_arrange_shared_legend(od550aer_plot,
                                         clt_plot,
                                         cltc_plot,
                                         cl_plot,
                                         clivi_plot)

deposition_plot <- grid_arrange_shared_legend(drybc_plot,
                                              wetbc_plot,
                                              tot_bc_plot,
                                              dryso2_plot,
                                              wetso2_plot,
                                              dryso4_plot,
                                              wetso4_plot,
                                              tot_s_plot)

# Print plots
setwd(paste0(emi_dir, '/output/model_layer'))

pdf(paste0(region, '_layer1_so2-at-hgt_per_diff.pdf'), height = 11, width = 8.5, paper = "letter")
grid.draw(emissions_plot)
grid.newpage()
grid.draw(column_plot)
grid.newpage()
grid.draw(forcing_plot)
grid.newpage()
grid.draw(cloud_plot)
grid.newpage()
grid.draw(deposition_plot)
dev.off()


#-------------------------Second Layer Thickness--------------------------------

# Plotting function for second model layer
plot_species <- function(variable, y, title, units, region_or_exper, model_colors, model_symbols){
  species <- variable
  species_plot <- ggplot(species, aes(x = second_layer, y = value, color = model, shape = model))+
    theme_bw()+
    labs(title=paste0(title,' - ', region_or_exper), y=units, x="Second Layer (m)") +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(species$value))-max(abs(species$sd)), max(abs(species$value))+max(abs(species$sd)))) +
    scale_colour_manual(values = model_colors) +
    scale_shape_manual(values = model_symbols) +
    geom_point( position=position_dodge(width=0.4), size = 1.5) +
    geom_errorbar(aes(ymin=value-sd, ymax=value+sd), size = 0.1, width = 0, position=position_dodge(0.4), show.legend = F)
  return(species_plot)
}

emibc_plot <- plot_species(emibc, value, 'surface flux of BC', expression(Delta*~emibc), region, model_colors, model_symbols)
emiso2_plot <- plot_species(emiso2, value, 'surface flux of SO2', expression(Delta*~emiso2), region, model_colors, model_symbols)
mmrbc_plot <- plot_species(mmrbc, value, 'surface concentration of BC', expression(Delta*~mmrbc), region, model_colors, model_symbols)
mmrso4_plot <- plot_species(mmrso4, value, 'surface concentration of SO4', expression(Delta*~mmrso4), region, model_colors, model_symbols)
so2_plot <- plot_species(so2, value, 'surface concentration of SO2', expression(Delta*~so2), region, model_colors, model_symbols)
rlut_plot <- plot_species(rlut, value, 'upwelling longwave flux \n at TOA', expression(Delta*~rlut), region, model_colors, model_symbols)
rsut_plot <- plot_species(rsut, value, 'upwelling shortwave flux \n at TOA', expression(Delta*~rsut), region, model_colors, model_symbols)
rsdt_plot <- plot_species(rsdt, value, 'incident shortwave flux \n at TOA', expression(Delta*~rsdt), region, model_colors, model_symbols)
rlutcs_plot <- plot_species(rlutcs, value, 'upwelling clear-sky longwave \n flux at TOA', expression(Delta*~rlutcs), region, model_colors, model_symbols)
rsutcs_plot <- plot_species(rsutcs, value, 'upwelling clear-sky shortwave \n flux at TOA', expression(Delta*~rsutcs), region, model_colors, model_symbols)
drybc_plot <- plot_species(drybc, value, 'dry deposition rate \n of BC', expression(Delta*~drybc), region, model_colors, model_symbols)
wetbc_plot <- plot_species(wetbc, value, 'wet deposition rate \n of BC', expression(Delta*~wetbc), region, model_colors, model_symbols)
dryso2_plot <- plot_species(dryso2, value, 'dry deposition rate \n of so2', expression(Delta*~dryso2), region, model_colors, model_symbols)
wetso2_plot <- plot_species(wetso2, value, 'wet deposition rate \n of so2', expression(Delta*~wetso2), region, model_colors, model_symbols)
dryso4_plot <- plot_species(dryso4, value, 'dry deposition rate \n of so4', expression(Delta*~dryso4), region, model_colors, model_symbols)
wetso4_plot <- plot_species(wetso4, value, 'wet deposition rate \n of so4', expression(Delta*~wetso4), region, model_colors, model_symbols)
od550aer_plot <- plot_species(od550aer, value, 'ambient aerosol optical \n thickness at 550nm', expression(Delta*~od550aer), region, model_colors, model_symbols)
clt_plot <- plot_species(clt, value, 'total cloud cover', expression(Delta*~clt), region, model_colors, model_symbols)
cltc_plot <- plot_species(cltc, value, 'convective cloud cover', expression(Delta*~cltc), region, model_colors, model_symbols)
cl_plot <- plot_species(cl, value, 'surface cloud cover', expression(Delta*~cl), region, model_colors, model_symbols)
clivi_plot <- plot_species(clivi, value, 'ice water path', 	expression(Delta*~clivi), region, model_colors, model_symbols)
dms_plot <- plot_species(dms, value, 'surface concentration of DMS', expression(Delta*~dms), region, model_colors, model_symbols)
loadbc_plot <- plot_species(loadbc, value, 'column mass burden of BC', expression(Delta*~loadbc), region, model_colors, model_symbols)
loadso2_plot <- plot_species(loadso2, value, 'column mass burden of SO2', expression(Delta*~loadso2), region, model_colors, model_symbols)
loadso4_plot <- plot_species(loadso4, value, 'column mass burden of SO4', expression(Delta*~loadso4), region, model_colors, model_symbols)
net_rad_plot <- plot_species(net_rad, value, 'net radiative flux \n at TOA', expression(Delta*~rlut~+~rsut), region, model_colors, model_symbols)
net_rad_cs_plot <- plot_species(net_rad_cs, value, 'clear-sky net radiative flux \n at TOA', expression(Delta*~rlutcs~+~rsutcs), region, model_colors, model_symbols)
tot_bc_plot <- plot_species(tot_bc, value, 'total deposition rate \n of BC', expression(Delta*~drybc~+~wetbc), region, model_colors, model_symbols)
tot_s_plot <- plot_species(tot_s, value, 'total deposition rate \n of S', expression(Delta*(~dryso2~+~wetso2)/2~+~(dryso4~+~wetso4)/3), region, model_colors, model_symbols)
imp_cld_plot <- plot_species(imp_cld, value, 'implied cloud response at \n TOA', expression(Delta*~rlut~+~rsut~-~rlutcs~-~rsutcs), region, model_colors, model_symbols)
so4_lifetime_plot <- plot_species(so4_lifetime, value, 'SO4 lifetime', expression(Delta*~loadso4/(dryso4~+~wetso4)), region, model_colors, model_symbols)
so2_timescale_plot <- plot_species(so2_timescale, value, 'SO2 timescale', expression(Delta*~loadso2/emiso2), region, model_colors, model_symbols)


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
    top = textGrob("2nd Model Layer Summary: SO2-at-height (percent difference)", gp = gpar(fontsize = 12)))
}

emissions_plot <- grid_arrange_shared_legend(emibc_plot,
                                             emiso2_plot,
                                             mmrbc_plot,
                                             mmrso4_plot,
                                             so2_plot,
                                             dms_plot)

column_plot <- grid_arrange_shared_legend(loadso4_plot,
                                          loadso2_plot,
                                          loadbc_plot,
                                          so4_lifetime_plot,
                                          so2_timescale_plot)

forcing_plot <- grid_arrange_shared_legend(rsut_plot,
                                           rlut_plot,
                                           net_rad_plot,
                                           rsdt_plot,
                                           rlutcs_plot,
                                           rsutcs_plot,
                                           net_rad_cs_plot,
                                           imp_cld_plot)

cloud_plot <- grid_arrange_shared_legend(od550aer_plot,
                                         clt_plot,
                                         cltc_plot,
                                         cl_plot,
                                         clivi_plot)

deposition_plot <- grid_arrange_shared_legend(drybc_plot,
                                              wetbc_plot,
                                              tot_bc_plot,
                                              dryso2_plot,
                                              wetso2_plot,
                                              dryso4_plot,
                                              wetso4_plot,
                                              tot_s_plot)

# Print plots
setwd(paste0(emi_dir, '/output/model_layer'))

pdf(paste0(region, '_layer2_so2-at-hgt_per_diff.pdf'), height = 11, width = 8.5, paper = "letter")
grid.draw(emissions_plot)
grid.newpage()
grid.draw(column_plot)
grid.newpage()
grid.draw(forcing_plot)
grid.newpage()
grid.draw(cloud_plot)
grid.newpage()
grid.draw(deposition_plot)
dev.off()


#-------------------------Third Layer Thickness--------------------------------

# Plotting function for third model layer
plot_species <- function(variable, y, title, units, region_or_exper, model_colors, model_symbols){
  species <- variable
  species_plot <- ggplot(species, aes(x = third_layer, y = value, color = model, shape = model))+
    theme_bw()+
    labs(title=paste0(title,' - ', region_or_exper), y=units, x="Third Layer (m)") +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(species$value))-max(abs(species$sd)), max(abs(species$value))+max(abs(species$sd)))) +
    scale_colour_manual(values = model_colors) +
    scale_shape_manual(values = model_symbols) +
    geom_point( position=position_dodge(width=0.4), size = 1.5) +
    geom_errorbar(aes(ymin=value-sd, ymax=value+sd), size = 0.1, width = 0, position=position_dodge(0.4), show.legend = F)
  return(species_plot)
}

emibc_plot <- plot_species(emibc, value, 'surface flux of BC', expression(Delta*~emibc), region, model_colors, model_symbols)
emiso2_plot <- plot_species(emiso2, value, 'surface flux of SO2', expression(Delta*~emiso2), region, model_colors, model_symbols)
mmrbc_plot <- plot_species(mmrbc, value, 'surface concentration of BC', expression(Delta*~mmrbc), region, model_colors, model_symbols)
mmrso4_plot <- plot_species(mmrso4, value, 'surface concentration of SO4', expression(Delta*~mmrso4), region, model_colors, model_symbols)
so2_plot <- plot_species(so2, value, 'surface concentration of SO2', expression(Delta*~so2), region, model_colors, model_symbols)
rlut_plot <- plot_species(rlut, value, 'upwelling longwave flux \n at TOA', expression(Delta*~rlut), region, model_colors, model_symbols)
rsut_plot <- plot_species(rsut, value, 'upwelling shortwave flux \n at TOA', expression(Delta*~rsut), region, model_colors, model_symbols)
rsdt_plot <- plot_species(rsdt, value, 'incident shortwave flux \n at TOA', expression(Delta*~rsdt), region, model_colors, model_symbols)
rlutcs_plot <- plot_species(rlutcs, value, 'upwelling clear-sky longwave \n flux at TOA', expression(Delta*~rlutcs), region, model_colors, model_symbols)
rsutcs_plot <- plot_species(rsutcs, value, 'upwelling clear-sky shortwave \n flux at TOA', expression(Delta*~rsutcs), region, model_colors, model_symbols)
drybc_plot <- plot_species(drybc, value, 'dry deposition rate \n of BC', expression(Delta*~drybc), region, model_colors, model_symbols)
wetbc_plot <- plot_species(wetbc, value, 'wet deposition rate \n of BC', expression(Delta*~wetbc), region, model_colors, model_symbols)
dryso2_plot <- plot_species(dryso2, value, 'dry deposition rate \n of so2', expression(Delta*~dryso2), region, model_colors, model_symbols)
wetso2_plot <- plot_species(wetso2, value, 'wet deposition rate \n of so2', expression(Delta*~wetso2), region, model_colors, model_symbols)
dryso4_plot <- plot_species(dryso4, value, 'dry deposition rate \n of so4', expression(Delta*~dryso4), region, model_colors, model_symbols)
wetso4_plot <- plot_species(wetso4, value, 'wet deposition rate \n of so4', expression(Delta*~wetso4), region, model_colors, model_symbols)
od550aer_plot <- plot_species(od550aer, value, 'ambient aerosol optical \n thickness at 550nm', expression(Delta*~od550aer), region, model_colors, model_symbols)
clt_plot <- plot_species(clt, value, 'total cloud cover', expression(Delta*~clt), region, model_colors, model_symbols)
cltc_plot <- plot_species(cltc, value, 'convective cloud cover', expression(Delta*~cltc), region, model_colors, model_symbols)
cl_plot <- plot_species(cl, value, 'surface cloud cover', expression(Delta*~cl), region, model_colors, model_symbols)
clivi_plot <- plot_species(clivi, value, 'ice water path', 	expression(Delta*~clivi), region, model_colors, model_symbols)
dms_plot <- plot_species(dms, value, 'surface concentration of DMS', expression(Delta*~dms), region, model_colors, model_symbols)
loadbc_plot <- plot_species(loadbc, value, 'column mass burden of BC', expression(Delta*~loadbc), region, model_colors, model_symbols)
loadso2_plot <- plot_species(loadso2, value, 'column mass burden of SO2', expression(Delta*~loadso2), region, model_colors, model_symbols)
loadso4_plot <- plot_species(loadso4, value, 'column mass burden of SO4', expression(Delta*~loadso4), region, model_colors, model_symbols)
net_rad_plot <- plot_species(net_rad, value, 'net radiative flux \n at TOA', expression(Delta*~rlut~+~rsut), region, model_colors, model_symbols)
net_rad_cs_plot <- plot_species(net_rad_cs, value, 'clear-sky net radiative flux \n at TOA', expression(Delta*~rlutcs~+~rsutcs), region, model_colors, model_symbols)
tot_bc_plot <- plot_species(tot_bc, value, 'total deposition rate \n of BC', expression(Delta*~drybc~+~wetbc), region, model_colors, model_symbols)
tot_s_plot <- plot_species(tot_s, value, 'total deposition rate \n of S', expression(Delta*(~dryso2~+~wetso2)/2~+~(dryso4~+~wetso4)/3), region, model_colors, model_symbols)
imp_cld_plot <- plot_species(imp_cld, value, 'implied cloud response at \n TOA', expression(Delta*~rlut~+~rsut~-~rlutcs~-~rsutcs), region, model_colors, model_symbols)
so4_lifetime_plot <- plot_species(so4_lifetime, value, 'SO4 lifetime', expression(Delta*~loadso4/(dryso4~+~wetso4)), region, model_colors, model_symbols)
so2_timescale_plot <- plot_species(so2_timescale, value, 'SO2 timescale', expression(Delta*~loadso2/emiso2), region, model_colors, model_symbols)


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
    top = textGrob("3rd Model Layer Summary: SO2-at-height (percent difference)", gp = gpar(fontsize = 12)))
}

emissions_plot <- grid_arrange_shared_legend(emibc_plot,
                                             emiso2_plot,
                                             mmrbc_plot,
                                             mmrso4_plot,
                                             so2_plot,
                                             dms_plot)

column_plot <- grid_arrange_shared_legend(loadso4_plot,
                                          loadso2_plot,
                                          loadbc_plot,
                                          so4_lifetime_plot,
                                          so2_timescale_plot)

forcing_plot <- grid_arrange_shared_legend(rsut_plot,
                                           rlut_plot,
                                           net_rad_plot,
                                           rsdt_plot,
                                           rlutcs_plot,
                                           rsutcs_plot,
                                           net_rad_cs_plot,
                                           imp_cld_plot)

cloud_plot <- grid_arrange_shared_legend(od550aer_plot,
                                         clt_plot,
                                         cltc_plot,
                                         cl_plot,
                                         clivi_plot)

deposition_plot <- grid_arrange_shared_legend(drybc_plot,
                                              wetbc_plot,
                                              tot_bc_plot,
                                              dryso2_plot,
                                              wetso2_plot,
                                              dryso4_plot,
                                              wetso4_plot,
                                              tot_s_plot)

# Print plots
setwd(paste0(emi_dir, '/output/model_layer'))

pdf(paste0(region, '_layer3_so2-at-hgt_per_diff.pdf'), height = 11, width = 8.5, paper = "letter")
grid.draw(emissions_plot)
grid.newpage()
grid.draw(column_plot)
grid.newpage()
grid.draw(forcing_plot)
grid.newpage()
grid.draw(cloud_plot)
grid.newpage()
grid.draw(deposition_plot)
dev.off()

#--------------------Number of Model Layers below 400 m-------------------------

# Plotting function for number of layers below 400 m
plot_species <- function(variable, y, title, units, region_or_exper, model_colors, model_symbols){
  species <- variable
  species_plot <- ggplot(species, aes(x = num_layer, y = value, color = model, shape = model))+
    theme_bw()+
    labs(title=paste0(title,' - ', region_or_exper), y=units, x="Number of Layers < 400 m") +
    theme(plot.title = element_text(hjust = 0.5, size = title_font),
          axis.text = element_text(size = axis_font),
          axis.title = element_text(size = axis_title_font)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-max(abs(species$value))-max(abs(species$sd)), max(abs(species$value))+max(abs(species$sd)))) +
    scale_colour_manual(values = model_colors) +
    scale_shape_manual(values = model_symbols) +
    geom_point( size = 1.5) +
    geom_errorbar(aes(ymin=value-sd, ymax=value+sd), size = 0.1, width = 0, show.legend = F)
  return(species_plot)
}

emibc_plot <- plot_species(emibc, value, 'surface flux of BC', expression(Delta*~emibc), region, model_colors, model_symbols)
emiso2_plot <- plot_species(emiso2, value, 'surface flux of SO2', expression(Delta*~emiso2), region, model_colors, model_symbols)
mmrbc_plot <- plot_species(mmrbc, value, 'surface concentration of BC', expression(Delta*~mmrbc), region, model_colors, model_symbols)
mmrso4_plot <- plot_species(mmrso4, value, 'surface concentration of SO4', expression(Delta*~mmrso4), region, model_colors, model_symbols)
so2_plot <- plot_species(so2, value, 'surface concentration of SO2', expression(Delta*~so2), region, model_colors, model_symbols)
rlut_plot <- plot_species(rlut, value, 'upwelling longwave flux \n at TOA', expression(Delta*~rlut), region, model_colors, model_symbols)
rsut_plot <- plot_species(rsut, value, 'upwelling shortwave flux \n at TOA', expression(Delta*~rsut), region, model_colors, model_symbols)
rsdt_plot <- plot_species(rsdt, value, 'incident shortwave flux \n at TOA', expression(Delta*~rsdt), region, model_colors, model_symbols)
rlutcs_plot <- plot_species(rlutcs, value, 'upwelling clear-sky longwave \n flux at TOA', expression(Delta*~rlutcs), region, model_colors, model_symbols)
rsutcs_plot <- plot_species(rsutcs, value, 'upwelling clear-sky shortwave \n flux at TOA', expression(Delta*~rsutcs), region, model_colors, model_symbols)
drybc_plot <- plot_species(drybc, value, 'dry deposition rate \n of BC', expression(Delta*~drybc), region, model_colors, model_symbols)
wetbc_plot <- plot_species(wetbc, value, 'wet deposition rate \n of BC', expression(Delta*~wetbc), region, model_colors, model_symbols)
dryso2_plot <- plot_species(dryso2, value, 'dry deposition rate \n of so2', expression(Delta*~dryso2), region, model_colors, model_symbols)
wetso2_plot <- plot_species(wetso2, value, 'wet deposition rate \n of so2', expression(Delta*~wetso2), region, model_colors, model_symbols)
dryso4_plot <- plot_species(dryso4, value, 'dry deposition rate \n of so4', expression(Delta*~dryso4), region, model_colors, model_symbols)
wetso4_plot <- plot_species(wetso4, value, 'wet deposition rate \n of so4', expression(Delta*~wetso4), region, model_colors, model_symbols)
od550aer_plot <- plot_species(od550aer, value, 'ambient aerosol optical \n thickness at 550nm', expression(Delta*~od550aer), region, model_colors, model_symbols)
clt_plot <- plot_species(clt, value, 'total cloud cover', expression(Delta*~clt), region, model_colors, model_symbols)
cltc_plot <- plot_species(cltc, value, 'convective cloud cover', expression(Delta*~cltc), region, model_colors, model_symbols)
cl_plot <- plot_species(cl, value, 'surface cloud cover', expression(Delta*~cl), region, model_colors, model_symbols)
clivi_plot <- plot_species(clivi, value, 'ice water path', 	expression(Delta*~clivi), region, model_colors, model_symbols)
dms_plot <- plot_species(dms, value, 'surface concentration of DMS', expression(Delta*~dms), region, model_colors, model_symbols)
loadbc_plot <- plot_species(loadbc, value, 'column mass burden of BC', expression(Delta*~loadbc), region, model_colors, model_symbols)
loadso2_plot <- plot_species(loadso2, value, 'column mass burden of SO2', expression(Delta*~loadso2), region, model_colors, model_symbols)
loadso4_plot <- plot_species(loadso4, value, 'column mass burden of SO4', expression(Delta*~loadso4), region, model_colors, model_symbols)
net_rad_plot <- plot_species(net_rad, value, 'net radiative flux \n at TOA', expression(Delta*~rlut~+~rsut), region, model_colors, model_symbols)
net_rad_cs_plot <- plot_species(net_rad_cs, value, 'clear-sky net radiative flux \n at TOA', expression(Delta*~rlutcs~+~rsutcs), region, model_colors, model_symbols)
tot_bc_plot <- plot_species(tot_bc, value, 'total deposition rate \n of BC', expression(Delta*~drybc~+~wetbc), region, model_colors, model_symbols)
tot_s_plot <- plot_species(tot_s, value, 'total deposition rate \n of S', expression(Delta*(~dryso2~+~wetso2)/2~+~(dryso4~+~wetso4)/3), region, model_colors, model_symbols)
imp_cld_plot <- plot_species(imp_cld, value, 'implied cloud response at \n TOA', expression(Delta*~rlut~+~rsut~-~rlutcs~-~rsutcs), region, model_colors, model_symbols)
so4_lifetime_plot <- plot_species(so4_lifetime, value, 'SO4 lifetime', expression(Delta*~loadso4/(dryso4~+~wetso4)), region, model_colors, model_symbols)
so2_timescale_plot <- plot_species(so2_timescale, value, 'SO2 timescale', expression(Delta*~loadso2/emiso2), region, model_colors, model_symbols)


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
    top = textGrob("Number of Model Layers below 400 m: SO2-at-height (percent difference)", gp = gpar(fontsize = 12)))
}

emissions_plot <- grid_arrange_shared_legend(emibc_plot,
                                             emiso2_plot,
                                             mmrbc_plot,
                                             mmrso4_plot,
                                             so2_plot,
                                             dms_plot)

column_plot <- grid_arrange_shared_legend(loadso4_plot,
                                          loadso2_plot,
                                          loadbc_plot,
                                          so4_lifetime_plot,
                                          so2_timescale_plot)

forcing_plot <- grid_arrange_shared_legend(rsut_plot,
                                           rlut_plot,
                                           net_rad_plot,
                                           rsdt_plot,
                                           rlutcs_plot,
                                           rsutcs_plot,
                                           net_rad_cs_plot,
                                           imp_cld_plot)

cloud_plot <- grid_arrange_shared_legend(od550aer_plot,
                                         clt_plot,
                                         cltc_plot,
                                         cl_plot,
                                         clivi_plot)

deposition_plot <- grid_arrange_shared_legend(drybc_plot,
                                              wetbc_plot,
                                              tot_bc_plot,
                                              dryso2_plot,
                                              wetso2_plot,
                                              dryso4_plot,
                                              wetso4_plot,
                                              tot_s_plot)

# Print plots
setwd(paste0(emi_dir, '/output/model_layer'))

pdf(paste0(region, '_layer_number_so2-at-hgt_per_diff.pdf'), height = 11, width = 8.5, paper = "letter")
grid.draw(emissions_plot)
grid.newpage()
grid.draw(column_plot)
grid.newpage()
grid.draw(forcing_plot)
grid.newpage()
grid.draw(cloud_plot)
grid.newpage()
grid.draw(deposition_plot)
dev.off()
