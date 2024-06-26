# Display Atlantic/Pacific ratios and differences

library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(tidyr)

# Set up blank dataframe to put all inputs into
input.all <- data.frame(model=character(),variable=character(),
                        NH.atlantic=numeric(),NH.atlantic.sd=numeric(),
                        NH.pacific=numeric(),NH.pacific.sd=numeric(),
                        difference=numeric(),diff.sd=numeric(),
                        ratio=numeric(),ratio.sd=numeric(),scenario=character())

# Function from stack exchange to generate a shared legend
grid_arrange_shared_legend <- function(...,text) {
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
    top = textGrob(text, gp = gpar(fontsize = 12)))
}

for (scenario in c("shp-30p-red","shp-60p-red","shp-60p-red-1950",
                   "shp-atl-shift","shp-atl-shift-1950",
                   "shp-ind-shift","shp-ind-shift-1950")) {
  setwd("~")
  input <- read.csv(paste0("atl-pac-comparison_",scenario,".csv"))
  input <- drop_na(input)
  
  # Correct model names
  input$model[which(input$model == "CESM")] <- "CESM1"
  input$model[which(input$model == "GISS")] <- "GISS-E2.1"
  input$model[which(input$model == "CAM5ATRAS")] <- "CAM-ATRAS"
  input$model[which(input$model == "GFDL")] <- "GFDL-ESM4"
  
  input$variable[which(input$variable == "rsut")] <- "Upwelling Shortwave Flux"
  input$variable[which(input$variable == "rlut")] <- "Upwelling Longwave Flux"
  input$variable[which(input$variable == "rsutcs")] <- "Clear-Sky Upwelling Shortwave Flux"
  input$variable[which(input$variable == "rlutcs")] <- "Clear-Sky Upwelling Longwave Flux"
  input$variable[which(input$variable == "net_rad")] <- "Net Radiative Flux"
  input$variable[which(input$variable == "ICR")] <- "Implied Cloud Response"
  
  # Plot aesthetics
  cbPalette <- c("#c4c4c3", "#4477aa", "#228833", "#66ccee", "#ccbb44","#ee6677", "#aa3377")
  
  model_colors <- c('CESM1' = cbPalette[1], 'GISS-E2.1' = cbPalette[2], 'CAM-ATRAS' = cbPalette[3], 'GEOS' = cbPalette[4], 'NorESM2' = cbPalette[5], 'GFDL-ESM4' = cbPalette[6], 'E3SM' = cbPalette[7])
  model_symbols <- c("CESM1" = 15, "GISS-E2.1" = 15, "CAM-ATRAS" = 17,  "GEOS" = 17, "NorESM2" = 17, "GFDL-ESM4" = 19, "E3SM" = 15)
  
  # plot
  plot_difference <- function(var) {
    df.select <- filter(input,variable==var)
    plot <- ggplot(df.select,aes(x=model,y=difference,color=model,shape=model)) +
      geom_point() +
      geom_errorbar(aes(ymin=difference-diff.sd,ymax=difference+diff.sd)) +
      geom_hline(yintercept = 0) +
      scale_color_manual(values = model_colors) +
      scale_shape_manual(values = model_symbols) +
      labs(title=paste0(var,' -\nDifference Between Atlantic and Pacific'),
           x="",y="Difference (W)\n(|Atlantic| - |Pacific|)") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    return(plot)
  }
  
  plot_ratio <- function(var) {
    df.select <- filter(input,variable==var)
    plot <- ggplot(df.select,aes(x=model,y=ratio,color=model,shape=model)) +
      geom_point() +
      geom_errorbar(aes(ymin=ratio-ratio.sd,ymax=ratio+ratio.sd)) +
      geom_hline(yintercept = 1) +
      scale_color_manual(values = model_colors) +
      scale_shape_manual(values = model_symbols) +
      labs(title=paste0(var,' -\nRatio Between Atlantic and Pacific'),
           x="",y="Ratio (|Atlantic/Pacific|)") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    return(plot)
  }
  
  # Generate plots
  rsut_diff_plot <- plot_difference("Upwelling Shortwave Flux")
  rlut_diff_plot <- plot_difference("Upwelling Longwave Flux")
  rsutcs_diff_plot <- plot_difference("Clear-Sky Upwelling Shortwave Flux")
  rlutcs_diff_plot <- plot_difference("Clear-Sky Upwelling Longwave Flux")
  net_rad_diff_plot <- plot_difference("Net Radiative Flux")
  ICR_diff_plot <- plot_difference("Implied Cloud Response")
  
  rsut_ratio_plot <- plot_ratio("Upwelling Shortwave Flux")
  rlut_ratio_plot <- plot_ratio("Upwelling Longwave Flux")
  rsutcs_ratio_plot <- plot_ratio("Clear-Sky Upwelling Shortwave Flux")
  rlutcs_ratio_plot <- plot_ratio("Clear-Sky Upwelling Longwave Flux")
  net_rad_ratio_plot <- plot_ratio("Net Radiative Flux")
  ICR_ratio_plot <- plot_ratio("Implied Cloud Response")
  
  difference_plots <- grid_arrange_shared_legend(rsut_diff_plot,
                                                 rlut_diff_plot,
                                                 rsutcs_diff_plot,
                                                 rlutcs_diff_plot,
                                                 net_rad_diff_plot,
                                                 ICR_diff_plot,
                                                 text = scenario)
  
  ratio_plots <- grid_arrange_shared_legend(rsut_ratio_plot,
                                            rlut_ratio_plot,
                                            rsutcs_ratio_plot,
                                            rlutcs_ratio_plot,
                                            net_rad_ratio_plot,
                                            ICR_ratio_plot,
                                            text = scenario)
  
  # Output plots as pdf
  setwd(paste0("C:/Users/done231/OneDrive - PNNL/Desktop/difference_plot_files/output/"))
  pdf(paste0(scenario, '_atl-pac-comparison.pdf'), height = 11, width = 8.5, paper = "letter")
  
  grid.draw(difference_plots)
  grid.newpage()
  grid.draw(ratio_plots)
  dev.off()
  
  # Append data to end of dataframe
  input.all <- bind_rows(input.all,input)
}

# Plot all scenario values
# Function
plot_difference_all <- function(var) {
  df.select <- filter(input.all,variable==var)
  plot <- ggplot(df.select,aes(x=scenario,y=difference,color=model,shape=model)) +
    geom_point(position=position_dodge(width=0.4), size = 1.5) +
    geom_errorbar(aes(ymin=difference-diff.sd,ymax=difference+diff.sd),
                  position=position_dodge(width=0.4)) +
    geom_hline(yintercept = 0) +
    scale_color_manual(values = model_colors) +
    scale_shape_manual(values = model_symbols) +
    labs(title=paste0(var,' -\nDifference Between Atlantic and Pacific'),
         x="",y="Difference (W)\n(|Atlantic| - |Pacific|)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  return(plot)
}

plot_ratio_all <- function(var) {
  df.select <- filter(input.all,variable==var)
  plot <- ggplot(df.select,aes(x=scenario,y=ratio,color=model,shape=model)) +
    geom_point(position=position_dodge(width=0.4), size = 1.5) +
    geom_errorbar(aes(ymin=ratio-ratio.sd,ymax=ratio+ratio.sd),
                  position=position_dodge(width=0.4)) +
    geom_hline(yintercept = 1) +
    scale_color_manual(values = model_colors) +
    scale_shape_manual(values = model_symbols) +
    labs(title=paste0(var,' -\nRatio Between Atlantic and Pacific'),
         x="",y="Ratio (|Atlantic/Pacific|)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  return(plot)
}

# Generate plots
rsut_diff_plot_all <- plot_difference_all("Upwelling Shortwave Flux")
rlut_diff_plot_all <- plot_difference_all("Upwelling Longwave Flux")
rsutcs_diff_plot_all <- plot_difference_all("Clear-Sky Upwelling Shortwave Flux")
rlutcs_diff_plot_all <- plot_difference_all("Clear-Sky Upwelling Longwave Flux")
net_rad_diff_plot_all <- plot_difference_all("Net Radiative Flux")
ICR_diff_plot_all <- plot_difference_all("Implied Cloud Response")

rsut_ratio_plot_all <- plot_ratio_all("Upwelling Shortwave Flux")
rlut_ratio_plot_all <- plot_ratio_all("Upwelling Longwave Flux")
rsutcs_ratio_plot_all <- plot_ratio_all("Clear-Sky Upwelling Shortwave Flux")
rlutcs_ratio_plot_all <- plot_ratio_all("Clear-Sky Upwelling Longwave Flux")
net_rad_ratio_plot_all <- plot_ratio_all("Net Radiative Flux")
ICR_ratio_plot_all <- plot_ratio_all("Implied Cloud Response")


difference_plots <- grid_arrange_shared_legend(rsut_diff_plot_all,
                                               rlut_diff_plot_all,
                                               rsutcs_diff_plot_all,
                                               rlutcs_diff_plot_all,
                                               net_rad_diff_plot_all,
                                               ICR_diff_plot_all,
                                               text = "Atlantic-Pacific comparison - all scenarios")

ratio_plots <- grid_arrange_shared_legend(rsut_ratio_plot_all,
                                          rlut_ratio_plot_all,
                                          rsutcs_ratio_plot_all,
                                          rlutcs_ratio_plot_all,
                                          net_rad_ratio_plot_all,
                                          ICR_ratio_plot_all,
                                          text = "Atlantic-Pacific comparison - all scenarios")

# Output plots as pdf
setwd(paste0("C:/Users/done231/OneDrive - PNNL/Desktop/difference_plot_files/output/"))
pdf(paste0('all-scenarios_atl-pac-comparison.pdf'), height = 11, width = 8.5, paper = "letter")

grid.draw(difference_plots)
grid.newpage()
grid.draw(ratio_plots)
dev.off()

# Get plots with just atl shift (modern and 1950)
input.all <- filter(input.all,scenario=="shp-atl-shift"|scenario=="shp-atl-shift-1950")

rsut_diff_plot_atl <- plot_difference_all("Upwelling Shortwave Flux")
rlut_diff_plot_atl <- plot_difference_all("Upwelling Longwave Flux")
rsutcs_diff_plot_atl <- plot_difference_all("Clear-Sky Upwelling Shortwave Flux")
rlutcs_diff_plot_atl <- plot_difference_all("Clear-Sky Upwelling Longwave Flux")
net_rad_diff_plot_atl <- plot_difference_all("Net Radiative Flux")
ICR_diff_plot_atl <- plot_difference_all("Implied Cloud Response")

rsut_ratio_plot_atl <- plot_ratio_all("Upwelling Shortwave Flux")
rlut_ratio_plot_atl <- plot_ratio_all("Upwelling Longwave Flux")
rsutcs_ratio_plot_atl <- plot_ratio_all("Clear-Sky Upwelling Shortwave Flux")
rlutcs_ratio_plot_atl <- plot_ratio_all("Clear-Sky Upwelling Longwave Flux")
net_rad_ratio_plot_atl <- plot_ratio_all("Net Radiative Flux")
ICR_ratio_plot_atl <- plot_ratio_all("Implied Cloud Response")


difference_plots <- grid_arrange_shared_legend(rsut_diff_plot_atl,
                                               rlut_diff_plot_atl,
                                               rsutcs_diff_plot_atl,
                                               rlutcs_diff_plot_atl,
                                               net_rad_diff_plot_atl,
                                               ICR_diff_plot_atl,
                                               text = "Atlantic-Pacific comparison - Atlantic shift scenarios")

ratio_plots <- grid_arrange_shared_legend(rsut_ratio_plot_atl,
                                          rlut_ratio_plot_atl,
                                          rsutcs_ratio_plot_atl,
                                          rlutcs_ratio_plot_atl,
                                          net_rad_ratio_plot_atl,
                                          ICR_ratio_plot_atl,
                                          text = "Atlantic-Pacific comparison - Atlantic shift scenarios")

# Output plots as pdf
setwd(paste0("C:/Users/done231/OneDrive - PNNL/Desktop/difference_plot_files/output/"))
pdf(paste0('atl-shift-scenarios_atl-pac-comparison.pdf'), height = 11, width = 8.5, paper = "letter")

grid.draw(difference_plots)
grid.newpage()
grid.draw(ratio_plots)
dev.off()

# Get ind shift plots
setwd("~")
input.ind <- read.csv("ind-comparison_shp-ind-shift.csv")
input.ind.1950 <- read.csv("ind-comparison_shp-ind-shift-1950.csv")
input.ind <- bind_rows(input.ind,input.ind.1950)

# Correct model names
input.ind$model[which(input.ind$model == "CESM")] <- "CESM1"
input.ind$model[which(input.ind$model == "GISS")] <- "GISS-E2.1"
input.ind$model[which(input.ind$model == "CAM5ATRAS")] <- "CAM-ATRAS"
input.ind$model[which(input.ind$model == "GFDL")] <- "GFDL-ESM4"

input.ind$variable[which(input.ind$variable == "rsut")] <- "Upwelling Shortwave Flux"
input.ind$variable[which(input.ind$variable == "rlut")] <- "Upwelling Longwave Flux"
input.ind$variable[which(input.ind$variable == "rsutcs")] <- "Clear-Sky Upwelling Shortwave Flux"
input.ind$variable[which(input.ind$variable == "rlutcs")] <- "Clear-Sky Upwelling Longwave Flux"
input.ind$variable[which(input.ind$variable == "net_rad")] <- "Net Radiative Flux"
input.ind$variable[which(input.ind$variable == "ICR")] <- "Implied Cloud Response"

plot_difference_ind <- function(var) {
  df.select <- filter(input.ind,variable==var)
  plot <- ggplot(df.select,aes(x=scenario,y=difference,color=model,shape=model)) +
    geom_point(position=position_dodge(width=0.4), size = 1.5) +
    geom_errorbar(aes(ymin=difference-diff.sd,ymax=difference+diff.sd),
                  position=position_dodge(width=0.4)) +
    geom_hline(yintercept = 0) +
    scale_color_manual(values = model_colors) +
    scale_shape_manual(values = model_symbols) +
    labs(title=paste0(var,' -\nDifference Between Ind and Atl+Pac'),
         x="",y="Difference (W)\n(|Ind| - |Atl+Pac|)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  return(plot)
}

plot_ratio_ind <- function(var) {
  df.select <- filter(input.ind,variable==var)
  plot <- ggplot(df.select,aes(x=scenario,y=ratio,color=model,shape=model)) +
    geom_point(position=position_dodge(width=0.4), size = 1.5) +
    geom_errorbar(aes(ymin=ratio-ratio.sd,ymax=ratio+ratio.sd),
                  position=position_dodge(width=0.4)) +
    geom_hline(yintercept = 0) +
    scale_color_manual(values = model_colors) +
    scale_shape_manual(values = model_symbols) +
    labs(title=paste0(var,' -\nRatio Between Ind and Atl+Pac'),
         x="",y="Difference (W)\n(|Ind|/|Atl+Pac|)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  return(plot)
}

rsut_diff_plot_ind <- plot_difference_ind("Upwelling Shortwave Flux")
rlut_diff_plot_ind <- plot_difference_ind("Upwelling Longwave Flux")
rsutcs_diff_plot_ind <- plot_difference_ind("Clear-Sky Upwelling Shortwave Flux")
rlutcs_diff_plot_ind <- plot_difference_ind("Clear-Sky Upwelling Longwave Flux")
net_rad_diff_plot_ind <- plot_difference_ind("Net Radiative Flux")
ICR_diff_plot_ind <- plot_difference_ind("Implied Cloud Response")

rsut_ratio_plot_ind <- plot_ratio_ind("Upwelling Shortwave Flux")
rlut_ratio_plot_ind <- plot_ratio_ind("Upwelling Longwave Flux")
rsutcs_ratio_plot_ind <- plot_ratio_ind("Clear-Sky Upwelling Shortwave Flux")
rlutcs_ratio_plot_ind <- plot_ratio_ind("Clear-Sky Upwelling Longwave Flux")
net_rad_ratio_plot_ind <- plot_ratio_ind("Net Radiative Flux")
ICR_ratio_plot_ind <- plot_ratio_ind("Implied Cloud Response")


difference_plots <- grid_arrange_shared_legend(rsut_diff_plot_ind,
                                               rlut_diff_plot_ind,
                                               rsutcs_diff_plot_ind,
                                               rlutcs_diff_plot_ind,
                                               net_rad_diff_plot_ind,
                                               ICR_diff_plot_ind,
                                               text = "Indian-Atl+Pac comparison - Indian shift scenarios")

ratio_plots <- grid_arrange_shared_legend(rsut_ratio_plot_ind,
                                          rlut_ratio_plot_ind,
                                          rsutcs_ratio_plot_ind,
                                          rlutcs_ratio_plot_ind,
                                          net_rad_ratio_plot_ind,
                                          ICR_ratio_plot_ind,
                                          text = "Indian-Atl+Pac comparison - Indian shift scenarios")

# Output plots as pdf
setwd(paste0("C:/Users/done231/OneDrive - PNNL/Desktop/difference_plot_files/output/"))
pdf(paste0('ind-shift-scenarios_ind-atl-pac-comparison.pdf'), height = 11, width = 8.5, paper = "letter")

grid.draw(difference_plots)
grid.newpage()
grid.draw(ratio_plots)
dev.off()