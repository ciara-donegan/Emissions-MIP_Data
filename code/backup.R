# ------------------------------------------------------------------------------

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









emibc <- dplyr::filter(summary_long, variable == "emibc")
emibc_plot <- ggplot(emibc, aes(x = experiment, y = value, color = model)) +
  theme_bw() +
  labs(title=paste0('surface flux of BC - ', region_or_exper), y="emibc (kg m-2 s-1)") +
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



#creates a new function that creates plots for cumulative species (such as net_rad_plot)
plot_cumulative_species <- function(x, y, title, units, region_or_exper){
  species_plot <- ggplot(species, aes(x = x, y = y, color = model))+
    theme_bw()+
    labs(title=paste0(title,' - ', region), y=units) +
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
