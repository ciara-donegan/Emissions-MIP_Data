

emi_dir <- 'C:/Users/such559/Documents/Emissions-MIP_Phase1b'
reg_name <- "arctic"
exper <- "shp-10p-red"

setwd(paste0(emi_dir,'/input/', reg_name,'/', exper, '/diff'))

# Read in csv files and bind into single data frame
target_filename <- list.files(getwd(), "*.csv")
regional_data <- rbind(map(target_filename, read.csv))
regional_data <- lapply(regional_data, function(x) {x["unit"] <- NULL; x})
regional_data <- bind_rows(regional_data)

# Extract model from file names (fifth segment) and bind to experiment data frame
models <- sapply(strsplit(target_filename, "[-.]+"),function(x) x[5])
rep_models <- rep(models, each = 5) # four years
regional_data$model <- rep_models

# Convert SO2 volume mixing ratio to mass mixing ratio by multiplying by molar
# mass of SO2 and dividing by molar mass of air, invert sign of forcing variables
# to be consistent with convention (i.e. positive value denotes a heating effect),
# then take the average over all years for each variable and calculate std dev
regional_data_summary <- regional_data %>%
    dplyr::group_by(variable, model) %>%
    within(value <- ifelse(variable == "so2", 64.066 / 28.96, 1) * value) %>%
    within(value <- ifelse(variable %in% c("rlut", "rsut", "rlutcs", "rsutcs"), -1, 1) * value) %>%
    within(value <- ifelse(variable %in% c("wetbc", "wetso2", "wetso4") & model == "CESM2", -1, 1) * value) %>%
    dplyr::summarise(regional_data = mean(value), regional_data_sd = sd(value))
