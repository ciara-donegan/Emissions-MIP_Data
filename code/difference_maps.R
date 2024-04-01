# ------------------------------------------------------------------------------
# Program Name: difference_maps.R
# Authors: Ciara Donegan
# Date Last Modified: February 14, 2024
# Program Purpose: Produces maps of the difference between reference datasets
# and perturbation datasets.
# Input Files: ~Emissions-MIP/input/
# Output Files: ~Emissions-MIP/output/
# TODO:
# ------------------------------------------------------------------------------

# Load required libraries
library(ncdf4)
library(lattice)
library(RColorBrewer)
library(raster)
library(ggplot2)
library(terra)
library(rasterVis)

# set path to netCDF files
file_dir <- paste0("C:/Users/done231/OneDrive - PNNL/Desktop/difference_plot_files")
setwd(paste0(file_dir))

# select file to open (for now, pick a file -- later make it take in all files?)
scenario <- "shp-ind-shift"
model <- "E3SM"
variable <- "clt"
units <- "W/m^2" # for labels later

# find ncdf file
filepath <- paste0(file_dir,"/input/",scenario,"/",model,"/")
filename <- list.files(path=filepath,pattern=paste0(variable,"_"))

# read in shapefile for coastline
coast_outline <- shapefile("ne_110m_coastline.shp")

# open netCDF file
ncin <- nc_open(paste0(filepath,filename))

# get coordinate vars
lon <- ncvar_get(ncin,"lon") # 0 to 360, rather than -180 to 180
lon <- lon - 180
nlon <- dim(lon)

lat <- ncvar_get(ncin,"lat") # -90 to 90
nlat <- dim(lat)

year <- ncvar_get(ncin,"year") # values are averaged across time range

# read in data from chosen variable
ncdf.array <- ncvar_get(ncin,"unknown")
fillvalue <- ncatt_get(ncin,"unknown","_FillValue") # get value used for missing data

# close netCDF file
nc_close(ncin)

# get ncdf coordinates to match shapefile (centered on prime meridian)
east <- ncdf.array[181:360,]
west <- ncdf.array[1:180,]
ncdf.shifted <- rbind(east,west)

# raster
r <- raster(t(ncdf.shifted),xmn=min(lon),xmx=max(lon),ymn=min(lat),ymx=max(lat),crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
r <- flip(r,direction='y')

# # convert to a format usable by ggplot
r.df <- as.data.frame(r,xy=TRUE)
r.df$layer <- r.df$layer*-1

# labels
map_title <- "test"

# levelplot from lattice
myTheme <- rasterTheme(region = rev(brewer.pal( 11, 'RdBu')))
jpeg_map <- levelplot(layer~x*y, data=r.df,
                      par.settings=myTheme,
                      margin=F,
                      xlab=NULL,
                      ylab=NULL,
                      main=map_title) +
  latticeExtra::layer(sp.lines(coast_outline, col="black", lwd=0.5))

jpeg_map

# # plot with ggplot
# ggplot() +
#   geom_raster(data=r.df,
#               aes(x=x,y=y,fill=layer)) +
#   geom_path(data=coast_outline,
#             aes(x=long,y=lat,group=group)) +
#   #scale_fill_gradientn(colors=c("#0000FFFF","#FFFFFFFF","#FF0000FF",midpoint=0)) +
#   #scale_color_binned() +
#   #scale_fill_stepsn(colors=rev(brewer.pal(n=11,name="RdBu"))) +
#   scale_fill_gradient2(low="blue",mid="aliceblue",high="red",breaks=c(-3,-1.5,0,1.5,3))+
#   #scale_fill_manual(values=scales::div_gradient_pal(low = "red", mid = "white",  high = "blue")(seq(0, 1, length.out = length(c(-Inf,-3:3,Inf))))) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(), axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5)) +
#   #easy_move_legend("bottom") +
#   ggtitle(paste0(model," Difference Between Reference and ",scenario)) +
#   xlab("") +
#   ylab("") +
#   labs(fill=units)




# # function
# read_ncdf <- function(var) {
#   file <- list.files(path=filepath,pattern=paste0(var,"_")) #underscore afterwards to prevent (eg) rsut from also reading in rsutcs
#   
#   # open netCDF file
#   ncin <- nc_open(paste0(filepath,file))
#   
#   # get coordinate vars
#   lon <- ncvar_get(ncin,"lon") # 0 to 360, rather than -180 to 180
#   lon <- lon - 180
#   nlon <- dim(lon)
#   
#   lat <- ncvar_get(ncin,"lat") # -90 to 90
#   nlat <- dim(lat)
#   
#   year <- ncvar_get(ncin,"year") # values are averaged across time range
#   
#   # read in data from chosen variable
#   ncdf.array <- ncvar_get(ncin,"unknown")
#   fillvalue <- ncatt_get(ncin,"unknown","_FillValue") # get value used for missing data
#   
#   # close netCDF file
#   nc_close(ncin)
#   
#   # get ncdf coordinates to match shapefile (centered on prime meridian)
#   east <- ncdf.array[181:360,]
#   west <- ncdf.array[1:180,]
#   ncdf.shifted <- rbind(east,west)
#   
#   # raster
#   r <- raster(t(ncdf.shifted),xmn=min(lon),xmx=max(lon),ymn=min(lat),ymx=max(lat),crs=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#   r <- flip(r,direction='y')
#   
#   # convert to a format usable by ggplot
#   r.df <- as.data.frame(r,xy=TRUE)
#   
#   # plot with ggplot
#   plot <- ggplot() +
#     geom_raster(data=r.df,
#                 aes(x=x,y=y,fill=layer)) +
#     geom_path(data=coast_outline,
#               aes(x=long,y=lat,group=group)) +
#     #scale_fill_stepsn(colors=rev(brewer.pal(n=7,name="RdBu"))) +
#     scale_fill_gradientn(colors=c("#0000FFFF","#FFFFFFFF","#FF0000FF",midpoint=0)) +
#     #scale_fill_brewer(palette="RdBu") +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_blank(), axis.text.x = element_blank(),
#           axis.ticks.x = element_blank(), axis.text.y = element_blank(),
#           axis.ticks.y = element_blank(), plot.title = element_text(hjust = 0.5)) +
#     #easy_move_legend("bottom") +
#     ggtitle(paste0(model," Difference Between Reference and ",scenario)) +
#     xlab("") +
#     ylab("") #+
#   #labs(fill=units)
#   
#   print(plot)
# }
