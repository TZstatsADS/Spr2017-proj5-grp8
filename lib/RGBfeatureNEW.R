rgbfeature<-function(){
  


# Required packages
library(biOps)
library(raster)

# Load and plot data
#data(logo)
experiment_dir <- "../data/"
jpg <- paste(experiment_dir, "images/", sep="")


plot.imagedata(jpg)

# Convert imagedata to raster
rst.blue <- raster(jpg[,,1])
rst.green <- raster(jpg[,,2])
rst.red <- raster(jpg[,,3])

# Plot single raster images and RGB composite
plot(stack(rst.blue, rst.green, rst.red), 
     main = c("Blue band", "Green band", "Red band"))
plotRGB(stack(rst.blue, rst.green, rst.red))