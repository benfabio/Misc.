
##### 25/11/19: R code to load the various bathymetry data downloaded from ©EMODNET (http://www.emodnet.eu/) and make the maps for Sónia in different styles, by Fabio Benedetti, UP group, D-USYS, ETH Zürich. 

### Last update: 25/11/19

# Libraries needed
# install.packages("") # to install packages
library("tidyverse")
library("raster")
library("RColorBrewer")
library("viridis")
library("maps")
library("marmap")

### ------------------------------------------------------------------------------------------------------------------------------------

### A°) Load the 2 different ASCII files (Berlengas_archipelago; Lisboa) as rasters for the high resolution bathymetry data
archi <- raster("Berlengas_archipelago")
lis <- raster("Lisboa.asc")
# Convert to data.frame because ggplot only works with data.frame
archi <- as.data.frame(archi, xy = T)
lis <- as.data.frame(lis, xy = T)
colnames(archi)[3] <- "bathy"
colnames(lis)[3] <- "bathy"
# str(archi); str(lis)
dim(lis) : dim(archi)
# summary(lis)

### Need to reverse the bathymetry values to put them as negative
lis$bathy <- (lis$bathy)*-1
archi$bathy <- (archi$bathy)*-1

### And add a bathymetry data of the whole Portuguese coast (at a lower resolution but sufficient)
# ?getNOAA.bathy
port <- getNOAA.bathy(lon1 = -11, lon2 = -8, lat1 = 36, lat2 = 44, resolution = 1)
port <- fortify(port)
str(port)
colnames(port) <- c("lon","lat","bathy")
summary(port)

### Portuguese coastline
#quartz()
m1 <- ggplot() + geom_raster(aes(x = lon, y = lat, fill = bathy), data = port[port$bathy <= 0,]) + 
        geom_raster(aes(x = lon, y = lat), data = port[port$bathy > 0,], fill = "grey25") + 
		geom_contour(aes(x = lon, y = lat, z = bathy), data = port[port$bathy <= 0,], colour = "grey30", binwidth = 500) + 
		scale_fill_distiller(name = "Depth (m)", palette = "Blues") + coord_quickmap() + 
        ylab("Latitude (°N)") + xlab("Longitude (°E)") + theme_light()

### Coastline perto de Lisboa
#quartz()
m2 <- ggplot() + geom_raster(aes(x = x, y = y, fill = bathy), data = lis[lis$bathy <= 0 & lis$bathy >= -300,]) + 
        geom_raster(aes(x = x, y = y), data = lis[lis$bathy > 0,], fill = "grey25") + 
        geom_raster(aes(x = x, y = y), data = lis[lis$bathy <= -300,], fill = "#4292c6") +
        geom_contour(aes(x = x, y = y, z = bathy), data = lis[lis$bathy <= 0 & lis$bathy >= -300,], colour = "grey30", binwidth = 50) + 
        geom_contour(aes(x = x, y = y, z = bathy), data = lis[lis$bathy <= -300,], colour = "grey30", binwidth = 500) + 
        scale_fill_distiller(name = "Depth (m)", palette = "Blues") + coord_quickmap() + 
        ylab("Latitude (°N)") + xlab("Longitude (°E)") + theme_light()

### Coastline of as Barlengas 
# summary(archi)
# Don't forget to add a point for the sampling station !

#quartz()
m3 <- ggplot() + geom_raster(aes(x = x, y = y, fill = bathy), data = archi[archi$bathy <= 0 & archi$bathy >= -300,]) + 
        geom_raster(aes(x = x, y = y), data = archi[archi$bathy > 0,], fill = "darkkhaki") + 
        geom_raster(aes(x = x, y = y), data = archi[archi$bathy <= -300,], fill = "#4292c6") +
        geom_contour(aes(x = x, y = y, z = bathy), data = archi[archi$bathy <= 0 & archi$bathy >= -300,], colour = "grey30", binwidth = 25) + 
        geom_contour(aes(x = x, y = y, z = bathy), data = archi[archi$bathy >= 0,], colour = "grey30", binwidth = 20) + 
        geom_contour(aes(x = x, y = y, z = bathy), data = archi[archi$bathy <= -300,], colour = "grey30", binwidth = 500) + 
        geom_point(aes(x = -9.50, y = 39.417), fill = 'red', colour = 'black', shape = 24, size = 2.5) + 
        scale_fill_distiller(name = "Depth (m)", palette = "Blues") + coord_quickmap() + 
        ylab("Latitude (°N)") + xlab("Longitude (°E)") + theme_light()


# Save as .pdf    
setwd("/Users/fabiobenedetti/Desktop/")   
ggsave(m1, filename = "map_Portugal.pdf", dpi = 300, width = 6, height = 6)
ggsave(m2, filename = "map_Lisboa.pdf", dpi = 300, width = 6, height = 6)
ggsave(m3, filename = "map_Barlengas.pdf", dpi = 300, width = 6, height = 6)



### ------------------------------------------------------------------------------------------------------------------------------------

