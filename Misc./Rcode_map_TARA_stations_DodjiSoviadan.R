
##### 29/11/19: R code to load the stations coordinates of TARA OCEAN's Multinet net tows and map them in different styles for Dodji, by Fabio Benedetti, UP group, D-USYS, ETH Zürich. 

### Last update: 29/11/19

# Libraries needed
# install.packages("") # to install packages
library("tidyverse")
library("raster")
library("RColorBrewer")
library("viridis")
library("maps")
library("marmap")

### ------------------------------------------------------------------------------------------------------------------------------------

### Load the global coastline using map_data, you will overlay it on the bathymetry data which are negative (remove altitude on land cells)
world <- map_data('world')
# Check quickly with a plot
ggplot() + geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey70", colour = "black", size = 0.3) +
			coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-120,-60,0,60,120,180),
           		labels = c("180°W","120°W","60°W","0°W","60°E","120°E","180°E"), expand = c(0,0)) +
			scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
	      		labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
			theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
				panel.grid.major = element_line(colour = "grey70",linetype = "dashed") )

### Load the global bathymetry data using getNOAA.bathy from marmap. Not too high a resolution because global scale.
# ?getNOAA.bathy
# Don't use res = 1 or 4 because wayt oo long to load and high resolution, 15 is enough for a global map
bathy <- getNOAA.bathy(lon1 = -180, lon2 = 180, lat1 = -90, lat2 = 90, resolution = 15) # This may take seconds to minutes, depending on grid size and resolution wanted
bathy <- fortify(bathy) # convert to ddf
class(bathy)
summary(bathy) ; head(bathy)
# Make a plot of global bathymetry by overlaying the coastline loaded above. Add geom_contour for the isobaths and play aound with the color palettes
ggplot() + geom_raster(aes(x = x, y = y, fill = z), data = bathy[bathy$z <= 0,]) + 
            scale_fill_distiller(name = "Depth (m)", palette = "Blues") + 
            geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey70", colour = "black", size = 0.3) +
			coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-120,-60,0,60,120,180),
           		labels = c("180°W","120°W","60°W","0°W","60°E","120°E","180°E"), expand = c(0,0)) +
			scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
	      		labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
			theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
				panel.grid.major = element_line(colour = "grey70",linetype = "dashed") )
# Nice map already, let's make it extra nicer :-)

# Option A) Add contours for isobaths : use the binwidth to choose ADVICE: don't choose binwidth lower than 1000 otherwise too many isobaths
ggplot() + geom_raster(aes(x = x, y = y, fill = z), data = bathy[bathy$z <= 0,]) + 
            scale_fill_distiller(name = "Depth (m)", palette = "Blues") + 
            #geom_contour(colour = "white", binwidth = 2000, size = 0.25, aes(x = x, y = y, z = z), data = bathy[bathy$z <= 0,]) +
            geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey70", colour = "black", size = 0.3) +
			coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-120,-60,0,60,120,180),
           		labels = c("180°W","120°W","60°W","0°W","60°E","120°E","180°E"), expand = c(0,0)) +
			scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
	      		labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
			theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
				panel.grid.major = element_line(colour = "grey70",linetype = "dashed") )

# Option B) Try white/ light grey for continents and grey for bathymetry
ggplot() + geom_raster(aes(x = x, y = y, fill = z), data = bathy[bathy$z <= 0,]) + 
            scale_fill_distiller(name = "Depth (m)", palette = "Greys") + 
            #geom_contour(colour = "white", binwidth = 2000, size = 0.25, aes(x = x, y = y, z = z), data = bathy[bathy$z <= 0,]) +
            geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey95", colour = "black", size = 0.3) +
			coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-120,-60,0,60,120,180),
           		labels = c("180°W","120°W","60°W","0°W","60°E","120°E","180°E"), expand = c(0,0)) +
			scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
	      		labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
			theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
				panel.grid.major = element_line(colour = "grey70",linetype = "dashed") )

# Option C) try the etopo gradient? 
ggplot() + geom_raster(aes(x = x, y = y, fill = z), data = bathy[bathy$z <= 0,]) + 
            scale_fill_etopo(name = "Depth (m)") + 
            geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey70", colour = "black", size = 0.3) +
			coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-120,-60,0,60,120,180),
           		labels = c("180°W","120°W","60°W","0°W","60°E","120°E","180°E"), expand = c(0,0)) +
			scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
	      		labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
			theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
				panel.grid.major = element_line(colour = "grey70",linetype = "dashed") )

# Let's not keep the isobaths because they make evertyhing more confusing and the bathymetric information is already there anyways
# Let's stick with option A) blue gradient for bathymetry, grey, white isobaths. I like the etopo (option C) gradient too though


### Load the coordinates of the station
coords <- read.csv("stations_multinet_TARA_Dodji.csv", h = T, sep = ";")
head(coords); str(coords)

### Add them as points with black contour on the map
ggplot() + geom_raster(aes(x = x, y = y, fill = z), data = bathy[bathy$z <= 0,]) + 
            scale_fill_distiller(name = "Depth (m)", palette = "Blues") + 
            geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey70", colour = "black", size = 0.3) +
			coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-120,-60,0,60,120,180),
           		labels = c("180°W","120°W","60°W","0°W","60°E","120°E","180°E"), expand = c(0,0)) +
			scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
	      		labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
			theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
				panel.grid.major = element_line(colour = "grey70",linetype = "dashed") ) + 
            geom_point(aes(x = x, y = y, colour = factor(basin)), data = coords) #+ scale_colour_brewer(name = "Ocean")

# Try with other colour palette because South Atlantic not super visible (blue on top of blue)
ggplot() + geom_raster(aes(x = x, y = y, fill = z), data = bathy[bathy$z <= 0,]) + 
            scale_fill_distiller(name = "Depth (m)", palette = "Blues") + 
            geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey70", colour = "black", size = 0.3) +
			coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-120,-60,0,60,120,180),
           		labels = c("180°W","120°W","60°W","0°W","60°E","120°E","180°E"), expand = c(0,0)) +
			scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
	      		labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
			theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
				panel.grid.major = element_line(colour = "grey70",linetype = "dashed") ) + 
            geom_point(aes(x = x, y = y, colour = factor(basin)), data = coords) + 
            scale_colour_brewer(name = "Ocean", palette = "Paired") # check names of palettes at http://colorbrewer2.org 
            
# Try other color gradients for bathymetry if you want to stick to your initial default palette
unique(coords$basin)
map1 <- ggplot() + geom_raster(aes(x = x, y = y, fill = z), data = bathy[bathy$z <= 0,]) + 
            scale_fill_distiller(name = "Depth (m)", palette = "Greys") + 
            geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey95", colour = "black", size = 0.3) +
			coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-120,-60,0,60,120,180),
           		labels = c("180°W","120°W","60°W","0°W","60°E","120°E","180°E"), expand = c(0,0)) +
			scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
	      		labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
			theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
				panel.grid.major = element_line(colour = "grey70",linetype = "dashed") ) + 
            geom_point(aes(x = x, y = y), data = coords, pch = 21, colour = "black", size = 2.5) + 
            geom_point(aes(x = x, y = y, colour = factor(basin)), data = coords) + 
            scale_colour_hue(name = "Ocean basin")
#
# Let's save this last map for Dodji, I thini it's the clearest
ggsave(plot = map1, filename = "map_TARA_stations_Dodji.pdf", dpi = 300, height = 6, width = 10)
# change dimensions of pdf with height and width arguments

### And use ggrepel to add the name of the stations next to the points ! 
require("ggrepel")
# Make the contours of the contients and countries less visible otherwose the stations labels might end up unreadable
map2 <- ggplot() + geom_raster(aes(x = x, y = y, fill = z), data = bathy[bathy$z <= 0,]) + 
            scale_fill_distiller(name = "Depth (m)", palette = "Greys") + 
            geom_polygon(aes(x = long, y = lat, group = group), data = world, fill = "grey95", colour = "grey75", size = 0.3) +
			coord_quickmap() + scale_x_continuous(name = "Longitude", breaks = c(-180,-120,-60,0,60,120,180),
           		labels = c("180°W","120°W","60°W","0°W","60°E","120°E","180°E"), expand = c(0,0)) +
			scale_y_continuous(name = "Latitude", breaks = c(-90,-60,-30,0,30,60,90),
	      		labels = c("-90°N","-60°N","-30°N","0°N","30°N","60°N","90°N"), expand = c(0,0)) +
			theme(panel.background = element_rect(fill = "white"),legend.key = element_rect(fill = "grey50"),
				panel.grid.major = element_line(colour = "grey70",linetype = "dashed") ) + 
            geom_point(aes(x = x, y = y, colour = factor(basin)), data = coords) + 
            geom_text_repel(aes(x = x, y = y, label = station), data = coords)
# save
ggsave(plot = map2, filename = "map_TARA_stations_Dodji_with labels.pdf", dpi = 300, height = 8, width = 12)


### ------------------------------------------------------------------------------------------------------------------------------------


