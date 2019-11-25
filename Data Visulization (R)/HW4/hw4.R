library(leaflet)
library(sf)
library(tilegramsR)
library(leaflet.extras)
library(colormap)
library(cartogram)
library(maptools)
library(tidyverse)
library(broom)
library(viridis)
library(ggplot2)
library(readr)
library(maps)
library(mapdata)
library(mapproj)
library(plyr)
library(digest)
library(rgdal)  
library(statebins)
library(rgeos)
library(dplyr)
library(ggmap)
library(reshape)
library(devtools)
if(!"makeTilegram" %in% installed.packages()[,"Package"])
  devtools::install_git("https://gitlab.com/lajh87/makeTilegram")

register_google(key = "AIzaSyDZi677w1rJFpQyYWW5hq1cx4QnY4R3Kug ")
has_google_key()

##################################################
# HW4.2a
##################################################

chicagoCrime = read_csv("/Users/appobs/Desktop/would you?/465/week7/crimeData.csv")
head(chicagoCrime)
set.seed(1234) #setting this so we'll always get the same subset
row_indexes <- sample(1:nrow(chicagoCrime), 10000, replace = F) # randomly generate 10,000 row indexes
crime <-data.frame(chicagoCrime, row_indexes) # get rows at those indexes
##################################################
# two methods to get the map from google
# **IMPORTANT: first enable the google API, and enter the encrpty key)
##################################################

#ChicagoMap = qmap("Chicago", zoom = 12, color = "bw", legend = "topleft")
Chicagogo = get_map(location = "chicago", maptype = "terrain", source = "google", zoom = 12)
map = ggmap(Chicagogo)
#bin_crimes <- chicagoCrime %>% group_by(chicagoCrime, Longitude, Latitude) %>%
#  summarise(Total = n()) %>%
#  arrange(desc(Total)) %>% ungroup()

map +
  stat_bin2d(data = chicagoCrime, aes(x = Longitude, y = Latitude, fill = ..density.., alpha = ..density..), bins = 30, size = 2) + 
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') + labs(title="Mapping Crimes in Chicago", subtitle="Crime Density in Chicago")

map + 
  stat_density2d( data = chicagoCrime, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 1, bins = 50, geom = 'polygon') +
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') +
  scale_alpha(range = c(.2, .3), guide = FALSE) +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) + labs(title="Mapping Crimes in Chicago", subtitle="Crime Density in Chicago")

##################################################
# HW4.2b
##################################################
# useless
##################################################
#chicagoShapes = readOGR("/Users/appobs/Desktop/hw/465/week8/ChicagoNeighborhoods/ChicagoShapesInR/Neighborhoods_2012b.shp")
#plot(chicagoShapes)
#head(chicagoShapes)
#head(chicagoCrime)

#chi = merge(chicagoShapes, chicagoCrime, by.x="PRI_NEIGH", by.y="Wards")
#p = colorRampPalette(c("white", "red"))(128)
#palette(p)

#assCrime = chi@data$Primary.Type = 'ASSAULT'
#plot(chi, col=assCrime)
##################################################

ASSAULTS = subset(crimeMap, Primary.Type == "ASSAULT")
KIDNAPPING = subset(crimeMap, Primary.Type == "KIDNAPPING")
NARCOTICS = subset(crimeMap, Primary.Type == "NARCOTICS")
THEFT = subset(crimeMap, Primary.Type == "THEFT")

map +
  stat_bin2d(data = ASSAULTS, aes(x = Longitude, y = Latitude, fill = ..density.., alpha = ..density..), bins = 30, size = 1) + 
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') + labs(title="Mapping ASSAULT in Chicago", subtitle="ASSAULT Density in Chicago")

map +
  stat_bin2d(data = KIDNAPPING, aes(x = Longitude, y = Latitude, fill = ..density.., alpha = ..density..), bins = 30, size = 1) + 
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') + labs(title="Mapping KIDNAPPING in Chicago", subtitle="KIDNAPPING Density in Chicago")

map +
  stat_bin2d(data = NARCOTICS, aes(x = Longitude, y = Latitude, fill = ..density.., alpha = ..density..), bins = 30, size = 1) + 
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') + labs(title="Mapping NARCOTICS in Chicago", subtitle="NARCOTICS Density in Chicago")

map +
  stat_bin2d(data = THEFT, aes(x = Longitude, y = Latitude, fill = ..density.., alpha = ..density..), bins = 30, size = 1) + 
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') + labs(title="Mapping THEFT in Chicago", subtitle="THEFT Density in Chicago")

#############################
# Using posted geo
#############################
chicagoShapes = readOGR("/Users/appobs/Desktop/hw/465/week9/hw/ChicagoWards/geo_export_b3753456-b758-4b87-a346-f457ad277e31.shp")
plot(chicagoShapes)

crimeMap = merge(chicagoCrime, chicagoShapes, by.x="Ward", by.y="ward")
head(crimeMap)

# We need to resort these for drawing since the polygon points have been resorted
# here by the merge command

crimeMap=arrange(crimeMap, District, Community.Area)

ASSAULTS = subset(crimeMap, Primary.Type == "ASSAULT")
KIDNAPPING = subset(crimeMap, Primary.Type == "KIDNAPPING")
NARCOTICS = subset(crimeMap, Primary.Type == "NARCOTICS")
THEFT = subset(crimeMap, Primary.Type == "THEFT")

head(ASSAULTS)
head(KIDNAPPING)
head(NARCOTICS)
head(THEFT)

# Now, let's draw these motherfuckers!!.
map +
  stat_bin2d(data = ASSAULTS, aes(x = Longitude, y = Latitude, fill = ..density.., alpha = ..density..), bins = 30, size = 1) + 
  scale_fill_gradient('Crime\nDensity', low = 'blue', high = 'orange') + labs(title="Mapping ASSAULT in Chicago", subtitle="ASSAULT Density in Chicago")

ggplot(ASSAULTS, aes(x=Longitude, y=Latitude, group=Ward, fill=Primary.Type)) + 
  geom_polygon(data = chicagoShapes, aes(x = Longitude, y = Latitude, fill = "red", alpha = 0.8), size = 1, shape = 21) + guides(fill=FALSE, alpha=FALSE, size=FALSE) +
  scale_alpha(range = c(.2, .3), guide = FALSE) +
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) + labs(title="Mapping ASSAULT in Chicago", subtitle="ASSAULT Density in Chicago") +
  stat_density2d( data = chicagoCrime, aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 1, bins = 50, geom = 'polygon') +
  scale_fill_gradient2('Crime\nDensity', low = 'blue', high = 'orange')
  
  

##################################################
# HW4.3b
##################################################

terrain = read.csv("/Users/appobs/Desktop/hw/465/week9/hw/terrain1.csv", header=T)
head(terrain)

# Now we can use this.  Note that we get no labels here
# but we do get the contours over the map. Note also
# that the geom_contour() expects to have a "z" aestheitic
ggplot(terrain, aes(x = x, y = y, z = z, fill=z)) + 
  geom_tile() + 
  scale_fill_gradientn(colors=terrain.colors(10)) + 
  geom_contour(color="blue")