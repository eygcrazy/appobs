
install.packages("ggmap")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages("mapproj")
install.packages("mapdata")
install.packages("maps")

library(ggplot2)
library(rgeos)
library(maptools)
library(rgdal)
library(maps)
library(mapdata)
library(mapproj)

chicagoShapes = readOGR("/Users/appobs/Desktop/hw/465/week8/ChicagoNeighborhoods/ChicagoShapesInR/Neighborhoods_2012b.shp")
plot(chicagoShapes)
head(chicagoShapes)

chicagoData = read.csv("/Users/appobs/Desktop/hw/465/week8/ChicagoNeighborhoods/ChicagoShapesInR/CCASF12010CMAP.csv")
head(chicagoData)

# The first row contains explanations of the fields, so remove them
# also we'll only keep the first three fields for now so that
# we have one population field
chicagoData = chicagoData[-1, 1:3]
names(chicagoData)
names(chicagoData)[1] = "Neighborhood"
names(chicagoData)[2] = "Key"
names(chicagoData)[3] = "Population"

chi = merge(chicagoShapes, chicagoData, by.x="PRI_NEIGH", by.y="Neighborhood")

# Create a color palette
p = colorRampPalette(c("white", "red"))(128)

# Set the color palette for base plotting
palette(p)

# Scale the total population to the palette
pop = chi@data$Population
plot(chi, col=pop)

###############################################################################
# Now, try ggplot
###############################################################################

# First we need to convert the shapefile to a dataframe.  This
# is done with the ggplot "fortify" function.  The merge keeps the 
# neighborhood names attached, otherwise we lose them
chicagoMap = merge(fortify(chicagoShapes), as.data.frame(chicagoShapes), by.x="id", by.y=0)
head(chicagoMap)
head(chicagoData)

pop_map = merge(chicagoMap, chicagoData, by.x="PRI_NEIGH", by.y="Neighborhood")

library(plyr)
pop_map=arrange(pop_map, group, order)
head(pop_map)

ggplot(pop_map, aes(x=long, y=lat, group=group, fill=Population)) + 
  geom_polygon(color="black") + 
  scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B",
                       midpoint=median(pop_map$Population))


