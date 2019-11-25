
# Load the "Geospatial Abstraction Library"
# which has powerful functions for loading shapefiles
# shapefiles are far more powerful polygon files that can hold
# a great deal of geo-spatial information about a region including
# some data
require(rgdal)

# Now we load the file with the more powerful
# readOGR
sport = readOGR(dsn = ".", "london_sport")

# Print out the fields
names(sport)

# Get some summary information about the file
summary(sport)

# Check the projection, by default it is a mercator
# projection with units in meters.
proj4string(sport)

# Unfortunately some tutorials say that the projection for this file
# was incorrectly tagged. But the "spatial reference system" detailied
# by the proj4string function above is exactly the one that corresponds
# to the spatial reference code for the "British National Grid" (27700)  
# a standard projection for great britain.  For lists of standard spatial 
# reference grids see "http://spatialreference.org".  The following is 
# a shorter way to specify the same result if you want.
proj4string(sport) = CRS("+init=epsg:27700")

require(ggplot2)

# Sport is not a data frame but a more complicated structure.  It contains
# a data frame, though named "Data" as one of its components
# We can get at it with the "@" operator much like the "$" operator 
# for data frames, but it accesses "attributes" from a shapfile
head(sport@data)

# Get a scatter plot of some of the data to get a 
# first view.  "Partic_per" is the sports participation per 100 people
# and Pop_2001 is fairly self-explanatory.
p = ggplot(sport@data, aes(Partic_Per, Pop_2001))
p + geom_point()

# Now, let's plot the shapes in the file
plot(sport)

# Now, let's incorporate some of the data.  Let's plot the zones that 
# have high participation (> 25)
plot(sport)
plot(sport[sport$Partic_Per > 25, ], col="blue", add=TRUE)

# To get this dataset to work with ggplot, we need to extract the polygon 
# data as a dataframe, and add in a "region" field.  The maptools "fortify" 
# method will do this for us but it will strip out a little too much and we 
# have to "merge" that back in
#
# Unfortunately, there are some older examples that use a funciton in "maptools"
# it will not work, so you need "rgeos" installed without maptools.
detach("package:maptools", unload=TRUE)
require(rgeos)

# Here we pull out the geometric data and tag it with a region label.  Note
# That we are just telling it to use the "ons_label" field from the dataset.
# The way "fortify" works, this just becomes an "id" field in the resulting 
# dataset.  When this is done, we have a polygon set much like the simple examples
# from before with records that are single (lon, lat) pairs
sport.f = fortify(sport, region = "ons_label") 
head(sport.f)

# Unfortunately, this loses the connection with the data in the shape file
# So we have to add it back in.  Notice that we link the two sets (the shapes
# with their "ID" field and the original "ons_label" field from the data)
sport.f = merge(sport.f, sport@data, by.x = "id", by.y = "ons_label")

# Note that EACH of the shape points now has the Partic_Per and Pop_2001 fields 
head(sport.f)

Map = ggplot(sport.f, aes(long, lat, group = group, fill = Partic_Per)) + geom_polygon() +
  coord_equal() + labs(x = "Easting (m)", y = "Northing (m)", fill = "% Sport Partic.") +
  ggtitle("London Sports Participation")

# We can use a simple gradient fill
Map + scale_fill_gradient(low = "lightblue", high = "red")

# Or we can use one of the color brewer palettes (default is "blues")
Map + scale_color_brewer()

# Now, let's try to put this data over a base map to give ourselves more context
require(ggmap)

# First, we change the coordinates in the polygons to latitude/longitude instead
# the great britain projection, and then we rebuild our data
sport.wgs84 <- spTransform(sport, CRS("+init=epsg:4326"))
sport.wgs84.f = fortify(sport.wgs84, region = "ons_label") 
head(sport.wgs84.f)
sport.wgs84.f = merge(sport.wgs84.f, sport@data, by.x = "id", by.y = "ons_label")

# Now, get the bounding box
b = bbox(sport.wgs84)

# scale longitude and latitude (increase bb by 5% for plot) replace 1.05
# with 1.xx for an xx% increase in the plot size
b[1, ] = (b[1, ] - mean(b[1, ])) * 1.05 + mean(b[1, ])
b[2, ] = (b[2, ] - mean(b[2, ])) * 1.05 + mean(b[2, ])

# We can download a google map for that location direct from google in a form
# That is immediately plottable in a ggplot window
lnd.b1 = ggmap(get_map(location = b))
lnd.b1 + geom_polygon(data = sport.wgs84.f, aes(x = long, y = lat, group = group, 
              fill = Partic_Per), alpha = 0.5) + scale_fill_gradient(low = "lightblue", high = "red")

# There is also another type of map served by google maps, created by a firm named
# "statmen" which can be better for statistical plots.  They are black & white 
# keeping the base color of the map from interfering with the map gradient
lnd.b2 <- ggmap(get_map(location = b, source = "stamen", maptype = "toner", crop = T))
lnd.b2 + geom_polygon(data = sport.wgs84.f, aes(x = long, y = lat, group = group, 
              fill = Partic_Per), alpha = 0.5) + scale_fill_gradient(low = "lightblue", high = "red")

# We can also decrease the size of detail marks & labels on the map with the 
# "zoom" setting
lnd.b3 <- ggmap(get_map(location = b, source = "stamen", maptype = "toner", 
                        crop = T, zoom = 11))
lnd.b3 + geom_polygon(data = sport.wgs84.f, aes(x = long, y = lat, group = group, 
              fill = Partic_Per), alpha = 0.5) + scale_fill_gradient(low = "lightblue", high = "red")


# Next, the question is, suppose we have some data about these boroughs in another
# table.  Can we merge the two.  The main problem we will have here is mismatches in 
# the names of geometric features.

crimeDat = read.csv("mps-recordedcrime-borough.csv")  
head(crimeDat)

summary(crimeDat$CrimeType)
crimeTheft = crimeDat[which(crimeDat$CrimeType == "Theft & Handling"), ]
head(crimeTheft)

crimeAg = aggregate(CrimeCount ~ Borough, FUN = "sum", data = crimeTheft)
head(crimeAg, 2)

library(rgdal)
lnd = readOGR(dsn = ".", "london_sport")
lnd = spTransform(lnd, CRS("+init=epsg:4326"))

# We try to find out which of the london borough names are found in the crime 
# dataset.  The easiest way is to use the R "%in%" operator.  This operator compares
# two arrays and returns true or false for each element of the first array depending
# on whether that element is found somewhere in the second array
lnd$name %in% crimeAg$Borough

# The following will ask which are the actual names.  On the inside we ask which names
# are NOT found in the cirme set (result of the %in% operator is false!) then we use
# those indices to look up the names
lnd$name[which(!lnd$name %in% crimeAg$Borough)]

# This tells us that "City of London" doesn't match.  So, what can we do?  Let's check 
# the values of Borough in the crime set. We can do this with the "Levels" function
# Element #25 is quite strange ... "NULL" ... that's the one
levels(crimeAg$Borough)

# Note that the indices don't match, so either we can set that level manually in the 
# crime set with
levels(crimeAg$Borough)[25] = "City of London"

# Or we can use the results of our "which" statement above to get the name from the
# names list.  The only issue with this is that it will work for only one missing value
# If you have more than one, you will need to somehow match them up.  Luckily here there 
# was only one.  If you have more than one, you'll need to get some way to match up the
# levels.  Hopefully the names are close.
lnd$name %in% crimeAg$Borough

# Now we do as before, but instead of merging the Sport data from the shapefile, we will 
# merge the crime data.  Note that this time we use the Borough's name as the region
# rather than the "id".
lndCrime = fortify(lnd, region = "name")
head(lndCrime)

# Now, like before, we merge on the id and the Borough fields
lndCrime = merge(lndCrime, crimeAg, by.x = "id", by.y = "Borough")
head(lndCrime)

# Now, we draw as before
b = bbox(sport.wgs84)
b[1, ] = (b[1, ] - mean(b[1, ])) * 1.05 + mean(b[1, ])
b[2, ] = (b[2, ] - mean(b[2, ])) * 1.05 + mean(b[2, ])

# We can download a google map for that location direct from google in a form
# That is immediately plottable in a ggplot window.  Note the "trans" argument to make the
# color scale logarithmic
lnd.b1 = ggmap(get_map(location = b))
lnd.b1 = ggmap(get_map(location = b, source = "stamen", maptype = "toner", crop = T))
lnd.b1 + geom_polygon(data = lndCrime, aes(x = long, y = lat, group = group, 
                    fill = CrimeCount), alpha = .7) + scale_fill_gradient(low = "green", high = "red", trans="log")

# Playing around with a diverging scale
require(scales)
lim = range(log(lndCrime$CrimeCount))
lnd.b1 + geom_polygon(data = lndCrime, aes(x = long, y = lat, group = group, 
                    fill = log(CrimeCount)), alpha = .7) + 
                    scale_fill_gradient2(limits=lim, midpoint=(lim[1] + lim[2]) / 2,
                                         low=muted("green"), mid="white", high=muted("red"))
