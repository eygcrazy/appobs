library(maps)
library(mapdata)
library(ggplot2)
library(mapproj)
library(plyr)
# Get some information about the maps package
help(package=maps)

# ggplot has a map_data function to load map data into a format it understands
# the maps are maps::county(), maps::france(), maps::italy(), maps::nz(), maps::state(), 
# maps::usa(), maps::world(), maps::world2().
states_map = map_data("state")


ggplot(states_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill="white", color="black")

# We can change the map projection if we wish.  Mercator is one
# option, but you can see the full list at 
#
#   https://cran.r-project.org/web/packages/mapproj/mapproj.pdf
ggplot(states_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("mercator")

ggplot(states_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("sinusoidal")

ggplot(states_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("gall", 0)

ggplot(states_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("polyconic")

######################################################
# Map of the world
######################################################

world_map = map_data("world")

# Defaults to an equirectangular projection
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_path()

# For some reason the world map likes to be given a  
# -reference latitude of -90 here
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("rectangular", -90)

# Some polygons bleed over with the mercator projection
# if manually applied
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("mercator")

# Polygons bleed over as well with sinusoidal.  This is due
# to some polygons on alaska not being subdivided over
# the international date line.
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("sinusoidal")

ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("gall", 0)

ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("polyconic")

#############################################################
# The world2 map can work a bit better, but it has an odd 
# center over the pacific ocean
#############################################################

world_map = map_data("world2")

ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_path()

ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("rectangular", 0)

ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("mercator")

ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("sinusoidal")

ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("gall", 0)

ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_path() + coord_map("polyconic")

# If we want to center on a different location, it is a little tricky because 
# the polygons will tear if we don't do something to stop it

# This first command uses a conversion function from ggplot that gives the 
# map extra structure so that ggplot can manipulate it.
mp1 = fortify(map(fill=TRUE, plot=FALSE))

# Now, we make a copy so we can shift the longitude by 360 degrees
# essentially making another copy of the earth out to the right on the
# map, connecting up the area where there will be a tear.  Then we 
# creat a new set of group numbers for the polygons (they will still have
# the same names for connecting to data!)
mp2 = mp1
mp2$long = mp2$long + 360
mp2$group = mp2$group + max(mp2$group) + 1

# Bind these two maps together by concatenating rows
mp = rbind(mp1, mp2)

# Now we can draw it with any limits we want. 
ggplot(aes(x = long, y = lat, group = group), data = mp) + 
  geom_path() + 
  scale_x_continuous(limits = c(-180, 180))


#############################################################
# There is another world map we can use, either with basic 
# "map" services from base R or from ggplot.  It has higher
# resolution on the boundaries ofthe countries and takes a
# significantly longer amount of time to load and draw
#############################################################

map("worldHires", xlim=c(-180, 180), ylim=c(-80, 80), col="gray90", fill=T, projection="mollweide")
map("worldHires", xlim=c(-180, 180), ylim=c(-55, 55), col="gray90", fill=T, projection="mercator")

world_map = map_data("worldHires")
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill="white", color="black") + coord_map("mollweide")

############################################################
# Now, let's map some data onto a map.  The data 
# for epidemic outbreaks will be mapped onto the world map
# and we will use ggplot to plot them
#############################################################################

# Notice that we have a location field that mostly has country names
ds = read.table("EpidemicOutbreaks.csv", header=T, sep=",")
head(ds)

# The "cases" doesn't come in as a number for some reason. Instead it comes
# in as a categorical factor, so we convert it
ds$Cases = as.numeric(ds$Cases)

# The dataset must have a group field for ggplot's mapping.  For the moment 
# it can be set to a constant of 1 because we are not mapping to color on the 
# polygons
ds$group=1   

# Now, we can map it.  Notice that we have to have two sets of Aesthetics here
# one for the base world map and one for the points.  
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill="white", color="black") + 
  geom_point(data=ds, aes(x=Long, y=Lat, color=Cases))

# Or we can change the color map
ggplot(world_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill="white", color="black") + 
  geom_point(data=ds, aes(x=Long, y=Lat, color=ds$Cases)) + 
  scale_color_gradient(high="red", low="green")
  

#####################################################################
## Basic Choropleth
#####################################################################

states_map = map_data("state")
head(states_map)

# First we get a view of the map.  Note that the states names are in 
# lower case!
ggplot(states_map, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill="white", color="black")

# The USArrests dataset has row names that are the states, but for the 
# map, we need to have a field with those names.  Moreover, the state
# names must be in lowercase, so we have to convert 
head(rownames(USArrests))  # Note, first letter is upper case
crimes = data.frame(state=tolower(rownames(USArrests)), USArrests)
head(crimes)

# Now, we have to merge the two datasets by joining on the two common
# fields.  They are not named the same, but that is not an issue.  In 
# the "states_map" the state field is called "region" and in our 
# crime dataset, it is called "state"
crime_map = merge(states_map, crimes, by.x="region", by.y="state")

# Notice that the "group" holds an ID of each state now, but the points
# for the first state of alabama are no longer in the same order!
head(crime_map)
head(states_map)

# We need to resort these for drawing since the polygon points have been resorted
# here by the merge command, unfortunately

crime_map=arrange(crime_map, group, order)
head(crime_map)

# Now, let's draw it.
ggplot(crime_map, aes(x=long, y=lat, group=group, fill=Assault)) + 
  geom_polygon(color="black") + scale_fill_gradient(low="lightgreen", high="pink")

ggplot(crime_map, aes(x=long, y=lat, group=group, fill=Assault)) + 
  geom_polygon(color="black") + coord_map("polyconic") + 
  scale_fill_gradient(low="lightgreen", high="pink")

ggplot(crime_map, aes(x=long, y=lat, group=group, fill=Assault)) + 
  geom_polygon(color="black") + coord_map("mercator") + 
  scale_fill_gradient(low="lightgreen", high="pink")

# The geom_map geometry type is a wrapper for the geom_polygon
# type, but which is optimized for maps.  This can be a little 
# more convenient because you don't have to have two sets of aesthetics!
ggplot(crimes, aes(map_id=state, fill=Assault)) + 
  geom_map(map=states_map, color="black") + 
  scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B",
                       midpoint=median(crimes$Assault)) + 
  expand_limits(x=states_map$long, y=states_map$lat) +
  coord_map("polyconic")
  

