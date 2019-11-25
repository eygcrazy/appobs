require(grDevices) # for colours
filled.contour(volcano, color = terrain.colors, asp = 1) # simple

x = 10*1:nrow(volcano)
y = 10*1:ncol(volcano)
par(mar=c(3, 3, 3, 3))
filled.contour(x, y, volcano, color = terrain.colors,
               plot.title = title(main = "The Topography of Maunga Whau",
                                  xlab = "Meters North", ylab = "Meters West"),
               plot.axes = { axis(1, seq(100, 800, by = 100))
                 axis(2, seq(100, 600, by = 100)) },
               key.title = title(main = "Height\n(meters)"),
               key.axes = axis(4, seq(90, 190, by = 10)))  # maybe also asp = 1

contour(x, y, volcano, add=T)
par(mar=c(3, 3, 3, 3))

mtext(paste("filled.contour(.) from", R.version.string),
      side = 1, line = 4, adj = 1, cex = .66)

########################################################
# A nicer function for this from the spaMM library
########################################################

library(spaMM)
spaMM.filled.contour(x, y, volcano, color = terrain.colors,
                     plot.title = title(main = "The Topography of Maunga Whau",
                                        xlab = "Meters North", ylab = "Meters West"),
                     plot.axes = { axis(1, seq(100, 800, by = 100))
                       axis(2, seq(100, 600, by = 100)) },
                     key.title = title(main = "Height\n(meters)"),
                     key.axes = axis(4, seq(90, 190, by = 10)))  # maybe also asp = 1
mtext(paste("spaMM.filled.contour(.) from", R.version.string),
      side = 1, line = 4, adj = 1, cex = .66)

################################################
# Since these can be a little frustrating for 
# overlaying lines (aspect ratio problems) some
# have provided the filled.contour2 function 
# found in filled.contour2.r in this folder
################################################

filled.contour3(x, y, volcano, color = terrain.colors,
               plot.title = title(main = "The Topography of Maunga Whau",
                                  xlab = "Meters North", ylab = "Meters West"),
               plot.axes = { axis(1, seq(100, 800, by = 100))
                 axis(2, seq(100, 600, by = 100)) },
               key.title = title(main = "Height\n(meters)"),
               key.axes = axis(4, seq(90, 190, by = 10)))  # maybe also asp = 1

