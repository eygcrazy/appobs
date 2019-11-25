
# This lists all files that end in ".cis".  The \\ is a special sequence
# that indicates that the "." is to be taken as a literal character, i.e. 
# the extension should start with a period.  The $ at the end says that this
# should be the end of the name.  
# 
# See: http://stackoverflow.com/questions/4876813/using-r-to-list-all-files-with-a-specified-extension
# for more information
files = list.files(pattern = "\\.CIS$")

widths = rep(2, 1034/2 - 1)
widths[1] = 3

# If we count the number of files we have and then take the sqrt, we can find out what size grid will hold 
# all of the graphs.  The only thing we need to make sure to do is to push the size to the next largest integer
# this is what the "ceiling" function does.
nFiles = length(files)
nRows = ceiling(sqrt(nFiles))

# Unfortunately, filled.contour doesn't play nice with the default grid facilities
# of "par", so we need to use some hacks by other people.  These are included in 
# this folder for your convenience.
source("Filled.contour3.R")
source("Filled.legend.R")

par(mfrow=c(nRows, nRows), oma = c(.3,.3,.3,.3) + 0.1, mar = c(1.2,1.2,1.2,1.2) + 0.1)
plot.new()
for (file in files)
{
  data = as.matrix(read.fwf(file, widths=widths))
  data = t(data)
  d = dim(data)
  data = data[, seq(d[2], 1)]
  
  x = unique(sort(unique(as.matrix(read.table("longrid.txt")))))
  y = unique(sort(unique(as.matrix(read.table("latgrid.txt")))))
  x = seq(0, 1, length.out = d[1])
  y = seq(0, 1, length.out = d[2])
  
  filled.contour3(x, y, data, color=terrain.colors, 
                  xlab = "", ylab = "", 
                  xlim = range(x), ylim = range(y), zlim = range(data))
}

filled.contour(x, y, data)
contour(x, y, data)  


