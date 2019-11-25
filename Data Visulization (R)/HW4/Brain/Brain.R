

# Read to the 64th page in the middle of the file ... notice that we ignore the 
# other pages.  The original file had a .txt extension even if it was a binary
# file.  
inFile = file("bruce_July13.bin", "rb")
for (i in 1:64)
{
  d = matrix(readBin(inFile, integer(), size=2, n=256*256, endian = "big"), ncol=256, nrow=256)  
}  
close(inFile)

require(RColorBrewer)

x = seq(1, 256, 1)
y = seq(1, 256, 1)

image(x, y, d)

# Getting the color brewer to work in here is just a little tricky because the color
# brewer is not designed to work with a continuous gamut.  We can increase the sensitivity
# of the palette by using the "colorRampPalette"
image(x, y, d, col=colorRampPalette(brewer.pal(9,"Oranges"))(100))
contour(x, y, d, levels = seq(50, 200, 50), add=T)

require(spatstat)
dImg = im(d, x, y)
dBlur = as.matrix(blur(im(d, x, y), sigma = 1))

image(x, y, dBlur, col=colorRampPalette(brewer.pal(9,"Oranges"))(100))
contour(x, y, dBlur, levels = seq(50, 200, 50), add=T)

# Now we will load a 3D version and extract an isosurface

inFile = file("bruce_July13.bin", "rb")
d = readBin(inFile, integer(), size=2, n=256*256*128, endian = "big")  
head(d)
dataCube = array(data=d, dim=c(256, 256, 128))
close(inFile)

require(misc3d)

xVals = seq(1, 256, 1)
yVals = seq(1, 256, 1)
zVals = seq(1, 128, 1)
contour3d(dataCube, level=50, x=xVals, y=yVals, z=zVals)

# Cheat for a 3D gaussian blur, we do it by layers. R doesn't have a full 3D blur function,
# Need MATLAB
for (i in 1:128)
{
  dataCube[, , i] = as.array(blur(im(dataCube[, , i], x, y), sigma=2))
}
contour3d(dataCube, level=50, x=xVals, y=yVals, z=zVals)
