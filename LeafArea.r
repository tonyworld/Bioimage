## Copyright Tong Wei, July 2012
## Graphical application of R/Bioconductor EBImage

## Automated leaf area measuring, with adjustable parameter of background 
## intensity (0.8 herein)


library(EBImage)

# read the image
setwd("D:/working/")
pic <- readImage("sample.jpg")

# extract the 'blue' channel
pic.data <- imageData(channel(pic, mode = "blue"))
pic.data <- 1 - pic.data  # reverse the image 
hist(pic.data)

# noticed that the background is around 0.4-0.55, while the leaf is about 0.9-1
pic.data[pic.data < .8] <- 0
pic.data[pic.data >= .8] <- 1
pic.lab <- bwlabel(pic.data)  # attempt to enclose the holes
pic.lab <- fillHull(pic.lab)  
kern <- makeBrush(3, shape = "disc", step = F)
pic.lab <- erode(pic.lab, kern)  # remove only a few noisy pixels

# calculate the area ratio
pic.shape <- computeFeatures.shape(pic.lab)
leaf.area <- pic.shape[, "s.area"]
area.ratio <- leaf.area[1] / leaf.area[2]
cat("The ratio of leaf areas is ", round(area.ratio, digits = 3), ".\n", 
    sep = "")

# layout the resulting image
pic.out <- paintObjects(pic.lab, pic, opac = c(NA, 0.45), col = c(NA, "red"))
xy <- computeFeatures.moment(pic.lab, pic)[, c("m.cx", "m.cy")]
font <- drawfont(weight = 600, size = 35)
pic.out <- drawtext(pic.out, xy = xy, 
                    labels = as.character(leaf.area), 
                    font = font, col = "white")
writeImage(pic.out, "sample.out.jpg")