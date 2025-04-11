library(tidyverse)
library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(sf)
library(mapview)

# remember to change the directory for the file with the camera co-ordinates (line 12)

camera <- read_excel("C:/Users/smain/Desktop/NewCameraLocations.xlsx")

cols = c("red","blue","green","orange","black","yellow","pink","purple")
#cols = palette.colors(n = 8)

# either version of "cols" could be used - the second version chooses eight 
# random colours but I prefer the first version as it offers eight very distinct 
# colours that are easy to spot on a map

map <- mapview(camera, xcol = "y-val", ycol = "x-val", zcol = "Camera",
               crs = 4269, grid = FALSE, col.regions = cols)
map