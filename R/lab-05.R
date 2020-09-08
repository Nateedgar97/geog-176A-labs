
library(tidyverse)
library(sf)
library(raster)
library(factoextra)
library(getlandsat)
library(mapview)
library(osmdata)

#Question 1
bb=read_csv("data/uscities.csv") %>%
  filter(city=="Palo") %>%
  st_as_sf(coords= c("lng","lat"),crs=4326) %>%
  st_transform(5070) %>%
  st_buffer(5000) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()


mapview(bb)

#######
#Question 2
bbwgs= bb %>% st_transform(4326)

bb=st_bbox(bbwgs)


scene=lsat_scenes() %>%
  filter(min_lat<= bb$ymin, max_lat >= bb$ymax) %>%
  filter(min_lon<= bb$xmin, max_lon >= bb$xmax) %>%
  filter(as.Date(acquisitionDate) == as.Date("2016-09-26"))

write.csv(scene,file="data/palo-flood.csv")


###

meta= read_csv("data/palo-flood.csv")

files= lsat_scene_files(meta$download_url) %>%
  filter(grepl(paste0("B", 1:6, ".TIF$", collapse= "|"), file)) %>%
  arrange(file) %>%
  pull(file)


lsat_image(files[1])
lsat_image(files[2])
lsat_image(files[3])

st= sapply(files,lsat_image)

s=stack(st) %>% setNames(c(paste0("band", 1:6)))


cropper= bbwgs %>%  st_transform(crs(s))

r=crop(s, cropper)

#Question 3
par(mfrow=c(1,2))
#True Color
plotRGB(r, r=4 ,g=3 ,b=2, stretch= "lin")
plotRGB(r, r=4 ,g=3 ,b=2, stretch= "hist")
#Color Infared
plotRGB(r, r=5 ,g=4 ,b=3, stretch= "lin")
plotRGB(r, r=5 ,g=4 ,b=3, stretch= "hist")
#False Color Water Focus
plotRGB(r, r=5 ,g=6 ,b=4, stretch= "lin")
plotRGB(r, r=5 ,g=6 ,b=4, stretch= "hist")
#My Choice
plotRGB(r, r=1 ,g=3 ,b=6, stretch= "lin")
plotRGB(r, r=1 ,g=3 ,b=6, stretch= "hist")

#Streching determines how the values are represented on the image.

#Question 4
palette= colorRampPalette(c("blue","white","red"))

ndvi=(r$band5 - r$band4) / (r$band5 + r$band4)
ndwi=(r$band3 - r$band5) / (r$band3 + r$band5) # (Done) modify to correct equation
mndwi=(r$band3 - r$band6) / (r$band3 + r$band6) # (Done) modify to correct equation
wri=(r$band3 + r$band4) / (r$band5 + r$band6)
swi= 1 / sqrt(r$band2 + r$band6)

plot(ndvi,col=palette(256), legend=FALSE)
#plot ndwi & mndwi

plot(wri,col=palette(256), legend=FALSE)
plot(swi,col=palette(256), legend=FALSE)


#Step 2
thresholding1= function(x){ifelse(x <= 0,1,NA)}
thresholding2= function(x){ifelse(x >= 0,1,NA)} # (Done) change sign!
thresholding3= function(x){ifelse(x >= 0,1,NA)} # (Done) change sign!
thresholding4= function(x){ifelse(x >= 1,1,NA)}
thresholding5= function(x){ifelse(x <= 5,1,NA)}

flood1= calc(ndvi,thresholding1)
flood2= calc(ndwi,thresholding2) # (Done) change nvdi to correct raster image
flood3= calc(mndwi,thresholding3) # (Done) change nvdi to correct raster image
flood4= calc(wri,thresholding4)
flood5= calc(swi,thresholding5)

plot(flood1, col= "blue", legend=FALSE)
plot(flood2, col= "blue", legend=FALSE)
plot(flood3, col= "blue", legend=FALSE)
plot(flood4, col= "blue", legend=FALSE)
plot(flood5, col= "blue", legend=FALSE) # fix blue square
mapview(flood1) #wth is this?

#Question 5
  #1. Split raster data/structure into just data
  #2. Calculate KMeans clustering on data
  #3. Add clustered data to original raster structure
set.seed(1)

#1. Splited Raster and get data to cluster
r_extract = getValues(r)
r_nna = na.omit(r_extract) #No NAs, no need to omit
#data was extracted band by band (there are 6 columns)

#2. Calcualting the KMeans Cluster
kmeans_r = kmeans(r_extract, 4)
fviz_cluster(kmeans_r, geom="point", data = r_extract)

#3. Applying the cluster to raster structure
kmeans_raster = flood5
values(kmeans_raster) = kmeans_r$cluster
plot(kmeans_raster, col = viridis::plasma(4))


#Step 5.3
com_table = table(flood4, kmeans_r)

data.frame %>%
  mutate %>%
  select %>%
  st_as_sf %>%


p1 = st_point(c(1,2))
class(p1)
st_bbox(p1)

