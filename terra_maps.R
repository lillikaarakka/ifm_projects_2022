###IMF Forest C Maps with Terra
setwd("/Volumes/Julie's TB/ForestC")

###Packages-------------------------------------------------------------------------------------------
library(terra)
library(raster)
library(ggplot2)


###Files----------------------------------------------------------------------------------------------
contigUS_projects <-read.csv("/Volumes/Julie's TB/ForestC/contigUS_site_locations.csv")
AK_projects <- read.csv("/Volumes/Julie's TB/ForestC/AK_project_locations.csv")
us <- vect("/Volumes/Julie's TB/ForestC/cb_2018_us_state_20m/cb_2018_us_state_20m.shp", crs="+proj=longlat +datum=WGS84")
pl <-rast("/Volumes/Julie's TB/ForestC/Cao Paper Maps/Fig5a.tif")
sc <- rast("/Volumes/Julie's TB/ForestC/Cao Paper Maps/Fig6a.tif")
whp_conus <- rast("WildfireHazardPotential/Data/whp2020_GeoTIF/whp2020_cls_conus.tif")
whp_AK <-rast("WildfireHazardPotential/Data/whp2020_GeoTIF/whp2020_cls_ak.tif")
whp_HI <-rast("WildfireHazardPotential/Data/whp2020_GeoTIF/whp2020_cls_hi.tif")

###project points prep--------------------------------------------------------------------------------
#albers equal area projection
aea <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

#CONUS
lonlat <-cbind(contigUS_projects$locations.long, contigUS_projects$locations.lat) #create coordinate dataframe
pts <-vect(lonlat, crs="+proj=longlat +datum=WGS84") #create spatVect
p <- terra::project(pts, aea) #convert to albers equal area

#AK
AK_lonlat <-cbind(AK_projects$locations.long, AK_projects$locations.lat)
AK_pts <-vect(AK_lonlat, crs="+proj=longlat +datum=WGS84")
AK_p <- terra::project(AK_pts, whp_AK)
AK_p


###state boundaries prep---------------------------------------------------------------------------------
#conus
e <- ext(-130,-60,20,50) #conus extent
conus <- crop(us, e)
c <- terra::project(conus, aea)

#alaska
AK_e <- ext(-180,-135, 50, 80) #AK extent
AK <- crop(us, AK_e)
a <- terra::project(AK, whp_AK)



#Hawaii
HI_e <- ext(-162, -154, 18, 24) #HI extent
HI <- crop(us, HI_e)
h<- terra::project(HI, whp_HI)


###litter carbon------------------------------------------------------------------------------------------
soil_colors <-c("#ffc180", "#cc6600", "#994d00", "#4d2600", "#000000")
png(file = "litter_terra.png", width = 8, height = 5, units = "in", res = 350)
plot(pl, col=soil_colors, type="interval", main="Litter carbon and project locations", axes=FALSE, plg=list(title="Litter Carbon (Mg/ha)"))
plot(c, lwd=.5, add=T)
points(p, cex=.75)
dev.off()

###soil carbon--------------------------------------------------------------------------------------------
soil_colors <-c("#ffc180", "#cc6600", "#994d00", "#4d2600", "#000000")

png(file = "soil_terra.png", width = 8, height = 5, units = "in", res = 350)
plot(sc, col=soil_colors, type="interval", main="Soil Organic Carbon and Project Locations", axes=FALSE, plg=list(title="Soil Organic Carbon (Mg/ha)"))
plot(c, lwd=.5, add=T)
points(p, cex=.75)
dev.off()


###wildfire hazard potential----------------------------------------------------------------------------
#colors
whp_color <- c("white", "#009933", "#addd8e","#f5ff71", "#ffad33", "#e31a1c","#969696", "#1d91c0")
#conus
coltab(whp_conus, layer=1)<-whp_color
levels(whp_conus) <- c("blank","very low", "low", "moderate", "high", "very high", "non-burnable", "water")
#ak
coltab(whp_AK, layer=1)<-whp_color
levels(whp_AK) <- c("blank","very low", "low", "moderate", "high", "very high", "non-burnable", "water")
#hi
coltab(whp_HI, layer=1)<-whp_color
levels(whp_HI) <- c("blank","very low", "low", "moderate", "high", "very high", "non-burnable", "water")

#extents for inserts
ak_in_ex <- ext(-2400000,-1400000, 400000, 1000000)
hi_in_ex <-ext(-1400000, -800000, 400000, 900000)
re_ak <- rescale(whp_AK,fx=0.3, fy=0.3, x0=-3000000, y0=750000)
re_ak_pt <- rescale(AK_p, fx=0.1, fy=0.3, x0=-3000000, y0=750000)

#conus map
png(file = "whpconus_terra.png", width = 8, height = 5, units = "in", res = 350)
plot(whp_conus, main="Wildfire Hazard Potential and Project Locations", axes=F, plg=list(title="Wilfire Hazard Potential"))
plot(c, lwd=.5, add=T)
points(p, cex=.75)
dev.off()


plot(c, lwd=.5, add=T)
points(p, cex=.75)
plot(whp_AK, axes=FALSE, legend=FALSE,add=T)
plot(a, lwd=.5, add=T)
points(AK_p, cex=.75)
dev.off()

#ak map
png(file = "whpak_terra.png", width = 8, height = 5, units = "in", bg=NA, res = 350)
ak_plot <- plot(whp_AK, axes=FALSE, legend=FALSE)
plot(a, lwd=1.5, add=T)
points(AK_p, cex=3)
dev.off()

#hi map
png(file = "whphi_terra.png", width = 8, height = 5, units = "in", bg=NA, res = 350)
plot(whp_HI, axes=FALSE, legend=FALSE)
plot(h, lwd=1.5, add=T)
dev.off()

