#AG carbon map
setwd("/Volumes/Julie's TB/ForestC")

##Packages------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(sp)
library(sf)

##Files---------------------------------------------------------------------------------
SE_AGcarbon <- read.csv("/Volumes/Julie's TB/ForestC/SE_AGcarbon.csv")
NE_AGcarbon <- read.csv("/Volumes/Julie's TB/ForestC/NE_AGcarbon.csv")
Mas <-read.csv("AG Carbon/tbl_coords_agcarb_MA.csv", sep=";")
MW_AGcarbon <- read.csv("/Volumes/Julie's TB/ForestC/MW_AGcarbon.csv")
SW_AGcarbon <- read.csv("/Volumes/Julie's TB/ForestC/SW_AGcarbon.csv")
RM_AGcarbon <- read.csv("/Volumes/Julie's TB/ForestC/RM_AGcarbon.csv")
WA <- read.csv("AG Carbon/tbl_coords_agcarb_WA.csv", sep=";")
OR <- read.csv("AG Carbon/tbl_coords_agcarb_OR.csv", sep=";")
CA <- read.csv("AG Carbon/tbl_coords_agcarb_CA.csv")
US <- st_read("/Volumes/Julie's TB/ForestC/cb_2018_us_state_20m/cb_2018_us_state_20m.shp")
contigUS_projects <-read.csv("/Volumes/Julie's TB/ForestC/contigUS_site_locations.csv")
AK <-read.csv("AG Carbon/tbl_coords_agcarb_AK.csv", sep=";")
AK_projects <- read.csv("/Volumes/Julie's TB/ForestC/AK_project_locations.csv")
HI <-read.csv("AG Carbon/tbl_coords_agcarb_HI.csv", sep=";")

#Project locations------------------------------------------------------------------------------
contigUS_coordinates <- data.frame(contigUS_projects$locations.long, contigUS_projects$locations.lat)
plot_contigUS_cords <- st_as_sf(contigUS_coordinates, coords = c("contigUS_projects.locations.long", "contigUS_projects.locations.lat"))
#set crs and convert to match map
st_crs(plot_contigUS_cords) <- 4326
crs(plot_contigUS_cords)
pts = st_transform(plot_contigUS_cords,CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

#create alaska points
AK_lonlat <-data.frame(AK_projects$locations.long, AK_projects$locations.lat)
str(AK_lonlat)
AK_pts <- st_as_sf(AK_lonlat, coords=c("AK_projects.locations.long","AK_projects.locations.lat"))
st_crs(AK_pts)<-4326
AK_p <- st_transform(AK_pts,CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


##Crop and convert base map to albers-------------------------------------------------------------
#just Alaska
Alaska <- st_crop(US, xmin=-180, xmax=-120,
                  ymin=50, ymax = 75)
#convert to albers
st_crs(Alaska) <- 4326
Alaska <- st_transform(Alaska, CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

#Just Hawaii
Hawaii <- st_crop (US, xmin = -161, xmax=-154,
                   ymin=18, ymax=23)
#convert to albers
st_crs(Hawaii) <- 4326
Hawaii <- st_transform(Hawaii, CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

#convert to US data to albers
st_crs(US) <- 4326
US <- st_transform(US, CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
#just contiguous us
US_contig <- st_crop(US, xmin=-2355620, xmax=2257390,
                     ymin=272090, ymax=3172092)


##Convert to sf objects-----------------------------------------------------------------------------
#SE
SE_AGcarbon <- na.omit(SE_AGcarbon)
sf_carb_SE <- st_as_sf(SE_AGcarbon, coords = c("LON","LAT"))
st_crs(sf_carb_SE) <- 4326
sf_carb_SE = st_transform(sf_carb_SE,CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


#NE
NE_AGcarbon <- na.omit(NE_AGcarbon)
sf_carb_NE <- st_as_sf(NE_AGcarbon, coords = c("LON","LAT"))
st_crs(sf_carb_NE) <- 4326
sf_carb_NE = st_transform(sf_carb_NE,CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

#Massachusets
mass <- na.omit(Mas)
sf_carb_mass <- st_as_sf(mass, coords = c("LON","LAT"))
st_crs(sf_carb_mass) <- 4326
sf_carb_mass = st_transform(sf_carb_mass,CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


##Mid West region
MW_AGcarbon <- na.omit(MW_AGcarbon)
sf_carb_MW <- st_as_sf(MW_AGcarbon, coords = c("LON","LAT"))
st_crs(sf_carb_MW) <- 4326
sf_carb_MW = st_transform(sf_carb_MW,CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


##South West
SW_AGcarbon <- na.omit(SW_AGcarbon)
sf_carb_SW <- st_as_sf(SW_AGcarbon, coords = c("LON","LAT"))
st_crs(sf_carb_SW) <- 4326
sf_carb_SW = st_transform(sf_carb_SW,CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


##Rocky Mountain region
RM_AGcarbon <- na.omit(RM_AGcarbon)
sf_carb_RM <- st_as_sf(RM_AGcarbon, coords = c("LON","LAT"))
st_crs(sf_carb_RM) <- 4326
sf_carb_RM = st_transform(sf_carb_RM,CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))


##Pacific
#Make all three state data frames match
WA<-data.frame(WA$LAT, WA$LON, WA$AB.Carbon.Total)
colnames(WA)<-c("LAT","LON","AB.Carbon.Total")
OR<-data.frame(OR$LAT, OR$LON, OR$AB.Carbon.Total)
colnames(OR)<-c("LAT","LON","AB.Carbon.Total")
CA<-data.frame(CA$LAT, CA$LON, CA$AB.Carbon.Total)
colnames(CA)<-c("LAT","LON","AB.Carbon.Total")

#combine and convert to sf
PA_AGcarbon <- rbind(WA, OR, CA)
PA_AGcarbon <- na.omit(PA_AGcarbon)
sf_carb_PA <- st_as_sf(PA_AGcarbon, coords = c("LON","LAT"))
st_crs(sf_carb_PA) <- 4326
sf_carb_PA = st_transform(sf_carb_PA,CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

##Alaska
AK <- na.omit(AK)
sf_carb_AK <- st_as_sf(AK, coords=c("LON","LAT"))
st_crs(sf_carb_AK) <-4326
sf_carb_AK <-st_transform(sf_carb_AK,CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

##Hawai
HI <- na.omit(HI)
sf_carb_HI <- st_as_sf(HI, coords=c("LON","LAT"))
st_crs(sf_carb_HI) <-4326
sf_carb_HI <-st_transform(sf_carb_HI,CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

##Map-------------------------------------------------------------------------------------------------
base<-ggplot() +
  coord_sf(crs="+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs") +
  theme_void() +
  ggtitle("Above Ground Carbon and Project Locations")+
  scale_fill_viridis_c(aesthetics = "color", name = "Above Ground Carbon \n(tons per acre)", direction=-1)
base
SE <- base +
  geom_sf(data = sf_carb_SE, size = .0001, shape = 16, aes(color= AB.Carbon.Total)) 

NE<- +
  geom_sf(data = sf_carb_NE, size = .0001, shape = 16, aes(color= AB.Carbon.Total))
mass<-NE+
  geom_sf(data = sf_carb_mass, size = .0001, shape = 16, aes(color= AB.Carbon.Total))
East<-mass+
  geom_sf(data = sf_carb_MW, size = .0001, shape = 16, aes(color= AB.Carbon.Total))

png(file = "AGCmap_east.png", width = 8, height = 5, units = "in", bg=NA, res = 350)
East
dev.off()

SW <-base+
  geom_sf(data = sf_carb_SW, size = .0001, shape = 16, aes(color= AB.Carbon.Total))
RM <- SW+
  geom_sf(data = sf_carb_RM, size = .0001, shape = 16, aes(color= AB.Carbon.Total))
PA <- RM+
  geom_sf(data = sf_carb_PA, size = .0001, shape = 16, aes(color= AB.Carbon.Total))
West<- PA+
  geom_sf(data=pts, size=1)+
  geom_sf(data=US_contig, size=.25, color="black", fill=NA)

png(file = "AGCmap_west.png", width = 8, height = 5, units = "in", bg=NA, res = 350)
West
dev.off()

#Alaska-------------------------------------------------------------------------------------------
AK_map <- ggplot()+
  geom_sf(data = sf_carb_AK, size = .0001, shape = 16, aes(color= AB.Carbon.Total))+
  geom_sf(data=AK_p, size=3.5)+
  geom_sf(data=Alaska, size=.5, color="black", fill=NA)+
  scale_fill_viridis_c(aesthetics = "color", name = "Above Ground Carbon \n(tons per acre)", direction=-1)+
  theme_void()+
  guides(color=F)

png(file = "agc_map_ak.png", width = 8, height = 5, units = "in", bg=NA, res = 350)
AK_map
dev.off()

#Hawaii-------------------------------------------------------------------------------------------
HI_map <- ggplot()+
  geom_sf(data = sf_carb_HI, size = 1, shape = 16, aes(color= AB.Carbon.Total))+
  geom_sf(data=Hawaii, size=.5, color="black", fill=NA)+
  scale_fill_viridis_c(aesthetics = "color", name = "Above Ground Carbon \n(tons per acre)", direction=-1)+
  theme_void()+
  guides(color=F)

png(file = "agc_map_ji.png", width = 8, height = 5, units = "in", bg=NA, res = 350)
HI_map
dev.off()  
  

  