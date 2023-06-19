# FOREST STRUCTURE (MLBS)

# This script follows the tutorial from the SPEC School virtual week
# It uses LiDAR data products for canopy height model (CHM) and vegetation structure to identify trees


library(sp)
library(raster)
library(neonUtilities)
library(neonOS)
library(geoNEON)


wd <- "/Users/gracemcleod/Documents/SPEC/StructureFlux"
setwd(wd)

# VEGETATION DATA...........................................................................................

# download woody vegetation data
veglist <- loadByProduct(dpID="DP1.10098.001",
                         site="MLBS", # wind river experimental forest
                         package="basic",
                         check.size= FALSE) # make sure you have enough space to download
summary(veglist) # whats in it?

# make a variable with coordinate info
vegmap <- getLocTOS(veglist$vst_mappingandtagging, # location data
                    "vst_mappingandtagging")

# new variable to join individual + geolocation data
veg <- joinTableNEON(veglist$vst_apparentindividual,
                     vegmap,
                     name1 = "vst_apparentindividual",
                     name2 = "vst_mappingandtagging")

# plot tree locations with stem diameter
symbols(veg$adjEasting[which(veg$plotID=="MLBS_074")],
        veg$adjNorthing[which(veg$plotID=="MLBS_074")], # easting and northing are in meters
        circles=veg$stemDiameter[which(veg$plotID=="MLBS_074")]/100/2, # convert stem diameter from centimeters to meters and calculate radius
        inches=F, xlab="Easting", ylab="Northing")

# add uncertainty bubbles around stems (location margin of error)
symbols(veg$adjEasting[which(veg$plotID=="MLBS_074")],
        veg$adjNorthing[which(veg$plotID=="MLBS_074")], 
        circles=veg$adjCoordinateUncertainty[which(veg$plotID == "MLBS_074")],
        inches=F, add=T, fg="lightblue")




# AOP DATA...........................................................................................
# read in and plot Canopy Height Model data

# download canopy height model data
byTileAOP(dpID="DP3.30015.001", # can download by tile or flightline
          site="MLBS",
          year="2017",
          easting=veg$adjEasting[which(veg$plotID=="MLBS_074")], # use same as diameter data
          northing=veg$adjNorthing[which(veg$plotID=="MLBS_074")],
          savepath="/Users/gracemcleod/Documents/SPEC/StructureFlux/Data")

# read in the tile
# burried way deep in the dowloaded folder
chm <- raster(paste0("/Users/gracemcleod/Documents/SPEC/StructureFlux/Data/
                     DP3.30015.001/neon-aop-products/2017/FullSite/D07/2017_MLBS_2/L3/
                     DiscreteLidar/CanopyHeightModelGtif/
                     NEON_D07_MLBS_DP3_542000_4136000_CHM.tif"))
              
plot(chm, col=topo.colors(5))



# COMPARE THE TOS AND AOP DATASETS.......................................................................................
extent(chm) 
# subset vegetation by the extent of the CHM tile (xmin, xmax, ymin, ymax)
vegsub <-veg[which(veg$adjEasting >= extent(chm) [1] &
                     veg$adjEasting <= extent(chm) [2] &
                     veg$adjNorthing >= extent(chm) [3] &   
                     veg$adjNorthing <= extent(chm) [4]),]

bufferCHM <- extract(chm, 
                     cbind(vegsub$adjEasting,
                           vegsub$adjNorthing),
                     buffer=veg$adjCoordinateUncertainty,
                     fun=max)

# plot against eachother
plot(bufferCHM~vegsub$height, pch=20, xlab="Height", 
     ylab="Canopy height model")
lines(c(0,50), c(0, 50), col="blue")

# look at correlation
cor(bufferCHM, vegsub$height, use="complete") # 0.352613



# FILTER OUT UNDERSTORY .......................................................................................

# Approach A








