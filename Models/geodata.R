##################
# geo data processing
##################

setwd('~/Documents/ComplexSystems/RobustnessDiscrepancy')

library(rgdal)
library(raster)

# communes
com <- readOGR('Data/communes-20150101-100m-shp','communes-20150101-100m')

# get Paris
cps = as.integer(as.vector(com@data$insee))
cps[is.na(cps)]=0
parisIndexes <- which(cps<76000&cps>=75000)
cps[parisIndexes]
#check names
com@data$nom[parisIndexes]
#test plot
plot(SpatialPolygons(com@polygons[parisIndexes]))

paris<- SpatialPolygons(com@polygons[parisIndexes])


## extract osm data




