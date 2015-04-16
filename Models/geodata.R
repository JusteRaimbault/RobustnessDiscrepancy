##################
# geo data processing
##################

setwd('~/Documents/ComplexSystems/RobustnessDiscrepancy')

library(rgdal)
library(raster)
library(rgeos)

# communes
com <- readOGR('Data/raw/communes','communes-20150101-100m')

# get Paris
cps = as.integer(as.vector(com@data$insee))
cps[is.na(cps)]=0
parisIndexes <- which(cps<76000&cps>=75000)
cps[parisIndexes]
#check names
com@data$nom[parisIndexes]
#test plot
plot(SpatialPolygons(com@polygons[parisIndexes]))

arrondissements<- SpatialPolygons(com@polygons[parisIndexes])

#writeOGR(SpatialPolygonsDataFrame(data=arrondissements),'Data/processed','arrondissements','ESRI Shapefile')

###################
## extract osm data
###################

#roads
lines <- readOGR('Data/raw/paris','paris_france.osm-line')
roadsIndexes = which(!is.na(lines@data$highway))
# better to use not na                
#%in% c("primary","secondary","tertiary",
                    #    "road","unclassified","motorway","living_street","pedestrian","primary_link",
                    #    "secondary_link","tertiary_link" ))

roads <- SpatialLines(lines@lines[roadsIndexes])

# test plot
plot(roads)
plot(paris,col="red",add=TRUE)

# get paris roads
paris_roads = crop(roads,extent(arrondissements))
plot(paris_roads)

writeOGR(paris_roads,'Data/processed','roads','ESRI Shapefile')

#buildings
pols <- readOGR('Data/raw/paris','paris_france.osm-line')

# TOO HEAVY FOR R -> use Qgis


#############
#############


buildings <- readOGR('Data/processed','buildings_centroids')

roads <- readOGR('Data/processed','roads')


###########

#test crop
arr <- crop(buildings,extent(SpatialPolygons(arrondissements@polygons[1])))
plot(arr)

#use overlay
arr<- over(SpatialPoints(buildings),arrondissements)

plot(buildings@coords[!is.na(arr),])


