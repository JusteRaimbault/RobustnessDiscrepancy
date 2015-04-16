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

# needs to reorder by hand to have good numerotation
arrondissements<-arrondissements[c(11,3,17,16,12,4,13,19,20,14,15,1,2,9,7,8,18,5,6,10)]
#c(12,13,2,6,18,19,15,16,14,20,1,5,7,10,11,4,3,17,8,9)

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



#############
#############
#extract single points from roads

roads_data=c()

for(i in 1:length(roads@lines)){
  coords = roads@lines[[i]]@Lines[[1]]@coords
  for(j in 2:length(coords[,1])){
    roads_data=append(roads_data,c(coords[j,1],coords[j,2],sqrt((coords[j,1]-coords[j-1,1])^2+(coords[j,2]-coords[j-1,2])^2)))
  }
}

# construct corresponding matrix
roads_raw = matrix(data=roads_data,nrow=length(roads_data)/3,byrow=TRUE)


