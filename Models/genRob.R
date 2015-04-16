
# Generalized Rob Ratio computation on Geo data
# (Paris districts)

# -> first load arrondissements, buildings and roads from geodata script



# define objective functions

# Given arrdt numbers, computes on corresponding data
# and returns both raw data and indicator value (used for weight)

# overlay for buildings 
arr_buildings=over(SpatialPoints(buildings),arrondissements)

# overlay for streets
arr_streets = over(roads,arrondissements)

#dmax
dmax = sqrt((2.428637-2.241859)^2+(48.9171-48.79654)^2)

# normalized distance by car to work per day on the all district
#   - proxy for O/D : from building to a random point on the extent.
#   - motorization parameter : take 40% of buildings only.
#   - normalized by diameter of all area

carToWork <- function(arrs){
  coords = buildings@coords[arr_buildings%in% arrs,]
  n=length(coords[,1])
  inds=sample(1:n,size=floor(0.1*n))
  with_car = coords[inds,]
  dests = matrix(data=c(runif(length(with_car[,1]),min=2.241859,max=2.428637),
                        runif(length(with_car[,1]),min=48.79654 ,max=48.9171)),nrow=length(with_car[,1]))
  d <- (dests - with_car)^2
  d <- sqrt(d[,1]+d[,2]) / dmax
  indic = mean(d)
  dat = matrix(0,n,3)
  dat[inds,3]=d
  dat[,1:2]=coords
  
  res=list()
  res[[1]]=dat
  res[[2]]=indic
  
  return(res)
}


# mean vehicle flow per street : number of lanes, pedestrian streets, etc
#  -> normalized by a max flow. Log-normal distrib fits well such empirical var.
#  (street hierarchy == betweeness == preferential attchment)

vehicleFlow <- function(arrs){
  
}








