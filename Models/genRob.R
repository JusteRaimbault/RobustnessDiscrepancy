
# Generalized Rob Ratio computation on Geo data
# (Paris districts)

# -> first load arrondissements, buildings and roads from geodata script



# define objective functions

# Given arrdt numbers, computes on corresponding data
# and returns both raw data and indicator value (used for weight)

# overlay for buildings 
arr_buildings=over(SpatialPoints(buildings),arrondissements)

# overlay for streets
arr_streets = over(SpatialPoints(roads_raw[,1:2]),arrondissements)

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
  coords = roads_raw[arr_streets%in% arrs,]
  # generate hierarchy
  flows <- rlnorm(length(coords[,1]),0,0.01)/2
  flows<- flows/max(flows)
  dat = matrix(0,length(coords[,1]),4)
  dat[,1:3]=coords
  dat[,4] = flows
  
  # compute indic
  indic = mean(flows*coords[,3]/max(coords[,3]))
  
  res=list()
  res[[1]]=dat
  res[[2]]=indic
  
  return(res)
  
}


# more qualitative indicator : proportion of pedestrian streets
#

pedestrianStreets <- function(arrs){
  coords = roads_raw[arr_streets%in% arrs,]
  dat = matrix(0,length(coords[,1]),4)
  dat[,1:3]=coords
  
  tot_length=sum(coords[,3])
  # set non pedestrian roads length to zero
  dat[sample(1:length(dat[,1]),floor(length(dat[,1])*0.8)),3] = 0
  indic = sum(dat[,3])/tot_length
  
  res=list()
  res[[1]]=roads_raw
  res[[2]]=indic
  
  return(res)
  
}

# OK 3 indicators


# compute robustness ratio

rawBound <- function(indics,arrs){
  weights <- c()
  discrs <- c()
  for(i in 1:length(indics)){
    r = indics[i][[1]](arrs)
    weights = append(weights,r[[2]])
    discrs = append(discrs,discrepancyCriteria(r[[1]],type=c('L2'))$DisL2)
  }
  weights=weights/sum(weights)
  return(sum(weights*discrs))
}

# test
all = c(carToWork,vehicleFlow,pedestrianStreets)
rawBound(all,c(1))














