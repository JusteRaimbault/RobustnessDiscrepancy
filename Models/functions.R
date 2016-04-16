
## 
#  Functions for robustness computation


library(rgdal)
library(rgeos)
library(dplyr)
library(cartography)
library(DiceDesign)





#'
#' (do not recompute spatial weights)
moran <-function(df,spdf,spatialWeights,var,sampling){
  
  w = spatialWeights[sampling,sampling]
  
  x = df[[var]] - mean(df[[var]],na.rm=TRUE )
  x[is.na(x)]=0
  x = x[sampling]
  
  localmorans = w
  for(i in 1:length(x)){localmorans[i,]<-localmorans[i,]*x[i]}
  localmorans = length(x) * localmorans%*%matrix(x,nrow=length(x))
  globalmoran = sum(localmorans) / ((matrix(rep(1,length(x)),ncol=length(x))%*%w%*%matrix(rep(1,length(x)),nrow=length(x)))[1,1]*(matrix(x,ncol=length(x))%*%matrix(x,nrow=length(x)))[1,1])
  localmorans = localmorans / ((w%*%matrix(rep(1,length(x)),nrow=length(x)))*(matrix(x,ncol=length(x))%*%matrix(x,nrow=length(x)))[1,1])
  localmorans[localmorans==0]=NA
  
  # note : what sense of local morans ? not so sure.
  
  res=list()
  
  res$localmorans = localmorans
  res$globalmoran = globalmoran
  # add data ? -> need centroid coords
  centroids = gCentroid(spdf,byid = TRUE)
  res$data = data.frame(centroids@coords[sampling,1],centroids@coords[sampling,2],df[[var]][sampling],df$population[sampling])
  res$data=res$data[!is.na(res$data[,3])&!is.na(res$data[,4]),]
  return(res)
}


#'
#'  d = \sum w_ij |d_i - d_j| / sum(w_ij)
dissimilarity <- function(df,spdf,spatialWeights,var,sampling){
  w = spatialWeights[sampling,sampling]
  # renormalize
  x = (df[[var]] - min(df[[var]],na.rm=TRUE ))/(max(df[[var]],na.rm=TRUE) - min(df[[var]],na.rm=TRUE ))
  x[is.na(x)]=0
  x = x[sampling]
  totweight=(matrix(rep(1,length(x)),ncol=length(x))%*%w%*%matrix(rep(1,length(x)),nrow=length(x)))[1,1]
  # construct abs(x_i-x_j)
  diss = (rep(1,nrow(w))%*%(w*abs(matrix(data=rep(x,length(x)),nrow=length(x),byrow=TRUE)-matrix(data=rep(x,length(x)),nrow=length(x),byrow=FALSE)))%*%rep(1,nrow(w)))[1,1]
  res=list()
  res$dissimilarity = diss/totweight
  centroids = gCentroid(spdf,byid = TRUE)
  res$data = data.frame(centroids@coords[sampling,1],centroids@coords[sampling,2],df[[var]][sampling],df$population[sampling])
  res$data=res$data[!is.na(res$data[,3])&!is.na(res$data[,4]),]
  return(res)
}

entropy<-function(df,var,sampling){
  x = as.numeric(df[[var]]*df$population)
  x = x / sum(x,na.rm=TRUE)
  x=x[sampling]
  e = -1 /log(length(x)) * sum(x*log(x),na.rm=TRUE)
  res=list()
  res$entropy=e
  res$data=data.frame(df[[var]][sampling],df$population[sampling])
  res$data=res$data[!is.na(res$data[,1])&!is.na(res$data[,2]),]
  return(res)
}



#'
#' indics function vec
robustness <- function(indicators,data){
  weights <- c()
  discrs <- c()
  for(i in 1:length(indics)){
    r = indics[i][[1]](arrs)
    weights = append(weights,r[[2]])
    #show(dim(r[[1]]))
    discrs = append(discrs,discrepancyCriteria(r[[1]],type=c('L2'))$DisL2)
  }
  res=list()
  
  #mem values
  res$weights=weights
  
  weights=weights/sum(weights)
  #show(weights)
  #show(discrs)
  
  res$robustness=sum(weights*discrs)
  
  return(res)
}


carto <- function(spdf,df,var,title){
  cols <- carto.pal(pal1 = "green.pal",n1 = 5, pal2 = "red.pal",n2 = 5)
  
  #plot(spdata, border = NA, col = NA, bg = "#E3DEBF")
  choroLayer(spdf = spdf,
             df = df,
             var=var,
             col=cols,
             nclass=10,
             #breaks = quantile(data$NBMEN11,probs=seq(from=0,to=1,by=0.2),na.rm=TRUE),
             add=FALSE,lwd = 0.1,
             legend.pos = "topleft",
             legend.title.txt = title,
             legend.values.rnd = 2
  )
  plot(spdf,border = "grey20", lwd=0.1, add=TRUE)
}



loadData<-function(layer){
  
  #departement = "69"
  # load 
  #iris <- readOGR('../Data/raw/iris/shpfr',paste0('CONTOURS-IRIS',departement))
  iris <- readOGR('../Data/raw/iris/shpfr',layer)
  
  # iris data
  datairis <- read.csv('../Data/raw/iris/structure-distrib-revenus-iris-2011/RFDM2011IRI.csv',sep=';')
  names(datairis)[1]="DCOMIRIS"
  # communes data
  datacom <- read.csv('../Data/raw/iris/structure-distrib-revenus-com-2011/RFDM2011COM.csv',sep=';')
  datacom$DCOMIRIS = paste0(datacom$COM,"0000")
  
  # joins
  ids = as.tbl(data.frame(ids=as.character(iris@data$DCOMIRIS)))
  cdata=as.tbl(data.frame(medincome=datacom$RFMQ211,population=datacom$NBMEN11,ids=datacom$DCOMIRIS))
  idata=as.tbl(data.frame(medincome=datairis$RFMQ211,population=datairis$NBMEN11,ids=datairis$DCOMIRIS))
  jdata=left_join(ids,cdata,by=c("ids"))
  jdata=left_join(jdata,idata,by=c("ids"),copy=TRUE)
  jdata$medincome = ifelse(!is.na(jdata$medincome.y),jdata$medincome.y,jdata$medincome.x)
  jdata$population =  ifelse(!is.na(jdata$population.y),jdata$population.y,jdata$population.x)
  jdata = data.frame(DCOMIRIS=jdata$ids,medincome=jdata$medincome,population=jdata$population)
  
  spdata =merge(iris,jdata)
  
  # compute distance matrix for spatial autocorr
  centroids = gCentroid(spdata,byid = TRUE)
  spweights = 1/spDists(centroids@coords)
  spweights=as.matrix(spweights)
  diag(spweights)<- 0
  p = jdata$population/sum(jdata$population,na.rm=TRUE)
  p[is.na(p)]=0
  for(i in 1:length(p)){spweights[i,]<-spweights[i,]*p[i]}
  for(j in 1:length(p)){spweights[,j]<-spweights[,j]*p[j]}
  #spweights = (diag(p)%*%spweights)%*%diag(p) # does not work quickly
  
  res=list()
  res$spdata=spdata;res$jdata=jdata;res$spweights=spweights
  return(res)
}





