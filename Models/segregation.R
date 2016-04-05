
#

setwd(paste0(Sys.getenv('CS_HOME'),'/RobustnessDiscrepancy/Models'))


library(rgdal)
library(rgeos)
library(dplyr)
library(cartography)

departement = "69"
# load 
#iris <- readOGR('../Data/raw/iris/shpfr',paste0('CONTOURS-IRIS',departement))
iris <- readOGR('../Data/raw/iris/shpfr','idf')


datairis <- read.csv('../Data/raw/iris/structure-distrib-revenus-iris-2011/RFDM2011IRI.csv',sep=';')
names(datairis)[1]="DCOMIRIS"
datacom <- read.csv('../Data/raw/iris/structure-distrib-revenus-com-2011/RFDM2011COM.csv',sep=';')
datacom$DCOMIRIS = paste0(datacom$COM,"0000")

spdata = merge(iris,data)
cols <- carto.pal(pal1 = "green.pal",n1 = 5, pal2 = "red.pal",n2 = 5)

#plot(spdata, border = NA, col = NA, bg = "#A6CAE0")
plot(spdata, border = NA, col = NA, bg = "#E3DEBF")
#plot(spdata, col  = "#E3DEBF", border=NA, add=TRUE)

choroLayer(spdf = spdata,
           df = datairis,
           var="RFMQ211",
           col=cols,
           #nclass=5
           #breaks = quantile(data$NBMEN11,probs=seq(from=0,to=1,by=0.2),na.rm=TRUE),
           #add=FALSE
           )


plot(spdata,border = "grey20", lwd=0.75, add=TRUE)

###################




ids = as.tbl(data.frame(ids=as.character(iris@data$DCOMIRIS)))
cdata=as.tbl(data.frame(medincome=datacom$RFMQ211,ids=datacom$DCOMIRIS))
idata=as.tbl(data.frame(medincome=datairis$RFMQ211,ids=datairis$DCOMIRIS))
jdata=left_join(ids,cdata,by=c("ids"))
jdata=left_join(jdata,idata,by=c("ids"),copy=TRUE)
#jdata$medincome = jdata$medincome.x*as.numeric(!is.na(jdata$medincome.x))+ jdata$medincome.y*as.numeric(!is.na(jdata$medincome.y))
# rq : a ifelse could do that easily
jdata$medincome = ifelse(!is.na(jdata$medincome.y),jdata$medincome.y,jdata$medincome.x) # rq : a ifelse could do that easily
x = jdata$medincome - mean(jdata$medincome,na.rm=TRUE )
x[is.na(x)]=0

# compute distance matrix for spatial autocorr

centroids = gCentroid(iris,byid = TRUE)
spweights = 1/spDists(centroids@coords)
w=as.matrix(spweights)
diag(w)<- 0

localmorans = (diag(x)%*%w)%*%matrix(x,nrow=length(x))
localmorans = localmorans / ((w%*%matrix(rep(1,length(x)),nrow=length(x)))*(matrix(x,ncol=length(x))%*%matrix(x,nrow=length(x)))[1,1])
localmorans[localmorans==0]=NA
df=data.frame(DCOMIRIS=iris@data$DCOMIRIS,localmoran=localmorans,medincome=jdata$medincome);names(df)[1]="DCOMIRIS"

spdata =merge(iris,df)

par(mfrow=c(1,2))
#plot(spdata, border = NA, col = NA, bg = "#E3DEBF")
choroLayer(spdf = spdata,
           df = df,
           var="localmoran",
           col=cols,
           nclass=10,
           #breaks = quantile(data$NBMEN11,probs=seq(from=0,to=1,by=0.2),na.rm=TRUE),
           add=FALSE,lwd = 0.05
)
#plot(spdata,border = "grey20", lwd=0.1, add=TRUE)

choroLayer(spdf = spdata,
           df = df,
           var="medincome",
           col=cols,
           nclass=10,
           #breaks = quantile(data$NBMEN11,probs=seq(from=0,to=1,by=0.2),na.rm=TRUE),
           add=FALSE,lwd = 0.05
)
#plot(spdata,border = "grey20", lwd=0.1, add=TRUE)


