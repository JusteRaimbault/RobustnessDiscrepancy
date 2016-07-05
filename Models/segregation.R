
#

setwd(paste0(Sys.getenv('CS_HOME'),'/RobustnessDiscrepancy/Models'))

source('functions.R')

###
# Data preparation
###




#####
#  Test indicators

#moran = moran(jdata,spdata,spweights,"medincome",1:nrow(jdata))
#dissimilarity = dissimilarity(jdata,spdata,spweights,"medincome",1:nrow(jdata))
#entropy = entropy(jdata,"medincome",1:nrow(jdata))

#df=data.frame(DCOMIRIS=iris@data$DCOMIRIS,
#              localmoran=localmorans,
#              medincome=jdata$medincome)
#names(df)[1]="DCOMIRIS"

#spdata =merge(iris,df)
#spdata = merge(SpatialPointsDataFrame(centroids@coords,iris@data),df)

#par(mfrow=c(1,2))
#carto(spdata,df,"localmoran")
#carto(spdata,df,"medincome")


####
#  Sensitivity to missing data
#    -> sample datasets

library(doParallel)

departements = c("75","92","93","94","69")

for(dep in departements){

cl <- makeCluster(15)
registerDoParallel(cl)

startTime = proc.time()[3]

nrep = 75

#for(k in 1:nrep){
res <- foreach(i=1:nrep) %dopar% {
  setwd(paste0(Sys.getenv('CS_HOME'),'/RobustnessDiscrepancy/Models'))
  source('functions.R')
  #d = loadData('grandParis')
  d=loadData(paste0('CONTOURS-IRIS',dep))
  jdata=d$jdata;spdata=d$spdata;spweights=d$spweights
  robs=c();missing=c();morans=c();entropies=c();dissim=c();discr1=c();discr2=c()
  for(m in seq(from=0.0,to=0.5,by=0.02)){
    sample = sample.int(nrow(jdata),size=floor(nrow(jdata)*(1-m)))
    mo = moran(jdata,spdata,spweights,"medincome",sample)
    diss = dissimilarity(jdata,spdata,spweights,"medincome",sample)
    entr = entropy(jdata,"medincome",sample)
    d1 = discrepancyCriteria(mo$data,type=c('L2'))$DisL2
    d2 = discrepancyCriteria(entr$data,type=c('L2'))$DisL2
    robs = append(robs,d1*mo$globalmoran+d1*diss$dissimilarity+d2*entr$entropy)
    missing=append(missing,m)
    morans=append(morans,mo$globalmoran);entropies=append(entropies,entr$entropy);dissim=append(dissim,diss$dissimilarity)
    discr1=append(discr1,d1);discr2=append(discr2,d2)
  }
  return(data.frame(robs,missing,morans,entropies,dissim,discr1,discr2))
}

stopCluster(cl)

show(proc.time()[3]-startTime)

save(res,file=paste0('res/missing_ext_withIndics_',dep,'.RData'))

}



