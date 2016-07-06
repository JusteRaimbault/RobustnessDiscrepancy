
## analysis of results


setwd(paste0(Sys.getenv('CS_HOME'),'/RobustnessDiscrepancy/Models'))
source('functions.R')

library(ggplot2)

# compte grd Paris with missing = 0
d = loadData('grandParis')
jdata=d$jdata;spdata=d$spdata;spweights=d$spweights
sample = 1:nrow(jdata)
mo = moran(jdata,spdata,spweights,"medincome",sample)
diss = dissimilarity(jdata,spdata,spweights,"medincome",sample)
entr = entropy(jdata,"medincome",sample)
d1 = discrepancyCriteria(mo$data,type=c('L2'))$DisL2
d2 = discrepancyCriteria(entr$data,type=c('L2'))$DisL2

allres=list()
for(dep in c("75","92","93","94")){
  load(paste0('res/missing_ext_withIndics_',dep,'.RData'))
  allres[[dep]] = res
}
load('res/missing.RData')
grdParis=res
 
deps = c()
for(n in names(allres)){deps=append(deps,rep(n,length(allres[[1]])*dim(allres[[1]][[1]])[1]))}

# get max/min of indics
maxentr = max(c(entr$entropy,sapply(allres,function(l){sapply(l,function(res){return(max(res$entropies))})})))
minentr = min(c(entr$entropy,sapply(allres,function(l){sapply(l,function(res){return(min(res$entropies))})})))
maxmor = max(c(mo$globalmoran,sapply(allres,function(l){sapply(l,function(res){return(max(res$morans))})})))
minmor = min(c(mo$globalmoran,sapply(allres,function(l){sapply(l,function(res){return(min(res$morans))})})))
maxdiss = max(c(diss$dissimilarity,sapply(allres,function(l){sapply(l,function(res){return(max(res$dissim))})})))
mindiss = min(c(diss$dissimilarity,sapply(allres,function(l){sapply(l,function(res){return(min(res$dissim))})})))

getRob<-function(d){d[6]*(d[3]-minmor)/(maxmor-minmor)+d[6]*(d[5]-mindiss)/(maxdiss-mindiss)+d[7]*(d[4]-minentr)/(maxentr-minentr)}

# grd Paris with all data as reference
reference_rob = getRob(c(0,0,mo$globalmoran,entr$entropy,diss$dissimilarity,d1,d2))

dr = as.tbl(data.frame(
  #rob=unlist(lapply(allres,function(res){unlist(lapply(res,function(d){d[,1]/grdParis[[1]][1,1]}))})),
  rob=unlist(lapply(allres,function(res){unlist(lapply(res,function(d){getRob(d)/reference_rob}))})),
  missing=unlist(lapply(allres,function(res){unlist(lapply(res,function(d){d[,2]}))})),
  departement=deps
))
df = dr %>% group_by(missing,departement) %>% summarise(robmean=mean(rob),robsd=sd(rob))

g = ggplot(data = df,aes(x=missing,y=robmean,colour=departement))
g+geom_line()+geom_point()+geom_errorbar(aes(ymin=robmean-robsd,ymax=robmean+robsd),width=0.01)+
   xlab("Missing Data")+ylab("Robustness")

g=ggplot(data=df,aes(x=missing,y=robsd,colour=departement))
g+geom_line()+geom_smooth()+xlab("Missing Data")+ylab("Std Robustness")

m=lm(robmean~missing,df)
# -> info in residuals ?
plot(m$residuals)



######
## Mapping

d = loadData('grandParis')
mo=moran(df = d$jdata,spdf = d$spdata,spatialWeights = d$spweights,var = "medincome",sampling = 1:nrow(d$jdata))

par(mfrow = c(1,2))
carto(d$spdata,d$jdata,"medincome","Median\nIncome")
carto(d$spdata,data.frame(d$jdata,moran=mo$localmorans),"moran","Local Moran")



