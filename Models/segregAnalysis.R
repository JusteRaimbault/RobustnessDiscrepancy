
## analysis of results


setwd(paste0(Sys.getenv('CS_HOME'),'/RobustnessDiscrepancy/Models'))
source('functions.R')

library(ggplot2)

allres=list()
for(dep in c("75","92","93","94")){
  load(paste0('res/missing_ext_',dep,'.RData'))
  allres[[dep]] = res
}
load('res/missing.RData')
grdParis=res
 
deps = c()
for(n in names(allres)){deps=append(deps,rep(n,length(allres[[1]])*dim(allres[[1]][[1]])[1]))}

dr = as.tbl(data.frame(
  rob=unlist(lapply(allres,function(res){unlist(lapply(res,function(d){d[,1]/grdParis[[1]][1,1]}))})),
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



