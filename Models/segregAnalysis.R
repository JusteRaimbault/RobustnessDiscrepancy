
## analysis of results


setwd(paste0(Sys.getenv('CS_HOME'),'/RobustnessDiscrepancy/Models'))
source('functions.R')

library(ggplot2)

load('res/missing.RData')

dr = as.tbl(data.frame(rob=unlist(lapply(res,function(d){d[,1]/d[1,1]})),missing=unlist(lapply(res,function(d){d[,2]}))))
df = dr %>% group_by(missing) %>% summarise(robmean=mean(rob),robsd=sd(rob))

g = ggplot(data = df,aes(x=missing,y=robmean))
g+geom_line()+geom_point()+geom_errorbar(aes(ymin=robmean-robsd,ymax=robmean+robsd),width=0.02)

m=lm(robmean~missing,df)
# -> info in residuals ?
plot(m$residuals)

