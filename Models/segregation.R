
#

setwd(paste0(Sys.getenv('CS_HOME'),'/RobustnessDiscrepancy/Models'))


library(rgdal)
library(dplyr)
library(cartography)

departement = "69"
# load 
iris <- readOGR('../Data/raw/iris/shpfr',paste0('CONTOURS-IRIS',departement))

data <- read.csv('../Data/raw/iris/structure-distrib-revenus-iris-2011/RFDM2011IRI.csv',sep=';')
names(data)[1]="DCOMIRIS"

spdata = merge(iris,data)
cols <- carto.pal(pal1 = "green.pal",n1 = 5, pal2 = "red.pal",n2 = 5)
choroLayer(spdf = spdata,
           df = data,
           var="NBMEN11",
           col=cols,
           nclass=10
           #breaks = quantile(data$NBMEN11,probs=seq(from=0,to=1,by=0.2),na.rm=TRUE),
           #add=FALSE
           )

plot(spdata,border = "grey20", lwd=0.75, add=TRUE)
