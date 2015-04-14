
#Generic Implementation for integrated robustness calculation

# -> one data file per indicator (simpler to import data)
# -> value of the indicator (to caluclate weights)

# ** We aggregate X and Y since this distinction has more a conceptual interest **


#DiceDesign packages for discrepancy calculation ?

library(DiceDesign)

#test the library
#p <- dmaxDesign(50,2,0.9,200)
#plot(p$design)
#dp <- discrepancyCriteria(p$design,type=c('L2'))$DisL2

#OK Discrepancy calculation

#Generic script for 2 districts (since it is our case) ; should be quickly generalisable

setwd('/Users/Juste/Documents/Cours/TAMUR/Data/indicators')

#random files for testing
#write.table(matrix(runif(20,min=0,max=1),nrow=10), file = "data1_1.csv",col.names = FALSE, row.names = FALSE)
#write.table(matrix(runif(30,min=0,max=1),nrow=10), file = "data1_2.csv",col.names = FALSE,row.names = FALSE)
#write.table(matrix(runif(10,min=0,max=1),nrow=10), file = "data2_1.csv",col.names = FALSE ,row.names = FALSE)
#write.table(matrix(runif(40,min=0,max=1),nrow=20), file = "data2_2.csv",col.names = FALSE ,row.names = FALSE)

dataFilesQ1 <- c("descartes/egt.csv","descartes/egt.csv","descartes/buildingsparking.csv",
                "descartes/buildingspositions.csv",
                "descartes/structpop.csv")
dataFilesQ2 <- c("bussy/egt.csv","bussy/egt.csv","bussy/roadsnature.csv",
                 "bussy/roadswidth.csv","bussy/buildingspositions.csv")

decQ1 <- c(",",",",".",".",".")
decQ2 <- c(",",",",".",".",".")
tQ1 <- c(FALSE,FALSE,FALSE,FALSE,TRUE);
tQ2 <- c(TRUE,TRUE,FALSE,FALSE,FALSE);

ids1 <- read.table("ids1.csv",header=FALSE)
ids2 <- read.table("ids2.csv",header=FALSE)

#load matrices
#format : table of matrices ?

#data1 = list(read.table(dataFilesQ1[1]))
#data2 = list(read.table(dataFilesQ2[1]))
data1=list();
data2=list();

#quartier 1
for(i in 1:length(dataFilesQ1)){
  data1[[i]] = read.table(dataFilesQ1[i],sep=";",header=FALSE,dec=decQ1[i])
  for(k in 1:length(data1[[i]][,1])){for(l in 1:length(data1[[i]][1,])){if(is.na(data1[[i]][k,l])){data1[[i]][k,l]=0};}}
  if(tQ1[i]){data1[[i]]=t(data1[[i]])}
}

#quartier 2
for(i in 1:length(dataFilesQ2)){
  #show(dataFilesQ2[i]);
  data2[[i]] = read.table(dataFilesQ2[i],sep=";",header=FALSE,dec=decQ2[i])
  for(k in 1:length(data2[[i]][,1])){for(l in 1:length(data2[[i]][1,])){if(is.na(data2[[i]][k,l])){data2[[i]][k,l]=0};}}
  if(tQ2[i]){data2[[i]]=t(data2[[i]])}
  
}


#create unique list of ids (structure of the space characterizing urban fact)
#ids <- unique(c(ids1,ids2))
#ids <- 
##DO NOT NEED IDS !!!


#load values from file (same order as data files)
valuesQ1 = read.table("valuesQ1.csv",header=FALSE)
valuesQ2 = read.table("valuesQ2.csv",header=FALSE)

#weights, no subjective for now
w1 = valuesQ1 / sum(valuesQ1)
w2 = valuesQ2 / sum(valuesQ2)



#function tp calcualte robustness ratio, given list of considered indicators
# -> as Florent suggested, the strength of the approach in that case of only 2 district
# may be the comparison of different combination of indicators


# Rij = \sum(w_c*D_c)/\sum(w'_c*D'_c)

d1 = c()
d2 = c()


robustnessRatio = function(indics1,indics2){

  for(i in indics1){
    show(dataFilesQ1[i]);
    d=discrepancyCriteria(data1[[i]],type=c('L2'))$DisL2;
    if(is.na(d)){d=0}
    d1 = append(d,d1);
    #d1 = append(discrepancyCriteria(matrix(rnorm(100),ncol=5),type=c('L2'))$DisL2,d1);
    show(d1)
  }
  for(i in indics2){
    show(dataFilesQ2[i])
    d=discrepancyCriteria(data2[[i]],type=c('L2'))$DisL2;
    if(is.na(d)){d=0}
    d2 = append(d,d2)
    #d2 = append(discrepancyCriteria(matrix(rnorm(100),ncol=5),type=c('L2'))$DisL2,d2);
    show(d2);
  }
  #return(sum(w1[indics1,]*d1)/sum(w2[indics2,]*d2))
  return(data.frame(d1,d2))
}

#Caluclation Total
r = robustnessRatio(1:5,1:5)

d1 = r$d1
d2 = r$d2
sum(w1[1:5,]*(1-d1))/sum(w2[1:5,]*(1-d2))

ratio = function(indics1,indics2){
  return(sum(w1$V1[indics1]*(1-d1[indics1]))/sum(w2$V1[indics2]*(1-d2[indics2])))
}

distanceRefs = function(indics1,indics2){
  #count same indics : 0, ≠ : 1
  res = 0
  for(id1 in ids1$V1[indics1]){
    inids2 = FALSE
    for(id2 in ids2$V1[indics2]){
      if(id1==id2){inids2 = TRUE}
    }
    if(!inids2){res=res+1}
  }
  for(id2 in ids2$V1[indics2]){
    inids1 = FALSE
    for(id1 in ids1$V1[indics1]){
      if(id1==id2){inids1 = TRUE}
    }
    if(!inids1){res=res+1}
  }
  return(res/min(length(indics1),length(indics2)))
}

subsets=list(1:1,2:2,3:3,4:4,5:5,
             1:2,2:3,3:4,4:5,c(1,3),c(1,4),c(1,5),c(2,4),c(2,5),c(3,5),
             1:3,2:4,3:5,c(1,3,4),c(1,4,5),c(2,4,5),
             1:4,2:5,c(1,2,4,5),c(1,3,4,5),
             1:5
             )

#for each pair in subset, plot distance and R-ratio
distances=c();
ratios=c();
for(i in 1:length(subsets)){
  for(j in 1:length(subsets)){
    if(i!=j){distances = append(distanceRefs(subsets[[i]],subsets[[j]]),distances);
      ratios = append(ratio(subsets[[i]],subsets[[j]]),ratios)
    }
  }
}

plot(distances,ratios,ylim=c(0,2))

