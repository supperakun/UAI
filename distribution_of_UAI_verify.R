
##Generate three random numbers from a uniform distribution
rand1 <- runif(5000000,0,1)
rand2 <- runif(5000000,0,1)
rand3 <- runif(5000000,0,1)
sum = rand1+rand2+rand3
#Order these random numbers
cut1 <- pmax(rand1,rand2,rand3)
cut2 <- pmin(rand1,rand2,rand3)
cut3 = sum-cut1-cut2
##Get the four random numbers that sum to 1
rand1cut <- 1-cut1
rand2cut <- cut1-cut3
rand3cut <- cut3-cut2
rand4cut = cut2
#Prepare data for statistics
dataview = as.data.frame(rand1cut)
dataview[,1]= rand1cut
dataview[,2]= rand2cut
dataview[,3]= rand3cut
dataview[,4]= rand4cut
dataview[,5]= pmax(rand1cut,rand2cut,rand3cut,rand4cut)

dataview[i,6]=0
dataview[i,7]=0
dataview[i,8]=0
dataview[i,9]=0
dataview[i,10]=0
#Once a degree great than 282-degree,it should be convert to peripheral angle of it
for (i in 1:5000000) {
  for(j in 1:4){
  if(dataview[i,j]==dataview[i,5]&dataview[i,5]>0.8){
  dataview[i,j]=1-dataview[i,5]
    }
  }
}
#Calulate the UAI
for (i in 1:5000000) {
  for(j in 1:4){
    if(dataview[i,j]>=0.2){
      dataview[i,j+5]=1
    }
  }
}

dataview[is.na(dataview)] <-0

for (i in 1:5000000) {
  for(j in 1:4){
    dataview[i,10]=(dataview[i,6]+dataview[i,7]+dataview[i,8]+dataview[i,9])/4
  }
}

write.table(dataview,"dataview.csv",row.names=FALSE,col.names=TRUE,sep=",")   











