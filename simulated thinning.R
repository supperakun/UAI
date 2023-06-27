
library(dplyr)

###function for calculating UAI
Near.f=function(x0,y0,data1){
  x1=abs(data1$x-x0); y1=abs(data1$y-y0)
  dis=sqrt(x1*x1+y1*y1)
  nord=order(dis)
  nord[1:5]
  data2=rbind(data1[nord[1],],data1[nord[2],],data1[nord[3],],data1[nord[4],],data1[nord[5],])
}
Angle.f=function(x0,y0,xi,yi){
  deltax=xi-x0
  deltay=yi-y0
  if (deltay>0 &deltax>=0) {Angle=atan(deltax/deltay)} else
    if (deltay==0 &deltax>=0) {Angle=pi*0.5} else
      if (deltay<=0 &deltax>=0) {Angle=pi+atan(deltax/deltay)} else
        if (deltay<0 &deltax<0) {Angle=pi+atan(deltax/deltay)} else
          if (deltay==0 &deltax<0) {Angle=pi*1.5} else
          {Angle=pi*2+atan(deltax/deltay)}
  Angle
}
AngleDiff.f=function(Angle1,Angle2){
  AngleDiff=Angle1-Angle2
  if (AngleDiff>pi) {AngleDiff=2*pi-AngleDiff}
  if (AngleDiff>=72*pi/180){AngleDiff=0} else
  {AngleDiff=1}
}
w.f=function(x0,y0,x,y){
  Angle4=c()
  for(i in 1:4){
    xi=x[i];yi=y[i]
    Angle4[i]=Angle.f(x0,y0,xi,yi)
  }
  Angle4o=order(Angle4)
  count12=AngleDiff.f(Angle4[Angle4o[2]],Angle4[Angle4o[1]])
  count23=AngleDiff.f(Angle4[Angle4o[3]],Angle4[Angle4o[2]])
  count34=AngleDiff.f(Angle4[Angle4o[4]],Angle4[Angle4o[3]])
  count41=AngleDiff.f(Angle4[Angle4o[4]],Angle4[Angle4o[1]])
  count=(count12+count23+count34+count41)/4
  w=count
}
W.even=function(x0,y0,Near4){
  for(i in 1:4){
    xi=Near4$x[i];yi=Near4$y[i]
    Near4$angle[i]=Angle.f(x0,y0,xi,yi)
  }
  Near4o=Near4[order(Near4[,"angle"]),]
  Near4o$count=NA
  for (i in 1:3){
    Near4o$count[i]=AngleDiff.f(Near4o$angle[i+1],Near4o$angle[i])
  }
  Near4o$count[4]=AngleDiff.f(Near4o$angle[4],Near4o$angle[1])
  evenNo=c(Near4o[Near4o$count==0,]$No)
  return(evenNo)
}
W.cluster=function(x0,y0,Near4){
  for(i in 1:4){
    xi=Near4$x[i];yi=Near4$y[i]
    Near4$angle[i]=Angle.f(x0,y0,xi,yi)
  }
  Near4o=Near4[order(Near4[,"angle"]),]
  Near4o$count=NA
  for (i in 1:3){
    Near4o$count[i]=AngleDiff.f(Near4o$angle[i+1],Near4o$angle[i])
  }
  Near4o$count[4]=AngleDiff.f(Near4o$angle[4],Near4o$angle[1])
  clusterNo=c(Near4o[Near4o$count==1,]$No)
  return(clusterNo)
}

#Read plot data
a1=read.csv(file.choose())
data1=as.data.frame(a1)
#Caculate the UAI before thinning 
  for (i in 1:length(data1$x)){
    x0=data1[i,]$x;y0=data1[i,]$y
    Near5=Near.f(x0,y0,data1)[1:5,]
    Near4=Near5[2:5,]
    data1$W[i]=w.f(x0,y0,Near4$x,Near4$y)
  }
#Caculate the HDIS before thinning 
HDIS0= sqrt(0.5*((sqrt(0.008)-sqrt(sum(data1$W==0)/length(data1$x)))^2
                 +(sqrt(0.224)-sqrt(sum(data1$W==0.25)/length(data1$x)))^2
                 +(sqrt(0.576)-sqrt(sum(data1$W==0.5)/length(data1$x)))^2
                 +(sqrt(0.16)-sqrt(sum(data1$W==0.75)/length(data1$x)))^2
                 +(sqrt(0.032)-sqrt(sum(data1$W==1)/length(data1$x)))^2))
#Buffer setting
  buff=2
  data2=data1[data1$x>buff&data1$x<28,]
  data2=data2[data2$y>buff&data2$y<18,]#data2 stand for trees in core area

  data3=data1[data1$x<=buff|data1$x>=28,]
  data3=rbind(data1[data1$y<=buff|data1$y>=18,],data3)
  data3=distinct(data3)#data3 stand for trees in buffer area
  
#The variables used to record the results
  diejl = matrix(nr=142,nc=80)
  no = c()
  z = c()
  w = c()
  ul = c()  
  
#start thinning,80 trees would be thinned
  for (k in 1:80) {
    ca = length(data2$x)#number of trees left in core area
    for(i in 1:ca){
      datal = data2[-i,]
      datal = rbind(datal,data3)#combine trees in buffer area for UAI calculation
      er = length(datal$x)
      for (o in 1:er) {
        x0=datal[o,]$x;y0=datal[o,]$y
        Near5=Near.f(x0,y0,datal)[1:5,]
        Near4=Near5[2:5,]
        datal$W[i]=w.f(x0,y0,Near4$x,Near4$y)#calculate UAI 
      }
      #calculate HDIS
      HDIS= sqrt(0.5*((sqrt(0.008)-sqrt(sum(datal$W==0)/length(datal$x)))^2+(sqrt(0.224)-sqrt(sum(datal$W==0.25)/length(datal$x)))^2+(sqrt(0.576)-sqrt(sum(datal$W==0.5)/length(datal$x)))^2+(sqrt(0.16)-sqrt(sum(datal$W==0.75)/length(datal$x)))^2+(sqrt(0.032)-sqrt(sum(datal$W==1)/length(datal$x)))^2))
      diejl[i,k]=HDIS
      diejl[is.na(diejl)] <- 100
      #Find out the row number of the removed tree which gets the minimum of HDIS 
      z=which(diejl[,k] == min(diejl[,k]), arr.ind=TRUE)[1]
    }
    #record the no of that tree
    no[k] = data2$no[z]
    #Remove that tree from core area
    data2=data2[-z,]
    w[k] = mean(datal$W)
    ul[k] = 0.496+sqrt(0.033984/length(datal$no))*1.96
  }
  
  
 for (i in 1:80) {
   diejl[142,i]=min(diejl[,i])
 }
#The change of HDIS during thinning  
bianhua =  c(Hellinger0,diejl[142,])

