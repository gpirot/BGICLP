rm(list=ls())
par(mfrow=c(1,1), mar=c(5.1, 4.1, 4.1, 2.1))
source("src/functionGenerator.R")
#install.packages("methods")
#install.packages("DiceKriging")
#install.packages("DiceOptim")
#install.packages("DiceDesign")
#install.packages("lattice")
#install.packages("colorRamps")
library("methods")
library("DiceKriging")
library("DiceOptim")
library("lattice")
source("src/generate_lhs_on_grid.R")
source("src/image.scale.R") # for plotting reasons only
set.seed(123)

# ***************************************************************
# Graphic parameters
# ***************************************************************
pal.1=colorRampPalette(c("yellow", "red", "black"), space="rgb")
colpoints <- "dodgerblue"#"cyan";#"blue";#"orange"; 
pchpoints <- 16
colnewpoint <- "orange"; 
colnewpointEI <- "red"; 
pchnewpoint <- 16
cexsize=1.5  


# ***************************************************************
# DEFINE SCENARIO AND EGO PARAMETERS
# ***************************************************************
# Geology: 1 or 2
selectedGeology <- 1
# Source c(89,-36) or c(100,10)
sourceCoord <- c(89,-36)
# Norm: 1 for L1 norm, 2 for L2 norm
pNorm <- 1
# Wells ID: combination of wells identified individually between 1 and 25
selectedWells <- c(3,2,4,1,5,8,7,9,6,10,13,12,14,11,15,18,17,19,16,20,23,22,24,21,25)
# number of EGO iterations
nseq <- 5
# numberof points in the initial design
n0 <- 9
# seed for latin hypercube sampling design of initial points
lhsseed <- 1

# ***************************************************************
# BUILD OBJECTIVE FUNCTION 
# ***************************************************************
full_grid_par <- read.table("data/full_grid_searching_zone_par.txt",header=F)
response.grid <- functionGenerator(selectedWells,selectedGeology,sourceCoord,pNorm)

argmin <- full_grid_par[which.min(response.grid),]
myTitle <- paste("Geology #",selectedGeology,", L",pNorm," norm, (x_s,y_s)=(",sourceCoord[1],",",sourceCoord[2],")",sep="")
xgrid <- seq(20,170,by=3)
ygrid <- seq(-75,75,by=3)
ngridx <- length(xgrid)
ngridy <- length(ygrid)

pdf(file="EGOdemo.pdf",width=12/2.54, height=11/2.54)
layout(matrix(c(1,2), nrow=1, ncol=2), widths=c(3,1))


	# GENERATE initial Latin Hypercube Sampling Design
 	design <- maximinESE_LHS(lhsDesign(n=n0, dimension=2, seed=lhsseed)$design)$design #round 
  	design <- round(design*51+0.5)
  	IDs <- (design[,2]-1)*51 + design[,1] 

   # OR LOAD initial Latin Hypercube Sampling Design
   # load("lhslike.Rda")
   mydesign0 <- full_grid_par[IDs,]
   myresponse0 <- response.grid[IDs]
   mydesign <- mydesign0
   myresponse <- myresponse0
   minresponse <- rep(min(myresponse),nrow(mydesign0))
   mindistance <- rep(min(sqrt((mydesign0[,1]-argmin[,1])^2+ (mydesign0[,2]-argmin[,2])^2)),nrow(mydesign0))

   # RUN EGO and save figures in pdf
   for(i in seq(1,nseq))
   {
      # GP model fitting and prediction 
      mykm <- km(~1, design=mydesign, response=myresponse,
              covtype="matern3_2", control=list(pop.size=50,trace=FALSE), 
              parinit=c(50, 50), lower=c(15,15), nugget = 1e-12)
      mypred <- predict(object=mykm, newdata=full_grid_par, type="UK")

      myEI <- apply(full_grid_par, 1, EI,mykm)
      EI.grid <- matrix(myEI, ngridx, ngridy)

      ################# SUMMARY GRAPHS ############
      kmean.grid <- matrix(mypred$mean, ngridx, ngridy) 
      breaks <- seq(min(matrix(kmean.grid, ngridx, ngridy)), max(matrix(kmean.grid, ngridx, ngridy)),length.out=100)
      par(mar=c(4.2,4.5,2.5,.5))
      image(xgrid, ygrid, matrix(kmean.grid, ngridx, ngridy),col=pal.1(length(breaks)-1),
            main=paste("GP mean prediction iter. ",i,sep=""),xlab="X(m)",ylab="Y(m)", axes=TRUE,cex.lab=cexsize,cex.axis=cexsize) # 
      points(mydesign[,1], mydesign[,2], pch=pchpoints, cex=1.5, col=colpoints)
      nextind <- which.max(myEI)
      box()
      par(mar=c(4.2,.5,2.5,5))
      image.scale(matrix(kmean.grid, ngridx, ngridy), col=pal.1(length(breaks)-1), breaks=breaks, horiz=FALSE,yaxt="n",xlab="",ylab="")
      axis(4,las=2,cex.axis=cexsize)
      box()
      #contour(xgrid,ygrid,kmean.grid, nlevel=30, add=TRUE)
      #
      ksd.grid <- matrix(mypred$sd, ngridx, ngridy)
      breaks <- seq(min(matrix(ksd.grid, ngridx, ngridy)), max(matrix(ksd.grid, ngridx, ngridy)),length.out=100)
      par(mar=c(4.2,4.5,2.5,.5))
      image(xgrid, ygrid, matrix(ksd.grid, ngridx, ngridy),col=pal.1(length(breaks)-1),
      main=paste("GP standard deviation iter. ",i,sep=""),xlab="X(m)",ylab="Y(m)", axes=TRUE,cex.lab=cexsize,cex.axis=cexsize) # 
      box()
      par(mar=c(4.2,.5,2.5,5))
      image.scale(matrix(ksd.grid, ngridx, ngridy), col=pal.1(length(breaks)-1), breaks=breaks, horiz=FALSE,yaxt="n",xlab="",ylab="")
      axis(4,las=2,cex.axis=cexsize)
      box()
      #contour(xgrid,ygrid,ksd.grid, nlevel=30, add=TRUE)
      #points(mydesign[,1], mydesign[,2], pch=pchpoints, lwd=2, col=colpoints)
      #
      breaks <- seq(min(matrix(EI.grid, ngridx, ngridy)), max(matrix(EI.grid, ngridx, ngridy)),length.out=100)
      par(mar=c(4.2,4.5,2.5,.5))
      image(xgrid, ygrid, matrix(EI.grid, ngridx, ngridy),col=pal.1(length(breaks)-1),
            main=paste("Expected Improvement iter. ",i,sep=""), xlab="X(m)",ylab="Y(m)", axes=TRUE,cex.lab=cexsize,cex.axis=cexsize) #
      #contour(xgrid,ygrid,EI.grid, nlevel=30, add=TRUE)
      #points(mydesign[,1], mydesign[,2], pch=pchpoints, lwd=2, col=colpoints)
      nextind <- which.max(myEI)
      nextpoint <- full_grid_par[nextind,]
      points(nextpoint[,1], nextpoint[,2], pch=pchnewpoint, cex=1.5,col=colnewpointEI)
      box()
      par(mar=c(4.2,.5,2.5,5))
      image.scale(matrix(EI.grid, ngridx, ngridy), col=pal.1(length(breaks)-1), breaks=breaks, horiz=FALSE,yaxt="n",xlab="",ylab="")
      axis(4,las=2,cex.axis=cexsize)
      box()
      ##############################################################################
      # NEXT
      mydesign <- rbind(mydesign, nextpoint)
      myresponse <- c(myresponse, response.grid[nextind])
      minresponse <- c(minresponse,min(myresponse))
      mindistance <- c(mindistance,min(sqrt((mydesign[,1]-argmin[,1])^2+ (mydesign[,2]-argmin[,2])^2)))
   }
   dev.off()
   graphics.off()

   res.design <- mydesign[seq(1,nrow(mydesign0)+nseq),]
   res.cummin <- minresponse[seq(1,nrow(mydesign0)+nseq)]
   res.mindist <- mindistance[seq(1,nrow(mydesign0)+nseq)]
   # SAVE DESIGN
   save(res.design,res.cummin,res.mindist,file=paste("resultsEGOlhs",n0,"+",nseq,".Rda",sep=""))


