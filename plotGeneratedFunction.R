rm(list=ls())
source("src/functionGenerator.R")
# ***************************************************************
# DEFINE PARAMETERS
# ***************************************************************
# Geology: 1 or 2
selectedGeology <- 1
# Source c(89,-36) or c(100,10)
sourceCoord <- c(89,-36)
# Norm: 1 for L1 norm, 2 for L2 norm
pNorm <- 1
# Wells ID: combination of wells identified individually between 1 and 25
selectedWells <- c(3,2,4,1,5,8,7,9,6,10,13,12,14,11,15,18,17,19,16,20,23,22,24,21,25)

# ***************************************************************
# BUILD OBJECTIVE FUNCTION 
# ***************************************************************
full_grid_par <- read.table("data/full_grid_searching_zone_par.txt",header=F)
response.grid <- functionGenerator(selectedWells,selectedGeology,sourceCoord,pNorm)


# ***************************************************************
# GET OBJECTIVE FUNCTION MINIMUM AND PLOT VERSUS THE SOURCE COORDINATES
# ***************************************************************
argmin <- full_grid_par[which.min(response.grid),]

myTitle <- paste("Geology #",selectedGeology,", L",pNorm," norm, (x_s,y_s)=(",sourceCoord[1],",",sourceCoord[2],")",sep="")
xgrid <- seq(20,170,by=3)
ygrid <- seq(-75,75,by=3)
ngridx <- length(xgrid)
ngridy <- length(ygrid)

plot.new()
image(xgrid, ygrid, matrix(response.grid, ngridx, ngridy),col=grey.colors(100),
      xlab="X",ylab="Y",main=myTitle)
contour(xgrid, ygrid, matrix(response.grid, ngridx, ngridy), nlevels=50 ,add=TRUE)
points(argmin,col="red",pch=12,cex=1)
points(sourceCoord[1],sourceCoord[2],col="yellow",pch="*",cex=0.5)


