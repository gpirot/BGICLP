rm(list=ls())
source("src/functionGenerator.R")
source("src/functionAddNoise.R")

# ***************************************************************
# Graphic parameters
# ***************************************************************
source("src/image.scale.R") # for plotting reasons only
pal.1=colorRampPalette(c("yellow", "red", "black"), space="rgb")


# ***************************************************************
# DEFINE PARAMETERS
# ***************************************************************
# Geology: 1 or 2
geolid <- 2
# Source c(89,-36) or c(100,10)
sourcexy <- c(89,-36)
# Norm: 1 for L1 norm, 2 for L2 norm
lpnorm <- 2
# Wells ID: combination of wells identified individually between 1 and 25
#wellid <- c(13)
#wellid <- c(11,13,15)
#wellid <- c(11,12,13,14,15)
#wellid <- c(11,12,13,14,15,1,2,3,4,5)
#wellid <- c(11,12,13,14,15,1,2,3,4,5,21,22,23,24,25)
#wellid <- c(11,12,13,14,15,1,2,3,4,5,21,22,23,24,25,6,7,8,9,10)
wellid <- c(11,12,13,14,15,1,2,3,4,5,21,22,23,24,25,6,7,8,9,10,16,17,18,19,20)
# proportional error relatively to the real concentrations
pctnoise <- 0/100 # usually around 10%


# ***************************************************************
# BUILD OBJECTIVE FUNCTION 
# ***************************************************************
full_grid_par <- read.table("data/full_grid_searching_zone_par.txt",header=F) # contains 2601 grid nodes coordinates followed by 2 sources coordinates
ixsrc <- which(full_grid_par[,1]==sourcexy[1] & full_grid_par[,2]==sourcexy[2]) # find position of source coordinates
srcid <- ixsrc[length(ixsrc)] # keep only 1
full_grid_par <- full_grid_par[1:2601,] # restrain to grid nodes
response.grid <- functionGenerator (wellid,geolid,srcid,lpnorm,pctnoise)


# ***************************************************************
# GET OBJECTIVE FUNCTION MINIMUM AND PLOT VERSUS THE SOURCE COORDINATES
# ***************************************************************
argmin <- full_grid_par[which.min(response.grid),]

myTitle <- paste("Geology #",geolid,", L",lpnorm," norm, (x_s,y_s)=(",sourcexy[1],",",sourcexy[2],"), %noise = ",pctnoise,sep="")
xgrid <- seq(20,170,by=3)
ygrid <- seq(-75,75,by=3)
ngridx <- length(xgrid)
ngridy <- length(ygrid)

breaks <- seq(min(matrix(response.grid, ngridx, ngridy)), max(matrix(response.grid, ngridx, ngridy)),length.out=100)
#breaks <- seq(0,0.05,length.out=100)


#col=grey.colors(100), ,main=myTitle
plot.new()
image(xgrid, ygrid, matrix(response.grid, ngridx, ngridy),col=pal.1(length(breaks)-1), breaks=breaks,
      xlab="X(m)",ylab="Y(m)",cex.lab=1.0,cex.axis=1.0) #, xaxt='n', yaxt='n', ann=FALSE
contour(xgrid, ygrid, matrix(response.grid, ngridx, ngridy), nlevels=50 ,add=TRUE)
points(argmin,col="magenta",pch=12,cex=1.0)
points(sourcexy[1],sourcexy[2],col="green",pch="o",cex=1.5)

nbwells<-length(wellid)
dev.copy(png,paste("figures/geol",geolid,"nbwells",nbwells,"noise",pctnoise*100,'.png',sep=""))
dev.off()
graphics.off()
