library("DiceDesign")
n0 <- 9 # 4 and 4 are very unlucky :-)
design <- maximinESE_LHS(lhsDesign(n=n0, dimension=2, seed = 1)$design)$design #round 
design <- round(design*51+0.5)
#designr[,1] <- design[,1]*3+20
#designr[,2] <- design[,2]*3-75
IDs <- (design[,2]-1)*51 + design[,1] 

#full_grid_par[IDs,]
plot(design)
save(file="lhslike.Rda", IDs)
