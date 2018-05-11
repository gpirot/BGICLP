functionGenerator <- function(wellid,geolid,srcid,lpnorm,pctnoise){
  load(paste("data/geol",geolid,"allC_1.Rda",sep = ""))		# load concentration c(i,t) file
  ct1 <- ct
  load(paste("data/geol",geolid,"allC_2.Rda",sep = ""))		# load concentration c(i,t) file
  ct2 <- ct
  load(paste("data/geol",geolid,"allC_3.Rda",sep = ""))		# load concentration c(i,t) file
  ct3 <- ct
  load(paste("data/geol",geolid,"allC_4.Rda",sep = ""))		# load concentration c(i,t) file
  ct4 <- ct
  load(paste("data/geol",geolid,"allC_5.Rda",sep = ""))		# load concentration c(i,t) file
  ct5 <- ct
  load(paste("data/geol",geolid,"allC_6.Rda",sep = ""))		# load concentration c(i,t) file
  ct6 <- ct
  load(paste("data/geol",geolid,"allC_7.Rda",sep = ""))		# load concentration c(i,t) file
  ct7 <- ct
  load(paste("data/geol",geolid,"allC_8.Rda",sep = ""))		# load concentration c(i,t) file
  ct8 <- ct
  load(paste("data/geol",geolid,"allC_9.Rda",sep = ""))		# load concentration c(i,t) file
  ct9 <- ct
  ct <- rep(0,(2603)*dim(ct1)[1]*dim(ct1)[2])
  dim(ct) = c(dim(ct1)[1],dim(ct1)[2],2603)
  istart=1
  iend = dim(ct1)[3]
  ct[,,istart:iend] <- ct1
  istart=istart+dim(ct1)[3]
  iend = iend+dim(ct2)[3]
  ct[,,istart:iend] <- ct2
  istart=istart+dim(ct2)[3]
  iend = iend+dim(ct3)[3]
  ct[,,istart:iend] <- ct3
  istart=istart+dim(ct3)[3]
  iend = iend+dim(ct4)[3]
  ct[,,istart:iend] <- ct4
  istart=istart+dim(ct4)[3]
  iend = iend+dim(ct5)[3]
  ct[,,istart:iend] <- ct5
  istart=istart+dim(ct5)[3]
  iend = iend+dim(ct6)[3]
  ct[,,istart:iend] <- ct6
  istart=istart+dim(ct6)[3]
  iend = iend+dim(ct7)[3]
  ct[,,istart:iend] <- ct7
  istart=istart+dim(ct7)[3]
  iend = iend+dim(ct8)[3]
  ct[,,istart:iend] <- ct8
  istart=istart+dim(ct8)[3]
  iend = iend+dim(ct9)[3]
  ct[,,istart:iend] <- ct9

  creal <- ct[wellid,,srcid]										# extract real concentrations
  cobs <- rep(functionAddNoise(creal,pctnoise),2601)				# add noise to produce observed concentrations
  dim(cobs) = c(dim(creal)[1],dim(creal)[2],2601)
  cloc <- ct[wellid,,1:2601]										# concentrations at all grid node locations
  tmp <- abs(cloc-cobs)^lpnorm									# diff to lpnorm power between observations and simulations
  out <- rep(0,2601)
  for (i in 1:2601){
    out[i]=(sum(tmp[,,i]))^(1/lpnorm)							# power normalization
  }
 
  return(out)
}

