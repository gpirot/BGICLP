functionGenerator <- function(selectedWells,selectedGeology,sourceCoord,pNorm){
  geolName <- switch(selectedGeology,"A0","A4")
  file_name <- paste("data/grid_25_wells_p",pNorm,"_",geolName,"_",sourceCoord[1],"_",sourceCoord[2],".txt",sep = "")
  out <- read.table(file_name, header=F)
  if (length(selectedWells)==1){
    out <- out[,selectedWells]^1/pNorm
  } else {
	out <- rowSums(out[,selectedWells])^1/pNorm
  }  
  return(out)
}
