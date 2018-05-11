functionAddNoise <- function(input,pctnoise){
  epsilon <- rnorm(length(input), mean = 0, sd = 1)
  dim(epsilon) = dim(input)
  output <- input * (1+pctnoise*epsilon/2.5)
  return(output)
}


