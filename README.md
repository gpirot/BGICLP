# BGICLP
Benchmark Generator Inspired by Contaminant Localization Problem

The R script plotGeneratedFunction.R illustrates how to handle the few parameters to customize the objective function.
The R script runEGO.R illustrates the use of the EI algorithm.

THe src folder contains source files. Following R packages need to be installed in R:
install.packages("methods")
install.packages("DiceKriging")
install.packages("DiceOptim")
install.packages("DiceDesign")
install.packages("lattice")
install.packages("colorRamps")

The data folder contains simulated concentration results and a grid description of the search zone (.txt files), as well as gslib and vtk files describing the 2 geological property fileds (log10 K).
