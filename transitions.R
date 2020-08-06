library(ape)
library(phangorn)#ancentral reconstruction using ML and maximum parsony

logTransformation = TRUE
#tree_file = input$tree_file_wo_transitions$datapath
#distances_file = input$distances_file$datapath
source("readT.R")

tree_file = "example/PDCV_discrete_MCC.tree"
distances_file = "example/Pig_farms_distances.csv"
tree<-readT("bedford/h3_small_sample.MCC.tre")

#TODO use the tree to make ancestral reconstruction use Maximum Parsony and ML.
#http://www.molecularecologist.com/2016/02/quick-and-dirty-tree-building-in-r/ 
  
subsetDeff<-read.csv("bedford/subsetDeff.txt", sep = "\t")#distance matrix based on air transportation 
subsetDgeo<-read.csv("bedford/subsetDgeo.txt", sep= "\t") # distance matrix based on geographic distance
bredford_UPGMA<-upgma(subsetDeff) #unweighted pair group method with arithmetic mean
bredford_NJ <- phangorn::NJ(subsetDeff)
plot(bredford_UPGMA)


#for ML I need the data that was used to create the tree or you find a package that can take mcc.tre files as input
#MCC tree as input for ancestral reconstruction 
#tranfer mcc tree to phydat file?
