########Other Packages for Ancestral reconstruction using ML##########
# SYMreconstruction <- ape::ace(country, tree, type="discrete", model="SYM", marginal=T) #symmetrical model 
# ARDreconstruction <- ace(country, tree, type="discrete", model="ARD") # all rates different model 
# 
# #####diversitree#
# install.packages("diversitree")
# install.packages("phytools")
# library(phytools)
# library(diversitree)
# 
# fitER <- rerootingMethod(tree, country, model = "ER")
# tree$tip.label[1]
# 
# ####treedater#
# install.packages("treedater")
# library(treedater)
# 
# 
# 
# #another idea is to use a python package via the reticulate package 
# install.packages("reticulate")
# library(reticulate)
# use_python("/home/tim/anaconda3/bin/python3")
# 
# ####treetime from python#
# treetime<-import("treetime")
# treetime$TreeAnc(tree = tree,  )


####Manual annotation of tree not needed because using function from tree.io#
#sort according to 2nd column of edge table (child node of branch)
#all_states_ER_sorted<-all_states_ER[tree$edge[,2]]
#tree_annotated<-tree

#for(i in 1:2776){
#  tree_annotated$root.annotation<-list("city"=all_states_ER[[1390]])
#  tree_annotated$annotations<- list("city"=all_states_ER_sorted[[i]])
#}