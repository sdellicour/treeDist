#sink(paste("output/",logfile,".log"))
#plotting_fun(logTransformation = logTransformation, distances = distances, transitions=transitions)
#linear_regression()
#sink()
#plotname=gsub("\\.|/","_",paste("Log",logTransformation,"Sym",makeSymmetric,"Treeannotation",city,distances_raw_file,tree_file,sep="-"))
#logfile=gsub("\\.|/","_",paste("Log",logTransformation,"Sym",makeSymmetric,"Treeannotation",city,distances_raw_file,tree_file,sep="-"))

###trials
# popsize<-read.table("input/PopSize.txt")
# popsize
# origin<-matrix(rep(popsize[,2],57),ncol=57)
# colnames(origin)<-popsize[,1]
# rownames(origin)<-popsize[,1]
# destination<-t(origin)
# write.csv(x=destination,file = "input/destination.txt")
# write.csv(x=origin,file = "input/origin.txt")




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