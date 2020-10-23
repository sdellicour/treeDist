library(ape)
source("readAnnotatedNexus.r")

#import trees
tree1 = read.nexus("input/h3_small_sample.MCC.tre")
tree2 = readAnnotatedNexus("input/h3_small_sample.MCC.tre")

#edge numbers connected to tips
EdgesLeadingToTip=sapply(1:length(tree2$tip.label), function(tipnode) which(tree2$edge[,2]==tipnode))
x<-data.frame(tree2$edge[,2][EdgesLeadingToTip],
              tree1$edge[,2][EdgesLeadingToTip], 
              tree2$tip.label[tree2$edge[,2][EdgesLeadingToTip]],
              tree1$tip.label[tree1$edge[,2][EdgesLeadingToTip]])
colnames(x)<-c("tree1 tip node", "tree 2 tip node", "tree 1 tip label", "tree 2 tip label")
head(x)

#check if all tip label are the same when sorted accordingly
all.equal.character(x$`tree 1 tip label`, x$`tree 2 tip label`)#TRUE

comparison<-function(ancestral_positions){#get ancestral positions and original states and output ratio of correctly reconstructed states
  max_ancestral_positions<-sapply(1:tree2$Nnode, 
                                  function(i) names(which(ancestral_positions$lik.anc[i,]==max(ancestral_positions$lik.anc[i,]))))
  original.states=c(tree2$root.annotation$states,
                    sapply(which(tree2$edge[,2]>(1389)),function(index){
                      tree2$annotations[[index]]$states
                    })
  )
  print(paste0(round(sum(max_ancestral_positions==original.states)/tree2$Nnode,2),"% of the states were correctly reconstructed."))
}

countries = read.table("input/Sampling_locations.txt")
ancestral_positions1 = ape::ace(countries[,2], tree2, type="discrete")#reconstruct for original tree imported with read.AnnotatedNexus()
tree2$edge[,2][EdgesLeadingToTip]<-tree1$edge[,2][EdgesLeadingToTip]#alter edge table
ancestral_positions2 = ape::ace(countries[,2], tree2, type="discrete")#reconstruct for altered tree

comparison(ancestral_positions1)
comparison(ancestral_positions2)
  