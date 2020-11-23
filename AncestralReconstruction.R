library(ape)
library(treeio)
library(dplyr)
library(castor)

chooseReconstructionMethod<-function(method, x){
  country<-x[[1]]
  tree_not_annotated<- x[[2]]
  if(method=="ML"){
    max_ancestral_positions<-ML_Reconstruction(country, tree_not_annotated)%>%
    max_ancestral_positions_ML(tree_not_annotated=tree_not_annotated, ancestral_positions= .)
  }
    if(method=="MP"){
      max_ancestral_positions<-prepare_states_MP(country)%>%
        MaximumParsimonyReconstruction(country_numerical = ., tree_not_annotated)%>%
        max_ancestral_positions_MP(country, tree_not_annotated=tree_not_annotated, ancestral_positions= .)
    }
  tree<-writeAnnotatedTree(tree_not_annotated=tree_not_annotated, max_ancestral_positions = max_ancestral_positions, country = country )
  list(tree=tree, max_ancestral_positions=max_ancestral_positions)
}

importingFilesNotAnnotated<-function(sampling_locations, tree_file_not_annotated, delimiter,session){
  tree_not_annotated <- read.nexus(tree_file_not_annotated)
  tree_not_annotated<-negativeBranchLength(tree_not_annotated)
  country <- read.table(sampling_locations, sep=delimiter)[,1]
  list(country, tree_not_annotated)
}

ML_Reconstruction<-function(country, tree_not_annotated){
  print("Creating ML reconstruction")
  ERreconstruction <- ape::ace(country, tree_not_annotated, type = "discrete")
  return(ERreconstruction)
}

####Maximum Parsimony
prepare_states_MP<-function(countries){
  encoding<-cbind(levels(countries),as.numeric(1:length(levels(countries))))
  countries_numerical<-as.numeric(lapply(countries, function(country) encoding[which(encoding[,1]==country),2]))
  countries_numerical
}

MaximumParsimonyReconstruction<-function(country_numerical, tree_not_annotated){
  print("Creating MP reconstruction")
  MP_ER <- asr_max_parsimony(
    tree = tree_not_annotated,
    tip_states = country_numerical,
    Nstates = NULL,
    transition_costs = "all_equal",
    edge_exponent = 0,
    weight_by_scenarios = TRUE,
    check_input = TRUE
  )
  
  MP_ER
}

#taking the most likely state at each node
max_ancestral_positions_ML<-function(tree_not_annotated, ancestral_positions){
  max_ancestral_positions<-sapply(1:tree_not_annotated$Nnode, function(i) names(which(ancestral_positions$lik.anc[i,]==max(ancestral_positions$lik.anc[i,]))))
  
  max_ancestral_positions
}

#taking the first item at each node
max_ancestral_positions_MP<-function(country, tree_not_annotated, ancestral_positions){

  colnames(ancestral_positions$ancestral_likelihoods)<-levels(country)
  max_ancestral_positions<-sapply(1:tree_not_annotated$Nnode, function(i) names(which(ancestral_positions$ancestral_likelihoods[i,]==max(ancestral_positions$ancestral_likelihoods[i,]))))
  
  max_ancestral_positions
}


writeAnnotatedTree<-function(tree_not_annotated, max_ancestral_positions, country){
  N <- Nnode2(tree_not_annotated)
  ancestral_states_all<-c(as.vector(country),max_ancestral_positions)

  annotations <- tibble(node = 1:N, states = ancestral_states_all)
  annotated_tree <- full_join(tree_not_annotated, annotations, by = "node")
  write.beast(annotated_tree, file = "input/test.tree")
  #system("cp output/test.tree input/")
  
  print("Reading annotated tree in beast format")
  readT("input/test.tree")
}


original_states<-function(tree_annotated){
  original.states=c(tree_annotated$root.annotation$states,
                    sapply(which(tree_annotatedree2$edge[,2]>(tree_annotated$Nnode)),function(index){
                      tree_annotated$annotations[[index]]$states
                    })
  )
}


comparison<-function(tree_annotated, tree_not_annotated, ancestral_positions){#get ancestral positions and original states and output ratio of correctly reconstructed states
  
  max_ancestral_positions<-max_ancestral_positions(tree_not_annotated, ancestral_positions)
  original.states<-original_states(tree_annotated)
  
  print(paste0(round(sum(max_ancestral_positions==original.states)/tree2$Nnode,2),"% of the states were correctly reconstructed."))
}




