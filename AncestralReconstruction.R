chooseReconstructionMethod<-function(method, tip_states, tree_not_annotated){
  if(method=="ML"){
    max_ancestral_positions<-ML_Reconstruction(tip_states, tree_not_annotated)%>%
      max_ancestral_positions_ML(tree_not_annotated=tree_not_annotated, ancestral_positions= .)
  }
  if(method=="MP"){
    max_ancestral_positions<-prepare_states_MP(tip_states)%>%
      MaximumParsimonyReconstruction(tip_states_numerical = ., tree_not_annotated)%>%
      max_ancestral_positions_MP(tip_states, tree_not_annotated=tree_not_annotated, ancestral_positions= .)
  }
  tree_annotated<-writeAnnotatedTree(tree_not_annotated=tree_not_annotated, max_ancestral_positions = max_ancestral_positions, tip_states = tip_states )
  tree_annotated
}

ML_Reconstruction<-function(tip_states, tree_not_annotated){
  print("Creating ML reconstruction")
  ERreconstruction <- ape::ace(tip_states, tree_not_annotated, type = "discrete")
  return(ERreconstruction)
}

####Maximum Parsimony
prepare_states_MP<-function(tip_states){
  encoding<-cbind(levels(tip_states),as.numeric(1:length(levels(tip_states))))
  tip_states_numerical<-as.numeric(lapply(tip_states, function(tip_state) encoding[which(encoding[,1]==tip_state),2]))
  tip_states_numerical
}

MaximumParsimonyReconstruction<-function(tip_states_numerical, tree_not_annotated){
  print("Creating MP reconstruction")
  MP_ER <- asr_max_parsimony(
    tree = tree_not_annotated,
    tip_states = tip_states_numerical,
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
max_ancestral_positions_MP<-function(tip_states, tree_not_annotated, ancestral_positions){
  colnames(ancestral_positions$ancestral_likelihoods)<-levels(tip_states)
  max_ancestral_positions<-sapply(1:tree_not_annotated$Nnode, function(i) names(which(ancestral_positions$ancestral_likelihoods[i,]==max(ancestral_positions$ancestral_likelihoods[i,]))))
  
  max_ancestral_positions
}


writeAnnotatedTree<-function(tree_not_annotated, max_ancestral_positions, tip_states){
  N <- Nnode2(tree_not_annotated)
  ancestral_states_all<-c(as.vector(tip_states),max_ancestral_positions)
  
  annotations <- tibble(node = 1:N, states = ancestral_states_all)
  annotated_tree <- full_join(tree_not_annotated, annotations, by = "node")
  write.beast(annotated_tree, file = "input/test.tree")
  #system("cp output/test.tree input/")
  
  as_tibble(annotated_tree)
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




