library(ape)
library(treeio)
library(dplyr)
library(castor)


logTransformation = TRUE
#tree_file = input$tree_file_wo_transitions$datapath
#distances_file = input$distances_file$datapath
source("readT.R")

#tree<-readT("bedford/h3_small_sample.MCC.tre")
tree <- read.nexus("input/h3_small_sample.MCC.tre")
tree2 <- readT("input/h3_small_sample.MCC.tre")
taxaCountries <- read.table("input/Sampling_locations.txt")
country <- taxaCountries[, 2]

#the Equal rates model assumes that the transition rates from one state to the other and that is just plainly wrong
#I need to take the distance of the cities into account probably. So here the transition rates would probably depend on
#the distance of the 2 locations?
ERreconstruction <-
  ape::ace(country, tree, type = "discrete")# equal rates model #at the root the US has highest likelhood with 0.05
lik.anc_ER <- ERreconstruction$lik.anc

#taking the most likely state at each node
ancestral_states_ER <- rep("", 1388)
for (i in 1:tree$Nnode) {
  ancestral_states_ER[i] <-
    colnames(lik.anc_ER)[which(lik.anc_ER[i, ] == max(lik.anc_ER[i, ]))]
}

ancestral_states_Bayesian <- rep("", 1388)
for (i in 1:tree2$Nnode) {
  index = which(tree2$edge[, 2] == (1389 + i))
  if (i == 1) {
    ancestral_states_Bayesian[i] <- tree2$root.annotation$states
  }
  else{
    ancestral_states_Bayesian[i] <- tree2$annotations[[index]]$states
  }
}
comparison = rep(NA, 1388)
for (i in 1:tree$Nnode) {
  comparison[i] = ancestral_states_Bayesian[i] %in% ancestral_states_ER[[i]]
}

sum(comparison)
sum(comparison) / length(comparison)
sum(comparison[1:floor(length(comparison) / 2)])
sum(comparison[1:floor(length(comparison) / 2)]) / (length(comparison) /2)

all_states_ER = c(as.vector(country), ancestral_states_ER)
length(ancestral_states_ER)
N <- Nnode2(tree)
annotations <- tibble(node = 1:N, city = all_states_ER)
annotated_tree <- full_join(tree, annotations, by = "node")
write.beast(annotated_tree, file = "output/annotated_tree_ML.tree")
system("mv output/annotated_tree_ML.tree input/")
tree_recontructed_ML = readT("input/annotated_tree_ML.tree")


####Maximum Parsimony
encoding<-cbind(levels(country),as.numeric(1:57))
country_numerical<-rep(0,length(country))
for(i in 1:length(country)){
  index=which(encoding==country[i])
  country_numerical[i]<-encoding[index,2]
}   
country_numerical<-as.numeric(country_numerical)

MP_ER <- asr_max_parsimony(
  tree,
  country_numerical,
  Nstates = NULL,
  transition_costs = "all_equal",
  edge_exponent = 0,
  weight_by_scenarios = TRUE,
  check_input = TRUE
)
ancestral_states_MP_ER<-rep(0,tree$Nnode)
for (i in 1:tree$Nnode) {
  ancestral_states_MP_ER[i]<-list(which(MP_ER$ancestral_likelihoods[i, ] == max( MP_ER$ancestral_likelihoods[i, ])))
}

ancestral_states_MP_ER_Countries<-rep("",tree$Nnode)
for(i in 1:tree$Nnode){
  for(j in 1:length(ancestral_states_MP_ER[[i]])){
    index=as.numeric(ancestral_states_MP_ER[[i]][j])
    ancestral_states_MP_ER_Countries[i]<-list(append(ancestral_states_MP_ER_Countries[[i]],encoding[index,1]))
  }
  ancestral_states_MP_ER_Countries[[i]]<-ancestral_states_MP_ER_Countries[[i]][-1]
}   
comparison_MP = rep(NA, 1388)
for (i in 1:tree$Nnode) {
  comparison_MP[i] = ancestral_states_Bayesian[i] %in% ancestral_states_MP_ER_Countries[[i]]
}

sum(comparison_MP)
sum(comparison_MP) / length(comparison_MP)
sum(comparison_MP[1:floor(length(comparison_MP) / 2)])
sum(comparison_MP[1:floor(length(comparison_MP) / 2)]) / (length(comparison_MP) /2)

N <- Nnode2(tree)
ancestral_states_MP_ER_Countries_all<-c(as.vector(country),ancestral_states_MP_ER_Countries)
#x<-lapply(ancestral_states_MP_ER_Countries_all,list)
annotations <- tibble(node = 1:N, city = ancestral_states_MP_ER_Countries_all)
annotated_tree <- full_join(tree, annotations, by = "node")
write.beast(annotated_tree, file = "output/annotated_tree_MP.tree")
system("mv output/annotated_tree_MP.tree input/")
tree_MP = readT("input/annotated_tree_MP.tree")
