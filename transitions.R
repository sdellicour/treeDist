library(ape)
library(phangorn)#ancentral reconstruction using ML and maximum parsony

logTransformation = TRUE
#tree_file = input$tree_file_wo_transitions$datapath
#distances_file = input$distances_file$datapath
source("readT.R")


tree1<-read.nexus("bedford/h3_small_sample.MCC.tre")
tree2<-readT("bedford/h3_small_sample.MCC.tre")
taxaCountries<-read.table("Sampling_locations.txt")
country<-taxaCountries[,2]

#the Equal rates model assumes that the transition rates from one state to the other and that is just plainly wrong
#I need to take the distance of the cities into account probably. So here the transition rates would probably depend on
#the distance of the 2 locations?
ERreconstruction <- ape::ace(country, tree1, type="discrete")# equal rates model #at the root the US has highest likelhood with 0.05
lik.anc_ER<-ERreconstruction$lik.anc

#taking the most likely state at each node
ancestral_states_ER<-rep("",1388)
for(i in 1:tree1$Nnode){
  ancestral_states_ER[i]<-colnames(lik.anc_ER)[which(lik.anc_ER[i,]==max(lik.anc_ER[i,]))]
}

ancestral_states_Bayesian<-rep("",1388)
for(i in 1:tree2$Nnode){
  index=which(tree2$edge[,2]==(1389+i))
  if (i==1){
    ancestral_states_Bayesian[i]<-tree2$root.annotation$states
  }
  else{
    ancestral_states_Bayesian[i]<-tree2$annotations[[index]]$states
  }
}
comparison=rep(NA,1388)
for(i in 1:tree1$Nnode){
  comparison[i]=ancestral_states_Bayesian[i]==ancestral_states_ER[[i]]
}

sum(comparison)
sum(comparison)/length(comparison)
sum(comparison[1:floor(length(comparison)/2)])
sum(comparison[1:floor(length(comparison)/2)])/(length(comparison)/2)

all_states_ER=c(as.vector(country), ancestral_states_ER)
all_states_ER_sorted<-all_states_ER[tree1$edge[,2]]
tree1_annotated<-tree1
tree2$tip.label
country


###TODO
for(i in 1:2776){
 # if(i==1){
  #  tree1_annotated$root.annotation<-list("city"=all_states_ER_sorted[[i]])
  #}else{
  tree1_annotated$annotation[[i]]<- list("city"=all_states_ER_sorted[[i]])
  }
#}

transitions = matrix(0, nrow=length(unique(country)), ncol=length(unique(country)))#0 matrix in size of distance matrix 
colnames(transitions)=unique(country)
rownames(transitions)=unique(country)
for (i in 1:dim(tree1_annotated$edge)[1])#the edge table should be read as: rows are the edge numbers and first col is start node  and 2nd is end node of the branch or edge
{
  if (tree1_annotated$edge[i,1]%in%tree1_annotated$edge[,2])#if a start node is also a end node, then we are going from somewhere to that city. We want to assign the start city as location 1
  {
    index = which(tree1_annotated$edge[,2]==tree1_annotated$edge[i,1])#get branch that has node of interest as end node
    location1 = tree1_annotated$annotation[[index]]$city #assign start node of branch as location1
  }	else	{
    location1 = tree1_annotated$root.annotation$city#tip nodes are not in column 1 so if it is not an internal node it is then the root node
  }
  location2 = tree1_annotated$annotation[[i]]$city#location 2 is the start of the branch we are at atm
  transitions[location1,location2] = transitions[location1,location2]+1
}


SYMreconstruction <- ape::ace(country, tree, type="discrete", model="SYM", marginal=T) #symmetrical model 
ARDreconstruction <- ace(country, tree, type="discrete", model="ARD") # all rates different model 

#####diversitree#
install.packages("diversitree")
install.packages("phytools")
library(phytools)
library(diversitree)

fitER <- rerootingMethod(tree, country, model = "ER")
tree$tip.label[1]

####treedater#
install.packages("treedater")
library(treedater)



#another idea is to use a python package via the reticulate package 
install.packages("reticulate")
library(reticulate)
use_python("/home/tim/anaconda3/bin/python3")

####treetime from python#
treetime<-import("treetime")
treetime$TreeAnc(tree = tree,  )

#check the model and let the user choose if they want to use ML (maybe even different models within ML but to start with just one), Maximum Parsimony 
#setting marginal to TRUE? but this might lead to only local optima?
#guy baele current opinion in virology to understand the discrete state inference   
#no need to have several ML 
# load packages
  
          