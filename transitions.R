library(ape)
library(phangorn)#ancentral reconstruction using ML and maximum parsony

logTransformation = TRUE
#tree_file = input$tree_file_wo_transitions$datapath
#distances_file = input$distances_file$datapath
source("readT.R")


tree<-readT("bedford/h3_small_sample.MCC.tre")
taxaCountries<-read.csv("bedford/taxaCountries.txt", sep = "\t")
colnames(taxaCountries)<-c("taxa", "countries")
taxa <- taxaCountries$taxa
x<-which(!(tree$tip.label %in% taxa))#there is one tip in the tree that is not in the taxa list, I remove it for now but perhaps I should rather add it to the taxa list?
tree<-drop.tip(tree,x)

country<-taxaCountries$countries
names(country)<-taxaCountries$taxa

ERreconstruction <- ape::ace(country, tree, type="discrete", model="ER")# equal rates model #at the root the US has highest likelhood with 0.05
lik.anc_ER<-ERreconstruction$lik.anc
tree$Nnode

#taking the most likely state at each node
ancestral_states_ER<-rep("",1387)
for(i in 1:tree$Nnode){
  ancestral_states_ER[i]<-colnames(lik.anc_ER)[max.col(lik.anc_ER)[i]]
}
ancestral_states_ER

tree$ann

SYMreconstruction <- ape::ace(country, tree, type="discrete", model="SYM", marginal = F) # all rates different model
ARDreconstruction <- ace(country, tree, type="discrete", model="ARD") # symmetrical model

#check the model and let the user choose if they want to use ML (maybe even different models within ML but to start with just one), Maximum Parsimony 
#setting marginal to TRUE? but this might lead to only local optima?
#guy baele current opinion in virology to understand the discrete state inference   
#no need to have several ML 

          