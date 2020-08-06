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
x<-which(!(tree$tip.label %in% taxa))#there is one tip in hte tree that is not in hte taxa list, I remove it for now but perhaps I should rather add it to the taxa list?
tree<-drop.tip(tree,x)
country<-taxaCountries$countries
names(country)<-taxaCountries$taxa

ERreconstruction <- ace(country, tree, type="discrete", model="ER")# equal rates model #at the root the US has highest likelhood with 0.05
SYMreconstruction <- ace(country, tree, type="discrete", model="SYM") # all rated different model
#ARDreconstruction <- ace(country, tree, type="discrete", model="ARD") # symmetrical model

#setting marginal to TRUE? but this might lead to only local optima?
  