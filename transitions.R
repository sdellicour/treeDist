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
SYMreconstruction <- ape::ace(country, tree, type="discrete", model="SYM") # all rates different model
#ARDreconstruction <- ace(country, tree, type="discrete", model="ARD") # symmetrical model

#setting marginal to TRUE? but this might lead to only local optima?


??phangorn
library(phangorn)
library(tidyr)
View(country)
country.df<-as.data.frame(country)
country.matrix<-model.matrix(~country, country.matrix)
colnames(country.matrix)<-levels(taxaCountries$countries)
colnames(country.matrix)<-unique(country)
country.matrix
country.phyDat<-as.phyDat(country.matrix, type="USER" , levels = c(0, 1))#take matrix with rows the taxa and cols the locations

library(help=phangorn)
parsimony(tree, country.phyDat)


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.10")
BiocManager::install("seqLogo")


fit = pml(tree, country.phyDat)
fit = optim.pml(fit, model="F81", control = pml.control(trace=0))
