library(raster)
library(shiny)
library(shinyIncubator)

shinyServer(function(input, output) {

observeEvent(input$start, {

  logTransformation = TRUE
  tree_file = input$tree_file_w_transitions$datapath
  distances_file = input$distances_file$datapath
  source("readT.R")
  
  output$plot = renderPlot({
    
    #tree_file = "example/PDCV_discrete_MCC.tree"
    #distances_file = "example/Pig_farms_distances.csv"
    tree = readT(tree_file)#created with ape package
    plot(tree)
    distances = read.csv(distances_file, head=T)
    
    head(print(tree)); print(distances)
    
    locations = colnames(distances)
    transitions = matrix(0, nrow=dim(distances)[1], ncol=dim(distances)[1])#0 matrix in size of distance matrix 
    row.names(transitions) = row.names(distances) #TODO are colnames always equal rownames? perhaps simplify
    colnames(transitions) = colnames(distances)
    for (i in 1:dim(tree$edge)[1])#the edge table should be read as: rows are the edge numbers and first col is start node  and 2nd is end node of the branch or edge
    {
      if (tree$edge[i,1]%in%tree$edge[,2])#if a start node is also a end node, then we are going from somewhere to that city. We want to assign the start city as location 1
      {
        index = which(tree$edge[,2]==tree$edge[i,1])#get branch that has node of interest as end node
        location1 = tree$annotations[[index]]$city #assign start node of branch as location1
      }	else	{
        location1 = tree$root.annotation$city#tip nodes are not in column 1 so if it is not an internal node it is then the root node
      }
      location2 = tree$annotations[[i]]$city#location 2 is the start of the branch we are at atm
      transitions[location1,location2] = transitions[location1,location2]+1
    }
    
    
    distances = distances[lower.tri(distances)]
    transitions = transitions[lower.tri(transitions)]#assign true to lower triangle values
    distances = distances[which(transitions!=0)]
    transitions = transitions[which(transitions!=0)]
    if (logTransformation == TRUE) distances = log(distances)
    
    par(mgp=c(0,0,0), oma=c(0,0,0,0), mar=c(3,3.5,1.5,2))
    cols1 = c("#FAA521","#4676BB"); cols2 = c("#FAA52150","#4676BB50")
    plot(distances, transitions, col=cols2[2], pch=16, cex=1.1, axes=F, ann=F)
    points(distances, transitions, col=cols1[2], pch=1, cex=1.1)
    axis(side=1, lwd.tick=0.2, cex.axis=0.9, mgp=c(0,0.4,0), lwd=0.2, tck=-0.02, col.tick="gray30", col.axis="gray30", col="gray30")
    axis(side=2, lwd.tick=0.2, cex.axis=0.9, mgp=c(0,0.6,0), lwd=0.2, tck=-0.02, col.tick="gray30", col.axis="gray30", col="gray30")
    title(ylab="transitions", cex.lab=1.1, mgp=c(2.0,0,0), col.lab="gray30")
    if (logTransformation == TRUE) title(xlab="distance (log)", cex.lab=1.1, mgp=c(1.4,0,0), col.lab="gray30")
    if (logTransformation != TRUE) title(xlab="distance", cex.lab=1.1, mgp=c(1.4,0,0), col.lab="gray30")

#extend functionality with the possibility to upload a tree that does not yet contain the information of transitions between locations
#create function that computes the transition frequencies using maximum likelihood or parsimony and then output enhanced tree file
    source("transitions.R")

}) # output$plot = renderPlot({
}) # observeEvent(input$start, {
}) # shinyServer(function(input, output) {

