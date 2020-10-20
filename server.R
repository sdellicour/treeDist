library(raster)
library(shiny)
library(dplyr)
library(shinyIncubator)

shinyServer(function(input, output) {

observeEvent(input$start, {

  output$plot = renderPlot({

    
#### Defining functions ##
    
transitions_raw_fun = function(distances_raw_file="input/subsetDgeo", city = "states") {
  distances_raw <- read.csv(distances_raw_file, head=T, sep="\t")
  locations = colnames(distances_raw)
  transitions_raw= matrix(0, nrow=dim(distances_raw)[1], ncol=dim(distances_raw)[1])#0 matrix in size of distance matrix 
  row.names(transitions_raw) = countries 
  colnames(transitions_raw) = countries
  
  for (i in 1:dim(tree$edge)[1])
    #the edge table should be read as: rows are the edge numbers and first col is start node  and 2nd is end node of the branch or edge
  {
    if (tree$edge[i, 1] %in% tree$edge[, 2])
      #if a start node is also an end node, then we are going from somewhere to that city. We want to assign the start city as location 1
    {
      index = which(tree$edge[, 2] == tree$edge[i, 1])#get node that is end node of edge in question
      location1 = get(city, tree$annotations[[index]]) #assign end node of branch as location1
      if (is.list(location1))
      {
        location1 = get(city, tree$annotations[[index]])[[1]]#take arbritarily the first MP ancestral state
      }
    }	else	{
      location1 = get(city, tree$root.annotation)#tip nodes are not in column 1 so if it is not an internal node it is then the root node
      if (is.list(location1))
      {
        location1 = get(city, tree$root.annotation)[[1]]
      }
    }
    location2 = get(city, tree$annotations[[i]])#location 2 is the start of the branch we are at atm
    if (is.list(location2))
    {
      location2 = get(city, tree$annotations[[i]])[[1]]
    }
    if (nchar(location2) > 2 | nchar(location1) > 2) {
      location2 = substr(location2, start = 1, stop = 2)
      location1 = substr(location1, start = 1, stop = 2)
    }
    transitions_raw[location1, location2] = transitions_raw[location1, location2] + 1
  }
  list=list(transitions_raw=transitions_raw,distances_raw=distances_raw)
  return(list)
}

plotting_prep <- function(makeSymmetric = TRUE, transitions_raw, distances_raw) {
  transitions=transitions_raw
  transitions_added=transitions_raw
  if (makeSymmetric == TRUE){
    for (i in colnames(transitions)){
      for (j in colnames(transitions)){
        transitions_added[i, j] <- transitions[j, i] + transitions[i, j]
      }
    }
    transitions <- transitions_added
  }
  names<-outer(X = colnames(transitions_raw),
               Y = rownames(transitions_raw),
               FUN = function(X,Y) paste(X,Y,sep="<->"))
  names = names[lower.tri(transitions)]#assign true to lower triangle values
  transitions = transitions[lower.tri(transitions)]#assign true to lower triangle values
  names=names[which(transitions!=0)]
  transitions = transitions[which(transitions != 0)]
  names(transitions)=names
  distances = distances_raw[lower.tri(distances_raw)]
  distances = distances[which(transitions != 0)]
  list<-list(transitions=transitions, distances=distances, transitions_added= transitions_added)
  return(list)
}

plotting_fun<-function(logTransformation=TRUE, distances, transitions){
  pdf(file=paste("output/Transitions",plotname, sep="-"))
  if (logTransformation == TRUE) distances = log(distances)
  par(mgp=c(0,0,0), oma=c(0,0,0,0), mar=c(3,3.5,1.5,2))
  cols1 = c("#FAA521","#4676BB"); 
  cols2 = c("#FAA52150","#4676BB50")
  plot(distances, transitions, col=cols2[2], pch=16, cex=1.1, axes = F, ann=F)
  points(distances, transitions, col=cols1[2], pch=1, cex=1.1)
  axis(side=1, lwd.tick=0.2, cex.axis=0.9, mgp=c(0,0.4,0), lwd=0.2, tck=-0.02, col.tick="gray30", col.axis="gray30", col="gray30")
  axis(side=2, lwd.tick=0.2, cex.axis=0.9, mgp=c(0,0.6,0), lwd=0.2, tck=-0.02, col.tick="gray30", col.axis="gray30", col="gray30")
  title(ylab="transitions", cex.lab=1.1, mgp=c(2.0,0,0), col.lab="gray30")
  if (logTransformation == TRUE) title(xlab="distance (log)", cex.lab=1.1, mgp=c(1.4,0,0), col.lab="gray30")
  if (logTransformation != TRUE) title(xlab="distance", cex.lab=1.1, mgp=c(1.4,0,0), col.lab="gray30")
}

linear_regression<-function(cut_off_residual=NULL, percentile=95){
    abline(lm(transitions ~ distances))
    dev.off()
    lm=lm(transitions~distances)
    pdf(file=paste("output/Residuals",plotname, sep="-"))
    plot(lm$residuals)
    dev.off()
    #TODO make this also applicable when there are more than one posible outlier.
    df_transitions_added<-data.frame(transitions_added)
    if(is.null(cut_off_residual)){ cut_off_residual=quantile((lm$residuals), percentile/100)}
    index=transitions[which(lm$residuals>cut_off_residual)]
    print(paste(length(index), "Possible Outlier(s):" , sep = " "))
    print(paste("cut_off_residual: ",cut_off_residual))
    output<-as.matrix(index%>%sort(decreasing =T))
    colnames(output)<-"Transitions"
    print(output)
    print(summary(lm))
    list(output=output,lm=lm)
    }   


#### Setting variables ##

tree_file="input/h3_small_sample.MCC.tre"
sampling.locations="input/Sampling_locations.txt"
countries<-levels(read.table(sampling.locations)[,2])
distances_raw_file="input/origin.txt"
city = "states"
makeSymmetric=T
logTransformation = FALSE
plotname=gsub("\\.|/","_",paste("Log",logTransformation,"Sym",makeSymmetric,"Treeannotation",city,distances_raw_file,tree_file,sep="-"))
#source("AncestralReconstruction.R")


#### Calling functions ##

source("readT.R")
tree <-readT(tree_file)
distances_raw=transitions_raw_fun(distances_raw_file=distances_raw_file, city = city)$distances_raw
transitions_raw=transitions_raw_fun(distances_raw_file=distances_raw_file, city =city)$transitions_raw

transitions<-plotting_prep(makeSymmetric=makeSymmetric, distances_raw = distances_raw, transitions_raw = transitions_raw)$transitions
distances<-plotting_prep(makeSymmetric=makeSymmetric, distances_raw = distances_raw, transitions_raw = transitions_raw)$distances
transitions_added<-plotting_prep(makeSymmetric=makeSymmetric, distances_raw = distances_raw, transitions_raw = transitions_raw)$transitions_added

plotting_fun(logTransformation = logTransformation, distances = distances, transitions=transitions)

linear_regression()



###trials

popsize<-read.table("input/PopSize.txt")
popsize

origin<-matrix(rep(popsize[,2],57),ncol=57)
colnames(origin)<-popsize[,1]
rownames(origin)<-popsize[,1]
destination<-t(origin)
write.csv(x=destination,file = "input/destination.txt")
write.csv(x=origin,file = "input/origin.txt")

}) # output$plot = renderPlot({
}) # observeEvent(input$start, {
}) # shinyServer(function(input, output) {

