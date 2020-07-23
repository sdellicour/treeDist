library(raster)
library(shiny)
library(shinyIncubator)

shinyServer(function(input, output) {

observeEvent(input$start, {

logTransformation = TRUE
tree_file = input$tree_file$datapath
distances_file = input$distances_file$datapath
source("readT.R")

output$plot = renderPlot({
							
#tree_file = "example/PDCV_discrete_MCC.tree"
#distances_file = "example/Pig_farms_distances.csv"

tree = readT(tree_file)#created with app
distances = read.csv(distances_file, head=T)

print(tree); print(distances)

locations = colnames(distances)
transitions = matrix(0, nrow=dim(distances)[1], ncol=dim(distances)[1])#0 matrix in size of distance matrix 
row.names(transitions) = row.names(distances) #TODO are colnames always equal rownames? perhaps simplify
colnames(transitions) = colnames(distances)
for (i in 1:dim(tree$edge)[1])
	{
		if (tree$edge[i,1]%in%tree$edge[,2])
			{
				index = which(tree$edge[,2]==tree$edge[i,1])
				location1 = tree$annotations[[index]]$city
			}	else	{
				location1 = tree$root.annotation$city
			}
		location2 = tree$annotations[[i]]$city
		transitions[location1,location2] = transitions[location1,location2]+1
	}
distances = distances[lower.tri(distances)]
transitions = transitions[lower.tri(transitions)]
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

}) # output$plot = renderPlot({
}) # observeEvent(input$start, {
}) # shinyServer(function(input, output) {

