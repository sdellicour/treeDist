library(raster)
library(shiny)
library(dplyr)
library(shinyIncubator)
source("Functions.R")
source("readT.R")

shinyServer(function(input, output) {

observeEvent(input$start, {
  
  #Ancestral Reconstruction
  source("AncestralReconstruction.R")
  tree_file_not_annotated<-input$tree_file_wo_transitions$datapath
  sampling_locations = input$sampling_locations$datapath
  method=input$Reconstruction_Method
  
  
  #Transitions
  distances_raw_file = input$distances_file$datapath
  state= "states" #input$Annotation_State
  makeSymmetric=input$Symmetrie
  logTransformation = input$LogTransform
  
  if (length(input$tree_file_wo_transitions) == 0){
    
    tree_file = input$tree_file_w_transitions$datapath
    x<- importingFiles(distances_raw_file = distances_raw_file,tree_file = tree_file)%>%
        GenerateRawTransitionMatrix(state = state, .)%>%
        GenerateFinal_Transitions_Distances(makeSymmetric = makeSymmetric, . )
  }else{
        tree_max_ancestral_positions<-importingFilesNotAnnotated(sampling_locations=sampling_locations, tree_file_not_annotated=tree_file_not_annotated)%>%
            chooseReconstructionMethod(method, x= .)
        x<-importingOnlyDist(distances_raw_file = distances_raw_file,tree = tree_max_ancestral_positions$tree)%>%
            GenerateRawTransitionMatrix(state = state, .)%>%
            GenerateFinal_Transitions_Distances(makeSymmetric = makeSymmetric, . )
  }
  
    
  #transitions<-x$transitions 
  #distances<-x$distances
  #names_matrix<-x$names_matrix
  
    output$plot = renderPlot({
      x%>%
        plotting_fun(logTransformation = logTransformation, .)%>%
        linear_regression(.)
}) # output$plot = renderPlot({
    
    renderText(
      expr,
      env = parent.frame(),
      quoted = FALSE,
      outputArgs = list(),
      sep = " "
    )
}) # observeEvent(input$start, {
}) # shinyServer(function(input, output) {

