library(raster)
library(shiny)
library(dplyr)
library(ggplot2)
library(shinyIncubator)
source("Functions.R")
source("readT.R")
source("AncestralReconstruction.R")


shinyServer(function(input, output) {

observeEvent(input$start, {
  
  #Ancestral Reconstruction
  tree_file_not_annotated<-input$tree_file_wo_transitions$datapath
  sampling_locations = input$sampling_locations$datapath
  method=input$Reconstruction_Method
  
  
  #Transitions
  distances_raw_file<-input$distances_file$datapath
  state= "states" #input$Annotation_State
  makeSymmetric=input$Symmetrie
  logTransformation = input$LogTransform
  
  if (length(input$tree_file_wo_transitions) == 0){
    tree_file = input$tree_file_w_transitions$datapath
    x<- importingFiles(distances_raw_file = distances_raw_file,tree_file = tree_file)%>%
        GenerateRawTransitionMatrix(state = state, .)%>%
        GenerateFinal_Transitions_Distances(makeSymmetric = makeSymmetric, . )
  }else{
    if (length(input$sampling_locations) == 0 | length(input$distances_file)==0){
      print("Please upload a list of tip states and a distance matrix")
    }else{
        tree_max_ancestral_positions<-importingFilesNotAnnotated(sampling_locations=sampling_locations, tree_file_not_annotated=tree_file_not_annotated)%>%
            chooseReconstructionMethod(method, x= .)
        x<-importingOnlyDist(distances_raw_file = distances_raw_file,tree = tree_max_ancestral_positions$tree)%>%
            GenerateRawTransitionMatrix(state = state, .)%>%
            GenerateFinal_Transitions_Distances(makeSymmetric = makeSymmetric, . )
    }
  }

  ##assign to global environment so that visible for other events
  transition_distances<<-x$transition_distances 
  vals<<-x$vals
  names_matrix<<-x$names_matrix
  
  output$plot = renderPlot({
    linear_regression(transition_distances)
    plotting_fun(logTransformation = logTransformation, transition_distances, vals)
      
}) # output$plot = renderPlot({
}) # observeEvent(input$start, {
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    res <- nearPoints(transition_distances, input$plot1_click, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(transition_distances, input$plot1_brush, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(transition_distances))
  })
  
}) # shinyServer(function(input, output) {

