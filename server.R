library(raster)
library(shiny)
library(dplyr)
library(shinyIncubator)
source("Functions.R")
source("readT.R")
shinyServer(function(input, output) {

observeEvent(input$start, {


  #tree_fileGlobal="input/annotated_tree_ML.tree"
  #distances_raw_fileGlobal="input/subsetDeff.txt"
   state="states"
  # makeSymmetric=F
  # logTransformation=F
  
 # source("AncestralReconstruction.R")
  tree_fileGlobal = input$tree_file_wo_transitions$datapath
  distances_raw_fileGlobal = input$distances_file$datapath
  stateGlobal= input$Annotation_State
  makeSymmetricGlobal=input$Symmetrie
  logTransformationGlobal = input$LogTransform
  
  x<-importingFiles(distances_raw_file = distances_raw_fileGlobal,tree_file = tree_fileGlobal)%>%
    GenerateRawTransitionMatrix(state = stateGlobal, .)%>%
    GenerateFinal_Transitions_Distances(makeSymmetric = makeSymmetricGlobal, . )
  
    output$plot = renderPlot({
     x%>%
        plotting_fun(logTransformation = logTransformationGlobal, .)%>%
        linear_regression(.)
    
}) # output$plot = renderPlot({
}) # observeEvent(input$start, {
}) # shinyServer(function(input, output) {

