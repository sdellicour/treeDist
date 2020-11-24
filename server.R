library(raster)
library(tidyverse)
library(ape)
library(treeio)
library(castor)
library(broom)
library(ggfortify)
library(shinyIncubator)
library(reactlog)
library(ggtree)

source("readT.R")#  # Objects in this file are shared across all sessions in the same R process

shinyServer(function(input, output) {
  reactlog_enable()#logging a reactivity tree
  # Objects in this file are defined in each session
  source("AncestralReconstruction.R", local=T)
  source("Functions.R", local=T)
  observeEvent(input$start, {
    #Ancestral Reconstruction
    tree_file_not_annotated<-input$tree_file_wo_transitions$datapath
    #tree_file_not_annotated<-"input/h3_small_sample.MCC.tre"
    sampling_locations = input$sampling_locations$datapath
    #sampling_locations = "input/hostnames.txt"
    
    method=input$Reconstruction_Method
    
    #Transitions
    delimiter<-input$delimiter
    distances_raw_file<-input$distances_file$datapath
    #distances_raw_file<-"input/predictors/bodySize.csv"
    state= input$Annotation_State
    order= input$order
    makeSymmetric=input$Symmetrie
    logTransformation=input$LogTransform
    
    if (length(tree_file_not_annotated) == 0){
      tree_file = input$tree_file_w_transitions$datapath
      #tree_file = "input/batRABV.MCC.keep.target.heights.trees"
      distances_raw_and_tree<-importingFiles(distances_raw_file = distances_raw_file,tree_file = tree_file, delimiter)
       x<- distances_raw_and_tree%>%
         GenerateRawTransitionMatrix(state = state, .)%>%
        GenerateFinal_Transitions_Distances(makeSymmetric = makeSymmetric, . )
    }else{
      if (length(sampling_locations) == 0 | length(distances_raw_file)==0){
        print("Please upload a list of tip states and a distance matrix")
      }else{
        tree_max_ancestral_positions<-importingFilesNotAnnotated(sampling_locations=sampling_locations, tree_file_not_annotated=tree_file_not_annotated, delimiter)%>%
          chooseReconstructionMethod(method, x= .)
        distances_raw_and_tree<-importingOnlyDist(distances_raw_file = distances_raw_file,tree = tree_max_ancestral_positions$tree, delimiter = delimiter)
        x<-distances_raw_and_tree%>%
          GenerateRawTransitionMatrix(state = state, .)%>%
          GenerateFinal_Transitions_Distances(makeSymmetric = makeSymmetric, . )
      }
    }
    
    #assign variables to local environment
    transition_distances<-x$transition_distances 
    vals <- reactiveValues(keeprows = rep(TRUE, nrow(transition_distances)))
    
    output$plot = renderPlotly({
      plotting_fun(logTransformation = logTransformation, transition_distances, vals)
      
    }) # output$plot = renderPlot({
    
    
    output$plot_res = renderPlotly({
      linear_regression(transition_distances, logTransformation = logTransformation, vals=vals)$x%>%
        plotting_residuals(transition_distances, vals, .)
      
    }) # output$plot = renderPlot({
    
    output$lm=renderTable({
      glance(linear_regression(transition_distances, logTransformation = logTransformation, vals=vals)$lm)
      
    }) # output$plot = renderTable({
    
    output$lm.summary=renderPrint({
      summary(linear_regression(transition_distances, logTransformation = logTransformation, vals=vals)$lm)
      
    }) # output$plot = renderTable({
    
    output$output=renderTable({
      linear_regression(transition_distances, logTransformation = logTransformation, vals=vals)$output
      
    }) # output$plot = renderTable({
    
    output$hover<-renderPrint({
      hover_data<-event_data("plotly_hover", source = "plot")
      transition_distances[which(transition_distances$Key==hover_data$key),]
    })
    
    # Toggle points that are brushed, when button is clicked only 
    observeEvent(input$exclude_toggle, {
      selected_data<-event_data("plotly_selected", source = "plot")
      
      res<- data.frame(selected_=rep(FALSE, nrow(transition_distances)))
      res$selected_[which(transition_distances$Key %in% selected_data$key)]<-TRUE
      vals$keeprows <- xor(vals$keeprows, res$selected_)
      
      
      #Below code for biderectional reactivity but it gets confusing when both plots contains selected points
      
      #res_res<- data.frame(selected_=rep(FALSE, nrow(transition_distances)))
      #res_res$selected_[which(transition_distances$Key %in%selected_data$key)]<-TRUE
      
      #if(sum(res$selected_)>0)     vals$keeprows <- xor(vals$keeprows, res$selected_)#the ones that are set to true in res are set to false
      #else                         vals$keeprows <- xor(vals$keeprows, res_res$selected_)
    })
    
    # Reset all points
    observeEvent(input$exclude_reset, {
      vals$keeprows <- rep(TRUE, nrow(transition_distances))
    })
    
    
    #assign variable tree and then source the tree file
    tree<-distances_raw_and_tree$tree
    source("Tree.R", local=T)
    
    
  }) # observeEvent(input$start, {
  
}) # shinyServer(function(input, output) {

