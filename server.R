library(raster)
library(plotly)
library(shiny)
library(dplyr)
library(ggplot2)
library(broom)
library(ggfortify)
library(shinyIncubator)
source("readT.R")#  # Objects in this file are shared across all sessions in the same R process

shinyServer(function(input, output, session) {
  # Objects in this file are defined in each session
  source("AncestralReconstruction.R", local=T)
  source("Functions.R", local=T)
  
  observeEvent(input$start, {
    #Ancestral Reconstruction
    tree_file_not_annotated<-input$tree_file_wo_transitions$datapath
    #tree_file_not_annotated<-"input/h3_small_sample.MCC.tre"
    sampling_locations = input$sampling_locations$datapath
    #sampling_locations = "input/Sampling_locations.txt"
    
    method=input$Reconstruction_Method
    
    #Transitions
    delimiter<-input$delimiter
    distances_raw_file<-input$distances_file$datapath
    # distances_raw_file<-"input/predictors/bodySize.csv"
    state= input$Annotation_State
    order= input$order
    makeSymmetric=input$Symmetrie
    logTransformation=input$LogTransform
    
    if (length(tree_file_not_annotated) == 0){
      tree_file = input$tree_file_w_transitions$datapath
      #tree_file = "input/batRABV.MCC.trees"
      x<- importingFiles(distances_raw_file = distances_raw_file,tree_file = tree_file, delimiter)%>%
        GenerateRawTransitionMatrix(state = state, .)%>%
        GenerateFinal_Transitions_Distances(makeSymmetric = makeSymmetric, . )
    }else{
      if (length(sampling_locations) == 0 | length(distances_raw_file)==0){
        print("Please upload a list of tip states and a distance matrix")
      }else{
        tree_max_ancestral_positions<-importingFilesNotAnnotated(sampling_locations=sampling_locations, tree_file_not_annotated=tree_file_not_annotated, delimiter,session)%>%
          chooseReconstructionMethod(method, x= .)
        x<-importingOnlyDist(distances_raw_file = distances_raw_file,tree = tree_max_ancestral_positions$tree, delimiter = delimiter)%>%
          GenerateRawTransitionMatrix(state = state, .)%>%
          GenerateFinal_Transitions_Distances(makeSymmetric = makeSymmetric, . )
      }
    }
    
    #assign variables to local environment
    transition_distances<-x$transition_distances 
    vals<-x$vals
    vals$keeprows <- rep(TRUE, nrow(transition_distances))
    names_matrix<-x$names_matrix
    
    output$plot = renderPlotly({
      plotting_fun(logTransformation = logTransformation, transition_distances, vals)
      
    }) # output$plot = renderPlot({
    
    
    output$plot_res = renderPlotly({
      linear_regression(transition_distances, order=order, logTransformation = logTransformation)$x%>%
        plotting_residuals(transition_distances, vals, .)
      
    }) # output$plot = renderPlot({
    
    output$lm=renderTable({
      expr =linear_regression(transition_distances, order=order, logTransformation = logTransformation)$statistics
      
    }) # output$plot = renderTable({
    
    output$output=renderTable({
      expr =linear_regression(transition_distances, order=order, logTransformation = logTransformation)$output
      
    }) # output$plot = renderTable({
    
    
    
    output$hover<-renderPrint({
      hover_data<-event_data("plotly_hover", source = "plot")
      transition_distances[which(transition_distances$Key==hover_data$key),]
    })
    
    # Toggle points that are brushed, when button is clicked
    observeEvent(input$exclude_toggle, {
      selected_data<-event_data("plotly_selected", source = "plot")
      
      res<- data.frame(selected_=rep(FALSE, nrow(transition_distances)))
      res$selected_[which(transition_distances$Key %in% selected_data$key)]<-TRUE
      
      res_res<- data.frame(selected_=rep(FALSE, nrow(transition_distances)))
      res_res$selected_[which(transition_distances$Key %in%selected_data$key)]<-TRUE
      
      if(sum(res$selected_)>0)     vals$keeprows <- xor(vals$keeprows, res$selected_)#the ones that are set to true in res are set to false
      else                         vals$keeprows <- xor(vals$keeprows, res_res$selected_)
    })
    
    # Reset all points
    observeEvent(input$exclude_reset, {
      vals$keeprows <- rep(TRUE, nrow(transition_distances))
    })
    
    #Order linear regression
    observeEvent(input$order, {
      order= input$order
    })
    
  }) # observeEvent(input$start, {
}) # shinyServer(function(input, output) {

