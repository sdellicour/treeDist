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

#source("readT.R")#  # Objects in this file are shared across all sessions in the same R process

shinyServer(function(input, output, session) {
  reactlog_enable()#logging a reactivity tree
  
  # Objects in this file are defined in each session
  source("AncestralReconstruction.R", local=T)
  source("Functions.R", local=T)
  
  #update the ui depending on the choice of annotated tree or not
  observe({
    annotated= input$annotations
    if(annotated==FALSE){
      updateSelectInput(
        session,
        inputId= "file_type",
        choices = c("Nexus" = "nexus",
                    "Newick" = "newick"),
        selected = "nexus"
      )
      updateSelectInput(
        session,
        inputId = "Annotation_State",
        choices = "states",
        selected = "states"
      )
    }else{
      updateSelectInput(
        session,
        inputId= "file_type",
        choices = c("Beast" = "beast"),
        selected = "beast"
      )
      updateSelectInput(
        session,
        inputId = "Annotation_State",
        choices=c("host", "state", "states", "city"),
        selected="host"
      )
    }
  })         
  
  observeEvent(input$start, {
    annotated= input$annotations
    file_type = input$file_type
    tree_file = input$tree_file$datapath
    #tree_file = "input/batRABV.MCC.keep.target.heights.trees"
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
    tip_states_tree<-importingTree(sampling_locations, tree_file, file_type)
    tree<-tip_states_tree[[2]]
    tip_states<-tip_states_tree[[1]]
    
    distances_raw<-as.matrix(importingDist(distances_raw_file, delimiter))
    if (annotated==FALSE){
      tree<- chooseReconstructionMethod(method, tip_states,  tree_not_annotated=tree)
    }
    transition_distances<-GenerateRawTransitionMatrix(state, distances_raw, tree)%>%
      GenerateFinal_Transitions_Distances(makeSymmetric = makeSymmetric, . )
    vals <- reactiveValues(keeprows = rep(TRUE, nrow(transition_distances)))
    #whenever this variable "vals" changes then all dependencies, expressions that
    #contain vals is called
    #reactiveValues are eager, and this would not work using reactive, unless I add an observer
    #around reactive which makes it eager as well
    
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
    
  tree<-as.treedata(tree)
  source("Tree.R", local=T)#only sourced when "run" is clicked and after variable tree is defined
    
  }) # observeEvent(input$start, {
  
}) # shinyServer(function(input, output) {

