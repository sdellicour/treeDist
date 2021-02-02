options(shiny.maxRequestSize=10*1024^2)#10mb max file size
shinyServer(function(input, output, session) {
  reactlog_enable()#logging a reactivity tree
  
  cdata <- session$clientData #https://community.rstudio.com/t/controlling-the-height-of-fluidrow-in-shiny/4968/3
  #the cdata is used to set the sitze of the facet wrap plot for plotly
  
  # Objects in this file are defined in each session
  source("Functions.R", local=T)
  source("Multivariate.R", local=T)
  source("AncestralReconstruction.R", local=T)
  
  
  # update the ui depending on the choice of annotated tree
  # Observer####
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
        choices=c("host", "state", "states", "city", "location.states"),
        selected="host"
      )
    }
    
    distances_raw_file_names = input$distances_file$name

    column_names<-as.vector(sapply(distances_raw_file_names, first.word))
    updateSelectInput(
      session,
      inputId= "Predictor_uni",
      choices = column_names,
      selected = column_names[1]    
    )
    
    updateCheckboxGroupInput(session, 
                             inputId= "variable", 
                             label = "Variables:", 
                             choices = column_names,
                             selected = column_names)
  })         
  

  #observeEvent (RUN) ####
  observeEvent(input$start, {
    Log_multi=input$Log
    variable_multi=input$variable
    distances_raw_file_names = input$distances_file$name
    column_names<-sapply(distances_raw_file_names, first.word)
    Predictor= input$Predictor_uni
    annotated= input$annotations
    file_type = input$file_type
    tree_file = input$tree_file$datapath
    #tree_file = "input/rabies/batRABV.MCC.keep.target.heights.trees"
    sampling_locations = input$sampling_locations$datapath
    #sampling_locations = "input/rabies/hostnames.txt"
    method=input$Reconstruction_Method
    #Transitions
    delimiter<-input$delimiter
    browser()
    distances_raw_file<-input$distances_file$datapath #for multivariate case this is a vector of strings, 
    #each string is the temp variable for the distance matrix
    
    #distances_raw_file<-"input/rabies/predictors/bodySize.csv"
    state= input$Annotation_State
    makeSymmetric=input$Symmetrie
    tip_states_tree<-importingTree(sampling_locations, tree_file, file_type)
    tree<-tip_states_tree[[2]]
    tip_states<-as.factor(tip_states_tree[[1]])
    
    distances_raw<-lapply(distances_raw_file, function(distances_raw_file) importingDist(distances_raw_file, delimiter))
    #distances_raw is a list of the actual distance matrices, the length of the list is the number of seected matrices
    if (annotated==FALSE){
      tree<- chooseReconstructionMethod(method, tip_states,  tree_not_annotated=tree)
    }
    transitions<-GenerateRawTransitionMatrix(state, distances_raw[[1]], tree) 
    #take any distance matrix to get the col names and dimensions for the transitions matrix
    
    transition_distances<-GenerateFinal_Transitions_Distances(makeSymmetric = makeSymmetric, transitions_raw=transitions, distances_raw=distances_raw, column_names=column_names)
    
    vals <- reactiveValues(keeprows = rep(TRUE, nrow(transition_distances)))
    logs<-reactiveValues(logtransform= rep(FALSE, 2))
    #whenever this variable "vals" changes then all dependencies, expressions that
    #contain vals is called
    #reactiveValues are eager, and this would not work using reactive, unless I add an observer
    #around reactive which makes it eager as well
    
    # Multivariate ####
    ## Plot ####
    output$multi_plot = renderPlotly({
      plotting_muĺti(transition_distances, vals, variable_multi=variable_multi, Log_multi=Log_multi, clientData=cdata)
    }) # output$plot = renderPlot({
    
    ## Glance #######
    output$lm_multi=renderTable({
      glance(lm_multi(transition_distances,cut_off_residual=NULL, percentile=95, vals, variable_multi=variable_multi, Log_multi=Log_multi)$lm)
    }) # output$plot = renderTable({
    
    ## Output ####
    output$output_multi=renderTable({
      lm_multi(transition_distances,cut_off_residual=NULL, percentile=95, vals, variable_multi=variable_multi, Log_multi=Log_multi)$output
    }) # output$plot = renderTable({
      
    output$lm.summary_multi=renderPrint({
      summary(lm_multi(transition_distances,cut_off_residual=NULL, percentile=95, vals, variable_multi=variable_multi, Log_multi=Log_multi)$lm)
    }) # output$plot = renderTable({
    
    # Univariate ####
    
    output$plot = renderPlotly({
      plotting_fun(transition_distances, logs,vals, Predictor)
    }) # output$plot = renderPlot({
    
    output$plot_res = renderPlotly({
      linear_regression(transition_distances,logs=logs, vals=vals, Predictor=Predictor)$x%>%
        plotting_residuals(transition_distances, vals, .)
      
    }) # output$plot = renderPlot({
    
    output$lm=renderTable({
      glance(linear_regression(transition_distances, logs=logs, vals=vals, Predictor=Predictor)$lm)
      
    }) # output$plot = renderTable({
    
    output$lm.summary=renderPrint({
      summary(linear_regression(transition_distances=transition_distances, logs=logs, vals=vals, Predictor=Predictor)$lm)
      
    }) # output$plot = renderTable({
    
    output$output=renderTable({
      linear_regression(transition_distances=transition_distances,logs=logs, vals=vals, Predictor=Predictor)$output
      
    }) # output$plot = renderTable({
    
    output$hover<-renderPrint({
      hover_data<-event_data("plotly_hover", source = "plot")
      transition_distances[which(transition_distances$Key==hover_data$key),]
    })
    
    # Toggle Points #### 
    
    observeEvent(input$log_transitions, {
      logs$logtransform[1]=!logs$logtransform[1]
    })
    
    observeEvent(input$log_distances, {
      logs$logtransform[2]=!logs$logtransform[2]
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
    
  # Tree ####
    
  tree<-as.treedata(tree)
  source("Tree.R", local=T)#only sourced when "run" is clicked and after variable tree is defined
    
  }) # observeEvent(input$start, {
  
}) # shinyServer(function(input, output) {

