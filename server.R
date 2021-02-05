options(shiny.maxRequestSize=10*1024^2) #10mb max file size

#' Shiny server is doing the back-end work for the treedist application. It consists of 5 files in total. This server.R file itsels, 3 files that are sourced upon startup of the app
#' (Functions.R, Multivariate.R, AncestralReconstruction.R) and the Tree.R file which is encapsulated within the "RUN" observer.
#' @param input
#' @param output
#' @param session
shinyServer(function(input, output, session) {
  reactlog_enable() #logging a reactivity tree
  cdata <- session$clientData #https://community.rstudio.com/t/controlling-the-height-of-fluidrow-in-shiny/4968/3
  #the cdata is used to set the size of the facet wrap plot for multivariate Plotly
  
  #Objects in this file are defined in each session
  #These documents are outside the actionbutton "RUN", everything outside of "RUN" canbe updated without doing all calculations again. 
  #When we need to add new distance matrices, another tree or another sampling_location file, then "RUN" is needed.
  source("Functions.R", local=T)
  source("Multivariate.R", local=T)
  source("AncestralReconstruction.R", local=T)
  
  #This observer creates the list:
  ##file types depending on whether the tree is already annotated.
  ##extracts the names of the covariates (the distance matrices) that are uploaded by the user. And then the selectInput widget for univariate predictor and the checkbox list for the
  ##multivariate predictor are updated.
  observe({
    #Depending on choice of annotations included or not in the tree different file types can be selected.
    if(input$annotations==FALSE){
      updateSelectInput(
        session,
        inputId= "file_type",
        choices = c("Nexus" = "nexus",
                    "Newick" = "newick"),
        selected = "nexus"
      )
    }else{
      updateSelectInput(
        session,
        inputId= "file_type",
        choices = c("Beast" = "beast"),
        selected = "beast"
      )
    }
    #For the univariate dropdown list of predictors.
    column_names<-as.vector(sapply(input$distances_file$name, first.word))
    updateSelectInput(
      session,
      inputId= "Predictor_uni",
      choices = column_names,
      selected = column_names[1]    
    )
    #Multivariate checkbox group with the same names of the covariates as for the univariat analysis.
    updateCheckboxGroupInput(session, 
                             inputId= "variable", 
                             label = "Variables:", 
                             choices = column_names,
                             selected = column_names)
  })         
  
  observeEvent(input$  uni_regression_output, {
    shinyjs::toggle(selector = "div.uni_regression_output", animType = "fade", anim=T)
  })
  #' observeEvent (RUN) that triggers the import of the files, the calculation of transition matrix and plotting of all plots.
  #' @param input$start Actionbutton that is pressed by the user triggers the execution of the code within this observer
  observeEvent(input$start, {
    #show all elements of type "div" that have the html class "regression.control"
    #Within the UI a range of these divs are hidden in the univariate tab to make the page look cleaner.
    shinyjs::show(selector = "div.regression_control", animType = "fade", anim=T)
    
    tree_file = input$tree_file$datapath
    #tree_file = "input/rabies/batRABV.MCC.keep.target.heights.trees"
    sampling_locations = input$sampling_locations$datapath
    #sampling_locations = "input/rabies/hostnames.txt"
    distances_raw_file<-input$distances_file$datapath 
    #for multivariate case this is a vector of strings, 
    #each string is the temp variable for the distance matrix
    
    #For the univariate dropdown list of predictors.
    #Multivariate checkbox group with the same names of the covariates as for the univariat analysis.
    column_names<-sapply(input$distances_file$name, first.word)
    
    #distances_raw is a list of the actual distance matrices, the length of the list is the number of seected matrices
    distances_raw<-lapply(distances_raw_file, function(distances_raw_file) importingDist(distances_raw_file, input$delimiter))
    #distances_raw_file<-"input/rabies/predictors/bodySize.csv"
    

    if(input$annotations){
      state= input$Annotation_State
    }else{
      state="states"
    }
    
    tip_states_tree<-importingTree(sampling_locations, tree_file, input$file_type)
    tree<-tip_states_tree[[2]]
    tip_states<-as.factor(tip_states_tree[[1]])
    
    if (input$annotations==FALSE){
      tree<- chooseReconstructionMethod(tip_states,  tree_not_annotated=tree)
    }
    transitions<-GenerateRawTransitionMatrix(state, distances_raw[[1]], tree) 
    #take any distance matrix to get the col names and dimensions for the transitions matrix
    transition_distances<-GenerateFinal_Transitions_Distances(transitions_raw=transitions, distances_raw=distances_raw, column_names=column_names)
    
    log_validated<-log_choices(transition_distances)

    vals <- reactiveValues(keeprows = rep(TRUE, nrow(transition_distances)))
    logs<-reactiveValues(logtransform= rep(FALSE, 2))
    #whenever this variable "vals" changes then all dependencies, expressions that
    #contain vals is called
    #reactiveValues are eager, and this would not work using reactive, unless I add an observer
    #around reactive which makes it eager as well
    
    # Multivariate ####
    ## Plot ####
    output$multi_plot = renderPlotly({
      plotting_muĺti(transition_distances, vals, clientData=cdata)
    }) # output$plot = renderPlot({
    
    ## Glance #######
    output$lm_multi=renderTable({
      glance(lm_multi(transition_distances,cut_off_residual=NULL, percentile=95, vals)$lm)
    }) # output$plot = renderTable({
    
    # ## Output ####
    # output$output_multi=renderTable({
    #   lm_multi(transition_distances,cut_off_residual=NULL, percentile=95, vals)$output
    # }) # output$plot = renderTable({
    # 
    output$lm.summary_multi=renderPrint({
      summary(lm_multi(transition_distances,cut_off_residual=NULL, percentile=95, vals)$lm)
    }) # output$plot = renderTable({
    
    # Univariate ####
    
    output$plot = renderPlotly({
      plotting_fun(transition_distances, logs,vals)
    }) # output$plot = renderPlot({
    
    output$plot_res = renderPlotly({
      linear_regression(transition_distances,logs=logs, vals=vals)$x%>%
        plotting_residuals(transition_distances, vals, .)
      
    }) # output$plot = renderPlot({
    
    output$lm=renderTable({
      glance(linear_regression(transition_distances, logs=logs, vals=vals)$lm)
      
    }) # output$plot = renderTable({
    
    output$lm.summary=renderPrint({
      summary(linear_regression(transition_distances=transition_distances, logs=logs, vals=vals)$lm)
      
    }) # output$plot = renderTable({
    
    # output$output=renderTable({
    #   linear_regression(transition_distances=transition_distances,logs=logs, vals=vals)$output
      
    #}) # output$plot = renderTable({
    
    # output$hover<-renderPrint({
    #   hover_data<-event_data("plotly_hover", source = "plot")
    #   transition_distances[which(transition_distances$Key==hover_data$key),]
    # })
    # 
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
    
    # The if clause below, checks if the location where ggtree is checking for the edge.length (tree@phylo$edge.length) is null
    #if it is null then the conversion from data.frame/tibble to treedata object did not work correctly and the edge.length has been written to
    #tree@data$edge.length, and in our case the root node has an associated edge.length of "NA". So I drop that value and then assign the edge.length data
    #to the location where ggtree expects it and the tree is plotted correctly.
    
    #I confirmed this by assigning the edge.length from na.omit(tree@data$edge.length) to a global variable when running the app with the annotated tree
    #in a next step the app can be run using the browser() function below with the non-annotated version and then the edge.length can be compared via the
    #following line of code, which can be entered in the console:
    #all.equal(x_global, tree$phylo$edge.length, check.attributes=F) 
    #check attributes false becasue x_global has attributes such as "na.action"=1368, which is the root node which was found to be NA.
    
    #browser() #uncomment for debugging mode
    if(is.null(tree@phylo$edge.length)){
      x=na.omit(tree@data$edge.length)
      tree@phylo$edge.length=x
      x_global <<- x
    }
    source("Tree.R", local=T)#only sourced when "run" is clicked and after variable tree is defined
    
  }) # observeEvent(input$start, {
  
}) # shinyServer(function(input, output) {

