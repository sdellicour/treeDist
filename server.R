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
  
  #Session variables
  transition_distances<-NULL 
  vals<-NULL
  #this variable assignment here, is then found by R when the actual value is assigned using the
  #"<<-" operator which goes up through the environments to assign this value, we need to declare it here
  #to keep the variable at session level. otherwise the variable will be global and available across sessions, for all users using
  #the app at the same time. 
  
  #reactive observe combination to make the recognition of state eager
  state<-reactive({
    req(input$annotations)
    if(input$annotations){
      state= input$Annotation_State
    }else{
      state="states"
    }
  })
  
  #the names of the distance matrices
  column_names<-reactive({
    req(state())
    sapply(input$distances_file$name, first.word)
  })
  
  tip_states<-reactive({
    sampling_locations = input$sampling_locations$datapath
    #sampling_locations = "input/rabies/hostnames.txt"
    
    tip_states<-as.factor(importingSamplingLocations(sampling_locations))
    tip_states
  })
  
  tree<-reactive({
    req(tip_states())
    tree_file = input$tree_file$datapath
    #tree_file = "input/rabies/batRABV.MCC.keep.target.heights.trees"
    tree_not_annotated<-importingTree(tree_file, input$file_type)
    
    if (input$annotations==FALSE){
      tree_annotated<- chooseReconstructionMethod(tip_states(),  tree_not_annotated)
      tree<-tree_annotated
    }else{
      tree<-tree_not_annotated
    }
    tree
  })

  #distances_raw is a list of the actual distance matrices, the length of the list is the number of seected matrices
  distances_raw<-reactive({
    #distances_raw_file<-"input/rabies/predictors/bodySize.csv"
    lapply(input$distances_file$datapath , function(distances_raw_file) importingDist(distances_raw_file, input$delimiter))
  })
  
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
    updateSelectInput(
      session,
      inputId= "Predictor_uni",
      choices = column_names(),
      selected = column_names()[1]    
    )
    #Multivariate checkbox group with the same names of the covariates as for the univariat analysis.
    updateCheckboxGroupInput(session, 
                             inputId= "variable", 
                             label = "Variables:", 
                             choices = column_names(),
                             selected = column_names())
  })         
  



  logs<-reactiveValues(logtransform= rep(FALSE, 2))  #whenever this variable "vals" changes then all dependencies, expressions that
  #contain vals is called
  #reactiveValues are eager, and this would not work using reactive, unless I add an observer
  #around reactive which makes it eager as well
  
  #Objects in this file are defined in each session
  #These documents are outside the actionbutton "RUN", everything outside of "RUN" canbe updated without doing all calculations again. 
  #When we need to add new distance matrices, another tree or another sampling_location file, then "RUN" is needed.
  source("Functions.R", local=T)
  source("Multivariate.R", local=T)
  source("AncestralReconstruction.R", local=T)
  source("Univariate.R", local=T)
  source("Tree.R", local=T)
  
  observeEvent(input$uni_regression_output, {
    shinyjs::toggle(selector = "div.uni_regression_output", animType = "fade", anim=T)
  })
  
  #' observeEvent (RUN) that triggers the import of the files, the calculation of transition matrix and plotting of all plots.
  #' @param input$start Actionbutton that is pressed by the user triggers the execution of the code within this observer
  observeEvent(input$start, {
    req(state())
    #show all elements of type "div" that have the html class "regression.control"
    #Within the UI a range of these divs are hidden in the univariate tab to make the page look cleaner.
    shinyjs::show(selector = "div.regression_control", animType = "fade", anim=T)
    transitions<-GenerateRawTransitionMatrix(distances_raw()[[1]], tree=tree()) 
    #take any distance matrix to get the col names and dimensions for the transitions matrix
    transition_distances <<- GenerateFinal_Transitions_Distances(transitions_raw=transitions, distances_raw=distances_raw())
    vals <<- reactiveValues(keeprows = rep(TRUE, nrow(transition_distances)))    
  }) # observeEvent(input$start, {
  
}) # shinyServer(function(input, output) {

