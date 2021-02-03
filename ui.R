#Univariate ####
##Sidebar####
shinyUI(fluidPage(
  title = "TreeDist",
  tabsetPanel(tabPanel(
    "Univariate",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$h3("TreeDist"),
        tags$h4("Univariate"),
        tags$h4("Input controls:"),
        
        fluidRow(column(
          12,
          radioButtons(
            inputId = "annotations",
            "Is the tree annotated?",
            c(
              "No, please annotate my tree" = FALSE,
              "Yes, I took care of this!" = TRUE
            )
          )
        )),
        wellPanel("Annotations",
                  conditionalPanel(condition = "input.annotations=='TRUE'",
                                   fluidRow(column(
                                     12,
                                     selectInput(
                                       inputId = "Annotation_State",
                                       "Annotation Label in tree",
                                       c("host", "state", "states", "city", "location.states")
                                     )
                                   ))),
                  conditionalPanel(condition = "input.annotations=='FALSE'",
                                   fluidRow(column(
                                     12,
                                     radioButtons(
                                       inputId = "Reconstruction_Method",
                                       "AR Method",
                                       c(
                                         "Maximum Parsimony" = "MP",
                                         "Maximum Likelihood" = "ML")))))),
       
        fluidRow(column(12,
                        fileInput(
                          "tree_file", label = ("Tree file")
                        ))),
        fluidRow(column(
          12,
          selectInput(
            inputId = "file_type",
            label = "Select Tree File Type:",
            choices = c("Beast" = "beast",
                        #"MrBayes" = "mrbayes",
                        #"phylip" = "phylip",
                        #"Nexus" = "nexus",
                        #"Newick" = "newick"),
                        selected = "beast"
            )
          ))#   selectInput(inputId = "Predictor_uni", label="Predictor", choices=c(NULL))
        ),
        fluidRow(column(12,
                        fileInput(
                          "distances_file", label = ("Distance matrix"), multiple=T
                        ))),
        fluidRow(column(
          12,
          textInput(inputId = "delimiter", "Delimiter Distance Matrices (Optional)", value ="")
        )),
        fluidRow(column(
          12,
          fileInput("sampling_locations", label ="Sampling locations")
        )),
        fluidRow(column(
          12,
          radioButtons(inputId = "Symmetrie", "Make matrix symmetric?",  c("No" =
                                                                             FALSE, "Yes" = TRUE))
        )),
        fluidRow(column(
          12,
          actionButton("start", label = h4("RUN"), col.label =
                         "red")
        ))
      ),
      ##MainPanel ####
      mainPanel(
        fluidRow(
          splitLayout(
            tags$h4("Plotly - Scatterplot"),
            tags$h4("Plot - Residuals Plot"))),
        fluidRow(
          splitLayout(
            plotlyOutput(outputId = "plot"),
            plotlyOutput(outputId = "plot_res")
          ),
          tags$h4("Regression controls:"),
          selectInput(inputId = "Predictor_uni", label="Univariate Predictor", choices=c(NULL)),
          actionButton("exclude_toggle", "Toggle points"),
          actionButton("exclude_reset", "Reset"),
          actionButton("log_transitions", "Toggle Log-Transitions"),
          actionButton("log_distances", "Toggle Log-Distance Metric")
        ),
        tags$h4("Hovering output:"),
        fluidRow(verbatimTextOutput("hover")),
        tags$h4("Basic univariate statistics:"),
        fluidRow(tableOutput(outputId = "lm")),
        tags$h4("Univariate Regression output:"),
        fluidRow(verbatimTextOutput(outputId = "lm.summary")),
        tags$h4("Possible Outliers"),
        fluidRow(tableOutput(outputId = "output"))
      )
    )),
    #Multivariate ####
    tabPanel(
      "Multivariate",
      sidebarLayout(
        ##Sidebar ####
        sidebarPanel(
          tags$h3("TreeDist"),
          tags$h4("Multivariate"),
          width = 12,
          fluidRow(
            splitLayout(
              checkboxGroupInput("variable", "Variables:",
                                 c("Updating" = "U")),
              tableOutput("data"),
              checkboxGroupInput("Log", "Log:",
                                 c("Updating" = "x")),
              tableOutput("data2")
            ))),
        ##MainPanel ####
        mainPanel(
          tags$h4("Basic Statistical Overview"),
          fluidRow(tableOutput(outputId = "lm_multi")),
          tags$h4("Multivariate Regression Model"),
          fluidRow(verbatimTextOutput(outputId = "lm.summary_multi")),
          tags$h4("Possible Outliers"),
          fluidRow(tableOutput(outputId = "output_multi")),
          tags$h4("Scatterplot per predictive variable:"),
          fluidRow(plotlyOutput(outputId = "multi_plot"))
          
        )
      )),
    #Explore Tree ####
    tabPanel(title = "Explore Tree",
             sidebarLayout(
               ##Sidebar ####
               sidebarPanel(
                 tags$h3("TreeDist"),
                 tags$h4("Tree"),
                 width=3,
                 fluidRow(
                   column(
                     12,
                     selectInput(
                       inputId = "Z_A_Tree",
                       "Zoom or Annotation Tree",
                       c("Zoom Tree (plotly)"="Z_Tree", "Annotation Tree (ggtree)"="A_Tree"))
                   )),
                 fluidRow(
                   column(
                     12,
                     checkboxInput(
                       inputId = "colour_by_states",
                       label= "Colour by States"
                     ))),
                 wellPanel("Coloured by states",
                           conditionalPanel(condition = "input.colour_by_states",
                                            fluidRow(
                                              column(
                                                12,
                                                numericInput(
                                                  inputId = "annotation_plot_legend_size",
                                                  label= "Annotation tree -  Legend text Size",
                                                  value =15
                                                ))))),
                 fluidRow(
                   column(
                     12,
                     numericInput(
                       inputId = "tree_plot_height",
                       label = "Select plot height",
                       value = 1000
                     ))),
                 useShinyjs(),
                 ###Annotation Tree only ####
                 wellPanel("Annotation Tree",
                           conditionalPanel(condition = "input.Z_A_Tree=='A_Tree'",
                                            fluidRow(
                                              column(
                                                12,
                                                checkboxInput(
                                                  inputId = "ancestral_states",
                                                  label= "Ancestral states"
                                                ))),
                                            wellPanel("Ancestral states options",
                                                      conditionalPanel(condition = "input.ancestral_states",
                                                                       fluidRow(
                                                                         column(
                                                                           12,
                                                                           numericInput(
                                                                             inputId = "ancestral_states_size",
                                                                             label= "Ancestral states Size",
                                                                             value =3
                                                                           ))))),
                                            uiOutput("select_node_render"),        
                                            fluidRow(
                                              column(
                                                12,
                                                checkboxInput(
                                                  inputId = "tip_labels",
                                                  label= "Tip label"
                                                ))),
                                            wellPanel("Tip label options",
                                                      conditionalPanel(condition = "input.tip_labels",
                                                                       fluidRow(
                                                                         column(
                                                                           12,
                                                                           numericInput(
                                                                             inputId = "tree_text_size",
                                                                             label = "Tip label size:",
                                                                             min = 0,
                                                                             value = 3
                                                                           ))))),
                                            fluidRow(
                                              column(
                                                12,
                                                selectizeInput(
                                                  inputId = "select_layout",
                                                  label = "Annotation tree - Select Layout:",
                                                  choices = c("rectangular", 
                                                              "slanted",
                                                              "fan", 
                                                              "circular", 
                                                              "radial", 
                                                              "unrooted", 
                                                              "equal_angle", 
                                                              "daylight"
                                                  ),
                                                  width = "100%"
                                                ))),
                                            fluidRow(
                                              column(
                                                12,
                                                checkboxInput(
                                                  inputId = "node_number",
                                                  label= "Node number"
                                                ))),
                                            wellPanel("Node numberoptions",
                                                      conditionalPanel(condition = "input.node_number",
                                                                       fluidRow(
                                                                         column(
                                                                           12,
                                                                           numericInput(
                                                                             inputId = "node_number_size",
                                                                             label= "Node number size",
                                                                             value = 3
                                                                           ))))),
                                            fluidRow(
                                              column(
                                                12,
                                                checkboxInput(
                                                  inputId = "tip_shapes",
                                                  label= "Annotation tree - Tip shapes"
                                                ))),
                                            fluidRow(
                                              column(
                                                12,
                                                checkboxInput(
                                                  inputId = "node_shapes",
                                                  label= "Node shapes"
                                                )))
                           )#)wellPanel
                 )#)conditionalPanel
               ),#)sidebarPanel
               ## MainPanel ####
               mainPanel(fluidRow(
                 useShinyjs(),
                 wellPanel("Tree",
                           conditionalPanel(condition = "input.Z_A_Tree=='Z_Tree'",
                                            uiOutput(outputId = "plotly_ui")
                           ),
                           conditionalPanel(condition= "input.Z_A_Tree=='A_Tree'",
                                            uiOutput(outputId = "plot_ui")
                           )
                 )#)wellpanel
               )#)fluidrow
               )#)mainPanel
             )#)SidebarLayout
    )#)TabPanel
    
    
  )#)tabsetPanel
)#)fluidpage
)#)ShinyUI
