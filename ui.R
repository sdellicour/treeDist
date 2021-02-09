#Univariate ####
##Sidebar####

shinyUI(fluidPage(
  shinyjs::useShinyjs(), #activate Shinyjs
                  tabsetPanel(tabPanel(
                    "Univariate analysis",
                    headerPanel(
                      list(HTML('<img src=Logo_Lemey_Lab.jpeg height=100 width=100/>'),
                           "TreeDist"),
                      windowTitle="TreeDist"
                    ),
                    sidebarLayout(
                      tags$div(class="sidebar", id="sidebar",
                               sidebarPanel(
                                 width = 3,
                                 tags$div(class="tree_file",
                                 tags$h4("Univariate analysis"),
                                 tags$h4("Input controls:"),
                                 fluidRow(column(12,
                                                 fileInput(
                                                   "tree_file", label = ("Tree file")
                                                 )))),
                                 fluidRow(column(
                                   12,
                                   selectInput(
                                     inputId = "file_type",
                                     label = "Select tree file type:",
                                     choices = c("Beast" = "beast",
                                                 #"MrBayes" = "mrbayes",
                                                 #"phylip" = "phylip",
                                                 #"Nexus" = "nexus",
                                                 #"Newick" = "newick"),
                                                 selected = "beast"
                                     )
                                   ))
                                 ),
                                 tags$div(class="distance_matrix_input",
                                          fluidRow(column(12,
                                                          fileInput(
                                                            "distances_file", label = ("Distance matrix"), multiple=T
                                                          ))),
                                 fluidRow(column(
                                   12,
                                   textInput(inputId = "delimiter", "Delimiter distance matrices (optional)", value ="")
                                 ))),
                                 fluidRow(column(
                                   12,
                                   radioButtons(
                                     inputId = "annotations",
                                     label = "Is the tree annotated?",
                                     choices = c(
                                       "No, please annotate my tree" = FALSE,
                                       "Yes, I took care of this!" = TRUE)
                                   )
                                 )),
                                 wellPanel("Annotations",
                                           conditionalPanel(condition = "input.annotations=='TRUE'",
                                                            fluidRow(column(
                                                              12,
                                                              selectInput(
                                                                inputId = "Annotation_State",
                                                                "Annotation label in tree",
                                                                #c("host", "state", "states", "city", "location.states")
                                                                NULL
                                                              )
                                                            ))),
                                           conditionalPanel(condition = "input.annotations=='FALSE'",
                                                            fluidRow(column(
                                                              12,
                                                              radioButtons(
                                                                inputId = "Reconstruction_Method",
                                                                "AR Method",
                                                                c(
                                                                  "Maximum parsimony" = "MP",
                                                                  "Maximum likelihood" = "ML")))))),
                                 shinyjs::hidden(
                                 tags$div(id="missings",
                                          fluidRow(column(
                                            12,
                                            checkboxGroupInput(inputId = "missings",
                                                               label ="Missings", 
                                                              choices = "", 
                                                              selected = "")
                                          ))
                                 )),
                                 tags$div(id="tag_sampling_locations",
                                 fluidRow(column(
                                   12,
                                   fileInput("sampling_locations", label ="Sampling locations")
                                 ))
                                 ),
                                 fluidRow(column(
                                   12,
                                   radioButtons(inputId = "Symmetrie", "Make matrix symmetric?",  c("No" =
                                                                                                      FALSE, "Yes" = TRUE))
                                 )),
                                 tags$div(class="run",
                                          fluidRow(
                                            column(6,actionButton("start", label = h4("RUN"), col.label ="red")),
                                            column(6,actionButton("reset", label = h4("RESET"), col.label ="red"))
                                            
                                          ))
                               )),
                      ##MainPanel ####
                      mainPanel(
                        fluidRow(
                          shinyjs::hidden(
                            tags$div(class="regression_control",
                                     conditionalPanel(condition = "input.Scatter_residual=='scatter'",
                                                      tags$h4("Plotly - scatterplot"),
                                                      plotlyOutput(outputId = "plot")
                                     ),
                                     conditionalPanel(condition= "input.Scatter_residual=='residuals'",
                                                      tags$h4("Plot - residuals plot"),
                                                      plotlyOutput(outputId = "plot_res")
                                     )))),
                        shinyjs::hidden(
                          tags$div(class="regression_control",
                                   wellPanel(
                                     fluidRow(
                                       column(
                                         6,
                                         selectInput(
                                           inputId = "Scatter_residual",
                                           "Scatter, residual plot selection",
                                           c("Scatter plot (plotly)"="scatter", "Residuals plot"="residuals"))
                                       ),
                                       column(6,selectInput(inputId = "Predictor_uni", label="Univariate predictor", choices=c(NULL))),
                                     ),
                                     fluidRow(
                                       tags$h4("Plot layout controls:"),
                                       column(4,numericInput(inputId = "stroke",label= "Stroke thickness", value=0.2,min = 0, step = 0.1)),
                                       column(4,numericInput(inputId = "size",label= "Point size", value=3, min = 0, step = 0.25 )),
                                       column(4,numericInput(inputId = "alpha",label= "Shading", value=0.5, min = 0, max = 1, step = 0.05))
                                     ),
                                     fluidRow(
                                       conditionalPanel(condition = "input.Scatter_residual=='scatter'",
                                                        column(3,actionButton("exclude_toggle", "Toggle points")),
                                                        column(3,actionButton("exclude_reset", "Reset")),
                                                        column(3,actionButton("log_transitions", "Toggle Log-Transitions")),
                                                        column(3,actionButton("log_distances", "Toggle Log-Distance Metric"))
                                       )),
                                     fluidRow(
                                       conditionalPanel(condition = "input.Scatter_residual=='scatter'",
                                                        tags$h4("Regression line controls:"),
                                                        column(4,checkboxInput(inputId = "regression_line",label= "Regression line")),
                                                        column(4, conditionalPanel(condition = "input.regression_line",
                                                                                   radioButtons(inputId = "se",
                                                                                                label= "Show Confidence Interval", 
                                                                                                choices = c(TRUE, FALSE), 
                                                                                                selected = FALSE))),
                                                        column(4, conditionalPanel(condition = "input.regression_line && input.se=='TRUE'",
                                                                                   numericInput(inputId = "level",
                                                                                                label= "Confidence level - shaded area",
                                                                                                value = 0.95, min = 0, max = 1, step = 0.05)))
                                       ))))),
                        #tags$h4("Hovering output:"),
                        #fluidRow(verbatimTextOutput("hover")),
                        shinyjs::hidden(
                          tags$div(class="regression_control",
                                   wellPanel(
                                     fluidRow(
                                       column(width=4, tags$h4("Univariate regression analysis:")),
                                       column(offset = 6, width = 2, checkboxInput(inputId = "uni_regression_output",label= "Show"))
                                     ),
                                     fluidRow(
                                       tags$div(class="uni_regression_output",
                                                tableOutput(outputId = "lm"))),
                                     fluidRow(
                                       tags$div(class="uni_regression_output",
                                                verbatimTextOutput(outputId = "lm.summary")))
                                     
                                   )))))),
                    #tags$h4("Possible outliers:"),
                    #fluidRow(tableOutput(outputId = "output"))
                    
                    #Multivariate ####
                    tabPanel(
                      "Multivariate analysis",
                      headerPanel(
                        list(HTML('<img src="Logo_Lemey_Lab.jpeg" height=100 width=100/>'), "TreeDist"),
                        windowTitle="TreeDist multivariate analysis"
                      ),
                      ##MainPanel ####
                      mainPanel(
                        width = 12,
                        wellPanel(
                          fluidRow(
                            column(width=4, tags$h4("Multivariate input control:")),
                            column(offset = 6, width = 2, checkboxInput(inputId = "multi_input_control",label= "Show"))
                          ),
                          fluidRow(
                            tags$div(class="multi_input_control",
                                     splitLayout(
                                       checkboxGroupInput("variable", "Variables:",
                                                          c("Updating" = "U")),
                                       uiOutput("log")
                                     )))),
                        wellPanel(
                          fluidRow(
                            column(width=4, tags$h4("Multivariate regression analysis:")),
                            column(offset = 6, width = 2, checkboxInput(inputId = "multi_regression_output",label= "Show"))
                          ),
                          tags$div(class="multi_regression_output",
                                   tags$h4("Basic statistical overview:"),
                                   fluidRow(tableOutput(outputId = "lm_multi")),
                                   tags$h4("Multivariate regression model:"),
                                   fluidRow(verbatimTextOutput(outputId = "lm.summary_multi"))
                          )),
                        wellPanel(
                          fluidRow(
                            column(width=4, tags$h4("Scatterplot per predictive variable:")),
                            column(offset = 6, width = 2, checkboxInput(inputId = "multi_plot_output",label= "Show"))
                          ),
                          tags$div(class="multi_plot_output",
                                   fluidRow(plotlyOutput(outputId = "multi_plot"))
                          ))
                      )
                    ),
                    #Explore Tree ####
                    tabPanel(title = "Explore tree",
                             headerPanel(
                               list(HTML('<img src="Logo_Lemey_Lab.jpeg" height=100 width=100/>'), "TreeDist"),
                               windowTitle="TreeDist tree exploration"
                             ),
                             sidebarLayout(
                               ##Sidebar ####
                               sidebarPanel(
                                 tags$h4("Tree"),
                                 width=3,
                                 fluidRow(
                                   column(
                                     12,
                                     selectInput(
                                       inputId = "Z_A_Tree",
                                       "Zoom or annotation tree",
                                       c("Zoom Tree (plotly)"="Z_Tree", "Annotation Tree (ggtree)"="A_Tree"))
                                   )),
                                 fluidRow(
                                   column(
                                     12,
                                     checkboxInput(
                                       inputId = "colour_by_states",
                                       label= "Colour by states"
                                     ))),
                                 wellPanel("Coloured by states",
                                           conditionalPanel(condition = "input.colour_by_states",
                                                            fluidRow(
                                                              column(
                                                                12,
                                                                numericInput(
                                                                  inputId = "annotation_plot_legend_size",
                                                                  label= "Annotation tree -  Legend text size",
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
                                                                                             label= "Ancestral states size",
                                                                                             value =3
                                                                                           ))))),
                                                            ### Annotation tree - Selection of internal node #####
                                                            ### This ui is generated in the tree.R file, so server side
                                                            ### It is used to select the root node of the subtree that
                                                            ### will be displayed when selected.
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
                                                                  label = "Select layout:",
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
                                                            wellPanel("Node number options",
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
                                                                  label= "Annotation tree - tip shapes"
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