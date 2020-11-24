library(plotly)

shinyUI(
  fluidPage(title="TreeDist",
            tabsetPanel(
              tabPanel("Transitions",
                       sidebarLayout(
                         sidebarPanel(
                           width=3,
                           tags$h3("TreeDist"),
                           p(""),
                           br(),
                           #Possibly make the app applicable for different tree file types
                           # fluidRow(
                           #   column(12, 
                           #     selectInput(
                           #       inputId = "file_type",
                           #       label = "Select Tree File Type:",
                           #       choices = c(
                           #         "Tree" = "tree",  
                           #         "Beast" = "beast",  
                           #         # "CodeML" = "codeml",
                           #         "CodeML mlc" = "mlc", 
                           #         # "HYPHY" = "hyphy", 
                           #         "jplace" = "jplace", 
                           #         "MrBayes" = "mrbayes", 
                           #         "NHX" = "nhx", 
                           #         "rst (CODEML/BASEML)" = "rst", 
                           #         "phylip" = "phylip", 
                           #         # "r8s" = "r8s",
                           #         "RAxML" = "raxml"
                           #       ),
                           #       selected = "tree"
                           #     )
                           #   )
                           #   ),
                           fluidRow(
                             column(12,
                                    fileInput("tree_file_wo_transitions", label=("Tree file: No transitions"))
                             )
                           ),
                           fluidRow(
                             column(12,
                                    fileInput("tree_file_w_transitions", label=("Tree file:  Including transitions"))
                             )
                           ),
                           fluidRow(
                             column(12,
                                    fileInput("distances_file", label=("Distance matrix"))
                             )
                           ),
                           fluidRow(
                             column(12,
                                    selectInput(inputId ="delimiter", "Delimiter", 
                                                c("comma"=",","tab"="\t"))
                             )
                           ),
                           fluidRow(
                             column(12,
                                    fileInput("sampling_locations", label=("Sampling locations"))
                             )
                           ),
                           fluidRow(
                             column(12,
                                    selectInput(inputId ="Annotation_State", "Annotation Label in tree", c("host","state", "states","city"))
                             )
                           ),
                           fluidRow(
                             column(12,
                                    radioButtons(inputId ="Symmetrie", "Make matrix symmetric?",  c("No"=FALSE, "Yes"=TRUE))
                             )
                           ),
                           fluidRow(
                             column(12,
                                    radioButtons(inputId ="LogTransform", "Log transform distance metric?", c("No"=FALSE, "Yes"=TRUE))
                             )
                           ),
                           fluidRow(
                             column(12,
                                    radioButtons(inputId ="Reconstruction_Method", "AR Method", 
                                                 c("Maximum Parsimony"="MP","Maximum Likelihood"="ML"))
                             )
                           ),
                           fluidRow(
                             column(12,
                                    actionButton("start", label=h4("RUN"), col.label="red")
                             )
                           )
                         ),
                         mainPanel(
                           fluidRow(
                             splitLayout(
                               plotlyOutput(outputId="plot"),
                               plotlyOutput(outputId="plot_res")
                             ),
                             actionButton("exclude_toggle", "Toggle points"),
                             actionButton("exclude_reset", "Reset"),
                           ),
                           fluidRow(
                             verbatimTextOutput("hover"),
                           ),
                           fluidRow(
                             tableOutput(outputId="lm")
                           ),
                           fluidRow(
                             verbatimTextOutput(outputId="lm.summary")
                           ),
                           fluidRow(
                             tableOutput(outputId="output")
                           )
                         )	
                       )
              ),#tabPanel
              tabPanel(title = "Explore Tree",
                      uiOutput("select_node_render"),
                       fluidRow(
                         uiOutput(
                           "subtree_render"
                         )
                       )
              )
            )#)tabsetPanel
  )#)fluidpage
)#)ShinyUI
