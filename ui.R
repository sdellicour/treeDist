library(plotly)

shinyUI(fluidPage(
  title = "TreeDist",
  tabsetPanel(tabPanel(
    "Univariate",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$h3("TreeDist"),
        p(""),
        br(),
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
        fluidRow(column(12,
                        fileInput(
                          "tree_file", label = ("Tree file")
                        ))),
        #Possibly make the app applicable for different tree file types
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
          ))
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
          selectInput(
            inputId = "Annotation_State",
            "Annotation Label in tree",
            c("host", "state", "states", "city")
          )
        )),
        fluidRow(column(
          12,
          radioButtons(inputId = "Symmetrie", "Make matrix symmetric?",  c("No" =
                                                                             FALSE, "Yes" = TRUE))
        )),
        fluidRow(column(
          12,
          radioButtons(
            inputId = "Reconstruction_Method",
            "AR Method",
            c(
              "Maximum Parsimony" = "MP",
              "Maximum Likelihood" = "ML"
            )
          )
        )),
        fluidRow(column(
          12,
          actionButton("start", label = h4("RUN"), col.label =
                         "red")
        ))
      ),
      mainPanel(
        fluidRow(
          splitLayout(
            plotlyOutput(outputId = "plot"),
            plotlyOutput(outputId = "plot_res")
          ),
          actionButton("exclude_toggle", "Toggle points"),
          actionButton("exclude_reset", "Reset"),
          actionButton("log_transitions", "Toggle Log-Transitions"),
          actionButton("log_distances", "Toggle Log-Distance Metric"),
          
          selectInput(inputId = "Predictor_uni", label="Predictor", choices=c(NULL))
        ),
        fluidRow(verbatimTextOutput("hover")),
        fluidRow(tableOutput(outputId = "lm")),
        fluidRow(verbatimTextOutput(outputId = "lm.summary")),
        fluidRow(tableOutput(outputId = "output"))
      )
    )),
  tabPanel(
    "Multivariate",
      mainPanel(
        fluidRow(
          splitLayout(
          checkboxGroupInput("variable", "Variables to include in regression:",
                           c("Updating" = "U")),
        tableOutput("data"),
           checkboxGroupInput("Log", "Log",
                           c("Updating" = "x")),
        tableOutput("data2")
      )),
        fluidRow(tableOutput(outputId = "lm_multi")),
        fluidRow(verbatimTextOutput(outputId = "lm.summary_multi")),
        fluidRow(tableOutput(outputId = "output_multi")),
        fluidRow(plotlyOutput(outputId = "multi_plot"))
      
      )
    ),
  #tabPanel
  tabPanel(title = "Explore Tree",
           sidebarLayout(
             sidebarPanel(uiOutput("select_node_render")),
             mainPanel(fluidRow(plotlyOutput(outputId = "tree")))))
)#)tabsetPanel
)#)fluidpage
)#)ShinyUI
