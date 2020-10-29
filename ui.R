library(raster)
library(shiny)
library(shinyIncubator)

shinyUI(fluidPage(
	titlePanel(""),
	sidebarLayout(
		sidebarPanel(
			width=3,
			h3("TreeDist"),
			p(""),
			br(),
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
			         fileInput("sampling_locations", label=("Sampling Locations"))
			  )
			),
		#	fluidRow(
		#	  column(6,
		#	         selectInput(inputId ="Annotation_State", "Annotation for most likely state", c("states",
		#	                                                                                        "city"))
		#	  )
		#	),
			fluidRow(
			  column(6,
			         radioButtons(inputId ="Symmetrie", "Remove directionality",  c("No"=FALSE, "Yes"=TRUE))
			  )
			),
			fluidRow(
			  column(6,
			         radioButtons(inputId ="LogTransform", "Log", c("Yes"=TRUE, "No"=FALSE))
			  )
			),
			fluidRow(
			  column(6,
			         radioButtons(inputId ="Reconstruction_Method", "Method for ancestral reconstruction", 
			                      c("Maximum Likelihood"="ML","Maximum Parsimony"="MP"))
			  )
			),
			fluidRow(
      			column(6,
					actionButton("start", label=h4("RUN"), col.label="red")
				)
			)
		),
		mainPanel(
		  splitLayout(
			plotOutput(outputId="plot", click = "plot1_click",
			           brush = brushOpts(
			             id = "plot1_brush"
			           )
			),
			actionButton("exclude_toggle", "Toggle points"),
			actionButton("exclude_reset", "Reset")
		  )
		)
	)	
))
