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
			         fileInput("sampling_locations", label=("Sampling locations"))
			  )
			),
		fluidRow(
		  column(6,
		         selectInput(inputId ="Annotation_State", "Annotation state", c("states","city"))
		  )
		),
			fluidRow(
			  column(6,
			         radioButtons(inputId ="Symmetrie", "Symmetrie",  c("No"=FALSE, "Yes"=TRUE))
			  )
			),
			fluidRow(
			  column(6,
			         radioButtons(inputId ="LogTransform", "Log", c("No"=FALSE, "Yes"=TRUE))
			  )
			),
			fluidRow(
			  column(6,
			         radioButtons(inputId ="Reconstruction_Method", "AR Method", 
			                      c("Maximum Parsimony"="MP","Maximum Likelihood"="ML"))
			  )
			),
			fluidRow(
      			column(6,
					actionButton("start", label=h4("RUN"), col.label="red")
				)
			)
		),
		mainPanel(
		  fluidRow(
		  splitLayout(
			  plotOutput(outputId="plot", click = "plot_click",
			           brush = brushOpts(
			             id = "plot_brush"
			           )
			),
			plotOutput(outputId="plot_res", click = "plot_res_click",
			           brush = brushOpts(
			             id = "plot_res_brush"
			           )
			)
			),
			actionButton("exclude_toggle", "Toggle points"),
			actionButton("exclude_reset", "Reset")
		  ),
			fluidRow(
			  tableOutput(outputId="lm")
			),
			fluidRow(
			  tableOutput(outputId="output")
			)
	)	
)
)
)
