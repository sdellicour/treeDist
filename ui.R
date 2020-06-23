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
					fileInput("tree_file", label=("Tree file"))
				)
			),
			fluidRow(
				column(12,
					fileInput("distances_file", label=("Distance matrix"))
				)
			),
			fluidRow(
      			column(6,
					actionButton("start", label=h4("RUN"), col.label="red")
				)
			)
		),
		mainPanel(
			plotOutput(outputId="plot")
		)
	)	
))
