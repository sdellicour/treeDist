library(raster)
library(animation)
library(shiny)
library(shinyIncubator)

shinyUI(fluidPage(
	titlePanel(""),
	sidebarLayout(
		sidebarPanel(
			h3("PGSviewer 2.0"),
			p("PGSviewer 2.0 allows to visualise the evolution of effective population size matrices during forward simulations performed in the same way as PHYLOGEOSIM 2.0. PGSviewer uses the same input files and stops at the end of the forward simulation."),
			p("To generate an animated GIF, PGSviewer 2.0 requires the preliminary installation of ImageMagik (imagemagick.org)."),
			br(),
			fluidRow(
				column(12,
					fileInput("input_file", label=("Input file"))
				)
				# column(4,
					# downloadButton("gif", label=h4("START"))
					# actionButton("start", label=h4("START"), col.label="red")
				# )
			),
			fluidRow(
				column(8,
					textInput("output_name", label=("Output name (without extension)"), value="PGSviewer_output")
				),
      			column(4,
					actionButton("start", label=h4("START"), col.label="red")
				)
			),
			fluidRow(
				column(6,
					numericInput("timeSlice", label=("Time slice (generations)"), value=1)
				),
				column(6,
					textInput("color", label=("Raster colorscale theme"), value="red")
				)
			),
			fluidRow(
				column(6,
					numericInput("gifInterval", label=("GIF time interval (s)"), value=0.25)
				),
				column(6,
					textInput("colorNA", label=("NA cell color"), value="gray80")
				)
			)
			# fluidRow(
				# column(12,
      			# checkboxInput("pdfCreation", label=("generate distint PDFs for each time slice instead of a GIF"), value=FALSE)
      			# )
      		# )
      		# img(src="Logo_PGS.png", height=550, width=550)
		),
		mainPanel(
			# img(src="Logo_PGS.png", height=550, width=550),
			imageOutput("image")
		)
	)	
))
