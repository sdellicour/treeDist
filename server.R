library(raster)
library(animation)
library(shiny)
library(shinyIncubator)

shinyServer(function(input, output) {

observeEvent(input$start, {

# output$gif = downloadHandler(
	# filename = "PGSViewer.gif",
	# content = function(file) {

input_file = "input.txt"
output_name = "PGSviewer"
timeSlice = 1
gifInterval = 0.25
color = "red"
colorNA = "gray80"

input_file = input$input_file$datapath
output_name = input$output_name
timeSlice = input$timeSlice
gifInterval = input$gifInterval
color = input$color
colorNA = input$colorNA
# pdfCreation = input$pdfCreation

pdfCreation = FALSE
if (pdfCreation == TRUE)
	{
		dir.create(file.path(output_name), showWarnings=FALSE)
	}
output_directory = paste(output_name,sep="")

output$image = renderImage({
							
progress = shiny::Progress$new()
on.exit(progress$close())
progress$set(message="Running...", value=0)
n = 1
for (i in 1:n) { progress$inc(1/n, detail = paste(" This can take a while...")) }

lldSimulation = function(x0, y0, maximalPopulationsSizes, distributionLDD, distributionLDDParameter1LDD, distributionLDDParameter2LDD)
	{
		x1 = x0; x2 = x0; y1 = y0; y2 = y0
		d1 = x0
		if (d1 < abs(x0-dim(maximalPopulationsSizes)[2])) d1 = abs(x0-dim(maximalPopulationsSizes)[2])
		d2 = y0
		if (d2 < abs(y0-dim(maximalPopulationsSizes)[1])) d2 = abs(y0-dim(maximalPopulationsSizes)[1])
		if (d1 < d2)
			{
				d_limit = d2
			}	else	{
				d_limit = d1
			}
		if (d_limit >= distributionLDDParameter1LDD)
			{
				d_limit = distributionLDDParameter1LDD
			}
		if ((distributionLDD=="lognormal")|(distributionLDD=="Lognormal")|(distributionLDD=="logN")|(distributionLDD=="logn"))
			{
				d_max = rlnorm(1, 0, distributionLDDParameter2LDD)
				if (d_max > d_limit)
					{
						while (d_max > d_limit)
							{
								d_max = rlnorm(1, 0, distributionLDDParameter2LDD)
							}
					}
			}	else	{
				d_max = runif(1, 0, d_limit)
			}
		c = 0
		lessThanOneHundredTrials = FALSE
		while (lessThanOneHundredTrials == FALSE)
			{
				nonZeroSizeDestination = FALSE
				while (nonZeroSizeDestination == FALSE)
					{
						maxDistanceReached = FALSE
						movmentOnTheGrid = TRUE
						while (maxDistanceReached == FALSE)
							{					
								r1 = runif(1, 0, 1)
								if (r1 < 0.5)
									{
										r2 = runif(1, 0, 1)
										if (r2 < 0.5) 
											{
												x2 = x1 + 1
											}	else	{
												x2 = x1 - 1
											}
									}	else	{
										r2 = runif(1, 0, 1)
										if (r2 < 0.5) 
											{
												y2 = y1 + 1
											}	else	{
												y2 = y1 - 1
											}
									}
								movmentOnTheGrid = TRUE	
								if (x2 <= 0) movmentOnTheGrid = FALSE
								if (x2 > dim(maximalPopulationsSizes)[2]) movmentOnTheGrid = FALSE
								if (y2 <= 0) movmentOnTheGrid = FALSE
								if (y2 > dim(maximalPopulationsSizes)[1]) movmentOnTheGrid = FALSE
								if (movmentOnTheGrid == TRUE)
									{
										d = sqrt(((y2-y0)*(y2-y0))+((x2-x0)*(x2-x0)))
										if (d < d_max)
											{
												x1 = x2; y1 = y2
											}
										else
											{
												maxDistanceReached = TRUE
											}
									}
							}
						if (maximalPopulationsSizes[y1,x1] > 0)
							{
								nonZeroSizeDestination = TRUE
								lessThanOneHundredTrials = TRUE
							}
					}
				c = c + 1
				if (c == 100)
					{
						lessThanOneHundredTrials = TRUE
					}
			}
		if (c == 100) print("counter == 100")
		toPopID = ((y1-1)*dim(maximalPopulationsSizes)[1]) + x1
		return(toPopID)
	}

txt = scan(file=input_file, what="character", sep="\n", blank.lines.skip=F)
if (grepl(" ",txt[4])) separator = " "
if (grepl("\t",txt[4])) separator = "\t"
line1 = unlist(strsplit(txt[4],separator))
nberOfPopulations = as.numeric(line1[1])
nberOfBackwards = as.numeric(line1[2])
nberOfForwards = as.numeric(line1[3])
tR = as.numeric(line1[4])
mf1 = as.numeric(line1[5])
mf2 = as.numeric(line1[6])
nberOfGroups = as.numeric(line1[7])
mLdd = 0; distributionLDD = "uniform"
distributionLDDParameter1LDD = -9999
distributionLDDParameter2LDD = -9999
if (length(line1) > 7) mLdd = as.numeric(line1[8])
if (length(line1) > 8) distributionLDD = line1[9]
if (length(line1) > 9) distributionLDDParameter1LDD = as.numeric(line1[10])
if (length(line1) > 10) distributionLDDParameter2LDD = as.numeric(line1[11])
b = sum(unlist(strsplit(txt[13],separator))!="")
a = nberOfPopulations/b
index = 13+(2*a)+4
times = unlist(strsplit(txt[index],separator))
times = times[times[]!=""]
index = index+2
mats = list()
mat = matrix(nrow=a, ncol=b)
for (j in 1:a)
	{
		line = unlist(strsplit(txt[index+j],separator))
		line = line[line[]!=""]
		for (k in 1:b) mat[j,k] = as.numeric(line[k])
	}
mats[[1]] = mat
for (i in 1:length(times))
	{
		index = index+1+a
		mat = matrix(nrow=a, ncol=b)
		for (j in 1:a)
			{
				line = unlist(strsplit(txt[index+j],separator))
				line = line[line[]!=""]
				for (k in 1:b) mat[j,k] = as.numeric(line[k])
			}
		mats[[i+1]] = mat
	}
# OH edit to remove the oldest time step if there is a unique final Ne in the input file:
if (sum(is.na(mats[[length(times)+1]])) > 0)
	{
		times = times[-length(times)] 
		mats = mats[-length(mats)]
	}
maxValue = 0
for (i in 1:length(mats))
	{
		if(maxValue < max(mats[[i]])) maxValue = max(mats[[i]])
	}
colourScale = colorRampPalette(c("white",color), bias=1)(3*maxValue/2)
brks = seq(-(maxValue/2), maxValue, 1)
times = times[length(times):1]
times = as.numeric(times)
buffer = list(); c = 0
for (i in length(mats):1)
	{
		c = c+1; buffer[[c]] = mats[[i]]
	}
mats = buffer

t0 = times[1]
mat = mats[[1]]
t1 = t0
maxNe_matrix = mats[[2]]
maxNe_vector = as.vector(t(mats[[2]]))
if (length(times) > 1) t2 = times[2]
equilibrium = FALSE

firstLevelPops = list()
secondLevelPops = list()
c = 0
for (i in 1:a)
	{
		for (j in 1:b)
			{
				c = c+1
				firstLevel = c()
				secondLevel = c()
				# for (k in 1:a)
				for (k in max(1,i-2):min(i+2,a)) # OH
					{
						# for (l in 1:b)
						for (l in max(1,j-2):min(j+2,b)) # OH
							{
								id = ((k-1)*b)+l
								d = sqrt(((i-k)*(i-k))+((j-l)*(j-l)))
								if ((d >= 1) & (d < 2))
									{
										firstLevel = c(firstLevel, id)
									}
								if ((d >= 2) & (d < 3))
									{
										secondLevel = c(secondLevel, id)
									}
							}
					}
				firstLevelPops[[c]] = firstLevel
				secondLevelPops[[c]] = secondLevel
			}
	}
t = t0
vals = reactiveValues(t=t0)

buffer = mat; buffer[maxNe_matrix[]==0] = NA; rast = raster(buffer)
if (pdfCreation == TRUE)
	{
		pdf(file=paste(output_directory,"/",t,"_generations_ago.pdf",sep=""), width=6, height=6) # dev.new(width=6, height=6)
		par(mar=c(0,0,0.5,0), oma=c(3,4,3,0.5), mgp=c(2,1,0), lwd=0.1, bty="o")
		plot(rast, col=colourScale, colNA=colorNA, box=F, axes=F, main=paste(t," generations ago",sep=""), cex.main=0.7, col.main="gray30", legend=F)
		dev.off()
		writeRaster(rast, paste(output_directory,"/",t,"_generations_ago.asc",sep=""))
	}	else	{
		dev.new(width=6, height=6); par(mar=c(0,0,0.5,0), oma=c(3,4,3,0.5), mgp=c(2,1,0), lwd=0.1, bty="o")
		plot(rast, col=colourScale, colNA=colorNA, box=F, axes=F, main=paste(t," generations ago",sep=""), cex.main=0.7, col.main="gray30", legend=F)
	}

c = 2

GIF = saveGIF(

for (t in t0:0)	{

if (t > 0)
	{
		if (length(times) > 1)
			{
				if (t == t2)
					{
						c = c+1
						if (length(times) > (c-1))
							{
								t2 = times[c]
							}
						maxNe_matrix = mats[[c]]
						maxNe_vector = as.vector(t(mats[[c]]))
						equilibrium = FALSE
						t1 = t
					}
			}
		if (equilibrium == FALSE)
			{
				print(t)
				m = as.vector(t(mat))
				# migration:
				inM = m; inM[] = 0
				ouM = m; ouM[] = 0
				for (i in 1:length(m))
					{
						if (mf1 > 0)
							{
								for (j in 1:length(firstLevelPops[[i]]))
									{
										id = firstLevelPops[[i]][j]
										if (m[id] > 0)
											{
												M = rbinom(1, m[id], mf1)
												ouM[id] = ouM[id]+M
												inM[i] = inM[i]+M
											}
									}
							}
						if (mf2 > 0)
							{
								for (j in 1:length(secondLevelPops[[i]]))
									{
										id = secondLevelPops[[i]][j]
										if (m[id] > 0)
											{
												M = rbinom(1, m[id], mf2)
												ouM[id] = ouM[id]+M
												inM[i] = inM[i]+M
											}
									}
							}
						if (mLdd > 0)
							{
								M = rbinom(1, m[i], mLdd)
								if (M > 0)
									{
										if (floor(i/b) < (i/b)) y0 = floor(i/b)+1
										if (floor(i/b) == (i/b)) y0 = (i/b)
										x0 = i-(b*(y0-1))														
										toPopID = lldSimulation(x0, y0, maximalPopulationsSizes=maxNe_matrix, distributionLDD, distributionLDDParameter1LDD, distributionLDDParameter2LDD)
										ouM[i] = ouM[i]+M
										inM[toPopID] = inM[toPopID]+M
									}
							}
					}
				for (i in 1:length(m))
					{
						m[i] = m[i]-ouM[i]+inM[i]
					}
				# reproduction:
				for (i in 1:length(m))
					{
						new_m = round(m[i]*tR)
						if (new_m < m[i]*tR)
							{
								if (runif(1,0,1) < 0.5)
									{
										m[i] = new_m
									}	else	{
										m[i] = new_m + 1
									}
							}
						if (new_m > m[i]*tR)
							{
								if (runif(1,0,1) < 0.5)
									{
										m[i] = new_m - 1
									}	else	{
										m[i] = new_m
									}
							}
						if (new_m == m[i]*tR)
							{
								m[i] = new_m
							}
					}	
				# ecremage:
				for (i in 1:length(m))
					{
						if (m[i] > maxNe_vector[i]) m[i] = maxNe_vector[i]
					}
				if (sum(m-maxNe_vector) == 0) equilibrium = TRUE		
				counter = 0
				for (i in 1:dim(mat)[1])
					{
						for (j in 1:dim(mat)[2])
							{
								counter = counter+1
								mat[i,j] = m[counter]
							}
					}
				if (t == t1)
					{
						t1 = t-timeSlice
						buffer = mat; buffer[maxNe_matrix[]==0] = NA; rast = raster(buffer); # rast[1] = 0
						if (pdfCreation==TRUE)
							{
								pdf(file=paste(output_directory,"/",t,"_generations_ago.pdf",sep=""), width=6, height=6) # dev.new(width=6, height=6)
								par(mar=c(0,0,0.5,0), oma=c(3,4,3,0.5), mgp=c(2,1,0), lwd=0.1, bty="o")
								plot(rast, col=colourScale, colNA=colorNA, box=F, axes=F, main=paste(t," generations ago",sep=""), cex.main=0.7, col.main="gray30", legend=F)
								dev.off()
								writeRaster(rast, paste(output_directory,"/",t,"_generations_ago.asc",sep=""))
							}	else	{
								par(mar=c(0,0,0.5,0), oma=c(3,4,3,0.5), mgp=c(2,1,0), lwd=0.1, bty="o")
								plot(rast, col=colourScale, colNA=colorNA, box=F, axes=F, main=paste(t," generations ago",sep=""), cex.main=0.7, col.main="gray30", legend=F)
							}
					}
			}
	}
} # for (t in t0:0)

, movie.name=paste(output_name,".gif",sep=""), interval=gifInterval) # saveGIF(

list(src=paste(output_name,".gif",sep=""), contentType="gif" #, width=400, height=400, alt="This is alternate text"
)
}, deleteFile=T) # output$image = renderImage({
}) # observeEvent(input$start, {
}) # shinyServer(function(input, output) {
