#### Defining functions ##
importingFiles<-function(distances_raw_file=distances_raw_fileGlobal, tree_file=tree_fileGlobal){
  
  tree <-readT(tree_file)
  distances_raw <- read.csv(distances_raw_file, head=T, sep="\t")
  
  list(distances_raw, tree)
}

GenerateRawTransitionMatrix = function(state, distance_raw, tree) {

  locations = colnames(distances_raw)
  countries=colnames(distances_raw)
  transitions_raw= matrix(0, nrow=dim(distances_raw)[1], ncol=dim(distances_raw)[1])#0 matrix in size of distance matrix 
  row.names(transitions_raw) = countries
  colnames(transitions_raw) = countries
  
  for (i in 1:dim(tree$edge)[1])
    #the edge table should be read as: rows are the edge numbers and first col is start node  and 2nd is end node of the branch or edge
  {
    if (tree$edge[i, 1] %in% tree$edge[, 2])
      #if a start node is also an end node, then we are going from somewhere to that state. We want to assign the start state as location 1
    {
      index = which(tree$edge[, 2] == tree$edge[i, 1])#get node that is end node of edge in question
      location1 = get(state, tree$annotations[[index]]) #assign end node of branch as location1
      if (is.list(location1))
      {
        location1 = get(state, tree$annotations[[index]])[[1]]#take arbritarily the first MP ancestral state
      }
    }	else	{
      location1 = get(state, tree$root.annotation)#tip nodes are not in column 1 so if it is not an internal node it is then the root node
      if (is.list(location1))
      {
        location1 = get(state, tree$root.annotation)[[1]]
      }
    }
    location2 = get(state, tree$annotations[[i]])#location 2 is the start of the branch we are at atm
    if (is.list(location2))
    {
      location2 = get(state, tree$annotations[[i]])[[1]]
    }
    if (nchar(location2) > 2 | nchar(location1) > 2) {
      location2 = substr(location2, start = 1, stop = 2)
      location1 = substr(location1, start = 1, stop = 2)
    }
    transitions_raw[location1, location2] = transitions_raw[location1, location2] + 1
  }
  
  list=list(trans_raw=transitions_raw,dist_raw=distances_raw)
}


makeSymmetric<-function(matrix){
  for (i in 1:dim(matrix)[1]){
    for (j in 1:dim(matrix)[2]){
      matrix_sym[i, j] <- matrix[j, i] + matrix[i, j]
    }
  }
  matrix_sym
}


GenerateFinal_Transitions_Distances <- function(makeSymmetric=makeSymmetricGlobal, transitions_raw, distances_raw) {

  names_matrixes<-outer(X = colnames(transitions_raw),
                        Y = rownames(transitions_raw),
                        FUN = function(X,Y) paste(X,Y,sep="->"))
  
  if(makeSymmetric==TRUE){
    transitions<-makeSymmetric(transitions_raw)
    names_matrixes = names_matrixes[lower.tri(transitions)]#names and distance call before call to transition
    distances = distances_raw[lower.tri(distances_raw,diag = F)]
    transitions = transitions[lower.tri(transitions)]#names and distances call before transition otherwise transition has changed
  }else{
    names_matrixes =names_matrixes[(lower.tri(names_matrixes) | upper.tri(names_matrixes))] #names and distance call before call to transition
    distances=distances_raw[(lower.tri(distances_raw) |upper.tri(distances_raw))]
    transitions=transitions[(lower.tri(transitions_raw) | upper.tri(transitions_raw))]
  }
  
  names_matrixes=names_matrixes[which(transitions!=0)]
  distances = distances[which(transitions != 0)]
  transitions = transitions[which(transitions != 0)]
  
  names(distances)=c(names_matrixes[upper.tri(names_matrixes)])
  names(transitions)=names_matrixes
  
  list(transitions=transitions, distances=distances, names_matrixes=names_matrixes)
}

plotting_fun<-function(logTransformationm,transitions, distances){
 
  #pdf(file=paste("output/Transitions",plotname, ".pdf",sep="-"))
  if (logTransformation == TRUE) distances = log(distances)
  par(mgp=c(0,0,0), oma=c(0,0,0,0), mar=c(3,3.5,1.5,2))
  cols1 = c("#FAA521","#4676BB"); 
  cols2 = c("#FAA52150","#4676BB50")
  plot(distances, transitions, col=cols2[2], pch=16, cex=1.1, axes = F, ann=F)
  points(distances, transitions, col=cols1[2], pch=1, cex=1.1)
  axis(side=1, lwd.tick=0.2, cex.axis=0.9, mgp=c(0,0.4,0), lwd=0.2, tck=-0.02, col.tick="gray30", col.axis="gray30", col="gray30")
  axis(side=2, lwd.tick=0.2, cex.axis=0.9, mgp=c(0,0.6,0), lwd=0.2, tck=-0.02, col.tick="gray30", col.axis="gray30", col="gray30")
  title(ylab="transitions", cex.lab=1.1, mgp=c(2.0,0,0), col.lab="gray30")
  if (logTransformation == TRUE) title(xlab="distance (log)", cex.lab=1.1, mgp=c(1.4,0,0), col.lab="gray30")
  if (logTransformation != TRUE) title(xlab="distance", cex.lab=1.1, mgp=c(1.4,0,0), col.lab="gray30")
}

linear_regression<-function(cut_off_residual=NULL, percentile=95){
  
  lm=lm(transitions~distances)
  abline(lm)
  #dev.off()
  #pdf(file=paste("output/Residuals",plotname,".pdf", sep="-"))
  plot(lm$residuals)
  #dev.off()
  #TODO make this also applicable when there are more than one posible outlier.
  df_transitions_added<-data.frame(transitions_added)
  if(is.null(cut_off_residual)){ cut_off_residual=quantile((lm$residuals), percentile/100)}
  index=transitions[which(lm$residuals>cut_off_residual)]
  print(paste(length(index), "Possible Outlier(s):" , sep = " "))
  print(paste("cut_off_residual: ",cut_off_residual))
  output<-as.matrix(index%>%sort(decreasing =T))
  colnames(output)<-"Transitions"
  print(output)
  print(summary(lm))
  list(output=output,lm=lm)
}   

  '%!in%' <- function(x,y){
    !('%in%'(x,y))
  }
  