#### Defining functions ##
importingFiles<-function(distances_raw_file=distances_raw_fileGlobal, tree_file=tree_fileGlobal, delimiter){
  print("Reading annotated tree")
  tree <-readT(tree_file)
  tree<-negativeBranchLength(tree)
  distances_raw <- read.csv(distances_raw_file, head=T, sep=delimiter)
  distances_raw<-reshape_Rownames(distances_raw)
  list("distances_raw"=distances_raw, "tree"=tree)
}

negativeBranchLength<-function(tree){
  if(length(which(tree$edge.length<=0))>0){
    shiny::showNotification(
      ui=paste0("There were negative branch lenght in your tree, these were set to 1e-100. \n
                Running treeannotator with the option 'keep target heights' should avoid negative and 0 branch length." ),
      type = 'message',
      duration=30)
    tree$edge.length[which(tree$edge.length<0)]=1e-100    }
  return(tree)
}


importingOnlyDist<-function(distances_raw_file=distances_raw_fileGlobal, tree=tree, delimiter){
  tree=tree
  distances_raw <- read.csv(distances_raw_file, head=T, sep=delimiter)
  distances_raw<-reshape_Rownames(distances_raw = distances_raw )
  list("distances_raw"=distances_raw, "tree"=tree)
}

reshape_Rownames<-function(distances_raw){
  if(!is.numeric(distances_raw[1,1])){
    rownames(distances_raw)=(distances_raw[,1])
    distances_raw=distances_raw[,2:length(distances_raw[1,])]
  }
  return(distances_raw)
}

GenerateRawTransitionMatrix = function(state, x) {
  distances_raw<-x[[1]]
  tree<-x[[2]]
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
  matrix_sym<-matrix
    for (i in 1:dim(matrix)[1]){
    for (j in 1:dim(matrix)[2]){
      matrix_sym[i, j] <- matrix[j, i] + matrix[i, j]
    }
  }
  matrix
}


GenerateFinal_Transitions_Distances <- function(makeSymmetric=makeSymmetricGlobal, x ) {
  
  transitions_raw<-x[[1]]
  distances_raw<-x[[2]]
  names_matrixes<-outer(X = colnames(transitions_raw),
                        Y = rownames(transitions_raw),
                        FUN = function(X,Y) paste(X,Y,sep="->"))
  
  if(makeSymmetric==TRUE){
    transitions<-makeSymmetric(transitions_raw)
    names_matrixes = names_matrixes[lower.tri(transitions)]#names and distance call before call to transition
    distances = distances_raw[lower.tri(distances_raw,diag = F)]#names and distances call before transition otherwise transition has changed
    transitions = transitions[lower.tri(transitions)]
  }else{
    names_matrixes =names_matrixes[(lower.tri(names_matrixes) | upper.tri(names_matrixes))] #names and distance call before call to transition
    distances=distances_raw[(lower.tri(distances_raw) |upper.tri(distances_raw))]
    transitions=transitions_raw[(lower.tri(transitions_raw) | upper.tri(transitions_raw))]
  }
  
  names_matrixes=names_matrixes[which(transitions!=0)]
  distances = distances[which(transitions != 0)]
  transitions = transitions[which(transitions != 0)]
  
  names(distances)=c(names_matrixes[upper.tri(names_matrixes)])
  names(transitions)=names_matrixes
  
  transition_distances<-data.frame(Transitions=transitions, Distances=distances, Key=names(transitions))
  
  list(transition_distances=transition_distances, names_matrixes=names_matrixes)
}

plotting_fun<-function(logTransformation,transition_distances,vals){
  
  keep    <- transition_distances[ vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]
  
  theme_set(theme_classic())
  if (logTransformation==FALSE) p <- ggplot(keep, mapping= aes(x=Distances,y=Transitions, key=Key)) 
  if (logTransformation==TRUE)  p <- ggplot(keep, mapping= aes(x=log(Distances),y=Transitions, key=Key)) #log transformation only here to keep recalculation time short
  
  p1<-p +
    geom_point() +
    geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
    geom_smooth(mapping=aes(key=NULL),method = "lm")+
    coord_cartesian(xlim = c(0, max(transition_distances$Distances)), ylim = c(0,max(transition_distances$Transitions)))
  p2 <- p1 %>% plotly::ggplotly(tooltip = c("Distances", "Transitions", "Key"), source="plot")
  
  return(p2)
}

plotting_residuals<-function(transition_distances,vals ,x){
  
  keep    <- transition_distances[ vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]

  theme_set(theme_classic())
  p_res<-ggplot(x, aes(fitted, residuals))+geom_point() +
  coord_cartesian(xlim = c(0, max(x$fitted)), ylim = c(0,max(x$residuals)))
  p_res1 <- p_res %>% plotly::ggplotly(tooltip =c("fitted", "residuals"), source="plot_res")
  return(p_res)
}

linear_regression<-function(transition_distances,cut_off_residual=NULL, percentile=95, logTransformation, vals){
  
  keep    <- transition_distances[ vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]
  
  if(logTransformation==FALSE)   lm=lm(keep$Transitions~keep$Distances)
  if(logTransformation==TRUE)   lm=lm(keep$Transitions~log(keep$Distances))
  x<-data.frame(lm$residuals,lm$fitted.values)
  colnames(x)<-c("residuals", "fitted")
  
  if(is.null(cut_off_residual)){ cut_off_residual=quantile((lm$residuals), percentile/100)}
  index=keep[which(lm$residuals>cut_off_residual), ]
  print(paste(length(index$Transitions), "Possible Outlier(s)" , sep = " "))
  print(paste("cut_off_residual: ",cut_off_residual))
  output<-index[order(-index$Transitions),]
  output<-data.frame(rownames(output), output)
  colnames(output)<-c("Countries", "# Transitions", "Distance between countries")
  list(lm=lm, output=output, x=x)
}   

  '%!in%' <- function(x,y){
    !('%in%'(x,y))
  }
  
  