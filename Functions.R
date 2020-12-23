#### Defining functions ##
importingTree<-function(sampling_locations, tree_file, file_type){
  print("Reading tree")
  switch(file_type,
         "nexus" = {
           tree <- read.nexus(tree_file)
         },
         "newick" = {
           tree <- read.tree(tree_file)
         },
         "beast" = {
           tree <- read.beast(tree_file)
           tree <- as_tibble(tree)
           colnames(tree)[which(colnames(tree) == "branch.length")] <-"edge.length"
         })
  tree <- negativeBranchLength(tree)
  tip_states <- read.table(sampling_locations, blank.lines.skip = F)[,1]
  list(tip_states, tree)
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

removeTipsMissingData<-function(tree){
}

importingDist<-function(distances_raw_file, delimiter){
  distances_raw <- read.csv(distances_raw_file, head=T, sep=delimiter)
  distances_raw<-reshape_Rownames(distances_raw = distances_raw )
  distances_raw<-as.matrix(distances_raw)
  "distances_raw"=distances_raw
}

reshape_Rownames<-function(distances_raw){
  if(!is.numeric(distances_raw[1,1])){
    rownames(distances_raw)=(distances_raw[,1])
    distances_raw=distances_raw[,2:length(distances_raw[1,])]
  }
  return(distances_raw)
}

GenerateRawTransitionMatrix = function(state, distances_raw, tree_df) {
  tip_states=colnames(distances_raw)
  transitions_raw= matrix(0, nrow=dim(distances_raw)[1], ncol=dim(distances_raw)[1])#0 matrix in size of distance matrix 
  row.names(transitions_raw) = tip_states
  colnames(transitions_raw) = tip_states
  tree_df<-tree_df
  for(node in tree_df$node){
    if(node!=rootnode(tree_df)$node){
      parent_loc = get(state, parent(tree_df,node))
      if(is.list(parent_loc)) parent_loc=parent_loc[[1]][1]
      parent_loc = str_split(parent_loc,"\\+")[[1]][1]
      parent_loc = str_split(parent_loc," ")[[1]][1]
      
      child_loc<-get(state,tree_df[tree_df$node==node,])
      if(is.list(child_loc)) child_loc=child_loc[[1]][1]
      child_loc = str_split(child_loc,"\\+")[[1]][1]
      child_loc = str_split(child_loc," ")[[1]][1]
      transitions_raw[parent_loc, child_loc]=transitions_raw[parent_loc, child_loc]+1
    }
  } 
  "transitions_raw"=transitions_raw
}


makeSymmetric<-function(matrix){
  matrix_sym<-matrix
  for (i in 1:dim(matrix)[1]){
    for (j in 1:dim(matrix)[2]){
      matrix_sym[i, j] <- matrix[j, i] + matrix[i, j]
    }
  }
  matrix_sym
}

GenerateFinal_Transitions_Distances <- function(makeSymmetric, transitions_raw, distances_raw, column_names) {
  names_matrixes<-outer(X = colnames(transitions_raw),
                        Y = rownames(transitions_raw),
                        FUN = function(X,Y) paste(X,Y,sep="->"))
  
  if(makeSymmetric==TRUE){
    transitions<-makeSymmetric(transitions_raw)
    names_matrixes = names_matrixes[lower.tri(transitions)]#names and distance call before call to transition
    
    distances = sapply(distances_raw, function(distances_raw) distances_raw[lower.tri(distances_raw,diag = F)])#names and distances call before transition otherwise transition has changed
    transitions = transitions[lower.tri(transitions)]
  }else{
    names_matrixes =names_matrixes[(lower.tri(names_matrixes) | upper.tri(names_matrixes))] #names and distance call before call to transition
    distances = sapply(distances_raw, function(distances_raw) distances_raw[(lower.tri(distances_raw) |upper.tri(distances_raw))])
    transitions=transitions_raw[(lower.tri(transitions_raw) | upper.tri(transitions_raw))]
  }
  colnames(distances)<-column_names
  transition_distances<-data.frame(Transitions=transitions, distances, Key=names_matrixes)
  rownames(transition_distances)<-names_matrixes
  
  transition_distances=transition_distances[which(transitions!=0),]
  transition_distances
}

plotting_fun<-function(logTransformation,transition_distances,vals, Predictor){
  keep    <- transition_distances[vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]
  theme_set(theme_classic())
    if (logTransformation==FALSE) p <- ggplot(keep, mapping= aes_string(x=Predictor,y="Transitions", key="Key")) 
    if (logTransformation==TRUE)  p <- ggplot(keep, mapping= aes_string(x=Predictor,y="Transitions", key="Key")) #log transformation only here to keep recalculation time short
    p1<-p +
      geom_point() +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
      geom_smooth(mapping=aes(key=NULL),method = "lm")+
      coord_cartesian(xlim = c(0, max(get(Predictor, transition_distances))), ylim = c(0,max(transition_distances$Transitions)))
    p2 <- p1 %>% plotly::ggplotly(tooltip = c(Predictor, "Transitions", "Key"), source="plot")
  return(p2)
}

plotting_residuals<-function(transition_distances,vals ,x, Predictor){#currently vals does not need to be pused here
  
  #keep    <- transition_distances[ vals$keeprows, , drop = FALSE]#not needed because only called after linear regression
  #exclude <- transition_distances[!vals$keeprows, , drop = FALSE]# see above

  theme_set(theme_classic())
  p_res<-ggplot(x, aes(fitted, residuals))+geom_point() +
  coord_cartesian(xlim = c(0, max(x$fitted)), ylim = c(0,max(x$residuals)))
  p_res1 <- p_res %>% plotly::ggplotly(tooltip =c("fitted", "residuals"), source="plot_res")
  return(p_res)
}

linear_regression<-function(transition_distances,cut_off_residual=NULL, percentile=95, logTransformation, vals, Predictor){
  
  keep    <- transition_distances[ vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]
  if(logTransformation==FALSE)   lm=lm(keep$Transitions~get(Predictor, keep))
  if(logTransformation==TRUE)   lm=lm(keep$Transitions~log(get(Predictor, keep)))
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
  
  first.word <- function(my.string, sep="\\."){
    strsplit(my.string, sep)[[1]][1]
  }
  