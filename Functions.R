# Importing a distance Matrix --------------------------------------------------

#' #Importing a distance matrix
#' @param delimiter A optional value that can be passed to this function and is optional user input at the start of the app.
#' @param distances_raw_file The temp filename that is generated by shiny when the user selects a file on their computer.
#' @return The matrices in R matrix format, named.
importingDist<-function(distances_raw_file, delimiter){
  
  #In case the user did not enter a delimiter, then the function detect delimiter is called.
  if(input$delimiter==""){
    delimiter<-detect_delimiter(distances_raw_file)
    if(is.null(delimiter)){
      shiny::showNotification(
        ui="Delimiter could not be automatically detected. \n Try specifying the delimiter manually in the optional field. \n
      Also check whether you included the correct file!",
        type="error",
        duration=10
      )
      return()
    }
  }
  
  distances_raw <- utils::read.csv(distances_raw_file, head=T, sep=delimiter)
  #Call to function reshape_Rownames
  distances_raw<-reshape_Rownames(distances_raw = distances_raw )
  distances_raw<-as.matrix(distances_raw)
  "distances_raw"=distances_raw
}

#' Detecting the delimiter is one of ",", "\t", ";", " ", "-", "_". Assumes a square matrix.
#' @param distances_raw_file The temp filename that is generated by shiny when the user selects a file on their computer. Here this variable is passed on from importingDist()
#' @return The delimiter that was detected.
detect_delimiter<-function(distances_raw_file){
  delimiters<-c(",", "\t", ";", " ", "-", "_")#if not one of these, then the method will fail, the user needs to specify uncommon delimiters
  for(delimiter_pre in delimiters){
    dim <- dim(read.csv(distances_raw_file, sep=delimiter_pre))
    if(dim[1]==(dim[2]-1) | dim[1]==(dim[2])){ #in case the matrix has a rownames column the number of cols can be equal or 1 less then number of rows
      delimiter<-delimiter_pre
      break
    }
  }
  return(delimiter)
}


#' This function is called from within importingDist() and checks whether the first entry in the first row and column is a number.
#' This function assumes, square matrices and numeric distances metrics.
#' @param distances_raw is a data.frame passed on from importingDist().
#' @return distances_raw with possibly first column removed and added as rownames.
reshape_Rownames<-function(distances_raw){
  if(!is.numeric(distances_raw[1,1])){
    rownames(distances_raw)=(distances_raw[,1])
    distances_raw=distances_raw[,2:length(distances_raw[1,])]#here we need to assume a square matrix to set the dimensions.
  }
  return(distances_raw)
}

# Import Population Sizes file -------------------------------------------
importingPopSizes<-function(population_sizes_file){
  pop_table <- utils::read.table(population_sizes_file, blank.lines.skip = F)
  values<-as.numeric(pop_table[,2])
  locations<-pop_table[,1]
  
  origin<-matrix(rep(values, length(values)), ncol=length(values))
  colnames(origin)<-locations
  rownames(origin)<-locations
  
  destination<-t(origin)
  
  list("origin"=origin,"destination"=destination)
  }
  
# Import tree -------------------------------------------------------------
#TODO documentation
importingTree<-function(tree_file, file_type){
  print("Reading tree")
  switch(file_type,
         "nexus" = {
           tree <- ape::read.nexus(tree_file)
         },
         "newick" = {
           tree <- ape::read.tree(tree_file)
         },
         "beast" = {
           shiny::showNotification(
             ui=paste0("Checking for annotations based on column names in distance matrices...\n
                       This can take a few seconds." ),
             type = 'message',
             duration=10)
           tree <- treeio::read.beast(tree_file)
           tree <- dplyr::as_tibble(tree)
           colnames(tree)[which(colnames(tree) == "branch.length")] <-"edge.length"
         })
  tree <- negativeBranchLength(tree)
  tree
}

# Import sampling location -------------------------------------------------------------
importingSamplingLocations<-function(sampling_locations){
  tip_states <- utils::read.table(sampling_locations, blank.lines.skip = F)[,1]
  tip_states
}

validate_sampling_locations<-function(session, sampling_locations, distances_raw, tree){
  distance_matrix_states<-colnames(distances_raw[[1]])    
  #validating sampling locations
  grouped_states<-lapply(sampling_locations, function(sampling_location) tree$tip.label[which(grepl(sampling_location, distance_matrix_states)==T)])
  only_in_sampling_locations<-unique(sampling_locations[which(lapply(grouped_states, function(grouped_state) which(length(grouped_state)==0))==1)])
  if(length(only_in_sampling_locations)>0){
    sampling_locations<-sampling_locations[sampling_locations %!in% only_in_sampling_locations]
    if(length(tree$tip.label)!=length(sampling_locations)){
      tree<-lapply(only_in_sampling_locations, function(only_in_sampling_location){
        tree<-treeio::drop.tip(tree, tree$tip.label[which(grepl(only_in_sampling_location, tree$tip.label)==T)])
      })
      if(length(tree$tip.labels)==length(sampling_locations)){
        shiny::showNotification(
          ui=paste0("The following state is present in your sampling locations file but not in the column names of the distance
          matrices: ", only_in_sampling_locations, " We removed this(these) state(s) from the sampling locations file and also removed the corresponding tips from the
                    tree."),
          type = "warning",
          duration=30)
      }else{
        stop() #throw error,equivalent to "throw()". I cannot think of a way out here, the user needs to upload updated documents. Error will be handled in calling observer in server.R
      }
    }else{
      shiny::showNotification(
        ui=paste0("The following state is present in your sampling locations file but not in the column names of the distance
          matrices: ", only_in_sampling_locations, " We removed this(these) state(s) from the sampling locations file. The tree was not modified but the resulting sampling
                  location files had the matchin number of states to tips in the tree."),
        type = "warning",
        duration=30)
    }
  }else{
    if(length(tree$tip.label)!=length(sampling_locations)){
      stop() #throw error,equivalent to "throw()". I cannot think of a way out here, the user needs to upload updated documents. Error will be handled in calling observer in server.R
    }
  }
  return(list("sampling_locations"=sampling_locations, "tree"=tree))
}


#TODO documentation
negativeBranchLength<-function(tree){
  if(length(which(tree$edge.length<=0))>0){
    shiny::showNotification(
      ui=paste0("There were negative branch lenght in your tree, these were set to 1e-100. \n
                Running treeannotator with the option 'keep target heights' should avoid negative and 0 branch length." ),
      type = 'message',
      duration=30)
    tree$edge.length[which(tree$edge.length<=0)]=1e-100   }
  return(tree)
}

#' Function that tries to guess the relevant annotation in the tree based on the distance matrices provided by the user. 
#' Function is only relevant in case tree was already annotated.
#' @param tree Tree in beast format 
#' @param distances_raw distance matrix in matrix format.
detectState<-function(tree, distances_raw){
  return()
}

GenerateRawTransitionMatrix = function(distances_raw, tree_df) {
  tip_states=colnames(distances_raw)
  transitions_raw= matrix(0, nrow=dim(distances_raw)[1], ncol=dim(distances_raw)[1])#0 matrix in size of distance matrix 
  row.names(transitions_raw) = tip_states
  colnames(transitions_raw) = tip_states
  tree_df<-tree_df
  for(node in tree_df$node){
    if(node!=tidytree::rootnode(tree_df)$node){
      parent_loc = get(state(), tidytree::parent(tree_df,node))
      if(parent_loc=="missing_root_state"){
        next
      }
      if(is.list(parent_loc)) parent_loc=parent_loc[[1]][1]
      parent_loc = stringr::str_split(parent_loc,"\\+")[[1]][1]
      parent_loc = stringr::str_split(parent_loc," ")[[1]][1]
      
      child_loc<-get(state(),tree_df[tree_df$node==node,])
      if(is.list(child_loc)) child_loc=child_loc[[1]][1]
      child_loc = stringr::str_split(child_loc,"\\+")[[1]][1]
      child_loc = stringr::str_split(child_loc," ")[[1]][1]
      transitions_raw[parent_loc, child_loc]=transitions_raw[parent_loc, child_loc]+1
    }
  } 
  "transitions_raw"=transitions_raw
}

GenerateFinal_Transitions_Distances <- function(transitions_raw, distances_raw) {
  if(input$annotations==FALSE){
    distances_raw<- lapply(distances_raw, function(matrix){
      rownames(matrix)<-colnames(matrix)
      matrix<-matrix[,colnames(matrix) %in% levels(tip_states$data)] 
      matrix<-matrix[rownames(matrix) %in% levels(tip_states$data),]
    })
  transitions_raw<-transitions_raw[, colnames(transitions_raw) %in% levels(tip_states$data)]
  transitions_raw<-transitions_raw[rownames(transitions_raw) %in% levels(tip_states$data),]
  }
  if(input$annotations==FALSE &  (input$Reconstruction_Method=="ML" |  input$Reconstruction_Method=="TT")){
    Q<-importingDist("treeTime/transition_rates.csv")
    distances_raw[[length(distances_raw)+1]]<-Q
  }
  if(!is.null(pop_sizes$data)){
    distances_raw[[length(distances_raw)+1]]<-pop_sizes$data$origin
    distances_raw[[length(distances_raw)+1]]<-pop_sizes$data$destination
  }
    names_matrixes<-outer(X = colnames(transitions_raw),
                        Y = rownames(transitions_raw),
                        FUN = function(X,Y) paste(X,Y,sep="->"))#all combination of transitions
    names_matrixes =names_matrixes[(lower.tri(names_matrixes) | upper.tri(names_matrixes))] #names and distance call before call to transition
    #only the diagonal is excluded otherwise we keep both directions
    distances = sapply(distances_raw, function(distances_raw) distances_raw[(lower.tri(distances_raw) |upper.tri(distances_raw))])
    #distances is a matrix with the columns representing each a distacne matrix
    transitions=transitions_raw[(lower.tri(transitions_raw) | upper.tri(transitions_raw))]
    #bring transitions in same format, 1 columns and each transition for 1 state to the other, bidirectional, in rows
  colnames(distances)<-column_names()   #adding the names to cols
  transition_distances<-data.frame(Transitions=transitions, distances, Key=names_matrixes)
  #adding the transitions as a column, adding the row_names as an additional variable (needed for tooltip)
  rownames(transition_distances)<-names_matrixes #adding rownames, not strictly required but neat
  
  write.csv(transition_distances, file = paste0("output/transition_distances_", input$tree_file$name, input$Reconstruction_Method,input$annotations, ".csv"))
  transition_distances
}
 
'%!in%' <- function(x,y){
  !('%in%'(x,y))
}

#gives first word in a string defaults to first word and the . as seperator
first.word <- function(my.string, sep="\\.", n=1){
  base::strsplit(my.string, sep)[[1]][n]
}

# extracting the tip labels from the sub tree
getTipLabels<-function(tree){
  if (isS4(tree)) {
    tip_labels <- tree@phylo$tip.label
  } else {
    tip_labels <- tree$tip.label
  }
  return(tip_labels)
}
