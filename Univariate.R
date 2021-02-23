# Univariate ####

##scatter plot output####
output$plot = renderPlotly({
  req(transition_distances, vals, logs)
  plotting_fun()
}) # output$plot = renderPlot({

##bar plot output####
output$bar = renderPlotly({
  req(transition_distances, vals, logs)
  histo_fun()
}) # output$plot = renderPlot({

##residual plot output####
output$plot_res = renderPlotly({
  req(transition_distances, vals, logs)
  linear_regression()$x%>%
    plotting_residuals(.)
}) # output$plot = renderPlot({

##glance output ####
output$lm=renderTable({
  req(transition_distances, vals, logs)
  glance(linear_regression()$lm)
}) # output$plot = renderTable({

## lm summary output ####
output$lm.summary=renderPrint({
  req(transition_distances, vals, logs)
  summary(linear_regression()$lm)
}) # output$plot = renderTable({

observeEvent(input$log_transitions, {
  req(transition_distances, vals, logs)
  logs$logtransform[1]=!logs$logtransform[1]
})

observeEvent(input$log_distances, {
  req(transition_distances, vals, logs)
  logs$logtransform[2]=!logs$logtransform[2]
})

# Toggle points that are brushed, when button is clicked only 
observeEvent(input$exclude_toggle, {
  req(transition_distances, vals, logs)
  selected_data<-event_data("plotly_selected", source = "plot")
  res<- data.frame(selected_=rep(FALSE, nrow(transition_distances)))
  res$selected_[which(transition_distances$Key %in% selected_data$key)]<-TRUE
  vals$keeprows <- xor(vals$keeprows, res$selected_)      
  #Below code for bidirectional reactivity but it gets confusing when both plots contains selected points
  
  #res_res<- data.frame(selected_=rep(FALSE, nrow(transition_distances)))
  #res_res$selected_[which(transition_distances$Key %in%selected_data$key)]<-TRUE
  
  #if(sum(res$selected_)>0)     vals$keeprows <- xor(vals$keeprows, res$selected_)#the ones that are set to true in res are set to false
  #else                         vals$keeprows <- xor(vals$keeprows, res_res$selected_)
})

# Reset all points
observeEvent(input$exclude_reset, {
  req(transition_distances, vals, logs)
  vals$keeprows <- rep(TRUE, nrow(transition_distances))
})

plotting_fun<-function(){
  selected_col<-grep(input$Predictor_uni, colnames(transition_distances))
  
  #takes care of log 
  transition_distances<-log_uni_data()
  
  #data modification part
  keep    <- transition_distances[vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]

  #plotting part
  ggplot2::theme_set(theme_classic())
  p <- ggplot(keep, mapping= aes_string(x=colnames(keep)[selected_col],y=colnames(keep)[1], key=colnames(keep)[dim(keep)[2]])) 
  p<-log_uni(p, transition_distances)
  p<-p + geom_point(data = exclude, shape = 21, fill = NA, color = "red", alpha = input$alpha, stroke = input$stroke, size=input$size)
  p<-colour_by_states_uni(p, keep)

   if(!input$regression_line==FALSE){ 
    p <- p + geom_smooth(mapping=aes(key=NULL), method = "lm", se=as.logical(input$se), level=input$level) 
   }
  p <- p %>% plotly::ggplotly(tooltip =c("x", "y", "key"), source="plot")
  return(p)
}

log_uni_data<-function(){
  #since for the predictor the column number is not fixed and the column name is not fixed we need to select via pattern search
  selected_col<-grep(input$Predictor_uni, colnames(transition_distances))

  if(logs$logtransform[1]==TRUE)   {
    transition_distances <- transition_distances%>%
      dplyr::mutate(across(Transitions, log))%>%
      rename(Transitions_Log=Transitions)
  }
  ##Give a warning if the predictive variable is log transformed but contains values equal or below 0.
  
  if(logs$logtransform[2]==TRUE)   {
    if(min(get(input$Predictor_uni, transition_distances))<=0){
      shiny::showNotification(
        ui=paste0("There are values smaller or equal to 0 in ", input$Predictor_uni, " the log transformation is not possible.
                The transform is rolled back and displayed as before." ),
        type = 'warning',
        duration=30)
      logs$logtransform[2]=FALSE
      return()
    }
    
    transition_distances <- transition_distances%>%
      dplyr::mutate(across(input$Predictor_uni, log))
      colnames(transition_distances)[selected_col]<- paste(input$Predictor_uni, "log", sep ="_")
  }
  transition_distances
}


histo_fun<-function(){
  keep_exclude<-summarize_to_from()
  keep<-keep_exclude$keep
  exclude<-keep_exclude$exclude
  p<-ggplot(data=keep, aes(x=get(input$to_from, keep), y=sum_Transitions ))+ geom_bar(stat="identity")
  p[["labels"]][["x"]]<-paste0("Transitions ", input$to_from)
  plotly::ggplotly(p)%>%layout(xaxis=list(tickangle=45))
}

summarize_to_from<-function(){
  keep    <- transition_distances[vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]

  keep<-keep%>% 
    separate(Key, c("From", "To"), "->")%>% 
    group_by(across(input$to_from))%>%
    summarise(sum_Transitions=sum(Transitions))%>%
    arrange(desc(sum_Transitions))
  exclude<-exclude%>% 
    separate(Key, c("From", "To"), "->")%>% 
    group_by(across(input$to_from))%>%
    summarise(sum_Transitions=sum(Transitions))%>%
    arrange(desc(sum_Transitions))
  
  list("keep"=keep, "exclude"=exclude)
}

log_uni<-function(p, transition_distances){
  selected_col<-grep(input$Predictor_uni, colnames(transition_distances))
  numeric_transitions_distances<-transition_distances[,1:selected_col]
  p<-p+scale_x_continuous(name =colnames(numeric_transitions_distances)[selected_col])+
        scale_y_continuous(name = colnames(numeric_transitions_distances)[1])+
        coord_cartesian(xlim =c(min(numeric_transitions_distances[,selected_col]), max(numeric_transitions_distances[,selected_col])), ylim=c(0,max(numeric_transitions_distances[,1])))
  return(p)
}

colour_by_states_uni<-function(p, keep){
  if(input$colour_by_states_uni=="To"){
    toStates<-unlist(lapply(keep$Key, function(key) first.word(my.string = key,sep =  "->", n= 2)))
    nbColoursUni_to<-length(unique(toStates))#the states are added as a list in order to unlist them I need to take only the first word otherwise we get too many states
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))#these 9 colours will be interpolated to obtain  the most divergent result
    p <- p + geom_point(aes(fill= toStates),alpha =  input$alpha , data=keep, shape=21, colour="#4D4D4D", stroke = input$stroke, size=input$size)+
      scale_fill_manual(values=getPalette(nbColoursUni_to))
  }else if(input$colour_by_states_uni=="From"){
    fromStates<-unlist(lapply(keep$Key, function(key) first.word(my.string = key,sep =  "->", n= 1)))
    nbColoursUni_from<-length(unique(fromStates))#  
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))#these 9 colours will be interpolated to obtain  the most divergent result
    p <- p + geom_point(aes( fill= fromStates),alpha =  input$alpha , data=keep, shape=21, colour="#4D4D4D",  stroke = input$stroke, size=input$size)+
      scale_fill_manual(values=getPalette(nbColoursUni_from))
  }else if (input$colour_by_states_uni=="All blue"){
    p<-p +geom_point(data=keep, shape=21, colour="#4D4D4D" ,fill=alpha("#0e74af", input$alpha),  stroke = input$stroke, size=input$size) 
  }
  return(p)
}



plotting_residuals<-function(x){
  #keep    <- transition_distances[ vals$keeprows, , drop = FALSE]#not needed because only called after linear regression
  #exclude <- transition_distances[!vals$keeprows, , drop = FALSE]# see above
  theme_set(theme_classic())
  p_res<-ggplot(x, aes(fitted, residuals))+
    geom_point(shape=21, colour="#4D4D4D", fill=alpha("#0e74af", input$alpha), stroke = input$stroke, size=input$size) + 
    coord_cartesian(xlim = c(min(x$fitted), max(x$fitted)), ylim = c(min(x$residuals),max(x$residuals)))
  p_res1 <- p_res %>% plotly::ggplotly(tooltip =c("fitted", "residuals"), source="plot_res")
  return(p_res)
}

linear_regression<-function(cut_off_residual=NULL, percentile=95){
  keep    <- transition_distances[ vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]

  variable=input$Predictor_uni
  if(logs$logtransform[2]==TRUE)   {
    variable=paste0("log(", input$Predictor_uni, ")")
  }
  
  if(logs$logtransform[1]==FALSE)   {
    f <- paste("Transitions",variable, sep="~")
  }
  if(logs$logtransform[1]==TRUE)   {
    f <- paste("log(Transitions)", variable, sep = "~")
  }
  
  lm=lm(as.formula(f), data=keep)
  lm[["call"]][["formula"]]<-lm$terms #this seemed to be the easiest way to have the evaluated variables printed to the summary output under "call" 
  x<-data.frame(lm$residuals,lm$fitted.values)
  colnames(x)<-c("residuals", "fitted")
  list(lm=lm, x=x)
}   