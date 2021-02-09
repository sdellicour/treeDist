# Univariate ####
output$plot = renderPlotly({
  req(transition_distances, vals, logs)
  plotting_fun()
}) # output$plot = renderPlot({

output$plot_res = renderPlotly({
  req(transition_distances, vals, logs)
  linear_regression()$x%>%
    plotting_residuals(.)
  
}) # output$plot = renderPlot({

output$lm=renderTable({
  req(transition_distances, vals, logs)
  glance(linear_regression()$lm)
  
}) # output$plot = renderTable({

output$lm.summary=renderPrint({
  req(transition_distances, vals, logs)
  summary(linear_regression()$lm)
  
}) # output$plot = renderTable({

# output$output=renderTable({
#   linear_regression(transition_distances=transition_distances,logs=logs, vals()=vals())$output

#}) # output$plot = renderTable({

# output$hover<-renderPrint({
#   hover_data<-event_data("plotly_hover", source = "plot")
#   transition_distances[which(transition_distances$Key==hover_data$key),]
# })
# 
# Toggle Points #### 

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
  #Below code for biderectional reactivity but it gets confusing when both plots contains selected points
  
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
  keep    <- transition_distances[vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]
  
  if(logs$logtransform[1]==TRUE)   {
    keep <- keep%>%
      dplyr::mutate_at("Transitions", log)
    exclude <- exclude%>%
      dplyr::mutate_at("Transitions", log)
  }
  
  ##Give a warning if the predictive variable is log transformed but contains values equal or below 0.
  if(min(get(input$Predictor_uni, transition_distances))<=0 & logs$logtransform[2]==TRUE){
    shiny::showNotification(
      ui=paste0("There are values smaller or equal to 0 in ", input$Predictor_uni, " the log transformation is not possible.
                The transform is rolled back and displayed as before." ),
      type = 'warning',
      duration=30)
    logs$logtransform[2]=FALSE
    return()
  }
  
  if(logs$logtransform[2]==TRUE)   {
    keep <- keep%>%
      dplyr::mutate_at(input$Predictor_uni, log)
    exclude <- exclude%>%
      dplyr::mutate_at(input$Predictor_uni, log)
  }
  
  ggplot2::theme_set(theme_classic())
  p <- ggplot(keep, mapping= aes_string(x=input$Predictor_uni,y="Transitions", key="Key")) 
  values<- get(input$Predictor_uni, transition_distances)
  
  if(!logs$logtransform[1]==TRUE & !logs$logtransform[2]==TRUE) {
    p<-p+scale_x_continuous(name =paste0(input$Predictor_uni),limits =  c(min(values), max(values)))+
      scale_y_continuous(name = "Transitions", limits=c(0,max(transition_distances$Transitions)))
  }
  if(logs$logtransform[1]==TRUE & !logs$logtransform[2]==TRUE){
    p<-p+scale_x_continuous(name =paste0(input$Predictor_uni),limits =  c(min(values), max(values)))+
      scale_y_continuous(name = "Transition_log", limits=c(0,max(log(transition_distances$Transitions))))
  }
  if(!logs$logtransform[1]==TRUE & logs$logtransform[2]==TRUE){
    p<-p+scale_x_continuous(name =paste0(input$Predictor_uni, "_log"),limits =  c(min(log(values)), max(log(values))))+
      scale_y_continuous(name = "Transitions", limits=c(0,max(transition_distances$Transitions)))
  }
  if(logs$logtransform[1]==TRUE & logs$logtransform[2]==TRUE){
    p<-p+scale_x_continuous(name =paste0(input$Predictor_uni, "_log"),limits =  c(min(log(values)), max(log(values))))+
      scale_y_continuous(name = "Transition_log", limits=c(0,max(log(transition_distances$Transitions))))
  }
  if(!input$regression_line==FALSE){ 
    p <- p + geom_smooth(mapping=aes(key=NULL), method = "lm", se=as.logical(input$se), level=input$level)
  }
  p<-p +
    geom_point(data=keep, shape=21, colour="#4D4D4D" ,fill=alpha("#0e74af", input$alpha),  stroke = input$stroke, size=input$size) + 
    geom_point(data = exclude, shape = 21, fill = NA, color = "red", alpha = input$alpha, stroke = input$stroke, size=input$size)
  
  p <- p %>% plotly::ggplotly(tooltip = c(input$Predictor_uni, "Transitions", "Key"), source="plot")
  
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
  if(logs$logtransform[1]==TRUE)   {
    keep <- keep%>%
      mutate_at("Transitions", log)
  }
  if(logs$logtransform[2]==TRUE)   {
    keep <- keep%>%
      mutate_at(input$Predictor_uni, log)
  }
  lm=lm(keep$Transitions~get(input$Predictor_uni, keep))
  x<-data.frame(lm$residuals,lm$fitted.values)
  colnames(x)<-c("residuals", "fitted")
  list(lm=lm, x=x)
}   