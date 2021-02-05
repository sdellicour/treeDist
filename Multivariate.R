
log_choices<-function(transition_distances){
  log_validated<-names(which(apply(transition_distances[,1:dim(transition_distances)[2]-1], 2, min)>0))
  return(log_validated)
}

observeEvent(input$multi_input_control, {
  shinyjs::toggle(selector = "div.multi_input_control", animType = "fade", anim=T)
})

observeEvent(input$multi_regression_output, {
  shinyjs::toggle(selector = "div.multi_regression_output", animType = "fade", anim=T)
})

observeEvent(input$multi_plot_output, {
  shinyjs::toggle(selector = "div.multi_plot_output", animType = "fade", anim=T)
})
  
plotting_muĺti<-function(transition_distances,vals, clientData){
  
  keep    <- transition_distances[ vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]
  
  keep<-keep%>%
    select(Transitions, input$variable, Key)%>%
    mutate_at(input$Log, log)
  colnames(keep)<-sapply(colnames(keep), function(colname){
    if(colname %in% input$Log) {
      paste0(colname, "_log")
    }else{
      colname
    }
  })
  
  keep_high<- tidyr::gather(data=keep, key="Predictors", value="value", -c(colnames(keep)[1], Key))
  theme_set(theme_classic())
  p1 <-ggplot(keep_high, aes_string(y = colnames(keep_high)[1], x ="value", group = "Predictors", key="Key")) + #color = Predictors
    facet_wrap(. ~ Predictors, scale="free", ncol=3) +
    geom_smooth(method = "lm")+
    geom_point(shape=21, colour="#4D4D4D", fill= "#0e74af80")
  p2 <- p1 %>% plotly::ggplotly(tooltip = c("value",  colnames(keep_high)[1], "Key"), source="multi_plot",  width = cdata$output_pid_width, height =  ceiling(length(unique(keep_high$Predictors))/2)*300)
  return(p2)
}


lm_multi<-function(transition_distances,cut_off_residual=NULL, percentile=95, vals){
  
  keep    <- transition_distances[ vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]
  
  variable=as.vector(sapply(input$variable, function(variable) {
    if (variable %in% input$Log){
      variable=paste0("log(", variable, ")")
    }
    variable
  }))
  
  if("Transitions" %!in% input$Log){
    lm=lm(as.formula(paste0("Transitions~",paste(variable, collapse="+"))), data=keep)
    lm=lm(as.formula(paste0("Transitions~",paste(variable, collapse="+"))), data=keep)
  }
  if("Transitions" %in% input$Log){
    lm=lm(as.formula(paste0("log(Transitions)~",paste(variable, collapse="+"))), data=keep)
    lm=lm(as.formula(paste0("log(Transitions)~",paste(variable, collapse="+"))), data=keep)
  }
  x<-data.frame(lm$residuals,lm$fitted.values)
  colnames(x)<-c("residuals", "fitted")
  
  list(lm=lm)#, output=output, x=x)
}   