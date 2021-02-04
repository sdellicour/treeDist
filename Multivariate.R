observe({
  updateCheckboxGroupInput(session, 
                           inputId= "Log", 
                           label = "Log", 
                           choices = c(input$variable, "Transitions"),
                           selected = NULL)
})

plotting_muĺti<-function(transition_distances,vals,variable_multi, Log_multi, clientData){
  
  keep    <- transition_distances[ vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]
  
  keep<-keep%>%
    select(Transitions, variable_multi, Key)%>%
    mutate_at(Log_multi, log)
  colnames(keep)<-sapply(colnames(keep), function(colname){
    if(colname %in% Log_multi) {
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


lm_multi<-function(transition_distances,cut_off_residual=NULL, percentile=95, vals, variable_multi, Log_multi){
  
  keep    <- transition_distances[ vals$keeprows, , drop = FALSE]
  exclude <- transition_distances[!vals$keeprows, , drop = FALSE]
  
  variable=as.vector(sapply(variable_multi, function(variable) {
    if (variable %in% Log_multi){
      variable=paste0("log(", variable, ")")
    }
    variable
  }))
  
  if("Transitions" %!in% Log_multi){
    lm=lm(as.formula(paste0("Transitions~",paste(variable, collapse="+"))), data=keep)
    lm=lm(as.formula(paste0("Transitions~",paste(variable, collapse="+"))), data=keep)
  }
  if("Transitions" %in% Log_multi){
    lm=lm(as.formula(paste0("log(Transitions)~",paste(variable, collapse="+"))), data=keep)
    lm=lm(as.formula(paste0("log(Transitions)~",paste(variable, collapse="+"))), data=keep)
  }
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