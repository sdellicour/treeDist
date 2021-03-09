
log_choices<-function(transition_distances){
  log_validated<-names(which(apply(transition_distances[,1:dim(transition_distances)[2]-1], 2, min)>0))
  return(log_validated)
}

logs_multi <- reactive({
  req(transition_distances, vals)#require to compute the transitions and the distance matrices first before executing this code
  log_validated = log_choices(transition_distances)
  return(c(c(log_validated[log_validated %in% c(input$variable)]), "Transitions"))
})

output$log <- renderUI({
  req(transition_distances, vals)
  output <- checkboxGroupInput(
    inputId = "Log",
    label = "Log",
    choices = logs_multi(),
    selected = NULL
  )
})

output$response_multi <- renderUI({
  req(transition_distances, vals)
  output<-selectizeInput(
    inputId= "response_multi",
    label="Response variable: ",
    choices = c(column_names(), "Transitions") ,
    selected =  "Transitions"
  )
})


observeEvent(input$multi_input_control, {
  shinyjs::toggle(selector = "div.multi_input_control", animType = "fade", anim=T)
})

observeEvent(input$multi_regression_output, {
  shinyjs::toggle(selector = "div.multi_regression_output", animType = "fade", anim=T)
})

observeEvent(input$multi_plot_output, {
  shinyjs::toggle(selector = "div.multi_plot_output", animType = "fade", anim=T)
})

plotting_muĺti<-function(transition_distances,vals){
  transition_distances<-transition_distances%>%
    select(Transitions, input$variable, Key)%>%
    mutate_at(input$Log, log)
  colnames(transition_distances)<-sapply(colnames(transition_distances), function(colname){
    if(colname %in% input$Log) {
      paste0(colname, "_log")
    }else{
      colname
    }
  })
  selected_col_response<-grep(input$response_multi, colnames(transition_distances))
  #transform the data into high format and then group by distance matrix
  transition_distances_high<- tidyr::gather(data=transition_distances, key="Predictor", value="Distance", -c(colnames(transition_distances)[selected_col_response], Key))
  theme_set(theme_classic())

  p1 <-ggplot(transition_distances_high, aes_string(y = colnames(transition_distances_high)[1], x ="Distance", group = "Predictor", key="Key")) + #color = Predictors
    facet_wrap(. ~ Predictor, scale="free", ncol=3 )+
    geom_smooth(method = "lm")+
    geom_point(shape=21, colour="#4D4D4D", fill= "#0e74af80")
  p2 <- p1 %>% plotly::ggplotly(tooltip = c("Predictor","Distance",  colnames(transition_distances_high)[1], "Key"), source="multi_plot",  width = cdata$output_multi_plot_width*0.95, height =  ceiling(length(unique(transition_distances_high$Predictor))/2)*300)
  return(p2)
}

lm_multi<-function(transition_distances, vals){
  
  variable=as.vector(sapply(input$variable, function(variable) {
    if (variable %in% input$Log){
      variable=paste0("log(", variable, ")")
    }
    variable
  }))
  
  if("Transitions" %!in% input$Log){
    f<-paste0("Transitions~",paste(variable, collapse="+"))
  }
  if("Transitions" %in% input$Log){
    f<-paste0("log(Transitions)~",paste(variable, collapse="+"))
  }
  lm=lm(as.formula(f), data=transition_distances)
  lm[["call"]][["formula"]]<-lm$terms #this seemed to be the easiest way to have the evaluated variables printed to the summary output under "call" 
  
  x<-data.frame(lm$residuals,lm$fitted.values)
  colnames(x)<-c("residuals", "fitted")
  
  list(lm=lm)#, output=output, x=x)
}   

# Multivariate ####
## Plot ####
observe({
  output$multi_plot = renderPlotly({
    req(transition_distances, vals, logs_multi(), input$variable)
    plotting_muĺti(transition_distances, vals)
  }) # output$plot = renderPlot({
  
  ## Glance #######
  output$lm_multi=renderTable({
    req(transition_distances, vals, logs_multi(), input$variable)
    glance(lm_multi(transition_distances, vals)$lm)
  }) # output$plot = renderTable({
  
  output$lm.summary_multi=renderPrint({
    req(transition_distances, vals, logs_multi(), input$variable)
    summary(lm_multi(transition_distances, vals)$lm)
  }) # output$plot = renderTable({
  
})