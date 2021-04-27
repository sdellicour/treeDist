line2user <- function(line, side) {
  lh <- par('cin')[2] * par('cex') * par('lheight')
  x_off <- diff(grconvertX(c(0, lh), 'inches', 'npc'))
  y_off <- diff(grconvertY(c(0, lh), 'inches', 'npc'))
  switch(side,
         `1` = grconvertY(-line * y_off, 'npc', 'user'),
         `2` = grconvertX(-line * x_off, 'npc', 'user'),
         `3` = grconvertY(1 + line * y_off, 'npc', 'user'),
         `4` = grconvertX(1 + line * x_off, 'npc', 'user'),
         stop("Side must be 1, 2, 3, or 4", call.=FALSE))
}

addfiglab <- function(lab, xl = par()$mar[2], yl = par()$mar[3]) {
  
  text(x = line2user(xl, 2), y = line2user(yl, 3), 
       lab, xpd = NA, font = 2, cex = 1.5, adj = c(0, 1))
  
}


standardize_pred<-function(df, response="Trns"){
  response_df<-data.frame(df[,response])
  colnames(response_df)<-response
  binaries<-apply(df,2, function(variable) is.binary(variable))
  binaries_df<- data.frame(df[, binaries])
  colnames(binaries_df)<- names(binaries[which(binaries==TRUE)])
  df<-scale(df[, !colnames(df) %in% c(colnames(binaries_df), response)], center = T, scale = T)
  data.frame(response_df,binaries_df ,df)
}

is.binary<-function(vector){
  if(length(unique(vector))==2 | length(unique(vector))==3) return(TRUE)
  return(FALSE)
}


ggregsubsets <- function(x, criterion="bic", label_bool=TRUE, padding=0){
  require(dplyr); require(ggplot2); require(tidyr)
  if(inherits(x, "regsubsets")) x <- summary(x)
  if(!inherits(x, "summary.regsubsets"))
    stop("The input to ggregsubsets() should be the result of regsubsets().")
  df <- bind_cols(
    as.data.frame(x$which), 
    as.data.frame(x[c("rsq","rss","adjr2","cp","bic")]),
    data.frame(nvars = 1:nrow(x$which))
  )
  names(df)[1] <- "Intercept"

  df<-df %>% 
    mutate(rsq = 100*rsq, adjr2 = 100*adjr2) %>% 
    gather(variable, is_in, -rsq, -rss, -adjr2, -cp, -bic, -nvars) %>% 
    gather(measure, value, -nvars, -variable, -is_in)%>%
    filter(measure==criterion)
  nbColours<-length(unique(df$variable))-1
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  
  df<-df%>%
    filter(is_in==TRUE)

  p<-df%>%
    ggplot(aes(variable, factor(round(value)))) +
    geom_raster(aes(fill=factor(round(value))), hjust =0,
                vjust =0) +
    scale_fill_manual(values = getPalette(nbColours), guide = FALSE) +
    labs(x = "", y = "") +
    theme_classic()+
    scale_y_discrete(limits=rev)+
    scale_x_discrete(limits=unique(df$variable))+
    theme(panel.grid.major = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90,vjust=-0.5,hjust=1))+
    theme(
      panel.background = element_rect(fill = NA),
      panel.ontop = TRUE
    )
  
  if(label_bool==FALSE) p<-p+theme( axis.text.x=element_blank())
  p <- p + theme(axis.title.y = element_text(margin = margin(r = padding)))
p
}
