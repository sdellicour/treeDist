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


standardize_pred<-function(df, response="Transitions"){
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
  df<-cbind(df[,1],df[,2:dim(df)[2]][,order(colnames(df[,2:dim(df)[2]]))])
  names(df)[1] <- "Intercept"

  #colnames(df)<-abbreviate(colnames(df), minlength=5)
  colnames(df)<-c("Int", "adjr2","GINSLE",  "LBRGIN",  "LBRSLE",  "bic","cp","dGec","dPdn","dPSz","dPrc", "dPrcss", "dTmp",  "dTmpss","dTT1k", "gCrceD","iborder", "ilang","nborder",  "nlang","nvars",          
    "oGec","oPdn","oPSz", "oPrc","oPrcss", "oTmp",  "oTmpss",   "oTT1k","rsq",   "rss",       "wtnC")  
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
    ggplot(aes(variable, factor(value))) +
    geom_raster(aes(fill=factor(value)), hjust =0,
                vjust =0) +
    scale_fill_manual(values = getPalette(nbColours), guide = FALSE) +
    labs(x = "", y = "") +
    theme_classic()+
    scale_y_discrete(limits=rev)+
    scale_x_discrete(limits=unique(df$variable))+
    theme(panel.grid.major = element_line(colour = "black"))+
    theme(axis.text.x = element_text(angle = 90,vjust=-0.35,hjust=1, size=13))+
    theme(
      panel.background = element_rect(fill = NA),
      panel.ontop = TRUE
    )
  
  if(label_bool==FALSE) p<-p+theme( axis.text.x=element_blank())
  p <- p + theme(axis.title.y = element_text(margin = margin(r = padding)), axis.text.y = element_blank())
p
}


fit<-function(x, criterion="bic", response="Transitions", data, distribution=FALSE){
  require(dplyr)
  x<-summary(x)
  df<-data.frame(x$which,
                 criterion=get(criterion,x))
  df<-df%>%
    filter(criterion==min(criterion))
  df<-df[which(df==TRUE)]
  predictors<-names(df[,2:dim(df)[2]])
  f<-as.formula(paste0(response, "~", paste(predictors, collapse = "+")))
  lm<-lm(f, data=data)
  lm
}

lasso<-function(response="Transitions", data){
  if("Transition_Rate" %in% colnames(data) && "Transition_Rate" != response) data<-data[,colnames(data)!="Transition_Rate"]
  y <-get(response, data)
  f<-as.formula(paste0(response, "~ ."))
  x <- model.matrix(object = f, data = data)[,-1]
  colnames(x)<-c("GINSLE",  "LBRGIN",  "LBRSLE","dGec","dPdn","dPSz","dPrc", "dPrcss", "dTmp",  "dTmpss","dTT1k", "gCrceD","iborder", "ilang","nborder",  "nlang",          
                  "oGec","oPdn","oPSz", "oPrc","oPrcss", "oTmp",  "oTmpss",   "oTT1k",  "wtnC")  
  grid<-10^seq(5,-5, length=100) #100 values of lambda covering the range 0.01 to 10^10
  
  set.seed(1)
  cv_out<-glmnet::cv.glmnet(x, y, alpha=1, lambda=grid, type.measure = "mse", nfolds = 10) #deviance a.k.a MSE
  # cv_out
  # plot(cv_out)
  out<-glmnet::glmnet(x,y,alpha=1, lambda=grid, standardize =T)
  coef(cv_out, s="lambda.min")
  bestlam<-cv_out$lambda.min
  lasso.coef<-predict(out, s=bestlam, type="coefficients")
  coefs<-lasso.coef[lasso.coef@i+1,]
  list("coefs"=coefs, "out"=out, "min.lambda"=cv_out$lambda.min)
}

ggplot.glmnet<-function(coefs,out, xlim=c(-15,2), textPos=2, textsize=2, min.lambda){
  require(dplyr)
  require(ggplot2)
  lambda_coef<-lapply(0:24, function(y) out$beta@x[which(out$beta@i==y)])
  #lambda_coef$hj <- rep(c(2,-2), length.out=length(names(coefs!=0)[-1]))
  max_length<-max(unlist(lapply(lambda_coef, length)))
  lambda_coef <- sapply (lambda_coef, function (x) {length (x) <-100; return (x)})
  lambda_coef$lambda<-out$lambda
  colnames(lambda_coef)<-c(out[["beta"]]@Dimnames[[1]], "lambda")
  lambda_coef<-data.frame(apply(lambda_coef,2,function(x){
    nb_zeros<-length(which(is.na(x)))
    na.omit(c(rep(0,nb_zeros),x))
  }))
  high<-tidyr::gather(lambda_coef, key="Predictor", value="coefficient",-c(lambda))
  tmp1<-high[which(high$lambda==min(high$lambda)),]
  tmp1<-tmp1 %>% filter(Predictor %in% names(coefs))
  sort<-data.frame(tmp1,"jitter"= rep(c(1,-1), length.out=length(names(coefs!=0)[-1])) )
  sort_tmp<-sort %>% arrange(desc(coefficient))
  hjust_sorted<-sort_tmp$jitter
  ggplot(high, aes(log(lambda), coefficient, color=Predictor)) +
    geom_line() +
    scale_colour_discrete(guide = 'none') +
    geom_text(size=textsize,data=high[high$Predictor %in% names(coefs!=0)[-1],] %>% group_by(Predictor) %>% filter(lambda==min(lambda)),
              aes(x=log(lambda)-textPos, label=names(coefs!=0)[-1]), position= position_jitter(width=1.5, height=max(high$coefficient)/20))+
    geom_vline(xintercept = log(min.lambda))+
    xlim(xlim)+
    theme_classic()
}

