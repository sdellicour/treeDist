source("utils_Master.R")
setwd("/home/tim/Documents/Bioinformatics/MasterThesis/treeDist/")
library("RColorBrewer")
library("cowplot")
library("gridExtra")
library(grid)
library(factoextra)
library(ggpubr)
#### import #### 
#Rabies
transition_distances_rabies<-read.csv("output/transition_distances_batRABV.MCC.keep.target.heights.treesMPTRUE.csv", row.names = 1)
transition_distances_rabies_mp<-read.csv("output/transition_distances_batRABV.MCC.keep.target.heights.treesMPFALSE.csv", row.names = 1)
transition_distances_rabies_ml<-read.csv("output/transition_distances_batRABV.MCC.keep.target.heights.treesMLFALSE.csv", row.names = 1)
transition_distances_rabies_tt<-read.csv("output/transition_distances_batRABV.MCC.keep.target.heights.treesTTFALSE.csv", row.names=1)

#Ebola
transition_distances_ebola<-read.csv("output/transition_distances_Makona_1610_cds_ig.GLM.MCC.treeMPTRUE.csv", row.names = 1)
transition_distances_ebola_mp<-read.csv("output/transition_distances_ebola_not_annotated.treeMPFALSE.csv",row.names = 1)
transition_distances_ebola_ml<-read.csv("output/transition_distances_ebola_not_annotated.treeMLFALSE.csv",row.names = 1)
transition_distances_ebola_tt<-read.csv("output/transition_distances_ebola_not_annotated.treeTTFALSE.csv",row.names = 1)

#Influenza
transition_distances_influenza<-read.csv("output/transition_distances_h3_small_sample.MCC.treMPTRUE.csv", row.names = 1)
transition_distances_influenza_mp<-read.csv("output/transition_distances_h3_small_sample.MCC.treMPFALSE.csv", row.names = 1)
transition_distances_influenza_ml<-read.csv("output/transition_distances_h3_small_sample.MCC.treMLFALSE.csv",row.names = 1)
transition_distances_influenza_tt<-read.csv("output/transition_distances_h3_small_sample.MCC.treTTFALSE.csv", row.names = 1)


transition_distances_rabies<- transition_distances_rabies_mp[colnames(transition_distances_rabies)!="Key"]
transition_distances_rabies_mp <- transition_distances_rabies_mp[colnames(transition_distances_rabies_mp)!="Key"]
transition_distances_rabies_ml <- transition_distances_rabies_ml[!colnames(transition_distances_rabies_ml) %in% c("Transition_Rates", "Key")]
transition_distances_rabies_tt <- transition_distances_rabies_tt[colnames(transition_distances_rabies_tt)!="Key"]
transition_distances_ebola <- transition_distances_ebola_mp[colnames(transition_distances_ebola)!="Key"]
transition_distances_ebola_mp <- transition_distances_ebola[colnames(transition_distances_ebola_mp)!="Key"]
transition_distances_ebola_ml <- transition_distances_ebola_ml[!colnames(transition_distances_ebola_ml) %in% c("Transition_Rates", "Key")]
transition_distances_ebola_tt <- transition_distances_ebola_tt[colnames(transition_distances_ebola_tt)!="Key"]
transition_distances_influenza <- transition_distances_influenza[colnames(transition_distances_influenza)!="Key"]
transition_distances_influenza_mp <- transition_distances_influenza_mp[colnames(transition_distances_influenza_mp)!="Key"]
transition_distances_influenza_ml <- transition_distances_influenza_ml[!colnames(transition_distances_influenza_ml) %in% c("Transition_Rates", "Key")]
transition_distances_influenza_tt <- transition_distances_influenza_tt[colnames(transition_distances_influenza_tt)!="Key"]

####correlation ####
data<-transition_distances_ebola
data<-data[,order(colnames(data))]
cor_matrix1<-corrplot::corrplot(cor(data), type="upper", 
                                col=brewer.pal(n=8, name="RdYlBu") )

data2<-transition_distances_ebola[transition_distances_ebola$Transitions!=0,]
data2<-data2[,order(colnames(data2))]
cor_matrix2<-corrplot::corrplot(cor(data2), type="upper", 
                                col=brewer.pal(n=8, name="RdYlBu") )

png(filename = "output/cor_eb", width = 21, height = 29.7,units = "cm", res = 300)
par(mfrow=c(2,1))
corrplot::corrplot(cor_matrix2, type="upper",order="original", diag = FALSE)
addfiglab("A")

corrplot::corrplot(cor_matrix1, type="upper",order="original", diag = FALSE)
addfiglab("B")
dev.off()



#### clustering #### 
elbow_incl<-factoextra::fviz_nbclust(transition_distances_ebola, FUN = hcut, method = "wss", k.max = 6)+
  theme(axis.text.x = element_text(size = 20, color = "red"), title = element_text(size = 20, color = "blue"))+
  labs(title="Elbow plot")

sil_not_incl<-factoextra::fviz_nbclust(transition_distances_ebola, FUN = hcut, method = "silhouette", k.max = 6) +
  theme(axis.text.x = element_text(size = 20, color = "red"), title = element_text(size = 20, color = "blue"))+
  labs(title="Silhouette plot")

elbow_not_incl<-factoextra::fviz_nbclust(transition_distances_ebola[transition_distances_ebola$Transitions!=0,], FUN = hcut, method = "wss", k.max = 6)+
  theme(axis.text.x = element_text(size = 20, color = "red"), title = element_text(size = 20, color = "blue"))+
  labs(title="Elbow plot")

sil_not_incl<-factoextra::fviz_nbclust(transition_distances_ebola[transition_distances_ebola$Transitions!=0,], FUN = hcut, method = "silhouette", k.max = 6) +
  theme(axis.text.x = element_text(size = 20, color = "red"), title = element_text(size = 20, color = "blue"))+
  labs(title="Silhouette plot")
grid_el_sil<-gridExtra::grid.arrange(arrangeGrob(elbow_incl, left = textGrob("A", x = unit(1, "npc"), 
                                                                                    y = unit(.95, "npc"),  gp = gpar(col = "black", fontsize = 28))), 
                                     arrangeGrob(sil_not_incl, left = textGrob("B", x = unit(1, "npc"), 
                                                                         y = unit(.95, "npc"),  gp = gpar(col = "black", fontsize = 28))), 
                                     arrangeGrob(elbow_not_incl, left = textGrob("C", x = unit(1, "npc"), 
                                                                               y = unit(.95, "npc"),  gp = gpar(col = "black", fontsize = 28))), 
                                     arrangeGrob(sil_not_incl, left = textGrob("D", x = unit(1, "npc"), 
                                                                               y = unit(.95, "npc"),  gp = gpar(col = "black", fontsize = 28))))
ggsave(filename = "output/elbow_sil", plot =grid_el_sil, device = "png", width = 21, height = 29.7, units = "cm", dpi = 300) 

library(dendextend)
transition_distances_ebola=transition_distances_ebola[,order(colnames(transition_distances_ebola))]
d<-dist(1-abs(cor(transition_distances_ebola)))
hcl<-hclust(d, method="average")  #AVARAGE LINK
dend<-as.dendrogram(hcl)
dend=dendextend::color_branches(hcl,k=3, groupLabels = FALSE)

d1<-dist(1-abs(cor(transition_distances_ebola[transition_distances_ebola$Transitions!=0,])))
hcl1<-hclust(d1, method="average")  #AVARAGE LINK
dend1<-as.dendrogram(hcl1)
dend1=dendextend::color_branches(hcl1,k=3, groupLabels = FALSE)

png(filename = "output/dendrograms", width = 21, height = 29.7, res = 300, units = "cm")
par(mfrow=c(2,1))

par(mar=c(1,1,1,12))
dend %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  set("leaves_cex", value=0.5) %>%
  plot(horiz=TRUE, axes=FALSE)
abline(v = 1.75, lty = 2)
addfiglab("A")

par(mar=c(1,1,1,12))
dend1 %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  plot(horiz=TRUE, axes=FALSE)
abline(v = 1.70, lty = 2)
addfiglab("B")
dev.off()


## regsubsets ####
#colnames(transition_distances_ebola)<-abbreviate(names(transition_distances_ebola), minlength=4)
#colnames(transition_distances_ebola_mp)<-abbreviate(names(transition_distances_ebola_mp), minlength=4)
#colnames(transition_distances_ebola_ml)<-abbreviate(names(transition_distances_ebola_ml), minlength=4)
#colnames(transition_distances_ebola_tt)<-abbreviate(names(transition_distances_ebola_tt), minlength=4)


##lm_regsubsets_MCC_nozero#### 
transition_distances <- transition_distances_ebola[transition_distances_ebola$Transitions!=0,]
transition_distances_scaled<-standardize_pred(transition_distances)
predictors<-transition_distances_scaled[colnames(transition_distances_scaled)!="Transitions"]
f<-as.formula(object = paste0("Transitions ~ ", paste(colnames(predictors), collapse = " + ")))
lm_regsubsets_MCC_nozero <- leaps::regsubsets(
  x=f,
  data=transition_distances_scaled,
  nvmax = 25,
  method="exhaustive")

## lm_regsubsets_MCC_zero##### 
transition_distances <- transition_distances_ebola
transition_distances_scaled<-standardize_pred(transition_distances)
predictors<-transition_distances_scaled[colnames(transition_distances_scaled)!="Transitions"]
f<-as.formula(object = paste0("Transitions ~ ", paste(colnames(predictors), collapse = " + ")))
lm_regsubsets_MCC_zero <- leaps::regsubsets(
  x=f,
  data=transition_distances_scaled,
  nvmax = 25,
  method="exhaustive")

# lm_regsubsets_MP_nozero #### 
transition_distances <- transition_distances_ebola_mp[transition_distances_ebola_mp$Transitions!=0,]
transition_distances_scaled<-standardize_pred(transition_distances)
predictors<-transition_distances_scaled[colnames(transition_distances_scaled)!="Transitions"]
f<-as.formula(object = paste0("Transitions ~ ", paste(colnames(predictors), collapse = " + ")))
lm_regsubsets_MP_nozero <- leaps::regsubsets(
  x=f,
  data=transition_distances_scaled,
  nvmax = 25,
  method="exhaustive")

# lm_regsubsets_MP_zero #### 
transition_distances <- transition_distances_ebola_mp
transition_distances_scaled<-standardize_pred(transition_distances)
predictors<-transition_distances_scaled[colnames(transition_distances_scaled)!="Transitions"]
f<-as.formula(object = paste0("Transitions ~ ", paste(colnames(predictors), collapse = " + ")))
lm_regsubsets_MP_zero <- leaps::regsubsets(
  x=f,
  data=transition_distances_scaled,
  nvmax = 25,
  method="exhaustive")

# lm_regsubsets_ML_nozero #### 
transition_distances <- transition_distances_ebola_ml[transition_distances_ebola_ml$Transitions!=0,]
transition_distances_scaled<-standardize_pred(transition_distances)
predictors<-transition_distances_scaled[!colnames(transition_distances_scaled) %in% c("Transitions", "Transition_Rate")]
f<-as.formula(object = paste0("Transitions ~ ", paste(colnames(predictors), collapse = " + ")))
lm_regsubsets_ML_nozero <- leaps::regsubsets(
  x=f,
  data=transition_distances_scaled,
  nvmax = 25,
  method="exhaustive")

# lm_regsubsets_ML_zero #### 
transition_distances <- transition_distances_ebola_ml
transition_distances_scaled<-standardize_pred(transition_distances)
predictors<-transition_distances_scaled[!colnames(transition_distances_scaled) %in% c("Transitions", "Transition_Rate")]
f<-as.formula(object = paste0("Transitions ~ ", paste(colnames(predictors), collapse = " + ")))
lm_regsubsets_ML_zero <- leaps::regsubsets(
  x=f,
  data=transition_distances_scaled,
  nvmax = 25,
  method="exhaustive")

# lm_regsubsets_TT_nozero #### 
transition_distances <- transition_distances_ebola_tt[transition_distances_ebola_tt$Transitions!=0,]
transition_distances_scaled<-standardize_pred(transition_distances)
predictors<-transition_distances_scaled[!colnames(transition_distances_scaled) %in% c("Transitions", "Transition_Rate")]
f<-as.formula(object = paste0("Transitions ~ ", paste(colnames(predictors), collapse = " + ")))
lm_regsubsets_TT_nozero <- leaps::regsubsets(
  x=f,
  data=transition_distances_scaled,
  nvmax = 25,
  method="exhaustive")

# lm_regsubsets_TT_zero #### 
transition_distances <- transition_distances_ebola_tt
transition_distances_scaled<-standardize_pred(transition_distances)
predictors<-transition_distances_scaled[!colnames(transition_distances_scaled) %in% c("Transitions", "Transition_Rate")]
f<-as.formula(object = paste0("Transitions ~ ", paste(colnames(predictors), collapse = " + ")))
lm_regsubsets_TT_zero <- leaps::regsubsets(
  x=f,
  data=transition_distances_scaled,
  nvmax = 25,
  method="exhaustive")

# lm_regsubsets_TT_nozero_rates #### 
transition_distances <- transition_distances_ebola_tt[transition_distances_ebola_tt$Transitions!=0,]
transition_distances_scaled<-standardize_pred(transition_distances, response = "Transition_Rates")
predictors<-transition_distances_scaled[!colnames(transition_distances_scaled) %in% c("Transitions", "Transition_Rates")]
f<-as.formula(object = paste0("Transition_Rates ~ ", paste(colnames(predictors), collapse = " + ")))
lm_regsubsets_TT_nozero_rates <- leaps::regsubsets(
  x=f,
  data=transition_distances_scaled,
  nvmax = 25,
  method="exhaustive")


# lm_regsubsets_TT_zero_rates #### 
transition_distances <- transition_distances_ebola_tt
transition_distances_scaled<-standardize_pred(transition_distances, response = "Transition_Rates")
predictors<-transition_distances_scaled[!colnames(transition_distances_scaled) %in% c("Transitions", "Transition_Rates")]
f<-as.formula(object = paste0("Transition_Rates ~ ", paste(colnames(predictors), collapse = " + ")))
lm_regsubsets_TT_zero_rates <- leaps::regsubsets(
  x=f,
  data=transition_distances_scaled,
  nvmax = 25,
  method="exhaustive")


lm_regsubsets_MCC_nozero_plot<-ggregsubsets(lm_regsubsets_MCC_nozero, "bic", label_bool = F)
lm_regsubsets_MCC_zero_plot<-ggregsubsets(lm_regsubsets_MCC_zero, "bic", label_bool = F)
lm_regsubsets_MP_nozero_plot<-ggregsubsets(lm_regsubsets_MP_nozero,  "bic", label_bool = F)
lm_regsubsets_MP_zero_plot<-ggregsubsets(lm_regsubsets_MP_zero,  "bic", label_bool = F)
lm_regsubsets_ML_nozero_plot<-ggregsubsets(lm_regsubsets_ML_nozero, "bic" ,label_bool = F)
lm_regsubsets_ML_zero_plot<-ggregsubsets(lm_regsubsets_ML_zero, "bic" ,label_bool = F)
lm_regsubsets_TT_nozero_plot<-ggregsubsets(lm_regsubsets_TT_nozero, "bic" ,label_bool =F)
lm_regsubsets_TT_zero_plot<-ggregsubsets(lm_regsubsets_TT_zero, "bic" ,label_bool =F)
lm_regsubsets_TT_nozero_rates_plot<-ggregsubsets(lm_regsubsets_TT_nozero_rates, "bic", label_bool =T, padding = 17)
lm_regsubsets_TT_zero_rates_plot<-ggregsubsets(lm_regsubsets_TT_zero_rates, "bic", label_bool = T, padding = 5)


grid_regsubsets<-gridExtra::grid.arrange(ncol=2,widths = c(10,10.5) ,heights = c(5.5,5,5,5, 9.5),
                                         # layout_matrix=rbind(
                                         #   c(1,2),
                                         #   c(3,4),
                                         #   c(5,6),
                                         #   c(7,8),
                                         #   c(9,10),
                                         #   c(9,10)
                                         # ),
                                     arrangeGrob(lm_regsubsets_MCC_nozero_plot, top="Only observed transitons", left = textGrob("A", x = unit(1, "npc"), 
                                                                             y = unit(.85, "npc"),  gp = gpar(col = "black", fontsize = 28))), 
                                     arrangeGrob(lm_regsubsets_MCC_zero_plot, top="Including zero transitions",right="MCC", left = textGrob("B", x = unit(1, "npc"), 
                                                                             y = unit(.85, "npc"),  gp = gpar(col = "black", fontsize = 28))), 
                                     arrangeGrob(lm_regsubsets_MP_nozero_plot, left = textGrob("C", x = unit(1, "npc"), 
                                                                             y = unit(.95, "npc"),  gp = gpar(col = "black", fontsize = 28))), 
                                     arrangeGrob(lm_regsubsets_MP_zero_plot, right = "MP", left = textGrob("D", x = unit(1, "npc"), 
                                                                             y = unit(.95, "npc"),  gp = gpar(col = "black", fontsize = 28))),
                                     arrangeGrob(lm_regsubsets_ML_nozero_plot, left = textGrob("E", x = unit(1, "npc"), 
                                                                             y = unit(.95, "npc"),  gp = gpar(col = "black", fontsize = 28))),
                                     arrangeGrob(lm_regsubsets_ML_zero_plot, right = "ML",  left = textGrob("F", x = unit(1, "npc"), 
                                                                             y = unit(.95, "npc"),  gp = gpar(col = "black", fontsize = 28))),
                                     arrangeGrob(lm_regsubsets_TT_nozero_plot, left = textGrob("G", x = unit(1, "npc"), 
                                                                             y = unit(.95, "npc"),  gp = gpar(col = "black", fontsize = 28))),
                                     arrangeGrob(lm_regsubsets_TT_zero_plot, right = "TT", left = textGrob("H", x = unit(1, "npc"), 
                                                                             y = unit(.95, "npc"),  gp = gpar(col = "black", fontsize = 28))),
                                     arrangeGrob(lm_regsubsets_TT_nozero_rates_plot, left = textGrob("I", x = unit(1.6, "npc"), 
                                                                            y = unit(.95, "npc"),  gp = gpar(col = "black", fontsize = 28))),
                                     arrangeGrob(lm_regsubsets_TT_zero_rates_plot, right = textGrob("TT - Transition Rates" ,y = unit(.75, "npc"),rot=270), 
                                                                            left = textGrob("K", x = unit(1, "npc"), 
                                                                            y = unit(.95, "npc"),  gp = gpar(col = "black", fontsize = 28))))
ggsave(filename = "output/regsubsets.png", plot =grid_regsubsets, device = "png", width = 21, height = 29.7, units = "cm", dpi = 300) 

