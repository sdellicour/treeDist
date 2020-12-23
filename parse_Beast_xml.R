library(xml2)
source("Functions.R")
x<-read_xml("2°_test/Makona_1610_cds_ig.joint_GLM.xml")
x_glm_model<-xml_child(xml_child(x, 33), 3)
design_matrix1<-xml_child(xml_child(x_glm_model, 1), 3)
design_matrix2<-xml_child(xml_child(x_glm_model, 2), 3)


#distance matrix names ####
transitons=data.frame()
for(i in 1:112){
  transitons[i,1]=xml_attrs(xml_child(design_matrix2, i))[1]
  transitons[i,2]=xml_attrs(xml_child(design_matrix2, i))[2]
}
names<-as.vector(sapply(transitons[,1], function(x) str_split(x,"_")[[1]][1])[1:56])

#distance matrix values ####
distances=data.frame()
for(i in 1:25){
  distances[i,1]=xml_attrs(xml_child(design_matrix1, i))[1]
  distances[i,2]=xml_attrs(xml_child(design_matrix1, i))[2]
}

distances[,1]
distances_num<-sapply(strsplit(distances[,2], " "), function(x) as.numeric(x))
for(i in 1:25){
  if(length(distances_num[[i]])==3081) distances_num[[i]]=distances_num[[i]][-1]
}

distances_final<-data.frame("names"=distances[,1], "values"=I(distances_num))
lapply(1:25, function(x){
  S<-diag(56)
  S[upper.tri(S, diag=F)]<-distances_final[x,2][[1]][1:(3080/2)]
  S[lower.tri(S, diag=F)]<-distances_final[x,2][[1]][(3080/2+1):3080]
  S=t(S)
  colnames(S)<-names
  rownames(S)<-names
  write.csv(S, file=paste0("2°_test/",distances_final[x,1], ".csv"))
})

##creating sampling locations from tree tips but removing all missing data tips since they will not be mapped ####
ebola<-read.beast("2°_test/Makona_1610_cds_ig.GLM.MCC.tree")
tips<-ebola@phylo[["tip.label"]]
missings<-c(which(as.vector(sapply(tips, function(x) str_split(x,"\\|")[[1]][5]))==""), 
            which(as.vector(sapply(tips, function(x) str_split(x,"\\|")[[1]][5]))=="?"),
            which(as.vector(sapply(tips, function(x) str_split(x,"\\|")[[1]][5]))=="WesternArea"))
ebola <-drop.tip(ebola,sort(missings, decreasing = T))
tips<-ebola@phylo[["tip.label"]]
sampling_locations<-as.vector(sapply(tips, function(x) str_split(x,"\\|")[[1]][5]))
write(sampling_locations, "2°_test/ebola_sampling_locations.txt")
a<-read.table("2°_test/ebola_sampling_locations.txt",blank.lines.skip = F)
write.beast(file = "2°_test/ebola_no_missings", treedata = ebola)
df<-as_tibble(ebola)
sort(unique(sampling_locations))
names
unique(names[names %!in% sampling_locations])
unique(sampling_locations[sampling_locations %!in% names])
  