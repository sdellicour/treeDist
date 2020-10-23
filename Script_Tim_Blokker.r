library(ape)
source("readAnnotatedNexus.r")
tree1 = read.nexus("input/h3_small_sample.MCC.tre")
tree2 = readAnnotatedNexus("input/h3_small_sample.MCC.tre")
countries = read.table("input/Sampling_locations.txt")


ancestral_positions = ape::ace(countries[,2], tree1, type="discrete")

comparison = matrix(nrow=dim(ancestral_positions$lik.anc)[1], ncol=2); 
difs = 0
for (i in 1:dim(comparison)[1])
	{
  		index = which(tree2$edge[,2]==(1389+i))
		if (length(index) == 0)
			{
				comparison[i,1] = tree2$root.annotation$states
			}	else	{
				comparison[i,1] = tree2$annotations[[index]]$states
			}
		index = which(ancestral_positions$lik.anc[i,]==max(ancestral_positions$lik.anc[i,]))
		comparison[i,2] = colnames(ancestral_positions$lik.anc)[index]
		if (comparison[i,1] != comparison[i,2]) difs = difs + 1
}
#1388 internal nodes including root
#1389 tip nodes
tree1$edge
tree1$Nnode[1]

