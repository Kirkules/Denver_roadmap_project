# Code for assigning random probabilities to out-edges from a given node, as
# opposed to assigning the same probability to each out-edge (uniform 
# distribution on out-edges).

# Author: Kirk A Boyer

reduced_sub_g <- fixed_reduced_sub_g
vertices <- V(reduced_sub_g)
edges <- E(reduced_sub_g)

edge_attr(reduced_sub_g,"prob") <- 0

for(i in 1:length(vertices)){
	if(i%%1000==0)
	print(i)
	outcomingEdges <-edges[vertices[i] %->% neighbors(reduced_sub_g,vertices[i],mode="out")] 
	
	#non-uniform

	#max number j could be is 5
	j<- length(outcomingEdges)
	if( j >=1){
		if(j==1)
			edge_attr(reduced_sub_g,"prob",outcomingEdges[1]) <- 1	
		else if(j>=2){
			chop <- runif(j-1,-1/(3*j),1/(3*j))
			chop <- c(0,chop)
			for(k in 1:j-1){
				edge_attr(reduced_sub_g,"prob",outcomingEdges[k]) <- 1/j + chop[k+1]-chop[k]
			}
			edge_attr(reduced_sub_g,"prob",outcomingEdges[j]) <- 1/j - chop[j]
		}
	}
}
edge_attr(reduced_sub_g,"prob")[1:100]

adj <- as_edgelist(reduced_sub_g)
adj[]

edge_attr(reduced_sub_g,"prob")[1:100]


prob<-edge_attr(reduced_sub_g,"prob")
min(prob)

length(prob[prob==1])
save(reduced_sub_g, file="reduced_with_non_uni_prob.RData")
