# Code to generate a random walk with given start point S and target T
# Author: Kirk A Boyer

random.walk <- function(adj_list, prob_list, edge_direction_list, coor_list,neighborhoods_by_node, start, target, len=10000,min_prob=0.1,times_of_scaling=2) {
	s_coor <- coor_list[[start]]
	t_coor <- coor_list[[target]]
	
	ans <- atan((t_coor[1]-s_coor[1])/(t_coor[2]-s_coor[2]))
	if( (t_coor[2]-s_coor[2]) < 0 )
		ans <- ans+pi
	s_t_angle <- ans/pi*180
#first of all, compute the direction from start to target
	cat("st_angle",s_t_angle,"\n")

	cur <- start
	prev <- start
	path <- vector("character",len)
	path[1] <- start
	pathLength <- 1
cat("start",start,"target",target,"\n")
	while(pathLength < len){
	cat("cur", cur,"\n")
	cat("prev", prev,"\n")
	
		#if(pathLength%%1000==0)
			#cat("pathLength:",pathLength,"\n")

	#if we reached the neighborhoods, we just assume we know the way,
	#we will just take the shortest path.
		if(as.logical(values(neighborhoods_by_node,cur)==values(neighborhoods_by_node,target))){
			cat("reaching target neiborhood",cur,"\n")
			#pathLength <- pathLength+1
			#path[pathLength] <- target
			path <- path[1:pathLength]
			break;
		}
	#otherwise we have not get to the target
		nbs <- adj_list[[cur]]
		
		choice <- 1
		if(length(nbs)<1){
			cat("no outlet, cur:",cur,"\n")
			path <- path[1:pathLength]
			break
		}

	#otherwise there are some out going edges

	#if there is only one way out, we just choose that
	#otherwise we would firstly rule out previous node to get rid of u-turns
		else if(length(nbs)>1){
			probs <- prob_list[[cur]]
		print("neighbors")
		print(nbs)
		print("original prob")
		print(probs)
			prev_idx <- 0
			for(k in 1:length(nbs)){
				if(nbs[k]==prev){
					prev_idx <- k
					break;
				}
			}
			if(prev_idx>0 ){
				if(probs[prev_idx]<1){
		
					ratio <- 1/(1-probs[prev_idx])
					for(k in 1:length(probs)){
						if(k==prev_idx){
							probs[k] <- 0
						}
						else{
							probs[k] <- probs[k]*ratio
						}
					}
		print("after taking out prev")
		print(probs)
				}
			}
			angles <- edge_direction_list[[cur]]
			angle_diff <- vector(,length(angles))

			nan <- is.nan(angles)
			for(k in 1:length(angles)){
				if(nan[k]==TRUE)
					angle_diff[k] <- 90
				else
					angle_diff[k] <- 	s_t_angle - angles[k]
			#round  angle_diff[k] to [-180,180]
	 			if(angle_diff[k] > 180)
					angle_diff[k] <- angle_diff[k] -360
				else if(angle_diff[k] < -180)
					angle_diff[k] <- angle_diff[k] +360
			#take the absolute value,
			#now angle_diff[k] is round to [0,180]
				if(angle_diff[k] <0)
					angle_diff[k] <- 0-angle_diff[k]
						
			}

		print("angle_diff")
		print(angle_diff)
			for(l in 1:times_of_scaling){
				sum_scaled_prob <- 0
				for(k in 1:length(probs)){
					probs[k] <- ((min_prob-1)/180*angle_diff[k]+1)*probs[k]
					sum_scaled_prob <- sum_scaled_prob +probs[k]
				}
				amp <- 1/sum_scaled_prob
				for(k in 1:length(probs)){
						probs[k] <- amp * probs[k]
				}
		print("after scaling by angle")
		print(probs)
			}
		#print("angles")
		#print(angles)

		print("after scaling by angle")
		print(probs)
		#now we have scaled the probabilities
			rand <- runif(1,0.0,1)
		
			for(k in 1:length(probs)){
				if(rand < probs[k]){
					choice <- k
					break
				}
				else
					rand <- rand - probs[k]
			}
		}
	
		#cat("choice",choice,"\n")
		vname<- nbs[choice]
		prev <- cur
		cur <- vname
		path[pathLength+1] <- cur
		pathLength <- pathLength + 1	
	}
	cat("pathLength",pathLength,"\n")
	path
}

	s <- as.character( sample(neighborhoods[[s_t_nb[1]]],1))
	t <- as.character( sample(neighborhoods[[s_t_nb[2]]],1))
onewalk <- random.walk(adj_list,prob_list,edge_direction_list,vertex_coordinate_list,neighborhoods_by_node,s,t,10)

timer_start <- proc.time()
shorter_walks <- list()
idx <- 1 
for(i in 1:10000){
	cat("i=",i,"\n")
	s_t_nb <- sample(length(neighborhoods),2)

	s <- as.character( sample(neighborhoods[[s_t_nb[1]]],1))
	t <- as.character( sample(neighborhoods[[s_t_nb[2]]],1))

	ter_len <- 500
	for(j in 1:3){
		onewalk <- random.walk(adj_list,prob_list,edge_direction_list,vertex_coordinate_list,neighborhoods_by_node,s,t,ter_len)
		if(length(onewalk)<ter_len){
			nb_to_t <- get.shortest.paths(fixed_reduced_sub_g,from=onewalk[length(onewalk)],to=t,mode="all")[[1]][[1]]$name
			onewalk <- onewalk[-length(onewalk)]
			onewalk <- append(onewalk,nb_to_t)
			shorter_walks[[idx]] <- onewalk
			idx <- idx + 1 
			break
		}
	}
}
length(shorter_walks)
proc.time()-timer_start

shorter_walks_7438 <- shorter_walks

save(shorter_walks_7438,file="shorter_walks_7438.RData")



shorter_walks_7394 <- shorter_walks
save(shorter_walks_7394,file="shorter_walks_7394.RData")

len <- vector()
for(i in 1:length(shorter_walks)){
	len[i] <- length(shorter_walks[[i]])
}
shorter_walks3 <- shorter_walks
save(shorter_walks2,file="shorter_walks2.RData")
length(unique(shorter_walks[[6]]))
len
sort(len)
plot(sort(len))
shorter_walks[[1]]
save(shorter_walks,file="shorter_walks.RData")
shorter_walks1 <- shorter_walks
shorter_walks2 <- shorter_walks


get.shortest.paths(fixed_reduced_sub_g,from="3945",to="3946",mode="all")[[1]][[1]]$name

for( i in 5:length(shorter_walks)){
	path <- shorter_walks[[i]]
	length(path)
 	plot.path(path, path[length(path)])
}

save(shorter_walks,file="shorter_walks.RData")
