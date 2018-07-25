# Code to get an R list of the directions of edges in the roadmap data.
# Edge direction just refers to whether streets are one-way, and which way
# that is.

# Author: Kirk A Boyer

get_edge_direction_list <- function (g,nodes, adj_list) {

    vertices <- V(g) 
    edge_direction_list <- lapply(vertices, function(x) {
        
        count <- length(adj_list[[x]])
        from <- names(vertices[x])
        print(from)
        result <- vector()

        if(count > 0) {
		s_x <- nodes[from,2]
		s_y <- nodes[from,1]
		#print(from_info)
		cat("sx",s_x," sy",s_y,"\n")
            for(i in 1:count) {
			print(adj_list[[x]][i])
			t_x <- nodes[adj_list[[x]][i],2]
			t_y <- nodes[adj_list[[x]][i],1]
			#cat("tx",t_x," ty",t_y,"\n")

			#cat("to_info",to_info,"\n")
		#dx is the diff in longitude
		#dy is the diff in latitude	
			dx <- t_x - s_x
			dy <- t_y - s_y
			
			#cat("dx",dx," dy",dy,"\n")

		#angle is defined in [-90,270]
			ans <- atan(dy/dx)
			if( dx<0 )
				ans <- ans+pi
			result[i] <- ans/pi*180
			print(result[i])
            }
        }

        result
    })
    edge_direction_list
}

edge_direction_list <- get_edge_direction_list(reduced_sub_g,fixed_reduced_sub_g_nodes,adj_list)
    save(edge_direction_list, file="edge_direction_list.RData")

get_vertex_coordinate_list <- function (g,nodes) {

    vertices <- V(g) 
    vertex_coordinate_list <- lapply(vertices, function(x) {
        
	vname <- names(vertices[x])
	result <- vector(,2)
	result[1] <- nodes[vname,1]
	result[2] <- nodes[vname,2]

	result
    })
    vertex_coordinate_list
}

vertex_coordinate_list <- get_vertex_coordinate_list(reduced_sub_g,fixed_reduced_sub_g_nodes)

    save(vertex_coordinate_list, file="vertex_coordinate_list.RData")


