# Code for reducing road network data.
# The data is not from a publicly available dataset.
# Author: Kirk A Boyer
require("igraph")

## function remove_nonintersections(g) 
# @param g: igraph version of the graph
# @return a graph in igraph represention with non-inersection nodes removed
remove_nonintersections <- function(g) {

    vertex_names <- names(V(g))

    ptm <- proc.time()
    len <- length(vertex_names)
    for(i in 1:len) {

        cat(i, "outof", len, "\n")
        t <- proc.time() - ptm
        t <- as.integer(t)[3]
        cat("running time:", t)
        cat("s, remaining:", t * len / i - t, " s\n")

	## group vertices based on its in-degree and out-degee

        in_edges <- incident(g, vertex_names[i], "in")
        out_edges <- incident(g, vertex_names[i], "out")

        in_count <- length(in_edges)
        out_count <- length(out_edges)

        if(in_count < 3 && out_count < 3) {
            # 1) in <- 0 || out <- 0
            # skip
            if(in_count == 0 || out_count == 0) {
                next
            }

            get_adjs <- function(v, e) {

                tmp_list <- strsplit(as_ids(e), "[|]")

                result <- vector()
                for(j in 1:length(tmp_list)) {
                    result <- c(result, as.vector(tmp_list[[j]]))
                }


                result <- unique(result[result != v])

            }

            the_adjs <- get_adjs(vertex_names[i], c(in_edges, out_edges)) 

            # skip intersections or no outlet nodes
            if(length(the_adjs) != 2) {
                next
            }

            # 2) in <- 2 && out <- 1
            if(in_count == 2 && out_count == 1) {
                print("2, 1")
                to <- get_adjs(vertex_names[i], out_edges)
                from <- the_adjs[the_adjs != to]
                
                edge_str1 <- paste(from, vertex_names[i], sep="|")
                edge_str2 <- paste(vertex_names[i], to, sep="|")
                new_weight <- E(g)[edge_str1]$weights + E(g)[edge_str2]$weights
                g <- add_edges(g, c(from, to), attr=list(weights=new_weight))
                g <- delete_edges(g, c(from, vertex_names[i]))
                g <- delete_edges(g, c(vertex_names[i], to))

            }

            # 3) in <- 1 && out <- 2 
            else if(in_count == 1 && out_count == 2) {
                print("1, 2")
                from <- get_adjs(vertex_names[i], in_edges)
                to <- the_adjs[the_adjs != from]
                edge_str1 <- paste(from, vertex_names[i], sep="|")
                edge_str2 <- paste(vertex_names[i], to, sep="|")
                new_weight <- E(g)[edge_str1]$weights + E(g)[edge_str2]$weights
                g <- add_edges(g, c(from, to), attr=list(weights=new_weight))
                g <- delete_edges(g, c(from, vertex_names[i]))
                g <- delete_edges(g, c(vertex_names[i], to))
            }

            # 4) in <- 2 && out <- 2
            else if(in_count == 2 && out_count == 2) {
                print("2,2")

                new_weight <- sum(in_edges$weights)
                #for(v_ in the_adjs) {
                #    new_weight <- new_weight + in_edges[ v_ %->% vertex_names[i]]$weights      
                #}
                from <- the_adjs[1]
                to <- the_adjs[2]
                g <- add_edges(g, c(from, to), attr=list(weights=new_weight))
                g <- add_edges(g, c(to, from), attr=list(weights=new_weight))
                g <- delete_vertices(g, vertex_names[i])
                
            }

            # 5) in <- 1 && out <- 1
            else if(in_count == 1 && out_count == 1) {
                print("1,1")
                if(length(the_adjs) == 2) {

                    from <- get_adjs(vertex_names[i], in_edges)
                    to <- get_adjs(vertex_names[i], out_edges)

                    new_weight <- sum(c(in_edges$weights, out_edges$weights))

                    g <- add_edges(g, c(from, to), attr=list(weights=new_weight))
                    g <- delete_vertices(g, vertex_names[i])
                }
            }

        }
    }
    # return the new graph
    g
}


## function count_0_out
# helper function: see how many vertices are 0-out
# @param: igraph g
# @return: return the number of 0-out nodes 

count_0_out <- function (g) {

    vertex_names <- names(V(g))

    len <- length(vertex_names)

    result <- vector()

    for(i in 1:len) {
        out_edges <- incident(g, vertex_names[i], "out")
        if(length(out_edges) == 0) {
           result[length(result) + 1] <- vertex_names[i]
        }
    }
    result
}


