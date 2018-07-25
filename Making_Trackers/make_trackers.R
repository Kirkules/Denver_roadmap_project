# Author: Kirk Boyer
# 2015 Privacy in Large Databases Project

require(igraph)
require(plyr)
load("fixed_reduced_sub_g.RData")
load("neighborhoods.hash.RData")

# load("randomwalk1.RData")

# rw1 <- random_walk(sub_g, start=1, steps=.Machine$integer.max, mode="out")

largest.component <- decompose.graph(fixed_reduced_sub_g, min.vertices=1000)[[1]]

get.graph.sep.trackers.by.freq2 <- function(graph, ntrackers=200, sep=10, rw_list=NULL, color=NULL, go.slowly=FALSE){
    rw <- unlist(rw_list)
    
    if (!is.null(color)){
        plot(fixed_reduced_sub_g_nodes[,2:1], pch=20, cex=0.2, main=paste("Separation:", sep), xaxt='n', yaxt='n')
    }
    
    # if no random walk passed, make a new one
    rwcount <- count(rw)
    rwcount <- rwcount[order(rwcount$freq, decreasing=TRUE),]

    # if there are fewer nodes seen than we want trackers (unlikely...) then return all nodes
    if (length(rwcount$x) < ntrackers){
        return(rwcount$x)
    }

    rwcount$x <- as.character(rwcount$x)
    rwcount$tracked <- FALSE
    rwcount$unexplored <- TRUE
    
    # take most frequent edge, then take next most frequent
    # edge that isn't too close.
		
    while (sum(rwcount$tracked) < ntrackers){
        # if no more candidates, reset unexplored candidates
        if (all(!(rwcount$unexplored & !rwcount$tracked))){
            rwcount$unexplored <- TRUE
        }

        # otherwise add the most frequent one left in the unexplored list
        # order is preserved when subsetting, so first item is most frequent
        best.index <- which.max(rwcount$unexplored & (!rwcount$tracked))
        rwcount$tracked[best.index] <- TRUE

        if (!is.null(color)){
            points(fixed_reduced_sub_g_nodes[rwcount$x[best.index], 2:1], col=color, pch=20, cex=1, xlab=NULL, ylab=NULL, xaxt='n', yaxt='n')
            # print(fixed_reduced_sub_g_nodes[rwcount$x[best.index], 2:1])
            if (go.slowly){
                Sys.sleep(.05)
            }
        }

        # count everything within sep as "explored"
        nbrs <- neighborhood(graph, order=sep, nodes=rwcount$x[best.index], mode="all")[[1]]$name

        rwcount$unexplored[rwcount$x %in% nbrs] <- FALSE
    }
    return( rwcount$x[rwcount$tracked] )
}



get.graph.sep.trackers.by.freq <- function(graph, ntrackers=200, sep=10, rw=NULL, color=NULL, go.slowly=FALSE){
   
    if (!is.null(color)){
        plot(fixed_reduced_sub_g_nodes[,2:1], pch=20, cex=0.2, main=paste("Separation:", sep), xaxt='n', yaxt='n')
    }
  
    # if no random walk passed, make a new one
    rwcount <- count(rw)
    rwcount <- rwcount[order(rwcount$freq, decreasing=TRUE),]

    # if there are fewer nodes seen than we want trackers (unlikely...) then return all nodes
    if (length(rwcount$x) < ntrackers){
        return(rwcount$x)
    }

    rwcount$x <- as.character(rwcount$x)
    rwcount$tracked <- FALSE
    rwcount$unexplored <- TRUE
    
    # take most frequent edge, then take next most frequent
    # edge that isn't too close.
		
    while (sum(rwcount$tracked) < ntrackers){
        # if no more candidates, reset unexplored candidates
        if (all(!(rwcount$unexplored & !rwcount$tracked))){
            rwcount$unexplored <- TRUE
        }

        # otherwise add the most frequent one left in the unexplored list
        # order is preserved when subsetting, so first item is most frequent
        best.index <- which.max(rwcount$unexplored & (!rwcount$tracked))
        rwcount$tracked[best.index] <- TRUE
        if (!is.null(color)){
            points(fixed_reduced_sub_g_nodes[rwcount$x[best.index], 2:1], col=color, pch=20, cex=1, xlab=NULL, ylab=NULL, xaxt='n', yaxt='n')
            # print(fixed_reduced_sub_g_nodes[rwcount$x[best.index], 2:1])
            if (go.slowly){
                Sys.sleep(.05)
            }
        }
        # count everything within sep as "explored"
        nbrs <- neighborhood(graph, order=sep, nodes=rwcount$x[best.index], mode="all")[[1]]$name

        rwcount$unexplored[rwcount$x %in% nbrs] <- FALSE
    }
    return( rwcount$x[rwcount$tracked] )
}


