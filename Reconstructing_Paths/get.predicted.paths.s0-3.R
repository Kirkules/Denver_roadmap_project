# Code to reconstruct paths in a road network around Denver from timestamped sightings
# Author: Kirk A Boyer
source("utility.R")
source("predicted.path.R")
load("fixedData_with_nonuni_prob.RData")
load("shorter_walks_7394.RData")
load("tracker_datasets_0_to_3_SW7394.RData")

library(hash)

tracker.pair.hash <- hash()


sp.btwn.trackers <- function(t1, t2){
    pair.name <- paste(t1, t2, sep="-")
    if (!is.null(tracker.pair.hash[[pair.name]])){
        return( tracker.pair.hash[[pair.name]] )
    }
    
    sp <- get.shortest.paths(fixed_reduced_sub_g, from=t1, to=t2)[[1]][[1]]
    tracker.pair.hash[[pair.name]] <- sp
    return( sp )
}


predict.path2 <- function(path, graph, trackers){
    # get trackers the path crossed
    crossed.trackers <- path[path %in% trackers]
    # crossed.trackers <- trackers[trackers %in% path]
    
    if (length(crossed.trackers) < 2){ 
        return(crossed.trackers)
    }   
    result <- c()       # is the shortest path for all the give nodes.
    for (i in 2:length(crossed.trackers)){
        this.shortest.path <- sp.btwn.trackers(crossed.trackers[i-1], crossed.trackers[i])
        result <- c(result, this.shortest.path)
    }   
    return( names(result) )
}

if (!exists("predicted.paths.s0")){
    predicted.paths.s0 <- list()
    for (n in seq(200, 1000, 50)){
        start <- proc.time()
        predicted.paths.s0[[n]] <- list()
        cat("s, n:", 0, ", ", n, "\n")
        for (w in 1:length(validation_walks)){
            predicted.paths.s0[[n]] [[w]] <- predict.path2(validation_walks[[w]], fixed_reduced_sub_g, trackers0[[n]])
        }
        cat("okay, ",(proc.time() - start), "\n")
    }
}

# for each dataset, get predicted path for each validation walk
predicted.paths.s1to3 <- list()
for (s in 1:3){
    predicted.paths.s1to3[[s]] <- list()
    for (n in seq(200, 1000, 50)){
        start <- proc.time()
        predicted.paths.s1to3[[s]] [[n]] <- list()
        cat("s, n:", s, ", ", n, "\n")
        for (w in 1:length(validation_walks)){
            predicted.paths.s1to3[[s]] [[n]] [[w]] <- predict.path2(validation_walks[[w]], fixed_reduced_sub_g, trackers1to3[[s]] [[n]])
        }
        cat("   - took , ",(proc.time() - start), " seconds\n")
    }
    save(predicted.paths.s1to3, predicted.paths.s0, file="all.predicted.paths.s0-3.RData")
}
