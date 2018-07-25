# Code to extrapolate the path through the road network taken by an agent
# whose location is recorded at selected locations and timestamps.

# Author: Kirk A Boyer
source("utility.R")
source("predicted.path.R")
load("fixedData_with_nonuni_prob.RData")
load("tracker_datasets_SW7394.RData")
load("shorter_walks_7394.RData")
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



# for each dataset, get predicted path for each validation walk
predicted.paths <- list()
for (s in 4:13){
    predicted.paths[[s]] <- list()
    for (n in seq(200, 1000, 50)){
        start <- proc.time()
        predicted.paths[[s]] [[n]] <- list()
        cat("s, n:", s, ", ", n, "\n")
        for (w in 1:length(validation_walks)){
            predicted.paths[[s]] [[n]] [[w]] <- predict.path2(validation_walks[[w]], fixed_reduced_sub_g, trackers[[s]] [[n]])
        }
        cat("okay, ",(proc.time() - start), "\n")
    }
    save(predicted.paths, file="all.predicted.paths.RData")
}
