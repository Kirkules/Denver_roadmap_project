# Code to identify which trackers are crossed by a known path in a
# road network, for a given set of tracker locations

# Author: Kirk A Boyer
source("utility.R")
source("predicted.path.R")
load("all.predicted.paths.RData")
load("tracker_datasets_SW7394.RData")
load("shorter_walks_7394.RData")


# get number of trackers crossed by each path in the given dataset
trackers.crossed <- list()
counter <- 1
for (s in 4:13){
    trackers.crossed[[s]] <- list()
    for (n in seq(200, 1000, 50)){
        trackers.crossed[[s]] [[n]] <- list()
        cat("Calculating # trackers crossed for s=", s, ", n=", n, "\n")
        for (w in 1:length(predicted.paths[[s]][[n]])){
            # some paths crossed no trackers, so....
            orig.path <- validation_walks[[w]]
            trackers.crossed [[s]] [[n]] [[w]] <- 0
            trackers.crossed [[s]] [[n]] [[w]] <- orig.path[orig.path %in% trackers[[s]] [[n]]]
        }   
    }   
}

save(trackers.crossed, file="trackers.crossed.RData")
