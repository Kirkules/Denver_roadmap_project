# Code to make trackers on a road network.
# Author: Kirk A Boyer

load("longwalk.RData")
source("make_trackers.R")

trackers1to3 <- list()
trackers0 <- list()

# because Jingwei's code doesn't use largest component yet
largest.component <- fixed_reduced_sub_g

load("shorter_walks_7394.RData")

# save(trackers, file="new_tracker_datasets.RData")
training_frac <- 5/6
training_walks <- shorter_walks_7394[1:(training_frac*length(shorter_walks_7394))]
validation_walks <- shorter_walks_7394[(training_frac*length(shorter_walks_7394)):length(shorter_walks_7394)]


for (n in seq(200, 1000, 50)){
    cat("starting with sep=", 0, "and n=", n, "\n")
    start <- proc.time()
    trackers0[[n]] <- get.graph.sep.trackers.by.freq2(largest.component, ntrackers=n, sep=0, rw_list=training_walks, go.slowly=FALSE)
    cat("   - Took", (proc.time() - start)[[3]], " seconds.\n")
}


for (s in 1:3){
    trackers1to3[[as.character(s)]] <- list()
    for (n in seq(200, 1000, 50)){
        cat("starting with sep=", s, "and n=", n, "\n")
        start <- proc.time()
        trackers1to3[[as.character(s)]] [[n]] <- get.graph.sep.trackers.by.freq2(largest.component, ntrackers=n, sep=s, rw_list=training_walks, go.slowly=FALSE)
        cat("   - Took", (proc.time() - start)[[3]], " seconds.\n")
    }
}


save(trackers0, trackers1to3, validation_walks, file="tracker_datasets_0_to_3_SW7394.RData")
