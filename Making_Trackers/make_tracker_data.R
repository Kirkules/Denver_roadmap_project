# Code to place trackers (at random) on a road network, given some parameters.

# Author: Kirk A Boyer

load("longwalk.RData")
source("make_trackers.R")

trackers <- list()

# because Jingwei's code doesn't use largest component yet
largest.component <- fixed_reduced_sub_g


if (FALSE){# BEGIN COMMENT BLOCK
for (s in 4:13){
    trackers[[s]] <- list()
    for (n in seq(200, 1000, 50)){
        cat("starting with sep=", s, "and n=", n, "\n")
        trackers[[s]] [[n]] <- get.graph.sep.trackers.by.freq(largest.component, ntrackers=n, sep=s, rw=longwalk)
    }
}
}# END COMMENT BLOCK




# save(trackers, file="new_tracker_datasets.RData")
training_frac <- 5/6
training_walks <- shorter_walks_7394[1:(training_frac*length(shorter_walks_7394))]
validation_walks <- shorter_walks_7394[(training_frac*length(shorter_walks_7394)):length(shorter_walks_7394)]

load("shorter_walks_7394.RData")
for (s in 1:13){
    trackers[[s]] <- list()
    for (n in seq(200, 1000, 50)){
        cat("starting with sep=", s, "and n=", n, "\n")
        start <- proc.time()
        trackers[[s]] [[n]] <- get.graph.sep.trackers.by.freq(largest.component, ntrackers=n, sep=s, rw_list=training_walks, color="red", go.slowly=TRUE)
        cat("   - Took", (proc.time() - start)[[3]], " seconds.\n")
    }
}


save(trackers, validation_walks, file="tracker_datasets_SW7394.RData")
