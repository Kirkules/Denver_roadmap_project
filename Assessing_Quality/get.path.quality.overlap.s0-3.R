# Code to compute the "overlap" measure of the quality of a predicted path.
# To see how "good" a prediction is, see how many of its edges are actually on
# the original path.

# Author: Kirk A Boyer

source("utility.R")
source("predicted.path.R")
load("all.predicted.paths.RData")
load("tracker_datasets_SW7394.RData")
load("shorter_walks_7394.RData")


# for each predicted path, get distance from actual path

path.quality.overlap.s1to3 <- list()
path.quality.overlap.s0 <- list()


counter <- 1
for (n in seq(200, 1000, 50)){
    cat("Calculating quality of paths for s=", 0, ", n=", n, "\n")
    for (w in 1:length(predicted.paths.s0[[n]])){
        if (!is.null(predicted.paths.s0[[n]] [[w]]) && length(predicted.paths.s0[[n]] [[w]]) > 0){
            output.object <- c(0, n, overlap.distance(validation_walks[[w]], predicted.paths.s0[[n]] [[w]])/length(validation_walks[[w]]), w)
            path.quality.overlap.s0[[counter]] <- output.object
            counter <- counter + 1
        } else {
            # some paths crossed no trackers, so....
            output.object <- c(0, n, 0, w)
            path.quality.overlap.s0[[counter]] <- output.object
            counter <- counter + 1
        }
    }   
}   


counter <- 1
for (s in 1:3){
    for (n in seq(200, 1000, 50)){
        cat("Calculating quality of paths for s=", s, ", n=", n, "\n")
        for (w in 1:length(predicted.paths.s1to3[[s]][[n]])){
            if (!is.null(predicted.paths.s1to3[[s]] [[n]] [[w]]) && length(predicted.paths.s1to3[[s]] [[n]] [[w]]) > 0){
                output.object <- c(s, n, overlap.distance(validation_walks[[w]], predicted.paths.s1to3[[s]] [[n]] [[w]])/length(validation_walks[[w]]), w)
                path.quality.overlap.s1to3[[counter]] <- output.object
                counter <- counter + 1
            } else {
                # some paths crossed no trackers, so....
                output.object <- c(s, n, 0, w)
                path.quality.overlap.s1to3[[counter]] <- output.object
                counter <- counter + 1
            }
        }   
    }   
}

path.quality.overlap.s0 <- data.frame(t(mapply(rbind, path.quality.overlap.s0)))
names(path.quality.overlap.s0) <- c("separation", "ntrackers", "overlap.distance", "val.walk.id") 
path.quality.overlap.s1to3 <- data.frame(t(mapply(rbind, path.quality.overlap.s1to3)))
names(path.quality.overlap.s1to3) <- c("separation", "ntrackers", "overlap.distance", "val.walk.id") 

save(path.quality.overlap.s0, path.quality.overlap.s1to3, file="path.quality.overlap.s0-3RData")
