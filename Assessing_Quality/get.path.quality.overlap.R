# Code to compute the "overlap" measure of the quality of a predicted path.
# To see how "good" a prediction is, see how many of its edges are actually on
# the original path

# Author: Kirk A Boyer

source("utility.R")
source("predicted.path.R")
load("all.predicted.paths.RData")
load("tracker_datasets_SW7394.RData")
load("shorter_walks_7394.RData")


# for each predicted path, get distance from actual path

path.quality.overlap <- list()
counter <- 1
for (s in 4:13){
    for (n in seq(200, 1000, 50)){
        cat("Calculating quality of paths for s=", s, ", n=", n, "\n")
        for (w in 1:length(predicted.paths[[s]][[n]])){
            if (!is.null(predicted.paths[[s]] [[n]] [[w]]) && length(predicted.paths[[s]] [[n]] [[w]]) > 0){
                output.object <- c(s, n, overlap.distance(validation_walks[[w]], predicted.paths[[s]] [[n]] [[w]])/length(validation_walks[[w]]), w)
                path.quality.overlap[[counter]] <- output.object
                counter <- counter + 1
            } else {
                # some paths crossed no trackers, so....
                output.object <- c(s, n, 0, w)
                path.quality.overlap[[counter]] <- output.object
                counter <- counter + 1
            }
        }   
    }   
}

path.quality.overlap <- data.frame(t(mapply(rbind, path.quality.overlap)))
names(path.quality.overlap) <- c("separation", "ntrackers", "overlap.distance", "val.walk.id") 

save(path.quality.overlap, file="path.quality.overlap.RData")
