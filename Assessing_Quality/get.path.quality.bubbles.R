# Code to compute the "k-bubble" measure of the quality of a predicted path.
# To see how "good" a prediction is, see how many of its edges are within k
# streets from the actual path.

# Author: Kirk A Boyer

source("utility.R")
source("predicted.path.R")
load("all.predicted.paths.RData")
load("tracker_datasets_SW7394.RData")
load("shorter_walks_7394.RData")


# get all the neighborhood-bubble-unions for validation walks
if ("bubble.unions.RData" %in% list.files()){
    cat("Loading bubble.unions from file.")
    load("bubble.unions.RData")
} else {
    cat("Building Bubble Unions...")
    bubble.unions <- list()
    for (w in 1:length(validation_walks)){
        bubbles <- neighborhood(fixed_reduced_sub_g, order=3)
        bubbles <- sapply(bubbles, function(z) { z$name } )
        bubble.hash <- hash(V(fixed_reduced_sub_g)$name, bubbles)
        the.union <- list()
        the.walk <- validation_walks[[w]]
        for (n in 1:length(the.walk)){
            the.union[[n]] <- bubble.hash[[ the.walk[[n]] ]]
        }
        bubble.unions[[w]] <- unique(unlist(the.union))
    }
    save(bubble.unions, file="bubble.unions.RData")
}

cat("Made Bubble Unions")



bubbles.distance <- function(validation_id, pred.path){
    the.union <- bubble.unions[[validation_id]]
    return( sum(pred.path %in% the.union) )
}


path.quality.bubbles <- list()

counter <- 1
for (s in 4:13){
    for (n in seq(200, 1000, 50)){
        start <- proc.time()
        cat("Calculating bubble quality of paths for s=", s, ", n=", n, "\n")
        for (w in 1:length(predicted.paths[[s]][[n]])){
            if (!is.null(predicted.paths[[s]] [[n]] [[w]]) && length(predicted.paths[[s]] [[n]] [[w]]) > 0){
                pred.path <- predicted.paths[[s]] [[n]] [[w]]
                output.object <- c(s, n, bubbles.distance(w, pred.path)/length(validation_walks[[w]]), w)

                path.quality.bubbles[[counter]] <- output.object
                counter <- counter + 1
            } else {
                # some paths crossed no trackers, so....
                output.object <- c(s, n, 0, w)
                path.quality.bubbles[[counter]] <- output.object
                counter <- counter + 1
            }
        }   
        cat("  -- Took", (proc.time() - start)[[3]], "seconds.\n")
    }   
}

path.quality.bubbles <- data.frame(t(mapply(rbind, path.quality.bubbles)))
names(path.quality.bubbles) <- c("separation", "ntrackers", "bubble.distance", "val.walk.id")

save(path.quality.bubbles, file="path.quality.bubbles.RData")

