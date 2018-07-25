# Code to draw examples showing reconstructed path, trackers, original path, etc.
# Author: Kirk A Boyer

load("fixed_reduced_sub_g.RData")
load("bubble.unions.RData")
load("trackers.crossed.RData")


# example with ~50% overlap score: validation path 48, s=4, ntrackers=200
draw.example <- function(which.s=7, which.ntrackers=400, which.path=202){

    # draw the background map
    par(mar=c(1,1,1,1))
    plot(fixed_reduced_sub_g_nodes[,1:2], pch=20, cex=0.14, ylab="", xlab="", xaxt='n', yaxt='n')


    # draw the "bubble region"
    points(fixed_reduced_sub_g_nodes[bubble.unions[[which.path]], 1:2], cex=0.4, pch=0, ylab="", xlab="", xaxt='n', yaxt='n', col="green")

    # draw the path
    lines(fixed_reduced_sub_g_nodes[validation_walks[[which.path]], 1:2], lwd=2, ylab="", xlab="", xaxt='n', yaxt='n', col="orange")


    # draw the predicted path

    lines(fixed_reduced_sub_g_nodes[predicted.paths[[which.s]][[which.ntrackers]][[which.path]], 1:2], cex=0.3, pch=19, ylab="", xlab="", xaxt='n', yaxt='n', col="blue")


    # draw all trackers
    points(fixed_reduced_sub_g_nodes[trackers[[which.s]][[which.ntrackers]], 1:2], pch=20, cex=0.4, ylab="", xlab="", xaxt='n', yaxt='n', col="red")

    # draw the trackers the path hit
    points(fixed_reduced_sub_g_nodes[unique(trackers.crossed[[which.s]][[which.ntrackers]][[which.path]]), 1:2], cex=2, ylab="", xlab="", xaxt='n', yaxt='n', col="red")

    dev.copy(png, paste("s", which.s, "n", which.ntrackers, "w", which.path, "example.png", sep=""))
    dev.off()
}
