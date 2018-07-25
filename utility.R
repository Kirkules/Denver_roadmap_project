# General useful functions for working on road network data for this project.

# Author: Kirk A Boyer

library(igraph)
library(hash)

load("fixed_reduced_sub_g.RData")
source("gdist.R")

get.neighborhoods.hash <- function(graph){
     return( hash(sapply(V(graph)$name, function(x) { neighbors(graph, x, mode="out")$name})) )
}

get.lat.long <- function(){
    nodes <- readLines("denver.nodes.dat")
    nodes <- strsplit(nodes, " ")
    nodes <- data.frame(matrix(unlist(nodes), ncol=3, byrow=T))
    names(nodes) <- nodes[1,]
    nodes <- nodes[-1,]
    return( nodes )
}

plot.map <- function(){
    par(mar=c(1,1,1,1))
    plot(fixed_reduced_sub_g_nodes[,1:2], pch=20, cex=0.2, ylab=NULL, main=NULL, xlab=NULL, xaxt='n', yaxt='n')
}

plot.trackers <- function(trackers, color="red"){
    plot.map()
    points(fixed_reduced_sub_g_nodes[trackers, 1:2], col=color, pch=20, cex=1, xlab=NULL, ylab=NULL, xaxt='n', yaxt='n')
}

plot.path <- function(path, target, color="green", slowly=TRUE){
    plot.map()
    # draw path
    # points(fixed_reduced_sub_g_nodes[path, 1:2], col=color, pch=20, cex=1, xlab=NULL, ylab=NULL, xaxt='n', yaxt='n')
    # draw start point
    points(fixed_reduced_sub_g_nodes[path[[1]], 1:2], col="red", pch="O", cex=1, xlab=NULL, ylab=NULL, xaxt='n', yaxt='n')
    # draw target
    points(fixed_reduced_sub_g_nodes[target, 1:2], col="red", pch=8, cex=1, xlab=NULL, ylab=NULL, xaxt='n', yaxt='n')

    # slowly draw path
    for (i in 2:length(path)){
        if (slowly){
            Sys.sleep(.085)
        }
        lines(fixed_reduced_sub_g_nodes[path[[i]], 1:2], col="green", pch=20, cex=.3, xlab=NULL, ylab=NULL, xaxt='n', yaxt='n')
    }
}

overlap.distance <- function(path1, path2){
    return( length(path1[path1 %in% path2]) )
}

library(Unicode)
library(stringdist)
DL.distance <- function(path1, path2){
    p1u <- as.u_char(as.numeric(path1))
    p2u <- as.u_char(as.numeric(path2))
    return( stringdist(p1u, p2u, method="dl") )
}


# load("bubble.hash.RData")

build.bubble.unions <- function(path.list, bubble.size=3, graph=fixed_reduced_sub_g){
    get.union <- function(the.path){
        
        the.union <- Reduce(function(accum, b){
            nbhd <- bubble.hash[[as.character(b)]]
            return( c(accum, nbhd) )
        }, the.path, c())
        return( unique(the.union) )
    }

    list.of.unions <- lapply(path.list, get.union)
    return( list.of.unions )
}

# the "bubbles" distance measure is not symmetric, so first entry sould be original
bubbles.distance <- function(original.path, pred.path, bubble.size=3, the.union=NULL){
    if (is.null(the.union)){
        the.union <- Reduce(function(accum, b){
            nbhd <- neighborhood(fixed_reduced_sub_g, order=bubble.size, nodes=b, mode="all")[[1]]$name
            return( c(accum, b) )
        }, original.path, c())
    }
    return( sum(pred.path %in% unique(the.union)) )
}
