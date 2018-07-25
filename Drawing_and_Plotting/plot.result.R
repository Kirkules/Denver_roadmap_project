# Code to plot the results of experiments (show trackers, or show a bubble region around a path)
# Author: Kirk A Boyer

get.qualified.percentage <- function(dataset,threshold){
	res <- list()
	for (s in 1:14){
		sublist <- list()
    		for (n in seq(200, 1000, 50)){
			qualities <- dataset[path.quality.bubbles$separation==(s-1) & path.quality.bubbles$ntrackers==n,3]
			ratio <- length(qualities[qualities>threshold])/length(qualities)
			sublist[[n]]<- ratio
   	 	}   
		res[[s]]<- sublist
	}
	return(res)
}

 
group.qualified.percentage.byS <- function(path.qualified_){
	res <- vector()
	for (s in 1:14){
		temp <- vector()
		for (n in seq(200, 1000, 50)){
			temp[n/50-3] <- path.qualified_[[s]][[n]]
		}
		res[s] <- mean(temp)
	}
	return(res)
}

path.quality.bubbles.size3_s4_13 <- path.quality.bubbles

path.quality.bubbles <- c()
path.quality.bubbles <- rbind(path.quality.bubbles.size3_s4_13, path.quality.bubbles.s1to3)
path.quality.bubbles <- rbind(path.quality.bubbles,path.quality.bubbles.s0)
nrow(path.quality.bubbles )

path.quality.bubbles<-path.quality.bubbles[order(path.quality.bubbles[,1]),]



#plot 1
path.quality.bubbles
path.bubble3_30 <- get.qualified.percentage(path.quality.bubbles,0.3)
path.bubble3_50 <- get.qualified.percentage(path.quality.bubbles,0.5)
path.bubble3_70 <- get.qualified.percentage(path.quality.bubbles,0.7)
path.bubble3_90 <- get.qualified.percentage(path.quality.bubbles,0.9)


path.bubble3_30.groupbys <- group.qualified.percentage.byS(path.bubble3_30)
path.bubble3_50.groupbys <- group.qualified.percentage.byS(path.bubble3_50)
path.bubble3_70.groupbys <- group.qualified.percentage.byS(path.bubble3_70)
path.bubble3_90.groupbys <- group.qualified.percentage.byS(path.bubble3_90)




plot(c(0:13),path.bubble3_30.groupbys,type="l",col=2,main="Bubble size=3",ylim=c(0,1),xlab="separation",ylab="average percentage of qualified paths")
lines(c(0:13),path.bubble3_50.groupbys,col=3)
lines(c(0:13),path.bubble3_70.groupbys,col=4)
lines(c(0:13),path.bubble3_90.groupbys,col=5)
legend(7.5,1.0,c("threshold 0.3","threshold 0.5","threshold 0.7","threshold 0.9"),lty=c(1:1),col=c(2,3,4,5))


 
group.qualified.percentage.fixedS <- function(path.qualified_ ,s){
	temp <- vector()
	for (n in seq(200, 1000, 50)){
		temp[n/50-3] <- path.qualified_[[s+1]][[n]]
	}
	return(temp)
}

path.size3.qualified_50_s0 <- group.qualified.percentage.fixedS(path.bubble3_50,0)
path.size3.qualified_50_s3 <- group.qualified.percentage.fixedS(path.bubble3_50,3)
path.size3.qualified_50_s7 <- group.qualified.percentage.fixedS(path.bubble3_50,7)
path.size3.qualified_50_s10<- group.qualified.percentage.fixedS(path.bubble3_50,10)
path.size3.qualified_50_s13<- group.qualified.percentage.fixedS(path.bubble3_50,13)


#plot 2
plot(seq(200,1000,50),path.size3.qualified_50_s0,type="l",main="Bubble size=3\nQualify threshold = 0.5",xlab="number of trackers",ylab="percentage of qualified paths",ylim=c(0,1.0),col=1)
lines(seq(200,1000,50),path.size3.qualified_50_s3,col=2)
lines(seq(200,1000,50),path.size3.qualified_50_s7,col=3)
lines(seq(200,1000,50),path.size3.qualified_50_s10,col=4)
lines(seq(200,1000,50),path.size3.qualified_50_s13,col=5)
legend(200,1.0,c("separation=0","separation=3","separation=7","separation=10","separation=13"),lty=c(1:1),col=c(1,2,3,4,5))


##plot 3s
group.qualified.percentage.byN <- function(path.qualified_ ,n){
	temp <- vector()
	for (s in 1:14){
		temp[s] <- path.qualified_[[s]][[n]]
	}
	return(temp)
}

path.size3.qualified_50_groupbyN <- list()
for(n in seq(200,1000,50)){
	path.size3.qualified_50_groupbyN[[n/50-3]] <- group.qualified.percentage.byN(path.bubble3_50,n)
}
path.size3.qualified_50_groupbyN



plot(c(0:13),path.size3.qualified_50_groupbyN[[1]],type="l",col=2,main="Qualify threshold=0.5\nBubble size =3",ylim=c(0,1),xlab="separation",ylab="percentage of qualified paths")
lines(c(0:13),path.size3.qualified_50_groupbyN[[8]],col=3)
lines(c(0:13),path.size3.qualified_50_groupbyN[[13]],col=4)
lines(c(0:13),path.size3.qualified_50_groupbyN[[17]],col=5)
legend(7.5,1.0,c("#of trackers 200","#of trackers 550","#of trackers 800","#of trackers 1000"),lty=c(1:1),col=c(2,3,4,5))





path.quality.overlap_s4_13 <- path.quality.overlap


path.quality.overlap <- c()
path.quality.overlap <- rbind(path.quality.overlap_s4_13,path.quality.overlap.s1to3)
path.quality.overlap <- rbind(path.quality.overlap,path.quality.overlap.s0)
path.quality.overlap <- path.quality.overlap[order(path.quality.overlap[,1]),]


#plot 4-6 overlap quality
path.overlap_30 <- get.qualified.percentage(path.quality.overlap,0.3)
path.overlap_50 <- get.qualified.percentage(path.quality.overlap,0.5)
path.overlap_70 <- get.qualified.percentage(path.quality.overlap,0.7)
path.overlap_90 <- get.qualified.percentage(path.quality.overlap,0.9)
path.overlap_30.groupbys <- group.qualified.percentage.byS(path.overlap_30)
path.overlap_50.groupbys <- group.qualified.percentage.byS(path.overlap_50)
path.overlap_70.groupbys <- group.qualified.percentage.byS(path.overlap_70)
path.overlap_90.groupbys <- group.qualified.percentage.byS(path.overlap_90)


#plot 4
plot(c(0:13),path.overlap_30.groupbys,type="l",col=2,main="Overlap(Bubble size =0)",ylim=c(0,0.8),xlab="separation",ylab="average percentage of qualified paths")
lines(c(0:13),path.overlap_50.groupbys,col=3)
lines(c(0:13),path.overlap_70.groupbys,col=4)
lines(c(0:13),path.overlap_90.groupbys,col=5)
legend(8,0.8,c("threshold 0.3","threshold 0.5","threshold 0.7","threshold 0.9"),lty=c(1:1),col=c(2,3,4,5))


#plot 5

path.overlap_50_s0 <- group.qualified.percentage.fixedS(path.overlap_50,0)
path.overlap_50_s3 <- group.qualified.percentage.fixedS(path.overlap_50,3)
path.overlap_50_s7 <- group.qualified.percentage.fixedS(path.overlap_50,7)
path.overlap_50_s10<- group.qualified.percentage.fixedS(path.overlap_50,10)
path.overlap_50_s13<- group.qualified.percentage.fixedS(path.overlap_50,13)

plot(seq(200,1000,50),path.overlap_50_s0,type="l",main="Overlap(Bubble size=0)\nQualify threshold = 0.5",xlab="number of trackers",ylab="percentage of qualified paths",ylim=c(0,0.4),col=1)
lines(seq(200,1000,50),path.overlap_50_s3,col=2)
lines(seq(200,1000,50),path.overlap_50_s7,col=3)
lines(seq(200,1000,50),path.overlap_50_s10,col=4)
lines(seq(200,1000,50),path.overlap_50_s13,col=5)
legend(200,0.4,c("separation=0","separation=3","separation=7","separation=10","separation=13"),lty=c(1:1),col=c(1,2,3,4,5))


#plot 6

path.overlap_50_groupbyN <- list()
for(n in seq(200,1000,50)){
	path.overlap_50_groupbyN[[n/50-3]] <- group.qualified.percentage.byN(path.overlap_50,n)
}
path.overlap_50_groupbyN



plot(c(0:13),path.overlap_50_groupbyN[[1]],type="l",col=2,main="Qualify threshold=0.5\nOverlap(Bubble size =0)",ylim=c(0,0.5),xlab="separation",ylab="percentage of qualified paths")
lines(c(0:13),path.overlap_50_groupbyN[[8]],col=3)
lines(c(0:13),path.overlap_50_groupbyN[[13]],col=4)
lines(c(0:13),path.overlap_50_groupbyN[[17]],col=5)
legend(7,0.5,c("#of trackers 200","#of trackers 550","#of trackers 800","#of trackers 1000"),lty=c(1:1),col=c(2,3,4,5))

