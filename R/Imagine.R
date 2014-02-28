Imagine <- function(comm, col=c(0,1), ordinate=TRUE, scores=1, fill=TRUE,  xlab='Species', ylab='Site', yline=2, xline=2, sitenames=rownames(comm), speciesnames=colnames(comm), binary=TRUE){
  
	require(metacom)
	if(ordinate == TRUE){
	comm=OrderMatrix(comm, binary= binary, scores=scores)}

	if(fill==TRUE){
	for(i in 1:dim(comm)[2]){
		temp=comm[,i]
		if(sum(temp) < 2){comm[,i]=temp
		}else{
		first=min(which(temp > 0))
		last=max(which(temp > 0))
		comm[first:last,i] <- max(temp)	
		}
		}
	}

# Format for plotting
 reverse <- nrow(comm) : 1
 comm <- comm[reverse,]

# Image plot
par(mar=c(2,6,6,1))

image(1:dim(comm)[2], 1:dim(comm)[1], t(comm), col=col, xlab="", ylab="", axes=FALSE) ; box()


if(length(sitenames)>1){
axis(2, at=1:dim(comm)[1], labels=sitenames, las= 1, cex.axis=1,lwd.ticks=0)
}

if(length(speciesnames)>1){
axis(3, at=1:dim(comm)[2], labels=speciesnames, cex.axis=1, lwd.ticks=0)
}

mtext(xlab, 3, cex=1.5, line=xline)
mtext(ylab, 2, cex=1.5, line=yline)
}








