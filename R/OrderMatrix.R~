OrderMatrix <-function(comm, scores=1, output.scores=FALSE, binary=TRUE){
	if(binary==TRUE){comm=(comm>0)+0}

	#reciprocal averaging
	temp=decorana(comm,ira=1)

	#ordering of matrix
	if(output.scores==FALSE){
	ret=comm[order(temp$rproj[,scores], decreasing=FALSE), order(temp$cproj[,scores],decreasing=FALSE)]
	ret=as.matrix(ret)	
  ret=apply(ret,2,rev)
	ret=0+(ret>0)
	return(ret)}

	if(output.scores==TRUE){
		return(list(speciesscores=temp$cproj[,scores], sitescores=temp$rproj[,scores]))}
}



