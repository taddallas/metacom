Coherence <-function(comm, method='r1', sims=1000, scores=1, order=TRUE,allow.empty=FALSE){

coherence <-function(web){
	zeros=which(web==0, arr.ind=TRUE)	
  ret=matrix(0, ncol=2)
	uncols=which(colSums(web)>1)
	for(i in 1:length(uncols)){
		temp=zeros[which(zeros[,2]== uncols[i]),]
		tempmin=min(which(web[,uncols[i]]==1))
		tempmax=max(which(web[,uncols[i]]==1))
		if(length(temp)< 3){
			if(temp[1] %in% tempmin:tempmax){ret=rbind(ret,as.vector(temp))}				
		}else{		
		temp=temp[which(temp[,1] %in% tempmin:tempmax),]
		ret=rbind(ret,temp)
	  }
	}

	unrows=which(rowSums(web)>1)
	for(j in 1:length(unrows)){
		temp=zeros[which(zeros[,1]==unrows[j]),]
		tempmin=min(which(web[unrows[j],]==1))
		tempmax=max(which(web[unrows[j],]==1))	
		if(length(temp)<3){
			if(temp[1] %in% tempmin:tempmax){ret=rbind(ret,as.vector(temp))}				
		}else{
		temp=temp[which(temp[,2] %in% tempmin:tempmax),]
  	ret=rbind(ret,temp)
	  }
	}
ret=ret[-1,]
ret=unique(ret)
return(dim(ret)[1])
}



if(order==TRUE){comm=OrderMatrix(comm, scores=scores)
}else{comm=comm}

	statistic=coherence(comm)
	nulls=NullMaker(comm=comm, sims=sims, method=method, allow.empty=allow.empty)

	simstat=as.numeric(lapply(nulls,coherence))
	varstat=sd(simstat)
	z = (mean(simstat)-statistic)/(varstat)
	pval=2*pnorm(-abs(z))
	return(list(EmbAbs=statistic, z=z, pval=pval, SimulatedMean=mean(simstat), SimulatedVariance=varstat, method=method))
}





