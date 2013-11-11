Turnover <-function(comm ,method="r1" ,sims=1000 ,scores=1, order=TRUE, allow.empty=FALSE){
if(order==TRUE){comm=OrderMatrix(comm,scores=scores)}

turnover=function(web){
   	for(i in 1:dim(web)[1]){
			temp=web[i,]
			if(sum(temp) < 2){break
			}else{
			web[i,min(which(temp==1)):max(which(temp==1))]<-1	
			}	
		}

		for(j in 1:dim(web)[2]){
			temp=web[,j]
			if(sum(temp) < 2){web[,j]=temp
			}else{
			first=min(which(temp==1))
			last=max(which(temp==1))
			web[first:last,j]<-1	
			}		
		}
	  D <- designdist(web, method = "(A-J)*(B-J)", terms = "minimum")
    return(sum(D))
}

	statistic=turnover(comm)
	nulls=nullmaker(comm=comm, sims=sims, method=method,allow.empty=allow.empty)
	simstat=as.numeric(lapply(nulls,turnover))
	varstat=sd(simstat)
	z = (mean(simstat)-statistic)/(varstat)
	pval=2*pnorm(-abs(z))
	return(list(Turnover=statistic, z=z,pval=pval,SimulatedMean=mean(simstat),SimulatedVariance=varstat,Method=method))
	
}
