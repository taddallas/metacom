BoundaryClump <-function(comm, order=TRUE, scores=1, binary=TRUE){
	if(order==TRUE){comm=OrderMatrix(comm, scores=scores, binary=binary)}

	for(i in 1:dim(comm)[1]){
			temp=comm[i,]
			comm[i,min(which(temp==1)):max(which(temp==1))]<-1	
		}

	M=0
	ComBnd=rep(0, ncol(comm))
	ComBndChi=0

	for(i in 1:nrow(comm)){
		ind1=which(comm[i,]==1)
		for(j in 1:ncol(comm)){
		  if(min(ind1) == j) {ComBnd[j]=ComBnd[j]+1}
			if(max(ind1) == j) {ComBnd[j]=ComBnd[j]+1}
		}
	}
		
	TotComBnd=(nrow(comm)*2) - ComBnd[1] - ComBnd[ncol(comm)]
	ExpComBnd=TotComBnd/(ncol(comm) - 2)
	df= -1
	for(z in 2:(ncol(comm) - 1)){
  	 M = M + ((ComBnd[z]/TotComBnd)*((ComBnd[z]-1)/(TotComBnd-1)));
     ComBndChi = ComBndChi + (((ComBnd[z] - ExpComBnd)**2) / ExpComBnd);
     df = df + 1
  }

	M= M * (ncol(comm) - 2)
	if(M<1){Mpr=pchisq(ComBndChi,df)
	}else{ 
	Mpr=1-pchisq(ComBndChi,df)
	}
  return(data.frame(index=M,P=Mpr,df=df))
}

