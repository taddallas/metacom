BoundaryClump <-function(comm, order=TRUE, scores=1){
	if(order==TRUE){comm=OrderMatrix(comm,scores=scores)}

	for(i in 1:dim(comm)[1]){
			temp=comm[i,]
			first=min(which(temp==1))
			last=max(which(temp==1))
			comm[i,first:last]<-1	
		}
	r=dim(comm)[1]
	c=dim(comm)[2]
	M=0
	ComBnd=rep(0,c)
	ComBndChi=0
	for(i in 1:r){
		ind1=which(comm[i,]==1)
		First=min(ind1)
		Last=max(ind1)  
		
		for(j in 1:c){
		  if(First==j) {ComBnd[j]=ComBnd[j]+1}
			if(Last==j) {ComBnd[j]=ComBnd[j]+1}
		}
	}
		
	TotComBnd=(r*2)-ComBnd[1]-ComBnd[c]
	ExpComBnd=TotComBnd/(c-2)
	df= -1
	for(z in 2:(c-1)){
	 M = M + ((ComBnd[z]/TotComBnd)*((ComBnd[z]-1)/(TotComBnd-1)));
   ComBndChi = ComBndChi + ((ComBnd[z] - ExpComBnd)**2 / ExpComBnd);
   df = df + 1
  }

	M=M*(c-2)
	if(M<1){Mpr=pchisq(ComBndChi,df)
	}else{ 
	Mpr=1-pchisq(ComBndChi,df)
	}
  return(data.frame(index=M,P=Mpr,df=df))
}
