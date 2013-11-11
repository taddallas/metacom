nullmaker <-
function(matrix, sims=1000,method='r1'){
	require(vegan)	
	i=1; ret=list()
	while(i < sims+1){
		temp=commsimulator(matrix,method=method)
		if(any(rowSums(temp) < 1) || 
			any(colSums(temp) <  1)){ 
		}else{ret[[i]] = OrderMatrix(temp)
		i=i+1}
	}
return(ret)
}
