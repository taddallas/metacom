metacommunity <-function(matrix, scores=1, method='r1', sims=1000, order=TRUE, allow.empty=FALSE){
	require(lattice)
	require(vegan)

	if(order==TRUE){mat=OrderMatrix(matrix,scores=scores)
	}else{mat=matrix}

comat=Coherence(mat, method=method, sims=sims, scores=scores, allow.empty=allow.empty)
	
boundmat=BoundaryClump(mat)
  ranmat=Turnover(mat,method=method, sims=sims, scores=scores ,allow.empty=allow.empty)
	
ret=list(Matrix=mat, Coherence=comat, Turnover=ranmat, Boundary=boundmat)
	return(ret)
}
