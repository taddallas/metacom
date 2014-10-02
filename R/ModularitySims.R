ModularitySims=function(mat, sims=1000, method='fixedfixed', allow.empty=FALSE){
  require(metacom)
  actual=BarberModularity(mat, output='Q')
  nulls=NullMaker((mat+0), sims=sims, method=method, allow.empty=allow.empty)
  simsim=vector()
  for(i in 1:sims){
    simsim[i]=BarberModularity(nulls[[i]], output='Q')
  } 
  z = (mean(simsim) - actual)/ sd(simsim)
  pval = 2 * pnorm(-abs(z))
  return(list(statistic=actual, sims=simsim, z=z, p=pval))  
}
