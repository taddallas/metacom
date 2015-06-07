NullMaker = function (comm, sims = 1000, method = "r1", ordinate = TRUE, scores = 1, allow.empty = FALSE, verbose = FALSE) {
  
  if(verbose == TRUE) pb = txtProgressBar(min = 0, max = sims, style = 3)
  
  #generate null matrices
  nm = nullmodel(comm, method = method)
  sm = simulate(nm, nsim = sims)
  sm.list = lapply(seq(dim(sm)[3]), function(i) sm[, , i]) 
  
  if(verbose == TRUE & allow.empty == TRUE) setTxtProgressBar(pb, length(sm.list))
  
  if(allow.empty == FALSE) {
    
    flag = FALSE
    
    #remove matrices with empty rows or cols
    sm.list = if(allow.empty == FALSE) lapply(sm.list, function(i) if(any(colSums(i) == 0) | any(rowSums(i) == 0)) NULL else i) 
    sm.list[sapply(sm.list,is.null)] = NULL
    
    if(verbose == TRUE)  setTxtProgressBar(pb, length(sm.list))
    
    while(flag == FALSE) {
      
      if(length(sm.list) == sims) flag = TRUE else {
        
        #generate extra matrices
        spares = simulate(nm, nsim = sims/10)
        spares.list = lapply(seq(dim(spares)[3]), function(i) spares[, , i])
        spares.list = lapply(spares.list, function(i) if(any(colSums(i) == 0) | any(rowSums(i) == 0)) NULL else i)
        spares.list[sapply(spares.list,is.null)] = NULL
        
        #replace in original sm object 
        sm.list = append(sm.list, spares.list)
        
        if(verbose == TRUE & length(sm.list) <= sims)  setTxtProgressBar(pb, length(sm.list))
        
        if(length(sm.list) >= sims) {
          sm.list=sm.list[1:sims]
          if(verbose == TRUE)  setTxtProgressBar(pb, length(sm.list))
          flag = TRUE }
      }
    }
  }
  
  #Run ordination
  if(ordinate == TRUE) sm.list = lapply(sm.list, OrderMatrix, scores = scores) 
  
  return(sm.list)
  
}


