IdentifyStructure = function(metacom.obj) {
  #Coherence
  if(as.numeric(t(metacom.obj$Coherence)[,3]) >= 0.05) "Random" else
   
    if(as.numeric(t(metacom.obj$Coherence)[,1]) < as.numeric(t(metacom.obj$Coherence)[,4]) & 
         as.numeric(t(metacom.obj$Coherence)[,3]) < 0.05) "Checkerboard (negative coherence)" else
           
           if(as.numeric(t(metacom.obj$Coherence)[,1]) >= as.numeric(t(metacom.obj$Coherence)[,4]) & 
                as.numeric(t(metacom.obj$Coherence)[,3]) < 0.05) {

             #Significant positive turnover
             if(as.numeric(t(metacom.obj$Turnover)[,1]) >= as.numeric(t(metacom.obj$Turnover)[,4]) &
                  as.numeric(t(metacom.obj$Turnover)[,3]) < 0.05 &
                  metacom.obj$Boundary[,1] >=0 & metacom.obj$Boundary[,2] < 0.05) "Clementsian" else
                    
                    if(as.numeric(t(metacom.obj$Turnover)[,1]) >= as.numeric(t(metacom.obj$Turnover)[,4]) &
                         as.numeric(t(metacom.obj$Turnover)[,3]) < 0.05 & metacom.obj$Boundary[,2] >= 0.05) "Gleasonian" else
                           
                           if(as.numeric(t(metacom.obj$Turnover)[,1]) >= as.numeric(t(metacom.obj$Turnover)[,4]) &
                                as.numeric(t(metacom.obj$Turnover)[,3]) < 0.05 &
                                metacom.obj$Boundary[,1] < 0 & metacom.obj$Boundary[,2] < 0.05) "Evenly spaced" else
                                  
                                  #Significant negative turnover
                                  if(as.numeric(t(metacom.obj$Turnover)[,1]) < as.numeric(t(metacom.obj$Turnover)[,4]) &
                                       as.numeric(t(metacom.obj$Turnover)[,3]) < 0.05 &
                                       metacom.obj$Boundary[,1] >=0 & metacom.obj$Boundary[,2] < 0.05) "Nested (clumped)" else
                                         
                                         if(as.numeric(t(metacom.obj$Turnover)[,1]) < as.numeric(t(metacom.obj$Turnover)[,4]) &
                                              as.numeric(t(metacom.obj$Turnover)[,3]) < 0.05 & metacom.obj$Boundary[,2] >= 0.05) "Nested (random)" else
                                                
                                                if(as.numeric(t(metacom.obj$Turnover)[,1]) < as.numeric(t(metacom.obj$Turnover)[,4]) &
                                                     as.numeric(t(metacom.obj$Turnover)[,3]) < 0.05 &
                                                     metacom.obj$Boundary[,1] < 0 & metacom.obj$Boundary[,2] < 0.05) "Nested (hyperdispersed" else
                                                       
                                                       #Non-significant positive turnover
                                                       if(as.numeric(t(metacom.obj$Turnover)[,1]) >= as.numeric(t(metacom.obj$Turnover)[,4]) &
                                                            as.numeric(t(metacom.obj$Turnover)[,3]) >= 0.05 &
                                                            metacom.obj$Boundary[,1] >=0 & metacom.obj$Boundary[,2] < 0.05) "Quasi-clementsian" else
                                                              
                                                              if(as.numeric(t(metacom.obj$Turnover)[,1]) >= as.numeric(t(metacom.obj$Turnover)[,4]) &
                                                                   as.numeric(t(metacom.obj$Turnover)[,3]) >= 0.05 & metacom.obj$Boundary[,2] >= 0.05) "Quasi-gleasonian" else
                                                                     
                                                                     if(as.numeric(t(metacom.obj$Turnover)[,1]) >= as.numeric(t(metacom.obj$Turnover)[,4]) &
                                                                          as.numeric(t(metacom.obj$Turnover)[,3]) >= 0.05 &
                                                                          metacom.obj$Boundary[,1] < 0 & metacom.obj$Boundary[,2] < 0.05) "Quasi-evenly spaced" else
                                                                            
                                                                            #Non-significant negative turnover
                                                                            if(as.numeric(t(metacom.obj$Turnover)[,1]) < as.numeric(t(metacom.obj$Turnover)[,4]) &
                                                                                 as.numeric(t(metacom.obj$Turnover)[,3]) >= 0.05 &
                                                                                 metacom.obj$Boundary[,1] >=0 & metacom.obj$Boundary[,2] < 0.05) "Quasi-nested (clumped)" else
                                                                                   
                                                                                   if(as.numeric(t(metacom.obj$Turnover)[,1]) < as.numeric(t(metacom.obj$Turnover)[,4]) &
                                                                                        as.numeric(t(metacom.obj$Turnover)[,3]) >= 0.05 & metacom.obj$Boundary[,2] >= 0.05) "Quasi-nested (random)" else
                                                                                          
                                                                                          if(as.numeric(t(metacom.obj$Turnover)[,1]) < as.numeric(t(metacom.obj$Turnover)[,4]) &
                                                                                               as.numeric(t(metacom.obj$Turnover)[,3]) >= 0.05 &
                                                                                               metacom.obj$Boundary[,1] < 0 & metacom.obj$Boundary[,2] < 0.05) "Quasi-nested (hyperdispersed)" }

}