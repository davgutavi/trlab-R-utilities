getTriclusters<-function(solProperties,datasetValues){
 
   aux <- unlist(strsplit(solProperties$solutions,"@"))
  
  triclusters <- list()
  
  for(sol in aux){
    
    parts <- unlist(strsplit(sol,";"))
    
    gv <- as.numeric(unlist(strsplit(parts[1],",")))
    sv <- as.numeric(unlist(strsplit(parts[2],",")))
    tv <- as.numeric(unlist(strsplit(parts[3],",")))
    
    tri <- data.frame(g = numeric(0), s=numeric(0), t=numeric(0), el=numeric(0))
    
    for (g in gv){
      
      for (s in sv){
        
        for (t in tv){
          
          timeSlice = datasetValues[[t+1]]
          
          el = timeSlice[g+1,s+1]
      
          tri[nrow(tri)+1,]<-c(g,s,t,el)
          
        }
        
      }
      
    }
    
    # str(tri)
    
    triclusters[[length(triclusters)+1]]<-tri
    
  }
  
  return(triclusters)
  
}