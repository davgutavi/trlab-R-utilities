optDataset<-function(datasetInfo,geneNames,headers){
  
  values<-optValues(datasetInfo)
  
  opt<-cbind(geneNames,values)
  
  names(opt)<-as.vector(headers[,1])
  
  return(opt)
  
}

optColumnNames<-function(sampleNames,timeNames){
  
  tn<-c()
  for(el in timeNames){
    tn<-append(tn,gsub(" ","",el))
  }
  
  cnames<-c("ProbeID")
  for(i in c(1:nrow(sampleNames))){
    s<-sampleNames[i,1]
    for(j in c(1:length(tn))){
      t<-tn[j]
      cnames<-append(cnames,paste0(s,"_",t))
    }
  }
  return(cnames)
}

optValues<-function(datasetInfo){
  
  nsamples<-ncol(datasetInfo[[1]])
  
  sampleIndex<-1
  dataset<-sampleFrame(datasetInfo,sampleIndex)
  
  for (sampleIndex in c(2:nsamples)){
    
    dataset<-cbind(dataset,sampleFrame(datasetInfo,sampleIndex))
    
  }
  
  return(dataset)
}

sampleFrame<-function(datasetInfo,sampleIndex){
  
  timeIndex<-1
  
  sframe<-data.frame(datasetInfo[[timeIndex]][,sampleIndex])
  
  for (timeIndex in c(2:length(datasetInfo))){
    
    sframe<-cbind(sframe,data.frame(datasetInfo[[timeIndex]][,sampleIndex]))
    
  }
  
  return(sframe)
  
}