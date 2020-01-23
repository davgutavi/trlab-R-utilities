source("general/Configuration.R")
source("utils/opt_utils.R")

headersRoot<-"/Users/davgutavi/Desktop/opt_headers"
outputRoot<-"/Users/davgutavi/Desktop/opt_datasets"

datasetName<-"elutriation"
headersPath<-paste0(headersRoot,"/",datasetName,"_headers.txt")

datasetXml<-getDataset(datasetName)
datasesTagPaths<-getGSTtags(datasetXml)
datasetInfo<-getDatasetValues(datasetXml)

geneNames<-read.csv(datasesTagPaths["genes"],header = F,sep = ";")
headers<-read.csv(headersPath,header = F)


opt<-optDataset(datasetInfo,geneNames,headers)

write.table(opt,paste0(outputRoot,"/",datasetName,"_opt.txt"),sep = "\t",quote = F,row.names = F,na ="0" )
























