library(Biobase)
library(GEOquery)
library(limma)



platform = "GPL339"

obj = getGEO(platform, destdir="/Users/davgutavi/Desktop/platform01/")
accessionNumber = "GSE4324"
datasetObject <- getGEO(accessionNumber, destdir="/Users/davgutavi/Desktop/dataset01/")



ids<-rownames(exprs(datasetObject))
pData(datasetObject)[,1:3]

exprs(datasetObject)

show(datasetObject)

head(Meta(datasetObject))


datos = Table(datasetObject)

dataset.name <- 

datos = Table(obj)
genes = datos[2]
write.table(genes, file="/Users/davgutavi/Desktop/dataset01/genes.csv",row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)

gds <- getGEO(dataset.name,destdir=".")
#gds <- getGEO(filename = system.file("GDS2860.soft.gz",package = "GEOquery"))
eset <- GDS2eSet(gds,do.log2=TRUE)
## extract affymetrix IDs
ids<-rownames(exprs(eset))


## Extract phenotypic information
## Use gsub to simplify the names (makes it easier to define factors)
state <- Columns(gds)$disease.state
state <- gsub("Parkinson's disease","parkinson",state)

## Define the factors for the statistical analysis
f <- factor(state)
design <- model.matrix(~0+f)
contrast.matrix<-makeContrasts(fparkinson-fcontrol,levels=design)

## Get the platform name and check that we got data
platform<-as.character(Meta(gds)$platform)
print(paste("Platform",platform,"contains ",length(ids),"spots"))

## Retrieve the platform information.
gpl.name <- paste(platform,".soft",sep="")
if (file.exists(gpl.name)) {
  gpl<-getGEO(filename=gpl.name,destdir=".")
} else {
  gpl<-getGEO(platform,destdir=".")
}

## This is the correspondence between the
## affymetrix IDs and the gene symbol
mapping <- Table(gpl)[,c("ID","Gene.Symbol")]

###
### t-test
fit<-lmFit(eset,design)
fit2<-contrasts.fit(fit,contrast.matrix)
fit2<-eBayes(fit2)

## Adjust for multiple testing
p.values<-fit2$p.value
p.BH <- p.adjust(p.values,method="BH")

# get the indices of all significant p values
ord<-order(p.val)
ord.sign<-subset(ord,p.val[ord]<0.1)

## check results
mapping[ord.sign,]

## Write study set
studyset.name = paste("study",dataset.name,".txt",sep="")
write.table(mapping[ord.sign,2],file=studyset.name,col.names=F,row.names=F,quote=F)

pop.name = paste("population",platform,"txt",sep="")
write.table(mapping[,2],col.names=F,row.names=F,quote=F,file=pop.name)