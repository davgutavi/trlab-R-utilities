library("GEOquery")
library("biomaRt")
library("BiocManager")
library("hgu219.db")


BiocManager("hgu219.db")


gse112660 <- getGEO("GSE112660")
gse112660_data <- exprs(gse112660[[1]])
probes <- rownames(gse112660_data)


gse112660_data["11715100_at",1]

ids <- as.data.frame(hgu219ENTREZID)
alias <- as.data.frame(hgu219ALIAS2PROBE)

names <- subset(alias, probe_id %in% probes)

nrow(names)
length(probes)





if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("biomaRt")
BiocManager::install("pd.hg.u219")
BiocManager::install("hgu219.db")

ensembl <- useEnsembl(biomart = "genes")
listEnsembl()

searchDatasets(mart = ensembl, pattern = "hsapiens")
ensembl <- useEnsembl(biomart = "genes", dataset = "hsapiens_gene_ensembl")

searchAttributes(mart = ensembl, pattern = "u219")

# affyids=c("202763_at","209310_s_at","207500_at","11715100_at","11715101_s_at")
affyids=c("11715112_at","11715101_s_at")

getBM(attributes = c('affy_hg_u133_plus_2', 'hgnc_symbol', 'chromosome_name',
                     'start_position', 'end_position', 'band'),
      filters = 'affy_hg_u133_plus_2', 
      values = affyids, 
      mart = ensembl)


featureData(gse112660[[1]])@dimLabels

dimLabels(featureData(gse112660[[1]]))


gse8988 <- getGEO("GSE8988")

phenoData(gse112660[[1]])
sampleNames(gse112660[[1]])

gse112660[[1]]@featureData
gse112660[[1]]@assayData

experimentData(gse112660[[1]])

data <- exprs(gse112660[[1]])
dimnames(gse112660[[1]])

head(Meta(gse112660))
show(gse112660)


head(Meta(gse8988))
show(gse8988)
gse112660[[1]]$GSM3075594

