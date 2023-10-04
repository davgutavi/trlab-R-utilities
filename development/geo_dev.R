library("GEOquery")
library("biomaRt")
library("BiocManager")
library("hgu219.db")

gse112660 <- getGEO("GSE112660")
gse112660_data <- exprs(gse112660[[1]])
gse112660_aff_probes <- rownames(gse112660_data)

# hgu219_ids <- as.data.frame(hgu219ENTREZID)
hgu219_alias <- as.data.frame(hgu219ALIAS2PROBE)
gse112660_names <- subset(hgu219_alias, probe_id %in% gse112660_aff_probes)


probes <- gse112660_aff_probes
gene_list <- gse112660_names

annnotated <- data.frame(Aff=character(0),Gene=character(0))
not_annnotated <- c()
multiple <- data.frame(Aff=character(0),Gene=character(0))
for (aff in probes){
  sym_list <- subset(gene_list, probe_id %in% aff)
  # Annotated  
  if(nrow(sym_list)==1){
    annnotated[nrow(annnotated) + 1,] = c(aff, sym_list[[1,"alias_symbol"]]) 
  }
  # Not annotated  
  else if(nrow(sym_list)==0){
    not_annnotated <- c(not_annnotated,aff)
    
  }
  # Multiple annotated  
  else{
    multiple <- rbind(multiple,data.frame(Aff=aff,Gene=sym_list$alias_symbol))
  }
}

multiple_df <- multiple
annotated_df <- annnotated
result <- data.frame(Aff=character(0),Gene=character(0))
aff_list <- unique(multiple_df$Aff)
ann_genes <- unique(annnotated$Gene)
dismissed <- c()
for (aff in aff_list){
  aff_gene_candidates <- multiple_df[multiple_df$Aff==aff,]$Gene
  available_genes <- aff_gene_candidates[!aff_gene_candidates 
                                         %in% intersect(aff_gene_candidates,ann_genes)]
  if (length(available_genes)!=0){
    selected_gene <- sample(available_genes,1)
    ann_genes <- c(ann_genes, selected_gene)
    result[nrow(result) + 1,] = c(aff, selected_gene) 
  }
  else{
    dismissed <- c(dismissed,aff)
  }
}

bio_dataset <- rbind(annnotated,result)

a<-length(not_annnotated)
b<-length(dismissed)
c<-nrow(gse112660_names)
p<- (a+b)/c
p*100



p
aff <- "11715100_at"
aff_gene_candidates <- multiple_df[multiple_df$Aff==aff,]$Gene
available_genes <- aff_gene_candidates[!aff_gene_candidates 
                                       %in% intersect(aff_gene_candidates,ann_genes)]
selected_gene <- sample(available_genes,1)
ann_genes <- c(ann_genes, selected_gene)
result[nrow(result) + 1,] = c(aff, selected_gene) 

aff <- "11715101_s_at"
aff_gene_candidates <- multiple_df[multiple_df$Aff==aff,]$Gene
available_genes <- aff_gene_candidates[!aff_gene_candidates 
                                       %in% intersect(aff_gene_candidates,ann_genes)]
selected_gene <- sample(available_genes,1)
ann_genes <- c(ann_genes, selected_gene)
result[nrow(result) + 1,] = c(aff, selected_gene) 




gse112660_names[[1,"alias_symbol"]]




annnotated


multiple[multiple$Aff=="11726638_at",]

library(tidyverse)
annnotated[annnotated$Gene=="C15orf40",]

x <- c(1, 1, 4, 5, 4, 6)
duplicated(x)
x[duplicated(x)]

annnotated$Gene[duplicated(annnotated$Gene)]

df[df$Gene=="H3/h", ]

subset(hgu219_alias, probe_id %in% c("11715100_at", "11715102_x_at"))



length(gse112660_names[gse112660_names$probe_id=="11715105_at",]$alias_symbol)



length(gse112660_names$probe_id)
length(unique(gse112660_names$probe_id))






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

