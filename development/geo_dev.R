library("GEOquery")
library("biomaRt")
library("BiocManager")
library("hgu219.db")
source("utils/bio_tools.R")

gse112660 <- getGEO("GSE112660")
gse112660_data <- exprs(gse112660[[1]])
gse112660_aff_probes <- rownames(gse112660_data)

# hgu219_ids <- as.data.frame(hgu219ENTREZID)
hgu219_alias <- as.data.frame(hgu219ALIAS2PROBE)
gse112660_names <- subset(hgu219_alias, probe_id %in% gse112660_aff_probes)

dfs <- get_annotated_lists(gse112660_aff_probes, gse112660_names)

mul <- process_multiple(dfs$multiple, dfs$annotated)

bio_dataset <- rbind(dfs$annnotated,mul$result)

a<-length(dfs$notannnotated)
b<-length(mul$dismissed)
c<-nrow(gse112660_names)
p<- (a+b)/c
p*100
print(paste0("Dismissed affymetrix: ",p*100," %"))

gene_data <- build_gene_data(annotation_list,gene_expression_values)

l1<-length(annotation_list$Gene)
l2<-length(unique(annotation_list$Gene))
print(paste0("Gene symbols with  more than one affymetrix: ",l1-l1))







colnames(res)<-colnames(gene_expression_values)











