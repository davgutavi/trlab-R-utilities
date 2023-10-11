library("GEOquery")
library("biomaRt")
library("BiocManager")
library("hgu219.db")
library("rae230a.db")
source("utils/bio_tools.R")
library("tictoc")

# GSE112660 ----
gse112660 <- getGEO("GSE112660")
gse112660_data <- exprs(gse112660[[1]])
gse112660_aff_probes <- rownames(gse112660_data)
hgu219_alias <- as.data.frame(hgu219ALIAS2PROBE)
gse112660_names <- subset(hgu219_alias, probe_id %in% gse112660_aff_probes)
gse112660_dfs <- get_annotated_lists(gse112660_aff_probes, gse112660_names)
gse112660_mul <- process_multiple(gse112660_dfs$multiple, gse112660_dfs$annotated)
gse112660_bio_dataset <- rbind(gse112660_dfs$annotated,gse112660_mul$result)

print(paste0("Dismissed affymetrix: ",
             (length(gse112660_dfs$notannotated)+length(gse112660_mul$dismissed))/nrow(gse112660_names)*100," %"))

# Dismissed dataset ----

tic("dismmissed")
gse112660_gene_data_dismmissed <- build_gene_data(gse112660_bio_dataset,gse112660_data)
toc("dismmissed")

write.table(gse112660_gene_data_dismmissed, 
            "/Users/davgutavi/Desktop/bio/gse112660/gse112660_gene_data_dismmissed.csv",
            sep=";", 
            row.names = F,
            quote=F)

write.table(rownames(gse112660_gene_data_dismmissed), 
            "/Users/davgutavi/Desktop/bio/gse112660/gse112660d/gse112660d_genes.txt",
            sep=",", 
            col.names = F,
            row.names = F,
            quote = F)

# Average dataset ----

tic("means")
gse112660_gene_data_means <- build_gene_data(gse112660_bio_dataset,gse112660_data,strategy="a")
tic("means")

write.table(gse112660_gene_data_means, 
            "/Users/davgutavi/Desktop/bio/gse112660/gse112660_gene_data_means.csv",
            sep=";", 
            row.names = F,
            quote=F)

write.table(rownames(gse112660_gene_data_means), 
            "/Users/davgutavi/Desktop/bio/gse112660/gse112660a/gse112660a_genes.txt",
            sep=",", 
            col.names = F,
            row.names = F,
            quote = F)

print(paste0("Gene symbols with  more than one affymetrix: ",
             length(gse112660_bio_dataset$Gene)-length(unique(gse112660_bio_dataset$Gene))))












