library("GEOquery")
library("biomaRt")
library("BiocManager")
library("hgu219.db")
library("rae230a.db")
source("utils/bio_tools.R")

# GSE112660 ----
gse112660 <- getGEO("GSE112660")
gse112660_data <- exprs(gse112660[[1]])
gse112660_aff_probes <- rownames(gse112660_data)
hgu219_alias <- as.data.frame(hgu219ALIAS2PROBE)
gse112660_names <- subset(hgu219_alias, probe_id %in% gse112660_aff_probes)

dfs <- get_annotated_lists(gse112660_aff_probes, gse112660_names)
mul <- process_multiple(dfs$multiple, dfs$annotated)

bio_dataset <- rbind(dfs$annotated,mul$result)

print(paste0("Dismissed affymetrix: ",
             (length(dfs$notannnotated)+length(mul$dismissed))/nrow(gse112660_names)*100," %"))

gene_data <- build_gene_data(bio_dataset,gse112660_data)

print(paste0("Gene symbols with  more than one affymetrix: ",
             length(bio_dataset$Gene)-length(unique(bio_dataset$Gene))))

# GSE8988 ----
gseGSE8988 <- getGEO("GSE8988")
gseGSE8988_data <- exprs(gseGSE8988[[1]])
gseGSE8988_aff_probes <- rownames(gseGSE8988_data)

rae230a_alias <- as.data.frame(rae230aALIAS2PROBE)
gseGSE8988_names <- subset(rae230a_alias, probe_id %in% gseGSE8988_aff_probes)

dfs <- get_annotated_lists(gseGSE8988_aff_probes, gseGSE8988_names)
mul <- process_multiple(dfs$multiple, dfs$annotated)
bio_dataset <- rbind(dfs$annotated,mul$result)

print(paste0("Dismissed affymetrix: ",
             (length(dfs$notannnotated)+length(mul$dismissed))/nrow(gseGSE8988_names)*100," %"))

gene_data_dismmissed <- build_gene_data(bio_dataset,gseGSE8988_data)

gene_data_means <- build_gene_data(bio_dataset,gseGSE8988_data,strategy="a")

print(paste0("Gene symbols with  more than one affymetrix: ",
             length(bio_dataset$Gene)-length(unique(bio_dataset$Gene))))



quarter <- c("GSM227353","GSM227359","GSM227395")
one <- c("GSM227366","GSM227404","GSM227405")
two <- c("GSM227390","GSM227398","GSM227399")
four <- c("GSM227368","GSM227371","GSM227393")
six <- c("GSM227383","GSM227384","GSM227394")
eight <- c("GSM227372","GSM227385","GSM227406")
ten <- c("GSM227360","GSM227361","GSM227377")
eleven <- c("GSM227362","GSM227373","GSM227402")
eleven_75 <- c("GSM227396","GSM227400","GSM227")

twelve_25 <- c("GSM227355","GSM227369","GSM227403")
thirteen <- c("GSM227356","GSM227357","GSM227378")
fourteen <- c("GSM227363","GSM227374","GSM227397")
sixteen <- c("GSM227379","GSM227386","GSM227387") 

eighteen <- c("GSM227364","GSM227365","GSM227375")
twenty <- c("GSM227376","GSM227380","GSM227381")
twentytwo <- c("GSM227388","GSM227391","GSM227392")
twentythree <- c("GSM227358","GSM227367","GSM227370")
twentythree_75 <- c("GSM227354","GSM227382","GSM227389")






