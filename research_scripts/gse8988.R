library("GEOquery")
library("biomaRt")
library("BiocManager")
library("hgu219.db")
library("rae230a.db")
source("utils/bio_tools.R")

gse8988 <- getGEO("GSE8988")
gse8988_data <- exprs(gse8988[[1]])
gse8988_aff_probes <- rownames(gse8988_data)

rae230a_alias <- as.data.frame(rae230aALIAS2PROBE)
gse8988_names <- subset(rae230a_alias, probe_id %in% gse8988_aff_probes)

gse8988_dfs <- get_annotated_lists(gse8988_aff_probes, gse8988_names)
gse8988_mul <- process_multiple(gse8988_dfs$multiple, gse8988_dfs$annotated)
gse8988_bio_dataset <- rbind(gse8988_dfs$annotated,gse8988_mul$result)

print(paste0("Dismissed affymetrix: ",
             (length(gse8988_dfs$notannotated)+length(gse8988_mul$dismissed))/nrow(gse8988_names)*100," %"))

gse8988_gene_data_dismmissed <- build_gene_data(gse8988_bio_dataset,gse8988_data)

write.table(gse8988_gene_data_dismmissed, 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988_gene_data_dismmissed.csv",
            sep=";", 
            row.names = F,
            quote=F)

write.table(rownames(gse8988_gene_data_dismmissed), 
            "/Users/davgutavi/Desktop/gse8988/gse8988d/gse8988_genes.txt",
            sep=",", 
            col.names = F,
            row.names = F,
            quote=F)

gse8988_gene_data_means <- build_gene_data(gse8988_bio_dataset,gse8988_data,strategy="a")

write.table(gse8988_gene_data_means, 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988_gene_data_means.csv",
            sep=";", 
            row.names = F,
            quote=F)

write.table(rownames(gse8988_gene_data_means), 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988_genes.txt",
            sep=",", 
            col.names = F,
            row.names = F,
            quote = F)


print(paste0("Gene symbols with more than one affymetrix: ",
             length(gse8988_bio_dataset$Gene)-length(unique(gse8988_bio_dataset$Gene))))


quarter <- c("GSM227353","GSM227359","GSM227395")
write.table(gse8988_gene_data_dismmissed[,quarter], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t0.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,quarter], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t0.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)

one <- c("GSM227366","GSM227404","GSM227405")
write.table(gse8988_gene_data_dismmissed[,one], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t1.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,one], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t1.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)

two <- c("GSM227390","GSM227398","GSM227399")
write.table(gse8988_gene_data_dismmissed[,two], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t2.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,two], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t2.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)


four <- c("GSM227368","GSM227371","GSM227393")
write.table(gse8988_gene_data_dismmissed[,four], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t3.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,four], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t3.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)

six <- c("GSM227383","GSM227384","GSM227394")
write.table(gse8988_gene_data_dismmissed[,six], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t4.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,six], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t4.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)


eight <- c("GSM227372","GSM227385","GSM227406")
write.table(gse8988_gene_data_dismmissed[,eight], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t5.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,eight], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t5.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)

ten <- c("GSM227360","GSM227361","GSM227377")
write.table(gse8988_gene_data_dismmissed[,ten], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t6.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,ten], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t6.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)

eleven <- c("GSM227362","GSM227373","GSM227402")
write.table(gse8988_gene_data_dismmissed[,eleven], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t7.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,eleven], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t7.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)

eleven_75 <- c("GSM227396","GSM227400","GSM227401")
write.table(gse8988_gene_data_dismmissed[,eleven_75], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t8.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,eleven_75], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t8.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)


twelve_25 <- c("GSM227355","GSM227369","GSM227403")
write.table(gse8988_gene_data_dismmissed[,twelve_25], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t9.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,twelve_25], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t9.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)

thirteen <- c("GSM227356","GSM227357","GSM227378")
write.table(gse8988_gene_data_dismmissed[,thirteen], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t10.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,thirteen], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t10.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)


fourteen <- c("GSM227363","GSM227374","GSM227397")
write.table(gse8988_gene_data_dismmissed[,fourteen], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t11.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,fourteen], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t11.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)

sixteen <- c("GSM227379","GSM227386","GSM227387") 
write.table(gse8988_gene_data_dismmissed[,sixteen], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t12.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,sixteen], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t12.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)

eighteen <- c("GSM227364","GSM227365","GSM227375")
write.table(gse8988_gene_data_dismmissed[,eighteen], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t13.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,eighteen], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t13.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)


twenty <- c("GSM227376","GSM227380","GSM227381")
write.table(gse8988_gene_data_dismmissed[,twenty], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t14.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,twenty], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t14.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)



twentytwo <- c("GSM227388","GSM227391","GSM227392")
write.table(gse8988_gene_data_dismmissed[,twentytwo], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t15.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,twentytwo], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t15.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)



twentythree <- c("GSM227358","GSM227367","GSM227370")
write.table(gse8988_gene_data_dismmissed[,twentythree], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t16.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,twentythree], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t16.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)


twentythree_75 <- c("GSM227354","GSM227382","GSM227389")
write.table(gse8988_gene_data_dismmissed[,twentythree_75], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988d/gse8988d_t17.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)
write.table(gse8988_gene_data_means[,twentythree_75], 
            "/Users/davgutavi/Desktop/bio/gse8988/gse8988a/gse8988a_t17.csv",
            sep=";", 
            row.names = F,
            col.names = F,
            quote = F)