get_annotated_lists <- function(probes,gene_list){
  annotated <- data.frame(Aff=character(0),Gene=character(0))
  not_annotated <- c()
  multiple <- data.frame(Aff=character(0),Gene=character(0))
  for (aff in probes){
    sym_list <- subset(gene_list, probe_id %in% aff)
    # Annotated  
    if(nrow(sym_list)==1){
      annotated[nrow(annotated) + 1,] = c(aff, sym_list[[1,"alias_symbol"]]) 
    }
    # Not annotated  
    else if(nrow(sym_list)==0){
      not_annotated <- c(not_annotated,aff)
    }
    # Multiple annotated  
    else{
      multiple <- rbind(multiple,data.frame(Aff=aff,Gene=sym_list$alias_symbol))
    }
  }    
  return(list(annotated=annotated,notannotated=not_annotated,multiple=multiple))
}

process_multiple <- function(multiple_df, annotated_df){
  result <- data.frame(Aff=character(0),Gene=character(0))
  aff_list <- unique(multiple_df$Aff)
  ann_genes <- unique(annotated_df$Gene)
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
  return(list(result=result,dismissed=dismissed))
  
}


build_gene_data <- function(annotation_list, gene_expression_values, strategy="d"){
  
  gene_list <- unique(annotation_list$Gene)
  res <- data.frame(matrix(NA, nrow = 0, ncol = ncol(gene_expression_values)))
  for (gene in gene_list){
    aff_list <- annotation_list[annotation_list$Gene==gene,]
    if (nrow(aff_list)==1){
      res <-  rbind(res,gene_expression_values[aff_list[1,1],])
      rownames(res)[nrow(res)] <- gene
    }
    else{
      # If a gene symbol has more than one affymetrix
      if (strategy=="a"){
        # Average
        res <-  rbind(res, colMeans(gene_expression_values[aff_list$Aff,]))
        rownames(res)[nrow(res)] <- gene
      }
      # else (strategy=="d") => dismiss
    }
  }
  colnames(res)<-colnames(gene_expression_values)
  return(res)
}


 