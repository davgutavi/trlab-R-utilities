library("GEOquery")
library("tidyverse")
library("rentrez")
library("affy")
library("xml2")

# Function that obtains only ids from the GEO DataSet database
id_obtainer <- function(term='',rtmx=20){
  handle <- entrez_search(db ='gds',     # Data Base = GEO DataSets
                          term = paste("gds[filter] 
                          AND (time[Subset Variable Type] OR age[Subset Variable Type]) 
                          AND (mouse[Organism] 
                          OR human[Organism] OR Saccharomyces cerevisiae[Organism]) ",
                          term),         # Creates the string to search
                          retmax=rtmx,   # Limits the elements of the results
                          use_history = TRUE) 
  return(handle$ids)
}

# Obtain the GDS ID to use for GEO_query
gds_list_obtainer <- function(ids){
  resumen <- entrez_summary(db="gds",id=ids)
  gds_list <- extract_from_esummary(resumen,elements = "accession")
  return(gds_list)
}

# Write Samples Files
write_samples_files <- function(ssinfo,gds,id){
  # We create the table with all the data from our GDS
  tabla <- dataTable(gds)
  times <- tabla@columns$time
  if(is.null(times)){
    times <- tabla@columns$age
  }
  # And an auxiliar table with the information of the ssinfo
  aux_table <- tabla@columns
  aux_table$time <- NULL
  aux_table$age <- NULL
  aux_table$sample <- NULL
  aux_table$description <- NULL
  # We need to obtain the number of columns an experiment has
  aux <- unique(times)
  tab <- tabulate(match(times, aux))
  time_points <- max(tab) # This is the number of columns
  # Now we need to obtain which ssinfo is suitable of creating a sample.txt
  aux_vect <- c()
  for(info in ssinfo){
    aux_vect <- c(aux_vect,length(unique(aux_table[[info]])))
  }
  indices <- which(aux_vect==0)
  aux_vect <- aux_vect[-indices] # List of numbers of diferent values a ssinfo has
  indices <- which((time_points/aux_vect) == as.integer((time_points/aux_vect)))
  
  # TODO si indices está vacío el experimento no se puede hacer en TrLab
  
  for(index in indices){
    samples <- c()
    col_elements <-gsub("[^[:alnum:] ]", "",toupper(unique(aux_table[[index]])),perl = TRUE) 
    for (i in 1:(max(tab)/aux_vect)[index]) {
      samples <- c(samples,paste(col_elements,"REP",i))
    }
    samples <- sort(gsub(" ", "", samples))
    # For every sample we write a file
    samples_file <- file(paste(id,gsub("[^[:alnum:] ]", ".", names(aux_table[index])),"samples.txt",sep = "_"))
    writeLines(paste(samples, collapse="\n"), 
               sep="", 
               samples_file)
    close(samples_file)
  }
}

# Function that gets only the times that allow TrLab to work
times_generator <- function(time){
  times <- unique(time)
  tab <- tabulate(match(time, times))
  times <- times[tab == max(tab)]
  return(times)
}

# Write times file
write_times_file <- function(times,id){
  times_file <- file(paste(id,"_times.txt",sep = ""))
  writeLines(paste(times, collapse="\n"), 
             sep="", 
             times_file)  
  close(times_file)
}

# Write genes file
write_genes_file <- function(gds,id){
  genes <- gds@dataTable@table$IDENTIFIER
  genes_file <- file(paste(id,"_genes.txt",sep = ""))
  writeLines(paste(genes, collapse="\n"), 
             sep="", 
             genes_file)  
  close(genes_file)
}

# Write experiment data  
write_experiment_files <- function(times,gds,id){
  # We create the table of the dataset
  tabla <- dataTable(gds)
  for(elem in times){
    dataset <- tabla@table[tabla@columns$time==elem]
    if(length(dataset)==0){
      dataset <- tabla@table[tabla@columns$age==elem]
    }
    dataset$ID_REF<- NULL
    dataset$IDENTIFIER <- NULL
    names(dataset) <- NULL
    csv_name <- paste(id,gsub("[^[:alnum:] ]", "", elem),sep = "_")
    write.csv(dataset, paste(csv_name,".csv",sep = ""),
              col.names = FALSE,
              row.names = FALSE,
              fileEncoding = "utf-8",
              sep = ",")
  }
}

# Create all TrLab items
getTrlab_items <- function(ids) {
  cwd <- getwd()                # Save the current wd
  setwd("resources")            # Set the wd on the folder
  resources_wd <- getwd()
  summ <- entrez_summary(db = "gds", id = ids)
  ssinfo <- extract_from_esummary(summ, elements = "ssinfo")
  gds_list <- gds_list_obtainer(ids)
  for (i in 1:length(ids)) {
    xml_doc <- read_xml("resources.xml")
    folder <- sprintf("%04d", length(xml_children(xml_doc))+2)
    # We create the folder of the experiment
    if (!dir.exists(folder)) { # Folder doesn't exists -> it's created
      dir.create(folder)
      setwd(folder)
    } else { # Folder exists -> experiment already downloaded
      print("This experiment has already been downloaded")
      next
    }
    # Create the experiment files
    gds <- getGEO(gds_list[i])
    ssinfo_id_samples <- unique(strsplit(ssinfo[i],split = ";")[[1]]) 
    ssinfo_id <- gsub(" ", ".", ssinfo_id_samples)
    if(length(ssinfo_id)==1){
      setwd(resources_wd)
      unlink(folder,recursive = TRUE)
      print("This dataset it's not suitable for TrLab")
      next
    }
    # Knowing that the dataset has a time or an age ssinfo we can get the times
    # just by checking
    
    
    time <- gds@dataTable@columns$time
    if(is.null(time)){
      time <- gds@dataTable@columns$age
    }
    times <- times_generator(time)
    
    if(length(times)==1){
      time <- gds@dataTable@columns$age
      if(is.null(time)){
        setwd(resources_wd)
        unlink(folder,recursive = TRUE)
        print("This dataset it's not suitable for TrLab")
        next
      }
      times <- times_generator(time)
      if(length(times)==1){
        setwd(resources_wd)
        unlink(folder,recursive = TRUE)
        print("This dataset it's not suitable for TrLab")
        next
      }
    }
    
    # We write all the files
    write_samples_files(gds = gds, ssinfo = ssinfo_id_samples, id = tolower(gds_list[i]))
    samples <- length(list.files(path = getwd() ,full.names=FALSE,recursive=FALSE,pattern = "samples.txt"))
    if(samples == 0){
      setwd(resources_wd)
      unlink(folder,recursive = TRUE)
      print("This dataset it's not suitable for TrLab")
      next
    }
    write_experiment_files(times = times, gds = gds, id = tolower(gds_list[i]))
    write_genes_file(gds = gds, id = tolower(gds_list[i]))
    write_times_file(times = times,id = tolower(gds_list[i]))
    # At last we update the XML file that TrLab uses
    xml_update(gds = gds, gds_id = gds_list[i],xml = xml_doc,wd = resources_wd,time = times)
  }
  setwd(cwd)
}

################################################################################
######################### XML PARSER FUNCTIONS #################################
################################################################################

organism_creator <- function(gds){
  sample_organism <- gsub(" ", "_", gds@header[["sample_organism"]])
  switch(sample_organism,
         Homo_sapiens = {organism<-"Human"},
         Mus_musculus = {organism<-"Mouse"},
         Saccharomyces_cerevisiae = {organism<-"Yeast"})
  return(organism)
}

# Update the resources.xml file from TrLab
xml_update <- function(gds,gds_id,xml,wd,time){
  organism <- organism_creator(gds)
  description <- paste("GEO Omnibus ",gds_id," experiment",sep = "")
  geneSize <- length(gds@dataTable@table[["IDENTIFIER"]])
  id <- sprintf("%04d", length(xml_children(xml))+2)
  timeSize <- length(time)
  
  samples <- list.files(path = getwd() ,full.names=FALSE,recursive=FALSE,pattern = "samples.txt")[1]
  sampleSize <- length(readLines(file(samples),warn=FALSE))
  
  genes <- list.files(path = getwd() ,full.names=FALSE,recursive=FALSE,pattern = "genes.txt")
  times <- list.files(path = getwd() ,full.names=FALSE,recursive=FALSE,pattern = "times.txt")
  resources <- list.files(path = getwd() ,full.names=FALSE,recursive=FALSE,pattern = ".csv")
  
  if(geneSize >= 200){
    maxG <- "200"
    minG <- "15"
  } else {
    maxG <- toString(geneSize)
    minG <- "2"
  }
  
  if(timeSize <= 2){
    minT <- toString(timeSize)
  } else {
    minT <- "3"
  }
  
  new_dataset <- xml_new_root("dataset")
  dataset_node <- xml_find_all(new_dataset, "//dataset")
  xml_attrs(dataset_node) <- c(description=description,
                              geneSize=geneSize,
                              id=id,
                              maxC= toString(sampleSize),
                              maxG= maxG,
                              maxT= toString(timeSize),
                              minC= "2",
                              minG= minG,
                              minT= minT,
                              name= toString(tolower(gds_id)),
                              organism= organism,
                              sampleSize= sampleSize,
                              timeSize= timeSize,
                              type='b')
  xml_add_child(new_dataset,"resources")  
  resources_nodes <- xml_find_all(new_dataset, "//resources")                   
  xml_attrs(resources_nodes) <- c(separator=",")
  for(resource in resources){
    xml_add_child(resources_nodes, "resource", resource)
  }
  xml_add_child(new_dataset,"genes", genes)
  xml_add_child(new_dataset,"samples",samples)
  xml_add_child(new_dataset,"times",times)
  xml_add_child(xml,new_dataset)
  
  setwd(wd)
  write_xml(xml, file = "resources.xml", options = "format", encoding = "UTF-8")
  return(FALSE)
}

################################################################################
##################### SHINY APP AUXILIAR FUNCTIONS #############################
################################################################################

create_search_table <- function(summary){
  search_df <- as.data.frame(extract_from_esummary(
    summary,
    elements = c("accession",
                 "title",
                 "pdat",
                 "taxon",
                 "n_samples")), #extract_from_esummary
    row.names = FALSE) # as.data.frame
  search_df <- data.frame(t(search_df))
  return(search_df)
}

# Function to save the directory of the instalation of TrLab within diferent sessions

save_Trlab_dir <- function(TrLab_dir) {
  write(TrLab_dir,file = "Trlab_dir.txt")
}

load_Trlab_dir <- function() {
  files <- list.files()
  if("Trlab_dir.txt" %in% files) {
    TrLab_dir <- readLines("Trlab_dir.txt")
  } else {
    return(NULL)
  }
  return(TrLab_dir)
}
