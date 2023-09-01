# First bioinformatic tools ----

# Single Graph V2 -------------------------------------------------------------
singleGraphV2 <-
  function(folderPath,
           w = 10,
           h = 4,
           lib = FALSE,
           color = TRUE) {
    if (lib) {
      library(lattice)
    }
    
    #gforctag <- "Genes por cada Condición"
    gforctag <- "Genes for each Condition"
    
    #tforctag <- "Tiempos por cada Condición"
    tforctag <- "Times for each Condition"
    
    #cforttag <- "Condiciones por cada Tiempo"
    cforttag <- "Conditions for each Time"
    
    #eltag <- "niveles de expresión"
    eltag <- "expression levels"
    
    #timetag <- "tiempos"
    timetag <- "times"
    
    gentag <- "genes"
    
    
    
    trellis.device(device = "postscript", color)
    paths <- list.files(folderPath, full.names = TRUE)
    
    for (path in paths) {
      aux <- unlist(strsplit(path, "\\."))
      aux2 <- unlist(strsplit(aux[1], "\\/"))
      name <- aux2[length(aux2)]
      out1 <- paste(aux[1], "_results.eps", sep = "")
      tricluster <- read.csv2(path)
      
      attach(tricluster, warn.conflicts = FALSE)
      
      fs <- factor(s)
      ft <- factor(t)
      fg <- factor(g)
      
      
      nel <-
        as.numeric(levels(tricluster$el)[as.double(tricluster$el)])
      
      
      distance <- 2
      left <- distance
      div <- ceiling(length(levels(fg)) / 2)
      right <- length(levels(fg)) - distance
      at <- c(left, div, right)
      labels <- c(g[left], g[div], g[right])
      
      
      postscript(
        file = out1,
        colormodel = "rgb",
        onefile = FALSE,
        horizontal = FALSE,
        paper = "special",
        width = w,
        height = h
      )
      
      g2 <- xyplot(
        nel ~ ft | fs,
        tricluster,
        main = gforctag,
        xlab = timetag,
        ylab = eltag,
        groups = g,
        type = "a",
        layout = c(1, nlevels(fs))
      )
      
      print(g2, split = c(1, 1, 3, 1), more = TRUE)
      
      g3 <- xyplot(
        nel ~ fg | fs,
        tricluster,
        main = tforctag,
        xlab = gentag,
        ylab = eltag,
        groups = t,
        type = "a",
        scales = list(x = list(at = at, labels = labels)),
        layout = c(1, nlevels(fs))
      )
      
      print(g3, split = c(2, 1, 3, 1), more = TRUE)
      
      g4 <- xyplot(
        nel ~ fg | ft,
        main = cforttag,
        xlab = gentag,
        ylab = eltag,
        groups = s,
        type = "a",
        scales = list(x = list(at = at, labels = labels)),
        layout = c(1, nlevels(ft))
      )
      
      print(g4, split = c(3, 1, 3, 1), more = FALSE)
      
      dev.off()
      
      
      
      show(paste (
        path,
        " --> [",
        length(levels(fg)),
        ",",
        length(levels(fs)),
        ",",
        length(levels(ft)),
        "] printed"
      ))
      
      
      tricluster = NULL
      
      
    }
    
    rm(list = ls())
    
  }

# Single Graph -------------------------------------------------------------
singleGraph <- function(folderPath,
                        w = 10,
                        h = 4,
                        lib = FALSE,
                        color = TRUE) {
  if (lib) {
    library(lattice)
  }
  
  trellis.device(device = "postscript", color)
  paths <- list.files(folderPath, full.names = TRUE)
  
  for (path in paths) {
    aux <- unlist(strsplit(path, "\\."))
    aux2 <- unlist(strsplit(aux[1], "\\/"))
    name <- aux2[length(aux2)]
    out1 <- paste(aux[1], "_results.eps", sep = "")
    tricluster <- read.csv2(path)
    
    attach(tricluster, warn.conflicts = FALSE)
    
    fs <- factor(s)
    ft <- factor(t)
    fg <- factor(g)
    
    
    nel <-
      as.numeric(levels(tricluster$el)[as.double(tricluster$el)])
    
    postscript(
      file = out1,
      colormodel = "rgb",
      onefile = FALSE,
      horizontal = FALSE,
      paper = "special",
      width = w,
      height = h
    )
    
    g2 <- xyplot(
      nel ~ ft | fs,
      tricluster,
      main = "Genes for each Condition",
      xlab = "times",
      ylab = "expression levels",
      groups = g,
      type = "a",
      layout = c(1, nlevels(fs))
    )
    
    print(g2, split = c(1, 1, 3, 1), more = TRUE)
    
    g3 <- xyplot(
      nel ~ g | fs,
      tricluster,
      main = "Times for each Condition",
      xlab = "genes",
      ylab = "expression levels",
      groups = t,
      type = "a",
      layout = c(1, nlevels(fs))
    )
    
    print(g3, split = c(2, 1, 3, 1), more = TRUE)
    
    g4 <- xyplot(
      nel ~ g | ft,
      main = "Conditions for each Time",
      xlab = "genes",
      ylab = "expression levels",
      groups = s,
      type = "a",
      layout = c(1, nlevels(ft))
    )
    
    print(g4, split = c(3, 1, 3, 1), more = FALSE)
    
    dev.off()
    
    
    
    show(paste (
      path,
      " --> [",
      length(levels(fg)),
      ",",
      length(levels(fs)),
      ",",
      length(levels(ft)),
      "] printed"
    ))
    
    
    tricluster = NULL
    
    
  }
  
  rm(list = ls())
  
}

# Experiment Graph --------------------------------------------------------
expGraph <- function(folderPath,
                     w = 8,
                     h = 5,
                     lib = FALSE,
                     color = TRUE) {
  if (lib) {
    library(lattice)
  }
  
  trellis.device(device = "postscript", color)
  
  paths <- list.files(folderPath, full.names = TRUE)
  
  for (path in paths) {
    aux <- unlist(strsplit(path, "\\."))
    aux2 <- unlist(strsplit(aux[1], "\\/"))
    name <- aux2[length(aux2)]
    out1 <- paste(aux[1], "_oGxCpT.eps", sep = "")
    out2 <- paste(aux[1], "_oGxTpC.eps", sep = "")
    out3 <- paste(aux[1], "_oTxGpC.eps", sep = "")
    out4 <- paste(aux[1], "_oCxGpT.eps", sep = "")
    
    tricluster <- read.csv2(path)
    attach(tricluster)
    fs <- factor(s)
    ft <- factor(t)
    fg <- factor(g)
    
    nel <-
      as.numeric(levels(tricluster$el)[as.double(tricluster$el)])
    
    #Vista: O = Genes, X = Tiempos, P = Condiciones oGxTpC
    postscript(
      file = out2,
      colormodel = "rgb",
      onefile = FALSE,
      horizontal = FALSE,
      paper = "special",
      width = w,
      height = h
    )
    g2 <- xyplot(
      nel ~ ft | fs,
      tricluster,
      main = paste(name, " : Genes for each Condition", sep = ""),
      xlab = "times",
      ylab = "expression levels",
      groups = g,
      type = "a",
      layout = c(1, nlevels(fs))
    )
    print(g2)
    dev.off()
    
    #Vista: O = Tiempos, X = Genes, P = Condiciones oTxGpC
    postscript(
      file = out3,
      colormodel = "rgb",
      onefile = FALSE,
      horizontal = FALSE,
      paper = "special",
      width = w,
      height = h
    )
    g3 <- xyplot(
      nel ~ g | fs,
      tricluster,
      main = paste(name, " : Times for each Condition", sep = ""),
      xlab = "genes",
      ylab = "expression levels",
      groups = t,
      type = "a",
      layout = c(1, nlevels(fs))
    )
    print(g3)
    dev.off()
    
    #Vista: O = Condiciones, X = Genes, P = Tiempos oCxGpT
    postscript(
      file = out4,
      colormodel = "rgb",
      onefile = FALSE,
      horizontal = FALSE,
      paper = "special",
      width = w,
      height = h
    )
    g4 <- xyplot(
      nel ~ g | ft,
      main = paste(name, " : Conditions for each Time", sep = ""),
      xlab = "genes",
      ylab = "niveles de expresión",
      groups = s,
      type = "a",
      layout = c(1, nlevels(ft))
    )
    print(g4)
    dev.off()
  }
  
  rm(list = ls())
  
}

# Build Normalized --------------------------------------------------------
buildTriGenNormalalized <-
  function(geoPlat,
           datasetAffPath,
           samples,
           times,
           affFolderPath,
           outPath) {
    auxdataset <- read.csv(datasetAffPath, header = FALSE)
    dataset <- as.character(auxdataset$V1)
    files <- list.files(affFolderPath, full.names = TRUE)
    
    
    general <- c()
    for (clusterAffPath in files) {
      auxcf <- read.csv(clusterAffPath, header = FALSE)
      cf <- as.character(auxcf$V1)
      i <- getClusterIndexes(cf, dataset)
      
      gc <- paste(i, collapse = ";")
      sc <- paste(s, collapse = ";")
      tc <- paste(t, collapse = ";")
      clu <- paste(gc, sc, tc, sep = "\n")
      
      general <- append(general, clu)
      
    }
    
    outStr = paste(general, collapse = "\n;;\n")
    
    write(outStr, file = outPath)
    
  }

# Get Cluster Indexes -----------------------------------------------------
getClusterIndexes <- function(affCluster, affDataset) {
  index <- c()
  for (af in affCluster) {
    i <- match(af, affDataset)
    i <- i - 1
    index <- append(index, i)
    
  }
  
  return(index)
  
}

# Check Affimetrix --------------------------------------------------------
checkAff <- function(affId, geoPlat) {
  row <- geoPlat[geoPlat$ID == affId, ]
  nf <- row$"Gene Symbol"
  name <- as.character(nf)
  return (name)
}

# Build Gene File ---------------------------------------------------------
buildGeneFile <-
  function(inputPath,
           outputPath,
           geoPlat,
           geoAcc,
           lib = FALSE) {
    if (!lib) {
      loadGEOlibraries()
    }
    
    if (missing(geoPlat)) {
      geoPlat <- getGEOplatform(geoAcc)
    }
    
    affGenes <- read.table(file = inputPath)
    r <- getGeneCluster(affGenes, geoPlat)
    write(r, file = outputPath, ncolumns = 1)
    
    print("DO IT!")
  }

# Get Gene Dataset --------------------------------------------------------
getGeneDataset <- function(geoPlatform) {
  gplT <- Table(gpl)
  #df <- gplT[,c("ID","Gene Symbol")]
  l <- c()
  
  for (i in 1:nrow(gplT)) {
    row <- gplT[i, ]
    name <- as.character(row$"Gene Symbol")
    len <- nchar(name)
    if (len == 0) {
      name = as.character(row$ID)
    }
    l <- append(l, name)
  }
  
  return (l)
}

# Get Gene Cluster --------------------------------------------------------
getGeneCluster <-
  function(clusterAffGenes,
           geoPlatform,
           whitoutRepeats = FALSE) {
    l <- c()
    for (v in clusterAffGenes$V1) {
      row <- geoPlatform[geoPlatform$ID == v, ]
      nf <- row$"Gene Symbol"
      name <- as.character(nf)
      
      if (whitoutRepeats) {
        l <- insertWhitOutDup(name, v, l)
      }
      else{
        l <- insertWhitDup(name, v, l)
      }
    }
    return (l)
  }


insertWhitDup <- function(name, v, l) {
  if (name == "") {
    l <- append(l, v)
  }
  else{
    l <- append(l, name)
  }
  return (l)
}

insertWhitOutDup <- function(name, v, l) {
  if (name == "") {
    if (!(v %in% l)) {
      l <- append(l, v)
    }
  }
  else{
    if (!(name %in% l)) {
      l <- append(l, name)
    }
  }
  return (l)
}

# Get GEO platform --------------------------------------------------------
getGEOplatform <- function(GEOaccession,
                           matrix = T,
                           lib = FALSE) {
  if (lib) {
    loadGEOlibraries()
  }
  obj <- getGEO(GEOaccession, GSEMatrix = matrix)
  return (obj)
}

# Get GEO table --------------------------------------------------------
getGEOtable <- function(GEOaccession, lib = FALSE) {
  if (lib) {
    loadGEOlibraries()
  }
  obj <- getGEO(GEOaccession)
  data <- Table(obj)
  return (data)
}

# Load GEO libraries ------------------------------------------------------
loadGEOlibraries <- function() {
  library(Biobase)
  library(GEOquery)
  library(limma)
}

# Get Gene Expression from GSM list ---------------------------------------
getExpressionLevels <- function(lgsm) {
  values <- list()
  names <- c()
  for (i in c(1:length(lgsm))) {
    obj <- lgsm[[i]]
    acc <- Accession(obj)
    names <- c(names, acc)
    table <- Table(obj)
    c <- as.numeric(table[, 2])
    values <- append(values, list(c))
  }
  df <- data.frame(values)
  names(df) <- names
  return (df)
}

# Get GSE resources from Accession -------------------------------------------------------
getGSEresAcc <- function(gseAccession, lib = F) {
  if (lib) {
    loadGEOlibraries()
  }
  gse <- getGEO(gseAccession, GSEMatrix = F)
  lgpl <- GPLList(gse)
  gpl <- lgpl[[1]]
  lgsm <- GSMList(gse)
  meta <- Meta(gse)
  r <- list(gpl, lgsm, meta)
  return (r)
  
}

# Get GSE resources from Platform -------------------------------------------------------
getGSEresPlat <- function(gsePlatform) {
  lgpl <- GPLList(gsePlatform)
  gpl <- lgpl[[1]]
  lgsm <- GSMList(gsePlatform)
  meta <- Meta(gsePlatform)
  r <- list(gpl, lgsm, meta)
  return (r)
  
}