con <- file("/Users/davgutavi/Desktop/Untitled.bib") # Read my bib file 
x <- readLines(con) 
close(con)

#whatRemove <- grep("abstract |doi |file |url |issn |mendeley-groups |keywords |pmid ", x) 

whatRemove <- grep("abstract |doi |file |url |issn |mendeley-groups |keywords |pmid |annote", x) 

y <- x[-whatRemove] 
write(y, file="/Users/davgutavi/Desktop/ref.bib")