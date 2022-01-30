#install.packages("quanteda")
#install.packages("tm")

#Library
#library("quanteda")
library("tm")
library(openxlsx)
library(stringr)

args <- commandArgs(trailingOnly=TRUE)

if (length(args) < 2) {
  print("You have to choose to CREATE or to FIND (if you already created)")
} else if ((length(args) == 2)) {
  if ((args[1] == "CREATE")) {
  pathDocuments <- args[2]
  keyOccurrence <- 10
  setwd(pathDocuments)
  
  #same words in corpus, ocurrency
  files <- list.files(pattern = "pdf$")
  keyPresentNbDocument <- (length(files)/2)
  corp <- Corpus(URISource(files),
                 readerControl = list(reader = readPDF))
  opinions.tdm <- TermDocumentMatrix(corp, 
                                     control = 
                                       list(removePunctuation = TRUE,
                                            stopwords = TRUE,
                                            tolower = TRUE,
                                            stemming = TRUE,
                                            removeNumbers = TRUE,
                                            bounds = list(global = c(keyPresentNbDocument, Inf)))) 
  #inspect(opinions.tdm)
  
  #keywords that with occurrence >= keyOccurrence
  term <- findFreqTerms(opinions.tdm, lowfreq = keyOccurrence, highfreq = Inf)
  
  #saved in excel
  length(opinions.tdm)
  if (length(opinions.tdm)) {
    col <- data.frame(as.matrix(opinions.tdm[term,]))
    df <- data.frame(TermsDocs = term, col)
    write.xlsx(df, 'keywords.xlsx', overwrite = TRUE)
  }
  }
}else if ((length(args) == 3) & (args[1] == "FIND")) {
  key = args[2]
  key = tolower(key)
  key = stemDocument(key)
  key = removeNumbers(key)
  pathDocuments <- args[3]
  setwd(pathDocuments)
  
  
  file <- list.files(pattern = "keywords.xlsx$")
  res <- read.xlsx(file, 1)
  n = 0
  for (elm in res[[1]]) {
    n = n + 1
    if (match(key, elm, nomatch = 0, incomparables = 0)) {
      col <- data.frame(as.matrix(res))
      col <- col[n,]
      indic <- col>20
      j = 0
      for (i in col) {
        i = str_replace_all(i, pattern=" ", replacement="")
        j = j + 1
        if (i > 0 & j > 1) {
          print(colnames(col[j]))
        }
      }
      n = 0
    }
  }
  if(n == length(res[[1]])) {
    print("No linked Documents")
    
  }
  
}

