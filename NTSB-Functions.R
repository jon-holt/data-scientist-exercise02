#####################################################################################################
# NTSB Aviation Accidents Analysis
#####################################################################################################

#####################################################################################################
# Function:  Process text column of a dataframe and return DTM
#####################################################################################################

make_DTM <- function(data, minFreq = 0.01, maxFreq = 0.8) {
  # create corpus
  corpus <- tm::Corpus(VectorSource(data))
  
  # remove line breaks, words, punctuation, numbers
  line_break <- function(x) gsub("\\\\r\\\\n", " ", x) 
  docs.s <- tm::tm_map(corpus, line_break)
  docs.s <- tm::tm_map(docs.s, content_transformer(tolower))
  docs.s <- tm::tm_map(docs.s, removeWords, stopwords("english"))
  docs.s <- tm::tm_map(docs.s, removePunctuation, preserve_intra_word_dashes = TRUE)
  docs.s <- tm::tm_map(docs.s, removeNumbers)
  docs.s <- tm::tm_map(docs.s, removeWords, c("th"))
  docs.s <- tm::tm_map(docs.s, stripWhitespace)
  
  # remove words that appear in less than 1%, or more than 80% of documents
  ndocs <- length(corpus)
  minDocFreq <- ndocs * 0.01
  maxDocFreq <- ndocs * 0.8
  
  # generate TF-IDF
  dtm <- tm::DocumentTermMatrix(docs.s, 
                                 control = list(bounds = list(global = c(minDocFreq, 
                                                                         maxDocFreq))))
  return(dtm)
}
