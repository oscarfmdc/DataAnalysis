# Check the working directory with wd. If it is not the one where your data are located, change it with setwd.
getwd()

# Now we load the required libraries. Only a couple of things to mention:

# Using the annotate function of the openNLP package requires to explicitly include the package name 
# (i.e., NLP::annotate) due to a name clash with ggplot2
# Need to change the memory allocated to Java to avoid out-of-memory problems

# Needed for OutOfMemoryError: Java heap space 
library(rJava)
.jinit(parameters="-Xmx4g")
# If there are more memory problems, invoke gc() after the POS tagging

library(openNLP) 
library(openNLPmodels.en)
library(tm)
library(stringr)

# getAnnotationsFromDocument returns annotations for the text document: word, sentence, part-of-speech, 
# and Penn Treebank parse annotations.

# As an alternative, the koRpus package uses TreeTagger for POS tagging.
getAnnotationsFromDocument = function(doc){
  x=as.String(doc)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator, y1)
#  parse_annotator <- Parse_Annotator()
#  y3 <- annotate(x, parse_annotator, y2)
  return(y2)  
}

# getAnnotatedMergedDocument returns the text document merged with the annotations.
getAnnotatedMergedDocument = function(doc,annotations){
  x=as.String(doc)
  y2w <- subset(annotations, type == "word")
  tags <- sapply(y2w$features, '[[', "POS")
  r1 <- sprintf("%s/%s", x[y2w], tags)
  r2 <- paste(r1, collapse = " ")
  return(r2)  
} 

# getAnnotatedPlainTextDocument returns the text document along with its annotations in an AnnotatedPlainTextDocument.
getAnnotatedPlainTextDocument = function(doc,annotations){
  x=as.String(doc)
  a = AnnotatedPlainTextDocument(x,annotations)
  return(a)  
} 

# detectPatternOnDocument returns the pattern detected on an AnnotatedPlainTextDocument.
detectPatternOnDocument <- function(doc, pattern) {
  x=as.String(doc)
  res=str_match(x,pattern)
  
  if (length(res)==1){
    return (res)
  } else {
    if (all(is.na(res[,2:length(res)])))
      return (NA)
    else {
      ret=list()
      for (i in 2:length(res)){
        ret = paste(ret,res[i])
      }
      return(ret)
    }
  }
}

# detectPatternOnDocumentWithContext returns the pattern detected on an AnnotatedPlainTextDocument with some context.
detectPatternOnDocumentWithContext <- function(doc, pattern) {
  txt=as.String(doc)
  number=50
  coord=str_locate(txt,pattern)
  res3=substr(txt,coord[1]-number,coord[2]+number)
  return (res3)
}

# detectPatternsInCorpus returns a data frame with all the patterns detected in a corpus.
detectPatternsInCorpus = function(corpus, patterns){
  vallEntities <- data.frame(matrix(NA, ncol = length(patterns)+1, nrow = length(corpus)))
  names(vallEntities) <- c("File",patterns)
  for (i in 1:length(patterns)) {
    vallEntities[,i+1]=unlist(lapply(corpus, detectPatternOnDocument, pattern=patterns[i]))
  }
  for (i in 1:length(corpus)) {
    vallEntities$File[i]=meta(corpus[[i]])$id
  }
  return (vallEntities)  
}

# detectPatternsInTaggedCorpus returns a data frame with all the patterns detected in an annotated corpus.
detectPatternsInTaggedCorpus = function(corpus, taggedCorpus, patterns){
  vallEntities <- data.frame(matrix(NA, ncol = length(patterns)+1, nrow = length(corpus)))
  names(vallEntities) <- c("File",patterns)
  for (i in 1:length(patterns)) {
    vallEntities[,i+1]=unlist(lapply(taggedCorpus, detectPatternOnDocument, pattern=patterns[i]))
  }
  for (i in 1:length(corpus)) {
    vallEntities$File[i]=meta(corpus[[i]])$id
  }
  return (vallEntities)  
}

# countMatchesPerColumn returns the number of matches per pattern/column.

# Counts the number of columns with non-NA values for each pattern.
countMatchesPerColumn = function (df) {
  entityCountPerPattern <- data.frame(matrix(NA, ncol = 2, nrow = length(names(df))-1))
  names(entityCountPerPattern) <- c("Entity","Count")
  
  for (i in 2:length(names(df))) {
    entityCountPerPattern$Entity[i-1] = names(df)[i]
    entityCountPerPattern$Count[i-1] = nrow(subset(df, !is.na(df[i])))
  }
  return (entityCountPerPattern)
}

# countMatchesPerRow returns the number of entities per file/row.

# Counts the number of rows with non-NA values for each file.
countMatchesPerRow = function (df) {
  entityCountPerFile <- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  names(entityCountPerFile) <- c("File","Count")
  
  for (i in 1:nrow(df)) {
    entityCountPerFile$File[i] = df$File[i]
    entityCountPerFile$Count[i] = length(Filter(Negate(is.na),df[i,2:length(df[i,])]))
  }
  return (entityCountPerFile[entityCountPerFile[2]!=0,])
}

# printMatchesPerPattern prints the matches found per pattern.
printMatchesPerPattern = function (patterns, matches) {
  for (i in 1:length(patterns)){
    print(paste("PATTERN: ",patterns[i]))
    strings = matches[,i+1][!is.na(unlist(matches[,i+1]))]
    print(strings)
    print(" ") 
  }
}

# mergeAllMatchesInLists returns a data frame with all the files and their matches in a single list per file.

mergeAllMatchesInLists = function (df) {
  matchesPerFile = rep(list(list()), nrow(df))
  
  for (i in 1:nrow(df)) {    
    matches=as.list(unname(unlist(Filter(Negate(is.na),df[i,2:length(df[i,])]))))
    matchesPerFile[[i]]=append(matchesPerFile[[i]],matches)
  }
  
  files = df[,1]
  matches = matchesPerFile
  
  allMatches<- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  names(allMatches) <- c("Files","Matches")
  
  allMatches$Files=files
  allMatches$Matches=matches
  
  return (allMatches)
}	

# mergeGoldStandardInLists returns a data frame with all the files and the gold standard matches in a single list per file.
mergeGoldStandardInLists = function (df) {
  matchesPerFile = rep(list(list()), nrow(df))
  
  for (i in 1:nrow(df)) {    
    matches=as.list(unlist(Filter(Negate(is.na),df[i,2:length(df)])))
    matchesPerFile[[i]]=append(matchesPerFile[[i]],matches)
  }
  
  files = df[,1]
  matches = matchesPerFile
  
  allMatches<- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  names(allMatches) <- c("Files","Matches")
  
  allMatches$Files=files
  allMatches$Matches=matches
  
  return (allMatches)
}

# calculateMetrics calculates precision, recall and f-measure according to a gold standard.
calculateMetrics = function (matches, matches.gs) {
  
  metrics<- data.frame(matrix(NA, ncol = 3, nrow = 1))
  names(metrics) <- c("Precision","Recall","Fmeasure")
  
  numCorrect = 0
  allAnswers = 0
  possibleAnswers = 0
  
  for (i in 1:nrow(matches)) {    
    if (length(matches.gs$Matches[[i]])!=0) {
      l = str_trim(unlist(matches[i,2]))
      l.gs = unname(unlist(matches.gs[i,2]))
      intersection = intersect(l, l.gs)
      numCorrect = numCorrect + length(intersect(l, l.gs))
      allAnswers = allAnswers + length (l)
      possibleAnswers = possibleAnswers + length(l.gs)    
    }
  }
  
  metrics$Precision = numCorrect / allAnswers
  metrics$Recall = numCorrect / possibleAnswers
  
  beta = 1
  metrics$Fmeasure= ((sqrt(beta)+1) * metrics$Precision * metrics$Recall) / 
    ((sqrt(beta)*metrics$Precision) + metrics$Recall)
  
  return(metrics)
}

# We are going to use the Movie review data version 2.0, created by Bo Pang and Lillian Lee.

# Once unzipped, the data splits the different documents into positive and negative opinions. 
# In this script we are going to use the positive opinions located in ./txt_sentoken/pos.

# We are only going to load the first 500 reviews.
source.pos = DirSource("./review_polarity/txt_sentoken/pos", encoding = "UTF-8")
corpus = Corpus(source.pos)
corpus = corpus[0:500]

# Let’s take a look at the document in the first entry.
corpus[[1]]

# We just apply the getAnnotationsFromDocument function to every document in the corpus using lapply.

# This step may take long depending on the size of the corpus and on the annotations that we want to identify.
annotations = lapply(corpus, getAnnotationsFromDocument)

# The first annotations are sentence annotations. They indicate where the sentence starts and where it ends.
# In constituents we can access the tokens in the sentence (and check the number of tokens it has).
# In parse we can access the parse tree.

head(annotations[[1]])

# Word annotations also are defined. They indicate where the word starts, where it ends, and the part-of-speech tag.
tail(annotations[[1]])

# We can create AnnotatedPlainTextDocuments that attach the annotations to the document and store the
# annotated corpus in another variable (since we destroy the corpus metadata).
corpus.tagged = Map(getAnnotatedPlainTextDocument, corpus, annotations)
corpus.tagged[[1]] 

# We can also store all the annotations inline with the text and store the annotated corpus 
# in another variable (since we destroy the corpus metadata).
corpus.taggedText = Map(getAnnotatedMergedDocument, corpus, annotations)
corpus.taggedText[[1]] 

# Based on the first file, we define some simple string patterns to try to identify people appearances.
pattern0=c("created by")
pattern0=c(pattern0,"screenwriter[s]?")
pattern0=c(pattern0,"cinematographer")
pattern0=c(pattern0,"oscar winner")

# We detect those patterns in the corpus and we can see in which files they do appear.
matches0 = detectPatternsInCorpus(corpus, pattern0)
matches0[!is.na(matches0[3]),c(1,3)]

# We check how many patterns we have found in each file.
countMatchesPerRow(matches0) 

# And we check how many times each pattern has been found.
countMatchesPerColumn(matches0) 

# And we print the context in which the patterns are found, to see if we can build better patterns.
for (i in 1:length(pattern0)){
  print(paste("PATTERN: ",pattern0[i]))
  strings = lapply(corpus, detectPatternOnDocumentWithContext, pattern=pattern0[i])
  print(unlist(strings[!is.na(unlist(strings))]))
  print(" ")
}

# Now we define more complex regular expressions that help identifying people appearances.
pattern1=c("created by ([A-z]* [A-z]*)")
pattern1=c(pattern1,"created by [A-z]* [A-z]* \\( and ([A-z]* [A-z]*)")
pattern1=c(pattern1,"screenwriter[s]? ([A-z]* [A-z]*)")
pattern1=c(pattern1,"cinematographer(?: ,)? ([A-z]* [A-z]*)")
pattern1=c(pattern1,"oscar winner ([A-z]* [A-z]*)")

# We detect those patterns in the corpus and we can see in which files they do appear.
matches1 = detectPatternsInCorpus(corpus, pattern1)
matches1[!is.na(matches1[4]),c(1,4)]

# We print the matches found per pattern.
printMatchesPerPattern(pattern1, matches1)

# We check how many patterns we have found in each file.
countMatchesPerRow(matches1) 

# And we check how many times each pattern has been found.
countMatchesPerColumn(matches1) 

# Now we include in our regular expressions part-of-speech information to avoid having incorrect answers.
pattern2=c("created/VBN by/IN ([A-z]*)/NN ([A-z]*)/NN")
pattern2=c(pattern2,"created/VBN by/IN [A-z]*/NN [A-z]*/NN \\(/-LRB- and/CC ([A-z]*)/JJ ([A-z]*)/NN")
pattern2=c(pattern2,"screenwriter[s]?/NN[S]? ([A-z]*)/(?:NN[S]?|JJ) ([A-z]*)/(?:NN|JJ)")
pattern2=c(pattern2,"cinematographer/NN(?: ,/,)? ([A-z]*)/NN ([A-z]*)/NN")
pattern2=c(pattern2,"cinematographer/NN(?: ,/,)? ([A-z]*)/NN ([A-z]*)/IN ([A-z]*)/NN")
pattern2=c(pattern2,"oscar/NN winner/NN ([A-z]*)/VBG ([A-z]*)/NNS")

# We detect those patterns in the POS-tagged corpus.
allEntities = detectPatternsInTaggedCorpus(corpus, corpus.taggedText, pattern2)
allEntities[!is.na(allEntities[4]),c(1,4)]

# We can also view the entities for a certain pattern.
Filter(Negate(is.na),allEntities[[4]])

printMatchesPerPattern(pattern2, allEntities)

# We count all the entities per pattern.

# And we can also draw a histogram of the counts.
entityCountPerPattern = countMatchesPerColumn(allEntities)
entityCountPerPattern

hist(entityCountPerPattern$Count)

# We count all the entities per file.

# And we can also draw a histogram of the counts.
entityCountPerFile=countMatchesPerRow(allEntities)
entityCountPerFile

hist(entityCountPerFile$Count)

# We can write our results to a CSV file, sowe can use them in other places.
write.table(allEntities, file = "allEntities.csv", row.names = F, na="", sep=";")

# Put all matches in a list for comparison with a gold standard.
allMatches = mergeAllMatchesInLists(allEntities)
head(allMatches)

# Load the gold standard and put all gold standard matches in a list for comparison.
goldStandard = read.table(file = "goldStandard.csv", quote = "", na.strings=c(""), colClasses="character", sep=";")
allMatchesGold = mergeGoldStandardInLists(goldStandard)
head(allMatchesGold)

# Calculate the metrics (precision, recall, f-measure).
metrics = calculateMetrics(allMatches, allMatchesGold)
metrics

