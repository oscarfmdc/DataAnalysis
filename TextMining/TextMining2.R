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

# getAnnotationsFromDocument returns annotations for the text document: 
# word, sentence, part-of-speech, and Penn Treebank parse annotations.

# As an alternative, the koRpus package uses TreeTagger for POS tagging.
getAnnotationsFromDocument = function(doc){
  x=as.String(doc)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator, y1)
  parse_annotator <- Parse_Annotator()
  y3 <- annotate(x, parse_annotator, y2)
  return(y3)  
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

# We are going to use the Movie review data version 2.0, created by Bo Pang and Lillian Lee.

# Once unzipped, the data splits the different documents into positive and negative opinions. 
# In this script we are going to use the positive opinions located in ./txt_sentoken/pos.
source.pos = DirSource("./Assigned", encoding = "UTF-8")
corpus = Corpus(source.pos)

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

# We can create AnnotatedPlainTextDocuments that attach the annotations to the document and store the annotated 
# corpus in another variable (since we destroy the corpus metadata).
corpus.tagged = Map(getAnnotatedPlainTextDocument, corpus, annotations)
corpus.tagged[[1]] 

# We can also store all the annotations inline with the text and store the annotated corpus in another variable
# (since we destroy the corpus metadata).
corpus.taggedText = Map(getAnnotatedMergedDocument, corpus, annotations)
corpus.taggedText[[1]]

# There are functions for accessing parts of an AnnotatedPlainTextDocument.
doc = corpus.tagged[[1]] 
doc

# For accessing the text representation of the document.
as.character(doc)

# For accessing its words.
head(words(doc))

# For accessing its sentences.
head(sents(doc),3)

# For accessing its tagged words.
head(tagged_words(doc))

# For accessing its tagged sentences.
head(tagged_sents(doc),3)

# For accessing the parse trees of its sentences.
head(parsed_sents(doc),3)
