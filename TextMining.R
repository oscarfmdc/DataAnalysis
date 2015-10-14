# Check the working directory with wd. If it is not the one where your data are located, change it with setwd.
getwd()
setwd("~/ownCloud/Trabajo/Docencia/2015 Intelligent Systems/R")

library(tm)
library(ggplot2)
library(wordcloud)
library(RWeka)
library(reshape2)

# Needed for a bug when calculating n-grams with weka
options(mc.cores=1)

# Once unzipped, the data splits the different documents into positive and negative opinions.
# In this script we are going to use the positive opinions located in ./txt_sentoken/pos.
source.pos = DirSource("../Corpus/review_polarity/txt_sentoken/pos", encoding = "UTF-8")
corpus = Corpus(source.pos)

# Let’s see how many entries there are in our corpus just by checking its length.
length(corpus)

# Taking a look at the three first entries, we can see that they are not simple documents.
# If we show the first entry, we can see that it contains the document and some metadata.
summary(corpus[1:3])
corpus[1]

# Let’s take a look at the document in the first entry.
corpus[[1]]

# And to its metadata. Note that we can also access metadata items individually.
meta(corpus[[1]])
meta(corpus[[1]])$id

# To create a term document matrix (TDM), we just invoke the TermDocumentMatrix function.
tdm = TermDocumentMatrix(corpus)

# Let’s take a look at the summary of the TDM. The summary informs us about the high sparsity of the TDM
# (i.e., most of the content of the matrix are zeroes).
tdm

# Let’s take a look at a subset of the TDM for four documents and four terms.
# There we can see an example of the sparsity of the matrix.
inspect(tdm[2000:2003,100:103])

# How many terms have been identified in the TDM? We can see it using the length function.
length(dimnames(tdm)$Terms)

# Taking a look at those terms (in our case we use the head and tail functions) we can see that
# these terms are not only words but any sequence of characters separated by spaces.
head(dimnames(tdm)$Terms,10)
tail(dimnames(tdm)$Terms,10)

# How frequently do those terms appear? Let’s sum the content of all terms (i.e., rows) 
# and see the frequency of the terms just shown.
freq=rowSums(as.matrix(tdm))
head(freq,10)
tail(freq,10)

# If we plot those frequencies ordered, we can see how the corpus behaves following Zipf’s law.
plot(sort(freq, decreasing = T),col="blue",main="Word frequencies", xlab="Frequency-based rank", ylab = "Frequency")

# And we can analyse the ten most frequent terms as well as check that 16322 terms out of 36469 only appear once in our corpus.
# Ten most frequent terms
tail(sort(freq),n=10)

# Number of terms only appearing once
sum(freq == 1)

# We can see the different transformations that can be applied to a document by invoking the getTransformations function.
getTransformations()

# Let’s take the first document in the corpus and apply some of these transformations.
# We will apply some transformations or others depending on our use case.

# Let’s just take a look at the first sentence of the document.
doc=corpus[1]
doc[[1]]$content[1]

# First, we remove stop words. We can check the stopwords used using the stopwords function.
stopwords()
doc = tm_map(doc,removeWords,stopwords())
doc[[1]]$content[1]

# Then, we remove punctuation symbols.
doc = tm_map(corpus[1],removePunctuation)
doc[[1]]$content[1]

# Then, we remove numbers.
doc = tm_map(doc,removeNumbers)
doc[[1]]$content[1]

# Then, we remove extra whitespace.
doc = tm_map(doc,stripWhitespace)
doc[[1]]$content[1]

# And, finally, we can stem the document.
doc = tm_map(doc,stemDocument)
doc[[1]]$content[1]

# Let’s create another term document matrix but now after applying transformations to our document.
tdm = TermDocumentMatrix(corpus,
                                    control=list(stopwords = T,
                                                 removePunctuation = T, 
                                                 removeNumbers = T,
                                                 stemming = T))
												 
# Let’s take a look at the summary of the new TDM.
tdm

# And let’s also take a look at a subset of the new TDM.
inspect(tdm[2000:2003,100:103])

# We can see how many terms have been identified in the TDM using the length function again.
length(dimnames(tdm)$Terms)
head(dimnames(tdm)$Terms,10)
tail(dimnames(tdm)$Terms,10)

# How frequently do those terms appear? Let’s sum the content of all terms (i.e., rows) 
# and see the frequency of the terms just shown.
freq=rowSums(as.matrix(tdm))
head(freq,10)
tail(freq,10)

# We can plot those frequencies ordered again.
plot(sort(freq, decreasing = T),col="blue",main="Word frequencies", xlab="Frequency-based rank", ylab = "Frequency")

# And we can analyse the ten most frequent terms as well as check that 9131 terms out of 22445 only appear once in our corpus.
# Ten most frequent terms
tail(sort(freq),n=10)

# Ten most frequent terms
tail(sort(freq),n=10)

# We can see that the two most frequent stems are “film” and “movi” (from movie). Since our corpus deals with movie reviews,
# these two terms (apart from appearing quite frequently) do not contribute by adding valueable information about the document.

# In these cases, we usually define custom stop words by adding new stop words to the predefined list in stopwords.
doc = corpus[1]
doc[[1]]$content[1]
myStopwords = c(stopwords(),"film","films","movie","movies")
doc = tm_map(corpus[1],removeWords,myStopwords)
doc[[1]]$content[1]

# Now let’s create another TDM with the transformations and the custom stop words.
tdm = TermDocumentMatrix(corpus,
                         control=list(stopwords = myStopwords,
                                      removePunctuation = T, 
                                      removeNumbers = T,
                                      stemming = T))
									  
# Let’s take a look at the summary of the new TDM.
tdm

# And let’s also take a look at a subset of the TDM.
inspect(tdm[2000:2003,100:103])

# We can see how many terms have been identified in the TDM using the length function again.
length(dimnames(tdm)$Terms)
head(dimnames(tdm)$Terms,10)
tail(dimnames(tdm)$Terms,10)

# How frequently do those terms appear? Let’s sum the content of all terms (i.e., rows) and see the frequency of the terms just shown.
freq=rowSums(as.matrix(tdm))
head(freq,10)
tail(freq,10)

# We can plot those frequencies ordered.
plot(sort(freq, decreasing = T),col="blue",main="Word frequencies", xlab="Frequency-based rank", ylab = "Frequency")

# And we can analyse the ten most frequent terms as well as check that 9131 terms out of 22445 only appear once in our corpus.
# Ten most frequent terms
tail(sort(freq),n=10)

# Number of terms only appearing once
sum(freq == 1)

# We can also show these terms and their frequencies in a bar plot.
high.freq=tail(sort(freq),n=10)
hfp.df=as.data.frame(sort(high.freq))
hfp.df$names <- rownames(hfp.df) 
ggplot(hfp.df, aes(reorder(names,high.freq), high.freq)) +
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Frequency") +
  ggtitle("Term frequencies")
  
# Let’s create a TDM applying TF-IDF weighting instead of term frequency. 
# This can be done as in previous cases but passing the weighting = weightTfIdf parameter.
tdm.tfidf = TermDocumentMatrix(corpus,
                               control = list(weighting = weightTfIdf,
                                              stopwords = myStopwords, 
                                              removePunctuation = T,
                                              removeNumbers = T,
                                              stemming = T))
											  
# Let’s take a look at the summary of the new TDM.
tdm.tfidf

# And let’s also take a look at a subset of the TDM.
inspect(tdm.tfidf[2000:2003,100:103])

# We can plot the TF-IDF values ordered.
freq=rowSums(as.matrix(tdm.tfidf))

plot(sort(freq, decreasing = T),col="blue",main="Word TF-IDF frequencies", xlab="TF-IDF-based rank", ylab = "TF-IDF")

# And we can analyse the ten terms with the highest TF-IDF.
tail(sort(freq),n=10)

# We can make the analysis of what words are more frequently associated with others.
# Let’s analyse those terms frequently associated with “star”.
asoc.star = as.data.frame(findAssocs(tdm,"star", 0.5))
asoc.star$names <- rownames(asoc.star) 
asoc.star

# We can also put them in a bar graph.
ggplot(asoc.star, aes(reorder(names,star), star)) +   
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Correlation") +
  ggtitle("\"star\" associations")
  
# And now those terms frequently associated with “indiana”.
asoc.indi = as.data.frame(findAssocs(tdm,"indiana", 0.5))
asoc.indi$names <- rownames(asoc.indi) 
asoc.indi

# And the same terms in a bar graph.
ggplot(asoc.indi, aes(reorder(names,indiana), indiana)) +   
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Terms") + ylab("Correlation") +
  ggtitle("\"indiana\" associations")
  
# Now let’s make a word-document frequency graph that shows in a graphical way the frequency of terms in documents.
# The first thing that we need to do, since we have a highly sparse TDM, is to remove sparse terms using the removeSparseTerms function.
tdm.small = removeSparseTerms(tdm,0.5)
dim(tdm.small)
tdm.small

# This way, instead of 22445 terms we have only the 28 terms that are more frequent in the corpus.
# We can clearly see how our new TDM is less sparse.
inspect(tdm.small[1:4,1:4])

# We create a matrix were we count all the appearances of terms in the documents.
matrix.tdm = melt(as.matrix(tdm.small), value.name = "count")
head(matrix.tdm)

# And we plot the word-document frequency graph. The grey color means that the term does not appear in the document.
# Besides, a stronger red color indicates a higher frequency.
ggplot(matrix.tdm, aes(x = Docs, y = Terms, fill = log10(count))) +
  geom_tile(colour = "white") +
  scale_fill_gradient(high="#FF0000" , low="#FFFFFF")+
  ylab("Terms") +
  theme(panel.background = element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
# Let’s choose a nice range of blue colors for the wordcloud. You can invoke the display.brewer.all function to see the whole palette.
pal=brewer.pal(8,"Blues")
pal=pal[-(1:3)]

# Now we extract the frequency of each term and set the random number generator seed to some value
# (this way, we will always get the same word cloud).
freq = sort(rowSums(as.matrix(tdm)), decreasing = T)
set.seed(1234)

# Finally, we invoke the wordcloud function to make the wordcloud with those terms that appear at least 400 times.
word.cloud=wordcloud(words=names(freq), freq=freq,
                     min.freq=400, random.order=F, colors=pal)
					 
# To create a bigram wordcloud, we apply transformations to the original corpus. 
# In this case, we add to the stop words list the “’s” and “’ve” words.

# Then, we use Weka’s n-gram tokenizer to create a TDM that uses as terms the bigrams that appear in the corpus.
corpus.ng = tm_map(corpus,removeWords,c(stopwords(),"s","ve"))
corpus.ng = tm_map(corpus.ng,removePunctuation)
corpus.ng = tm_map(corpus.ng,removeNumbers)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm.bigram = TermDocumentMatrix(corpus.ng,
                                control = list(tokenize = BigramTokenizer))
								
# We extract the frequency of each bigram and analyse the twenty most frequent ones.
freq = sort(rowSums(as.matrix(tdm.bigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

# And we plot the wordcloud.
wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)

# We could also plot the most frequent bigrams in a bar graph.
ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +   
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Bigrams") + ylab("Frequency") +
  ggtitle("Most frequent bigrams")
  
# To create a trigram wordcloud, the approach is the same but this time we tell the n-gram tokenizer to find trigrams.
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm.trigram = TermDocumentMatrix(corpus.ng,
                                control = list(tokenize = TrigramTokenizer))
								
# We extract the frequency of each trigram and analyse the twenty most frequent ones.
freq = sort(rowSums(as.matrix(tdm.trigram)),decreasing = TRUE)
freq.df = data.frame(word=names(freq), freq=freq)
head(freq.df, 20)

# And we plot the wordcloud.
wordcloud(freq.df$word,freq.df$freq,max.words=100,random.order = F, colors=pal)

# We could also plot the most frequent trigrams in a bar graph.
ggplot(head(freq.df,15), aes(reorder(word,freq), freq)) +   
  geom_bar(stat="identity") + coord_flip() + 
  xlab("Trigrams") + ylab("Frequency") +
  ggtitle("Most frequent trigrams")