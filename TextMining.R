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