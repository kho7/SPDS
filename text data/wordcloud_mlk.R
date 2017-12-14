
install.packages("XML")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("NLP")
install.packages("tm")

library(XML)
library(wordcloud)
library(NLP)
mlkLocation <-URLencode("http://www.analytictech.com/mb021/mlk.htm")
doc.html<- htmlTreeParse(mlkLocation, useInternal=TRUE)
mlk <- unlist(xpathApply(doc.html, '//p', xmlValue))
head(mlk, 3)

library(tm)
words.vec <- VectorSource(mlk)
words.corpus <- Corpus(words.vec)
words.corpus

words.vec <- VectorSource(mlk)
words.corpus <- Corpus(words.vec)
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

tdm <- TermDocumentMatrix(words.corpus)
tdm
inspect(tdm)

m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)

cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)

set.seed(1234)
wordcloud(cloudFrame$word,cloudFrame$freq)
wordcloud(names(wordCounts),wordCounts, min.freq=1,random.order=FALSE, max.words=200,scale=c(4,.5), rot.per=0.35,colors=brewer.pal(8,"Dark2"))




