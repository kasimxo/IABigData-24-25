install.packages("tm", dependencies=TRUE)
library(tm)

source<-URISource("http://www.gutenberg.org/cache/epub/2000/pg2000.txt",
                   encoding="UTF-8")
quijote<-Corpus(source)
quijote
text<-c("En un lugar de la Mancha de cuyo nombre no quiero acordarme...",
        "Miguel de Cervantes Saavedra (1547-1616)")
corpus<-Corpus(VectorSource(text))
corpus<-tm_map(corpus, tolower)
corpus[[2]]$content


docs<-c("primer documento al principio", "segundo documento tras el primer
documento", "tercer documento tras el segundo documento", "cuarto
documento")
corpus<-Corpus(VectorSource(docs))
tdm<-TermDocumentMatrix(corpus)
inspect(tdm)


tdmTfIdf<-TermDocumentMatrix(corpus, control=list(weighting=weightTfIdf))
inspect(tdmTfIdf)

tdmTfIdf<-TermDocumentMatrix(corpus, control=list(wordLengths=c(3,6)))
inspect(tdmTfIdf)


acq<-Corpus(DirSource(directory="C:\\Users\\Admin25\\Downloads\\acq-20241217T190919Z-001\\acq", 
            encoding = "UTF-8",
            pattern = "*.txt"))
crude<-Corpus(DirSource(directory="C:\\Users\\Admin25\\Downloads\\crude-20241217T192132Z-001\\crude", 
                      encoding = "UTF-8",
                      pattern = "*.txt"))

corpus<-Corpus(VectorSource(c(acq$content,crude$content)))
corpus<-tm_map(corpus, tolower)
corpus<-tm_map(corpus, stemDocument)
corpus<-tm_map(corpus, removeNumbers)
corpus<-tm_map(corpus, removePunctuation)
tdm<-TermDocumentMatrix(corpus)
inspect(tdm)

findFreqTerms(tdm,20)

tf<-rowSums(as.matrix(tdm))
tf<-sort(tf, decreasing = TRUE)
length(tf)
top10<-tf[1:10]
top10
barplot(top10, horiz = TRUE)
