library(readxl)
library(bibliometrix)
help("bibliometrix")
dbt <- readFiles("Documents/Doctorado all docs/darktourism bibliometria/fulldt.bib")
M <- convert2df(dbt, dbsource = "scopus", format = "bibtex")
analisables <- biblioAnalysis(M, sep = ";")
S <- summary(object = analisables, k = 10, pause = FALSE)

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ". ")
n <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(n, analysis = "collaboration", network = "countries", sep = ";")
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.8)
print(net)
'''otras lineas de codigo a estudiar'''
table(analizables$SO)
library(dplyr)
papers <- filter(analizables, !grepl('CYBEREUROPE|CYBERSIGHTINGS',TI))
titles <- papers$TI
library(quanteda)
my_corpus <- corpus(titles)
docvars(my_corpus, "Year") <- papers$PY
docvars(my_corpus, "Citations") <- papers$TC
docvars(my_corpus, "Journal") <- papers$SO
avert <- summary(my_corpus, n=6678)


CIHB <- corpus_subset(my_corpus, Journal == "COMPUTERS IN HUMAN BEHAVIOR")
summary(CIHB)
CY_BE <- corpus_subset(my_corpus, Journal == "CYBERPSYCHOLOGY AND BEHAVIOR")
summary(CY_BE)
CYBER <- corpus_subset(my_corpus, Journal == "CYBERPSYCHOLOGY, BEHAVIOR, AND SOCIAL NETWORKING")
summary(CYBER)
HCI <- corpus_subset(my_corpus, Journal == "HUMAN-COMPUTER INTERACTION")
summary(HCI)

#kwic(CIHB, "behavior")
#kwic(CY_BE, "behavior")
#kwic(CYBER, "behavior")
#kwic(HCI, "behavior")

clichewords <- c("©", "also", "can", "study", "ltd", "research", "elsevier", "reserved", "rights", "using", "used", "mary", "ann", "liebert", "inc", "results", "online")
tdmCIHB <- dfm(CIHB, remove = c(stopwords("en"), clichewords), remove_punct = TRUE)
topfeatures(tdmCIHB, 20)

tdmCY_BE <- dfm(CY_BE, remove = c(stopwords("en"), clichewords), remove_punct = TRUE)
topfeatures(tdmCY_BE, 20)

tdmCYBER <- dfm(CYBER, remove = c(stopwords("en"), clichewords), remove_punct = TRUE)
topfeatures(tdmCYBER, 20)

tdmHCI <- dfm(HCI, remove = c(stopwords("en"), clichewords), remove_punct = TRUE)
topfeatures(tdmHCI, 20)


tdm <- dfm(my_corpus, tolower = TRUE, remove = c(clichewords, stopwords("en")), remove_punct = TRUE)
topfeatures(tdm, 20)
tdmjournals <- dfm_group(tdm, groups = "Journal", fill = TRUE)
topfeatures(tdmjournals, 20)


library(textmineR)
library(ggplot2)


# Clusters for CIHB
tf_mat1 <- TermDocFreq(tdmCIHB)
tfidf1 <- t(tdmCIHB[ , tf_mat1$term ]) * tf_mat1$idf
tfidf1 <- t(tfidf1)
csim1 <- tfidf1 / sqrt(rowSums(tfidf1 * tfidf1))
csim1 <- csim1 %*% t(csim1)
cdist1 <- as.dist(1 - csim1)
hc1 <- hclust(cdist1, "ward.D")
clustering1 <- cutree(hc1, 10)
library(ggdendro)
Fig1 <- ggdendrogram(hc1, rotate = FALSE, size = 0.2, labels = FALSE) + labs(title="    Computers in Human Behavior")

p_words1 <- colSums(tdmCIHB) / sum(tdmCIHB)
cluster_wordsCIHB <- lapply(unique(clustering1), function(x){
  rows1 <- tdmCIHB[ clustering1 == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows1 <- rows1[ , colSums(rows1) > 0 ]
  
  colSums(rows1) / sum(rows1) - p_words1[ colnames(rows1) ]
})

clustersCIHB <- data.frame(cluster = unique(clustering1),
                           size = as.numeric(table(clustering1)),
                           top_words = sapply(cluster_wordsCIHB, function(d){
                             paste(
                               names(d)[ order(d, decreasing = TRUE) ][ 1:10 ], 
                               collapse = ", ")
                           }),
                           stringsAsFactors = FALSE)
clustersCIHB$Journal <- "CHB"

# Clusters for Cyberpsychology and Behavior
tf_mat2 <- TermDocFreq(tdmCY_BE)
tfidf2 <- t(tdmCY_BE[ , tf_mat2$term ]) * tf_mat2$idf
tfidf2 <- t(tfidf2)
csim2 <- tfidf2 / sqrt(rowSums(tfidf2 * tfidf2))
csim2 <- csim2 %*% t(csim2)
cdist2 <- as.dist(1 - csim2)
hc2 <- hclust(cdist2, "ward.D")
clustering2 <- cutree(hc2, 10)
Fig2 <- ggdendrogram(hc2, rotate = FALSE, size = 0.2, labels = FALSE) + labs(title="    Cyberpsychology and Behavior")
#
p_words2 <- colSums(tdmCY_BE) / sum(tdmCY_BE)
cluster_wordsCY_BE <- lapply(unique(clustering2), function(x){
  rows2 <- tdmCY_BE[ clustering2 == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows2 <- rows2[ , colSums(rows2) > 0 ]
  
  colSums(rows2) / sum(rows2) - p_words2[ colnames(rows2) ]
})

clustersCY_BE <- data.frame(cluster = unique(clustering2),
                            size = as.numeric(table(clustering2)),
                            top_words = sapply(cluster_wordsCY_BE, function(d){
                              paste(names(d)[ order(d, decreasing = TRUE) ][ 1:10 ], 
                                    collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)
clustersCY_BE$Journal <- "C&B"

# Clusters for Cyberpsychology, Behavior and Social Networking
tf_mat3 <- TermDocFreq(tdmCYBER)
tfidf3 <- t(tdmCYBER[ , tf_mat3$term ]) * tf_mat3$idf
tfidf3 <- t(tfidf3)
csim3 <- tfidf3 / sqrt(rowSums(tfidf3 * tfidf3))
csim3 <- csim3 %*% t(csim3)
cdist3 <- as.dist(1 - csim3)
hc3 <- hclust(cdist3, "ward.D")
clustering3 <- cutree(hc3, 10)
Fig3 <- ggdendrogram(hc3, rotate = FALSE, size = 0.01, labels = FALSE) + labs(title="    Cyberpsychology, Behavior, and Social Networking")

p_words3 <- colSums(tdmCYBER) / sum(tdmCYBER)
cluster_wordsCYBER <- lapply(unique(clustering3), function(x){
  rows3 <- tdmCYBER[ clustering3 == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows3 <- rows3[ , colSums(rows3) > 0 ]
  
  colSums(rows3) / sum(rows3) - p_words3[ colnames(rows3) ]
})

clustersCYBER <- data.frame(cluster = unique(clustering3),
                            size = as.numeric(table(clustering3)),
                            top_words = sapply(cluster_wordsCYBER, function(d){
                              paste(names(d)[ order(d, decreasing = TRUE) ][ 1:10 ], 
                                    collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)
clustersCYBER$Journal <- "CBSN"

# Clusters for Human-Computer Interaction
tf_mat4 <- TermDocFreq(tdmHCI)
tfidf4 <- t(tdmHCI[ , tf_mat4$term ]) * tf_mat4$idf
tfidf4 <- t(tfidf4)
csim4 <- tfidf4 / sqrt(rowSums(tfidf4 * tfidf4))
csim4 <- csim4 %*% t(csim4)
cdist4 <- as.dist(1 - csim4)
hc4 <- hclust(cdist4, "ward.D")
clustering4 <- cutree(hc4, 10)
Fig4 <- ggdendrogram(hc4, rotate = FALSE, size = 0.2, labels = FALSE) + labs(title="    Human-Computer Interaction")

p_words4 <- colSums(tdmHCI) / sum(tdmHCI)
cluster_wordsHCI <- lapply(unique(clustering4), function(x){
  rows4 <- tdmHCI[ clustering4 == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows4 <- rows4[ , colSums(rows4) > 0 ]
  
  colSums(rows4) / sum(rows4) - p_words4[ colnames(rows4) ]
})

clustersHCI <- data.frame(cluster = unique(clustering4),
                          size = as.numeric(table(clustering4)),
                          top_words = sapply(cluster_wordsHCI, function(d){
                            paste(names(d)[ order(d, decreasing = TRUE) ][ 1:10 ], 
                                  collapse = ", ")
                          }),
                          stringsAsFactors = FALSE)
clustersHCI$Journal <- "HCI"

ClusterAll <- rbind(clustersCIHB, clustersCY_BE, clustersCYBER, clustersHCI)
library(ggpubr)
FigA <- ggarrange(Fig1, Fig2, Fig3, Fig4, 
                  labels = c("(A)", "(B)", "(C)", "(D)"),
                  ncol = 2, nrow = 2)
FigA
write.csv(ClusterAll, "Clusters.csv")

MostCited <- analizables  %>% filter(TC >= 550)