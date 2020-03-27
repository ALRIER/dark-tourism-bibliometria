library(dplyr)
library(RefManageR)
library(bibliometrix)
library(quanteda)
library(ggplot2)
library(ggpubr)
library(igraph)
library(network)
library(intergraph)
library(miniCRAN)

help("bibliometrix")
help("igraph")
help("network")

D <- readFiles("/home/alrier/Documentos/Dark tourism bibliometria/fulldt.bib")
M <- convert2df(D, dbsource = "scopus", format = "bibtex")
analizables <- M %>% filter(PY >=2015)
analisables <- biblioAnalysis(analizables, sep = ";")
options(width = 50)
S <- summary(object = analisables, k = 20, pause = FALSE)
plot(x = analisables, k = 20, pause = FALSE)

'''primera red matrici de datos cpn su grafica'''
NetMatrix <- biblioNetwork(analizables, analysis = "co-occurrences", network = "author_keywords", sep = ";")
P <- normalizeSimilarity(NetMatrix, type = "association")
perrosnet <- networkPlot(P, n = 20, Title = "co-occurrence network", type = "fruchterman", 
                   labelsize = 1, size = 10, size.cex = T, halo = T, cluster = "walktrap",
                   remove.isolates = F, curved = 0.9, edgesize = 3,remove.multiple = T, noloops = T, weighted = TRUE)

'''segunda red matricial de datos con su grafica'''
NetMatrix2 <- biblioNetwork(analizables, analysis = "co-citation", network = "references", sep = ". ")
n <- metaTagExtraction(analizables, Field = "AU_CO", sep = ";")
NetMatrix3 <- biblioNetwork(n, analysis = "collaboration", network = "countries", sep = ";")
net=networkPlot(NetMatrix3, n = 10, Title = "Country Collaboration", type = "fruchterman", labelsize = 1, size = 10, size.cex = T, halo = T, cluster = "spinglass",
                remove.isolates = T, curved = 0.9, edgesize = 3,remove.multiple = T, noloops = T, weighted = TRUE)


                   
'''Del total de resultados, extraigo los papers más citados'''
AU <- analisables$MostCitedPapers 
AUT <- AU[1:2]
View(AUT)
View(AU)
MCP <- graph(c("LIGHT D, 2017, TOUR MANAGE", "YAN BJ, 2016, TOUR MANAGE",
               "YAN BJ, 2016, TOUR MANAGE", "ASHWORTH GJ, 2015, TOUR RECREAT RES", "COLLINS-KREINER N, 2016, CURR ISSUES TOUR"))
plot(MCP, edge.arrow.size=.5, vertex.color="gold", vertex.size=15, 
     
     vertex.frame.color="gray", vertex.label.color="black", 
     
     vertex.label.cex=0.8, vertex.label.dist=2, edge.curved=0.2) 
'''Lets see most productive countries'''
Paises <- analisables$Countries
Paises <- S$MostProdCountries
View(Paises)
'''Lets keep the first and thirs column of this dataframe'''
Paises <- Paises[c(1, 3)]
'''Lets change the name of the first column'''
names(Paises)[1] <- "Country"
'''Pongamos los nombres en Español'''
Paises$Country <- c("USA", "Taiwan", "Korea",  "Reino Unido", "Alemania", "Holanda", "Italia", "Canada", "España", "China")
Paises$Freq <- suppressWarnings(as.numeric(Paises$Freq))
'''Lets see the production'''
Produccion <- S$AnnualProduction
'''Lets change the name of the first column'''
names(Produccion)[1] <- "Year"
'''Lets set as numeric the records of the second column'''
Produccion$Articles <- as.numeric(Produccion$Articles)

'''graficas y plots'''

Fig1A <- ggplot(Paises, aes(x=reorder(Country, Freq) , y=Freq)) + geom_bar(stat = "identity", fill="blue") + coord_flip() + xlab("Country") + ylab("Frequency")
Fig1B <- ggplot(Produccion, aes(x=Year , y=Articles)) + geom_bar(stat = "identity", fill="blue") + xlab("Year") + ylab("Articles") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggarrange (Fig1A, Fig1B, labels = c("A", "B"), ncol = 2, nrow = 1)
