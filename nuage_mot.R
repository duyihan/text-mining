# text mining----nuage de mot
# DU Yihan

setwd("~/EISTI/txt mining")


# Installer
#install.packages("tm")  # pour le text mining
#install.packages("SnowballC") # pour le text stemming
#install.packages("wordcloud") # g?n?rateur de word-cloud 
#install.packages("RColorBrewer") # Palettes de couleurs
# Charger
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


# Lire le fichier texte


#harry potter
text1 <- readLines("~/EISTI/txt mining/harry/1_Harry Potter and the Sorcerer's Stone.txt")

text2 <- readLines("~/EISTI/txt mining/harry/2_HARRY POTTER AND THE CHAMBER OF SECRETS.txt")

text3 <- readLines("~/EISTI/txt mining/harry/3_Harry Potter and the Prisoner of Azkaban.txt")

text4 <- readLines("~/EISTI/txt mining/harry/4_Harry Potter and the Goblet of Fire.txt")

text5 <- readLines("~/EISTI/txt mining/harry/5_Harry Potter and the Order of the Phoenix.txt")

text6 <- readLines("~/EISTI/txt mining/harry/6_Harry Potter and The Half-Blood Prince.txt")

text7 <- readLines("~/EISTI/txt mining/harry/7_Harry Potter and the Deathly Hallows.txt")


nuage_mot <- function(text){
  # Charger les donn?es comme un corpus
  docs <- Corpus(VectorSource(text))
  
  
  #inspect(docs)
  
  #Remplacer ???/???, ???@??? et ???|??? avec un espace
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  
  
  # Convertir le texte en minuscule
  docs <- tm_map(docs, content_transformer(tolower))
  # Supprimer les nombres
  docs <- tm_map(docs, removeNumbers)
  # Supprimer les mots vides anglais
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Supprimer votre propre liste de mots non d?sir?s
  docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
  # Supprimer les ponctuations
  docs <- tm_map(docs, removePunctuation)
  # Supprimer les espaces vides suppl?mentaires
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  #head(d, 10)
  
  
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  
  
  findFreqTerms(dtm, lowfreq = 4)
  
  
  findAssocs(dtm, terms = "freedom", corlimit = 0.3)
  
  
  head(d, 10)
  
  
  barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
          col ="lightblue", main ="Most frequent words",
          ylab = "Word frequencies")
  
  
}

nuage_mot(text1)

nuage_mot(text2)

nuage_mot(text3)

nuage_mot(text4)

nuage_mot(text5)

nuage_mot(text6)

nuage_mot(text7)

