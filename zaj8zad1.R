



# Wymagane pakiety ----
library(tm)           # Przetwarzanie tekstu
library(SnowballC)    # Stemming
library(cluster)      # Klastrowanie
library(wordcloud)    # Chmury s³ów
library(factoextra)   # Wizualizacje klastrów
library(RColorBrewer) # Kolory
library(ggplot2)      # Wykresy
library(dplyr)        # Przetwarzanie danych
library(ggrepel)      # Dodawania etykiet w wykresach
library(DT)           # Interaktywne tabele

install.packages("VCorpus")

# Dane tekstowe ----

# Ustaw Working Directory!
# Za³aduj dokumenty z folderu
docs <- DirSource("textfolder2")
# W razie potrzeby dostosuj œcie¿kê
# np.: docs <- DirSource("C:/User/Documents/textfolder2")



# Utwórz korpus dokumentów tekstowych
corpus <- VCorpus(docs)


### Gdy tekst znajduje siê w jednym pliku csv:
### data <- read.csv("file.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
### corpus <- VCorpus(VectorSource(data$text))


# Korpus
inspect(corpus)


# Korpus - zawartoœæ przyk³adowego elementu
corpus[[1]]
corpus[[1]][[1]][7:9]
corpus[[1]][2]




# 1. Przetwarzanie i oczyszczanie tekstu ----
# (Text Preprocessing and Text Cleaning)


# Normalizacja i usuniêcie zbêdnych znaków ----


# Zapewnienie kodowania w ca³ym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))



# Funkcja do zamiany znaków na spacjê
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


# Usuñ zbêdne znaki lub pozosta³oœci url, html itp.

# symbol @
corpus <- tm_map(corpus, toSpace, "@")

# symbol @ ze s³owem (zazw. nazwa u¿ytkownika)
corpus <- tm_map(corpus, toSpace, "@\\w+")

# linia pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# CA£Y adres URL:
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

# http i https
corpus <- tm_map(corpus, toSpace, "http\\w*")

# tylko ukoœnik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# pozosta³oœæ po re-tweecie
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

# inne pozosta³oœci
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")


# Sprawdzenie
corpus[[1]][[1]][7:9]



corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


# Sprawdzenie
corpus[[1]][[1]][7:9]

# usuniêcie ewt. zbêdnych nazw w³asnych
corpus <- tm_map(corpus, removeWords, c("rose", "roses", "kate", "kates", "iris", "tyler", "tylers", "javi", "javis", "reed", "josh", "joshs", "elliot", "elliots", "julian", "julians", "patrick", "patricks", "margot", "margots", "one", "however", "ladybug"))
corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie
corpus[[1]][[1]][7:9]




# Stemming ----

# zachowaj kopiê korpusu 
# do u¿ycia jako dictionary w uzupe³nianiu rdzeni
corpus_copy <- corpus

# wykonaj stemming w korpusie
corpus_stemmed <- tm_map(corpus, stemDocument)


# Sprawdzenie
corpus[[1]][[1]][7:9]
# Sprawdzenie
corpus_stemmed[[1]][[1]][7:9]



# Uzupe³nienie rdzeni s³ów po stemmingu ----

# funkcja pomocnicza: wykonuje stemCompletion linia po linii
complete_stems <- content_transformer(function(x, dict) {
  x <- unlist(strsplit(x, " "))                  # podziel na s³owa
  x <- stemCompletion(x, dictionary = corpus_copy, type="longest") # uzupe³nij rdzenie
  paste(x, collapse = " ")                       # po³¹cz z powrotem w tekst
})

# wykonaj stemCompletion do ka¿dego dokumentu w korpusie
corpus_completed <- tm_map(corpus_stemmed, complete_stems, dict = corpus_copy)

# usuñ NA
corpus_completed <- tm_map(corpus_completed, toSpace, "NA")
corpus_completed <- tm_map(corpus_completed, stripWhitespace)


# Sprawdzenie
corpus_completed[[1]][[1]][1]

# Porównaj:
corpus[[1]][[1]][7:9]
corpus_stemmed[[1]][[1]][7:9]



# Decyzja dotycz¹ca korpusu ----
# Nale¿y w tym momencie rozwa¿yæ, 
# który obiekt u¿yæ do dalszej analizy:
#
# - corpus (oryginalny, bez stemmingu)
# - corpus_stemmed (po stemmingu)
# - corpus_completed (uzupe³nione rdzenie)





# Tokenizacja ----


# Macierze czêstoœci TDM i DTM ----


# a) Funkcja TermDocumentMatrix() ----
# tokeny = wiersze, dokumenty = kolumny
tdm <- TermDocumentMatrix(corpus_completed)
tdm
inspect(tdm)


tdm_m <- as.matrix(tdm)

tdm_m[1:5, 1:5]
# Mo¿na zapisaæ TDM w pliku .csv
# write.csv(tdm_m, file="TDM.csv")


# b) Funkcja DocumentTermMatrix() ----
# dokumenty = wiersze, tokeny = kolumny
dtm <- DocumentTermMatrix(corpus_completed)
dtm
inspect(dtm)

dtm_m <- as.matrix(dtm)

dtm_m[1:5, 1:5]
# Mo¿na zapisaæ DTM w pliku .csv
# write.csv(dtm_m, file="DTM.csv")



# 2. Zliczanie czêstoœci s³ów ----
# (Word Frequency Count)

# Mo¿na zliczyæ same czêstoœci s³ów w macierzach
# dla TDM i DTM da to identyczny rezultat
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)

v2 <- sort(colSums(dtm_m), decreasing = TRUE)
dtm_df <- data.frame(word = names(v2), freq = v2)
head(dtm_df, 10)



# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura s³ów (globalna)
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))


# Wyœwietl top 10
print(head(tdm_df, 10))




# 4. In¿ynieria cech w modelu Bag of Words: ----
# Reprezentacja s³ów i dokumentów w przestrzeni wektorowej ----
# (Feature Engineering in vector-space BoW model)


# - podejœcie surowych czêstoœci s³ów
# (czêstoœæ s³owa = liczba wyst¹pieñ w dokumencie)
# (Raw Word Counts)



# U¿yj utworzonej wczeœniej macierzy DTM
dtm

inspect(dtm)

dtm_m[1:5, 1:5]




# UCZENIE MASZYNOWE NIENADZOROWANE ----
# (Unsupervised Machine Learning)



# Klastrowanie k-œrednich (k-means) ----


# Dobór liczby klastrów
# Metoda sylwetki (silhouette)
fviz_nbclust(t(dtm_m), kmeans, method = "silhouette") +
  labs(title = "Dobór liczby klastrów", subtitle = "Metoda sylwetki")



# Wykonaj klastrowanie kmeans
# (sprawdŸ wyniki dla k = 3,4,5)
set.seed(123) # ziarno losowe dla replikacji wyników



# a) Ustaw liczbê klastrów k = 2 ----
k <- 2 # ustaw liczbê klastrów


klastrowanie <- kmeans(dtm_m, centers = k)


# Wizualizacja klastrów
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrów dokumentów")



# Interaktywna tabela z przypisaniem dokumentów i top 5 s³ów
# Dla ka¿dego klastra: liczba dokumentów oraz top 5 s³ów
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentów = length(cluster_docs_idx),
    Top_5_s³ów = top_words,
    stringsAsFactors = FALSE
  )
})

# Po³¹cz wszystko w ramkê danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokumentów z korpusu
document_names <- names(corpus)

# Tabela przypisania dokumentów do klastrów
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Do³¹czamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pe³nym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczêstsze s³owa i licznoœæ klastrów",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury s³ów dla ka¿dego klastra
for (i in 1:k) {
  # znajdŸ indeksy dokumentów w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plików odpowiadaj¹ce dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  # generuj chmurê s³ów dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura s³ów - Klaster", i))
}




# a) Przypisanie dokumentów do klastrów ----
document_names <- names(corpus)  # Nazwy dokumentów z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokumentów do klastrów

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podgl¹d
print(documents_clusters)


# a) Wizualizacja przypisania dokumentów do klastrów ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokumentów do klastrów",
       x = "Dokument",
       y = "Liczba wyst¹pieñ (powinna wynosiæ 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)








# b) Ustaw liczbê klastrów k = 3 ----
k <- 3 # ustaw liczbê klastrów


klastrowanie <- kmeans(dtm_m, centers = k)


# Wizualizacja klastrów
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrów dokumentów")



# Interaktywna tabela z przypisaniem dokumentów i top 5 s³ów
# Dla ka¿dego klastra: liczba dokumentów oraz top 5 s³ów
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentów = length(cluster_docs_idx),
    Top_5_s³ów = top_words,
    stringsAsFactors = FALSE
  )
})

# Po³¹cz wszystko w ramkê danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokumentów z korpusu
document_names <- names(corpus)

# Tabela przypisania dokumentów do klastrów
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Do³¹czamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pe³nym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczêstsze s³owa i licznoœæ klastrów",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury s³ów dla ka¿dego klastra
for (i in 1:k) {
  # znajdŸ indeksy dokumentów w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plików odpowiadaj¹ce dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  # generuj chmurê s³ów dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura s³ów - Klaster", i))
}




# b) Przypisanie dokumentów do klastrów ----
document_names <- names(corpus)  # Nazwy dokumentów z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokumentów do klastrów

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podgl¹d
print(documents_clusters)


# b) Wizualizacja przypisania dokumentów do klastrów ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokumentów do klastrów",
       x = "Dokument",
       y = "Liczba wyst¹pieñ (powinna wynosiæ 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)





# c) Ustaw liczbê klastrów k = 4 ----
k <- 4 # ustaw liczbê klastrów


klastrowanie <- kmeans(dtm_m, centers = k)


# Wizualizacja klastrów
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastrów dokumentów")



# Interaktywna tabela z przypisaniem dokumentów i top 5 s³ów
# Dla ka¿dego klastra: liczba dokumentów oraz top 5 s³ów
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokumentów = length(cluster_docs_idx),
    Top_5_s³ów = top_words,
    stringsAsFactors = FALSE
  )
})

# Po³¹cz wszystko w ramkê danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokumentów z korpusu
document_names <- names(corpus)
# Tabela przypisania dokumentów do klastrów
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Do³¹czamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pe³nym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najczêstsze s³owa i licznoœæ klastrów",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury s³ów dla ka¿dego klastra
for (i in 1:k) {
  # znajdŸ indeksy dokumentów w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plików odpowiadaj¹ce dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  # generuj chmurê s³ów dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura s³ów - Klaster", i))
}




# c) Przypisanie dokumentów do klastrów ----
document_names <- names(corpus)  # Nazwy dokumentów z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokumentów do klastrów

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podgl¹d
print(documents_clusters)


# c) Wizualizacja przypisania dokumentów do klastrów ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokumentów do klastrów",
       x = "Dokument",
       y = "Liczba wyst¹pieñ (powinna wynosiæ 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)






