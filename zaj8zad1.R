



# Wymagane pakiety ----
library(tm)           # Przetwarzanie tekstu
library(SnowballC)    # Stemming
library(cluster)      # Klastrowanie
library(wordcloud)    # Chmury s��w
library(factoextra)   # Wizualizacje klastr�w
library(RColorBrewer) # Kolory
library(ggplot2)      # Wykresy
library(dplyr)        # Przetwarzanie danych
library(ggrepel)      # Dodawania etykiet w wykresach
library(DT)           # Interaktywne tabele

install.packages("VCorpus")

# Dane tekstowe ----

# Ustaw Working Directory!
# Za�aduj dokumenty z folderu
docs <- DirSource("textfolder2")
# W razie potrzeby dostosuj �cie�k�
# np.: docs <- DirSource("C:/User/Documents/textfolder2")



# Utw�rz korpus dokument�w tekstowych
corpus <- VCorpus(docs)


### Gdy tekst znajduje si� w jednym pliku csv:
### data <- read.csv("file.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
### corpus <- VCorpus(VectorSource(data$text))


# Korpus
inspect(corpus)


# Korpus - zawarto�� przyk�adowego elementu
corpus[[1]]
corpus[[1]][[1]][7:9]
corpus[[1]][2]




# 1. Przetwarzanie i oczyszczanie tekstu ----
# (Text Preprocessing and Text Cleaning)


# Normalizacja i usuni�cie zb�dnych znak�w ----


# Zapewnienie kodowania w ca�ym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))



# Funkcja do zamiany znak�w na spacj�
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


# Usu� zb�dne znaki lub pozosta�o�ci url, html itp.

# symbol @
corpus <- tm_map(corpus, toSpace, "@")

# symbol @ ze s�owem (zazw. nazwa u�ytkownika)
corpus <- tm_map(corpus, toSpace, "@\\w+")

# linia pionowa
corpus <- tm_map(corpus, toSpace, "\\|")

# tabulatory
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")

# CA�Y adres URL:
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")

# http i https
corpus <- tm_map(corpus, toSpace, "http\\w*")

# tylko uko�nik odwrotny (np. po http)
corpus <- tm_map(corpus, toSpace, "/")

# pozosta�o�� po re-tweecie
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")

# inne pozosta�o�ci
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "–")


# Sprawdzenie
corpus[[1]][[1]][7:9]



corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


# Sprawdzenie
corpus[[1]][[1]][7:9]

# usuni�cie ewt. zb�dnych nazw w�asnych
corpus <- tm_map(corpus, removeWords, c("rose", "roses", "kate", "kates", "iris", "tyler", "tylers", "javi", "javis", "reed", "josh", "joshs", "elliot", "elliots", "julian", "julians", "patrick", "patricks", "margot", "margots", "one", "however", "ladybug"))
corpus <- tm_map(corpus, stripWhitespace)

# Sprawdzenie
corpus[[1]][[1]][7:9]




# Stemming ----

# zachowaj kopi� korpusu 
# do u�ycia jako dictionary w uzupe�nianiu rdzeni
corpus_copy <- corpus

# wykonaj stemming w korpusie
corpus_stemmed <- tm_map(corpus, stemDocument)


# Sprawdzenie
corpus[[1]][[1]][7:9]
# Sprawdzenie
corpus_stemmed[[1]][[1]][7:9]



# Uzupe�nienie rdzeni s��w po stemmingu ----

# funkcja pomocnicza: wykonuje stemCompletion linia po linii
complete_stems <- content_transformer(function(x, dict) {
  x <- unlist(strsplit(x, " "))                  # podziel na s�owa
  x <- stemCompletion(x, dictionary = corpus_copy, type="longest") # uzupe�nij rdzenie
  paste(x, collapse = " ")                       # po��cz z powrotem w tekst
})

# wykonaj stemCompletion do ka�dego dokumentu w korpusie
corpus_completed <- tm_map(corpus_stemmed, complete_stems, dict = corpus_copy)

# usu� NA
corpus_completed <- tm_map(corpus_completed, toSpace, "NA")
corpus_completed <- tm_map(corpus_completed, stripWhitespace)


# Sprawdzenie
corpus_completed[[1]][[1]][1]

# Por�wnaj:
corpus[[1]][[1]][7:9]
corpus_stemmed[[1]][[1]][7:9]



# Decyzja dotycz�ca korpusu ----
# Nale�y w tym momencie rozwa�y�, 
# kt�ry obiekt u�y� do dalszej analizy:
#
# - corpus (oryginalny, bez stemmingu)
# - corpus_stemmed (po stemmingu)
# - corpus_completed (uzupe�nione rdzenie)





# Tokenizacja ----


# Macierze cz�sto�ci TDM i DTM ----


# a) Funkcja TermDocumentMatrix() ----
# tokeny = wiersze, dokumenty = kolumny
tdm <- TermDocumentMatrix(corpus_completed)
tdm
inspect(tdm)


tdm_m <- as.matrix(tdm)

tdm_m[1:5, 1:5]
# Mo�na zapisa� TDM w pliku .csv
# write.csv(tdm_m, file="TDM.csv")


# b) Funkcja DocumentTermMatrix() ----
# dokumenty = wiersze, tokeny = kolumny
dtm <- DocumentTermMatrix(corpus_completed)
dtm
inspect(dtm)

dtm_m <- as.matrix(dtm)

dtm_m[1:5, 1:5]
# Mo�na zapisa� DTM w pliku .csv
# write.csv(dtm_m, file="DTM.csv")



# 2. Zliczanie cz�sto�ci s��w ----
# (Word Frequency Count)

# Mo�na zliczy� same cz�sto�ci s��w w macierzach
# dla TDM i DTM da to identyczny rezultat
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)

v2 <- sort(colSums(dtm_m), decreasing = TRUE)
dtm_df <- data.frame(word = names(v2), freq = v2)
head(dtm_df, 10)



# 3. Eksploracyjna analiza danych ----
# (Exploratory Data Analysis, EDA)


# Chmura s��w (globalna)
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))


# Wy�wietl top 10
print(head(tdm_df, 10))




# 4. In�ynieria cech w modelu Bag of Words: ----
# Reprezentacja s��w i dokument�w w przestrzeni wektorowej ----
# (Feature Engineering in vector-space BoW model)


# - podej�cie surowych cz�sto�ci s��w
# (cz�sto�� s�owa = liczba wyst�pie� w dokumencie)
# (Raw Word Counts)



# U�yj utworzonej wcze�niej macierzy DTM
dtm

inspect(dtm)

dtm_m[1:5, 1:5]




# UCZENIE MASZYNOWE NIENADZOROWANE ----
# (Unsupervised Machine Learning)



# Klastrowanie k-�rednich (k-means) ----


# Dob�r liczby klastr�w
# Metoda sylwetki (silhouette)
fviz_nbclust(t(dtm_m), kmeans, method = "silhouette") +
  labs(title = "Dob�r liczby klastr�w", subtitle = "Metoda sylwetki")



# Wykonaj klastrowanie kmeans
# (sprawd� wyniki dla k = 3,4,5)
set.seed(123) # ziarno losowe dla replikacji wynik�w



# a) Ustaw liczb� klastr�w k = 2 ----
k <- 2 # ustaw liczb� klastr�w


klastrowanie <- kmeans(dtm_m, centers = k)


# Wizualizacja klastr�w
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastr�w dokument�w")



# Interaktywna tabela z przypisaniem dokument�w i top 5 s��w
# Dla ka�dego klastra: liczba dokument�w oraz top 5 s��w
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokument�w = length(cluster_docs_idx),
    Top_5_s��w = top_words,
    stringsAsFactors = FALSE
  )
})

# Po��cz wszystko w ramk� danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokument�w z korpusu
document_names <- names(corpus)

# Tabela przypisania dokument�w do klastr�w
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Do��czamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pe�nym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najcz�stsze s�owa i liczno�� klastr�w",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury s��w dla ka�dego klastra
for (i in 1:k) {
  # znajd� indeksy dokument�w w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plik�w odpowiadaj�ce dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  # generuj chmur� s��w dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura s��w - Klaster", i))
}




# a) Przypisanie dokument�w do klastr�w ----
document_names <- names(corpus)  # Nazwy dokument�w z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokument�w do klastr�w

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podgl�d
print(documents_clusters)


# a) Wizualizacja przypisania dokument�w do klastr�w ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokument�w do klastr�w",
       x = "Dokument",
       y = "Liczba wyst�pie� (powinna wynosi� 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)








# b) Ustaw liczb� klastr�w k = 3 ----
k <- 3 # ustaw liczb� klastr�w


klastrowanie <- kmeans(dtm_m, centers = k)


# Wizualizacja klastr�w
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastr�w dokument�w")



# Interaktywna tabela z przypisaniem dokument�w i top 5 s��w
# Dla ka�dego klastra: liczba dokument�w oraz top 5 s��w
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokument�w = length(cluster_docs_idx),
    Top_5_s��w = top_words,
    stringsAsFactors = FALSE
  )
})

# Po��cz wszystko w ramk� danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokument�w z korpusu
document_names <- names(corpus)

# Tabela przypisania dokument�w do klastr�w
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Do��czamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pe�nym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najcz�stsze s�owa i liczno�� klastr�w",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury s��w dla ka�dego klastra
for (i in 1:k) {
  # znajd� indeksy dokument�w w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plik�w odpowiadaj�ce dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  # generuj chmur� s��w dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura s��w - Klaster", i))
}




# b) Przypisanie dokument�w do klastr�w ----
document_names <- names(corpus)  # Nazwy dokument�w z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokument�w do klastr�w

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podgl�d
print(documents_clusters)


# b) Wizualizacja przypisania dokument�w do klastr�w ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokument�w do klastr�w",
       x = "Dokument",
       y = "Liczba wyst�pie� (powinna wynosi� 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)





# c) Ustaw liczb� klastr�w k = 4 ----
k <- 4 # ustaw liczb� klastr�w


klastrowanie <- kmeans(dtm_m, centers = k)


# Wizualizacja klastr�w
fviz_cluster(list(data = dtm_m, cluster = klastrowanie$cluster),
             geom = "point",
             main = "Wizualizacja klastr�w dokument�w")



# Interaktywna tabela z przypisaniem dokument�w i top 5 s��w
# Dla ka�dego klastra: liczba dokument�w oraz top 5 s��w
cluster_info <- lapply(1:k, function(i) {
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- sort(colSums(cluster_docs), decreasing = TRUE)
  top_words <- paste(names(word_freq)[1:5], collapse = ", ")
  data.frame(
    Klaster = i,
    Liczba_dokument�w = length(cluster_docs_idx),
    Top_5_s��w = top_words,
    stringsAsFactors = FALSE
  )
})

# Po��cz wszystko w ramk� danych
cluster_info_df <- do.call(rbind, cluster_info)

# Nazwy dokument�w z korpusu
document_names <- names(corpus)
# Tabela przypisania dokument�w do klastr�w
documents_clusters <- data.frame(
  Dokument = document_names,
  Klaster = klastrowanie$cluster,
  stringsAsFactors = FALSE
)

# Do��czamy dane z podsumowania (JOIN po klastrze)
documents_clusters_z_info <- left_join(documents_clusters, cluster_info_df, by = "Klaster")

# Interaktywna tabela z pe�nym podsumowaniem
datatable(documents_clusters_z_info,
          caption = "Dokumenty, klastry, najcz�stsze s�owa i liczno�� klastr�w",
          rownames = FALSE,
          options = list(pageLength = 10))




# Chmury s��w dla ka�dego klastra
for (i in 1:k) {
  # znajd� indeksy dokument�w w danym klastrze
  cluster_docs_idx <- which(klastrowanie$cluster == i)
  
  # nazwy plik�w odpowiadaj�ce dokumentom w tym klastrze
  doc_names <- names(klastrowanie$cluster)[cluster_docs_idx]
  
  # generuj chmur� s��w dla klastra
  cluster_docs <- dtm_m[cluster_docs_idx, , drop = FALSE]
  word_freq <- colSums(cluster_docs)
  wordcloud(names(word_freq), freq = word_freq, 
            max.words = 15, colors = brewer.pal(8, "Dark2"))
  title(paste("Chmura s��w - Klaster", i))
}




# c) Przypisanie dokument�w do klastr�w ----
document_names <- names(corpus)  # Nazwy dokument�w z korpusu
clusters <- klastrowanie$cluster  # Przypisanie dokument�w do klastr�w

# Ramka danych: dokumenty i ich klastry
documents_clusters <- data.frame(Dokument = document_names,
                                 Klaster = as.factor(clusters))

# Podgl�d
print(documents_clusters)


# c) Wizualizacja przypisania dokument�w do klastr�w ----
ggplot(documents_clusters, aes(x = reorder(Dokument, Klaster), fill = Klaster)) +
  geom_bar(stat = "count", width = 0.7) +
  coord_flip() +
  labs(title = "Przypisanie dokument�w do klastr�w",
       x = "Dokument",
       y = "Liczba wyst�pie� (powinna wynosi� 1)",
       fill = "Klaster") +
  theme_minimal(base_size = 13)






