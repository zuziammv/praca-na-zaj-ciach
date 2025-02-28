
R version 4.4.2 (2024-10-31) -- "Pile of Leaves"
Copyright (C) 2024 The R Foundation for Statistical Computing
Platform: aarch64-apple-darwin20

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[R.app GUI 1.81 (8462) aarch64-apple-darwin20]

[Workspace restored from /Users/zuzia/.RData]
[History restored from /Users/zuzia/.Rapp.history]

> # Wymagane pakiety
> install.packages(c("tm", "tidytext", "dplyr", "ggplot2", "wordcloud", "RColorBrewer"))
--- Please select a CRAN mirror for use in this session ---
Error in if (res > nrow(m)) { : argument is of length zero
> library(tm)
Loading required package: NLP
> library(tidytext)
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:

    filter, lag

The following objects are masked from ‘package:base’:

    intersect, setdiff, setequal, union

> library(ggplot2)

Attaching package: ‘ggplot2’

The following object is masked from ‘package:NLP’:

    annotate

> library(wordcloud)
Loading required package: RColorBrewer
> library(RColorBrewer)
> 
> # Wczytaj dane tekstowe
> # Wybierz plik tekstowy z dysku
> text <- readLines(file.choose())
> 
> 
> # Przetwarzanie tekstu ----
> 
> # Konwersja na ramkę danych
> text_df <- data.frame(line = 1:length(text), text = text, stringsAsFactors = FALSE)
> 
> # Tokenizacja tekstu (rozbicie na pojedyncze słowa)
> tidy_text <- text_df %>%
+   unnest_tokens(word, text) %>%
+   anti_join(stop_words, by = "word")  # Usunięcie stop słów
> 
> # Analiza częstości słów ----
> 
> # Obliczenie częstości występowania słów
> frequent_terms <- tidy_text %>%
+   count(word, sort = TRUE)
> 
> 
> # Wizualizacja częstości słów ----
> 
> # Wykres słupkowy 10 najczęściej występujących słów
> ggplot(frequent_terms %>% top_n(10), aes(x = reorder(word, n), y = n)) +
+   geom_bar(stat = "identity", fill = "skyblue") +
+   labs(x = "Słowo", y = "Częstość") +
+   coord_flip() +
+   theme_minimal() +
+   ggtitle("Najczęściej występujące słowa")
Selecting by n
Error in UseMethod("depth") : 
  no applicable method for 'depth' applied to an object of class "NULL"
> 
> # Tworzenie chmury słów ----
> # Chmura słów z domyślnymi parametrami
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n)
> 
> # Opcje chmury słów ----
> # Ograniczenie liczby słów w chmurze poprzez określenie minimalnej częstości
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4)
Error in text.default(x1, y1, words[i], cex = size[i], offset = 0, srt = rotWord *  : 
  plot.new has not been called yet
> 
> # Ograniczenie liczby słów w chmurze poprzez określenie maksymalnej liczby słów
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n, max.words = 5)
> 
> # Dodanie koloru do chmury słów
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4, colors = brewer.pal(8, "Dark2"))
> 
> # Dodanie różnych palet kolorystycznych
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4, colors = brewer.pal(9, "Blues"))
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4, colors = brewer.pal(9, "Reds"))
Error in text.default(x1, y1, words[i], cex = size[i], offset = 0, srt = rotWord *  : 
  plot.new has not been called yet
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4, colors = brewer.pal(9, "Greens"))
> 
> # Wymagane pakiety
> install.packages(c("tm", "tidytext", "dplyr", "ggplot2", "wordcloud", "RColorBrewer"))
--- Please select a CRAN mirror for use in this session ---
Error in if (res > nrow(m)) { : argument is of length zero
> library(tm)
> library(tidytext)
> library(dplyr)
> library(ggplot2)
> library(wordcloud)
> library(RColorBrewer)
> 
> # Wczytaj dane tekstowe
> # Wybierz plik tekstowy z dysku
> text <- readLines(file.choose())
> 
> 
> # Przetwarzanie tekstu ----
> 
> # Konwersja na ramkę danych
> text_df <- data.frame(line = 1:length(text), text = text, stringsAsFactors = FALSE)
> 
> # Tokenizacja tekstu (rozbicie na pojedyncze słowa)
> tidy_text <- text_df %>%
+   unnest_tokens(word, text) %>%
+   anti_join(stop_words, by = "word")  # Usunięcie stop słów
> 
> # Analiza częstości słów ----
> 
> # Obliczenie częstości występowania słów
> frequent_terms <- tidy_text %>%
+   count(word, sort = TRUE)
> 
> 
> # Wizualizacja częstości słów ----
> 
> # Wykres słupkowy 10 najczęściej występujących słów
> ggplot(frequent_terms %>% top_n(10), aes(x = reorder(word, n), y = n)) +
+   geom_bar(stat = "identity", fill = "skyblue") +
+   labs(x = "Słowo", y = "Częstość") +
+   coord_flip() +
+   theme_minimal() +
+   ggtitle("Najczęściej występujące słowa")
Selecting by n
> 
> # Tworzenie chmury słów ----
> # Chmura słów z domyślnymi parametrami
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n)
> 
> # Opcje chmury słów ----
> # Ograniczenie liczby słów w chmurze poprzez określenie minimalnej częstości
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4)
> 
> # Ograniczenie liczby słów w chmurze poprzez określenie maksymalnej liczby słów
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n, max.words = 5)
> 
> # Dodanie koloru do chmury słów
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4, colors = brewer.pal(8, "Dark2"))
> 
> # Dodanie różnych palet kolorystycznych
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4, colors = brewer.pal(9, "Blues"))
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4, colors = brewer.pal(9, "Reds"))
> wordcloud(words = frequent_terms$word, freq = frequent_terms$n, min.freq = 4, colors = brewer.pal(9, "Greens"))
Warning message:
In wordcloud(words = frequent_terms$word, freq = frequent_terms$n,  :
  president could not be fit on page. It will not be plotted.
> 
> 