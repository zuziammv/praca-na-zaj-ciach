



# S³owniki sentymentu

library(tm)
library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(ggthemes)

# Pobierz s³owniki (leksykony sentymentu) 
# w uporz¹dkowanym formacie, gdzie ka¿demu s³owu odpowiada jeden wiersz,
# - jest to forma, któr¹ mo¿na po³¹czyæ z zestawem danych 
# zawieraj¹cym jedno s³owo na wiersz.
# https://juliasilge.github.io/tidytext/reference/get_sentiments.html
#
# get_sentiments(lexicon = c("bing", "afinn", "nrc", "loughran"))



# S³ownik Bing ----
# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
get_sentiments("bing")

# Podsumowujemy s³ownik Bing, licz¹c wyst¹pienia s³ów
get_sentiments("bing") %>%
  count(sentiment)
# W s³owniku Bing znajduje siê ponad 4 tysi¹ce negatywnych
# oraz ponad 2 tysi¹ce pozytywnych terminów



# S³ownik Afinn ----
# https://darenr.github.io/afinn/
get_sentiments("afinn")

# Podsumowujemy s³ownik Afinn, sprawdzaj¹c minimaln¹ i maksymaln¹ wartoœæ
get_sentiments("afinn") %>%
  summarize(
    min = min(value),
    max = max(value)
  )

# Wartoœci sentymentu mieszcz¹ siê w przedziale od -5 do 5



# S³ownik NRC ----
# https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
get_sentiments("nrc")

# Zliczmy liczbê s³ów powi¹zanych z ka¿dym sentymentem w s³owniku NRC
get_sentiments("nrc") %>% 
  count(sentiment) %>% 
  # Sortujemy liczebnoœæ sentymentów w kolejnoœci malej¹cej
  arrange(desc(n))

# Pobieramy s³ownik NRC, liczymy sentymenty i sortujemy je wed³ug liczebnoœci
sentiment_counts <- get_sentiments("nrc") %>% 
  count(sentiment) %>% 
  mutate(sentiment2 = fct_reorder(sentiment, n))

# Wizualizacja liczby wyst¹pieñ sentymentów 
# u¿ywaj¹c nowej kolumny typu factor o nazwie sentiment2
ggplot(sentiment_counts, aes(x=sentiment2, y=n)) +
  geom_col(fill="goldenrod1") +
  coord_flip() +
  # Wstawiamy tytu³, nazwê osi X jako "Sentyment" i osi Y jako "Liczba"
  labs(x = "Sentyment", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Kategorie sentymentu w NRC")



# S³ownik Loughran ----
# dostêpny w pakiecie "lexicon"
# https://emilhvitfeldt.github.io/textdata/reference/lexicon_loughran.html
get_sentiments("loughran")


# Podsumowujemy s³ownik Loughran w nastêpuj¹cy sposób:
sentiment_counts <- get_sentiments("loughran") %>%
  count(sentiment) %>%
  mutate(sentiment2 = fct_reorder(sentiment, n))

ggplot(sentiment_counts, aes(x=sentiment2, y=n)) + 
  geom_col(fill="darkorchid3") +
  coord_flip() +
  labs(x = "Sentyment", y = "Liczba") +
  theme_gdocs() + 
  ggtitle("Kategorie sentymentu w Loughran")




