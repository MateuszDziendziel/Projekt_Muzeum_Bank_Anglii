#Wczytanie pliku i ustawienie odpowiedniego folderu 
setwd("C:/Users/mateu/OneDrive/Pulpit/Statystyka w biznesie")
museum_data <- read.csv("BankOfEnglandMuseumComments (1).csv")
#Wczytanie potrzebnych pakietów do pracy z danymi i ich uaktywnienie 
install.packages("tidyverse")
install.packages("tidytext")
install.packages("wordcloud2")
install.packages("lubridate")
install.packages("car")
install.packages("tm")
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(lubridate)
library(car)
library(tm)

# Sprawdzenie zbioru danych
glimpse(museum_data)

# Na początku należy oczyścić dane, ponieważ w dużej mierze są one nieczytelne lub zbędne.  
# Pierwszym krokiem będzie wybór kolumn istotnych dla dalszej analizy.  
# Następnie należy przekształcić nieczytelne daty na liczbę lat od momentu dodania opinii,  
# a także wprowadzić kilka dodatkowych kosmetycznych zmian.

clean_museum_data <- museum_data %>%
  select(is_user_local_guide, published_at_datetime, score, text)

# Dodatkowy krok pozwalający ustalić, której funkcji konwertującej należy później użyć 
#(w tym przypadku as.Date dla typu "character")
 class(clean_museum_data$published_at_datetime) #Wychodzi, że "published_at_datetime" to "character"


clean_museum_data <- clean_museum_data %>%
  mutate(published_at_datetime = as.Date.character(published_at_datetime)) %>% 
  mutate(published_at_datetime = as.numeric( max(published_at_datetime) - published_at_datetime)) %>% 
  mutate(published_at_datetime = (published_at_datetime / 356)) %>% 
  rename(years_since_publishing = published_at_datetime) %>% 
  mutate(is_user_local_guide = dplyr::recode(is_user_local_guide, "t" = "TAK", "f" = "NIE"))
  

  
# Nasze dane są już czyste i uporządkowane, można przejść do zadań:

#1. Jaka jest średnia ocena muzeum 
Ex_1 <- list() # Tworzenie listy dla zadania 1, gdzie będą zapisywane wczystkie wyniki 

Ex_1$average_museum_score <- clean_museum_data %>%
   summarize(average_museum_score = round(mean(score), 2)) %>%
  pull(average_museum_score)


#Wykres 1.1 Rozkład ilości wystawionych gwiazdek 
ggplot(clean_museum_data, aes(x = score)) + 
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  geom_text(
    stat = "bin",
    aes(label = ..count..), 
    binwidth = 1, 
    vjust = -0.5, 
    color = "black", 
    size = 4
  ) +
  labs(title = "Rozkład ilości wystawionych gwiazdek ", x = "Ilość gwiazdek", y = "Częstość") + 
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
  )


 
# Z wykresu możemy wyciągnąć kilka ciekawych wniosków. Po pierwsze, najczęściej wystawianą
# oceną jest 5 gwiazdek, a najrzadziej – 2 gwiazdki. Po drugie, widoczna jest wyraźna
# dominacja ocen pozytywnych.
 
# Na tym etapie warto również zwrócić uwagę na inne miary takie jak mediana 
 Ex_1$summary_score <- summary(clean_museum_data$score)

# Z wyników możemy odczytać m.in., że mediana wynosi 5, co oznacza, że przynajmniej 50% ocen
# to 5. Dodatkowo, 1. kwartyl wynosi 4, co oznacza, że co najmniej 75% ocen to 4 lub wyższe.
 

 
#2.Czy zmienia się ona w czasie? (proszę przyjąć założenie o doborze losowym 
# respondentów w zbiorze danych i zastosować odpowiedni test statystyczny)
Ex_2 <- list()
# W pierwszej kolejności tworzymy grupy, aby porównać między nimi średnie za pomocą testu ANOVA. 
# Grupy zostaną podzielone według lat: 0–2 → okres popandemiczny, 3–4 → okres pandemii, 
# 5–6 → okres przedpandemiczny, 7–14 → dalsza przeszłość.
 
clean_museum_data <- clean_museum_data %>%
  mutate(Years = cut(years_since_publishing, c(0, 2, 4, 6, 14), right = TRUE,
                     labels = c("popandemiczny(2024-2022)", "pandemia(2021-2020)", "przedpandemiczny(2019-2018)", "przeszłość(2017-2010)"), 
                     include.lowest = TRUE))

# Zanim przejdziemy do testu statystycznego, warto przyjrzeć się wykresowi. UWAGA: punkty na wykresie
# są rozmieszczane losowo w obrębie swojej grupy i oceny przy każdym generowaniu wykresu. Oznacza to,
# że duża liczba punktów po prawej stronie niekoniecznie oznacza dużą liczbę obserwacji na końcu zakresu
# danej grupy.


#Wykres 2.1 Całkowita ilość wystawionych recenzji oraz ilość wystawionych gwiazdek w recenzjach
#w zależności od wykresu
ggplot(clean_museum_data, aes(x = Years, y = score, color = Years)) + geom_jitter() +
  labs(title = "Całkowita ilość wystawionych recenzji oraz \nilość wystawionych gwiazdek w recenzjach w zależności od okresu", 
       y = "Ilość wystawionych gwiazdek",  color = "Okres") + 
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold", margin = margin(b = 15)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 13)),
        axis.text.x = element_blank()) 

# Z wykresu możemy wyciągnąć kilka interesujących wniosków. Po pierwsze, rzuca się w oczy, że w latach 
# 3-4 jest bardzo mało obserwacji, co najprawdopodobniej jest związane z pandemią i ograniczoną dostępnością
# muzeów w tym okresie, co naturalnie prowadzi do braku recenzji. Po drugie, można zaobserwować, że rozkład
# ocen w latach przedpandemicznych (5-6) jest zbliżony do tego po pandemii (0-2). W związku z tym, można
# stwierdzić, że "jakość" muzeów, mimo okresu zamknięcia, nie spadła, a utrzymuje się na podobnym poziomie.
# Na koniec, widać pokrycie z poprzednim wykresem - dominację pozytywnych recenzji oraz stosunkowo 
# niewielką liczbę recenzji negatywnych.


# Teraz, aby przeprowadzić test ANOVA i zbadać, czy istnieje istotna statystycznie różnica między średnimi
# w grupach, musimy spełnić kilka warunków:

# 1) Dane w każdej z grup powinny mieć rozkład normalny.
#    Na podstawie wykresu może się wydawać, że ten warunek nie jest spełniony, 
#    jednak każda z grup zawiera więcej niż 30 obserwacji. Zgodnie z twierdzeniem granicznym, 
#    możemy przyjąć, że ten warunek jest spełniony. → Spełniony

# 2) Minimum trzy grupy (dla dwóch lub mniej grup zastosowalibyśmy test t-Studenta). → Spełniony

# 3) Homoskedastyczność (czyli podobne wariancje między grupami).
     leveneTest(score ~ Years, data = clean_museum_data)
#    Wynik testu Levene’a jest bardzo bliski 0, co wskazuje na istotne różnice w wariancjach.
#    Nie jest to zaskoczeniem – na wykresie widać, że dane w latach 3–4 są bardzo rozproszone.
#    W związku z tym warunek homoskedastyczności nie jest spełniony,
#    co może wpływać na wiarygodność wyników testu ANOVA.

# 4) Niezależność obserwacji. → Spełniony

# W związku z tym, że nie spełniamy warunku homoskedastyczności, który jest niezbędny do uzyskania 
# poprawnych wyników w teście ANOVA, zatosujemy test Test Welch’a (Welch’s ANOVA), który działa dobrze
# nawet, przy dużych różnicach w wariancjach między grupami 
     oneway.test(score ~ Years, data = clean_museum_data, var.equal = FALSE)

# P-value tego testu wynosi 0.00313. Przyjmując poziom istotności α = 0.05,
# możemy odrzucić hipotezę zerową (H0): średnie wartości w porównywanych grupach są równe,
# na rzecz hipotezy alternatywnej (H1): przynajmniej jedna z grup ma średnią różną od pozostałych.
# Taki wynik jest logiczny i spójny z wcześniejszą wizualizacją, na której zauważyliśmy,
# że lata 3-4 charakteryzowały się znacznie niższą średnią ocen niż pozostałe okresy.

#Teraz gdy przeprowadziliśmy już test i wiemy, że średnie między grupami różnią się, spróbujmy to
#ponownie pokazać na wykresie 
Ex_2$average_group_score <- clean_museum_data %>%
  group_by(Years) %>%
  summarize(average_score = mean(score))
    
#Wykres 2.2 Średnia ilość wystawionych gwiazdek ze względu na okres 
     ggplot(Ex_2$average_group_score, aes(x = Years, y = average_score, group = 1)) +
       geom_line(color = "steelblue", size = 1) + 
       geom_point(size = 3, shape = 21, fill = "white", color = "steelblue", stroke = 1.2) +
       geom_text(aes(label = round(average_score, 2)), vjust = -1, size = 3.5) +  
       labs(title = "Średnia ilość wystawionych gwiazdek ze względu na okres", x = "Okres", y = "Srednia ilość wystawionych gwiazdek") + 
       expand_limits(y = 0) +  
       theme_minimal() +
       scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
       theme(
         plot.title = element_text(hjust = 0.5, face = "bold", size = 13, margin = margin(b = 20)),
         axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
         axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 15)))
     
#Warto tutaj głównie zwrócić uwagę na lata 3-4, w których średnia poszybowała w dół. Z racji na to
#iż jest to okres pandemii, warto by się zastanwoić czy aby na pewno były przestrzegane wszystkie \
#normy w tym czasie oraz czy pracownicy dbali o higienę i bezpieczeństwo

#3.1 jaki odsetek osób zostawiających ocenę Muzeum zostawia także komentarz 
#słowny?
Ex_3 <- list()

Ex_3$wiersze_bez_tekstu <- clean_museum_data %>%
  filter(text == "") %>%
  count(text) %>%
  summarize(liczba_wszystkich_wierszy_bez_tekstu = sum(n)) %>%
  pull(liczba_wszystkich_wierszy_bez_tekstu) #Wyciąga liczbę, co pozwala później na operacje między elementami znajdującymi się w liście 
Ex_3$wszystkie_wiersze <- clean_museum_data %>%
  count(text) %>%
  summarize(liczba_wszystkich_wierszy = sum(n)) %>%
  pull(liczba_wszystkich_wierszy)

Ex_3$wiersze_z_tekstem  = Ex_3$wszystkie_wiersze - Ex_3$wiersze_bez_tekstu
Ex_3$komentarz_oraz_gwiazdki = round(Ex_3$wiersze_z_tekstem / Ex_3$wszystkie_wiersze, 3)
Ex_3$gwiazdki_bez_komentarzu = round(1 - Ex_3$komentarz_oraz_gwiazdki, 3)

Ex_3$komentarz_oraz_gwiazdki_w_procentach = Ex_3$komentarz_oraz_gwiazdki * 100
Ex_3$gwiazdki_bez_komentarzu_w_procentach = Ex_3$gwiazdki_bez_komentarzu * 100
Ex_3$percentages = c(Ex_3$komentarz_oraz_gwiazdki_w_procentach, Ex_3$gwiazdki_bez_komentarzu_w_procentach )

#Wykres 3.1 Wystawione recenzje z komentarzem/ bez komenatrza 
pie(c(Ex_3$komentarz_oraz_gwiazdki, Ex_3$gwiazdki_bez_komentarzu), labels = c("Recenzja z gwiazdkami i komentarzem", "Recenzja z gwiazdkami bez komentarza"),
    col = c("green", "red"), main = "Wystawione recenzje z komentarzem/bez komentarza ") + text(0, 0.35, labels = paste(Ex_3$wiersze_z_tekstem, "(", Ex_3$percentages[1], "%)"), cex = 1.5)  
text(0.2, -0.3, labels = paste(Ex_3$wiersze_bez_tekstu , "(", Ex_3$percentages[2], "%)"), cex = 1.5) 

#3.2 czy osoby oceniające Muzeum lepiej mają większe prawdopodobieństwo 
#zostawienia komentarza? 

# Dodamy kolumnę, która zawiera wartość TRUE, jeżeli długość tekstu w kolumnie "text" 
# jest większa niż 0, oraz FALSE, jeżeli długość tekstu wynosi 0. Następnie przekonwertujemy 
# te wartości na liczby 0 i 1. W ten sposób będziemy mogli obliczyć, jaki ułamek osób 
# zostawiających ocenę dodaje również komentarz w każdej grupie.

clean_museum_data<- clean_museum_data %>%
  mutate(text_exists = nchar(text) > 0) %>% 
  mutate(text_exists = as.numeric(text_exists))  
 
 Ex_3$scores_having_comments <- clean_museum_data  %>%
  group_by(score) %>%
  summarize(fraction_of_reviews_having_text =   100 * (round(mean(text_exists), 2))) 

#Wykres 3.2 Procent recenzji zawierający komentarz ze względu na ilość wystawionych gwiazdek
 ggplot(Ex_3$scores_having_comments, aes(x = score, y = fraction_of_reviews_having_text, fill = factor(score))) +
   geom_col() + geom_text(aes(label = fraction_of_reviews_having_text),  vjust = -0.5, size = 4, fontface = "bold") + 
   scale_y_continuous(limits = c(0, 60)) + 
                        labs(title = "procent recenzji zawierający komentarz ze względu na ilość wystawionych gwiazdek",
                      y = "procent recenzji posiadających komentarz",
                      fill = " Ilość gwiazdek") + 
   theme( axis.title.x = element_blank(),  
          axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
          axis.title.y = element_text(face = "bold", size = 11, margin = margin(r = 13)))

    
 
# Na podstawie wyników możemy zauważyć, że osoby wystawiające najwyższe oceny 
# najczęściej zostawiają również komentarze. Z kolei, najmniej komentarzy pojawia się 
# przy ocenach średnich. Może to wynikać z faktu, że osoby wystawiające oceny 3 lub 4 
# nie przeżywają tak silnych emocji jak przy ocenach skrajnych (1 lub 5), 
# dlatego nie czują potrzeby dzielenia się swoją opinią.
 
 
#4.1 w jakich okresach dodawanych jest najwięcej komentarzy?
Ex_4 <- list()

Ex_4$periods_with_most_comments <- clean_museum_data %>%
  filter(text_exists == "1") %>%
  group_by(Years) %>%
  count(text_exists) %>%
  arrange(desc(n)) %>%
  rename(number_of_comments = n) %>%
  select(-text_exists)
#Wykres 4.1 Ilość dodanych komentarzy ze względu na okres 
ggplot(Ex_4$periods_with_most_comments, aes( x = Years, y = number_of_comments, fill = Years)) + geom_col() + 
  geom_text(aes(label = number_of_comments), vjust = -0.5) + 
  labs(title = "Ilość dodanych komentarzy ze względu na okres", y = "Ilość dodanych komentarzy", fill= "Okres") + theme_minimal() +
  theme( plot.title = element_text(hjust = 0.5, size = 13, face = "bold", margin = margin(b = 20)),
         axis.title.x = element_blank(), axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
         axis.text.x = element_blank()) 


#Najwięcej komentarzy pojawiło się 5 - 6 lat temu, najmniej było ich 3-4 lata temu, czyli w okresie 
#pandemii co ma oczywiście dużo sensu. Muzea w tym okresie były zamknięte co naturalnie skutkuje
#dużo niższą ilością recenzji w tym okresie. 


#4.2 Jakiej długości są zostawione komentarze?
 # \\s -->usuwa znaki białe, \\p{P} -->usuwa znaki interpunkcyjne , będzie to liczone z użyciem tego
 Ex_4$length_of_comments <- clean_museum_data %>%
  select(text) %>%  
  mutate(characters_clear = nchar(str_remove_all(text,"[\\s, \\p{P}]"))) %>% 
  filter(characters_clear >0) 
 
 
 Ex_4$average_and_total_number_of_clear_characters <- Ex_4$length_of_comments %>%
   summarize(average_number_of_clear_characters = round(mean(characters_clear)), 
             total_number_of_clear_characters = sum(characters_clear),
             min_number_of_clear_characters = min(characters_clear),
             max_number_of_clear_characters = max(characters_clear),
             median_number_of_clear_characters = median(characters_clear))
 
 print(Ex_4$average_and_total_number_of_clear_characters)

# Średnia długość komentarza wynosi 105 znaków, podczas gdy mediana to 64 znaki. 
# Oznacza to, że istnieje kilka bardzo długich komentarzy, które podwyższają średnią. 
# Dodatkowo, możemy stwierdzić, że dokładnie połowa komentarzy ma długość nie większą 
# niż 64 znaki. Minimalna długość komentarza wynosi 3 znaki, a maksymalna to aż 1664 znaki.
 

#5.1 jakie sformułowania pojawiają się najczęściej w komentarzach? 
Ex_5 <- list()


Ex_5$number_of_each_word <-clean_museum_data %>% 
   select(text) %>%
   mutate(number_of_characters = nchar(text)) %>% #Zlicza ilość znaków w komentarzu
   filter(number_of_characters > 0) %>% #Zostawia tylko te wiersze, gdzie rzeczywiście były komentarze
   select(-number_of_characters) %>% #Przefiltrowalismy więc to już nie jest potrzebne
   unnest_tokens(word, text ) %>% #Rozdzielanie tekstu na pojedyczne słowa 
   count(word, sort = TRUE) %>% #Zliczanie ile razy dane słowo się pojawiło + sortowanie
   rename(number_of_appearances = n) %>% #Zmiana nazwy kolumny 
   arrange(desc(number_of_appearances)) 
#Konkretnie pod wykres, tak aby móc wciąż podejrzeć wszystkie w razie potrzeby, ale na wykres mieć tylko 
#25 najbardziej licznych 
Ex_5$number_of_each_word_graph <- Ex_5$number_of_each_word %>%
   arrange(desc(number_of_appearances)) %>%
   slice_max(number_of_appearances, n = 25)


#Wykres 5.1 Ilość wystąpień słów w komentarzach ( ze stopwordsami)
ggplot(Ex_5$number_of_each_word_graph, aes(x = number_of_appearances, y = fct_reorder(word, number_of_appearances))) + geom_col(fill= "skyblue", color = "white") + 
  geom_text(aes(label = number_of_appearances), hjust = 1, size = 3.5, color = "black") + scale_x_continuous(expand = c(0, 0)) +
   labs( y = "Słowo", x = "Ilość wystąpień", title = "Ilość wystąpień słów w komentarzach (ze stopwordsami)") +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold", margin = margin(b = 10)), 
                                  axis.title.y = element_text(size = 11.5, face = "bold"),
                                  axis.title.x = element_text(size = 11.5, face = "bold", margin = margin(t = 10)))


# Teraz wczytam stopwordsy, czyli słowa, które niewiele wnoszą do analizy, 
# aby przefiltrować dane i pozostawić jedynie te, które rzeczywiście są istotne.

Ex_5$stopwords <- stop_words  


Ex_5$number_of_each_word_without_stopwords <- Ex_5$number_of_each_word %>%
anti_join(Ex_5$stopwords, by= "word") %>%
  arrange(desc(number_of_appearances))

#Tak samo jak wcześniej, stricte pod wykres
Ex_5$number_of_each_word_without_stopwords_graph <- Ex_5$number_of_each_word_without_stopwords %>%
  arrange(desc(number_of_appearances)) %>%
  slice_max(number_of_appearances, n = 25)



#Wykres 5.2 Ilość wystąpień słów w komentarzach ( bez stopwordsów)
ggplot(Ex_5$number_of_each_word_without_stopwords_graph, aes(x = number_of_appearances, y = fct_reorder(word, number_of_appearances))) + geom_col(fill= "skyblue", color = "white") + 
  geom_text(aes(label = number_of_appearances), hjust = 1, size = 3.5, color = "black") + scale_x_continuous(expand = c(0, 0)) +
  labs( y = "Słowo", x = "Ilość wystąpień", title = "Ilość wystąpień słów w komentarzach (bez stopwordsów)") +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold", margin = margin(b = 10)), 
        axis.title.y = element_text(size = 11.5, face = "bold"),
        axis.title.x = element_text(size = 11.5, face = "bold", margin = margin(t = 10)))


 
#5.2 Czy są one nacechowane pozytywnie czy negatywnie? 

# Słownik, który dzieli słowa na negatywne i pozytywne, niestety ma swoje ograniczenia 
# w postaci ograniczonej liczby słów. Dlatego może się zdarzyć, że niektóre słowa zostaną 
# pominięte, mimo że mogłyby być istotne dla analizy.


Ex_5$bing_dictionary <- get_sentiments("bing")  


Ex_5$words_positive_negative <- Ex_5$number_of_each_word_without_stopwords %>%
inner_join(Ex_5$bing_dictionary, by="word") 

#Przygotowanie pod wykres z samymi "pozytywnymi słowami" 
Ex_5$words_positive_graph <- Ex_5$words_positive_negative %>%
  filter(sentiment == "positive") %>%
  slice_max(number_of_appearances, n = 25)

#Przygotowanie pod wykres z samymi "negatywnymi słowami"
Ex_5$words_negative_graph <- Ex_5$words_positive_negative %>%
  filter(sentiment == "negative") %>%
  slice_max(number_of_appearances, n = 25)

#Wykres 5.3 Ilość wystąpień pozytywnych słów w komentarzach (bez stopwordsów)
ggplot(Ex_5$words_positive_graph, aes(x = number_of_appearances, y = fct_reorder(word, number_of_appearances))) + geom_col(fill= "green", color = "white") + 
  geom_text(aes(label = number_of_appearances), hjust = 1, size = 3.5, color = "black") + scale_x_continuous(expand = c(0, 0)) +
  labs( y = "Słowo", x = "Ilość wystąpień", title = "Ilość wystąpień pozytywnych słów w komentarzach (bez stopwordsów)") +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold", margin = margin(b = 10)), 
        axis.title.y = element_text(size = 11.5, face = "bold"),
        axis.title.x = element_text(size = 11.5, face = "bold", margin = margin(t = 10)))

#Wykres 5.4 Ilość wystąpień negatywnych słów w komentarzach (bez stopwordsów)

ggplot(Ex_5$words_negative_graph, aes(x = number_of_appearances, y = fct_reorder(word, number_of_appearances))) + geom_col(fill= "lightcoral", color = "white") + 
  geom_text(aes(label = number_of_appearances), hjust = 1, size = 3.5, color = "black") + scale_x_continuous(expand = c(0, 0)) +
  labs( y = "Słowo", x = "Ilość wystąpień", title = "Ilość wystąpień negatywnych słów w komentarzach (bez stopwordsów)") +
  theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold", margin = margin(b = 10)), 
        axis.title.y = element_text(size = 11.5, face = "bold"),
        axis.title.x = element_text(size = 11.5, face = "bold", margin = margin(t = 10)))



#Podsumowanie 
Ex_5$words_positive_negative_total <- Ex_5$words_positive_negative %>%
  group_by(sentiment) %>%
  summarize(total_number_of_words = sum(number_of_appearances))


# Słów nacechowanych pozytywnie jest łącznie 1471, natomiast negatywnie 201. 
# To oznacza nieco ponad 7-krotną przewagę słów pozytywnych, co wskazuje na 
# pozytywne odczucia związane z muzeum, co jest zgodne z wcześniejszymi analizami.


#6. Jak można wizualnie podsumować treść komentarzy? 
Ex_6 <- list()

# Przefiltrujmy nieco listę słów, aby nie było ich zbyt wiele. Jest to niezbędne dla lepszej 
# czytelności wykresu.
Ex_6$words_positive_negative_slicemax_30 <- Ex_5$words_positive_negative %>%
  mutate(color = ifelse(sentiment == "positive", "green", "red")) %>%
  rename(freq = number_of_appearances) %>%
  arrange(desc(freq)) %>%
  slice_max(freq, n = 30)
#Wykres 6.1 Wordcloud z podziałem na słowa pozytywne/negatywne (COŚ TU NIE DZIAŁA, SŁOWA 
# : GEM I HELPFUL SIĘ MIKSUJĄ)
wordcloud2(Ex_6$words_positive_negative_slicemax_30, size = 1.7, color = Ex_6$words_positive_negative_slicemax_30$color)

#Wykres 6.2 Wordcloud bez podziału na słowa pozytywne/negatywne 
wordcloud2(Ex_6$words_positive_negative_slicemax_30, size = 1.7)

# Możemy zauważyć, że największe słowa są w większości nacechowane pozytywnie. Ciekawym przypadkiem 
# jest słowo "gold". Można się zastanawiać, czy pojawienie się tego słowa wynika z określenia 
# czegoś jako "gold", najprawdopodobniej jednak jest to odniesienie do elementu, który był bardzo popularny 
# i wzbudził wiele emocji wśród odwiedzających. Jedną z głównych (najprawdopodobniej) zalet wskazywanych przez 
# zwiedzających jest "free", które z dużym prawdopodobieństwem odnosi się do darmowego wstępu.
#Należy jednak pamiętać, że te słowa mogły być połączone z "no"/"not", co mogłoby nadać im zupełnie przeciwne znaczenie.
# W przypadku tego konkretnego słowa można by łatwo zweryfikować ten fakt, 
# sprawdzając, czy rzeczywiście wstęp był darmowy. Jeśli tak było, 
# można wyciągnąć wniosek, że jest to jedna z największych zalet wskzywanych wśród zwiedzających.


#DODATKOWA ANALIZA 
ex_extra <- list()

#Ile osób zalicza się jako local_guide, a ile nie? 
ex_extra$number_local_guide <- clean_museum_data %>%
  select(is_user_local_guide) %>%
  group_by(is_user_local_guide) %>%
  count() %>%
  rename(number = n) 

ex_extra$not_local_guide <- ex_extra$number_local_guide[1,2]
ex_extra$is_local_guide <- ex_extra$number_local_guide[2,2]  


ex_extra$not_local_guide_fraction <- round(ex_extra$not_local_guide / sum(ex_extra$is_local_guide + ex_extra$not_local_guide), 3)
ex_extra$is_local_guide_fraction <- round(ex_extra$is_local_guide / sum(ex_extra$is_local_guide + ex_extra$not_local_guide), 3)

ex_extra$not_local_guide_percent <- ex_extra$not_local_guide_fraction * 100
ex_extra$is_local_guide_percent <- ex_extra$is_local_guide_fraction * 100


ex_extra$results = c(1180, 531)
ex_extra$labels = c("Osoba jest local guide", "osoba nie jest local guide")

#Wykres Extra_1 Osoba jest/nie jest local guide 
pie(ex_extra$results, labels = ex_extra$labels, col = c("green", "red"), main = "Osoba jest/ nie jest local guide ") + 
  text(0.1, 0.25, labels = paste(ex_extra$is_local_guide , "(", ex_extra$is_local_guide_percent, "%)"), cex = 1.2)  +
  text(0.3, -0.4, labels = paste(ex_extra$not_local_guide, "(", ex_extra$not_local_guide_percent, "%)"), cex = 1.2)  




#1.2 Czy osoby będące "local_guide" wystawiają średnio wyższe oceny? 

ex_extra$local_guide_score <- clean_museum_data %>%
   group_by(is_user_local_guide) %>%
   summarize(average_score = round(mean(score), 2))
#Wykres Extra_2 Średnia liczba wystawionych gwiazdek w zależności od bycia local guide
ggplot(ex_extra$local_guide_score, aes(x = is_user_local_guide, y = average_score, fill = is_user_local_guide)) + geom_col() +
  labs(title = "Średnia liczba wystawionych gwiazdek w zależności od bycia local guide",  y = "Średnia liczba wystawionych gwiazdek", x=element_blank(), fill = "local guide") + 
  theme_minimal() + 
  geom_text(aes(label = round(average_score, 2)), vjust = -0.5, size = 4) +
  theme( plot.title = element_text(face = "bold", size = 13, margin = margin(b = 20)),
         axis.title.y = element_text(face = "bold", size = 11.5, margin = margin(r = 10)), axis.text.x =  element_blank()) + 
  expand_limits(y = c(0, 4.5))



#1.3 czy osoby te zamieszczają więcej komentarzy?

ex_extra$local_guide_comments <- clean_museum_data %>%
  group_by(is_user_local_guide) %>%
  summarize(average_comment =  100 * round(mean(text_exists), 2))

#Wykres Extra_3  procent całości komentarzy w zależności od bycia local guide 
ggplot(ex_extra$local_guide_comments, aes(x = is_user_local_guide, y = average_comment, fill = is_user_local_guide)) + geom_col() +
  labs(title = "procent całości komentarzy w zależności od bycia local guide",  y = "procent wszystkich komentarzy", x=element_blank(), fill = "local guide") + 
  theme_minimal() + 
  geom_text(aes(label = round(average_comment, 2)), vjust = -0.5, size = 4) +
  theme( plot.title = element_text(face = "bold", size = 13, margin = margin(b = 20), hjust = 0.5),
         axis.title.y = element_text(face = "bold", size = 11.5, margin = margin(r = 10)), axis.text.x =  element_blank()) + 
  expand_limits(y = c(0, 100))












  


  

  
 

        
                              

 




 


  



