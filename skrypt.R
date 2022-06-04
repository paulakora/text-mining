# Porządki,  biblioteki,  środowisko --------------------------------------

cat("\014")
rm(list = ls())
dev.off()

library(tidyverse)
library(tidytext)
library(stringr)
library(lubridate)
library(SnowballC)
library(wordcloud)

setwd("/Users/paulakoralewska/Desktop/studia/semestr 3/text mining")


# Przygotowanie danych ----------------------------------------------------

load("Projekt_zal_dane.RData")
View(articles)

articles <- articles %>%
  mutate(section = str_sub(url, 43, 48)) %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date),
         hour = hour(date),
         wday = factor(wday(date),
                       levels = c(2, 3, 4, 5, 6, 7, 1),
                       labels = c("pn", "wt", "śr", "czw", "pt", "sb", "nd")))

head(articles)

articles_sample <- articles %>%
  filter(section == 114881) %>%
  filter(month == 10) %>%
  filter(year == 2019)

articles_sample_copy <- articles_sample


# Timeseries --------------------------------------------------------------

articles %>% 
  count(year, month) %>%
  ggplot +
  geom_col(aes(make_date(year, month, 1), n), fill = "darkblue", color = "darkblue") +
  scale_x_date(date_breaks = "1 months", date_labels = "%m.%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(plot.background = element_rect(fill = "white"), 
        panel.background = element_rect(fill = "lightblue"), 
        panel.grid = element_line(color = "white"), 
        axis.text = element_text(color = "darkblue", size = 10),
        axis.title = element_text(color = "white")) 
  


# Top 10 słów dla lead + wordcloud----------------------------------------------

lead_words <- articles_sample %>% unnest_tokens(word, lead, to_lower = TRUE, token = "words")
counts <- lead_words %>% count(word, sort = TRUE)

pl_stop_words <- readLines("polish_stopwords.txt")
pl_stop_words <- str_split(pl_stop_words, ', ', simplify = T)
pl_stop_words <- drop(pl_stop_words)
stopwords_pl <- tibble(word = pl_stop_words)

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

articles_sample[1, 5]

articles_sample$lead<- enc2utf8(articles_sample$lead) 

articles_sample$lead <- as.character(articles_sample$lead) %>%
  tryTolower() %>% 
  {gsub("[^[:alnum:][:blank:]!'*,-./:;?`]", "",.)} %>%
  {gsub("u2019", "'",.)} %>%
  {gsub("\\n", " ", .)} %>%
  {gsub("[?!]+",".", .)} %>%
  {gsub("[0-9]"," ", .)} %>%
  {gsub("-"," ", .)} %>%
  {gsub("\"+"," ", .)} %>%
  {gsub("\\.+","\\. ", .)} %>%
  {gsub(" +"," ", .)} %>%
  {gsub("\\. \\.","\\. ", .)}

articles_sample[1, 5]

j <- 1
for (j in 1:nrow(articles_sample)) {
  temp_review <- anti_join(articles_sample[j,] %>% 
                             unnest_tokens(word, lead, drop=FALSE, to_lower=FALSE, token = "words"), stopwords_pl)
  
  stemmed_review <- wordStem(temp_review[,"word"], language = "porter")
  
  articles_sample[j,"lead"] <- paste((stemmed_review), collapse = " ")
  }

cleaned_words <- articles_sample %>% unnest_tokens(word, lead, to_lower=FALSE, token = "words") 

cleaned_counts <- cleaned_words %>% count(word, sort=TRUE)

cleaned_counts %>% 
  mutate(word = reorder(word,n)) %>% 
  top_n(10, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() + 
  labs(x = NULL, y = "Liczba wystapien") + 
  coord_flip() + 
  theme(text = element_text(size = 17)) + 
  ggtitle("Histogram")

wordcount_final <- articles_sample %>%
  unnest_tokens(word, lead, to_lower=FALSE, token = "words") %>%
  count(word, sort=TRUE)

wordcloud(words = wordcount_final$word, freq = wordcount_final$n,
          max.words=30, random.order=FALSE, 
          colors = brewer.pal(8, "Dark2"))


# Top 10 słów dla body ----------------------------------------------------

body_words <- articles_sample %>% unnest_tokens(word, body, to_lower = TRUE, token = "words")
counts_body <- body_words %>% count(word, sort = TRUE)

articles_sample[1, 6]

articles_sample$body<- enc2utf8(articles_sample$body) 

articles_sample$body <- as.character(articles_sample$body) %>%
  tryTolower() %>% 
  {gsub("[^[:alnum:][:blank:]!'*,-./:;?`]", "",.)} %>%
  {gsub("u2019", "'",.)} %>%
  {gsub("\\n", " ", .)} %>%
  {gsub("[?!]+",".", .)} %>%
  {gsub("[0-9]"," ", .)} %>%
  {gsub("-"," ", .)} %>%
  {gsub("\"+"," ", .)} %>%
  {gsub("\\.+","\\. ", .)} %>%
  {gsub(" +"," ", .)} %>%
  {gsub("\\. \\.","\\. ", .)}

articles_sample[1, 6]

j <- 1
for (j in 1:nrow(articles_sample)) {
  temp_review <- anti_join(articles_sample[j,] %>% 
                             unnest_tokens(word, body, drop=FALSE, to_lower=FALSE, token = "words"), stopwords_pl)
  
  stemmed_review <- wordStem(temp_review[,"word"], language = "porter")
  
  articles_sample[j,"body"] <- paste((stemmed_review), collapse = " ")
}

cleaned_words_body <- articles_sample %>% unnest_tokens(word, body, to_lower=FALSE, token = "words") 

cleaned_counts_body <- cleaned_words_body %>% count(word, sort=TRUE)

cleaned_counts_body %>% 
  mutate(word = reorder(word,n)) %>% 
  top_n(10, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() + 
  labs(x = NULL, y = "Liczba wystapien") + 
  coord_flip() + 
  theme(text = element_text(size = 17)) + 
  ggtitle("Histogram")


# Analiza sentymentu ------------------------------------------------------



