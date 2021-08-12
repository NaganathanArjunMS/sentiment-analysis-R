# Import tidytext package present in 'sentiments' dataset
library(tidytext)
sentiments


# Retreive the lexicons using get_sentiments()
get_sentiments("bing")


# Import libraries janeaustenr, stringr, tidytext, dplyr
library(janeaustenr)
library(stringr)
library(tidytext)
library(dplyr)


# unnest_tokens() - Convert the book text to tidy format
tidy_data <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
  chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


# Using the lexicon 'bing'
# books used - Sense, Sensibility
# This function helps to observe positive words 
positive_senti <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

tidy_data %>%
  filter(book == "Emma") %>%
  semi_join(positive_senti) %>%
  count(word, sort = TRUE)


# spread() - To segregate data 
# mutate() - To calculate the total sentiment (positive - negative sentiment)
library(tidyr)
bing <- get_sentiments("bing")
Emma_sentiment <- tidy_data %>%
  inner_join(bing) %>%
  count(book = "Emma" , index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


# Visualization of words in the Book EMMA
library(ggplot2)

ggplot(Emma_sentiment, aes(index, sentiment, fill = book)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")


# Count of most positive and negative words in the novel
counting_words <- tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)
head(counting_words)


# Visualization of sentiment score
counting_words %>%
  filter(n > 150) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  coord_flip() +
  labs(y = "Sentiment Score")

# Visualization to create a word cloud - comparision.cloud()
library(reshape2)
library(wordcloud)
tidy_data %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"), max.words = 100)