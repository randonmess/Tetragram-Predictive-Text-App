library(readr); library(dplyr); library(tidytext);
library(stringr); library(tidyr); library(tm); library(data.table)

blog <- read_lines('./data/final/en_US/en_US.blogs.txt') %>%
        tibble()
twitter <- read_lines('./data/final/en_US/en_US.twitter.txt') %>%
        tibble()
news <- read_lines('./data/final/en_US/en_US.news.txt') %>%
        tibble()
set.seed(2636)
blog <- slice_sample(blog, prop = 0.02)
news <- slice_sample(news, prop = 0.02)
twitter <- slice_sample(twitter, prop = 0.02)
dataset <- bind_rows(blog, news, twitter)
rm(blog,news,twitter)
names(dataset) <- 'text'
dataset <- dataset %>%
        mutate(doc_id = 1:nrow(dataset)) %>%
        mutate(doc_id = as.character(doc_id)) %>%
        select(doc_id, text)
corpus <- VCorpus(DataframeSource(dataset))
rm(dataset)

corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, "latin1", "ASCII", sub="")))
corpus <- tm_map(corpus, content_transformer(tolower))

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
corpus <- tm_map(corpus, toSpace, "-")
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "\\(")
corpus <- tm_map(corpus, toSpace, "\\)")

corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)

dataset <- tidy(corpus)[8]
rm(toSpace, corpus)
gc()

unigrams <- unnest_tokens(dataset, word, text) %>%
        count(word1, sort = T)

bigrams <- dataset %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        filter(!is.na(bigram)) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        count(word1, word2, sort = TRUE)
        
trigrams <- dataset %>%
        unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
        filter(!is.na(trigram)) %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        count(word1, word2, word3, sort = TRUE)

quadgrams <- dataset %>%
        unnest_tokens(quadgram, text, token = "ngrams", n = 4) %>%
        filter(!is.na(quadgram)) %>%
        separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
        count(word1, word2, word3, word4, sort = TRUE)

fwrite(unigrams, "./data/unigram_df.txt")
fwrite(bigrams, "./data/bigram_df.txt")
fwrite(trigrams, "./data/trigram_df.txt")
fwrite(quadgrams, "./data/quadgram_df.txt")

unigrams <- fread("./data/unigram_df.txt", data.table = FALSE)
bigrams <- fread("./data/bigram_df.txt", data.table = FALSE)
trigrams <- fread("./data/trigram_df.txt", data.table = FALSE)
quadgrams <- fread("./data/quadgram_df.txt", data.table = FALSE)

unigram_predict <- function() {
        c("There were no matches. These are the top 10 most frequent words in the corpus:", unlist(unigrams[1:10, 1]))
}

bigram_predict <- function(words, words_length) {
        predictor <- data.frame(word1 = words[words_length])
        prediction <- filter(bigrams, word1 == predictor$word1)
        if (nrow(prediction) == 0) {
                unigram_predict()
        }
        else {
                prediction[1,2]
        }
}

trigram_predict <- function(words, words_length) {
        predictor <- data.frame(word1 = words[words_length-1],
                                word2 = words[words_length])
        prediction <- filter(trigrams, word1 == predictor$word1,
                             word2 == predictor$word2)
        if (nrow(prediction) == 0) {
                bigram_predict(words, words_length)
        }
        else {
                prediction[1,3]
        }
}

quadgram_predict <- function(words, words_length) {
        predictor <- data.frame(word1 = words[words_length-2],
                                word2 = words[words_length-1],
                                word3 = words[words_length])
        prediction <- filter(quadgrams, word1 == predictor$word1,
                             word2 == predictor$word2,
                             word3 == predictor$word3)
        if (nrow(prediction) == 0) {
                trigram_predict(words, words_length)
        }
        else {
                prediction[1,4]
        }
}

cleantext <- function (text) {
text <- str_to_lower(text)
text <-  gsub("-", ' ', text)
text <- gsub("/", ' ', text)
text <- gsub("'", '', text)
text <- gsub('[[:punct:] ]+', ' ', text)
text <- gsub("\\s+", " ", text)
}

predictword <- function(text) {

words <- unlist(str_split(text," "))
words_length <- length(words)


if (words_length >= 3) {
        quadgram_predict(words, words_length)
} else if (words_length == 2) {
        trigam_predict(words, words_length)
} else {
        bigram_predict(words, words_length)
}}
