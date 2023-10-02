
library(shiny)
library(data.table); library(dplyr); library(stringr);

unigrams <- fread("./data/unigram_df.txt", data.table = FALSE)
bigrams <- fread("./data/bigram_df.txt", data.table = FALSE)
trigrams <- fread("./data/trigram_df.txt", data.table = FALSE)
quadgrams <- fread("./data/quadgram_df.txt", data.table = FALSE)

cleantext <- function (text) {
        text <- str_to_lower(text)
        text <-  gsub("-", ' ', text)
        text <- gsub("/", ' ', text)
        text <- gsub("'", '', text)
        text <- gsub('[[:punct:] ]+', ' ', text)
        text <- gsub("\\s+", " ", text)
        text
}

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

predictword <- function(text) {
        
        words <- unlist(str_split(text," "))
        words_length <- length(words)
        
        
        if (words_length >= 3) {
                quadgram_predict(words, words_length)
        } else if (words_length == 2) {
                trigram_predict(words, words_length)
        } else {
                bigram_predict(words, words_length)
        }}

function(input, output, session) {
        
        output$predicted_next_word <- renderText({
                
                validate(need(input$user_input != '', 'Please begin typing above...'))
                
                text <- cleantext(input$user_input)
                
                predictword(text)
            
            })
}
