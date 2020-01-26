#server.R
#Author: Kadiresan Dhanasekaran
#Date: 22-Jan-2020
#Description: Final Capstone Project - John Shiny Server, Coursera Data Science Capstone Final Project

library(shiny)
library(dplyr)
library(stringi)
library(stringr)
library(quanteda)


# load the bigram, trigram & quadgram model data
#load("~/NgramTop1PredictModel.RData") ## Load the variable with model data - qcorpus.tokenized.data.ngrams.top1
#ngramPredictionData <- readRDS(file = "/Users/Keerthi/Downloads/ngramPrediction.rds") ## Load the variable with model data - qcorpus.tokenized.data.ngrams.top1

#load(file = "/Users/Keerthi/Downloads/ngramPrediction.rds") ## Load the variable with model data - qcorpus.tokenized.data.ngrams.top1
#load("~/Downloads/profanity.RData") ## Variable profanity

load(file = "ngramPrediction.rds") ## Load the variable with model data - qcorpus.tokenized.data.ngrams.top1
load("profanity.RData") ## Variable profanity

ngramUsedToPredict <- reactiveVal(value = "", label = "")

randomPredictIndex <- 1

predictNextWord <- function(inputText) {
    inputText <- as.list(inputText)

    if (length(inputText) == 0 )
    {
        randomPredictIndex <- sample(1:nrow(qcorpus.tokenized.data.ngrams.top1), 1) 
        ngramUsedToPredict("Not in dictionary")
        return("No Prediction")
    }
    else
    {
        ngramText <-  str_c(inputText, collapse = " ")
        ngramUsedToPredict(ngramText)
        predictedWord <- qcorpus.tokenized.data.ngrams.top1 %>%
                         filter(type == ngramText) 

        if(nrow(predictedWord) >= 1)
        {
            if (nrow(predictedWord) == 1) {
                return(predictedWord$predict)
            }
            else {
                return(predictedWord[-1]$predict) 
            }
            
        }
        else
        {
            return(predictNextWord(inputText[-1]))
        }
    }
}

parseInput <- function(input) {
    
    #do the same that we did for cleansing the model input data such as  
    # remove_punct = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE, remove_separators = TRUE, remove_twitter = TRUE, remove_url = TRUE
    # remove profanity
    # convert to lower case
    
    if (input == "" | is.na(input)) {
        return("")
    }
    
    if (input == "" | is.na(input)) {
        return("")
    }
    
    input <- corpus(toString(input)) %>%  #Convert the text data to corpus for processing
        tokens(., what = "word", remove_numbers = TRUE, 
               remove_punct = TRUE, remove_symbols = TRUE,
               remove_hyphens = TRUE, remove_separators = TRUE,
               remove_twitter = TRUE, remove_url = TRUE) %>%   #Tokenize
        tokens_tolower(.) %>% #Convert to lower
        tokens_select(., pattern = profanity, selection = 'remove') %>%  #Remove profanity words
        tokens_select(., pattern = '[^\x01-\x7F]+', selection ='remove', valuetype = "regex") #Remove foreign characters
    
    return(tail(input[[1]],3))
    
}

shinyServer(function(input, output) {
    # reactive controls
    
    observe({

        cleanInputText <- parseInput(input$inputText)

        #What is the actual pharse that was used to predict the next word
        output$ngramToPredict <- renderText({
            ngramUsedToPredict()                     # rv$value
        })
        
        #What is the actual next word predicted
        output$nextWord <- reactive({
            f <- as.list(predictNextWord(cleanInputText))
            #print(ngramUsedToPredict)
            f[1]
        })

        #What is the actual input you typed. Full message
        output$rawInput <- renderText({input$inputText})

        #What is the cleansed input after removing profanity words. Full message
        output$cleansedInput <- renderText(do.call(paste, c(as.list(cleanInputText), sep = " ")))


    })
    
})

