library(pryr)
library(ggplot2)
library(quanteda)
quanteda_options(threads = 24)
quanteda_options("threads")
sessionInfo()

library(lexicon)
library(dplyr)
library(tidytext)
library(purrr)

suppressWarnings()


# 1. Read the blog file contents
file.name <- "en_US.blogs.txt"
con <- file(file.name, "r") 
text <- readLines(con, encoding = "UTF-8")
close(con)

# 2. Read the twitter file contents
file.name <- "en_US.twitter.txt"
con <- file(file.name, "r") 
text <- c(text, rbind(text,readLines(con, encoding = "UTF-8")))
#####text2 <- readLines(con, encoding = "UTF-8")
close(con)

# 3. Read the news file contents
file.name <- "en_US.news.txt"
con <- file(file.name, "r") 
text <- c(text, rbind(text,readLines(con, encoding = "UTF-8")))
close(con)

#For saving memory. garbage collection
rm(file.name)
rm(con)

# 4. Name the rows so we can refer them later if required
#######row.names(text) <- c('blogs','twitter','news')

# 5. Create a master list of all profanity words
# We need to use this to cleanse the master text data that we have at hand
profanity <- tolower(profanity_alvarez)
profanity <- list(profanity, tolower(profanity_arr_bad))
profanity <- list(profanity, tolower(profanity_banned))
profanity <- list(profanity, tolower(profanity_racist))
######profanity <- list(profanity, tolower(profanity_zac_anger))
profanity <- unique(unlist(profanity))

# 6. Get the full corpus of the data containing text from Blogs, Twitter &  News feed files
# Remove all the numbers, punctuations, symbols, hypens, separators, twitter handles, any web urls - These are not necessary for predicting the next word.
# Note: I have not removed the stopwords as they are very much needed for predicting the next word scenario. 

start_time <- Sys.time()
start_time
qcorpus.tokenized.data <- corpus(text) %>%  #Convert the text data to corpus for processing
                          tokens(., what = "word", remove_numbers = TRUE, 
                                remove_punct = TRUE, remove_symbols = TRUE,
                                remove_hyphens = TRUE, remove_separators = TRUE,
                                remove_twitter = TRUE, remove_url = TRUE) %>%   #Tokenize
                          tokens_tolower(.) %>% #Convert to lower
                          tokens_select(., pattern = profanity, selection = 'remove') %>%  #Remove profanity words
                          tokens_select(., pattern = '[^\x01-\x7F]+', selection ='remove', valuetype = "regex") #Remove foreign characters
end_time <- Sys.time()
end_time
end_time - start_time

save(qcorpus.tokenized.data, file="PredictTokens.Rdata")

#names(qcorpus.tokenized.data2) <- gsub(x = names(qcorpus.tokenized.data2), pattern = "text", replacement = "b")  


#For saving memory. garbage collection
rm(text)
rm(profanity)  
  
# 7. Objective: We are planning to get atleast bigrams, trigrams & quadgrams for the given data
# The approach is going to be similar in all of these.
# First identify the ngrams = 2 or 3 or 4
# Create frequency of ngrams
# Split the ngrams into ngrams-1, unigram. Meaning if you are looking at trigram, it will be split into bigram + unigram
# Find the unigram that is occurring max following the ngram-1 occurrence. 
# Keep that and discard others.
# Build the list of ngrams like for all ngrams that has atleast 5 occurrence - This is strictly done for performance reasons as my laptop is not able to process more. 
#       Ideally I will like to keep all occurrences as some of those unique occurrences are pretty good in predicting as thats the only combination the data comes 99%
# Save the ngrams list onto a file


# 7a. Build Bigram model
-------------------------------------------------------------------------------------------------------------------------------------------------

library(tidyr)

qcorpus.length <- length(qcorpus.tokenized.data)
i <- 1
increment <- 1000000 
j <- increment
k <- 1

qcorpus.tokenized.data.bigrams.total <- data.frame(freq = numeric(),type = character(),predict = character())

while(TRUE) {
      
      print(
          paste(
              paste(
                  paste('Corpus Tokens processed for index' , toString(i))
                  ,' to ') 
              , toString(j)
              )
      )
  
      if(i > qcorpus.length ){
        break
      }
    
      if(j > qcorpus.length) {
        j <- qcorpus.length
      }

  
      tok <- qcorpus.tokenized.data[i:j]
  
      start_time <- Sys.time()
      start_time
      qcorpus.tokenized.data.bigrams <- tokens_ngrams(tok, n = 2, concatenator = "~")
      end_time <- Sys.time()
      end_time
      end_time - start_time
      #pryr::mem_used()
      
      start_time <- Sys.time()
      start_time
      qcorpus.tokenized.data.bigrams <- dfm(qcorpus.tokenized.data.bigrams)
      end_time <- Sys.time()
      end_time
      end_time - start_time
      #pryr::mem_used()
      
      start_time <- Sys.time()
      start_time
      qcorpus.tokenized.data.bigrams <- colSums(qcorpus.tokenized.data.bigrams)
      end_time <- Sys.time()
      end_time
      end_time - start_time
      #pryr::mem_used()
      
      start_time <- Sys.time()
      start_time
      qcorpus.tokenized.data.bigrams <- as.data.frame( qcorpus.tokenized.data.bigrams )
      end_time <- Sys.time()
      end_time
      end_time - start_time
      #pryr::mem_used()
      
      start_time <- Sys.time()
      start_time
      qcorpus.tokenized.data.bigrams$ngrams <- rownames(qcorpus.tokenized.data.bigrams)
      end_time <- Sys.time()
      end_time
      end_time - start_time
      #pryr::mem_used()
      
      start_time <- Sys.time()
      start_time
      names(qcorpus.tokenized.data.bigrams) <- c("freq","ngrams")
      end_time <- Sys.time()
      end_time
      end_time - start_time
      #pryr::mem_used()
      
      start_time <- Sys.time()
      start_time
      qcorpus.tokenized.data.bigrams$ngrams <- rownames(qcorpus.tokenized.data.bigrams)
      end_time <- Sys.time()
      end_time
      end_time - start_time
      #pryr::mem_used()
      
      #start_time <- Sys.time()
      #start_time
      #qcorpus.tokenized.data.bigrams[which(qcorpus.tokenized.data.bigrams$freq >= 2),]
      #end_time <- Sys.time()
      #end_time
      #end_time - start_time
      #pryr::mem_used()
      
      start_time <- Sys.time()
      start_time
      qcorpus.tokenized.data.bigrams <- separate(qcorpus.tokenized.data.bigrams,ngrams, c("word1", "word2"), sep = "~", remove=TRUE)
      end_time <- Sys.time()
      end_time
      end_time - start_time
      #pryr::mem_used()
      
      start_time <- Sys.time()
      start_time
      names(qcorpus.tokenized.data.bigrams) <- c("freq","type","predict")    #Rename the columns for clarity
      end_time <- Sys.time()
      end_time
      end_time - start_time
      #pryr::mem_used()
      
      start_time <- Sys.time()
      start_time
      rownames(qcorpus.tokenized.data.bigrams) <- NULL   #Rename the columns for clarity
      end_time <- Sys.time()
      end_time
      end_time - start_time
      #pryr::mem_used()
      
      qcorpus.tokenized.data.bigrams.total  <- rbind(qcorpus.tokenized.data.bigrams.total, qcorpus.tokenized.data.bigrams)
      
      #qcorpus.tokenized.data.bigrams.total1 <- aggregate( freq ~ type + predict ,qcorpus.tokenized.data.bigrams.total, FUN = sum)
      
      #library(plyr)
      #groupColumns <- c("type","predict")
      #dataColumns <- c("freq")
      #res <- ddply(qcorpus.tokenized.data.bigrams.total, groupColumns, function(x) colSums(x[dataColumns]))
      

      
      i <- i + increment
      j <- j + increment

}  

library(data.table)
start_time <- Sys.time()
start_time
qcorpus.tokenized.data.bigrams.total <- setDT(qcorpus.tokenized.data.bigrams.total)[,.(freq = sum(freq)), by = 'type,predict']
end_time <- Sys.time()
end_time
end_time - start_time

save(qcorpus.tokenized.data.bigrams.total,file="BiPredictModel.Rdata")

qcorpus.tokenized.data.bigrams.max <- aggregate( freq ~ type , qcorpus.tokenized.data.bigrams.total, max) %>%  #Find the max rows for each word1 based on frequency
  merge(. , qcorpus.tokenized.data.bigrams.total)  #Merge those with the original table

qcorpus.tokenized.data.bigrams.total <- arrange(qcorpus.tokenized.data.bigrams.total,qcorpus.tokenized.data.bigrams.total$type,-qcorpus.tokenized.data.bigrams.total$freq  )
qcorpus.tokenized.data.bigrams.total <- setDT(qcorpus.tokenized.data.bigrams.total)[, id := rowid(type)]
qcorpus.tokenized.data.bigrams.top5 <- qcorpus.tokenized.data.bigrams.total[which(qcorpus.tokenized.data.bigrams.total$id <= 5 & qcorpus.tokenized.data.bigrams.total$freq >= 5),]
qcorpus.tokenized.data.bigrams.top3 <- qcorpus.tokenized.data.bigrams.total[which(qcorpus.tokenized.data.bigrams.total$id <= 3 & qcorpus.tokenized.data.bigrams.total$freq >= 5),]
qcorpus.tokenized.data.bigrams.top1 <- qcorpus.tokenized.data.bigrams.total[which(qcorpus.tokenized.data.bigrams.total$id <= 1 & qcorpus.tokenized.data.bigrams.total$freq >= 5),]



# 7b. Build Trigram model
-------------------------------------------------------------------------------------------------------------------------------------------------
  
library(tidyr)

qcorpus.length <- length(qcorpus.tokenized.data)
i <- 1
increment <- 1000000 
j <- increment
k <- 1

qcorpus.tokenized.data.trigrams.total <- data.frame(freq = numeric(),type = character(),predict = character())

while(TRUE) {
  
  print(
    paste(paste(
      paste(
        paste('Corpus Tokens processed for index' , toString(i))
        ,' to ') 
      , toString(j)
    ),paste(" Memory Used: ",toString(pryr::mem_used())))
  )
  
  if(i > qcorpus.length ){
    break
  }
  
  if(j > qcorpus.length) {
    j <- qcorpus.length
  }
  
  
  tok <- qcorpus.tokenized.data[i:j]
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.trigrams <- tokens_ngrams(tok, n = 3, concatenator = "~")
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.trigrams <- dfm(qcorpus.tokenized.data.trigrams)
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.trigrams <- colSums(qcorpus.tokenized.data.trigrams)
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.trigrams <- as.data.frame( qcorpus.tokenized.data.trigrams )
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.trigrams$ngrams <- rownames(qcorpus.tokenized.data.trigrams)
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  names(qcorpus.tokenized.data.trigrams) <- c("freq","ngrams")
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.trigrams$ngrams <- rownames(qcorpus.tokenized.data.trigrams)
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  #start_time <- Sys.time()
  #start_time
  #qcorpus.tokenized.data.bigrams[which(qcorpus.tokenized.data.bigrams$freq >= 2),]
  #end_time <- Sys.time()
  #end_time
  #end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.trigrams <- separate(qcorpus.tokenized.data.trigrams,ngrams, c("word0","word1", "word2"), sep = "~", remove=TRUE) %>%   #Split into n words
    mutate(word1 = paste(word0, word1, sep = " ")) %>% #Concatenate the ngram-1 separately
    subset(., select = -c(word0))
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  names(qcorpus.tokenized.data.trigrams) <- c("freq","type","predict")    #Rename the columns for clarity
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  rownames(qcorpus.tokenized.data.trigrams) <- NULL   #Rename the columns for clarity
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  qcorpus.tokenized.data.trigrams.total  <- rbind(qcorpus.tokenized.data.trigrams.total, qcorpus.tokenized.data.trigrams)
  
  #qcorpus.tokenized.data.bigrams.total1 <- aggregate( freq ~ type + predict ,qcorpus.tokenized.data.bigrams.total, FUN = sum)
  
  #library(plyr)
  #groupColumns <- c("type","predict")
  #dataColumns <- c("freq")
  #res <- ddply(qcorpus.tokenized.data.bigrams.total, groupColumns, function(x) colSums(x[dataColumns]))
  
  
  
  i <- i + increment
  j <- j + increment
  
}  

library(data.table)
start_time <- Sys.time()
start_time
qcorpus.tokenized.data.trigrams.total <- setDT(qcorpus.tokenized.data.trigrams.total)[,.(freq = sum(freq)), by = 'type,predict']
end_time <- Sys.time()
end_time
end_time - start_time

save(qcorpus.tokenized.data.trigrams.total,file="TriPredictModel.Rdata")

qcorpus.tokenized.data.trigrams.max <- aggregate( freq ~ type , qcorpus.tokenized.data.trigrams.total, max) %>%  #Find the max rows for each word1 based on frequency
  merge(. , qcorpus.tokenized.data.trigrams.total)  #Merge those with the original table

qcorpus.tokenized.data.trigrams.total <- setDT(qcorpus.tokenized.data.trigrams.total)
qcorpus.tokenized.data.trigrams.total <- qcorpus.tokenized.data.trigrams.total[order(type,-freq)]
qcorpus.tokenized.data.trigrams.total <- qcorpus.tokenized.data.trigrams.total[, id := rowid(type)]
qcorpus.tokenized.data.trigrams.top5 <- qcorpus.tokenized.data.trigrams.total[which(qcorpus.tokenized.data.trigrams.total$id <= 5 & qcorpus.tokenized.data.trigrams.total$freq >= 5),]
qcorpus.tokenized.data.trigrams.top3 <- qcorpus.tokenized.data.trigrams.total[which(qcorpus.tokenized.data.trigrams.total$id <= 3 & qcorpus.tokenized.data.trigrams.total$freq >= 5),]
qcorpus.tokenized.data.trigrams.top1 <- qcorpus.tokenized.data.trigrams.total[which(qcorpus.tokenized.data.trigrams.total$id <= 1 & qcorpus.tokenized.data.trigrams.total$freq >= 5),]


# 7C. Build Quadgram model
-------------------------------------------------------------------------------------------------------------------------------------------------
  
library(tidyr)

qcorpus.length <- length(qcorpus.tokenized.data)
i <- 1
increment <- 1000000 
j <- increment
k <- 1

qcorpus.tokenized.data.quadgrams.total <- data.frame(freq = numeric(),type = character(),predict = character())

while(TRUE) {
  
  print(
    paste(paste(
      paste(
        paste('Corpus Tokens processed for index' , toString(i))
        ,' to ') 
      , toString(j)
    ),paste(" Memory Used: ",toString(pryr::mem_used())))
  )
  
  
  if(i > qcorpus.length ){
    break
  }
  
  if(j > qcorpus.length) {
    j <- qcorpus.length
  }
  
  
  tok <- qcorpus.tokenized.data[i:j]
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.quadgrams <- tokens_ngrams(tok, n = 4, concatenator = "~")
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.quadgrams <- dfm(qcorpus.tokenized.data.quadgrams)
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.quadgrams <- colSums(qcorpus.tokenized.data.quadgrams)
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.quadgrams <- as.data.frame( qcorpus.tokenized.data.quadgrams )
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.quadgrams$ngrams <- rownames(qcorpus.tokenized.data.quadgrams)
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  names(qcorpus.tokenized.data.quadgrams) <- c("freq","ngrams")
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.quadgrams$ngrams <- rownames(qcorpus.tokenized.data.quadgrams)
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  #start_time <- Sys.time()
  #start_time
  #qcorpus.tokenized.data.bigrams[which(qcorpus.tokenized.data.bigrams$freq >= 2),]
  #end_time <- Sys.time()
  #end_time
  #end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  qcorpus.tokenized.data.quadgrams <- separate(qcorpus.tokenized.data.quadgrams,ngrams, c("word00","word0","word1", "word2"), sep = "~", remove=TRUE) %>%   #Split into n words
    mutate(word1 = paste(paste(word00,word0), word1, sep = " ")) %>% #Concatenate the ngram-1 separately
    subset(., select = -c(word00, word0))
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  names(qcorpus.tokenized.data.quadgrams) <- c("freq","type","predict")    #Rename the columns for clarity
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  start_time <- Sys.time()
  start_time
  rownames(qcorpus.tokenized.data.quadgrams) <- NULL   #Rename the columns for clarity
  end_time <- Sys.time()
  end_time
  end_time - start_time
  #pryr::mem_used()
  
  qcorpus.tokenized.data.quadgrams.total  <- rbind(qcorpus.tokenized.data.quadgrams.total, qcorpus.tokenized.data.quadgrams)
  
  i <- i + increment
  j <- j + increment
  
}  

library(data.table)
start_time <- Sys.time()
start_time
qcorpus.tokenized.data.quadgrams.total <- setDT(qcorpus.tokenized.data.quadgrams.total)[,.(freq = sum(freq)), by = 'type,predict']
end_time <- Sys.time()
end_time
end_time - start_time

save(qcorpus.tokenized.data.quadgrams.total,file="QuadPredictModel.Rdata")

qcorpus.tokenized.data.quadgrams.max <- aggregate( freq ~ type , qcorpus.tokenized.data.quadgrams.total, max) %>%  #Find the max rows for each word1 based on frequency
  merge(. , qcorpus.tokenized.data.quadgrams.total)  #Merge those with the original table

qcorpus.tokenized.data.quadgrams.total <- setDT(qcorpus.tokenized.data.quadgrams.total)
qcorpus.tokenized.data.quadgrams.total <- qcorpus.tokenized.data.quadgrams.total[order(type,-freq)]
qcorpus.tokenized.data.quadgrams.total <- qcorpus.tokenized.data.quadgrams.total[, id := rowid(type)]
qcorpus.tokenized.data.quadgrams.top5 <- qcorpus.tokenized.data.quadgrams.total[which(qcorpus.tokenized.data.quadgrams.total$id <= 5 & qcorpus.tokenized.data.quadgrams.total$freq >= 5),]
qcorpus.tokenized.data.quadgrams.top3 <- qcorpus.tokenized.data.quadgrams.total[which(qcorpus.tokenized.data.quadgrams.total$id <= 3 & qcorpus.tokenized.data.quadgrams.total$freq >= 5),]
qcorpus.tokenized.data.quadgrams.top1 <- qcorpus.tokenized.data.quadgrams.total[which(qcorpus.tokenized.data.quadgrams.total$id <= 1 & qcorpus.tokenized.data.quadgrams.total$freq >= 5),]

qcorpus.tokenized.data.ngrams.top1 <- rbind(qcorpus.tokenized.data.quadgrams.top1,qcorpus.tokenized.data.trigrams.top1,qcorpus.tokenized.data.bigrams.top1)

save(qcorpus.tokenized.data.ngrams.top1,file="ngramPrediction.rds")
-------------------------------------------------------------------------------------------------------------------------------------------------




save(qcorpus.tokenized.data.bigrams.top5,file="BigramTop5PredictModel.Rdata")
save(qcorpus.tokenized.data.bigrams.top3,file="BigramTop3PredictModel.Rdata")
save(qcorpus.tokenized.data.bigrams.top1,file="BigramTop1PredictModel.Rdata")
save(qcorpus.tokenized.data.trigrams.top5,file="TrigramTop5PredictModel.Rdata")
save(qcorpus.tokenized.data.trigrams.top3,file="TrigramTop3PredictModel.Rdata")
save(qcorpus.tokenized.data.trigrams.top1,file="TrigramTop1PredictModel.Rdata")
save(qcorpus.tokenized.data.quadgrams.top5,file="QuadgramTop5PredictModel.Rdata")
save(qcorpus.tokenized.data.quadgrams.top3,file="QuadgramTop3PredictModel.Rdata")
save(qcorpus.tokenized.data.quadgrams.top1,file="QuadgramTop1PredictModel.Rdata")
save(qcorpus.tokenized.data.ngrams.top1,file="NgramTop1PredictModel.Rdata")

save(profanity,file="profanity.Rdata")

save(qcorpus.tokenized.data.bigrams.total,file="BigramTotalPredictModel.Rdata")
save(qcorpus.tokenized.data.trigrams.total,file="TrigramTotalPredictModel.Rdata")
save(qcorpus.tokenized.data.quadgrams.total,file="QuadgramTotalPredictModel.Rdata")
