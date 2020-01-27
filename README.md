# Next Word Predict

Author: Kadiresan Dhanasekaran

Date: Jan 26th 2020

-----

Capstone Project for Data Science Specialization @ Johns Hopkins University

Course Link: https://www.coursera.org/specializations/jhu-data-science

What does the App do?
========================================================
The app has a model built via supervised NLP processing using Swiftkey dataset. <br/>This dataset contains data from twitter feeds, news articles and blog posts.

As the user types in the textbox, the app 
* cleans the input to remove data that doesn't add any value
* tokenizes the input to feed the ngram model
* extracts the last `(n=3)` grams to predict the next word
* recursively finds the next word based on the given `(n)` or `(n-1)` or `(n-2)` gram
* If no match, displays "No Prediction"


Machine Learning Model Algorithm
========================================================

* Corpus tokenized & model built using Quanteda package
* Bigram, Trigram & Quadgram tokens are created out of the corpus
* Take n-1 grams from the above to predict the nth gram
* Create a model data of top nth gram occurrence for the given `(n-1)` grams
* Model created with bi/tri/quad combinations with frequency `>=5` for performance reasons

**Example:**
* Bigram:  "of the" occurs max for the unigram "of"
* Trigram:  "wish you were" occurs max for the bigram "wish you"

Demo url: https://kamkadir17.shinyapps.io/capstonPredictWord/
R Presentation: http://rpubs.com/kamkadir17/jhu_ds_capstone

