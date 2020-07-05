library(readxl)
library(sentimentr)
library(janitor)
library(tidyverse)

hospital <- read_excel("Desktop/NHS/hospital.xls", skip = 5) %>% janitor::clean_names()
hospital

nhs = hospital %>% select(org_name, liked, disliked, advice, comment_title)
nhs$comment_title = as.character(nhs$comment_title)

comment_sent = sentimentr::extract_sentiment_terms(nhs$comment_title)
liked_emotion = sentimentr::extract_emotion_terms(nhs$liked)

disliked_emotion = sentimentr::extract_profanity_terms(nhs$disliked)

library(tm)


corpus = VCorpus(VectorSource(nhs$comment_title))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

liked_tdm = TermDocumentMatrix(corpus,
                               control = list(removePunctuation = TRUE,
                                              stopwords = TRUE))
tdm <- removeSparseTerms(liked_tdm, sparse = 0.975)
tdm_m <- as.matrix(tdm)
liked_dist <- dist(tdm_m)

hc <- hclust(liked_dist)
plot(hc)
 


staff_liked = liked %>% str_detect("staff")
