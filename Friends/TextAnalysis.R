library(friends)
library(tidyverse)
library(tidytext)
library(textstem)
library(tm)
library(qdapDictionaries)

#A function that will attempt to label each term as a name or a word
is.name  <- function(x) toupper(x) %in% NAMES$name

#Load the dataframe
script = friends::friends
#See who speaks the most
script %>% group_by(speaker) %>% summarize(N=n()) %>% arrange(desc(N))

#Experiment with removing directions
#script = script %>% filter(speaker != "Scene Directions")

#Separate words into multiple columns
words_wide = script %>% separate(text, into=paste0("Word", 1:200), sep=" ")
#Make each word it's own row
words_long=pivot_longer(words_wide, cols=starts_with("Word"), names_to="Number", values_to="Word", values_drop_na = T)

#Remove punctuation, lowercase, and lemmatize each word
lemmas<-words_long %>% mutate(Lemma = lemmatize_words(tolower(removePunctuation(Word))))

#Calculate TFIDF using the episode as the document
tfidf=lemmas %>% mutate(Document = paste(season, episode)) %>% group_by(Document) %>%
  group_by(Document, Lemma) %>% summarize(N=n()) %>% filter(Lemma!="") %>%
  bind_tf_idf(term=Lemma, Document, N)

#Find words with high tf_idf, that are used at least 8 times, and are not too common or uncommon in the series as a whole
words = tfidf %>% arrange(desc(tf_idf)) %>% filter(N>=8, idf<5, idf>.5, !is.name(Lemma)) %>% slice(1)  %>%
  separate(Document, into=c("season", "episode"), " ", convert = T) %>%
  full_join(friends::friends_info) 

#Find names with high tf_idf, that are used at least 5 times, and are not too common or uncommon in the series as a whole
names = tfidf %>% arrange(desc(tf_idf)) %>% filter(N>=5, idf>.7, is.name(Lemma)) %>% slice(1)  %>%
  separate(Document, into=c("season", "episode"), " ", convert = T) %>%
  full_join(friends::friends_info) 

#Join and format result
result =words %>% select(season, episode, Word=Lemma, title) %>%
  full_join(names%>%select(season, episode, Name=Lemma, title)) %>%
  mutate(`My Title` = str_to_title(paste0("The one about ", Word, " with ", Name)), .keep="unused") %>%
  arrange(season, episode)

View(result)
result %>% write_csv("Suggested_Names.csv")

##To do: Come up with better heuristics for choosing the name and word so NA never is used
##Figure out the de-lemmatized version of the word used in the episode and its part of speech to construct a more coherent sentence
##Latent-Dirichlet Topic Modeling
