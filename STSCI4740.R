#STSCI 4740 project
rm(list=ls())
library(tm)
library(tidyverse)
library(stringr)
setwd("C:/Users/jw2495/Downloads/STSCI4740")
data = read.csv("job_skill_short.csv",stringsAsFactors =F)
#feature extraction
feature_extraction = function(description_vec,stop_words,n=50){
  result=data_frame(text = description_vec) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = str_remove_all(text, '[[:punct:]]')) %>%
  mutate(tokens = str_split(text, "\\s+")) %>%
  unnest() %>% 
  count(tokens) %>% 
  filter(!tokens %in% stop_words) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(desc(n))
  return(result[1:n,1])
}

n=50 #select the most frequent 50 words

#Responsibilities
stop_words_Responsibilities=c("and","to","the","of","with","for","a","in","on","as","work","our","that","their","including","by","across","are","through","other","an","all","be","your")
token_Responsibilities=feature_extraction(data$Responsibilities,stop_words_Responsibilities,n=n)
as.data.frame(token_Responsibilities)

#Minimum.Qualifications
stop_words_Minimum=c("in","or","and","a",'of',"to","with","the","andor","be","an","as","on","at","between","must","for","eg","able")
token_Minimum=feature_extraction(data$Minimum.Qualifications,stop_words_Minimum,n=n)
as.data.frame(token_Minimum)

#Minimum.Qualifications
stop_words_Preferred=c("and","to","in","with","of","a","the","or","as","for","an","on","andor","work")
token_Preferred=feature_extraction(data$Preferred.Qualifications,stop_words_Preferred,n=n)
as.data.frame(token_Preferred)

#detect if certain words exist in a given description
str_detect1=function(description,bag_of_words){
  result = array()
  for (i in 1:nrow(bag_of_words)){
    result[i]=str_detect(description,as.character(bag_of_words[i,1]))
  }
  return(result)
}

#create dummy variables for each description
dummy_var=function(description_vec,token){
  dummy_variable=do.call(rbind,sapply(description_vec,FUN = function(x){str_detect1(x,token)},simplify = F))
  rownames(dummy_variable)=1:length(description_vec)
  colnames(dummy_variable)=as.data.frame(token[,1])[,1]
  return(dummy_variable)
}

var_Responsibilities=dummy_var(data$Responsibilities,token_Responsibilities)
var_Minimum=dummy_var(data$Minimum.Qualifications,token_Minimum)
var_Preferred=dummy_var(data$Preferred.Qualifications,token_Preferred)

data_new=cbind(data$Category,data$Location,data$Title,var_Responsibilities,var_Minimum,var_Preferred)

#variable selection
