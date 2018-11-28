library(dplyr)
library(tidytext)
library(SnowballC)

setwd("C:/Users/raymo/OneDrive/Desktop")
data = read.csv("job_skill_short.csv",stringsAsFactors =F)
data = data[-101,]  #delete Japanese char

resp <- data_frame(line = 1:1226, text = data$Responsibilities, doc = data$Category)
mqual <- data_frame(line = 1:1226, text = data$Minimum.Qualifications, doc = data$Category)
pqual <- data_frame(line = 1:1226, text = data$Preferred.Qualifications, doc = data$Category)
all <- do.call("rbind",list(resp,mqual,pqual))

# # responsibilities
# resp.words <- resp %>%
#   unnest_tokens(word, text) %>%
#   count(doc, word, sort = TRUE) %>%
#   ungroup()
# resp.total <- resp.words %>%
#   group_by(doc) %>%
#   summarise(tot = sum(n))
# resp.words <- left_join(resp.words, resp.total)
# data(stop_words)
# resp.words <- resp.words %>%
#   anti_join(stop_words)
# resp.words <- resp.words %>%
#   bind_tf_idf(word, doc, n)
# resp.words %>%
#   select(-tot) %>%
#   arrange(desc(tf_idf))
# 
# # minimum
# mqual.words <- mqual %>%
#   unnest_tokens(word, text) %>%
#   count(doc, word, sort = TRUE) %>%
#   ungroup()
# mqual.total <- mqual.words %>%
#   group_by(doc) %>%
#   summarise(tot = sum(n))
# mqual.words <- left_join(mqual.words, mqual.total)
# data(stop_words)
# mqual.words <- mqual.words %>%
#   anti_join(stop_words)
# mqual.words <- mqual.words %>%
#   bind_tf_idf(word, doc, n)
# mqual.words %>%
#   select(-tot) %>%
#   arrange(desc(tf_idf))
# 
# # preferred
# pqual.words <- pqual %>%
#   unnest_tokens(word, text) %>%
#   count(doc, word, sort = TRUE) %>%
#   ungroup()
# pqual.total <- pqual.words %>%
#   group_by(doc) %>%
#   summarise(tot = sum(n))
# pqual.words <- left_join(pqual.words, pqual.total)
# data(stop_words)
# pqual.words <- pqual.words %>%
#   anti_join(stop_words)
# pqual.words <- pqual.words %>%
#   bind_tf_idf(word, doc, n)
# pqual.words %>%
#   select(-tot) %>%
#   arrange(desc(tf_idf))

# all
data(stop_words)
all.words <- all %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>%
  count(doc, word, sort = TRUE) %>%
  ungroup()
all.total <- all.words %>%
  group_by(doc) %>%
  summarise(tot = sum(n))
all.words <- left_join(all.words, all.total)
all.words <- all.words %>%
  bind_tf_idf(word, doc, n)
all.feature <- all.words %>%
  select(-tot) %>%
  arrange(desc(tf_idf))

n.feat = 1000  # number of top features in all 3 (with duplicates)
all.flist=unique(all.feature$word[1:n.feat]) #get the unique set

all.data = data 
all.data[all.flist] <- NA
all.data$Company <- NULL
all.data$Location <- NULL
all.data$Title <- NULL
all.data$Responsibilities <- NULL
all.data$Minimum.Qualifications = NULL
all.data$Preferred.Qualifications = NULL
all.data$X = NULL

for (i in 1:1226) {
  rdf = 0
  mdf = 0
  pdf = 0
  
  if(data[i,"Responsibilities"] != ""){
    rdf <- data_frame(text = data[i,"Responsibilities"]) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      mutate(word = wordStem(word))
  }
  if(data[i,"Minimum.Qualifications"] != ""){
    mdf <- data_frame(text = data[i,"Minimum.Qualifications"]) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      mutate(word = wordStem(word))
  }
  if(data[i,"Preferred.Qualifications"] != ""){
    pdf <- data_frame(text = data[i,"Preferred.Qualifications"]) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>%
      mutate(word = wordStem(word))
  }
  for (feat in all.flist) {
    all.data[i,feat] = sum(as.integer(rdf==feat)) + sum(as.integer(mdf==feat)) + sum(as.integer(pdf==feat))
  }
}

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)

write.csv(all.data, "train.csv")
  








