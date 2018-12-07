library(dplyr)
library(tidytext)
library(SnowballC)

setwd("C:/Users/raymo/OneDrive/Desktop")
data = read.csv("job_skill_short.csv",stringsAsFactors =F)
data = data[-101,]  #delete Japanese char

#put all text into only frame
resp <- data_frame(line = 1:1226, text = data$Responsibilities, doc = data$Category)         
mqual <- data_frame(line = 1:1226, text = data$Minimum.Qualifications, doc = data$Category)
pqual <- data_frame(line = 1:1226, text = data$Preferred.Qualifications, doc = data$Category)
all <- do.call("rbind",list(resp,mqual,pqual))

# tokenize all words, remove the stop words, stem the words
data(stop_words)
all.words <- all %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word)) %>%
  count(doc, word, sort = TRUE) %>%
  ungroup()
# count frequencies
all.total <- all.words %>%
  group_by(doc) %>%
  summarise(tot = sum(n))
all.words <- left_join(all.words, all.total)
# calculate tf
all.words <- all.words %>%
  bind_tf_idf(word, doc, n)
all.feature <- all.words %>%
  select(-tot) %>%
  arrange(desc(tf_idf))

n.feat = 500  # number of top features in all 3 (with duplicates)
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

r.data = all.data
p.data = all.data
m.data = all.data

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
    r.data[i,feat] = sum(as.integer(rdf==feat))
    p.data[i,feat] = sum(as.integer(pdf==feat))
    m.data[i,feat] = sum(as.integer(mdf==feat))
  }
}

colnames(r.data)= 0:425
colnames(q.data)= 425:850
colnames(m.data)= 850:1275

all.data = cbind(r.data,p.data[,2:426],m.data[,2:426])


write.csv(all.data, "train.csv")
