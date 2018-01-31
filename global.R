library(dplyr)
library(plotly)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(leaflet)
library(data.table)
abc <- read.csv("abc.csv", header = T, sep = ",")
bbc <- read.csv("bbc.csv", header = T, sep = ",")
# cbs <- read.csv("cbs.csv", header = T, sep = ",")
cnn <- read.csv("cnn.csv", header = T, sep = ",")
fox <- read.csv("fox.csv", header = T, sep = ",")
huf <- read.csv("huf.csv", header = T, sep = ",")
# lat <- read.csv("lat.csv", header = T, sep = ",")
nbc <- read.csv("nbc.csv", header = T, sep = ",")
# npr <- read.csv("npr.csv", header = T, sep = ",")
# nyt <- read.csv("nyt.csv", header = T, sep = ",")

###ref###
#sum(allmedia$likes_count)
sum(abc$likes_count)
sum(bbc$likes_count)


abc = subset(
  abc, select = -c(X1, page_id, description, status_type, comments_count, picture))

bbc = subset(
  bbc, select = -c(X1, page_id, description, status_type, comments_count, picture))

cnn = subset(
  cnn, select = -c(X1, page_id, description, status_type, comments_count, picture))

fox = subset(
  fox, select = -c(X1, page_id, description, status_type, comments_count, picture))

huf = subset(
  huf, select = -c(X1, page_id, description, status_type, comments_count, picture))

nbc = subset(
  nbc, select = -c(X1, page_id, description, status_type, comments_count, picture))
# abc[complete.cases(abc),]

abc <- abc %>%
  arrange(desc(likes_count)) %>%
  slice(1:1000)
bbc <- bbc %>%
  arrange(desc(likes_count)) %>%
  slice(1:1000)
cnn <- cnn %>%
  arrange(desc(likes_count)) %>%
  slice(1:1000)
fox <- fox %>%
  arrange(desc(likes_count)) %>%
  slice(1:1000)
huf <- huf %>%
  arrange(desc(likes_count)) %>%
  slice(1:1000)
nbc <- nbc %>%
  arrange(desc(likes_count)) %>%
  slice(1:1000)

allmedia <- rbind(abc, bbc, cnn, fox, huf, nbc)
summary(allmedia)
write.csv(allmedia, file="allmedia.csv", row.names = FALSE)
allmedia2 <- allmedia %>%
  select(allmedia, Title = name)

wcmedia <- allmedia2 %>%
  select(starts_with("message"))

write.csv(wcmedia, file="wcmedia.csv", row.names = FALSE)
summary(wcmedia)
  
          

allmedia2 <- allmedia %>%
  select(-starts_with("?..id"), -starts_with("posted_at")) %>%
  group_by(media) %>%
  top_n(50, likes_count) %>%
  select(year, media, everything())

   
nbc_byyear <- nbc %>%
  group_by(year) %>%
  summarise(nbc2012 = sum(nbc$likes_count, year=="2012")) %>%
  summarise(nbc2014 = sum(nbc$likes_count, year=="2014"))

summarise_each(nbcyear=nbc$likes_count, funs(sum))

nbc_byyear <- nbc%>%
  group_by(year) %>%
  summarise_each(funs(sum, mean))




nbc%>%
  group_by(year) %>%
  summarise(nbc_likes = sum(likes_count)) 
nbc_byyear


summary(nbc$year)

nbc%>%
  group_by(year) %>%
  summarise(nbc_year = sum(likes_count))

allmedia %>%
  group_by(media) %>%
  summarise_each(funs(mean, sum), likes_count, shares_count, love_count, sad_count, angry_count)

by_med <- allmedia %>%
  group_by(media) %>%
  summarise(sum_media = sum(likes_count, na.rm=TRUE))
bymedia
#######################################################
#### Group_by media for mean and sum of likes_cout#####
#### variable: likesbymedia for select tab ############

groupbymedia <- group_by(allmedia, media)
likesbymedia <- summarise(groupbymedia,
                          AVG_Likes = mean(likes_count, na.rm=TRUE),
                          Total_Likes = sum(likes_count, na.rm=TRUE))

ggplot(likesbymedia, aes(x = media, y = AVG_Likes, fill = media)) +
  geom_col(position = position_dodge()) 
#######################################################
#### LINE PLOT of TOTAL LIKES by YEAR, with MEDIA #####
#### variable: likesbyyear for first page #############

likesbyyear <- allmedia %>%
  group_by(year, media) %>%
  summarise(Total_Likes = sum(likes_count))
ggplot(likesbyyear, aes(x = year, y = Total_Likes)) +geom_line(size=1, aes(color=media)) 




#######################################################
#### SUMMARY OF EACH MEDIA WITH 6LIKES FOR X-AXIS###### 

###ABC
abc_summary <- allmedia %>%
  select(year, media, shares_count, likes_count, love_count, wow_count, sad_count, angry_count) %>%
  filter(media == "ABC") %>%
  group_by(year) %>%
  replace(is.na(.), 0)

ggplot(abc_summary, aes(x=year, y=likes_count)) + 
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=likes_count), color="white", size=3.5) +
  theme_minimal()

###BBC###
bbc_summary <- allmedia %>%
  select(year, media, shares_count, likes_count, love_count, wow_count, sad_count, angry_count) %>%
  filter(media == "BBC") %>%
  group_by(year) %>%
  replace(is.na(.), 0)

ggplot(bbc_summary, aes(x=year, y=likes_count)) + 
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=likes_count), vjust=1.6, color="white", size=3.5) +
  theme_minimal()

###CNN###
cnn_summary <- allmedia %>%
  select(year, media, shares_count, likes_count, love_count, wow_count, sad_count, angry_count) %>%
  filter(media == "CNN") %>%
  group_by(year) %>%
  replace(is.na(.), 0)

ggplot(cnn_summary, aes(x=year, y=likes_count)) + 
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=likes_count), vjust=1.6, color="white", size=3.5) +
  theme_minimal()

###FOX###
fox_summary <- allmedia %>%
  select(year, media, shares_count, likes_count, love_count, wow_count, sad_count, angry_count) %>%
  filter(media == "FOX") %>%
  group_by(year) %>%
  replace(is.na(.), 0)

ggplot(fox_summary, aes(x=year, y=likes_count)) + 
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=likes_count), vjust=1.6, color="white", size=3.5) +
  theme_minimal()

###HUF###
huf_summary <- allmedia %>%
  select(year, media, shares_count, likes_count, love_count, wow_count, sad_count, angry_count) %>%
  filter(media == "Huffington") %>%
  group_by(year) %>%
  replace(is.na(.), 0)

ggplot(huf_summary, aes(x=year, y=likes_count)) + 
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=likes_count), vjust=1.6, color="white", size=3.5) +
  theme_minimal()

###NBC###
nbc_summary <- allmedia %>%
  select(year, media, shares_count, likes_count, love_count, wow_count, sad_count, angry_count) %>%
  filter(media == "NBC") %>%
  group_by(year) %>%
  replace(is.na(.), 0)

ggplot(nbc_summary, aes(x=year, y=likes_count)) + 
  geom_bar(stat="identity", fill="steelblue") +
  geom_text(aes(label=likes_count), vjust=1.6, color="white", size=3.5) +
  theme_minimal()

########################################################################
#####WORD CLOUD#######
#install.packages("SnowballC")
#install.packages("RColorBrewer")
library(SnowballC) 
library(RColorBrewer)
library(wordcloud)
#install.packages("wordcloud")
library(wordcloud2)
# install.packages("tm")
library(tm)

wcdata <- readLines("allmedia_text.txt")

wc_corpus <- Corpus(VectorSource(wcdata))
wc_corpus_Clean <- tm_map(wc_corpus, tolower)
wc_corpus_Clean <- tm_map(wc_corpus_Clean, removeNumbers)
wc_corpus_Clean <- tm_map(wc_corpus_Clean, removeWords, stopwords())
wc_corpus_Clean <- tm_map(wc_corpus_Clean, removeWords, c("http", "com", "facebook", "www", "photo", "abc", "bbc", "cnn", "fox", "huffington", "nbc", "timeline", "video", "videostype", "foxnews"))
wc_corpus_Clean <- tm_map(wc_corpus_Clean, stemDocument)
wordcloud(wc_corpus_Clean, min.freq=5, colors=brewer.pal(6, "Set2"), random.order = F, rot.per = .30)
 

###########################################################
groupbyyear <- group_by(abc, year)
abc_likesbyyear <- summarise(groupbyyear,
                             abc_avg_likes = mean(likes_count, na.rm=TRUE),
                             abc_sum_likes = sum(likes_count, na.rm=TRUE))
##############################################################
allmedia %>%
  group_by(year) %>%
  summarise(all_likes = sum(likes_count))
ggplot(allmedia, aes(x=year, y=likes_count))+geom_line(size=1, aes(color=media))
  
  
ggplot(abc_likesbyyear, aes(x = year, y = abc_sum_likes, color=media)) +
  geom_line(size=1) 

#######################################################

