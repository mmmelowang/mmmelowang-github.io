---
title: "Yelp data analysis"
author: "Mengting Wang, Zhengze Hou, Naijia Wu"
date: "2022/4/29"
output:
  html_document:
    code_folding: "hide"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r warning=FALSE}

setwd("E:/2021«Ô’–/BaiduNetdiskWorkspace/∏Á¥Ûqmss/DV/Group_T_digital-products/data")
bar <- read.csv("bar.csv")
barreview <- read.csv("barreview.csv")
bus_attr <- read.csv("business_allatt.csv")
user <- read.csv("10%eliteuser.csv")
```


```{r message=FALSE, warning=FALSE}
#library(tidyverse) 
library(stringr)
library(wordcloud) 
library(tidytext) 
library(DT) 
library(dplyr)
library(leaflet) 
```

Part 1: Map Visualization for Vegas Bars

```{r message=FALSE, warning=FALSE}
library(readr)
library(ggplot2)
library(ggthemes)
library(maps)
library(dplyr)
library("DT")
library(stringr)
library("leaflet")
library(RColorBrewer)
library("data.table")
```

```{r message=FALSE, warning=FALSE}
library(ggmap)
register_google(key = "AIzaSyBlZS1dMHmz5qAZww5VQRBBSlJa3VFfyVg", write = TRUE)
```

Las Vegas Map

```{r message=FALSE, warning=FALSE}
map_lv <- get_map("Las Vegas",
                            zoom = 12,
                            source = "stamen",
                            maptype = "toner-background")
ggmap(map_lv)
```


Below is the map that shows bar location
 
```{r message=FALSE, warning=FALSE}
g_location <- ggmap(map_lv) + theme_map()
g_location + geom_point(data=bar, aes(x=longitude,y=latitude),
                    size=0.3, alpha=0.3, color="blue")
```

Highlighting the bars in hot spot
```{r message=FALSE, warning=FALSE}
g_density <- g_location + geom_density2d(aes(x=longitude,y=latitude), 
  data=bar, color="green", size=1, bins=12) +
  stat_density2d(aes(x=longitude,y=latitude,
    fill = ..level.., alpha = ..level..),
    data=bar, geom = 'polygon', bins=12) +
  scale_fill_gradient2(low = "green", mid="yellow", high = "red") +
  scale_alpha(range = c(0.00, 0.5)) 
g_density
```


Visualize the neighborhood each bar belongs to:
```{r message=FALSE, warning=FALSE}
#add legend of stars
lvbar_map_neighborhood <- 
  leaflet(bar) %>%
  addTiles() %>%    # Add OpenStreetMap map tiles
  addCircles(lng = ~longitude, lat = ~latitude)

pal = colorFactor("Set1", domain = bar$neighborhood) # Grab a palette
color_neighborhood = pal(bar$neighborhood)

lvbar_map_neighborhood %>% addCircles(color=color_neighborhood) %>%
  addLegend(pal = pal, values = ~bar$neighborhood, title = "Neighborhood")

```
```{r message=FALSE, warning=FALSE}
content <- paste("Name:",bar$name,"<br/>",
                 "Address:",bar$address,"<br/>",
                 "Stars:",bar$stars,"<br/>",
                 "Neighborhood:", bar$neighborhood,"<br/>")

pal = colorFactor("YlOrRd", domain = bar$stars) # Grab a palette
color_stars = pal(bar$stars)

lvbar_map_neighborhood %>% addCircles(color=color_stars, popup = content) %>%
  addLegend(pal = pal, values = ~bar$stars, title = "Stars")


```

Part2: text analysis

Most Popular Categories regarding bars:
```{r message=FALSE, warning=FALSE}
fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

categories = str_split(bar$categories,";")
categories = as.data.frame(unlist(categories))
colnames(categories) = c("Name")

categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  head(10) %>%
  
  
  ggplot(aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of Category', y = 'Count', 
       title = 'Top 10 Categories regarding bars') +
  coord_flip() + 
  theme_bw()

```

Bars with most number of five Star Reviews:
```{r message=FALSE, warning=FALSE}
stars_5 <- barreview %>%
  filter(stars ==5) %>%
  group_by(business_id) %>%
  select(business_id,stars,text) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() 


five = merge(stars_5, bar, by= "business_id")
five2 <- five %>%
  filter(stars ==5) %>%
  filter(is_open==1)

fivestar <- five2 %>%
  arrange(stars) %>%
  head(10)
```

```{r message=FALSE, warning=FALSE}

fillColor2 = "#F1C40F"

fivestar %>%
  mutate(name = reorder(name,Count)) %>%
  ggplot(aes(x = name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 2, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of the Bars', 
       y = 'Count', 
       title = 'Name of the bars and Count') +
  coord_flip() +
  theme_bw()
```

Most 5 starred bar - J Karaoke Bar:
```{r message=FALSE, warning=FALSE}
J_karaoke = bar %>% filter(business_id == "3pSUr_cdrphurO6m1HMP9A") %>%
  select(name,neighborhood,city,state,postal_code,categories)

datatable(head(J_karaoke), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```

A wordcloud to see the common words of reviews on "J Karaoke Bar"
```{r message=FALSE, warning=FALSE}
createWordCloud = function(train)
{
  train %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  count(word,sort = TRUE) %>%
  ungroup()  %>%
  head(30) %>%
  
  with(wordcloud(word, n, max.words = 30,colors=brewer.pal(8, "Dark2")))
}

createWordCloud(barreview %>%
  filter(business_id == "3pSUr_cdrphurO6m1HMP9A"))
```

From the wordcloud, we can derive the ingisht that people praise the atmosphere, music, cleaning environment, services and food(especially chicken) in this bar, and indicates that they spend happy and comfortable time in this J Karaoke bar.


Similarly, there are the bars with most number of one star reviews:
```{r message=FALSE, warning=FALSE}
#library()
stars_1 <- barreview %>%
  filter(stars ==1|stars==1.5) %>%
  group_by(business_id) %>%
  select(business_id,stars,text) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() 


one = merge(stars_1, bar, by= "business_id")
one2 <- one %>%
  filter(stars ==1|stars==1.5) %>%
  filter(is_open==1)

onestar <- one2 %>%
  arrange(stars) %>%
  head(10)

fillColor2 = "#F1C40F"

onestar %>%
  mutate(name = reorder(name,Count)) %>%
  ggplot(aes(x = name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 2, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of the Bars', 
       y = 'Count', 
       title = 'Name of the bars and Count') +
  coord_flip() +
  theme_bw()
```


Surprisingly, the bar named "Triumph property management" only has one star rating, and there are 12 reviews on that bar.

So we are interested to see the common words of reviews on "Triumph property management":
```{r message=FALSE, warning=FALSE}
createWordCloud = function(train)
{
  train %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  count(word,sort = TRUE) %>%
  ungroup()  %>%
  head(30) %>%
  
  with(wordcloud(word, n, max.words = 30,colors=brewer.pal(8, "Dark2")))
}

createWordCloud(barreview %>%
  filter(business_id == "Zh6fxrqsKqdSVmTK3roxBQ"))
```

People in their reviews complain about the house/environment of the bar.

Let's create a datatable to see some information regarding "Triumph property management":
```{r message=FALSE, warning=FALSE}
Triumph = bar %>% filter(business_id == "Zh6fxrqsKqdSVmTK3roxBQ") %>%
  select(name,neighborhood,city,state,postal_code,categories)

datatable(head(Triumph), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
```


```{r message=FALSE, warning=FALSE}
goodbar <- barreview %>%
  filter(stars == 5) %>%
  group_by(business_id) %>%
  ungroup()

star_five <- merge(goodbar,bar,by = "business_id")

badbar <- barreview %>%
  filter(stars == 1) %>%
  group_by(business_id) %>%
  ungroup()

star_one <- merge(badbar,bar,by = "business_id")
```



```{r message=FALSE, warning=FALSE}
library(tm)
df_five = data.frame(doc_id = star_five$business_id, text = star_five$text,stringsAsFactors = F)
star_five2 <- DataframeSource(df_five)
star_five2 <- VCorpus(star_five2)

df_one = data.frame(doc_id = star_one$business_id, text = star_one$text,stringsAsFactors = F)
star_one2 <- DataframeSource(df_one)
star_one2 <- VCorpus(star_one2)
```


```{r message=FALSE, warning=FALSE}
#Remove unnecessary words(stop words), synatx, punctuation, numbers, white space etc.
library(stringr)
remove_nonalphanum <- function(x){str_replace_all(x, "[^[:alnum:]]", " ")}
remove_brandnames <- function(x){str_replace_all(x, "\\b[A-Z]+\\b", " ")}

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(remove_nonalphanum))
  corpus <- tm_map(corpus, content_transformer(remove_brandnames))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}
```


```{r message=FALSE, warning=FALSE}
#cleaning two datasets
star_five_clean <- clean_corpus(star_five2)
star_one_clean <- clean_corpus(star_one2)
```

```{r message=FALSE, warning=FALSE}
#create a document-term-matrix:
library(tm)
#create the dtm from the corpus
corpus_five_dtm <- DocumentTermMatrix(star_five_clean)
corpus_one_dtm <- DocumentTermMatrix(star_one_clean)

```

```{r message=FALSE, warning=FALSE, include=FALSE}
#provide a word cloud of the most frequent words for "five_star" bars and "one_star" bars
library(tidytext)
corpus_five_dt <- tidy(corpus_five_dtm)
corpus_one_dt <- tidy(corpus_one_dtm) 
head(corpus_five_dt)
```

```{r message=FALSE, warning=FALSE}
#tf-idf
corpus_five_tdidf <- corpus_five_dt %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

corpus_one_tdidf <- corpus_one_dt %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

head(corpus_five_tdidf)
```

```{r message=FALSE, warning=FALSE}
term_frequency_DT_five <- corpus_five_tdidf
term_frequency_DT_one <- corpus_one_tdidf

library(wordcloud)
```

```{r message=FALSE, warning=FALSE}
#Set seed
set.seed(2103)

#create a wordcloud to show the frequent words of five stars bars
wordcloud(term_frequency_DT_five$term, term_frequency_DT_five$tf,
          max.words=50, colors=brewer.pal(8, "Dark2"))
```

```{r message=FALSE, warning=FALSE}
#create a wordcloud to show the frequent words of one stars bars
wordcloud(term_frequency_DT_one$term, term_frequency_DT_one$tf,
          max.words=50, colors=brewer.pal(8, "Dark2"))
```

Here is a pyramid plot to show how the words between five-stars and one-stars bars differ in word frequency:
```{r message=FALSE, warning=FALSE}
#combine corpus of the most successful and unsuccessful projects
#select top 20 words
corpus_five_dt$bestworst <- "Top"
corpus_one_dt$bestworst <- "Bottom"
corpus_top_bottom_dt <- rbind(corpus_five_dt,corpus_one_dt)


corpus_top_bottom_count <- corpus_top_bottom_dt %>%
  group_by(term) %>%
  summarize(total_word = sum(count)) %>%
  arrange(desc(total_word)) %>%
  head(20)
```

```{r message=FALSE, warning=FALSE}
pyramid = left_join(corpus_top_bottom_dt, corpus_top_bottom_count, by='term')

pyramid <- pyramid %>%
  filter(!is.na(total_word)) %>%
  group_by(bestworst) %>%
  mutate(count_plot = ifelse(bestworst == 'Bottom', count*(-1), count))

```

```{r message=FALSE, warning=FALSE}
ggplot(pyramid, aes(x = reorder(term, total_word),
                  y = count_plot, fill = bestworst)) +
  geom_bar(data = filter(pyramid, bestworst == "Top"), stat = "identity") +
  geom_bar(data = filter(pyramid, bestworst == "Bottom"), stat = "identity") +
  scale_fill_brewer(palette = "Set1", direction=-1) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-50,50,25)) +
  scale_fill_discrete(name = 'bars star rating', labels=c('one star', 'five star')) +
  ylab("") +
  ggthemes::theme_tufte() + 
  labs(
    x = 'Top 20 Words',
    y= 'Count',
    title = 'Pyramid Plot of Top 20 Words, for one star bars and five star bars'
  )
```

Sentiment analysis of reviews:

Positive v.s. negative words in the reivews of J Karaoke Bar:

```{r message=FALSE, warning=FALSE}
library(tidytext)
library(tidyverse)

positiveWordsBarGraph <- function(SC) {
  contributions <- SC %>%
    unnest_tokens(word, text) %>%
    count(word,sort = TRUE) %>%
    ungroup() %>%
    
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(value))

  
  contributions %>%
    top_n(20, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    head(20) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() + theme_bw()
}

positiveWordsBarGraph(barreview %>%
                       filter(business_id == "3pSUr_cdrphurO6m1HMP9A"))
```



```{r message=FALSE, warning=FALSE, include=FALSE}
#calculate sentiment for "J Karaoke Bar"
J_Karaoke_reviews = star_five %>%
  filter(business_id == "3pSUr_cdrphurO6m1HMP9A")

calculate_sentiment <- function(review)
{
  sentiment_lines  =  review %>%
                  unnest_tokens(word, text) %>%
                  inner_join(get_sentiments("afinn"), by = "word") %>%
                  group_by(user_id) %>%
                  summarize(sentiment = mean(value),words = n()) %>%
                  ungroup() %>%
                  filter(words >= 10) 

  return(sentiment_lines)
  
}


sentiment_lines = calculate_sentiment(J_Karaoke_reviews)

head(sentiment_lines)
```

Display top 10 most positive reviews for 5 star bars:

```{r message=FALSE, warning=FALSE}
display_pos_sentiments <- function(sentiment_lines,review_text)
{
  pos_sentiment_lines = sentiment_lines %>%
  arrange(desc(sentiment))  %>%
  top_n(10, sentiment) %>%
  inner_join(review_text, by = "user_id") %>%
  select(date,sentiment,text) 
  
datatable(pos_sentiment_lines, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

}

display_pos_sentiments(sentiment_lines,J_Karaoke_reviews)
```

Positive v.s. negative words in the reivews of Triumph property management:

```{r message=FALSE, warning=FALSE}
positiveWordsBarGraph <- function(SC) {
  contributions <- SC %>%
    unnest_tokens(word, text) %>%
    count(word,sort = TRUE) %>%
    ungroup() %>%
    
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(value))

  
  contributions %>%
    top_n(20, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    head(20) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() + theme_bw()
}

positiveWordsBarGraph(barreview %>%
                       filter(business_id == "Zh6fxrqsKqdSVmTK3roxBQ"))
```

```{r message=FALSE, warning=FALSE}
Triumph_reviews = barreview %>%
  filter(business_id == "Zh6fxrqsKqdSVmTK3roxBQ")

calculate_sentiment <- function(review)
{
  sentiment_lines  =  review %>%
                  unnest_tokens(word, text) %>%
                  inner_join(get_sentiments("afinn"), by = "word") %>%
                  group_by(user_id) %>%
                  summarize(sentiment = mean(value),words = n()) %>%
                  ungroup() %>%
                  filter(words >= 10) 

  return(sentiment_lines)
  
}


sentiment_lines = calculate_sentiment(Triumph_reviews)
```

Display top 10 most negative reviews for Triumph property management:
```{r message=FALSE, warning=FALSE}
display_neg_sentiments <- function(sentiment_lines,review_text)
{
  neg_sentiment_lines = sentiment_lines %>%
  arrange(desc(sentiment))  %>%
  top_n(-10, sentiment) %>%
  inner_join(review_text, by = "user_id") %>%
  select(date,sentiment,text) 
  
datatable(neg_sentiment_lines, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
}

display_neg_sentiments(sentiment_lines,Triumph_reviews)
```



Visualize the geographical location of the top 10 five star bars(blue dot) and bottom 1 star or 1.5 star bars(yellow dot):
```{r message=FALSE, warning=FALSE, include=FALSE}
library(devtools)
devtools::install_github("rstudio/leaflet")
```

```{r message=FALSE, warning=FALSE}

LasvegasCoords = bar %>% filter(city == "Las Vegas")
center_lon = median(LasvegasCoords$longitude,na.rm = TRUE)
center_lat = median(LasvegasCoords$latitude,na.rm = TRUE)

map <- leaflet(rbind(fivestar,onestar)) %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude,radius = ~sqrt(review_count))  %>%
  addCircleMarkers(data=fivestar,col="blue",group="fivestar") %>%
  addCircleMarkers(data=onestar,color='yellow',group="onestar") %>%
    #Layers control
  addLayersControl(overlayGroups = c("fivestar","onestar"),
                   options = layersControlOptions(collapsed = FALSE)
                   ) %>%
  # controls
  setView(lng=center_lon, lat=center_lat,zoom = 13)
```
```{r}
map
```

Top five stars bars are more centralized and concentrated; while the worst one star bars are relatively more sparse in their location, and are not close to transportation hub.

Part 3: Social network analysis & regression models:

There are mainly three parts in this analysis. p1, p8 are network graphs, p2-p5 are descriptive analysis graphs, p6-p7 are correlation graphs. 
```{r message=FALSE, warning=FALSE}

library(readr)
library(networkD3)
library(visNetwork)
library(ggplot2)
library(plotly)
library(sjPlot)
```


```{r message=FALSE, warning=FALSE}
## Every user has reviewed one of the bars in Vegas

name <- user[, c("user_id", "friends")]

## 1. Network graph of top elite users(whose fans >= 100 and useful value >=  900)
net <- separate_rows(name, friends, sep = ",", convert = TRUE)
## No outside friends now
net <- net %>%
  filter(friends %in% user_id)
## Delete users with no inside followers

net <- rename(net, source = user_id)
net <- rename(net, target = friends)

netnode <- user %>%
  filter(user_id %in% net$source)


net <- as.data.frame(net)
p1 <- simpleNetwork(net, 
              nodeColour = "blue", 
              zoom=T,
              fontSize = 16)


p1
```
 There is no obvious network between the elite users°£
 
 
 Among the elite users who have reviewed any of bars in Vegas, what are their relationship? Is there some inside network between these celebrities in Yelp? Or do they just review and travel separately? To figure it out, I find those elite users who also has at least one elite user friend and construct a network between them. As a result, most of the links are individual and not connected to another links. There exists A-B-C chains, which means A is B's friend, and B is C's friend, but no classical social network "circle" or group. On the other word, A is not C's friend. Thus, as Yelp is not a social media, but a review app, elite users don't interact with each other in the app. It is more possible that every elite user is the center of his/her fans, not the center of his/her peers.




 While there is no group among the elite(influential) users, the other problem is whether this kind of group exists among the bars in Vegas. We assume that when a consumer goes to bar A, he/she will be more accessible to bar B in the same group because of recommendation or similarity. If there is a large portion of consumers go to both Bar A and Bar B, these two bars will be more possible to be in the same group compared to Bar C outside. Thus, we check all the reviews of top50 bar in Vegas beased on review_count. Then we extract the reviewers' id of every bar. After that, we define at least more than 50 common reviewers between two bars can be seen as a link. Finally, a network of bars is constructed. The links' sizes mean the number of common consumers. The nodes' sizes mean the number of reviews. While the colours of nodes mean different neighborhood in Vegas.


 In this graph, we can find that there are so many links between these bars. Basicaaly, all the top bars are clustered and not far away from each other. Of course, if you are a consumer seeking a different or similar bar compared to the previous one, you can just use this graph to find your preferred next bar. As an owner of a bar, you can also find your friend bars(or actually competitors). 


 Furthermore, it is obvious that the locaion doesn't separate the bars in different groups. Perhaps Vegas is not a big place, so the distance doesn't matter.  


```{r message=FALSE, warning=FALSE}
## 8. Append: Network of bars
rb <- barreview %>%
  group_by(business_id) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(50)
r50 <- barreview %>%
   filter(business_id %in% rb$business_id) %>%
   select(user_id, business_id)

## One bar have a branch in very close location also listed in top50, so actually the number of bars is 49

r50 <- left_join(r50, bar, by = "business_id")
r50 <- r50[, c("user_id", "name", "neighborhood", "review_count")]
r50$name<-gsub('["]',"", r50$name)
r50$name <- ifelse(r50$name == "Bachi Burger" & r50$neighborhood == "Southeast",  "Bachi Burger(SE)", r50$name)
r50 <- mutate(r50, barnum = as.factor(name))
barlink <- NULL
 for (i in 1 : 48) {
   for (j in i : 48) {
      a <- r50 %>% filter(as.numeric(barnum) == i)
      a <- a$user_id
      b <- r50 %>% filter(as.numeric(barnum) == j)
      b <- b$user_id
      c <- intersect(a, b)
      d <- length(c)
      tmp <- c(i, j, d)
      barlink <- rbind(barlink, tmp)
   }
}
barlink <- as.data.frame(barlink)
barlink <- barlink %>% 
  filter(V1 != V2) %>% 
  filter(V3 > 0)
barlink <- rename(barlink, source = V1, target = V2, value = V3)
barlink2 <- barlink %>% filter(value >= 50)
barlink2$value <- barlink2$value / 50
barlink2$source = barlink2$source - 1
barlink2$target = barlink2$target - 1  
barnode <- r50[, c("barnum", "neighborhood", "review_count")] %>% 
     distinct()
barnode$neighborhood <- as.factor(barnode$neighborhood)
barnode$rcsize <- barnode$review_count / 100 - 10
p8 <- forceNetwork(Links = barlink2, 
             Nodes = barnode, 
             Source = "source",
             Target = "target", 
             Value = "value", 
             NodeID = "barnum",
             Nodesize = "rcsize",
             Group = "neighborhood", 
             opacity = 0.6, zoom = TRUE)
p8
```

 If you would like to know some basic information of bars in Vegas. Here are the tables. The most popular 10 bars are listed below for your recommendation. Do you want to go to Lotus of Siam or Bachi Burger?

 This picture tells you the distribution of bars in Vegas' neighborhood. The strip is leading, and Eastside and Downtown are also great. However, if we take the average review scores into account, maybe Downtown is the best area for entertainment.
 
```{r message=FALSE, warning=FALSE}
neighborhood <- bar %>%
  group_by(neighborhood) %>%
  summarize(avgstar = mean(stars), count = n())
neighborhood[1, 1] <- "Not known"
neighborhoodgraph <- ggplot(neighborhood, aes(x = count, y = reorder(neighborhood, count), fill = round(avgstar, 2))) + 
  geom_bar(stat="identity", width=1, color="white") +
  labs(x="review count", y="name of neighborhood")

## 3. Which neighborhood has most bars?
p3 <- ggplotly(neighborhoodgraph)
p3
```

This table tells you if we define the bars who have in average more than 4 stars and greater than 100 reviews are the top bars, how many of all the bars in Vegas are the top bars. The result is that only 240 bars are top, and the rest 1139 are normal. The gap of average review counts in the two types of bars is huge. Even in Vegas, good bars are rare. But their popularity and quality are undoubtful.

```{r message=FALSE, warning=FALSE}
topstar <- mutate(bar, topstar = ifelse(stars >= 4 & review_count >= 100, "Top bars", "Non top bars"))
toppct <- topstar %>%
  group_by(topstar) %>%
  summarize(count = n(), avgreview = mean(review_count))

## 4. The gap between topbars and non top bars
p4 <- tab_df(toppct)
p4
```

 In the distribution of top bars, we find Spring Valley and China town are great areas. They are easy to be ignored if we just concentrate to the location crowded with bars.
```{r message=FALSE, warning=FALSE}
topstar <- topstar %>%
  filter(topstar == "Top bars") %>%
  group_by(neighborhood) %>%
  summarize(count = n(), avgstar = mean(stars))
topstar[1, 1] <- "Not known"
topbargraph <- ggplot(topstar, aes(x = count, y = reorder(neighborhood, count), fill = round(avgstar, 2))) + 
  geom_bar(stat="identity", width=1, color="white") +
  labs(x="review count", y="name of neighborhood")

## 5. Which neighborhood has most top bars?
p5 <- ggplotly(topbargraph)
p5
```

Then let us dig deeper to discover the relationship among the attributes of elite users who has reviewed in Vegas' bars. It seems that you will be more picky when you experience more places. But you may also be more conservative to criticize when your number of fans increases. The first linear regression graph shows this trend.
 
```{r echo=FALSE, message=FALSE, warning=FALSE}
## 6. What influences the elite users' review scores most who has reviewed in Las Vegas.
lm1 <- lm(average_stars ~ review_count + fans + useful + compliment_hot, user)
summary(lm1)
userstars <- ggplot(data = user, aes(x = fans, y = average_stars)) +
  stat_smooth(method = "lm", col = "blue") + 
  xlab("Number of fans") + ylab("Average stars")
p6 <- ggplotly(userstars)
p6
```

 It is said that some popular bars or restaurants may not worth a high review score. But at least this linear regression proves that bars in Vegas with more reviews tend to have higher reputation. They are worthy of their names.


 Does the amount of review have a positive correlation with the bars' reputation in Las Vegas.
```{r message=FALSE, warning=FALSE}

lm2 <- lm(stars ~ review_count, bar, na.action = na.omit)
summary(lm2)
barstars <- ggplot(data = bar, aes(x = review_count, y = stars)) +
  stat_smooth(method = "lm", col = "blue") + 
  xlab("Number of reviews") + ylab("Average stars")
p7 <- ggplotly(barstars)
p7
```