# Packages ----------------------------------------------------------------
library(quanteda)
library(tidyverse)
library(quanteda.textplots)
library(quanteda.textstats)
library(RColorBrewer)
library(rtweet)
library(topicmodels)
library(igraph)
library(maps)
library(corpustools)
library(tidyverse)
library(topicmodels)
library(SentimentAnalysis)
library(ggpubr)
library(academictwitteR)
library(qdapDictionaries)


# Retrieving the data for the newspaper analysis, naming them and dividing them into weeks--------------------------
#Headlines FOX has the headlines from FOX News from the 22nd of January 2021 until the 19th of March 2021
#FCHA stands for Fox Climate Headlines All
FCHA = HeadlinesFOX
#Headlines New York Times has the headlines from the New York Times from the 22nd of January 2021 until the 19th of March 2021\
#NYTCHA stands for New York Times Climate Headlines All
NYTCHA=HeadlinesNYT
#Headlines HuffingtonPost has the headlines from the Huffington Post from the 22nd of January 2021 until the 19th of March 2021
#HPCHA stands for Huffinton Post Climate Headlines All
HPCHA=HeadlinesHP

#Dividing FOX News headlines into 8 weeks:
FOX1=rename(FCHA, id=stories_id)
FOX=FOX1%>%select(id,title,publish_date)
week1FOX=FOX%>%filter(id %in% 1830826434:1838009976)
week2FOX=FOX%>%filter(id %in% 1838322548:1844127661)
week3FOX=FOX%>%filter(id %in% 1844514723:1850522271)
week4FOX=FOX%>%filter(id %in% 1850983173:1857728381)
week5FOX=FOX%>%filter(id %in% 1857927918:1864684049)
week6FOX=FOX%>%filter(id %in% 1865488168:1871132287)
week7FOX=FOX%>%filter(id %in% 1873308106:1877660995)
week8FOX=FOX%>%filter(id %in% 1878169059:1884219626)

#Dividing New York Times headlines into 8 weeks:
NYT1=rename(NYTCHA, id=stories_id)
NYT=NYT1%>%select(id,title,publish_date)
week1NYT=NYT%>%filter(id %in% 1830824605:1838060139)
week2NYT=NYT%>%filter(id %in% 1838373520:1843887683)
week3NYT=NYT%>%filter(id %in% 1844490523:1850496589)
week4NYT=NYT%>%filter(id %in% 1851045991:1857713030)
week5NYT=NYT%>%filter(id %in% 1857818962:1864656538)
week6NYT=NYT%>%filter(id %in% 1864960109:1871001403)
week7NYT=NYT%>%filter(id %in% 1871761772:1877488900)
week8NYT=NYT%>%filter(id %in% 1878123559:1883722942)

#Dividing Huffington Post headlines into 8 weeks:
HP1=rename(HPCHA, id=stories_id)
HP=HP1%>%select(id,title,publish_date)
week1HP=HP%>%filter(id %in% 1830788800:1837762147)
week2HP=HP%>%filter(id %in% 1838503784:1844047954)
week3HP=HP%>%filter(id %in% 1845383806:1850487803)
week4HP=HP%>%filter(id %in% 1852632078:1857645047)
week5HP=HP%>%filter(id %in% 1858113040:1864037788)
week6Hp=HP%>%filter(id %in% 1865046719:1871205936)
week7HP=HP%>%filter(id %in% 1871483394:1876635471)
week8HP=HP%>%filter(id %in% 1877995431:1882114657)


# Retreiving the data for the tweets and dividing them into 8 weeks --------------------------------------

#Week 1
Tweetsweek1 <-
  get_all_tweets(
    query = "#climate", 
    start_tweets = "2021-01-22T00:00:00Z",
    end_tweets = "2021-01-29T00:00:00Z",
    file = "climate",
    data_path = "data/",
    n = 1000,
    is_retweet=FALSE)
#Week 2
tweetsWeek2 <-
  get_all_tweets(
    query = "#climate", 
    start_tweets = "2021-01-30T00:00:00Z",
    end_tweets = "2021-02-05T00:00:00Z",
    file = "climate",
    data_path = "data/",
    n = 1000,
    is_retweet=FALSE)
#Week 3
tweetsWeek3 <-
  get_all_tweets(
    query = "#climate", 
    start_tweets = "2021-02-06T00:00:00Z",
    end_tweets = "2021-02-12T00:00:00Z",
    file = "climate",
    data_path = "data/",
    n = 1000,
    is_retweet=FALSE)
#Week 4
tweetsWeek4 <-
  get_all_tweets(
    query = "#climate", 
    start_tweets = "2021-02-13T00:00:00Z",
    end_tweets = "2021-02-19T00:00:00Z",
    file = "climate",
    data_path = "data/",
    n = 1000,
    is_retweet=FALSE)
#Week 5
tweetsWeek5 <-
  get_all_tweets(
    query = "#climate", 
    start_tweets = "2021-02-20T00:00:00Z",
    end_tweets = "2021-02-26T00:00:00Z",
    file = "climate",
    data_path = "data/",
    n = 1000,
    is_retweet=FALSE)
#Week 6
tweetsWeek6 <-
  get_all_tweets(
    query = "#climate", 
    start_tweets = "2021-02-27T00:00:00Z",
    end_tweets = "2021-03-05T00:00:00Z",
    file = "climate",
    data_path = "data/",
    n = 1000,
    is_retweet=FALSE)
#Week 7
tweetsWeek7 <-
  get_all_tweets(
    query = "#climate", 
    start_tweets = "2021-03-06T00:00:00Z",
    end_tweets = "2021-03-12T00:00:00Z",
    file = "climate",
    data_path = "data/",
    n = 1000,
    is_retweet=FALSE)
#Week 8
tweetsWeek8 <-
  get_all_tweets(
    query = "#climate", 
    start_tweets = "2021-03-13T00:00:00Z",
    end_tweets = "2021-03-19T00:00:00Z",
    file = "climate",
    data_path = "data/",
    n = 1000,
    is_retweet=FALSE)


# Preliminary Codes Necessary for Sentiment Analysis ----------------------
?DictionaryGI
names(DictionaryGI)
?weak.words
GI_dict = dictionary(DictionaryGI)
HL_dict = dictionary(list(positive=positive.words, negative=negation.words))

#Sentiment Analysis of News Papers  --------------------------------------
#For FOX News:
corpfox1 = corpus(week1FOX, text_field = 'title')
dtmFOX1 = corpfox1 %>% tokens %>% dfm()
Foxsentimentweek1 = dtmFOX1 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
FSW1=Foxsentimentweek1%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.6266667 
#Positive average:0.6
FSW1=FSW1%>%rename(AverageNegativeFoxWeek1='mean(negative)')
FSW1=FSW1%>%rename(AveragePositiveFoxWeek1='mean(positive)')
FSW1 = FSW1 %>% mutate(sentimentFoxweek1=(AveragePositiveFoxWeek1 - AverageNegativeFoxWeek1) / (AveragePositiveFoxWeek1 + AverageNegativeFoxWeek1))
#Sentiment = -0.02173913

corpfox2 = corpus(week2FOX, text_field = 'title')
dtmFOX2 = corpfox2 %>% tokens %>% dfm()
Foxsentimentweek2 = dtmFOX2 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
FSW2=Foxsentimentweek2%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.75 
#Positive average:0.7115385
FSW2=FSW2%>%rename(AverageNegativeFoxWeek2='mean(negative)')
FSW2=FSW2%>%rename(AveragePositiveFoxWeek2='mean(positive)')
FSW2 = FSW2 %>% mutate(sentimentFoxweek2=(AveragePositiveFoxWeek2 - AverageNegativeFoxWeek2) / (AveragePositiveFoxWeek2 + AverageNegativeFoxWeek2))
#Sentiment = -0.02631579

corpfox3 = corpus(week3FOX, text_field = 'title')
dtmFOX3 = corpfox3 %>% tokens %>% dfm()
Foxsentimentweek3 = dtmFOX3 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
FSW3=Foxsentimentweek3%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.7586207 
#Positive average:0.7241379
FSW3=FSW3%>%rename(AverageNegativeFoxWeek3='mean(negative)')
FSW3=FSW3%>%rename(AveragePositiveFoxWeek3='mean(positive)')
FSW3 = FSW3 %>% mutate(sentimentFoxweek3=(AveragePositiveFoxWeek3 - AverageNegativeFoxWeek3) / (AveragePositiveFoxWeek3 + AverageNegativeFoxWeek3))
#Sentiment = -0.02325581

corpfox4 = corpus(week4FOX, text_field = 'title')
dtmFOX4 = corpfox4 %>% tokens %>% dfm()
Foxsentimentweek4 = dtmFOX4 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
FSW4=Foxsentimentweek4%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.65625
#Positive average:0.6875
FSW4=FSW4%>%rename(AverageNegativeFoxWeek4='mean(negative)')
FSW4=FSW4%>%rename(AveragePositiveFoxWeek4='mean(positive)')
FSW4 = FSW4 %>% mutate(sentimentFoxweek4=(AveragePositiveFoxWeek4 - AverageNegativeFoxWeek4) / (AveragePositiveFoxWeek4 + AverageNegativeFoxWeek4))
#Sentiment = 0.02325581

corpfox5 = corpus(week5FOX, text_field = 'title')
dtmFOX5 = corpfox5 %>% tokens %>% dfm()
Foxsentimentweek5 = dtmFOX5 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
FSW5=Foxsentimentweek5%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.8461538
#Positive average:0.5897436
FSW5=FSW5%>%rename(AverageNegativeFoxWeek5='mean(negative)')
FSW5=FSW5%>%rename(AveragePositiveFoxWeek5='mean(positive)')
FSW5 = FSW5 %>% mutate(sentimentFoxweek5=(AveragePositiveFoxWeek5 - AverageNegativeFoxWeek5) / (AveragePositiveFoxWeek5 + AverageNegativeFoxWeek5))
#Sentiment = -0.1785714

corpfox6 = corpus(week6FOX, text_field = 'title')
dtmFOX6 = corpfox6 %>% tokens %>% dfm()
Foxsentimentweek6 = dtmFOX6 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
FSW6=Foxsentimentweek6%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.4117647
#Positive average:0.4705882
FSW6=FSW6%>%rename(AverageNegativeFoxWeek6='mean(negative)')
FSW6=FSW6%>%rename(AveragePositiveFoxWeek6='mean(positive)')
FSW6 = FSW6 %>% mutate(sentimentFoxweek6=(AveragePositiveFoxWeek6 - AverageNegativeFoxWeek6) / (AveragePositiveFoxWeek6 + AverageNegativeFoxWeek6))
#Sentiment = 0.06666667

corpfox7 = corpus(week7FOX, text_field = 'title')
dtmFOX7 = corpfox7 %>% tokens %>% dfm()
Foxsentimentweek7 = dtmFOX7 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
FSW7=Foxsentimentweek7%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.5757576
#Positive average:0.8484848
FSW7=FSW7%>%rename(AverageNegativeFoxWeek7='mean(negative)')
FSW7=FSW7%>%rename(AveragePositiveFoxWeek7='mean(positive)')
FSW7 = FSW7 %>% mutate(sentimentFoxweek7=(AveragePositiveFoxWeek7 - AverageNegativeFoxWeek7) / (AveragePositiveFoxWeek7 + AverageNegativeFoxWeek7))
#Sentiment = 0.1914894

corpfox8 = corpus(week8FOX, text_field = 'title')
dtmFOX8 = corpfox8 %>% tokens %>% dfm()
Foxsentimentweek8 = dtmFOX8 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
FSW8=Foxsentimentweek8%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.8666667
#Positive average:0.4333333
FSW8=FSW8%>%rename(AverageNegativeFoxWeek8='mean(negative)')
FSW8=FSW8%>%rename(AveragePositiveFoxWeek8='mean(positive)')
FSW8 = FSW8 %>% mutate(sentimentFoxweek8=(AveragePositiveFoxWeek8 - AverageNegativeFoxWeek8) / (AveragePositiveFoxWeek8 + AverageNegativeFoxWeek8))
#Sentiment = -0.3333333

#For the New York Times 
corpNYT1 = corpus(week1NYT, text_field = 'title')
dtmNYT1 = corpNYT1 %>% tokens %>% dfm()
NYTsentimentweek1 = dtmNYT1 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
NYTSW1=NYTsentimentweek1%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.4081633 
#Positive average:0.3673469
NYTSW1=NYTSW1%>%rename(AverageNegativeNewYTWeek1='mean(negative)')
NYTSW1=NYTSW1%>%rename(AveragePositiveNewYTWeek1='mean(positive)')
NYTSW1 = NYTSW1 %>% mutate(sentimentNewYTweek1=(AveragePositiveNewYTWeek1 - AverageNegativeNewYTWeek1) / (AveragePositiveNewYTWeek1 + AverageNegativeNewYTWeek1))
#Sentiment=-0.05263158


corpNYT2 = corpus(week2NYT, text_field = 'title')
dtmNYT2 = corpNYT2 %>% tokens %>% dfm()
NYTsentimentweek2 = dtmNYT2 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
NYTSW2=NYTsentimentweek2%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.6184211 
#Positive average:0.5526316
NYTSW2=NYTSW2%>%rename(AverageNegativeNewYTWeek2='mean(negative)')
NYTSW2=NYTSW2%>%rename(AveragePositiveNewYTWeek2='mean(positive)')
NYTSW2 = NYTSW2 %>% mutate(sentimentNewYTweek2=(AveragePositiveNewYTWeek2 - AverageNegativeNewYTWeek2) / (AveragePositiveNewYTWeek2 + AverageNegativeNewYTWeek2))
#Sentiment=-0.05617978


corpNYT3 = corpus(week3NYT, text_field = 'title')
dtmNYT3 = corpNYT3 %>% tokens %>% dfm()
NYTsentimentweek3 = dtmNYT3 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
NYTSW3=NYTsentimentweek3%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.3518519 
#Positive average:0.3148148
NYTSW3=NYTSW3%>%rename(AverageNegativeNewYTWeek3='mean(negative)')
NYTSW3=NYTSW3%>%rename(AveragePositiveNewYTWeek3='mean(positive)')
NYTSW3 = NYTSW3 %>% mutate(sentimentNewYTweek3=(AveragePositiveNewYTWeek3 - AverageNegativeNewYTWeek3) / (AveragePositiveNewYTWeek3 + AverageNegativeNewYTWeek3))
#Sentiment=-0.05555556


corpNYT4 = corpus(week4NYT, text_field = 'title')
dtmNYT4 = corpNYT4 %>% tokens %>% dfm()
NYTsentimentweek4 = dtmNYT4 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
NYTSW4=NYTsentimentweek4%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.4578313
#Positive average:0.3493976
NYTSW4=NYTSW4%>%rename(AverageNegativeNewYTWeek4='mean(negative)')
NYTSW4=NYTSW4%>%rename(AveragePositiveNewYTWeek4='mean(positive)')
NYTSW4 = NYTSW4 %>% mutate(sentimentNewYTweek4=(AveragePositiveNewYTWeek4 - AverageNegativeNewYTWeek4) / (AveragePositiveNewYTWeek4 + AverageNegativeNewYTWeek4))
#Sentiment=-0.1343284

corpNYT5 = corpus(week5NYT, text_field = 'title')
dtmNYT5 = corpNYT5 %>% tokens %>% dfm()
NYTsentimentweek5 = dtmNYT5 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
NYTSW5=NYTsentimentweek5%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.5416667
#Positive average:0.4583333
NYTSW5=NYTSW5%>%rename(AverageNegativeNewYTWeek5='mean(negative)')
NYTSW5=NYTSW5%>%rename(AveragePositiveNewYTWeek5='mean(positive)')
NYTSW5 = NYTSW5 %>% mutate(sentimentNewYTweek5=(AveragePositiveNewYTWeek5 - AverageNegativeNewYTWeek5) / (AveragePositiveNewYTWeek5 + AverageNegativeNewYTWeek5))
#Sentiment=-0.08333333

corpNYT6 = corpus(week6NYT, text_field = 'title')
dtmNYT6 = corpNYT6 %>% tokens %>% dfm()
NYTsentimentweek6 = dtmNYT6 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
NYTSW6=NYTsentimentweek6%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.3958333
#Positive average:0.3333333
NYTSW6=NYTSW6%>%rename(AverageNegativeNewYTWeek6='mean(negative)')
NYTSW6=NYTSW6%>%rename(AveragePositiveNewYTWeek6='mean(positive)')
NYTSW6 = NYTSW6 %>% mutate(sentimentNewYTweek6=(AveragePositiveNewYTWeek6 - AverageNegativeNewYTWeek6) / (AveragePositiveNewYTWeek6 + AverageNegativeNewYTWeek6))
#Sentiment=-0.08571429

corpNYT7 = corpus(week7NYT, text_field = 'title')
dtmNYT7 = corpNYT7 %>% tokens %>% dfm()
NYTsentimentweek7 = dtmNYT7 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
NYTSW7=NYTsentimentweek7%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.36
#Positive average:0.44
NYTSW7=NYTSW7%>%rename(AverageNegativeNewYTWeek7='mean(negative)')
NYTSW7=NYTSW7%>%rename(AveragePositiveNewYTWeek7='mean(positive)')
NYTSW7 = NYTSW7 %>% mutate(sentimentNewYTweek7=(AveragePositiveNewYTWeek7 - AverageNegativeNewYTWeek7) / (AveragePositiveNewYTWeek7 + AverageNegativeNewYTWeek7))
#Sentiment=0.1

corpNYT8 = corpus(week8NYT, text_field = 'title')
dtmNYT8 = corpNYT8 %>% tokens %>% dfm()
NYTsentimentweek8 = dtmNYT8 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
NYTSW8=NYTsentimentweek8%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.4769231
#Positive average:0.4
NYTSW8=NYTSW8%>%rename(AverageNegativeNewYTWeek8='mean(negative)')
NYTSW8=NYTSW8%>%rename(AveragePositiveNewYTWeek8='mean(positive)')
NYTSW8 = NYTSW8 %>% mutate(sentimentNewYTweek8=(AveragePositiveNewYTWeek8 - AverageNegativeNewYTWeek8) / (AveragePositiveNewYTWeek8 + AverageNegativeNewYTWeek8))
#Sentiment=-0.0877193


#For the Huffington Post
corpHP1 = corpus(week1HP, text_field = 'title')
dtmHP1 = corpHP1 %>% tokens %>% dfm()
HPsentimentweek1 = dtmHP1 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
HPSW1=HPsentimentweek1%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.4166667 
#Positive average:0.4166667
HPSW1=HPSW1%>%rename(AverageNegativeHPWeek1='mean(negative)')
HPSW1=HPSW1%>%rename(AveragePositiveHPWeek1='mean(positive)')
HPSW1 = HPSW1 %>% mutate(sentimentHPweek1=(AveragePositiveHPWeek1 - AverageNegativeHPWeek1) / (AveragePositiveHPWeek1 + AverageNegativeHPWeek1))
#Sentiment=0

corpHP2 = corpus(week2HP, text_field = 'title')
dtmHP2 = corpHP2 %>% tokens %>% dfm()
HPsentimentweek2 = dtmHP2 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
HPSW2=HPsentimentweek2%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.8 
#Positive average:0.8
HPSW2=HPSW2%>%rename(AverageNegativeHPWeek2='mean(negative)')
HPSW2=HPSW2%>%rename(AveragePositiveHPWeek2='mean(positive)')
HPSW2 = HPSW2 %>% mutate(sentimentHPweek2=(AveragePositiveHPWeek2 - AverageNegativeHPWeek2) / (AveragePositiveHPWeek2 + AverageNegativeHPWeek2))
#Sentiment=0

corpHP3 = corpus(week3HP, text_field = 'title')
dtmHP3 = corpHP3 %>% tokens %>% dfm()
HPsentimentweek3 = dtmHP3 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
HPSW3=HPsentimentweek3%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.75 
#Positive average:0.5
HPSW3=HPSW3%>%rename(AverageNegativeHPWeek3='mean(negative)')
HPSW3=HPSW3%>%rename(AveragePositiveHPWeek3='mean(positive)')
HPSW3 = HPSW3 %>% mutate(sentimentHPweek3=(AveragePositiveHPWeek3 - AverageNegativeHPWeek3) / (AveragePositiveHPWeek3 + AverageNegativeHPWeek3))
#Sentiment=-0.2

corpHP4 = corpus(week4HP, text_field = 'title')
dtmHP4 = corpHP4 %>% tokens %>% dfm()
HPsentimentweek4 = dtmHP4 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
HPSW4=HPsentimentweek4%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:1.153846
#Positive average:0.4615385
HPSW4=HPSW4%>%rename(AverageNegativeHPWeek4='mean(negative)')
HPSW4=HPSW4%>%rename(AveragePositiveHPWeek4='mean(positive)')
HPSW4 = HPSW4 %>% mutate(sentimentHPweek4=(AveragePositiveHPWeek4 - AverageNegativeHPWeek4) / (AveragePositiveHPWeek4 + AverageNegativeHPWeek4))
#Sentiment=-0.4285714

corpHP5 = corpus(week5HP, text_field = 'title')
dtmHP5 = corpHP5 %>% tokens %>% dfm()
HPsentimentweek5 = dtmHP5 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
HPSW5=HPsentimentweek5%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.5714286
#Positive average:0.6666667
HPSW5=HPSW5%>%rename(AverageNegativeHPWeek5='mean(negative)')
HPSW5=HPSW5%>%rename(AveragePositiveHPWeek5='mean(positive)')
HPSW5 = HPSW5 %>% mutate(sentimentHPweek5=(AveragePositiveHPWeek5 - AverageNegativeHPWeek5) / (AveragePositiveHPWeek5 + AverageNegativeHPWeek5))
#Sentiment=0.07692308


corpHP6 = corpus(week6Hp, text_field = 'title')
dtmHP6 = corpHP6 %>% tokens %>% dfm()
HPsentimentweek6 = dtmHP6 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
HPSW6=HPsentimentweek6%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.6428571
#Positive average:0.5
HPSW6=HPSW6%>%rename(AverageNegativeHPWeek6='mean(negative)')
HPSW6=HPSW6%>%rename(AveragePositiveHPWeek6='mean(positive)')
HPSW6 = HPSW6 %>% mutate(sentimentHPweek6=(AveragePositiveHPWeek6 - AverageNegativeHPWeek6) / (AveragePositiveHPWeek6 + AverageNegativeHPWeek6))
#Sentiment=-0.125

corpHP7 = corpus(week7HP, text_field = 'title')
dtmHP7 = corpHP7 %>% tokens %>% dfm()
HPsentimentweek7 = dtmHP7 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
HPSW7=HPsentimentweek7%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.4545455
#Positive average:0.4545455
HPSW7=HPSW7%>%rename(AverageNegativeHPWeek7='mean(negative)')
HPSW7=HPSW7%>%rename(AveragePositiveHPWeek7='mean(positive)')
HPSW7 = HPSW7 %>% mutate(sentimentHPweek7=(AveragePositiveHPWeek7 - AverageNegativeHPWeek7) / (AveragePositiveHPWeek7 + AverageNegativeHPWeek7))
#Sentiment=0

corpHP8 = corpus(week8HP, text_field = 'title')
dtmHP8 = corpHP8 %>% tokens %>% dfm()
HPsentimentweek8 = dtmHP8 %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
HPSW8=HPsentimentweek8%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.6363636
#Positive average:0.8181818
HPSW8=HPSW8%>%rename(AverageNegativeHPWeek8='mean(negative)')
HPSW8=HPSW8%>%rename(AveragePositiveHPWeek8='mean(positive)')
HPSW8 = HPSW8 %>% mutate(sentimentHPweek8=(AveragePositiveHPWeek8 - AverageNegativeHPWeek8) / (AveragePositiveHPWeek8 + AverageNegativeHPWeek8))
#Sentiment=0.125


# #Sentiment Analysis of Tweets -------------------------------------------
#Week 1
corp1 = corpus(Tweetsweek1)
week1tweets_dtm = corp1 %>% tokens %>% dfm()
SentimentWeek1 = week1tweets_dtm %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
TSW1=SentimentWeek1%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.684
#Positive average:1.245
TSW1=TSW1%>%rename(AverageNegativeTweetsWeek1='mean(negative)')
TSW1=TSW1%>%rename(AveragePositiveTweetsWeek1='mean(positive)')
TSW1=TSW1%>%mutate(SentimentWeek1=(AveragePositiveTweetsWeek1 - AverageNegativeTweetsWeek1)/(AveragePositiveTweetsWeek1 + AverageNegativeTweetsWeek1))
#Sentiment=0.2908243

#Week 2
corp2 = corpus(tweetsWeek2)
week2tweets_dtm = corp2 %>% tokens %>% dfm()
SentimentWeek2 = week2tweets_dtm %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
TSW2=SentimentWeek2%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.722
#Positive average:1.191
TSW2=TSW2%>%rename(AverageNegativeTweetsWeek2='mean(negative)')
TSW2=TSW2%>%rename(AveragePositiveTweetsWeek2='mean(positive)')
TSW2=TSW2%>%mutate(SentimentWeek2=(AveragePositiveTweetsWeek2 - AverageNegativeTweetsWeek2)/(AveragePositiveTweetsWeek2 + AverageNegativeTweetsWeek2))
#Sentiment=0.2451647

#Week 3
corp3 = corpus(tweetsWeek3)
week3tweets_dtm = corp3 %>% tokens %>% dfm()
SentimentWeek3 = week3tweets_dtm %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
TSW3=SentimentWeek3%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.673
#Positive average:1.179
TSW3=TSW3%>%rename(AverageNegativeTweetsWeek3='mean(negative)')
TSW3=TSW3%>%rename(AveragePositiveTweetsWeek3='mean(positive)')
TSW3=TSW3%>%mutate(SentimentWeek3=(AveragePositiveTweetsWeek3 - AverageNegativeTweetsWeek3)/(AveragePositiveTweetsWeek3 + AverageNegativeTweetsWeek3))
#Sentiment=0.2732181

#Week 4
corp4 = corpus(tweetsWeek4)
week4tweets_dtm = corp4 %>% tokens %>% dfm()
SentimentWeek4 = week4tweets_dtm %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
TSW4=SentimentWeek4%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.77
#Positive average:1.158
TSW4=TSW4%>%rename(AverageNegativeTweetsWeek4='mean(negative)')
TSW4=TSW4%>%rename(AveragePositiveTweetsWeek4='mean(positive)')
TSW4=TSW4%>%mutate(SentimentWeek4=(AveragePositiveTweetsWeek4 - AverageNegativeTweetsWeek4)/(AveragePositiveTweetsWeek4 + AverageNegativeTweetsWeek4))
#Sentiment=0.2012448

#Week 5
corp5 = corpus(tweetsWeek5)
week5tweets_dtm = corp5 %>% tokens %>% dfm()
SentimentWeek5 = week5tweets_dtm %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
TSW5=SentimentWeek5%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.615
#Positive average:1.106
TSW5=TSW5%>%rename(AverageNegativeTweetsWeek5='mean(negative)')
TSW5=TSW5%>%rename(AveragePositiveTweetsWeek5='mean(positive)')
TSW5=TSW5%>%mutate(SentimentWeek5=(AveragePositiveTweetsWeek5 - AverageNegativeTweetsWeek5)/(AveragePositiveTweetsWeek5 + AverageNegativeTweetsWeek5))
#Sentiment=0.2852992

#Week 6
corp6 = corpus(tweetsWeek6)
week6tweets_dtm = corp6 %>% tokens %>% dfm()
SentimentWeek6 = week6tweets_dtm %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
TSW6=SentimentWeek6%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.674
#Positive average:1.275
TSW6=TSW6%>%rename(AverageNegativeTweetsWeek6='mean(negative)')
TSW6=TSW6%>%rename(AveragePositiveTweetsWeek6='mean(positive)')
TSW6=TSW6%>%mutate(SentimentWeek6=(AveragePositiveTweetsWeek6 - AverageNegativeTweetsWeek6)/(AveragePositiveTweetsWeek6 + AverageNegativeTweetsWeek6))
#Sentiment=0.3083633

#Week 7
corp7 = corpus(tweetsWeek7)
week7tweets_dtm = corp7 %>% tokens %>% dfm()
SentimentWeek7 = week7tweets_dtm %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
TSW7=SentimentWeek7%>%mutate(mean(negative))%>%mutate(mean(positive))
#Negative average:0.717
#Positive average:1.233
TSW7=TSW7%>%rename(AverageNegativeTweetsWeek7='mean(negative)')
TSW7=TSW7%>%rename(AveragePositiveTweetsWeek7='mean(positive)')
TSW7=TSW7%>%mutate(SentimentWeek7=(AveragePositiveTweetsWeek7 - AverageNegativeTweetsWeek7)/(AveragePositiveTweetsWeek7 + AverageNegativeTweetsWeek7))
#Sentiment=0.2646154

#Week 8
corp8 = corpus(tweetsWeek8)
week8tweets_dtm = corp8 %>% tokens %>% dfm()
SentimentWeek8 = week8tweets_dtm %>% dfm_lookup(GI_dict) %>% convert(to = "data.frame") %>% as_tibble
TSW8=SentimentWeek8%>%mutate(mean(negative))%>%mutate(mean(positive))
#Where TSW8 stands for Tweest Sentiment Week 8
#Negative average:0.828
#Positive average:1.217
TSW8=TSW8%>%rename(AverageNegativeTweetsWeek8='mean(negative)')
TSW8=TSW8%>%rename(AveragePositiveTweetsWeek8='mean(positive)')
TSW8=TSW8%>%mutate(SentimentWeek8=(AveragePositiveTweetsWeek8 - AverageNegativeTweetsWeek8)/(AveragePositiveTweetsWeek8 + AverageNegativeTweetsWeek8))
#Sentiment=0.19022


# Regression --------------------------------------------------------------
AverageSentimentOverview=Average.Scores.Media.csv
#Calculating the difference between the Twitter sentiment and the Newspaper sentiment
AverageSentimentOverview=AverageSentimentOverview%>%mutate(Difference=(Media-Twitter))
#Shows that the general sentiment of the media was more negative than the twitter sentiment, in all 8 weeks

#Correlation Regression
ggplot(AverageSentimentOverview, aes(x = Media, y =Twitter )) +
  geom_point() +
  stat_smooth()
cor(AverageSentimentOverview$Twitter, AveragesData$Media)

 #correlation = 0.4800105 meaning that there is a relatively weak positive linear correlation between the two variables 

model <- lm(Twitter ~ Media, data = AverageSentimentOverview)
model
#This gives us the regression model to predict twitter based on media 
#namely - Twitter sentiment=0.2714+0.2574 * Media Sentiment








