#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# course: STA 504/SPRING 2020                                                  #
# title: Graduate Project                                                      #
# author: William Ofosu Agyapong, section A                                    #
# date last updated: May 14, 2020                                              #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# loading required packages
library(tidyverse)
library(tidytext)
library(textdata)
library(lubridate)
library(scales)
library(rtweet)
library(wordcloud)
library(RColorBrewer)

# setting ggplot theme for all plots
theme_set(theme_bw())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file processes the datasets for use in the shiny application with some codes for demonstration
#
# First time running of this file requires one to log in to a twitter account to be able to access
# the Twitter API via the rtweet library
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# retrieving Obama's tweet timelines
obama_tweets <- get_timeline("BarackObama", n=3200) # get tweets from twiter API
obama_tweets_clean <- obama_tweets %>%
  select(created_at,screen_name,text,name,location, is_retweet) %>%
  mutate(date = as.Date(created_at)) %>%
  filter(!is_retweet)


# deriving hourly tweets in a day
hourly_tweet_percent <- obama_tweets_clean %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>% # extract the hour from each time tweet was posted
  count(name, hour) %>% # get the frequency of each hour of the day
  group_by(name) %>%
  mutate(percent = n / sum(n)) # convert frequency to a percent

# visualizing hour of day tweets were posted
  ggplot(hourly_tweet_percent, aes(hour, percent)) +
  geom_line(show.legend = FALSE, color="dodgerblue") +
  geom_point(show.legend = FALSE,color="dodgerblue") +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)", y = "% of tweets", color = "",
       title = "Barack Obama's Tweets Pattern per Hour of Day",
       subtitle = paste("From ", format(obama_tweets_clean$date[length(obama_tweets_clean$date)],"%b %d, %Y"), " to ", format(obama_tweets_clean$date[1],"%b %d, %Y")),
       caption = "\n Source: Data collected from Twitter's REST API via rtweet") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
        )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                          Extracting words for all tweets
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))" # define a regex to capture Twitter special characters
tweet_words <- obama_tweets_clean %>%
  filter(!str_detect(text, '^"')) %>% # make sure we don't get words from quotes
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>% # remove links
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%  # obtain individual words in tweet
  filter(!word %in% stop_words$word & word!="http" & word!="obama") %>% # remove words that are not informative
  filter(str_detect(word, "[a-z]")) # remove numeric characters


# top 20 common words used by Barack Obama
top_20_words <- tweet_words  %>%
  group_by(word) %>%
  summarise(frequency= n()) %>%
  ungroup() %>%
  arrange(desc(frequency)) %>%
  top_n(20, frequency) %>%
  mutate(word = reorder(word, frequency))


# creating bar chart of top 20 commonly used words
ggplot(top_20_words) +
  geom_bar(
    aes(x=word, y=frequency), fill = "dodgerblue", color = "grey40", alpha = .5,
    stat="identity") +
  theme_classic() +
  labs(x="Words", y="Number of Times Used",
       title="20 most commom Words in Barack Obama's Tweets",
       caption = "\n Source: Data collected from Twitter's REST API via rtweet") +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 13, face = "italic", hjust = 0,vjust = 0),
        axis.text=element_text(size=14),
        plot.title = element_text(size=18, hjust = 0.5, face="bold")
  ) +
  coord_flip()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Displaying most commonly used words using wordcloud
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# setting seed for reproducibility of same results
set.seed(22222)

# most common words from Obama
tweet_words %>%
  anti_join(stop_words, by = "word") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 random.order=FALSE,
                 colors= brewer.pal(8,"Dark2"),
                 scale = c(3,0.5)))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    Sentiment Analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# using the nrc lexicon to assign appropriate sentiment to words
sentiments <- get_sentiments("nrc")

# count and compare the frequencies of each sentiment appearing in each device
  sentiment_summary <- tweet_words %>%
  left_join(sentiments, by = "word") %>%
  count(screen_name, sentiment) %>% # find sentiment score
  spread(screen_name, n) %>%
  rename(score = BarackObama) %>%
  filter(!is.na(sentiment))


# Creating a barplot to visualize sentiment scores
  ggplot(sentiment_summary) +
    geom_bar(
      aes(x=sentiment, y=score, fill=sentiment),
      stat="identity") +
    scale_fill_brewer(palette = "Set3") +
    theme_classic()+
    labs(x="Sentiment", y="Score",
         title="Sentiment Scores for Barack Obama's Tweets",
         caption = "\n Source: Data collected from Twitter's REST API via rtweet") +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.caption = element_text(size = 13, face = "italic", hjust = 0,vjust = 0),
          axis.text=element_text(size=14),
          plot.title = element_text(size=18, hjust = 0.5, face="bold")
    ) +
    coord_flip()


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #   Saving Processed Data for use in the Shiny App
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #write.csv(obama_tweets_clean, "obama_tweets.csv")
  save(obama_tweets_clean, file = "obama_tweets_clean.rda")
  save(tweet_words, file="tweet_words.rda")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # End of File
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
