#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# File: Server side logic                                                       #
# author: William Ofosu Agyapong                                                #
# Last updated: Feb 22, 2022                                                    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define server logic ----
function(input, output) {
  
  # get the year and data based on selected year
  tweet_year <- reactive(input$year)
  
  this_tweet_words <- reactive({
    if(tweet_year()==0) {
      tweet_words
      
    } else {
      # deriving houly tweets in a day
      tweet_words %>%
        filter(year(date) == tweet_year())
      
      # this_date <- this_tweet_words %>% filter(year(date) == tweet_year())
    }
  })
  
  # displaying hourly tweeting pattern
  #______________________________________________________________________________________________  
  
  output$trend_plot <- renderPlotly({
    
    if(tweet_year()==0) {
      # deriving hourly tweets in a day across all years
      hourly_tweet_percent <- obama_tweets_clean %>%
        mutate(hour = hour(with_tz(created_at, "EST"))) %>%
        count(name, hour) %>%
        group_by(name) %>%
        mutate(percent = n / sum(n))
      
      this_date <- obama_tweets_clean$date
    } else {
      # deriving houly tweets in a day for a single year
      hourly_tweet_percent <- obama_tweets_clean %>%
        filter(year(date) == tweet_year()) %>%
        mutate(hour = hour(with_tz(created_at, "EST"))) %>%
        count(name, hour) %>%
        group_by(name) %>%
        mutate(percent = n / sum(n))
      
      this_date <- obama_tweets_clean %>% filter(year(date) == tweet_year()) %>% pull(date)
    }
    
    # visualizing hour of day tweets were posted
    ggplotly(ggplot(hourly_tweet_percent, aes(hour, percent)) +
               geom_line(show.legend = FALSE, color="dodgerblue") +
               geom_point(show.legend = FALSE,color="dodgerblue") +
               scale_y_continuous(labels = percent_format()) +
               labs(x = "Hour of day (EST)", y = "% of tweets", color = "",
                    title = "Barack Obama's Tweets Pattern per Hour of Day",
                    subtitle = paste("From ", format(this_date[length(this_date)],"%b %d, %Y"), " to ", 
                                     format(this_date[1],"%b %d, %Y")),
                    caption = "\n Source: Data collected from Twitter's REST API via rtweet") +
               theme(plot.title = element_text(hjust = 0.5),
                     plot.subtitle = element_text(hjust = 0.5)
               )) %>%
      layout(title = list(text = paste0("Barack Obama's Tweets Pattern per Hour of Day",
                                        '<br>',
                                        '<sup>',
                                        paste("From ", format(this_date[length(this_date)],"%b %d, %Y"), " to ", 
                                              format(this_date[1],"%b %d, %Y")),
                                        '</sup>'))) %>%
      config(displayModeBar = F) #Removing plotly tool bar
  })
  
  
  # Displaying bar chart of top commonly used words
  #______________________________________________________________________________________________  
  output$most_common_plot <- renderPlotly({
    
    # top common words used by Barack Obama
    top_words <- this_tweet_words()  %>%
      group_by(word) %>%
      summarise(frequency= n()) %>%
      ungroup() %>%
      arrange(desc(frequency)) %>%
      top_n(input$num_words, frequency) %>%
      mutate(word = reorder(word, frequency))
    
    this_date <- this_tweet_words()$date # used in the plot subtitle
    
    ggplotly(ggplot(top_words) +
               geom_bar(
                 aes(x=word, y=frequency), fill = "dodgerblue", alpha = .5,
                 stat="identity") +
               labs(x="", y="Number of Times Used",
                    title=paste(input$num_words," most commom Words in Barack Obama's Tweets"),
                    caption = "\n Source: Data collected from Twitter's REST API via rtweet") +
               theme(legend.position = "none",
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     plot.caption = element_text( face = "italic", hjust = 0,vjust = 0),
                     axis.text=element_text(size=14),
                     plot.title = element_text( hjust = 0.5,)
               ) +
               coord_flip() )%>%
      layout(title = list(text = paste0(input$num_words," most commom Words in Barack Obama's Tweets",
                                        '<br>',
                                        '<sup>',
                                        paste("From ", format(this_date[1],"%b %d, %Y"), " to ",
                                              format(this_date[length(this_date)],"%b %d, %Y")),
                                        '</sup>'))) %>%
      config(displayModeBar = F) #Removing plotly tool bar
    
  })
  
  # displaying word cloud plot
  #______________________________________________________________________________________________
  output$wordcloud_plot<- renderPlot({
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Displaying most common used words using wordcloud
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # this_date <- this_tweet_words()$date # used in the plot subtitle
    # setting seed for reproducibility of same results
    set.seed(22222)
    
    # most common words from Obama
    
    word_cloud <- this_tweet_words() %>%
      anti_join(stop_words, by = "word") %>%
      count(word) 
    # display word cloud
    wordcloud(words=word_cloud$word,
              word_cloud$n, 
              max.words = 100,
              random.order=FALSE,
              colors= brewer.pal(8,"Dark2"),
              scale = c(3,0.5))
    
  })
  
  
  #__________________________________________________________________________
  output$sentiment_plot <- renderPlotly({
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #    Sentiment Analysis
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # using the nrc lexicon to assign appropriate sentiment to words 
    sentiments <- get_sentiments("nrc")
    
    # count and compare the frequencies of each sentiment appearing in each device
    sentiment_summary <- this_tweet_words() %>%
      left_join(sentiments, by = "word") %>%
      count(screen_name, sentiment) %>% # find sentiment score
      spread(screen_name, n) %>%
      rename(score = BarackObama) %>%
      filter(!is.na(sentiment))
    
    this_date <- this_tweet_words()$date # used in the plot subtitle
    
    # Creating a barplot to visualize sentiment scores
    ggplotly(ggplot(sentiment_summary) + 
               geom_bar(
                 aes(x=sentiment, y=score, fill=sentiment),
                 stat="identity") +
               scale_fill_brewer(palette = "Set3") +
               labs(x="Sentiment", y="Score",
                    title="Sentiment Scores for Barack Obama's Tweets",
                    caption = "\n Source: Data collected from Twitter's REST API via rtweet") +
               theme(legend.position = "none",
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     plot.caption = element_text(size = 13, face = "italic", hjust = 0,vjust = 0),
                     axis.text=element_text(size=14),
                     plot.title = element_text(hjust = 0.5, )
               ) +
               coord_flip())%>%
      layout(title = list(text = paste0("Sentiment Scores for Barack Obama's Tweets",
                                        '<br>',
                                        '<sup>',
                                        paste("From ", format(this_date[1],"%b %d, %Y"), " to ", 
                                              format(this_date[length(this_date)],"%b %d, %Y")),
                                        '</sup>'))) %>%
      config(displayModeBar = F) #Removing plotly tool bar 
    
    
  })
  
}


############################# END OF FILE ######################