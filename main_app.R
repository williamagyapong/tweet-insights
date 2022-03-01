#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Purpose: Main application interface                                                      #
# author: William Ofosu Agyapong                                                #
# Last updated: Feb 22, 2022                                                    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        Loading Required Packages
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(tidytext)
library(textdata)
library(lubridate)
library(scales)
library(wordcloud)
library(RColorBrewer)
library(shiny)
library(shinytitle)
library(plotly)

#-----------------------------------------------------------------
# Import file dependencies
source("custom_functions.R")

#------------------------------------------------------------------
# Set a default ggplot theme for all plots
theme_set(theme_classic())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#        Loading Preprocessed Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# retrieving Obama's tweet timelines directly from twiter api. requires twiter account authentication.
# obama_tweets <- get_timeline("BarackObama", n=3200) # get tweets from twiter API
# obama_tweets_clean <- obama_tweets %>%
#   select(created_at,screen_name,text,name,location, is_retweet) %>%
#   mutate(date = as.Date(created_at)) %>%
#   filter(!is_retweet)

# Set the current working directory to the file path
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# or use already prepared tweets dataset
load("obama_tweets_clean.rda")
load("tweet_words.rda")

# import the nrc lexicon to assign appropriate sentiment to words
load("nrc_sentiments.RData")

# getting years
years <- sort(unique(year((obama_tweets_clean$date))), decreasing = T) # 0 for all years
years <- setNames(c(0, years), c(paste(years[length(years)], '-', years[1]), years))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                  Visualization of content with Shiny App
# The basic layout of this dashboard was adapted from my group project with Kelvin Njuki
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Define UI ----
ui <- fluidPage(theme=bslib::bs_theme(bootswatch="united"),
                tags$head(tags$style(internal_css())),

                # used here to make change_window_title() in the server side work
                use_shiny_title(),
    titlePanel(
      # creating a title bar
      div("TWEET INSIGHTS",
          style="text-align:center;width:100%;height:60px;margin-top:-15px;font-weight:bold;
        font-size: 20px;font-family: Tahoma; background-color: #E1EDEB; line-height:60px;
        color:gray;")

    ),

    tabsetPanel(
      tabPanel("Home",
               # shared controls area
               fluidRow(
                 column(style="border-bottom:2px #E1EDEB solid;padding-bottom:1px;",
                        width=12,
                        # controller for trend
                        selectInput("year", "Select Year:",years),
                 )
               ),
               #first row
               fluidRow(
                 column(style="margin-top:0px;padding-bottom:15px;border-bottom:2px #E1EDEB solid;",
                        width=6,
                        h3("Tweeting Pattern",
                           style=" color: #B1967C;font-family: 'Arial Black', serif;font-weight: normal;font-size: 1.2em;
             border-bottom: 2px #E1EDEB solid;padding-left: 25px;"),

                        # controller for trend
                        # selectInput("year", "Select Year:",years),
                        plotlyOutput("trend_plot")
                 ),

                 column(style="margin-top:0px;border-left:2px #E1EDEB solid;border-bottom:2px #E1EDEB solid;padding-bottom:20px;",
                        width=6,
                        h3("Most Common Words ",
                           style=" color: #B1967C;font-family: 'Arial Black', serif;font-weight: normal;font-size: 1.2em;
              border-bottom: 2px #E1EDEB solid;padding-left: 25px;"),


                        sliderInput( inputId = "num_words",
                                     label = "Choose the number of words:",
                                     value = 10, min = 2, max = 15
                        ),

                        plotlyOutput("most_common_plot")
                 )
               )
               ,
               fluidRow(

               ),
               #second row
               fluidRow(
                 column(style="padding-top:20px;",
                        width=6,
                        h3("Common Words on Wordcloud",
                           style=" color: #B1967C;font-family: 'Arial Black', serif;font-weight: normal;font-size: 1.2em;
              border-bottom: 2px #E1EDEB solid;padding-left: 25px;"),

                        # selectInput("year4", "Select Year:",years),
                        plotOutput("wordcloud_plot")
                 ),

                 column(style="padding-top:20px; margin-bottom:20px;border-left: 2px #E1EDEB solid;",
                        width=6,
                        h3("Sentiment Analysis",
                           style=" color: #B1967C;font-family: 'Arial Black', serif;font-weight: normal;font-size: 1.2em;border-bottom: 2px #E1EDEB solid;padding-left: 25px;"),
                        # selectInput("year2", "Select Year:",years),
                        plotlyOutput("sentiment_plot")
                 )
               )

      ),

      tabPanel("Trends",
               fluidRow(
                 column(width = 12,style="height:350px;font-style:italic; padding:100px;margin-bottom:360px",
                        h4("Looking for the spikes and dips over time? ..."),
                        HTML('<div style="text-align:center;"><br><br>
                         Relax while we put the bolts and nuts together.</div>'),
                        plotlyOutput("trend")
                 )
               ),
               fluidRow(
                 column(width = 12)
               )
      ),

      tabPanel("Sentiment Analysis",
               fluidRow(
                 column(width = 12,style="height:350px;font-style:italic; padding:100px;margin-bottom:360px",
                        h4("Let's take the sentiment analysis ..."),
                        HTML('<div style="text-align:center;"><br><br>
                         further down the road.</div>'),
                        plotlyOutput("sentiment")
                 )
               )
      ),
      tabPanel("Word Analytics",
               fluidRow(
                 column(width = 12,style="height:350px;font-style:italic;padding:100px;margin-bottom:360px",
                        h4("Come with us for a ride ..."),
                        HTML('<div style="text-align:center;"><br><br>into the words that make up every tweet.</div>'),
                        plotlyOutput("word_analytics")
                 )
               )
      ),
      tabPanel("Network Analysis",
               fluidRow(
                 column(width = 12,style="height:350px;font-style:italic;padding:100px;margin-bottom:360px",
                        h4("Let's mine the relations ..."),
                        HTML('<div style="text-align:center;"><br><br>among users, topics, and many more.</div>'),
                        plotlyOutput("networks")
                 )
               )
      ),
      tabPanel("About the App",
               fluidRow(
                 column(width = 12,style="height:350px;padding:100px;margin-bottom:360px;",
                        HTML('
                         <h4>The goal is to provide a free online platform for users to be able to effortlessly draw insteresting
                         and valuable insights from the data generated on <a href="https://twitter.com/">Twitter</a> ...</h4>

                          We come your way with a tool for understanding tweeting patterns or trends, discovering frequently used words,
                          detecting emotions and sentiments expressed in tweets, and many more.  For instance, from the home page, Former President Obama’s 2014 - 2020 tweets
                           are seen to be highly <strong>positive</strong>, full of <strong>trust</strong>, quite <strong>joyful</strong> and <strong>fearful</strong>, and with great <strong>anticipation</strong>.
                           The President appeared to have tweeted a lot about the <strong>climate</strong>, <strong>people</strong>, and the <strong>pandemic</strong>.'),
                        HTML('

                            <div style="margin-top:15px;">
                              <ul>
                                <li> The app is in its early stage of development. </li>
                                <li>This app is fed with data obtained directly from Twitter with the help of the <i>Twitter REST API</i> via
                                the <a href="https://github.com/ropensci/rtweet">rtweet</a> package.</li>
                                <li> Currently, the app features the tweets from a preprocessed data for a single twitter user account. </li>
                                <li>But the ultimate aim is to make the app more dynamic  by allowing users to view information
                                about different user accounts on the fly.</li>
                                <li>The home page is meant to provide a general overview about the content served on the app and
                                other information deemed necessary.
                                <li>Thematic contents will be delivered under various taps as seen.</li>
                                </li>
                                <li> <a href="#"><i>read more...</i></a> </li>
                              </ul>
                            </div>

                           '),
                        plotlyOutput("about-app")
                 )
               )
      )
    ),
  # footer
  fluidRow(class = "footer",
            style="box-sizing: border-box;width:100%; height: 130px;
            background-color: whitesmoke; padding:8px;",
    column(width = 5,
              HTML('<span style="font-weight:bold;font-style:italic;">Data Source:</span> <br>
             Retrieved from Twitter REST API via the
             <a href="https://github.com/ropensci/rtweet">rtweet</a> package.')
           ),
    column(width = 4, style="border-right:2px solid #fff; border-left:2px solid #fff;pading-left:10px;",
            HTML('<span style="font-weight:bold;font-style:italic;">Developed By:</span><br>
            William O. Agyapong <br>
           PhD (Data Science) student <br>University of Texas at El Paso <br>
           <span> &copy; 2022 </span>')
           ),
    column(width = 3, style="padding-left:10px;",
            HTML('<span style="font-weight:bold;font-style:italic;">Follow on:</span> <br>
              <ul>
                <li><a href="https://gitHub.com/williamagyapong">Github</a></li>
                <li><a href="https://www.datacamp.com/profile/williamagyapong">DataCamp</a></li>
                <li><a href="https://linkedin.com/in/william-agyapong-372aa4146">LinkedIn</a></li>
              </ul>')
    )
  )

)

# Define server logic ----
server <- function(input, output) {

  # theme for plots
  thematic::thematic_shiny()

  # change browser tab title
  change_window_title(title = "Tweet Insights")

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
    # nrc_sentiments <- get_sentiments("nrc")

    # count and compare the frequencies of each sentiment appearing in each device
    sentiment_summary <- this_tweet_words() %>%
      left_join(nrc_sentiments, by = "word") %>%
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

# Run the app ----
shinyApp(ui = ui, server = server)

############################# END OF FILE ######################
