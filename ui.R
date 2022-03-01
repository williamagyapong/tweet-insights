#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# File: UI/client side logic                                                       #
# author: William Ofosu Agyapong                                                #
# Last updated: Feb 22, 2022                                                    #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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
library(plotly)

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

# getting years
years <- sort(unique(year((obama_tweets_clean$date))), decreasing = T) # 0 for all years
years <- setNames(c(0, years), c(paste(years[length(years)], '-', years[1]), years))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                  Visualization of content with Shiny App
# The basic layout of this dashboard was adapted from my group project with Kelvin Njuki
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Define UI ----
ui <- fluidPage(
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
               column(width = 12,style="height:350px;font-style:italic; padding:20px;",
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
               column(width = 12,style="height:350px;font-style:italic; padding:20px;",
                      h4("Let's take the sentiment analysis ..."),
                      HTML('<div style="text-align:center;"><br><br>
                         further down the road.</div>'),
                      plotlyOutput("sentiment")
               )
             )
    ),
    tabPanel("Word Analytics",
             fluidRow(
               column(width = 12,style="height:350px;font-style:italic;padding:20px;",
                      h4("Come with us for a ride ..."),
                      HTML('<div style="text-align:center;"><br><br>into the words that make up every tweet.</div>'),
                      plotlyOutput("word_analytics")
               )
             )
    ),
    tabPanel("Network Analysis",
             fluidRow(
               column(width = 12,style="height:350px;font-style:italic;padding:20px;",
                      h4("Let's mine the relations ..."),
                      HTML('<div style="text-align:center;"><br><br>among users, topics, and many more.</div>'),
                      plotlyOutput("networks")
               )
             )
    ),
    tabPanel("About the App",
             fluidRow(
               column(width = 12,style="height:350px;padding:20px;",
                      HTML('
                         <h4>The goal is to provide a free online platform for users to draw insteresting 
                         and valuable insights from the data generated on <a href="https://twitter.com/">Twitter</a> ...</h4>'),
                      HTML('
                      
                            <div style="">
                              <ul> 
                                <li> The app is in its early stage of development. </li>
                                <li>This app is fed with data obtained directly from Twitter with the help of the <i>Twitter REST API</i> via 
                                the <a href="https://github.com/ropensci/rtweet">rtweet</a> package.</li>
                                <li> Currently, the app features the tweets from a preprocessed data for a single twitter user account. </li>
                                <li>But the ultimate aim is to make the app more dynamic  by allowing users to view information
                                about different user accounts on the fly.</li>
                                <li>The home page is meant to provide a general overview about the content served on the app and 
                                other information deemed necessary. Thematic contents will be delivered under various taps as seen.
                                </li>
                                <li> <a href="#"><i>read more...</i></a> </li>
                              </ul>
                            </div>
                           
                           ')
                      # plotlyOutput("word_analytics")
               )
             )
    )
  ),
  
  # footer
  fluidRow(
    HTML(
      '
        <footer style="box-sizing: border-box;width:100%; height: 130px;
            background-color: whitesmoke; padding:6px;">
           <div style=" width:40%; padding:8px;">
           <span style="font-weight:bold;">Data Source:</span> <br>
             Retrieved from Twitter REST API via the 
             <a href="https://github.com/ropensci/rtweet">rtweet</a> package
           </div>
           <div style="width:35%;border-right:2px solid #fff; border-left:2px solid #fff;
              position:relative;left:40%;top:-50px; padding-left:10px;">
           <span style="font-weight:bold;">Developed By:</span><br>
            William O. Agyapong <br>
           PhD (Data Science) student <br>University of Texas at El Paso <br>
           <span> &copy; 2022 </span>
           </div>
           <div style=" width:25%;position:relative;left:75%; top:-150px;padding-left:10px;">
           <span style="font-weight:bold;">Follow on:</span> <br>
              <ul>
                <li><a href="https://gitHub.com/williamagyapong">Github</a></li>
                <li><a href="https://www.datacamp.com/profile/williamagyapong">DataCamp</a></li>
                <li><a href="https://linkedin.com/in/william-agyapong-372aa4146">LinkedIn</a></li>
              </ul>
           </div>
         </footer>'
      # '
      # <div class="container-fluid"
      #     <div class="row">
      #       <div class="col-4">
      #           <span style="font-weight:bold;">Data Source:</span> <br>
      #           Data Retrieved from Twitter REST API via rtweet <br>
      #           <span> &copy; 2022 </span>
      #       </div>
      #       <div class="col-4"> 
      #           <span style="font-weight:bold;">Developed By:</span><br>
      #           William O. Agyapong <br>
      #           PhD (Data Science) student <br>University of Texas at El Paso
      #       </div>
      #       <div class="col-4"> 
      #         
      #       </div>
      #         <span> Social Icons </span>
      #     </div>
      # </div>
      # '
    )
  )
  
)

############################# END OF FILE ######################