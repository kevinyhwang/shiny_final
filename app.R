library(dplyr)
library(shiny)
library(shinythemes)
library(data.table)
library(DT)
library(ggplot2)
library(wordcloud)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("darkly"), 
  titlePanel("Facebook Posts by US Media Analysis"),
  
  
  tabsetPanel(
    
    ########TAB 1######
    
    tabPanel(
      "Likes Analysis",
      h1("Number of Likes by 6 major medias in US", style = "font-size: 90%"),
      plotOutput("coolplot")
    ),
    
    
    
    ########TAB 2######
    
    tabPanel(
      "Likes Statistics By Media",
      
      # br(),
      selectInput("media_summary", "Media",
                  choices = c("ABC", "BBC", "CNN", "FOX", "HUFFPOST", "NBC")
      ),
      plotOutput("summaryplot")
    ),
    
    ########TAB 3######
    
    tabPanel(
      "Most Liked Post", h2("Most Liked Posts by 6 US Media"),
      fluidRow(
        tabBox(
          title = tags$b("ABC", style="color:DodgerBlue"),
          id = "abc", height = "200px",
          tabPanel("967,776 people like this.",  p("WWII Vet Reunites With Man He Saved From Concentration Camp"),
                   br(),
                   a(href="	https://www.facebook.com/ABCNews/videos/10154411726563812/", "View Post"))
        ),
        tabBox(
          title = tags$b("BBC", style="color:DodgerBlue"),
          id = "bbc", height = "200px",
          tabPanel("1,032,488 people like this.",  p("I won't give you the gift of hating you?"),
                   br(),
                   a(href="https://www.facebook.com/bbcnews/videos/10153213901157217/", "View Post"))
        ),
        tabBox(
          title = tags$b("CNN", style="color:DodgerBlue"),
          id = "cnn", height = "200px",
          tabPanel("1,155,249 people like this.",  p("At least 22 hours after the earthquake struck Nepal, a miracle emerged from the rubble."),
                   br(),
                   a(href="	https://www.facebook.com/cnn/photos/a.369810096508.159795.5550296508/10153609954131509/?type=3", "View Post"))
        ),
        tabBox(
          title = tags$b("FOX", style="color:DodgerBlue"),
          id = "fox", height = "200px",
          tabPanel("1,072,112 people like this.",  p("Chris Kyle's hometown of Odessa, Texas unveiled this memorial statue of the American Sniper"),
                   br(),
                   a(href="https://www.facebook.com/FoxNews/photos/a.184044921335.134777.15704546335/10154481813316336/?type=3", "View Post"))
        ),
        tabBox(
          title = tags$b("HUFFINGTON", style="color:DodgerBlue"), 
          id = "huf", height = "200px",
          tabPanel("1,105,940	people like this.",  p("Baby Elephants Who Want To Be Lap Dogs"),
                   br(),
                   a(href="https://www.facebook.com/HuffingtonPost/videos/10153727875261130/", "View Post"))
        ),
        tabBox(
          title = tags$b("NBC", style="color:DodgerBlue"),
          id = "nbc", height = "200px",
          tabPanel("340,359 people like this.",  p("When a snowstorm hit Birmingham, Ala., Tuesday, it turned the city's streets and highways into parking lots."),
                   br(),
                   a(href="https://www.facebook.com/NBCNews/photos/a.162132393806799.30950.155869377766434/757393630947336/?type=3", "View Post"))
        )
      )
    ),
    
    
    
    ########TAB4######
    
    tabPanel(
      "Wordcloud",
      plotOutput("wcplot")
    ),
    
    ########TAB 5######
    
    tabPanel(
      "Data Table", h3("List of 1000 posts by ABC, BCC, CNN, FOX, Huffington, and NBC"),
      DT::dataTableOutput ("table")
      
    )
  )
)

# Define server logic required to draw a histogram
server = function(input, output, session){
  
  output$coolplot <- renderPlot({
    ggplot(likesbyyear, aes(x = year, y = Total_Likes)) +
      geom_line(size=1.5, aes(color=media))
  })
  output$wcplot <- renderPlot({
    wcdata <- readLines("allmedia_text.txt")
    
    wc_corpus <- Corpus(VectorSource(wcdata))
    wc_corpus_Clean <- tm_map(wc_corpus, tolower)
    wc_corpus_Clean <- tm_map(wc_corpus_Clean, removeNumbers)
    wc_corpus_Clean <- tm_map(wc_corpus_Clean, removeWords, stopwords())
    wc_corpus_Clean <- tm_map(wc_corpus_Clean, removeWords, c("http", "com", "facebook", "www", "photo", "abc", "bbc", "cnn", "fox", "huffington", "nbc", "timeline", "video", "videostype", "foxnews"))
    wc_corpus_Clean <- tm_map(wc_corpus_Clean, stemDocument)
    # wordcloud(wc_corpus_Clean, min.freq=5, colors=brewer.pal(6, "Set2"), random.order = F, rot.per = .30)
    wordcloud(wc_corpus_Clean, min.freq=5, max.words = 1000, colors=brewer.pal(6, "Set2"), random.order = F, rot.per = .20)
  })
  
  
  
  output$summaryplot <- reactivePlot(function(){
    
    if (input$media_summary == "ABC") {
      ggplot(abc_summary, aes(x=year, y=likes_count)) + 
        geom_bar(stat="identity", fill="steelblue4") +
        geom_text(aes(label=likes_count), vjust=3.5, color="white", size=3.5) +
        theme_minimal(base_size = 11)
    }
    
    else if (input$media_summary == "BBC") {
      ggplot(bbc_summary, aes(x=year, y=likes_count)) + 
        geom_bar(stat="identity", fill="steelblue4") +
        geom_text(aes(label=likes_count), vjust=3.5, color="white", size=3.5) +
        theme_minimal(base_size = 11)
      
    }
    
    else if (input$media_summary == "CBS") {
      ggplot(cbs_summary, aes(x=year, y=likes_count)) + 
        geom_bar(stat="identity", fill="steelblue4") +
        geom_text(aes(label=likes_count), vjust=3.5, color="white", size=3.5) +
        theme_minimal(base_size = 11)
    }
    
    else if (input$media_summary == "FOX") {
      ggplot(fox_summary, aes(x=year, y=likes_count)) + 
        geom_bar(stat="identity", fill="steelblue4") +
        geom_text(aes(label=likes_count), vjust=3.5, color="white", size=3.5) +
        theme_minimal(base_size = 11)
    }
    
    else if (input$media_summary == "HUFFPOST") {
      ggplot(huf_summary, aes(x=year, y=likes_count)) + 
        geom_bar(stat="identity", fill="steelblue4") +
        geom_text(aes(label=likes_count), vjust=3.5, color="white", size=3.5) +
        theme_minimal(base_size = 11)
    }
    
    else #(input$media_summary == "NBC") 
    {
      ggplot(nbc_summary, aes(x=year, y=likes_count)) + 
        geom_bar(stat="identity", fill="steelblue4") +
        geom_text(aes(label=likes_count), vjust=3.5, color="white", size=3.5) +
        theme_minimal(base_size = 11)
    }
  })
  
  
  output$table <- DT::renderDataTable({
    DT::datatable(allmedia2, rownames=FALSE, filter = 'top') %>% 
      formatStyle('year', color = 'black', fontWeight = 'bold') %>%
      formatStyle('media', color = 'black', fontWeight = 'bold') %>%
      formatStyle('name', color = 'black', fontWeight = 'bold') %>%
      formatStyle('message', color = 'black') %>%
      formatStyle('post_type', color = 'black', fontWeight = 'bold') %>%
      formatStyle('likes_count', color = 'black', fontWeight = 'bold') %>%
      formatStyle('shares_count', color = 'black', fontWeight = 'bold') %>%
      formatStyle('love_count', color = 'black', fontWeight = 'bold') %>%
      formatStyle('wow_count', color = 'black', fontWeight = 'bold') %>%
      formatStyle('haha_count', color = 'black', fontWeight = 'bold') %>%
      formatStyle('sad_count', color = 'black', fontWeight = 'bold') %>%
      formatStyle('thankful_count', color = 'black', fontWeight = 'bold') %>%
      formatStyle('angry_count', color = 'black', fontWeight = 'bold') %>%
      formatStyle('link', color = 'steelblue', width = '50%')
  })
}


# Run the application 

shinyApp(ui = ui, server = server)

