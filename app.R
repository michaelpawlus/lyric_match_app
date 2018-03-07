library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  theme = shinythemes::shinytheme("simplex"),
  
  titlePanel("Song Lyric Matching Terms"),
  
  br(),
  
  sidebarPanel(
  # Copy the line below to make a text input box
  textInput("sa1", label = h3("Song 1 Artist"), value = "The Beatles"),
  
  # Copy the line below to make a text input box
  textInput("st1", label = h3("Song 1 Title"), value = "Don't Let Me Down"),
  
  # Copy the line below to make a text input box
  textInput("sa2", label = h3("Song 2 Artist"), value = "The Beach Boys"),
  
  # Copy the line below to make a text input box
  textInput("st2", label = h3("Song 2 Title"), value = "Wouldn't It Be Nice"),
  
  br(),
  
  submitButton("Search for Common Terms")
  
  ),
  
  mainPanel(
    
    helpText("This simple app uses Josiah Parry's geniusR package to find lyrics for the songs 
             that you choose from the genius.com website. It then looks for matching
             three word terms, two word terms and finally single words with the exception of 
             extremely common words (i.e. 'stop words')"),
    
    br(),
    
    helpText("The example using the default options will find matches."),
    
    br(),
    
    helpText("This has no real utility of any kind except if anyone else out there listens to 
             BBC Radio 6 and plays along with the Tea Time Theme Time game in which case it is
             still very rarely heplful."),
    
    h3("Matching Terms"),
    
    tableOutput("contents"),
    
    helpText("Note: If you get no matches and feel there should be some
or you see Error: Not Found (HTTP 404) search for the Genius page for the song 
             you are looking for and make sure your spelling matches theirs.")
  )
  
)

# Define server logic to read selected file ----
server <- function(input, output) {

  output$contents <- renderTable({

    library(geniusR)
    library(tidyverse)
    library(tidytext)

    lyric_match <- function(artist1, title1, artist2, title2) {

      song1 <- genius_lyrics(artist = artist1, song = title1)

      song2 <- genius_lyrics(artist = artist2, song = title2)

      song1_tri <- song1 %>%
        unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        count(word1, word2, word3, sort = TRUE)

      song2_tri <- song2 %>%
        unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
        separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        count(word1, word2, word3, sort = TRUE)

      common_tris <- song1_tri %>%
        inner_join(song2_tri, by = c("word1","word2","word3")) %>%
        count(word1, word2, word3, sort = TRUE)

      common_tris <- common_tris %>%
        unite(word, word1, word2, word3, sep = " ")

      song1_bi <- song1 %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        count(word1, word2, sort = TRUE)

      song2_bi <- song2 %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        count(word1, word2, sort = TRUE)

      common_bis <- song1_bi %>%
        inner_join(song2_bi, by = c("word1","word2")) %>%
        count(word1, word2, sort = TRUE)

      common_bis <- common_bis %>%
        unite(word, word1, word2, sep = " ")

      song1_words <- song1 %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        count(word, sort = TRUE)

      song2_words <- song2 %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words) %>%
        count(word, sort = TRUE)

      common_words <- song1_words %>%
        inner_join(song2_words, by = "word") %>%
        count(word, sort = TRUE)

      all_matches <- bind_rows(common_tris, common_bis, common_words)

      print(all_matches)

    }

    lyric_match(input$sa1, input$st1, input$sa2, input$st2)

  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
