library("wordcloud")
library("RColorBrewer")
library("wordcloud2")
library("shiny")
library("ggplot2")
library("readr")
library("tm")
library("SnowballC")
library("dplyr")
omics <- readLines("/Users/aa9gj/Documents/wordcloud/wordcloud/omics.txt") 
get_wordcloud <- function(x) {
  docs <- Corpus(VectorSource(x))
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)
  wordcloud <- wordcloud2(df)
  return(wordcloud)
}
#wordcloud shiny
ui <- fluidPage(
  h1("Word Cloud"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "source",
        label = "Word source",
        choices = c(
          "Omics" = "book",
          "Use your own words" = "own",
          "Upload a file" = "file"
        )
      ),
      conditionalPanel(
        condition = "input.source == 'own'",
        textAreaInput("text", "Enter text", rows = 7)
      ),
      conditionalPanel(
        condition = "input.source == 'file'",
        fileInput("file", "Select a file")
      ),
      #numericInput("num", "Maximum number of words",
                   #value = 100, min = 5),
      #colourInput("col", "Background color", value = "white"),
      # Add a "draw" button to the app
      actionButton(inputId = 'draw', label = "Draw!")
    ),
    mainPanel(
      wordcloud2Output("cloud")
    )
  )
)

server <- function(input, output) {
  data_source <- reactive({
    if (input$source == "book") {
      data <- omics
    } else if (input$source == "own") {
      data <- input$text
    } else if (input$source == "file") {
      data <- input_file()
    }
    return(data)
  })
  
  input_file <- reactive({
    if (is.null(input$file)) {
      return("")
    }
    readLines(input$file$datapath)
  })
  
  output$cloud <- renderWordcloud2({
    # Add the draw button as a dependency to
    # cause the word cloud to re-render on click
    input$draw
    isolate({
      get_wordcloud(data_source()) #size = input$num,
                       #backgroundColor = input$col)
    })
  })
}

shinyApp(ui = ui, server = server)


