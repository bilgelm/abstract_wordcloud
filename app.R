#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

library(waiter)

# pubmed search
library(RISmed)

# text mining
library(tm)
library(SnowballC)

# wordcloud generation
library(wordcloud2)

library(htmlwidgets)
library(webshot)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(
    title = "Generate wordcloud from PubMed abstracts",
    titleWidth = "100%"
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(
        width = 4,
        box(
          width = NULL,
          textInput("query", "PubMed query",
            value = "Murat Bilgel[AU]"
          ),
          actionButton("generate", "Search PubMed",
            class = "btn-block btn-info"
          ),
        ),
        box(
          width = NULL,
          p(
            class = "text-muted",
            paste(
              "Common English stopwords (is, are, was, at, some, ...) ",
              "will be removed from results."
            )
          ),
          textAreaInput(
            "remove_words", "Additional words to remove (comma-separated):",
            value = paste(
              c(
                "abstract", "abstracttext", "objective", "purpose",
                "introduction", "background", "method", "methods", "material",
                "materials", "discussion", "conclusion", "conclusions",
                "reference", "references", "bibliography",
                "however", "can", "may", "also"
              ),
              collapse = ", "
            ),
            rows = 7
          ),
          selectInput("shape", "Wordcloud shape",
            choices = c(
              "circle", "cardioid", "diamond",
              "triangle-forward", "triangle", "pentagon",
              "star"
            )
          ),
          actionButton("update", "Generate wordcloud",
            class = "btn-block btn-info"
          )
        )
      ),
      column(
        width = 8,
        box(
          width = NULL,
          use_waiter(),
          wordcloud2Output("wordcloud", width = "100%"),
          downloadButton("download", "Download wordcloud")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  intermediate_docs <- eventReactive(input$generate, {
    id <- showNotification("Searching PubMed...",
      duration = NULL, closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)

    # get pubmed data
    search_query <- EUtilsSummary(input$query)
    records <- EUtilsGet(search_query)
    pubmed_data <- data.frame(
      "Title" = ArticleTitle(records),
      "Abstract" = AbstractText(records)
    )

    # get abstract body only
    docs <- VCorpus(VectorSource(pubmed_data$Abstract))

    # get rid of unnecessary characters in text
    to_space <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
    docs <- tm_map(docs, to_space, "/")
    docs <- tm_map(docs, to_space, "@")
    docs <- tm_map(docs, to_space, "\\|")

    # Convert the text to lower case
    docs <- tm_map(docs, content_transformer(tolower))
    # Remove numbers
    docs <- tm_map(docs, removeNumbers)
    # Remove english common stopwords (is, are, was, at, some, ...)
    docs <- tm_map(docs, removeWords, stopwords("english"))
    # Remove punctuations
    docs <- tm_map(docs, removePunctuation)
    # Eliminate extra white spaces
    docs <- tm_map(docs, stripWhitespace)
    docs
  })

  remove_words <- eventReactive(input$update, input$remove_words)

  counts_table <- reactive({
    # Remove additional words
    docs <- tm_map(
      intermediate_docs(), removeWords,
      strsplit(remove_words(), ", ", fixed = TRUE)[[1]]
    )

    # generate word frequency table
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(Word = names(v), Count = v)
    d
  })

  wordcloud <- reactive({
    mywordcloud <- wordcloud2(counts_table(),
      size = 1.0,
      color = "random-dark",
      shape = input$shape,
    )

    mywordcloud
  })

  output$counts_table <- renderTable(head(counts_table(), 10))
  output$wordcloud <- renderWordcloud2(wordcloud())
  output$download <- downloadHandler(
    filename = "wordcloud.png",
    content = function(file) {
      waiter <- Waiter$new(
        html = tagList(
          span("   Preparing wordcloud...", style = "color:white;"),
          spin_wandering_cubes()
        ),
        color = "rgba(96,96,96,.8)"
      )
      waiter$show()
      on.exit(waiter$hide())
      saveWidget(wordcloud(), "wordcloud.html")
      webshot("wordcloud.html", file,
        delay = 10,
        vwidth = 2000, vheight = 1500
      )
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
