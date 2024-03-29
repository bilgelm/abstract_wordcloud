library(shiny)

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

if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()

function(input, output, session) {
  intermediate_docs <- reactive({
    id <- showNotification("Searching PubMed...",
      duration = NULL, closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)

    # get pubmed data
    search_query <- EUtilsSummary(input$query)
    # from RISmed::EUtilsGet documentation:
    # In order not to overload the E-utility servers,
    # NCBI recommends that users post no more than three URL requests per second
    # to prevent HTTP 429 errors, here's a half second wait
    Sys.sleep(0.5)
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

  counts_table <- reactive({
    # Remove additional words
    docs <- tm_map(
      intermediate_docs(), removeWords,
      strsplit(input$remove_words, ", ", fixed = TRUE)[[1]]
    )

    # generate word frequency table
    dtm <- TermDocumentMatrix(docs)
    if (dtm$nrow==0) {
      d <- data.frame(Word = c("Noresult",
                               "PubMedquerydidnotyieldresults"), Count = c(1, 0))
    } else {
      m <- as.matrix(dtm)
      v <- sort(rowSums(m), decreasing = TRUE)
      d <- data.frame(Word = names(v), Count = v)
    }
    d
  })

  wordcloud <- reactive({
    mywordcloud <- wordcloud2(counts_table(),
      size = 1.0,
      color = "random-dark",
      shape = input$shape
    )

    mywordcloud
  }) %>% bindEvent(input$generate)

  output$wordcloud <- renderWordcloud2(wordcloud())

  output$downloadUI <- renderUI({
    if (is.null(wordcloud())) {
      return()
    }
    downloadButton("download", "Download wordcloud")
  })
  
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
