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

dashboardPage(
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
          textInput(
            "query", "PubMed query",
            value = "Murat Bilgel[AU]"
          )
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
          actionButton("generate", "Generate wordcloud",
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
          uiOutput("downloadUI")
        )
      )
    )
  )
)
