# install packages (if necessary)
list_of_packages <- c(
  "RISmed",
  "tm",
  "SnowballC",
  "wordcloud2",
  "webshot",
  "htmlwidgets",
  "IRdisplay"
)

new_packages <- list_of_packages[
  !(list_of_packages %in% installed.packages()[, "Package"])
]

if (length(new_packages)) install.packages(new_packages)

webshot::install_phantomjs()

# pubmed search
library(RISmed)

# text mining
library(tm)
library(SnowballC)

# wordcloud generation
library(wordcloud2)

# figure save and display
library(webshot)
library(htmlwidgets)
library(IRdisplay)

# Code put together by Murat Bilgel from:
# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
# https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html
# https://www.r-graph-gallery.com/196-the-wordcloud2-library/

# PubMed query
my_query <- "Murat Bilgel[AU]"

# get pubmed data
search_query <- EUtilsSummary(my_query)
records <- EUtilsGet(search_query)
pubmed_data <- data.frame(
  "Title" = ArticleTitle(records),
  "Abstract" = AbstractText(records)
)
# take a look at the first two results
head(pubmed_data, 2)

# get abstract body only
docs <- VCorpus(VectorSource(pubmed_data$Abstract))

# take a look at the results
lapply(docs, inspect)

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
# Remove additional words
docs <- tm_map(
  docs, removeWords,
  c(
    "abstract", "abstracttext", "objective", "purpose",
    "introduction", "background", "method", "methods", "material",
    "materials", "discussion", "conclusion", "conclusions",
    "reference", "references", "bibliography",
    "however", "can", "may"
  )
)
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# generate word frequency table
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 20) # look at top 20 words

# plot wordcloud
mywordcloud <- wordcloud2(d, size = 1.0, color = "random-dark")
mywordcloud

# first save wordcloud as an interactive HTML, then export as png
saveWidget(mywordcloud, "mywordcloud.html", selfcontained = FALSE)

# a longer delay might be necessary to capture more words
webshot("mywordcloud.html", "mywordcloud.png", delay = 60,
        vwidth = 1000, vheight = 1000)
