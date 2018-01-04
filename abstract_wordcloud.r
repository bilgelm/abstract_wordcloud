
# install packages (if necessary)
install.packages(c("RISmed","tm","SnowballC","wordcloud2","webshots","htmlwidgets","IRdisplay"),
                 repos="https://cran.rstudio.com/")
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
records<- EUtilsGet(search_query)
pubmed_data <- data.frame('Title'=ArticleTitle(records),'Abstract'=AbstractText(records))
head(pubmed_data,1) # take a look at the first result

# get abstract body only
docs <- Corpus(VectorSource(pubmed_data$Abstract))

inspect(docs) # take a look at the results

# get rid of unnecessary characters in text
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords (is, are, was, at, some, ...)
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove additional words
docs <- tm_map(docs, removeWords, c("abstract","objective","purpose","introduction","background",
                                    "method","methods","material","materials",
                                    "discussion","conclusion","conclusions",
                                    "reference","references","bibliography",
                                    "however")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming (i.e., get rid of suffixes)
# docs <- tm_map(docs, stemDocument)

# generate word frequency table
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20) # look at top 20 words

# plot wordcloud
# due to lack of htmlwidget support in jupyter notebook, we cannot display this directly here. 
# We'll first save it as an interactive HTML, then export as png.
mywordcloud = wordcloud2(d, size=1.6, color='random-dark')

saveWidget(mywordcloud, "mywordcloud.html", selfcontained = F)

# a longer delay might be necessary to capture more words
webshot("mywordcloud.html","mywordcloud.png", delay=60, vwidth=800, vheight=800)

display_png(file="mywordcloud.png")
