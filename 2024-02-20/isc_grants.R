install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud2")
library(wordcloud2)
install.packages("tm")
library(tm)

isc_grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-20/isc_grants.csv')

isc_grants

view(isc_grants)

summary(isc_grants)
glimpse(isc_grants)

# Combine the title and summary columns into one column
isc_grants$text <- paste(isc_grants$title, isc_grants$summary, sep = " ")

#Create a vector containing only the text
text <- isc_grants$text

# text

# Create a corpus  
docs <- VCorpus(VectorSource(text))

# Clean text data
# Strip out punctuation, whitespace, and stopwords
docs <- docs %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# Create document term matrix
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)


# Generate the word cloud
set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 10, max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale=c(4.5,0.25))

# view(df)
