# Aux functions

# Function to clean tweets
cleanTweets <- function(tweet){
  # Remove http links
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  tweet = gsub("http\\w+", "", tweet)
  # Remove retweets
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Remove “#Hashtag”
  tweet = gsub("#\\w+", " ", tweet)
  # Remove usernames “@people”
  tweet = gsub("@\\w+", " ", tweet)
  # Remove punctuation
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Remove numbers
  tweet = gsub("[[:digit:]]", " ", tweet)
  # Remove unnecessary spaces
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  # Converting character encoding and converting to lower case
  tweet <- stringi::stri_trans_general(tweet, "latin-ascii")
  tweet <- tryTolower(tweet)
  tweet <- iconv(tweet, from = "UTF-8", to = "ASCII")
}

# Function to clean corpus
cleanCorpus <- function(myCorpus){
  library(tm)
  myCorpus <- tm_map(myCorpus, tolower)
  # Remove punctuation
  myCorpus <- tm_map(myCorpus, removePunctuation)
  # Remove numbers
  myCorpus <- tm_map(myCorpus, removeNumbers)
}

# Converting to lower case
tryTolower = function(x)
{
  # Create a missing data (NA)
  y = NA
  # correct error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if don't have any error, converting to lower case
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # Return result
  return(y)
}


