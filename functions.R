source("config.R")

# Simple function to read from datasets.*
read_datasets <- function(filename){
  url <- paste0("http://datasets.wikimedia.org/aggregate-datasets/search/", filename)
  con <- base::url(url)
  data <- readr::read_delim(base::url(url), delim = "\t")
  close(con)
  return(as.data.table(data))
}

# Identify whether a series of URLs represent search requests.
# This should probably make it into the 'wmf' package.
is_search <- function(urls){
  
  # First, direct searches
  is_direct_search <- grepl(x = urls, pattern = ":Search", fixed = TRUE)
  
  # Then API searches
  is_api_search <- grepl(x = urls, pattern = "(action=(opensearch|languagesearch)|list=(prefixsearch|search|geosearch))",
                         perl = TRUE, useBytes = TRUE)
  
  # If it's one or the other (or both!) TRUE, else FALSE.
  return(is_direct_search | is_api_search)
}

read_cirrus <- function(filepath){
  
  # Read in data
  data <- read.delim(file = gzfile(filepath), header = FALSE, as.is = TRUE)
  
  # Extract the columns we care about. This varies depending on the sample period,
  # but luckily so does the column count, so we can make them match.
  if(ncol(data) == 9){
    data <- data[data$V2 == "phrase-slop-a", c("V4", "V8", "V7", "V9")]
    data <- data[sample(1:nrow(data), round(nrow(data)/5)),]
  } else {
    data <- data[data$V2 == "suggest-confidence-b", c("V4", "V8", "V7","V3")]
  }
  setnames(data, 1:ncol(data), c("has_results", "user_agent", "ip_address", "type"))
  
  # Easily distinguish prefix/full-text search and no results/some results.
  data$type[!grepl(x = data$type, pattern = "full_text")] <- "Prefix"
  data$type[data$type != "Prefix"] <- "Full"
  data$has_results <- (as.character(data$has_results) != "0")
  
  # Add the timestamp, reformat it, and aggregate it for memory savings.
  # Then return.
  filepath <- gsub(x = filepath, pattern = "[^0-9]", replacement = "")
  data$timestamp <- as.Date(strptime(filepath, "%Y%m%d", tz = "UTC"))
  data <- as.data.table(data)
  data <- data[,j=list(searches = .N*100), by = c("timestamp","user_agent","ip_address","type", "has_results")]
  return(data)
}

Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
Rcpp::sourceCpp("is_automata.cpp")