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