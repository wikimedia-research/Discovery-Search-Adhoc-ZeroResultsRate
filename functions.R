source("config.R")

# Simple function to read from datasets.*
read_datasets <- function(filename){
  url <- paste0("http://datasets.wikimedia.org/aggregate-datasets/search/", filename)
  con <- base::url(url)
  data <- readr::read_delim(base::url(url), delim = "\t")
  close(con)
  return(as.data.table(data))
}