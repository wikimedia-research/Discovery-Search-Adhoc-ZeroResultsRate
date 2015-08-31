# A quick look at our existing data source
source("functions.R")

main <- function(){
  
  # Start with the high-level
  data <- read_datasets("cirrus_query_aggregates.tsv")
  proportions <- data[, j = list(
    proportion = value[variable == "Zero Result Queries"]/value[variable == "Search Queries"]
  ), by = "date"]
  ggsave(file = "overall_zrr.png",
         plot = ggplot(proportions, aes(date, proportion)) + 
           geom_line(size = 1.6) + theme_fivethirtyeight() + stat_smooth() +
           scale_x_date(breaks = "2 weeks") + scale_y_continuous(labels = percent) +
           labs(x = "Date", y = "Zero Results Rate (%)",
                title = "Zero Results Rate over time"))
  
  # Breakdown by prefix/full
  data <- read_datasets("cirrus_query_breakdowns.tsv")
  ggsave(file = "by_type_zrr.png",
         plot =  ggplot(data, aes(date, value, group = variable, colour = variable)) + 
           geom_line(size=1.6) + theme_fivethirtyeight() + stat_smooth() +
           scale_x_date(breaks = "2 weeks") + scale_y_continuous(labels = percent) +
           labs(x = "Date", y = "Zero Results Rate (%)",
                title = "Zero Results Rate over time, by search type"))
  
  
  # Big rise on 2015-06-16. Will dig into the request logs and try to identify a source.
  # Rise mostly seems to be for full-text search.
  files <- wmf::get_logfile(earliest = "2015-08-15", latest = "2015-08-17")
  data <- do.call("rbind", lapply(files, wmf::read_sampled_log))
  
  # Identify dates and limit on that
  data$timestamp <- as.Date(wmf::from_log(data$timestamp))
  data <- data[data$timestamp %in% as.Date(c("2015-08-15", "2015-08-16")),]
  
  # Filter down to successful requests and search requests, in that order, then
  # turn it into a data.table
  data <- data[grepl(x = data$status_code, pattern = "(200|304)"),]
  data <- as.data.table(data[is_search(data$url),])
  
  # Identify user agents
  agents <- data[, j = {as.data.table(table(user_agent))}, by = "timestamp"]
  agents <- agents[order(agents$N, decreasing = TRUE)]
}