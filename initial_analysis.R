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
  
}