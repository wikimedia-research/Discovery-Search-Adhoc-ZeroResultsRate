source("functions.R")

main <- function(){
  
  # Read in data
  files <- list.files("./data", full.names = TRUE)
  results <- parallel::mclapply(files, read_cirrus, mc.cores = 4)
  results <- do.call("rbind", results)
  
  # Aggregate so that we can match it against the existing data sources.
  aggregated_results <- results[, j = list(rate = sum(searches[results == FALSE])/sum(searches)), c("timestamp","type")]
  aggregated_results$type[aggregated_results$type == "Prefix"] <- "Prefix (A/B logs)"
  aggregated_results$type[aggregated_results$type == "Full"] <- "Full (A/B logs)"
  
  # Read in existing data sources and format them
  existing_data <- read_datasets("cirrus_query_breakdowns.tsv")
  existing_data$variable <- ifelse((existing_data$variable == "Prefix Search"), "Prefix (Dashboard)", "Full (Dashboard)")
  existing_data <- existing_data[existing_data$date %in% unique(aggregated_results$timestamp),]
  setnames(existing_data, 1:3, c("timestamp", "type", "rate"))
  
  # Bind together and graph
  bound_data <- rbind(existing_data, aggregated_results)
  
  ggsave(file = "ab_dashboard_comparison.png",
         plot =  ggplot(bound_data, aes(timestamp, rate, group = type, colour = type)) + 
           geom_line(size=1.6) + theme_fivethirtyeight() +
           scale_x_date(breaks = "week") + scale_y_continuous(labels = percent) +
           expand_limits(y = 0) +
           labs(x = "Date", y = "Zero Results Rate (%)",
                title = "Zero Results Rate over time, by search type and log type"))
}

main()