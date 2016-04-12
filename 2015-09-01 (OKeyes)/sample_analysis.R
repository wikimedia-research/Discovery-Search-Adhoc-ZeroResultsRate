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
  
  # Look for automata in the initial results
  results$is_automata <- is_automata(results$user_agent)
  
  # Look for automata in the IP addresses. Brute-force it; over 100k == bad person
  ip_aggregate <- results[results$is_automata==FALSE, j=list(searches = sum(searches)), by = "ip_address"]
  over_k <- ip_aggregate$ip_address[ip_aggregate$searches > 100000]
  results$is_automata[results$ip_address %in% over_k] <- TRUE
  
  bot_breakdown <- results[, j=list(requests = sum(searches)), by = c("timestamp","type","has_results","is_automata")]
  bot_breakdown$type <- paste(bot_breakdown$type, ifelse(bot_breakdown$is_automata == TRUE, "(automata)", "(non-automata)"))
  bot_breakdown <- bot_breakdown[, j = list(requests = (sum(requests[has_results == FALSE])/sum(requests))), by = c("timestamp","type")]
  ggsave(file = "zrr_bot_breakdown.png",
         plot =  ggplot(bot_breakdown, aes(timestamp, requests, group = type, colour = type)) + 
           geom_line(size=1.6) + theme_fivethirtyeight() +
           scale_x_date(breaks = "week") + scale_y_continuous(labels = percent) +
           expand_limits(y = 0) +
           labs(x = "Date", y = "Zero Results Rate (%)",
                title = "Zero Results Rate over time, automata/non-automata"))
  
  without_bots <- bot_breakdown[grepl(x = bot_breakdown$type, pattern = "non")]
  without_bots$type <- gsub(x = without_bots$type, pattern = " (non-automata)", fixed = TRUE, replacement = "")
  ggsave(file = "without_bot_by_type.png",
         plot =  ggplot(without_bots, aes(timestamp, requests, group = type, colour = type)) + 
           geom_line(size=1.6) + theme_fivethirtyeight() + stat_smooth() +
           scale_x_date(breaks = "week") + scale_y_continuous(labels = percent) +
           labs(x = "Date", y = "Zero Results Rate (%)",
                title = "Zero Results Rate over time, without automata"))
}

main()