# A quick look at our existing data source
source("functions.R")

main <- function(){
  
  # Start with the high-level
  data <- read_datasets("cirrus_query_aggregates.tsv")
  proportions <- data[, j = list(
    proportion = value[variable == "Zero Result Queries"]/value[variable == "Search Queries"]
  ), by = "date"]
  ggsave(file = "overall_zrr.png",
         plot = ggplot(proportions, aes(date, proportion, colour = "#F8766D")) + 
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
  files <- wmf::get_logfile(earliest = "2015-06-15", latest = "2015-06-17")
  data <- do.call("rbind", lapply(files, function(filename){
    data <- wmf::read_sampled_log(filename)
    
    # Identify dates and limit on that
    data$timestamp <- as.Date(wmf::from_log(data$timestamp))
    data <- data[data$timestamp %in% as.Date(c("2015-06-15", "2015-06-16")),]
    
    # Filter down to successful requests and search requests, in that order, then
    # turn it into a data.table
    data <- data[grepl(x = data$status_code, pattern = "(200|304)"),]
    data <- as.data.table(data[is_search(data$url),])
    
    # Done
    return(data)
  }))
  
  # 7% increase in the FT ZRR. And the increase in searches, full stop, is...
  searches_by_day <- as.data.frame(table(data$timestamp))
  str(searches_by_day)
  # 9%!
  
  # Identify user agents
  agents <- data[, j = {as.data.table(table(user_agent))}, by = "timestamp"]
  
  # Check out increase between -15 and -16
  agent_increase <- agents[, j = {
    if(.N == 1){
      NULL
    } else {
      prop_delta <- .SD$N[.SD$timestamp == as.Date("2015-06-16")]/.SD$N[.SD$timestamp == as.Date("2015-06-15")]
      raw_delta <- .SD$N[.SD$timestamp == as.Date("2015-06-16")] - .SD$N[.SD$timestamp == as.Date("2015-06-15")]
      list(proportion_change = prop_delta,
           numeric_change = raw_delta)
    }
  }, by = "user_agent"]
  agent_increase <- agent_increase[order(agent_increase$numeric_change, agent_increase$proportion_change,
                                         decreasing = TRUE),]
  
  # Big change appears to be Lagotto products.
  # Map density of change
  ggsave(file = "user_agent_change_density.png",
    plot = ggplot(agent_increase, aes(proportion_change)) + 
    geom_density(fill = "#F8766D") + theme_fivethirtyeight() +
    labs(title = "Proportionate increase in searches by user agent, 2015-08-15 - 2015-08-16") +
      scale_x_log10())
  
  # Map IP change
  data$ip_address <- iptools::xff_extract(data$ip_address, data$x_forwarded)
  ips <- data[, j = {as.data.table(table(ip_address))}, by = "timestamp"]
  ip_increase <- ips[, j = {
    if(.N == 1){
      NULL
    } else {
      prop_delta <- .SD$N[.SD$timestamp == as.Date("2015-06-16")]/.SD$N[.SD$timestamp == as.Date("2015-06-15")]
      raw_delta <- .SD$N[.SD$timestamp == as.Date("2015-06-16")] - .SD$N[.SD$timestamp == as.Date("2015-06-15")]
      list(proportion_change = prop_delta,
           numeric_change = raw_delta)
    }
  }, by = "ip_address"]
  ggsave(file = "ip_address_change_density.png",
         plot = ggplot(ip_increase, aes(proportion_change)) + 
           geom_density(fill = "#F8766D") + theme_fivethirtyeight() +
           labs(title = "Proportionate increase in searches by IP address, 2015-08-15 - 2015-08-16") + 
           scale_x_log10())
  
}