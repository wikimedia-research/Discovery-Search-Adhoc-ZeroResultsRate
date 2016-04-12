## All possible query types
start_date <- Sys.Date()-8
end_date <- Sys.Date()-1
query_types <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching aggregates from", as.character(date), "\n")
  # Write query and run it
  query <- paste0("USE wmf_raw;
                  SELECT query_type, COUNT(1) AS total_requests FROM (
                    SELECT
                      requests[size(requests)-1].querytype AS query_type
                    FROM cirrussearchrequestset ", wmf::date_clause(date)$date_clause, "
                  ) AS cirrusrequests GROUP BY query_type;")
  results <- wmf::query_hive(query)
  results <- results[!is.na(results$total_requests), ]
  return(cbind(date = date, results, stringsAsFactors = FALSE))
}))
readr::write_tsv(query_types, "~/cirrus-request-query-types.tsv")
system("scp stat2:/home/bearloga/cirrus-request-query-types.tsv data/")

## Number of search requests per IP address
start_date <- Sys.Date()-1
end_date <- Sys.Date()-1
requests <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching aggregates from", as.character(date), "\n")
  # Write query and run it
  query <- paste0("ADD JAR /home/ebernhardson/refinery-hive-0.0.21-SNAPSHOT.jar;
                  CREATE TEMPORARY FUNCTION array_sum AS 'org.wikimedia.analytics.refinery.hive.ArraySumUDF';
                  CREATE TEMPORARY FUNCTION is_spider as 'org.wikimedia.analytics.refinery.hive.IsSpiderUDF';
                  CREATE TEMPORARY FUNCTION ua_parser as 'org.wikimedia.analytics.refinery.hive.UAParserUDF';
                  CREATE TEMPORARY FUNCTION is_wikimedia as 'org.wikimedia.analytics.refinery.hive.IsWikimediaBotUDF';
                  USE wmf_raw;
                  SELECT source, ip_address, query_type, is_automata,
                    COUNT(1) AS total_requests,
                    SUM(IF(zero_result, 1, 0)) AS zero_results_requests
                  FROM (
                    SELECT
                      source, ip AS ip_address,
                      array_sum(requests.hitstotal, -1) = 0 AS zero_result,
                      requests[size(requests)-1].querytype AS query_type,
                      CASE
                        WHEN ((ua_parser(useragent)['device_family'] = 'Spider') OR
                               is_spider(useragent) OR is_wikimedia(useragent) OR
                               ip = '127.0.0.1') THEN 'TRUE' ELSE 'FALSE'
                      END AS is_automata
                    FROM cirrussearchrequestset ", wmf::date_clause(date)$date_clause, "
                    AND requests[size(requests)-1].querytype IN('regex', 'more_like', 'full_text', 'comp_suggest', 'prefix', 'near_match')
                  ) AS searches GROUP BY source, ip_address, query_type, is_automata;")
  results <- wmf::query_hive(query)
  results <- results[!is.na(results$total_requests), ]
  results <- cbind(date = date, results, stringsAsFactors = FALSE)
  names(results) <- c('date', 'source', 'ip_address', 'query_type', 'is_automata', 'total_requests', 'zero_results_requests')
  results$dashboard_type <- ifelse(results$query_type %in% c("full_text", "degraded_full_text", "regex", "more_like"),
                                   "Full-Text Search", "Prefix Search")
  return(results)
}))

library(data.table)
requests <- as.data.table(requests, key = c("date", "source", "ip_address", "query_type", "is_automata"))
zrr_by_ip <- requests[, j = list(total = sum(total_requests),
                                 zrr = sum(zero_results_requests)/sum(total_requests),
                                 source = head(source, 1)),
                        by = c("ip_address", "is_automata")]
zrr_by_ip[head(order(zrr_by_ip$total, decreasing = TRUE), 20),,]

## Number of search requests per IP address
start_date <- as.Date("2016-03-13") # Sys.Date()-29
end_date <- as.Date("2016-04-10") # Sys.Date()-1
requests <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching aggregates from", as.character(date), "\n")
  # Write query and run it
  query <- paste0("ADD JAR /home/ebernhardson/refinery-hive-0.0.21-SNAPSHOT.jar;
                  CREATE TEMPORARY FUNCTION array_sum AS 'org.wikimedia.analytics.refinery.hive.ArraySumUDF';
                  CREATE TEMPORARY FUNCTION is_spider as 'org.wikimedia.analytics.refinery.hive.IsSpiderUDF';
                  CREATE TEMPORARY FUNCTION ua_parser as 'org.wikimedia.analytics.refinery.hive.UAParserUDF';
                  CREATE TEMPORARY FUNCTION is_wikimedia as 'org.wikimedia.analytics.refinery.hive.IsWikimediaBotUDF';
                  USE wmf_raw;
                  SELECT source, query_type, is_automata,
                    COUNT(1) AS total_requests,
                    SUM(IF(zero_result, 1, 0)) AS zero_results_requests
                  FROM (
                    SELECT
                      source,
                      array_sum(requests.hitstotal, -1) = 0 AS zero_result,
                      requests[size(requests)-1].querytype AS query_type,
                      CASE
                        WHEN ((ua_parser(useragent)['device_family'] = 'Spider') OR
                               is_spider(useragent) OR is_wikimedia(useragent) OR
                               ip = '127.0.0.1') THEN 'TRUE' ELSE 'FALSE'
                      END AS is_automata
                    FROM cirrussearchrequestset ", wmf::date_clause(date)$date_clause, "
                  ) AS searches GROUP BY source, query_type, is_automata;")
  results <- wmf::query_hive(query)
  results <- results[!is.na(results$total_requests), ]
  results <- cbind(date = date, results, stringsAsFactors = FALSE)
  names(results) <- c('date', 'source', 'query_type', 'is_automata', 'total_requests', 'zero_results_requests')
  results$dashboard_type <- ifelse(results$query_type %in% c("full_text", "degraded_full_text", "regex", "more_like"),
                                   "Full-Text Search", "Prefix Search")
  results$dashboard_type_new <- results$dashboard_type
  results$dashboard_type_new[!(results$query_type %in% c('regex', 'more_like', 'full_text', 'comp_suggest', 'prefix', 'near_match'))] <- "Other"
  return(results)
}))
requests <- requests[order(requests$date, requests$source, requests$query_type, requests$is_automata), ]
readr::write_tsv(requests, "~/cirrus-request-aggregates.tsv")
system("scp stat2:/home/bearloga/cirrus-request-aggregates.tsv data/")
