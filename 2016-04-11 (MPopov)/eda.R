library(ggplot2)
library(magrittr)
library(dplyr)
library(cowplot)
library(mgcv)

query_types <- readr::read_tsv("data/cirrus-request-query-types.tsv")

theme_set(ggthemes::theme_tufte(base_family = "Gill Sans"))

query_types$dashboard_type <- ifelse(query_types$query_type %in% c("full_text", "degraded_full_text", "regex", "more_like"),
                                     "Full-Text Search", "Prefix Search")

query_types %>%
  select(query_type, dashboard_type) %>%
  distinct %>%
  mutate(change = "   ") %>%
  arrange(query_type) %>%
  knitr::kable()

query_types %>%
  group_by(query_type) %>%
  summarize(dashboard_type = head(dashboard_type, 1),
            requests = sum(total_requests)) %>%
  mutate(proportion = requests/sum(requests)) %>%
  ggplot(aes(y = proportion, x = query_type, fill = dashboard_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_y_continuous("Proportion of Cirrus search requests", labels = scales::percent_format(), limits = c(0, 0.8)) +
  labs(title = "Query types found in CirrusSearchRequestSet table", x = "Query Type") +
  scale_fill_brewer(name = "How dashboards categorize it", type = "qual", palette = "Set1") +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black")) +
  geom_text(aes(label = sprintf("%.3f%% (%s)", 100 * proportion, polloi::compress(requests, 1)), color = dashboard_type),
            position = position_dodge(width = 1), hjust = -0.1) +
  scale_color_brewer(name = "How dashboards categorize it", type = "qual", palette = "Set1")

aggregates <- readr::read_tsv("data/cirrus-request-aggregates.tsv")

aggregates %>%
  group_by(date, dashboard_type) %>%
  summarize(total = sum(total_requests), zero = sum(zero_results_requests)) %>%
  mutate(zrr = zero/total) %>%
  ggplot(aes(x = date, y = zrr, color = dashboard_type)) +
  geom_line(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 10), se = FALSE) +
  scale_color_brewer(name = "Dashboard category", type = "qual", palette = "Set1") +
  scale_y_continuous("Zero results rate", labels = scales::percent_format(), limits = c(0.2, 0.5)) +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black")) +
  labs(title = "Overall zero results rate", x = "Date")

aggregates %>%
  filter(dashboard_type_new != "Other") %>%
  group_by(date, dashboard_type) %>%
  summarize(total = sum(total_requests), zero = sum(zero_results_requests)) %>%
  mutate(zrr = zero/total) %>%
  ggplot(aes(x = date, y = zrr, color = dashboard_type)) +
  geom_line(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 10), se = FALSE) +
  scale_color_brewer(name = "Dashboard category (fixed)", type = "qual", palette = "Set1") +
  scale_y_continuous("Zero results rate", labels = scales::percent_format(), limits = c(0.2, 0.5)) +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black")) +
  labs(title = "Overall zero results rate (fixed query_types)", x = "Date")

p1 <- aggregates %>%
  group_by(date, is_automata, dashboard_type) %>%
  summarize(total = sum(total_requests), zero = sum(zero_results_requests)) %>%
  mutate(zrr = zero/total) %>%
  ggplot(aes(x = date, y = zrr, color = dashboard_type)) +
  geom_line(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 10), se = FALSE) +
  scale_color_brewer(name = "Dashboard category (fixed)", type = "qual", palette = "Set1") +
  scale_y_continuous("Zero results rate", labels = scales::percent_format(), limits = 0:1) +
  labs(title = "(a) Zero results rate by automata status", x = "Date") +
  facet_wrap(~is_automata, nrow = 1,
             labeller = function(x) { x$is_automata <- ifelse(x$is_automata, "Automata", "Not automata"); return(x) }) +
  theme_bw(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black"))

p2 <- aggregates %>%
  # filter(dashboard_type_new != "Other") %>%
  filter(query_type %in% c("comp_suggest", "full_text", "prefix", "more_like")) %>%
  group_by(date, is_automata, dashboard_type) %>%
  summarize(total = sum(total_requests), zero = sum(zero_results_requests)) %>%
  mutate(zrr = zero/total) %>%
  ggplot(aes(x = date, y = zrr, color = dashboard_type)) +
  geom_line(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 10), se = FALSE) +
  scale_color_brewer(name = "Dashboard category (fixed)", type = "qual", palette = "Set1") +
  scale_y_continuous("Zero results rate", labels = scales::percent_format(), limits = 0:1) +
  labs(title = "(b) ZRR with fixed categorization", x = "Date") +
  facet_wrap(~is_automata, nrow = 1,
             labeller = function(x) { x$is_automata <- ifelse(x$is_automata, "Automata", "Not automata"); return(x) }) +
  theme_bw(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black"))

p3 <- aggregates %>%
  filter(query_type %in% c("comp_suggest", "full_text", "prefix", "more_like")) %>%
  group_by(date, is_automata, query_type) %>%
  summarize(total = sum(total_requests), zero = sum(zero_results_requests),
            dashboard_type = head(dashboard_type, 1)) %>%
  mutate(zrr = zero/total) %>%
  ggplot(aes(x = date, y = zrr, color = query_type, linetype = dashboard_type)) +
  geom_line(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 10), se = FALSE) +
  scale_color_brewer(name = "Query Type", type = "qual", palette = "Set1") +
  scale_y_continuous("Zero results rate", labels = scales::percent_format(), limits = 0:1) +
  scale_linetype_discrete(name = "Dashboard categorization") +
  labs(title = "(c) ZRR from (b) but broken down by query_type", x = "Date") +
  facet_wrap(~is_automata, nrow = 1,
             labeller = function(x) { x$is_automata <- ifelse(x$is_automata, "Automata", "Not automata"); return(x) }) +
  theme_bw(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black"))

p4 <- aggregates %>%
  # filter(dashboard_type_new != "Other") %>%
  filter(query_type %in% c("comp_suggest", "full_text", "prefix")) %>%
  group_by(date, is_automata, dashboard_type) %>%
  summarize(total = sum(total_requests), zero = sum(zero_results_requests)) %>%
  mutate(zrr = zero/total) %>%
  ggplot(aes(x = date, y = zrr, color = dashboard_type)) +
  geom_line(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 10), se = FALSE) +
  scale_color_brewer(name = "Dashboard category", type = "qual", palette = "Set1") +
  scale_y_continuous("Zero results rate", labels = scales::percent_format()) +
  labs(title = "(d) ZRR by automata, excluding query_type='more_like'", x = "Date") +
  facet_wrap(~is_automata, nrow = 1,
             labeller = function(x) { x$is_automata <- ifelse(x$is_automata, "Automata", "Not automata"); return(x) }) +
  theme_bw(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black"))

ggsave(filename = "zrr_by_type_automata.png", plot_grid(p1, p2, p3, p4, nrow = 2), path = "figures", width = 16, height = 12, dpi = 72)

aggregates %>%
  filter(dashboard_type_new != "Other") %>%
  group_by(date, source, dashboard_type) %>%
  summarize(total = sum(total_requests), zero = sum(zero_results_requests)) %>%
  mutate(zrr = zero/total) %>%
  ggplot(aes(x = date, y = zrr, color = dashboard_type)) +
  geom_line(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cr", k = 10), se = FALSE) +
  scale_color_brewer(name = "Dashboard category (fixed)", type = "qual", palette = "Set1") +
  scale_y_continuous("Zero results rate", labels = scales::percent_format(), limits = 0:1) +
  labs(title = "ZRR by source (with fixed query_types)", x = "Date") +
  facet_wrap(~source, nrow = 1) +
  theme_bw(base_family = "Gill Sans") +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black"))

p <- aggregates %>%
  filter(dashboard_type_new != "Other") %>%
  group_by(date, source, query_type) %>%
  summarize(total = sum(total_requests)) %>%
  mutate(proportion = total/sum(total)) %>%
  ggplot(aes(x = date, y = proportion, fill = query_type)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~source, nrow = 1, labeller = function(x) {
    x$source <- paste("Source:", toupper(x$source))
    return(x)
  }) +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  scale_fill_manual(name = "Query Type", values = RColorBrewer::brewer.pal(7, "Set1")[-6]) +
  scale_y_continuous("Proportion of Cirrus requests", labels = scales::percent_format()) +
  labs(title = "Proportions of query types", x = "Date") +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))
ggsave(filename = "query_types_by_source.png", p, path = "figures", width = 8, height = 4)

aggregates %>%
  filter(dashboard_type_new != "Other") %>%
  group_by(query_type) %>%
  summarize(total = sum(total_requests)) %>%
  mutate(proportion = sprintf("%.2f%%", 100*total/sum(as.numeric(total))))

aggregates %>%
  filter(query_type %in% c("comp_suggest", "full_text", "prefix")) %>%
  group_by(date, is_automata, source, query_type) %>%
  summarize(total = sum(total_requests), zero = sum(zero_results_requests),
            dashboard_type = head(dashboard_type, 1)) %>%
  mutate(zrr = zero/total) %>% # %T>% View() %>%
  ggplot(aes(x = date, y = zrr, color = query_type, linetype = dashboard_type)) +
  geom_line(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 5), se = FALSE) +
  scale_color_brewer(name = "Query type", type = "qual", palette = "Set1") +
  scale_y_continuous("Zero results rate", labels = scales::percent_format()) +
  labs(title = "ZRR by source & automata status", x = "Date") +
  facet_wrap(source ~ is_automata, nrow = 2,
             labeller = function(x) {
               x$is_automata <- ifelse(x$is_automata, "Automata", "Not automata")
               x$source <- paste("Source:", x$source)
               return(x)
             }, scales = "free_y") +
  theme_bw(base_family = "Gill Sans") +
  scale_linetype_discrete(name = "Dashboard categorization") +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black"))
