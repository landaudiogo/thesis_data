setwd("C:/Users/DLA149/Documents/Thesis/Reassign Algorithm")
here::i_am("pareto_front.R")

library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(tidyquant)
library(magrittr)
library(here)
library(ggrepel)
library(emoa)

dir.create(here("pareto_front"))

get_cols_idxs <- function(df, col_names) {
  return(which(names(df) %in% col_names))
}

combined <- data.frame(
  delta=numeric(),
  algorithm=character(),
  avg_diff_to_min=numeric(),
  avg_rscore=numeric(),
  start_conditions=character()
)

files = c(
  "speed_0.csv", 
  "speed_50.csv",
  "speed_100.csv", 
  "uniform_start.csv"
)
for(file_name in files) {
  algorithms <- read.csv(file=here(file_name))
  algorithms$file <- sub("^(\\w+)_(\\d+)", "\\2", algorithms$file)
  algorithms$file <- as.numeric(algorithms$file)
  names(algorithms)[names(algorithms) == "file"] <- "delta"
  
  rscore_avg <- algorithms %>% 
    group_by(delta, algorithm) %>% 
    summarize(avg_rscore=mean(Rscore_Algorithm_Capacity))
  
  ggplot(rscore_avg, aes(x=delta, y=avg_rscore, color=algorithm)) + 
    geom_point() + geom_line(data=rscore_avg, aes(group=algorithm))
  
  wider_nconsumers <- algorithms %>% 
    summarize(delta, iteration, algorithm, Number.of.Consumers) %>%
    pivot_wider(names_from=algorithm, values_from=Number.of.Consumers)
  
  wider_nconsumers$min_consumers <- apply(wider_nconsumers[,-get_cols_idxs(wider_nconsumers, c("delta", "iteration"))], 1, FUN=min, na.rm=TRUE)
  wider_nconsumers[, -get_cols_idxs(wider_nconsumers, c("delta", "iteration"))] %<>% 
    mutate_at(vars(-min_consumers), list(~ (. - min_consumers)/min_consumers))
  
  longer_nconsumers <- wider_nconsumers %>% pivot_longer(!c("delta", "iteration", "min_consumers"), names_to="algorithm", values_to="diff") 
  group_nconsumers <- longer_nconsumers %>% group_by(delta, algorithm) %>% summarize(avg_diff_to_min=mean(diff)) 
  
  newdf <- merge(group_nconsumers, rscore_avg)
  newdf$start_conditions <- file_name
  combined <- rbind(combined, newdf)
}

pareto_deltas <- data.frame(
  delta=numeric(),
  algorithm=character(),
  avg_diff_to_min=numeric(),
  avg_rscore=numeric(),
  dominated=logical()
)
for(delta_v in c(5,10,15,20,25)) {
  test <- combined %>%
    group_by(delta, algorithm) %>% 
    summarise(avg_diff_to_min=mean(avg_diff_to_min), avg_rscore=mean(avg_rscore)) %>%
    filter(delta==delta_v)
  
  nd <- nondominated_points(t(as.matrix(test[c("avg_diff_to_min", "avg_rscore")])))
  nd <- data.frame(t(nd))
  nd$dominated <- FALSE
  
  ps <- left_join(test, nd)
  ps[is.na(ps$dominated),]$dominated <- TRUE
  ps$delta <- delta_v
  ggplot(ps, aes(x=avg_diff_to_min, y=avg_rscore, color=dominated)) + 
    labs(
      title=sprintf("Pareto Front for a delta value of %d", delta_v),
      x="Relative Number of Consumers above minimum", 
      y="Rscore"
    ) +
    geom_point() + 
    geom_step(data=ps[ps$dominated == FALSE,], direction="hv") +
    geom_text_repel(aes(label=algorithm), color="black")
  
  pareto_deltas <- rbind(pareto_deltas, ps)
}

pareto_deltas %>%
  ggplot(aes(x=avg_diff_to_min, y=avg_rscore, shape=dominated)) +
  geom_point() + 
  geom_step(data={. %>% filter(dominated==FALSE)}, alpha=0.2) +
  geom_text_repel(aes(label=algorithm), color="black") + 
  facet_wrap(~delta) + 
  labs(
    title="Pareto Front for each Delta", 
    x="Relative Number of consumers above minimum",
    y="Rscore"
  ) + 
  theme_bw()
ggsave(filename=here("pareto_front/facet_wrapped.png"))

deltas <- pareto_deltas %>% as.data.frame() %>% distinct(delta)
for(d in deltas$delta) {
  temp_plot <- pareto_deltas %>% filter(delta==d) %>% 
    ggplot(aes(x=avg_diff_to_min, y=avg_rscore, shape=dominated)) +
    geom_point() + 
    geom_step(data={. %>% filter(dominated==FALSE)}, alpha=0.2) +
    geom_text_repel(aes(label=algorithm), color="black") + 
    labs(
      x="Relative Number of consumers above minimum",
      y="Rscore"
    ) + 
    theme_bw()
  ggsave(temp_plot, filename=here(sprintf("pareto_front/%d.png", d)))
}


