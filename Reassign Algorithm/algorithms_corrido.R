setwd("C:/Users/DLA149/Documents/Thesis/Reassign Algorithm")
here::i_am("algorithms_corrido.R")
library(here)

library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(tidyquant)
library(magrittr)

dir.create(here("generated_plots"), showWarnings=FALSE)

get_cols_idxs <- function(df, col_names) {
  return(which(names(df) %in% col_names))
}

algorithms <- read.csv(file=here("uniform_start.csv"))
algorithms$file <- sub("^(\\w+)_(\\d+)", "\\2", algorithms$file)
algorithms$file <- as.numeric(algorithms$file)



wider_nconsumers <- algorithms %>% 
  summarize(file, iteration, algorithm, Number.of.Consumers) %>%
  pivot_wider(names_from=algorithm, values_from=Number.of.Consumers)

wider_nconsumers$min_consumers <- apply(wider_nconsumers[,-get_cols_idxs(wider_nconsumers, c("file", "iteration"))], 1, FUN=min, na.rm=TRUE)
wider_nconsumers[, -get_cols_idxs(wider_nconsumers, c("file", "iteration"))] %<>% 
  mutate_at(vars(-min_consumers), list(~ (. - min_consumers)/min_consumers))

longer_nconsumers <- wider_nconsumers %>% pivot_longer(!c("file", "iteration", "min_consumers"), names_to="algorithm", values_to="diff") 
group_nconsumers <- longer_nconsumers %>% group_by(file, algorithm) %>% summarize(avg_diff_to_min=mean(diff)) 

nalgorithms <- group_nconsumers %>% as.data.frame() %>% 
  summarize(algorithm) %>% distinct() %>% nrow()
ggplot(group_nconsumers, aes(x=file, y=avg_diff_to_min, shape=algorithm)) + 
  scale_shape_manual(values=1:nalgorithms) +
  geom_point() + 
  geom_line(data=group_nconsumers, aes(group=algorithm), alpha=0.2) +
  labs(
    y="Average Relative Deviation from minimum number of consumers", 
    x="Delta"
  ) +
  theme_bw()
ggsave(filename=here("generated_plots/relative.png"))


filtered_nconsumers <- group_nconsumers %>% 
  filter(algorithm %in% c("mwf", "mbf", "mbfp", "mwfp", "bfd")) #, "mbfp", "mbf", "mwf", "mwfp"))
nalgorithms <- filtered_nconsumers %>% as.data.frame() %>%
  summarize(algorithm) %>% distinct() %>% nrow()
ggplot(filtered_nconsumers, aes(x=file, y=avg_diff_to_min, shape=algorithm)) + 
  scale_shape_manual(values=1:nalgorithms) +
  geom_point() + 
  geom_line(data=filtered_nconsumers, aes(group=algorithm), alpha=0.2) +
  labs(
    y="Average Relative Deviation from minimum number of consumers", 
    x="Delta"
  ) + 
  theme_bw()
ggsave(here("generated_plots/filtered_relative.png"))



rscore_avg <- algorithms %>% 
  group_by(file, algorithm) %>% 
  summarize(avg_rscore=mean(Rscore_Algorithm_Capacity))

nalgorithms <- rscore_avg %>% as.data.frame() %>% summarize(algorithm) %>% distinct() %>% nrow()
ggplot(rscore_avg, aes(x=file, y=avg_rscore, shape=algorithm)) + 
  scale_shape_manual(values=1:nalgorithms) + 
  geom_point() + geom_line(data=rscore_avg, aes(group=algorithm), alpha=0.2) + 
  labs(
    y="Average Rscore", 
    x="Delta"
  ) + 
  theme_bw()
ggsave(here("generated_plots/rscore.png"))