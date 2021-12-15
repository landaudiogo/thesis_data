library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(tidyquant)
library(magrittr)

get_cols_idxs <- function(df, col_names) {
  return(which(names(df) %in% col_names))
}

algorithms <- read.csv(file="uniform_start.csv")
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

ggplot(group_nconsumers, aes(x=file, y=avg_diff_to_min, color=algorithm)) + 
  geom_point() + 
  geom_line(data=group_nconsumers, aes(group=algorithm)) +
  labs(
    y="Average Relative Deviation from minimum number of consumers", 
    x="Delta"
  )


filtered_nconsumers <- group_nconsumers %>% 
  filter(algorithm %in% c("mwf", "mbf", "mbfp", "mwfp", "bfd")) #, "mbfp", "mbf", "mwf", "mwfp"))

ggplot(filtered_nconsumers, aes(x=file, y=avg_diff_to_min, color=algorithm)) + 
  geom_point() + 
  geom_line(data=filtered_nconsumers, aes(group=algorithm)) +
  labs(
    y="Average Relative Deviation from minimum number of consumers", 
    x="Delta"
  )



rscore_avg <- algorithms %>% 
  group_by(file, algorithm) %>% 
  summarize(avg_rscore=mean(Rscore_Algorithm_Capacity))

ggplot(rscore_avg, aes(x=file, y=avg_rscore, color=algorithm)) + 
  geom_point() + geom_line(data=rscore_avg, aes(group=algorithm)) + 
  labs(
    y="Average Rscore", 
    x="Delta"
  )