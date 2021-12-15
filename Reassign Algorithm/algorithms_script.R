library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(tidyquant)

get_cols_idxs <- function(df, col_names) {
  return(which(names(df) %in% col_names))
}


algorithms <- read.csv(file="most_recent12.csv")
algorithms$file <- sub("^(\\w+)_(\\d+)", "\\2", algorithms$file)
algorithms$file <- as.numeric(algorithms$file)

algorithms %>% 
  group_by(algorithm, file) %>% 
  summarize(
    avg_rscore=mean(Rscore_Algorithm_Capacity), 
    sd_rscore=sd(Rscore_Algorithm_Capacity),
  ) %>%
  arrange(file, avg_rscore)

wider_rscore <- algorithms %>%
  summarize(file, iteration, algorithm, Rscore_Algorithm_Capacity) %>%
  pivot_wider(names_from=algorithm, values_from=Rscore_Algorithm_Capacity)
wider_rscore$min_Rscore <- apply(wider_rscore[, -get_cols_idxs(wider_rscore, c("file", "iteration"))], 1, FUN=min, na.rm=TRUE)
wider_rscore[, -get_cols_idxs(wider_rscore, c("file", "iteration"))] <- wider_rscore[, -get_cols_idxs(wider_rscore, c("file", "iteration"))] %>% 
  mutate_at(vars(-min_Rscore), funs(. - min_Rscore))
longer_rscore <- wider_rscore %>% pivot_longer(-c("file", "iteration", "min_Rscore"), names_to="algorithm", values_to="diff")
grouped_rscore <- longer_rscore %>% group_by(file, algorithm) %>% summarize(avg_diff=mean(diff)) # %>% filter(algorithm %in% c("bfd", "mbfp", "mbf", "mwf", "mwfp"))
ggplot(grouped_rscore, aes(x=file, y=avg_diff, color=algorithm)) + 
  geom_point() + 
  geom_line(data=grouped_rscore, aes(group=algorithm)) + 
  labs(title="RSCORE diff")

wider_nconsumers <- algorithms %>% 
  summarize(file, iteration, algorithm, Number.of.Consumers) %>%
  pivot_wider(names_from=algorithm, values_from=Number.of.Consumers)
wider_nconsumers[, -get_cols_idxs(wider_nconsumers, c("file", "iteration"))]
wider_nconsumers$min_consumers <- apply(wider_nconsumers[,-get_cols_idxs(wider_nconsumers, c("file", "iteration"))], 1, FUN=min, na.rm=TRUE)
wider_nconsumers[, -get_cols_idxs(wider_nconsumers, c("file", "iteration"))] <- wider_nconsumers[,-get_cols_idxs(wider_nconsumers, c("file", "iteration"))] %>% 
  mutate_at(vars(-min_consumers), funs(. - min_consumers)) 
longer_nconsumers <- wider_nconsumers %>% pivot_longer(!c("file", "iteration", "min_consumers"), names_to="algorithm", values_to="diff") 
group_nconsumers <- longer_nconsumers %>% group_by(file, algorithm) %>% summarize(avg_diff_to_min=mean(diff)) # %>% filter(algorithm %in% c("bfd", "mbfp", "mbf", "mwf", "mwfp"))

ggplot(group_nconsumers, aes(x=file, y=avg_diff_to_min, color=algorithm)) + 
  geom_point() + 
  geom_line(data=group_nconsumers, aes(group=algorithm)) +
  labs(title="nConsumers diff")

test <- algorithms %>% 
  group_by(file, algorithm) %>% 
  summarize(avg_rscore=mean(Rscore_Algorithm_Capacity))

ggplot(test, aes(x=file, y=avg_rscore, color=algorithm)) + 
  geom_point() + geom_line(data=test, aes(group=algorithm)) + 
  labs(title="average Rscore")
