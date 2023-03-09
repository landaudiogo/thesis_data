here::i_am("Consumer Capacity/consumer_capacity_corrido.R")

library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(tidyquant)
library(magrittr)
library(here)
library(ggpattern)

dir.create(here("Consumer Capacity", "generated_plots"), showWarnings=FALSE)

test1 <- read.csv(file=here("Consumer Capacity", "test1.csv"))
colnames(test1) <- c(
  "bytes", 
  "time_total", 
  "time_gather",
  "time_process", 
  "time_bq", 
  "rate", 
  "rows", 
  "tables"
)
test1$rate <- as.numeric(gsub(",", "", test1$rate))
test1$test <- "test1"

test2 <- read.csv(file=here("Consumer Capacity", "test2.csv"))
colnames(test2) <- c(
  "bytes", 
  "time_total", 
  "time_gather",
  "time_process", 
  "time_bq", 
  "rate", 
  "rows", 
  "tables"
)
test2$rate <- as.numeric(gsub(",", "", test2$rate))
test2$test <- "test2"

test3 <- read.csv(file=here("Consumer Capacity", "test3.csv"))
colnames(test3) <- c(
  "bytes", 
  "time_total", 
  "time_gather",
  "time_process", 
  "time_bq", 
  "rate", 
  "rows", 
  "tables"
)
test3$rate <- as.numeric(gsub(",", "", test3$rate))
test3$test <- "test3"

citerations <- union(test1, test2)
citerations <- union(citerations, test3)

dens1 <- density(test1$rate)
mode1 <- dens1$x[which(dens1$y == max(dens1$y))]

dens2 <- density(test2$rate)
mode2 <- dens2$x[which(dens2$y == max(dens2$y))]

dens3 <- density(test3$rate)
mode3 <- dens3$x[which(dens3$y == max(dens3$y))]

densciterations <- density(citerations$rate)
mode <- densciterations$x[which(densciterations$y == max(densciterations$y))]

one_decimal_place <- function(x) sprintf("%.1f", x/1000000)
citerations %>% 
  filter(bytes >= 5000000) %>% 
  ggplot(aes(rate, linetype=test)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted")) + 
  scale_x_continuous(labels=one_decimal_place)+
  geom_density(size=1) + 
  geom_vline(xintercept=mode1) + 
  geom_vline(xintercept=mode2) + 
  geom_vline(xintercept=mode3) +
  labs(
    x="Rate (Mbytes/s)",
    y="Density"
  ) +
  theme_bw() + 
  theme(text = element_text(size = 18))
ggsave(here("Consumer Capacity/generated_plots", "density.pdf"))
