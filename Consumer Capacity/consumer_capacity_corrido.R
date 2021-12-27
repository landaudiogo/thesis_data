setwd("C:/Users/DLA149/Documents/Thesis/Consumer Capacity")
here::i_am("consumer_capacity_corrido.R")

library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(tidyquant)
library(magrittr)
library(here)
library(ggpattern)

dir.create(here("generated_plots"), showWarnings=FALSE)

test1 <- read.csv(file="test1.csv")
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

test2 <- read.csv(file="test2.csv")
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

test3 <- read.csv(file="test3.csv")
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

citerations %>% 
  filter(bytes >= 5000000) %>% 
  ggplot(aes(rate, linetype=test)) +
  scale_linetype_manual(values=c("solid", "dashed", "dotted")) + 
  geom_density(size=1) + 
  geom_vline(xintercept=mode1) + 
  geom_vline(xintercept=mode2) + 
  geom_vline(xintercept=mode3) + theme_bw()
ggsave(here("generated_plots/density.png"))
