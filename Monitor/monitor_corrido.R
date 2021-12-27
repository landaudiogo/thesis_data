setwd("C:/Users/DLA149/Documents/Thesis/Monitor")
here::i_am("monitor_corrido.R")

library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(tidyquant)
library(magrittr)
library(rjson)
library(gsubfn)
library(ggpubr)
library(here)

dir.create(here("generated_plots"), showWarnings=FALSE)

load_monitor_producer <- function(monitor_file, producer_file) {
  monitor_log <- fromJSON(file=monitor_file)
  producer_log <- fromJSON(file=producer_file)
  
  monitor <- data.frame()
  for (item in monitor_log) {
    monitor <- rbind(monitor, item)
  }
  colnames(monitor) <- c("speed", "timestamp")
  start_monitor <- min(monitor$timestamp)
  monitor$scope <- "monitor"
  
  producer <- data.frame()
  for (item in producer_log) {
    producer <- rbind(producer, item)
  }
  colnames(producer) <- c("speed", "timestamp")
  start_producer <- min(producer$timestamp)
  producer$scope <- "producer"
  
  if(start_producer < start_monitor) {
    start_time <- start_producer
  } else {
    start_time <- start_monitor
  }
  
  new_col <- monitor %>% 
    summarise(time=timestamp-start_time)
  monitor$time <- new_col$time/1000
  
  new_col <- producer %>%
    summarise(time=timestamp-start_time)
  producer$time <- new_col$time/1000
  measurements <- rbind(monitor, producer)
  return(measurements)
}

measurements_1 <- load_monitor_producer(here("monitor_t1.log"), here("producer_t1.log"))

ggplot(measurements_1, aes(x=time, y=speed, group=scope, linetype=scope)) + 
  geom_line() + 
  labs(
    x="Time (s)", 
    y="Measured Throughput (bits/s)"
  ) +
  theme_bw()
ggsave(filename=here("generated_plots/step.png"))

measurements_2 <- load_monitor_producer(here("monitor_t2.log"), here("producer_t2.log"))
producer2 <- measurements_2[measurements_2$scope == "producer", ]
monitor2 <- measurements_2[measurements_2$scope == "monitor", ]

ylim1 <- boxplot.stats(producer2$speed)$stats[c(1, 5)]

p1 <- ggplot(measurements_2, aes(x=time, y=speed, group=scope, linetype=scope)) + 
  geom_line() + 
  labs(
    x="Time (s)",
    y="Measured Troughput (bits/s)"
  ) + 
  theme_bw()

p2 <- ggplot(producer2, aes(y=speed, line_type=scope)) + 
  geom_boxplot(aes(x=150),width=250, alpha=0) + 
  geom_line(data=monitor2, aes(x=time, y=speed)) + coord_cartesian(ylim = ylim1*0.95) + 
  labs(
    x="Time(s)", 
    y="Measured Throughput (bits/s)"
  ) + 
  theme_bw()
ggsave(p1, filename=here("generated_plots/random_without_boxplot.png"))
ggsave(p2, filename=here("generated_plots/random_with_boxplot.png"))

ggarrange(p1, p2, width=c(1.5,1.5))
ggsave(filename=here("generated_plots/random_arrange.png"))