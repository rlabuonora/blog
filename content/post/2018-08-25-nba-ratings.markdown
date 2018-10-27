---
title: NBA Ratings
author: Rafa
date: '2018-08-25'
slug: nba-ratings
categories: []
tags: []
draft: true
---

Explorando los ratings de la NBA.

TODO: Pasarlo a Plotly. Poner los dos graficos en un solo panel con gridExtra.


```r
library(glue)
library(tidyverse)
library(rvest)



get_ratings_data <- function(year) {
  base_url <- "https://www.basketball-reference.com/leagues/"
  ratings_template_url <- "NBA_{year}_ratings.html"
  df <- glue(base_url, ratings_template_url) %>% 
    read_html %>% 
    html_table %>% 
    `[[`(1)
  
  colnames(df) <- df[1,]
  df <- df[-1,]
  
  df %>% 
    mutate_at(vars(W:`NRtg/A`), as.numeric) %>% 
    mutate_at(vars(Team, Conf, Div), factor) 
}

rt.17 <- get_ratings_data("2017")
fit <- lm(W~ORtg + DRtg, data = rt.17)

g_off <- ggplot(rt.17, aes(ORtg, W, color = Conf)) + 
  geom_point() + 
  geom_smooth(inherit.aes = FALSE, aes(x=ORtg, y=W)) + 
  labs(title = "Offensive Rating vs Wins")

#ggplotly(g_off)
g_off
```

<img src="2018-08-25-nba-ratings_files/figure-html/unnamed-chunk-1-1.png" width="672" />

```r
g_def <- ggplot(rt.17, aes(DRtg, W, color = Conf)) + 
  geom_point() + 
  geom_smooth( inherit.aes = FALSE, aes(x=DRtg, y=W)) + 
  labs(title = "Defensive Rating vs Wins")
 g_def
```

<img src="2018-08-25-nba-ratings_files/figure-html/unnamed-chunk-1-2.png" width="672" />

