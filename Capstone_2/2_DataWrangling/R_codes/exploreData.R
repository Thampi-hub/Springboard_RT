library(rstudioapi)
library(tidyverse)
library(data.table)
library(stringr)
library(glue)
library(Hmisc)
# library(janitor)
library(hms)
library(lubridate)
library(ggplot2)
library(corrplot)
library(patchwork)

cat("\014")
rm(list = ls(all.names = TRUE))
path = "C:/Users/nandi/OneDrive/Desktop/Brain Station - Data Analysis/Kaggle Dataset/Trending YouTube video stats"
source("C:/Users/nandi/OneDrive/Desktop/Brain Station - Data Analysis/Kaggle Dataset/setup.R")

dt = readRDS(file.path(path,"ALLvideos.rds"))

#' Data type: Char/Num/Date/Datetime/Boolean + Category/Discrete/Continuous
#' Basic stats: min, max, mean, quantiles, sd
#' Missing values & percentage
#' Use regex to find unusual values
#' ID vars

dt_len = format(nrow(dt), big.mark=",", scientific=F)
print(glue("Length of dataset = {dt_len}"))


# Descriptive stats per variable in data: ---------------------------------

# for(i in colnames(dt)){
  # coldata = unlist(dt[, ..i])
  # 
  # cat(glue("\n\n\n---------- {i} ----------\n\n"))
  # 
  # # Data Type
  # dt_type = class(coldata)
  # cat(glue("Data type: {dt_type}\n\n"))
  # 
  # # Descriptive stats
  # 
  # if( dt_type=="character" ){
  #   coldata = str_replace_all(coldata, '\"', "\'")
  #   descr_stat = c(
  #     Min = glue("{min(nchar(coldata), na.rm=T)} chars"),
  #     Max = glue("{max(nchar(coldata), na.rm=T)} chars"),
  #     Mean = glue("{round(mean(nchar(coldata), na.rm=T), 1)} chars"),
  #     Uniques = glue("{length(unique(coldata))} unique values")
  #   )
  # 
  #   cat("Descriptive stats:\n")
  #   print(descr_stat)
  # 
  # } else if( dt_type %in% c("integer","double","numeric") ){
  #     descr_stat = c(Min  = min(coldata, na.rm=T),
  #                    Max  = max(coldata, na.rm=T),
  #                    Q1   = unname(quantile(coldata,0.25,na.rm=T)),
  #                    Q3   = unname(quantile(coldata,0.75,na.rm=T)),
  #                    Mean = mean(coldata, na.rm=T),
  #                    Median = median(coldata, na.rm=T),
  #                    SD   = sd(coldata),
  #                    Skew = e1071::skewness(coldata, na.rm=T) )
  #     descr_stat = c(descr_stat,
  #                    Outliers_perc = length(coldata[coldata<(descr_stat["Q1"] - 1.5*IQR(coldata,na.rm=T)) |
  #                                              coldata>(descr_stat["Q3"] + 1.5*IQR(coldata,na.rm=T))
  #                                              ])/length(coldata)*100
  #                    ) %>% round(2)
  # 
  #     cat("Descriptive stats:\n")
  #     print(descr_stat)
  # 
  # }
# }


### Histogram: Publishing time over hours & Day of the week
ggplot(dt) + 
  geom_histogram(aes(x=hour(publish_dt), colour="white"), bins=24) +
  scale_x_continuous(breaks = 0:24 ) +
  facet_wrap( ~ wday(publish_dt), scales="free" )
#' Commonly observed to have peaks on 15-17 (local time) with >4000 uploads.
#' Closer to the end of the weekend (Sun-Mon), the peak plateaus at ~3000-3500.
#' Many viewers are available and actively looking for content to watch after work or school hours, typically in the late afternoon and early evening.
#' Alternatively, 13-14 is also a good time period targeting those watchig YouTube over their lunch break.


### Histogram: Trending videos over the months (2017-11-14 to 2018-06-14)
ggplot(dt) + 
  geom_histogram(aes(x=month(trending_dt), colour="white"), bins=12) +
  scale_x_continuous(breaks = 1:12 ) +
  scale_y_continuous(breaks = seq(0,60000,10000),limits = c(0,60000) ) 
#' Roughly a uniform distribution of videos trending over the months. Nov 2017 and June 2018 has lesser considering data is available from/till mid of the month.


ggplot(dt, aes(x=tags_n, colour="white")) +
  geom_histogram(binwidth=5) +
  scale_x_continuous(breaks = seq(0,155,5), limits = c(0,160)) +
  stat_bin(binwidth = 5, geom = "text", aes(label = ..count..), vjust = -0.5)
  # facet_wrap( ~ Country, scales="free" )
#' Most trending videos tend to have up to 25 or 30 tags. More is not necessarily better. Seldom does videos have beyond 40 tags.


# Correlation between popularity scores:
dt[videoID_fl=="Y", .(Likes=likes, Dislikes=dislikes, Comments=comments, Views=views, "No. of tags"=tags_n, Duration=as.numeric(duration))] %>% 
  cor() %>% 
  # Plot the correlation matrix using corrplot
  corrplot(., method = "color", type = "upper", 
           tl.cex = 0.8, 
           tl.col = "black", 
           addCoef.col = "black",
           number.cex = 0.7, 
           col = colorRampPalette(c("blue", "white", "red"))(200))




dt2 = dt[videoID_fl=="Y", .(likes, dislikes, comments, views)]
pop_freq = data.frame(
    category = c("Responders", "Non-responders"),
    likes_val    = round(c( (sum(dt2$likes)/sum(dt2$views)), 1-(sum(dt2$likes)/sum(dt2$views)) )*100, 1),
    dislikes_val = round(c( (sum(dt2$dislikes)/sum(dt2$views)), 1-(sum(dt2$dislikes)/sum(dt2$views)) )*100, 1),
    comm_val     = round(c( (sum(dt2$comments)/sum(dt2$views)), 1-(sum(dt2$comments)/sum(dt2$views)) )*100, 1),
    stringsAsFactors = FALSE
  )

likes_plot = ggplot(pop_freq, aes(x = "", y = likes_val, fill = reorder(category, c(1,2)) )) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Likes",
       fill = "Category",
       y = "Value") +
  theme_void() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +  # Custom colors
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  annotate("text", x = 0, y = 0, label = paste0(pop_freq$likes_val[1], "%"), color = "black", size = 6)

dislikes_plot = ggplot(pop_freq, aes(x = "", y = dislikes_val, fill = reorder(category, c(1,2)) )) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Dislikes",
       fill = "Category",
       y = "Value") +
  theme_void() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +  # Custom colors
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  annotate("text", x = 0, y = 0, label = paste0(pop_freq$dislikes_val[1], "%"), color = "black", size = 6)

comm_plot = ggplot(pop_freq, aes(x = "", y = comm_val, fill = reorder(category, c(1,2)) )) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Comments",
       fill = "Category",
       y = "Value") +
  theme_void() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +  # Custom colors
  theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  annotate("text", x = 0, y = 0, label = paste0(pop_freq$comm_val[1], "%"), color = "black", size = 6)

piecharts = likes_plot + dislikes_plot + comm_plot + plot_layout(nrow=1)

