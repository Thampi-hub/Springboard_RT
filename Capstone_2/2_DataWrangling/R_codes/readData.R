#' ------ ASSUMPTION ------ 
#' Considering category_id==29 to be 'Nonprofits & Activism' for data 
#' of all countries (not just US). Only US has category name for id==29 currently 
#' in their JSON meta file

cat("\014")

# Load packages: ----------------------------------------------------------

library(rstudioapi)
library(tidyverse)
library(data.table)
library(stringr)
library(glue)
library(Hmisc)
library(jsonlite)
library(xlsx)
library(hms)


# Environment setup -------------------------------------------------------

rm(list = ls(all.names = TRUE))
source("C:/Users/nandi/OneDrive/Documents/Brain Station - Data Analysis/Kaggle Dataset/setup.R")
path = "C:/Users/nandi/OneDrive/Documents/Brain Station - Data Analysis/Kaggle Dataset/Trending YouTube video stats/"
setwd(path)

# List of country data/meta to read
country_data = list.files(glue("{path}/Original_data/"), pattern = "*.csv$") %>% 
  str_match_all("^[A-Z]+") %>% 
  unlist()

country_meta = list.files(glue("{path}/Original_data/"), pattern = "*.json$") %>% 
  str_match_all("^[A-Z]+") %>% 
  unlist()

# UDF for reading Country wise csv/JSON data:
readDT <- function(x, format){
  if(format=="json"){
    outlist = fromJSON(glue("{path}/Original_data/{x}_category_id.json"))
    out = data.table(category_id = outlist$items$id,
                     category    = outlist$items$snippet$title)
    
  } else if(format=="csv"){
    out = setDT(read_csv(glue("{path}/Original_data/{x}videos.csv"))) %>% suppressMessages()
    
  }
  cat(glue("{x}\n\n"))
  
  out[, Country := x][]
  return(out)
}


# Compiled JSON metadata  -------------------------------------------------

cat("\n\nLoading metadata for...\n")

all_metalist = lapply(country_meta, readDT, format="json")
names(all_metalist) = country_meta

all_meta = setDT(rbindlist(l=all_metalist)) %>% 
            mutate(category_id = as.numeric(category_id) ) %>% 
            # Based on assumption made at start of script.
            select(-Country) %>% distinct()
  
cat("...Completed!\n")

# Compiled CSV data: ------------------------------------------------------

cat("\n\nLoading data for...\n")

all_dtlist = lapply(country_data, readDT, format="csv")
names(all_dtlist) = country_data

all_data1 = setDT(rbindlist(l=all_dtlist)) %>% 
          merge(., all_meta, by=c("category_id"), all.x=TRUE)

cat("...Completed!\n")


# Data Pre-processing (Clean up): -----------------------------------------

cat("\n\nPreprocessing & cleaning data... ")

all_data1[, trending_dt := as.Date(trending_date, "%y.%d.%m")]   #Converted to Date var
all_data1[, publish_dt := as.POSIXct(publish_time, format="%Y-%m-%d %H:%M:%S")]  #Converted to Datetime var
all_data1[, publish_da := as.Date( substr(publish_time,1,10), format="%Y-%m-%d")]  #Date component
all_data1[, publish_tm := as_hms(publish_dt)]  #Time component
all_data1[, tags := str_replace_all(tags, '"', '')][tags=="[none]", tags := NA_character_ ]   #Removed double quotes in string - messes with data operations
all_data1[, tags_n := length(unique(tstrsplit(tags, "\\|"))), by=.(Country, video_id, title,  channel_title, category_id, category, trending_dt, publish_dt) ][
          is.na(tags), tags_n := 0]    #No. of tags per video
all_data1[, duration := trending_dt - publish_da]    #Duration video took to reach popularity
all_data1[, tags_category := case_when(tags_n==0 | is.na(tags_n) ~ "Missings tags",
                                       tags_n>0 & tags_n<=30 ~ "Upto 30 tags",
                                       tags_n>30 & tags_n<=60 ~ "30-60 tags",
                                       tags_n>60 ~ ">60 tags",
                                       TRUE ~ NA_character_)]
all_data1[, disabled_cat := case_when(ratings_disabled & comments_disabled ~ "Ratings & Comments Disabled",
                                      !ratings_disabled & comments_disabled ~ "Only comments",
                                      ratings_disabled & !comments_disabled ~ "Only Ratings",
                                      !ratings_disabled & !comments_disabled ~ "Comments & Ratings Enabled")]

# For Debugging duplicate record derivation:
# all_data1 = all_data1[order(Country, video_id, title, channel_title, category_id, category, trending_dt, publish_dt, -views, -comment_count, -likes, -dislikes)][
#     , mean_comm := mean(comment_count), by=.(Country, video_id, title, channel_title, category, category_id, trending_dt, publish_dt)][
#     , diff_comm := comment_count - mean_comm][
#       
#     , mean_likes := mean(likes), by=.(Country, video_id, title, channel_title, category, category_id, trending_dt, publish_dt)][
#     , diff_likes := likes - mean_likes][
#       
#     , mean_dislikes := mean(dislikes), by=.(Country, video_id, title, channel_title, category, category_id, trending_dt, publish_dt)][
#     , diff_dislikes := dislikes - mean_dislikes][
#       
#     , mean_views := mean(views), by=.(Country, video_id, title, channel_title, category, category_id, trending_dt, publish_dt)][
#     , diff_views := views - mean_views][]


all_data2 = all_data1[!grepl("^#.+", video_id)] %>% 
  group_by(Country, video_id, channel_title, category_id, trending_dt, publish_dt) %>% 
  
  # DUP-REC DERIVATION: select max values of likes, dislikes, views, comment_count from each group(i.e. dup-rec):
  mutate(max_likes_per_group = max(likes, na.rm=TRUE),
         max_dislikes_per_group = max(dislikes, na.rm=TRUE),
         max_views_per_group = max(views, na.rm=TRUE),
         max_comm_per_group = max(comment_count, na.rm=TRUE) ) %>% 
  select(-views,-likes,-dislikes,-comment_count) %>% 
  rename(views = max_views_per_group,
         likes = max_likes_per_group,
         dislikes = max_dislikes_per_group,
         comments = max_comm_per_group) %>% 
  select(Country, video_id, title, channel_title, category, category_id, trending_dt, publish_dt, publish_da, publish_tm, duration, views, likes, dislikes, comments, tags_n, tags, tags_category, description, comments_disabled, ratings_disabled, disabled_cat, video_error_or_removed, thumbnail_link) %>%
  distinct() %>% 
  ungroup() %>% 
  setDT()
  
# log-transformed popularity scores:
all_data2[, log_views := log(views+1)] 
all_data2[, log_likes := log(likes+1)] 
all_data2[, log_dislikes := log(dislikes+1)] 
all_data2[, log_comm := log(comments+1)] 


all_data3 = all_data2[order(video_id, -trending_dt), head(.SD, n=1), by=video_id][
                      , .(video_id,Country,channel_title,category_id,trending_dt,likes,dislikes,comments,views, videoID_fl="Y")] %>%
  
  merge(., all_data2, all.y=T, 
        by=c("video_id","Country","channel_title","category_id","trending_dt","likes","dislikes","comments","views") ) %>% 
  .[is.na(videoID_fl), videoID_fl:=""]


cat("Completed! ")
saveRDS(all_data3, file.path(path,"ALLvideos.rds"))
write_excel_csv(all_data3, file.path(path,"ALLvideos.csv"))
