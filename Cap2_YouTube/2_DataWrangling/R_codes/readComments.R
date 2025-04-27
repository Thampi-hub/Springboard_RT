library(httr)
library(jsonlite)

# Your YouTube API key
api_key <- "AIzaSyAfl82IMY8v-XCgV-sN0V4MR_NqBMBsP7g"

# Function to get comments for a video
get_youtube_comments <- function(video_id, api_key, maxresult) {
  url <- paste0("https://www.googleapis.com/youtube/v3/commentThreads?part=snippet&videoId=", video_id, "&key=", api_key, "&maxResults=", maxresult)
  response <- GET(url)
  content <- content(response, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(content)
  
  # Extract comments
  
  outcomm = json_data$items$snippet$topLevelComment$snippet$textOriginal
  return(outcomm)
}

# Example: Get comments for a specific video ID
all_comments = list()
i = 1
for(vid in unique(all_data$video_id)[1:1000] ){
    comments <- get_youtube_comments(vid, api_key, 50) %>% list()
    i = i+1
    print(glue("Run: {i}"))
    all_comments = c(all_comments, comments)
}

# Print the comments
