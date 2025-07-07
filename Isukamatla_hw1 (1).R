#This code is written by Abhigna Isukamatla on 2/9/2025
library(dplyr)
library(ggplot2)
library(lubridate)
spotify_data <- read.csv('C:\\Users\\iabhi\\Downloads\\spotify_history.csv')

grouped_data <- group_by(spotify_data, track_name)
summarised_data <- summarise(grouped_data, total_ms_played = sum(ms_played))
#descending order
arranged_data <- arrange(summarised_data, desc(total_ms_played))
#top 10 songs
top_10_songs <- slice(arranged_data, 1:10)
print(top_10_songs)

# what day of the year had the most songs played and time
most_songs_day <- spotify_data %>%
  group_by(ts) %>%
  summarise(songs_played = n()) %>%
  arrange(desc(songs_played)) %>%
  head(1)

print("Day with the Most Songs Played:")
print(most_songs_day)

#bargraph of Top 10 Most Listened to Albums
top_10_albums <- spotify_data %>%
  group_by(album_name) %>%
  summarise(total_ms_played = sum(ms_played)) %>%
  arrange(desc(total_ms_played)) %>%
  slice(1:10)
print(top_10_albums)
barplot(top_10_albums_vector, 
        xlab= "Album Name", 
        ylab ="Total Listening Time (milliseconds)", 
        main = "Top 10 Most Listened to Albums", 
        col = "blue", 
        las = 2, 
        cex.names = 0.8)
#bargraph of 
top_10_albums <- spotify_data %>%
  group_by(track_name) %>%
  summarise(total_ms_played = sum(ms_played)) %>%
  arrange(desc(total_ms_played)) %>%
  slice(1:10)
print(top_10_albums)
barplot(top_10_albums_vector, 
        xlab= "Track Name", 
        ylab ="Total Listening Time (milliseconds)", 
        main = "Top 10 Most Listened to Tracks", 
        col = "blue", 
        las = 2, 
        cex.names = 0.8)

#have trouble producing graphs. Tried many things but nothing seemed to work.
#Took my cousin's help to code the of the year and time that had most songs.
#Finding the top 10 listened to songs was easy. 
#Because I'm still learning R, I still don't know completely what some operators like "%>%" do or what the new liabraries i downloaded do