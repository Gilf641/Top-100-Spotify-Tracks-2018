#Load all the required libraries
library(rio)
library(tibble)
library(ggplot2)
library(dplyr)


#set the working directory
setwd("C:\\Users\\Rohan Shetty\\Documents\\Kaggle Datasets\\top-spotify-tracks-of-2018")

getwd()

#Read the csv file
spotify_data<- read.csv("top2018.csv")
spotify_data

head(spotify_data)

summary(spotify_data)

glimpse(spotify_data)

datacol<- as.factor(colnames(spotify_data))

#First step is to check whether there are any na values or not
for (i in datacol){
  
  print(sum(is.na(i)))
}
#so there are no NA values

#So we will do EDA with Artists as variable
str(unique(spotify_data$artists))
#so there are 70 levels or artists in this dataset
head(spotify_data)
tail(spotify_data)

#Variables available loudness speechiness acousticness instrumentalness etc
colnames(spotify_data)
#Energetic songs

Energetic_Ones<- spotify_data %>% 
  group_by(name, energy) %>% 
  arrange(desc(energy)) %>% select(name, energy, artists) %>% head(n=20) %>%
  ggplot(aes(x=artists,y= energy)) + geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Energetic Spotify Tracks 2018") + geom_text(mapping = aes(label = name)) + coord_flip()+
  labs(x= " Artists", y = "Energy Index") + scale_y_continuous(breaks = seq(0,1, 0.2))

Energetic_Ones
head(spotify_data)
#Insights gained
#1. Drake's Nice What.. has the highest energy index i.e 0.909
#2. Billie Ellish's Love... song has 0.296 energy index


spotify_data %>% group_by(artists) %>% 
  mutate(Overall_Index = (danceability + energy + acousticness)/3) %>% arrange(Overall_Index)

#Longest one
Longest_One<- spotify_data %>% group_by(name, duration_ms)  %>%
  arrange(desc(duration_ms)) %>% select(name, artists, duration_ms, mode) %>%
  head(n=20) %>% ggplot(aes(x=artists, y=duration_ms, fill = mode)) + geom_bar(stat= "identity", position = "dodge")+ 
  ggtitle("Longest Spotify Tracks 2018") + geom_text(mapping = aes(label = name,size = 5))+ coord_flip()+
  labs(x = "Artists", y= "Duration in MilliSeconds(ms)") + 
  scale_y_continuous(breaks = seq(0, 1000000, 50000)) + theme(axis.title = element_text(size=10)) +
  theme(axis.text.x = element_text(size=6)) + theme(plot.)
Longest_One

head(spotify_data)


#Average Duration
Average_One<- spotify_data %>% group_by(artists) %>% arrange(artists) %>%
  select(artists, duration_ms) %>% 
 # summarise(Avg_duration= sum())
Average_One  
tail(Average_One)


artists_name<-unique(spotify_data$artists) #so there are almost 70 different artists

#but in overall data there are almost 100 artists
spotify_data

str(artists_name)
 
names_in_order<- spotify_data %>% group_by(artists) %>% arrange(artists) %>% select(artists)
names_in_order

for (i in as.character(names_in_order)){
  count = 0
  if ( i  == artists ) {
    count= count+1
    
    print(i)
    print(count)
    }
}

Top_Artists_Duration<- spotify_data %>%
  filter(artists == "Drake" | artists == "Ed Sheeran"|artists == "Anne Marie"|artists == "Ariana Grande"|artists == "The Weeknd") %>% group_by(duration_ms) %>%
  arrange(desc(duration_ms)) %>% ggplot(aes(x=name, y=duration_ms, fill = mode))+
  geom_bar(stat = "identity", position = "dodge")+ theme_classic() +
  coord_flip() + geom_text(mapping= aes(label= artists, vjust = 1, hjust = 1))+
  scale_y_continuous(breaks = seq(0, 1000000, 90000)) + labs(title="Top 5 Artists",x= "Artist Name", y= "Duration In Milliseconds")

Top_Artists_Duration

Top_Artists_Danceablity<- spotify_data %>%
  filter(artists == "Drake" | artists == "Ed Sheeran"|artists == "Anne Marie"|artists == "Ariana Grande"|artists == "The Weeknd") %>% group_by(duration_ms) %>%
  arrange(desc(danceability)) %>% ggplot(aes(x=name, y=danceability, fill = mode))+
  geom_bar(stat = "identity", position = "dodge")+ theme_classic() +
  coord_flip() + geom_text(mapping= aes(label= artists, vjust = 1, hjust = 1)) +
  labs(title = "Danceability",x= "Artist Name", y= "Danceability Index")
Top_Artists_Danceablity


head(spotify_data,20)

Top_Artists_LI<- spotify_data %>%
  filter(artists == "Drake" | artists == "Ed Sheeran"|artists == "Anne Marie"|artists == "Ariana Grande"|artists == "The Weeknd") %>% group_by(duration_ms) %>%
  arrange(desc(liveness)) %>% ggplot(aes(x=name, y=liveness, fill = mode))+
  geom_bar(stat = "identity", position = "dodge")+ theme_classic() +
  coord_flip() + geom_text(mapping= aes(label= artists, vjust = 1, hjust = 1)) +
  labs(title = "Liveness",x= "Artist Name", y= "Liveness Index")
Top_Artists_LI

#Liveness
#Detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live.

Liveness<- spotify_data %>%
  arrange(desc(liveness)) %>% head(n=25) %>%
  ggplot(aes(x= name,y=liveness,fill=mode))+ geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  geom_text(color = "Yellow", aes(label= artists, vjust = 0.1,hjust = 1))+
  labs(title = "Top25 LI", caption = "Liveness Index detects the presence of an audience in the recording", x= "Artist", y= "Liveness Index")
Liveness


#what type of songs do spotify users listen the most
#valence: A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track.
#Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).

TopValence_Plot<- spotify_data %>% group_by(artists, valence, rank) %>% 
  arrange(rank) %>% head(n=20) %>% select(name, artists, valence, rank)
TopValence_Plot
Average_Valence<- sum(Valence_Plot$valence)/20
Average_Valence
#Average Valence of Top 20 spotify tracks is 0.48

High_ValencePlot<-spotify_data %>% arrange((valence)) %>% 
  select(rank, name, artists, valence) #%>% arrange(valence) %>% select() #%>% ggplot(aes(x=artists, y=valence, fill ))

#Rank Category
#Top25, 25 - 50, 50 - 75, 75 - 100
spotify_data$rank <- as.factor(spotify_data$rank)
spotify_data$rank_category<- ifelse(as.numeric(rank) < 25, "Top25",ifelse(rank > 25 | rank < 50, "25-50",ifelse(rank > 50| rank < 75, "50-75","75-100")))
colnames(spotify_data)

if(as.numeric(spotify_data$rank) < 25){
  spotify_data$rank_category ="Top25"
}


spotify_data$rank <- as.numeric(spotify_data$rank)

spotify_data$rank_category<-  sapply(rank, function(x) if(rank<=25) "Top25" else if (rank > 25 | rank < 50) "25 to 50" else "over 50")

str(spotify_data$rank)

#Resume Point 1 rank category and then plot it!

Top_Valence<- spotify_data %>%group_by(duration_ms) %>%
  arrange(desc(valence)) %>% head(n=20) %>%ggplot(aes(x=name, y=valence, fill = mode))+
  geom_bar(stat = "identity", position = "dodge")+ theme_classic() + 
  coord_flip() + geom_text(size = 3.25, color= "Red",aes(label= artists, vjust = 0.4, hjust = 0.95)) +
  scale_y_continuous(breaks = seq(0, 1, 0.08))+ theme(axis.title = element_text(size = 9)) +
  labs(title = "Top 20 Songs",x= "Artist Name", y= "Valence Index", caption = "Valence Index is a measure from 0.0 to 1.0 describing the musical positiveness") + theme(axis.text.x = element_text(size = 7, angle = 90))
Top_Valence

High_ValencePlot


#Predict one feature using another once you learn ML algos
