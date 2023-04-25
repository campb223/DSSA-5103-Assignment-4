library(tidyverse)
library(lubridate)
library(tidyquant)
library (ggplot2)
library(knitr)
library(dplyr)

#reading in our data (netflix)
netflix <- read_csv(file = "data/netflix.csv")


#getting overview of data
glimpse(netflix)
head(netflix)
str(netflix)
summary(netflix)

#column names 
names(netflix)

unique(netflix$date_added)
sum(is.na(netflix$date_added)) #date added doesn't really have any value as most are NA

unique(netflix$listed_in) #looking to see how many unique types 

#extracting the year out of the date_added date 
netflix$date_added <-format(as.Date(netflix$date_added, format="%d/%m/%Y"),"%Y")

netflix %>%
  group_by(date_added ) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

type <-netflix %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# df for all movies
movies <- filter(netflix, type == "Movie")
# df for all tv shows
shows <- filter(netflix, type == "TV Show")

#number of shows by country
showsbycountry <-shows %>%
  filter(type == "TV Show") %>%
  count(country, sort = TRUE)

#number of movies by country
moviesbycountry <-movies %>%
  filter(type == "Movie") %>%
  count(country, sort = TRUE)

#movies by genre
moviesbygenre <- movies %>%
  filter(type == "Movie") %>%
  count(listed_in,sort = TRUE)

#number of movies with the same duration 
movieduration <-movies %>%
  group_by(duration) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#number of shows with the same number of seasons 
showduration <-shows %>%
  group_by(duration) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Movies and TV Shows by genre
genre <- netflix %>%
  group_by(listed_in) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


#hist of movies & tv shows by year
ggplot(netflix, aes(x = release_year, fill = type)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.5) +
  scale_fill_manual(values = c("#FF9933", "#3366CC")) +
  labs(title = "Number of Movies and TV Shows Released by Year", 
       x = "Release Year", y = "Count") +
  theme_classic() +
  theme(axis.ticks = element_blank())

# releases by year
release <- netflix %>%
  group_by(release_year) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# number of releases by country
country <-netflix %>%
  select(country) %>%
  count(country) %>%
  arrange(desc(n)) %>%
  View()


# convert the "release_year" column to a numeric vector
netflix$release_year <- as.numeric(netflix$release_year)

# plot a histogram of the "released" column
hist(netflix$release_year, main = "Year", xlab = "Year", breaks = 45)



#################################
netflix_clean <- netflix %>%
  filter(director != "Not Given")

##top directors
netflix_clean %>%   group_by(director) %>%
  summarise(count = n()) %>%
  top_n(10, count) %>%
  ggplot(aes(x = reorder(director, count), y = count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_classic()+
  labs(title = "Top 10 Directors by Count", x = "Director", y = "Count")


##top genres 
netflix_clean %>% group_by(listed_in) %>%
  summarize(count = n()) %>%
  top_n(5, count) %>%
  ggplot(aes(x = reorder(listed_in, count), y= count)) +
  geom_col(fill= "maroon")+
  coord_flip()+
  theme_classic()+
  labs(title= "5 Most Popular Genres", x= "Genre")



unique(netflix_clean$rating)

## BY RATING
##not a kid friendly platform - primarily for adults use
netflix_clean %>% group_by(rating) %>%
  summarize(count=n()) %>%
  ggplot(aes(x=reorder(rating, count), y= count)) +
  geom_col()+
  theme_classic()+
  labs(title = "Ratings")
  

unique(netflix_clean$country)

#removing not given countries
netflix_clean_country <- netflix_clean %>%
  filter(country != "Not Given")

#top 10 countries by freq
top10_countries <- netflix_clean_country %>%
  count(country, sort = TRUE) %>%
  head(10)

#top 5 genre
top5_listed_in <- netflix_clean_country %>%
  count(listed_in, sort = TRUE) %>%
  head(5)

#merging
netflix_filtered <- netflix_clean_country %>%
  filter(country %in% top10_countries$country) %>%
  filter(listed_in %in% top5_listed_in$listed_in)

#this df could also be used
top10countries <- inner_join(netflix_clean_country, top10_countries, by= "country")

### 10 most active countries + genre 

ggplot(netflix_filtered, aes(x = country, fill = listed_in)) +
  geom_bar(position = "stack") +
  scale_fill_brewer(palette = "Set3") +
  labs(x = "Country", y = "Count", fill = "Genre") +
  ggtitle("Netflix Content by Genre and Country (Top 10)") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 1))+
  theme_classic()
  
  
