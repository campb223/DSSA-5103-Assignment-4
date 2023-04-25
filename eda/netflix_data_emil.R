#libraries 
library(tidyverse)
library(ggeasy)
library (ggplot2)
library(knitr)
library(psych)
library(dplyr)
library(lubridate)
library(stringr)

#working directory
#setwd("~/Desktop/R Studio/TeamA")

#reading our dataset
netflix <- read_csv('data/netflix.csv', col_names = TRUE)

#viewing the data
dim(netflix) 
glimpse(netflix)
str(netflix)
summary(netflix)
attach(netflix)


#column names 
names(netflix)


#extracting the year out of the date_added date 
netflix$date_added <-format(as.Date(netflix$date_added, format="%d/%m/%Y"),"%Y")


# reduced listed_in column 
## remove the first string for values starting with "TV" or "Movie"
### netflix$listed_in <- gsub("^TV Show\\s*", "", netflix$listed_in)
### netflix$listed_in <- gsub("^Movies\\s*", "", netflix$listed_in)


## play with listed_in. I have kept the first string in the rows, which determines the genre
netflix$listed_in <- sapply(strsplit(netflix$listed_in, " "), `[`, 1)

netflix <- netflix[!grepl("^TV|^Movies", netflix$listed_in), ]


## documentary
netflix$listed_in <-gsub("(?i)Documentaries", "Documentary", netflix$listed_in)
netflix$listed_in <-gsub("(?i)Documentaries,", "Documentary", netflix$listed_in)
netflix$listed_in <-gsub("(?i)Docuseries,", "Documentary", netflix$listed_in)
netflix$listed_in <-gsub("(?i)Docuseries", "Documentary", netflix$listed_in)
                          
                     
                          
## drama

netflix$listed_in <-gsub("(?i)Dramas,", "Drama", netflix$listed_in)
netflix$listed_in <-gsub("(?i)Dramas", "Drama", netflix$listed_in)


## comedy

netflix$listed_in <-gsub("(?i)Comedies,", "Comedy", netflix$listed_in)
netflix$listed_in <-gsub("(?i)Comedies", "Comedy", netflix$listed_in)
netflix$listed_in <-gsub("(?i)Stand-Up", "Comedy", netflix$listed_in)

## international and british 

netflix$listed_in <-gsub("(?i)International,", "International", netflix$listed_in)
netflix$listed_in <-gsub("(?i)British", "International", netflix$listed_in)
netflix$listed_in <-gsub("(?i)Spanish-Language", "International", netflix$listed_in)

## kids and childern

netflix$listed_in <-gsub("(?i)Kids'", "Kids", netflix$listed_in)
netflix$listed_in <-gsub("(?i)Children", "Kids", netflix$listed_in)



print(unique(netflix$listed_in)) 

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
  count(genre,sort = TRUE)

#number of movies with the same duration 
movieduration <-movies %>%
  group_by(duration) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#number of shows with the same number of seassons 
showduration <-shows %>%
  group_by(duration) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Movies and TV Shows by genre
genre <- netflix %>%
  group_by(listed_in) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


# Movies and Shows by reduced category names 
category <- netflix %>%
  group_by(listed_in$dramas <-str_extract(genre$listed_in, "\\w+")) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

genres <- genre$category <-str_extract(genre$listed_in, "\\w+")
print(unique(genres))

genrefreq <- data.frame(table(dramas$listed_in))

ggplot()

ggplot(netflix, aes(x = release_year, fill = Type)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.5) +
  scale_fill_manual(values = c("#FF9933", "#3366CC")) +
  labs(title = "Number of Movies and TV Shows Released by Year", 
       x = "Release Year", y = "Count") +
  theme_bw()

# releases by year
releasse <- netflix %>%
  group_by(release_year) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# number of releases by country
country <-netflix %>%
  select(country) %>%
  count(country) %>%
  arrange(desc(n)) %>%
  View()


origin <-view(sort(table(country), decreasing = TRUE))
barplot(sort(table(country), decreasing = TRUE))
barplot(sort(table(country), decreasing = FALSE), horiz = TRUE, 
        main = "Number of Observations by Country", 
        xlab = "Number of Observations", ylab = "Country",
        las = 1)


#checking for missing values 
sum(is.na(netflix))

# convert the "release_year" column to a numeric vector
netflix$release_year <- as.numeric(netflix$release_year)

# plot a histogram of the "released" column
hist(netflix$release_year, main = "Year", xlab = "Year", breaks = 45)

movies <-sum(netflix == "Movie")
shows <-sum(netflix == "TV Show")

print(movies)
print(shows)

# Year of release 
ggplot(netflix, aes(x=movies, fill=shows)) + 
  geom_histogram(alpha=0.5, binwidth = 2, position = "identity") +
  geom_histogram(aes(x=shows, fill=movies), alpha=5, binwidth = 200, position = "identity") +
  labs(title="Movies vs Shows on Netflix", x="Value", y="Frequency", fill=NULL)



