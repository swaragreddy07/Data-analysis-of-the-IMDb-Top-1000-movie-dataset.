
install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("tidyr")

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)


installed.packages()

data <- read_csv("./imdb_top_1000.csv")
head(data)


#1 : Checking for the null values in each of the columns
missing_by_column <- colSums(is.na(data))
print(missing_by_column)

#2 : Omitting values which have multiple values
data_clean <- na.omit(data)
head(data_clean)
num_rows <- nrow(data_clean)
print(num_rows)


# First we convert the Column Released_Year into numerical value
data_clean$`Released_Year` <- as.numeric(data_clean$`Released_Year`)

# Then we use the ggplot library to generate the histogram 
ggplot(data_clean, aes(x = `Released_Year`)) + 
geom_histogram(binwidth = 1.5, fill = "white", color = "red") +
labs(title = "Histogram depicting movies according to their release year", x = "Release Year", y = "No. of Movies") +
theme_minimal()


#1 : Average rating grouped by name of genre

#Creating a longer version of the data where we create a separate row for each genre
# Separation done based on presence of commas in the data

data_clean_long <- data_clean %>%
  separate_rows(Genre, sep = ",\\s*")

average_rating_by_genre <- data_clean_long %>%
  group_by(Genre) %>%
  summarise(Rating_Avg = mean(IMDB_Rating, na.rm = TRUE)) %>%
  arrange(desc(Rating_Avg))

ggplot(average_rating_by_genre, aes(x = reorder(Genre, Rating_Avg), y = Rating_Avg)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = round(Rating_Avg, 1)), hjust = -0.2, color = "black") +  
  coord_flip() + 
  labs(title = "Average Rating of each Genre", x = "Genre", y = "Avg. Rating") +
  theme_minimal()

#2 : Average rating grouped by Metascore
#First we pull and calculate Average metascore based on Genre
Avg_Metascore_by_genre <- data_clean_long %>%
  group_by(Genre) %>%
  summarise(Avg_Metascore = mean(Meta_score, na.rm = TRUE)) %>%
  arrange(desc(Avg_Metascore))

#Using the ggplots we do the plots

ggplot(Avg_Metascore_by_genre, aes(x = reorder(Genre, Avg_Metascore), y = Avg_Metascore)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = round(Avg_Metascore, 1)), hjust = -0.2, color = "black") + 
  coord_flip() + 
  labs(title = "Average Metascore of each Genre", x = "Genre", y = "Avg. Metascore") +
  theme_minimal()


#Bar graph for the movies with least number of No_of_Votes
#First we fetch the list of the least voted movies 

least_voted_movies <- data_clean %>%
  arrange(No_of_Votes) %>%  
  head(10)

#Then we use ggplot to plot the least 10 voted movies

ggplot(least_voted_movies, aes(x = reorder(Series_Title, No_of_Votes), y = No_of_Votes)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = No_of_Votes), hjust = -0.2, color = "black") +  
  coord_flip() + 
  labs(title = "10 Movies with the Least Number of No_of_Votes", x = "Movie Title", y = "No_of_Votes") +
  theme_minimal()


#Plot to represent the names of movie directors with most number of movies
#First we get the list of directors with highest number of movies in the list

top_directors <- data_clean %>%
  group_by(Director) %>%
  summarise(Count_of_movies = n()) %>%  # Count the number of movies per director
  arrange(desc(Count_of_movies)) %>%  # Arrange in descending order of movie count
  head(10)

#We then use the ggplot to plot the graph for the same
ggplot(top_directors, aes(x = reorder(Director, Count_of_movies), y = Count_of_movies)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = Count_of_movies), hjust = -0.2, color = "black") +  
  coord_flip() + 
  labs(title = "Best 10 directors with most movies", x = "Director Name", y = "No. of Movies") +
  theme_minimal()


#1: Count of each IMDB rating 
#Display count of movies for each IMDB rating
#First we get the list of movies for each IMDB rating

count_of_rating <- data_clean %>%
  group_by(IMDB_Rating) %>%
  summarise(Count = n()) %>%  
  arrange(desc(Count)) 

#We then use the ggplot to plot the graph for the same

ggplot(count_of_rating, aes(x = IMDB_Rating, y = Count)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = Count), vjust = -0.5, color = "black") +  
  labs(title = "Count of Movies for each IMDb Rating", x = "IMDb Rating", y = "Count of Movies") +
  theme_minimal()

#2: Count of each genre
# Assuming the genre column is named `Genre
#WE have already the split the genre separated by commas
genre_counts <- data_clean_long %>%
  group_by(Genre) %>%
  summarise(Count = n()) %>%  
  arrange(desc(Count))  

print(genre_counts)

ggplot(genre_counts, aes(x = reorder(Genre, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +
  geom_text(aes(label = Count), hjust = -0.2, color = "black") +  
  coord_flip() +  
  labs(title = "Count of Movies by Genre", x = "Genre", y = "Count of Movies") +
  theme_minimal()


#Names of top fifty movies based on their IMDB ratings

top_50_movies <- data_clean %>%
  arrange(desc(IMDB_Rating)) %>%  
  select(Series_Title, IMDB_Rating) %>%  
  head(50)

print(top_50_movies)


#Distribution of runtimes of movies

# Remove " min" and convert to numeric the column for runtime in minutes
data_clean$Runtime <- as.numeric(gsub(" min", "", data_clean$Runtime))

ggplot(data_clean, aes(x = Runtime)) +
  geom_histogram(binwidth = 7.5, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Movie Runtimes", x = "Runtime (minutes)", y = "Number of Movies") +
  theme_minimal()


#Box-plot for IMDB rating comparing with Movie Certificate

ggplot(data_clean, aes(x = Certificate, y = IMDB_Rating)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box-plot for IMDB rating vs Movie Cert.", x = "Movie Certificate", y = "IMDB Rating") +
  theme_minimal()


#Grouping movies by duration in a released year and printing their respective directors, stars, runtime and series title

longest_movies_by_released_year <- data_clean %>%
  group_by(Released_Year) %>%
  filter(Runtime == max(Runtime, na.rm = TRUE)) %>%
  select(Released_Year, Series_Title, Director, Star1, Star2, Star3, Star4, Runtime)

print(longest_movies_by_released_year)
