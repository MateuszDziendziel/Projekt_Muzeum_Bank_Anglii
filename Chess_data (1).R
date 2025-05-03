#Preparing for work 
setwd("C:/Users/mateu/OneDrive/Pulpit/Projekty dodatkowe")
install.packages("dplyr")
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
chess_data_1 <- read.csv("C:/Users/mateu/OneDrive/Pulpit/Projekty dodatkowe/Chess FIDE Rankings.csv")
list <- list()
#First glimpse into the chess_data_1
glimpse(chess_data_1)

# At this point, it's important to mention that there are players registered under the FIDE federation.
# Since they do not represent any specific country, they will be excluded from the team-based analysis.
# However, they will still be needed for later stages of the analysis.
# Therefore, two datasets will be created: chess_data_1 (including FIDE players) and chess_data_2 (excluding them).

chess_data_2 <- chess_data_1 %>%
  filter(trimws(federation) != "FIDE")

# 1.1 How many titled players are there in chess_data, and how many belong to each title category?

list$chess_titles <- chess_data_1 %>%
  count(title) 

# The results show that all players in this dataset hold the title of Grandmaster.
# There are exactly 200 of them.


# 1.2 What are the average and median ELO ratings for players in this dataset?

list$avg_ELO <- chess_data_1 %>%
  summarize(average_ELO = mean(ELO))
list$avg_median <- chess_data_1 %>%
  summarize(median_ELO = median(ELO))

# We can see that the average ELO is higher than the median ELO.
# This suggests that there are some players with unusually high ratings
# that skew the average upward — although the difference isn't very large.


#Let's check that hypothesis by visualizing data 

ggplot(chess_data_1, aes(x = ELO)) + 
  geom_histogram(bins = 27, fill = "steelblue", color = "white") + 
  labs(title = "Distribution of ELO ranking", x = "ELO Ranking", y = "Frequency") + 
  theme_minimal() + 
  theme(
    axis.title.y = element_text(margin = margin(r = 15), face = "bold"),
    axis.title.x = element_text(margin = margin(t = 15), face="bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
# The hypothesis was correct — the histogram shows that a few players 
# have exceptionally high ratings. Moreover, the graph indicates that 
# most players are concentrated on the left side of the distribution. 
# The most frequent rating appears to be around 2620, with approximately 
# 25–27 players holding that rating


# 1.3 What are the highest and lowest ratings among these players, and what is the difference between them?


list$ELO_max <- chess_data_1 %>%
  summarize(max_ELO = max(ELO))
list$ELO_min <- chess_data_1 %>%
  summarize(min_ELO = min(ELO))

list$ELO_difference = list$ELO_max - list$ELO_min

#The highest ranking is 2864, the lowest one is 2608, the difference between them is 256.


# 1.4 Who is the youngest and who is the oldest player in this dataset?

list$Age_youngest <- chess_data_1 %>%
  summarize(Youngest = max(birth_year))

list$Age_oldest <- chess_data_1 %>%
  summarize(Oldest = min(birth_year))

list$Age_difference <- list$Age_youngest - list$Age_oldest 

list$Age_average <- chess_data_1 %>%
  summarize(Average_age = round(mean(birth_year)))

list$Age_median <- chess_data_1 %>%
  summarize(Median_age = median(birth_year))

# We can see that the youngest player was born in 2006 (19 years old) 
# and the oldest in 1965 (60 years old), with a 41-year age difference between them.
# Additionally, the average age of players in this dataset is 36 years (born in 1989), 
# while the median age is 35 years (born in 1990). This suggests that there are likely a few 
# very young players skewing the average downward. However, the difference between the average 
# and median is minimal, only 1 year.

#Let's visualize the age of players in this dataset using histogram

ggplot(chess_data_1, aes(x=birth_year)) + 
  geom_histogram(bins = 10, fill="steelblue", color="white") +
  labs(title = "Birth_year distribution", x = "Birth year", y = "Frequency") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(margin = margin(r = 15), face="bold"),
    axis.title.x = element_text(margin = margin(t = 15), face="bold")) 

# We can observe that most players were born in years around 1983-1992. 
# Based on this graph, it is also observable that there seems to be 
# more younger players than older ones among the best.



# Let's now analyze which federations have the most and the least players out of the 200 given, 
# and try to determine which federations could form the strongest 4-player team 
# based on their ELO ratings.


# 2.1 How many federations are in the chess_data_2, 
# and how many players out of these 200 are in each federation?


list$Federations <- chess_data_2 %>%
  count(federation) %>%
  rename( number_of_players = n)
  

# 2.2 Which federations are the most frequent among the players?

list$Fed_desc <- Federations %>%
   slice_max(number_of_players, n=5)
   
# 2.3 Which federations are the least frequent among the players?

list$Fed_asc <- Federations %>%
   slice_min(number_of_players, n=5)
 
# Now, let's filter for federations that have at least 4 players to fully complete a team.
# We are assuming that teams can only be created from players in this dataset. However, in reality,
# there could be a 201st player who might be the one to complete a 4-man team, which is not 
# accounted for in this analysis.

# 2.4 Which federations have the highest chance of winning?

list$filtered_chess_data <- chess_data_2 %>%
  group_by(federation) %>%
  filter(n() >=4) %>%
  slice_max(ELO, n=5) %>%
  summarize(average_team_elo = mean(ELO)) %>%
  arrange(desc(average_team_elo))

# We now know that there are only 16 federations that could form a 4-man team, 
# and among these 16, the United States, China, and the Russian Federation 
# have the highest chance of winning based on their rankings.

#Now let's put that on graph

list$top_5_filtered_chess_data <- filtered_chess_data %>%
  slice_max(average_team_elo, n=5) %>%
  arrange(desc(average_team_elo))

ggplot(top_5_filtered_chess_data, aes(x = federation, y = average_team_elo, fill = federation)) + 
  geom_col() +
  geom_text(aes(label = round(average_team_elo)), vjust = -0.5, size = 4) +
  coord_cartesian(ylim = c(2500, 2800)) +
  labs(
    title = "Federations with highest average team ELO", 
    x = "Federation name",
    y = "Average federation ELO") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 15), face="bold"),
    axis.title.x = element_text(margin = margin(t = 15), face="bold"))


  








