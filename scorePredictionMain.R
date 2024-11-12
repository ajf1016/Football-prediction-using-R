# Load necessary libraries
library(readr) 
library(data.table)
library(tidyr) 
library(dplyr) 
library(ggplot2) 
library(ggthemes)
library(corrplot) 
library(lubridate) 
library(purrr) 
library(cowplot)
library(viridis)

# Read in data files
matches <- read.csv("WorldCupMatches.csv")
players <- read.csv("WorldCupPlayers.csv")
cups <- read.csv("WorldCups.csv")

# Check data structure
str(matches)

# Top 10 matches by attendance
top_attendance <- matches %>%
  arrange(desc(Attendance)) %>%
  select(Home.Team.Name, Away.Team.Name, Attendance) %>%
  head(n = 10)

# Average attendance by year
avg_attendance <- matches %>%
  filter(!is.na(Year), !is.na(Attendance)) %>%
  group_by(Year) %>%
  summarise(avy_att = mean(Attendance)) %>%
  as.data.frame()

# Plot average attendance over the years
ggplot(avg_attendance, aes(x = Year, y = avy_att)) +
  geom_line(color = "blue") +
  labs(title = "Average Attendance Over the Years",
       x = "Year", y = "Average Attendance") +
  theme_minimal()

# Total goals by year
total_goals <- matches %>%
  mutate(total_goals = Away.Team.Goals + Home.Team.Goals) %>%
  group_by(Year) %>%
  summarise(sum = sum(total_goals)) %>%
  as.data.frame()

# Plot total goals over the years
ggplot(total_goals, aes(x = Year, y = sum)) +
  geom_line(color = "green") +
  labs(title = "Total Goals Over the Years",
       x = "Year", y = "Total Goals") +
  theme_minimal()

# Home team win/draw/loss summary
matches1 <- na.omit(matches) %>%
  mutate(home_team_win = ifelse(Home.Team.Goals > Away.Team.Goals, "WIN",
                                ifelse(Home.Team.Goals < Away.Team.Goals, "LOSS", "DRAW")))

matches1_home <- matches1 %>%
  group_by(home_team_win) %>%
  summarise(sum = n()) %>%
  as.data.frame()

# Plot home team outcomes (win/draw/loss)
ggplot(matches1_home, aes(x = home_team_win, y = sum, fill = home_team_win)) +
  geom_bar(stat = "identity") +
  labs(title = "Home Team Outcomes (Win/Draw/Loss)",
       x = "Outcome", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("WIN" = "green", "DRAW" = "orange", "LOSS" = "red"))

# Distribution of home team goals
matches_home_goals <- matches1 %>%
  group_by(Home.Team.Goals) %>%
  summarise(sum = n()) %>%
  as.data.frame()

# Plot distribution of home team goals
ggplot(matches_home_goals, aes(x = Home.Team.Goals, y = sum)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribution of Home Team Goals",
       x = "Home Team Goals", y = "Frequency") +
  theme_minimal()

# Distribution of away team goals
matches_away_goals <- matches1 %>%
  group_by(Away.Team.Goals) %>%
  summarise(sum = n()) %>%
  as.data.frame()

# Plot distribution of away team goals
ggplot(matches_away_goals, aes(x = Away.Team.Goals, y = sum)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Distribution of Away Team Goals",
       x = "Away Team Goals", y = "Frequency") +
  theme_minimal()

# Top 20 home teams by wins
home_matches <- matches1 %>%
  group_by(Home.Team.Name, home_team_win) %>%
  summarise(sum = n()) %>%
  as.data.frame()

top_teams <- home_matches %>%
  filter(home_team_win == "WIN") %>%
  arrange(desc(sum)) %>%
  head(n = 20)

# Plot top 20 home teams by wins
ggplot(top_teams, aes(x = reorder(Home.Team.Name, -sum), y = sum, fill = Home.Team.Name)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 20 Home Teams by Wins",
       x = "Team", y = "Number of Wins") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")

# Coaches in finals
final <- matches %>%
  filter(Stage == "Final") %>%
  inner_join(players %>% select(MatchID, Coach.Name) %>% unique(), by = "MatchID")

final_coach <- final %>%
  group_by(Coach.Name) %>%
  summarise(n = n()) %>%
  as.data.frame()

# Plot coaches in finals
ggplot(final_coach, aes(x = reorder(Coach.Name, -n), y = n, fill = Coach.Name)) +
  geom_bar(stat = "identity") +
  labs(title = "Coaches Appearing in Finals",
       x = "Coach", y = "Number of Appearances") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none")

# Half-time goals by year (home vs. away)
halftime <- matches %>%
  group_by(Year) %>%
  summarise(home = sum(Half.time.Home.Goals, na.rm = TRUE),
            away = sum(Half.time.Away.Goals, na.rm = TRUE)) %>%
  na.omit()

halftime_long <- halftime %>%
  pivot_longer(cols = c(home, away), names_to = "type", values_to = "value")

# Plot half-time goals by year (home vs. away)
ggplot(halftime_long, aes(x = Year, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ type, scales = "free_y") +
  labs(title = "Half-Time Goals by Year (Home vs. Away)",
       x = "Year", y = "Goals") +
  theme_minimal() +
  scale_fill_viridis_d()
