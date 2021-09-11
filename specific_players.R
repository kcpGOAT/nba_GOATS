# Imports set of necessary packages, with these being ggplot2 and dplyr
library(tidyverse)


## This code transforms the original dataset into another one that only includes the yearly averages of the specified statistics for the player Stephen Curry.
curry_progress <- nba_data_historical %>%
  filter(name_common == "Stephen Curry", G > 10) %>%
  rename(TS = "TS%") %>%
  rename(raptorO = "Raptor O") %>%
  rename(raptorW = "Raptor WAR") %>%
  rename(AST = "AST%") %>%
  group_by(year_id) %>%
  summarize(TS = mean(TS, na.rm = TRUE), 
            raptorO = mean(raptorO, na.rm = TRUE),
            raptorW = mean(raptorW, na.rm = TRUE),
            AST = mean(AST, na.rm = TRUE)) 


## This bar chart depicts Stephen Curry's TS% over his NBA career. 
ggplot(data = curry_progress, 
       mapping = aes(x = year_id, y = TS - 60)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  scale_x_continuous(breaks = 
                       pretty(curry_progress$year_id, n = 10)) +
  scale_y_continuous(breaks = 
                       pretty(-3:4, n = 8)) +
  labs(x = "Year", 
       y = "True Shooting Percentage minus 60%", 
       title = "Stephen Curry's Progress as a Shooter: 2010-2019", 
       caption = "Source: fivethirtyeight")


## This code creates a data frame that contains the yearly averages for both Stephen Curry and James Harden. 
curry_harden <- nba_data_historical %>%
  filter(name_common %in% c("Stephen Curry", "James Harden")) %>%
  rename(TS = "TS%") %>%
  rename(raptorO = "Raptor O") %>%
  group_by(name_common, year_id) %>%
  summarize(TS = mean(TS, na.rm = TRUE), 
            raptorO = mean(raptorO, na.rm = TRUE)) %>%
  arrange(year_id)


## This graph depicts the TS% of Stephen Curry over the years using base R. 
plot(TS ~ year_id, 
     col = "blue",
     data = curry_harden[curry_harden$name_common == "Stephen Curry", ])
regression_curry <- lm(TS ~ year_id, 
                       data = curry_harden[curry_harden$name_common 
                                           == "Stephen Curry", ])
abline(regression_curry)


## Also using base R, this graph depicts the TS% of James Harden over the years on top of the previous plot. 
par(new = TRUE)
plot(TS ~ year_id, 
     col = "red",
     axes = FALSE,
     data = curry_harden[curry_harden$name_common 
                         == "James Harden", ])
regression_harden <- lm(TS ~ year_id, 
                        data = curry_harden[curry_harden$name_common 
                                            == "James Harden", ])
abline(regression_harden)


## Same thing as above but using ggplot2. 
ggplot(data = curry_harden, mapping = aes(x = year_id, y = TS)) +
  geom_point(mapping = aes(color = name_common)) +
  geom_smooth(method = "lm", 
              mapping = aes(color = name_common), 
              se = FALSE) +
  theme_light()


## This bar chart depicts the difference between Harden's TS% and Curry's TS% using two separate bars for each year, with each ear representing each player. 
ggplot(data = curry_harden, 
       mapping = aes(x = year_id, y = TS - 40)) +
  geom_bar(stat = "identity", 
           position = "dodge", 
           mapping = aes(fill = name_common)) +
  theme_light() +
  scale_x_continuous(breaks = pretty(2010:2020, n = 11)) +
  labs(x = "Year", 
       y = "True Shooting Percentage minus 50%", 
       title = "James Harden vs. Stephen Curry", 
       fill = "Player", 
       caption = "Source: fivethirtyeight")


## This code creates a data frame that contains the numerical difference between Harden's TS% and Curry's TS%, as well as a boolean that indicates whether or not
## the difference is positive or negative. 
curry_harden_diff <- curry_harden %>%
  group_by(year_id) %>%
  summarize(TS_diff = TS[name_common == "James Harden"] - 
              TS[name_common == "Stephen Curry"], 
            raptorO_diff = raptorO[name_common 
                                   == "James Harden"] - 
              raptorO[name_common == "Stephen Curry"]) %>%
  mutate(posTS = TS_diff > 0, posO = raptorO_diff > 0)


## This creates a bar chart that shows the aforementioned difference, with the color of the bar being based on whether the difference is positive or negative. 
ggplot(data = curry_harden_diff, 
       mapping = aes(x = year_id, y = TS_diff)) +
  geom_bar(stat = "identity", mapping = aes(fill = posTS)) +
  theme_light() +
  scale_x_continuous(breaks = pretty(2010:2020, n = 11)) +
  labs(x = "Year", 
       y = "Harden TS% - Curry TS%", 
       title = "James Harden vs. Stephen Curry: True Shooting", 
       fill = "Player", 
       caption = "Source: fivethirtyeight") +
  guides(fill = FALSE)


## This depicts the same thing as above but for Offensive RAPTOR Rating instead. 
ggplot(data = curry_harden_diff, 
       mapping = aes(x = year_id, y = raptorO_diff)) +
  geom_bar(stat = "identity", mapping = aes(fill = posO)) +
  theme_light() +
  scale_x_continuous(breaks = pretty(2010:2020, n = 11)) +
  labs(x = "Year", 
       y = "Harden's Raptor O - Curry's Raptor O", 
       title = "James Harden vs. Stephen Curry", 
       fill = "Player", 
       caption = "Source: fivethirtyeight") +
  guides(fill = FALSE)


## This data frame gathers the different offensive values into being categories of offense type, which is the new column, thereby enabling geom_bar to select 
## a fill based on offense type. 
new_curry_harden_diff <- curry_harden_diff %>%
  gather(key = type_offense, 
         value = offense, 
         TS_diff:raptorO_diff)


## This bar chart shows the difference between Harden and Curry based on both TS% and Offensive RAPTOR rating. 
ggplot(data = new_curry_harden_diff, 
       mapping = aes(year_id, offense)) +
  geom_bar(stat = "identity", 
           position = "dodge", 
           mapping = aes(fill = type_offense)) +
  theme_light() +
  scale_x_continuous(breaks = pretty(2010:2020, n = 11)) +
  labs(x = "Year", 
       y = "Harden's Rating - Curry's Rating", 
       title = "James Harden vs. Stephen Curry", 
       fill = "Type of Offensive Rating", 
       caption = "Source: fivethirtyeight")


## This part compares the top players of 2010 draft class
## and their performance during the regular season. 
best_2010_RS <- nba_data_historical %>%
  rename(TS = "TS%") %>%
  rename(raptorO = "Raptor O") %>%
  rename(raptorW = "Raptor WAR") %>%
  filter(G > 10, 
         name_common %in% c("John Wall", "DeMarcus Cousins", 
                            "Gordon Hayward", "Paul George"), 
         type == "RS") %>%
  group_by(name_common, year_id) %>%
  summarize(TS = mean(TS, na.rm = TRUE), 
            raptorO = mean(raptorO, na.rm = TRUE), 
            raptorW = mean(raptorW, na.rm = TRUE))

ggplot(data = best_2010_RS, mapping = aes(y = TS)) +
  geom_boxplot(mapping = aes(fill = name_common)) +
  theme_light() +
  labs(fill = "Player", 
       y = "True Shooting Percentage", 
       title = "Boxplot of top players among 2010 draft class and their TS%", 
       caption = "Source: fivethirtyeight") +
  theme(axis.text.x = element_blank())

ggplot(data = best_2010_RS, mapping = aes(year_id, TS)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red") +
  scale_x_continuous(breaks = pretty(2010:2020, n = 6)) +
  labs(y = "True Shooting Percentage", 
       title = "Career TS% among the top players of the 2010 draft class", 
       caption = "Source: fivethirtyeight") +
  facet_wrap(~name_common, nrow = 2) +
  theme_fivethirtyeight() +
  theme(axis.title.y = element_text(vjust = 2.5))


ggplot(data = best_2010_RS, mapping = aes(year_id, TS)) +
  geom_point(color = "blue") +
  geom_line(color = "red") +
  scale_x_continuous(breaks = pretty(2010:2020, n = 6)) +
  labs(y = "True Shooting Percentage", 
       title = "Career TS% among the top players of the 2010 draft class", 
       caption = "Source: fivethirtyeight") +
  facet_wrap(~name_common, nrow = 2) +
  theme_fivethirtyeight() +
  theme(axis.title.y = element_text(vjust = 2.5))

ggplot(data = best_2010_RS, mapping = aes(year_id, TS)) +
  geom_point(color = "blue") +
  geom_line(color = "red") +
  scale_x_continuous(breaks = pretty(2010:2020, n = 6)) +
  labs(y = "True Shooting Percentage", 
       title = "Career TS% among the top players of the 2010 draft class", 
       caption = "Source: fivethirtyeight") +
  facet_wrap(~name_common, nrow = 2) +
  theme_light() +
  theme(axis.title.y = element_text(vjust = 2.5))
