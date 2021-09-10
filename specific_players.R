library(tidyverse)

# Stephen Curry's Progress
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


# Stephen Curry vs James Harden
curry_harden <- nba_data_historical %>%
  filter(name_common %in% c("Stephen Curry", "James Harden")) %>%
  rename(TS = "TS%") %>%
  rename(raptorO = "Raptor O") %>%
  group_by(name_common, year_id) %>%
  summarize(TS = mean(TS, na.rm = TRUE), 
            raptorO = mean(raptorO, na.rm = TRUE)) %>%
  arrange(year_id)

## Base R
plot(TS ~ year_id, 
     col = "blue",
     data = curry_harden[curry_harden$name_common == "Stephen Curry", ])
regression_curry <- lm(TS ~ year_id, 
                       data = curry_harden[curry_harden$name_common 
                                           == "Stephen Curry", ])
abline(regression_curry)
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

## ggplot
ggplot(data = curry_harden, mapping = aes(x = year_id, y = TS)) +
  geom_point(mapping = aes(color = name_common)) +
  geom_smooth(method = "lm", 
              mapping = aes(color = name_common), 
              se = FALSE) +
  theme_light()

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

curry_harden_diff <- curry_harden %>%
  group_by(year_id) %>%
  summarize(TS_diff = TS[name_common == "James Harden"] - 
              TS[name_common == "Stephen Curry"], 
            raptorO_diff = raptorO[name_common 
                                   == "James Harden"] - 
              raptorO[name_common == "Stephen Curry"])

ggplot(data = curry_harden_diff, 
       mapping = aes(x = year_id, y = TS_diff)) +
  geom_bar(stat = "identity") +
  theme_light() +
  scale_x_continuous(breaks = pretty(2010:2020, n = 11)) +
  labs(x = "Year", 
       y = "Harden TS% - Curry TS%", 
       title = "James Harden vs. Stephen Curry: True Shooting", 
       fill = "Player", 
       caption = "Source: fivethirtyeight")

ggplot(data = curry_harden_diff, 
       mapping = aes(x = year_id, y = raptorO_diff)) +
  geom_bar(stat = "identity") +
  theme_light() +
  scale_x_continuous(breaks = pretty(2010:2020, n = 11)) +
  labs(x = "Year", 
       y = "Harden's Raptor O - Curry's Raptor O", 
       title = "James Harden vs. Stephen Curry", 
       fill = "Player", 
       caption = "Source: fivethirtyeight")

new_curry_harden_diff <- curry_harden_diff %>%
  gather(key = type_offense, 
         value = offense, 
         TS_diff:raptorO_diff)

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
