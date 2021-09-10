# player progress charts
lebron_james_TS <- nba_data_historical %>%
  rename(TS = "TS%") %>%
  filter(name_common == "LeBron James") %>%
  group_by(year_id) %>%
  summarize(avg_TS = mean(TS, na.rm = TRUE))
ggplot(data = lebron_james_TS, mapping = aes(x = year_id, y = avg_TS)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = pretty(lebron_james_TS$year_id, n = 10)) +
  labs(x = "Year", 
       y = "True Shooting Percentage", 
       title = "LeBron James' True Shooting Percentage by Year",
       caption = "Source: fivethirtyeight")
# Note: for some reason, for pretty, you have to repeat the name
# of the data frame



kobe_bryant_TS <- nba_data_historical %>%
  rename(TS = "TS%") %>%
  filter(name_common == "Kobe Bryant") %>%
  group_by(year_id) %>%
  summarize(avg_TS = mean(TS, na.rm = TRUE))
ggplot(data = kobe_bryant_TS, mapping = aes(x = year_id, y = avg_TS)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = pretty(kobe_bryant_TS$year_id, n = 10)) +
  labs(x = "Year", 
       y = "True Shooting Percentage", 
       title = "Kobe Bryant's True Shooting Percentage by Year",
       caption = "Source: fivethirtyeight")


michael_jordan_TS <- nba_data_historical %>%
  rename(TS = "TS%") %>%
  filter(name_common == "Michael Jordan") %>%
  group_by(year_id) %>%
  summarize(avg_TS = mean(TS, na.rm = TRUE))
ggplot(data = michael_jordan_TS, mapping = aes(x = year_id, y = avg_TS)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 
                       pretty(michael_jordan_TS$year_id, n = 15)) +
  scale_y_continuous(breaks = 
                       pretty(michael_jordan_TS$avg_TS, n = 6)) +
  labs(x = "Year", 
       y = "True Shooting Percentage", 
       title = "Michael Jordan's True Shooting Percentage by Year",
       caption = "Source: fivethirtyeight")

kareem_TS <- nba_data_historical %>%
  rename(TS = "TS%") %>%
  filter(name_common == "Kareem Abdul-Jabbar") %>%
  group_by(year_id) %>%
  summarize(avg_TS = mean(TS, na.rm = TRUE))

shaq_TS <- nba_data_historical %>%
  rename(TS = "TS%") %>%
  filter(name_common == "Shaquille O'Neal") %>%
  group_by(year_id) %>%
  summarize(avg_TS = mean(TS, na.rm = TRUE))

magic_TS <- nba_data_historical %>%
  rename(TS = "TS%") %>%
  filter(name_common == "Magic Johnson") %>%
  group_by(year_id) %>%
  summarize(avg_TS = mean(TS, na.rm = TRUE))


# for all six (last three haven't had plots yet)
goats <- nba_data_historical %>%
  rename(TS = "TS%") %>%
  rename(raptorD = "Raptor D") %>%
  rename(raptorW = "Raptor WAR") %>%
  filter(MPG > 20, G > 10, 
         name_common %in% c("Kobe Bryant", "LeBron James", 
                            "Michael Jordan", "Shaquille O'Neal", 
                            "Kareem Abdul-Jabbar", "Magic Johnson")) %>%
  group_by(name_common, year_id) %>%
  filter(G > 10) %>%
  summarize(avg_TS = mean(TS, na.rm = TRUE), 
            avg_raptorD = mean(raptorD, na.rm = TRUE), 
            avg_raptorW = mean(raptorW, na.rm = TRUE))
# Note: %in% works while == doesn't, not sure why yet, considering
# that some values were still included when == was used
ggplot(data = goats, mapping = aes(x = year_id, y = avg_TS)) +
  geom_point(mapping = aes(color = name_common), size = 0.5) +
  geom_line(mapping = aes(color = name_common)) +
  theme_light() +
  scale_x_continuous(breaks = pretty(goats$year_id, n = 8)) +
  labs(x = "Year", 
       y = "True Shooting Percentage",
       title = "Comparison of NBA GOATs by True Shooting Percentage",
       caption = "Source: fivethirtyeight",
       color = "Name")

# with regression instead of line
ggplot(data = goats, mapping = aes(x = year_id, y = avg_TS)) +
  geom_point(mapping = aes(color = name_common)) +
  geom_smooth(method = "loess", aes(color = name_common), se = FALSE) +
  theme_light() +
  scale_x_continuous(breaks = pretty(goats$year_id, n = 8)) +
  labs(x = "Year", 
       y = "True Shooting Percentage",
       title = "Comparison of NBA GOATs by True Shooting Percentage",
       caption = "Source: fivethirtyeight",
       color = "Name")

goat_avgs <- goats %>%
  group_by(name_common) %>%
  summarize(avg_TS = mean(avg_TS), 
            avg_raptorD = mean(avg_raptorD), 
            avg_raptorW = mean(avg_raptorW)) %>%
  arrange(desc(avg_TS))


# GOATs' defensive raptor ratings together
ggplot(data = goats, mapping = aes(x = year_id, y = avg_raptorD)) +
  geom_jitter(mapping = aes(color = name_common)) +
  geom_smooth(method = "lm", 
              mapping = aes(color = name_common), 
              se = FALSE) +
  theme_light() +
  scale_x_continuous(breaks = pretty(goats$year_id, n = 8)) +
  labs(x = "Year", 
       y = "Defensive Raptor Rating", 
       title = "Comparison of NBA GOATs by Defensive Raptor Rating", 
       caption = "Source: fivethirtyeight", 
       color = "Name")


# GOATs' overall raptor ratings together
ggplot(data = goats, mapping = aes(x = year_id, y = avg_raptorW)) +
  geom_jitter(mapping = aes(color = name_common)) +
  geom_smooth(method = "lm", 
              mapping = aes(color = name_common), 
              se = FALSE) +
  theme_light() +
  scale_x_continuous(breaks = pretty(goats$year_id, n = 8)) +
  labs(x = "Year", 
       y = "Raptor WAR", 
       title = "Comparison of NBA GOATs by Raptor WAR", 
       caption = "Source: fivethirtyeight", 
       color = "Name")