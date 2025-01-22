# Chapter 2: Exploratory Data Analysis (EDA): Stable Versus Unstable QB Stats

library("tidyverse")
library("nflfastR")
library("ggthemes")

pbp_r_1624 <- load_pbp(2016:2024)

pbp_r_p_1624 <-
  pbp_r_1624 |>
  filter(play_type == "pass" & !is.na(air_yards))

pbp_r_p_1624 <-
  pbp_r_p_1624 |>
  mutate(pass_length_air_yards = ifelse(air_yards >= 20, "long", "short"),
         passing_yards = ifelse(is.na(passing_yards), 0, passing_yards))

pbp_r_p_1624 |>
  pull(passing_yards) |>
  summary()

pbp_r_p_1624 |>
  filter(pass_length_air_yards == "short") |>
  pull(passing_yards) |>
  summary()

pbp_r_p_1624 |>
  filter(pass_length_air_yards == "long") |>
  pull(passing_yards) |>
  summary()

# We can see how the numbers change for long or short passes
# within the quartile ranges

# We can look at epa for passes as well

pbp_r_p_1624 |>
  filter(pass_length_air_yards == "short") |>
  pull(epa) |>
  summary()

pbp_r_p_1624 |>
  filter(pass_length_air_yards == "long") |>
  pull(epa) |>
  summary()

# In general a longer pass can be seen as more valuable,
# but at a cost due to the larger likelihood of loss of down?

ggplot(pbp_r_1624, aes(x = passing_yards)) + 
  geom_histogram()

pbp_r_p_1624 |>
  filter(pass_length_air_yards == "long") |>
  ggplot(aes(passing_yards)) + 
  geom_histogram(binwidth = 1) +
  ylab("Count") + 
  xlab("Yards gained (lost) during long passing plays") + 
  theme_bw()

ggplot(pbp_r_p_1624, aes(x= pass_length_air_yards, y = passing_yards))+
  geom_boxplot() + 
  theme_bw() +
  xlab("Pass length in yards (long >= 20, short <20)") +
  ylab("Yards gained (lost) during a passing play")
  
pbp_r_p_s <-
  pbp_r_p_1624 |>
  group_by(passer_player_name, passer_player_id, season) |>
  summarize(
    ypa = mean(passing_yards, na.rm = TRUE),
    n = n(),
    .groups = "drop")

pbp_r_p_s |>
  arrange(-ypa) |>
  print()

# Need to filter a bit

pbp_r_p_100 <-
  pbp_r_p_1624 |>
  group_by(passer_id, passer, season) |>
  summarize(
    n = n(), ypa = mean(passing_yards),
    .groups = "drop") |>
  filter(n >= 100) |>
  arrange(-ypa)

pbp_r_p_100 |>
  print(n = 20)

# Is a qb good at throwing deep?

air_yards_r <-
  pbp_r_p_1624 |>
  select(passer_id, passer, season, pass_length_air_yards, passing_yards) |>
  arrange(passer_id, season, pass_length_air_yards) |>
  group_by(passer_id, passer, pass_length_air_yards, season) |>
  summarize(n = n(), ypa = mean(passing_yards),.groups = "drop") |>
  filter((n >= 100 & pass_length_air_yards == "short") |
           (n >= 30 & pass_length_air_yards == "long")) |>
  select(-n)

air_yards_lag_r <-
  air_yards_r |>
  mutate(season = season + 1) |>
  rename(ypa_last = ypa)

pbp_r_p_s_pl <-
  air_yards_r |>
  inner_join(air_yards_lag_r,
             by = c("passer_id", "pass_length_air_yards", "season", "passer"))

pbp_r_p_s_pl |>
  filter(passer %in% c("T.Brady", "A.Rodgers")) |>
  print(n = Inf)

pbp_r_p_s_pl |>
  distinct(passer_id) |>
  nrow()

# Scatterplot

scatter_ypa_r <-
  ggplot(pbp_r_p_s_pl, aes(x=ypa_last, y = ypa)) + 
  geom_point() + 
  facet_grid(cols = vars(pass_length_air_yards)) + 
  labs(
    x = "Yards per Attempt, Year n",
    y = "Yards per Attempt, Year n+1") +
  theme_bw() + 
  theme(strip.background = element_blank())+ 
  geom_smooth(method = "lm")


print(scatter_ypa_r)


pbp_r_p_s_pl |>
  filter(!is.na(ypa) & !is.na(ypa_last)) |>
  group_by(pass_length_air_yards) |>
  summarize(correlation = cor(ypa, ypa_last))



# Exercises



# 1. Create the same histograms as in "Histograms" on page 30 but for EPA per pass attempt

ggplot(pbp_r_1624, aes(x = epa)) + 
  geom_histogram()

# 2. Create the same boxplots as in "Histograms" on page 30 but for EPA per pass attempt

ggplot(pbp_r_p_s_pl, aes(x = pbp_r_p_s_pl["pass_length_air_yards"] == "long", y = epa)) + 
  geom_boxplot()

# 3. Perform same stability analysis but for EPA per pass attempt. Are results similar?
# So any players have similar YPA one year to the next but different EPA? 
# Why? 


# 4. Find a cutoff that equally splits data for long pass vs short pass stability
# Do the results stay the same? 


