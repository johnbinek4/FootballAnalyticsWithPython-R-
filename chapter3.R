# Chapter 3 Simple Linear Regression: Rushing Yards Over Expected

library(tidyverse)
library(nflfastR)

pbp_r <- load_pbp(2016:2024)

pbp_r_run <-
  pbp_r |>
  filter(play_type == "run" & !is.na(rusher_id)) |>
  mutate(rushing_yards = ifelse(is.na(rushing_yards), 0, rushing_yards))

ggplot(pbp_r_run, aes(x = ydstogo, y = rushing_yards)) + 
  geom_point() + 
  theme_bw() +
  stat_smooth(method = "lm")

# Is this not explained by defense playing farther off the ball due to the 
# threat of the offense passing? 

pbp_r_run_avg <-
  pbp_r_run |>
  group_by(ydstogo) |>
  summarise(ypc = mean(rushing_yards))

ggplot(pbp_r_run_avg, aes(x = ydstogo, y = ypc)) +
  geom_point() + 
  theme_bw() +
  stat_smooth(method = "lm")
  
yard_to_go_r <-
  lm(rushing_yards ~ 1 + ydstogo, data = pbp_r_run)

summary(yard_to_go_r)

pbp_r_run <-
  pbp_r_run |>
  mutate(ryoe = resid(yard_to_go_r))

# ------------ Who was the best in RYOE? --------------------

ryoe_r <-
  pbp_r_run |>
  group_by(season, rusher_id, rusher) |>
  summarise(
    n = n(), 
    ryoe_total = sum(ryoe),
    ryoe_per = mean(ryoe),
    yards_per_carry = mean(rushing_yards)
  ) |>
  arrange(-ryoe_total) |>
  filter(n > 50)

print(ryoe_r)

ryoe_r |>
  arrange(-ryoe_per)

print(ryoe_r)


# ------------ Is RYOE a better metric? --------------------

ryoe_now_r <-
  ryoe_r |>
  select(-n, -ryoe_total)

ryoe_last_r <-
  ryoe_r |>
  select(-n, -ryoe_total) |>
  mutate(season = season + 1) |>
  rename(ryoe_per_last = ryoe_per,
         yards_per_carry_last = yards_per_carry)

ryoe_lag_r <-
  ryoe_now_r |>
  inner_join(ryoe_last_r,
             by = c("rusher_id", "rusher", "season")) |>
  ungroup()

ryoe_lag_r |>
  select(yards_per_carry, yards_per_carry_last) |>
  cor(use = "complete.obs")

ryoe_lag_r |>
  select(ryoe_per, ryoe_per_last) |>
  cor(use = "complete.obs")


# Exercises

# 1. 

# 2. 

# 3. 

# 4. 