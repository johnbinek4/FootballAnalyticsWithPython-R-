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

# 1. What happens if you repeat the correlation analysis with 100
# carries as the threshold? What happens to the difference in r values?

ryoe_r_100_carries <- ryoe_r |> 
  filter(n > 100)

ryoe_lag_r_100 <- ryoe_now_r |> 
  inner_join(ryoe_last_r, by = c("rusher_id", "rusher", "season")) |> 
  filter(rusher_id %in% ryoe_r_100_carries$rusher_id) |> 
  ungroup()

cor_100_carries <- ryoe_lag_r_100 |>
  select(yards_per_carry, yards_per_carry_last) |>
  cor(use = "complete.obs")

cor_100_carries_ryoe <- ryoe_lag_r_100 |>
  select(ryoe_per, ryoe_per_last) |>
  cor(use = "complete.obs")

print(cor_100_carries)
print(cor_100_carries_ryoe)

# Nothing changed? 


# 2. Assume all of Alstott's carries were on third down and 1 yard to go, 
# while all of Dunn's carries came on first down and 10 yards to go. Is that
# enough to explain the discrepancy in their yards-per-carry values (3.7 vs 4.0)?
# Use the coefficient from the simple linear model in this chapter to understand the
# question. 

coef_yard_to_go_r <- coef(yard_to_go_r)

expected_yards_alstott <- coef_yard_to_go_r[1] + coef_yard_to_go_r[2] * 1
expected_yards_dunn <- coef_yard_to_go_r[1] + coef_yard_to_go_r[2] * 10

print(paste("Expected yards for Alstott: ", expected_yards_alstott))
print(paste("Expected yards for Dunn: ", expected_yards_dunn))

difference <- expected_yards_alstott - expected_yards_dunn
print(paste("Difference in expected yards: ", difference))

# There must be other factors outside the model at play here. 

# 3. What happens if you repeat the analysis in this chapter with yards to go to
# the endzone (yardline_100) as your feature?

yard_to_go_endzone_r <- lm(rushing_yards ~ 1 + yardline_100, data = pbp_r_run)
summary(yard_to_go_endzone_r)

pbp_r_run <- pbp_r_run |> 
  mutate(ryoe_endzone = resid(yard_to_go_endzone_r))

ryoe_endzone_r <- pbp_r_run |>
  group_by(season, rusher_id, rusher) |>
  summarise(
    n = n(), 
    ryoe_total = sum(ryoe_endzone),
    ryoe_per = mean(ryoe_endzone),
    yards_per_carry = mean(rushing_yards)
  ) |>
  arrange(-ryoe_total) |>
  filter(n > 50)  # Only include rushers with more than 50 carries

# Print the results
print(ryoe_endzone_r)

print("RYOE Results using yardline_100:")
print(ryoe_endzone_r)
print("RYOE Results using ydstogo:")
print(ryoe_r)

# 4. Repeat the process within this chapter with receivers and the passing game. 
# To do this, you have to filter by play_type == "pass" and receiver_id not 
# being NA or NULL.


pbp_r_q4 <- load_pbp(2016:2024)

pbp_r_pass <-
  pbp_r_q4 |>
  filter(play_type == "pass" & !is.na(receiver_id)) |>
  mutate(receiving_yards = ifelse(is.na(receiving_yards), 0, receiving_yards))

ggplot(pbp_r_pass, aes(x = ydstogo, y = receiving_yards)) + 
  geom_point() + 
  theme_bw() +
  stat_smooth(method = "lm")

# Is this not explained by defense playing farther off the ball due to the 
# threat of the offense passing? 

pbp_r_pass_avg <-
  pbp_r_pass |>
  group_by(ydstogo) |>
  summarise(ypc = mean(receiving_yards))

ggplot(pbp_r_pass_avg, aes(x = ydstogo, y = ypc)) +
  geom_point() + 
  theme_bw() +
  stat_smooth(method = "lm")

yard_to_go_r <-
  lm(receiving_yards ~ 1 + ydstogo, data = pbp_r_pass)

summary(yard_to_go_r)

pbp_r_pass <-
  pbp_r_pass |>
  mutate(ryoe = resid(yard_to_go_r))

# ------------ Who was the best in RYOE? --------------------

ryoe_r <-
  pbp_r_pass |>
  group_by(season, receiver_id, receiver) |>
  summarise(
    n = n(), 
    ryoe_total = sum(ryoe),
    ryoe_per = mean(ryoe),
    yards_per_reception = mean(receiving_yards)
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
         yards_per_reception_last = yards_per_reception)

ryoe_lag_r <-
  ryoe_now_r |>
  inner_join(ryoe_last_r,
             by = c("receiver_id", "receiver", "season")) |>
  ungroup()

ryoe_lag_r |>
  select(yards_per_reception, yards_per_reception_last) |>
  cor(use = "complete.obs")

ryoe_lag_r |>
  select(ryoe_per, ryoe_per_last) |>
  cor(use = "complete.obs")

# Thoughts?

# Suggested Readings are paywalled :(
# After seeing 2025 Season it would seem the league has changed. 

