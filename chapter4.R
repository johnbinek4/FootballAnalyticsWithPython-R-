# Chapter 4: Multiple Regression: Rushing Yards Over Expected

# A lot of variables are important to control for

library(tidyverse)

demo_data_r <- tibble(down = c("first", "second"), ydstogo = c(10, 5))

model.matrix(~ ydstogo + down, data = demo_data_r)

model.matrix(~ ydstogo + down - 1, data = demo_data_r)

# EDA

library(nflfastR)

pbp_r <- load_pbp(2016:2022)

pbp_r_run <- pbp_r |>
  filter(play_type == "run" & !is.na(rusher_id) &
           !is.na(down) & !is.na(run_location)) |>
  mutate(rushing_yards = ifelse(is.na(rushing_yards),0,rushing_yards))


# want to plot with downs as integers

pbp_r_run <-
  pbp_r_run |>
  mutate(down = as.character(down))

ggplot(pbp_r_run, aes(x = rushing_yards)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(down), ncol = 2,
             labeller = label_both) + 
  theme_bw() + 
  theme(strip.background = element_blank())

pbp_r_run |>
  filter(ydstogo == 10) |>
  ggplot(aes(x = down, y = rushing_yards)) + 
  geom_boxplot() + 
  theme_bw()

# Simpson's Paradox

# Including a third variable changes the relationship between two others

ggplot(pbp_r_run, aes(x = yardline_100, y = rushing_yards)) + 
  geom_point(alpha = .25) + 
  stat_smooth(method = "lm") +
  theme_bw()

# yardline_100 is yards to go to the endzone btw
# So the diag is rushes that went for max yardage, and negative are losses

pbp_r_run |>
  group_by(yardline_100) |>
  summarize(rushing_yards_mean = mean(rushing_yards)) |>
  ggplot(aes(x = yardline_100, y = rushing_yards_mean)) + 
  geom_point() + 
  stat_smooth(method = "lm") + 
  theme_bw()

# yup that's not linear at all

ggplot(pbp_r_run, aes(run_location, rushing_yards)) + 
  geom_boxplot() + 
  theme_bw()

# the python doesn't order correctly, thankfully R does. 
n
# since we do see some difference here, we should keep in the model,
# if we didn't then it would be questionable to add any value

pbp_r_run |>
  group_by(score_differential) |>
  summarize(rushing_yards_mean = mean(rushing_yards)) |>
  ggplot(aes(score_differential, rushing_yards_mean)) + 
  geom_point() + 
  stat_smooth(method = "lm") + 
  theme_bw()

# since we see clear different here as well we will keep this in the model too

# now make model

pbp_r_run <-
  pbp_r_run |>
  mutate(down = as.character(down))

exp_yards_r <-
  lm(rushing_yards ~ 1 + down + ydstogo + down:ydstogo + yardline_100 + run_location +
        score_differential, data = pbp_r_run)

pbp_r_run <-
  pbp_r_run |>
  mutate(ryoe = resid(exp_yards_r))

print(summary(exp_yards_r))

library(broom)
install.packages("hms")
install.packages("kableExtra", dependencies = TRUE)
library(kableExtra)
exp_yards_r |>
  tidy(conf.int = TRUE) |>
  kbl(format = "pipe", digits = 2) |>
  kable_styling()

# kableExtra gone? overflow fix

# RYOE

ryoe_r <-
  pbp_r_run |>
  group_by(season, rusher_id, rusher) |>
  summarize(
    n = n(), ryoe_total = sum(ryoe), ryoe_per = mean(ryoe),
    yards_per_carry = mean(rushing_yards)
  ) |>
  filter(n > 50)

ryoe_r |>
  arrange(-ryoe_total) |>
  print()

ryoe_r |>
  filter(n > 50) |>
  arrange(-ryoe_per) |>
  print()

ryoe_now_r <-
  ryoe_r |>
  select(-n, -ryoe_total)

ryoe_lasat_r <-
  ryoe_r |>
  select(-n, -ryoe_total) |>
  mutate(season = season + 1) |>
  rename(ryoe_per_last = ryoe_per,
        yards_per_carry_last = yards_per_carry)

ryoe_lag_r <-
  ryoe_now_r |>
  inner_join(ryoe_lasat_r,
             by = c("rusher_id", "rusher", "season")) |>
  ungroup()

ryoe_lag_r |>
  select(yards_per_carry, yards_per_carry_last) |>
  cor(use = "complete.obs")

ryoe_lag_r |>
  select(ryoe_per, ryoe_per_last) |>
  cor(use = "complete.obs")

# ---------------------------------

par(mfrow = c(2, 2))
plot(exp_yards_r)

exp_yards_filter_r <-
  pbp_r_run |>
  filter(rushing_yards > 15 & rushing_yards < 90) |>
  lm(formula = rushing_yards ~ 1 + down + ydstogo + down: ydstogo +
       yardline_100 + run_location + score_differential)

par(mfrow = c(2,2))
plot(exp_yards_filter_r)


# Exercises -------------------

# 1. Change the carries threshold to 50 carries to 100 carries. Do you still see the stability
# differences that you found in this chapter?



# 2. Use the full nflfastR dataset to show that rushing is less efficient than passing,
# both using yards per play and EPA per play. Also inspect the variability of these play types



# 3. Is rushing more valuable than passing in some situations (near opposing endzone?)



# 4. Inspect James Conner's RYOE values for his career relative to Bell's. 
# What do you notice about the metric for both backs? 




# 5. Repeat the process in this chapter for receivers. Use down and distance, 
# but look to add other things such as air_yards. 
