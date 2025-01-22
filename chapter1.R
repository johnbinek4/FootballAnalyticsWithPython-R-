install.packages("nflfastR")

library("tidyverse")
library("nflfastR")

pbp_r22 <- load_pbp(2022)
pbp_r23 <- load_pbp(2023)
pbp_r24 <- load_pbp(2024)


pbp_r_pass22 <- pbp_r22 |>
  filter(play_type == "pass" & !is.na(air_yards))

pbp_r_pass23 <- pbp_r23 |>
  filter(play_type == "pass" & !is.na(air_yards))

pbp_r_pass24 <- pbp_r24 |>
  filter(play_type == "pass" & !is.na(air_yards))  

pbp_r_pass22 |>
  group_by(passer_id, passer) |>
  summarize(n = n(), adot = mean(air_yards)) |>
  filter(n >= 100 & !is.na(passer)) |>
  arrange(-adot) |>
  print(n = Inf)

pbp_r_pass23 |>
  group_by(passer_id, passer) |>
  summarize(n = n(), adot = mean(air_yards)) |>
  filter(n >= 100 & !is.na(passer)) |>
  arrange(-adot) |>
  print(n = Inf)

pbp_r_pass24 |>
  group_by(passer_id, passer) |>
  summarize(n = n(), adot = mean(air_yards)) |>
  filter(n >= 100 & !is.na(passer)) |>
  arrange(-adot) |>
  print(n = Inf)

# adot (average depth of target) is commonly used
# used to represent a qbs aggression, but there are a
# few other alternatives that better represent this.

# Maybe we consider the wr seperation from
# defender? The tighter the throw the more
# aggressive the qb.

# Also, we can look at adot with respect to
# down and distance. A qb throwing 15 yards
# downfield on its own is no big deal, but
# to do so when the team needs 2 yards on
# a third down, suggests the qb has aggression.

# My issue with adot alone is that it does not
# capture the game state and may only lead
# to highter numbers for qb forced to throw
# deep and repeatedly when down in the second
# half of a football game.

# Let's try and get these metrics now...

# Factor Down&Distance and WR separation

print(pbp_r_pass24)

# Add a new column for down-and-distance aggression
pbp_r_pass24 <- pbp_r_pass24 |>
  mutate(
    down_distance_aggression = ifelse(air_yards > (ydstogo + 10), 1, 0)
  )

# Summarize QB aggression metrics
qb_aggression <- pbp_r_pass24 |>
  group_by(passer_id, passer) |>
  summarize(
    n = n(),
    avg_adot = mean(air_yards, na.rm = TRUE),
    avg_down_distance_aggression = mean(down_distance_aggression, na.rm = TRUE)
  ) |>
  filter(n >= 100, !is.na(passer)) |>
  arrange(desc(avg_down_distance_aggression), desc(avg_adot))

# Print the results
print(qb_aggression, n = Inf)

# The results are similar, which does surprise me a bit
# but this may also be from the same issues with bad
# teams trying to come back. I can continue to raise
# ydstogo with distance to see what changes.

# I would expect to see some good qbs at top of list
# Burrow, Allen, and Tua (due to Miami vertical attack)