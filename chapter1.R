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