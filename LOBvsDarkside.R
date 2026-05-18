install.packages("nflfastR")
library(nflfastR)
install.packages("tidyverse")
library(tidyverse)

pbp_2013 <- load_pbp(2013)
pbp_2025 <- load_pbp(2025)

sea_2013_def <- pbp_2013 %>%
  filter(defteam == "SEA")

sea_2025_def <- pbp_2025 %>%
  filter(defteam == "SEA")


# Create Game Results Table (2013)
games_2013 <- pbp_2013 %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%
  group_by(game_id) %>%
  summarize(
    home_team = last(home_team),
    away_team = last(away_team),
    home_score = last(home_score),
    away_score = last(away_score)
  )

# Create Game Results Table (2025)
games_2025 <- pbp_2025 %>%
  filter(!is.na(home_score), !is.na(away_score)) %>%
  group_by(game_id) %>%
  summarize(
    home_team = last(home_team),
    away_team = last(away_team),
    home_score = last(home_score),
    away_score = last(away_score)
  )

# Calculate 2013 Seahawks Points Allowed - 14.26
sea_2013_pa <- games_2013 %>%
  mutate(
    points_allowed = case_when(
      home_team == "SEA" ~ away_score,
      away_team == "SEA" ~ home_score
    )
  ) %>%
  filter(!is.na(points_allowed)) %>%
  summarize(avg_pa = mean(points_allowed))

# Calculate 2025 Seahawks Points Allowed - 16.9
sea_2025_pa <- games_2025 %>%
  mutate(
    points_allowed = case_when(
      home_team == "SEA" ~ away_score,
      away_team == "SEA" ~ home_score
    )
  ) %>%
  filter(!is.na(points_allowed)) %>%
  summarize(avg_pa = mean(points_allowed))

# Calculate 2013 League Average Points Allowed - 23.43
league_2013_pa <- games_2013 %>%
  summarize(
    avg_pa = mean(c(home_score, away_score))
  )

# Calculate 2025 League Average Points Allowed - 22.98
league_2025_pa <- games_2025 %>%
  summarize(
    avg_pa = mean(c(home_score, away_score))
  )

# Era Adjustments - Comparing to the rest of the NFL
pts_adj_2013 <- sea_2013_pa$avg_pa / league_2013_pa$avg_pa # 0.608
pts_adj_2025 <- sea_2025_pa$avg_pa / league_2025_pa$avg_pa # 0.735
# 2013 defense allowed ~39% fewer points than league average
# 2025 defense Allowed ~26% fewer points than league average





# 2013 Seahawks Defensive EPA per play - -0.118
sea_2013_epa <- pbp_2013 %>%
  filter(defteam == "SEA", !is.na(epa)) %>%
  summarize(
    def_epa_play = mean(epa)
  )

# 2025 Seahawks Defensive EPA per play - -0.113
sea_2025_epa <- pbp_2025 %>%
  filter(defteam == "SEA", !is.na(epa)) %>%
  summarize(
    def_epa_play = mean(epa)
  )

# 2013 League Defensive EPA per play - 0.006
league_2013_epa <- pbp_2013 %>%
  filter(!is.na(epa)) %>%
  summarize(avg_def_epa = mean(epa))

# 2025 League Defensive EPA per play - 0.013
league_2025_epa <- pbp_2025 %>%
  filter(!is.na(epa)) %>%
  summarize(avg_def_epa = mean(epa))

# Era Adjustments - - Comparing to the rest of the NFL
epa_2013_adj <- sea_2013_epa$def_epa_play - league_2013_epa$avg_def_epa # -0.124
epa_2025_adj <- sea_2025_epa$def_epa_play - league_2025_epa$avg_def_epa # -0.126
# From EPA: 2013 and 2025 are basically identical 
# 2025 is slightly better on paper
# 2025 defense = elite at limiting play-by-play efficiency





# 2013 Seahawks turnovers created - 47
sea_2013_turnovers <- pbp_2013 %>%
  summarize(
    interceptions = sum(
      interception == 1 &
        defteam == "SEA",
      na.rm = TRUE
    ),
    
    fumble_recoveries = sum(
      fumble_lost == 1 &
        fumble_recovery_1_team == "SEA",
      na.rm = TRUE
    )
  ) %>%
  mutate(
    total_takeaways = interceptions + fumble_recoveries
  )

# 2025 Seahawks turnovers created - 32
sea_2025_turnovers <- pbp_2025 %>%
  summarize(
    interceptions = sum(
      interception == 1 &
        defteam == "SEA",
      na.rm = TRUE
    ),
    
    fumble_recoveries = sum(
      fumble_lost == 1 &
        fumble_recovery_1_team == "SEA",
      na.rm = TRUE
    )
  ) %>%
  mutate(
    total_takeaways = interceptions + fumble_recoveries
  )


# Raw totals aren’t fair unless we normalize by opportunities
sea_2013_plays <- pbp_2013 %>% # 1563 Total Def Plays 
  filter(defteam == "SEA") %>%
  summarize(total_plays = n())

sea_2025_plays <- pbp_2025 %>% # 1644 Total Def Plays
  filter(defteam == "SEA") %>%
  summarize(total_plays = n())


# Turnover Rate Per Play
sea_2013_to_rate <- sea_2013_turnovers$total_takeaways / sea_2013_plays$total_plays
# 0.03
sea_2025_to_rate <- sea_2025_turnovers$total_takeaways / sea_2025_plays$total_plays
# 0.02

league_2013_to_rate <- pbp_2013 %>%
  summarize(
    interceptions = sum(interception, na.rm = TRUE),
    fumble_recoveries = sum(fumble_lost, na.rm = TRUE),
    total_takeaways = interceptions + fumble_recoveries,
    plays = n()
  ) %>%
  mutate(rate = total_takeaways / plays)
# 0.017

league_2025_to_rate <- pbp_2025 %>%
  summarize(
    interceptions = sum(interception, na.rm = TRUE),
    fumble_recoveries = sum(fumble_lost, na.rm = TRUE),
    total_takeaways = interceptions + fumble_recoveries,
    plays = n()
  ) %>%
  mutate(rate = total_takeaways / plays)
# 0.014

to_2013_adj <- sea_2013_to_rate / league_2013_to_rate$rate # 1.720
to_2025_adj <- sea_2025_to_rate / league_2025_to_rate$rate # 1.457
# 2013 Seahawks were ~65% better than NFL at forcing turnovers 
# 2025 Seahawks were ~46% better than NFL at forcing turnovers





# 2013 Seahawks Explosive Plays Allowed - 45
sea_2013_explosive <- pbp_2013 %>%
filter(defteam == "SEA") %>%
  summarize(
    explosive_plays = sum(yards_gained >= 20, na.rm = TRUE)
  )

# 2025 Seahawks Explosive Plays Allowed - 63
sea_2025_explosive <- pbp_2025 %>%
  filter(defteam == "SEA") %>%
  summarize(
    explosive_plays = sum(yards_gained >= 20, na.rm = TRUE)
  )

# Explosive play rate allowed
sea_2013_exp_rate <- sea_2013_explosive$explosive_plays / sea_2013_plays$total_plays
# 0.029
sea_2025_exp_rate <- sea_2025_explosive$explosive_plays / sea_2025_plays$total_plays
# 0.038


# 2013 League Explosive Rate 
league_2013_exp <- pbp_2013 %>%
  summarize(
    explosive_plays = sum(yards_gained >= 20, na.rm = TRUE),
    plays = n()
  ) %>%
  mutate(rate = explosive_plays / plays)
# 0.043

# 2025 League Explosive Rate 
league_2025_exp <- pbp_2025 %>%
  summarize(
    explosive_plays = sum(yards_gained >= 20, na.rm = TRUE),
    plays = n()
  ) %>%
  mutate(rate = explosive_plays / plays)
# 0.041


# Era-adjusted explosive play defense
exp_2013_adj <- sea_2013_exp_rate / league_2013_exp$rate # 0.673
exp_2025_adj <- sea_2025_exp_rate / league_2025_exp$rate # 0.940
# 2013 allowed ~33% fewer explosive plays than NFL
# 2025 allowed ~6% fewer explosive plays than NFL





# 2013 Seahawks Red Zone Defense 
sea_2013_redzone <- pbp_2013 %>%
  filter(
    defteam == "SEA",
    yardline_100 <= 20
  ) %>%
  summarize(
    redzone_plays = n(),
    touchdowns_allowed = sum(touchdown == 1, na.rm = TRUE)
  )

# 2025 Seahawks Red Zone Defense 
sea_2025_redzone <- pbp_2025 %>%
  filter(
    defteam == "SEA",
    yardline_100 <= 20
  ) %>%
  summarize(
    redzone_plays = n(),
    touchdowns_allowed = sum(touchdown == 1, na.rm = TRUE)
  )


# Calculate Touchdowns per Red Zone play
sea_2013_rz_rate <- sea_2013_redzone$touchdowns_allowed /
  sea_2013_redzone$redzone_plays
# 0.098

sea_2025_rz_rate <- sea_2025_redzone$touchdowns_allowed /
  sea_2025_redzone$redzone_plays
# 0.125

# On any individual play in the red zone, the offense scored a touchdown 
# about 9.8% and 12.5% of the time against the 2013 and 2025 Seahawks defense.




# Tables
raw_defense_metrics <- tibble(
  season = c("2013 Legion of Boom", "2025 Darkside"),
  
  points_allowed = c(
    sea_2013_pa$avg_pa,
    sea_2025_pa$avg_pa
  ),
  
  total_defensive_plays = c(
    sea_2013_plays$total_plays,
    sea_2025_plays$total_plays
  ),
  
  epa_per_play = c(
    sea_2013_epa$def_epa_play,
    sea_2025_epa$def_epa_play
  ),
  
  interceptions = c(
    sea_2013_turnovers$interceptions,
    sea_2025_turnovers$interceptions
  ),
  
  fumble_recoveries = c(
    sea_2013_turnovers$fumble_recoveries,
    sea_2025_turnovers$fumble_recoveries
  ),
  
  total_takeaways = c(
    sea_2013_turnovers$total_takeaways,
    sea_2025_turnovers$total_takeaways
  ),
  
  explosive_plays_allowed = c(
    sea_2013_explosive$explosive_plays,
    sea_2025_explosive$explosive_plays
  ),
  
  redzone_td_allowed_rate = c(
    sea_2013_rz_rate,
    sea_2025_rz_rate
  )
)



adjusted_defense_metrics <- tibble(
  season = c("2013 Legion of Boom", "2025 Darkside"),
  
  points_allowed_adj = c(
    pts_adj_2013,
    pts_adj_2025
  ),
  
  epa_adj = c(
    epa_2013_adj,
    epa_2025_adj
  ),
  
  turnover_adj = c(
    to_2013_adj,
    to_2025_adj
  ),
  
  explosive_adj = c(
    exp_2013_adj,
    exp_2025_adj
  )
)





write.csv(raw_defense_metrics,
          "raw_defense_metrics.csv",
          row.names = FALSE)

write.csv(adjusted_defense_metrics,
          "adjusted_defense_metrics.csv",
          row.names = FALSE)

