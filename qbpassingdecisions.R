install.packages("nflreadr")
install.packages("nflfastR")
install.packages("sportyR")
install.packages("caret")
library(nflreadr)
library(nflfastR)
library(dplyr)
library(sportyR)
seasons <- list(2021, 2022, 2023, 2024)
pbpdata <- load_pbp(2021:2024)
#-----------------------------------------
passes <- pbpdata %>%
  filter(
    play_type == "pass",
    two_point_attempt == 0,                
    abs(score_differential) <= 16          
  )
passfiltered <- passes %>%
  select(
    game_id, play_id, season_type, posteam, defteam, yardline_100, qtr,
    down, ydstogo, desc, play_type, yards_gained, td_prob, qb_spike, pass_length,
    pass_location, air_yards, yards_after_catch, posteam_score,
    defteam_score, score_differential, ep, epa, air_epa, yac_epa,
    comp_air_epa, comp_yac_epa, wp, def_wp, wpa, vegas_wpa, air_wpa,
    yac_wpa, comp_air_wpa, comp_yac_wpa, incomplete_pass, interception,
    qb_hit, pass_attempt, sack, touchdown, fumble, complete_pass,
    passer_player_id, passer_player_name, passing_yards, receiver_player_id,
    receiver_player_name, receiving_yards, season, cp, cpoe,
    play_type_nfl, aborted_play, passer, receiver, pass, first_down,
    qb_epa, xyac_epa, xpass, pass_oe, xyac_mean_yardage 
  )
participation <- load_participation(2021:2024)

passes_with_routes <- passfiltered %>%
  left_join(
    participation %>%
      select(
        nflverse_game_id, play_id,
        ngs_air_yards, time_to_throw, was_pressure,
        route, defense_man_zone_type, defense_coverage_type
      ),
    by = c("game_id" = "nflverse_game_id", "play_id" = "play_id")
  )

passes_clean <- passes_with_routes %>%
  filter(
    sack == 0,
    fumble == 0,
    interception == 0
  )
#------------------------------------------------------------

passes_clean <- passes_clean %>%
  mutate(depth_bucket = case_when(
    air_yards <= 0 ~ "behind",
    air_yards <= 10 ~ "short",
    air_yards <= 20 ~ "intermediate",
    air_yards > 20  ~ "deep",
    TRUE ~ NA_character_
  ))

passes_clean %>%
  count(defense_coverage_type, sort = TRUE)

passes_clean <- passes_clean %>%
  mutate(defense_coverage_type = ifelse(is.na(defense_coverage_type), "UNKNOWN", defense_coverage_type))

passes_clean %>%
  group_by(passer_player_id, passer_player_name) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(passer_player_id) %>%
  filter(n_distinct(passer_player_name) > 1) %>%
  arrange(passer_player_id, desc(n))

passes_clean <- passes_clean %>%
  mutate(passer_player_name = case_when(
    passer_player_id == "00-0023459" ~ "A.Rodgers",
    passer_player_id == "00-0028118" ~ "T.Taylor",
    passer_player_id == "00-0034857" ~ "J.Allen",
    TRUE ~ passer_player_name
  ))

#-----------------------------------------------------
cpoe_summary <- passes_clean %>%
  group_by(passer_player_id, passer_player_name, pass_location, depth_bucket, defense_coverage_type) %>%
  summarise(
    qb_cpoe = mean(cpoe, na.rm = TRUE),
    attempts = n(),
    .groups = "drop"
  ) %>%
  mutate(qb_cpoe = ifelse(attempts < 15, 0, qb_cpoe))

cpoe_summary <- cpoe_summary %>%
  mutate(qb_cpoe = qb_cpoe / 100)

#-----------------------------------------------------
install.packages("ggplot2")
library(ggplot2)
library(scales)

allen_cover3 <- cpoe_summary %>%
  filter(passer_player_name == "J.Allen",
         defense_coverage_type == "COVER_3") %>%
  mutate(depth_bucket = factor(depth_bucket, levels = c("behind", "short", "intermediate", "deep")))

field_background <- data.frame(
  ymin = 0:3,
  ymax = 1:4
)

ggplot() +
  geom_rect(data = field_background,
            aes(xmin = 0, xmax = 4, ymin = ymin, ymax = ymax),
            fill = c("#006400", "#008000", "#006400", "#008000"),
            color = NA) +
  geom_tile(data = allen_cover3,
            aes(x = as.numeric(factor(pass_location)),
                y = as.numeric(depth_bucket)-1 + 0.5,
                fill = qb_cpoe),
            color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "CPOE") +
  geom_text(data = allen_cover3,
            aes(x = as.numeric(factor(pass_location)),
                y = as.numeric(depth_bucket)-1 + 0.5,
                label = round(qb_cpoe,2)),
            size = 4) +
  scale_x_continuous(breaks = 1:3, labels = c("Left", "Middle", "Right")) +
  scale_y_continuous(breaks = 1:4, labels = c("behind","short","intermediate","deep")) +
  labs(title = "Josh Allen CPOE vs Cover 3 by Location and Depth") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold")
  )

#-----------------------------------------------------------------------
passes_qbxcp <- passes_clean %>%
  left_join(
    cpoe_summary %>%
      select(passer_player_id, defense_coverage_type, pass_location, depth_bucket, qb_cpoe),
    by = c("passer_player_id", "defense_coverage_type", "pass_location", "depth_bucket")
  ) %>%
  mutate(
    logit_cp = log(cp / (1 - cp)),
    qbxcp = 1 / (1 + exp(-(logit_cp + qb_cpoe))),
    qbxcp = pmin(pmax(qbxcp, 0), 1)
  ) %>%
  select(-logit_cp)

#-------------------------------------------------------------------------
# Modeling for expected epa

passes_model <- passes_qbxcp %>%
  mutate(
    total_yards = air_yards + xyac_mean_yardage,
    first_down_flag = ifelse(total_yards >= ydstogo, 1, 0),
    xendyardline_100 = yardline_100 - total_yards
  ) %>%
  select(game_id, play_id, epa, air_yards, xyac_mean_yardage, yardline_100, xendyardline_100, first_down_flag, complete_pass)

passes_model <- passes_model %>%
  mutate(
    xyac_mean_yardage = ifelse(is.na(xyac_mean_yardage), 0, xyac_mean_yardage),
    xendyardline_100 = ifelse(is.na(xendyardline_100), 0, xendyardline_100),
    xTD = ifelse(xendyardline_100 == 0, 1, 0),
    first_down_flag = ifelse(is.na(first_down_flag), 1, first_down_flag)
  )

colSums(is.na(passes_model))
print_na_air_yards <- function(df) {
  na_rows <- df %>% filter(is.na(air_yards)) %>% select(game_id, play_id)
  print(na_rows)
}
print_na_air_yards(passes_model)
# Two NA air yards throws, cross reference game and play ids to determine events of play
impute_air_yards <- function(df, game_id_val, play_id_val, new_air_yards) {
  df %>%
    mutate(air_yards = ifelse(game_id == game_id_val & play_id == play_id_val,
                              new_air_yards, air_yards))
}

# Impute 2-yard TD throw
passes_model <- impute_air_yards(passes_model, "2021_02_NO_CAR", 1558, 2)

# Impute sack/aborted throw as 0 air yards
passes_model <- impute_air_yards(passes_model, "2023_04_CIN_TEN", 905, 0)

#-------------------------------
library(caret)
set.seed(42)
