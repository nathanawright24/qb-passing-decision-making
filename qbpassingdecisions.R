install.packages("nflreadr")
install.packages("nflfastR")
install.packages("sportyR")
install.packages("caret")
install.packages("nflplotR")
install.packages("gt")
install.packages("gtExtras")
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

impute_air_yards <- function(df, game_id_val, play_id_val, new_air_yards) {
  df %>%
    mutate(air_yards = ifelse(game_id == game_id_val & play_id == play_id_val,
                              new_air_yards, air_yards))
}


passes_model <- impute_air_yards(passes_model, "2021_02_NO_CAR", 1558, 2)
passes_model <- impute_air_yards(passes_model, "2023_04_CIN_TEN", 905, 0)

#-------------------------------
set.seed(42)

passes_completed <- passes_model %>% filter(complete_pass == 1)

k <- 5
folds <- sample(rep(1:k, length.out = nrow(passes_completed)))

cv_results <- data.frame(R2 = numeric(k), RMSE = numeric(k))

for (i in 1:k) {
  train_data <- passes_completed[folds != i, ]
  test_data  <- passes_completed[folds == i, ]
  
  fit <- lm(epa ~ air_yards + xyac_mean_yardage + xendyardline_100 + first_down_flag + xTD,
            data = train_data)
  
  preds <- predict(fit, newdata = test_data)
  
  ss_res <- sum((test_data$epa - preds)^2)
  ss_tot <- sum((test_data$epa - mean(test_data$epa))^2)
  
  cv_results$R2[i]   <- 1 - ss_res/ss_tot
  cv_results$RMSE[i] <- sqrt(mean((test_data$epa - preds)^2))
}

print(cv_results)
print(colMeans(cv_results))
#-------------------------------------------

final_fit <- lm(
  epa ~ air_yards + xyac_mean_yardage + xendyardline_100 + first_down_flag + xTD,
  data = passes_completed
)

passes_model <- passes_model %>%
  mutate(xepa = predict(final_fit, newdata = passes_model))

passes_qbxcp <- passes_qbxcp %>%
  left_join(
    passes_model %>% select(game_id, play_id, xepa),
    by = c("game_id", "play_id")
  )

passes_qbxcp <- passes_qbxcp %>%
  mutate(wepa = xepa * qbxcp)

#--------------------------------------------
# Visualization
library(ggplot2)

ggplot(passes_qbxcp, aes(x = xepa, y = epa)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(
    title = "Actual EPA vs Predicted xEPA",
    x = "Predicted xEPA",
    y = "Actual EPA"
  ) +
  theme_minimal()

qb_summary_career <- passes_qbxcp %>%
  group_by(passer_player_id, passer_player_name, posteam) %>%
  summarise(
    total_attempts_team = n(),
    .groups = "drop"
  ) %>%
  group_by(passer_player_id) %>%
  filter(total_attempts_team == max(total_attempts_team)) %>%
  slice(1) %>% # in case of ties, just pick one
  rename(team = posteam) %>%
  select(passer_player_id, team) %>%
  right_join(
    passes_qbxcp %>%
      group_by(passer_player_id, passer_player_name) %>%
      summarise(
        total_attempts = n(),
        total_xwepa = sum(xepa * qbxcp, na.rm = TRUE),
        xwepa_per_attempt = mean(xepa * qbxcp, na.rm = TRUE),
        total_raw_wepa = sum(epa * qbxcp, na.rm = TRUE),
        raw_wepa_per_attempt = mean(epa * qbxcp, na.rm = TRUE),
        total_epa = sum(epa, na.rm = TRUE),
        epa_per_attempt = mean(epa, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "passer_player_id"
  ) %>%
  filter(total_attempts >= 1000) %>%
  mutate(
    boost_xwepa_vs_epa = xwepa_per_attempt - epa_per_attempt,
    boost_xwepa_vs_rawwepa = xwepa_per_attempt - raw_wepa_per_attempt,
    boost_rawwepa_vs_epa = raw_wepa_per_attempt - epa_per_attempt,
    rank_xwepa = min_rank(desc(xwepa_per_attempt)),
    rank_rawwepa = min_rank(desc(raw_wepa_per_attempt)),
    rank_epa = min_rank(desc(epa_per_attempt)),
    rank_boost_xwepa_vs_epa = min_rank(desc(boost_xwepa_vs_epa)),
    rank_boost_xwepa_vs_rawwepa = min_rank(desc(boost_xwepa_vs_rawwepa)),
    rank_boost_rawwepa_vs_epa = min_rank(desc(boost_rawwepa_vs_epa))
  ) %>%
  arrange(rank_xwepa)

qb_summary_2024_ranked <- passes_qbxcp %>%
  filter(season == 2024) %>%
  group_by(passer_player_id, passer_player_name, posteam) %>%
  summarise(total_attempts_team = n(), .groups = "drop") %>%
  group_by(passer_player_id) %>%
  filter(total_attempts_team == max(total_attempts_team)) %>%
  slice(1) %>%
  rename(team = posteam) %>%
  select(passer_player_id, team) %>%
  right_join(
    passes_qbxcp %>%
      filter(season == 2024) %>%
      group_by(passer_player_id, passer_player_name) %>%
      summarise(
        total_attempts = n(),
        total_xwepa = sum(xepa * qbxcp, na.rm = TRUE),
        xwepa_per_attempt = mean(xepa * qbxcp, na.rm = TRUE),
        total_raw_wepa = sum(epa * qbxcp, na.rm = TRUE),
        raw_wepa_per_attempt = mean(epa * qbxcp, na.rm = TRUE),
        total_epa = sum(epa, na.rm = TRUE),
        epa_per_attempt = mean(epa, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "passer_player_id"
  ) %>%
  filter(total_attempts >= 238) %>%
  mutate(
    boost_xwepa_vs_epa = xwepa_per_attempt - epa_per_attempt,
    boost_xwepa_vs_rawwepa = xwepa_per_attempt - raw_wepa_per_attempt,
    boost_rawwepa_vs_epa = raw_wepa_per_attempt - epa_per_attempt,
    rank_xwepa = min_rank(desc(xwepa_per_attempt)),
    rank_rawwepa = min_rank(desc(raw_wepa_per_attempt)),
    rank_epa = min_rank(desc(epa_per_attempt)),
    rank_boost_xwepa_vs_epa = min_rank(desc(boost_xwepa_vs_epa)),
    rank_boost_xwepa_vs_rawwepa = min_rank(desc(boost_xwepa_vs_rawwepa)),
    rank_boost_rawwepa_vs_epa = min_rank(desc(boost_rawwepa_vs_epa))
  ) %>%
  ungroup() %>%
  mutate(
    consensus_rank = min_rank(
      rowMeans(cbind(rank_xwepa, rank_rawwepa, rank_epa), na.rm = TRUE)
    )
  ) %>%
  arrange(consensus_rank)

#---------------------------------------------
library(dplyr)
library(gt)

qb_primary_team <- passes_qbxcp %>%
  group_by(passer_player_id, passer_player_name, team = posteam) %>%
  summarise(attempts = n(), .groups = "drop") %>%
  arrange(passer_player_id, desc(attempts)) %>%
  group_by(passer_player_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(passer_player_id, team)

qb_career_raw <- passes_qbxcp %>%
  group_by(passer_player_id, passer_player_name) %>%
  summarise(
    total_attempts   = n(),
    epa_per_pa       = mean(epa, na.rm = TRUE),
    wepa_per_pa      = mean(epa * qbxcp, na.rm = TRUE),
    xwepa_per_pa     = mean(xepa * qbxcp, na.rm = TRUE),
    total_epa        = sum(epa, na.rm = TRUE),
    total_wepa       = sum(epa * qbxcp, na.rm = TRUE),
    total_xwepa      = sum(xepa * qbxcp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(qb_primary_team, by = "passer_player_id")

qb_career_ranked <- qb_career_raw %>%
  filter(total_attempts >= 1000) %>%
  mutate(
    rank_epa    = min_rank(desc(epa_per_pa)),
    rank_wepa   = min_rank(desc(wepa_per_pa)),
    rank_xwepa  = min_rank(desc(xwepa_per_pa)),
    consensus_raw = (rank_epa + rank_wepa + rank_xwepa) / 3,
    consensus_rank = min_rank(consensus_raw)
  ) %>%
  arrange(consensus_rank)

qb_career_table <- qb_career_ranked %>%
  select(
    consensus_rank,
    passer_player_name,
    team,
    total_attempts,
    epa_per_pa, wepa_per_pa, xwepa_per_pa,
    rank_epa, rank_wepa, rank_xwepa
  ) %>%
  gt() %>%
  cols_label(
    consensus_rank   = "Consensus Rank",
    passer_player_name = "Quarterback",
    team             = "Team",
    total_attempts   = "Attempts",
    epa_per_pa       = "EPA/PA",
    wepa_per_pa      = "WEPA/PA",
    xwepa_per_pa     = "xWEPA/PA",
    rank_epa         = "EPA Rank",
    rank_wepa        = "WEPA Rank",
    rank_xwepa       = "xWEPA Rank"
  ) %>%
  fmt_number(columns = c(epa_per_pa, wepa_per_pa, xwepa_per_pa), decimals = 3) %>%
  tab_options(table.font.names = "Calibri") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(consensus_rank, passer_player_name))
  )

qb_career_table

library(dplyr)
library(gt)
library(gtExtras)
library(nflplotR)

team_logos <- nflreadr::load_teams() %>%
  select(team_abbr, logo = team_logo_wikipedia)

qb_career_ranked_pretty <- qb_career_ranked %>%
  left_join(team_logos, by = c("team" = "team_abbr")) %>%
  arrange(consensus_rank)

qb_career_table_pretty <- qb_career_ranked_pretty %>%
  select(
    consensus_rank,
    passer_player_name,
    logo,
    total_attempts,
    epa_per_pa, wepa_per_pa, xwepa_per_pa,
    rank_epa, rank_wepa, rank_xwepa
  ) %>%
  gt() %>%
  cols_label(
    consensus_rank     = "Consensus Rank",
    passer_player_name = "Quarterback",
    logo               = "Team",
    total_attempts     = "Attempts",
    epa_per_pa         = "EPA/PA",
    wepa_per_pa        = "WEPA/PA",
    xwepa_per_pa       = "xWEPA/PA",
    rank_epa           = "EPA Rank",
    rank_wepa          = "WEPA Rank",
    rank_xwepa         = "xWEPA Rank"
  ) %>%
  gt_img_rows(columns = logo, height = 30) %>%
  fmt_number(columns = c(epa_per_pa, wepa_per_pa, xwepa_per_pa), decimals = 3) %>%
  data_color(
    columns = c(rank_epa, rank_wepa, rank_xwepa),
    fn = scales::col_numeric(
      palette = c("#2ECC71", "#F1C40F", "#E74C3C"),
      domain = NULL
    )
  ) %>%
  tab_options(table.font.names = "Calibri") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(consensus_rank, passer_player_name))
  )

qb_career_table_pretty

install.packages("webshot2")
library(webshot2)

gtsave(qb_career_table_pretty, "qb_table.png")
gtsave(
  qb_career_table_pretty,
  "C:/Users/NathanielWright/OneDrive - IMG Academy/Documents/Projects/Football/qb_table.png"
)
#-------------
team_logos <- nflreadr::load_teams() %>%
  select(team_abbr, logo = team_logo_wikipedia)

str(qb_summary_2024)        # check column types
summary(qb_summary_2024$xwepa_per_attempt)
summary(qb_summary_2024$raw_wepa_per_attempt)
summary(qb_summary_2024$epa_per_attempt)

qb_2024_ranked <- qb_summary_2024 %>%
  ungroup() %>%
  mutate(
    rank_xwepa   = min_rank(desc(xwepa_per_attempt)),
    rank_rawwepa = min_rank(desc(raw_wepa_per_attempt)),
    rank_epa     = min_rank(desc(epa_per_attempt)),
    consensus_rank = min_rank(rowMeans(cbind(rank_xwepa, rank_rawwepa, rank_epa), na.rm = TRUE))
  ) %>%
  arrange(consensus_rank)

qb_2024_ranked_pretty <- qb_2024_ranked %>%
  left_join(team_logos, by = c("team" = "team_abbr"))

qb_2024_table <- qb_2024_ranked_pretty %>%
  select(
    consensus_rank,
    passer_player_name,
    logo,
    total_attempts,
    epa_per_attempt, raw_wepa_per_attempt, xwepa_per_attempt,
    rank_epa, rank_rawwepa, rank_xwepa
  ) %>%
  gt() %>%
  cols_label(
    consensus_rank        = "Consensus Rank",
    passer_player_name    = "Quarterback",
    logo                  = "Team",
    total_attempts        = "Attempts",
    epa_per_attempt       = "EPA/PA",
    raw_wepa_per_attempt  = "Raw WEPA/PA",
    xwepa_per_attempt     = "xWEPA/PA",
    rank_epa              = "EPA Rank",
    rank_rawwepa          = "WEPA Rank",
    rank_xwepa            = "xWEPA Rank"
  ) %>%
  gt_img_rows(columns = logo, height = 30) %>%
  fmt_number(columns = c(epa_per_attempt, raw_wepa_per_attempt, xwepa_per_attempt), decimals = 3) %>%
  data_color(
    columns = c(rank_epa, rank_rawwepa, rank_xwepa),
    fn = scales::col_numeric(
      palette = c("#2ECC71", "#F1C40F", "#E74C3C"),
      domain = NULL
    )
  ) %>%
  tab_options(table.font.names = "Calibri") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(consensus_rank, passer_player_name))
  )

qb_2024_table

gtsave(
  qb_2024_table,
  "C:/Users/NathanielWright/OneDrive - IMG Academy/Documents/Projects/Football/qb_table_2024.png"
)

#-----------------------------------------------
install.packages("ggrepel")
library(nflplotR)
library(ggplot2)

ggplot(qb_summary_2024, aes(x = xwepa_per_attempt, y = epa_per_attempt)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.06) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "grey40") +
  labs(
    title = "EPA vs xWEPA per Attempt (2024)",
    x = "xWEPA/Attempt",
    y = "EPA/Attempt"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

# qbxcp volume distribution
team_colors <- nflfastR::teams_colors_logos %>%
  select(team_abbr, team_color = team_color)

# Filter top 5 QBs from 2024
top5_qbs <- c("L.Jackson", "B.Purdy", "J.Goff", "B.Mayfield", "S.Darnold")

qbxcp_top5 <- passes_qbxcp %>%
  filter(season == 2024, passer_player_name %in% top5_qbs) %>%
  left_join(team_colors, by = c("posteam" = "team_abbr"))

ggplot(qbxcp_top5, aes(x = qbxcp, fill = passer_player_name)) +
  geom_density(alpha = 0.3, color = NA) +  # reduced alpha for more transparency
  scale_fill_manual(
    values = setNames(qbxcp_top5$team_color[match(top5_qbs, qbxcp_top5$passer_player_name)],
                      top5_qbs)
  ) +
  labs(
    title = "Distribution of QBxCP per Pass Attempt (Top 5 QBs, 2024)",
    x = "QB Completion Probability (xCP)",
    y = "Density",
    fill = "Quarterback"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

ggplot(qbxcp_top5, aes(x = qbxcp, fill = passer_player_name, color = passer_player_name)) +
  geom_density(alpha = 0.3, size = 1) +  # fill is transparent, color is outline
  scale_fill_manual(
    values = setNames(qbxcp_top5$team_color[match(top5_qbs, qbxcp_top5$passer_player_name)],
                      top5_qbs)
  ) +
  scale_color_manual(
    values = setNames(qbxcp_top5$team_color[match(top5_qbs, qbxcp_top5$passer_player_name)],
                      top5_qbs)
  ) +
  labs(
    title = "Distribution of QBxCP per Pass Attempt (Top 5 QBs, 2024)",
    x = "QB Completion Probability (xCP)",
    y = "Density",
    fill = "Quarterback",
    color = "Quarterback"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

install.packages("showtext")
library(showtext)

# Load Google font
font_add_google("Roboto Condensed", "roboto")
showtext_auto()

ggplot(qbxcp_top5, aes(x = qbxcp, fill = passer_player_name, color = passer_player_name)) +
  geom_density(alpha = 0.3, size = 1) +
  scale_fill_manual(
    values = setNames(qbxcp_top5$team_color[match(top5_qbs, qbxcp_top5$passer_player_name)],
                      top5_qbs)
  ) +
  scale_color_manual(
    values = setNames(qbxcp_top5$team_color[match(top5_qbs, qbxcp_top5$passer_player_name)],
                      top5_qbs)
  ) +
  labs(
    title = "Distribution of QBxCP per Pass Attempt (Top 5 QBs, 2024)",
    x = "QB Completion Probability (xCP)",
    y = "Density",
    fill = "Quarterback",
    color = "Quarterback"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "roboto"),   # apply font
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

library(dplyr)
library(ggplot2)

# bottom 5 list
bottom5_qbs <- c("A.Rodgers", "D.Jones", "C.Rush", "C.Williams", "B.Nix")

qbxcp_bottom5 <- passes_qbxcp %>%
  filter(season == 2024, passer_player_name %in% bottom5_qbs) %>%
  left_join(team_colors, by = c("posteam" = "team_abbr")) %>%
  mutate(group = "Bottom 5")

qbxcp_top5 <- qbxcp_top5 %>%
  mutate(group = "Top 5")

qbxcp_compare <- bind_rows(qbxcp_top5, qbxcp_bottom5)

# ---------------------------
# 1. Faceted version (stacked panels)
# ---------------------------
p_facet <- ggplot(qbxcp_compare, aes(x = qbxcp, fill = passer_player_name, color = passer_player_name)) +
  geom_density(alpha = 0.3, size = 1) +
  scale_fill_manual(values = setNames(qbxcp_compare$team_color, qbxcp_compare$passer_player_name)) +
  scale_color_manual(values = setNames(qbxcp_compare$team_color, qbxcp_compare$passer_player_name)) +
  labs(
    title = "Distributions of QBxCP per Pass Attempt (2024)",
    subtitle = "Top 5 vs Bottom 5 Qualified Quarterbacks",
    x = "QB Completion Probability (xCP)",
    y = "Density"
  ) +
  facet_wrap(~group, ncol = 1) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "roboto"),
    legend.position = "bottom",
    legend.title = element_blank()
  )


# ---------------------------
# 2. Overlay version (all in one)
# ---------------------------
p_overlay <- ggplot(qbxcp_compare, aes(x = qbxcp, fill = passer_player_name, color = passer_player_name, linetype = group)) +
  geom_density(alpha = 0.25, size = 1) +
  scale_fill_manual(values = setNames(qbxcp_compare$team_color, qbxcp_compare$passer_player_name)) +
  scale_color_manual(values = setNames(qbxcp_compare$team_color, qbxcp_compare$passer_player_name)) +
  labs(
    title = "Distributions of QBxCP per Pass Attempt (2024)",
    subtitle = "Top 5 vs Bottom 5 Overlay",
    x = "QB Completion Probability (xCP)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "roboto"),
    legend.position = "bottom",
    legend.title = element_blank()
  )

p_facet
p_overlay

#------------------------------------------------------
qbxcp_summary <- qbxcp_compare %>%
  group_by(passer_player_name, group) %>%
  summarise(mean_qbxcp = mean(qbxcp, na.rm = TRUE),
            sd_qbxcp = sd(qbxcp, na.rm = TRUE),
            .groups = "drop")

ggplot(qbxcp_summary, aes(x = reorder(passer_player_name, mean_qbxcp), y = mean_qbxcp, fill = group)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_qbxcp - sd_qbxcp, ymax = mean_qbxcp + sd_qbxcp), width = 0.2) +
  coord_flip() +
  scale_fill_manual(values = c("Top 5" = "#1f77b4", "Bottom 5" = "#d62728")) +
  labs(
    title = "Average QBxCP with Spread (2024)",
    x = "Quarterback",
    y = "Mean QBxCP ± SD"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")
#---------------------------------------------------------
install.packages("ggimage")
library(ggimage)

qb_summary_2024_ranked <- qb_summary_2024_ranked %>%
  left_join(team_logos, by = c("team" = "team_abbr"))

plot_qb_scatter_logo <- function(df, xvar, yvar, xlabel, ylabel) {
  ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]])) +
    geom_image(aes(image = logo), size = 0.07, by = "width", asp = 1) +
    theme_minimal(base_family = "roboto") +
    labs(x = xlabel, y = ylabel, title = paste(ylabel, "vs", xlabel)) +
    theme(plot.title = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 12))
}

plot_qb_scatter_logo(qb_summary_2024_ranked, "xwepa_per_attempt", "raw_wepa_per_attempt",
                     "xWEPA/Attempt", "WEPA/Attempt")
plot_qb_scatter_logo(qb_summary_2024_ranked, "xwepa_per_attempt", "epa_per_attempt",
                     "xWEPA/Attempt", "EPA/Attempt")
plot_qb_scatter_logo(qb_summary_2024_ranked, "raw_wepa_per_attempt", "epa_per_attempt",
                     "WEPA/Attempt", "EPA/Attempt")
#------------------------------------------------------------