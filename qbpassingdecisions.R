install.packages("nflreadr")
install.packages("nflfastR")
install.packages("sportyR")
install.packages("caret")
install.packages("nflplotR")
install.packages("gt")
install.packages("gtExtras")
install.packages("tidyr")
library(nflreadr)
library(nflfastR)
library(dplyr)
library(sportyR)
library(tidyr)
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
#--------------- Summarization --------------
#--------------------------------------------
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

#------------------------------------------------------------
#----------------------- Visualization ----------------------
#------------------------------------------------------------
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

#Faceted version (stacked panels)
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

#Overlay version (all in one)
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

ggsave(
  filename = "C:/Users/NathanielWright/OneDrive - IMG Academy/Documents/Projects/Football/qb_scatter_xwepa_vs_wepa.png",
  plot = plot_qb_scatter_logo(qb_summary_2024_ranked, "xwepa_per_attempt", "raw_wepa_per_attempt",
                              "xWEPA/Attempt", "WEPA/Attempt"),
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "C:/Users/NathanielWright/OneDrive - IMG Academy/Documents/Projects/Football/qb_scatter_xwepa_vs_epa.png",
  plot = plot_qb_scatter_logo(qb_summary_2024_ranked, "xwepa_per_attempt", "epa_per_attempt",
                              "xWEPA/Attempt", "EPA/Attempt"),
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "C:/Users/NathanielWright/OneDrive - IMG Academy/Documents/Projects/Football/qb_scatter_wepa_vs_epa.png",
  plot = plot_qb_scatter_logo(qb_summary_2024_ranked, "raw_wepa_per_attempt", "epa_per_attempt",
                              "WEPA/Attempt", "EPA/Attempt"),
  width = 8,
  height = 6,
  dpi = 300
)

#------------------------------------------------------------
library(sportyR)
library(dplyr)
library(ggplot2)

lamar_cpoe <- cpoe_summary %>%
  filter(passer_player_name == "L.Jackson", defense_coverage_type == "Cover 3") %>%
  group_by(pass_location, depth_bucket) %>%
  summarise(CPOE = mean(qb_cpoe, na.rm = TRUE), attempts = n(), .groups = "drop")

library(ggplot2)
library(dplyr)

lamar_cpoe <- lamar_cpoe %>%
  mutate(
    x = case_when(
      pass_location == "Left" ~ 20,
      pass_location == "Middle" ~ 60,
      pass_location == "Right" ~ 100
    ),
    y = case_when(
      depth_bucket == "Shallow" ~ 10,
      depth_bucket == "Intermediate" ~ 27,
      depth_bucket == "Deep" ~ 45
    )
  )

p <- ggplot(lamar_cpoe, aes(x = x, y = y, fill = CPOE)) +
  geom_tile(width = 15, height = 15, color = NA, alpha = 0.7) +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.key = element_rect(fill = "transparent", color = NA)
  )

ggsave("lamar_heatmap.png", p, bg = "transparent", width = 6, height = 3.5, dpi = 300)

#----------------------------------------------
#-------------- Stress Testing ----------------
#----------------------------------------------
# Generating W/L and ppg
dplyr::glimpse(passes_qbxcp)
pbpdata_small <- pbpdata %>%
  select(
    play_id,
    game_id,
    season_type,
    season,
    week,
    home_team,
    away_team,
    posteam,
    defteam,
    passer_player_id,
    passer_player_name,
    home_score,
    away_score,
    result
  )
dplyr::glimpse(pbpdata_small)
#----
games_qb <- pbpdata %>%
  group_by(game_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(game_id, season_type, week, home_team, away_team,
         home_score, away_score, result)

qbs <- pbpdata %>%
  filter(!is.na(passer_player_id)) %>%
  group_by(game_id, posteam, passer_player_id, passer_player_name) %>%
  summarise(pass_attempts = n(), .groups = "drop") %>%
  arrange(game_id, posteam, desc(pass_attempts)) %>%
  group_by(game_id, posteam) %>%
  slice_max(order_by = pass_attempts, n = 1, with_ties = FALSE) %>% 
  ungroup()

qbs_wide <- qbs %>%
  pivot_wider(
    id_cols = game_id,
    names_from = posteam,
    values_from = c(passer_player_id, passer_player_name),
    names_glue = "{posteam}_{.value}"
  )

games_full <- games_qb %>%
  left_join(qbs_wide, by = "game_id") %>%
  rowwise() %>%
  mutate(
    home_passer_id = get(paste0(home_team, "_passer_player_id")),
    away_passer_id = get(paste0(away_team, "_passer_player_id")),
    home_passer_name = get(paste0(home_team, "_passer_player_name")),
    away_passer_name = get(paste0(away_team, "_passer_player_name")),
    winning_player_id = case_when(
      home_score > away_score ~ home_passer_id,
      away_score > home_score ~ away_passer_id,
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

games_qb <- games_full %>%
  mutate(
    winner_passer_id = case_when(
      home_score > away_score ~ home_passer_id,
      away_score > home_score ~ away_passer_id,
      TRUE ~ NA_character_
    ),
    winner_passer_name = case_when(
      home_score > away_score ~ home_passer_name,
      away_score > home_score ~ away_passer_name,
      TRUE ~ NA_character_
    )
  )

#--------------------------------
# Generating year over year xwepa
qb_summary_season <- passes_qbxcp %>%
  group_by(season, passer_player_id, passer_player_name, posteam) %>%
  summarise(total_attempts_team = n(), .groups = "drop") %>%
  group_by(season, passer_player_id) %>%
  filter(total_attempts_team == max(total_attempts_team)) %>%
  slice(1) %>%
  rename(team = posteam) %>%
  select(season, passer_player_id, team) %>%
  right_join(
    passes_qbxcp %>%
      group_by(season, passer_player_id, passer_player_name) %>%
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
    by = c("season", "passer_player_id")
  ) %>%
  filter(total_attempts >= 238) %>%
  group_by(season) %>%
  mutate(
    boost_xwepa_vs_epa = xwepa_per_attempt - epa_per_attempt,
    boost_xwepa_vs_rawwepa = xwepa_per_attempt - raw_wepa_per_attempt,
    boost_rawwepa_vs_epa = raw_wepa_per_attempt - epa_per_attempt,
    rank_xwepa = min_rank(desc(as.numeric(xwepa_per_attempt))),
    rank_rawwepa = min_rank(desc(as.numeric(raw_wepa_per_attempt))),
    rank_epa = min_rank(desc(as.numeric(epa_per_attempt))),
    rank_boost_xwepa_vs_epa = min_rank(desc(as.numeric(boost_xwepa_vs_epa))),
    rank_boost_xwepa_vs_rawwepa = min_rank(desc(as.numeric(boost_xwepa_vs_rawwepa))),
    rank_boost_rawwepa_vs_epa = min_rank(desc(as.numeric(boost_rawwepa_vs_epa))),
    consensus_rank = min_rank(
      rowMeans(cbind(
        as.numeric(rank_xwepa),
        as.numeric(rank_rawwepa),
        as.numeric(rank_epa)
      ), na.rm = TRUE)
    )
  ) %>%
  ungroup() %>%
  arrange(season, consensus_rank)

#-----------------------------
games_qb <- games_qb %>%
  mutate(season = as.integer(substr(game_id, 1, 4)))

games_qb <- games_qb %>%
  left_join(
    qb_summary_season %>%
      select(season, passer_player_id,
             epa_per_attempt, raw_wepa_per_attempt, xwepa_per_attempt),
    by = c("season", "home_passer_id" = "passer_player_id")
  ) %>%
  rename(
    home_epa_per_attempt = epa_per_attempt,
    home_raw_wepa_per_attempt = raw_wepa_per_attempt,
    home_xwepa_per_attempt = xwepa_per_attempt
  ) %>%
  left_join(
    qb_summary_season %>%
      select(season, passer_player_id,
             epa_per_attempt, raw_wepa_per_attempt, xwepa_per_attempt),
    by = c("season", "away_passer_id" = "passer_player_id")
  ) %>%
  rename(
    away_epa_per_attempt = epa_per_attempt,
    away_raw_wepa_per_attempt = raw_wepa_per_attempt,
    away_xwepa_per_attempt = xwepa_per_attempt
  )
#------------------------------------
# Modeling Wins
dplyr::glimpse(games_qb)
qb_game_compare <- games_qb %>%
  filter(!is.na(home_epa_per_attempt) & !is.na(away_epa_per_attempt)) %>%
  mutate(
    epa_winner_id = ifelse(home_epa_per_attempt > away_epa_per_attempt, home_passer_id,
                           ifelse(away_epa_per_attempt > home_epa_per_attempt, away_passer_id, NA_character_)),
    rawwepa_winner_id = ifelse(home_raw_wepa_per_attempt > away_raw_wepa_per_attempt, home_passer_id,
                               ifelse(away_raw_wepa_per_attempt > home_raw_wepa_per_attempt, away_passer_id, NA_character_)),
    xwepa_winner_id = ifelse(home_xwepa_per_attempt > away_xwepa_per_attempt, home_passer_id,
                             ifelse(away_xwepa_per_attempt > home_xwepa_per_attempt, away_passer_id, NA_character_)),
    epa_winner_won = epa_winner_id == winning_player_id,
    rawwepa_winner_won = rawwepa_winner_id == winning_player_id,
    xwepa_winner_won = xwepa_winner_id == winning_player_id
  ) %>%
  select(game_id, season, week, home_team, away_team, home_passer_id, away_passer_id,
         home_epa_per_attempt, away_epa_per_attempt,
         home_raw_wepa_per_attempt, away_raw_wepa_per_attempt,
         home_xwepa_per_attempt, away_xwepa_per_attempt,
         epa_winner_id, rawwepa_winner_id, xwepa_winner_id,
         epa_winner_won, rawwepa_winner_won, xwepa_winner_won)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Prepare long format for plotting ----
qb_win_plot <- qb_game_compare %>%
  pivot_longer(
    cols = c(epa_winner_won, rawwepa_winner_won, xwepa_winner_won),
    names_to = "metric",
    values_to = "winner_won"
  )

# ---- Chart 1: Overall win rate by metric ----
overall_win_rate <- qb_win_plot %>%
  group_by(metric) %>%
  summarise(win_rate = mean(winner_won, na.rm = TRUE))

ggplot(overall_win_rate, aes(x = metric, y = win_rate, fill = metric)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Win Rate of QB with Higher Stat per Game",
    x = "Metric",
    y = "Win Rate",
    fill = "Metric"
  ) +
  theme_minimal()

# ---- Chart 2: Win rate by season ----
season_win_rate <- qb_win_plot %>%
  group_by(season, metric) %>%
  summarise(win_rate = mean(winner_won, na.rm = TRUE), .groups = "drop")

ggplot(season_win_rate, aes(x = factor(season), y = win_rate, color = metric, group = metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Seasonal Win Rate of QB with Higher Stat",
    x = "Season",
    y = "Win Rate",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

library(ggplot2)
library(dplyr)
library(tidyr)
library(showtext)

font_add_google("Roboto", "roboto")
showtext_auto()

qb_win_plot <- qb_game_compare %>%
  pivot_longer(
    cols = c(epa_winner_won, rawwepa_winner_won, xwepa_winner_won),
    names_to = "metric",
    values_to = "winner_won"
  ) %>%
  mutate(
    metric = case_when(
      metric == "epa_winner_won" ~ "EPA",
      metric == "rawwepa_winner_won" ~ "Raw WEPA",
      metric == "xwepa_winner_won" ~ "xWEPA"
    )
  )

season_win_rate <- qb_win_plot %>%
  group_by(season, metric) %>%
  summarise(win_rate = mean(winner_won, na.rm = TRUE), .groups = "drop")

ggplot(season_win_rate, aes(x = factor(season), y = win_rate, fill = metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("EPA" = "#1f77b4", "Raw WEPA" = "#ff7f0e", "xWEPA" = "#2ca02c")) +
  labs(
    title = "Win Rate of QB with Higher Stat by Season",
    x = "Season",
    y = "Win Rate",
    fill = "Stat Metric"
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
#-----------------------------------------------------------------
# Examining points per game
team_ppg <- pbpdata_small %>%
  filter(!is.na(passer_player_id)) %>%
  group_by(season, posteam, game_id) %>%
  summarise(team_points = max(home_score * (home_team == posteam) + away_score * (away_team == posteam)),
            .groups = "drop") %>%
  group_by(season, posteam) %>%
  summarise(ppg = mean(team_points), .groups = "drop")

qb_ppg_season <- qb_summary_season %>%
  left_join(team_ppg, by = c("season", "team" = "posteam")) %>%
  select(season, passer_player_id, passer_player_name, team,
         epa_per_attempt, raw_wepa_per_attempt, xwepa_per_attempt, ppg)

library(ggplot2)
library(ggimage)
library(dplyr)

qb_ppg_season <- qb_ppg_season %>%
  left_join(team_logos, by = c("team" = "team_abbr"))

qb_ppg_long <- qb_ppg_season %>%
  pivot_longer(
    cols = c(epa_per_attempt, raw_wepa_per_attempt, xwepa_per_attempt),
    names_to = "stat",
    values_to = "value"
  )

install.packages("ggpubr")
library(ggpubr)

stat_labels <- c(
  epa_per_attempt = "EPA",
  raw_wepa_per_attempt = "Raw WEPA",
  xwepa_per_attempt = "xWEPA"
)

ggplot(qb_ppg_long, aes(x = value, y = ppg, image = logo)) +
  geom_image(size = 0.05) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black", linetype = "dotted") +
  facet_wrap(~stat, scales = "free_x", labeller = labeller(stat = stat_labels)) +
  labs(
    x = "Stat per Attempt",
    y = "Team PPG",
    title = "Relationships between QB Stat per Attempt and Team PPG"
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )