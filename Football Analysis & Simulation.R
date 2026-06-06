library(rlang)
library(tidyverse)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(ggpubr)
library(car)
library(multcomp)
library(MASS)
library(broom)
library(ggrepel)
library(scales)
library(viridis)
library(patchwork)
library(Matrix)

top20 <- read.csv("C:/Users/laila/Documents/Football Project/top20_contenders.csv", stringsAsFactors = FALSE)
players <- read.csv("C:/Users/laila/Documents/Football Project/player_summary_top20.csv", stringsAsFactors = FALSE)
worldcup_2026 <- read.csv("C:/Users/laila/Documents/Football Project/wc2026_all48_summary.csv", stringsAsFactor = FALSE)
players <- players %>% rename(TEAM = Nation)

master <- top20 %>%
  left_join(players, by = "TEAM") %>%
  dplyr::rename(rank = rank.x) %>%
  dplyr::select(-rank.y)

cat("Master table shape:", nrow(master), "x", ncol(master), "\n")
cat("Teams:", master$TEAM, "\n")


#EDA
plot1 <- ggplot(master, aes(x = reorder(TEAM, composite_score), y = composite_score,
                            fill = confederation)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(composite_score, 3)), hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "2026 World Cup Contenders — Composite Score",
       subtitle = "Weighted combination of performance metrics (1978–2024)",
       x = NULL, y = "Composite Score", fill = "Confederation") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))
print(plot1)

#Distribution of Key Metrics
metrics_long <- master %>%
  dplyr::select(TEAM, goals_per_game, goal_diff_per_game, cleansheet_pct, win_rate, recent_win_rate) %>%
  pivot_longer(-TEAM, names_to = "metric", values_to = "value")

plot2 <- ggplot(metrics_long, aes(x=value, fill = metric)) +
  geom_histogram(bins = 10, color = "white", alpha = 0.8) +
  facet_wrap(~metric, scales = "free", ncol = 3) +
  scale_fill_viridis_d() +
  labs(title    = "Distribution of Major Performance Indicators",
       subtitle = "Throughout top 20 World Cup Contenders",
       x        = "Value",
       y        = "Count") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))
print(plot2)

#CORRELATION MATRIX
cor_vars <- master %>%
  dplyr::select(goals_per_game, goals_conceded_per_game, goal_diff_per_game, cleansheet_pct, win_rate, wc_appearances, recent_win_rate, wc2022_stage, composite_score, avg_ovr, elite_count, avg_age)
cor_matrix <- cor(cor_vars, use = "complete.obs")
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         number.cex = 0.6,
         tl.cex = 0.8,
         tl.col = "black",
         col = brewer.pal(10, "RdYlGn"),
         title = "Football Performance Indicators Correlation Matrix", 
         mar = c(0,0,2,0))

cor_with_composite <- sort(cor_matrix[,"composite_score"], decreasing = TRUE)
print(round(cor_with_composite, 3))

#ATTACK VS DEFENCE SCATTERPLOT
plot4 <- ggplot(master, aes(x = goal_diff_per_game,
                            y = cleansheet_pct, color= confederation, size = wc_appearances)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label=TEAM), size = 3, vjust = -0.8, hjust = 0.5) +
  scale_color_brewer(palette = "Set1") +
  scale_size_continuous(range = c(3, 8)) +
  labs(title = "Attack vs Defence for Top 20 Contenders",
       subtitle = "Size = World Cup Appearances",
       x = "Goal Difference per Game(Attack)",
       y = "Clean Sheet % (Defence)",
       color = "Confederation",
       size = "WC Appearances") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
print(plot4)

#CONFEDERATION STRENGTH ANALYSIS
conf_summary <- master %>%
  group_by(confederation) %>%
  summarise(
    n             = n(),
    mean_gdpg     = round(mean(goal_diff_per_game), 3),
    sd_gdpg       = round(sd(goal_diff_per_game), 3),
    mean_winrate  = round(mean(win_rate), 3),
    mean_composite= round(mean(composite_score), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_composite))
print(conf_summary)

anova_model <- aov(goal_diff_per_game ~ confederation, data = master)
print(summary(anova_model))
print(leveneTest(goal_diff_per_game ~ as.factor(confederation), data = master))

anova_win_rate <- aov(win_rate ~ confederation, data = master)
print(summary(anova_win_rate))

anova_comp_score <- aov(composite_score ~ confederation, data = master)
print(summary(anova_comp_score))

tukey_result <- TukeyHSD(anova_comp_score)
print(tukey_result)

#CONFEDERATION BOX PLOTS
plot5_1 <- ggplot(master, aes(x = reorder(confederation, goal_diff_per_game, median),
                            y = goal_diff_per_game, fill = confederation)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +
  geom_text(aes(label = TEAM), size = 2.5, vjust = -0.8, hjust = 0.5 ) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Goal Difference per Game by Confederation",
       subtitle = "One-way ANVOA test for group differences",
       x = "Confederation", y = "Goal Difference per Game") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))
plot5_2 <- ggplot(master, aes(x = reorder(confederation, win_rate, median),
                              y = win_rate, fill = confederation)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +
  geom_text(aes(label = TEAM), size = 2.5, vjust = -0.8, hjust = 0.5) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Win Rate by Confederation",
       x = "Confederation", y = "Win Rate") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))
plot_5 <- plot5_1/ plot5_2
print(plot_5)

#LINEAR REGRESSION
lm_full <- lm(composite_score ~ goal_diff_per_game + cleansheet_pct + win_rate + wc_appearances + recent_win_rate + wc2022_stage, data = master)
print(summary(lm_full))

print(vif(lm_full))

lm_step <- stepAIC(lm_full, direction = "both", trace = FALSE)
print(summary(lm_step))
par(mfrow = c(2, 2))
plot(lm_full, main = "Linear Regression Diagnostics")

#Coefficient 
coef_df <- tidy(lm_full, conf.int = TRUE) %>%
  filter(term != "(Intercept)")

plot7 <- ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate,
                          color = estimate > 0)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  coord_flip() +
  scale_color_manual(values = c("red3", "steelblue"),
                     labels = c("Negative", "Positive")) +
  labs(title = "Linear Regression Coefficients",
       subtitle = "Predictors of composite score (with 95% CI)",
       x = NULL, y = "Coefficient Estimate",
       color = "Direction") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

coef_df <- tidy(lm_full, conf.int = TRUE) %>%
  filter(term != "(Intercept)")
print(plot7)


#POISSON REGRESSION
poisson_model <- glm(goals_scored ~ matches_played + avg_ovr +
                       wc_appearances + recent_win_rate + wc2022_stage,
                     family = poisson(link = "log"),
                     data = master)
print(summary(poisson_model))

irr <- exp(coef(poisson_model))
irr_ci <- exp(confint(poisson_model))
print(round(cbind(IRR = irr, irr_ci), 3))

poisson_rate <- glm(goals_scored ~ avg_ovr + wc_appearances + recent_win_rate + confederation,
                    family = poisson(link = "log"),
                    offset = log(matches_played),
                    data = master)
print(summary(poisson_rate))

master$predicted_goals_per_game <- predict(poisson_rate, type = "response") /
  master$matches_played

plot8 <- ggplot(master, aes(x=goals_per_game, y = predicted_goals_per_game,
                            color = confederation)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(aes(label = TEAM), size = 3, max.overlaps = 20) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Poisson Regression - Actual vs Predicted Goals per Game",
       subtitle = "Points on dashed line = perfect prediction",
       x = "Actual Goals per Game",
       y = "Predicted Goals per Game",
       color = "Confederation") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))
print(plot8)

cluster_vars <- master %>%
  dplyr::select(goal_diff_per_game, cleansheet_pct, win_rate,
         recent_win_rate, wc2022_stage, avg_ovr) %>%
  scale()
rownames(cluster_vars) <- master$TEAM
set.seed(52)
kmeans_res <- kmeans(cluster_vars, centers = 4, nstart = 25)
master$cluster <- as.factor(kmeans_res$cluster)
cluster_means <- master %>%
  group_by(cluster) %>%
  summarise(mean_composite = mean(composite_score), .groups = "drop") %>%
  arrange(desc(mean_composite)) %>%
  mutate(tier = c("Elite", "Strong", "Mid-tier", "Developing"))
master <- master %>%
  left_join(cluster_means %>% dplyr::select(cluster, tier), by = "cluster")
print(master %>% dplyr::select(TEAM, confederation, composite_score, tier) %>%
        arrange(tier, desc(composite_score)))

#CLUSTER SCATTER PLOT
plot9 <- ggplot(master, aes(x = goal_diff_per_game, y = avg_ovr,
                            color = tier, shape = confederation)) +
  geom_point(size = 4, alpha = 0.85) +
  geom_text(aes(label=TEAM), size = 3, vjust = -0.8, hjuts = 0.5) +
  scale_color_manual(values = c("Elite" = "#e74c3c",
                                "Strong" = "#e67e22",
                                "Mid-tier" = "#3498db",
                                "Developing" = "#95a5a6")) +
  labs(title = "Cluster Analysis -- Team Performance Tiers",
       subtitle = "K-means clustering (k=4) on performance & player quality indicators",
       x = "Goal Difference per Game",
       y = "Average Player OVR(top 23)",
       color = "Tier",
       shape = "Confederation") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")
print(plot9)

#PLAYER QUALITY ANALYSIS
skills_long <- master %>%
  dplyr::select(TEAM, PAC, SHO, PAS, DRI, DEF, PHY) %>%
  pivot_longer(-TEAM, names_to = "skill", values_to = "rating")
plot10 <- ggplot(skills_long, aes(x=skill, y = reorder(TEAM, rating),
                                  fill = rating)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = round(rating, 0)), size = 2.8, color = "white") +
  scale_fill_viridis_c(option = "plasma", name = "Rating") +
  labs(title = "FIFA Skill Ratings by Nation -- Top 20",
       x = "Skill", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 9))
print(plot10)

#SQUAD AGE PROFILE
age_long <- master %>%
  dplyr::select(TEAM, young_count, peak_count, aging_count) %>%
  pivot_longer(-TEAM, names_to = "age_group", values_to = "count") %>%
  mutate(age_group = factor(age_group,
                            levels = c("young_count", "peak_count", "aging_count"),
                            labels = c("Under 24", "24-29 (Peak)", "30+ (Aging)")))
plot11 <- ggplot(age_long, aes(x = reorder(TEAM, count), y = count, fill = age_group)) +
  geom_col(position = "stack", width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("Under 24" = "#2ecc71",
                               "24-29 (Peak)" = "#3498db",
                               "30+ (Aging)"= "#e74c3c")) +
  labs(title = "Sqaud Age Profile -- Top 20 Nations",
       subtitle = "Based on top 23 players by OVR rating",
       x = NULL, y = "Player Count", fill = "Age Group") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")
print(plot11)

#POSITION STRENGTH RADAR
position_long <- master %>%
  dplyr::select(TEAM, Goalkeeper, Defender, Midfielder, Winger, Forward) %>%
  pivot_longer(-TEAM, names_to = "position", values_to = "avg_ovr") %>%
  filter(!is.na(avg_ovr))
  
plot12 <- ggplot(position_long, aes(x = position, y = avg_ovr,
                                    group = TEAM, color = TEAM)) +
  geom_line(alpha = 0.4) +
  geom_point(alpha = 0.6, size = 2) +
  stat_summary(aes(group = 1), fun = mean, geom = "line",
               color = "black", linewidth = 1.5,
               linetype = "dashed") +
  scale_color_viridis_d() +
  labs(title = "Position Strength by Nation",
       subtitle = "Avg OVR of top 5 players per position -- dashed line = overall average",
       x = "Position", y = "Average OVR (top 5)",
       color = "Nation") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right",
        legend.text = element_text(size = 7))
print(plot12)

#MONTE CARLO TOURNAMENT SIMULATION
groups <- list(
  A = c("Mexico",  "South Africa",       "South Korea",      "Czech Republic"),
  B = c("Canada",         "Bosnia and Herzegovina",      "Qatar", "Switzerland"),
  C = c("Brazil",         "Morocco",      "Haiti",      "Scotland"),
  D = c("United States",         "Paraguay",       "Australia",        "Turkey"),
  E = c("Germany",        "Curacao",      "Ivory Coast",     "Ecuador"),
  F = c("Netherlands",        "Japan",     "Sweden",      "Tunisia"),
  G = c("Belgium",      "Egypt",        "Iran",  "New Zealand"),
  H = c("Spain",         "Cape Verde",     "Saudi Arabia",  "Uruguay"),
  I = c("France",          "Senegal",       "Iraq",        "Norway"),
  J = c("Argentina",    "Algeria",    "Austria",     "Jordan"),
  K = c("Portugal",       "DR Congo",      "Uzbekistan",         "Colombia"),
  L = c("England",        "Croatia",  "Ghana",       "Panama")
)

team_strength <- worldcup_2026 %>%
  dplyr::select(TEAM, goals_per_game, goals_conceded_per_game,
                recent_win_rate, composite_score, confederation) %>%
  mutate(
    attack = as.numeric(scale(goals_per_game)),
    defence = as.numeric(scale(-goals_conceded_per_game))
  )
fill_missing <- function(team_name, conf, df) {
  if (team_name %in% df$TEAM) return(df)
  conf_avg <- df %>%
    dplyr::filter(confederation == conf) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE))
  conf_avg$TEAM <- team_name
  conf_avg$confederation <- conf
  bind_rows(df, conf_avg)
}

simulate_match <- function(team_a, team_b, strength_df) {
  a <- strength_df[strength_df$TEAM == team_a, ]
  b <- strength_df[strength_df$TEAM == team_b, ]
  
  if (nrow(a) == 0 || nrow(b) == 0) {
    # If team not found, 50/50
    return(if (runif(1) > 0.5) team_a else team_b)
  }
  
  comp_a <- ifelse(length(a$composite_score) > 0, a$composite_score[1], 0.5)
  comp_b <- ifelse(length(b$composite_score) > 0, b$composite_score[1], 0.5)
  
  lambda_a <- max(0.3, 1.0 + 1.5 * (comp_a - comp_b) + rnorm(1, 0, 0.15))
  lambda_b <- max(0.3, 1.0 + 1.5 * (comp_b - comp_a) + rnorm(1, 0, 0.15))
  
  goals_a <- rpois(1, lambda_a)
  goals_b <- rpois(1, lambda_b)
  
  if (goals_a > goals_b) return(team_a)
  if (goals_b > goals_a) return(team_b)
  
  prob_a <- comp_a / (comp_a + comp_b)
  if (runif(1) < prob_a) return(team_a) else return(team_b)
}

#GROUP STAGE
simulate_group_stage <- function (groups, strength_df) {
  group_winners <- c()
  group_runners <- c()
  all_third <- list()
  
  for (grp_name in names(groups)) {
    teams <- groups[[grp_name]]
    teams <- teams[teams %in% strength_df$TEAM]
    if (length(teams) < 2) next
    pts   <- setNames(rep(0, length(teams)), teams)
    gf    <- setNames(rep(0, length(teams)), teams)
    ga    <- setNames(rep(0, length(teams)), teams)
    
    matchups <- combn(teams, 2, simplify = FALSE)
    
    for (match in matchups) {
      t1 <- match[1]
      t2 <- match[2]
      
      s1 <- strength_df[strength_df$TEAM == t1, ]
      s2 <- strength_df[strength_df$TEAM == t2, ]
      
      lambda1 <- max(0.3, 1.2 + 0.35 * s1$attack[1] - 0.25 * s2$defence[1])
      lambda2 <- max(0.3, 1.2 + 0.35 * s2$attack[1] - 0.25 * s1$defence[1])
      
      g1 <- rpois(1, lambda1)
      g2 <- rpois(1, lambda2)
      
      gf[t1] <- gf[t1] + g1
      ga[t1] <- ga[t1] + g2
      gf[t2] <- gf[t2] + g2
      ga[t2] <- ga[t2] + g1
      
      if (g1 > g2) {pts[t1] <- pts[t1] + 3}
      else if (g2 > g1) { pts[t2] <- pts[t2] + 3 }
      else { pts[t1] <- pts[t1] + 1; pts[t2] <- pts[t2] + 1}
    }
    
    gd <- gf - ga
    ranking <- order(-pts, -gd, -gf)
    ranked_teams <- teams[ranking]
    
    group_winners <- c (group_winners, ranked_teams[1])
    group_runners <- c(group_runners, ranked_teams[2])
  
    if (length(ranked_teams) >= 3) {
      all_third[[grp_name]] <- data.frame(
        TEAM = ranked_teams[3],
        pts = pts[ranked_teams[3]],
        gd = gd[ranked_teams[3]],
        gf = gf[ranked_teams[3]]
      )
    }
  }

#Best 8 3rd place teams
  third_df    <- do.call(rbind, all_third)
  third_order <- order(-third_df$pts, -third_df$gd, -third_df$gf)
  best_third  <- third_df$TEAM[third_order[1:min(8, nrow(third_df))]]
  
  list(
    winners    = group_winners,
    runners    = group_runners,
    best_third = best_third,
    r32_teams  = c(group_winners, group_runners, best_third)
  )
}


#KNOCKOUT SIMULATION
simulate_knockout <- function(teams, strength_df)
{
  current_round <- teams
  while (length(current_round) > 1) {
    next_round <- c()
    # Pair teams sequentially
    for (i in seq(1, length(current_round) - 1, by = 2)) {
      winner <- simulate_match(current_round[i],
                               current_round[i+1], strength_df)
      next_round <- c(next_round, winner)
    }
    if (length(current_round) %% 2 == 1) {
      next_round <- c(next_round, current_round[length(current_round)])
    }
    current_round <- next_round
  }
  return(current_round[1])
}

#ENTIRE TOURNAMENT SIMULATION
set.seed(2026)
N_SIM <- 10000

all_teams <- worldcup_2026$TEAM
win_count <- setNames(rep(0, length(all_teams)), all_teams)
final_count <- setNames(rep(0, length(all_teams)), all_teams)
semi_count <- setNames(rep(0, length(all_teams)), all_teams)
r16_count    <- setNames(rep(0, length(all_teams)), all_teams)
group_count  <- setNames(rep(0, length(all_teams)), all_teams)
cat("\nRunning", N_SIM, "simulations...\n")

for (sim in 1:N_SIM) {
  
  # Group stage
  gs_result <- simulate_group_stage(groups, team_strength)
  r32_teams <- gs_result$r32_teams
  
  # Track group qualifiers
  for (t in r32_teams) {
    if (t %in% names(group_count)) group_count[t] <- group_count[t] + 1
  }
  
  # Shuffle for random bracket
  r32_shuffled <- sample(r32_teams, length(r32_teams))
  
  # Round of 32
  if (length(r32_shuffled) >= 16) {
    r16_teams <- c()
    for (i in seq(1, min(32, length(r32_shuffled)) - 1, by = 2)) {
      winner <- simulate_match(r32_shuffled[i], r32_shuffled[i+1], team_strength)
      r16_teams <- c(r16_teams, winner)
    }
  } else {
    r16_teams <- r32_shuffled
  }
  
  # Track R16 qualifiers
  for (t in r16_teams) {
    if (t %in% names(r16_count)) r16_count[t] <- r16_count[t] + 1
  }
  
  # Quarter-finals
  qf_teams <- c()
  for (i in seq(1, length(r16_teams) - 1, by = 2)) {
    winner <- simulate_match(r16_teams[i], r16_teams[i+1], team_strength)
    qf_teams <- c(qf_teams, winner)
  }
  
  # Semi-finals
  sf_teams <- c()
  for (i in seq(1, length(qf_teams) - 1, by = 2)) {
    winner <- simulate_match(qf_teams[i], qf_teams[i+1], team_strength)
    sf_teams <- c(sf_teams, winner)
  }
  
  # Track semi-finalists
  for (t in sf_teams) {
    if (t %in% names(semi_count)) semi_count[t] <- semi_count[t] + 1
  }
  
  # Final
  if (length(sf_teams) >= 2) {
    finalist1 <- sf_teams[1]
    finalist2 <- sf_teams[2]
    final_count[finalist1] <- final_count[finalist1] + 1
    final_count[finalist2] <- final_count[finalist2] + 1
    champion <- simulate_match(finalist1, finalist2, team_strength)
    win_count[champion] <- win_count[champion] + 1
  }
  
  if (sim %% 1000 == 0) cat("  Completed", sim, "simulations...\n")
  
}
cat("Done!\n")

#RESULTS
mc_results <- data.frame(
  TEAM          = names(win_count),
  win_pct       = round(win_count   / N_SIM * 100, 2),
  final_pct     = round(final_count / N_SIM * 100, 2),
  semi_pct      = round(semi_count  / N_SIM * 100, 2),
  r16_pct       = round(r16_count   / N_SIM * 100, 2),
  group_adv_pct = round(group_count / N_SIM * 100, 2)
) %>%
  left_join(worldcup_2026 %>% dplyr::select(TEAM, confederation, composite_score, rank),
            by = "TEAM") %>%
  arrange(desc(win_pct))

cat("\n=== 2026 WORLD CUP MONTE CARLO RESULTS (", N_SIM, "simulations) ===\n")
print(mc_results %>% head(20) %>%
        dplyr::select(TEAM, confederation, win_pct, final_pct, semi_pct, composite_score))
print(mc_results)

worldcup_2026 %>%
  dplyr::filter(TEAM %in% c("Australia", "Argentina", "Iran", 
                            "Spain", "France", "Brazil")) %>%
  dplyr::select(TEAM, composite_score, goal_diff_per_game, 
                win_rate, confederation)

write.csv(mc_results, 
          "C:/Users/laila/Documents/Football Project/monte_carlo_results_2026.csv",
          row.names = FALSE)

cat("Saved! Rows:", nrow(mc_results), "\n")
print(head(mc_results, 5))