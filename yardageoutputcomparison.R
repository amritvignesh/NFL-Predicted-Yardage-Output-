install.packages("tidyverse")
install.packages("nflfastr")
install.packages("vip")
install.packages("ggimage")
install.packages("ggplot2")
install.packages("gt")
install.packages("nflreadr")
install.packages("dplyr")
install.packages("nflplotR")
install.packages("ggrepel")

library(tidyverse)
library(nflfastR)
library(vip)
library(ggimage)
library(ggplot2)
library(gt)
library(nflreadr)
library(dplyr)
library(nflplotR)
library(ggrepel)


pbp <- load_pbp(2022)
colnames(pbp)


team_stats <- pbp %>%
  filter(!is.na(posteam)) %>%
  mutate(ispass = ifelse(play_type == "pass", 1, 0)) %>%
  mutate(isrun = ifelse(play_type == "run", 1, 0)) %>%
  group_by(posteam) %>%
  filter(!is.na(isrun)) %>%
  filter(!is.na(ispass)) %>%
  filter(!is.na(yards_gained)) %>%
  summarize(passrate = mean(ispass), runrate = mean(isrun), yards = mean(yards_gained))

log_passvsrun <- glm(formula = yards ~ passrate + runrate, data = team_stats)

log_passvsrun

summary(log_passvsrun)

vip(log_passvsrun)

team_stats <- team_stats %>%
  mutate(pred_yards = log_passvsrun$fitted.values)

colnames(team_stats)

team_stats %>%
  ggplot(aes(x = pred_yards, y = yards)) +
  geom_hline(yintercept = mean(team_stats$yards), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(team_stats$pred_yards), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_nfl_logos(aes(team_abbr = posteam), width = 0.03, alpha = 0.75) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm') +
  labs(x = "Predicted Yards Per Play",
       y = "Actual Yards Per Play",
       title = "NFL Team Predicted vs Actual Yards Per Play",
       subtitle = "Logistic Regression Model Based on Pass and Run Play Usage | 2022 NFL Season",
       caption = "Amrit Vignesh") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), plot.subtitle = element_text(size = 10, hjust = 0.5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))


