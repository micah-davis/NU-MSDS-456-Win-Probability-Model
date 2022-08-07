library(nflfastR)
library(dplyr)
library(ggplot2)
library(caTools)

pbp_2021 <- load_pbp(2021)
pbp_2021 <- pbp_2021 %>% mutate(winner = ifelse(home_score > away_score,
                                                home_team,
                                                away_team))
pbp_2021 <- pbp_2021 %>% mutate(poswins = ifelse(winner == posteam, "Yes",
                                                 "No"))
pbp_2021$poswins <- as.factor(pbp_2021$poswins)

filtered_pbp_2021 <- pbp_2021 %>%
  filter(qtr <= 4 & poswins != "NA" & play_type != "no_play"
         & play_type != "NA") %>%
  select(game_id, home_team, away_team, yardline_100,
         game_seconds_remaining/60, posteam, poswins, down,
         ydstogo, score_differential, home_wp, away_wp, wp,
         desc)

set.seed(123)
split = sample.split(filtered_pbp_2021$poswins, SplitRatio = 0.8)
train = filtered_pbp_2021 %>% filter(split == TRUE)
test = filtered_pbp_2021 %>% filter(split == FALSE)

model1 <- glm(poswins ~ down +
              yardline_100 + game_seconds_remaining +
              ydstogo +
              score_differential,
              train,
              family = "binomial")
summary(model1)

pred_home = predict(model1, train, type = "response")

train <- cbind(train, pred_home)
train <- mutate(train, pred_home = ifelse(posteam == home_team,
                                      pred_home, 1 - pred_home))

ggplot(filter(train, game_id == "2021_01_ARI_TEN", !is.na(down)),
    aes(x=game_seconds_remaining/60, y = pred_home)) +
  geom_line(size = 2, color = "lightblue") + scale_x_reverse() +
  ylim(c(0,1)) + theme_minimal() +
  xlab("Time Remaining") + ylab("Win Probability") + 
  geom_line(aes(game_seconds_remaining/60, 1 - pred_home), col = "red",
            size = 2) +
  geom_line(aes(game_seconds_remaining/60, home_wp), col = "black") +
  geom_line(aes(game_seconds_remaining/60, away_wp), col = "darkgray") +
  annotate("text", x = 10, y = 0.9, label = "Davis WP Model",
           col = "red") +
  annotate("text", x = 10, y = 0.85, label = "nflfastR WP Cardinals",
           col = "darkgray") +
  annotate("text", x = 10, y = .3, label = "nflfastR WP Titans",
           col = "black") +
  annotate("text", x = 10, y = 0.25, label = "Davis WP Model",
           col = "lightblue") +
  geom_vline(aes(xintercept = 30), lty = "dashed") +
  geom_vline(aes(xintercept = 45), lty = "dashed") +
  geom_vline(aes(xintercept = 15), lty = "dashed")
  