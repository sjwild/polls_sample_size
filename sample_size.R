library(tidyverse)
library(lubridate)
library(magrittr)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(tidybayes) 
library(here)
library(ThemePark)
library(ggtext)
library(britpol)


# Load polls between 2017 and 2019 election
data("pollbase")
election_day_2017 <- ymd("2017-06-08")
election_day_2019 <- ymd("2019-12-12")
d <- subset(pollbase, end %in% election_day_2017:election_day_2019)


# Prep data
N_days <- as.numeric(election_day_2019 - election_day_2017) + 1
N_polls <- nrow(d)
N_pollsters <- length(unique(d$pollster))
y <- d$con
y_moe <- sqrt(y * (1 - y) / d$n)
start_election <- 0.423
end_election <- 0.436
poll_date <- as.numeric(d$end - election_day_2017) + 1
pollster_id <- as.numeric(as.factor(d$pollster))
nu <- 10


# for plotting
pollster_map <- levels(as.factor(d$pollster))

data_list <- list(
  N_days = N_days,
  N_polls = N_polls,
  N_pollsters = N_pollsters,
  start_election = start_election,
  end_election = end_election,
  y = y,
  y_moe = y_moe,
  poll_date = poll_date,
  pollster_id = pollster_id,
  nu = nu,
  sample_size_reported = d$n

  
)

# fit cmdstan model
mod <- cmdstan_model("jackman_pooling_polls.stan")



fit <- mod$sample(data = data_list,
                  seed = 431343,
                  chains = 4,
                  parallel_chains = 4,
                  iter_warmup = 1000,
                  iter_sampling = 1000,
                  max_treedepth = 12)



# plot estimated sample size vs real
sample_rows <- sample(row.names(d), size = 100, replace = FALSE)
ratio_rvar <- as_draws_rvars(fit$draws("sample_ratio"))
ratio_rvar %>%
  spread_draws(sample_ratio[j]) %>%
  filter(j %in% sample_rows) %>%
  ggplot(aes(y = reorder(as.factor(j), desc(sample_ratio)), x = sample_ratio)) +
  stat_pointinterval(.width = c(.50, .80, .95),
                     show.legend = FALSE,
                     colour = barbie_theme_colors[7]) +
  scale_y_discrete(labels = NULL) +
  labs(title = "Ratio of effective to reported sample size",
       subtitle = "One hundred randomly chosen polls",
       y = NULL,
       x = "Ratio",
       caption = "This is a first attempt, so estimates are probably wrong
       Source: {britpol} R package
       Chart by sjwild.github.io. 2023/Sept/29") +  
  coord_flip() +
  theme_barbie() +
  theme(plot.caption = element_text(size = 10),
        legend.position = "top",
        legend.direction = "horizontal",
        #plot.title = element_markdown(),
        plot.background = element_rect(fill = barbie_theme_colors["panel"]),
        axis.ticks.x = element_blank())


# Plot histogram of values
ratio_rvar %>%
  spread_draws(sample_ratio[j]) %>%
  ggplot(aes(x = sample_ratio)) +
  geom_histogram(fill = barbie_theme_colors[7]) +
  scale_y_continuous(labels = NULL) +
  labs(title = "Histogram of the ratio of effective to reported sample size",
       y = NULL,
       x = "Ratio",
       caption = "This is a first attempt, so estimates are probably wrong
       Source: {britpol} R package
       Chart by sjwild.github.io. 2023/Sept/29") +  
  theme_barbie() +
  theme(plot.caption = element_text(size = 10),
        legend.position = "top",
        legend.direction = "horizontal",
        plot.subtitle = element_markdown(),
        plot.background = element_rect(fill = barbie_theme_colors["panel"]),
        axis.ticks.y = element_blank())
  

sigma_rvar <- as_draws_rvars(fit$draws("sigma"))
sigma_rvar %>%
  spread_draws(sigma[j]) %>%
  ggplot(aes(y = j, x = sigma)) +
  stat_pointinterval(.width = c(.50, .80, .95),
                     show.legend = FALSE,
                     colour = barbie_theme_colors[7]) +
  geom_point(aes(x = y_moe, y = 1:457), data = data.frame(y_moe = y_moe)) +
  scale_y_discrete(labels = NULL) +
  labs(title = "Ratio of effective to reported sample size",
       subtitle = "One hundred randomly chosen polls",
       y = NULL,
       x = "Ratio",
       caption = "This is a first attempt, so estimates are probably wrong
       Source: {britpol} R package
       Chart by sjwild.github.io. 2023/Sept/29") +  
  coord_flip() +
  theme_barbie() +
  theme(plot.caption = element_text(size = 10),
        legend.position = "top",
        legend.direction = "horizontal",
        #plot.title = element_markdown(),
        plot.background = element_rect(fill = barbie_theme_colors["panel"]),
        axis.ticks.x = element_blank())


# Plot sigma_pollster
sigma_pollster_rvar <- as_draws_rvars(fit$draws("sigma_pollster"))
sigma_pollster_rvar %>%
  spread_draws(sigma_pollster[j]) %>%
  ggplot(aes(y = reorder(as.factor(j), desc(sigma_pollster)), x = sigma_pollster)) +
  stat_pointinterval(.width = c(.50, .80, .95),
                     show.legend = FALSE,
                     colour = barbie_theme_colors[7]) +
  scale_y_discrete(labels = pollster_map,
                   breaks = 1:N_pollsters) +
  labs(title = "Sigma pollster",
       y = NULL,
       x = "Size",
       caption = "This is a first attempt, so estimates are probably wrong
       Source: {britpol} R package
       Chart by sjwild.github.io. 2023/Sept/29") +  
  theme_barbie() +
  theme(plot.caption = element_text(size = 10),
        legend.position = "top",
        legend.direction = "horizontal",
        #plot.title = element_markdown(),
        plot.background = element_rect(fill = barbie_theme_colors["panel"]),
        axis.ticks.x = element_blank())
