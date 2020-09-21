### initial analysis and recreation of Fig.1

# libraries
source('source.R')
# data
tourn_df <- read_csv('../Data/final_tourn_df.csv')
df <- read_csv('../Data/final_match_df.csv')

### initial analysis
## how many matches?

df %>%
  select(match_id) %>%
  unique() %>%
  nrow(.)

## how many players?

df %>%
  select(match_id, player_1, player_2) %>%
  pivot_longer(2:3, values_to = 'player') %>%
  select(player) %>%
  unique() %>%
  nrow(.)

## how many tournaments

tourn_df %>%
  nrow()

## recreate figure 1

# number of tournaments by season
tourn_season_plot <- df %>%
  mutate(season = as.integer(season)) %>%
  group_by(season) %>%
  summarise(no_tournaments = n_distinct(tourn_id)) %>%
  ggplot(aes(x = season, y = no_tournaments)) +
  geom_bar(stat='identity', fill = '#008DA8',
           color = ablack) +
  scale_x_continuous(expand = c(0,0), breaks = seq(1968,2020,2)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  labs(x = 'Season', y = 'Number of Tournaments') +
  theme(axis.line = element_line(colour = ablack),
        axis.text=element_text(size=12),
        plot.title.position = 'plot',
        panel.grid.major.x = element_blank(),
        legend.text=element_text(size=12),
        legend.position = c(0.85,0.9),
        axis.ticks.y = element_line(colour = ablack),
        axis.ticks.x = element_line(colour = ablack),
        axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(size = 14, face = 'bold'),
        axis.title=element_text(size=14))
tourn_season_plot

# number of players by season
player_season_plot <- df %>%
  select(season, player_1, player_2) %>%
  pivot_longer(cols = 2:3, values_to = 'player') %>%
  mutate(season = as.integer(season)) %>%
  group_by(season) %>%
  summarise(no_players = n_distinct(player)) %>%
  ggplot(aes(x = season, y = no_players)) +
  geom_bar(stat='identity', fill = '#008DA8',
           color = ablack) +
  theme_minimal() +
  scale_x_continuous(expand = c(0,0), breaks = seq(1968,2020,2)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = 'Season', y = 'Number of Players') +
  theme(axis.line = element_line(colour = ablack),
        axis.text=element_text(size=12),
        plot.title.position = 'plot',
        panel.grid.major.x = element_blank(),
        legend.text=element_text(size=12),
        legend.position = c(0.85,0.9),
        axis.ticks.y = element_line(colour = ablack),
        axis.ticks.x = element_line(colour = ablack),
        axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(size = 14, face = 'bold'),
        axis.title=element_text(size=14))

# find the result frequencies of each player
player_result_df <- df %>%
  filter(walkover != TRUE) %>%
  select(match_id, season, player_1, player_2, player_1_score, player_2_score) %>%
  mutate(winner = if_else(player_1_score > player_2_score,
                          player_1, player_2),
         loser = if_else(player_1_score > player_2_score,
                         player_2, player_1),
  ) %>%
  select(match_id, winner, loser) %>%
  pivot_longer(cols = 2:3, values_to = 'player', names_to = 'result') %>%
  group_by(result, player) %>%
  tally() %>%
  ungroup %>%
  pivot_wider(names_from = 'result', values_from = 'n') %>%
  mutate(loser = replace_na(loser, 0),
         winner = replace_na(winner,0),
         total = loser + winner) %>%
  pivot_longer(cols = 2:4, names_to = 'result',
               values_to = 'value')

# plot the pdf of results obtained by each player
result_pdf <- player_result_df %>%
  group_by(result, value) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  filter(value != 0) %>%
  ggplot(aes(x = value, y = freq, col = result)) +
  geom_point() +
  scale_y_log10(labels=trans_format('log10',math_format(10^.x))) +
  scale_x_log10(labels=trans_format('log10',math_format(10^.x))) +
  labs(y = 'PDF',
       x = 'Number of Matches'
  ) +
  theme_minimal() +
  scale_color_manual(name = '',
                     breaks = c('total', 'winner', 'loser'),
                     values = c('#008DA8','#C3627D','#4A904D'),
                     labels = c('Total', 'Won', 'Lost')) +
  theme(axis.line = element_line(colour = ablack),
        axis.text=element_text(size=12),
        plot.title.position = 'plot',
        legend.text=element_text(size=12),
        legend.position = c(0.85,0.85),
        plot.title = element_text(size = 14, face = 'bold'),
        axis.title=element_text(size=14))

# ... and the corresponding CCDF
result_ccdf <- player_result_df %>%
  group_by(result, value) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n),
         ccdf = 1 - cumsum(freq)) %>%
  filter(value != 0) %>%
  ggplot(aes(x = value, y = ccdf, col = result)) +
  geom_point() +
  scale_y_log10(labels=trans_format('log10',math_format(10^.x))) +
  scale_x_log10(labels=trans_format('log10',math_format(10^.x))) +
  labs(y = 'CCDF',
       x = 'Number of Matches'
  ) +
  theme_minimal() +
  scale_color_manual(name = '',
                     breaks = c('total', 'winner', 'loser'),
                     values = c('#008DA8','#C3627D','#4A904D'),
                     labels = c('Total', 'Won', 'Lost')) +
  theme(axis.line = element_line(colour = ablack),
        axis.text=element_text(size=12),
        plot.title.position = 'plot',
        legend.text=element_text(size=12),
        legend.position = c(0.85,0.85),
        plot.title = element_text(size = 14, face = 'bold'),
        axis.title=element_text(size=14))
result_ccdf

# all the plots together
plot_grid(tourn_season_plot,
          player_season_plot,
          result_pdf, result_ccdf,
          labels = c('(a)', '(b)', '(c)', '(d)'),
          axis = 'b',
          align = 'v',
          hjust = -0.1)
