### author: Joey O'Brien
### date: 18/10/20
### compare the different ranking measures from 1990-2019
### via the Jaccard similarity to reproduce Figure 5

# libraries
source('./Code/source.R')
# data
tourn_df <- read_csv('./Data/final_tourn_df.csv')
df <- read_csv('./Data/final_match_df.csv')
rank_df <- read_csv('./Data/WS_ranks.csv')

### years to analyze
seasons <- seq(1990,2019)
full_df <- tibble()

# perform the pagerank for each year
for(year in seasons){
  full_df <- bind_rows(full_df,
                       df %>%
                         filter(walkover != TRUE,
                                season == year) %>%
                         page_rank_sim(.) %>%
                         filter(rank_pre <= 50 | rank_str <= 50) %>%
                         mutate(year = year)
  )
}
full_df <- full_df %>%
  select(player, rank_pre, rank_str, year)


rank_df <- rank_df %>%
  filter(season %in% seasons) %>%
  arrange(season, desc(-rank)) %>%
  group_by(season) %>%
  filter(rank <= 50) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 50) %>%
  select(player, rank, season)

### now using the three scores (ws, pagerank, & in-strength)

all_method_ranks <- full_df %>%
  select(year, player, rank_pre, rank_str) %>%
  pivot_longer(3:4, values_to = 'rank', names_to = 'method') %>%
  filter(rank <= 50) %>%
  mutate(year = as.integer(year)) %>%
  bind_rows(. ,rank_df %>%
              ungroup() %>%
              select(season, player, rank) %>%
              mutate(method = rep('rank_ws', nrow(.)),
                     season = as.integer(season)) %>%
              rename('year' = 'season')) %>%
  mutate(player = match(.$player, unique(.$player))) %>%
  filter(rank <= 100) %>%
  arrange(desc(-year), desc(-rank)) %>%
  pivot_wider(names_from = 'method', values_from = 'player')

# now to compare the three ranks by Jaccard similarity
# for a number of different rank ranges - top 5, 10, 25, 50
Jacc_df <- tibble()
thres_vals = c(5, 10, 25, 50)
for(thres in thres_vals){
  for(season in seasons){
    Jacc_df <-  Jacc_df %>%
      bind_rows(., all_method_ranks %>%
                  filter(year == season, rank <= thres) %>%
                  summarize(year = season,
                            thres = thres,
                            pre_ws = jac_similarity(rank_pre, rank_ws),
                            pre_str = jac_similarity(rank_pre, rank_str)))
  }
}

### now to do the plots

j50 <- Jacc_df %>%
  pivot_longer(cols = 3:4, names_to = 'method',
               values_to = 'Jaccard') %>%
  filter(thres == 50) %>%
  ggplot(., aes(x = year, y = Jaccard, color = method)) +
  geom_line() +
  geom_line() +
  geom_point(size = 2.9, color = 'white') +
  geom_point(size = 2.5) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~ (. * 100 /(. + 1)),
                                         name = 'Number of Players',
                                         breaks = c(35,40,45))
  ) +
  scale_x_continuous(breaks = seq(1990,2020,2)) +
  labs(x = 'Season',
       y = NULL) +
  scale_color_manual(name = '',
                     breaks = c('pre_str', 'pre_ws'),
                     values = c('#008DA8','#C3627D'),
                     labels = c('In-strength', 'World Snooker')) +
  theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        plot.title.position = 'plot',
        panel.grid.minor.x = element_blank(),
        legend.text=element_text(size=12),
        legend.position = c(0.8,0.2),
        axis.ticks.y = element_line(colour = ablack),
        axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(size = 14, face = 'bold'),
        axis.title=element_text(size=14))

####### other thresholds

j10 <- Jacc_df %>%
  pivot_longer(cols = 3:4, names_to = 'method',
               values_to = 'Jaccard') %>%
  filter(thres == 10) %>%
  ggplot(., aes(x = year, y = Jaccard, color = method)) +
  geom_line() +
  geom_line() +
  geom_point(size = 2.9, color = 'white') +
  geom_point(size = 2.5) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~ (. * 20 /(. + 1)),
                                         name = 'Number of Players',
                                         breaks = c(5,6,7,8,9,10))
  ) +
  scale_x_continuous(breaks = seq(1990,2020,2)) +
  labs(x = NULL,
       y = NULL
  ) +
  scale_color_manual(name = '',
                     breaks = c('pre_str', 'pre_ws'),
                     values = c('#008DA8','#C3627D'),
                     labels = c('In-strength', 'World Snooker')) +
  theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        plot.title.position = 'plot',
        panel.grid.minor.x = element_blank(),
        legend.text=element_text(size=12),
        legend.position = 'none',
        axis.ticks.y = element_line(colour = ablack),
        axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(size = 14, face = 'bold'),
        axis.title=element_text(size=14))

j5 <- Jacc_df %>%
  pivot_longer(cols = 3:4, names_to = 'method',
               values_to = 'Jaccard') %>%
  filter(thres == 5) %>%
  ggplot(., aes(x = year, y = Jaccard, color = method)) +
  geom_line() +
  geom_line() +
  geom_point(size = 2.9, color = 'white') +
  geom_point(size = 2.5) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~ (. * 10 /(. + 1)),
                                         name = NULL,
                                         breaks = c(1,2,3,4,5))
  ) +
  scale_x_continuous(breaks = seq(1990,2020,2)) +
  labs(x = NULL,
       y = 'Jaccard Similarity'
  ) +
  scale_color_manual(name = '',
                     breaks = c('pre_str', 'pre_ws'),
                     values = c('#008DA8','#C3627D'),
                     labels = c('In-strength', 'World Snooker')) +
  theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        plot.title.position = 'plot',
        panel.grid.minor.x = element_blank(),
        legend.text=element_text(size=12),
        legend.position = 'none',
        axis.ticks.y = element_line(colour = ablack),
        axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(size = 14, face = 'bold'),
        axis.title=element_text(size=14))

j25 <- Jacc_df %>%
  pivot_longer(cols = 3:4, names_to = 'method',
               values_to = 'Jaccard') %>%
  filter(thres == 25) %>%
  ggplot(., aes(x = year, y = Jaccard, color = method)) +
  geom_line() +
  geom_line() +
  geom_point(size = 2.9, color = 'white') +
  geom_point(size = 2.5) +
  theme_minimal() +
  scale_y_continuous(sec.axis = sec_axis(~ (. * 50 /(. + 1)),
                                         name = NULL,
                                         breaks = c(17,20,23))
  ) +
  scale_x_continuous(breaks = seq(1990,2020,2)) +
  labs(x = 'Season', y = 'Jaccard Similarity') +
  scale_color_manual(name = '',
                     breaks = c('pre_str', 'pre_ws'),
                     values = c('#008DA8','#C3627D'),
                     labels = c('In-strength', 'World Snooker')) +
  theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12),
        plot.title.position = 'plot',
        panel.grid.minor.x = element_blank(),
        legend.text=element_text(size=12),
        legend.position = 'none',
        axis.ticks.y = element_line(colour = ablack),
        axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(size = 14, face = 'bold'),
        axis.title=element_text(size=14))


plot_grid(j5, j10, j25, j50,
          ncol = 2, align ='hv',
          hjust = -0.1,
          labels = c('(a)', '(b)', '(c)', '(d)'))

ggsave('Images/Jaccard.pdf', height = 8, width = 12,
       units = 'in', device = cairo_pdf)
