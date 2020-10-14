### compare the different ranking measures during the 2018-19 season
### reproduce Figure 4

# libraries
source('./Code/source.R')
# data
tourn_df <- read_csv('./Data/final_tourn_df.csv')
df <- read_csv('./Data/final_match_df.csv')
rank_df <- read_csv('./Data/WS_ranks.csv')
# year to analyze
year <- 2018

# combine the ranks after doing the pagerank
ranks_all <- df %>%
  filter(season == year) %>%
  page_rank_sim(.) %>%
  select(player, rank_pre, rank_str) %>%
  pivot_longer(cols = 2:3, values_to = 'rank', names_to = 'method') %>%
  bind_rows(., rank_df %>%
              filter(season == year) %>%
              select(player, rank) %>%
              mutate(method = rep('rank_ws', nrow(.)))) %>%
  pivot_wider(names_from = 'method', values_from = 'rank')

# plot pagerank vs win rank
pre_str <- ranks_all %>%
  select(player, rank_pre, rank_str) %>%
  filter(rank_pre <= 30 | rank_str <= 30) %>%
  ggplot(aes(x = rank_str, y = rank_pre, label = player)) +
  geom_abline(slope = 1, intercept = 0,
              color="red",
              linetype="dashed", size=1) +
  geom_point() +
  theme_minimal() +
  geom_text_repel(size = 2.25) +
  labs(x = 'Rank by Wins',
       y = 'Rank by PageRank') +
  theme(axis.line = element_line(colour = ablack),
        axis.text=element_text(size=10),
        plot.title.position = 'plot',
        plot.title = element_text(size = 14, face = 'bold'),
        plot.subtitle = element_text(size = 12),
        axis.title=element_text(size=12))
# correlation of the two
ranks_all %>%
  select(player, rank_pre, rank_str) %>%
  filter(rank_pre <= 30 | rank_str <= 30) %>%
  summarise(kendall = cor(rank_pre, rank_str, method = 'kendall'),
            spearman = cor(rank_pre, rank_str, method = 'spearman'))

# plot pagerank rank vs world snooker rank
pre_ws <- ranks_all %>%
  select(player, rank_pre, rank_ws) %>%
  filter(rank_pre <= 30 | rank_ws <= 30) %>%
  ggplot(aes(x = rank_ws, y = rank_pre, label = player)) +
  geom_abline(slope = 1, intercept = 0,
              color="red",
              linetype="dashed", size=1) +
  geom_point() +
  theme_minimal() +
  geom_text_repel(size = 2.25) +
  labs(x = 'World Snooker Ranks',
       y = 'Rank by PageRank') +
  theme(axis.line = element_line(colour = ablack),
        axis.text=element_text(size=10),
        plot.title.position = 'plot',
        plot.title = element_text(size = 14, face = 'bold'),
        plot.subtitle = element_text(size = 12),
        axis.title=element_text(size=12))
# correlation between the two
ranks_all %>%
  select(player, rank_pre, rank_ws) %>%
  filter(rank_pre <= 30 | rank_ws <= 30) %>%
  summarise(kendall = cor(rank_pre, rank_ws, method = 'kendall'),
            spearman = cor(rank_pre, rank_ws, method = 'spearman'))

# two plots together
plot_grid(pre_str, pre_ws,
          ncol = 2, align ='hv',
          hjust = -0.1,
          labels = c('(a)', '(b)'))

ggsave('Images/2018_ranks.pdf', width = 12,
       height = 4, units = 'in',
       device = cairo_pdf)
