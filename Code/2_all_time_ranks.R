### perform the all-time pagerank analysis
### to reproduce Fig.2 and Table 1

# libraries
source('source.R')
# data
tourn_df <- read_csv('../Data/final_tourn_df.csv')
df <- read_csv('../Data/final_match_df.csv')

set.seed(1)
# perform the pagerank
all_time_df <- page_rank_sim(df)

# recreate figure 2
all_time_df %>%
  filter(rank_pre <= 30 | rank_str <= 30) %>%
  ggplot(aes(x = rank_str, y = rank_pre, label = player)) +
  geom_abline(slope = 1, intercept = 0,
              color="red",
              linetype="dashed", size=1) +
  geom_point() +
  theme_minimal() +
  geom_text_repel(size = 2.25) +
  labs(x = 'Rank by Wins',
       y = 'Rank by Prestige') +
  theme(axis.line = element_line(colour = ablack),
        axis.text=element_text(size=10),
        plot.title.position = 'plot',
        plot.title = element_text(size = 14, face = 'bold'),
        plot.subtitle = element_text(size = 12),
        axis.title=element_text(size=12))

# correlation of different ranks
all_time_df %>%
  filter(rank_pre <= 30 | rank_str <= 30) %>%
  summarize(cor(rank_pre, rank_str, method = c('pearson')),
            cor(rank_pre, rank_str, method = c('kendall')),
            cor(rank_pre, rank_str, method = c('spearman')))

# top 20 players
all_time_df %>%
  slice(1:20)
