### perform the seasonal pagerank analysis
### to reproduce Table 2

# libraries
source('./Code/source.R')
# data
tourn_df <- read_csv('./Data/final_tourn_df.csv')
df <- read_csv('./Data/final_match_df.csv')
number_one_df <- read_csv('./Data/number_ones.csv')

### analysis
## compute the pagerank score for each season
years <- seq(min(df$season), max(df$season))
year_df <- tibble()
for(i in 1:length(years)){
  year = years[i]
  year_df <- year_df %>% bind_rows(., df %>%
                                     filter(season == year) %>%
                                     page_rank_sim(.) %>%
                                     select(player, rank_pre, rank_str) %>%
                                     pivot_longer(cols = 2:3, values_to = 'rank') %>%
                                     filter(rank == 1) %>%
                                     select(player, method = name) %>%
                                     mutate(year = year))
}

## load in the official world rankings
number_one_df <- number_one_df %>%
  mutate(year = str_extract(season, "[^-]+"),
         year = as.integer(year) - 1)
number_one_df <- number_one_df %>%
  mutate(method = rep('WS', nrow(.))) %>%
  select(player,method,year)

full_no1_df <- bind_rows(year_df, number_one_df %>%
                           mutate(year = as.integer(year)))

full_no1_df %>%
  pivot_wider(names_from = method, values_from = player) %>%
  select(year, rank_pre, rank_str, WS)
