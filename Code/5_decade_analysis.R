### author: Joey O'Brien
### date: 18/10/20
### perform the pagerank in each decade
### to reproduce Table 3

# libraries
source('./Code/source.R')
# data
tourn_df <- read_csv('./Data/final_tourn_df.csv')
df <- read_csv('./Data/final_match_df.csv')

### each decade
years_to_do <- tibble(year_start = c(1970, 1980, 1990, 2000, 2010),
                      year_end = c(1979, 1989, 1999, 2009, 2019))
### compute the pagerank score
era_df <- tibble()
for(i in 1:nrow(years_to_do)){
  year = years_to_do %>% slice(i)
  era_df <- era_df %>% bind_rows(., df %>%
                                   filter(season <= year$year_end & season >= year$year_start) %>%
                                   page_rank_sim(.) %>%
                                   slice(1:10) %>%
                                   select(rank_pre, player) %>%
                                   mutate(era = rep(paste(year$year_start, year$year_end, sep ='-'), nrow(.))))
}

era_df %>%
  pivot_wider(names_from = era, values_from = player)

