### author: Joey O'Brien
### date: 18/10/20
### visualize the PageRank rankings using the rank
### clock as per Figure 6

# libraries
source('./Code/source.R')
# data
tourn_df <- read_csv('./Data/final_tourn_df.csv')
df <- read_csv('./Data/final_match_df.csv')
rank_df <- read_csv('./Data/WS_ranks.csv')

all_time_df <- page_rank_sim(df)
# maximum rank to show on plots
max_rank <- 50
# which players to plot
players_rank_clock <- all_time_df %>%
  slice(1:12) %>%
  pull(player)

full_df <- tibble()
# calculate pagerank for each season
for(year in df$season %>% unique()){
  temp <- page_rank_sim(df %>% filter(season == year))
  full_df <- full_df %>%
    bind_rows(., temp %>%
                filter(player %in% players_rank_clock) %>%
                mutate(season = rep(year, nrow(.))))
}

# make the plot for each of the players

for(i in 1:length(players_rank_clock)){
  player_to_plot <- players_rank_clock[i]

  temp <- full_df %>%
    filter(player == player_to_plot) %>%
    select(rank = rank_pre, season) %>%
    mutate(angle = seq(from = 1, to = 321, length.out = nrow(.)),
           no1 = ifelse(rank == 1, 1, 0),
           label_angle = ifelse(angle > 180, -90 - angle, 90 - angle))

  hline_df <- tibble(y = 10 + c(1,seq(10, min(max_rank,
                                              (ceiling(max(temp$rank)/10)*10)), by = 10)),
                     yend = 10 + c(1,seq(10, min(max_rank,
                                                 (ceiling(max(temp$rank)/10)*10)), by = 10))) %>%
    mutate(x = rep(min(temp$angle), nrow(.)), xend = rep(max(temp$angle), nrow(.)))

  vline_df <- tibble(x = temp$angle,
                     xend = temp$angle) %>%
    mutate(y = rep(11, nrow(.)),
           yend = 10 + rep(min(max_rank,(ceiling(max(temp$rank)/10)*10)), nrow(.)))

  assign(paste('p',i,sep = ''), temp %>%
           ggplot(., aes(x = angle, y = 10 + rank, group = 1)) +
           geom_segment(data = hline_df,
                        aes(x = x, xend = xend, y = y, yend = yend),
                        colour = "grey70", size = 0.2) +
           geom_segment(data = vline_df,
                        aes(x = x, xend = xend, y = y, yend = yend),
                        colour = "grey70", size = 0.2) +
           theme_minimal() +
           theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.text.y = element_blank(),
                 plot.title = element_text(hjust = 0.5,
                                           color = ablack,
                                           size = 14),
                 axis.text.x = element_text(
                   color = ablack,
                   size = 8,
                   angle= temp$label_angle),
                 legend.position = 'none') +
           geom_line(color = ablack) +
           geom_point(color = 'white', size = 2.25) +
           geom_point(aes(color = as.factor(no1)), size = 2) +
           scale_color_manual(values = c('#4A904D', '#C3627D')) +
           scale_x_continuous(limits = c(0,360), breaks = temp$angle,
                              labels = paste('  ',temp$season,'  ')) +
           scale_y_continuous(limits = c(1, min(max_rank + 10,(ceiling(max(temp$rank)/10)*10) + 10))) +
           coord_polar() +
           annotate("text",
                    x = rep(0,length(c(1,seq(10, min(max_rank,(ceiling(max(temp$rank)/10)*10)), by = 10)))),
                    y = 10 + c(1,seq(10, min(max_rank,(ceiling(max(temp$rank)/10)*10)), by = 10)),
                    label = c(1,seq(10, min(max_rank,(ceiling(max(temp$rank)/10)*10)), by = 10)),
                    size=2.5 , angle=0, hjust=1.2,
                    color = ablack) +
           labs(x = NULL, y = NULL,
                title = player_to_plot))
}

cowplot::plot_grid(p1,p2,p3,p4,p5,
                   p6,p7,p8,p9,p10,
                   p11,p12,
                   ncol = 3)

ggsave('Images/rank_clocks.pdf', width = 9,
       height = 12, units = 'in',
       device = cairo_pdf)
