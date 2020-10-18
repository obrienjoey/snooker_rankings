### author: Joey O'Brien
### date: 18/10/20
### analysis of the prize-fund to reproduce Fig.3

# libraries
source('./Code/source.R')
# data
tourn_df <- read_csv('./Data/final_tourn_df.csv')

tourn_df %>%
  filter(season > 2008) %>%
  mutate(prize_fund = gsub('GBP ', '', prize_fund),
         prize_fund = as.numeric(gsub(',', '', prize_fund))) %>%
  ggplot(aes(x = as.factor(season), y = prize_fund)) +
  geom_half_boxplot(aes(fill = as.factor(season)),
                    color = ablack,
                    fatten = 1,
                    position = position_nudge(x = 0, y = 0)) +
  geom_half_point(aes(fill = as.factor(season)),
                  alpha = 0.85,
                  color = ablack,
                  shape = 21,
                  transformation = position_jitter(width = 1)) +
  scale_y_continuous(labels=scientific) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(x = 'Season', y = 'Prize Fund (GBP)') +
  theme(legend.position = 'none',
        axis.ticks.y = element_line(colour = ablack),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.line = element_line(colour = ablack),
        axis.text=element_text(size=10),
        axis.title=element_text(size=14),
        axis.ticks.x = element_line(colour = ablack),
        panel.grid = element_blank())

ggsave('Images/prize_fund_distribution.pdf',
       height = 4, width = 6, units = 'in',
       device = cairo_pdf)
