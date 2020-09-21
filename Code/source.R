### source file

library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(Matrix)
library(ggrepel)
library(extrafont)
library(scales)
library(cowplot)
library(gt)
library(knitr)
library(gghalves)
loadfonts(device = "win")
theme_set(theme_minimal(base_family = "Roboto"))

ablack = '#202020'

jac_similarity <- function(x, y) {
  k <- length(intersect(x, y))
  n <- length(union(x, y))
  k / n
}

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}

scientific <- function(x){
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scientific_format()(x)))))
}

page_rank_sim = function(df){

  node_list <- df %>%
    filter(walkover != TRUE) %>%
    select(match_id, player_1, player_2, player_1_score, player_2_score) %>%
    mutate(winner = if_else(player_1_score > player_2_score,
                            player_1, player_2),
           loser = if_else(player_1_score > player_2_score,
                           player_2, player_1),
    ) %>%
    select(winner, loser) %>%
    gather() %>%
    select(-key) %>%
    unique() %>%
    rename(label = value) %>%
    rowid_to_column("id")

  el <- df %>%
    filter(walkover != TRUE) %>%
    select(match_id, player_1, player_2, player_1_score, player_2_score) %>%
    mutate(winner = if_else(player_1_score > player_2_score,
                            player_1, player_2),
           loser = if_else(player_1_score > player_2_score,
                           player_2, player_1),
    ) %>%
    select(winner, loser) %>%
    count(winner, loser, name = 'weight') %>%
    rename(target = winner,
           source = loser) %>%
    left_join(., node_list, by = c('source' = 'label')) %>%
    rename(from = id) %>%
    left_join(., node_list, by = c('target' = 'label')) %>%
    rename(to = id) %>%
    select(to, from, weight)

  snooker_net <- tbl_graph(nodes = node_list,
                           edges = el,
                           directed = TRUE)
  adj <- as_adj(snooker_net, sparse = TRUE, attr = 'weight')

  ### algorithm for prestige

  N <- node_list %>% nrow()
  P0 <- rep(1/N, N) # initial values
  q = 0.15 # page rank factor
  s_out <- rowSums(adj) # outdegree

  P_temp <- P0
  thres <- 1e-20
  error <- 1
  iter = 1
  while(error > thres & iter < 1e4){
    temp <- (1-q)*((P_temp/s_out) %*% adj) + (q/N) + ((1-q)/N)*(P0 * (s_out == 0))
    error <- max(P_temp - temp)
    P_temp <- temp
    iter <- iter + 1
  }

  prestige <- as.vector(temp)

  result <- tibble(player = node_list$label, prestige = prestige,
                   in_strength = as.vector(colSums(adj))) %>%
    arrange(desc(prestige)) %>%
    mutate(rank_pre = rank(-prestige, ties.method = "first"),
           rank_str = rank(-in_strength, ties.method = "first"))

  result
}
