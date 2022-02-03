library(tidyverse)
library(igraph)

## data set---------------------------------------------------------------------
rollcall <- read_csv("data/HSall_rollcalls.csv")
vote <- read_csv("data/HSall_votes.csv")
mem <- read_csv("data/HSall_members.csv")

south <- c("VA", "NC", "SC", "GA", "FL", "TN", "AL", "MS", "AR", "LA", "TX")
b_south <- c("MO", "KY", "WV", "DE")
d_south <- c("LA", "MS", "AL", "GA", "SC")
rollcall <- rollcall %>% 
  select(congress, rollnumber, bill_number)

# 単純計算----------------------------------------------------------------------
edge_list_byCon <- map(40:86, ~{
  con <- .x
  vote_by_congress <- vote %>%
    filter(congress == con, chamber == "House")
  vote_by_congress_yea <- vote_by_congress %>%
    filter(cast_code == 1) %>% # extract only votes with "yea"
    group_by(congress, rollnumber) %>%
    summarise(co_votes = list(icpsr), yea_p = 1)
  edge_list_all <- map2_df(vote_by_congress_yea$co_votes, 
                           vote_by_congress_yea$yea_p, ~{
                             combi <- expand.grid(.x, .x)
                             edge_list <- subset(combi, Var1 < Var2) %>%
                               as_tibble() %>% 
                               mutate("yea_p" = .y)
                             return(edge_list)
                           }) %>%
    group_by(Var1, Var2) %>%
    summarise(weight = sum(yea_p))
  # southern congressmen
  icpsr_south <- mem %>% 
    filter(congress == con, state_abbrev %in% south) %>% 
    .$icpsr
  # info about party
  icpsr_party <- mem %>% 
    filter(congress == con) %>% 
    select(icpsr, party_code) %>% 
    distinct(icpsr, .keep_all = TRUE)
  icpsr_state <- mem %>% 
    filter(congress == con) %>% 
    select(icpsr, state_abbrev) %>% 
    distinct(icpsr, .keep_all = TRUE)
  g <- graph.data.frame(edge_list_all, directed = FALSE)
  V(g)$south <- grepl(paste(icpsr_south, collapse = "|"), 
                      V(g)$name) # wether south or not
  V(g)$party <- map_chr(V(g)$name, ~{
    icpsr_party %>% 
      filter(as.character(icpsr) == .x) %>% 
      .$party_code
  }) %>% 
    as.integer()
  V(g)$state <- map_chr(V(g)$name, ~{
    icpsr_state %>% 
      filter(as.character(icpsr) == .x) %>% 
      .$state_abbrev
  })
  return(g)
})

# 重み付け調整------------------------------------------------------------------
edge_list_byCon <- map(40:86, ~{
  con <- .x
  vote_by_congress <- vote %>%
    filter(congress == con, chamber == "House")
  vote_by_congress_yea <- vote_by_congress %>%
    filter(cast_code == 1) %>% # extract only votes with "yea"
    group_by(congress, rollnumber) %>%
    summarise(co_votes = list(icpsr),
              yea_p = 1/n())
  edge_list_all <- map2_df(vote_by_congress_yea$co_votes, 
                           vote_by_congress_yea$yea_p, ~{
    combi <- expand.grid(.x, .x)
    edge_list <-  subset(combi, Var1 < Var2) %>%
      as_tibble() %>% 
      mutate("yea_p" = .y)
    return(edge_list)
  }) %>%
    group_by(Var1, Var2) %>%
    summarise(weight = sum(yea_p))
  # southern congressmen
  icpsr_south <- mem %>% 
    filter(congress == con, state_abbrev %in% south) %>% 
    .$icpsr
  # info about party
  icpsr_party <- mem %>% 
    filter(congress == con) %>% 
    select(icpsr, party_code) %>% 
    distinct(icpsr, .keep_all = TRUE)
  icpsr_state <- mem %>% 
    filter(congress == con) %>% 
    select(icpsr, state_abbrev) %>% 
    distinct(icpsr, .keep_all = TRUE)
  g <- graph.data.frame(edge_list_all, directed = FALSE)
  V(g)$south <- grepl(paste(icpsr_south, collapse = "|"), 
                      as.character(V(g))) # wether south or not
  V(g)$party <- map_chr(V(g)$name, ~{
    icpsr_party %>% 
      filter(as.character(icpsr) == .x) %>% 
      .$party_code
  }) %>% 
    as.integer()
  V(g)$state <- map_chr(V(g)$name, ~{
    icpsr_state %>% 
      filter(as.character(icpsr) == .x) %>% 
      .$state_abbrev
  })
  return(g)
})





