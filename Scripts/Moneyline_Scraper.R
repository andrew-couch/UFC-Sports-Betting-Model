library(tidyverse)
library(rvest)
library(here)

past_predictions <- read_csv(here("Data/prediction_history.csv")) %>% slice_max(pred_date)
moneyline_history <- read_csv(here("Data/moneyline.csv"))

get_moneyline <- function(fighter){
  tibble(fighter = !!fighter) %>% 
    mutate(query = str_squish(fighter) %>% str_replace_all(" ", "+"),
           link = paste0("https://www.bestfightodds.com/search?query=", query)) %>% 
    select(fighter, link) %>% 
    mutate(link = map(link, read_html),
           link = map(link, ~html_nodes(.x, "#page-content a")),
           hist = map(link, ~html_attr(.x, "href")),
           name = map(link, html_text)) %>% 
    unnest(c(hist, name)) %>% 
    filter(fighter == name) %>% 
    select(fighter, hist) %>% 
    mutate(hist = paste0("https://www.bestfightodds.com", hist),
           hist = map(hist, read_html),
           hist = map(hist, .f = function(link){
             link %>%
               html_element(".flex-section") %>% 
               html_element(".flex-content-wrap") %>% 
               html_element("#page-wrapper") %>% 
               html_element("#page-container") %>% 
               html_element("#team-stats-container") %>% 
               html_table() %>%
               select(1, 3) %>% 
               mutate(var = rep(c("Card", "fighter_a", "fighter_b"), times = n() / 3),
                      pk = rep(c(1, 0, 0), times = n() / 3),
                      pk = cumsum(pk),
                      value = if_else(var == "Card", Matchup, paste0(Matchup, "_", `Closing range`))) %>% 
               select(pk, var, value) %>% 
               pivot_wider(names_from = var, values_from = value) %>%
               separate(fighter_a, c("fighter_a", "fighter_a_odds"), sep = "\\_") %>% 
               separate(fighter_b, c("fighter_b", "fighter_b_odds"), sep = "\\_") %>% 
               select(-pk)
           })) 
}

moneyline_df <- past_predictions %>% 
  select(card, fighter, opponent) %>% 
  pivot_longer(c(fighter, opponent), values_to = "fighter") %>% 
  distinct(fighter) %>% 
  mutate(moneyline = map(fighter, safely(get_moneyline))) %>% 
  select(moneyline) %>% 
  mutate(moneyline = map(moneyline, ~pluck(.x, 1))) %>% 
  unnest(moneyline) %>% 
  unnest(hist) %>% 
  select(-fighter)

bind_rows(
  moneyline_history,
  moneyline_df,
) %>% 
  distinct() %>%
  write_csv(here("Data/moneyline.csv"), append = TRUE)