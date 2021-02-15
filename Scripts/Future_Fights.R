library(tidyverse)
library(here)
library(rvest)

read_html("http://ufcstats.com/statistics/events/upcoming") %>% 
  html_nodes(".b-link_style_black") %>% 
  html_attr("href") %>% 
  tibble(future_cards = .) %>% 
  bind_cols(card = read_html("http://ufcstats.com/statistics/events/upcoming") %>% 
              html_nodes(".b-link_style_black") %>% 
              html_text() %>% 
              str_trim() %>% 
              str_squish()) %>% 
  mutate(fights = map(future_cards, ~read_html(.x) %>% 
                        html_nodes(".l-page_align_left .b-link_style_black") %>% 
                        html_text() %>% 
                        as_tibble() %>% 
                        mutate(value = str_trim(value) %>% str_squish(),
                               index = row_number() %% 2,
                               type = if_else(index == 1, "Fighter", "Opponent"),
                               index = cumsum(index)) %>% 
                        pivot_wider(names_from = type, values_from = value) %>% 
                        select(-index))) %>% 
  select(card, fights) %>% 
  unnest(fights) %>% 
  write_csv(here("Data/future_fights.csv"))