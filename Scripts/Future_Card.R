library(tidyverse)
library(tidymodels)
library(rvest)
library(here)

# Load models and fighter data 
load(here("Models/Final_Model.RDS"))
fighter_df <- read_csv(here("Data/fighter_table.csv"))

# Scrape future cards and head-to-head matchups
future_cards <- read_html("http://ufcstats.com/statistics/events/upcoming") %>% 
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
  unnest(fights)

# Join fighter data with fights 
fighter_df <- fighter_df %>% 
  inner_join(future_cards %>% 
               pivot_longer(-card) %>% 
               select(fighter = value) %>% 
               distinct(),
             by = "fighter") %>% 
  group_by(fighter) %>% 
  filter(fight_pk == max(fight_pk)) %>% 
  ungroup()

# Create a fighter dataset for model
fighter_cards <- future_cards %>% 
  left_join(fighter_df, by = c("Fighter" = "fighter")) %>% 
  left_join(fighter_df %>% rename_all(.funs = ~paste0("opp_", .x)), by = c("Fighter" = "opp_fighter")) %>% 
  rename(fighter = Fighter, opp_fighter = Opponent)

# Create an opponent dataset for model
opponent_cards <- future_cards %>% 
  rename(Fighter = Opponent, Opponent = Fighter) %>% 
  left_join(fighter_df, by = c("Fighter" = "fighter")) %>% 
  left_join(fighter_df %>% rename_all(.funs = ~paste0("opp_", .x)), by = c("Fighter" = "opp_fighter")) %>% 
  rename(fighter = Fighter, opp_fighter = Opponent)

# Predict fighter components 
fighter_components <- tibble(
  predict(kd_model, new_data = fighter_cards) %>% rename("kd" = 1),
  predict(sig_strike_model, new_data = fighter_cards) %>% rename("sig_strike_landed" = 1),
  predict(strike_model, new_data = fighter_cards) %>% rename("strike_landed" = 1),
  predict(sub_model, new_data = fighter_cards) %>% rename("sub_attempts" = 1),
  predict(td_model, new_data = fighter_cards) %>% rename("td_landed" = 1)
) %>% bind_cols(fighter_cards %>% select(fighter))

# Predict opponent components 
opponent_components <- tibble(
  predict(kd_model, new_data = opponent_cards) %>% rename("kd" = 1),
  predict(sig_strike_model, new_data = opponent_cards) %>% rename("sig_strike_landed" = 1),
  predict(strike_model, new_data = opponent_cards) %>% rename("strike_landed" = 1),
  predict(sub_model, new_data = opponent_cards) %>% rename("sub_attempts" = 1),
  predict(td_model, new_data = opponent_cards) %>% rename("td_landed" = 1)
) %>% rename_all(~paste0("opp_", .x)) %>% 
  bind_cols(opponent_cards %>% select(fighter))

# Join fighter and opponent together and predict outcomes and calculate probability intervals
bind_cols(
  predict(outcome_model, bind_cols(fighter_components, opponent_components), type = "prob") %>% rename("L1" = 1, "W1" = 2),
  predict(outcome_model, bind_cols(fighter_components, opponent_components) %>% 
            rename_all(~paste0("opp_", .x) %>% 
                         str_replace_all("opp_opp_", "")), type = "prob") %>% 
    rename("W2" = 1, "L2" = 2)
) %>% 
  bind_cols(fighter_components %>% rename_if(is.numeric, ~paste0("fighter_", .x, "_est"))) %>% 
  bind_cols(opponent_components %>% rename("opponent" = "fighter") %>% rename_if(is.numeric, ~paste0("opponent_", .x, "_est"))) %>% 
  mutate(fighter_win = (W1 + W2) / 2,
         fighter_upper = if_else(W2 > W1, W2, W1),
         fighter_lower = if_else(W2 > W1, W1, W2),
         opponent_win = (L1 + L2) / 2,
         opponent_upper = if_else(L1 > L2, L1, L2),
         opponent_lower = if_else(L1 > L2, L2, L1)) %>% 
  bind_cols(future_cards %>% select(card)) %>% 
  select(card, fighter, opponent, fighter_lower, fighter_win, fighter_upper, opponent_lower, opponent_win, opponent_upper, everything()) %>% 
  select(-W1, -W2, -L1, -L2) %>% 
  mutate_if(is.numeric, ~if_else(.x < 0, 0, .x)) %>% 
  write_csv(here("Data/future_card_predictions.csv"))