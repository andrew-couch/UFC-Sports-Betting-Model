library(tidyverse)
library(kableExtra)
library(ggtext)
library(here)

df <- read_csv(here("Data/future_card_predictions.csv"))
fights <- read_csv(here("Data/fight_data.csv"))
elo <- read_csv(here("Data/fighter_table.csv"))


# Plotting functions ------------------------------------------------------
get_fight_prob_plot <- function(card, filename){
  p <- df %>% 
    filter(card == !!card) %>% 
    mutate(
      fight = paste0("<i style='color:red'>", fighter, 
                     "</i>","<i style='color:black'>", " vs. ", "</i>" ,
                     "<i style='color:blue'>", opponent,"</i>"),
      pk = row_number(),
      fight = reorder(fight, -pk),
      fighter_favor = if_else(fighter_win > opponent_win, fighter_upper + 0.1, fighter_lower -0.1),
      opponent_favor = if_else(opponent_win > opponent_win, opponent_upper + 0.1, opponent_lower -0.1)) %>%   
    ggplot() + 
    geom_vline(xintercept = 0.5, linetype = "dashed") + 
    geom_errorbarh(aes(x = fighter_win, xmin = fighter_lower, xmax = fighter_upper, y = fight), color = "red") + 
    geom_errorbarh(aes(x = opponent_win, xmin = opponent_lower, xmax = opponent_upper, y = fight), color = "blue") + 
    geom_point(aes(x = fighter_win, y = fight), color = "red") + 
    geom_point(aes(x = opponent_win, y = fight), color = "blue") + 
    scale_x_continuous(labels = scales::label_percent()) + 
    labs(x = "Probability of Winning", y = "") +
    scale_y_discrete(expand = c(0, 1)) + 
    theme(
      axis.text.y = element_markdown(),
      plot.caption = element_markdown(lineheight = 1.2)
    ) + 
    labs(title = paste0(card, " Predictions"))
  
  ggsave(filename = filename, plot = p)
  
}

get_fight_component_plot <- function(card, filename){
  p <- df %>% 
    filter(card == !!card) %>%  
    select(fighter, opponent, fighter_kd_est:opponent_opp_td_landed_est) %>% 
    mutate(
      fight = paste0("<i style='color:red'>", fighter, 
                     "</i>","<i style='color:black'>", " vs. ", "</i>" ,
                     "<i style='color:blue'>", opponent,"</i>"),
      pk = row_number(),
      fight = reorder(fight, -pk)) %>% 
    select(-opponent, -fighter, -pk) %>% 
    rename_all(~str_replace(.x, "fighter_", "fighter.") %>% str_replace("opponent_", "opponent.")) %>% 
    pivot_longer(-c(fight)) %>% 
    mutate(name = str_replace(name, "opp_", "")) %>% 
    separate(name, c("fighter_type", "feature"), sep = "\\.") %>% 
    pivot_wider(names_from = fighter_type, values_from = value) %>% 
    ggplot(aes(y = fight, yend = fight)) + 
    geom_segment(aes(x = fighter, xend = opponent)) + 
    geom_point(aes(x = fighter), color = "blue") + 
    geom_point(aes(x = opponent), color = "red") + 
    facet_wrap(~feature, scales = "free_x", ncol = 1) + 
    theme(
      axis.text.y = element_markdown(),
      plot.caption = element_markdown(lineheight = 1.2)
    ) + 
    labs(x = "", y = "") + 
    labs(title = paste0(card, " Component Predictions"))
  
  ggsave(filename = filename, plot = p, height = 11, width = 8.5, units = "in")
  
}

get_summary_table <- function(card, filename){
  df %>% 
    filter(card == !!card) %>% 
    select(-card) %>% 
    mutate_all(as.character) %>% 
    rename_all(~str_replace_all(.x, "_est", "") %>% 
                 str_replace_all("fighter_", "fighter.") %>% 
                 str_replace_all("opponent_", "opponent.")) %>% 
    mutate_all(as.character) %>%
    mutate(fight = paste0(fighter, " vs ", opponent)) %>% 
    rename(fighter.name = fighter, opponent.name = opponent) %>% 
    pivot_longer(-fight) %>% 
    separate(name, c("fighter_type", "feature"), sep = "\\.") %>% 
    mutate(feature = str_replace(feature, "opp_", "")) %>% 
    pivot_wider(names_from = feature, values_from = value) %>% 
    unnest() %>% 
    select(-fighter_type) %>% 
    rename_all(~str_replace_all(.x, "_", " ") %>% str_to_title() %>% str_replace_all("Sig", "Sig.") %>% str_replace_all("Td", "TDs")) %>% 
    rename(Fighter = Name) %>%
    mutate(
      across(-c(Fight, Fighter), as.numeric),
      across(Lower:Upper, .fns = ~round(100*.x, digits = 0) %>% paste0("%")),
      across(Kd:`TDs Landed`, ~round(.x, digits = 2))) %>% 
    kable(caption = paste0(card)) %>% 
    column_spec(1:2, bold = T, width = "100em") %>% 
    column_spec(6:10, bold = T, width = "50em") %>% 
    column_spec(3:5, bold = T, width = "25em") %>% 
    column_spec(7, bold = T, width = "75em") %>% 
    save_kable(file = filename, keep_tex = TRUE)
}

get_betting_table <- function(card, filename){
  df %>% 
    filter(card == card) %>% 
    select(fighter_name = fighter, opponent_name = opponent, fighter_win, opponent_win) %>% 
    mutate(fight = paste0(fighter_name, " vs. ", opponent_name)) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(-fight) %>% 
    separate(name, c("fighter_type", "feature"), sep = "\\_") %>% 
    pivot_wider(names_from = feature, values_from = value) %>% 
    mutate(win = as.numeric(win),
           moneyline = if_else(win >= 0.5, -100 / (1/ win - 1), (1 / win - 1) * 100),
           odds = win / (1 - win),
           win = round(100*win, digits = 0) %>% paste0("%"),
           moneyline = round(moneyline),
           odds = round(odds, digits = 2)) %>% 
    select(-fighter_type) %>% 
    rename_all(str_to_title) %>% 
    rename(Fighter = Name, Probability = Win) %>% 
    kable(caption = paste0(card)) %>% 
    column_spec(1:2, bold = T, width = "100em") %>% 
    column_spec(3:5, bold = T, width = "10em") %>% 
    save_kable(file = filename, keep_tex = TRUE)
}


# Create Plots ------------------------------------------------------------
df %>% 
  select(card) %>% 
  distinct() %>% 
  mutate(index = row_number(),
         filename = paste0("Plots/card_predictions_", index, ".png"),
         filename = here(filename),
         plot = walk2(card, filename, get_fight_prob_plot))

df %>% 
  select(card) %>% 
  distinct() %>% 
  mutate(index = row_number(),
         filename = paste0("Plots/component", index, ".png"),
         filename = here(filename),
         plot = walk2(card, filename, get_fight_component_plot))

df %>% 
  select(card) %>% 
  distinct() %>% 
  mutate(index = row_number(),
         filename = paste0("Plots/table", index, ".png"),
         filename = here(filename),
         plot = walk2(card, filename, get_summary_table))

df %>% 
  select(card) %>% 
  distinct() %>% 
  mutate(index = row_number(),
         filename = paste0("Plots/gamble_table", index, ".png"),
         filename = here(filename),
         plot = walk2(card, filename, get_betting_table))