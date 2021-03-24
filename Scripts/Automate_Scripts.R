suppressPackageStartupMessages({
  library(tidyverse)
  library(tidymodels)
  library(rvest)
  library(progress)
  library(kableExtra)
  library(ggtext)}
)

rm(list = ls())

# # Schedule Automation Script
# taskscheduler_create(taskname = "RUN_UFC_Data",
#                      rscript = "E:/School/R Work/UFC-Sports-Betting-Model/Scripts/Automate_Scripts.R",
#                      schedule = "WEEKLY",
#                      starttime = "09:00",
#                      startdate = "03/15/2021",
#                      days = "MON")
# 
# taskscheduler_delete("RUN_UFC_DATA")

# Scrape Data -------------------------------------------------------------

# Scraping Functions ------------------------------------------------------
message("Scraping Cards")
scrape_cards <- function(link){
  
  link %>% 
    read_html() %>% 
    html_nodes(".b-link_style_black") %>% 
    html_attr("href") %>% 
    tibble("cards" = .)
}
scrape_dates <- function(link){
  
  link %>% 
    read_html() %>% 
    html_nodes(".b-list__box-list-item:nth-child(1)") %>% 
    html_text() %>% 
    tibble("fight_date" = .) %>% 
    separate(fight_date, into = c("key", "value"), sep = ":") %>% 
    select(date = value) %>% 
    mutate(date = str_replace_all(date, "\n","")) %>% 
    mutate(date = str_trim(date))
  
}
scrape_fights <- function(link){
  
  link %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    tibble("fights" = .) %>% 
    filter(str_detect(fights, "fight-details"))
  
}
scrape_fight_summary_data <- function(link){
  
  link <- link %>% read_html()
  
  
  table_df <- link %>% html_nodes("table")
  
  
  summary_data <- table_df[1] %>% 
    html_table(trim = TRUE, fill = TRUE) %>% 
    do.call("rbind", .) %>% 
    as_tibble() %>% 
    rename("Fighter" = 1, "KD" = 2, "Sig_Strike" = 3, "Sig_Strike_Percent" = 4, "Total_Strikes" = 5, 
           "TD" = 6, "TD_Percent" = 7, "Sub_Attempts" = 8, "Pass" = 9, "Rev" = 10) %>% 
    gather() %>% 
    separate(value, into = c("fighter_1", "fighter_2"), sep = "  ", extra = "merge") %>% 
    mutate_all(~str_replace_all(.x, "\n", "")) %>% 
    mutate_all(str_trim) %>% 
    pivot_wider(names_from = key, values_from = c(fighter_1, fighter_2)) %>% 
    separate(fighter_1_Sig_Strike, into = c("fighter_1_Sig_Strike_Landed", "fighter_1_Sig_Strike_Attempts"), 
             sep = " of ", extra = "merge") %>% 
    separate(fighter_2_Sig_Strike, into = c("fighter_2_Sig_Strike_Landed", "fighter_2_Sig_Strike_Attempts"), 
             sep = " of ", extra = "merge") %>% 
    separate(fighter_1_Total_Strikes, into = c("fighter_1_Strike_Landed", "fighter_1_Strike_Attempts"), 
             sep = " of ", extra = "merge") %>%
    separate(fighter_2_Total_Strikes, into = c("fighter_2_Strike_Landed", "fighter_2_Strike_Attempts"), 
             sep = " of ", extra = "merge") %>%
    separate(fighter_1_TD, into = c("fighter_1_TD_Landed", "fighter_1_TD_Attempts"), 
             sep = " of ", extra = "merge") %>%
    separate(fighter_2_TD, into = c("fighter_2_TD_Landed", "fighter_2_TD_Attempts"), 
             sep = " of ", extra = "merge") %>% 
    mutate_at(vars(contains("Percent")), ~.01*str_replace(.x, "%", "") %>% as.numeric()) %>% 
    mutate_at(vars(-contains("Fighter", ignore.case = FALSE)), as.numeric) 
  
  
  fight_details <- link %>% 
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "b-fight-details__text", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//i') %>% 
    html_text() %>% 
    as_tibble() %>% 
    mutate(value = str_replace_all(value, "\n", "")) %>% 
    mutate(value = str_trim(value, side = "both")) %>% 
    separate(value, into = c("feature", "value"), sep = ":", extra = "merge") %>% 
    mutate(value = str_trim(value)) %>% 
    replace_na(list(value = "")) %>% 
    group_by(feature) %>% 
    filter(value != "") %>% 
    ungroup() %>% 
    pivot_wider(names_from = feature, values_from = value) %>% 
    rename_all(.funs = ~str_replace(.x, "\\s|/", "_") %>% tolower()) %>% 
    cbind(
      link %>% 
        html_node(".b-fight-details__persons") %>% 
        html_text() %>% 
        str_extract("[:upper:]{1}") %>% 
        tibble("fighter_1_res" = .)) %>% 
    mutate(fighter_2_res = case_when(
      fighter_1_res == "L" ~ "W",
      fighter_1_res == "W" ~ "L",
      TRUE ~ "D"
    )) %>% 
    rename("round_finished" = "round") %>% 
    cbind(
      link %>%       
        html_nodes(".b-fight-details__fight-title") %>% 
        html_text() %>% 
        str_replace_all("\n", "") %>% 
        str_trim() %>% 
        tibble(weight_class = .))
  
  summary_data <- cbind(summary_data, fight_details)
  pb$tick()
  Sys.sleep(1/100)
  summary_data %>% as_tibble()
  
}
scrape_round_data <- function(link){
  
  link <- link %>% read_html()
  
  table_df <- link %>% html_nodes("table")
  
  
  round_data <- table_df[2] %>% 
    html_table(trim = TRUE, fill = TRUE) %>% 
    do.call("rbind", .) %>% 
    as_tibble(.name_repair = "unique") %>% 
    rename("Fighter" = 1, "KD" = 2, "Sig_Strike" = 3, "Sig_Strike_Percent" = 4, "Total_Strikes" = 5, 
           "TD" = 6, "TD_Percent" = 7, "Sub_Attempts" = 8, "Pass" = 9, "Rev" = 10) %>% 
    gather() %>% 
    separate(value, into = c("fighter_1", "fighter_2"), sep = "  ", extra = "merge") %>% 
    mutate_all(~str_replace_all(.x, "\n", " ")) %>% 
    mutate_all(str_trim) %>% 
    pivot_wider(names_from = key, values_from = c(fighter_1, fighter_2)) %>% 
    unnest() %>% 
    mutate(round = row_number()) %>% 
    separate(fighter_1_Sig_Strike, into = c("fighter_1_Sig_Strike_Landed", "fighter_1_Sig_Strike_Attempts"), 
             sep = " of ", extra = "merge") %>% 
    separate(fighter_2_Sig_Strike, into = c("fighter_2_Sig_Strike_Landed", "fighter_2_Sig_Strike_Attempts"), 
             sep = " of ", extra = "merge") %>% 
    separate(fighter_1_Total_Strikes, into = c("fighter_1_Strike_Landed", "fighter_1_Strike_Attempts"), 
             sep = " of ", extra = "merge") %>%
    separate(fighter_2_Total_Strikes, into = c("fighter_2_Strike_Landed", "fighter_2_Strike_Attempts"), 
             sep = " of ", extra = "merge") %>%
    separate(fighter_1_TD, into = c("fighter_1_TD_Landed", "fighter_1_TD_Attempts"), 
             sep = " of ", extra = "merge") %>%
    separate(fighter_2_TD, into = c("fighter_2_TD_Landed", "fighter_2_TD_Attempts"), 
             sep = " of ", extra = "merge") %>% 
    mutate_at(vars(contains("Percent")), ~.01*str_replace(.x, "%", "") %>% as.numeric()) %>% 
    mutate_at(vars(-contains("Fighter", ignore.case = FALSE)), as.numeric)
  
  fight_details <- link %>% 
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "b-fight-details__text", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]//i') %>% 
    html_text() %>% 
    as_tibble() %>% 
    mutate(value = str_replace_all(value, "\n", "")) %>% 
    mutate(value = str_trim(value, side = "both")) %>% 
    separate(value, into = c("feature", "value"), sep = ":", extra = "merge") %>% 
    mutate(value = str_trim(value)) %>% 
    replace_na(list(value = "")) %>% 
    group_by(feature) %>% 
    filter(value != "") %>% 
    ungroup() %>% 
    pivot_wider(names_from = feature, values_from = value) %>% 
    rename_all(.funs = ~str_replace(.x, "\\s|/", "_") %>% tolower()) %>% 
    cbind(
      link %>% 
        html_node(".b-fight-details__persons") %>% 
        html_text() %>% 
        str_extract("[:upper:]{1}") %>% 
        tibble("fighter_1_res" = .)) %>% 
    mutate(fighter_2_res = case_when(
      fighter_1_res == "L" ~ "W",
      fighter_1_res == "W" ~ "L",
      TRUE ~ "D"
    )) %>% 
    rename("round_finished" = "round") %>% 
    cbind(
      link %>%       
        html_nodes(".b-fight-details__fight-title") %>% 
        html_text() %>% 
        str_replace_all("\n", "") %>% 
        str_trim() %>% 
        tibble(weight_class = .))
  
  
  round_data <- cbind(round_data, fight_details)
  round_data %>% as_tibble()
  
}

# Data Scraping -----------------------------------------------------------
scraped_cards <- read_csv("E:/School/R Work/UFC-Sports-Betting-Model/Data/fight_data_raw.csv")

UFC <- tibble(UFC_Page = "http://ufcstats.com/statistics/events/completed?page=all") %>% 
  # Scrape Fight Card links
  mutate(cards = map(UFC_Page, scrape_cards)) %>% unnest(cards) %>% 
  # Remove cards that have already been scraped
  anti_join(scraped_cards %>% select(cards) %>% distinct(), by = "cards") %>% 
  # Scrape card dates
  mutate(dates =  map(cards, scrape_dates)) %>% unnest(dates) %>% 
  # Scrape Fights from every Fight Card
  mutate(fights = map(cards, scrape_fights)) %>% unnest(fights)

pb <- progress_bar$new(total = nrow(UFC),
                       format = "  downloading [:bar] ETA: :eta :current/:total")

UFC_Data <- UFC %>% mutate(fight_data = map(fights, safely(scrape_fight_summary_data)))

UFC_Data %>% 
  unnest(fight_data) %>% 
  filter(row_number() %% 2 != 0) %>% 
  unnest(fight_data) %>% 
  mutate(round_finished = as.numeric(round_finished)) %>% 
  distinct() %>% 
  bind_rows(scraped_cards %>% mutate(time = as.character(time))) %>% 
  write_csv("E:/School/R Work/UFC-Sports-Betting-Model/Data/fight_data_raw.csv")


# Data Cleaning -----------------------------------------------------------
message("Cleaning Data")
rm(list = ls())


df <- read_csv("E:/School/R Work/UFC-Sports-Betting-Model/Data/fight_data_raw.csv")

# Clean head-to-head data
df <- df %>% 
  # Deselect fields that will not be used in the model
  select(-UFC_Page, -cards, -fights) %>% 
  # Convert field name to lower case
  rename_all(tolower) %>% 
  # Convert date
  mutate(date = as.Date(date, format = "%B %d, %Y")) %>% 
  arrange(desc(date)) %>% 
  # Convert fields to categorical variables
  mutate(fighter_1_fighter = as.factor(fighter_1_fighter),
         fighter_2_fighter = as.factor(fighter_2_fighter),
         method = as.factor(method),
         fighter_1_res = as.factor(fighter_1_res),
         fighter_2_res = as.factor(fighter_2_res),
         weight_class = as.factor(weight_class)) %>% 
  # Adjust percents 
  mutate(fighter_1_sig_strike_percent = fighter_1_sig_strike_landed / fighter_1_sig_strike_attempts,
         fighter_1_td_percent = fighter_1_td_landed / fighter_1_td_attempts,
         fighter_2_sig_strike_percent = fighter_2_sig_strike_landed / fighter_2_sig_strike_attempts,
         fighter_2_td_percent = fighter_2_td_landed / fighter_2_td_attempts) %>% 
  # If no attempts than percent will be 0 (may be adjusted to a not attempted variable)
  mutate_at(vars(contains("percent")), .funs = ~if_else(is.nan(.x) == TRUE, 0, .x)) %>% 
  # Convert rounds and round_finished
  mutate(time_format = str_extract(time_format, "\\d")) %>% 
  rename("rounds" = time_format) %>% 
  mutate(rounds  = as.factor(rounds),
         round_finished = as.factor(round_finished)) %>% 
  # Convert method
  separate(method, into = c("method"), sep = "-", extra = "drop") %>% 
  mutate(method = str_trim(method),
         method = as.factor(method)) %>% 
  # Convert weight_class
  mutate(gender = str_extract(weight_class, "Women")) %>% 
  mutate(gender = if_else(is.na(gender) == TRUE, "Men", gender)) %>% 
  mutate(weight_class = str_extract(weight_class, "\\w+weight")) %>% 
  mutate(weight_class = if_else(is.na(weight_class == TRUE), "Catchweight", weight_class)) %>% 
  unite("weight_class", c(gender, weight_class)) %>% 
  mutate(weight_class = as.factor(weight_class)) %>% 
  # Convert referee
  mutate(referee = if_else(is.na(referee) == TRUE, "Missing", referee)) %>% 
  mutate(referee = as.factor(referee)) %>% 
  # Convert rounds
  mutate(rounds = if_else(is.na(rounds) == TRUE, round_finished, rounds)) %>% 
  # Create a fight pk for future feature engineering 
  mutate(row_num = row_number()) %>% 
  arrange(desc(row_num)) %>% 
  mutate(fight_pk = row_number()) %>% 
  arrange(row_num) %>% 
  select(-row_num) 

# Fighter 1 data 
fighter_1 <- df %>% 
  select(-fighter_2_fighter, -referee, -fighter_2_res) %>% 
  rename_all(.funs = ~str_replace(.x, "fighter_1_", "")) %>% 
  # Create defensive metrics 
  mutate("sig_strikes_avoided" = fighter_2_sig_strike_attempts - fighter_2_sig_strike_landed,
         "tds_defended" = fighter_2_td_attempts - fighter_2_td_landed,
         "strikes_avoided" = fighter_2_strike_attempts - fighter_2_strike_landed) %>% 
  # Create damage metrics 
  rename("kds_received" = fighter_2_kd,
         "sig_strikes_received" = fighter_2_sig_strike_landed,
         "strikes_received" = fighter_2_strike_landed,
         "tds_received" = fighter_2_td_landed) %>% 
  mutate(strike_percent = strike_landed / strike_attempts,
         sig_reg_mixture = sig_strike_attempts / strike_attempts,
         sig_reg_percent = (sig_strike_landed + strike_landed) / (sig_strike_attempts + strike_attempts)) %>% 
  # Remove unnecessary columns
  select(-fighter_2_sig_strike_attempts, 
         -fighter_2_sig_strike_percent,
         -fighter_2_strike_attempts,
         -fighter_2_td_attempts,
         -fighter_2_td_percent,
         -fighter_2_sub_attempts,
         -fighter_2_pass,
         -fighter_2_rev) %>% 
  # Sort the columns
  select(sort(current_vars())) 

# Fighter 2 data
fighter_2 <- df %>% 
  select(-fighter_1_fighter, -referee, -fighter_1_res) %>% 
  rename_all(.funs = ~str_replace(.x, "fighter_2_", "")) %>% 
  # Create defensive metrics 
  mutate("sig_strikes_avoided" = fighter_1_sig_strike_attempts - fighter_1_sig_strike_landed,
         "tds_defended" = fighter_1_td_attempts - fighter_1_td_landed,
         "strikes_avoided" = fighter_1_strike_attempts - fighter_1_strike_landed) %>% 
  # Create damage metrics 
  rename("kds_received" = fighter_1_kd,
         "sig_strikes_received" = fighter_1_sig_strike_landed,
         "strikes_received" = fighter_1_strike_landed,
         "tds_received" = fighter_1_td_landed) %>% 
  mutate(strike_percent = strike_landed / strike_attempts,
         sig_reg_mixture = sig_strike_attempts / strike_attempts,
         sig_reg_percent = (sig_strike_landed + strike_landed) / (sig_strike_attempts + strike_attempts)) %>% 
  # Remove unnecessary columns
  select(-fighter_1_sig_strike_attempts, 
         -fighter_1_sig_strike_percent,
         -fighter_1_strike_attempts,
         -fighter_1_td_attempts,
         -fighter_1_td_percent,
         -fighter_1_sub_attempts,
         -fighter_1_pass,
         -fighter_1_rev) %>% 
  # Sort the columns 
  select(sort(current_vars()))

# Combine fighter 1 and fighter 2 data 
fight_data <- bind_rows(fighter_1, fighter_2) %>% arrange(desc(fight_pk))

write_csv(fight_data,"E:/School/R Work/UFC-Sports-Betting-Model/Data/fight_data.csv")


# Future Cards ------------------------------------------------------------
message("Scraping Future Data")
rm(list = ls())

# Load models and fighter data 
load("E:/School/R Work/UFC-Sports-Betting-Model/Models/Final_Model.RDS")
fighter_df <- read_csv("E:/School/R Work/UFC-Sports-Betting-Model/Data/fighter_table.csv")

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
  write_csv("E:/School/R Work/UFC-Sports-Betting-Model/Data/future_card_predictions.csv")


# README Plots ------------------------------------------------------------
message("Creating README Plots")
rm(list = ls())


df <- read_csv("E:/School/R Work/UFC-Sports-Betting-Model/Data/future_card_predictions.csv")
fights <- read_csv("E:/School/R Work/UFC-Sports-Betting-Model/Data/fight_data.csv")
elo <- read_csv("E:/School/R Work/UFC-Sports-Betting-Model/Data/fighter_table.csv")


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
  
  return(p)
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
  
  return(p)
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
    save_kable(file = filename)
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
    save_kable(file = filename)
}


# Create Plots ------------------------------------------------------------
df %>% 
  select(card) %>% 
  distinct() %>% 
  mutate(index = row_number(),
         filename = paste0("E:/School/R Work/UFC-Sports-Betting-Model/Plots/card_predictions_", index, ".png"),
         plot = map2(card, filename, get_fight_prob_plot))

df %>% 
  select(card) %>% 
  distinct() %>% 
  mutate(index = row_number(),
         filename = paste0("E:/School/R Work/UFC-Sports-Betting-Model/Plots/component", index, ".png"),
         plot = map2(card, filename, get_fight_component_plot))

df %>% 
  select(card) %>% 
  distinct() %>% 
  mutate(index = row_number(),
         filename = paste0("E:/School/R Work/UFC-Sports-Betting-Model/Plots/table", index, ".png"),
         plot = map2(card, filename, get_summary_table))

df %>% 
  select(card) %>% 
  distinct() %>% 
  mutate(index = row_number(),
         filename = paste0("E:/School/R Work/UFC-Sports-Betting-Model/Plots/gamble_table", index, ".png"),
         plot = map2(card, filename, get_betting_table))