library(rvest)
library(tidyverse)

# Scraping Functions ------------------------------------------------------



scrape_cards <- function(link){
  
  link %>% 
    read_html() %>% 
    html_nodes(".b-link_style_black") %>% 
    html_attr("href") %>% 
    tibble("links" = .)
}


scrape_fights <- function(link){
  
  link %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    tibble("details" = .) %>% 
    filter(str_detect(details, "fight-details"))
  
}


scrape_fight_summary_data <- function(link){
  
  table_df <- link %>% 
    read_html() %>%
    html_nodes("table")
  
  
  table_df[1] %>% 
    html_table(trim = TRUE, fill = TRUE) %>% 
    do.call("rbind", .) %>% 
    as_tibble() %>% 
    rename("Fighter" = 1, "KD" = 2, "Sig_Strike" = 3, "Sig_Strike_Percent" = 4, "Total_Strikes" = 5, 
           "TD" = 6, "TD_Perc" = 7, "Sub_Attempts" = 8, "Pass" = 9, "Rev" = 10) %>% 
    gather() %>% 
    separate(value, into = c("fighter_1", "fighter_2"), sep = "  ", extra = "merge") %>% 
    mutate_all(~str_replace_all(.x, "\n", "")) %>% 
    mutate_all(str_trim) %>% 
    pivot_wider(names_from = key, values_from = c(fighter_1, fighter_2)) 
  
  
}



scrape_round_data <- function(link){
  
  table_df <- link %>% 
    read_html() %>%
    html_nodes("table")
  
  
  table_df[2] %>% 
    html_table(trim = TRUE, fill = TRUE) %>% 
    do.call("rbind", .) %>% 
    as_tibble(.name_repair = "unique") %>% 
    rename("Fighter" = 1, "KD" = 2, "Sig_Strike" = 3, "Sig_Strike_Percent" = 4, "Total_Strikes" = 5, 
           "TD" = 6, "TD_Percent" = 7, "Sub_Attempts_Perc" = 8, "Pass" = 9, "Rev" = 10) %>% 
    gather() %>% 
    separate(value, into = c("fighter_1", "fighter_2"), sep = "  ", extra = "merge") %>% 
    mutate_all(~str_replace_all(.x, "\n", " ")) %>% 
    mutate_all(str_trim) %>% 
    pivot_wider(names_from = key, values_from = c(fighter_1, fighter_2)) %>% 
    unnest() %>% 
    mutate(round = row_number())
  
}



# Testing Functions -------------------------------------------------------



link <- "http://ufcstats.com/fight-details/3bdacc82209b33f5"

link <- read_html(link)
table_df <- link %>% html_nodes("table") 





# Fight Summary Data ------------------------------------------------------


# Fight Manual way 
table_df[1] %>% 
  html_table(trim = TRUE, fill = TRUE) %>% 
  do.call("rbind", .) %>% 
  as_tibble() %>% 
  rename("Fighter" = 1, "KD" = 2, "Sig_Strike" = 3, "Sig_Strike_Percent" = 4, "Total_Strikes" = 5, 
         "TD" = 6, "TD_Perc" = 7, "Sub_Attempts" = 8, "Pass" = 9, "Rev" = 10) %>% 
  separate(Fighter, into = c("Fighter_1", "Fighter_2"), sep = "  ", extra = "merge") %>% 
  separate(KD, into = c("KD_1", "KD_2"), sep = "  ", extra = "merge") %>% 
  separate(Sig_Strike, into = c("Sig_Strike_1", "Sig_Strike_2"), sep = "  ", extra = "merge") %>% 
  separate(Sig_Strike_Percent, into = c("Sig_Strike_Percent_1", "Sig_Strike_Percent_2"), sep = "  ", extra = "merge") %>% 
  separate(Total_Strikes, into = c("Total_Strikes_1", "Total_Strikes_2"), sep = "  ", extra = "merge") %>% 
  separate(TD, into = c("TD_1", "TD_2"), sep = "  ", extra = "merge") %>% 
  separate(TD_Perc, into = c("TD_Perc_1", "TD_Perc_2"), sep = "  ", extra = "merge") %>% 
  separate(Sub_Attempts, into = c("Sub_Attempts_1", "Sub_Attempts_2"), sep = "  ", extra = "merge") %>% 
  separate(Pass, into = c("Pass_1", "Pass_2"), sep = "  ", extra = "merge") %>% 
  separate(Rev, into = c("Rev_1", "Rev_2"), sep = "  ", extra = "merge") %>% 
  mutate_all(str_trim)


# Fight Tidy format
table_df[1] %>% 
  html_table(trim = TRUE, fill = TRUE) %>% 
  do.call("rbind", .) %>% 
  as_tibble() %>% 
  rename("Fighter" = 1, "KD" = 2, "Sig_Strike" = 3, "Sig_Strike_Percent" = 4, "Total_Strikes" = 5, 
         "TD" = 6, "TD_Perc" = 7, "Sub_Attempts" = 8, "Pass" = 9, "Rev" = 10) %>% 
  gather() %>% 
  separate(value, into = c("fighter_1", "fighter_2"), sep = "  ", extra = "merge") %>% 
  mutate_all(~str_replace_all(.x, "\n", "")) %>% 
  mutate_all(str_trim) %>% 
  pivot_longer(cols = c(fighter_1, fighter_2)) %>% 
  pivot_wider(names_from = key, values_from = value)


# Fight Head to head format
table_df[1] %>% 
  html_table(trim = TRUE, fill = TRUE) %>% 
  do.call("rbind", .) %>% 
  as_tibble() %>% 
  rename("Fighter" = 1, "KD" = 2, "Sig_Strike" = 3, "Sig_Strike_Percent" = 4, "Total_Strikes" = 5, 
         "TD" = 6, "TD_Perc" = 7, "Sub_Attempts" = 8, "Pass" = 9, "Rev" = 10) %>% 
  gather() %>% 
  separate(value, into = c("fighter_1", "fighter_2"), sep = "  ", extra = "merge") %>% 
  mutate_all(~str_replace_all(.x, "\n", "")) %>% 
  mutate_all(str_trim) %>% 
  pivot_wider(names_from = key, values_from = c(fighter_1, fighter_2)) 
  




# Round-by-Round Data -----------------------------------------------------


# Round Manual way
table_df[2] %>% 
  html_table(trim = TRUE, fill = TRUE) %>% 
  do.call("rbind", .) %>% 
  as_tibble(.name_repair = "unique") %>% 
  rename("Fighter" = 1, "KD" = 2, "Sig_Strike" = 3, "Sig_Strike_Percent" = 4, "Total_Strikes" = 5, 
         "TD" = 6, "TD_Percent" = 7, "Sub_Attempts_Perc" = 8, "Pass" = 9, "Rev" = 10) %>% 
  mutate_all(~str_replace_all(.x, "\n", "")) %>% 
  mutate(Round = row_number()) %>% 
  separate(KD, into = c("KD_1", "KD_2"), sep = " ", extra = "merge") %>% 
  separate(Sig_Strike_Percent, into = c("Sig_Strike_Percent_1", "Sig_Strike_Percent_2"), sep = " ", extra = "merge") %>% 
  separate(Sub_Attempts_Perc, into = c("Sub_Attempts_Perc_1", "Sub_Attempts_Perc_2"), sep = " ", extra = "merge") %>% 
  separate(Pass, into = c("Pass_1", "Pass_2"), sep = " ", extra = "merge") %>% 
  separate(Rev, into = c("Rev_1", "Rev_2"), sep = " ", extra = "merge") %>% 
  separate(TD_Percent, into = c("TD_Percent_1", "TD_Percent_2"), sep = " ", extra = "merge") %>% 
  separate(Fighter, into = c("Fighter_1", "Fighter_2"), sep = "  ", extra = "merge") %>% 
  separate(Sig_Strike, into = c("Sig_Strike_1", "Sig_Strike_2"), sep = "  ", extra = "merge") %>% 
  separate(Total_Strikes, into = c("Total_Strikes_1", "Total_Strikes_2"), sep = "  ", extra = "merge") %>% 
  separate(TD, into = c("TD_1", "TD_2"), sep = "  ", extra = "merge")


# Round Tidy Format
table_df[2] %>% 
  html_table(trim = TRUE, fill = TRUE) %>% 
  do.call("rbind", .) %>% 
  as_tibble(.name_repair = "unique") %>% 
  rename("Fighter" = 1, "KD" = 2, "Sig_Strike" = 3, "Sig_Strike_Percent" = 4, "Total_Strikes" = 5, 
         "TD" = 6, "TD_Percent" = 7, "Sub_Attempts_Perc" = 8, "Pass" = 9, "Rev" = 10) %>% 
  gather() %>% 
  separate(value, into = c("fighter_1", "fighter_2"), sep = "  ", extra = "merge") %>% 
  mutate_all(~str_replace_all(.x, "\n", " ")) %>% 
  mutate_all(str_trim) %>% 
  pivot_longer(cols = c(fighter_1, fighter_2)) %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  unnest()


# Round Head to Head format 
table_df[2] %>% 
  html_table(trim = TRUE, fill = TRUE) %>% 
  do.call("rbind", .) %>% 
  as_tibble(.name_repair = "unique") %>% 
  rename("Fighter" = 1, "KD" = 2, "Sig_Strike" = 3, "Sig_Strike_Percent" = 4, "Total_Strikes" = 5, 
         "TD" = 6, "TD_Percent" = 7, "Sub_Attempts_Perc" = 8, "Pass" = 9, "Rev" = 10) %>% 
  gather() %>% 
  separate(value, into = c("fighter_1", "fighter_2"), sep = "  ", extra = "merge") %>% 
  mutate_all(~str_replace_all(.x, "\n", " ")) %>% 
  mutate_all(str_trim) %>% 
  pivot_wider(names_from = key, values_from = c(fighter_1, fighter_2)) %>% 
  unnest() %>% 
  mutate(round = row_number())



# Data Scraping -----------------------------------------------------------



UFC <- "http://ufcstats.com/statistics/events/completed?page=all"
Cards <- scrape_cards(UFC)
Fights <- Cards %>% mutate(fight_links = map(links, scrape_fights))
