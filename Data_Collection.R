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



link <- "http://ufcstats.com/fight-details/7200343d2e957cf3"

link <- read_html(link)
table_df <- link %>% html_nodes("table") 




# Fight Result ------------------------------------------------------------



link %>% 
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
  rename_all(.funs = ~str_replace(.x, "\\s|/", "_")) 
  


link %>% 
  html_node(".b-fight-details__person-status_style_gray") %>% 
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()



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
         "TD" = 6, "TD_Percent" = 7, "Sub_Attempts" = 8, "Pass" = 9, "Rev" = 10) %>% 
  gather() %>% 
  separate(value, into = c("fighter_1", "fighter_2"), sep = "  ", extra = "merge") %>% 
  mutate_all(~str_replace_all(.x, "\n", "")) %>% 
  mutate_all(str_trim) %>% 
  pivot_longer(cols = c(fighter_1, fighter_2)) %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  separate(Sig_Strike, into = c("Sig_Strike_Landed", "Sig_Strike_Attempts"), sep = " of ", extra = "merge") %>% 
  separate(Total_Strikes, into = c("Strike_Landed", "Strike_Attempts"), sep = " of ", extra = "merge") %>% 
  separate(TD, into = c("TD_Landed", "TD_Attempts"), sep = " of ", extra = "merge") %>% 
  mutate_at(vars(contains("Percent")), ~.01*str_replace(.x, "%", "") %>% as.numeric()) %>% 
  mutate_at(vars(-contains(c("name", "fighter"))), as.numeric)


# Fight Head to head format
table_df[1] %>% 
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
  




# Round-by-Round Data -----------------------------------------------------


# Round Manual way
table_df[2] %>% 
  html_table(trim = TRUE, fill = TRUE) %>% 
  do.call("rbind", .) %>% 
  as_tibble(.name_repair = "unique") %>% 
  rename("Fighter" = 1, "KD" = 2, "Sig_Strike" = 3, "Sig_Strike_Percent" = 4, "Total_Strikes" = 5, 
         "TD" = 6, "TD_Percent" = 7, "Sub_Attempts" = 8, "Pass" = 9, "Rev" = 10) %>% 
  mutate_all(~str_replace_all(.x, "\n", "")) %>% 
  mutate(Round = row_number()) %>% 
  separate(KD, into = c("KD_1", "KD_2"), sep = " ", extra = "merge") %>% 
  separate(Sig_Strike_Percent, into = c("Sig_Strike_Percent_1", "Sig_Strike_Percent_2"), sep = " ", extra = "merge") %>% 
  separate(Sub_Attempts, into = c("Sub_Attempts_1", "Sub_Attempts_2"), sep = " ", extra = "merge") %>% 
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
         "TD" = 6, "TD_Percent" = 7, "Sub_Attempts" = 8, "Pass" = 9, "Rev" = 10) %>% 
  gather() %>% 
  separate(value, into = c("fighter_1", "fighter_2"), sep = "  ", extra = "merge") %>% 
  mutate_all(~str_replace_all(.x, "\n", " ")) %>% 
  mutate_all(str_trim) %>% 
  pivot_longer(cols = c(fighter_1, fighter_2)) %>% 
  pivot_wider(names_from = key, values_from = value) %>% 
  unnest() %>% 
  separate(Sig_Strike, into = c("Sig_Strike_Landed", "Sig_Strike_Attempts"), sep = " of ", extra = "merge") %>% 
  separate(Total_Strikes, into = c("Strike_Landed", "Strike_Attempts"), sep = " of ", extra = "merge") %>% 
  separate(TD, into = c("TD_Landed", "TD_Attempts"), sep = " of ", extra = "merge") %>% 
  mutate_at(vars(contains("Percent")), ~.01*str_replace(.x, "%", "") %>% as.numeric()) %>% 
  mutate_at(vars(-contains(c("name", "fighter"))), as.numeric) %>% 
  view()
  


# Round Head to Head format 
table_df[2] %>% 
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
  mutate_at(vars(-contains("Fighter", ignore.case = FALSE)), as.numeric) %>% 
  view()



# Data Scraping -----------------------------------------------------------



UFC <- "http://ufcstats.com/statistics/events/completed?page=all"
Cards <- scrape_cards(UFC)
Fights <- Cards %>% mutate(fight_links = map(links, scrape_fights))




















