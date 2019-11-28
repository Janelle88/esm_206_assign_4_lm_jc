# ------
# attach packages
# ------

library(tidyverse)
library(janitor)
library(kableExtra)

# ------
# Results A
# Lauren working on this
# ------

salamander_df <- read_csv("mack_creek_vertebrates.csv") %>% 
  clean_names() %>% 
  filter(species %in% c("DITE"))


# ------
# Results B
# Table
# ------

channel_class_df <- salamander_df %>% 
  filter(section %in% c("CC", "OG")) %>% 
  filter(unittype %in% c("C", "SC", "P")) %>% 
  count(section, unittype)

channel_class_table <- channel_class_df %>% 
  pivot_wider(names_from = section,
              values_from = n)

channel_class_table %>% 
  kable(col.names = c("Clear Cut", 
                      "Old Growth")),
row.names = c("Channel", 
              "Side-Channel", 
              "Pool"))
