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
  #filter data for only Clear Cut and Old Growth
  filter(unittype %in% c("C", "SC", "P")) %>%
  #filter for only pools, side channels and channel
  mutate("creek_area" = ifelse(unittype == "C", "Channel",
                        ifelse(unittype == "SC", "Side-Channel", "Pool"))) %>%
  # make names nicer for table
  count(section, creek_area)
# count the data for salamanders in these variables

channel_class_table <- channel_class_df %>% 
  pivot_wider(names_from = section,
              values_from = n) %>%
  #pivot wider to make a contingency table, long format bad for this
  adorn_percentages(denominator = "row") %>% 
  # denominator = row to get pct on row
  adorn_pct_formatting(digits = 1) %>% 
  # rounding the decimal places
  adorn_ns(position = "front")
# numbers in front of percentages

channel_class_table %>% 
  kable(col.names = c("Area of Creek",
                      "Clear Cut", 
                      "Old Growth")) %>% 
  #column names - can't change row names in kable
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                position = "center") %>% 
  # full width (F), centered table
  add_header_above(bold = TRUE,
                   line = TRUE, 
                   c("Salamanders Observed in Mack Creek" = 3))
# header number has to match number of columns
