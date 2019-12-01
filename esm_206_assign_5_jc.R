# ------
# attach packages
# ------

library(tidyverse)
library(janitor)
library(kableExtra)
library(car)
library(beeswarm)

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
              values_from = n) 

channel_class_table_props <- channel_class_table %>%
  #pivot wider to make a contingency table, long format bad for this
  adorn_percentages(denominator = "row") %>% 
  # denominator = row to get pct on row
  adorn_pct_formatting(digits = 1) %>% 
  # rounding the decimal places
  adorn_ns(position = "front")
# numbers in front of percentages

channel_class_table_props %>% 
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

# -----
# Results C
# chi-squared
# -----

# Using the 2017 counts found in Results B above, answer: is there a significant difference in where in the channel Pacific giant salamanders are located (pool, cascade or side channel) between the two sections (old growth and clear cut)?

# Is there an association between where PGS live in the channel and what type of forest it is?

# H0: There is no association in where in the channel PGS are located b/w the two sections (CC vs. OG)
# HA: There is an association in where in the channel PGS are located b/w the two sections (CC vs. OG)
# Chi-squared test 

channel_class_table_chi <- channel_class_table %>% 
  select(-creek_area)

channel_class_chi <- chisq.test(channel_class_table_chi)


# ----
# Results E
# ----

creek_area_weight <- salamander_df %>%  
  #Call out data and assign it to an object.
  #Pipe to move to the next step.
  filter(year %in% c("2017")) %>%
  # Filter out the observations during the year 2017.
  filter(unittype %in% c("C", "P", "SC")) %>% 
  # want only channel pool and side-channel
  group_by(unittype, year) %>%
  # Group receiving section and year by collapsing
  # multiple data points into a single row.
  # Pipe to move to the next step.
  select(year, unittype, weight) %>%
  # Select specific columns to keep in the data frame.
  mutate("creek_area" = ifelse(unittype == "C", "Channel",
                                 ifelse(unittype == "P", "Pool", "Side-Channel")))
# Compare weights of Pacific giant salamanders in pools, cascades and side-channels of Mack Creek in 2017.

creek_area_weight_av <- creek_area_weight %>% 
  #Call out data and assign it to an object.
  #Pipe to move to the next step.
  group_by(unittype, year) %>%
  # Group receiving section and year by collapsing
  # multiple data points into a single row.
  # Pipe to move to the next step.
  summarize(weight_av = mean(weight, na.rm = TRUE)) %>% 
  # Summarize (new column = whatever you're doing(metric)).
  filter(year %in% c(2017))
# Filter out the observations during the year 2017.

# visually explore the data to see if means are normally distributed

ggplot() +
  geom_jitter(data = creek_area_weight, 
              aes(x = creek_area, 
                  y = weight,
                  color = creek_area)) +
  scale_color_manual(breaks = c("Cascade", 
                                "Pool",
                                "Side-Channel"), 
                     values = c("darkseagreen3", 
                                "darkslategray",
                                "lightsalmon")) +
  # Specify colors for the points.
  theme_minimal() +
  # Call out a theme to tweak the display of an existing theme.
  theme(legend.position = "none") +
geom_point(data = creek_area_weight_av,
           aes(x = weight_av,
               y = creek_area))

ggplot(data = creek_area_weight) +
  geom_density(aes(x = weight,
               # Create a graph and call out data to be used,
               # including what the x variable is.
               fill = as.character(creek_area),
               color = as.character(creek_area)),
               alpha = 0.3) +
  # Using as.character allows me to group them by year.
  scale_fill_manual(breaks = c("Channel", 
                               "Pool",
                               "Side-Channel"), 
                    values = c("darkseagreen3", 
                               "darkslategray",
                               "lightsalmon")) +
  # Specify colors for the fill of the curves.
  scale_color_manual(breaks = c("Channel", 
                                "Pool",
                                "Side-Channel"), 
                     values = c("darkseagreen3", 
                                "darkslategray",
                                "lightsalmon")) +
  # Specify colors for the lines.
  theme_minimal() +
  # Call out a theme to tweak the display of an existing theme.
  theme(legend.position = "none") +  
  # Don't want a legend, label manually.
  annotate(
    geom = "curve",
    # Use geom="curve" for curved line, geom="segment" for straight.
    x = 3.95,
    # Line start.
    y = 0.125,
    # Line start.
    xend = 14,
    # Line end.
    yend = 0.1155,
    # Line end.
    curvature = .2,
    # Level of curve.
    color = "lightsalmon") +
  # Values entered manually for where I want lines to begin and end.
  annotate(geom = "text",
           x = 14.55,
           # Where my text will be.
           y = .115,
           # Where my text will be.
           label = "Side-Channel",
           hjust = "left",
           color = "lightsalmon") +
  # Annotation for Side-Channel.
  annotate(
    geom = "curve",
    # Use geom="curve" for curved line, geom="segment" for straight.
    x = 12.5,
    # Start of line.
    y = 0.03,
    # Start of line.
    xend = 25,
    # End of line.
    yend = 0.045,
    # End of line.
    curvature = .2,
    # Level of curve.
    color = "darkseagreen3") +
  # Values entered manually for where I want the lines to begin and end.
  annotate(geom = "text",
           x = 26,
           # Where my label will sit.
           y = 0.045,
           # Where my label will sit.
           label = "Channel",
           hjust = "left",
           color = "darkseagreen3") +
  # Annotation for Channel.
  annotate(
    geom = "curve",
    # Use geom="curve" for curved line, geom="segment" for straight.
    x = 18,
    # Start of line.
    y = 0.015,
    # Start of line.
    xend = 30,
    # End of line.
    yend = 0.025,
    # End of line.
    curvature = .2,
    # Level of curve.
    color = "darkslategray") +
  # Values entered manually for where I want the lines to begin and end.
  annotate(geom = "text",
           x = 31,
           # Where my label will sit.
           y = 0.0255,
           # Where my label will sit.
           label = "Pool",
           hjust = "left",
           color = "darkslategray") +
  geom_vline(xintercept = 7.520850,
             color = "darkseagreen3",
             linetype = "dashed",
             size = 0.5) +
  geom_vline(xintercept = 9.297500,
             color = "darkslategray",
             linetype = "dashed",
             size = 0.5) +
  geom_vline(xintercept = 5.676646,
             color = "lightsalmon",
             linetype = "dashed",
             size = 0.5) +
  # Means taken from lobster_size_av dataframe.
  # Using vline allows me to draw a vertical line of the mean.
  labs(x = "Salamander weight (grams)",
       y = "Kernel density")
  # Create labels for the x and y axes and title.
  # Create caption below the graph and edit font size and style.




ggplot(data = creek_area_weight, aes(x = weight)) +
  geom_histogram() +
  facet_wrap(~creek_area)

ggplot(data = creek_area_weight, aes(sample = weight)) +
  geom_qq() +
  facet_wrap(~creek_area)

# samples are not normally distributed but we can assume that because of central limit theorem that the means of samples will be

#need to test for equal variance because ANOVA only works if group variance in the largest is only 4x greater than the smallest

#levene's test

# H0: Group variances are equal
# HA: Group variances are not equal

salamander_var <- leveneTest(weight ~ creek_area, data = creek_area_weight)

# retain null - variances are equal, can do ANOVA

salamander_aov <- aov(weight ~ creek_area, data = creek_area_weight)

summary(salamander_aov)

# p = 0.007, there is a significant difference between at least 2 of the creek areas
# tukey's HSD for post-hoc

salamander_post_hoc <- TukeyHSD(salamander_aov)
salamander_post_hoc

# side-channel and channel (p = 0.05), side-channel and pool (p = 0.01) have signifcant differences, pool and channel don't (p=0.28)
