################################################################################
# title
################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(here,
               tidyverse                                                        
)

# Load data --------------------------------------------------------------------
eu_panel <- readRDS(file = here("data", "economic_unit_annual_panel.rds"))

## PROCESSING ##################################################################

# Identify shrimp fishing vessels ----------------------------------------------
shrimp_eus <- eu_panel %>% 
  group_by(eu_rnpa, state) %>% 
  summarize(shrimp = all(shrimp),
            tuna = any(tuna),
            sardine = any(sardine),
            others = any(others),
            n = n()) %>% 
  ungroup() %>% 
  filter(shrimp, !tuna, !sardine, !others, n >= 2) %>% 
  pull(eu_rnpa)

# Assemble data for figure -----------------------------------------------------
pct_shrimp_data <- eu_panel %>%
  filter(year > 2011) %>% 
  ungroup() %>%
  mutate(s = ifelse(eu_rnpa %in% shrimp_eus, "shrimp", "other")) %>%
  group_by(year, s) %>%
  summarize(subsidy_cap_l = sum(subsidy_cap_l, na.rm = T)) %>%
  ungroup() %>% 
  group_by(year) %>% 
  mutate(subsidy_cap_l = subsidy_cap_l / sum(subsidy_cap_l)) %>% 
  ungroup() %>% 
  drop_na(subsidy_cap_l)

# What is the range? -----------------------------------------------------------
pct_shrimp_data %>%
  filter(s == "shrimp") %>%
  pull(subsidy_cap_l) %>%
  range()

## VISUALIZE ###################################################################

# Build figure ----------------------------------------------------------------------------
pct_shrimp <- 
  ggplot(data = pct_shrimp_data,
         mapping = aes(x = year, y = subsidy_cap_l, fill = s)) +
  geom_col(color = "black") +
  labs(x = "Year",
       y = "% of total subsidy allocated",
       fill = "Fishery") +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(2012, 2019, by = 2)) +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  theme_bw()

pct_shrimp

## END OF SCRIPT ###############################################################