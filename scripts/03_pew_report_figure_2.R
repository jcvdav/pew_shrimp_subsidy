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
               cowplot,                                                         # Combine ggplot objects
               tidyverse                                                        # General data management
)

# Define list of relevant states -----------------------------------------------
states <- c("Baja california",
            "Baja california sur",
            "Campeche",
            "Chiapas",
            "Nayarit",
            "Oaxaca",
            "Quintana roo",
            "Sinaloa",
            "Sonora",
            "Tamaulipas",
            "Veracruz")

# Load data --------------------------------------------------------------------
#Anual national average
prices <- readRDS(file = here(
  "data",
  "annual_national_diesel_prices_2011_2020.rds")) %>% 
  filter(between(year, 2012, 2019))

# State level, when available
annual_state_prices <- 
  readRDS(
    file = here(
      "data",
      "annual_state_diesel_prices_cre_2017_2020.rds"
    )
  )
## PROCESSING ##################################################################

# Combine national and state-level data  ---------------------------------------
annual_state_diesel_prices <- annual_state_prices %>% 
  left_join(prices %>% select(year, rate), by = "year") %>% 
  mutate(mean_diesel_price_mxn_l = rate * mean_diesel_price_mxn_l) %>% 
  filter(between(year, 2012, 2019),
         state %in% states)

## VISUALIZE ###################################################################

# Time serie splot -------------------------------------------------------------
ts <- ggplot(data = prices,
             mapping = aes(x = year,
                           y = mean_diesel_price_mxn_l)) + 
  geom_line(data = annual_state_diesel_prices,
            aes(x = year, y = mean_diesel_price_mxn_l, group = state),
            size = 0.1) +
  geom_line() + 
  geom_point(size = 4, shape = 21, fill = "steelblue") +
  theme_minimal() +
  labs(x = "Year",
       y = bquote("Diesel price ("~MXP[2019]/L~")"))

# Subsidy as percent of price --------------------------------------------------
pct_sub <- ggplot(data = prices,
                  mapping = aes(x = year, y = 2 / mean_diesel_price_mxn_l)) +
  geom_col(fill = "steelblue", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(x = "Year",
       y = "Percent price subsidized\n(% of market price)")

# Combine ----------------------------------------------------------------------
fuel_prices_plot <- plot_grid(ts,
                              pct_sub,
                              ncol = 1,
                              labels = "AUTO", label_x = 0.95)

fuel_prices_plot

# END OF SCRIPT ################################################################
