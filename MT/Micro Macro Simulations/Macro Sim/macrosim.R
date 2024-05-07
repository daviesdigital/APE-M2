install.packages("tidyverse")
install.packages("readxl")

library(readxl)
library(tidyverse)


# Import Data -------------------------------------------------------------
data <- read_excel("C:/Users/david/OneDrive/Desktop/Education/APE-M2/MT/Micro Macro Simulations/Macro Sim/MacroSim_Data.xlsx",
                   sheet = "Sheet2")

# Rename Data -------------------------------------------------------------
data <- data %>%
  rename(year = Country)
  
  tail(data)

  
  # Given values
  alpha <- 0.4
  theta <- 2


# Calculate H -------------------------------------------------------------

  data = data %>% 
    mutate(
      FRA_H = FRA_hours_pc * FRA_pop,
      GER_H = GER_hours_pc * GER_pop,
      ITA_H = ITA_hours_pc * ITA_pop,
      UK_H = UK_hours_pc * UK_pop,
      US_H = US_hours_pc * US_pop
    )
    

# Finding K ---------------------------------------------------------
  data = data %>% 
    mutate(
      K_FRA = (FRA_gdp / FRA_A * (FRA_hours_pc)^0.6)^(1/0.4),
      K_GER = (GER_gdp / GER_A * (GER_hours_pc)^0.6)^(1/0.4),
      K_ITA = (ITA_gdp / ITA_A * (ITA_hours_pc)^0.6)^(1/0.4),
      K_UK = (UK_gdp / UK_A * (UK_hours_pc)^0.6)^(1/0.4),
      K_US = (US_gdp / UK_A * (US_hours_pc)^0.6)^(1/0.4)
    )
  
  
  
  # Compute MRS and MP for each country
  data <- data %>%
    mutate(
      MRS_FRA = 1 / (FRA_C * (1 - FRA_hours_pc / 100)),
      MPH_FRA = FRA_gdp / (FRA_pop * (1 - alpha) * FRA_hours_pc^(-alpha) / 100),
      MRS_GER = 1 / (GER_C * (1 - GER_hours_pc / 100)),
      MPH_GER = GER_gdp / (GER_pop * (1 - alpha) * GER_hours_pc^(-alpha) / 100),
      MRS_ITA = 1 / (ITA_C * (1 - ITA_hours_pc / 100)),
      MPH_ITA = ITA_gdp / (ITA_pop * (1 - alpha) * ITA_hours_pc^(-alpha) / 100),
      MRS_UK = 1 / (UK_C * (1 - UK_hours_pc / 100)),
      MPH_UK = UK_gdp / (UK_pop * (1 - alpha) * UK_hours_pc^(-alpha) / 100),
      MRS_US = 1 / (US_C * (1 - US_hours_pc / 100)),
      MPH_US = US_gdp / (US_pop * (1 - alpha) * US_hours_pc^(-alpha) / 100)
    )
  
  # Compute the gap for each country
  data <- data %>%
    mutate(
      gap_FRA = MRS_FRA - MPH_FRA,
      gap_GER = MRS_GER - MPH_GER,
      gap_ITA = MRS_ITA - MPH_ITA,
      gap_UK = MRS_UK - MPH_UK,
      gap_US = MRS_US - MPH_US
    )
  
  tail(data)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  