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


# Finding K ---------------------------------------------------------
data = data %>%
  mutate(
    FRA_K = (FRA_gdp / (FRA_A * (FRA_H)^(1-alpha)) )^(1/alpha),
    GER_K = (GER_gdp / (GER_A * (GER_H)^(1-alpha))  )^(1/alpha),
    ITA_K = (ITA_gdp / (ITA_A * (ITA_H)^(1-alpha)) )^(1/alpha),
    UK_K = (UK_gdp / (UK_A * (UK_H)^(1-alpha)) )^(1/alpha),
    US_K = (US_gdp / (UK_A * (US_H)^(1-alpha)) )^(1/alpha)
  )



# Time Devoted to Labour --------------------------------------------------
total_possible_hours <- 8760  # in hours

# Calculate the share of time devoted to leisure for each country
data <- data %>%
  mutate(
    FRA_h = 1 - FRA_hours_pc /  total_possible_hours,
    GER_h = 1 - GER_hours_pc /  total_possible_hours,
    ITA_h = 1 - ITA_hours_pc / total_possible_hours,
    UK_h = 1 - UK_hours_pc / total_possible_hours,
    US_h = 1 - US_hours_pc / total_possible_hours
  )

data <- data %>%
  mutate(
    FRA_h =  FRA_hours_pc /  total_possible_hours,
    GER_h =  GER_hours_pc /  total_possible_hours,
    ITA_h =  ITA_hours_pc / total_possible_hours,
    UK_h = UK_hours_pc / total_possible_hours,
    US_h =  US_hours_pc / total_possible_hours
  )


# Compute MRS and MP for each country
data <- data %>%
  mutate(
    MRS_FRA = -(1 - FRA_h)  / (theta * FRA_C),
    MPH_FRA = FRA_A * FRA_K^(alpha) * (1-alpha) * FRA_H^(-alpha),
    MRS_GER = -(1 - GER_h)  / (theta * GER_C),
    MPH_GER = GER_A * GER_K^(alpha) * (1-alpha) * GER_H^(-alpha),
    MRS_ITA = -(1 - ITA_h)  / (theta * ITA_C),
    MPH_ITA = ITA_A * ITA_K^(alpha) * (1-alpha) * ITA_H^(-alpha),
    MRS_UK = -(1 - UK_h)  / (theta * UK_C),
    MPH_UK = UK_A * UK_K^(alpha) * (1-alpha) * UK_H^(-alpha),
    MRS_US = -(1 - US_h)  / (theta * US_C),
    MPH_US = US_A * US_K^(alpha) * (1-alpha) * US_H^(-alpha),
  )


# Compute the gap for each country
data <- data %>%
  mutate(
    MRS_gap_FRA = MRS_US - MRS_FRA,
    MRS_gap_GER = MRS_US - MRS_GER,
    MRS_gap_ITA = MRS_US - MRS_ITA,
    MRS_gap_UK = MRS_US - MRS_UK
  )

data <- data %>%
  mutate(
    MPH_gap_FRA = MPH_US - MPH_FRA,
    MPH_gap_GER = MPH_US - MPH_GER,
    MPH_gap_ITA = MPH_US - MPH_ITA,
    MPH_gap_UK = MPH_US - MPH_UK
  )


  
  

# Graph results -----------------------------------------------------------

  # Convert 'year' to numeric
  data$year <- as.numeric(data$year)
  
  # Plotting total hours worked for each country over the years
  ggplot(data, aes(x = year)) +
    geom_line(aes(y = US_H, color = "US"), size = 1.1, na.rm = TRUE) +
    labs(x = "Year", y = "Hours worked (in Millions)", color = "Country", title = "Total hours worked") +
    scale_x_continuous(limits = c(1970, max(data$year))) +
    theme_minimal()
  
  # Calculate the maximum hours worked across all countries for each year
  max_hours <- apply(data[, c("FRA_H", "GER_H", "ITA_H", "UK_H", "US_H")], 1, max)
  max_hours
  
  # Normalize the total hours worked for each country by the maximum value for each year
  data <- data %>%
    mutate(
      FRA_H_norm = FRA_H / max_hours,
      GER_H_norm = GER_H / max_hours,
      ITA_H_norm = ITA_H / max_hours,
      UK_H_norm = UK_H / max_hours,
    )
  
  # Plotting normalized total hours worked for each country over the years
  ggplot(data, aes(x = year)) +
    geom_line(aes(y = FRA_H_norm, color = "France"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = GER_H_norm, color = "Germany"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = ITA_H_norm, color = "Italy"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = UK_H_norm, color = "UK"), size = 1.1, na.rm = TRUE) +
    #geom_line(aes(y = US_H_norm, color = "US"), size = 1.1, na.rm = TRUE) +
    labs(x = "Year", y = "Proportion of US Hours Worked", color = "Country", title = "Total hours worked (Proportion of US)") +
    scale_x_continuous(limits = c(1970, max(data$year))) +
    theme_minimal()
  
  
  
  # Plotting hours worked per person  for each country over the years
  ggplot(data, aes(x = year)) +
    geom_line(aes(y = FRA_hours_pc, color = "France"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = GER_hours_pc, color = "Germany"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = ITA_hours_pc, color = "Italy"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = UK_hours_pc, color = "UK"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = US_hours_pc, color = "US"), size = 1.1, na.rm = TRUE) +
    labs(x = "Year", y = "Hours worked", color = "Country", title = "Hours worked per person") +
    scale_x_continuous(limits = c(1970, max(data$year))) +
    theme_minimal()
  
  # Plotting 
  ggplot(data, aes(x = year)) +
    geom_line(aes(y = gap_FRA, color = "France"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = gap_GER, color = "Germany"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = gap_ITA, color = "Italy"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = gap_UK, color = "UK"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = gap_US, color = "US"), size = 1.1, na.rm = TRUE) +
    labs(x = "Year", y = "Difference", color = "Country", title = "Gap between MRS and MPH") +
    scale_x_continuous(limits = c(1985, max(data$year))) +
    theme_minimal()
  
  # Plotting Labour Force Paricipation for each country over the years
  ggplot(data, aes(x = year)) +
    geom_line(aes(y = FRA_LFS, color = "France"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = GER_LFS, color = "Germany"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = ITA_LFS, color = "Italy"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = UK_LFS, color = "UK"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = US_LFS, color = "US"), size = 1.1, na.rm = TRUE) +
    labs(x = "Year", y = "%", color = "Country", title = "Labour Force Paricipation") +
    scale_x_continuous(limits = c(1970, max(data$year))) +
    theme_minimal()
  
  # Plotting Employment for each country over the years
  ggplot(data, aes(x = year)) +
    geom_line(aes(y = FRA_EMP, color = "France"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = GER_EMP, color = "Germany"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = ITA_EMP, color = "Italy"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = UK_EMP, color = "UK"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = US_EMP, color = "US"), size = 1.1, na.rm = TRUE) +
    labs(x = "Year", y = "%", color = "Country", title = "Employment Rate") +
    scale_x_continuous(limits = c(1970, max(data$year))) +
    theme_minimal()


  # Plotting gap between MRS and MPH
  ggplot(data, aes(x = year)) +
    geom_line(aes(y = MRS_gap_FRA, color = "France"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = MRS_gap_GER, color = "Germany"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = MRS_gap_ITA, color = "Italy"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = MRS_gap_UK, color = "UK"), size = 1.1, na.rm = TRUE) +
    labs(x = "Year", y = "Difference", color = "Country", title = "MRS gap with US") +
    scale_x_continuous(limits = c(1970, max(data$year))) +
    scale_y_continuous(limits = c(0, 0.0000025)) +
    theme_minimal() 
  
  ggplot(data, aes(x = year)) +
    geom_line(aes(y = MPH_gap_FRA, color = "France"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = MPH_gap_GER, color = "Germany"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = MPH_gap_ITA, color = "Italy"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = MPH_gap_UK, color = "UK"), size = 1.1, na.rm = TRUE) +
    labs(x = "Year", y = "Difference", color = "Country", title = "MPH gap with US") +
    scale_x_continuous(limits = c(1985, max(data$year))) +
    theme_minimal()
  
  
  ggplot(data, aes(x = year)) +
    geom_line(aes(y = FRA_TAX, color = "France"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = GER_TAX, color = "Germany"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = ITA_TAX, color = "Italy"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = UK_TAX, color = "UK"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y = US_TAX, color = "US"), size = 1.1, na.rm = TRUE) +
    labs(x = "Year", y = "% of labour cost", color = "Country", title = "Tax wedge") +
    scale_x_continuous(limits = c(2000, max(data$year))) +
    theme_minimal()
  
  

# Find the new Theta ------------------------------------------------------
  
  data <- data %>%
    mutate (
      theta_FRA = ((1 - US_h) * FRA_C) / ((1 - FRA_h) * US_C),
      theta_GER = ((1 - US_h) * GER_C) / ((1 - GER_h) * US_C),
      theta_ITA = ((1 - US_h) * ITA_C) / ((1 - ITA_h) * US_C),
      theta_UK = ((1 - US_h) * UK_C) / ((1 - UK_h) * US_C)
    )
  
  ggplot(data, aes(x = year)) +
    geom_line(aes(y =  theta_FRA, color = "France"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y =  theta_GER, color = "Germany"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y =  theta_ITA, color = "Italy"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y =  theta_UK, color = "UK"), size = 1.1, na.rm = TRUE) +
    labs(x = "Year", y = "Value of Theta", color = "Country", title = " Theta value to close labour gap with US") +
    scale_x_continuous(limits = c(1970, max(data$year))) +
    scale_y_continuous(limits = c(0, 0.3)) +
    theme_minimal() 

# Robustness Check --------------------------------------------------------

  sigma = 0.5
  robusttheta = 2
 
  #MRS with MU_C/MU_(1-h)
  data <- data %>%
    mutate(
      robustMRS_FRA = ((FRA_C^robusttheta * (1-FRA_h)^(1-robusttheta))^(-sigma) * robusttheta * FRA_C^(robusttheta-1)*(1-FRA_h)^(1-robusttheta)) /
        ((FRA_C^robusttheta * (1-FRA_h)^(1-robusttheta))^(-sigma) * FRA_C^(robusttheta)*(1-robusttheta)*(1-FRA_h)^(-robusttheta)),
      robustMRS_GER =  ((GER_C^robusttheta * (1-GER_h)^(1-robusttheta))^(-sigma) * robusttheta * GER_C^(robusttheta-1)*(1-GER_h)^(1-robusttheta)) /
        ((GER_C^robusttheta * (1-GER_h)^(1-robusttheta))^(-sigma) * GER_C^(robusttheta)*(1-robusttheta)*(1-GER_h)^(-robusttheta)),
      robustMRS_ITA =  ((ITA_C^robusttheta * (1-ITA_h)^(1-robusttheta))^(-sigma) * robusttheta * ITA_C^(robusttheta-1)*(1-ITA_h)^(1-robusttheta)) /
        ((ITA_C^robusttheta * (1-ITA_h)^(1-robusttheta))^(-sigma) * ITA_C^(robusttheta)*(1-robusttheta)*(1-ITA_h)^(-robusttheta)),
      robustMRS_UK =   ((UK_C^robusttheta * (1-UK_h)^(1-robusttheta))^(-sigma) * robusttheta * UK_C^(robusttheta-1)*(1-UK_h)^(1-robusttheta)) /
        ((UK_C^robusttheta * (1-UK_h)^(1-robusttheta))^(-sigma) * UK_C^(robusttheta)*(1-robusttheta)*(1-UK_h)^(-robusttheta)),
      robustMRS_US =  ((US_C^robusttheta * (1-US_h)^(1-robusttheta))^(-sigma) * robusttheta * US_C^(robusttheta-1)*(1-US_h)^(1-robusttheta)) /
        ((US_C^robusttheta * (1-US_h)^(1-robusttheta))^(-sigma) * US_C^(robusttheta)*(1-robusttheta)*(1-US_h)^(-robusttheta))
    )
  
  #Graph MRS
  ggplot(data, aes(x = year)) +
    geom_line(aes(y =  robustMRS_FRA, color = "France"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y =  robustMRS_GER, color = "Germany"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y =  robustMRS_ITA, color = "Italy"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y =  robustMRS_UK, color = "UK"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y =  robustMRS_US, color = "US"), size = 1.1, na.rm = TRUE) +
    labs(x = "Year", y = "Value", color = "Country", title = " Robust MRS") +
    scale_x_continuous(limits = c(1970, max(data$year))) +
    #scale_y_continuous(limits = c(0, 0.000005)) +
    theme_minimal() 
  
  
  # Compute the robust gap for each country
  data <- data %>%
    mutate(
      robustMRS_gap_FRA = robustMRS_US - robustMRS_FRA,
      robustMRS_gap_GER = robustMRS_US - robustMRS_GER,
      robustMRS_gap_ITA = robustMRS_US - robustMRS_ITA,
      robustMRS_gap_UK = robustMRS_US - robustMRS_UK
    )
  
  #Graph Robust MRS Gap
  ggplot(data, aes(x = year)) +
    geom_line(aes(y =  robustMRS_gap_FRA, color = "France"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y =  robustMRS_gap_GER, color = "Germany"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y =  robustMRS_gap_ITA, color = "Italy"), size = 1.1, na.rm = TRUE) +
    geom_line(aes(y =  robustMRS_gap_UK, color = "UK"), size = 1.1, na.rm = TRUE) +
    labs(x = "Year", y = "Difference", color = "Country", title = " robustMRS gap with US (Theta = 3.5)") +
    scale_x_continuous(limits = c(1970, max(data$year))) +
    #scale_y_continuous(limits = c(0, 0.000005)) +
    theme_minimal() 

  
