#### CH 2 ANALYSIS - PLANT NUTRIENTS ####
# Nov 8, 2019 with help from Norah

# Install packages
install.packages("here")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("cowplot")

# Load packages
library(here)
library(ggplot2)
library(tidyverse)
library(cowplot)

# Load data
plant_inventory <- read.csv(file = "Data_sheets/Inventory_SUBSAMPLE.csv")
plant_nutr <- read.csv(file = "Data_sheets/nutr_results_SUBSAMPLE.csv")

# Get rid of NA columns 
plant_inventory <- na.omit(plant_inventory[,c(1:7)])
head(plant_inventory)

plant_nutr <- na.omit(plant_nutr[,c(1:18)])
head(plant_nutr)

# Merge dataframes (inventory + nutrient data)
plant_data <- merge(plant_inventory, plant_nutr, by = "unq_ID")
head(plant_data)

# Removing M from sample_pt column
plant_data$sample_pt <- gsub("M", "", plant_data$sample_pt, fixed = TRUE)
head(plant_data)
plant_data$sample_pt <- as.numeric(plant_data$sample_pt)


##### DATA VIS ####
# Box plots -------------------------------------------------------------------------------

# Visualize Ca by site type
Ca <- ggplot(data = plant_data,
       aes(y = Ca, x = site_type, col = species)) + geom_boxplot()

# Visualize N by site type
N <- ggplot(data = plant_data,
       aes(y = total_N, x = site_type, col = species)) + geom_boxplot()

# Visualize P by site type
P <- ggplot(data = plant_data,
       aes(y = P, x = site_type, col = species)) + geom_boxplot()

# Visualize K by site type
K <- ggplot(data = plant_data,
       aes(y = K, x = site_type, col = species)) + geom_boxplot()

# Visualize total_S by site type
total_S <- ggplot(data = plant_data,
       aes(y = total_S, x = site_type, col = species)) + geom_boxplot()

# Visualize S by site type
S <- ggplot(data = plant_data,
       aes(y = S, x = site_type, col = species)) + geom_boxplot()

# Visualize Mg by site type
Mg <- ggplot(data = plant_data,
       aes(y = Mg, x = site_type, col = species)) + geom_boxplot()

# Plot all together
plot_grid(N, P, K, Ca, S, Mg, ncol = 3, nrow = 2)


#### Scatter plots: looking at distance from shore ####

# Ca --------------------------------------------------------------------
# Visualize Ca with VAPA
Ca_VAPA <- ggplot(data = plant_data %>% filter(species == "VAPA"),
            aes(y = Ca, x = sample_pt, col = site_type)) + geom_point() + geom_smooth(method = "lm") +
            ggtitle("VAPA") 
# Visualize Ca with GASH
Ca_GASH <- ggplot(data = plant_data %>% filter(species == "GASH"),
            aes(y = Ca, x = sample_pt, col = site_type)) + geom_point() + geom_smooth(method = "lm") +
            ggtitle("GASH") + theme(legend.position = "none")
# Visualize Ca with MEFE
Ca_MEFE <- ggplot(data = plant_data %>% filter(species == "MEFE"),
            aes(y = Ca, x = sample_pt, col = site_type)) + geom_point() + geom_smooth(method = "lm") +
            ggtitle("MEFE") + theme(legend.position = "none")

plot_grid(Ca_GASH, Ca_MEFE, Ca_VAPA, ncol = 3)


# N --------------------------------------------------------------------
# Visualize N with VAPA
total_N_VAPA <- ggplot(data = plant_data %>% filter(species == "VAPA"),
                  aes(y = total_N, x = sample_pt, col = site_type)) + geom_point() + geom_smooth(method = "lm") +
  ggtitle("VAPA") 
# Visualize N with GASH
total_N_GASH <- ggplot(data = plant_data %>% filter(species == "GASH"),
                  aes(y = total_N, x = sample_pt, col = site_type)) + geom_point() + geom_smooth(method = "lm") +
  ggtitle("GASH") + theme(legend.position = "none")
# Visualize N with MEFE
total_N_MEFE <- ggplot(data = plant_data %>% filter(species == "MEFE"),
                  aes(y = total_N, x = sample_pt, col = site_type)) + geom_point() + geom_smooth(method = "lm") +
  ggtitle("MEFE") + theme(legend.position = "none")

plot_grid(total_N_GASH, total_N_MEFE, total_N_VAPA, ncol = 3)


# P --------------------------------------------------------------------
# Visualize P with VAPA
P_VAPA <- ggplot(data = plant_data %>% filter(species == "VAPA"),
                  aes(y = P, x = sample_pt, col = site_type)) + geom_point() + geom_smooth(method = "lm") +
  ggtitle("VAPA") 
# Visualize P with GASH
P_GASH <- ggplot(data = plant_data %>% filter(species == "GASH"),
                  aes(y = P, x = sample_pt, col = site_type)) + geom_point() + geom_smooth(method = "lm") +
  ggtitle("GASH") + theme(legend.position = "none")
# Visualize P with MEFE
P_MEFE <- ggplot(data = plant_data %>% filter(species == "MEFE"),
                  aes(y = P, x = sample_pt, col = site_type)) + geom_point() + geom_smooth(method = "lm") +
  ggtitle("MEFE") + theme(legend.position = "none")

plot_grid(P_GASH, P_MEFE, P_VAPA, ncol = 3)


# K --------------------------------------------------------------------
# Visualize K with VAPA
K_VAPA <- ggplot(data = plant_data %>% filter(species == "VAPA"),
                 aes(y = K, x = sample_pt, col = site_type)) + geom_point() + geom_smooth(method = "lm") +
  ggtitle("VAPA") 
# Visualize K with GASH
K_GASH <- ggplot(data = plant_data %>% filter(species == "GASH"),
                 aes(y = K, x = sample_pt, col = site_type)) + geom_point() + geom_smooth(method = "lm") +
  ggtitle("GASH") + theme(legend.position = "none")
# Visualize K with MEFE
K_MEFE <- ggplot(data = plant_data %>% filter(species == "MEFE"),
                 aes(y = K, x = sample_pt, col = site_type)) + geom_point() + geom_smooth(method = "lm") +
  ggtitle("MEFE") + theme(legend.position = "none")

plot_grid(K_GASH, K_MEFE, K_VAPA, ncol = 3)
