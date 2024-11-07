## -------------------------- Formation Yann ---------------------------------###
# project/
  # - README.md   # Project description written in Markdown (we will talk about this later)
  # - /R/ # Where the R scripts live
  # - /input/"      # Data files
  # - /output/     # Results: plots, tables..
  
##### Introduction ####
data <- readr::read_csv("input/data.csv")

summary(data)

print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Torgersen")$bill_length_mm), 2))
print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Biscoe")$bill_length_mm), 2))
print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Dream")$bill_length_mm), 2))


# Plot
penguins_clean <- na.omit(data)
plot(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm, type = "n", xlab = "Bill Length (mm)", ylab = "Bill Depth (mm)", main = "Penguin Bill Dimensions")
points(
  penguins_clean$bill_length_mm[penguins_clean$species == "Adelie"], penguins_clean$bill_depth_mm[penguins_clean$species == "Adelie"],
  col = "red", pch = 16
)
points(penguins_clean$bill_length_mm[penguins_clean$species == "Chinstrap"], penguins_clean$bill_depth_mm[penguins_clean$species == "Chinstrap"], col = "green", pch = 17)
points(penguins_clean$bill_length_mm[penguins_clean$species == "Gentoo"],
  penguins_clean$bill_depth_mm[penguins_clean$species == "Gentoo"],
  col = "blue", pch = 18
)
legend("topright",
  legend = unique(penguins_clean$species),
  col = c(
    "red",
    "green",
    "blue"
  ), pch = c(16, 17, 18)
)


##### Tidyverse est votre ami ####
library(tidyverse)

round(mean(subset(na.omit(data), species == "Adelie")$bill_length_mm), 2)
data %>%
  filter(!is.na(bill_length_mm), species == "Adelie") %>%
  summarise(mean_bill_length = mean(bill_length_mm)) %>%
  pull(mean_bill_length) %>%
  round(2)
# Devoir :
library(dplyr)
library(ggplot2)

library(tidyverse)

# Read data using readr
data <- readr::read_csv("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/data_2.csv")

# Summary
summary(data)

# Calculating mean bill length for different species and islands using dplyr
data %>%
  filter(species == "Adelie") %>%
  group_by(island) %>%
  summarize(mean_bill_length = num(mean(bill_length_mm, na.rm = TRUE), digits = 2))

# Plot using ggplot2
data %>%
  na.omit() %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)) +
  geom_point() +
  labs(x = "Bill Length (mm)", y = "Bill Depth (mm)", title = "Penguin Bill Dimensions") +
  scale_shape_manual(values = c("Adelie" = 16, "Chinstrap" = 17, "Gentoo" = 18))


#### Formatage automatique avec styler #####
# install.packages("styler")

#### Projet autonome ####
# voir en haut du script les modifications apportées.

### Utiliser les projets Rstudio ####
getwd() # pour vérifier le dossier dans lequel R recherche les fichiers.

# Set the working directory
setwd("C:/Users/janninma/Documents/Formations/productive-r-workflow/input/") 
# Ask R to read a file that is located in this directory
data <- read.csv("data.csv")
# ca permet de changer de répertoire facilement - mais dangereux sur le long term, et peut reproductible.
data <- readr::read_csv("input/data.csv")


#### Lire directement à partir d'excel ####
# Load the package (not that it is not part of the core tidyverse!)
library(readxl)

# Read a file
data <- read_excel("input/data.xlsx", na = "NA")
# data <- read_excel("data.xlsx", sheet = "sheetNameOrNumber")
# data <- read_excel("data.xlsx", na = "-")
# data <- read_excel("data.xlsx", col_types = c("date", "skip", "guess", "numeric"))


#### Creer des fonctions ####
calc_mean_bill <- function(island_name) {
  filtered_data <- subset(na.omit(data), species == "Adelie" & island == island_name)
  mean_bill_length <- mean(filtered_data$bill_length_mm)
  return(round(mean_bill_length, 2))
}

# Call the function for each island
calc_mean_bill("Torgersen")
calc_mean_bill("Biscoe")
calc_mean_bill("Dream")

multiplyBy234 <- function(a) {
  return(a * 311)
}
multiplyBy234(234)  

# addition <- function(b) {
#   return(b + 8934)
# }
# addition(3256) # ma version

sum_two_numbers <- function(a, b) {
  return(a + b)
} # correction
sum_two_numbers(a = 3256, b = 8934)

# devoir : 
# plot function
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Define the scatterplot function
create_scatterplot <- function(data, selected_species, selected_island) {
  # Filter the data for the specified species and island
  filtered_data <- data %>%
    na.omit() %>%
    filter(species == selected_species, island == selected_island)
  
  # Create the scatterplot
  plot <- ggplot(
    filtered_data,
    aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)
  ) +
    geom_point() +
    labs(
      x = "Bill Length (mm)",
      y = "Bill Depth (mm)",
      title = paste("Penguin Bill Dimensions -", selected_species, "-", selected_island)
    )
  
  return(plot)
}

# Use the function
create_scatterplot(data, "Adelie", "Torgersen")


#### Diviser votre travail ####
library(tidyverse)
library(readxl)

# Source functions
source(file="R/functions.R")

# Read data using readxl
data <- read_excel("input/data.xlsx", na = "NA")

# Summary
summary(data)

# Calculating mean bill length for different species and islands using dplyr
data %>%
  filter(species == "Adelie") %>%
  group_by(island) %>%
  summarize(mean_bill_length = round(mean(bill_length_mm, na.rm = TRUE), 2))

# Use the function in functions.R
create_scatterplot(data, "Adelie", "Torgersen")


#### Diviser votre travail ####
data <- readRDS(file = "input/clean_data.rds")

# 1er rapport 
library(rmarkdown)

