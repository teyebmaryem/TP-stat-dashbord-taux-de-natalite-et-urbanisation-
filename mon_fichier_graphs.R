# Chargement des packages nécessaires
library(readxl)
library(ggplot2)
library(tidyr)

# Charger le dataset (assure-toi que don_mls.xlsx est dans le même dossier)
my_data <- read_excel("don_mls.xlsx")
as.data.frame(my_data)
# --------------------
#  Scatter plot

scatter_plot <- ggplot(my_data, aes(x = `Taux d'urbanisation`, y = `Taux de natalité`)) +
  geom_point(color = "darkgreen") +
  labs(title = "Scatter plot : Urbanisation vs Natalité",
       x = "Taux d'urbanisation",
       y = "Taux de natalité") +
  theme_minimal()

# --------------------
# Régression linéaire

my_model <- lm(`Taux de natalité` ~ `Taux d'urbanisation`, data = my_data)

regression_plot <- ggplot(my_data, aes(x = `Taux d'urbanisation`, y = `Taux de natalité`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Régression linéaire",
       x = "Taux d'urbanisation",
       y = "Taux de natalité") +
  theme_minimal()

# --------------------
#Corrélation

correlation_plot <- ggplot(my_data,
                           aes(x = `Taux d'urbanisation`,
                               y = `Taux de natalité`)) +
  geom_point(color = "grey40") +
  geom_smooth(method = "lm", color = "blue", fill = "lightblue") +
  labs(title = "Corrélation : urbanisation vs natalité") +
  theme_minimal()

# --------------------
# Boxplot 


box_data <- my_data |>
  pivot_longer(
    cols = c(`Taux d'urbanisation`, `Taux de natalité`),
    names_to = "variable",
    values_to = "value"
  )

boxplot_both <- ggplot(box_data, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(
    title = "Boxplots : Urbanisation vs Natalité",
    x = "",
    y = "Valeur"
  ) +
  theme_minimal()

