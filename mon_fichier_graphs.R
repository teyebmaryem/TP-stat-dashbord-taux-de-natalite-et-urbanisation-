# Chargement des packages nécessaires
library(readxl)
library(ggplot2)
library(tidyr)

# Charger le dataset (assure-toi que don_mls.xlsx est dans le même dossier)
my_data <- read_excel("don_mls.xlsx")
as.data.frame(my_data)
# --------------------
# 1) Scatter plot simple
# --------------------
scatter_plot <- ggplot(my_data, aes(x = `Taux d'urbanisation`, y = `Taux de natalité`)) +
  geom_point(color = "darkgreen") +
  labs(title = "Scatter plot : Urbanisation vs Natalité",
       x = "Taux d'urbanisation",
       y = "Taux de natalité") +
  theme_minimal()

# --------------------
# 2) Régression linéaire
# --------------------
my_model <- lm(`Taux de natalité` ~ `Taux d'urbanisation`, data = my_data)

regression_plot <- ggplot(my_data, aes(x = `Taux d'urbanisation`, y = `Taux de natalité`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Régression linéaire",
       x = "Taux d'urbanisation",
       y = "Taux de natalité") +
  theme_minimal()

# --------------------
# 3) Corrélation (scatter + lm line)
# --------------------
correlation_plot <- ggplot(my_data,
                           aes(x = `Taux d'urbanisation`,
                               y = `Taux de natalité`)) +
  geom_point(color = "grey40") +
  geom_smooth(method = "lm", color = "blue", fill = "lightblue") +
  labs(title = "Corrélation : urbanisation vs natalité") +
  theme_minimal()

# --------------------
# 4) Boxplot (2 variables)
# --------------------
# boxplot_urban <- ggplot(my_data, aes(y = `Taux d'urbanisation`)) +
#   geom_boxplot(fill = "orange") +
#   labs(title = "Boxplot du taux d'urbanisation") +
#   theme_minimal()
# 
# boxplot_nat <- ggplot(my_data, aes(y = `Taux de natalité`)) +
#   geom_boxplot(fill = "purple") +
#   labs(title = "Boxplot du taux de natalité") +
#   theme_minimal()
# 
# 
# 
# boxplot_both <- ggplot(my_data, aes(x = c(`Taux d'urbanisation`,`Taux de natalité`))) +
#   geom_boxplot() +
#   labs(title = "Boxplots dans un seul graphique") +
#   theme_minimal()


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

