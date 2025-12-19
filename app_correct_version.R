library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(ggplot2)
library(tidyr)

# Importation les graphiques et Charger le dataset
source("mon_fichier_graphs.R")

df <- read_excel("don_mls.xlsx")

colnames(df) <- make.names(colnames(df))

#--------- HEADER ---------
header <- dashboardHeader(
  titleWidth = 200,
  title = "Barre des tâches"
)

#--------- SIDEBAR ---------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Accueil", tabName = "home", icon = icon("home")),
    menuItem("Data", tabName = "dataset", icon = icon("table")),
    menuItem("Graphiques", tabName = "graphs", icon = icon("chart-line")),
    menuItem("Prediction", tabName = "prediction", icon = icon("chart-bar")),
    menuItem("Correlation", tabName = "correlation", icon = icon("project-diagram"))
  )
)

#--------- BODY ---------
bodyy <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "home",
            h2("Bienvenue dans le dashboard !"),
            p("On va étudier le taux d’urbanisation et le taux de natalité.")
    ),
    
    tabItem(tabName = "dataset",
            h2("Visualisation du Dataset"),
            DTOutput("table_data")
    ),
    
    tabItem(tabName = "graphs",
            h2("Graphiques"),
            plotOutput("plot1"),
            plotOutput("plot6")
    ),
    
    tabItem(tabName = "prediction",
            h2("Régression Linéaire"),
            plotOutput("plot2"),
            plotOutput("plot3")
    ),
    
    tabItem(tabName = "correlation",
            h2("Calcul du taux de natalité"),
            fluidRow(
              box(
                title = "Entrer le taux d'urbanisation",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                numericInput(
                  "inputTU",
                  "Valeur d'urbanisation :",
                  value = 19, min = 0, max = 100
                )
              )
            ),
            fluidRow(
              box(
                title = "Résultat",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                verbatimTextOutput("resultTNAT")
              )
            )
    )
  )
)

#--------- UI ---------
ui <- dashboardPage(
  skin = "green",
  header = header,
  sidebar = sidebar,
  body = bodyy
)

#--------- SERVER ---------
server <- function(input, output) {
  
  # Modèle linéaire
  model <- lm(
    Taux.de.natalité ~ Taux.d.urbanisation,
    data = df
  )
  
  # Prédiction réactive
  predicted_TNAT <- reactive({
    req(input$inputTU)
    
    predict(
      model,
      newdata = data.frame(
        Taux.d.urbanisation = input$inputTU
      )
    )
  })
  
  # Table
  output$table_data <- renderDT({
    df
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Graphiques
  output$plot1 <- renderPlot({ scatter_plot })
  output$plot2 <- renderPlot({ regression_plot })
  output$plot3 <- renderPlot({ correlation_plot })
  output$plot6 <- renderPlot({ boxplot_both })
  
  # Texte résultat
  output$resultTNAT <- renderText({
    paste0(
      "Pour un taux d'urbanisation = ",
      input$inputTU, "% :\n",
      "➡ Taux de natalité prédit = ",
      round(predicted_TNAT(), 3)
    )
  })
}

shinyApp(ui, server)
