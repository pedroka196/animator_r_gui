#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(utf8)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  
  # Sidebar with a slider input for number of bins 
  navbarPage("Graficante",
             theme = shinytheme("superhero"),
             tabPanel("Selecao dados",
                      sidebarPanel(fileInput("arquivo","Insira um arquivo",
                                             buttonLabel = "Arquivo CSV ou XLSX",
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv",
                                                        ".xlsx")),
                                   uiOutput("selecao_dados"),
                                   uiOutput("linhas"),
                                   #numericInput("numero_linhas","Numero de colunas",value = 1),
                                   uiOutput("selecionador"),
                                   uiOutput("seleciona_Y"),
                                   uiOutput("seleciona_grupos")),
                      mainPanel(
                        uiOutput("contents"),
                        uiOutput("contents2")
             )),
             tabPanel("Grafico",
                      sidebarPanel(
                        uiOutput("graficos_tipo"),
                        uiOutput("graficos_nome")
                      ),
                      mainPanel(
                        plotOutput("graficos")
                      )),
             tabPanel("Animação",
                      sidebarPanel(
                        uiOutput("tempo_animacao")#,
                        #uiOutput("graficos_nome")
                      ),
                      mainPanel(
                        imageOutput("grafico_animado")
                      ))
             )
))
