library(shiny)
library(ggplot2) # load ggplot
theme_set(theme_bw(base_size=20))
library(dplyr)
library(macpan2)

fluidPage(
  titlePanel("Two Population Measles Model"),
  
  fluidRow(
    column(6,
      fluidRow(
        column(6,
          h3("Model Settings"),
          checkboxInput("det", "Deterministic", value = FALSE),
          checkboxInput("stoch", "Stochastic", value = TRUE),
          numericInput("nsims", "Number of Simulations", value = 10),
          sliderInput("Rzero", "Reproduction Number", min = 0, max = 18, value = 12)
        ),
        column(6,
          br(),
          sliderInput("A0", "Awareness", min = 0.001, max = 0.2, value = 0.1),
          sliderInput("pref", "Within Group Mixing", min = 0, max = 1, value = 0.99),
          sliderInput("eff", "Vaccine Effectiveness", min = 0, max = 1, value = 0.95)
        )
      ),
      fluidRow(
        column(6,
          h3("Red Controls"),
          numericInput("pop_red", "Pop Red", value = 1000),
          sliderInput("vaxprop_red", "Vaccine Coverage of RED", min = 0, max = 1, value = 0.5),
          sliderInput("iso_red", "Infection Isolation of RED", min = 0, max = 1, value = 0.5)
        ),
        column(6,
          h3("Blue Controls"),
          numericInput("pop_blue", "Pop Blue", value = 1000),
          sliderInput("vaxprop_blue", "Vaccine Coverage of BLUE", min = 0, max = 1, value = 0.5),
          sliderInput("iso_blue", "Infection Isolation of BLUE", min = 0, max = 1, value = 0.5)
        )
      )
    ),
    
    column(6,
      plotOutput("incplot", height = "1000px")
    )
  )
)
