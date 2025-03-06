library(shiny)
library(ggplot2) # load ggplot
theme_set(theme_bw(base_size=20))
library(dplyr)
library(macpan2)

# Define UI for application that draws a histogram
fluidPage(
	# Application title
	titlePanel("Two population Incidence"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
		sidebarPanel(
		  checkboxInput("det"
		                ,"Deterministic"
		                , value = FALSE
		  )
		  ,
		  checkboxInput("stoch"
		                ,"Stochastic"
		                , value = TRUE
		  )
		  ,
		  numericInput("nsims"
		    , "Number of Simulations" 
		    , value = 10 
		    )
		    ,
		  numericInput("time_steps"
		    , "Days" 
		    , value = 365 
		    )
		  ,
		  numericInput("pop_red"
		               , "Pop Red" 
		               , value = 1000 
		  )
		  ,
		  numericInput("pop_blue"
		               , "Pop Blue" 
		               , value = 1000 
		  )
		  ,
		  sliderInput("pref"
		    ,"Within Group Mixing"
		    , min = 0
		    , max = 1
		    , value=0.99
		  )
		  ,
      	sliderInput("vaxprop_red"
         	,"Vaccine Coverage of RED"
      		, min = 0
      		, max = 1
      		, value=0.5
      		)
      	,
      	sliderInput("vaxprop_blue"
      	            ,"Vaccine Coverage of BLUE"
      	            , min = 0
      	            , max = 1
      	            , value=0.5
      	),
		  sliderInput("iso_red"
		    ,"Infection Isolation of RED"
		    , min = 0
		    , max = 1
		    , value=0.5
		  )
		  ,
		  sliderInput("iso_blue"
		    ,"Infection Isolation of BLUE"
		    , min = 0
		    , max = 1
		    , value=0.5
		  )
		)
	, mainPanel(
         plotOutput("incplot",height = 950)

		
      )
   )
)

