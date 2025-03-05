library(shiny)
library(ggplot2) # load ggplot
theme_set(theme_bw())
library(dplyr)
library(macpan2)

# Define server logic required to draw a histogram
function(input, output) {
   
   output$incplot <- renderPlot({
		defaultSpecs <- readRDS("specs.rds")
		newparams <- list(
		  pop_red = input$pop_red[1]
		  , pop_blue = input$pop_blue[1]
		  , pref = input$pref[1]
		  , vaxprop_red = input$vaxprop_red[1]
		  , vaxprop_blue = input$vaxprop_blue[1]
		  , iso_red = input$iso_red[1]
		  , iso_blue = input$iso_blue[1]
		  )
   	specs <- mp_tmb_update(defaultSpecs, default=newparams)


nsims <- input$nsims[1]

time_steps = input$time_steps[1]

outputs <- c("incidence_red", "incidence_blue")

# simulator objects
seir_det = mp_simulator(
    model = specs
  , time_steps = time_steps
  , outputs = outputs
)

seir_stoch = mp_simulator(
    model = mp_euler_multinomial(specs)
  , time_steps = time_steps
  , outputs = outputs
)

## wrapper for stochastic simulations
ssfun <- function(x){
  set.seed(x)
	dd <- (mp_trajectory(seir_stoch)
		%>% mutate(NULL
			, iter = x
		)
	)
}

detf <- (mp_trajectory(seir_det)
	%>% mutate(NULL
		, iter = 0
	)
)

incdf <- (bind_rows(lapply(1:nsims, ssfun), detf)
#	|> mutate(VaccineCoverage_red = newparams$vaxprop_red
#		, VaccineCoverage_blue = newparams$vaxprop_blue
#		, Iso_blue = newparams$iso_red
#		, Iso_blue = newparams$iso_blue
#		, pref = newparams$pref
#	)
)

## Takes in the simulated stochastic simulations and plot it

alpha <- max(1/nsims,0.05)
## over <- 10
## alpha <- something about the number of simulatiosn

simdat <- incdf 

gg <- (ggplot(simdat|>filter(iter!=0), aes(time,value,group=interaction(iter,matrix),color=matrix))
	+ scale_color_manual(values=c("blue","red"))
	+ theme(legend.position = "none")
	+ ylab("")
	+ xlab("Days")
)

if(input$stoch[1]){
  gg <- gg + geom_line(alpha=alpha)
}
if(input$det[1]){
  gg <- gg  + geom_line(data=simdat |> filter(iter==0))
}

print(gg)



   })
}

