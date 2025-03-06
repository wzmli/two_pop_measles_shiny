library(shiny)
library(ggplot2) # load ggplot
theme_set(theme_bw(base_size=20))
library(dplyr)
library(tidyr)
library(macpan2)
library(cowplot)

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

stochf <- bind_rows(mp_trajectory_replicate(seir_stoch,n=nsims),.id="iter")

detf <- (mp_trajectory(seir_det)
	%>% mutate(NULL
		, iter = "0"
	)
)

incdf <- (bind_rows(stochf, detf)
#	|> mutate(VaccineCoverage_red = newparams$vaxprop_red
#		, VaccineCoverage_blue = newparams$vaxprop_blue
#		, Iso_blue = newparams$iso_red
#		, Iso_blue = newparams$iso_blue
#		, pref = newparams$pref
#	)
)

## Takes in the simulated stochastic simulations and plot it

alpha <- max((1/nsims)^(2/3),0.05)
## over <- 10
## alpha <- something about the number of simulatiosn

simdat <- (incdf |> mutate(matrix = ifelse(matrix == "incidence_red", "Red","Blue")
           , matrix = factor(matrix,levels=c("Red","Blue"))
          )
)

gg <- (ggplot(simdat|>filter(iter!="0"), aes(time,value,group=interaction(iter,matrix),color=matrix))
	+ scale_color_manual(values=c("red","blue"))
	+ theme(legend.position = "none")
	+ ylab("")
	+ xlab("Days")
	+ ggtitle("Incidence")
)

if(input$stoch[1]){
  gg <- gg + geom_line(alpha=alpha)
}
if(input$det[1]){
  gg <- gg  + geom_line(data=simdat |> filter(iter==0))
}

# print(gg)

fsdf <- (simdat 
  |> filter(iter != "0")
  |> group_by(iter,matrix)
  |> summarise(fs=sum(value))
)

gghist <- (ggplot(fsdf, aes(fs))
      + geom_histogram(position = "identity")
      + facet_wrap(~matrix,nrow=1,scale="free")
      + xlab("Final size")
)

fsdf2 <- (fsdf
  |> ungroup()
  |> group_by(matrix)
  |> mutate(maxfs = max(fs))
  |> ungroup()
)

probdat <- (expand.grid(Infections = 0:max(fsdf$fs))
  |> group_by(Infections)
  |> mutate(Red = mean(Infections>=filter(fsdf2,matrix=="Red")|>pull(fs))
    , Blue = mean(Infections>=filter(fsdf2,matrix=="Blue")|>pull(fs)) 
    )
  |> pivot_longer(-Infections,names_to="matrix",values_to = "Probability")
  |> mutate(matrix = factor(matrix,levels=c("Red","Blue")))
)

ggprob <- (ggplot(probdat, aes(Infections,Probability))
           + geom_line()
           + facet_wrap(~matrix,nrow=1)
           + xlab("Infections")
           + ylab("Probability")
           + ylim(c(0,1))
)



comboplot <- plot_grid(gg,gghist,ggprob,nrow=3)

print(comboplot)

   })
}

