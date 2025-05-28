library(shiny)
library(ggplot2) # load ggplot
theme_set(theme_bw(base_size=20))
library(dplyr)
library(tidyr)
library(macpan2)
library(cowplot)

get_newparams = function(input) {
  list(
        pop_red = input$pop_red[1]
      , pop_blue = input$pop_blue[1]
      , pref = input$pref[1]
      , eff = input$eff[1]
      , beta = input$Rzero[1] * spec$default[["gamma"]]
      , vaxprop_red = input$vaxprop_red[1]
      , vaxprop_blue = input$vaxprop_blue[1]
      , iso_red = input$iso_red[1]
      , iso_blue = input$iso_blue[1]
    )
}

# Define server logic required to draw a histogram
function(input, output) {
  simulators <- reactiveValues(seir_det = NULL, seir_stoch = NULL)
  observeEvent(list(input$time_steps), {
    message("Recomputing simulators ...")
    start = Sys.time()
    newparams <- get_newparams(input)
    spec <- mp_read_rds("specs.rds") |> mp_tmb_update(default = newparams)
    outputs <- c("incidence_red", "incidence_blue")
    simulators$seir_det <- mp_tmb_calibrator(spec
      , time = mp_sim_bounds(1, input$time_steps, "steps")
      , par = names(newparams)
      , outputs = outputs
    )
    simulators$seir_stoch <- mp_tmb_calibrator(mp_euler_multinomial(spec)
      , time = mp_sim_bounds(1, input$time_steps, "steps")
      , par = names(newparams)
      , outputs = outputs
    )
    end = Sys.time()
    message("Took: ", end - start, " seconds")
  })
  
  output$incplot <- renderPlot({
    message("Generating simulations ...")
    start = Sys.time()
    newparams <- get_newparams(input)
    nsims <- input$nsims[1]
    time_steps = input$time_steps[1]
    
    stochf = mp_trajectory_replicate(simulators$seir_stoch
      , n = 10
      , parameter_updates = newparams
    )
    detf = (simulators$seir_det
      |> mp_trajectory()
      |> mutate(NULL, iter = "0")
    )
    stochf = (simulators$seir_stoch
      |> mp_trajectory_replicate(n = nsims)
      |> bind_rows(.id = "iter")
    )
    incdf <- bind_rows(stochf, detf)
    end = Sys.time()
    message("Took: ", end - start, " seconds")
    ## Takes in the simulated stochastic simulations and plot it
    
    message("Plotting simulations ...")
    start = Sys.time()
    alpha <- max((1/nsims)^(2/3),0.05)
    ## over <- 10
    ## alpha <- something about the number of simulations
    
    simdat <- (incdf 
      |> mutate(
          matrix = ifelse(matrix == "incidence_red", "Red","Blue")
        , matrix = factor(matrix, levels=c("Red","Blue"))
      )
    )
    
    gg <- (simdat
      |> filter(iter != "0")
      |> ggplot(
          aes(time, value, group = interaction(iter, matrix), color = matrix)
      )
      + scale_color_manual(values = c("red", "blue"))
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
    
    
    countdf <- (fsdf
      |> ungroup()
      |> group_by(matrix,fs)
      |> summarise(count = n())
    )
    
    
    gghist <- (ggplot(countdf, aes(fs,count))
       #      + geom_histogram(position = "identity")
       + geom_point()
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
    
    ggprob <- (ggplot(probdat, aes(Infections, Probability))
       + geom_line()
       + facet_wrap(~matrix, nrow = 1)
       + xlab("Infections")
       + ylab("Probability")
       + ylim(c(0,1))
    )
    
    
    
    comboplot <- plot_grid(gg, gghist, ggprob, nrow = 3)
    
    print(comboplot)
    end = Sys.time()
    message("Took: ", end - start, " seconds")
    
  })
}
