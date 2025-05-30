library(shiny)
library(ggplot2) # load ggplot
theme_set(theme_bw(base_size=20))
library(dplyr)
library(tidyr)
library(macpan2)
library(cowplot)

get_newparams = function(input, spec) {
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
  
  models <- reactiveValues(seir_det_simulator = NULL, seir_stoch_simulator = NULL, seir_spec = NULL)
  observeEvent(list(input$time_steps), {
    withProgress(message = 'Loading and preparing model ...', value = 0, style = "old", {
      start = Sys.time()
      
      read_fn = if (packageVersion("macpan2") >= "2.2.1") mp_read_rds else readRDS
      spec <- read_fn("specs.rds")
      newparams <- get_newparams(input, spec)
      models$seir_spec = mp_tmb_update(spec, default = newparams)
      outputs <- c("incidence_red", "incidence_blue")
      models$seir_det_simulator <- mp_tmb_calibrator(models$seir_spec
         , time = mp_sim_bounds(1, input$time_steps, "steps")
         , par = names(newparams)
         , outputs = outputs
      )
      models$seir_stoch_simulator <- mp_tmb_calibrator(mp_euler_multinomial(models$seir_spec)
         , time = mp_sim_bounds(1, input$time_steps, "steps")
         , par = names(newparams)
         , outputs = outputs
      )
      trash = mp_trajectory(models$seir_det_simulator)
      trash = mp_trajectory(models$seir_stoch_simulator)
      end = Sys.time()
      showNotification(sprintf("Took %.2f seconds to load and prepare model", end - start))
    })
  })
  
  output$incplot <- renderPlot({
    withProgress(message = 'Generating simulations ...', value = 0, style = "old", {
      start = Sys.time()
      newparams <- get_newparams(input, models$seir_spec)
      
      nsims <- input$nsims[1]
      time_steps = input$time_steps[1]
      
      detf = (models$seir_det_simulator
              |> mp_trajectory_par(parameter_updates = newparams)
              |> mutate(NULL, iter = "0")
      )
      stochf = (models$seir_stoch_simulator
                |> mp_trajectory_replicate(n = nsims, parameter_updates = newparams)
                |> bind_rows(.id = "iter")
      )
      incdf <- bind_rows(stochf, detf)
      end = Sys.time()
      showNotification(sprintf("Took %.2f seconds to generate simulations", end - start))
    })
    
    withProgress(message = 'Plotting simulations ...', value = 0, style = "old", {
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
      showNotification(sprintf("Took %.2f seconds to plot simulations", end - start))
    })
  })
}
