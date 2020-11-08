library("shiny")
library("shinyMatrix")
library("random")
library("formattable")
library("assertthat")


house_sizes <-
  a("here", href = "https://gov.wales/annual-population-survey-july-2019-june-2020")
infection_rates <-
  a("here", href = "https://doi.org/10.1101/2020.08.19.20177188")

#default data
initial_prob_house_size <- c(0.22, 0.28, 0.28, 0.10, 0.04)
mean_probs <- c(0.49, 0.41, 0.32, 0.25, 0.25)
SD <-  c(0.36, 0.38, 0.35, 0.28, 0.13)
prev_rates <- c(0, 0.150, 0.100, 0.050, 0.015, 0.005)

#initialise input matrix
m <-
  matrix(, 3, 5, dimnames = list(
    c(
      "Probability of household size",
      "Probability of mean infection",
      "SD of mean infection probability"
    ),
    c(
      "Household size: 2",
      "Household size: 3",
      "Household size: 4",
      "Household size: 5",
      "Household size: 6+"
    )
  ))

m[1, ] <- initial_prob_house_size
m[2, ] <- mean_probs
m[3, ] <- SD


ui <- fluidPage(
  titlePanel("Secondary infections from returning students"),
  
  
  column(12, wellPanel(
    h5(
      "This application uses a Monte-Carlo process to simulate the expected number of possible secondary infections due to students returning to their non-term time households. The Monte-Carlo process is simulated 1000 times to allow us to estimate the mean and 95% confidence intervals. The stochastic simulations are dependent on statistics for household sizes within Wales (see",
      house_sizes,
      ") and the secondary infection rate, which varies with household size (data based on UK wide cases, see",
      infection_rates,
      "). To allow for revised data, or to use the simulator for other regions, these input data can be modified by clicking the chosen value."
    ),
  )),
  
  
  
  #input data panel
  column(12, wellPanel(
    h4(
      "Input data (click on a value to edit, the numbers can be reset using the buttons at the bottom)"
    ),
    
    matrixInput(
      "Data_input_table",
      value = m,
      rows = list(names = TRUE),
      cols = list(names = TRUE),
      class = 'numeric'
    ),
    
    h4(textOutput("Data_warning"))
    
  )),
  
  
  column(4, wellPanel(
    h4("Simulation parameters"),
    
    sliderInput(
      "Num_of_students",
      "Number of students, N:",
      min = 0,
      max = 25000,
      value = 1000,
      step = 500
    ),
    
    
    sliderInput(
      "Man_prev",
      "Percentage prevalence, I (%):",
      min = 0,
      max = 100,
      value = 20,
      step = 1
    )
    
  )),
  
  
  #results table panel
  column(
    8,
    align = "center",
    h3("Simulation results"),
    formattableOutput("Results_table"),
    
    
    #simulation button panel
    wellPanel(
      h4("Simulate secondary infections"),
      
      actionButton("init_sim_but", "Run"),
      
      actionButton("reset_but", "Reset to default data"),
      
      actionButton("reset_but_paras", "Reset to default parameters")
    )
  ),
  
  
  
  
  
)

server <- function(input, output, session) {
  #initialise the output table
  Results <-
    matrix(, length(prev_rates), 4, dimnames = list(
      NULL,
      c(
        "Prevalence (%)",
        "No. of secondary infections",
        "Confidence interval lower",
        "Confidence interval upper"
      )
    ))
  output$Results_table <-
    renderFormattable(formattable(as.data.frame(Results), align = c("c", "c", "c", "c")))
  
  
  
  observeEvent(input$reset_but, {
    #reset the data matrix to the default values
    m <-
      matrix(, 3, 5, dimnames = list(
        c(
          "Probability of household size",
          "Probability of mean infection",
          "SD of mean infection probability"
        ),
        c(
          "Household size: 2",
          "Household size: 3",
          "Household size: 4",
          "Household size: 5",
          "Household size: 6+"
        )
      ))
    m[1, ] <- initial_prob_house_size
    m[2, ] <- mean_probs
    m[3, ] <- SD
    updateMatrixInput(session, "Data_input_table", value = m)
    
    #clear the results table
    Results <-
      matrix(, length(prev_rates), 4, dimnames = list(
        NULL,
        c(
          "Prevalence (%)",
          "No. of secondary infections",
          "Confidence interval lower",
          "Confidence interval upper"
        )
      ))
    output$Results_table <-
      renderFormattable(formattable(as.data.frame(Results), align = c("c", "c", "c", "c")))
    output$Data_warning <- renderText(invisible(NULL))
    
    
  })
  
  
  observeEvent(input$reset_but_paras, {
    #reset the sliders
    updateSliderInput(session, "Num_of_students", value = 1000)
    updateSliderInput(session, "Man_prev", value = 20)
    
    Results <-
      matrix(, length(prev_rates), 4, dimnames = list(
        NULL,
        c(
          "Prevalence (%)",
          "No. of secondary infections",
          "Confidence interval lower",
          "Confidence interval upper"
        )
      ))
    output$Results_table <-
      renderFormattable(formattable(as.data.frame(Results), align = c("c", "c", "c", "c")))
    
    
    
  })
  
  observeEvent(input$init_sim_but, {
    #validate the input data
    def_data <- matrix(, 3, 5)
    def_data[1, ] <- initial_prob_house_size
    def_data[2, ] <- mean_probs
    def_data[3, ] <- SD
    
    test_data <- input$Data_input_table
    new_data <-
      matrix(, 3, 5, dimnames = list(
        c(
          "Probability of household size",
          "Probability of mean infection",
          "SD of mean infection probability"
        ),
        c(
          "Household size: 2",
          "Household size: 3",
          "Household size: 4",
          "Household size: 5",
          "Household size: 6+"
        )
      ))
    
    
    for (i in 1:3) {
      for (j in 1:5) {
        temp <- test_data[i, j]
        if (is.na(temp) ||
            as.numeric(temp) < 0 || as.numeric(temp) > 1) {
          temp <- def_data[i, j]
          output$Data_warning <-
            renderText("WARNING: INVALID INPUT DATA - ALL INPUT DATA VALUES MUST BE IN [0,1]")
          
        }
        
        new_data[i, j] <- temp
        
      }
    }
    
    if (identical(test_data, new_data)) {
      output$Data_warning <- renderText(invisible(NULL))
    }
    
    updateMatrixInput(session, "Data_input_table", value = new_data)
    
    
    #get user defined values
    NS <- 1000
    N <- as.numeric(input$Num_of_students)
    Hhsizes <- as.numeric(new_data[1, ])
    Mean_prob_from_t <- as.numeric(new_data[2, ])
    Sd <- as.numeric(new_data[3, ])
    prev_rates[1] <- as.numeric(input$Man_prev) * 0.01
    
    #initiate progress bar
    progress <-
      Progress$new(session, min = 0, max = (length(prev_rates) * NS))
    on.exit(progress$close())
    progress$set(message = 'Simulation in progress')
    count <- 0
    
    
    #rescale the probability boundaries to 1 for housesizes
    Hhsizes <- cumsum(Hhsizes / sum(Hhsizes))
    Hhsizes <- append(0, Hhsizes)
    
    #initiate outputs of simulation
    mean_of_sim <- matrix(, 1, length(prev_rates))
    sd_of_sim <- matrix(, 1, length(prev_rates))
    
    #Monte-Carlo algorithm for secondary infections from each student for each prevalence value
    for (l in 1:length(prev_rates)) {
      rate <- prev_rates[l]
      Noit <- vector()
      
      for (j in 1:NS) {
        p1 <- array(runif(N), c(1, N)) < rate
        
        p2 <- matrix(, (length(Hhsizes) - 1), N)
        p3 <- matrix(, (length(Hhsizes) - 1), N)
        
        r <- runif(N)
        
        for (i in 1:(length(Hhsizes) - 1)) {
          p2[i, ] <- i * (Hhsizes[i] <= r) * (r < Hhsizes[i + 1])
          p3[i, ] <-
            pmin(rep(1, N), abs(Mean_prob_from_t[i] + Sd[i] * rnorm(N)))
        }
        
        
        p2p3 <- p2 * p3
        p2p3 <- colSums(p2p3)
        Noi <- p1 * p2p3
        Noit <- append(Noit, rowSums(Noi))
        
        count <- count + 1
        progress$set(value = count)
      }
      
      mean_of_sim[l] <- round(mean(Noit), digits = 0)
      sd_of_sim[l] <- round(sd(Noit), digits = 1)
    }
    
    #output the results to the data table
    Results <-
      matrix(, length(prev_rates), 4, dimnames = list(
        NULL,
        c(
          "Prevalence (%)",
          "No. of secondary infections",
          "Confidence interval lower",
          "Confidence interval upper"
        )
      ))
    
    Results[, 1] <- round(prev_rates * 100, digits = 1)
    Results[, 2] <- mean_of_sim
    Results[, 3] <-
      round(pmax(0, mean_of_sim - 1.96 * sd_of_sim), digits = 1)
    Results[, 4] <-
      round(mean_of_sim + 1.96 * sd_of_sim, digits = 1)
    
    output$Results_table <-
      renderFormattable(formattable(
        as.data.frame(Results),
        align = c("c", "c", "c", "c"),
        list(
          area(col = 2:4) ~ color_tile("transparent", "pink"),
          area(col = 1, row = 1) ~ color_tile("lightblue", "lightblue")
        )
      ))
    
    
  })
  
  
  
}

shinyApp(ui, server)