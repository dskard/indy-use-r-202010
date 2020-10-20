library(shiny)
library(ggplot2)
library(plotly)
library(tibble)

library(dplyr)
library(pins)
library(lubridate)

results_name <- "spirograph_results"
results_board <- "indy-use-r-202010-data" 
results_board_repo <- "dskard/indy-use-r-202010-data" 
results_version <- "096da9c"

# register the board where we store pinned spirograph data
board_register_github(
  name = results_board,
  repo = results_board_repo
)

# retrieve the pinned spirograph data
pinned_spirograph_data <- pin_get(
  name = results_name,
  board = results_board,
  version = results_version
)

spiro <- function(n1,n2,n3) {
  t <- seq(0,1,length.out=1000)
  z <- exp(1i*2*pi*n1*t) + exp(1i*2*pi*n2*t) + exp(1i*2*pi*n3*t)
  result <- tibble(x=Re(z),y=Im(z))
  return (result)
}

save_result <- function(n1, n2, n3, result) {
  new_spirograph_data <- tribble(
    ~date, ~n1, ~n2, ~n3, ~x, ~y,
     now(), n1,  n2,  n3, result$x, result$y
  )
  
  all_spirograph_data <- bind_rows(pinned_spirograph_data, new_spirograph_data)
  
  pin(
    all_spirograph_data,
    name=results_name,
    description="saved results from spirograph simulations",
    board=results_board
  )
}

# Define UI for application that draws a spirograph
ui <- fluidPage(

  # Application title
  titlePanel("Spirograph"),

  sidebarLayout(

    sidebarPanel(
      # input sliders
      sliderInput(inputId="n1",label="n1",value=13,min=-10,max=20,step=1),
      sliderInput(inputId="n2",label="n2",value=-7,min=-10,max=20,step=1),
      sliderInput(inputId="n3",label="n3",value=-3,min=-10,max=20,step=1)
    ),

    mainPanel(
      # output plot
      plotlyOutput("spirograph"),
      textOutput("cached")
    )
  )
)

# Define server logic required to draw a spirograph
server <- function(input, output) {
  # setup a variable "cached" that is reactive,
  # when it changes, it will trigger other reactive expressions to be updated
  values <- reactiveValues(
    cached = "No"
  )
  
  output$spirograph <- renderPlotly({
    
    # check for previously saved results for these inputs
    result <- pinned_spirograph_data %>%
      filter(n1 == input$n1, n2 == input$n2, n3 == input$n3)
    
    if(nrow(result) == 0) {
      # no results found for these inputs
      # run the simulation
      result <- spiro(input$n1,input$n2,input$n3)
      values$cached <- "No"
    } else {
      result <- result %>%
        select(x,y)  %>%
        tidyr::unnest(c(x, y))
      values$cached <- "Yes"
    }
    
    # plot the spirograph
    ggplot(data=result,aes(x=x,y=y)) +
        geom_path() +
        xlab("Real(z)") +
        ylab("Imag(z)")
  })
  
  # tell the user if the result was previously calculated
  output$cached <- renderText({ sprintf("Cached Result: %s", values$cached) })
}

# Run the application
shinyApp(ui,server)
