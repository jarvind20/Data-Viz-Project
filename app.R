library(shiny)
library(dplyr)
library(ggplot2)

# Load the data
presidents_data <- read.csv("presidents.csv")

# UI
ui <- fluidPage(
  titlePanel("U.S. Presidents Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("president", "Select President:", choices = presidents_data$Name)
    ),
    mainPanel(
      h3(textOutput("selected_president")),
      tableOutput("president_info"),
      plotOutput("term_chart")
    )
  )
)

# Server
server <- function(input, output) {
  output$selected_president <- renderText({
    input$president
  })
  
  output$president_info <- renderTable({
    presidents_data %>%
      filter(Name == input$president) %>%
      select(Birthplace, Birthday, Religion, Party, Term, Life, VP)
  })
  
  output$term_chart <- renderPlot({
    presidents_data %>%
      mutate(Term_start = as.numeric(substr(Term, 1, 4))) %>%
      ggplot(aes(x = Term_start, y = Name)) +
      geom_point() +
      geom_segment(aes(xend = Term_start + 4, yend = Name)) +
      theme_minimal() +
      labs(title = "Presidential Terms", x = "Year", y = "President")
  })
}

# Run the app
shinyApp(ui = ui, server = server)
