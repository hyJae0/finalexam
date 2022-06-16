library(shiny)
library(DT)
library(magrittr)
library(palmerpenguins)
library(dplyr)
library(ggplot2)
library(bslib)
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "superhero"),
  # Application title
  titlePanel(h1("펭귄 데이터 분석")),
  tags$h1(tags$style(".titlePanel{ 
                         color: green;
                         font-size: 20px;
                         font-style: italic;
                         }")),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "spes",
        label = "펭귄 종류를 선택하세요", 
        choices = list("Adelie", "Gentoo", "Chinstrap"),
        selected = "Adelie")
      ,
      selectInput("select1", label = h4("x축을 선택하세요"), 
                  choices = list("bill_length_mm", "bill_depth_mm", "flipper_length_mm",
                                 "body_mass_g"), 
                  selected ="bill_length_mm")
      ,
      selectInput("select2", label = h4("y축을 선택하세요"), 
                  choices = list("bill_length_mm", "bill_depth_mm", "flipper_length_mm",
                                 "body_mass_g"), 
                  selected ="body_mass_g")
      ,
      sliderInput("bins",
                  "점 크기를 선택하세요",
                  min = 1,
                  max = 10,
                  value = 5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput('dynamic'),
      plotOutput('penguins_plot')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  spenguins = reactive ({
    penguins %>% filter(species %in% input$spes)
  })
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$dynamic = renderDataTable ({
    spenguins() %>% datatable()
  })
  output$penguins_plot = renderPlot({
    
    spenguins() %>%
      ggplot(aes_string(x = input$select1, y = input$select2, color = "species", shape = "sex"))+
      geom_point(size = as.numeric(input$bins)) + theme_bw()
    
  })  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
