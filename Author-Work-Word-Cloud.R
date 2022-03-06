library(shiny)
library(tidyverse)
library(stringr)
library(tidytext)

#UI
ui <- fluidPage(
  
  titlePanel("Author's Work Word Cloud"),
  
  sidebarLayout(
    sidebarPanel(
      h5("Please upload CSV file containing data frame of publication titles. Then, provide the column name which contain the data of interest"),
      fileInput(inputId = "file", label = "Upload CSV", accept = ".csv"),
      textInput(inputId = "name", label = "Type column name containing publication titles", "Title")),

    fluidPage(tabsetPanel(
      tabPanel("Word Count Plot", plotOutput(outputId = "plot")),
      tabPanel("Data Table", mainPanel(tableOutput(outputId = "table"))),
      )
    )
  )
)

#Server
server <- function(input, output) {
  #FIND A WAY TO DISPLAY PLOT BEFORE THE TABLE OR SEPERATE INTO TWO PAGES  
  output$plot <- renderPlot({
    inputFile <- input$file
    csv <- read.csv(inputFile$datapath)
    
    clean_dat <- csv %>% 
      select(input$name) %>% 
      unnest_tokens(output = word, input = input$name) %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE)
    
    top10_clean_dat <- clean_dat[1:10,] 
    
    plot <- top10_clean_dat %>%
      mutate(word = reorder(word,n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      coord_flip() +
      labs(x = "Words \n", y = "\n Counts", title = "Frequent Words on Author's Publications Title \n") +
      geom_text(aes(label = n), hjust = 1.5, color = "white", fontface = "bold") +
      theme_grey()
    plot
  })
  
  output$table <- renderTable({
    if(is.null(input$file)){paste("No CSV File Uploaded")}
    else {
      inputFile <- input$file
      read.csv(inputFile$datapath)}
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)