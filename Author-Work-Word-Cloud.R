library(shiny)
library(tidyverse)
library(stringr)
library(tidytext)

#UI
ui <- fluidPage(
  
  titlePanel("Author's Work Word Cloud"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file", label = "Upload CSV"), accept = ".csv"),
#NEED TO ADD INSTRUCTIONS
      mainPanel(tableOutput(outputId = "table"),
        plotOutput(outputId = "plot"))
    )
  )

#Server
server <- function(input, output) {
#FIND A WAY TO DISPLAY PLOT BEFORE THE TABLE OR SEPERATE INTO TWO PAGES  
  output$plot <- renderPlot({
    inputFile <- input$file
    csv <- read.csv(inputFile$datapath)
    
    clean_dat <- csv %>% 
      select(Title) %>% 
      unnest_tokens(output = word, input = Title) %>%
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