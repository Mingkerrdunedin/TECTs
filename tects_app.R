# Shiny app for TECTs data

library(tidyverse)
library(shiny)
library(DT)
library(tibble)
library(dplyr)
library(lubridate)
library(scales)
library(ggplot2)
library(RColorBrewer)

tects <- read_csv("tects.csv")

colnames(tects) <- c("Date", "RTO", "Product", "Spend($M)")

tects$`Spend($M)` <- round(tects$`Spend($M)`, digits = 2)
tects$Date <- dmy(tects$Date)
tects$Date <- floor_date(tects$Date, "month")

rto <- unique(tects$RTO)
product <- unique(tects$Product)

min_date <- min(tects$Date)-1
max_date <- max(tects$Date)

tects <- tects %>%
  mutate(Month = month(Date, label = TRUE)) %>%
  mutate(Year = as.integer(year(Date)))


ui <- fluidPage(
  br(),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = 'x',
        label = "RTO",
        choices = rto,
        selected = "Enterprise Dunedin"
      ),
      dateRangeInput(
        inputId = "daterange",
        label = "Select date range:",
        start = "2018-03-31",
        end = "2021-06-01",
        min = min_date,
        max = max_date,
        startview = "year"
      ),
      checkboxGroupInput(
        inputId = "product_sel",
        label = "Select product(s):",
        choices = product,
        selected = product
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "barchart"),
      dataTableOutput(outputId = "spendtable"),
      br()
    )
  ))

server <- function(input, output, session) {
  output$barchart <- renderPlot({
    tects %>%
      filter(RTO == input$x) %>%
      filter(Product == input$product_sel) %>% 
      filter(Date >= as.POSIXct(input$daterange[1]) &
               Date <= as.POSIXct(input$daterange[2])) %>%
      ggplot(aes(x = Date, y = `Spend($M)`, fill = Product)) +
      geom_col() +
      scale_x_date(
        labels = date_format("%b-%y"),
        date_breaks = "3 months",
        expand = c(0, 0)
      ) +
      xlab("Month/Year")+
      ylab("Spend ($M)") +
      scale_fill_brewer(palette = "Dark2") +
      theme(text = element_text(size = 18),
            axis.text = element_text(size = 14),
            axis.title.x = element_text(size = 14, vjust = -1),
            axis.title.y = element_text(size = 14, vjust = 2),
            legend.key.size = unit(1, "cm")) +
      ggtitle(paste("Domestic visitor spending in", input$x))
  })
  
  output$spendtable <- renderDataTable({
    from_selected_rto <- tects %>%
      filter(RTO == input$x) %>%
      filter(Date >= as.POSIXct(input$daterange[1]) &
               Date <= as.POSIXct(input$daterange[2])) %>%
      select(Month, Year, Product, `Spend($M)`)
    DT::datatable(
      data = from_selected_rto,
      options = list(pageLength = 8,
                     columnDefs = list(
                       list(className = "dt-left", targets = "_all")
                     )),
      filter = list(position = "top", clear = TRUE),
      rownames = FALSE,
      class = "cell-border stripe",
      caption = "Table: Monthly domestic visitor spending on all products by chosen RTO."
    )
    
  })
}

shinyApp(ui, server)
