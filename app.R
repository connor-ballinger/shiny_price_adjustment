

library(tidyverse)
library(readabs)
library(DT)
library(shiny)

series <- read_abs(series_id = "A2303730T") |>
  select(date, value) |>
  mutate(
    growth = scales::label_percent(accuracy = 0.01)(
      (value - lag(value, n = 4L)) / lag(value, n = 4L)
    ), 
    year = year(date)
  ) |> 
  filter(year > 1999)


ui <- fluidPage(

    titlePanel("Price Adjustments"),
    
    fluidRow(
      
      column(
        
        width = 6,
        offset = 0.2,
        h2("Notes"),
        p("Series: Gross Domestic Product."),
        p("Series ID = A2303730T."),
        p("Index numbers, seasonally adjusted, quarterly measure."),
        p("Packages: tidyverse, readabs, DT, shiny.")
        
      ), 
      
    ),
    
    fluidRow(

      sidebarLayout(
        
        sidebarPanel(
          
          sliderInput(
            inputId = "year_range",
            label = "Year Range",
            min = 2000,
            max = max(series$year), 
            value = c(2010, max(series$year)),
            step = 1,
            sep = ""
          ),
          
          selectInput(
            inputId = "quarter",
            label = "Quarter",
            choices = list(
              "March" = 1, 
              "June" = 2,
              "September" = 3, 
              "December" = 4
              ),
            selected = quarter(
              last(
                series$date
              )
            )
          ),
          
          p(
            paste0(
              "ABS GDP figures have been published up to ",
              format(
                last(
                  series$date
                  ),
                "%B, %Y."
                )
              )
            ),
          
          downloadButton(
            outputId = "download",
            label = "Download Selected Data"
          )
          
        ),
        
        mainPanel(
          width = 6,
          DTOutput("tbl")
        )
      )
    )
)


server <- function(input, output) {

  subseries <- reactive({
    
    series |>
      filter(
        between(
          year,
          input$year_range[[1]], 
          input$year_range[[2]]
        )
      ) |>
      filter(month(date) == month(max(date))) |> 
      mutate(multiplier = last(value) / value)
    
  })
  
  output$tbl <- DT::renderDT({

    subseries() |> 
      datatable() |> 
      formatRound(columns = "multiplier", digits = 3)
    
  })
  
  download_data <- reactive({
    
    subseries() |> 
      select("year", "multiplier")
    
  })
  
  output$download <- downloadHandler(
    
    filename = paste(
      Sys.Date(),
      "price-adjustment.csv",
      sep = "_"
    ),
    
    content = function(file) {
      write.csv(
        download_data(),
        file
      )
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
