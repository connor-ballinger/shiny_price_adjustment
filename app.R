
library(tidyverse)
library(readxl)
library(readabs)
library(reactable)
library(shiny)
library(bslib)
library(conr)

# Functions --------------------------------------------------------------------
fn_clean_aihw <- function(file, sheet) {
  df <- read_excel(file, sheet = sheet, skip = 1, n_max = 50) |> 
    filter(!if_all(everything(), is.na))
  # truncate table at first footnote
  footnotes <- min(str_which(df$Index, "^[[:punct:]]"))
  df <- slice_head(df, n = (footnotes - 1))
  df |>
    mutate(across(-Index, as.double)) |>
    pivot_longer(cols = -Index, names_to = "Year") |>
    pivot_wider(names_from = Index, values_from = value) |>
    rename_with(~ str_remove(.x, "\\([a-z]+\\)$")) |>
    select(
      Year,
      `Total health price index`,
      `Government final consumption expenditure on hospitals and nursing homes`,
      `Professional health workers wage rates`,
      `PBS pharmaceuticals`,
      `GDP IPD`,
      `GNE IPD`
    ) |>
    arrange(Year) |> 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = ~ last(.x) / .x,
        .names = "{.col} Multiplier"
      )
    ) |> 
    mutate(Year = str_replace(Year, "[:punct:]", "-"))
}
# Theme ------------------------------------------------------------------------
shiny_theme <- bs_theme(
  version = 5, 
  preset = "minty"
  # primary = "rgb(82, 46, 145)",
  # secondary = "rgb(242, 102, 73)"
)
# Data Prep --------------------------------------------------------------------
data_abs_in <- read_abs(series_id = "A2303730T") |>
  select(date, value) |>
  mutate(
    growth = scales::label_percent(accuracy = 0.01)(
      (value - lag(value, n = 4L)) / lag(value, n = 4L)
    ), 
    year = year(date)
  ) |> 
  filter(year > 1999) |> 
  arrange(desc(date))
text_aihw <- paste0(
  "AIHW has a captcha which prevents webscraping - you will need to manually ",
  "upload the relevant data to this page where it can be wrangled for a ",
  "simple output. Find the most recent *Health Expenditure Australia* report ",
  "and click on *Data* and download the *Data tables: Health expenditure ",
  "Australia* item. For example, the 2022-2023 download can be found here: ",
  "https://www.aihw.gov.au/reports/health-welfare-expenditure/health-",
  "expenditure-australia-2022-23/data. ",
  "Then upload the file."
)
# Shiny UI ---------------------------------------------------------------------
ui <- page_navbar( 
  theme = shiny_theme,
  title = "Price Adjustments",
  # ABS Sidebar ----------------------------------------------------------------
  nav_panel(
    title = "Nominal GDP Deflator",
    layout_sidebar(
      sidebar = sidebar(
        title = "Period of Interest",
        width = "25%",
        sliderInput(
          inputId = "year_range",
          label = "Year Range",
          min = 2000,
          max = max(data_abs_in$year), 
          value = c(2010, max(data_abs_in$year)),
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
            max(
              data_abs_in$date
            )
          )
        ),
        downloadButton(
          outputId = "download_abs",
          label = "Download Selected Data"
        ),
        p(
          paste0(
            "ABS GDP figures have been published up to ",
            format(max(data_abs_in$date), "%B, %Y.")
          )
        )
      ),
      card(
        # ABS Table ------------------------------------------------------------
        card(
          card_header("ABS Data"),
          reactableOutput("tbl_abs")
        ),
        # Notes ----------------------------------------------------------------
        card(
          card_header("Notes"),
          card_body(
            p("Series: Gross Domestic Product."),
            p("Series ID: A2303730T."),
            p("Index numbers, seasonally adjusted, quarterly measure."),
            markdown(
              paste0(
                "Australian Bureau of Statistics. (",
                format(max(data_abs_in$date), "%Y, %B"),
                "). *Australian National Accounts: National Income, Expenditure",
                " and Product.*",
                " ABS. ",
                "https://www.abs.gov.au/statistics/economy/national-accounts/",
                "australian-national-accounts-national-income-expenditure-and-",
                "product/latest-release."
              )
            ),
            markdown(
              "Code: https://github.com/connor-ballinger/shiny_price_adjustment."
            )
          )
        )
      )
    )
  ),
  # AIHW Page ------------------------------------------------------------------
  nav_panel(
    title = "AIHW Indices",
    # icon = icon("chart-line"),
    layout_sidebar(
      sidebar = sidebar(
        title = "AIHW Report",
        width = "20%",
        p("Upload the most recent AIHW release.", 
          "The default shown is for 2022-23 (released Nov 2024)."),
        fileInput(
          inputId = "aihw_file",
          label = "AIHW File",
          accept = ".xlsx"
        ),
        textInput(
          inputId = "sheet_name",
          label = "Sheet Name",
          value = "Table 39"
        ),
        downloadButton(
          outputId = "download_aihw",
          label = "Download Selected AIHW Data"
        )
      ),
      # AIHW Table -------------------------------------------------------------
      card(
        card_header("AIHW Notes"),
        markdown(text_aihw),
      ),
      card(
        full_screen = TRUE,
        card_header("AIHW Data"),
        card_body(
          reactableOutput("tbl_aihw")
        )
      )
    )
  )
)
# Shiny Server -----------------------------------------------------------------
server <- function(input, output) {
  ## ABS Data ------------------------------------------------------------------
  data_abs_out <- reactive({
    data_abs_in |>
      filter(
        between(
          year,
          input$year_range[[1]], 
          input$year_range[[2]]
        )
      ) |>
      filter(month(date) == month(max(date))) |> 
      arrange(date) |>
      mutate(multiplier = last(value) / value) |> 
      rename_with(str_to_title)
  })
  ## ABS Output ----------------------------------------------------------------
  output$tbl_abs <- renderReactable({
    data_abs_out() |> 
      wrap_reactable(
        columns = list(Multiplier = colDef(format = colFormat(digits = 4))),
        table_id = NULL,
        downloadable = FALSE,
        pagination = FALSE
      )
  })
  ## ABS Download --------------------------------------------------------------
  download_data_abs <- reactive({
    data_abs_out() |> 
      select(Year, Multiplier)
  })
  output$download_abs <- downloadHandler(
    filename = paste(
      Sys.Date(),
      "price-adjustment.csv",
      sep = "_"
    ),
    content = function(file) {
      write.csv(
        download_data_abs(),
        file
      )
    }
  )
  ## AIHW Data -----------------------------------------------------------------
  data_aihw_out <- reactive({
    if (is.null(input$aihw_file)) {
      df_aihw <- fn_clean_aihw(file = "aihw.xlsx", sheet = input$sheet_name)
    } else {
      df_aihw <- fn_clean_aihw(file = input$aihw_file, sheet = input$sheet_name)
    }
  })
  ## AIHW Output ---------------------------------------------------------------
  output$tbl_aihw <- renderReactable({
    react_aihw <- data_aihw_out()
    cols_numeric <- react_aihw |> select(where(is.numeric)) |> colnames()
    react_aihw |>
      wrap_reactable(
        columns = map(cols_numeric, ~ colDef(
          format = colFormat(digits = 2)
        )) |>
          set_names(cols_numeric),
        defaultColDef = colDef(
          header = function(value) {
            tags$span(value, style = "white-space: normal;")
          }
        ),
        table_id = NULL,
        downloadable = FALSE,
        pagination = FALSE
      )
  })
  ## AIHW Download -------------------------------------------------------------
  download_data_aihw <- reactive({
    data_aihw_out() |> 
      select(Year, matches("Multiplier"))
  })
  output$download_aihw <- downloadHandler(
    filename = paste(
      Sys.Date(),
      "price-adjustment.csv",
      sep = "_"
    ),
    content = function(file) {
      write.csv(
        download_data_aihw(),
        file
      )
    }
  )
}
shinyApp(ui = ui, server = server)
