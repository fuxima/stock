
# Load packages ----
library(shiny)
library(bslib)
library(quantmod)

# Source helpers ----
#source("helpers.R")
if (!exists(".inflation")) {
  .inflation <- getSymbols('CPIAUCNS', src = 'FRED',
                           auto.assign = FALSE)
}

# adjusts Google finance data with the monthly consumer price index
# values provided by the Federal Reserve of St. Louis
# historical prices are returned in present values
adjust <- function(data) {

  latestcpi <- last(.inflation)[[1]]
  inf.latest <- time(last(.inflation))
  months <- split(data)

  adjust_month <- function(month) {
    date <- substr(min(time(month[1]), inf.latest), 1, 7)
    coredata(month) * latestcpi / .inflation[date][[1]]
  }

  adjs <- lapply(months, adjust_month)
  adj <- do.call("rbind", adjs)
  axts <- xts(adj, order.by = time(data))
  axts[ , 5] <- Vo(data)
  axts
}
#===================================



# User interface ----
ui <- page_sidebar(
  title = "stockVis",
  sidebar = sidebar(
    helpText(
      "Select a stock to examine.

        Information will be collected from Yahoo finance."
    ),
    textInput("symb", "Symbol", "SPY"),
    dateRangeInput(
      "dates",
      "Date range",
      start = "2013-01-01",
      end = as.character(Sys.Date())
    ),
    br(),
    br(),
    checkboxInput(
      "log",
      "Plot y axis on log scale",
      value = FALSE
    ),
    checkboxInput(
      "adjust",
      "Adjust prices for inflation",
      value = FALSE
    )
  ),
  card(
    card_header("Price over time"),
    plotOutput("plot")
  )
)

# Server logic
server <- function(input, output) {

  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })

  data <- reactive({
    if (!input$adjust) return(dataInput())
    adjust(dataInput())})

  output$plot <- renderPlot({
    chartSeries(data(), theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
  })

}

# Run the app
shinyApp(ui, server)
