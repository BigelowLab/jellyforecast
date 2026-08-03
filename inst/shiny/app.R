suppressPackageStartupMessages({
  library(jellyforecast)
  library(shiny)
  library(bslib)
  library(bigelowshinytheme)
  library(dplyr)
})

# bigelowshinytheme::copy_www(dest = "inst/shiny")

PRODS = jellyforecast::read_products()
ix = PRODS$name == "coel"
if (any(ix)) {
  PRODS$longname[ix] <- PRODS$cfg[[ix]]$longname <- "Jellyfish"
}

# here we assume that all forecasts cover the same dates
DATES = PRODS$daily[[1]] |> names() |> as.Date()
DATE = Sys.Date()
ORIGIN = as.Date("1970-01-01")
WIDTH = "90%"
HEIGHT = "80%"

##### UI ######

ui <- shiny::fluidPage(
  theme = bigelowshinytheme::bigelow_theme(),
  includeCSS("www/additionalStyles.css"),
  
  # Header
  bigelowshinytheme::bigelow_header(
    h2("Jellyfish Forecast"), 
    h6("Gulf of Maine")),
  
  # Main content
  bigelowshinytheme::bigelow_main_body(
    # Introduction
    p("Jellyfish forecast for the likely appearance of a patch of high abundance using Coperncius data layers"),
    br(),
    
    bigelowshinytheme::bigelow_card(
      headerContent = "Patch Likelihood", 
      footerContent = NULL, 
      fluidRow(
        selectInput("longname", 
                    label = "Species",
                    choices = PRODS$longname,
                    selected = PRODS$longname[1]),
        sliderInput("dateSlider",
                    label = "Date",
                    min = min(DATES), max = max(DATES),
                    value = DATE)
        ), # fluid row
      imageOutput("imageOutput",
                  width = WIDTH,
                  height = HEIGHT) ,
    bigelowshinytheme::bigelow_footer("Data courtesy of Copernicus Marine Data Store and Ecomon")
    ) # card
  ) # main_body
) #fluidPage

server <- function(input, output, session) {
  
  species_name = reactive({
    input$longname
  })
  
  date_string = reactive({
    as.Date(input$dateSlider, origin = ORIGIN) |> 
      format(format = "%Y-%m-%d")
  })
  output$imageOutput <- renderImage({
    
    date =  date_string()
    
    src = (PRODS |>
      dplyr::filter(longname == species_name()))$daily[[1]]
    
    list(src = src[date],
         width  = WIDTH,
         height = HEIGHT,
         alt = paste("Date", date))
  }, delete = FALSE)
  
}

# Applying ggplot styling and render app
# bigelowshinytheme::bigelow_style_plots()
shinyApp(ui, server)

