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
    HTML(r"(
      <p> <b>The Gulf of Maine has seen an increase in jellyfish over recent years.</b> The map below shows a day-today forecast of the likelihood of 
      jellyfish at locations across the Gulf of Maine. Scroll forward to see the prediction for upcoming days.</p>
      
      <p> <b>Have you seen jellyfish?</b> You can add to our dataset by submitting your sightings. You can either enter them at <a href="https://form.jotform.com/240723600886154">jellyfish.bigelow.org</a> 
      or email your sighting directly to <a href="mailto:jellyfish@bigelow.org">jellyfish@bigelow.org </a>.</p>
      
      <p> <b>How the forecast works:</b> the National Marine Fisheries Service in the US has been <a href="https://www.fisheries.noaa.gov/new-england-mid-atlantic/ecosystems/monitoring-ecosystem-northeast"> monitoring plankton for many decades.</a> 
      One of the observations they record is the presence of <a href="https://en.wikipedia.org/wiki/Jellyfish"> jellyfish</a>, <a href="https://en.wikipedia.org/wiki/Salp"> salps</a>, <a href="https://en.wikipedia.org/wiki/Siphonophore"> siphonophores</a>, 
      and <a href="https://en.wikipedia.org/wiki/Ctenophora"> ctenophores</a> (different types of gelatinous animals). 
      We combined these observations with the ocean forecasts provided by the <a href="https://www.copernicus.eu/en"> European Union's Space Program</a>. 
      Using ocean modeling and artificial intelligence, we created a forecast product for jellyfish, salps, and siphonophores.</p>
    )"),
    
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

