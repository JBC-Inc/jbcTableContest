#' Server function for the application
#'
#' @param input Internal parameter for `{shiny}`
#' @param output Internal parameter for `{shiny}`
#' @param session Internal parameter for `{shiny}`
#'
app_server <- function(input, output, session) {

  # Input Logics ----

  shinyjs::hide("sl")                    # superlocation map
  shinyjs::hide("reset-app")             # refresh app button
  shinyjs::hide("res-over")              # reservoir override input
  shinyjs::hide("render")                # render wells button
  shinyjs::hide("down")                  # download button
  shinyjs::hide("opt")                   # options button

  shinyhelper::observe_helpers()

  on.exit(waiter::waiter_hide())

  shapefile <- fileInputServer(
    id = "file_input",
    wells = wells,
    texas_counties = texas_counties,
    texas_grids = texas_grids
  )

  sf_override <- reservoirServer(id = "reservoir_override_input", data = shapefile)

  shiny::observe(label = "Data input event", {

    shiny::req(sf_override())
    shinyjs::hide("sls")                 # DIV  startup map
    shinyjs::show("sl")                  # DIV  superlocation map
    shinyjs::hide("file-input")          # DIV  file upload
    shinyjs::hide("load-samp")           # DIV  load samples button
    shinyjs::show("reset-app")           # DIV  reset app button
    shinyjs::show("render")              # DIV  render wells button
  })

  filtered <- choicesServer(
    id = "choices",
    data = sf_override,
    date = input$date
  )

  shiny::observe(label = "Show download & plot options buttons", {
    shinyjs::show("down")
    shinyjs::show("opt")
  }) |>
    shiny::bindEvent(filtered())

  leafletServer(
    id = "superloc",
    data = sf_override,
    filtered = filtered,
    eur = eur,
    texas = texas_state
  )

  # Normalized Lateral Length -------------------------------------------------

  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule("nll", shinyvalidate::sv_numeric())
  iv$enable()

  nll <- shiny::debounce(
    r = shiny::reactive({
      shiny::req(iv$is_valid())
      input$nll
    }),
    millis = 1000
  )

  nll <- shiny::reactive({ input$nll })

  # Datatable -----------------------------------------------------------------

  datatableServer(
    id = "summary",
    filtered = filtered,
    eur = eur,
    nll = nll
  )

  # non-module ----------------------------------------------------------------

  output$startmap <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::setView(lng = -95.3613571, lat = 29.7544409, zoom = 12) |>  # orig =18
      leaflet::addTiles(options = leaflet::tileOptions(maxZoom = 18, minZoom = 6)) |>
      leaflet.extras::addResetMapButton() |>
      leaflet::addCircleMarkers(lng = -95.3613571,
                                lat = 29.7544409,
                                weight = 2,
                                color = 'black',
                                opacity = 1,
                                fillColor = 'chartreuse',
                                fillOpacity = 1,
                                popup = as.character("Headquarters"),
                                label = as.character("Headquarters"))
  })

  shiny::observe(label = "Reload", {
    session$reload()
  }) |>
    shiny::bindEvent(input$`reset_app-reload`)
}


