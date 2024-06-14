
#' Leaflet UI
#'
#' @param id An ID string that corresponds with the ID used in the modules server function.
#' @param height Height of the map in units view height vh.
#'
#' @return `leafletUI()` returns a leaflet UI element.
#'
leafletUI <- function(id, height = "50vh") {

  ns <- shiny::NS(id)

  list(
    leaf = leaflet::leafletOutput(outputId = ns("superloc"), height = height)

    # shape_label = shiny::selectInput(inputId = ns("shape_label"), label = "Shape Label", choices = NULL)
  )
}

#' Leaflet Server
#'
#' Two operations, initial leaflet generated is from the calculated superlocation data from file inputs.
#' Subsequent map is generated from the filtered choices dataset as a proxy update.
#'
#' @param id An ID string that corresponds with the ID used to call the modules UI functions.
#' @param data processed SL dataset.
#' @param filtered filtered subset of the SL dataset.
#' @param eur usfsrpmsc EUR data.
#' @param texas [sf] containing Texas state polygons.
#
#  plugin_path: file path containing files needed to run the grouped control layers leaflet plugin.
#'
#' @return `leafletServer` returns a leaflet object or leaflet proxy object.
#'
leafletServer <- function(id, data, filtered, eur, texas) { # , plugin_path) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    ids <- shiny::reactiveVal()

    overlay_groups <- shiny::reactiveVal()

    shiny::observe(label = "Assign overlay groups", {

      if(data()$multi) {
        ids(data()$shape_ids)
        overlay_groups(c("Counties", "Grids", stringr::str_sort(ids())))
      } else {
        overlay_groups(c("AOIs", "Counties", "Grids", "Super Location"))
      }
    }) |>
      shiny::bindEvent(data())

    shiny::observe(label = "Update leaflet", {

      # Include only the widest area region -----------------------------------
      # Points & trajectories for the map (prevent duplicate rendering)
      # When multiple polygons are present, there is no radius to filter by.

      if (filtered()$multi) {
        filtered_final <- filtered()$fill
      } else {
        filtered_final <- filtered()$fill |> dplyr::filter(aoi_region == "25 Miles")
      }

      # top 10 (count) reservoirs when applicable -----------------------------

      if (length(unique(filtered_final$reservoir)) > 10) {
        filtered_final <- filtered_final |>
          dplyr::mutate(reservoir = forcats::fct_lump_n(reservoir, n = 10, ties.method = "max"))
      }

      # add EUR data to wells -------------------------------------------------

      filtered_final <- dplyr::left_join(filtered_final, eur,
                                         by = c("entity_id" = "pden_id"),
                                         multiple = "all")

      res_wells <- split(filtered_final, f = filtered_final$reservoir)

      # palette for baseGroup layer (reservoirs) ------------------------------

      res_names <- names(res_wells)

      pal <- leaflet::colorFactor(palette = viridis::viridis_pal(option = "H")(length(res_names)),
                                  levels = forcats::fct_infreq(res_names),
                                  reverse = FALSE)

      leaf <- leaflet::leafletProxy(mapId = shiny::NS(id, id), session = session)

      all_reservoirs <- data.frame(res = unique(data()$well_layers$reservoir)) |>
        dplyr::filter(!res %in% overlay_groups())

      leaf |>
        leaflet::clearGroup(group = c(all_reservoirs$res, "Other", "All")) |>
        leaflet::hideGroup( group = c(all_reservoirs$res, "Other", "All"))

      # each individual reservoir layer ---------------------------------------

      leaf |> addCircleLayers(df_list = res_wells, palette = pal)

      # 'all' reservoirs layers -----------------------------------------------

      if(length(unique(filtered_final$reservoir)) > 1) {

        leaf |>
          leaflet::addCircleMarkers(data = filtered_final,
                                    lng = filtered_final$lng,
                                    lat = filtered_final$lat,

                                    weight = 1,                     # border
                                    color = 'black',
                                    opacity = 1,

                                    fillColor = ~pal(reservoir),   # filling
                                    fillOpacity = 0.7,

                                    radius = 4,

                                    group = "All",

                                    popup = mapLabels(filtered_final),

                                    label = mapLabels(filtered_final),
                                    labelOptions = leaflet::labelOptions(
                                      style = list("font-weight" = "normal",
                                                   "font-size" = "14px",
                                                   "direction" = "auto",
                                                   "text-align" = "left")
                                      ),
                                    popupOptions = leaflet::popupOptions(closeOnClick = FALSE,
                                                                         autoClose = FALSE),
                                    options = leaflet::pathOptions(pane = "markerPane")) |>
          leaflet::addPolylines(data = filtered_final$traj,
                                weight = 0.7,
                                color = 'gray',
                                group = "All")

        res_names <- append(res_names, "All")
      }

      leaf |>
        leaflet::addLayersControl(
          overlayGroups = overlay_groups(),
          baseGroups = res_names,
          options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex = TRUE),
          position = "topright"
          )

      if (length(unique(filtered_final$reservoir)) > 1) {

        leaf |> leaflet::showGroup("All")

      } else {

        leaf |> leaflet::showGroup(head(names(res_wells), 1))
      }
    }) |>
      shiny::bindEvent(filtered())

    output$superloc <- leaflet::renderLeaflet({

      shiny::req(overlay_groups())

      leaf <-
        leaflet::leaflet(options = leaflet::leafletOptions(worldCopyJump = TRUE,
                                                           preferCanvas = TRUE,
                                                           zoomSnap = 0.1
                                                           )
                         ) |>

        leaflet::addScaleBar(position = "bottomleft") |>
        leaflet.extras::addResetMapButton() |>
        leaflet.extras::addFullscreenControl() |>

        leaflet::setView(lng = data()$center_lng,
                         lat = data()$center_lat,
                         zoom = data()$zoom)

      # conditional -----------------------------------------------------------

      if (data()$multi) {

        for(i in 1:length(ids())) {
          leaf <- leaf |>
            leaflet::addPolygons(data = data()$shapefile[i,],
                                 fill = TRUE,              # blue
                                 weight = 0.5,
                                 fillOpacity = 0.1,
                                 group = paste0(ids()[i]),
                                 options = leaflet::pathOptions(pane = "overlayPane"))
        }

        centers <- data.frame(sf::st_coordinates(sf::st_centroid(data()$shapefile$geometry)))
        centers$id <- ids()

        leaf <- leaf |>
          leaflet::addLabelOnlyMarkers(data = centers,
                                       lng = ~X,
                                       lat = ~Y,
                                       label = ~id,
                                       labelOptions = leaflet::labelOptions(noHide = TRUE,
                                                                            direction = 'top',
                                                                            textOnly = TRUE))
      } else {

        leaf <- leaf |>
          leaflet::addPolygons(data = data()$shapefile,
                               fill = TRUE,                # blue
                               weight = 1,
                               fillOpacity = 0.42,
                               group = 'Super Location',
                               options = leaflet::pathOptions(pane = "overlayPane")) |>

          leaflet::addPolygons(data = data()$aoi_polys,
                               fill = FALSE,
                               weight = 2,
                               opacity = 0.5,
                               # color = 'cyan',
                               group = 'AOIs',
                               options = leaflet::pathOptions(pane = "overlayPane"))
      }

      # standard --------------------------------------------------------------

      leaf <- leaf |>

        leaflet::addPolygons(data = texas,
                             fill = FALSE,
                             weight = 2,
                             opacity = 1,
                             color = 'black',
                             options = leaflet::pathOptions(pane = "overlayPane")) |>

        leaflet::addPolygons(data = data()$counties,
                             fill = FALSE,
                             weight = 3,
                             # opacity = 0.7,
                             color = 'black',
                             group = 'Counties',
                             options = leaflet::pathOptions(pane = "overlayPane")) |>

        leaflet::addPolygons(data = data()$grids,
                             fill = FALSE,
                             weight = 0.2,
                             opacity = 0.5,
                             color = 'black',
                             group = "Grids",
                             options = leaflet::pathOptions(pane = "overlayPane")) |>

        leaflet::addEasyButton(
          leaflet::easyButton(position = "topleft",
                              title = "Fly to Super Location",
                              icon = shiny::span(class = "star", shiny::HTML("&starf;")),
                              onClick = htmlwidgets::JS(
                                "function(btn, map) {
                                  var sl_layer = map.layerManager.getLayerGroup('Super Location');
                                  map.flyTo(sl_layer.getBounds().getCenter(), 14);}"))) |>
        leaflet::addMeasure(position = "topleft") |>

        # leaflet::hideGroup("Grids") |>

        registerPlugin(ctrlGrouped("./inst/app/www")) |>

        htmlwidgets::onRender(jsCode = readr::read_file("./inst/app/www/map_loading.js")) |>
        htmlwidgets::onRender(jsCode = readr::read_file("./inst/app/www/map_layers.js"))
    })
  })
}
