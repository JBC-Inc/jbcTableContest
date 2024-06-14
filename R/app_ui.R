#' The application user interface
#'
#' @param request Internal parameter for `{shiny}`
#'
app_ui <- function(request) {
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "2024 Table Contest"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(id = "sidebarmenu",
        shinydashboard::menuItem(tabName = "main","Shiny Dashboard",icon = shiny::icon("map"))
      ),
      shiny::tags$div(id = "load-samp", fileInputUI(id = "file_input")$load_sample),
      shiny::tags$div(id = "render", choicesUI(id = "choices")$render),
      shiny::tags$br(),
      shiny::numericInput(
        inputId = "nll",
        label = "Normalized Lateral Length",
        value = 10000,
        min = 1,
        max = 30000
      )
    ),
    shinydashboard::dashboardBody(       #---- dashboardBody ----

      shiny::includeCSS("./inst/app/www/custom.css"),
      shiny::includeScript("./inst/app/www/mapresize.js"),

      shinyjs::useShinyjs(),

      waiter::useWaiter(),
      waiter::waiter_show_on_load(html = shiny::tagList(
        waiter::spin_loaders(12),
        shiny::HTML("<br/><br/><h4>Initializing...</h4>")
      )),

      shinydashboard::tabItems(
        shinydashboard::tabItem(
        tabName = "main",

        shiny::fluidRow(
          shiny::tags$div(
            class = "output-container",
            shiny::tags$div(
              id = "leaflet-busy",
              class = "map-loading",
              shiny::tags$p("...loading...")
            ),
            shiny::tags$div(
              id = "sl",
              shinycssloaders::withSpinner(ui_element = leafletUI(id = "superloc", height = "50vh")$leaf)
            ),
            shiny::tags$div(
              id = "sls",
              shinycssloaders::withSpinner(
                ui_element = leaflet::leafletOutput(outputId = "startmap", height = "50vh")
              )
            )
          ),
          shiny::tags$br(),
          shiny::tags$div(id = "fa-container", datatableUI("summary")$filter_active),
          shiny::tags$br(),
          shiny::tags$div(class = "output-container", datatableUI(id = "summary")$dt),
          shiny::tags$div(id = "fa-break", shiny::tags$br())

        )
      ))
    )
  )
}
