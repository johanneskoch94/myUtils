#' Explore the CES tree of REMIND
#'
#' Opens a shiny app in which the CES parameters and values may be explored.
#'
#' @param gdx_filepaths A vector of strings to REMIND gdx files
#' @export
explore_CEStree <- function(gdx_filepaths) {

  rlang::check_installed(c("shiny", "ggplot2", "networkD3"))

  my_data <- read_items_from_gdxs(gdx_filepaths, c("pm_cesdata", "vm_cesIO", "cesOut2cesIn"))

  # Get names of CES parameters
  my_CESparams <- my_data$pm_cesdata$cesParameter %>% unique()

  # Add some custom parameters, like elasticity. Convert time dimension to numeric.
  my_data_pm <- my_data$pm_cesdata %>%
    tidyr::pivot_wider(names_from = "cesParameter") %>%
    dplyr::mutate("eff*effgr" = .data$eff * .data$effgr,
                  sigma = 1 / (1 - .data$rho),
                  tall = as.numeric(.data$tall)) %>%
    tidyr::pivot_longer(c(tidyselect::all_of(my_CESparams), "eff*effgr", "sigma"), names_to = "cesParameter")
  my_data$vm_cesIO <- dplyr::mutate(my_data$vm_cesIO, tall = as.numeric(.data$tall))

  # Drop down menus
  my_ins <- my_data_pm$all_in %>% unique()
  my_CESparams <- my_data_pm$cesParameter %>% unique()
  my_runs <- my_data$vm_cesIO$run %>% unique()
  my_countries <- my_data$vm_cesIO$all_regi %>% unique()
  my_periods <- my_data$vm_cesIO$tall %>% unique()
  my_styles <- c("stylized", "stylized_rev", "values", "values_rev")

  # Create shiny app
  ui <- shiny::fluidPage(
    shiny::titlePanel("CES Tree Explorer"),
    shiny::tabsetPanel(
      shiny::tabPanel("pm_cesdata", shiny::wellPanel(shiny::fluidRow(
        shiny::column(4, shiny::selectInput(
          "selectNode1", "Choose CES tree node", choices = my_ins, width = 200
        )),
        shiny::column(4, shiny::selectInput(
          "selectParameter1", "Choose CES parameter", choices = my_CESparams, width = 200
        ))),
        shiny::plotOutput("plot1", height = "auto"),
        shiny::downloadButton("db1", "Save plot")
      )),
      shiny::tabPanel("vm_cesIO", shiny::wellPanel(
        shiny::selectInput("selectNode2", "Choose CES tree node", choices = my_ins, width = 200),
        shiny::plotOutput("plot2", height = "auto"),
        shiny::downloadButton("db2", "Save plot"))),
      shiny::tabPanel("sankey(in development)", shiny::wellPanel(shiny::fluidRow(
        shiny::column(3, shiny::selectInput("selectRun", "Choose run", choices = my_runs, width = 400)),
        shiny::column(3, shiny::selectInput("selectCountry", "Choose country", choices = my_countries, width = 110)),
        shiny::column(2, shiny::selectInput("selectPeriod", "Choose year", choices = my_periods, width = 100)),
        shiny::column(2, shiny::selectInput("selectStyle", "Choose style", choices = my_styles, width = 100))),
        networkD3::sankeyNetworkOutput("plot3", height = "auto")))))

  server <- function(input, output, session) {
    output$plot1 <- shiny::renderPlot({
      return(plot_ces_parameters(my_data_pm, input$selectNode1, input$selectParameter1))
    }, height = function() {
      session$clientData$output_plot1_width / 2
    })

    output$plot2 <- shiny::renderPlot({
      return(ggplot2::ggplot(dplyr::filter(my_data$vm_cesIO, .data$all_in == input$selectNode2)) +
               ggplot2::geom_line(ggplot2::aes(x = .data$tall, y = .data$value, color = .data$run)) +
               ggplot2::geom_vline(ggplot2::aes(xintercept = 2020), linetype = 2) +
               ggplot2::facet_wrap(facets = "all_regi", ncol = 3, scales = "free_y") +
               ggplot2::xlab("Year") +
               ggplot2::ylab(paste0(input$selectNode2)) +
               ggplot2::theme_bw() +
               ggplot2::theme(strip.text.x = ggplot2::element_text(size = 8,
                                                                   margin = ggplot2::margin(1, 0, 1, 0))))
    }, height = function() {
      session$clientData$output_plot1_width / 2
    })

    output$plot3 <- networkD3::renderSankeyNetwork({
      return(plot_sankey(my_data$cesOut2cesIn,
                         my_data$vm_cesIO,
                         input$selectRun,
                         input$selectCountry,
                         input$selectPeriod,
                         input$selectStyle))
    })

    output$db <- shiny::downloadHandler(
      filename = "cesTreeExplorerPlot.png",
      content = function(file) ggplot2::ggsave(file, width = 10, height = 8)
    )
    output$db2 <- shiny::downloadHandler(
      filename = "cesTreeExplorerPlot.png",
      content = function(file) ggplot2::ggsave(file, width = 10, height = 8)
    )
  }
  shiny::shinyApp(ui, server)
}

plot_ces_parameters <- function(data, in_all_in, in_cesParameter) {
  ggplot2::ggplot(dplyr::filter(data, .data$all_in == in_all_in, .data$cesParameter == in_cesParameter)) +
    ggplot2::geom_line(ggplot2::aes(x = .data$tall, y = .data$value, color = .data$run)) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(paste0(in_all_in, ",   ", in_cesParameter)) +
    ggplot2::facet_wrap(facets = "all_regi", ncol = 3, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text.x = ggplot2::element_text(size = 8, margin = ggplot2::margin(1, 0, 1, 0)))
}

plot_sankey <- function(cesOut2cesIn, cesIO, my_run, my_reg, my_period, style) {

  cesOut2cesIn <- cesOut2cesIn %>%
    dplyr::filter(.data$run == my_run) %>%
    dplyr::select(-"run") %>%
    dplyr::rename("target" = "all_in", "source" = "all_in_1")
  n <- tibble::tibble(name = unique(c(cesOut2cesIn$source, cesOut2cesIn$target)))

  l <- cesOut2cesIn %>%
    dplyr::left_join(cesIO %>%
                       dplyr::filter(.data$run == my_run, .data$all_regi == my_reg, .data$tall == my_period) %>%
                       dplyr:: select("source" = "all_in", "value") %>%
                       # scale up everything except for capital!
                       dplyr:: mutate(value = dplyr::if_else(.data$source == "kap", .data$value, .data$value * 100)),
                     by = dplyr::join_by(source)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(.cols = c(1, 2), ~which(.x == n$name) - 1 %>% as.integer())) %>%
    dplyr::arrange(source) %>%
    dplyr::ungroup()

  if (grepl("stylized", style)) l <- dplyr::mutate(l, value = 1)
  if (grepl("_rev", style)) l <- dplyr::rename(l, "target" = "source", "source" = "target")

  networkD3::sankeyNetwork(Links = as.data.frame(l),
                           Nodes = as.data.frame(n),
                           Source = "source",
                           Target = "target",
                           Value = "value",
                           NodeID = "name",
                           units = "",
                           fontSize = 12,
                           nodeWidth = 30)
}
