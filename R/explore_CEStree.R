#' Explore the CES tree of a REMIND run
#'
#' explore_CEStree opens a shiny app in which the ces parameters and values may
#' be explored.
#'
#' @param gdx_filepaths A vector of strings to REMIND gdx files
#'
#' @export
#'
explore_CEStree <- function(gdx_filepaths) {

  my_data <- read_items_from_gdxs(gdx_filepaths,
                                  list(list(name = "pm_cesData"),
                                       list(name = "vm_cesIO", field = "l"),
                                       list(name = "cesOut2cesIn")))


  # Get names of CES parameters
  my_CESparams <- my_data$pm_cesData$cesParameter %>% levels()

  # EDA
  my_data_pm <- my_data$pm_cesData %>%
    tidyr::pivot_wider(names_from = cesParameter) %>%
    mutate("eff*effgr"=eff*effgr, sigma=1/(1-rho)) %>%
    tidyr::pivot_longer(c(tidyselect::all_of(my_CESparams), "eff*effgr", sigma), names_to = "cesParameter")

  # Drop down menus
  my_ins <- my_data_pm$all_in %>% levels()
  my_CESparams <- my_data_pm$cesParameter %>% unique()
  my_runs <- my_data$vm_cesIO$run %>% unique()
  my_countries <- my_data$vm_cesIO$all_regi %>% levels()
  my_periods <- my_data$vm_cesIO$tall %>% unique()
  my_styles <- c("stylized","values")

  # Create shiny app
  ui <-fluidPage(titlePanel("CES Tree Explorer"),
                 tabsetPanel(
                   tabPanel("pm_cesData",
                            wellPanel(fluidRow(column(4,
                                                      selectInput("selectNode1",
                                                                  "Choose CES tree node",
                                                                  choices=my_ins,
                                                                  width = 200)),
                                               column(4,
                                                      selectInput("selectParameter1",
                                                                  "Choose CES parameter",
                                                                  choices=my_CESparams,
                                                                  width = 200))),
                                      plotOutput("plot1", height = "auto"),
                                      downloadButton("db1","Save plot"))),
                   tabPanel("vm_cesIO",
                            wellPanel(selectInput("selectNode2",
                                                  "Choose CES tree node",
                                                  choices=my_ins,
                                                  width = 200),
                                      plotOutput("plot2", height = "auto"),
                                      downloadButton("db2","Save plot"))),
                   tabPanel("sankey(in development)",
                            wellPanel(fluidRow(column(4,
                                                      selectInput("selectRun",
                                                                  "Choose run",
                                                                  choices=my_runs,
                                                                  width = 200)),
                                               column(4,
                                                      selectInput("selectCountry",
                                                                  "Choose country",
                                                                  choices=my_countries,
                                                                  width = 200)),
                                               column(4,
                                                      selectInput("selectPeriod",
                                                                  "Choose year",
                                                                  choices=my_periods,
                                                                  width = 200)),
                                               column(4,
                                                      selectInput("selectStyle",
                                                                  "Choose style",
                                                                  choices=my_styles,
                                                                  width = 200))),
                                      networkD3::sankeyNetworkOutput("plot3", height = "auto")))))




  server <- function(input, output, session){
    output$plot1 <- renderPlot({
      return(plot_ces_parameters(my_data_pm, input$selectNode1, input$selectParameter1))
    }, height = function() {
      session$clientData$output_plot1_width/2
    })

    output$plot2 <- renderPlot({
      return(ggplot(filter(my_data$vm_cesIO,
                           all_in==input$selectNode2)) +
               geom_line(aes(x=tall, y=value, color=run)) +
               geom_vline(aes(xintercept = 2020), linetype = 2) +
               facet_wrap(facets = "all_regi", ncol = 3, scales = "free_y") +
               xlab("Year") +
               ylab(paste0(input$selectNode2)) +
               theme_bw() +
               theme(strip.text.x = element_text(size = 8,
                                                 margin = margin(1,0,1,0))))
    }, height = function() {
      session$clientData$output_plot1_width/2
    })

    output$plot3 <- networkD3::renderSankeyNetwork({
      return(plot_sankey(my_data$cesOut2cesIn, my_data$vm_cesIO,
                         input$selectRun, input$selectCountry, input$selectPeriod, input$selectStyle))
    })

    output$db1 <- downloadHandler(
      filename = "cesTreeExplorerPlot.png",
      content = function(file) { ggsave(file, width = 10, height = 8) }
    )
    output$db2 <- downloadHandler(
      filename = "cesTreeExplorerPlot.png",
      content = function(file) { ggsave(file, width = 10, height = 8) }
    )
  }
  shinyApp(ui,server)
}

#' plot_ces_parameters
#'
#' plot_ces_parameters does this and that
#'
#' @param data A tibble with the pm_cesData for REMIND runs. E.g. the result of
#'   [read_items_from_gdxs()].
#' @param in_all_in A string with the all_in parameter to be plotted.
#' @param in_cesParameter A string with the cesParameter to be plotted.
#' @param run_names A string of REMIND run names, or NULL.
#'
#' @return A ggplot2 object.
#'
plot_ces_parameters <- function(data,
                                in_all_in,
                                in_cesParameter,
                                run_names = NULL) {
  if (!is.null(run_names)) {
    my_params <- data$cesParameter %>% levels()
    plot_data <- data %>%
      tidyr::pivot_wider(names_from = cesParameter) %>%
      mutate("eff*effgr"=eff*effgr, sigma=1/(1-rho)) %>%
      tidyr::pivot_longer(c(tidyselect::all_of(my_params), "eff*effgr", sigma), names_to = "cesParameter")
  } else {
    plot_data <- data
  }

  gg1 <- ggplot(filter(plot_data,
                       all_in==in_all_in,
                       cesParameter==in_cesParameter)) +
    geom_line(aes(x=tall, y=value, color=run)) +
    xlab("Year") +
    ylab(paste0(in_all_in,",   ",in_cesParameter)) +
    facet_wrap(facets = "all_regi", ncol = 3, scales = "free_y") +
    theme_bw() +
    theme(strip.text.x = element_text(size = 8,
                                      margin = margin(1,0,1,0)))

  return(gg1)
}


#' Title
#'
#' @param cesOut2cesIn t
#' @param cesIO t
#' @param my_run t
#' @param my_reg t
#' @param my_period t
#' @param style t
#'
#' @return A sankeyNetwork object
#'
plot_sankey <- function(cesOut2cesIn, cesIO, my_run, my_reg, my_period, style) {

  cesOut2cesIn <- cesOut2cesIn %>%
    filter(run == my_run) %>%
    rename("all_in.1" = value)

  n <- tibble(name = cesOut2cesIn$all_in) %>%
    bind_rows(tibble(name = cesOut2cesIn$all_in.1)) %>%
    distinct()

  l <- cesOut2cesIn %>%
    left_join(cesIO %>%
                filter(run == my_run, all_regi == my_reg, tall == my_period) %>%
                select(all_in.1 = all_in, value) %>%
                mutate(value = if_else(all_in.1 == "kap",value,value*100))) %>%
    select("source"=all_in.1, "target"=all_in, value) %>%
    rowwise() %>%
    mutate(across(.cols = c(1,2), ~which(.x == n$name)-1 %>% as.integer())) %>%
    arrange(source) %>%
    ungroup()

  if (style == "stylized") l <- l %>% mutate(value = 1)

  p <- networkD3::sankeyNetwork(Links =l, Nodes =n, Source = "source",
                                Target = "target", Value = "value", NodeID = "name",
                                units = "-", fontSize = 12, nodeWidth = 30)

  return(p)
}
