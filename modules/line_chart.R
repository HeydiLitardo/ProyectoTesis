import("shiny")
import("ggplot2")
import("glue")
import("dplyr")
import("lubridate")

export("ui")
export("init_server")

ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$div(
      class = "panel-header",
      selectInput(
        ns("chart_type"), "Tipo de gráfico",
        choices = c("Gráfico de Líneas" = "line"),
        selected = "line"
      )
    ),
    tags$div(
      class = "chart-time-container",
      plotOutput(ns("summary_plot"), height = "210px", width = "100%")
    )
  )
}

init_server <- function(id) {
  callModule(server, id)
}

server <- function(input, output, session) {
  data <- reactiveVal(NULL)

  observe({
    req(session$userData$uploaded_data)
    df <- session$userData$uploaded_data()

    # Verificar que el DataFrame contiene las columnas necesarias
    req(all(c("Fecha.y.hora", "Divisa", "Cantidad") %in% colnames(df)))
    data(df)
  })

  selected_data <- reactive({
    df <- data()
    req(df)

    # Convertir la columna Cantidad a numérico, removiendo cualquier carácter no numérico
    df <- df %>%
      mutate(
        Fecha.y.hora = as.POSIXct(Fecha.y.hora, format = "%d/%m/%Y, %I:%M:%S %p", tz = "UTC"),
        Cantidad = as.numeric(gsub("[^0-9.]", "", Cantidad))
      ) %>%
      filter(!is.na(Cantidad), !is.na(Fecha.y.hora))

    df
  })

  output$summary_plot <- renderPlot({
    df <- selected_data()
    req(df)

    ggplot(df, aes(x = Fecha.y.hora, y = Cantidad, color = Divisa, group = Divisa)) +
      # geom_line(size = 1.2, alpha = 0.8) +
      geom_point(size = 2) +
      geom_smooth(method = "loess", se = TRUE, span = 0.2, alpha = 0.2, linewidth = 1.1) +
      theme_minimal() +
      labs(title = "Gráfico de Líneas", x = "Fecha", y = "Cantidad") +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(color = "#f0f0f008"),
        panel.grid.minor = element_line(color = "#f0f0f0")
      )
  })
}
