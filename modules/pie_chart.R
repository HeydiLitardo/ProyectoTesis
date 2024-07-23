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
        choices = c("Barra" = "bar", "Pastel" = "pie"),
        selected = "bar"
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
    req(all(c("Estado", "Cantidad") %in% colnames(df)))
    data(df)
  })
  
  selected_data <- reactive({
    df <- data()
    req(df)
    
    # Convertir la columna Cantidad a numérico, removiendo cualquier carácter no numérico
    df <- df %>%
      mutate(Cantidad = as.numeric(gsub("[^0-9.]", "", Cantidad))) %>%
      filter(!is.na(Cantidad))
    
    df %>%
      group_by(Estado) %>%
      summarise(Cantidad = sum(Cantidad, na.rm = TRUE)) %>%
      ungroup()
  })
  
  output$summary_plot <- renderPlot({
    df <- selected_data()
    req(df)
    
    if (input$chart_type == "bar") {
      ggplot(df, aes(x = Estado, y = Cantidad, fill = Estado)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = "Cantidad por Estado", x = "Estado", y = "Cantidad") +
        theme(
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"),
        )
      
    } else if (input$chart_type == "pie") {
      ggplot(df, aes(x = "", y = Cantidad, fill = Estado)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        theme_void() +
        labs(title = "Cantidad por Estado")
        
    }
  })
}
