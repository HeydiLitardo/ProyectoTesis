import("shiny")
import("dygraphs")
import("glue")
import("dplyr")
import("lubridate")
import("xts")

export("ui")
export("init_server")

ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$div(
      class = "panel-header",
      selectInput(
        ns("chart_type"), "Tipo de gráfico",
        choices = c("Linea" = "line"),
        selected = "bar"
      )
    ),
    tags$div(
      class = "chart-time-container",
      dygraphOutput(ns("dygraph"), height = "210px", width = "100%")
    )
  )
}

init_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactiveVal(NULL)
    
    observe({
      req(session$userData$uploaded_data)
      df <- session$userData$uploaded_data()
      
      # Verificar que el DataFrame contiene las columnas necesarias
      req(all(c("Fecha.y.hora", "Estado", "Cantidad") %in% colnames(df)))
      data(df)
    })
    
    output$dygraph <- renderDygraph({
      req(data())
      
      df <- data()
      
      if (nrow(df) == 0) {
        showNotification("No data available for the selected grouping column.", type = "error")
        return(NULL)
      }
      
      # Convertir los datos a objeto xts agrupado por día
      df <- df %>%
        mutate(
          Fecha.y.hora = floor_date(as.POSIXct(Fecha.y.hora, format = "%d/%m/%Y, %I:%M:%S %p", tz = "UTC"), "day"),
          Cantidad = as.numeric(gsub("[^0-9.]", "", Cantidad))
        ) %>%
        filter(!is.na(Fecha.y.hora), !is.na(Estado), !is.na(Cantidad)) %>%
        group_by(Fecha.y.hora, Estado) %>%
        summarize(Cantidad = sum(Cantidad, na.rm = TRUE)) %>%
        ungroup()
      
      # Convertir los datos a objeto xts para cada grupo
      xts_data_list <- df %>%
        split(.$Estado) %>%
        lapply(function(sub_df) {
          if (nrow(sub_df) > 0) {
            xts(sub_df$Cantidad, order.by = sub_df$Fecha.y.hora)
          } else {
            return(NULL)
          }
        })
      
      # Filtrar los NULL de la lista
      xts_data_list <- Filter(Negate(is.null), xts_data_list)
      
      # Combinar todas las series xts
      if (length(xts_data_list) == 0) {
        showNotification("No data available after processing.", type = "error")
        return(NULL)
      }
      
      combined_xts_data <- do.call(cbind, xts_data_list)
      
      # Asignar nombres a las series
      colnames(combined_xts_data) <- names(xts_data_list)
      
      dygraph(combined_xts_data) %>%
        dyOptions(
          drawPoints = TRUE,
          pointSize = 2,
          strokeWidth = 3, 
          axisLineColor = "#585858",
          gridLineColor = "#bdc2c6",
          axisLabelFontSize = 12,
          axisLabelColor = "#585858",
          disableZoom = TRUE,
          stepPlot = FALSE,  
          drawGapEdgePoints = TRUE,
          connectSeparatedPoints = TRUE, 
          title = "Cantidad por Estado",
        ) %>%
        dyAxis("x", label = "Fecha")
    })
  })
}
