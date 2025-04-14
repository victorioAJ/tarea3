library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(DescTools)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel(title = div(icon("chart-bar"), "Análisis Inteligente de Datos")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Cargar archivo de datos"),
      fileInput("archivo", "Selecciona un archivo (.csv o .xlsx)", accept = c(".csv", ".xlsx")),
      uiOutput("varSeleccion"),
      actionButton("analizar", "Ejecutar Análisis", icon = icon("play"), class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("📂 Datos Cargados",
                 br(),
                 h5("Primeras filas del archivo:"),
                 tableOutput("vistaPrevia")
        ),
        
        tabPanel("📈 Exploración de Variables",
                 h5("Resumen estadístico:"),
                 tableOutput("estadisticas"),
                 br(),
                 h5("Visualización gráfica:"),
                 plotOutput("grafico")
        ),
        
        tabPanel("🧪 Resultados del Análisis",
                 h5("Tipo de prueba sugerida:"),
                 verbatimTextOutput("sugerencia"),
                 br(),
                 h5("Resultado técnico:"),
                 verbatimTextOutput("resultado"),
                 br(),
                 h5("Interpretación en lenguaje simple:"),
                 textOutput("interpretacion")
        )
      )
    )
  )
)

server <- function(input, output) {
  datos <- reactive({
    req(input$archivo)
    ext <- tools::file_ext(input$archivo$name)
    if (ext == "csv") {
      read.csv(input$archivo$datapath)
    } else if (ext == "xlsx") {
      read_excel(input$archivo$datapath)
    } else {
      validate("Formato no permitido. Sube un .csv o .xlsx")
    }
  })
  
  output$vistaPrevia <- renderTable({
    head(datos(), 10)
  })
  
  output$varSeleccion <- renderUI({
    req(datos())
    selectInput("vars", "Elige variables para analizar (2 o 3):", 
                choices = names(datos()), 
                multiple = TRUE)
  })
  
  output$sugerencia <- renderText({
    req(input$vars)
    v <- input$vars
    if (length(v) == 2) {
      return("🔍 Recomendación: t-test si tienes una variable numérica y una categórica con 2 grupos.")
    } else if (length(v) == 3) {
      return("🔍 Recomendación: ANOVA si hay una variable dependiente numérica y dos independientes categóricas.")
    } else {
      return("Selecciona exactamente 2 o 3 variables.")
    }
  })
  
  output$estadisticas <- renderTable({
    req(input$vars)
    dat <- datos()[, input$vars, drop = FALSE]
    
    tabla <- lapply(dat, function(col) {
      if (is.numeric(col)) {
        data.frame(
          Tipo = "Numérica",
          Media = mean(col, na.rm = TRUE),
          Mediana = median(col, na.rm = TRUE),
          Moda = Mode(col),
          Mínimo = min(col, na.rm = TRUE),
          Máximo = max(col, na.rm = TRUE),
          Rango = max(col, na.rm = TRUE) - min(col, na.rm = TRUE),
          Desv = sd(col, na.rm = TRUE),
          Coef_Var = sd(col, na.rm = TRUE) / mean(col, na.rm = TRUE)
        )
      } else {
        data.frame(
          Tipo = "Categórica",
          Frecuencias = paste(names(table(col)), "=", as.numeric(table(col)), collapse = ", ")
        )
      }
    })
    
    bind_rows(tabla, .id = "Variable")
  })
  
  output$grafico <- renderPlot({
    req(input$vars)
    dat <- datos()
    var <- input$vars[1]
    if (is.numeric(dat[[var]])) {
      ggplot(dat, aes_string(x = var)) +
        geom_histogram(bins = 15, fill = "#2c3e50", color = "white") +
        theme_minimal() +
        labs(title = paste("Distribución de", var),
             x = var, y = "Frecuencia")
    } else {
      ggplot(dat, aes_string(x = var)) +
        geom_bar(fill = "#e67e22") +
        theme_minimal() +
        labs(title = paste("Frecuencia de", var),
             x = var, y = "Conteo")
    }
  })
  
  resultado_analisis <- eventReactive(input$analizar, {
    req(input$vars)
    dat <- datos()
    v <- input$vars
    if (length(v) < 2) return(NULL)
    
    tipos <- sapply(dat[, v], function(x) if (is.numeric(x)) "num" else "cat")
    
    # t-test
    if (length(v) == 2 && all(c("num", "cat") %in% tipos)) {
      dep <- v[which(tipos == "num")]
      indep <- v[which(tipos == "cat")]
      if (length(unique(dat[[indep]])) == 2) {
        return(t.test(as.formula(paste(dep, "~", indep)), data = dat))
      } else {
        return("❗ La variable categórica debe tener solo 2 niveles para aplicar t-test.")
      }
    }
    
    # ANOVA
    if ("num" %in% tipos && sum(tipos == "cat") >= 1) {
      dep <- v[which(tipos == "num")[1]]
      indep <- v[which(tipos == "cat")]
      form <- as.formula(paste(dep, "~", paste(indep, collapse = "+")))
      return(summary(aov(form, data = dat)))
    }
    
    return("❗ No se pudo ejecutar el análisis con las variables seleccionadas.")
  })
  
  output$resultado <- renderPrint({
    res <- resultado_analisis()
    if (is.null(res)) return("Análisis no ejecutado.")
    print(res)
  })
  
  output$interpretacion <- renderText({
    res <- resultado_analisis()
    if (is.null(res) || is.character(res)) return("Aún no se ha generado un resultado interpretable.")
    
    if ("htest" %in% class(res)) {
      p <- res$p.value
      if (p < 0.05) {
        return(paste("✔ Resultado significativo: p =", round(p, 4), "- Se rechaza H₀."))
      } else {
        return(paste("✖ No significativo: p =", round(p, 4), "- No se rechaza H₀."))
      }
    }
    
    if ("anova" %in% class(res)) {
      p <- res[[1]][["Pr(>F)"]][1]
      if (p < 0.05) {
        return(paste("✔ Resultado significativo: p =", round(p, 4), "- Diferencias entre grupos detectadas."))
      } else {
        return(paste("✖ No hay diferencias significativas: p =", round(p, 4)))
      }
    }
    
    return("Resultado no interpretable.")
  })
}

shinyApp(ui, server)
