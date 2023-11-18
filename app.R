library(shiny)
library(tidyverse)
library(plotly)
library(shinythemes)
library(lubridate)
library(janitor)
library(leaflet)
library(rgdal)
library(fresh)
library(bslib)
library(shinyWidgets)


# APP MAPA ENTRE RIOS
# Carga de mapa
shape <- rgdal::readOGR(dsn = "pxdptodatosok.shp", verbose = FALSE)
shapeData <- sp::spTransform(shape, CRS("+proj=longlat +datum=WGS84 +no_defs"))
shapeData <- sf::st_as_sf(shapeData, wkt = 'polygons', crs = st_crs(4326)) # make sf
shapeData <- rmapshaper::ms_simplify(shapeData, keep = 0.01, keep_shapes = TRUE) %>% 
  filter(provincia == "Entre Ríos")

# Bases
agro <- read_delim("data/Estimaciones_agro.csv", delim = ";")
publico <- read_delim("data/publicos-total.txt", delim = "\t")
privado <- read_delim("data/privados-sub.txt")
ratio <- read_delim("data/ratio.txt")

#### ui #####
ui <- navbarPage("Mapa Interactivo de información económica de Entre Ríos", 
                 
                 theme = bs_theme(
                   version = 5, bootswatch = "flatly"
                 ),
                 
                 
                 # theme = bs_theme(bg = "gray",
                 #                  fg = "black",
                 #                  primary = "maroon",
                 #                  base_font = font_google("Montserrat")),
                 
            tabPanel("Mapa",
              sidebarLayout(
                sidebarPanel(
                  h4(strong("Panel de selección")),
                  
                  sliderInput("anio1", 
                              label = h3("Periodo"), 
                              min = 2015,
                              max = 2022,
                              value = 2018),     
                  
                  
                  
                  conditionalPanel("input.indicador == 12",
                                   pickerInput(inputId = "rama",
                                               label = "Rama productiva: ",
                                               choices = c("A - Agric. Ganad..." = "A",
                                                           "B - Explot. Minas..." = "B",
                                                           "C - Ind. Manufact..." = "C",
                                                           "D - Elect. Gas..." = "D",
                                                           "E - Agua, cloacas..." = "E",
                                                           "F - Construcción" = "F",
                                                           "G - Comercio..." = "G", 
                                                           "H - Transporte..." = "H",
                                                           "I - Alojamiento y..." = "I",
                                                           "J - Información y..." = "J",
                                                           "K - Int. Financiera..." = "K",
                                                           "L - Inmobiliarios..." = "L",
                                                           "M - Serv. Prof..."  = "M",
                                                           "N - Administrativos" = "N",
                                                           "P - Enseñanza" = "P",
                                                           "Q - Salud etc." = "Q",
                                                           "R - Artisticos etc." = "R",
                                                           "S - Asociaciones etc." = "S",
                                                           "OTROS" = "Otro"))
                  ),
                  conditionalPanel("input.indicador == 2",
                                   pickerInput(inputId = "cultivo1",
                                               label = "Cultivo: ",
                                               choices = c("Algodón" = "Algodón",
                                                           "Cebada forrajera" = "Cebada forrajera",
                                                           "Lenteja" = "Lenteja",
                                                           "Soja total" = "Soja total",
                                                           "Alpiste" = "Alpiste",
                                                           "Cebada total" = "Cebada total",
                                                           "Lino" = "Lino",
                                                           "Sorgo" = "Sorgo",
                                                           "Arroz" = "Arroz",
                                                           "Centeno" = "Centeno",
                                                           "Maíz" = "Maíz",
                                                           "Trigo candeal" = "Trigo candeal",
                                                           "Arveja" = "Arveja",
                                                           "Colza" = "Colza",
                                                           "Mijo" = "Mijo",
                                                           "Trigo total" = "Trigo total",
                                                           "Avena" = "Avena",
                                                           "Garbanzo" = "Garbanzo",
                                                           "Soja 1ra" = "Soja 1ra",
                                                           "Cebada cervecera" = "Cebada cervecera",
                                                           "Girasol" = "Girasol",
                                                           "Soja 2da" = "Soja 2da"),
                                               selected = "Avena")),
                  
                  pickerInput(inputId = "indicador",
                              label = "Indicador:",
                              choices = c("Empleo público" = 0,
                                          "Empleo privado Agregado" = 1,
                                          "Empleo privado por Rama" = 12,
                                          "Ratio Priv/Pub" = 3,
                                          "Sector Agropecuario" = 2),
                              selected = 0),
                  
                  ),
                  
                  # MAPAS
                  
                  mainPanel(
                    conditionalPanel("input.indicador == 0",
                                     leafletOutput("empleadospublicos",
                                                   height = "80vh",
                                                   width = "80vh")),
                    
                    conditionalPanel("input.indicador == 1",
                                     leafletOutput("privadostotal",
                                                   height = "80vh",
                                                   width = "80vh")),
                    
                    conditionalPanel("input.indicador == 12",
                                     leafletOutput("privadosrama",
                                                height = "80vh",
                                                width = "80vh")),
                    
                    conditionalPanel("input.indicador == 2",
                                     leafletOutput("agro",
                                                   height = "80vh",
                                                   width = "80vh")),
                    
                    conditionalPanel("input.indicador == 3",
                                     leafletOutput("ratios",
                                                   height = "80vh",
                                                   width = "80vh"))
                    
                            )
                            )
),

tabPanel("Acerca De",
         sidebarLayout(
           sidebarPanel(h4(strong("Resumen")),
                        p(strong("AUTOR | "), " Nicolás Gottig"),
                        p(strong("FECHA | "), " Mayo - 2023"),
                        p(strong("CONSULTA DE DATOS | "), "Mayo 2023"),
                        p(strong("CODIGO DISPONIBLE EN | "), a(href = 'https://github.com/NicoGottig/ind-prov', 'GitHub', .noWS = "outside")),
                        p(strong("PAQUETES UTILIZADOS | "), 
                          "Tidyverse, Leaflet, Shiny, Rvest, readODS,
                                           Lubridate, Naniar, Zoo, Janitor,
                                           Rgdal, Sf, Sp.")
                        
           ),
           mainPanel(
             h4("Mapa de Indicadores Económicos de ", em("Entre Ríos")),
             h5("Resumen: "),
             p(align = "justify", "El mapa es una aplicación que integra indicadores económicos del sector productivo por departamento. Resume la cantidad de trabajo registrado por rama y las toneladas producidas, entre los años 2015 y 2022."),
             p(align = "justify", em("Dependiendo el año y el tipo de cultivo o rama productiva, puede visualizarse el mapa vacío o un mensaje de error. ")),
             h5("Metodología:"),
             p(align = "justify", "• Para el cálculo de cantidad de trabajadores se utilizó un promedio anual, dado que el registro es por mes. Luego, se sumaron (omitiendo nulos) la cantidad de trabajadores por sub-rama o rama según corresponda. Los datos corresponden al las estimaciones agrícolas del Min. de Agricultura, Ganadería y Pesca y a los registros de los Datos Abiertos de Desarrollo Productivo."),
             p(align = "justify", "• El ratio de empleados públicos y privados es un cociente entre los promedios anuales de empleados privados en todas las ramas sobre los promedios anuales de empleados públicos. Cuando es mayor a 1 indica que hay más empleados privados que públicos."),
             p(align = "justify", "• La metodología se encuentra completa en la documentación de Github. Eventualmente pueden agregarse más indicadores. Además, convendría informar los casos nulos en el el análisis por sectores, ya que hay provincias que no tienen empleo registrado en algunos."),
             hr(),
             p(em("Última revisión: may-2023."))
           )
         )
)

)




#### server #####

server <- function(input, output, session){
  
  # PUBLICOS
  df_publico <- reactive({
    tmp <- publico %>% 
      filter(year == input$anio1)
    
    tmp <- sp::merge(shapeData, tmp, by.x = "departamen", by.y = "departamen")
    
    tmp
  })
  
  # PRIVADOS TOTAL
  df_privado_total <- reactive({
    
    tmp <- privado %>% 
      group_by(year, departamen = depto) %>%
      summarise(puestos = sum(puestos, na.rm = TRUE)) %>% 
      filter(year == input$anio1)
    
    tmp <- sp::merge(shapeData, tmp, by.x = "departamen", by.y = "departamen")
    
    tmp
})
  
  # PRIVADOS RAMA
  df_privado_rama <- reactive({
    
    tmp <- privado %>% 
      group_by(year, departamen = depto, letra) %>%
      summarise(puestos = sum(puestos, na.rm = TRUE)) %>% 
      filter(year == input$anio1,letra == input$rama)
    
    tmp <- sp::merge(shapeData, tmp, by.x = "departamen", by.y = "departamen")
    
    tmp
  })
  
  
  # AGRO
  df_agro <- reactive({
    
    tmp <- agro %>% 
      filter(Cultivo == input$cultivo1, Fecha == input$anio1) %>% 
      select(Cultivo, prod = Producción, departamen = Depart, Fecha)
    
    tmp <- sp::merge(shapeData, tmp, by.x = "departamen", by.y = "departamen")
    
    tmp
    
  })
  
  # RATIO
  df_ratio <- reactive({
    
    tmp <- ratio %>% 
      filter(year == input$anio1)
    
    tmp <- sp::merge(shapeData, tmp, by.x = "departamen", by.y = "departamen")
    
    tmp
    
  })
  
  
  # MAPA EMPLEADOS PUBLICOS
  output$empleadospublicos <- renderLeaflet({
    var <- as.numeric(df_publico()$mean)
    bins <- c(0, quantile(var, .20), 
              quantile(var, .40),
              quantile(var, .60),
              quantile(var, .80),
              quantile(var, 1))
    
    # Colores
    pal = colorBin("BuPu", var, bins = bins)
    
    # Texto
    textos <- paste0("<b>", "INFORMACIÓN ", "</b>",
                     "<br><b>Depto: </b> ",
                     df_publico()$departamen,
                     "<br><b>Emp. Publicos: </b> ",
                     round(df_publico()$mean,0))
    
    map <- leaflet(df_publico()) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(-59.5, -32, 7.2) %>% 
      addPolygons(weight = 2,
                  color = "black",
                  dashArray = 1,
                  fillOpacity = 0.8,
                  fillColor = ~pal(as.numeric(mean)),
                  highlight = highlightOptions(weight = 3,
                                               color = "black",
                                               fillOpacity = 0.9,
                                               dashArray = "",
                                               bringToFront = TRUE),
                  label = ~lapply(as.list(textos), htmltools::HTML),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto")) %>% 
  addLegend(pal = pal, values = ~as.numeric(var), opacity = 0.7,
            title = "Promedio anual de empleados públicos",
            position = "bottomright")
    
    map
    
  })
  
  
  # MAPA EMPLEADOS PRIVADOS TOTAL
  output$privadostotal <- renderLeaflet({
    var <- as.numeric(df_privado_total()$puestos)
    bins <- c(0, quantile(var, .20), 
              quantile(var, .40),
              quantile(var, .60),
              quantile(var, .80),
              quantile(var, 1))
    
    # Colores
    pal = colorBin("BuPu", var, bins = bins)
    
    # Texto
    textos <- paste0("<b>", "INFORMACIÓN ", "</b>",
                     "<br><b>Depto: </b> ",
                     df_privado_total()$departamen,
                     "<br><b>Emp. Privados: </b> ",
                     round(as.numeric(df_privado_total()$puestos)))
    
    map <- leaflet(df_privado_total()) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(-59.5, -32, 7.2) %>% 
      addPolygons(weight = 2,
                  color = "black",
                  dashArray = 1,
                  fillOpacity = 0.8,
                  fillColor = ~pal(as.numeric(puestos)),
                  highlight = highlightOptions(weight = 3,
                                               color = "black",
                                               fillOpacity = 0.9,
                                               dashArray = "",
                                               bringToFront = TRUE),
                  label = ~lapply(as.list(textos), htmltools::HTML),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"))%>% 
      addLegend(pal = pal, values = ~as.numeric(var), opacity = 0.7,
                title = "Promedio anual de empleados privados",
                position = "bottomright")
    
    map
    
  })
  
  
  # MAPA EMPLEADOS PRIVADOS SEPARADOS
  output$privadosrama <- renderLeaflet({
    var <- as.numeric(df_privado_rama()$puestos)
    bins <- c(0, quantile(var, .20), 
              quantile(var, .40),
              quantile(var, .60),
              quantile(var, .80),
              quantile(var, 1))
    
    
    
    # Colores
    pal = colorBin("BuPu", var, bins = bins)
    
    # Texto
    textos <- paste0("<b>", "INFORMACIÓN ", "</b>",
                     "<br><b>Depto: </b> ",
                     df_privado_rama()$departamen,
                     "<br><b>Emp. Privado registrado: ", "</b><br>",
                     round(as.numeric(df_privado_rama()$puestos)))
    
    map <- leaflet(df_privado_rama()) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(-59.5, -32, 7.2) %>% 
      addPolygons(weight = 2,
                  color = "black",
                  dashArray = 1,
                  fillOpacity = 0.8,
                  fillColor = ~pal(as.numeric(puestos)),
                  highlight = highlightOptions(weight = 3,
                                               color = "black",
                                               fillOpacity = 0.9,
                                               dashArray = "",
                                               bringToFront = TRUE),
                  label = ~lapply(as.list(textos), htmltools::HTML),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"))%>% 
      addLegend(pal = pal, values = ~as.numeric(var), opacity = 0.7,
                title = "Promedio anual de emp. priv. por rama: ",
                position = "bottomright")
    
    map
    
  })
  
  # MAPA AGRO
  output$agro <- renderLeaflet({
    var <- as.numeric(df_agro()$prod)
    bins <- c(0, quantile(var, .20), 
              quantile(var, .40),
              quantile(var, .60),
              quantile(var, .80),
              quantile(var, 1))
    
    # Colores
    pal = colorBin("YlGn", var, bins = bins)
    
    # Texto
    textos <- paste0("<b>", "INFORMACIÓN ", "</b>",
                     "<br><b>Depto: </b> ",
                     df_agro()$departamen,
                     "<br><b>Producción: </b>",
                     round(as.numeric(df_agro()$prod)))
    
    map <- leaflet(df_agro()) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(-59.5, -32, 7.2) %>% 
      addPolygons(weight = 2,
                  color = "black",
                  dashArray = 1,
                  fillOpacity = 0.8,
                  fillColor = ~pal(as.numeric(prod)),
                  highlight = highlightOptions(weight = 3,
                                               color = "black",
                                               fillOpacity = 0.9,
                                               dashArray = "",
                                               bringToFront = TRUE),
                  label = ~lapply(as.list(textos), htmltools::HTML),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"))%>% 
      addLegend(pal = pal, values = ~as.numeric(var), opacity = 0.7,
                title = paste("Producción (en toneladas) de ", df_agro()$Cultivo[1]),
                position = "bottomright")
    
    map
    
    
  })
  
  # MAPA RATIO
  output$ratios <- renderLeaflet({
    var <- as.numeric(df_ratio()$ratio)
    bins <- c(0, quantile(var, .20), 
              quantile(var, .40),
              quantile(var, .60),
              quantile(var, .80),
              quantile(var, 1))
    
    # Colores
    pal = colorBin("BuPu", var, bins = bins)
    
    # Texto
    textos <- paste0("<b>", "INFORMACIÓN ", "</b>",
                     "<br><b>Depto: </b> ",
                     df_ratio()$departamen,
                     "<br><b>Hay </b>", round(as.numeric(df_ratio()$ratio), 2), " emp. Priv. c/ <b>1</b> público.")
    
    map <- leaflet(df_ratio()) %>% 
      addProviderTiles("CartoDB.Positron") %>% 
      setView(-59.5, -32, 7.2) %>% 
      addPolygons(weight = 2,
                  color = "black",
                  dashArray = 1,
                  fillOpacity = 0.8,
                  fillColor = ~pal(as.numeric(ratio)),
                  highlight = highlightOptions(weight = 3,
                                               color = "black",
                                               fillOpacity = 0.9,
                                               dashArray = "",
                                               bringToFront = TRUE),
                  label = ~lapply(as.list(textos), htmltools::HTML),
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"))%>% 
      addLegend(pal = pal, values = ~as.numeric(var), opacity = 0.7,
                title = "Ratio de empleados privados sobre públicos",
                position = "bottomright")
    
    map
    
    
  })
  
  
}

shinyApp(ui = ui, server = server)

