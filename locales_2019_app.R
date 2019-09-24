# PAQUETES ####
library(shiny)
library(tidyverse)
library(RCurl)
library(rsconnect)
library(RColorBrewer)

# UTF 8 ####
options(encoding = "UTF-8")

# 1. Importar datos de las encuestas desde Github ####
bta <- read.csv("https://raw.githubusercontent.com/nelsonamayad/elecciones_locales_2019/master/Ciudad/bogota.csv") %>%
  gather(key=candidato,value=int_voto,-fecha,-encuestador,-n,-fuente,-m_error) %>%
  mutate(ciudad="Bogota", int_voto = int_voto %>% as.numeric())

cali <- read.csv("https://raw.githubusercontent.com/nelsonamayad/elecciones_locales_2019/master/Ciudad/cali.csv")   %>%
  gather(key=candidato,value=int_voto,-fecha,-encuestador,-n,-fuente,-m_error) %>%
  mutate(ciudad="Cali", int_voto = int_voto %>% as.numeric())

medellin <- read.csv("https://raw.githubusercontent.com/nelsonamayad/elecciones_locales_2019/master/Ciudad/medellin.csv") %>%
  gather(key=candidato,value=int_voto,-fecha,-encuestador,-n,-fuente,-m_error) %>%
  mutate(ciudad="Medellin", int_voto = int_voto %>% as.numeric())

# 2. Datos ####
d <- bta %>%
  bind_rows(cali) %>%
  bind_rows(medellin) %>%
  mutate(min=int_voto-m_error,max=int_voto+m_error)

# 3. Fecha de actualizacion ####
f <- c(Sys.Date())

# User Interface ####
ui <- fluidPage(
  # Titulo:
  titlePanel("Entrada: Tendencias en 3 alcaldias de Colombia 2019"),
  # Autor:
  p(em("Por:", a(href="https://twitter.com/NelsonAmayaD", "@nelsonamayad"))),
  p(em("Última actualización: ",f)),
  hr(),
  # Descripcion:
  tags$abbr("Esta nueva entrada recoge: 1) la intención de voto de cada candidato en cada encuesta, con su respectivo márgen de error, y 2) una curva", a(href="https://en.wikipedia.org/wiki/Local_regression","(LOESS)"),"."),
  br(),
  br(),
  tags$abbr("Los datos compilados de las encuestas salen directamente de",a(href="https://github.com/nelsonamayad/elecciones_locales_2019/tree/master/Ciudad","este GitHub"),"gracias a la información que las encuestadoras @Invamer, @CNCSocial, @Datexco, @Yanhaas, @Guarumoapps y @LosMosqueteros han hecho públicas."),
  br(),
  br(),
  tags$abbr("Buen provecho."),
  br(),
  br(),
  # Menus:
  sidebarPanel(
    selectInput("ciudad","Seleccione una ciudad (Bogota, Medellin o Cali):",
                choices=factor(d$ciudad),
                selected = 1),
    downloadButton("descargar","Descargar datos (.csv)")),
  
  #Panel principal con grafica
  mainPanel(
    plotOutput("elecciones")
  ))

# Server ####
server <- function(input, output) {
  #download button
  output$descargar <- downloadHandler(
    filename = function() {
      paste('elecciones_2019', Sys.Date(), '.csv', sep='')},
    content = function(con) {
      write.csv(d, con)})
  
  #Reactive ciudad
  filtro <- reactive(
    data <- d %>%
      filter(ciudad==input$ciudad, candidato!="ns_nr", candidato!="ninguno")
  )
  
  # Scatter y curva de tendencia de candidatos
  output$elecciones <- renderPlot({ 
    if (input$ciudad=="Bogota") {
      filtro() %>% 
        ggplot(aes(x=fecha %>% as.Date(),y=int_voto,color=candidato, group=candidato))+
        geom_point(aes(shape=encuestador))+
        geom_linerange(aes(ymin=min,ymax=max))+
        geom_smooth(method = "loess", se=F)+
        theme_classic()+
        labs(y="Intencion de voto", x="")+
        theme_classic()+
        scale_color_brewer(palette = "Set1")
    }
    else if (input$ciudad=="Medellin") {
      filtro() %>%
        filter(candidato!="cesar_hernandez", 
               candidato!="jesus_ramirez", 
               candidato!="luis_hoyos",
               candidato!="victor_correa", 
               candidato!="jairo_herran",
               candidato!="gemma_mejia",
               candidato!="jorge_gutierrez",
               candidato!="luiz_munoz"
        ) %>%
        ggplot(aes(x=fecha %>% as.Date(),y=int_voto,color=candidato, group=candidato))+
        geom_point(aes(shape=encuestador))+
        geom_linerange(aes(ymin=min,ymax=max))+
        geom_smooth(method = "loess", se=F)+
        theme_classic()+
        labs(y="Intencion de voto", x="")+
        theme_classic()+
        scale_color_brewer(palette = "Paired")
    }
    else {
      filtro() %>%
        ggplot(aes(x=fecha %>% as.Date(),y=int_voto,color=candidato, group=candidato))+
        geom_point(aes(shape=encuestador))+
        geom_linerange(aes(ymin=min,ymax=max))+
        geom_smooth(method = "loess", se=F)+
        theme_classic()+
        labs(y="Intencion de voto", x="")+
        theme_classic()+
        scale_color_brewer(palette = "Paired")
    }
  })}

# RUN! ####
shinyApp(ui = ui, server = server)
