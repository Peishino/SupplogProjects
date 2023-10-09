{
  library(shiny) # biblioteca do shiny
  library(shinydashboard) # biblioteca do shiny dashboards que ajuda na estruturação da pagina como um dashboard
  library(leaflet) # biblioteca leaflet que ajuda na criação de mapas
  library(readxl) # biblioteca para ler as planilhas
  library(tidyverse) # biblioteca para manipular os dados
  library(leaflet.extras)
  library(DT)
  library(lpSolve)
}

# Carregando a base de dados de latitudes e longitudes 
# load("LongLat.RData")

# UI recebe as características visuais que mais tarde vão iteragir com as funções do Server
ui <- dashboardPage(
  dashboardHeader(title = "Consolidador"),
  dashboardSidebar(
    fileInput("file", "Inserir Planilha Excel"),
    selectInput("origem", "Filtrar por Origem", choices = c("Todos", NULL), selected = "Todos"),
    selectInput("regiao", "Filtrar por Região", choices = c("Todos", NULL), selected = "Todos"),
    dateRangeInput("range_dates", "Selecione um período de datas:", start = NULL, end = NULL),
    selectInput("cidade_transbordo", "Cidade de Transbordo", choices = NULL),
    numericInput("custo_armazen", "Custo do Armazen", value = NULL),
    numericInput("preco_lotacao", "Custo da Lotação", value = NULL)
  ),
  dashboardBody(
    leafletOutput("mapa"),
    fluidRow(
      valueBoxOutput("volumetria",width = 3),
      valueBoxOutput("cubagem",width = 3),
      valueBoxOutput("peso",width = 3),
      valueBoxOutput("preco_mercadoria",width = 3),
      dataTableOutput("tabela_notinhas")
    )
  )
)

# Server recebe as funções que serão executadas em cada visual definido pela UI
server <- function(input, output, session) {
  dados <- reactive({
    req(input$file)
    df <- read_xlsx(input$file$datapath)
    df$`Data Faturamento` <- as.Date(df$`Data Faturamento`, format = "%d/%m/%Y")
    return(df)
  })
  
  # Escolhas possíveis do filtro de Origem
  observe({
    updateSelectInput(session, "origem", choices = c("Todos",unique(dados()$Filial)))
  })
  
  # Regiões possíveis do filtro de Regiões, organizar uma visão por mesoregião.
  observe({
    origem <- input$origem
    if (!is.null(origem)) {
      # Se a origem for == "Todos" eu não quero filtrar a base
      if (origem == "Todos") {
        filtered_data <- dados()
        updateSelectizeInput(session, "regiao", choices = c("Todos",unique(filtered_data$`Região Brasil`)))
      } else {
        
        filtered_data <- dados() %>% filter(Filial == origem)
        updateSelectizeInput(session, "regiao", choices = c("Todos",unique(filtered_data$`Região Brasil`)))
      }
    }
  })
  
  #data
  observe({
    origem <- input$origem
    regiao <- input$regiao
    
    if (!is.null(origem) && !is.null(regiao)) {
      if (origem == "Todos" && regiao == "Todos") {
        filtered_data <- dados()
      } else if (origem == "Todos" && regiao != "Todos") {
        filtered_data <- dados() %>% filter(`Região Brasil` == regiao)
      } else if (origem != "Todos" && regiao == "Todos") {
        filtered_data <- dados() %>% filter(Filial == origem)
      } else {
        filtered_data <- dados() %>% filter(Filial == origem, `Região Brasil` == regiao)
      }
      dates <- unique(filtered_data$`Data Faturamento`)
      updateDateRangeInput(session, "range_dates", start = min(dates), end = max(dates))
    }
  })
  
  #cidade selecionavel
  observe({
    updateSelectizeInput(
      session,
      "cidade_transbordo",
      choices = unique(BD$Região),
      options = list(
        placeholder = "Digite a cidade de transbordo...",
        onInitialize = I('function() { this.setValue(""); }'),
        maxOptions = 10,
        render = I(
          '{
          item: function(item, escape) {
            return "<div>" + escape(item.text) + "</div>";
          },
        }'
        ),
        labelField = "text",
        searchField = "text"   
      )
    )
  })
  
  dados_filtrados <- reactive({
    filtered_data <- dados()
    
    origem <- input$origem
    regiao <- input$regiao
    intervalo_datas <- input$range_dates
    
    if (origem != "Todos") {
      filtered_data <- filtered_data %>% filter(Filial == origem)
    }
    
    if (regiao != "Todos") {
      filtered_data <- filtered_data %>% filter(`Região Brasil` == regiao)
    }
    
    if (!is.null(intervalo_datas)) {
      filtered_data <- filtered_data %>% filter(`Data Faturamento` >= intervalo_datas[1] & `Data Faturamento` <= intervalo_datas[2])
    }
    
    return(filtered_data)
  })
  TransbIcon <- awesomeIcons(
    icon = 'truck-fast',
    iconColor = 'black',
    library = 'fa',
    markerColor = 'orange'
  )
  OriginIcon <- awesomeIcons(
    icon = 'warehouse',
    iconColor = 'black',
    library = 'fa',
    markerColor = 'orange'
  )
  output$mapa <- renderLeaflet({
    mapa <- leaflet(data = dados_filtrados()) %>%
      addTiles() %>%
      addMarkers(
        popup = ~paste(`Cidade Destino`, "<br>", `Nota Fiscal`),
        clusterOptions = markerClusterOptions()
      )
    Origem_filtrada <- BD[BD$Cidade == input$origem,]
    mapa <- mapa %>%
      addAwesomeMarkers(
        data = Origem_filtrada,
        lng = ~Longitude,
        lat = ~Latitude,
        icon = OriginIcon

      )
    Transbordo_filtrado <- BD[BD$Região == input$cidade_transbordo,]
    mapa <- mapa %>%
      addAwesomeMarkers(
        data = Transbordo_filtrado,
        lng = ~Longitude,
        lat = ~Latitude,
        icon = TransbIcon
      )
    
    return(mapa)
  })
  
  output$volumetria <- renderValueBox({
    valueBox(
      value = nrow(dados_filtrados()),
      subtitle = "Volumetria",
      icon = icon("warehouse"),
      color = "blue"
    )
  })
  
  output$cubagem <- renderValueBox({
    valueBox(
      value = sum(dados_filtrados()$Cubagem),
      subtitle = "Cubagem",
      icon = icon("boxes-stacked"),
      color = "blue"
    )
  })
  
  output$peso <- renderValueBox({
    valueBox(
      value = sum(dados_filtrados()$`Peso NF`),
      subtitle = "Peso",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$preco_mercadoria <- renderValueBox({
    valueBox(
      value = sum(dados_filtrados()$`Valor da NF`),
      subtitle = "Preço da Mercadoria",
      icon = icon("coins"),
      color = "blue"
    )
  })
  
  observe({
    custo_armazen <- input$custo_armazen
    preco_lotacao <- input$preco_lotacao
    
  })
  output$tabela_notinhas <- renderDataTable({
    data <- dados_filtrados()
    columns_to_show <- c('Filial', 'Nota Fiscal','Concatenar cidade', 'Tomador', 'Cubagem', 'Peso NF', 'Valor da NF')
    
    datatable(
      data[, columns_to_show, drop = FALSE],
      options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        pageLength = 10,
        dom = 'tip',
        rownames = FALSE,
        select = 'single',
        ordering = FALSE
      ),
      callback = JS(
        "table.on('select', function(e, dt, type, indexes) {",
        "  var selectedData = table.rows(indexes).data().toArray();",
        "  Shiny.setInputValue('selected_notinhas', selectedData);",
        "});"
      )
    )
  })
  
}

shinyApp(ui, server)