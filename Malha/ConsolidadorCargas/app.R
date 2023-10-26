{
  library(shiny) # biblioteca do shiny
  library(shinydashboard) # biblioteca do shiny dashboards que ajuda na estruturação da pagina como um dashboard
  library(shinyjs)
  library(leaflet) # biblioteca leaflet que ajuda na criação de mapas
  library(readxl) # biblioteca para ler as planilhas
  library(tidyverse) # biblioteca para manipular os dados
  library(leaflet.extras)
  library(DT)
  library(lpSolve)
}

gerar_chave <- function(origem, cidade_transbordo, veiculo) {
  chave <- paste(origem, cidade_transbordo, veiculo, sep = "_")
  return(chave)
}


# Carregando a base de dados de latitudes e longitudes 
load("LongLat.RData")
load("Transb.RData")

# UI recebe as características visuais que mais tarde vão iteragir com as funções do Server
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Consolidador"),
  dashboardSidebar(
    fileInput("file", "Inserir Planilha Excel"),
    selectInput("origem", "Filtrar por Origem", choices = c("Todos", NULL), selected = "Todos"),
    selectInput("regiao", "Filtrar por Região", choices = c("Todos", NULL), selected = "Todos"),
    dateRangeInput("range_dates", "Selecione um período de datas:", start = NULL, end = NULL),
    selectInput("cidade_transbordo", "Cidade de Transbordo", choices = NULL),
    numericInput("custo_armazen", "Reais KG Armazen", value = 0),
    numericInput("custo_lotacao","Reais KG Lotação", value = 0),
    numericInput("custo_pfinal", "Reais KG LastMail", value = 0)
  ),
  dashboardBody(
    fluidRow(
      column(width = 9,
        leafletOutput("mapa")
        ),
      column(width = 3,
        valueBoxOutput(width = 12, "Valor_total"),
        valueBoxOutput(width = 12, "Valor_Inputs"),
        valueBoxOutput(width = 12, "Valor_restante")
      )
    ),
    fluidRow(
      valueBoxOutput("volumetria",width = 3),
      valueBoxOutput("cubagem",width = 3),
      valueBoxOutput("peso",width = 3),
      valueBoxOutput("preco_mercadoria",width = 3),
      valueBoxOutput("num_carretas", width = 3),
      valueBoxOutput("num_trucks", width = 3),
      valueBoxOutput("num_tocos", width = 3),
      valueBoxOutput("num_34s", width = 3),
      dataTableOutput("tabela_notinhas")
    )
  )
)

# Server recebe as funções que serão executadas em cada visual definido pela UI
server <- function(input, output, session) {
  # Leitura da planilha input e alguns tratamentos
  dados <- reactive({
    req(input$file)
    df <- read_xlsx(input$file$datapath)
    df$`Data Faturamento` <- as.Date(df$`Data Faturamento`, format = "%d/%m/%Y")
    df <- df[!is.na(df$DISTRIBUICAO) & df$DISTRIBUICAO != "RETENCAO", ]
    df$`Custo NF` <- as.numeric(df$`Custo NF`)
    df$`Peso Kg` <- as.numeric(df$`Peso Kg`)
    df$Cubagem <- as.numeric(df$Cubagem)
    df <- df %>% filter(!is.na(`Peso Kg`) & !is.na(`Custo NF`) & !is.na(Cubagem))
    return(df)
  })
  
  # Seleções de Filtros
  {
  # Origem
  observe({
    updateSelectInput(session, "origem", choices = c("Todos",unique(dados()$Filial)))
  })
  
  # Regiões possíveis do filtro de Regiões (organizar uma visão por mesoregião).
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
  # Data
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
  }
  
  # Cidade selecionavel p/transbordo
  observeEvent(input$origem, {
    origem_selecionada <- input$origem
    
    if (!is.null(origem_selecionada) && origem_selecionada != "Todos") {
      # Filtrar as opções com base na origem selecionada
      opcoes_filtradas <- unique(Transb[Transb$ORIGEM == origem_selecionada, "CONCATENA"])
    } else {
      # Se "Todos" for selecionado, mostre todas as opções
      opcoes_filtradas <- unique(Transb$CONCATENA)
    }
    updateSelectizeInput(
      session,
      "cidade_transbordo",
      choices = opcoes_filtradas,
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
  
  # Filtro dos Dados baseado nas seleções
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
  
  # Valor Custo Transbordo
  valor_atual <- reactive({
    data <- dados_filtrados()
    custo_armazen <- input$custo_armazen
    custo_lotacao <- input$custo_lotacao
    custo_pfinal <- input$custo_pfinal
    valor_atual <- sum(max(sum(data$Cubagem)*250,data$`Peso NF`)*custo_lotacao,(data$`Quantidade Volume`*custo_armazen),(custo_pfinal*max(sum(data$Cubagem)*250,data$`Peso NF`)))
    return(valor_atual)
  })
  
  # icones pro Mapa 
  {
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
  }
  
  # Mapa
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
  
  # ValueBoxes V.C.P.P.
  {
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
  }
  # ValueBoxes Veículos
  {
  output$num_carretas <- renderValueBox({
    num_carretas <- melhor_combinacao_veiculos()[[1]]
    valueBox(
      value = num_carretas,
      subtitle = "Número de carretas",
      icon = icon("truck"),
      color = "aqua"
    )
  })
  
  output$num_trucks <- renderValueBox({
    num_trucks <- melhor_combinacao_veiculos()[[2]]
    valueBox(
      value = num_trucks,
      subtitle = "Número de trucks",
      icon = icon("truck"),
      color = "aqua"
    )
  })
  
  output$num_tocos <- renderValueBox({
    num_tocos <- melhor_combinacao_veiculos()[[3]]
    valueBox(
      value = num_tocos,
      subtitle = "Número de tocos",
      icon = icon("truck"),
      color = "aqua"
    )
  })
  
  output$num_34s <- renderValueBox({
    num_34s <- melhor_combinacao_veiculos()[[4]]
    valueBox(
      value = num_34s,
      subtitle = "Número de 3/4",
      icon = icon("truck"),
      color = "aqua"
    )
  })
  }
  # ValueBoxes Direita Grafico
  {
  output$Valor_total <- renderValueBox({
    valor_total <- dados_filtrados()
    valor_total <- sum(valor_total$`Custo NF`)
    valueBox(
      value = valor_total,
      subtitle = "Custo NF Total",
      icon = icon("coins"),
      color = "orange"
    )
  })
  
  output$Valor_Inputs <- renderValueBox({
    valor_inputs <- valor_atual()
    valueBox(
      value = valor_inputs,
      subtitle = "Custo Transbordo",
      icon = icon("truck"),
      color =  "orange"
    )
  })
  
  output$Valor_restante <- renderValueBox({
    valor_total <- dados_filtrados()
    valor_total <- sum(valor_total$`Custo NF`)
    valor_restante <- ifelse(valor_atual() == 0, 0,valor_total - valor_atual())
    valueBox(
      value = valor_restante,
      subtitle = "Restante",
      icon = icon("sack-dollar"),
      color = "orange"
    )
  })
  }
  
  # Tabela Com as Notas Fiscais
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
    )
  })
  
  # Combinação de veículos 
  melhor_combinacao_veiculos <- reactive({
    cubagem_total <- sum(dados_filtrados()$Cubagem)
    peso_nf_total <- sum(dados_filtrados()$`Peso NF`)
    valor_nf_total <- sum(dados_filtrados()$`Valor da NF`)
    
    num_carretas <- 0
    num_trucks <- 0
    num_tocos <- 0
    num_34s <- 0 
    
    while (cubagem_total > 0 & peso_nf_total > 0 & valor_nf_total > 0) {
      if (cubagem_total > 100) {
        num_carretas <- num_carretas + 1
        cubagem_total <- cubagem_total - (100 * 0.8)
      } else if (cubagem_total > 42.5) {
        num_trucks <- num_trucks + 1
        cubagem_total <- cubagem_total - (42.5 * 0.8)
      } else if (cubagem_total > 22.5){
        num_tocos <- num_tocos + 1
        cubagem_total <- cubagem_total - (22.5 * 0.8)
      } else {
        num_34s <- num_34s + 1
        cubagem_total <- cubagem_total - (20 * 0.8)
      }
    }
    
    return(list(num_carretas, num_trucks, num_tocos, num_34s))
  })
  
  chaves <- reactiveVal(character(0))
  
}

shinyApp(ui, server)