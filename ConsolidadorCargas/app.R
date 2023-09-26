# A ideia é criar uma aplicação onde eu insira uma planilha das notas roteirizadas no mês.
# Agrupar as notas por regiões do Brasil e pelas origens. Retornando algumas informações desses agrupamentos.
# criar um calculo com inputs tbm para definir a cidade de transbordo para o destino final.

library(shiny) # biblioteca do shiny
library(shinydashboard) # biblioteca do shiny dashboards que ajuda na estruturação da pagina como um dashboard
library(leaflet) # biblioteca leaflet que ajuda na criação de mapas
library(readxl) # biblioteca para ler as planilhas
library(tidyverse) # biblioteca para manipular os dados

# Carregando a base de dados de latitudes e longitudes 
load("BaseLongLat.RData")

# UI recebe as características visuais que mais tarde vão iteragir com as funções do Server
ui <- dashboardPage(
    dashboardHeader(title = "Consolidador"),
    dashboardSidebar(
        fileInput("file", "Inserir Planilha Excel"),
        selectInput("origem", "Filtrar por Origem", choices = NULL),
        selectInput("regiao", "Filtrar por Região", choices = NULL),
        selectInput("data_disponivel", "Escolha uma Data", choices = NULL),
        selectInput("cidade_transbordo", "Cidade de Transbordo", choices = NULL),
        numericInput("custo_transbordo", "Custo do Transbordo", value = NULL),
        numericInput("preco_lotacao", "Custo da Lotação", value = NULL)
    ),
    dashboardBody(
        leafletOutput("mapa"),
        fluidRow(
            valueBoxOutput("volumetria",width = 3),
            valueBoxOutput("cubagem",width = 3),
            valueBoxOutput("peso",width = 3),
            valueBoxOutput("preco_mercadoria",width = 3)
        )
    )
)

# Server recebe as funções que serão executadas em cada visual definido pela UI
server <- function(input, output, session) {
    dados <- reactive({
        req(input$file)
        df <- read_xlsx(input$file$datapath)
        
        # Converter a coluna de data para o formato correto (dd/mm/aaaa)
        df$`Data Faturamento` <- as.Date(df$`Data Faturamento`, format = "%d/%m/%Y")
        
        return(df)
    })
    
    observe({
        updateSelectInput(session, "origem", choices = unique(dados()$Filial))
    })
    
    observe({
        origem <- input$origem
        if (!is.null(origem)) {
            filtered_data <- dados() %>% filter(Filial == origem)
            updateSelectInput(session, "regiao", choices = unique(filtered_data$`Região Brasil`))
        }
    })
    
    observe({
        origem <- input$origem
        regiao <- input$regiao
        if (!is.null(origem) && !is.null(regiao)) {
            filtered_data <- dados() %>% filter(Filial == origem, `Região Brasil` == regiao)
            updateSelectInput(session, "data_disponivel", choices = unique(filtered_data$`Data Faturamento`))
        }
    })
    observe({
        updateSelectInput(session, "cidade_transbordo", choices = unique(BD$Região))
    })
    
    dados_filtrados <- reactive({
        filtered_data <- dados()
        
        if (!is.null(input$origem)) {
            filtered_data <- filtered_data %>% filter(Filial == input$origem)
        }
        if (!is.null(input$regiao)) {
            filtered_data <- filtered_data %>% filter(`Região Brasil` == input$regiao)
        }
        if (!is.null(input$data_disponivel)) {
            filtered_data <- filtered_data %>% filter(`Data Faturamento` == input$data_disponivel)
        } 
        
        return(filtered_data)
    })


    output$mapa <- renderLeaflet({
        mapa <- leaflet(data = dados_filtrados()) %>%
            addTiles() %>%
            addMarkers(
                lng = ~longitude,
                lat = ~latitude,
                popup = ~`Cidade Destino`
            )
        
        if (!is.null(cidade_selecionada)) {
            # Filtra os dados da cidade selecionada
            cidade_data <- BD[BD$região == cidade_selecionada,]
            
            # Adiciona marcadores vermelhos para cada cidade
            mapa <- mapa %>%
                addMarkers(
                    data = cidade_data,
                    lng = ~Longitude,
                    lat = ~Latitude,
                    popup = ~Cidade,
                    icon = makeIcon(iconUrl = "http://www.clker.com/cliparts/n/T/q/X/8/V/red-marker-png-hi.png", iconWidth = 30, iconHeight = 40)
                )
        }
        
        return(mapa)
    })
    
    output$volumetria <- renderValueBox({
        valueBox(
            value = nrow(dados_filtrados()),
            subtitle = "Volumetria",
            icon = icon("list"),
            color = "blue"
        )
    })
    
    output$cubagem <- renderValueBox({
        valueBox(
            value = sum(dados_filtrados()$Cubagem),
            subtitle = "Cubagem",
            icon = icon("list"),
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
            icon = icon("list"),
            color = "blue"
        )
    })
    
    observe({
        cidade_transbordo <- input$cidade_transbordo
        custo_transbordo <- input$custo_transbordo
        preco_lotacao <- input$preco_lotacao
        
        # continuação
        # ...
    })
}

shinyApp(ui, server)