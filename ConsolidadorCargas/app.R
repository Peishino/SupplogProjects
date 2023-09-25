# A ideia é criar uma aplicação onde eu insira uma planilha das notas roteirizadas no mês.
# Agrupar as notas por regiões do Brasil e pelas origens. Retornando algumas informações desses agrupamentos.
# criar um calculo com inputs tbm para definir a cidade de transbordo para o destino final.

library(shiny) # biblioteca do shiny
library(shinydashboard) # biblioteca do shiny dashboards que ajuda na estruturação da pagina como um dashboard
library(leaflet) # biblioteca leaflet que ajuda na criação de mapas
library(readxl)


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
        read_xlsx(input$file$datapath)
    })
    
    observe({
        updateSelectInput(session, "origem", choices = unique(dados()$Filial))
    })
    
    observe({
        origem <- input$origem
        if (!is.null(origem)) {
            filtered_data <- dados()[dados()$Filial == origem,]
            updateSelectInput(session, "regiao", choices = unique(filtered_data$`Região Brasil`))
        }
    })
    
    observe({
        origem <- input$origem
        regiao <- input$regiao
        if (!is.null(origem) && !is.null(regiao)) {
            filtered_data <- dados()[dados()$Filial == origem & dados()$`Região Brasil` == regiao,]
            updateSelectInput(session, "data_disponivel", choices = unique(filtered_data$`Data Faturamento`))
        }
    })
    
    dados_filtrados <- reactive({
        filtered_data <- dados()
        if (!is.null(input$origem)) {
            filtered_data <- filtered_data[filtered_data$Filial == input$origem,]
        }
        if (!is.null(input$regiao)) {
            filtered_data <- filtered_data[filtered_data$`Região Brasil` == input$regiao,]
        }
        #if (!is.null(input$data_disponivel)) {
        #    filtered_data <- filtered_data[filtered_data$`Data Faturamento` == input$data_disponivel,]
        #} 
        #minha função de filtrar a data está dando errado.
        return(filtered_data)
    })
    output$mapa <- renderLeaflet({
        leaflet(data = dados_filtrados()) %>%
            addTiles() %>%
            addMarkers(
                lng = ~longitude,
                lat = ~latitude,
                popup = ~`Cidade Destino`
            )
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