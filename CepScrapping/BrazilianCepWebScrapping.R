library(tidyverse)
library(rvest)
library(httr)
library(stringr)



# Função para melhorar o código
# o site é subdividido em outros sites por isso vamos procurar por alguns links em listas ul

# estava tendo um erro na raspagem dos dados, a solução encontrada foi definir um usuário e colocar um tempo entre as solicitações

# definindo um usuário
headers <- c(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"
)

extract_links <- function(url) {
  response <- GET(url, add_headers(headers), decode = FALSE)
  html <- read_html(content(response, "text", encoding = "UTF-8"))
  html %>%
    html_element("ul") %>%
    html_elements("a") %>%
    html_attr("href")
}

# Site Lista Cep que contem os ceps do Brasil
url <- "https://listacep.com/"

estados_links <- extract_links(url)
estados_links <- unique(estados_links)

cidades_links <- estados_links %>%
  map(function(link) {
    Sys.sleep(0.1)  # tempo entre as solicitações
    extract_links(link)
  }) %>%
  flatten_chr()

cidades_links <- unique(cidades_links)
cidades_links <- setdiff(cidades_links, estados_links)

bairros_links <- cidades_links %>% 
  map(function(link) {
    Sys.sleep(0.1)  # tempo entre as solicitações
    extract_links(link)
  }) %>%
  flatten_chr()

bairros_links <- unique(bairros_links)
bairros_links <- setdiff(bairros_links,cidades_links)

