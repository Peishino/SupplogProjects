library(tidyverse)
library(rvest)
library(stringr)

# Site Lista Cep que contem todos os ceps do Brasil

url <- "https://listacep.com/"

html <- read_html(url)

# o site é subdividido em outros sites por isso vamos procurar por alguns links em listas ul

# Função para melhorar o código

extract_links <- function(url) {
  html <- read_html(url)
  html %>%
    html_elements("ul a") %>%
    html_attr("href")
}

# como o número de cidades existentes é mais q 11000 os loops estavam pesando muito o código então decidi usar

estados_links <- extract_links(url)

cidades_links <- estados_links %>%
  map(extract_links) %>%
  flatten_chr()

bairros_links <- cidades_links %>%
  map(extract_links) %>%
  flatten_chr()



UfList <- c()
CepsList <- c()
RuasList <- c()
BairroList <- c() 

for (bairro in BairrosLinks){
  html4 <- read_html(bairro)
  
  Uf <- html4 %>%
    html_element("nav")%>%
    html_element("li")[3]%>%
    html_text2()
    
  UfList <- c(UfList,Uf)
  
  Bairro2 <- html4%>%
    html_element("nav")%>%
    html_element("li")[4]%>%
    html_text2()
  
  BairroList <- c(BairroList, Bairro2)
  
  
}