library(tidyverse)
library(rvest)
library(stringr)

url <- "https://listacep.com/"

html <- read_html(url)

# o site é subdividido em outros sites por isso vamos procurar por alguns links

estados <- html %>%
  html_elements("ul a")%>%
  html_attr("href")

CidadesLinks <- c()
for (link in estados){
  html2 <- read_html(link)
  
  cidades <- html2 %>%
    html_element("ul")%>%
    html_elements("a")%>%
    html_attr("href")
  CidadesLinks <- c(CidadesLinks,cidades)
}

BairrosLinks <- c()
for (links in CidadesLinks) {
  html3 <- read_html(links)
  
  bairros <- html3 %>%
    html_element("ul")%>%
    html_elements("a")%>%
    html_attr("href")
  BairrosLinks <- c(BairrosLinks, bairros)
}

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