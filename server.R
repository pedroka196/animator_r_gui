pacotes_necessarios <- c("ggplot2","gganimate","gifski","tidyverse","zoo","xlsx","shiny","readxl","tidyverse","shinythemes")

lista_instalados <- pacotes_necessarios %in% rownames(installed.packages())

for(i in length(lista_instalados)){
  if(lista_instalados[i]==F){
    install.packages(pacotes_necessarios[i])
  }
}

lapply(pacotes_necessarios, require,character.only = TRUE)

# 
# library(shiny)
# library(xlsx)
# library(readxl)
# library(tidyverse)
# library(ggplot2)


source("funcoes.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
 #### Funcao para saber se dados sao XLSX ou CSV ####
 tipo_dado <- reactive({
  inFile <- input$arquivo
  if(is.null(input$arquivo)){
    return(NULL)
  }
  if(grepl(pattern =".xls",x = inFile$datapath)) {
    return("xls")
  }
  if(grepl(pattern =".csv",x = inFile$datapath)) {
    return("csv")
  }
  
 })
 #### Função de leitura dos dados ####
 dados_csv <- reactive({
  a <- tipo_dado()
  inFile <- input$arquivo
  if(is.null(a)){
   return(NULL)
  }
  if(a == "xls"){
   dados_csv <- read_excel(path = inFile$datapath,
               sheet = input$escolha,
               skip = input$pula_linha,
               n_max = input$ultima_linha-input$pula_linha)
  }
  if(a == "csv"){
   dados_csv <- as.data.frame(read.csv(file = inFile$datapath,
                         header = T,
                         skip = input$pula_linha-1,
                         sep = input$escolha,
                         nrows = input$ultima_linha))
   
   # if(ncol(dados_csv) == 1){
   #  dados_csv <- read.csv(file = inFile$datapath,header = T,sep = ";")
   # }
  }
  if(a == "NAO"){
    dados_csv <- NULL
  }
  cat("\n\n\n",a,"\n\n")
  dados_csv <- as.data.frame(dados_csv)
  return(dados_csv)
  
  #return(dados_csv)
 })
 # Campo de selecao
 #### Seleção das variáveis X ####
 output$selecionador <- renderUI({
   input1 <- NULL
   input2 <- NULL
   input3 <- NULL
   input4 <- NULL
   # Se dados nao sao nulos, mostra variaveis nulas
  dados <- dados_csv()
  if(!is.null(dados)){
    input1 <- selectInput(inputId = "VariaveisX",
                          label = "Selecione X",
                          choices = names(dados),
                          multiple = F)
    
    input2 <- radioButtons(inputId = "Dados_Wide",
                           label = "Dados wide?",
                           choices = c(T,F),
                           inline = T,
                           selected = T)
    
    # input3 <- selectInput(inputId = "VariaveisY",
    #                       label = "Selecione Y",
    #                       choices = names(dados),
    #                       multiple = T)
    # 
   
    
    }
  list(input1,input2,input3,input4)
 })
 
 #### Seleção de Y ####
 output$seleciona_Y <- renderUI({
   # Se dados nao sao nulos, mostra variaveis nulas
   input1 <- NULL
   input2 <- NULL
   dados <- dados_csv()
   if(!is.null(dados)){
     if(input$Dados_Wide == T){
     input1 <- selectInput(inputId = "VariaveisY",
                           label = "Selecione Y",
                           choices = names(dados),
                           multiple = T)
     }
     
     else{
       dados_separados <- separador(dados, input$VariaveisX,F)
       nomes <- unique(dados_separados$chaves)
       input1 <- selectInput(inputId = "VariaveisY1",label = "Selecione Y",choices = nomes,multiple = T)
       #cat("fUNCIONA WIDE\n")
       escolhas <- c("Data","Número Real","Número Inteiro")
       input2 <- radioButtons(inputId = "tipo_dado",
                              label = "Qual o tipo da chave",
                              choices = escolhas,
                              inline = T)
     }
     
   }
   list(input1,input2)
 })
 ####
 ### Daqui para baixo tem dados de acesso a dados
 #### Seleção da planilha ou do separador####
 output$selecao_dados  <- renderUI({
   input1 <- NULL
   csv_valido <- tipo_dado()
   data_path <- input$arquivo$datapath
  if(!is.null(csv_valido)){
    
  if(csv_valido == "xls") {
    rotulo <- "Escolha a planilha"
    escolhas <- excel_sheets(data_path)
  }
   if(csv_valido == "csv") {
     rotulo <- "Escolha o separador"
     escolhas <- c(",",";")
   }
   input1<-selectizeInput(inputId = "escolha",
                  label = rotulo,
                  choices = escolhas)
  }
   
   list(input1)
 })
 # ### Separador do CSV
 #### Seleção das linhas ####
  output$linhas  <- renderUI({
    input1 <- NULL
    input2 <- NULL
    csv_valido <- tipo_dado()
    data_path <- input$arquivo$datapath
    if(csv_valido == "csv") {
      numero_linhas <- nrow(read.csv(data_path,sep = input$escolha))
      rotulo1 = "Quantas pulos de linha"
      rotulo2 = "Escolha ultima linha"
    }
    if(csv_valido == "xls") {
      numero_linhas <- nrow(read_excel(path = data_path,sheet = input$escolha))
      rotulo1 = "Quantas pulos de linha"
      rotulo2 = "Escolha ultima linha"
      
    }
    input1 <- numericInput(inputId = "pula_linha",
                           label = rotulo1,
                           value = 1,
                           max = numero_linhas,
                           width = 100)
    input2 <- numericInput(inputId = "ultima_linha",
                           label = rotulo2,
                           value = numero_linhas,
                           width = 100)
    list(input1,input2)
    })

  #### Tabela de dados wide ####
  output$contents2 <- renderTable({
    tabela1 <- NULL
    dados <- dados_csv()
    dados_graph <- separador(dados,input$VariaveisX,input$Dados_Wide)
    if(!is.null(dados)){
       tabela1 <- rbind.data.frame(head(dados_graph),tail(dados_graph))
    }
    
    list(tabela1)
    },hover = T,
    striped = T,
    digits = 1,
    bordered = T,
    width = 10,
    align = "c",
    rownames = F,
    na = "--")
  
  output$contents <- renderTable({
    tabela1 <- NULL
    dados <- dados_csv()
    if(!is.na(dados)){
      tabela1 <- rbind.data.frame(head(dados),tail(dados))
      # tabela2 <- "-------------------------------------"
      # tabela3 <- rbind.data.frame(head(dados_graph),tail(dados))
    }
    
    tabela1
    },hover = T,
    striped = T,
    digits = 1,
    bordered = T,
    width = 10,
    align = "c",
    rownames = F,
    na = "--")
 
 #### Output do gráfico na segunda aba ####
 output$graficos <- renderPlot({
    grafico <- grafico_gerado()
    grafico
})
 
 #### Função que gera gráfico ####
 grafico_gerado <- reactive({
   dados <- dados_csv()
   dados_graph <- separador(dados, input$VariaveisX,input$Dados_Wide)
    # escolhas <- c("Data","Número Real","Número Inteiro")
   # if( input$Dados_Wide == F & input$tipo_dado == "Data") {
   #   dados_graph$Var_X <- as.Date(as.integer(dados_graph$Var_X),origin = "1900-01-01")
   # }
   variaveis <- input$VariaveisY
   titulo = input$titulo_grafico
   fonte = paste("Fonte:",input$fonte_grafico,sep = " ")
   
   # gerador_grafico(X = dadosX,Y=dadosY,tipos = "linha",nomes = "UAU")
   
    p1 <- gerador_grafico_2(base = dados_graph,
                     variaveis = variaveis,
                     tipos = input$tipo_grafico,
                     titulo = titulo,
                     fonte = fonte,
                     tamanho_fonte = input$tamanho_texto)
    
    # p1 = p1 + theme()
    if(input$tipo_grafico == "linha"){
      p1 = p1+geom_line(size=input$grafico_linha)
    }
    if(input$tipo_grafico == "ponto"){
      p1 = p1+geom_point(size=input$grafico_linha)
    }
    if(input$tira_legenda == T){
      p1 = p1 + theme(legend.position = "none")
    }
    
    return(p1)
 })
 
 #### Tipo do gráfico ####
 output$graficos_tipo <- renderUI({
   tipos <- c("linha","barra","ponto")
   input1 <- radioButtons(inputId = "tipo_grafico",
                label = "Qual o tipo do gráfico?",
                choices = tipos,
                inline = T)
   input2 <- sliderInput(inputId = "grafico_linha",
                         label = "Qual tamanho da linha?",
                         min = 0,
                         max = 10,
                         value = 1,
                         round = F)
   input3 <- sliderInput(inputId = "tamanho_texto",
                         label = "Qual tamanho dos rótulos numéricos?",
                         min = 0,
                         max = 10,
                         value = 1,
                         round = F)
   input4 <- checkboxInput("tira_legenda","Remover legenda?",value = F)
   
   list(input1,input2,input3, input4)
 })
 #### Output do nome dos gráficos ####
 output$graficos_nome <- renderUI({
  input1 <- textInput("titulo_grafico","Qual o título do grafico?")
  input2 <- textInput("fonte_grafico","Qual a fonte do grafico?")
  
  list(input1,input2)
 })
 
 #### output do tempo de animação na terceira aba ####
 output$tempo_animacao <- renderUI({
   input1 <- NULL
   input2 <- NULL
   input1 <- sliderInput("quadros_totais", label = "Tempo de animação (Segundos)",min = 1,max = 200,value = 1)
   input2 <- sliderInput("tempo_animado",label = "Quadros por segundo",min = 1,max = 24,value = 12)
   input3 <- sliderInput("tempo_parado",label = "Segundos no final da animação",min = 0,max = 100,value = 0)
   input4 <- numericInput("tamanho_X", "Qual a largura do grafico?",value=400,min=100,max = 1600)
   input5 <- numericInput("tamanho_Y", "Qual a altura do grafico?",value=400,min=100,max = 1600)
   input6 <- actionButton(inputId = "renderiza", label = "Renderizar")
   
   list(input1,input2,input3,input4,input5,input6)
   })
 
 #### Output do gif animado do gráfico ####
 output$grafico_animado <- renderImage({
   outfile <- tempfile(fileext = '.gif')
   
   if (gif_animado$anima == F) {
     return()
   }
   
     list(src = "outfile.gif",
          contentType = 'image/gif'
          # width = 400,
          # height = 300,
          # alt = "This is alternate text"
     )

   
   # grafico_animado()
  
   

 })
 # grafico_animado <- reactive({
   # grafico <- grafico_gerado()
   # 
   # grafico_animado_p <- grafico + transition_reveal(Var_X)
   # 
   # #animate(grafico_animado,fps = input$tempo_animado, nframe = )
   # grafico_animado_p <- animate(grafico_animado_p,
   #                              fps = input$tempo_animado,
   #                              nframe=input$tempo_animado*input$quadros_totais,
   #                              end_pause = input$tempo_animado*input$tempo_parado,
   #                              width=input$tamanho_X,
   #                              height=input$tamanho_Y)
   # 
   # anim_save("output.gif", animation = grafico_animado_p)


   # return()

 # })
 
 # renderiza <- reactiveValues(data = F)
 # Renderiza
 #### Verifica se o botão de render foi pressionado ####
 observeEvent(input$renderiza, {
   gif_animado$anima = F

   
   outfile <- tempfile(fileext='.gif')
   # # 
   # # 
   grafico <- grafico_gerado()
   # 
   grafico_animado_p <- grafico + transition_reveal(Var_X)
   # 
   # animate(grafico_animado,fps = input$tempo_animado, nframe = )
   grafico_animado_p <- animate(grafico_animado_p,
                                fps = input$tempo_animado,
                                nframe=input$tempo_animado*input$quadros_totais,
                                end_pause = input$tempo_animado*input$tempo_parado,
                                width=input$tamanho_X,
                                height=input$tamanho_Y)
   # 
   # # Create a Progress object
   # progress <- shiny::Progress$new()
   # 
   # on.exit(progress$close())
   # 
   # progress$set(message = "Renderizando", value = 0)
   # 
   anim_save("outfile.gif", animation = grafico_animado_p)
  
  cat("UAU\n\n\n",gif_animado$anima)
   
   gif_animado$anima = T
   # progress$inc(1, detail = "Renderizado.")
   # session$sendCustomMessage(type = 'Aviso',
   #                           message = 'Renderizado')
   
 })
 gif_animado <- reactiveValues(anima = F)
 
})
# Determina de faz ou nao ou load do gif
