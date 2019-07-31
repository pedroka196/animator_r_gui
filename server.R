pacotes_necessarios <- c("ggplot2","extrafont","gganimate","gifski","tidyverse","zoo","xlsx","shiny","readxl","tidyverse","shinythemes","viridis","wesanderson","ggrepel")

lista_instalados <- pacotes_necessarios %in% rownames(installed.packages())

options(scipen = 999)

for(i in length(lista_instalados)){
  if(lista_instalados[i]==F){
    install.packages(pacotes_necessarios[i])
  }
}

lapply(pacotes_necessarios, require,character.only = TRUE)

# font_import(paths = "fonts/Open_Sans/")
# font_import(paths = "fonts/Roboto/")

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
    if(is.null(inFile)){
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
                                          skip = input$pula_linha,
                                          sep = input$escolha,
                                          nrows = input$ultima_linha,
                                          dec = input$escolhas_decimal))
      
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
      
      input3 <- radioButtons(inputId = "Dados_Grupos",
                             label = "Grupo de dados??",
                             choices = c(T,F),
                             inline = T,
                             selected = F)
      
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
        
        if(input$Dados_Grupos == T){
          input2 <- selectInput(inputId = "grupos",
                                label = "Selecione os grupos de dados",
                                choices = names(dados),
                                multiple = F)
        }
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
  
  #### Output grupos ####
  output$seleciona_grupos <- renderUI({
    input1 <- NULL
    dados <- dados_csv()
    if(!is.null(dados)){
      lista_valores <- unique(dados[input$grupos])
      input1 <- selectInput(inputId = "grupos_selecionados",
                            label = "Quais grupos deseja selecionar?",
                            choices = lista_valores,
                            multiple = T)
    }
    list(input1)
  })
  ####
  ### Daqui para baixo tem dados de acesso a dados
  #### Seleção da planilha ou do separador####
  output$selecao_dados  <- renderUI({
    input1 <- NULL
    input2 <- NULL
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
        escolhas_decimal <- c(",",".")
        input2 <- selectizeInput(inputId = "separa_decimal",
                                 label = "Qual separador de decimal?",
                                 choices = escolhas_decimal)
      }
      input1<-selectizeInput(inputId = "escolha",
                             label = rotulo,
                             choices = escolhas)
    }
    
    list(input1,input2)
  })
  # ### Separador do CSV
  #### Seleção das linhas ####
  output$linhas  <- renderUI({
    input1 <- NULL
    input2 <- NULL
    csv_valido <- tipo_dado()
    data_path <- input$arquivo$datapath
    cat("O CSV é \n\n",csv_valido,"\n\n")
    if(!is.null(csv_valido)){
    if(csv_valido == "csv") {
      numero_linhas <- nrow(read.csv(data_path,sep = input$escolha))
      rotulo1 = "Quantas pulos de linha"
      rotulo2 = "Escolha ultima linha"
    }
    if(!is.null(csv_valido) & csv_valido == "xls") {
      numero_linhas <- nrow(read_excel(path = data_path,sheet = input$escolha))
      rotulo1 = "Quantas pulos de linha"
      rotulo2 = "Escolha ultima linha"
      
    }
    input1 <- numericInput(inputId = "pula_linha",
                           label = rotulo1,
                           value = 0,
                           max = numero_linhas,
                           width = 100)
    input2 <- numericInput(inputId = "ultima_linha",
                           label = rotulo2,
                           value = numero_linhas,
                           width = 100)
    }
    list(input1,input2)
  })
  
  #### Tabela de dados wide ####
  output$contents2 <- renderTable({
    tabela1 <- NULL
    dados <- dados_csv()
    if(!is.null(dados)){
      dados_graph <- separador(dados,input$VariaveisX,input$Dados_Wide)
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
    if(!is.null(dados)){
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
    p1 <- NULL
    if(!is.null(dados)){
      dados_graph <- separador(dados, input$VariaveisX,input$Dados_Wide)
    
    
    if(input$Dados_Grupos == F){
      variaveis <- input$VariaveisY
      titulo = input$titulo_grafico
      fonte = paste("Fonte:",input$fonte_grafico,sep = " ")
      
      # gerador_grafico(X = dadosX,Y=dadosY,tipos = "linha",nomes = "UAU")
      
      p1 <- gerador_grafico_2(base = dados_graph,
                              variaveis = variaveis,
                              tipos = input$tipo_grafico,
                              titulo = titulo,
                              fonte = fonte,
                              tamanho_fonte = input$tamanho_texto,
                              rotulo_acompanha = input$rotulo_acompanha,
                              proporcao = input$escala_proporcional)
    }
    
    if(input$Dados_Grupos == T){
      X <- input$VariaveisX
      Y <- input$VariaveisY
      variavel_agrupamento <- input$grupos
      grupos_selecionados <- input$grupos_selecionados
      titulo = input$titulo_grafico
      fonte = paste("Fonte:",input$fonte_grafico,sep = " ")
      
      p1 <- gerador_grafico_3(base = dados,
                              X = X,
                              Y = Y,
                              var_grupo = variavel_agrupamento,
                              grupos = grupos_selecionados,
                              titulo = titulo,
                              fonte=fonte,
                              tipos = input$tipo_grafico,
                              tamanho_fonte = input$tamanho_texto,
                              rotulo_acompanha = input$rotulo_acompanha,
                              proporcao = input$escala_proporcional)
    }
    
    # p1 = p1 + theme()
    if(input$tipo_grafico == "linha"){
      p1 = p1+geom_line(size=input$grafico_linha)
    }
    if(input$tipo_grafico == "ponto"){
      p1 = p1+geom_point(size=input$grafico_linha)
    }
    # if(input$tipo_grafico == "barra" & input$escala_proporcional == T){
    #   p1 = p1+geom_col(position = "fill")
    # }
    if(input$tira_legenda == T){
      p1 = p1 + theme(legend.position = "none")
    }
    
    
    
    p1 = p1 + 
      scale_fill_brewer(type = "div",palette = "Dark2") + 
      scale_color_brewer(type = "div",palette = "Dark2")
    }
    return(p1)
  })
  
  #### Tipo do gráfico ####
  output$graficos_tipo <- renderUI({
    tipos <- c("linha","barra","ponto","area")
    input6 <- NULL
    #input$tipo_grafico = "linha"
    input1 <- radioButtons(inputId = "tipo_grafico",
                           label = "Qual o tipo do gráfico?",
                           choices = tipos,
                           inline = T)
    input2 <- sliderInput(inputId = "grafico_linha",
                          label = "Qual tamanho da linha?",
                          min = 0,
                          max = 10,
                          value = 1,
                          round = F,
                          step = 0.1)
    input3 <- sliderInput(inputId = "tamanho_texto",
                          label = "Qual tamanho dos rótulos numéricos?",
                          min = 0,
                          max = 10,
                          value = 4,
                          round = F,
                          step = 0.1)
    input4 <- checkboxInput("tira_legenda","Remover legenda?",value = F)
    input5 <- checkboxInput("rotulo_acompanha","Rótulo deve acompanhar dados?",value = T)
    input6 <- checkboxInput("escala_proporcional","Participação em relação ao total?",value = T)
    
    list(input1,input2,input3, input4,input5,input6)
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
      return(NULL)
    }
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )
    
  })
  
  #### Verifica se o botão de render foi pressionado ####
  observeEvent(input$renderiza, {
    gif_animado$anima = F
    
    outfile <- tempfile(fileext='.gif')
    # # 
    # # 
    grafico <- grafico_gerado()
    nome_var_x <- names(grafico[[1]][1])
    #
    grafico_animado_p <- grafico + transition_reveal(!!sym(nome_var_x))
    # 
    # animate(grafico_animado,fps = input$tempo_animado, nframe = )
    cat("Ainda não teve erro na linha 445\n\n")
    
    grafico_animado_p <- animate(grafico_animado_p,
                                 fps = input$tempo_animado,
                                 nframe=input$tempo_animado*input$quadros_totais,
                                 end_pause = input$tempo_animado*input$tempo_parado,
                                 width=input$tamanho_X,
                                 height=input$tamanho_Y)
    cat("Não teve erro na linha 445\n\n")
    
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
