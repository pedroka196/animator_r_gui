library(ggplot2)
library(gganimate)
library(gifski)
library(tidyverse)
library(zoo)
library(xlsx)
library(colorspace)
library(rlang)
library(RColorBrewer)
# library(gridExtra)
# library(grid)
# library(extrafont)
# library(png)

separador <- function(dados,variavel_separada,wide){
  indice <- match(variavel_separada,names(dados))
  nomes <- names(dados)
  nomes[indice] <- "Var_X"
  names(dados) <- nomes
  
  if(wide == T) {
  dado_grafico13 <- dados %>%
    gather(key = chaves, value = Valores,-Var_X) %>%
    mutate(chaves = as.factor(chaves))
  }
  else {
    dado_grafico13 <- dados %>%
    gather(key = chaves, value = Valores,-Var_X)
    
    names(dado_grafico13) <- c("chaves","Var_X","Valores")
    dado_grafico13$chaves <- as.factor(dado_grafico13$chaves)
  }
  dado_grafico13$Valores <- as.numeric(dado_grafico13$Valores)
  # dado_grafico13$chaves <- na.exclude(dado_grafico13$chaves)
  # dado_grafico13$Var_X <- na.exclude(dado_grafico13$Var_X)
  # dado_grafico13$Valores <- na.exclude(dado_grafico13$Valores)
  return(dado_grafico13)
}

separador2 <- function(dados,variavel_separada,grupo){
  indice <- match(variavel_separada,names(dados))
  nomes <- names(dados)
  nomes[indice] <- "Var_X"
  names(dados) <- nomes
  
  if(wide == T) {
    dado_grafico13 <- dados %>%
      gather(key = chaves, value = Valores,-Var_X) %>%
      mutate(chaves = as.factor(chaves))
  }
  else {
    dado_grafico13 <- dados %>%
      gather(key = chaves, value = Valores,-Var_X)
    
    names(dado_grafico13) <- c("chaves","Var_X","Valores")
    dado_grafico13$chaves <- as.factor(dado_grafico13$chaves)
  }
  dado_grafico13$Valores <- as.numeric(dado_grafico13$Valores)
  # dado_grafico13$chaves <- na.exclude(dado_grafico13$chaves)
  # dado_grafico13$Var_X <- na.exclude(dado_grafico13$Var_X)
  # dado_grafico13$Valores <- na.exclude(dado_grafico13$Valores)
  return(dado_grafico13)
}


gerador_grafico <-function(Base,X,Y,tipos,nome,grupos){
  # Lista de argumentos
  
  # Tamanho dos argumentos
  tamanho_argumentos <- length(tipos)
  argumentos <- c("linha","ponto")
  for(i in 1:tamanho_argumentos){
    if(tipos[i] %in% argumentos){
      cat("taokei\n\n")
    }
    else{
      stop("Argumento de tipo ", tipos[i], "invalidado")
    }
  }
  tamanho<-length(Y)
  
  if(length(nomes)==tamanho){
    cat("Tamanho de nome ok\n")
  }
  else{
    cat("Nomes estão errados")
  }
  
  # Criando data_frame
  valores <- as.data.frame(X)
  
  if(tamanho>=1){
    for(i in 1:tamanho){
      if(is.numeric(Y[[i]])|is.integer(Y[[i]])){
        if(length(Y[[i]]==length(X))){
            valores <- cbind.data.frame(valores,Y[[i]])
        }
        else{
          stop("O argumento ", names(Y[[i]]), " não tem o mesmo tamanho que X")
        }
      }
      else{
        stop("O argumento ", names(Y[[i]]), " não é data.frame")
      }
    } 
  }
  else{
    stop("Não há argumentos o suficiente")
  }

  nomes_variaveis<-c("X",nomes)
  names(valores)=nomes_variaveis
nome_var_x = names(valores[1])

p <- ggplot(data=valores,aes_string(x=nome_var_x))

cat(class(valores))
for(i in 1:tamanho){
  nome_var_y <-names(valores[i+1])
  
    if(tipos[i]=="linha"){
      p<-p+geom_line(data=valores,aes_string(y=nome_var_y))
    
    }
  if(tipos[i]=="ponto"){
    p<-p+geom_point(data=valores,aes_string(y=nome_var_y))
  }
}


tema <- theme(line = element_line(),
              axis.line.x = element_line(),
              axis.line.y = element_line(),
              title = element_text(family = "Calibri",size = 12,hjust=0.5),
              plot.margin = unit(c(0.5,0.5,0.5,0.5),"cm"),
              plot.caption = element_text(family = "Cambria",hjust=0.5),
              legend.key = element_blank())

p<-p+tema

p<-p+coord_cartesian(ylim = c(0,100))

return(p)
#     geom_line(aes(y=Valores),size=1)+
#     geom_hline(yintercept = 0)+
#     geom_text(aes(y=Valores,
#                   label=paste(as.character(chaves),"\n",(round(Valores,digits = 4)*100),"%"),
#                   x=posicao+2*365),
#               size=3,
#               nudge_y = 0.01)+
#     geom_point(aes(y=Valores),size=2)+
#     labs(title=toupper("Gráfico 13. Resultado primário do setor público consolidado \n acumulado em 12 meses - % do PIB
# "),
#          subtitle = "Ano: {frame_along}",
#          caption="Fonte: Banco Central. Elaboração: IFI")+
#     theme(#title = element_text(size=12),
#       legend.position = "bottom",
#       legend.title = element_blank(),
#       axis.title = element_blank())+
#     coord_cartesian(clip="off")
}


gerador_grafico_2 <-function(base,variaveis,tipos,titulo,fonte,tamanho_fonte,rotulo_acompanha){
  cores<-c("#0C648C","#D2988F","#85AEC3","#BB615C","#CD8E86","#C98B87","#9ABBCE","#d73027",
           "#fdae61","#5aae61")
  # Lista de argumentos
  # if(exists("base$chaves")){
  #   cat("Base positiva e operante\n\n")
  # }
  # else{
  #   stop("Essa base não tem chave")
  # }
  
  cores_usadas <- cores[length(variaveis)]
  if(!exists("rotulo_acompanha")){
    rotulo_acompanha = T
  }
  

  #if(variaveis %in% unique(base$chaves)){
    base_usada <- base %>%
      filter(chaves %in% variaveis)
  # }
  # else(
  #   stop("variaveis não existem")
  # )
  classe_Var_X <-class(base_usada$Var_X)
  if(classe_Var_X[1] == "POSIXct" | classe_Var_X[1] == "POSIXt"){
    base_usada$Var_X <- as.Date(base_usada$Var_X)
  }
  
  if(rotulo_acompanha == T){
    p<-base_usada %>%
      mutate(posicaoX = Var_X,posicaoY = ifelse(Valores>0,1.1*Valores,0.9*Valores)) %>%
      ggplot(aes(x=Var_X,color=chaves,fill=chaves,y=Valores,group=chaves))
  }
  else{
    p<-base_usada %>%
      mutate(posicaoX = max(Var_X),posicaoY = ifelse(Valores>0,Valores+0.1*Valores,Valores-0.1*Valores)) %>%
      ggplot(aes(x=Var_X,color=chaves,fill=chaves,y=Valores,group=chaves))
  }
  
  
  #for(i in length(tipos)){
    if(tipos == "linha"){
      p<-p+geom_line()
    }
    if(tipos == "ponto"){
      p<-p+geom_point()
    }
    if(tipos =="barra"){
      p<-p+geom_col(position = "identity")
    }
  
  p <- p + geom_hline(yintercept = 0) +
      geom_text(aes(y = posicaoY,
                    label = format(round(Valores,digits = 2),big.mark = ".",decimal.mark = ","),
                    x = posicaoX),
                size = tamanho_fonte,
                nudge_y = 0.1,
                nudge_x = +1) +
    theme(#title = element_text(size=12),
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title = element_blank())+
    coord_cartesian(clip="off")
  
  tema <- theme(line = element_line(),
                axis.line.x = element_line(),
                axis.line.y = element_line(),
                title = element_text(family = "Calibri Light",size = 14,hjust=0.5),
                plot.margin = unit(c(0.5,1,0.5,0.5),"cm"),
                plot.caption = element_text(family = "Cambria",hjust=0.5),
                legend.key = element_blank())

  p<-p+labs(title = titulo, caption=fonte)
  
  #p <- p + scale_fill_manual(cores_usadas) + scale_color_manual(cores_usadas)

 # p2<-p+scale_color_manual(values=cores)
  return(p + tema)
}

gerador_grafico_3 <-function(base,X,Y,var_grupo,grupos,tipos,titulo,fonte,tamanho_fonte){
  base <- as.data.frame(base)
  valores <- c(" ","(",")")
  substituto <- c("_","_","_")
  names(base) <- sub(pattern = " ",replacement = "_",x = names(base))
  X <- gsub(pattern = " ",replacement = "_",x = X)
  Y <- gsub(pattern = " ",replacement = "_",x = Y)
  var_grupo <- gsub(pattern = " ",replacement = "_",x = var_grupo)
  
  #var_grupo <- 
  #grupos <- gsub(pattern = " ",replacement = "_",x = grupos)
  
    ### !!sym
  base_usada <- base %>%
    filter((!!sym(var_grupo)) %in% grupos) %>%
    mutate(Posicao = X) %>%
    select(X,Y,var_grupo,Posicao)
  
  # base_usada[Y] <- as.numeric(base_usada[Y])
  
  
  
  #print()
  
  p <- base_usada %>%
    group_by((!!sym(var_grupo))) %>%
    ggplot(aes_string(x=X,y=Y,color=var_grupo,fill=var_grupo))
  
  ### Tipo de gráfico
  if(tipos == "linha"){
    p<-p+geom_line()
  }
  if(tipos == "ponto"){
    p<-p+geom_point()
  }
  if(tipos =="barra"){
    p<-p+geom_col()
  }
  
  p <- p + geom_hline(yintercept = 0) +
    geom_text(aes(y = (!!sym(Y)),
                  label = round((!!sym(Y)),digits = 2),
                  x = (!!sym(X))),
              size = tamanho_fonte,
              nudge_y = 0.01,
              nudge_x = -0.5#,
              #hjust = "outward"
              ) +
    
    theme(#title = element_text(size=12),
      legend.position = "bottom",
      legend.title = element_blank(),
      axis.title = element_blank())+
    coord_cartesian(clip="off")
  
  tema <- theme(line = element_line(),
                axis.line.x = element_line(),
                axis.line.y = element_line(),
                title = element_text(family = "Calibri Light",size = 14,hjust=0.5),
                plot.margin = unit(c(0.5,1,0.5,0.5),"cm"),
                plot.caption = element_text(family = "Cambria",hjust=0.5),
                legend.key = element_blank())
  
  p<-p+labs(title = titulo, caption=fonte)
  
  
  
  return(p)
}
