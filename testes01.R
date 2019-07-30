dados <- read_excel("siga.xlsx")

dados_educ <- read_excel("siga_educacao.xlsx")


dados2 <- dados %>%
  #mutate(`Função DESP` = as.factor(`Função DESP`)) %>%
  #select(-`Subfunção (Ajustada)`) %>%
  gather(key = `FunçãoDESP`,-Ano)

names(dados) <- c("Ano","FunçãoDESP", "Dotação_Inicial_IPCA","Pago_RP_Pago_IPCA")
p1 <- dados %>%
  group_by(`FunçãoDESP`) %>%
  ggplot(aes(x=Ano,y=Pago_RP_Pago_IPCA,fill=`FunçãoDESP`,colour=`FunçãoDESP`))+
  #geom_bar(stat="Identity",position = "identity")
  geom_col(position = "fill")+
  labs(title = "{closest_state}")

animate(p1+transition_states(Ano)+enter_fade()+ease_aes()+view_follow(fixed_x = TRUE,fixed_y = T),nframes = 15)

p3 <- gerador_grafico_3(base = dados,
                       X = "Ano",
                       Y = "Dotação_Inicial_IPCA",
                       var_grupo = "FunçãoDESP",
                       grupos = c("SAÚDE","SANEAMENTO","GESTÃO AMBIENTAL"),
                       tipos = "linha",
                       titulo = "UAU",
                       fonte = "UE",tamanho_fonte = 5)

p3

a <-"FunçãoDESP"
p2 <- dados %>%
  filter_("Saúde" %in% "FunçãoDESP")
  # ggplot(aes_string(x="Ano",y="Dotação_Inicial_IPCA",color = "FunçãoDESP"))+
  # geom_line()
p2


p_educ <- dados_educ %>%
  ggplot(aes(x=Ano,y=`Pago e RP Pago (IPCA)`,
             #color = `Subfunção (Ajustada)`,
             fill=`Subfunção (Ajustada)`))+
  geom_col(position = "fill")+
  scale_fill_brewer(type="div")+
  #scale_color_brewer(type="div")+
  theme(legend.title = element_blank(),legend.position = "bottom")+
  geom_text(aes(label = format(`Pago e RP Pago (IPCA)`/1000000,big.mark = ".",decimal.mark = ",",digits = 0)),position = "fill",size=3,vjust=1)

p_educ
