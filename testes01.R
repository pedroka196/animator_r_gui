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
  ggplot(aes(x=Ano,y=`Pago e RP Pago IPCA`,
             #color = `Subfunção (Ajustada)`,
             fill=`Subfunção Ajustada`))+
  geom_area(position = "fill")+
  scale_fill_brewer(type="div")+
  #scale_color_brewer(type="div")+
  theme(legend.title = element_blank(),legend.position = "bottom")+
  geom_text(aes(label = format(`Pago e RP Pago IPCA`/1000000,big.mark = ".",decimal.mark = ",",digits = 0)),position = "fill",size=3,vjust=1)

names(dados_educ)

p_educ <- gerador_grafico_3(base = dados_educ,
                            X = "Ano",
                            Y = "Pago e RP Pago IPCA",
                            var_grupo = "Subfunção Ajustada",
                            grupos = c("EDUCAÇÃO BÁSICA","ENSINO SUPERIOR","ENSINO PROFISSIONAL"),
                            tipos = "linha",
                            titulo = "UAU",
                            fonte = "UAU2",
                            tamanho_fonte = 3,
                            rotulo_acompanha = T,
                            proporcao = T)

p_educ+theme(text = element_text(color = "orange"))
p_educ 
p_educ_anim <- p_educ + transition_reveal(!!sym("Ano"))
animate(p_educ_anim,nframes = 25)
base<-dados_educ


#### RAF ####

dados_raf <- read_excel("RAF28_MAIO2019_Graficos_Tabelas (1).xlsx",
                        sheet = 14,
                        skip = 3,
                        n_max = 197)
nomes_raf <-names(dados_raf)

dados_grupo <- separador(dados = dados_raf,variavel_separada = nomes_raf[1],T)



grafico_raf <- gerador_grafico_2(base = dados_grupo,
                                 variaveis = nomes_raf[2:5],
                                 tipos = "linha",
                                 titulo = "EUU",
                                 fonte = "EUSJKK",
                                 tamanho_fonte = 2,
                                 rotulo_acompanha = T,
                                 proporcao = F)

grafico_raf+theme(axis.line = element_line(colour = "#858585"))

animacao_grafico_raf <- grafico_raf + transition_reveal(Var_X)

animate(animacao_grafico_raf,nframes = 25)

names(grafico_raf[[1]][1])

#### dados ciencia e tec ####
dados_cti <- read_excel("CIENCIA_E_TEC.xlsx",sheet = 1)

nomes_cti = names(dados_cti)

grafico_cti <- gerador_grafico_3(base = dados_cti,
                                 X = nomes_cti[1],
                                 Y = nomes_cti[4],
                                 tipos = "linha",
                                 var_grupo = nomes_cti[2],
                                 grupos = "DESENVOLVIMENTO CIENTÍFICO",
                                 rotulo_acompanha = T,
                                 proporcao = T,
                                 titulo = "uheuheu",
                                 fonte = "asuhaus",
                                 tamanho_fonte = 2)
