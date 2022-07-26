
#Not Run
#please install rtn package and see some possibilities of using a set of time series about Brazilian expenses and revenues

# install.packages("devtools")
devtools::install_github("tchiluanda/rtn")

#Not Run

time_series<- "1.3 - Arrecadação Líquida para o RGPS" #inform a time series

serie_artigo<-
  rtn::get_account_data_by_month(time_series, month = 1:12, match_required = FALSE) %>%
  filter(Data<="2019-12-31") %>%
  arrange(Data)




serie_artigo %>%
  select(Data, valor_atualizado) %>%
  ggplot()+
  geom_line(aes(x=Data, y= valor_atualizado), color = "white") +
  theme_light()+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.title = element_blank()
  )


serie_artigo %>%
  select(Rubrica, valor_atualizado) %>%
  plot_seasonality(first_year = 1997)



serie_artigo %>%
  select(Rubrica, valor_atualizado) %>%
  data_seasonable_periodgram() %>%
  plot_periodgram(texto_frequencia_fundamental = "Sazonalidade mais importante")

dtrend_serie_artigo<- prepara_serie_fft(serie_artigo$valor_atualizado)


dtrend_serie_artigo<-tibble(time=seq(1:NROW(dtrend_serie_artigo)), value=dtrend_serie_artigo)

g_artigo<-
  dtrend_serie_artigo %>%
  ggplot() +
  geom_line(aes(x=time, y=value))

g_artigo

dtrend_serie_artigo$mes<-rep(1:12,NROW(dtrend_serie_artigo)/12)


detail_artigo<-
  fft0_v2(dtrend_serie_artigo$value)


detail_artigo$mes<-rep(1:12,NROW(detail_artigo)/12)

ano<- (purrr::map_dfr(1:(NROW(dtrend_serie_artigo)/12),function(x){tibble(ano=rep(x,12))}))$ano

detail_artigo$ano<- rep(ano,length(unique(detail_artigo$h)))



detail_artigo %>%
  plot_series_direcao_freq_facet (freq=c(145,97,73,49,25),
                                  levels=c(145,97,73,49,25),
                                  labels=c("bimestral","trimestral","quadrimestral","semestral", "anual"),
                                  mes=1:12,
                                  dimensao=1000)


detail_artigo %>%
  plot_series_transformadas_analitico(freq=c(145,25),
                                      levels=c(145,25),
                                      labels=c("bimestral","anual"),
                                      mes=1:12,
                                      ano=1:23,
                                      mes_destaque = 12)

detail_artigo %>%
  plot_series_transformadas_acumulado (freq=c(145,25),
                                       levels=c(145,25),
                                       labels=c("bimestral","anual"),
                                       mes=1:12,
                                       ano=1:23)


detail_artigo %>%
  plot_series_transformadas_total (freq=c(145,25),
                                   levels=c(145,25),
                                   labels=c("bimestral","anual"),
                                   mes=1:12,
                                   ano=1:23)


dtrend_serie_artigo %>%
  mutate(mes= as.factor(mes)) %>%
  group_by(mes) %>%
  summarise(
    value= sum(value)
  ) %>%
  ggplot() +
  geom_col(
    aes(x=mes, y=value),fill= "white"
  )+
  theme_light()+
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#15202B"),
    legend.position = "bottom",
    axis.text = element_text(size = 6),
    axis.title = element_blank()
  )



modulo<- function(real,imaginario){
  sqrt(real^2+imaginario^2)
}

