prepara_serie_fft<-
  function (x, spans = NULL, kernel = NULL, taper = 0.1, pad = 0,
          fast = TRUE, demean = FALSE, detrend = TRUE, plot = TRUE,
          na.action = na.fail, ...){
  series <- deparse1(substitute(x))
  x <- na.action(as.ts(x))
  xfreq <- frequency(x)
  x <- as.matrix(x)
  N <- N0 <- nrow(x)
  nser <- ncol(x)
  if (!is.null(spans))
    kernel <- {
      if (is.tskernel(spans))
        spans
      else kernel("modified.daniell", spans%/%2)
    }
  if (!is.null(kernel) && !is.tskernel(kernel))
    stop("must specify 'spans' or a valid kernel")
  if (detrend) {
    t <- 1L:N - (N + 1)/2
    sumt2 <- N * (N^2 - 1)/12
    for (i in 1L:ncol(x)) x[, i] <- x[, i]    - # - mean(x[, i])
      sum(x[, i] * t) * t/sumt2
  }
  else if (demean) {
    x <- sweep(x, 2, colMeans(x), check.margin = FALSE)
  }
  x <- spec.taper(x, taper)
  u2 <- (1 - (5/8) * taper * 2)
  u4 <- (1 - (93/128) * taper * 2)
  if (pad > 0) {
    x <- rbind(x, matrix(0, nrow = N * pad, ncol = ncol(x)))
    N <- nrow(x)
  }
  NewN <- if (fast)
    nextn(N)
  else N
  x <- rbind(x, matrix(0, nrow = (NewN - N), ncol = ncol(x)))
  c(x)
}


plot_series_transformadas_analitico<-function(.data,freq,
                                              levels,labels,
                                              ano,
                                              mes=1:12,
                                              mes_destaque,
                                              exibe_textos_eixos=FALSE){

  library(ggrepel)

  var_ano<-ano
  var_mes<-mes


  analitico<-
    .data %>%
    filter(h%in%freq,
           ano %in% var_ano,
           mes %in%  var_mes)%>%
    mutate(h = factor(h, levels = levels, labels = labels),
           ano= stringr:: str_c("ano"," ",str_pad(ano,2,pad="0")))


  destaque<-
    .data %>%
    filter(h%in%freq,
           ano %in% var_ano,
           mes== mes_destaque)%>%
    mutate(h = factor(h, levels = levels, labels = labels),
           ano= stringr:: str_c("ano"," ",str_pad(ano,2,pad="0")))

  g<-
  analitico %>%
    ggplot(aes(x=real, y=imaginario))+
    geom_vline(xintercept = 0, color="white",linetype= "dotted")+
    geom_hline(yintercept = 0, color="white",linetype= "dotted")+
    geom_point(aes(fill=h),pch=21, color="#444444", size=2, alpha=1) +
    geom_segment(aes(x=0,xend=real,y=0,yend=imaginario, color =h))+
    geom_text_repel(data= destaque, aes(label = round(z,0), color=h), size=2.5)+
    facet_wrap(as.factor(ano)~.)+
    scale_fill_discrete_qualitative(palette = "Dark 2")+
    scale_color_discrete_qualitative(palette = "Dark 2")+
    theme_light()+
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#15202B"),
      legend.position = "bottom",
      strip.background = element_rect(fill = "#505050"),
      strip.text = element_text(margin = margin(0.03,0,0.03,0, "cm"), size=8),
      axis.text = element_text(size = 6) ,
      axis.title = element_blank()
    )+
    labs(
      color= "Frequência",
      fill= "Frequência"
    )

  if(!exibe_textos_eixos){
    g<-
      g+
      theme(
        axis.text = element_blank()
      )
  }

  g

}



plot_series_transformadas_acumulado<-function(.data,
                                              freq,
                                              levels,
                                              labels,
                                              ano,
                                              mes=1:12,
                                              exibe_textos_eixos = FALSE){

  library(ggrepel)

  var_ano<-ano
  var_mes<-mes

  acum<-
    .data %>%
    filter(h%in%freq,
           ano %in% var_ano,
           mes %in%  var_mes) %>%
    mutate(h = factor(h, levels = levels, labels = labels),
           ano= stringr:: str_c("ano"," ",str_pad(ano,2,pad="0"))) %>%
    group_by(h,ano) %>%
    summarise(
      real= sum(real),
      imaginario = sum(imaginario),
      modulo = sqrt(real^2+imaginario^2))

  g<-
  acum %>%
    ggplot(aes(x=real, y=imaginario))+
    facet_wrap(as.factor(ano)~.)+
    geom_point(aes(x=real, y=imaginario, color=h))+
    geom_segment(aes(x=0,xend=real,y=0,yend=0, color =h),linetype= "dashed")+
    geom_segment(aes(x=0,xend=0,y=0,yend=imaginario, color =h),linetype= "dashed")+
    geom_segment(aes(x=0,xend=real,y=0,yend=imaginario, color =h))+
    geom_text_repel(data = acum,aes(label = round(modulo,0),color= h), size=2.5,
                    force_pull = 0.1)+
    scale_color_discrete_qualitative(palette = "Dark 2")+
    theme_light()+
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#15202B"),
      legend.position = "bottom",
      strip.background = element_rect(fill = "#505050"),
      strip.text = element_text(margin = margin(0.03,0,0.03,0, "cm"), size=8),
      axis.text = element_text(size = 6),
      axis.title = element_blank()
    )+
    labs(
      color= "Frequência",
      fill= "Frequência"
    )

  if (!exibe_textos_eixos){
    g<-
      g+
      theme(
        axis.text = element_blank()
      )
  }

  g

}


plot_series_direcao<-function(.data,freq, ano, mes=12, dimensao=1000 ){

  library(ggrepel)

  var_ano<-ano
  var_mes<-mes


  analitico<-
    .data %>%
    filter(h%in%freq,
           ano %in% var_ano,
           mes %in%  var_mes) %>%
    mutate(real = dimensao*cos((h-1)*Im(ff)),
           imaginario = dimensao*sin((h-1)*Im(ff)) ) %>%
    mutate(h = as.factor(h))

  analitico %>%
    ggplot(aes(x=real, y=imaginario))+
    geom_point(aes(color=h), alpha=0.8) +
    geom_text_repel(aes(label = mes))+
    facet_wrap(as.factor(ano)~.)+
    geom_vline(xintercept = 0)+
    geom_hline(yintercept = 0)

}


plot_series_direcao_freq_facet<-function(.data,freq,levels,labels, mes=1:12, dimensao=1000 ){

  library(ggrepel)
  library(viridis)
  library(colorspace)

  var_mes<-mes


  analitico<-
    .data %>%
    filter(h%in%freq,
           mes %in%  var_mes,
           ano==1) %>%
    mutate(real = dimensao*cos((h-1)*Im(ff)),
           imaginario = dimensao*sin((h-1)*Im(ff)) ) %>%
    mutate(h = factor(h, levels = levels, labels = labels))

  analitico %>%

    ggplot(aes(x=real, y=imaginario))+
    geom_vline(xintercept = 0, color= "white",linetype= "dotted")+
    geom_hline(yintercept = 0, color= "white",linetype= "dotted")+
    geom_point(size=3, color="white") +
    geom_text_repel(aes(label = mes),color="white", max.overlaps = 14, force_pull = 30, force = 20, size=2.5)+
    facet_wrap(h~.) +
    theme_light()+
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#15202B"),
      legend.position = "none",
      strip.background = element_rect(fill = "#505050"),
      axis.text = element_text(size = 6)
    )+
    labs(
      color= "Frequência"
    )

}


plot_series_transformadas_total<-function(.data,
                                          freq,
                                          levels,
                                          labels,
                                          ano,
                                          mes=1:12,
                                          exibe_textos_eixos = FALSE){

  library(ggrepel)

  var_ano<-ano
  var_mes<-mes

  acum<-
    .data %>%
    filter(h%in%freq,
           ano %in% var_ano,
           mes %in%  var_mes)  %>%
    mutate(h = factor(h, levels = levels, labels = labels))%>%
    group_by(h) %>%
    summarise(
      real= sum(real),
      imaginario = sum(imaginario),
      modulo = sqrt(real^2+imaginario^2))

  g<-
  acum %>%
    ggplot(aes(x=real, y=imaginario))+
    geom_point(aes(x=real, y=imaginario, color=h))+
    geom_segment(aes(x=0,xend=real,y=0,yend=0, color =h),linetype= "dashed")+
    geom_segment(aes(x=0,xend=0,y=0,yend=imaginario, color =h),linetype= "dashed")+
    geom_segment(aes(x=0,xend=real,y=imaginario,yend=imaginario, color =h),linetype= "dashed")+
    geom_segment(aes(x=real,xend=real,y=0,yend=imaginario, color =h),linetype= "dashed")+
    geom_segment(aes(x=0,xend=real,y=0,yend=imaginario, color =h),  size=1.2)+
    geom_text_repel(aes(label = round(modulo,0),color= h), size=2.5)+
    geom_text_repel(aes(x=0, y=0,label = 0,color= h),nudge_x = 200, size=2.5)+
    scale_color_discrete_qualitative(palette = "Dark 2")+
    theme_light()+
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#15202B"),
      legend.position = "bottom",
      axis.text = element_text(size = 6) ,
      axis.title = element_blank()
    )+
    labs(
      color= "Frequência",
      fill= "Frequência"
    )

  if (!exibe_textos_eixos){
    g<-
      g+
      theme(
        axis.text = element_blank()
      )
  }

  g

}


plot_seasonality<- function(.data, value_type="1", clean_names = TRUE, polar = TRUE, first_year){

  nome_variavel_referencia<- stringr::str_replace_all(names(.data)[1], "-", " ")
  names(.data)[1]<-"Rubrica"
  column<- ifelse(value_type==1, "valor_atualizado", "valor_historico")
  texto_eixo_y<- ifelse(value_type==1, "valor atualizado", "valor histórico")

  if (clean_names){

    .data<-
      .data%>%
      dplyr::mutate(Rubrica =  stringr::str_trim( stringr::str_remove_all(Rubrica,"[:punct:]|[0-9]|[:symbol:]")))

  }

  values<-.data[,2]

  conta<- unique(.data$Rubrica)

  if (length(conta)>1){
    stop("Error. |The dataset must refer to just one account")
  }

  forecast::ggseasonplot(x= ts(data = values/10^6,frequency = 12, start = c(first_year,1)),  polar = polar, lwd=5)+
    ggplot2::ylab("R$ milhões") +
    #ggplot2::ggtitle(paste0("Seasonality graph - ",conta)) +
    colorspace::scale_color_discrete_sequential(palette = "PinkYl")+
    ggplot2::theme_light() +
    ggplot2::theme(
      title = element_blank(),
      panel.background = ggplot2::element_rect(fill= "black") ,
      axis.text.x =  ggplot2::element_text(color = "white")
    )

}


plot_slt<- function(.data, first_year){

  names(.data)[2]<-"values"

  values<-.data$values

  conta<- unique(.data$Rubrica)

  ts<- ts(data = values/10^6,frequency = 12, start = c(first_year,1))

  stl_ts = stl(ts, "periodic")
  seasonal_stl   <- stl_ts$time.series[,1]
  trend_stl     <- stl_ts$time.series[,2]
  random_stl  <- stl_ts$time.series[,3]

  plot(ts)
  plot(as.ts(seasonal_stl))
  plot(trend_stl)
  plot(random_stl)
  plot(stl_ts)

}


#Detecção de sazonalidades usando fourier
get_seasonable_series<- function(.data, gatilho=2){

  names(.data)[1]<- "rubrica"
  names(.data)[2]<- "total"

  rubricas<-unique(.data$rubrica)

  .data<-
    purrr::map_dfr(rubricas, function(id_rubrica){
      print(id_rubrica)

      data_fourier<-
        .data %>%
        filter(rubrica == id_rubrica)

      if (NROW(data_fourier)<=12) {
        return()
      }

      # compute the Fourier Transform
      #p <- periodogram(data_fourier$total, plot = FALSE)
      p<- spec.pgram(data_fourier$total, plot = FALSE)
      dd <- data.frame(series= id_rubrica,  freq=p$freq, spec=p$spec)
      dd$time<- 1/dd$freq

      cvg<- sqrt(exp(sd(log(dd$spec))^2-1))

      print(cvg)
      if (cvg>gatilho){
        tibble(serie=id_rubrica,periodo= dd$time[dd$spec==max(dd$spec)] )
      } else{

        return()
      }

    })
}



#Detecção de sazonalidades usando fourier
data_seasonable_periodgram<- function(.data, gatilho=2){

  names(.data)[1]<- "rubrica"
  names(.data)[2]<- "total"

  rubricas<-unique(.data$rubrica)

  .data<-
    purrr::map_dfr(rubricas, function(id_rubrica){
      print(id_rubrica)

      data_fourier<-
        .data %>%
        filter(rubrica == id_rubrica)

      if (NROW(data_fourier)<=12) {
        return()
      }

      # compute the Fourier Transform
      p<- spec.pgram(data_fourier$total, plot = FALSE)
      dd <- data.frame(series= id_rubrica,  freq=p$freq, spec=p$spec)
      dd$time<- 1/dd$freq

      cvg<- sqrt(exp(sd(log(dd$spec))^2-1))

      print(cvg)
      if (cvg>gatilho){
        dd$serie<- rep(id_rubrica,NROW(dd))
        dd

      } else{

        return()
      }

    })
}


plot_periodgram<- function(.data,
                           exibe_texto_eixo_y=FALSE,
                           texto_frequencia_fundamental= ""){
  g<-
  .data %>%
    filter(time<=12)%>%
    ggplot(aes(x=time, y=spec)) +
    geom_line(color="white")+
    scale_x_continuous(breaks = seq(1,12,1))+
    theme_light()+
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#15202B"),
      legend.position = "bottom",
      axis.text = element_text(size = 6),
      axis.title = element_blank()
    ) +
    labs(
      x="Período",
      y="Espectro"
    )

  if(!exibe_texto_eixo_y){
    g<- g+
      theme(
        axis.text.y = element_blank()
      )
  }

  if(texto_frequencia_fundamental!=""){

    max_spec<- max(.data$spec)
    pos_freq_fundamental<-
      .data%>%
      filter(spec==max_spec )

    g<-
      g+
      geom_point(data=pos_freq_fundamental,
                 color = "white")+
      geom_text_repel(data=pos_freq_fundamental,
                      label= texto_frequencia_fundamental,
                      color ="white")
  }



  g
}

## Slow Discrete Fourier Transform (DFT) - e.g., for checking the formula
fft0_v2 <- function(z, inverse=FALSE) {
  n <- length(z)
  if(n == 0) return(z)
  k <- 0:(n-1)
  ff <- (if(inverse) 1 else -1) * 2*pi * 1i * k/n

  #soma<-NULL
  purrr::map_dfr (1:n,function(h){
    tibble(h=h,
           z=z,
           ff=ff,
           real=Re(z * exp(ff*(h-1))),
           imaginario= Im(z * exp(ff*(h-1))),
           modulo= abs(z * exp(ff*(h-1))),
           fase= atan(Im(z * exp(ff*(h-1)))/Re(z * exp(ff*(h-1)))))
  })

  #for (h in 1:n){
     #soma<-c(soma,sum(z * exp(ff*(h-1)) + complex(1)))
  #}

  #soma

  #vapply(1:n, function(h) sum(z * exp(ff*(h-1))), complex(1))
}

