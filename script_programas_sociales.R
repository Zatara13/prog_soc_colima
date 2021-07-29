## Cobertura y distribución de los programas sociales en Colima, 2018-2020
## NOTA: En las líneas 132 y 173 hay notas relevantes para el funcionamiento del código
## En caso de error, revisar la anotación y hacer los cambios sugeridos
## Por: Zatara


################### Librerías de trabajo ###############
## Función para descargar paquetes en automático
foo <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}


## Cargamos librerías de trabajo
foo(c("readr", ## leer bases de datos en formato csv
      "tidyverse", ## manipulación de bases de datos
      "ggsci", ## colores bonitos para gráficas I
      "ggthemes", ## Colors bonitos para gráficas II
      "sf", ## Mapas con lógica ggplot
      "srvyr", ## Estimaciones de encuestas
      "kableExtra", ## Crear tablas bonitas
      "doBy" ## Paquete que usa INEGI para estimar deciles de ingreso
      ))

################## Descarga de datos de trabajo ##############
## 2018
url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_concentradohogar_csv.zip"

##Creación de directorio temporal
td<- tempdir()

# Descarga del archivo temporal
tf = tempfile(tmpdir=td,
              fileext=".zip")
download.file(url,
              tf)

# descomprimir
unzip(tf,
      files="concentradohogar.csv",
      exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,
                "concentradohogar.csv")
unlink(td)

#Leer el archivo
concentrado_hogar_2018<-read.csv(fpath)

## 2020
url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_concentradohogar_csv.zip"

##Creación de directorio temporal
td<- tempdir()

# Descarga del archivo temporal
tf = tempfile(tmpdir=td,
              fileext=".zip")
download.file(url,
              tf)

# descomprimir
unzip(tf,
      files="concentradohogar.csv",
      exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,
                "concentradohogar.csv")
unlink(td)

#Leer el archivo
concentrado_hogar_2020<-read.csv(fpath)

## Limpiar área de trabajo
rm(foo,
   fpath,
   td,
   tf,
   url)

################ Procesamiento de datos para Colima ##################

## ENIGH 2018
enigh18_colima <- concentrado_hogar_2018 %>% 
  ## Filtramos para Colima
  filter(ubica_geo %in% c(6001,
                          6002,
                          6003,
                          6004,
                          6005,
                          6006,
                          6007,
                          6008,
                          6009,
                          6010)) 

## ENIGH 2020
enigh20_colima <- concentrado_hogar_2020 %>% 
  ## Filtramos para Colima
  filter(ubica_geo %in% c(6001,
                          6002,
                          6003,
                          6004,
                          6005,
                          6006,
                          6007,
                          6008,
                          6009,
                          6010)) 

## Eliminamos archivos no necesarios
rm(concentrado_hogar_2018,
   concentrado_hogar_2020)

##################### Deciles de Ingreso para Análisis ####################
## Identificamos deciles de ingreso en BD
## Para calcular los deciles de ingreso, utilicé la recomendación sugerida en el descriptor de cálculo diseñado por el INEGI
## Aquí está el enlace del documento base: https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/doc/enigh2020_ns_descripcion_calculo_r.pdf
## Adapté el código de la página 7

## 2018
# Deja activa la tabla enigh18_colima
attach(enigh18_colima)
# Ordena Conc de acuerdo a ing_cor,folioviv,foliohog.
## NOTA: Por algún motivo extraño, mi computadora guarda la variable folioviv como ï..folioviv.
## Si el código te da problemas en la siguiente línea, cambia la variable ï..folioviv por folioviv.
enigh18_colima<- orderBy(~+ing_cor+ï..folioviv+foliohog, data=enigh18_colima)
# Suma todos los factores y guarda el valor en el vector tot_hogares.
tot_hogares <- sum(factor)
# Se divide la suma de factores entre diez para sacar el tamaño del decil 
#(se debe de truncar el resultado quitando los decimales).
tam_dec<-trunc(tot_hogares/10)
# Muestra la suma del factor en variable hog.
enigh18_colima$tam_dec=tam_dec
# Se renombra la tabla concentrado a BD1.
BD1 <- enigh18_colima
# Dentro de la tabla BD1 se crea la variable MAXT y se le asigna los valores que tienen el ing_cor.
BD1$MAXT<-BD1$ing_cor
# Se ordena de menor a mayor segun la variable MAXT.
BD1<-BD1[with(BD1, order(rank(MAXT))),]
# Se aplica la función cumsum, suma acumulada a la variable factor.
BD1$ACUMULA<-cumsum(BD1$factor)
### Entra a un ciclo donde iremos generando los deciles 1 a 10.
for(i in 1:9){
  a1<-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1,]$factor
  BD1<-rbind(BD1[1:(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),],BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])
                                                                  [1]+1):dim(BD1[1])[1],])
  b1<-tam_dec*i-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}
BD1$ACUMULA2<-cumsum(BD1$factor)
BD1$DECIL<-0
BD1[(BD1$ACUMULA2<=tam_dec),]$DECIL<-1
for(i in 1:9){
  BD1[((BD1$ACUMULA2>tam_dec*i)&(BD1$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}
BD1[BD1$DECIL%in%"0",]$DECIL<-10

enigh18_colima <- BD1

## 2020
# Deja activa la tabla enigh18_colima
attach(enigh20_colima)
# Ordena Conc de acuerdo a ing_cor,folioviv,foliohog.
## NOTA: Por algún motivo extraño, mi computadora guarda la variable folioviv como ï..folioviv.
## Si el código te da problemas en la siguiente línea, cambia la variable ï..folioviv por folioviv
enigh20_colima<- orderBy(~+ing_cor+ï..folioviv+foliohog, data=enigh20_colima)
# Suma todos los factores y guarda el valor en el vector tot_hogares.
tot_hogares <- sum(factor)
# Se divide la suma de factores entre diez para sacar el tamaño del decil 
#(se debe de truncar el resultado quitando los decimales).
tam_dec<-trunc(tot_hogares/10)
# Muestra la suma del factor en variable hog.
enigh20_colima$tam_dec=tam_dec
# Se renombra la tabla concentrado a BD1.
BD1 <- enigh20_colima
# Dentro de la tabla BD1 se crea la variable MAXT y se le asigna los valores que tienen el ing_cor.
BD1$MAXT<-BD1$ing_cor
# Se ordena de menor a mayor segun la variable MAXT.
BD1<-BD1[with(BD1, order(rank(MAXT))),]
# Se aplica la función cumsum, suma acumulada a la variable factor.
BD1$ACUMULA<-cumsum(BD1$factor)
### Entra a un ciclo donde iremos generando los deciles 1 a 10.
for(i in 1:9){
  a1<-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1,]$factor
  BD1<-rbind(BD1[1:(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),],BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])
                                                                  [1]+1):dim(BD1[1])[1],])
  b1<-tam_dec*i-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}
BD1$ACUMULA2<-cumsum(BD1$factor)
BD1$DECIL<-0
BD1[(BD1$ACUMULA2<=tam_dec),]$DECIL<-1
for(i in 1:9){
  BD1[((BD1$ACUMULA2>tam_dec*i)&(BD1$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}
BD1[BD1$DECIL%in%"0",]$DECIL<-10

enigh20_colima <- BD1

rm(BD1,
   a1,
   b1,
   i,
   tam_dec,
   tot_hogares)

########################### Distribución de programas sociales en Colima ##########################

## Creamos variable para identificar los hogares a los que llegan los programas sociales
## 2018
enigh18_colima <- enigh18_colima %>% 
  mutate(prog_soc = case_when(bene_gob > 0 ~ 1,
                              TRUE ~ 0))

## 2020
enigh20_colima <- enigh20_colima %>% 
  mutate(prog_soc = case_when(bene_gob > 0 ~ 1, 
                              TRUE ~ 0))


## Empezamos las estimaciones
## 2018
## Diseño muestral 2018
design18 <- enigh18_colima %>% 
  as_survey_design(ids=upm,
                   strata=est_dis,
                   weights=factor)

## Por totales
## Hogares en Colima en 2018
hogs_decil_18t <- design18 %>% 
  summarise(hogares=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(hogares_cv=
           hogares_cv*100
  ) %>% 
  mutate(año = 2018)

## Hogares a los que llegaba un programa social en 2018
prog_soc18t <- design18 %>%
  filter(prog_soc==1)%>%
  summarise(prog_soc=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(prog_soc_cv=
           prog_soc_cv*100,
  ) %>% 
  mutate(año = 2018)

## Porcentaje de hogares a los que llegaba un programa social en 2019
prog_soc_18_pcth_t <- merge(hogs_decil_18t,
                          prog_soc18t,
                          by = "año") %>% 
  mutate(pct_hog_ps = prog_soc / hogares * 100) %>% 
  mutate(año = 2018) %>% 
  select(año,
         pct_hog_ps,
         prog_soc_cv)

## Por deciles
## Hogares por decil de ingresos en 2018
hogs_decil_18 <- design18 %>% 
  group_by(DECIL) %>% 
  summarise(hogares=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(hogares_cv=
           hogares_cv*100
  )

## Hogares a los que llegaba un programa social en 2018 por decil de ingresos
prog_soc18 <- design18 %>%
  filter(prog_soc==1)%>%
  group_by(DECIL) %>% 
  summarise(prog_soc=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(prog_soc_cv=
           prog_soc_cv*100,
  )

## Unimos tablas para calcular porcentajes de hogares a los que llega un programa social por decil
prog_soc_18_pcth <- merge(hogs_decil_18,
                          prog_soc18,
                          by = "DECIL") %>% 
  mutate(pct_hog_ps = prog_soc / hogares * 100) %>% 
  mutate(año = 2018) %>% 
  select(año,
         DECIL,
         pct_hog_ps,
         prog_soc_cv)

## Distribución de programas sociales en deciles de ingreso.
prog_soc18_dist <-  design18 %>%
  filter(prog_soc==1)%>%
  group_by(DECIL) %>% 
  summarise(prog_soc=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(prog_soc_cv=
           prog_soc_cv*100,
  ) %>% 
  mutate(prog_soc_dist = prog_soc / 53695 * 100) %>% 
  mutate(año = as.factor(2018)) %>% 
  select(año,
         DECIL,
         prog_soc_dist,
         prog_soc_cv)

## 2020
## Diseño muestral 2020
design20 <- enigh20_colima %>% 
  as_survey_design(ids=upm,
                   strata=est_dis,
                   weights=factor)

## Hogares a los que llegaba un programa social en 2020

## Por totales
## Hogares en Colima en 2020
hogs_decil_20t <- design20 %>% 
  summarise(hogares=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(hogares_cv=
           hogares_cv*100
  ) %>% 
  mutate(año = 2020)

## Hogares a los que llegaba un programa social en 2020
prog_soc20t <- design20 %>%
  filter(prog_soc==1)%>%
  summarise(prog_soc=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(prog_soc_cv=
           prog_soc_cv*100,
  ) %>% 
  mutate(año = 2020)

## Porcentaje de hogares a los que llegaba un programa social en 2019
prog_soc_20_pcth_t <- merge(hogs_decil_20t,
                            prog_soc20t,
                            by = "año") %>% 
  mutate(pct_hog_ps = prog_soc / hogares * 100) %>% 
  mutate(año = 2020) %>% 
  select(año,
         pct_hog_ps,
         prog_soc_cv)

## Por deciles
## Hogares por decil de ingresos en 2020
hogs_decil_20 <- design20 %>% 
  group_by(DECIL) %>% 
  summarise(hogares=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(hogares_cv=
           hogares_cv*100
  )

## Hogares a los que llegaba un programa social en 2020
prog_soc20 <- design20 %>%
  filter(prog_soc==1)%>%
  group_by(DECIL) %>% 
  summarise(prog_soc=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(prog_soc_cv=
           prog_soc_cv*100,
  )

## Unimos tablas para calculas porcentajes
prog_soc_20_pcth <- merge(hogs_decil_20,
                          prog_soc20,
                          by = "DECIL") %>% 
  mutate(pct_hog_ps = prog_soc / hogares * 100) %>% 
  mutate(año = 2020) %>% 
  select(año,
         DECIL,
         pct_hog_ps,
         prog_soc_cv)

## Distribución de programas sociales en deciles de ingreso.
prog_soc20_dist <-  design20 %>%
  filter(prog_soc==1)%>%
  group_by(DECIL) %>% 
  summarise(prog_soc=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(prog_soc_cv=
           prog_soc_cv*100,
  ) %>% 
  mutate(prog_soc_dist = prog_soc / 63565 * 100) %>% 
  mutate(año = as.factor(2020)) %>% 
  select(año,
         DECIL,
         prog_soc_dist,
         prog_soc_cv)



############## Comparación de estimados #############
## Por totales
prog_soc_pct_tot <- bind_rows(prog_soc_18_pcth_t,
                              prog_soc_20_pcth_t)

## Por porcentanje de hogares a los que llegan programas sociales, de acuerdo a deciles
prog_soc_pct_dec <- bind_rows(prog_soc_18_pcth,
                          prog_soc_20_pcth) %>% 
  mutate(año = as.factor(año))

## Distribución de los programas sociales de acuerdo a deciles
prog_soc_pct_dist <- bind_rows(prog_soc18_dist,
                               prog_soc20_dist) %>% 
  mutate(prog_soc_dist = as.double(format(round(prog_soc_dist,
                                      2),
                                nsmall = 2))) %>% 
  mutate(DECIL = as.factor(DECIL))

## Eliminados archivos redundantes con las tablas de comparación
rm(design18,
   design20,
   enigh18_colima,
   enigh20_colima,
   hogs_decil_18,
   hogs_decil_18t,
   hogs_decil_20,
   hogs_decil_20t,
   prog_soc_18_pcth,
   prog_soc18_dist,
   prog_soc_20_pcth,
   prog_soc_20_pcth_t,
   prog_soc_18_pcth_t,
   prog_soc18t,
   prog_soc20_dist,
   prog_soc20t,
   prog_soc18,
   prog_soc20)


################ Visualización de datos #############
## Gráfica de barras: Porcentaje de hogares que reciben un programa social o beca 2018-2020
prog_soc_pct_dec %>%
  ggplot(aes(x = DECIL,
             y = pct_hog_ps,
             fill = año)) +
  theme_bw()+
  theme(text = element_text(size=15), ## Ajustamos la letra del texto a 11 puntos
        plot.title = element_text(hjust = 0.5), ## Alineamos el título al centro
        axis.title.x =  element_blank()) + 
  geom_bar(stat = "identity",
           position=position_dodge()) +
  scale_fill_brewer(palette="Paired")+
  scale_x_continuous(breaks = c(1:10),
                     labels = c("I",
                                "II",
                                "III",
                                "IV",
                                "V",
                                "VI",
                                "VII",
                                "VIII",
                                "IX",
                                "X"))+
  labs(title = "Porcentaje de hogares que reciben programas sociales en Colima",
       subtitle = "Colima, 2018-2020",
       x = "Decil de ingresos",
       y = "Porcentaje de hogares que reciben programa sociales",
       caption = "Fuente: ENIGH 2018 y 2020, INEGI.
       Elaborado por @jkvisfocri")

## Gráfica de barras lateral: Distribución de programas sociales por deciles de ingreso
prog_soc_pct_dist %>% 
  mutate(DECIL = fct_relevel(DECIL,
                             rev)) %>% 
  ggplot(aes(x = año,
             y = prog_soc_dist,
             fill = DECIL))+
  theme_bw()+
  theme(text = element_text(size=17), ## Ajustamos la letra del texto a 11 puntos
        plot.title = element_text(hjust = 0.5), ## Alineamos el título al centro
        axis.title.y =  element_blank()) + 
  geom_bar(stat="identity",
           width = .7,
           colour="black",
           lwd=0.1) +
  geom_text(aes(label=prog_soc_dist),
            position=position_stack(vjust=0.5),
            colour="white",
            size = 5) +
  scale_fill_discrete(labels = c("X",
                                 "IX",
                                 "VIII",
                                 "VII",
                                 "VI",
                                 "V",
                                 "IV",
                                 "III",
                                 "II",
                                 "I"))+
  coord_flip()+
  labs(title = "Distribución de los programas sociales de acuerdo a los deciles de ingreso",
       subtitle = "Colima, 2018-2020",
       x = "Año",
       y = "Distribución de los programas sociales",
       fill = "Decil",
       caption = "Fuente: ENIGH 2018 y 2020, INEGI.
       Elaborado por @jkvisfocri")
