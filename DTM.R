####---------------------------------***--------------------------------#
#
#  Script para el procesamiento de variables meteoorlogicas Para DTM
#
# -----------------------------------***--------------------------------#

#install.packages("plotly")
library(ggplot2)
library(openair)
library(latticeExtra)
library(directlabels)
library(dplyr)
library(scales)
library(readxl)
library(egg)
library(gtable)
library(gridExtra)
library(grid)
library(tidyverse)
library(openxlsx)
library(zoo)
library(lubridate)
library(patchwork)
library(arsenal)
library(hms)
library(readr)
library("data.table")
library(ggplotify)

# - - - - - - - - GRAFICAS CON DATOS  minutales - - - - - - - #


# Funciones auxiliares

# RedE-> calculo de Maximos
# BlueE-> Calculo de Minimos
# Prom -> calculo de promedios

RedE<- function(x){
  lake=sum(is.na(x))
  if(lake>5){
    mon= NA
  }else{
    mon=max(x, na.rm=T)
  }
}

#MIN -> BlueE
BlueE<-function(x){
  lake=sum(is.na(x))
  if(lake>5){
    mon= NA
  }else{
    mon=min(x, na.rm=T)
  }
}

#funcion de medias-------
Prom <- function(x){
  lake=sum( is.na(x))
  if (lake > 5){
    mon = NA
  }else{ 
    mon = mean(x, na.rm = T)
  } 
  return(mon)
}

#Ruta de directorio de trabajo
setwd("C:/Users/hgomez/Desktop/DTM")

#Lectura de Datos 
df <- read.csv("datosPrueba.csv", sep=';')
head(df)

# generación de columna de fechas
dfi <-  mutate(df, Fecha = as.Date(paste(Anio, Mes, Dia, sep='-')))
dfi$datetime <- as.POSIXct(paste(dfi$Fecha,dfi$Hora), format="%Y-%m-%d %H")

head(dfi)

#calculo de valores diarios - usar las funciones 
df1<-group_by(dfi, Anio, Mes,Dia)

df2<-summarise(df1,
              Prec = sum(Precip),
              Tx = RedE(Temp),
              Tmi = BlueE(Temp),
              Tmed = Prom(Temp),
              HR = Prom(HR))

df2 <-  mutate(df2, Fecha = as.Date(paste(Anio, Mes, Dia, sep='-')))
head(df2)

###--------------Temperatura--------------

# a) grafico de variacion diaria de temperatura 
temperatura <- df2 %>% ggplot(aes(x=Fecha, color=Leyenda)) +
  geom_line(aes(y= Tx, color ="Temp. Máxima"))+
  geom_point(aes(y= Tx, color ="Temp. Máxima")) +
  geom_line(aes(y= Tmi, color = "Temp. Mínima") ) +
  geom_point(aes(y= Tmi, color = "Temp. Mínima")) +
  geom_line(aes(y= Tmed, color = "Temp. Media"))+
  geom_point(aes(y= Tmed, color = "Temp. Media"))+  
  theme_light()+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")))+
  ggtitle("Variación diaria de la Temperatura del Aire")+
  labs(x = "Años") +
  scale_y_continuous(limits = c(5,35),
                     breaks = seq(5,35,3),
                     expand = c(0.01,0.01),
                     name = "(°C)")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")

temperatura

ggsave("temperatura.png", plot = temperatura, width = 10, height = 6, units = "in", dpi = 300)

# b) grafico de variacion horaria multianual

# Conversion y calculo de promedios horarios
dfih<- group_by(dfi, Anio, Hora)
dfih <- summarise(dfih, tmed = Prom(Temp))
head(dfih)

dfih %>% ggplot(aes(x= Hora, y=tmed, color=Leyenda)) +
  geom_line(aes(color="Temp. horaria"))+
  geom_point(aes(color="Temp. horaria"))+
  facet_wrap(~ Anio, scales = "free_x", ncol = 2) +
  labs(x = "Hora del día", y = "°C") +
  scale_y_continuous(limits = c(10,30),
                     breaks = seq(10,30,2),
                     expand = c(0.01,0.01),
                     name = "(°C)")+
  scale_x_continuous(limits = c(0,24),
                     breaks = seq(0,24,1),
                     expand =c(0.01,0.01),
                     name = "Horas")+
  theme_minimal()+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")))+
  ggtitle("Variación de la temperatura horaria por año") -> Temphor

Temphor

ggsave("tempHor1.png", plot = Temphor, width = 10, height = 6, units = "in", dpi = 300)


# c) grafico de variacion Mensual

# haciendo calculos mensuales
df31 <- group_by(df2, Anio, Mes)
df32 <- summarise(df31, Tmax = Prom(Tx), Tmin= Prom(Tmi), Tme=Prom(Tmed))
df32 <- mutate(df32, Fecha=as.Date(paste(Anio,Mes,1, sep='-')))

# grafico

df32 %>% ggplot(aes(x=Fecha, color=Leyenda))+
  geom_line(aes(y=Tmax, color="Temp. Máxima"))+
  geom_point(aes(y=Tmax, color="Temp. Máxima"))+
  geom_line(aes(y=Tmin, color="Temp. Mínima"))+
  geom_point(aes(y=Tmin, color="Temp. Mínima"))+
  geom_line(aes(y=Tme, color="Temp. Media"))+
  geom_point(aes(y=Tme, color="Temp. Media"))+
  theme_light()+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")))+
  ggtitle("Variación mensual de la Temperatura del Aire")+
  labs(x = "Años") +
  scale_y_continuous(limits = c(5,35),
                     breaks = seq(5,35,3),
                     expand = c(0.01,0.01),
                     name = "(°C)")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y") -> Tmen

Tmen

ggsave("tempmen.png", plot = Tmen, width = 10, height = 6, units = "in", dpi = 300)


# box plot a nivel mensual
df32$Mon <- month(df32$Mes, label = T, abbr = FALSE)
df32$Mon <- as.factor(df32$Mon)


df32 %>% ggplot(aes(x=Mon, fill=Leyenda))+
  geom_boxplot(aes(y = Tmax, fill="Temp. Máxima"))+
  geom_boxplot(aes(y = Tmin, fill="Temp. Mínima"))+
  geom_boxplot(aes(y = Tme, fill="Temp. Media"))+
  scale_y_continuous(limits = c(0,35),
                     breaks = seq(0,35,5),
                     expand = c(0.01,0.01),
                     name = "(°C)")+
  theme_minimal()+
  ggtitle("Variación mensual de Temperatura")+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))) -> Tmp_BP

Tmp_BP

ggsave("temp_bp_men.png", plot = Tmp_BP, width = 10, height = 6, units = "in", dpi = 300)



####---------------------- Precipitación ----------------------------

# Variacion diaria 
Prec <- df2 %>% ggplot(aes(x=Fecha, color=Leyenda))+
  geom_line(aes(y= Prec, color ="Precipitación"))+
  geom_point(aes(y= Prec, color ="Precipitación"))+ 
  scale_color_manual(values = "blue", labels="Precipitación")+
  scale_y_continuous(limits = c(0,15),
                     breaks = seq(0,15,1),
                     expand = c(0.01,0.01),
                     name = "(mm)")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
  theme_minimal()+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")))+
  ggtitle("Variación diaria de Precipitación ")
Prec

ggsave("Prec_diaria.png", plot = Prec, width = 10, height = 6, units = "in", dpi = 300)


# Variación mensual
df3 <- group_by(df2, Anio, Mes)
df4 <- summarise(df3, PP = sum(Prec))
df41 <- group_by(df4, Mes)
df42 <- summarise(df41, PPi = mean(PP))
df42$Mon <- month(df42$Mes, label = T, abbr = FALSE)
df42$Mon <- as.factor(df42$Mon)
               
PPmon <- df42 %>% ggplot(aes(x=Mon, y=PPi, color = Leyenda))+
  geom_bar(aes(color="Precipitación"),fill="steelblue", show.legend = TRUE,stat = "identity",position = "dodge",)+
  scale_color_manual(values = "black", labels="Precipitación")+
  scale_y_continuous(limits = c(0,20),
                     breaks = seq(0,20,2),
                     expand = c(0.01,0.01),
                     name = "(mm)")+
  theme_minimal()+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")))+
  ggtitle("Variación mensual de precipitación promedio")
PPmon
ggsave("Prec_Men_bar.png", plot = PPmon, width = 10, height = 6, units = "in", dpi = 300)


# variacion mensual promedio de precipiacion 
df5 <- group_by(df2, Anio, Mes)
df6 <- summarise(df5, PP = sum(Prec))

df6$Mon <- month(df6$Mes, label = T, abbr = FALSE)
df6$Mon <- as.factor(df6$Mon)


df6 %>% ggplot(aes(x=Mon, fill=Leyenda))+
  geom_boxplot(aes(y=PP,fill="Precipitación"))+
  scale_fill_manual(values = "steelblue", labels="Precipitación")+
  scale_y_continuous(limits = c(0,20),
                     breaks = seq(0,20,2),
                     expand = c(0.01,0.01),
                     name = "(mm)")+
  theme_minimal()+
  ggtitle("Variación mensual de Precipitación ")+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))) -> PP_bp

PP_bp
ggsave("Prec_Men_bp.png", plot = PP_bp, width = 10, height = 6, units = "in", dpi = 300)


###----------------------------Humedad Relativa-----------------------------  

# a) variación diaria anual
df2 %>% ggplot(aes(x= Fecha, y=HR, color = Leyenda))+
  geom_line(aes(color="Hum. Relativa"))+
  geom_point(aes(color="Hum. Relativa"))+
  scale_color_manual(values = "blue", labels="Humedad Relativa")+
  scale_y_continuous(limits = c(40,100),
                     breaks = seq(40,100,5),
                     expand = c(0.01,0.01),
                     name = "(%)")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
  theme_minimal()+
  theme_minimal()+
  ggtitle("Variación diaria de Humedad Relativa")+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))) -> Hrel

Hrel
ggsave("HrelD.png", plot = Hrel, width = 10, height = 6, units = "in", dpi = 300)


# b) variación mensual multianual
df2$Mon <- month(df2$Mes, label = T, abbr = FALSE)
df2$Mon <- as.factor(df2$Mon)

df52 <- group_by(df2, Mon)
df512 <- summarise(df52, Hrl=mean(HR))


HrelM <- df512 %>% ggplot(aes(x=Mon, y=Hrl, color = Leyenda))+
  geom_bar(aes(color="Precipitación"),fill="#4169E1", show.legend = TRUE,stat = "identity",position = "dodge",)+
  scale_color_manual(values = "black", labels="Humedad Relativa")+
  scale_y_continuous(limits = c(0,100),
                     breaks = seq(0,100,5),
                     expand = c(0.01,0.01),
                     name = "(%)")+
  theme_minimal()+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")))+
  ggtitle("Variación mensual de Humedad Relativa promedio")
HrelM
ggsave("Hrel_Men_bar.png", plot = HrelM, width = 10, height = 6, units = "in", dpi = 300)



####-------------------------------Rosas de Viento---------------------------------

# Conversion y calculo de promedios horarios
dfvh<- group_by(df1, Anio, Hora)
dfvh <- summarise(dfvh, Wsp = mean(Wspeed))

dfvh %>% ggplot(aes(x= Hora, y=Wsp, color=Leyenda)) +
  geom_line(aes(color="Velocidad horaria"))+
  geom_point(aes(color="Velocidad horaria"))+
  scale_color_manual(values = "green4", labels="Velocidad del viento")+
  facet_wrap(~ Anio, scales = "free_x", ncol = 2) +
  labs(x = "Hora del día", y = "m/s") +
  scale_y_continuous(limits = c(0,10),
                     breaks = seq(0,10,1),
                     expand = c(0.01,0.01),
                     name = "(m/s)")+
  scale_x_continuous(limits = c(0,24),
                     breaks = seq(0,24,1),
                     expand =c(0.01,0.01),
                     name = "Horas")+
  theme_minimal()+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")))+
  ggtitle("Variación de viento horario por año") -> Vientohor

Vientohor

ggsave("WindHor.png", plot =Vientohor, width = 10, height = 6, units = "in", dpi = 300)


# grafico de rosas 
dfv1 <- df1

dfv1$Mon <- month(dfv1$Mes, label = T, abbr = FALSE)
dfv1$Mon <- as.factor(dfv1$Mon)

# cLasificacion por horas
dfv1<- dfv1 %>% 
  mutate(horario = case_when(between(Hora, 1,6) ~ "Horario Nocturno",
                             between(Hora, 7,18) ~ "Horario Diurno",
                             between(Hora, 19,24) ~ "Horario Nocturno"))

dfv1$horario<- factor(dfv1$horario, levels = c("Horario Diurno", "Horario Vespertino", "Horario Nocturno"))

# cLasificacion por estaciones
dfv1<- dfv1 %>% 
  mutate(Season = case_when(between(Mes, 1,3) ~ "Verano",
                             between(Mes, 4,6) ~ "Otoño",
                             between(Mes, 7,9) ~ "Invierno",
                             between(Mes, 10,12) ~ "Primavera"))

dfv1$Season<- factor(dfv1$Season, levels = c("Verano", "Otoño", "Invierno", "Primavera"))

#trace(windRose, edit = TRUE) #codigo para modificar otra funcion          type = c("horario","mes"),

# rosa de viento total
WRose<-windRose(dfv1, ws = "Wspeed", wd = "Wdir", 
                paddle = FALSE, 
                key.header = "Velocidad del Viento",
                breaks = c(0, 3, 5.5, 8),
                angle = 22.5,
                cols = c("#92D050", "#FFFF00", "#FFC000", "#FF0000"),
                auto.text = TRUE,
                labels = c("1","2","3"),
                key.footer = expression((m.* s^-1)))
WRose


png(filename = "WroseTotal.png", width = 600, height = 600)
WRose
dev.off()


# por horario diurno y nocturno
WRoseH<-windRose(dfv1, ws = "Wspeed", wd = "Wdir", type = c("horario"),
                paddle = FALSE, 
                key.header = "Velocidad del Viento",
                breaks = c(0, 3, 5.5, 8),
                angle = 22.5,
                cols = c("#92D050", "#FFFF00", "#FFC000", "#FF0000"),
                auto.text = TRUE,
                labels = c("1","2","3"),
                key.footer = expression((m.* s^-1)))
WRoseH

png(filename = "Wrose_Hor.png", width = 600, height = 600)
WRoseH
dev.off()



# por estación
WRoseSea<-windRose(dfv1, ws = "Wspeed", wd = "Wdir", type = c("Season"),
                 paddle = FALSE, 
                 key.header = "Velocidad del Viento",
                 breaks = c(0, 3, 5.5, 8),
                 angle = 22.5,
                 cols = c("#92D050", "#FFFF00", "#FFC000", "#FF0000"),
                 auto.text = TRUE,
                 labels = c("1","2","3"),
                 key.footer = expression((m.* s^-1)))
WRoseSea

png(filename = "Wrose_Season.png", width = 600, height = 600)
WRoseSea
dev.off()

#######------------ Fin de graficos y procesamiento de datos Meteorológicos-------------------------#
####
##




#---------------------------------------------*****-----------------------------------------------#
#
####                             Graficos de calidad de aire
#
#-------------------------------------------------------------------------------------------------#

dfpm <- read.csv("datospm.csv", sep=';')
dfpm <- mutate(dfpm, Fecha=as.Date(paste(Anio,Mes,Dia, sep='-')))

dfpm_ene <- filter(dfpm, Mes==1)
dfpm_jun <- filter(dfpm, Mes==6)


# GRafica para enero
dfpm_ene %>% ggplot(aes(x=Fecha, color = Leyenda))+
  geom_bar(aes(y = PM25, color="PM2.5"), fill="steelblue",width = 0.9 ,show.legend = TRUE,stat = "identity",position = "dodge")+
  scale_x_date(limit = c(as.Date("2015-01-01"), as.Date("2015-01-31")),date_breaks = "1 day", date_labels =  "%d")+
  geom_hline(aes(yintercept = 50, color="ECA PM2.5"), color = "red", linetype = "dashed", size = 1, name = "dias de enero") +
  scale_y_continuous(limits = c(0,80),
                     breaks = seq(0,80,5),
                     expand = c(0.01,0.01),
                     name = "(µg.m^-3)")+
  theme_minimal()+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")))+
  ggtitle("Variación de concentracion de PM2.5 para el mes de Enero") -> PM25_ene

PM25_ene 

ggsave("PM25_ene.png", plot = PM25_ene , width = 10, height = 6, units = "in", dpi = 300)


# Grafica para Junio
dfpm_jun %>% ggplot(aes(x=Fecha, color = Leyenda))+
  geom_bar(aes(y = PM25, color="PM2.5"), fill="steelblue",width = 0.9 ,show.legend = TRUE,stat = "identity",position = "dodge")+
  geom_hline(aes(yintercept = 50, color="ECA PM2.5"), color = "red", linetype = "dashed", size = 1) +
  scale_x_date(limit = c(as.Date("2015-05-30"), as.Date("2015-06-30")),date_breaks = "1 day", date_labels =  "%d",  name = "dias de junio")+
  scale_y_continuous(limits = c(0,80),
                     breaks = seq(0,80,5),
                     expand = c(0.01,0.01),
                     name = "(µg.m^-3)")+
  theme_minimal()+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")))+
  ggtitle("Variación de concentracion de PM2.5 para el mes de Junio") -> PM25_jun

PM25_jun

ggsave("PM25_jun.png", plot = PM25_jun , width = 10, height = 6, units = "in", dpi = 300)

