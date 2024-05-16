####---------------------------------***--------------------------------#
#
#         Script para el procesamiento de variables meteorlógicas
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

# - - - - - - - - GRAFICAS CON DATOS  minutales - - - - - - - #
setwd("C:/Users/hgomez/Desktop/DTM")

# Funciones auxiliares
source("C:/Users/hgomez/Documents/Boletin/marcapomacocha/crep.r") # RedE->MAx, BlueE-> Min

df <- read.csv("datosPrueba.csv", sep=';')
                         
head(df)

#df$Anio <- df$Anio+2000
#df$Temp <- df$Temp - 273
#df$Anio <- as.integer(df$Anio)
#df$Mes <- as.numeric(df$Mes)
#df$Dia <- as.numeric(df$Dia)
#df$Hora <- as.numeric(df$Hora)

dfi <-  mutate(df, Fecha = as.Date(paste(Anio, Mes, Dia, sep='-')))
dfi$datetime <- as.POSIXct(paste(dfi$Fecha,dfi$Hora), format="%Y-%m-%d %H")

head(dfi)

#calculo de valores diarios - usar las funciones de Crep.r
df1<-group_by(dfi, Anio, Mes,Dia, Fecha)

df2<-summarise(df1,
              Prec = sum(Precip),
              Tx = RedE(Temp),
              Tmi = BlueE(Temp),
              Tmed = raiza(Temp),
              HR = raiza(HR))
head(df2)

#------------ Temperatura --------------#

# a) grafico de variacion diaria de temperatura 
temperatura <- df2 %>% ggplot(aes(x=Fecha, color=Leyenda)) +
  geom_line(aes(y= Tx, color ="Temp. Máxima"))+
  geom_point(aes(y= Tx, color ="Temp. Máxima")) +
  geom_line(aes(y= Tmi, color = "Temp. Mínima") ) +
  geom_point(aes(y= Tmi, color = "Temp. Mínima")) +
  geom_line(aes(y= Tmed, color = "Temp. Media"))+
  geom_point(aes(y= Tmed, color = "Temp. Media"))+  
  theme_minimal()+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")))+
  ggtitle(" Variación diaria de temperatura ")+
  scale_y_continuous(limits = c(0,40),
                     breaks = seq(0,40,5),
                     expand = c(0.01,0.01),
                     name = "(°C)")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")

temperatura


#a) grafico de variacion horaria multianual

# Conversion y calculo de promedios horarios
dfih<- group_by(dfi, Anio, Hora)
dfih <- summarise(dfih, tmed = raiza(Temp))

dfih %>% ggplot(aes(x= Hora, y=tmed, color=Leyenda)) +
  geom_line(aes(color="Temp. horaria"))+
  geom_point(aes(color="Temp. horaria"))+
  facet_wrap(~ Anio, scales = "free_x", ncol = 2) +
  labs(title = "Variación horaria por año", x = "Hora del día", y = "°C") +
  scale_y_continuous(limits = c(10,30),
                     breaks = seq(10,30,5),
                     expand = c(0.01,0.01),
                     name = "(°C)")+
  theme_minimal()+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))) -> Temphor

Temphor

####---------------------- Precipitación ----------------------------####

# Variacion diaria 
Prec <- df2 %>% ggplot(aes(x=Fecha, color=Leyenda))+
  geom_line(aes(y= Prec, color ="Precipitación"))+
  geom_point(aes(y= Prec, color ="Precipitación"))+ 
  scale_color_manual(values = "blue", labels="Prec")+
  ggtitle("Variación diaria de Precipitación ")+
  scale_y_continuous(limits = c(0,10),
                     breaks = seq(0,10,1),
                     expand = c(0.01,0.01),
                     name = "(°C)")+
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")+
  theme_minimal()+
  theme(legend.text = element_text(size=7,face = "bold"))+
  theme(axis.line = element_line(color = "black", size = 1),
        axis.title = element_text(colour = "black", lineheight = 12, face = "bold"),
        axis.title.x = element_text(lineheight = 10, color = "black", face = "bold"),
        axis.title.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(lineheight = 10, color = "black", face = "bold", margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm")),
        axis.text.y = element_text(lineheight = 10, color = "black", face = "bold", family = "Arial", margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")))

Prec


# Variación mensual
df3 <- group_by(df2, Mes)
df4 <- summarise(df3, PP = sum(Prec))

PPmon <- df4 %>% ggplot(aes(x=Mes, y=PP, color = Leyenda))+
  geom_col(aes(color="Humedad"),fill="skyblue")
PPmon


# variacion 
df2$Mon <-  df2$Mon = month(df2$Mes, label = TRUE, abbr = FALSE, locale = "es_ES")







