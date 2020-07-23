library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(writexl)

## Load DataSet
covid_ll <- read_excel("C:/users/catal/projects/covid/data/covid_ll.xls")
View(covid_ll)

covid_ll$fecha <- dmy(covid_ll$fecha)
covid_ll$hora <- hm(covid_ll$hora)

# Calculate Positivity Rate
tasa_positividad <- covid_ll %>%  
  filter(resultado %in% c("Positivo", "No Detectable")) %>%
  group_by(fecha) %>%
  summarize(positives = sum(resultado == "Positivo"), 
                            tests = n()) %>%
  mutate(rate = positives / tests) %>%
  mutate(weekday = factor(wday(fecha)))%>%
  mutate(semana = week(fecha))


tasa_positividad

## Gr?fico de Tasa de Positividad

tasa_positividad %>%
  ggplot(aes(fecha, rate))+
  geom_point(aes(fecha, rate), alpha=0.50, size=1)+
  geom_smooth()+
  xlab("Fecha")+ ylab("Tasa de Positividad")

ggsave("figs/tasa_pos_scatter.png") 

tasa_positividad %>%
  filter(rate <1)%>%
     ggplot(aes(fecha, rate))+
  geom_col(aes(fecha, rate), 
               alpha=0.50, color="gray45", fill="palegreen2")+
  xlab("Fecha")+ ylab("Tasa de Positividad")

ggsave("figs/tasa_pos_bar.png") 

write_xlsx(tasa_positividad,
           "data/hnfpositivos.xlsx")

## Medidas de resumen

tp_semanal <- covid_ll %>%
  group_by(week(fecha))%>% 
  summarize (positives = sum(resultado == "Positivo"), tests = n()) %>%
  mutate(rate = positives / tests)

tp_semanal %>%
  ggplot(aes(`week(fecha)`, rate))+
  geom_point(aes(`week(fecha)`, rate), alpha=0.50, size=1)+
  geom_smooth()+
  xlab("Fecha")+ ylab("Tasa de Positividad")
 

write_xlsx(tp_semanal,
           "data/tp_semanal.xlsx")

## Resultados positivos

p<- tasa_positividad %>%
  ggplot()+
  geom_point(aes(fecha, positives),alpha=0.5)+
  geom_line (aes(fecha, positives),alpha=0.3)+
  xlab("Fecha")+ ylab("HNF Positivos")+ 
  ggtitle("Hisopados Positivos por d?a")


agrup_sem <- aggregate(tasa_positividad, by= list(
            tasa_positividad$semana), FUN=mean, na.rm=TRUE) 

p +
  geom_line(data=agrup_sem,(aes(fecha, positives)),
              alpha=0.5, color= "seagreen4",size=1.5)
  

p +
  geom_line(data=agrup_sem,(aes(fecha, positives)),
            alpha=0.5,size=1.5, color="seagreen4")

ggsave("figs/hnf_positivos.png") 

## Hisopados realizados

hisopados <- covid_ll %>%  
  filter(resultado %in% c("Positivo", "No Detectable")) %>%
  group_by(fecha) %>%
  summarize(positivos = sum(resultado == "Positivo"), 
            negativos = sum(resultado =="No Detectable"), 
                            tests = n())

hisopados %>%
  ggplot() + 
  geom_bar(aes(y=tests, x=fecha), 
           position="stack", stat="identity", 
           color ="grey", fill="palegreen2")+
  geom_line(aes(fecha, positivos), color="red")+
  xlab("Fecha")+ ylab("Hisopados NF")+
  ggtitle("Hisopados totales y positivos")

ggsave("figs/hisopados_totales_bar.png")

covid_ll %>%
  filter(edad < 100)%>%
  ggplot()+
  geom_histogram(aes(edad), color="grey", fill="coral1")+
  xlab("Edad en a?os")+
  ylab("Pacientes")+
  ggtitle("Edades de los Pacientes Hisopados")

ggsave("figs/edad_hist.png") 

