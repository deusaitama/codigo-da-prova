#a corregando os dados
install.packages("readr")
library(readr)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("kmed")
library(kmed)
install.packages("sjPlot")
library(sjPlot)
install.packages("esquisse")
library(esquisse)
install.packages("corrplot")
library(corrplot)

#b analise exploratioria
#matriz de correlaçao

M<-cor(cerveja)
corrplot(M,method='number')

M<-cor(cerveja)
corrplot(M,method='color')

ggplot(data = cerveja, aes(x=temp_media,y=consumo))+
  geom_point(size=4)+
  theme_classic()+
  xlab("Temp_media")+
  ylab("Consumo")+
  geom_smooth(method = "lm",se=FALSE)

ggplot(data = cerveja, aes(x=temp_min,y=consumo))+
  geom_point(size=4)+
  theme_classic()+
  xlab("Temp_min")+
  ylab("Consumo")+
  geom_smooth(method = "lm",se=FALSE)

ggplot(data = cerveja, aes(x=temp_max,y=consumo))+
  geom_point(size=4)+
  theme_classic()+
  xlab("Temp_max")+
  ylab("Consumo")+
  geom_smooth(method = "lm",se=FALSE)

ggplot(data = cerveja, aes(x=precipitacao,y=consumo))+
  geom_point(size=4)+
  theme_classic()+
  xlab("precipitacao")+
  ylab("Consumo")+
  geom_smooth(method = "lm",se=FALSE)

ggplot(data = cerveja, aes(x=final_de_semana,y=consumo))+
  geom_point(size=4)+
  theme_classic()+
  xlab("final_de_semana:")+
  ylab("Consumo")+
  geom_smooth(method = "lm",se=FALSE)


cerveja$final_de_semana<-factor(cerveja$final_de_semana)


#c

modelo_1<-lm(formula=consumo~temp_max,data=cerveja)
summary(modelo_1)

#d

modelo_2<-lm(formula=consumo~.,data=cerveja)
summary(modelo_2)

modelo_3<-lm(formula=consumo~temp_max+precipitacao+final_de_semana,data=cerveja)
summary(modelo_3)
 
#E
#verifica a disribuição dos rsiduos e normal
qqnorm(modelo_3$residuals)
qqline(modelo_3$residuals)
#verificação da media parece ser zero
t.test(modelo_3$residuals)
#vendo a homoscedastidade
plot(modelo_3$fitted.values,modelo_3$residuals)
abline(0,0)

#F


eu<-data.frame(temp_media=20,temp_min=16,temp_max=24,precipitacao=0,final_de_semana=1)
predict(modelo_2,eu,type='response')







