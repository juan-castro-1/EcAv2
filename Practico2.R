#--------------------Modelos Económicos de Crimen con datos de panel-------------
setwd("C:/Users/juan_/Desktop/Econometría/Practico2")
library(stargazer)
library(plm)
library(readxl)
library(openxlsx)
library(Formula)
dir()
#-------------------> Cargo la Base de datios
data<-read.csv("cornwell.csv")
panel<-pdata.frame(data, index = c("county","year"))
attach(panel)
names(panel)
#-------------------> Armo el modelo
modelo.estimar<-Formula(lcrmrte~lprbarr+lprbconv+lprbpris+lavgsen
                        +lpolpc+ldensity+lpctymle
                        +lwcon+lwtuc+lwtrd+lwfir+lwser+lwmfg+lwfed+lwsta+lwloc
                        +west+central+urban
                        +lpctmin)


#-------------------> 1) Estimación Between
estimador.be<-plm(modelo.estimar, data=panel, model="between")
stargazer(estimador.be,  type="latex",
          dep.var.labels=c("crime rate")
          , out="between.txt")


#-------------------> 2) Estimación Within
estimador.ef<-plm(modelo.estimar, data=panel, model="within")
stargazer(estimador.ef,  type="latex",
          dep.var.labels=c("crime rate")
          , out="within.txt")

# Test de efectos inobservables. H0: sigma_m^2 = 0 
# Wooldridge 2002, 10.4.4
pwtest(modelo.estimar, data = panel)

#-------------------> 3) Estimacion Efectos Aleatorios
estimador.re<-plm(modelo.estimar, data=panel, model="random")
stargazer(estimador.re,  type="latex",
          dep.var.labels=c("crime rate")
          , out="random.txt")

#Test de Hausman:
phtest(estimador.ef,estimador.re)
# H0: FE es consistente, RE es consistente y eficiente
# HA: FE es consistente, RE es inconsistente

#-------------------> Tabla con todos los estimadores juntos
stargazer(estimador.be, estimador.ef, estimador.re,  type="latex",
          dep.var.labels=c("l_crime rate")
          , out="todos.txt")

#-------------------> 5) Efectos Aleatorios y Correlación serial 
# Test conjuntos de correlación serial y efectos aletorios 
#(bajo normalidad y homoscedasticidad)
pbsytest(modelo.estimar, data = panel, test = "J")

# Test robusto local para correlación serial o Efectos Aleatorios
pbsytest(modelo.estimar, data = panel)

# Test robusto local para correlación serial o Efectos Aleatorios, teninedo en cuenta que la 
# varianza debe ser no negativa
pbsytest(modelo.estimar, data = panel, test = "RE")

# Test condicional LM para errores AR(1) o MA(1) bajo Efectos aleatorios
pbltest(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen + lpolpc + 
          ldensity + lpctymle + lwcon + lwtuc + lwtrd + lwfir + lwser + 
          lwmfg + lwfed + lwsta + lwloc + west + central + urban + 
          lpctmin, data = panel, alternative = "onesided")



