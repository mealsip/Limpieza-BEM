rm(list=ls())

library(qdapRegex)
library(stringr)
library(dplyr)
library(ggplot2)
library(daewr)
library(xlsx)
library(lubridate)

getwd()
setwd("C:/Users/SFLORES/Documents/SIP/BEM 2018/Limpieza BD")

#Definiciones

CULTIVO5=c("MAIZ","FRIJOL","TRIGO","CEBADA","SORGO")

#Cultivo

cultivo=c("DESCANSO",
          "ALFALFA",
          "AVENA",
          "CEBADA",
          "FRIJOL",
          "MAIZ",
          "SORGO",
          "TRIGO",
          "TRITICALE",
          "AJONJOLI",
          "ALGODON",
          "AMARANTO",
          "ARROZ",
          "CANOLA",
          "CARTAMO ",
          "CEMPOALXOCHITL",
          "FRESA",
          "GARBANZO",
          "HABA",
          "SOYA",
          "OTRO",
          "CALABAZA",
          "CHIA",
          "CHICHARO",
          "BROCOLI",
          "CANAVALIA",
          "GIRASOL",
          "EBO",
          "CANA",
          "RABANO",
          "CROTALARIA")

#Variables tipo Caracter

CharVar=c("Tipo.de.parcela..testigo.o.innovación.",
          "Nombre.de.la.sección",
          "Cultivo.sembrado",
          "Nombre.del.principal.producto.a.obtener",
          "Tipo.de.semilla",
          "Nombre.de.la.variedad.sembrada",
          "Humedad.de.la.tierra.al.sembrar",
          "Arreglo.de.la.siembra",
          "Tipo.de.siembra",
          "Tipo.de.sembradora.utilizada.para.la.siembra",
          "Si.el.cultivo.es.maíz.o.frijol..indique.el.color.de.la.semilla")


#FUNCIONES

#Cambio a Mayusculas

mayus=function(based,variables){
  based = mutate_at(based,vars(variables), toupper)
  return(based)
}

#Elimina acentos
sinacento_aux=function(x){
  iconv(x, to="ASCII//TRANSLIT")
}

sinacento=function(based,variables){
  based = mutate_at(based,vars(variables), sinacento_aux)
  return(based)
}

#Remueve espacios en blanco

whitespace=function(based,variables){
  based = mutate_at(based,vars(variables), trimws)
  based = mutate_at(based,vars(variables), rm_white_multiple)
  return(based)
}

# Espacios en blanco los lleva a NAs

whiteNA=function(x){
  for (i in 1:length(x)) {
    if (!is.na(x[i])){
      if (x[i]=="") x[i]=NA
    }
  }
  return(x)
}

#Nombre del cultivo sembrado

Cultivosembrado=function(based){
  for (i in 1:dim(based)[1]) {
    if (length(grep("MAIZ|CLTHW13001",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="MAIZ"
    }
    else if (length(grep("CEBADA",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="CEBADA"
    }    
    else if (length(grep("FRIJOL",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="FRIJOL"
    }    
    else if (length(grep("SORGO",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="SORGO"
    }
    else if (length(grep("TRIGO",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="TRIGO"
    }
    else if (length(grep("TRITICALE|TITRICALE",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="TRITICALE"
    }
    else if (length(grep("ALFALFA",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="ALFALFA"
    }
    else if (length(grep("AVENA",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="AVENA"
    }
    else if (length(grep("AGUACATE",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="AGUACATE"
    }
    else if (length(grep("CANAVALI|CANNAVALIA",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="CANAVALIA"
    }
    else if (length(grep("CEMPASUCHIL|CEMPOALXOCHITL",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="CEMPOALXOCHITL"
    }
    else if (length(grep("MOCUNA|MUCUNA",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="MUCUNA"
    }
    else if (length(grep("IBES",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="IBES"
    }
    else if (length(grep("LIMON",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]="LIMON"
    }
    else if (length(grep("SIN INFORMACION|PERDIDA|DESCANSO|SIN CULTIVO|FRUTALES|PASTO FORRAJERO",based$Cultivo.sembrado[i]))){
      based$Cultivo.sembrado[i]=NA
    }
  }
  return(based)
}

Cultivocosecha=function(based){
  for (i in 1:dim(based)[1]) {
    if (length(grep("MAIZ|CLTHW13001|CRIOLLO|MAIS|CROPLAN",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="MAIZ"
    }
    else if (length(grep("CEBADA",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="CEBADA"
    }    
    else if (length(grep("FRIJOL",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="FRIJOL"
    }    
    else if (length(grep("SORGO",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="SORGO"
    }
    else if (length(grep("TRIGO",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="TRIGO"
    }
    else if (length(grep("TRITICALE|TITRICALE",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="TRITICALE"
    }
    else if (length(grep("ALFALFA",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="ALFALFA"
    }
    else if (length(grep("AVENA",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="AVENA"
    }
    else if (length(grep("AGUACATE",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="AGUACATE"
    }
    else if (length(grep("CANAVALI|CANNAVALIA",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="CANAVALIA"
    }
    else if (length(grep("CEMPASUCHIL|CEMPOALXOCHITL",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="CEMPOALXOCHITL"
    }
    else if (length(grep("MOCUNA|MUCUNA",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="MUCUNA"
    }
    else if (length(grep("IBES",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="IBES"
    }
    else if (length(grep("LIMON",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="LIMON"
    }
    else if (length(grep("PASTO",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="PASTO"
    }
    else if (length(grep("CALABAZA",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]="CALABAZA"
    }
    else if (length(grep("SIN INFORMACION|PERDIDA|DESCANSO|SIN CULTIVO|FRUTALES|PASTO FORRAJERO|SINIESTRADO|RASTROJO|REHABILITACION",based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]))){
      based$Cultivo.al.que.corresponde.la.actividad.de.cosecha[i]=NA
    }
  }
  return(based)
}



#Datos 

siembra=read.csv(file='13_siembra_Resiembra_descripcio.csv',header=TRUE)

siembra=mayus(siembra,CharVar)

siembra=sinacento(siembra,CharVar)

siembra=whitespace(siembra,CharVar)

siembra = mutate_at(siembra,vars(CharVar), whiteNA)

siembra=siembra[,c("ID.de.la.bitácora..clave.foránea.","ID.de.tipo.de.bitácora..clave.foránea.","Tipo.de.parcela..testigo.o.innovación.","Nombre.de.la.sección","Cultivo.sembrado","Fecha.de.siembra","Tipo.de.semilla","Nombre.de.la.variedad.sembrada","Densidad.de.siembra.en.kg..kg.ha.","Densidad.de.siembra.en.número.de.semillas..semillas.ha.","Costo.total.de.la.semilla.a.precio.de.mercado....ha.","Costo.de.transporte.de.la.semilla....ha.","Costo.por.la.labor.de.siembra....ha.","Humedad.de.la.tierra.al.sembrar","Arreglo.de.la.siembra","Tipo.de.siembra","Tipo.de.sembradora.utilizada.para.la.siembra","Número.de.cuerpos.de.la.sembradora","X.La.sembradora.fue.prestada.por.CIMMYT.","Distancia.entre.plantas.o.matas..cm.","Número.de.semillas.por.golpe","Distancia.entre.surcos..cm.","Profundidad.de.siembra..cm.","Si.el.cultivo.es.maíz.o.frijol..indique.el.color.de.la.semilla")]

#Estandariza valores de Cultivo sembrado

siembra=Cultivosembrado(siembra)

siembra=siembra[!is.na(siembra$Cultivo.sembrado),]

siembra=unique(siembra)

#Agrega variables de Caracteristicas bitacora

bitacoras=read.csv(file='01_caracteristicas_Bitácora.csv',header=TRUE)

bitacoras=bitacoras[,c("ID.de.la.bitácora..clave.primaria.",
                                 "Año",
                                 "Ciclo.agronómico",
                                 "Tipo.de.producción")]

bitacoras=subset(bitacoras,Año>= 2012)

bitacoras=subset(bitacoras,Año <= 2019)

bitacoras=subset(bitacoras,!duplicated(ID.de.la.bitácora..clave.primaria.))

bitacoras=mayus(bitacoras,c("Ciclo.agronómico","Tipo.de.producción"))

bitacoras=sinacento(bitacoras,c("Ciclo.agronómico","Tipo.de.producción"))

bitacoras=whitespace(bitacoras,c("Ciclo.agronómico","Tipo.de.producción"))

bitacoras = mutate_at(bitacoras,vars(c("Ciclo.agronómico","Tipo.de.producción")), whiteNA)

siembra=merge.data.frame(siembra,bitacoras, by.x="ID.de.la.bitácora..clave.foránea.", by.y="ID.de.la.bitácora..clave.primaria.", all.x = TRUE)



#Fecha de siembra

siembra$Fecha.de.siembra=as.Date(substr(as.character(siembra$Fecha.de.siembra),1,10),format="%d/%m/%Y")

siembra=mutate(siembra,Fecha.de.siembra=as.Date(ifelse(year(Fecha.de.siembra)<2011,NA,Fecha.de.siembra),origin = "1970-01-01"))

descarga=as.Date("2019/02/22",format="%Y/%m/%d")

siembra=mutate(siembra,Fecha.de.siembra=as.Date(ifelse(year(Fecha.de.siembra)>descarga,NA,Fecha.de.siembra),origin = "1970-01-01"))

siembra=mutate(siembra,anio_siembra=year(Fecha.de.siembra),mes_siembra=month(Fecha.de.siembra))

siembra=subset(siembra,!is.na(Año))

#Fecha de cosecha

cosecha=read.csv(file='11_labores_Culturales.csv',header=TRUE)

cosecha=cosecha[,c("ID.de.la.bitácora..clave.foránea.","ID.de.tipo.de.bitácora..clave.foránea.","Nombre.de.la.sección","Fecha.de.la.actividad","Nombre.de.la.actividad.realizada","Cultivo.al.que.corresponde.la.actividad.de.cosecha")]

cosecha=mayus(cosecha,c("Nombre.de.la.sección","Nombre.de.la.actividad.realizada","Cultivo.al.que.corresponde.la.actividad.de.cosecha"))

cosecha=sinacento(cosecha,c("Nombre.de.la.sección","Nombre.de.la.actividad.realizada","Cultivo.al.que.corresponde.la.actividad.de.cosecha"))

cosecha=whitespace(cosecha,c("Nombre.de.la.sección","Nombre.de.la.actividad.realizada","Cultivo.al.que.corresponde.la.actividad.de.cosecha"))

cosecha = mutate_at(cosecha,vars(c("Nombre.de.la.sección","Nombre.de.la.actividad.realizada","Cultivo.al.que.corresponde.la.actividad.de.cosecha")), whiteNA)

cosecha=subset(cosecha,Nombre.de.la.sección%in%c("MI. COSECHA MANUAL","MII. COSECHA MECANICA"))

cosecha=Cultivocosecha(cosecha)

cosecha=subset(cosecha,Cultivo.al.que.corresponde.la.actividad.de.cosecha%in%CULTIVO5)

extraecosecha=function(based){
  indcosecha=rep(NA,dim(based)[1])
  j=1
  for (i in 1:dim(based)[1]){
    if (length(grep("CORTE|PIZCA|PISCA|TRILLA|COSECHA|ARRANQUE|RECOLECCION|DESGRAN|AMOGOTE",based$Nombre.de.la.actividad.realizada[i]))){
      indcosecha[j]=i
      j=j+1
    }
  }
  return(indcosecha)
}

indcosecha=extraecosecha(cosecha)

indcosecha=indcosecha[1:sum(!is.na(indcosecha))]

cosecha=cosecha[indcosecha,]

cosecha=mutate(cosecha,dummybitacoraCultivo=paste(str_pad(as.character(ID.de.la.bitácora..clave.foránea.),7,pad="0"),str_pad(as.character(ID.de.tipo.de.bitácora..clave.foránea.),7,pad="0"),Cultivo.al.que.corresponde.la.actividad.de.cosecha,sep=""))

cosecha$Fecha.de.la.actividad=as.Date(substr(as.character(cosecha$Fecha.de.la.actividad),1,10),format="%d/%m/%Y")

index0=order(cosecha$dummybitacoraCultivo,cosecha$Fecha.de.la.actividad,method = "radix")
cosecha=cosecha[index0,]

cosecha=subset(cosecha, !duplicated(dummybitacoraCultivo))

cosecha=cosecha[,c("ID.de.la.bitácora..clave.foránea.","ID.de.tipo.de.bitácora..clave.foránea.","Fecha.de.la.actividad","Cultivo.al.que.corresponde.la.actividad.de.cosecha")]

cosecha=rename(cosecha,fecha_cosecha=Fecha.de.la.actividad)

siembra=merge.data.frame(siembra,cosecha, by.x=c("ID.de.la.bitácora..clave.foránea.","ID.de.tipo.de.bitácora..clave.foránea.","Cultivo.sembrado"), by.y=c("ID.de.la.bitácora..clave.foránea.","ID.de.tipo.de.bitácora..clave.foránea.","Cultivo.al.que.corresponde.la.actividad.de.cosecha"), all.x = TRUE)

siembra=mutate(siembra,fecha_cosecha=as.Date(ifelse(year(fecha_cosecha)<2011,NA,fecha_cosecha),origin = "1970-01-01"))

siembra=mutate(siembra,longitud_cultivo=difftime(fecha_cosecha,Fecha.de.siembra,units = "days"))

siembra$longitud_cultivo=as.numeric(siembra$longitud_cultivo)

#Longitud ciclo del cultivo

longcultivo=function(based){
  n=dim(based)[1]
  ind_long_cultivo=rep(0,n)
  for (i in 1:n){
    if (based$Nombre.de.la.sección[i]=="C. SIEMBRA"){
      if (based$Cultivo.sembrado[i]=="MAIZ" & !is.na(based$longitud_cultivo[i])) {
        if (based$longitud_cultivo[i] < 100 | based$longitud_cultivo[i] >  275) ind_long_cultivo[i]=1
      }
      else if (based$Cultivo.sembrado[i]=="TRIGO" & !is.na(based$longitud_cultivo[i])){
        if (based$longitud_cultivo[i] < 100 | based$longitud_cultivo[i] > 200 ) ind_long_cultivo[i]=1
      }
      else if (based$Cultivo.sembrado[i]=="FRIJOL" & !is.na(based$longitud_cultivo[i])){
        if (based$longitud_cultivo[i] < 60 | based$longitud_cultivo[i] >  150) ind_long_cultivo[i]=1
      }
      else if (based$Cultivo.sembrado[i]=="CEBADA" & !is.na(based$longitud_cultivo[i])){
        if (based$longitud_cultivo[i] < 90 | based$longitud_cultivo[i] >  200) ind_long_cultivo[i]=1
      }
      else if (based$Cultivo.sembrado[i]=="SORGO" & !is.na(based$longitud_cultivo[i])){
        if (based$longitud_cultivo[i] < 100 | based$longitud_cultivo[i] >  200) ind_long_cultivo[i]=1
      }
    }
  }
  return(cbind(based,ind_long_cultivo))
}

siembra=longcultivo(siembra)

siembra$ind_long_cultivo=as.numeric(siembra$ind_long_cultivo)



#Diferencias en año de bitacora y año de fecha de siembra

diferente_anio=function(base_siembra){
  n=dim(base_siembra)[1]
  ind_diferencia_anio=rep(0,n)
  diferencia=rep(0,n)
  for (i in 1:n) {
    if (base_siembra$Cultivo.sembrado[i]%in%CULTIVO5 & !is.na(base_siembra$Fecha.de.siembra[i])) {
      diferencia[i]=base_siembra$Año[i]-base_siembra$anio_siembra[i]
      if (diferencia[i]==1 & base_siembra$mes_siembra[i]%in%c(1:9)) ind_diferencia_anio[i]=1
      else if (diferencia[i] > 1 | diferencia[i] <= -1 ) ind_diferencia_anio[i]=1
    }
  }
  return(cbind(base_siembra,ind_diferencia_anio,diferencia))
}

siembra=diferente_anio(siembra)

#Corrige año

corrige_anio=function(){
  n=dim(siembra)[1]
  anio=rep(NA,n)
  for (i in 1:n) {
    
    if (siembra$Cultivo.sembrado[i]%in%CULTIVO5 & siembra$Nombre.de.la.sección[i]=="C. SIEMBRA"){
      if (siembra$ind_diferencia_anio[i]==1) {
        if (siembra$ind_long_cultivo[i]==0 & !is.na(siembra$longitud_cultivo[i])) {
          anio[i]=siembra$anio_siembra[i]
        }
        else anio[i]=siembra$Año[i]
      }
      
      else {
        if (siembra$diferencia[i]==0 & siembra$mes_siembra[i]%in%c(10,11,12)){
          if (siembra$ind_long_cultivo[i]==0 & !is.na(siembra$longitud_cultivo[i])) {
            anio[i]=siembra$Año[i]+1
          }
          else anio[i]=siembra$Año[i]
        }
        else anio[i]=siembra$Año[i]
      }
    }
    else anio[i]=siembra$Año[i]
  }
  return(cbind(siembra,anio))
}

siembra=corrige_anio()


#Ciclo en base a fecha de siembra

corrige_ciclo=function() {
  temp=mutate(siembra,Dummysiembra=as.Date(gsub(" ","",paste("2016/",format(siembra$Fecha.de.siembra,"%m/%d")),fixed=TRUE),format="%Y/%m/%d"))  
  n=dim(temp)[1]
  l1=as.Date("2016/03/01",format="%Y/%m/%d")
  l2=as.Date("2016/10/01",format="%Y/%m/%d")
  ciclo=rep(NA,n)
  
  for (i in 1:n){
    if (temp$Cultivo.sembrado[i]%in%CULTIVO5 & temp$Nombre.de.la.sección[i]=="C. SIEMBRA"){
      if (!is.na(temp$Dummysiembra[i])){
        if (temp$Dummysiembra[i] >=l1 & temp$Dummysiembra[i] < l2){
          if (temp$ind_long_cultivo[i]==0 & !is.na(temp$longitud_cultivo[i])) ciclo[i]="PRIMAVERA-VERANO"
          else ciclo[i]=temp$Ciclo.agronómico[i]
        } 
        else if (temp$ind_long_cultivo[i]==0 & !is.na(temp$longitud_cultivo[i])) ciclo[i]="OTONO-INVIERNO"
        else ciclo[i]=temp$Ciclo.agronómico[i]
      }
      else ciclo[i]=temp$Ciclo.agronómico[i]
    }
    else ciclo[i]= temp$Ciclo.agronómico[i]
  }
  return(cbind(siembra,ciclo))
}

siembra=corrige_ciclo()

siembra=subset(siembra,anio!=2019)

siembra=select(siembra,-c(Año,Ciclo.agronómico,anio_siembra,mes_siembra,ind_diferencia_anio,diferencia))



#Asigna tipo de semilla por Nombre de variedad

TSEM=function(based){
  for (i in 1:dim(based)[1]){
    if (based$Cultivo.sembrado[i]%in%CULTIVO5) {
      if (!is.na(based$Tipo.de.semilla[i])){
        if (based$Tipo.de.semilla[i]!="VARIEDAD MEJORADA (O UN CRIOLLO SOMETIDO A UN PROCESO DE MEJORAMIENTO)"){
          if (length(grep("CIOLLO|CRIOL|CRIL|CFRIOLLO|CREOLLO|CRI0L|CRIIOL|CRIOOL|CRIOYO|CROLL|CRRIOL",based$Nombre.de.la.variedad.sembrada[i]))&!length(grep("HIBRIDO|MEJORAD|SELECCIONAD|ADAPTAD",based$Nombre.de.la.variedad.sembrada[i]))){
            based$Tipo.de.semilla[i]="CRIOLLO"
          }
        }
      }
      else {
        if (length(grep("CIOLLO|CRIOL|CRIL|CFRIOLLO|CREOLLO|CRI0L|CRIIOL|CRIOOL|CRIOYO|CROLL|CRRIOL",based$Nombre.de.la.variedad.sembrada[i]))&!length(grep("HIBRIDO|MEJORAD|SELECCIONAD|ADAPTAD",based$Nombre.de.la.variedad.sembrada[i]))){
          based$Tipo.de.semilla[i]="CRIOLLO"
        }
      }
      if (length(grep("MEJORAD|SELECCIONAD|ADAPTAD",based$Nombre.de.la.variedad.sembrada[i]))) {
        based$Tipo.de.semilla[i]="VARIEDAD MEJORADA (O UN CRIOLLO SOMETIDO A UN PROCESO DE MEJORAMIENTO)"
      }
      if (length(grep("MESTIZ|SINTETIC",based$Nombre.de.la.variedad.sembrada[i]))){
        based$Tipo.de.semilla[i]="MESTIZO/VARIEDAD SINTETICA"
      }
      if (length(grep("HIBRIDO",based$Nombre.de.la.variedad.sembrada[i]))){
        based$Tipo.de.semilla[i]="HIBRIDO"
      }
    }
  }
  return(based)
}

siembra=TSEM(siembra)

NVARIEDAD=function(based){
  nuevoTipoSemilla=rep(NA,dim(based)[1])
  for (i in 1:dim(based)[1]){
    if (based$Cultivo.sembrado[i]=="MAIZ"){
      
      if (is.na(based$Tipo.de.semilla[i])){
        if (length(grep("V-234|V 234|V234|V-536|V536|V 536|VS-201|VS201|VS 201|VARIEDAD CELAYA|JAGUAN|S03TLW|CAFIME|V-526|V-424|SAN JOSE|V-322|V322|V 322|SAN JUAN|HERCULES|V-560|V 560|V560|CHICHEN ITZA|SAC BEH|SAC-BEH|VS556|VS 556|VS-556",based$Nombre.de.la.variedad.sembrada[i]))){
          nuevoTipoSemilla[i]="VARIEDAD MEJORADA (O UN CRIOLLO SOMETIDO A UN PROCESO DE MEJORAMIENTO)"
        }
        else if (length(grep("CELAYA|CRISTALI|CRUZTON|CRUSTON|NAAL|XMEJE|XOOY|XOY|XNUK|XNUC|SAK BE|SAC BE|SAK NAL|SAC NAL|TORO|OLOTON|OLOTILLO|ZAPALOTE|ZAPALOLTE|CHALQUENO|PEPITILLA|CACAHUAZINTLE|TAMPIQUENO|CREMOSO|COSTENO|CAMPEON|ARROCILLO|ROCAMEY|MONTANES|OLOTE|TABASQUEN|CHIAPANEC|SAN PABLENO",based$Nombre.de.la.variedad.sembrada[i]))) {
          nuevoTipoSemilla[i]="CRIOLLO"
        }
        else if (length(grep("PALMERA|TORNADO|ZAPATA|CERE|ZARCO|ZORRO|COPLAN|CRISTIAN|CROPLAN|IMPACTO|IMPACT0|IMPALA|JAGUAR|MURANO|NOVASEM|CLTHW|CSTHW|HERME|N83|N85|N1|NA35|NA-35|NA 35|NM1078|NM-1078|NM 1078|PANTERA|CRM-52|PAS 540|PAS540|PAS-540|PIONNER|PIONER|PIONEER|PIOONER|REGA|REGGA|RW5000|RW 5000|RW-5000|SBA 470|SBA470|SBA-470|DEKALB|DEKALD|AZ 60|AZ60|AZ-60|BG 1384|BG1384|BG-1384|P 4226|P4226|P-4226|P3015|P 3015|P-3015|P 3055|P3055|P-3055|P3966|P 3966|P-3966|P4083|P 4083|P-4083|XR10|XR 10|XR-10|XR12|XR 12|XR-12|XR21|XR 21|XR-21|PUMA|ASPRO|CIMARRON|JABALI|GORILA|GARANON|CEBU|TIGRE|BARRIGA|LEOPARDO|ANTILOPE|ALBATROS|ALICANTE|ARES|ARRAYAN|BERRENDO|BIDA|BUHO|CAIMAN|CANELO|CANGURO|CARDENAL|CEBU|FAISAN|NIEBLA|OCELOTE|RETINTO|SORENTO|SULTAN|RW4000|RW 4000|RW-4000|P4082|DK357|DK-357|DK 357|DK-390|DK390|DK 390|DK - 390|DK-370|DK 370|DK370|P4063|Z-60|Z60|Z 60|H-377|H 377|H377|H-565|H565|H 565|H-50|H 50|H50|AS-722|AS 722|AS722|H-516|H 516|H516|A7573|DK 7500|DK-7500|DK7500|HC8|H-C8|DK-380|DK380|DK 380|H-318|H 318|H318|H-48|H 48|H48|DK-2027|DK 2027|DK2027|H-52|H 52|H52|30F96|215W|215 W|P3251|P3252|AS-1503|AS 1503|AS1503|H-40|H 40|H40|30F94|DK-2061|DK 2061|DK2061|DK-2034|DK2034|DK 2034|30F35|DK-7500|DK 7500|DK7500|H-515|H515|H 515|H-507|H507|H 507|HV-313|HV313|HV 313|B-33|B33|B 33|H-357|H357|H 357|H-66|H66|H 66|H-563|H563|H 563|DK-2042|DK2042|DK 2042|DK-395|DK 395|DK395|9209-W|P4081|AS-948|AS948|AS 948|DK-2027|DK2027|DK 2027|30A60|GUEPARDO|30F53|30P16|HS-23|HS23|HS 23|QT-377|QT 377|QT377|DK-2069|DK2069|DK 2069|DK-353|DK353|DK 353|DK-7088|DK7088|DK 7088|VICTORIA|GLADIADOR|KILATES|ARTILLERO|IYADILPRO|CARIBU|TITAN|ACULCO|AQUILES|AZTECA|ORION|RIO BLANCO|ALMIRANTE|BISONTE|GOLDEN|CONLEE RANCHERO|NOVACEN|LUCINO|AGUILA|DIAMANTE|GALLERO|BRONCE|BUFALO|MATADOR|MORO|P3164W|DK-2038|DK2038|DK 2038|H-562|H562|H 562|3028W|30F32|DK-2060|DK2060|DK 2060|3025W|NB11|P3057W|DK-2031|DK 2031|DK2031|P2844|P1832|P1879|DK-2020|DK2020|DK 2020|H-311|H311|H 311|CRM-28|CRM 28|CRM28|DEKALB 395|DEKALB-395|DEKALB395|DKALB 395|DKALB-395|DKALB395|H-375|H 375|H375|XR20A|3066W|AS-823|AS 823|AS823|H-374|H 374|H374|P2948W|P3258W|XR 45|XR-45|XR45|DAS2380|DK-2025|DK 2025|DK2025|P4028W|2A120|H-316|H316|H 316|H319|H 319|H-319|P3254W|RX715|EUROS|NB21|P3368W|AS-1501|AS1501|AS 1501|AS-720|AS 720|AS720|AS-900|AS 900|AS900|DK-2037|DK 2037|DK2037|H-313|H 313|H313|NB-15|NB15|NB 15|DK-234|DK234|DK 234|Z-21|Z 21|Z21|21610|21622|DK-393|DK 393|DK393|HIT-7|HIT7|HIT 7|9703|CB-427|CB 427|CB427|30P49|AS-820|AS 820|AS820|DAS2382|DAS3359|DK-2040|DK 2040|DK2040|H-378|H 378|H378|HS-15|HS 15|HS15|P1894|7525|1863W|30B74|AS-1502|AS 1502|AS1502|SB-325|SB 325|SB325|H7540|H 7540|H-7540|CLTHW14001|CLTHY13002",based$Nombre.de.la.variedad.sembrada[i]))){
          nuevoTipoSemilla[i]="HIBRIDO"
        }
        else if (length(grep("VS 535|VS535|VS-535|VS 536|VS536|VS-536|VS 558|VS558|VS-558|SB-101|SB101|SB 101",based$Nombre.de.la.variedad.sembrada[i]))){
          nuevoTipoSemilla[i]="MESTIZO/VARIEDAD SINTETICA"
        }
        else if (length(grep("TUXPENO|NATIVO|SERRANO|MAZATECO|ANCHO|PINTO|TZULUWITZ|HOJERO|OJERO|BAAQUIL|MUSHITO|ROJO|COLORADO|POZOLERO|POSOLERO|MORADO|CONICO|VANDENO|CHAPINGO|BOLITA|AZUL|NEGRO|REGION|ZONA|NORTENO|BOFO|JALA|OVANENO|TABL|REVENTADOR|ZAMORANO|BACAL|COMITECO|TEHUA|COSCO|RATON|TEPECIN|CHAPALOTE|MIXTECO|MIXE|ZANAHORIA|LOCAL",based$Nombre.de.la.variedad.sembrada[i]))){
          nuevoTipoSemilla[i]="CRIOLLO"
        }
      }
      else {
        if (based$Tipo.de.semilla[i]=="CRIOLLO"){
          if (length(grep("V-234|V 234|V234|V-536|V536|V 536|VS-201|VS201|VS 201|VARIEDAD CELAYA|JAGUAN|S03TLW|CAFIME|V-526|V-424|SAN JOSE|V-322|V322|V 322|SAN JUAN|HERCULES|V-560|V 560|V560|CHICHEN ITZA|SAC BEH|SAC-BEH|VS556|VS 556|VS-556",based$Nombre.de.la.variedad.sembrada[i]))){
            nuevoTipoSemilla[i]="VARIEDAD MEJORADA (O UN CRIOLLO SOMETIDO A UN PROCESO DE MEJORAMIENTO)"
          }
          else if (length(grep("PALMERA|TORNADO|ZAPATA|CERE|ZARCO|ZORRO|COPLAN|CRISTIAN|CROPLAN|IMPACTO|IMPACT0|IMPALA|JAGUAR|MURANO|NOVASEM|CLTHW|CSTHW|HERME|N83|N85|N1|NA35|NA-35|NA 35|NM1078|NM-1078|NM 1078|PANTERA|CRM-52|PAS 540|PAS540|PAS-540|PIONNER|PIONER|PIONEER|PIOONER|REGA|REGGA|RW5000|RW 5000|RW-5000|SBA 470|SBA470|SBA-470|DEKALB|DEKALD|AZ 60|AZ60|AZ-60|BG 1384|BG1384|BG-1384|P 4226|P4226|P-4226|P3015|P 3015|P-3015|P 3055|P3055|P-3055|P3966|P 3966|P-3966|P4083|P 4083|P-4083|XR10|XR 10|XR-10|XR12|XR 12|XR-12|XR21|XR 21|XR-21|PUMA|ASPRO|CIMARRON|JABALI|GORILA|GARANON|CEBU|TIGRE|BARRIGA|LEOPARDO|ANTILOPE|ALBATROS|ALICANTE|ARES|ARRAYAN|BERRENDO|BIDA|BUHO|CAIMAN|CANELO|CANGURO|CARDENAL|CEBU|FAISAN|NIEBLA|OCELOTE|RETINTO|SORENTO|SULTAN|RW4000|RW 4000|RW-4000|P4082|DK357|DK-357|DK 357|DK-390|DK390|DK 390|DK - 390|DK-370|DK 370|DK370|P4063|Z-60|Z60|Z 60|H-377|H 377|H377|H-565|H565|H 565|H-50|H 50|H50|AS-722|AS 722|AS722|H-516|H 516|H516|A7573|DK 7500|DK-7500|DK7500|HC8|H-C8|DK-380|DK380|DK 380|H-318|H 318|H318|H-48|H 48|H48|DK-2027|DK 2027|DK2027|H-52|H 52|H52|30F96|215W|215 W|P3251|P3252|AS-1503|AS 1503|AS1503|H-40|H 40|H40|30F94|DK-2061|DK 2061|DK2061|DK-2034|DK2034|DK 2034|30F35|DK-7500|DK 7500|DK7500|H-515|H515|H 515|H-507|H507|H 507|HV-313|HV313|HV 313|B-33|B33|B 33|H-357|H357|H 357|H-66|H66|H 66|H-563|H563|H 563|DK-2042|DK2042|DK 2042|DK-395|DK 395|DK395|9209-W|P4081|AS-948|AS948|AS 948|DK-2027|DK2027|DK 2027|30A60|GUEPARDO|30F53|30P16|HS-23|HS23|HS 23|QT-377|QT 377|QT377|DK-2069|DK2069|DK 2069|DK-353|DK353|DK 353|DK-7088|DK7088|DK 7088|VICTORIA|GLADIADOR|KILATES|ARTILLERO|IYADILPRO|CARIBU|TITAN|ACULCO|AQUILES|AZTECA|ORION|RIO BLANCO|ALMIRANTE|BISONTE|GOLDEN|CONLEE RANCHERO|NOVACEN|LUCINO|AGUILA|DIAMANTE|GALLERO|BRONCE|BUFALO|MATADOR|MORO|P3164W|DK-2038|DK2038|DK 2038|H-562|H562|H 562|3028W|30F32|DK-2060|DK2060|DK 2060|3025W|NB11|P3057W|DK-2031|DK 2031|DK2031|P2844|P1832|P1879|DK-2020|DK2020|DK 2020|H-311|H311|H 311|CRM-28|CRM 28|CRM28|DEKALB 395|DEKALB-395|DEKALB395|DKALB 395|DKALB-395|DKALB395|H-375|H 375|H375|XR20A|3066W|AS-823|AS 823|AS823|H-374|H 374|H374|P2948W|P3258W|XR 45|XR-45|XR45|DAS2380|DK-2025|DK 2025|DK2025|P4028W|2A120|H-316|H316|H 316|H319|H 319|H-319|P3254W|RX715|EUROS|NB21|P3368W|AS-1501|AS1501|AS 1501|AS-720|AS 720|AS720|AS-900|AS 900|AS900|DK-2037|DK 2037|DK2037|H-313|H 313|H313|NB-15|NB15|NB 15|DK-234|DK234|DK 234|Z-21|Z 21|Z21|21610|21622|DK-393|DK 393|DK393|HIT-7|HIT7|HIT 7|9703|CB-427|CB 427|CB427|30P49|AS-820|AS 820|AS820|DAS2382|DAS3359|DK-2040|DK 2040|DK2040|H-378|H 378|H378|HS-15|HS 15|HS15|P1894|7525|1863W|30B74|AS-1502|AS 1502|AS1502|SB-325|SB 325|SB325|H7540|H 7540|H-7540|CLTHW14001|CLTHY13002",based$Nombre.de.la.variedad.sembrada[i]))){
            nuevoTipoSemilla[i]="HIBRIDO"
          }
          else if (length(grep("VS 535|VS535|VS-535|VS 536|VS536|VS-536|VS 558|VS558|VS-558|SB-101|SB101|SB 101",based$Nombre.de.la.variedad.sembrada[i]))){
            nuevoTipoSemilla[i]="MESTIZO/VARIEDAD SINTETICA"
          }
        }
        else if (based$Tipo.de.semilla[i]=="HIBRIDO"){
          if (length(grep("V-234|V 234|V234|V-536|V536|V 536|VS-201|VS201|VS 201|VARIEDAD CELAYA|JAGUAN|S03TLW|CAFIME|V-526|V-424|SAN JOSE|V-322|V322|V 322|SAN JUAN|HERCULES|V-560|V 560|V560|CHICHEN ITZA|SAC BEH|SAC-BEH|VS556|VS 556|VS-556",based$Nombre.de.la.variedad.sembrada[i]))){
            nuevoTipoSemilla[i]="VARIEDAD MEJORADA (O UN CRIOLLO SOMETIDO A UN PROCESO DE MEJORAMIENTO)"
          }
          else if (length(grep("VS 535|VS535|VS-535|VS 536|VS536|VS-536|VS 558|VS558|VS-558|SB-101|SB101|SB 101",based$Nombre.de.la.variedad.sembrada[i]))){
            nuevoTipoSemilla[i]="MESTIZO/VARIEDAD SINTETICA"
          }
        }
        else if (based$Tipo.de.semilla[i]=="MESTIZO/VARIEDAD SINTETICA"){
          if (length(grep("V-234|V 234|V234|V-536|V536|V 536|VS-201|VS201|VS 201|VARIEDAD CELAYA|JAGUAN|S03TLW|CAFIME|V-526|V-424|SAN JOSE|V-322|V322|V 322|SAN JUAN|HERCULES|V-560|V 560|V560|CHICHEN ITZA|SAC BEH|SAC-BEH|VS556|VS 556|VS-556",based$Nombre.de.la.variedad.sembrada[i]))){
            nuevoTipoSemilla[i]="VARIEDAD MEJORADA (O UN CRIOLLO SOMETIDO A UN PROCESO DE MEJORAMIENTO)"
          }
          else if (length(grep("PALMERA|TORNADO|ZAPATA|CERE|ZARCO|ZORRO|COPLAN|CRISTIAN|CROPLAN|IMPACTO|IMPACT0|IMPALA|JAGUAR|MURANO|NOVASEM|CLTHW|CSTHW|HERME|N83|N85|N1|NA35|NA-35|NA 35|NM1078|NM-1078|NM 1078|PANTERA|CRM-52|PAS 540|PAS540|PAS-540|PIONNER|PIONER|PIONEER|PIOONER|REGA|REGGA|RW5000|RW 5000|RW-5000|SBA 470|SBA470|SBA-470|DEKALB|DEKALD|AZ 60|AZ60|AZ-60|BG 1384|BG1384|BG-1384|P 4226|P4226|P-4226|P3015|P 3015|P-3015|P 3055|P3055|P-3055|P3966|P 3966|P-3966|P4083|P 4083|P-4083|XR10|XR 10|XR-10|XR12|XR 12|XR-12|XR21|XR 21|XR-21|PUMA|ASPRO|CIMARRON|JABALI|GORILA|GARANON|CEBU|TIGRE|BARRIGA|LEOPARDO|ANTILOPE|ALBATROS|ALICANTE|ARES|ARRAYAN|BERRENDO|BIDA|BUHO|CAIMAN|CANELO|CANGURO|CARDENAL|CEBU|FAISAN|NIEBLA|OCELOTE|RETINTO|SORENTO|SULTAN|RW4000|RW 4000|RW-4000|P4082|DK357|DK-357|DK 357|DK-390|DK390|DK 390|DK - 390|DK-370|DK 370|DK370|P4063|Z-60|Z60|Z 60|H-377|H 377|H377|H-565|H565|H 565|H-50|H 50|H50|AS-722|AS 722|AS722|H-516|H 516|H516|A7573|DK 7500|DK-7500|DK7500|HC8|H-C8|DK-380|DK380|DK 380|H-318|H 318|H318|H-48|H 48|H48|DK-2027|DK 2027|DK2027|H-52|H 52|H52|30F96|215W|215 W|P3251|P3252|AS-1503|AS 1503|AS1503|H-40|H 40|H40|30F94|DK-2061|DK 2061|DK2061|DK-2034|DK2034|DK 2034|30F35|DK-7500|DK 7500|DK7500|H-515|H515|H 515|H-507|H507|H 507|HV-313|HV313|HV 313|B-33|B33|B 33|H-357|H357|H 357|H-66|H66|H 66|H-563|H563|H 563|DK-2042|DK2042|DK 2042|DK-395|DK 395|DK395|9209-W|P4081|AS-948|AS948|AS 948|DK-2027|DK2027|DK 2027|30A60|GUEPARDO|30F53|30P16|HS-23|HS23|HS 23|QT-377|QT 377|QT377|DK-2069|DK2069|DK 2069|DK-353|DK353|DK 353|DK-7088|DK7088|DK 7088|VICTORIA|GLADIADOR|KILATES|ARTILLERO|IYADILPRO|CARIBU|TITAN|ACULCO|AQUILES|AZTECA|ORION|RIO BLANCO|ALMIRANTE|BISONTE|GOLDEN|CONLEE RANCHERO|NOVACEN|LUCINO|AGUILA|DIAMANTE|GALLERO|BRONCE|BUFALO|MATADOR|MORO|P3164W|DK-2038|DK2038|DK 2038|H-562|H562|H 562|3028W|30F32|DK-2060|DK2060|DK 2060|3025W|NB11|P3057W|DK-2031|DK 2031|DK2031|P2844|P1832|P1879|DK-2020|DK2020|DK 2020|H-311|H311|H 311|CRM-28|CRM 28|CRM28|DEKALB 395|DEKALB-395|DEKALB395|DKALB 395|DKALB-395|DKALB395|H-375|H 375|H375|XR20A|3066W|AS-823|AS 823|AS823|H-374|H 374|H374|P2948W|P3258W|XR 45|XR-45|XR45|DAS2380|DK-2025|DK 2025|DK2025|P4028W|2A120|H-316|H316|H 316|H319|H 319|H-319|P3254W|RX715|EUROS|NB21|P3368W|AS-1501|AS1501|AS 1501|AS-720|AS 720|AS720|AS-900|AS 900|AS900|DK-2037|DK 2037|DK2037|H-313|H 313|H313|NB-15|NB15|NB 15|DK-234|DK234|DK 234|Z-21|Z 21|Z21|21610|21622|DK-393|DK 393|DK393|HIT-7|HIT7|HIT 7|9703|CB-427|CB 427|CB427|30P49|AS-820|AS 820|AS820|DAS2382|DAS3359|DK-2040|DK 2040|DK2040|H-378|H 378|H378|HS-15|HS 15|HS15|P1894|7525|1863W|30B74|AS-1502|AS 1502|AS1502|SB-325|SB 325|SB325|H7540|H 7540|H-7540|CLTHW14001|CLTHY13002",based$Nombre.de.la.variedad.sembrada[i]))){
            nuevoTipoSemilla[i]="HIBRIDO"
          }
        }
        else if (based$Tipo.de.semilla[i]=="VARIEDAD MEJORADA (O UN CRIOLLO SOMETIDO A UN PROCESO DE MEJORAMIENTO)"){
          if (length(grep("PALMERA|TORNADO|ZAPATA|CERE|ZARCO|ZORRO|COPLAN|CRISTIAN|CROPLAN|IMPACTO|IMPACT0|IMPALA|JAGUAR|MURANO|NOVASEM|CLTHW|CSTHW|HERME|N83|N85|N1|NA35|NA-35|NA 35|NM1078|NM-1078|NM 1078|PANTERA|CRM-52|PAS 540|PAS540|PAS-540|PIONNER|PIONER|PIONEER|PIOONER|REGA|REGGA|RW5000|RW 5000|RW-5000|SBA 470|SBA470|SBA-470|DEKALB|DEKALD|AZ 60|AZ60|AZ-60|BG 1384|BG1384|BG-1384|P 4226|P4226|P-4226|P3015|P 3015|P-3015|P 3055|P3055|P-3055|P3966|P 3966|P-3966|P4083|P 4083|P-4083|XR10|XR 10|XR-10|XR12|XR 12|XR-12|XR21|XR 21|XR-21|PUMA|ASPRO|CIMARRON|JABALI|GORILA|GARANON|CEBU|TIGRE|BARRIGA|LEOPARDO|ANTILOPE|ALBATROS|ALICANTE|ARES|ARRAYAN|BERRENDO|BIDA|BUHO|CAIMAN|CANELO|CANGURO|CARDENAL|CEBU|FAISAN|NIEBLA|OCELOTE|RETINTO|SORENTO|SULTAN|RW4000|RW 4000|RW-4000|P4082|DK357|DK-357|DK 357|DK-390|DK390|DK 390|DK - 390|DK-370|DK 370|DK370|P4063|Z-60|Z60|Z 60|H-377|H 377|H377|H-565|H565|H 565|H-50|H 50|H50|AS-722|AS 722|AS722|H-516|H 516|H516|A7573|DK 7500|DK-7500|DK7500|HC8|H-C8|DK-380|DK380|DK 380|H-318|H 318|H318|H-48|H 48|H48|DK-2027|DK 2027|DK2027|H-52|H 52|H52|30F96|215W|215 W|P3251|P3252|AS-1503|AS 1503|AS1503|H-40|H 40|H40|30F94|DK-2061|DK 2061|DK2061|DK-2034|DK2034|DK 2034|30F35|DK-7500|DK 7500|DK7500|H-515|H515|H 515|H-507|H507|H 507|HV-313|HV313|HV 313|B-33|B33|B 33|H-357|H357|H 357|H-66|H66|H 66|H-563|H563|H 563|DK-2042|DK2042|DK 2042|DK-395|DK 395|DK395|9209-W|P4081|AS-948|AS948|AS 948|DK-2027|DK2027|DK 2027|30A60|GUEPARDO|30F53|30P16|HS-23|HS23|HS 23|QT-377|QT 377|QT377|DK-2069|DK2069|DK 2069|DK-353|DK353|DK 353|DK-7088|DK7088|DK 7088|VICTORIA|GLADIADOR|KILATES|ARTILLERO|IYADILPRO|CARIBU|TITAN|ACULCO|AQUILES|AZTECA|ORION|RIO BLANCO|ALMIRANTE|BISONTE|GOLDEN|CONLEE RANCHERO|NOVACEN|LUCINO|AGUILA|DIAMANTE|GALLERO|BRONCE|BUFALO|MATADOR|MORO|P3164W|DK-2038|DK2038|DK 2038|H-562|H562|H 562|3028W|30F32|DK-2060|DK2060|DK 2060|3025W|NB11|P3057W|DK-2031|DK 2031|DK2031|P2844|P1832|P1879|DK-2020|DK2020|DK 2020|H-311|H311|H 311|CRM-28|CRM 28|CRM28|DEKALB 395|DEKALB-395|DEKALB395|DKALB 395|DKALB-395|DKALB395|H-375|H 375|H375|XR20A|3066W|AS-823|AS 823|AS823|H-374|H 374|H374|P2948W|P3258W|XR 45|XR-45|XR45|DAS2380|DK-2025|DK 2025|DK2025|P4028W|2A120|H-316|H316|H 316|H319|H 319|H-319|P3254W|RX715|EUROS|NB21|P3368W|AS-1501|AS1501|AS 1501|AS-720|AS 720|AS720|AS-900|AS 900|AS900|DK-2037|DK 2037|DK2037|H-313|H 313|H313|NB-15|NB15|NB 15|DK-234|DK234|DK 234|Z-21|Z 21|Z21|21610|21622|DK-393|DK 393|DK393|HIT-7|HIT7|HIT 7|9703|CB-427|CB 427|CB427|30P49|AS-820|AS 820|AS820|DAS2382|DAS3359|DK-2040|DK 2040|DK2040|H-378|H 378|H378|HS-15|HS 15|HS15|P1894|7525|1863W|30B74|AS-1502|AS 1502|AS1502|SB-325|SB 325|SB325|H7540|H 7540|H-7540|CLTHW14001|CLTHY13002",based$Nombre.de.la.variedad.sembrada[i]))){
            nuevoTipoSemilla[i]="HIBRIDO"
          }
          else if (length(grep("VS 535|VS535|VS-535|VS 536|VS536|VS-536|VS 558|VS558|VS-558|SB-101|SB101|SB 101",based$Nombre.de.la.variedad.sembrada[i]))){
            nuevoTipoSemilla[i]="MESTIZO/VARIEDAD SINTETICA"
          }
        }
      }
    }
    if (is.na(nuevoTipoSemilla[i])) nuevoTipoSemilla[i]=based$Tipo.de.semilla[i]
  }
  return(cbind(based,nuevoTipoSemilla))
}

siembra=NVARIEDAD(siembra)

####################Arreglo de la siembra#######################

ArrSiembra1=function(variable){
  #variable=gsub("[[:punct:]]","",variable)
  variable=gsub("HLERA","HILERA",variable)
  variable=gsub("HILLERA","HILERA",variable)
  variable=gsub("HILLERAS","HILERA",variable)
  variable=gsub("HILO","HILERA",variable)
  variable=gsub("HILOS","HILERA",variable)
  variable=gsub("COVERTURA","COBERTURA",variable)
  variable=gsub("COBERTUTA","COBERTURA",variable)
  variable=gsub("COBERURA","COBERTURA",variable)
  variable=gsub("COBETURA","COBERTURA",variable)
  variable=gsub("TRPLE","TRIPLE",variable)
  variable=gsub("SIS","SEIS",variable)
  variable=gsub("SURCADO","SURCO",variable)
  variable=gsub("HILERAS","HILERA",variable)
  variable=gsub("TRADICIOAL","TRADICIONAL",variable)
  variable=gsub("EN TRIANGULO","TRIANGULAR",variable)
  variable=gsub("SIEMBRA TRADICIONAL","TRADICIONAL",variable)
  return(variable)
}

ArrSiembra2=function(based){
  
  for (i in 1:dim(based)[1]) {

    if (length(grep("4 HILERA|CUATRO HILERA",based$Arreglo.de.la.siembra[i]))){
      based$Arreglo.de.la.siembra[i]="4 HILERAS"
    }
    else if (length(grep("5 HILERA|CINCO HILERA",based$Arreglo.de.la.siembra[i]))){
      based$Arreglo.de.la.siembra[i]="5 HILERAS"
    }
    else if (length(grep("6 HILERA|SEIS HILERA",based$Arreglo.de.la.siembra[i]))){
      based$Arreglo.de.la.siembra[i]="6 HILERAS"
    }
    else if (length(grep("7 HILERA|SIETE HILERA",based$Arreglo.de.la.siembra[i]))){
      based$Arreglo.de.la.siembra[i]="7 HILERAS"
    }
    else if (length(grep("8 HILERA|OCHO HILERA",based$Arreglo.de.la.siembra[i]))){
      based$Arreglo.de.la.siembra[i]="8 HILERAS"
    }
    else if (length(grep("VOLEO|AZAR",based$Arreglo.de.la.siembra[i]))){
      based$Arreglo.de.la.siembra[i]="AL VOLEO (DESPARRAMADO)"
    }
    else if (length(grep("DOBLE HILERA|SEMIDOBLE HILERA",based$Arreglo.de.la.siembra[i]))){
      based$Arreglo.de.la.siembra[i]="DOBLE HILERA"
    }
    else if (length(grep("TRIPLE HILERA|3 HILERA|TRES HILERA",based$Arreglo.de.la.siembra[i]))){
      based$Arreglo.de.la.siembra[i]="TRIPLE HILERA"
    }
    else if (length(grep("MATEADO|GOLPE|ESPEQUE",based$Arreglo.de.la.siembra[i]))){
      based$Arreglo.de.la.siembra[i]="MATEADO"
    }
    else if (length(grep("EN HILERA|HILERA Y MEDIO|HILERA COBERTURA TOTAL|HILERA (COBERTURA TOTAL)|HILERA EN CAMA|HILERA CONTINUA",based$Arreglo.de.la.siembra[i]))){
      based$Arreglo.de.la.siembra[i]="HILERA"
    }
    else if (length(grep("SURCO|CHORRILLO|HILERA",based$Arreglo.de.la.siembra[i]))){
      based$Arreglo.de.la.siembra[i]="HILERA"
    }
    else if (length(grep("OTRO|NORIA|90 X 60 X 90|EFECTO DE ORILLA|TRADICIONAL|DEBRADORA|CAMA|COBERTURA TOTAL|JUNTO CON EL MAIZ|TIRO ANIMAL|CURVAS A NIVEL|TRIANGULAR|TRIANGULO|TRESBOLILLO|TRES BOLILLO",based$Arreglo.de.la.siembra[i]))){
      based$Arreglo.de.la.siembra[i]="OTRO (ESPECIFIQUE)"
    }
  }
  return(based)
}

siembra=mutate(siembra,Arreglo.de.la.siembra=ArrSiembra1(siembra$Arreglo.de.la.siembra))

siembra=ArrSiembra2(x)


#TIPO SIEMBRA

TipoSiembra1=function(variable){
  variable=gsub("ENDULLO",NA,variable)
  variable=gsub("SEMIPRESICION","SEMIPRECISION",variable)
  variable=gsub("PREICION","PRECISION",variable)
  variable=gsub("PRESICION","PRECISION",variable)
  variable=gsub("JONH","JOHN",variable)
  variable=gsub("ZACAPICO","ZAPAPICO",variable)
  variable=gsub("GAZPARDO","GASPARDO",variable)
  variable=gsub("MANTEADO","MATEADO",variable)
  return(variable)
}

TipoSiembra2=function(based){
  for (i in 1:dim(based)[1]) {
    if (length(grep("MECANICA CON TRACCION ANIMAL|HECHIZA",based$Tipo.de.siembra[i]))){
      based$Tipo.de.siembra[i]="SEMBRADORA TRACCION ANIMAL"
    }
    else if (length(grep("MANUAL|ESPEQUE|MANO|PIE|GOLPE|TAPAIE|TAPAPIE|PASO|BARRETI|VOLEO|ESTACA|COA|PALA|MATRACA|PUNZON|MACANA|ZAPAPICO|YUNTA|MATEADO|SEMBRADORES|SIN SEMBRADORA|PEONES|XUL|XUULPAKAL|PERSONA",based$Tipo.de.siembra[i]))){
      based$Tipo.de.siembra[i]="MANUAL CON PALA/MATRACA/PUNZON/COA"
    }
    else if (length(grep("FAMAQ|GASPARDO|JONH DEER|SEMEATO|MOTOCULTOR|DOBLADENSE|TRACTOR|MAQUINA MULTIUSOS|NEUMATICA|MONOSEM|PIGOLLI|MECANICA|PRECISION|SEMBRADORA DEL BAJIO|SEMBRADORA Z|VOLEADORA|TROMPO|1700|SEMIPRECISION",based$Tipo.de.siembra[i]))){
      based$Tipo.de.siembra[i]="MECANICA (DE ACOPLE AL TRACTOR)"
    }
    else if (length(grep("TRACCION ANIMAL",based$Tipo.de.siembra[i]))){
      based$Tipo.de.siembra[i]="SEMBRADORA TRACCION ANIMAL"
    }
    else if (length(grep("SEMBRADORA|SURCADORA|RASTRA|Z",based$Tipo.de.siembra[i]))){
      based$Tipo.de.siembra[i]="MECANICA (DE ACOPLE AL TRACTOR)"
    }
    else if (length(grep("TRADICIONAL|NO APLICA|MANTEADO|XALCO",based$Tipo.de.siembra[i]))){
      based$Tipo.de.siembra[i]=NA
    }
    
  }
  return(based)
}

siembra=mutate(siembra,Tipo.de.siembra=TipoSiembra1(siembra$Tipo.de.siembra))

siembra=TipoSiembra2(siembra)

#Freq_TIPO_S=data.frame(table(siembra$Cultivo.sembrado,siembra$Tipo.de.siembra,useNA = "ifany"))
#Freq_TIPO_S=Freq_TIPO_S[Freq_TIPO_S$Freq!=0,]
#write.csv(Freq_TIPO_S, file = "Freq_TIPO_S.csv")  


#Tipo de sembradora

Sembradora1=function(variable){
  variable=gsub("FAMAC","FAMAQ",variable)
  variable=gsub("MARCA ","",variable)
  variable=gsub("BOLEADORA ","VOLEADORA",variable)
  variable=gsub("ADOPTADA","ADAPTADA",variable)
  variable=gsub("CORDEERO","CORDERO",variable)
  variable=gsub("CORTEZ","CORTES",variable)
  variable=gsub("SEMBRADORAS","SEMBRADORA",variable)
  variable=gsub("SEMRADORA","SEMBRADORA",variable)
  variable=gsub("GASSPARDO","GASPARDO",variable)
  variable=gsub("JUMILL","JUMIL",variable)
  variable=gsub("LAUFELL","LAUFEL",variable)
  variable=gsub("PREWSICION","PRECISION",variable)
  variable=gsub("MONOSEN","MONOSEM",variable)
  variable=gsub("VASQUEZ","VAZQUEZ",variable)
  variable=gsub("VAZQUES","VAZQUEZ",variable)
  variable=gsub("ASQUEZ","VAZQUEZ",variable)
  variable=gsub("TRADIICIONAL","TRADICIONAL",variable)
  variable=gsub("INTERNACIONAL","INTERNATIONAL",variable)
  variable=gsub("INTERNASIONAL","INTERNATIONAL",variable)
  variable=gsub("INTERNACIONAL","INTERNATIONAL",variable)
  variable=gsub("SWISEMEX","SWISSMEX",variable)
  variable=gsub("SWISMEX","SWISSMEX",variable)
  variable=gsub("PYGOLI","PIGOLI",variable)
  variable=gsub("PIGOLLI","PIGOLI",variable)
  variable=gsub("SETA","ZETA",variable)
  variable=gsub("MONOCEN","MONOSEM",variable)
  variable=gsub("MISMO ","",variable)
  variable=gsub("ELABORADA","FABRICADA",variable)
  variable=gsub("HECHA","FABRICADA",variable)
  variable=gsub("PRESICION","PRECISION",variable)
  variable=gsub("SEMIPRESICION","SEMIPRECISION",variable)
  variable=gsub("HILOS","HILERAS",variable)
  variable=gsub("HLERAS","HILERAS",variable)
  variable=gsub("CAMAS ","",variable)
  variable=gsub("TRACION","TRACCION",variable)
  variable=gsub("TRADISIONAL","TRADICIONAL",variable)
  variable=gsub("MAY PLANTER","MAXPLANTER",variable)
  variable=gsub("MAX PLANTER","MAXPLANTER",variable)
  variable=gsub("SEMBEADORA","SEMBRADORA",variable)
  return(variable)
}

Sembradora2=function(based){
  for (i in 1:dim(based)[1]) {
    if (length(grep("JOHN DEERE|MP25|MP 25|JOHN DEER|MAXPLANTER",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA JOHN DEERE"
    }
    else if (length(grep("BAJIO",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA DEL BAJIO"
    }
    else if (length(grep("NARDI",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA NARDI"
    }
    else if (length(grep("DOBLADENSE",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA DOBLADENSE"
    }
    else if (length(grep("MULTIUSO|MUMC",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="MAQUINA MULTIUSO"
    }
    else if (length(grep("VAZQUEZ",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA VAZQUEZ"
    }
    else if (length(grep("CORDERO",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA CORDERO"
    }
    else if (length(grep("MENONITA|MENONA",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA MENONITA"
    }
    else if (length(grep("GASPARDO",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA GASPARDO"
    }
    else if (length(grep("HAPPY SEED",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA HAPPY SEEDER"
    }
    else if (length(grep("MONOSEM",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA MONOSEM"
    }
    else if (length(grep("SEMEATO",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA SEMEATO"
    }
    else if (length(grep("SWISSMEX",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA SWISSMEX"
    }
    else if (length(grep("JUMIL",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA JUMIL"
    }
    else if (length(grep("APACHE",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA APACHE"
    }
    else if (length(grep("AZUL",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA AZUL"
    }
    else if (length(grep("BUFFALO",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA BUFFALO"
    }
    else if (length(grep("FAMAQ|FAMA",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA FAMAQ"
    }
    else if (length(grep("INTERNATIONAL",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA INTERNATIONAL"
    }
    else if (length(grep("PIGOLI",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA PIGOLI"
    }
    else if (length(grep("LAUFEL",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA LAUFEL"
    }
    else if (length(grep("LUMBER",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA LUMBER"
    }
    else if (length(grep("MASSEY ",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA MASSEY FERGUSON"
    }
    else if (length(grep("NEW HOLLAND",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA NEW HOLLAND"
    }
    else if (length(grep("AZTECA",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA AZTECA"
    }
    else if (length(grep("DASA|BRAVO",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA DASA"
    }
    else if (length(grep("LUCATERO",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA LUCATERO"
    }
    else if (length(grep("MUMC",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA MUMC"
    }
    else if (length(grep("CORTES",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA CORTES"
    }
    else if (length(grep("INDIANA",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA INDIANA"
    }
    else if (length(grep("FUTURA",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA MASCAR"
    }
    else if (length(grep("TATU",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA TATU"
    }
    else if (length(grep("PROSEM",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA PROSEM"
    }
    else if (length(grep("DOS HERMANOS",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA DOS HERMANOS"
    }
    else if (length(grep("3 BOTES",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA DE 3 BOTES"
    }
    else if (length(grep("MF",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA MASSEY FERGUSON"
    }
    else if (length(grep("REGION|FABRICADA POR LA ORGANIZACION|FABRICADA POR EL PRODUCTOR|FABRICADA POR ELLOS|CASERA",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA FABRICADA EN LA REGION"
    }
    else if (length(grep("ADAPTAD|ADAPTACION|MODIFICADA",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA ADAPTADA"
    }
    else if (length(grep("HECHIZA|ECHIZA",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA HECHIZA"
    }
    else if (length(grep("CATARINA",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA DE CATARINA"
    }
    else if (length(grep("CUCHARA|CUCHARITA",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA DE CUCHARA"
    }
    else if (length(grep("GOLPE",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA A GOLPE"
    }
    else if (length(grep("VOLEO|VOLEADORA",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA A VOLEO"
    }
    else if (length(grep("TIMS",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA TIMS"
    }
    else if (length(grep("TRADICIONAL|CONVENCIONAL",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA TRADICIONAL"
    }
    else if (length(grep("PRECISION NEUMATICA",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA NEUMATICA DE PRECISION"
    }
    else if (length(grep("SEMIPRECISION",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA DE SEMIPRECISION"
    }
    else if (length(grep("PLATO",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA DE PLATO"
    }
    else if (length(grep("DESCONOCID|MANUAL|ANTIGUA|HILERA|ANGULO|MATEADO|SEMBRADORES|NO VISIBLE|TRACCION ANIMAL|TIRO ANIMAL|TRIGUERA|ARADO|BLANCO|BOTADORES|BRASILENA|FORD|MASAGRO|PRODUCTOR|QUIMES|SIN MARCA|SURCADORA|TROMPO|RASTRA|6|XALCO|MECANICA|CONO DISPERSOR",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]=NA
    }
    else if (length(grep("ZETA|Z",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
      based$Tipo.de.sembradora.utilizada.para.la.siembra[i]="SEMBRADORA Z"
    }
    
  }
  
  for (i in 1:dim(based)[1]){
    if (is.na(based$Tipo.de.siembra[i])){
      if (length(grep("SEMBRADORA|MAQUINA",based$Tipo.de.sembradora.utilizada.para.la.siembra[i]))){
        based$Tipo.de.siembra[i]="MECANICA (DE ACOPLE AL TRACTOR)"
      }
    }
  }
  return(based)
}

siembra=mutate(siembra,Tipo.de.sembradora.utilizada.para.la.siembra=Sembradora1(siembra$Tipo.de.sembradora.utilizada.para.la.siembra))

siembra=Sembradora2(siembra)

#Freq_TIPO_Sembradora=data.frame(table(siembra$Cultivo.sembrado,siembra$Tipo.de.sembradora.utilizada.para.la.siembra,useNA = "ifany"))
#Freq_TIPO_Sembradora=Freq_TIPO_Sembradora[Freq_TIPO_Sembradora$Freq!=0,]
#write.csv(Freq_TIPO_Sembradora, file = "Freq_TIPO_Sembradora.csv")


#Distancia entre surcos

distancia_surco=subset(siembra,Cultivo.sembrado%in%CULTIVO5 & Nombre.de.la.seccion=="C. SIEMBRA" & Anio %in% c(2015,2016,2017) & Tipo.de.parcela.testigo.o.innovacion %in% c("PARCELA AREA DE EXTENSION","PARCELA INNOVACION","PARCELA TESTIGO"))

distancia_surco=mutate(distancia_surco,Distancia.entre.surcos.cm=ifelse(Distancia.entre.surcos.cm==0,NA,Distancia.entre.surcos.cm))

distancia_surco=distancia_surco[!is.na(distancia_surco$Distancia.entre.surcos.cm),]

x11()
ggplot(distancia_surco[distancia_surco$Cultivo.sembrado%in%c("CEBADA","MAIZ","SORGO","TRIGO"),],aes(Distancia.entre.surcos.cm,fill=Cultivo.sembrado)) + geom_histogram(aes(y=5*..density..),binwidth = 5) + scale_x_continuous(breaks = seq(0,400,10)) + labs(x="Distancia entre surcos cm", y="Frecuencia relativa") + facet_wrap(~Cultivo.sembrado,ncol=1) + theme(text = element_text(size=18))

x11()
ggplot(distancia_surco[distancia_surco$Cultivo.sembrado=="FRIJOL",],aes(Distancia.entre.surcos.cm,fill=Cultivo.sembrado)) + geom_histogram(aes(y=5*..density..),binwidth = 5) + scale_x_continuous(breaks = seq(0,400,20)) + labs(x="Distancia entre surcos cm", y="Frecuencia relativa") + facet_wrap(~Cultivo.sembrado,ncol=1) + theme(text = element_text(size=18))

distanciasurco=function(based){
  CULTIVO5=c("MAIZ","FRIJOL","TRIGO","CEBADA","SORGO")
  distancia_li=c(45,35,15,15,15)
  distancia_ls=c(400,400,200,200,200)
  for (j in 1:length(CULTIVO5)){
    for (i in 1:dim(based)[1]){
      if (based$Cultivo.sembrado[i]==CULTIVO5[j]){
        if (!is.na(based$Distancia.entre.surcos.cm[i])){
          if (based$Distancia.entre.surcos.cm[i]<distancia_li[j] | based$Distancia.entre.surcos.cm[i]>distancia_ls[j]) based$Distancia.entre.surcos.cm[i]=NA
        }
      }
    }
  }
  return(based)
}

#siembra=distanciasurco(siembra)

# Distancia entre plantas


distancia_planta=subset(siembra,Cultivo.sembrado%in%CULTIVO5 & Nombre.de.la.seccion=="C. SIEMBRA" & Anio %in% c(2015,2016,2017) & Tipo.de.parcela.testigo.o.innovacion %in% c("PARCELA AREA DE EXTENSION","PARCELA INNOVACION","PARCELA TESTIGO"))

distancia_planta=mutate(distancia_planta,Distancia.entre.plantas.o.matas.cm=ifelse(Distancia.entre.plantas.o.matas.cm==0,NA,Distancia.entre.plantas.o.matas.cm))

distancia_planta=distancia_planta[!is.na(distancia_planta$Distancia.entre.plantas.o.matas.cm),]

distancia_planta_maiz=distancia_planta[distancia_planta$Cultivo.sembrado=="MAIZ",]
distancia_planta_maiz=distancia_planta_maiz[distancia_planta_maiz$Distancia.entre.plantas.o.matas.cm<100,]


x11()
ggplot(distancia_planta_maiz,aes(Distancia.entre.plantas.o.matas.cm,fill=Arreglo.de.la.siembra)) + geom_histogram(aes(y=5*..density..),binwidth = 5) + scale_x_continuous(breaks = seq(0,100,5)) + labs(x="Distancia entre plantas cm", y="Frecuencia relativa") + facet_wrap(~Arreglo.de.la.siembra,ncol=1) + theme(text = element_text(size=18)) + theme(legend.position="bottom") + ggtitle("Distancia entre plantas en la siembra de Maíz")


x11()
ggplot(distancia_planta[distancia_planta$Cultivo.sembrado=="MAIZ",],aes(Distancia.entre.plantas.o.matas.cm,fill=Cultivo.sembrado)) + geom_histogram() + scale_x_continuous() +  facet_wrap(~Cultivo.sembrado,ncol=1)


x11()
ggplot(distancia_planta[distancia_surco$Cultivo.sembrado%in%c("CEBADA","MAIZ","SORGO","TRIGO"),],aes(Distancia.entre.surcos.cm,fill=Cultivo.sembrado)) + geom_histogram(aes(y=5*..density..),binwidth = 5) + scale_x_continuous(breaks = seq(0,400,10)) + labs(x="Distancia entre surcos cm", y="Densidad") + facet_wrap(~Cultivo.sembrado,ncol=1) + theme(text = element_text(size=18))


x11()
ggplot(distancia_surco[distancia_surco$Cultivo.sembrado=="FRIJOL",],aes(Distancia.entre.surcos.cm,fill=Cultivo.sembrado)) + geom_histogram(aes(y=5*..density..),binwidth = 5) + scale_x_continuous(breaks = seq(0,400,20)) + labs(x="Distancia entre surcos cm", y="Densidad") + facet_wrap(~Cultivo.sembrado,ncol=1) + theme(text = element_text(size=18))

distanciasurco=function(based){
  CULTIVO5=c("MAIZ","FRIJOL","TRIGO","CEBADA","SORGO")
  distancia_li=c(45,35,15,15,15)
  distancia_ls=c(400,400,200,200,200)
  for (j in 1:length(CULTIVO5)){
    for (i in 1:dim(based)[1]){
      if (based$Cultivo.sembrado[i]==CULTIVO5[j]){
        if (!is.na(based$Distancia.entre.surcos.cm[i])){
          if (based$Distancia.entre.surcos.cm[i]<distancia_li[j] | based$Distancia.entre.surcos.cm[i]>distancia_ls[j]) based$Distancia.entre.surcos.cm[i]=NA
        }
      }
    }
  }
  return(based)
}

#siembra=distanciasurco(siembra)


#Costos

#Costo semilla

CS=function(base_siembra){
  tipo_semilla=c("HIBRIDO","CRIOLLO","VARIEDAD MEJORADA (O UN CRIOLLO SOMETIDO A UN PROCESO DE MEJORAMIENTO)","MESTIZO/VARIEDAD SINTETICA")
  n=dim(siembra)[1]
  
  ind_costo_semilla=rep(0,n)
  ind_densidad=rep(0,n)
  for (i in 1:n){
    if (!is.na(base_siembra$Cultivo.sembrado[i]) & base_siembra$Cultivo.sembrado[i]=="MAIZ" & !is.na(base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]) & !is.na(base_siembra$Densidad.de.siembra.en.número.de.semillas..semillas.ha.[i])){
      if (base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]<100 | base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]>10000) ind_costo_semilla[i]=1
      if (base_siembra$Densidad.de.siembra.en.número.de.semillas..semillas.ha.[i]<1000 | base_siembra$Densidad.de.siembra.en.número.de.semillas..semillas.ha.[i]>250000) ind_densidad[i]=1
    }
    
    else if (!is.na(base_siembra$Cultivo.sembrado[i]) & base_siembra$Cultivo.sembrado[i]=="TRIGO" & !is.na(base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]) & !is.na(base_siembra$Densidad.de.siembra.en.kg..kg.ha.[i])){
      if (base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]<100 | base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]>2500) ind_costo_semilla[i]=1
      if (base_siembra$Densidad.de.siembra.en.kg..kg.ha.[i]>300 | base_siembra$Densidad.de.siembra.en.kg..kg.ha.[i]<1) ind_densidad[i]=1
    }
    
    else if (!is.na(base_siembra$Cultivo.sembrado[i]) & base_siembra$Cultivo.sembrado[i]=="SORGO" & !is.na(base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]) & !is.na(base_siembra$Densidad.de.siembra.en.kg..kg.ha.[i])){
      if (base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]<50 | base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]>3000) ind_costo_semilla[i]=1
      if (base_siembra$Densidad.de.siembra.en.kg..kg.ha.[i]>30 | base_siembra$Densidad.de.siembra.en.kg..kg.ha.[i]<1) ind_densidad[i]=1
    }
    
    else if (!is.na(base_siembra$Cultivo.sembrado[i]) & base_siembra$Cultivo.sembrado[i]=="FRIJOL" & !is.na(base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]) & !is.na(base_siembra$Densidad.de.siembra.en.kg..kg.ha.[i])){
      if (base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]<50 | base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]>3000) ind_costo_semilla[i]=1
      if (base_siembra$Densidad.de.siembra.en.kg..kg.ha.[i]>150 | base_siembra$Densidad.de.siembra.en.kg..kg.ha.[i]<1) ind_densidad[i]=1
    }
    
    else if (!is.na(base_siembra$Cultivo.sembrado[i]) & base_siembra$Cultivo.sembrado[i]=="CEBADA" & !is.na(base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]) & !is.na(base_siembra$Densidad.de.siembra.en.kg..kg.ha.[i])){
      if (base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]<50 | base_siembra$Costo.total.de.la.semilla.a.precio.de.mercado....ha.[i]>3000) ind_costo_semilla[i]=1
      if (base_siembra$Densidad.de.siembra.en.kg..kg.ha.[i]>150 | base_siembra$Densidad.de.siembra.en.kg..kg.ha.[i]<1) ind_densidad[i]=1
    }
    
        
  }
  
  temp = cbind(base_siembra,ind_costo_semilla,ind_densidad)
  temp = mutate(temp,dummybitacoraCultivo=paste(str_pad(as.character(ID.de.la.bitácora..clave.foránea.),7,pad="0"),str_pad(as.character(ID.de.tipo.de.bitácora..clave.foránea.),7,pad="0"),Cultivo.sembrado,nuevoTipoSemilla,Nombre.de.la.sección,sep=""))

  for (k in 1:4){
    temp2=subset(temp,Cultivo.sembrado=="MAIZ" & nuevoTipoSemilla==tipo_semilla[k] & ind_costo_semilla==0 & ind_densidad==0 & !is.na(Costo.total.de.la.semilla.a.precio.de.mercado....ha.) & !is.na(Densidad.de.siembra.en.número.de.semillas..semillas.ha.))
    n.outliers   <- 0.05*nrow(temp2)
    m.dist.order <- order(mahalanobis(temp2[,c("Densidad.de.siembra.en.número.de.semillas..semillas.ha.","Costo.total.de.la.semilla.a.precio.de.mercado....ha.")], colMeans(temp2[,c("Densidad.de.siembra.en.número.de.semillas..semillas.ha.","Costo.total.de.la.semilla.a.precio.de.mercado....ha.")]), cov(temp2[,c("Densidad.de.siembra.en.número.de.semillas..semillas.ha.","Costo.total.de.la.semilla.a.precio.de.mercado....ha.")])), decreasing=TRUE)
    temp2=temp2[m.dist.order[1:n.outliers],]
    temp=mutate(temp,ind_costo_semilla=ifelse(dummybitacoraCultivo%in%temp2$dummybitacoraCultivo,1,ind_costo_semilla))
  }
  
  temp2=subset(temp,Cultivo.sembrado=="TRIGO" & ind_costo_semilla==0 & ind_densidad==0 & !is.na(Costo.total.de.la.semilla.a.precio.de.mercado....ha.) & !is.na(Densidad.de.siembra.en.kg..kg.ha.))
  n.outliers   <- 0.05*nrow(temp2)
  m.dist.order <- order(mahalanobis(temp2[,c("Densidad.de.siembra.en.kg..kg.ha.","Costo.total.de.la.semilla.a.precio.de.mercado....ha.")], colMeans(temp2[,c("Densidad.de.siembra.en.kg..kg.ha.","Costo.total.de.la.semilla.a.precio.de.mercado....ha.")]), cov(temp2[,c("Densidad.de.siembra.en.kg..kg.ha.","Costo.total.de.la.semilla.a.precio.de.mercado....ha.")])), decreasing=TRUE)
  temp2=temp2[m.dist.order[1:n.outliers],]
  temp=mutate(temp,ind_costo_semilla=ifelse(dummybitacoraCultivo%in%temp2$dummybitacoraCultivo,1,ind_costo_semilla))
  
  temp2=subset(temp,Cultivo.sembrado=="SORGO" & ind_costo_semilla==0 & ind_densidad==0 & !is.na(Costo.total.de.la.semilla.a.precio.de.mercado....ha.) & !is.na(Densidad.de.siembra.en.kg..kg.ha.))
  n.outliers   <- 0.05*nrow(temp2)
  m.dist.order <- order(mahalanobis(temp2[,c("Densidad.de.siembra.en.kg..kg.ha.","Costo.total.de.la.semilla.a.precio.de.mercado....ha.")], colMeans(temp2[,c("Densidad.de.siembra.en.kg..kg.ha.","Costo.total.de.la.semilla.a.precio.de.mercado....ha.")]), cov(temp2[,c("Densidad.de.siembra.en.kg..kg.ha.","Costo.total.de.la.semilla.a.precio.de.mercado....ha.")])), decreasing=TRUE)
  temp2=temp2[m.dist.order[1:n.outliers],]
  temp=mutate(temp,ind_costo_semilla=ifelse(dummybitacoraCultivo%in%temp2$dummybitacoraCultivo,1,ind_costo_semilla))
  
  for (k in 1:3){
    temp2=subset(temp,Cultivo.sembrado=="FRIJOL" & nuevoTipoSemilla==tipo_semilla[k] & ind_costo_semilla==0 & ind_densidad==0 & !is.na(Costo.total.de.la.semilla.a.precio.de.mercado....ha.) & !is.na(Densidad.de.siembra.en.kg..kg.ha.))
    n.outliers   <- 0.02*nrow(temp2)
    m.dist.order <- order(mahalanobis(temp2[,c("Densidad.de.siembra.en.kg..kg.ha.","Costo.total.de.la.semilla.a.precio.de.mercado....ha.")], colMeans(temp2[,c("Densidad.de.siembra.en.kg..kg.ha.","Costo.total.de.la.semilla.a.precio.de.mercado....ha.")]), cov(temp2[,c("Densidad.de.siembra.en.kg..kg.ha.","Costo.total.de.la.semilla.a.precio.de.mercado....ha.")])), decreasing=TRUE)
    temp2=temp2[m.dist.order[1:n.outliers],]
    temp=mutate(temp,ind_costo_semilla=ifelse(dummybitacoraCultivo%in%temp2$dummybitacoraCultivo,1,ind_costo_semilla))
  }

  return(temp)
}



x=CS(siembra)

temp2=subset(siembra,Cultivo.sembrado=="CEBADA")

temp3=subset(temp2,!is.na(Densidad.de.siembra.en.kg..kg.ha.))

temp3=subset(temp3,Densidad.de.siembra.en.kg..kg.ha.>1 & Densidad.de.siembra.en.kg..kg.ha.< 250)

temp3=subset(temp3,Costo.total.de.la.semilla.a.precio.de.mercado....ha.>50 & Costo.total.de.la.semilla.a.precio.de.mercado....ha.<3000)


x11()
ggplot(temp3,aes(x=Densidad.de.siembra.en.kg..kg.ha.,y=Costo.total.de.la.semilla.a.precio.de.mercado....ha.))+geom_point()+theme(legend.position="bottom") +scale_x_continuous(name="Densidad de siembra Número de semillas/ha")+scale_y_continuous(name="Costo de semilla $/ha")+ggtitle("Costo de semilla de Sorgo a precio de mercado") 



temp3=select(temp3,c(Densidad.de.siembra.en.kg..kg.ha.,Costo.total.de.la.semilla.a.precio.de.mercado....ha.))

n.outliers   <- 0.05*nrow(temp3) # Mark as outliers the 2 most extreme points
m.dist.order <- order(mahalanobis(temp3, colMeans(temp3), cov(temp3)), decreasing=TRUE)
is.outlier   <- rep(FALSE, nrow(temp3))
is.outlier[m.dist.order[1:n.outliers]] <- TRUE
pch <- is.outlier * 16
x11()
plot(temp3, pch=pch,xlab="Densidad de siembra kg/ha",ylab="Costo de semilla $/ha",main="Costo de semilla de Cebada a precio de mercado")


table(temp3$nuevoTipoSemilla)

temp3=rename(temp3,Tipo_semilla=nuevoTipoSemilla)

table(temp3$Tipo_semilla)

x11()
ggplot(temp3,aes(x=Densidad.de.siembra.en.kg..kg.ha.,y=Costo.total.de.la.semilla.a.precio.de.mercado....ha.,colour=Tipo_semilla))+geom_point()+theme(legend.position="bottom") +scale_x_continuous(name="Densidad de siembra Número de semillas/ha")+scale_y_continuous(name="Costo de semilla $/ha")+ggtitle("Costo de semilla de Frijol a precio de mercado") 





