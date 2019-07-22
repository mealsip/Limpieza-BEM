rm(list=ls())
library(stringr)
library(dplyr)
library(ggplot2)
library(daewr)
library(xlsx)
library(qdapRegex)
library(lubridate)
library(multcomp)

getwd()
setwd("C:/Users/SFLORES/Documents/SIP/BEM 2018/Limpieza BD")

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

#Nombre del cultivo Cosechado

CultivoCosecha=function(based){
  for (i in 1:dim(based)[1]) {
      if (length(grep("MAIZ|CRIOLLO|MAIZANCHO|CLTHW13001",based$Nombre.del.cultivo.cosechado[i]))){
        based$Nombre.del.cultivo.cosechado[i]="MAIZ"
      }
      else if (length(grep("TITRICALE",based$Nombre.del.cultivo.cosechado[i]))){
        based$Nombre.del.cultivo.cosechado[i]="TRITICALE"
      }
      else if (length(grep("MUCUNA",based$Nombre.del.cultivo.cosechado[i]))){
        based$Nombre.del.cultivo.cosechado[i]="MUCUNA"
      }
      else if (length(grep("AGUACATE",based$Nombre.del.cultivo.cosechado[i]))){
        based$Nombre.del.cultivo.cosechado[i]="AGUACATE"
      }
      else if (length(grep("AVENA",based$Nombre.del.cultivo.cosechado[i]))){
        based$Nombre.del.cultivo.cosechado[i]="AVENA"
      }
      else if (length(grep("CALABAZA",based$Nombre.del.cultivo.cosechado[i]))){
        based$Nombre.del.cultivo.cosechado[i]="CALABAZA"
      }
      else if (length(grep("PASTO",based$Nombre.del.cultivo.cosechado[i]))){
        based$Nombre.del.cultivo.cosechado[i]="PASTO"
      }
      else if (length(grep("RABANO",based$Nombre.del.cultivo.cosechado[i]))){
        based$Nombre.del.cultivo.cosechado[i]="RABANO"
      }
      else if (length(grep("SIN INFORMACION|NINGUNO|OTRO|NO TUVO|FORRAJE|REHABILITACION|EJOTE|HOJA DE PLATANO PARA TAMALES",based$Nombre.del.cultivo.cosechado[i]))){
        based$Nombre.del.cultivo.cosechado[i]=NA
      }
  }
  return(based)
}

#Nombre del producto de interes

producto=function(based){
  for (i in 1:dim(based)[1]) {

    if (!is.na(based$Nombre.del.cultivo.cosechado[i])){
      if (based$Nombre.del.cultivo.cosechado[i]%in%c("MAIZ","TRIGO","SORGO","CEBADA")){
        if (length(grep("SEMILLA|FRUTO",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
          based$Nombre.del.producto.de.interés.económico.obtenido[i]="GRANO"
        }
        else if (length(grep("EJOTE",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
          based$Nombre.del.producto.de.interés.económico.obtenido[i]=NA
        }
      }
      else if (based$Nombre.del.cultivo.cosechado[i]%in%c("FRIJOL","CHICHARO","CROTALARIA","GARBANZO")){
        if (length(grep("SEMILLA|FRUTO",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
          based$Nombre.del.producto.de.interés.económico.obtenido[i]="GRANO"
        }
        else if (length(grep("ELOTE",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
          based$Nombre.del.producto.de.interés.económico.obtenido[i]="EJOTE"
        }
      }
      else if (based$Nombre.del.cultivo.cosechado[i]%in%c("CALABAZA")){
        if (length(grep("GRANO",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
          based$Nombre.del.producto.de.interés.económico.obtenido[i]="SEMILLA"
        }
      }
    }     

    if (length(grep("GRANO",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="GRANO"
    }
    else if (length(grep("ELOTE",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="ELOTE"
    }
    else if (length(grep("SEMILLA",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="SEMILLA"
    }
    else if (length(grep("HOJA",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="HOJA DE LA MAZORCA"
    }
    else if (length(grep("MAZORCA",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="ELOTE"
    }
    else if (length(grep("FORRAJE EN VERDE",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="FORRAJE EN VERDE"
    }
    else if (length(grep("FORRAJE|PAJA|PACA|RESIDUOS DE COSECHA|PASTURA|PASTO|HENIFICADO|ENEIFICADO|ENSILAJE|ENSILADO|SE DEJO EN PIE PARA GANADO|ZACATE MOLIDO|RASTROJO|RESIDUOS|COBERTURA",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="FORRAJE SECO"
    }
    else if (length(grep("SECO",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="FORRAJE SECO"
    }
    else if (length(grep("MAIZ|SILO",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="GRANO"
    }
    else if (length(grep("TUBERCULO|VAINAS VERDES|NARANJA|AGUACATE|ACEITUNA|CACAHUATE|CANA|CALABAZA|MANZANA|MELON|NOGAL|NUEZ|PINA|HABA",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="FRUTO"
    }
    else if (length(grep("FLOR|CALICES",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="FLOR"
    }
    else if (length(grep("PLANTULA",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="PLANTULA"
    }
    else if (length(grep("FRUTO",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="FRUTO"
    }
    
    if (length(grep("ESPECIFIQUE|NINGUNO|SINIESTRADO|PERDIDA|NO HUBO|NO HYA|NO HAy|NO SE COSECHO|NADA|NO SE TUVO|NO SE OBTUVO|SEGURO AGRICOLA|MANOJO|PLANTA COMPLETA|BIOMASA|DESARROLLO DE LA PLANTA|EN PIE|PAS 540|RICINUS|ABONO VERDE|HUITLACOCHE",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]=NA
    }
  }
  return(based)
}

#Unidad de medida

umedida=function(based){
  for (i in 1:dim(based)[1]) {
    
    if (length(grep("TON",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="TONELADA/HA"
    }
    else if (length(grep("PACA",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="PACA/HA"
    }
    else if (length(grep("COSTAL",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="COSTAL/HA"
    }
    else if (length(grep("BULTO",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="COSTAL/HA"
    }
    else if (length(grep("BOLSA",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="BOLSA/HA"
    }
    else if (length(grep("MANOJO",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="MANOJO"
    }
    else if (length(grep("ROLLO",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="ROLLO"
    }
    else if (length(grep("CAJA",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="CAJA"
    }
    else if (length(grep("CAMION|VIAJE|TRAILA|REMOLQUE|CARRO",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="VIAJE"
    }
    else if (length(grep("RASTROJO|COBERTURA",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="RASTROJO"
    }
    else if (length(grep("GAVILLA",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="GAVILLA"
    }
    else if (length(grep("CARGA",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="CARGA"
    }
    else if (length(grep("MOGOTE",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="MOGOTE"
    }
    else if (length(grep("PIEZA",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="PIEZA/HA"
    }
    else if (length(grep("LONA",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="LONA"
    }
    else if (length(grep("MELGA",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="MELGA"
    }
    else if (length(grep("KILO|KG",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="KILOGRAMO/HA"
    }
    else if (length(grep("ALMUD",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="ALMUD"
    }
    
    else {
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]=NA
    }
  }
  return(based)
}


  
#Base de rendimiento

rendimiento=read.csv(file='24_rendimiento.csv',header=TRUE)

rendimiento=rendimiento[,c("ID.de.la.bitácora..clave.foránea.","ID.de.tipo.de.bitácora..clave.foránea.","Tipo.de.parcela..testigo.o.innovación.",
"Nombre.del.cultivo.cosechado","Nombre.del.producto.de.interés.económico.obtenido","Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido","Rendimiento.real..unidad.ha.",
"Uso.que.le.da.al.producto.de.interés.económico.obtenido","Precio.de.venta.del.producto.de.interés.económico.obtenido")]

#Variables tipo Caracter

CharVar=c("Tipo.de.parcela..testigo.o.innovación.","Nombre.del.cultivo.cosechado","Nombre.del.producto.de.interés.económico.obtenido","Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido",
          "Uso.que.le.da.al.producto.de.interés.económico.obtenido")


#Limpieza variables tipo caracter

rendimiento=mayus(rendimiento,CharVar)

rendimiento=sinacento(rendimiento,CharVar)

rendimiento=whitespace(rendimiento,CharVar)

#Estandariza valores de Cultivo cosechado

rendimiento=CultivoCosecha(rendimiento)

#Estandariza valores de Producto obtenido

rendimiento=producto(rendimiento)

rendimiento=mutate(rendimiento,Nombre.del.producto.de.interés.económico.obtenido=ifelse(Nombre.del.producto.de.interés.económico.obtenido=="",NA,Nombre.del.producto.de.interés.económico.obtenido))

rendimiento=mutate(rendimiento,TipoParcela=ifelse(Tipo.de.parcela..testigo.o.innovación.=="PARCELA TESTIGO","TESTIGO","INNOVACION"))

#Estandariza valores de unidad de medida

rendimiento=umedida(rendimiento)

# Se elimina #NA´s

rendimiento=subset(rendimiento,!is.na(rendimiento$Nombre.del.cultivo.cosechado))
rendimiento=subset(rendimiento,!is.na(rendimiento$Nombre.del.producto.de.interés.económico.obtenido))
rendimiento=subset(rendimiento,!is.na(rendimiento$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido))

#Se eliminan registros con rendimiento NA o < 0

rendimiento=subset(rendimiento,Rendimiento.real..unidad.ha.>0)

#Elimina filas duplicadas

rendimiento=unique(rendimiento)

# Genera variables indicadoras para aquellos con mas de un cultivo, producto, unidad, uso

rendimiento=mutate(rendimiento,Dummy_bitacora=paste(str_pad(as.character(ID.de.la.bitácora..clave.foránea.),7,pad="0"),str_pad(as.character(ID.de.tipo.de.bitácora..clave.foránea.),7,pad="0"),sep=""))

duplicados0=subset(rendimiento,duplicated(Dummy_bitacora))

duplicados=subset(rendimiento,Dummy_bitacora%in%duplicados0$Dummy_bitacora)

index0=order(duplicados$Dummy_bitacora,duplicados$Nombre.del.cultivo.cosechado,duplicados$Nombre.del.producto.de.interés.económico.obtenido,duplicados$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido,duplicados$Uso.que.le.da.al.producto.de.interés.económico.obtenido,method = "radix")
duplicados=duplicados[index0,]

ICPUU=function(basedt){
  n=dim(basedt)[1]
  ind_cultivo=as.numeric(rep(0,n))
  ind_producto=rep(0,n)
  ind_unidad=rep(0,n)
  ind_uso=rep(0,n)
  
  dbitac=""
  ctvo=""
  prodcto=""
  unidad=""
  usoecon=""

  for (i in 1:n){
    if (dbitac!=basedt$Dummy_bitacora[i]){
      dbitac=basedt$Dummy_bitacora[i]
      if (!is.na(basedt$Nombre.del.cultivo.cosechado[i])) ctvo=basedt$Nombre.del.cultivo.cosechado[i]
      if (!is.na(basedt$Nombre.del.producto.de.interés.económico.obtenido[i])) prodcto=basedt$Nombre.del.producto.de.interés.económico.obtenido[i]
      if (!is.na(basedt$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i])) unidad=basedt$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]
      if (!is.na(basedt$Uso.que.le.da.al.producto.de.interés.económico.obtenido[i])) usoecon=basedt$Uso.que.le.da.al.producto.de.interés.económico.obtenido[i]
    }
    else {
      if (is.na(basedt$Nombre.del.cultivo.cosechado[i]) | ctvo!=basedt$Nombre.del.cultivo.cosechado[i]){
        ind_cultivo[i]=1
        if (!is.na(basedt$Nombre.del.cultivo.cosechado[i])) ctvo=basedt$Nombre.del.cultivo.cosechado[i] else ctvo=""
        if (!is.na(basedt$Nombre.del.producto.de.interés.económico.obtenido[i])) prodcto=basedt$Nombre.del.producto.de.interés.económico.obtenido[i] else prodcto=""
        if (!is.na(basedt$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i])) unidad=basedt$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i] else unidad=""
        if (!is.na(basedt$Uso.que.le.da.al.producto.de.interés.económico.obtenido[i])) usoecon=basedt$Uso.que.le.da.al.producto.de.interés.económico.obtenido[i] else usoecon=""
      }
      else {
        if (is.na(basedt$Nombre.del.producto.de.interés.económico.obtenido[i]) | prodcto!=basedt$Nombre.del.producto.de.interés.económico.obtenido[i]){
          ind_producto[i]=1
          if (!is.na(basedt$Nombre.del.producto.de.interés.económico.obtenido[i])) prodcto=basedt$Nombre.del.producto.de.interés.económico.obtenido[i] else prodcto=""
          if (!is.na(basedt$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i])) unidad=basedt$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i] else unidad=""
          if (!is.na(basedt$Uso.que.le.da.al.producto.de.interés.económico.obtenido[i])) usoecon=basedt$Uso.que.le.da.al.producto.de.interés.económico.obtenido[i] else usoecon=""
        }
        else {
          if (is.na(basedt$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]) | unidad!=basedt$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]){
            ind_unidad[i]=1
            if (!is.na(basedt$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i])) unidad=basedt$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i] else unidad=""
            if (!is.na(basedt$Uso.que.le.da.al.producto.de.interés.económico.obtenido[i])) usoecon=basedt$Uso.que.le.da.al.producto.de.interés.económico.obtenido[i] else usoecon=""
          }
          else{
            if (is.na(basedt$Uso.que.le.da.al.producto.de.interés.económico.obtenido[i]) | usoecon!=basedt$Uso.que.le.da.al.producto.de.interés.económico.obtenido[i]){
              ind_uso[i]=1
              if (!is.na(basedt$Uso.que.le.da.al.producto.de.interés.económico.obtenido[i])) usoecon=basedt$Uso.que.le.da.al.producto.de.interés.económico.obtenido[i] else usoecon=""
            }
          }
        }
      }
    }
  }
  return(cbind(Dummy_bitacora=basedt$Dummy_bitacora,ind_cultivo,ind_producto,ind_unidad,ind_uso))
}

temp=data.frame(ICPUU(duplicados))

temp$ind_cultivo=as.numeric(as.character(temp$ind_cultivo))
temp$ind_producto=as.numeric(as.character(temp$ind_producto))
temp$ind_unidad=as.numeric(as.character(temp$ind_unidad))
temp$ind_uso=as.numeric(as.character(temp$ind_uso))

temp.grouped=group_by(temp,Dummy_bitacora)

temp=summarise(temp.grouped,ind_cultivo=sum(ind_cultivo),ind_producto=sum(ind_producto),ind_unidad=sum(ind_unidad),ind_uso=sum(ind_uso))

rendimiento=merge(rendimiento,temp,by.x="Dummy_bitacora",by.y = "Dummy_bitacora", all.x = TRUE)

rendimiento=mutate(rendimiento,ind_cultivo=ifelse(is.na(ind_cultivo),0,ind_cultivo),ind_producto=ifelse(is.na(ind_producto),0,ind_producto),ind_unidad=ifelse(is.na(ind_unidad),0,ind_unidad),ind_uso=ifelse(is.na(ind_uso),0,ind_uso))


#Agrega información de bitacora


bitacora=read.csv(file='01_caracteristicas_Bitácora.csv',header=TRUE)

bitacora=bitacora[,c("ID.de.la.bitácora..clave.primaria.","Año","Ciclo.agronómico","Tipo.de.producción","ID.de.la.parcela..clave.foránea.","Nombre.de.la.institución")]

bitacora=subset(bitacora,!duplicated(ID.de.la.bitácora..clave.primaria.))

CharVarbit=c("Ciclo.agronómico","Tipo.de.producción")

bitacora=mayus(bitacora,CharVarbit)

bitacora=sinacento(bitacora,CharVarbit)

bitacora=whitespace(bitacora,CharVarbit)

rendimiento=merge.data.frame(rendimiento,bitacora,by.x ="ID.de.la.bitácora..clave.foránea.",by.y="ID.de.la.bitácora..clave.primaria.",all.x=TRUE) 

rendimiento=mutate(rendimiento,Tipo.de.producción=ifelse(Tipo.de.producción=="PUNTA DE RIEGO","RIEGO",Tipo.de.producción))


#Agrega información de parcelas

parcela=read.csv(file='04_parcela.csv',header=TRUE)

parcela=parcela[,c("ID.de.la.parcela..clave.primaria.","Superficie..ha.","Estado","Municipio","Localidad","Nombre.del.Hub","Latitud.N","Longitud.W")]

parcela=subset(parcela,!duplicated(ID.de.la.parcela..clave.primaria.))

CharVarPar=c("Estado","Municipio","Localidad","Nombre.del.Hub")

parcela=mayus(parcela,CharVarPar)

parcela=sinacento(parcela,CharVarPar)

parcela=whitespace(parcela,CharVarPar)

rendimiento=merge.data.frame(rendimiento,parcela,by.x ="ID.de.la.parcela..clave.foránea.",by.y="ID.de.la.parcela..clave.primaria.",all.x=TRUE) 

#Kilogramos a toneladas

rendimiento=mutate(rendimiento,Rendimiento.real..unidad.ha.=ifelse(Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido=="KILOGRAMO/HA",Rendimiento.real..unidad.ha./1000,Rendimiento.real..unidad.ha.))

rendimiento=mutate(rendimiento,Precio.de.venta.del.producto.de.interés.económico.obtenido=ifelse(Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido=="KILOGRAMO/HA",Precio.de.venta.del.producto.de.interés.económico.obtenido*1000,Precio.de.venta.del.producto.de.interés.económico.obtenido))

rendimiento=mutate(rendimiento,Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido=ifelse(Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido=="KILOGRAMO/HA","TONELADA/HA",Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido))


#Observaciones por encima de record

cultivos=c("MAIZ","FRIJOL","TRIGO","SORGO","CEBADA")
records=c(28.5,11.5,16.5,16.13,13.8)

rend_up=rep(0,dim(rendimiento)[1])

for (i in 1:dim(rendimiento)[1]){
  if (!is.na(rendimiento$Nombre.del.cultivo.cosechado[i]) & !is.na(rendimiento$Nombre.del.producto.de.interés.económico.obtenido[i]) & !is.na(rendimiento$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i])){
    for (j in 1:5){
      if (rendimiento$Nombre.del.cultivo.cosechado[i]==cultivos[j] & rendimiento$Nombre.del.producto.de.interés.económico.obtenido[i]=="GRANO" & rendimiento$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]=="TONELADA/HA"){
        if (rendimiento$Rendimiento.real..unidad.ha.[i] > records[j]) rend_up[i]=1 
      }
    }
  }
}

rendimiento=cbind(rendimiento,rend_up)

#MECANIZACION

#Mecanizacion aplicacion insumos

insumos=read.csv(file='08_aplicacion Insumos_descripci.csv',header=TRUE)

insumos=mutate(insumos,Dummy_bitacora=paste(str_pad(as.character(ID.de.la.bitácora..clave.foránea.),7,pad="0"),str_pad(as.character(ID.de.tipo.de.bitácora..clave.foránea.),7,pad="0"),sep=""))

insumos=mayus(insumos,"Forma.de.aplicación")

insumos=sinacento(insumos,"Forma.de.aplicación")

insumos=whitespace(insumos,"Forma.de.aplicación")

insumosMec=subset(insumos,Forma.de.aplicación!="") #solo aparece FI. FERTILIZACIÓN QUÍMICA AL SUELO

insumosMec=subset(insumosMec,Dummy_bitacora%in%rendimiento$Dummy_bitacora)

index1=order(insumosMec$Dummy_bitacora,insumosMec$Forma.de.aplicación,decreasing = TRUE,method = "radix")
insumosMec=insumosMec[index1,] #Primero Mecanica en Forma de Aplicacion

insumosMec=subset(insumosMec,!duplicated(Dummy_bitacora))

insumosMec=mutate(insumosMec,AplicacionInsumos=ifelse(Forma.de.aplicación=="(1) MANUAL","MANUAL","MECANICA"))

rendimiento=merge.data.frame(rendimiento,insumosMec[,c("Dummy_bitacora","AplicacionInsumos")],by.x ="Dummy_bitacora",by.y="Dummy_bitacora",all.x=TRUE)


#Mecanizacion siembra

siembra=read.csv(file='siembraV0.csv',header=TRUE)

siembra=mutate(siembra,Dummy_bitacora=paste(str_pad(as.character(ID.de.la.bitácora..clave.foránea.),7,pad="0"),str_pad(as.character(ID.de.tipo.de.bitácora..clave.foránea.),7,pad="0"),sep=""))

siembraMec=subset(siembra,!is.na(Tipo.de.siembra))

siembraMec=mutate(siembraMec,TipoSiembra=ifelse(Tipo.de.siembra=="MANUAL CON PALA/MATRACA/PUNZON/COA","MANUAL","MECANICA"))

index2=order(siembraMec$Dummy_bitacora,siembraMec$TipoSiembra,decreasing = TRUE,method = "radix")
siembraMec=siembraMec[index2,] #Primero Mecanica en Tipo de siembra

siembraMec=subset(siembraMec,!duplicated(Dummy_bitacora)) 

siembraMec=siembraMec[,c("Dummy_bitacora","TipoSiembra")]

rendimiento=merge.data.frame(rendimiento,siembraMec,by.x ="Dummy_bitacora",by.y="Dummy_bitacora",all.x=TRUE)

table(rendimiento$TipoSiembra,rendimiento$AplicacionInsumos,useNA = "ifany")


#Insumos

#Numero de diferentes categorias de insumos aplicados (0-8)

insumos.grouped=group_by(insumos,Dummy_bitacora,Nombre.de.la.sección)

insumosBit=summarise(insumos.grouped,freq=n())

insumos.grouped2=group_by(insumosBit,Dummy_bitacora)

insumosBit2=summarise(insumos.grouped2,freq_insumos=n())

rendimiento=merge.data.frame(rendimiento,insumosBit2,by.x ="Dummy_bitacora",by.y="Dummy_bitacora",all.x=TRUE)

rendimiento=mutate(rendimiento,freq_insumos=ifelse(is.na(freq_insumos),0,freq_insumos))

rendimiento$freq_insumos=as.factor(rendimiento$freq_insumos)

InsumoMaiz=subset(rendimiento,Nombre.del.cultivo.cosechado=="MAIZ" & Nombre.del.producto.de.interés.económico.obtenido=="GRANO" & Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido=="TONELADA/HA" & rend_up==0)

x11()
ggplot(InsumoMaiz,aes(x=freq_insumos,y=Rendimiento.real..unidad.ha.))+stat_boxplot(geom = 'errorbar',width=0.6)+geom_boxplot(width=0.6)

aov.Insumomaiz <- aov(Rendimiento.real..unidad.ha. ~ freq_insumos, data = InsumoMaiz)

summary(glht(aov.Insumomaiz, linfct = mcp(freq_insumos = "Tukey")))


#Tecnologías

tecnologia=read.csv(file='02_tecnologias.csv',header=TRUE)

tecnologia=mutate(tecnologia,Dummy_bitacora=paste(str_pad(as.character(ID.de.la.bitácora),7,pad="0"),str_pad(as.character(ID.de.tipo.de.bitácora),7,pad="0"),sep=""))

descripcionTec=data.frame(table(tecnologia$Id.tecnología,tecnologia$Descripción.de.la.tecnología))
descripcionTec=descripcionTec[descripcionTec$Freq!=0,]
descripcionTec=descripcionTec[descripcionTec$Var1!=1,]

#numero de tecnologias

tecnologia.grouped=group_by(tecnologia,Dummy_bitacora)

tecnologiaBit=summarise(tecnologia.grouped,freq_tec=n())

rendimiento=merge.data.frame(rendimiento,tecnologiaBit,by.x ="Dummy_bitacora",by.y="Dummy_bitacora",all.x=TRUE)

#tipo tecnologias Otra-1; Variedad-2; Herramienta-4; Fert-5; AC-10; Merca-12; Posc-14


index3=order(tecnologia$Dummy_bitacora,tecnologia$Id.tecnología,method = "radix")
tecnologia=tecnologia[index3,]

tecno=function(){
  bitacoraT=rep(NA,300000)
  tecnologiaT=rep(NA,300000)
  
  dbitac=""
  dtec=""
  j=1
  m=0
  
  for (i in 1:dim(tecnologia)[1]){
    if (dbitac!=tecnologia$Dummy_bitacora[i]){
      bitacoraT[j]=tecnologia$Dummy_bitacora[i]
      tecnologiaT[j]=tecnologia$Id.tecnología[i]
      dbitac=tecnologia$Dummy_bitacora[i]
      dtec=tecnologia$Id.tecnología[i]
      j=j+1
      m=m+1
    }
    else {
      if (dtec!=tecnologia$Id.tecnología[i]){
        tecnologiaT[m]=paste(tecnologiaT[m],tecnologia$Id.tecnología[i],sep="-")
        dtec=tecnologia$Id.tecnología[i]
      }
    }
  }
  return (cbind(bitacoraT,tecnologiaT))
}

TecnologiaTipo=as.data.frame(tecno())
TecnologiaTipo=subset(TecnologiaTipo,!is.na(bitacoraT))

rendimiento=merge.data.frame(rendimiento,TecnologiaTipo,by.x ="Dummy_bitacora",by.y="bitacoraT",all.x=TRUE)


#Labores culturales

labores=read.csv(file='11_labores_Culturales.csv',header=TRUE)

labores=mutate(labores,Dummy_bitacora=paste(str_pad(as.character(ID.de.la.bitácora..clave.foránea.),7,pad="0"),str_pad(as.character(ID.de.tipo.de.bitácora..clave.foránea.),7,pad="0"),sep=""))

cosecha=subset(labores,Nombre.de.la.sección%in%c("MI. COSECHA MANUAL","MII. COSECHA MECÁNICA"))

index4=order(cosecha$Dummy_bitacora,cosecha$Nombre.de.la.sección,decreasing = TRUE, method = "radix")
cosecha=cosecha[index4,]

cosecha=subset(cosecha,!duplicated(Dummy_bitacora))

cosecha=mutate(cosecha,cosecha=ifelse(Nombre.de.la.sección=="MI. COSECHA MANUAL","MANUAL","MECANICA"))

rendimiento=merge.data.frame(rendimiento,cosecha[,c("cosecha","Dummy_bitacora")],by.x ="Dummy_bitacora",by.y="Dummy_bitacora",all.x=TRUE)

#Base Preliminar

#rendimientoV1=subset(rendimiento,Nombre.del.cultivo.cosechado%in%c("MAIZ","TRIGO","FRIJOL","SORGO","CEBADA") & Nombre.del.producto.de.interés.económico.obtenido=="GRANO" & Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido=="TONELADA/HA" & rend_up==0)

#Se tienen duplicados por mas de un cultivo, mas de un uso de producto

#write.csv(rendimientoV1, file = "rendimientoV1.csv",row.names = FALSE)


#Base de Utilidad

UtilidadAE18=read.csv(file='Utilidad_AE_2018.csv',header=TRUE)

UtilidadMod18=read.csv(file='Utilidad_MOD_2018.csv',header=TRUE)

Utilidad18=rbind(UtilidadAE18,UtilidadMod18)

Utilidad18=Utilidad18[,c(1,20,2,3,6,7,11,22,24,26,28:52)]

Utilidad_12_17=read.csv(file='Utilidad_2012_2017.csv',header=TRUE)

Utilidad_12_17=Utilidad_12_17[,c(1,19,2,3,5,6,10,21,23,25,27:51)]

Utilidad_12_18=rbind(Utilidad_12_17,Utilidad18)

#Variables tipo Caracter

CharVarUtil=c("nb.Ciclo","nb.Tipo.Parcela","nb.Productor","nb.Institucion","nb.Hub","nb.Estado","nb.Municipio","nb.Localidad","nb.Cultivos.Cosechados","nb.Tipo.Produccion")

#Limpieza variables tipo caracter

Utilidad_12_18=mayus(Utilidad_12_18,CharVarUtil)

Utilidad_12_18=sinacento(Utilidad_12_18,CharVarUtil)

Utilidad_12_18=whitespace(Utilidad_12_18,CharVarUtil)

Utilidad_12_18=mutate(Utilidad_12_18,Dummy_bitacora=paste(str_pad(as.character(id.Ciclo.Agronomico),7,pad="0"),str_pad(as.character(id.Tipo.Bitacora),7,pad="0"),sep=""))

Utilidad_12_18=subset(Utilidad_12_18,!duplicated(Dummy_bitacora))

Utilidad_12_18=unique(Utilidad_12_18)

Utilidad_12_18=subset(Utilidad_12_18,nb.Cultivos.Cosechados%in%c("MAIZ","TRIGO","FRIJOL","SORGO","CEBADA","CALABAZA, MAIZ","FRIJOL, MAIZ","CALABAZA, FRIJOL, MAIZ"))

Utilidad_12_18=subset(Utilidad_12_18,INGRESOS....ha.>0)

Utilidad_12_18_Mono=subset(Utilidad_12_18,nb.Cultivos.Cosechados%in%c("MAIZ","TRIGO","FRIJOL","SORGO","CEBADA"))

rendimientoC5=subset(rendimiento,Nombre.del.cultivo.cosechado%in%c("MAIZ","TRIGO","FRIJOL","SORGO","CEBADA") & Nombre.del.producto.de.interés.económico.obtenido%in%c("GRANO","SEMILLA") & Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido=="TONELADA/HA")

rendimientoC5_DP=subset(rendimientoC5,duplicated(Dummy_bitacora)) #Duplicados por mas de un producto, uso de producto

Utilidad_12_18_Mono=subset(Utilidad_12_18_Mono,!(Dummy_bitacora%in%rendimientoC5_DP$Dummy_bitacora))

Utilidad_12_18_Mono=merge.data.frame(Utilidad_12_18_Mono,rendimientoC5[,c("Dummy_bitacora","Rendimiento.real..unidad.ha.","Precio.de.venta.del.producto.de.interés.económico.obtenido","ind_cultivo","ind_producto","ind_unidad","ind_uso","AplicacionInsumos","TipoSiembra","freq_insumos","freq_tec","tecnologiaT","cosecha")],by.x = "Dummy_bitacora",by.y = "Dummy_bitacora",all.x = TRUE)

Utilidad_12_18_Mono=subset(Utilidad_12_18_Mono,!is.na(Rendimiento.real..unidad.ha.))
