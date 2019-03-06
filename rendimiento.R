
library(stringr)
library(dplyr)
library(ggplot2)
library(daewr)
library(xlsx)
library(qdapRegex)
library(lubridate)


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
      else if (length(grep("SIN INFORMACION|NINGUNO|OTRO|NO TUVO|FORRAJE|REHABILITACION",based$Nombre.del.cultivo.cosechado[i]))){
        based$Nombre.del.cultivo.cosechado[i]=NA
      }
  }
  return(based)
}

#Nombre del producto de interes

producto=function(based){
  for (i in 1:dim(based)[1]) {

    if (!is.na(based$Nombre.del.cultivo.cosechado[i])){
      if (based$Nombre.del.cultivo.cosechado[i]%in%c("MAIZ","TRIGO","SORGO")){
        if (length(grep("SEMILLA|FRUTO",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
          based$Nombre.del.producto.de.interés.económico.obtenido[i]="GRANO"
        }
        else if (length(grep("EJOTE",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
          based$Nombre.del.producto.de.interés.económico.obtenido[i]=NA
        }
      }
      else if (based$Nombre.del.cultivo.cosechado[i]=="FRIJOL"){
        if (length(grep("SEMILLA|FRUTO|EJOTE",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
          based$Nombre.del.producto.de.interés.económico.obtenido[i]="GRANO"
        }
      }
    }     

    if (length(grep("GRANO",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="GRANO"
    }
    else if (length(grep("ELOTE|MAZORCA",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="ELOTE"
    }
    else if (length(grep("SEMILLA",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="SEMILLA"
    }
    else if (length(grep("HOJA",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="HOJA DE LA MAZORCA"
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
    else if (length(grep("FRUTO|TUBERCULO|VAINAS VERDES|NARANJA|EJOTE|AGUACATE|ACEITUNA|CACAHUATE|CANA|CALABAZA|MANZANA|MELON|NOGAL|NUEZ|PINA|HABA",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="FRUTO"
    }
    else if (length(grep("FLOR|CALICES",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="FLOR"
    }
    else if (length(grep("PLANTULA",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]="PLANTULA"
    }
    else if (length(grep("NINGUNO|SINIESTRADO|PERDIDA|NO HUBO|NO HYA|NO HAy|NO SE COSECHO|NADA|NO SE TUVO|NO SE OBTUVO|OTRO|SEGURO AGRICOLA|MANOJO|PLANTA COMPLETA|BIOMASA|DESARROLLO DE LA PLANTA|EN PIE|PAS 540|RICINUS|ABONO VERDE|HUITLACOCHE",based$Nombre.del.producto.de.interés.económico.obtenido[i]))){
      based$Nombre.del.producto.de.interés.económico.obtenido[i]=NA
    }
  }
  return(based)
}

#Elimina registros sin informacion por variable

borra=function(x,based){
  z=which(is.na(x))
  if (length(z)>1) w=based[-z,]
  else w=based
  return(w)
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
    else if (length(grep("KILO|KG",based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]))){
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]="KILOGRAMO/HA"
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
    
    else {
      based$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]=NA
    }
  }
  return(based)
}

lleva_NA=function(tabla,variable1,variable2,valor1,valor2){
  w=tabla
  for (i in 1:length(variable1)){
    z=which((tabla[,variable1]%in%valor1[i]) & !(tabla[,variable2]%in%valor2))
    w[z,variable2]=NA
  }
  return(w)
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


#Estandariza valores de unidad de medida

rendimiento=umedida(rendimiento)

#Se eliminan registros con rendimiento NA o < 0

rendimiento=subset(rendimiento,Rendimiento.real..unidad.ha.>0)

#Elimina rows duplicados

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

rendimiento=merge.data.frame(rendimiento,temp,by="Dummy_bitacora",all.x=TRUE)


#Agrega información de bitacora


bitacora=read.csv(file='01_caracteristicas_Bitácora.csv',header=TRUE)

bitacora=bitacora[,c("ID.de.la.bitácora..clave.primaria.","Año","Ciclo.agronómico","Tipo.de.producción","ID.de.la.parcela..clave.foránea.")]

bitacora=subset(bitacora,!duplicated(ID.de.la.bitácora..clave.primaria.))

rendimiento=merge.data.frame(rendimiento,bitacora,by.x ="ID.de.la.bitácora..clave.foránea.",by.y="ID.de.la.bitácora..clave.primaria.",all.x=TRUE) 


#Kilogramos a toneladas

rendimiento=mutate(rendimiento,Rendimiento.real..unidad.ha.=ifelse(!is.na(Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido) & Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido=="KILOGRAMO/HA",Rendimiento.real..unidad.ha./1000,Rendimiento.real..unidad.ha.))

rendimiento=mutate(rendimiento,Precio.de.venta.del.producto.de.interés.económico.obtenido=ifelse(!is.na(Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido) & Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido=="KILOGRAMO/HA",NA,Precio.de.venta.del.producto.de.interés.económico.obtenido))

rendimiento=mutate(rendimiento,Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido=ifelse(!is.na(Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido) & Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido=="KILOGRAMO/HA","TONELADA/HA",Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido))


#Observaciones por encima de record

cultivos=c("MAIZ","FRIJOL","TRIGO","SORGO","CEBADA")
records=c(28.5,11.5,16.5,16.13,13.8)

rend_up=rep(0,dim(rendimiento)[1])

for (i in 1:dim(rendimiento)[1]){
  for (j in 1:5){
    if (!is.na(rendimiento$Nombre.del.cultivo.cosechado[i]) & !is.na(rendimiento$Nombre.del.producto.de.interés.económico.obtenido[i]) & !is.na(rendimiento$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i])){
      if (rendimiento$Nombre.del.cultivo.cosechado[i]==cultivos[j] & rendimiento$Nombre.del.producto.de.interés.económico.obtenido[i]=="GRANO" & rendimiento$Unidad.de.medida.de.rendimiento.para.el.producto.de.interés.económico.obtenido[i]=="TONELADA/HA"){
        if (rendimiento$Rendimiento.real..unidad.ha.[i] > records[j]) rend_up[i]=1 
      }
    }
  }
}

rendimiento=cbind(rendimiento,rend_up)

