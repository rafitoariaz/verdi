rm(list = ls())

#Establecer directorio de trabajo
#setwd("/home/luis/Documents/Verdi/Analisis de datos")
#setwd("E:/Ultimo respaldo/Ubuntu/Documentos/Verdi")

#Cargar librerías
library("tibble")


##############################
#CÓDIGO PARA DAR FORMATO A LAS VENTAS

#Cargar archivo
woo.ventas<-read.csv("Base de datos/ventas a procesar woocommerce.csv")

#Renombrar columnas
colnames(woo.ventas)<-c("num_pedido","estatus_pedido","Nombre","Apellido","Fecha_compra",
                        "Cantidad","Nombre_producto","Precio_unitario","Medio_pago",
                        "Ciudad_envio","Estado_envio","Fecha_envio","Modo_envio",
                        "Total_envio")

#Seleccionar pedidos que ya fueron pagados o que fueron completados
woo.ventas<-subset(woo.ventas,woo.ventas$estatus_pedido!="En espera")

#Reemplazaar los acentos en el Nombre y Apellido en el que se envía
woo.ventas$Nombre<-chartr("áéíóúñ", "aeioun", woo.ventas$Nombre)
woo.ventas$Apellido<-chartr("áéíóúñ", "aeioun", woo.ventas$Apellido)

#Dar formato a la fecha de compra
#Dar formato a la fecha
woo.ventas$Fecha_compra<-as.Date(woo.ventas$Fecha_compra,format="%Y-%m-%d")

#Agregar la familia a la cual pertenece la planta
woo.ventas<-add_column(woo.ventas,Familia="Cactaceae",.after="Cantidad")

#Dividir el nombre del producto en tres columnas, que serían género, especie y variedad
#Dividir en varias columnas para tener toda la informacion
library("tidyr")
woo.ventas<-woo.ventas %>% separate(Nombre_producto,c("Genero","Especie","Variedad",
                                                      "Tamano_maceta","Extra"))

#Las últimas 3 columnas tienen la información del tamaño de la maceta, entonces si la sumo me
#daría el tamaño de la maceta
woo.ventas$Tamano_maceta<-rowSums(cbind(as.numeric(woo.ventas$Variedad),as.numeric(woo.ventas$Tamano_maceta),
                                 as.numeric(woo.ventas$Extra)),na.rm=TRUE)

#La columna de variedad tiene a veces la variedad u otras veces números o NA's. Si es un número
#le pongo NA, pero si no es un número dejo la columna igual y así deja el texto
woo.ventas$Variedad<-ifelse(is.na(as.numeric(woo.ventas$Variedad))==TRUE,woo.ventas$Variedad,
                          NA)

#A veces la variedad tiene la palabra "injertado/a" y necesito quitarla y substituirla por
#un NA
woo.ventas$Variedad<-ifelse(woo.ventas$Variedad=="injertado" | 
                              woo.ventas$Variedad=="injertada",NA,
                            woo.ventas$Variedad)

#Eliminar columna
woo.ventas$Extra<-NULL

#Reemplazaar los acentos en el medio de pago
woo.ventas$Medio_pago<-chartr("áéíóúñ", "aeioun", woo.ventas$Medio_pago)

#Reemplazaar los acentos en la ciudad y estado en el que se envía
woo.ventas$Ciudad_envio<-chartr("áéíóúñ", "aeioun", woo.ventas$Ciudad_envio)
woo.ventas$Estado_envio<-chartr("áéíóúñ", "aeioun", woo.ventas$Estado_envio)

#Dar formato a fecha de envío
woo.ventas$Fecha_envio<-as.Date(woo.ventas$Fecha_envio,format="%Y-%m-%d")

#Agregar columnas de Promocion, sexo y comentarios
#woo.ventas<-add_column(woo.ventas,Fecha_recibido=Sys.Date(),.after="Fecha_envio")
woo.ventas<-add_column(woo.ventas,Fecha_recibido=NA,.after="Fecha_envio")
woo.ventas<-add_column(woo.ventas,Promocion=0,.after="Fecha_recibido")
woo.ventas<-add_column(woo.ventas,Sexo="Hombre",.after="Promocion")
woo.ventas<-add_column(woo.ventas,Comentarios="NA",.after="Sexo")

#Tengo que agregar una línea que indique cómo se envió el paquete. Para ello, primero tengo que
#hacer una lista en donde cada slot contendrá el data.frame de la información de cada pedido.
#Después obtendría una base de datos con los datos del envío y la pegaría en la última fila
#de su respectivo pedido
#Hacer una lista siendo cada slot un data.frame de cada pedido
base.dividida<-split(woo.ventas,f=woo.ventas$num_pedido)

#Hacer base con la infromación de envío por pedido
fletes.info<-unique(woo.ventas[,c("num_pedido","Modo_envio","Total_envio")])

#Agregar la información del flete en la última línea de cada pedido
for (i in 1:length(base.dividida)){
  
  #Hacer una copia de una línea y pegarla en la última línea de cada pedido
  base.dividida[[i]]<-rbind(base.dividida[[i]],base.dividida[[i]][1,])
  
  #Obtener la información de envío del un pedido e específico
  flete.i<-subset(fletes.info,fletes.info$num_pedido==base.dividida[[i]][1,"num_pedido"])
  #Agregar la información del flete a la última línea
  base.dividida[[i]][nrow(base.dividida[[i]]),"Cantidad"]<-1
  base.dividida[[i]][nrow(base.dividida[[i]]),"Genero"]<-"Flete"
  base.dividida[[i]][nrow(base.dividida[[i]]),"Especie"]<-as.character(flete.i$Modo_envio)
  base.dividida[[i]][nrow(base.dividida[[i]]),"Variedad"]<-"NA"
  base.dividida[[i]][nrow(base.dividida[[i]]),"Tamano_maceta"]<-0
  base.dividida[[i]][nrow(base.dividida[[i]]),"Precio_unitario"]<-flete.i$Total_envio
}

#Hacer de la lista que tiene la información del flete un data.frame de nuevo
woo.ventas<-do.call("rbind",base.dividida)

#Copiar base dividida para usar la información de las formas de pago para agregar una línea
#en la compra de plantas
base.dividida.ventas<-base.dividida

#Eliminar columnas del flete que ya no sirvan
woo.ventas$Modo_envio<-NULL
woo.ventas$Total_envio<-NULL

#Obtener columna de total y ponerla después de la columna de total
library("tibble")
woo.ventas<-add_column(woo.ventas,Total=woo.ventas$Cantidad*woo.ventas$Precio_unitario,
                       .after="Precio_unitario")

#Agregar columnas de menudeo y medio de pago
woo.ventas<-add_column(woo.ventas,Menudeo=1,.after="Total")
woo.ventas<-add_column(woo.ventas,Medio_contacto=0,.after="Menudeo")

#Agregar la familia a la cual pertenece la planta en el caso del flete, fertilizante, sustrato
woo.ventas$Familia<-ifelse(woo.ventas$Genero=="Flete","Flete",woo.ventas$Familia)
woo.ventas$Familia<-ifelse(woo.ventas$Genero=="Sustrato","Sustrato",woo.ventas$Familia)
woo.ventas$Familia<-ifelse(woo.ventas$Genero=="Fertilizante","Fertiilzante",woo.ventas$Familia)
woo.ventas$Familia<-ifelse(woo.ventas$Genero=="Flete","Flete",woo.ventas$Familia)

#Modificar el nombre del sustrato para cactus de 1 kg y de 20 kg
woo.ventas$Genero<-ifelse(woo.ventas$Genero=="Sustrato" & woo.ventas$Tamano==1,"Sustrato cactus",
                          woo.ventas$Genero)
woo.ventas$Especie<-ifelse(woo.ventas$Genero=="Sustrato","NA",
                          woo.ventas$Especie)
woo.ventas$Variedad<-ifelse(woo.ventas$Genero=="Sustrato","NA",
                           woo.ventas$Variedad)
woo.ventas$Genero<-ifelse(woo.ventas$Genero=="Sustrato" & woo.ventas$Tamano==20,
                          "Bulto 20 kg sustrato para cactus",woo.ventas$Genero)

#Modificar el nombre de Fertilizante
woo.ventas$Especie<-ifelse(woo.ventas$Genero=="Fertilizante","NA",
                          woo.ventas$Especie)
woo.ventas$Variedad<-ifelse(woo.ventas$Genero=="Fertilizante","NA",
                           woo.ventas$Variedad)

#Agregar si se realizó la venta por internet o en el invernadero
woo.ventas<-add_column(woo.ventas,Punto_venta="Internet",.after="Comentarios")

#Exportar archivo
write.csv(woo.ventas[,c("Nombre","Apellido","Fecha_compra","Cantidad","Familia","Genero",
                        "Especie","Variedad","Tamano_maceta","Precio_unitario","Total","Menudeo",
                        "Medio_contacto","Medio_pago","Ciudad_envio","Estado_envio","Fecha_envio",
                        "Fecha_recibido","Promocion","Sexo","Comentarios","Punto_venta")], 
          file = "Base de datos/Base ventas.csv",row.names=F)


##############################
#CÓDIGO PARA DAR OBTENER UNA BASE DE DATOS CON LOS PRECIOS QUE LOS PROVEEDORES ME DAN
#library("xlsx")
#Abrir archivo. Esta hoja contiene informacion de las compras que realicé a los proovedores
#compra_plantas<-read.xlsx("Gastos ventas.xlsx",sheetIndex=1)

library("gdata")
compra_plantas<-read.xls("Base de datos/Gastos ventas.xlsx",sheet=1,header=T)

#Seleccionar las columnas de interes
compra_plantas<-compra_plantas[,c("Proveedor","Nombre","Apellido","Fecha","Cantidad","Genero",
                               "Especie","Variedad","Tamano_maceta","Precio_unitario","Total",
                               "Fecha_compra")]

#Llenar celdas vacias con NA's
compra_plantas[compra_plantas==""]<-NA 

#Dar formato a la fecha
compra_plantas$Fecha_compra<-as.Date(compra_plantas$Fecha_compra,
                                     origin = "1900-01-05")
#compra_plantas$Fecha_compra<-as.Date(as.numeric(compra_plantas$Fecha_compra),
                                     #origin = "1899-12-30")

#Seleccionar un proveedor en específico (solamente si lo necesitas en especial)
#compra_plantas<-subset(compra_plantas,compra_plantas$Proveedor=="Tono")

#Hacer una base con el último precio que me dieron mis provedores
#Obtener las columnas que necesito
compra_plantas<-compra_plantas[,c("Fecha_compra","Genero","Especie","Variedad","Tamano_maceta",
                                  "Precio_unitario")]

#Seleccionar líneas que tengan solamente información sobre las plantas
compra_plantas<-subset(compra_plantas,compra_plantas$Genero!="Comision")
#compra_plantas<-subset(compra_plantas,compra_plantas$Genero!="Flete")
compra_plantas<-subset(compra_plantas,compra_plantas$Genero!="Gasolina")
compra_plantas<-subset(compra_plantas,compra_plantas$Genero!="Salario")
compra_plantas<-subset(compra_plantas,compra_plantas$Genero!="Propina")

#Crear un dataframe que tenga como dimensión el número de especies que he comprado
base.precios.proovedores<-unique(compra_plantas[,c("Genero","Especie","Variedad",
                                                   "Tamano_maceta")])
#Agregar la fecha de compra y precio unitario
base.precios.proovedores<-add_column(base.precios.proovedores,Fecha_compra=Sys.Date(),
                                     .before="Genero")
base.precios.proovedores<-add_column(base.precios.proovedores,Precio_unitario=0,
                                     .after="Tamano_maceta")

#Tengo que separa la base por especie para tener pequeñas bases y sacar la fila con la ultima
#fecha de compra
#Hacer una lista siendo cada slot un data.frame de cada especie
base.dividida<-split(compra_plantas,f=paste(compra_plantas$Genero,compra_plantas$Especie,
                                            compra_plantas$Variedad,
                                            compra_plantas$Tamano_maceta))

#Obtener el último precio al que se me dio la planta
for (i in 1:length(base.dividida)){
  ultima.compra<-subset(base.dividida[[i]],
         base.dividida[[i]][,"Fecha_compra"]==max(base.dividida[[i]][,"Fecha_compra"]))
#Aqui no entiendo porque para Ariocarpus kotshcoubeyanus elephantidens no pega el precio
  base.precios.proovedores$Precio_unitario<-ifelse(paste(base.precios.proovedores$Genero,
                                                         base.precios.proovedores$Especie,
                                                         base.precios.proovedores$Variedad,
                                                         base.precios.proovedores$Tamano_maceta)==
                                                     paste(ultima.compra$Genero,
                                                           ultima.compra$Especie,
                                                           ultima.compra$Variedad,
                                                           ultima.compra$Tamano_maceta),
                                                   as.numeric(as.character(ultima.compra$Precio_unitario)),
                                                   base.precios.proovedores$Precio_unitario)
  
  base.precios.proovedores$Fecha_compra<-ifelse(paste(base.precios.proovedores$Genero,
                                                         base.precios.proovedores$Especie,
                                                         base.precios.proovedores$Variedad,
                                                         base.precios.proovedores$Tamano_maceta)==
                                                     paste(ultima.compra$Genero,
                                                           ultima.compra$Especie,
                                                           ultima.compra$Variedad,
                                                           ultima.compra$Tamano_maceta),
                                                ultima.compra$Fecha_compra,
                                                base.precios.proovedores$Fecha_compra)
  
  }

#Ordenar la base de datos
base.precios.proovedores<-base.precios.proovedores[order(base.precios.proovedores$Genero,
                                                         base.precios.proovedores$Especie,
                                                         base.precios.proovedores$Variedad,
                                                         base.precios.proovedores$Tamano_maceta),]

#write.csv(base.precios.proovedores,"Base precios cactus Tono.csv",row.names = FALSE)
subset(base.precios.proovedores,is.na(base.precios.proovedores$Precio_unitario)=="TRUE")[,2:5]
##############################
#CÓDIGO PARA DAR FORMATO A LAS COMPRAS QUE YO REALICE
#Copiar columnas que voy a utilizar para hacer la base de datos de cuando yo compro
#compras.verdi<-woo.ventas[,c("num_pedido","Nombre","Apellido","Cantidad","Genero","Especie","Variedad",
#                             "Precio_unitario","Tamano_maceta","Fecha_compra")]

compras.verdi<-woo.ventas[,c("num_pedido","Nombre","Apellido","Cantidad","Genero","Especie","Variedad",
                             "Tamano_maceta","Fecha_compra","Medio_pago")]

#Agregar la columna del proovedor
compras.verdi<-add_column(compras.verdi,Proveedor="Tono",.before="Nombre")

#Poner en la columna de proovedor el tipo de envío que se realizó
compras.verdi$Proveedor<-ifelse(compras.verdi$Genero=="Flete",compras.verdi$Especie,
                                compras.verdi$Proveedor)

#Agregar la columna de la fecha en la que se compró la planta
#compras.verdi<-add_column(compras.verdi,Fecha=Sys.Date(),.after="Apellido")
compras.verdi<-add_column(compras.verdi,Fecha="NA",.after="Apellido")

#Hacer merge de los precios que tengo de los proveedores
compras.verdi<-merge(compras.verdi,
                     base.precios.proovedores[,c("Genero","Especie","Variedad",
                                                 "Tamano_maceta","Precio_unitario")],
                     by=c("Genero","Especie","Variedad","Tamano_maceta"),all=T)

#Obtener las filas que tienen información de la compra que se realizó
compras.verdi<-subset(compras.verdi,is.na(compras.verdi$Nombre)=="FALSE")

#Seleccionar las columnas que necesito
compras.verdi<-compras.verdi[,c("num_pedido","Proveedor","Nombre","Apellido","Fecha","Cantidad",
                                "Genero","Especie","Variedad","Tamano_maceta","Precio_unitario",
                                "Fecha_compra","Medio_pago")]



#Tengo que agregar una línea para agregar el salario y la comisión por venta. Para ello, primero
#tengo que hacer una lista en donde cada slot contendrá el data.frame de la información de cada 
#pedido. Después obtendría el 10% de cada venta de una base de datos donde agregué el total por
#pedido
#Hacer una lista siendo cada slot un data.frame de cada pedido
base.dividida<-split(compras.verdi,f=compras.verdi$num_pedido)

#Agregar total por pedido
total.pedido<-aggregate(woo.ventas$Total,by=list(num_pedido=woo.ventas$num_pedido),FUN=sum)

#Renombrar columnas
colnames(total.pedido)[2]<-"Total"

#Agregar la información del flete en la última línea de cada pedido
for (i in 1:length(base.dividida)){
  
  #Hacer una copia de una línea y pegarla en la última línea de cada pedido
  base.dividida[[i]]<-rbind(base.dividida[[i]],base.dividida[[i]][1,])
  
  #Obtener la información de envío del un pedido e específico
  total.i<-subset(total.pedido,total.pedido$num_pedido==base.dividida[[i]][1,"num_pedido"])
  #Agregar la información del flete a la última línea
  base.dividida[[i]][nrow(base.dividida[[i]]),"Proveedor"]<-"Salario"
  base.dividida[[i]][nrow(base.dividida[[i]]),"Cantidad"]<-1
  base.dividida[[i]][nrow(base.dividida[[i]]),"Genero"]<-"Salario"
  base.dividida[[i]][nrow(base.dividida[[i]]),"Especie"]<-"NA"
  base.dividida[[i]][nrow(base.dividida[[i]]),"Variedad"]<-"NA"
  base.dividida[[i]][nrow(base.dividida[[i]]),"Tamano_maceta"]<-0
  base.dividida[[i]][nrow(base.dividida[[i]]),"Precio_unitario"]<-ifelse(total.i$Total*0.1>300,300,total.i$Total*0.1)
}

#Agregar la información de la comisión cobrada por paypal
for (i in 1:length(base.dividida)){
  if(base.dividida.ventas[[i]][1,"Medio_pago"]=="PayPal"){
  #Hacer una copia de una línea y pegarla en la última línea de cada pedido
  base.dividida[[i]]<-rbind(base.dividida[[i]],base.dividida[[i]][1,])
  
  #Agregar la información del flete a la última línea
  base.dividida[[i]][nrow(base.dividida[[i]]),"Proveedor"]<-"Comision"
  base.dividida[[i]][nrow(base.dividida[[i]]),"Cantidad"]<-1
  base.dividida[[i]][nrow(base.dividida[[i]]),"Genero"]<-"Comision"
  base.dividida[[i]][nrow(base.dividida[[i]]),"Especie"]<-"Paypal"
  base.dividida[[i]][nrow(base.dividida[[i]]),"Variedad"]<-"NA"
  base.dividida[[i]][nrow(base.dividida[[i]]),"Tamano_maceta"]<-0
  base.dividida[[i]][nrow(base.dividida[[i]]),"Precio_unitario"]<-0
  }
}

#Agregar la información de la comisión cobrada por mercado libre
for (i in 1:length(base.dividida)){
  if(base.dividida.ventas[[i]][1,"Medio_pago"]=="other"){
    #Hacer una copia de una línea y pegarla en la última línea de cada pedido
    base.dividida[[i]]<-rbind(base.dividida[[i]],base.dividida[[i]][1,])
    
    #Agregar la información del flete a la última línea
    base.dividida[[i]][nrow(base.dividida[[i]]),"Proveedor"]<-"Comision"
    base.dividida[[i]][nrow(base.dividida[[i]]),"Cantidad"]<-1
    base.dividida[[i]][nrow(base.dividida[[i]]),"Genero"]<-"Comision"
    base.dividida[[i]][nrow(base.dividida[[i]]),"Especie"]<-"Mercado Libre"
    base.dividida[[i]][nrow(base.dividida[[i]]),"Variedad"]<-"NA"
    base.dividida[[i]][nrow(base.dividida[[i]]),"Tamano_maceta"]<-0
    base.dividida[[i]][nrow(base.dividida[[i]]),"Precio_unitario"]<-0
  }
}


#Hacer de la lista que tiene la información del flete un data.frame de nuevo
compras.verdi<-do.call("rbind",base.dividida)

#Eliminar columna de medio de pago
compras.verdi$Medio_pago<-NULL

#Agregar columnas de menudeo y medio de pago
compras.verdi<-add_column(compras.verdi,Total=compras.verdi$Precio_unitario*
                            compras.verdi$Cantidad,.after="Precio_unitario")

#A veces la variedad tiene la palabra "injertado/a" y necesito quitarla y substituirla por
#un NA
compras.verdi$Variedad<-ifelse(compras.verdi$Variedad=="injertado" | 
                              compras.verdi$Variedad=="injertada",NA,
                            compras.verdi$Variedad)

#Modificar el nombre del sustrato para cactus de 1 kg y de 20 kg
compras.verdi$Genero<-ifelse(compras.verdi$Genero=="Sustrato" & compras.verdi$Tamano==1,
                             "Sustrato cactus",compras.verdi$Genero)
compras.verdi$Genero<-ifelse(compras.verdi$Genero=="Sustrato" & compras.verdi$Tamano==20,
                          "Bulto 20 kg sustrato para cactus",compras.verdi$Genero)

#Agregar la nueva información a la base anterior
#Leer base de datos
#base.compras<-read.xlsx("Gastos ventas.xlsx",sheet=1)

#Agregar la información
#base.compras<-rbind(base.compras[,c("Proveedor","Nombre","Apellido","Fecha","Cantidad","Genero",
#                                    "Especie","Variedad","Tamano_maceta","Precio_unitario",
#                                    "Fecha_compra")],
#                    compras.verdi[,c("Proveedor","Nombre","Apellido","Fecha",
#                                                  "Cantidad","Genero","Especie","Variedad",
#                                                  "Tamano_maceta","Precio_unitario","Fecha_compra")])

#Exportar archivo
write.csv(compras.verdi[,c("Proveedor","Nombre","Apellido","Fecha","Cantidad","Genero","Especie",
                           "Variedad","Tamano_maceta","Precio_unitario","Total","Fecha_compra")], 
          file = "Base de datos/Base compras.csv",row.names=F)

