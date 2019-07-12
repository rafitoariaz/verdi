rm(list = ls())
library("rmarkdown")

#Abrir libreria para leer archivo Excel
library("openxlsx")
library("dplyr")

#Abrir archivo. Esta hoja contiene informacion de las ventas que realize a los clientes
compras<-read.xlsx("Compra a proveedores2.xlsx",sheet=1)

#Dar formato a la fecha
compras$Fecha_compra_prov<-as.Date(as.numeric(compras$Fecha_compra_prov),origin = "1899-12-30")

#Aquí hice una lista y cada slot contiene una especie con determinado tamaño
#que le fue comprada a determinado proveedor
base.dividida<-split(compras,f=paste(compras$Proveedor,compras$Genero,
                                     compras$Especie,compras$Variedad,
                                     compras$Tamano_maceta_orig))

#Ahorita cada fila tiene la cantidad comprada de cada especie (por ejemplo, 
#2 Echinocactus grusonii). Lo que hace este loop es que por cada elemento en la
#lista va a repetir el número que existe ese producto en forma de filas (por ejemplo,
#2 Echinocactus grusoni, 2 Echinocactus grusoni). El problema es que la cantidad
#conserva el valor original de la base, por lo que en este ejemplo tendríamos 4 
#Echinocactus grusonii en vez de 2. Este problema se soluciona en la siguiente línea
for (i in 1:length(base.dividida)){
  
  base.dividida[[i]]<-as.data.frame(lapply(base.dividida[[i]], rep, 
                                           base.dividida[[i]]$Cantidad))
}


#Se une la lista para hacer un data frame y se cambian las cantidades a 1
compras2<-do.call("rbind",base.dividida) %>% 
  mutate(Cantidad=1)

#Quitar la base de datos que ya no use
rm(base.dividida,compras)

#Poner el tamano final en el que se transplanto la maceta
compras2$Tamano_maceta_final<-ifelse(is.na(compras2$Tamano_maceta_final)=="TRUE",
                                     compras2$Tamano_maceta_orig,
                                     compras2$Tamano_maceta_final)

#Agregar por nombre científico y tamano
agregado.compras<-aggregate(compras2$Cantidad,by=list(paste(compras2$Genero,
                                                      compras2$Especie,
                                                      compras2$Variedad,
                                                      compras2$Tamano_maceta_final)),
                            FUN=sum)

#Renombrar columna
colnames(agregado.compras)<-c("Nombre_cientifico","Total_inventario")

#Importar base de datos de ventas
source("Scripts/Limpieza base gastos.R")

#Obtener especies que son parte del inventario que tenemos
agregado.ventas<-subset(gastos_plantas,is.na(gastos_plantas$Clave_compra)=="FALSE")

#Quitar base de datos que no use
rm(gastos_plantas)

#Agregar por nombre cientifico y tamano
agregado.ventas<-aggregate(agregado.ventas$Cantidad,by=list(paste(agregado.ventas$Genero,
                                                            agregado.ventas$Especie,
                                                            agregado.ventas$Variedad,
                                                            agregado.ventas$Tamano_maceta)),
                            FUN=sum)

#Renombrar columna
colnames(agregado.ventas)<-c("Nombre_cientifico","Total_ventas")

#Multiplicar por -1 para realizar la resta al inventario de lo que se vendio
agregado.ventas$Total_ventas<-agregado.ventas$Total_ventas*-1

#Unir bases de inventario y de ventas
inventario<-merge(agregado.compras,agregado.ventas,by=c("Nombre_cientifico"),
                  all=TRUE)

#Realizar la resta de lo que se vendio y lo que hay en inventario
inventario$Existencias<-rowSums(inventario[,c("Total_inventario","Total_ventas")],
                                na.rm=T)

#write.csv(inventario,"borrar2.csv")

#Hacer una columna con el nombre científico y tamaño
compras2$Nombre_cientifico<-paste(compras2$Genero,
                                  compras2$Especie,
                                  compras2$Variedad,
                                  compras2$Tamano_maceta_final)

lista.productos.proveedores<-compras2[!duplicated(compras2[,c("Nombre_cientifico","Proveedor")]),]
#unique(compras2[,c("Nombre_cientifico","Proveedor")])



#Aquí estoy juntando la base que tiene las existencias con la base que tiene el
#nombre científico, proveedor y precio unitario para seleccionar especies que tengan
#una existencia menor a cierta cantidad y saber a quién le voy a pedir la planta 
#y en cuánto me la vendió
plantas.a.comprar<-merge(inventario,
                         lista.productos.proveedores[,c("Nombre_cientifico","Proveedor","Precio_unitario")],
                         by=c("Nombre_cientifico"),all=T)

#Aqui puedo ver las plantas que voy a pedir
subset(plantas.a.comprar,
       plantas.a.comprar$Existencias<2 & 
         plantas.a.comprar$Existencias>0)[,c("Existencias","Nombre_cientifico","Proveedor","Precio_unitario")]

#Función para sleccionar plantas con una existencia menor a cierta cantidad y que
#vengan de un proveedor en específico. NO SIRVE LA FUNCION
resurtir<-function(existencias,proveedor){
  ifelse(proveedor=="NA",
         subset(plantas.a.comprar,plantas.a.comprar$Existencias<=existencias)[,c("Proveedor","Nombre_cientifico","Precio_unitario","Existencias")]
         ,subset(plantas.a.comprar,plantas.a.comprar$Existencias<=existencias &
                   plantas.a.comprar$Proveedor==proveedor)[,c("Proveedor","Nombre_cientifico","Precio_unitario","Existencias")])
  
}

resurtir(existencias=1,proveedor="NA")
resurtir(existencias=1,proveedor="Tono")


library("rlist")
#Aquí quiero hacer una función para poner el genero y/o especie y/o variedad
#y/o tamano y que me lo muestre. NO FUNCIONA
seleccionar<-function(base.datos,genero,especie,variedad,tamano){
  for (i in 1:length(base.datos)){
    print(subset(base.datos,
                 list.rbind(strsplit(plantas.a.comprar$Nombre_cientifico," "))[i,1]==genero))[i,1]
  }
}

#Para checar si se está haciendo mal la operación
subset(inventario,inventario$Existencias<0) 
#ERRORES
#Lithops spa NA 3 no lo di de alta en compra proveedores
subset(inventario,
       inventario$Nombre_cientifico=="Turbinicarpus pseudomacrochele NA 3")

#Ver qué plantas voy a comprar
subset(plantas.a.comprar,plantas.a.comprar$Existencias==0)[,c("Nombre_cientifico",
                                                              "Existencias",
                                                              "Proveedor",
                                                              "Precio_unitario")]

subset(plantas.a.comprar,plantas.a.comprar$Existencias>0 & 
         plantas.a.comprar$Existencias<2)[,c("Nombre_cientifico",
                                                              "Existencias",
                                                              "Proveedor",
                                                              "Precio_unitario")]



plantas.a.comprar
compras2$Nombre_cientifico<-paste(compras2$Genero,compras2$Especie,
                                  compras2$Variedad,compras2$Tamano_maceta_final)

