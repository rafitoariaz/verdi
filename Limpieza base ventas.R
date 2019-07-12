#Limpira base de gastos ventas para analisis
#Abrir libreria para leer archivo Excel
library("openxlsx")

#Abrir archivo. Esta hoja contiene informacion de las ventas que realize a los clientes
ventas<-read.xlsx("Gastos ventas.xlsx",sheet=2)

#Llenar celdas vacias con NA's
ventas[ventas==""]<-NA 

#Las primeras 3 filas tienen informacion de un cliente que me compro plantas pero no pude
#obtener el dato de la fecha de compra, por lo que lo voy a quitar
ventas<-ventas[-1:-3,]

#Dar formato a la fecha
ventas$Fecha_compra<-as.Date(as.numeric(ventas$Fecha_compra),origin = "1899-12-30")
ventas$Fecha_envio<-as.Date(as.numeric(ventas$Fecha_envio),origin = "1899-12-30")
ventas$Fecha_recibido<-as.Date(as.numeric(ventas$Fecha_recibido),origin = "1899-12-30")

#Ventas anuales. Extraer el ano de cada venta
ventas$ano<-strftime(ventas$Fecha_compra, "%Y")
