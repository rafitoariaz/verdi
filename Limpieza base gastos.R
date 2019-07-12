#Limpieza de gastos hechos cada vez que se compra una planta
#Abrir archivo de gastos
gastos_plantas<-read.xlsx("Gastos ventas.xlsx",sheet=1) #Gastos que tuve al comprar plantas que los clientes me pedian, salario, comisiones por venta

#Dar formato a la fecha
gastos_plantas$Fecha<-as.Date(gastos_plantas$Fecha,origin = "1899-12-30")

#Agregar los gastos de plantas por ano
#Darle formato a la fecha de compra
gastos_plantas$Fecha_compra<-as.Date(gastos_plantas$Fecha_compra,origin = "1899-12-30")

#Extraer ano de la fecha
gastos_plantas$ano<-strftime(gastos_plantas$Fecha_compra, "%Y")
