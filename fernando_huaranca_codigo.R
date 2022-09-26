#Limpio enviroment

rm(list=ls())
#Instalo paquetes

install.packages("PCICt")
install.packages("ncdf4")
install.packages("metR")
install.packages("udunits2")
install.packages("here")
install.packages("ncdf4")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("gridExtra")

# Cargo librerias --------------------------------------------------------

library(gridExtra)
library(ncdf4)
library(udunits2)
library(metR)
library(here)
library(ncdf4)
library(lubridate)
library(ggplot2)

# ITEM A ------------------------------------------------------------------

#Borramos el enviroment

rm(list=ls())

#Seteamos la Ruta de los archivos ncdf4 utilizando la libreria here

archivo_temp = here("Temp_dec2013.nc")
archivo_altura = here("HGT_dec2013.nc")

#Vemos las dimensiones y datos relevantes del archivo utilizando una 
#funcion de la libreria metR

GlanceNetCDF(archivo_temp)
GlanceNetCDF(archivo_altura)

#Leemos y extraemos los datos del ncdf4 con la libreria metR y lo almacenamos como
#un dataframe, la cual es el output por defecto. Seleccionamos los niveles
#pedidos de manera de cargar solo lo necesario

#Obtenemos dataframe de temperatura

datos_temperatura =ReadNetCDF(archivo_temp,
                              vars="air",
                              subset = list(level=list(1000,850,500)))

#Obtenemos dataframe de altura geopotencial

datos_altura = ReadNetCDF(archivo_altura,
                          vars="hgt",
                          subset = list(level=list(1000,850,500)))


#Definimos una funcion para obtener las variables de temperatura y altura
#geopotencial

#La siguiente funcion tiene 3 argumentos

#dataframe: se le asigna un objeto de tipo dataframe con el cual operar

#nombre_variable: se le asigna como un caracter el nombre de la columna
#en la cual se encuentra la informacion deseada

#niveles: se le asigna un vector con los niveles deseados.

extrae_variables = function(dataframe,nombre_variable,niveles){
  
  #Genera una lista donde va a almacenar las variables de cada nivel
  mis_variables = list()
  
  #Se genera un ciclo que en cada iteracion obtiene informacion de un nivel
  
  for(i in 1:length(niveles)){
    
    #De mi dataframe me quedo con los datos de la columna "nombre_variable"
    #que a su vez correspondan al nivel "niveles" deseado de la columna
    #levels
    
    mis_variables[[i]] = dataframe[[as.character(nombre_variable)]][dataframe[["level"]] == niveles[i]]
  }
  
  #La funcion devuelve una lista con el largo de los niveles pedidos
  #donde en cada contenedor tengo la variable de cada nivel
  
  return(mis_variables)
}

#Utilizo la funcion creada para obtener:
#Datos de temperatura del nivel de 1000, 850 y 500 y asignarlo a una variable
lista_temperaturas =extrae_variables(datos_temperatura,"air",c(1000,850,500))

#Datos de altura geopotencial del nivel de 1000,850 y 500 y asignarlo a una variable
lista_alturas = extrae_variables(datos_altura,"hgt",c(1000,850,500))

#Concateno la informacion de cada contenedor de modo de generar un vector
#unico con la informacion de todos los niveles 

variable_temp = c(lista_temperaturas[[1]],lista_temperaturas[[2]],lista_temperaturas[[3]])
variable_alt = c(lista_alturas[[1]],lista_alturas[[2]],lista_alturas[[3]])

#Almaceno el vector en una variable binaria de tipo endian big size 4
writeBin(variable_temp,"archivobintemp",endian = "big",size=4)
writeBin(variable_alt,"archivobinalt",endian="big",size=4)

# Leo el archivo para ver si lo almacene correctamente --------------------

#La direccion de mi archivo binario

archivo_binario_1 = here("archivobintemp")
archivo_binario_2 = here("archivobinalt")

#El largo de mi vector 
N = length(lista_temperaturas[[1]])*3

#Leo mi archivo binario de tipo numerico
datos_1 = readBin(archivo_binario_1,"numeric",n=N,endian = "big",size=4)
datos_2 = readBin(archivo_binario_2,"numeric",n=N,endian = "big",size=4)
#En esta matriz reorganizo mis datos de temperatura, podria estar indicado
#en los datos del CTL

matriz_1 = array(datos_1,dim=c(85932,3))
matriz_2 = array(datos_2,dim=c(85932,3))

#Muestro una seccion de mi matriz
head(matriz_1)
head(matriz_2)


# ITEM B ------------------------------------------------------------------

#Funcion que permite graficar campos medios de temperatura o altura geopotencial

#Cuenta con los siguientes argumentos

#archivo_ncdf4 : Le pasamos la direccion donde se almacena nuestro archivo

#variable_ncdf4 : Le indicamos el nombre de nuestra variable a leer en el
#archivo ncddf4. Indicarlo como un string 

#dimensiones_array: Las dimensiones que tiene nuestro ncdf4 para generar una
#matriz. Por orden longitudxlatitudxnivelxtiempos

#nivel: Ingresamos como un vector el nivel o los niveles que deseamos
#graficar. 

#nombre_escala : El nombre de la variable con la que estabamos trabajando.
#Esta se colocora en la leyenda de nuestra guide. Se indica como un string.
#Por defecto si no se indica ningun valor se le asignara "sintitulo"

#titulo_grafico: El titulo que se colocara en la parte superior de la imagen
#Tener en cuenta a la hora de considerar uno o mas graficos, elegir un buen 
#titulo que represente lo que se observa.Se indica como string. Por defecto 
#"sintitulo".

#numero_bines: Permite cambiar el numero de los contornos en la imagen. Tener
#en cuenta que esto tambien puede afectar a la escala de colores. 
#Por recomendacion no superar los 11 bines. Se indica como un numero. Por 
#defecto es NULL.

#columnas: El numero de columnas que deseamos tener en nuestro grafico. Por
#defecto es 1. Esto es recomendable cambiar a la hora de representar varios
#graficos.

#filas: El numero de filas que deseamos tener en nuestro grafico. Por defecto
#es de 1. Esto es recomendable cambiar a la hora de representar varios graficos.

#altura_guide: Permite cambiar la altura de la guide o leyenda al lado del 
#grafico. Por defecto es de 15. 

#ancho_guide: Permite cambiar el ancho de la guide o leyenda al lado del grafico.
#Por defecto es de 2. Es recomendable 1 a la hora de hacer muchos graficos.

#celcius: Por defecto es FALSE. Si se asigna TRUE realiza un ajuste de manera
#que al trabajar con valores de temperatura en kelvin los convierte en grados
#celcius.

#colores: Por defecto un vector con paletas de colores establecidas. Es posible 
#cambiarlo para diseniar su propio grafico

campo_medio_mensual = function(archivo_ncdf4,
                               variable_ncdf4,
                               dimensiones_array,
                               nivel,
                               nombre_escala="sintitulo",
                               titulo_grafico="sintitulo",
                               numero_bines=NULL,
                               columnas=1,filas=1,
                               altura_guide=15,ancho_guide=2,
                               celcius=FALSE,
                               colores=c("BuPu","YlGnBu","RdPu","BuGn","PuRd","PuBuGn")){
  
  #########LECTURA DE ARCHIVOS NCDF4###################
  
  #Leo mi archivo y lo almaceno como un dataframe utilizando la funcion
  #ReadNetCDF de MetR
  
  df_nc = ReadNetCDF(archivo_ncdf4,vars = variable_ncdf4)
  
  #Lee mi archivo ncdf4 como una lista la cual utilizo para crear un array mas adelante
  array_nc = ReadNetCDF(archivo_ncdf4,vars = variable_ncdf4, out = c("array"))
  
  #Transformo mi archivo ncdf4 a un array con las dimensiones que necesito
  big_data_nc = array(array_nc[[1]],dim=dimensiones_array) 
  
  #Si mi variable celcius es TRUE realiza una resta a mis datos de manera 
  #de cambiar las unidades. Si no continua le ejecucion.
  
  if (celcius==TRUE){
    big_data_nc = big_data_nc - 273
  }
  
  
  #####PROCESAMIENTO DE DATOS PARA GENERAR UN DATAFRAME###########
  
  #Extraigo los niveles de presion de mi dataframe utilizando la funcion
  #unique que me permite contar solo un valor
  
  presiones = unique(df_nc$level)
  
  #Generamos dos vectores con latitudes y longitudes con los datos de df_nc
  
  valores_latitudes = unique(df_nc$lat)
  valores_longitudes = unique(df_nc$lon)
  
  #Repito la cantidad de veces necesarias la cantidad de datos para el dataframe
  #Esto lo hago para generar un nuevo dataframe de graficado
  
  #Repite y almacena los valores de latitudes cada valor de longitud
  #O sea si tengo 20 longitudes el valor de latitud se repite 20 veces
  #y luego continua al siguente valor.
  
  latitudes_nuevas = rep(valores_latitudes,each=length(valores_longitudes))
  
  #Repite y almacena los valores de longitudes la cantidad de longitudes
  longitudes_nuevas = rep(valores_longitudes,length(valores_latitudes))
  
  #Cargo de la libreria ggplot2 los datos de un mapa mundial
  mapa = map_data("world")
  
  #Vamos a graficar los contornos del mapa. Long y lat son los nombres
  #asignados a las columnas del dataframe mapa
  mi_mapa = geom_path(data = mapa,aes(long,lat,group=group),size=0.1)
  
  #Genero una lista vacia donde se almacenaran dataframes
  
  lista_vacia = list()
  
  #Extraigo el nivel que quiera obtener
  
  #El siguiente ciclo generara una lista donde en cada contenedor habra
  #un dataframe distinto. Estos seran utilizados para el graficado de cada
  #campo medio. En caso de indicarse un solo nivel solo se recorrere una vez.
  #Por cada iteracion se genera un nuevo dataframe almacenado en una lista
  
  for (i in 1:length(nivel)){
    
    #Elemento toma el valor de la posicion en la que presion y nivel sean
    #iguales. Ejemplo si en la funcion ingrese c(1000,500). En la primera
    #iteracion elemento toma el valor de 1. Y en la segunda iteracion el 
    #valor donde se halle 500.
    
    elemento = which(nivel[i]==presiones)
    
    #Fijo un nivel en mi matriz, esto indicado por el valor de elemento.
    #Mi matriz de 4 dimensiones pasa a ser de 3 dimensiones.
    #Obtengo lonxlatxtiempo
    
    matriz_3d_nivel = big_data_nc[,,elemento,]
    
    #Genera una matriz con los datos correspondientes al promedio
    #Promedio en el tiempo de modo que mi matriz tiene dimensiones de
    #lonxlat. Una grilla
    matriz_2d_nivel = apply(matriz_3d_nivel,c(1,2),mean)
    
    #Transformo a mi matriz en un vector para luego agregarlo a mi dataframe
    #Esto lo hago debido a que necesito reacomodar los datos para poder 
    #representarlos. 
    
    datos_vector = as.vector(matriz_2d_nivel)
    
    #Creamos un dataframe con los datos nuevos. Nos aseguramos de convertir
    #las longitudes utilizando una funcion de la liberia metR
    #El dataframe tiene latitudes repetidas hasta cambiar de longitud
    #datos_variable tiene la variable que voy a graficar ya sea temperatura
    #o altura geopotencial
    
    df = data.frame(lat = latitudes_nuevas, 
                    lon = ConvertLongitude(longitudes_nuevas),
                    datos_variable = datos_vector)
    
    #Agrega una columna al dataframe con el valor de la presion de ese nivel
    df$nombre_nivel = paste("Nivel",nivel[i],"hpa")
    
    #Una vez generado el dataframe lo almaceno en un contenedor de mi lista
    
    lista_vacia[[i]] = df
    
    #Reasigno el dataframe a un valor 0
    df = 0
    
    #El ciclo se repite hasta que se haya generado 1 dataframe por nivel
  }
  
  #############GRAFICADO################################
  
  #Genero una lista vacia que almacenara mis graficos de ggplot2
  
  graficos = list()
  
  #El siguiente ciclo genera graficos 1 por cada dataframe que tengamos 
  #De 1 hasta la cantidad de niveles que pedi
  for (i in 1:length(nivel)){
    
    #Por defecto la paleta de colores que estableci. 
    
    #Tabla toma el valor del contenedor i en cada vuelta del ciclo
    tabla = get("lista_vacia")[[i]]
    
    #En mi lista graficos se van almacenando los graficos de ggplot2 que genero
    
    #Tabla es mi dataframe que va variando dependiendo la iteracion. 
    #Grafico las longitudes en el eje X y latitudes en el eje Y
    
    graficos[[i]] = ggplot(tabla,mapping=aes(x=lon,y=lat))+
      
      #Relleno los contornos que voy a estar generando. Con fill stat level 
      #nos aseguramos de que la escala de color que se utilice sea discreta.
      geom_contour_filled(aes(z=datos_variable,fill=stat(level)),bins=numero_bines)+
      
      #Los contornos que voy a estar generando con mi variable. Son de color negro.
      #Numero de bines se puede cambiar para aumentar la cantidad de contornos.
      #Por defecto es NULL
      geom_contour(aes(z=datos_variable),color="black",size=0.2,bins=numero_bines)+
      
      #Indicamos la paleta de colores que queremos utilizar. Va variando segun 
      #la iteracion.Indicamos el ancho y la altura de la leyenda al lado del grafico.
      #Por defecto la escala de colores es una que se indica en la funcion
      scale_fill_distiller(palette =colores[i] ,direction = 1,super=ScaleDiscretised,
                           guide=guide_colorsteps(barheight=altura_guide,
                                                  barwidth=ancho_guide))+
      
      #Agregamos una capa con el mapa como contorno.
      mi_mapa+
      
      #Indicamos los limites de nuestro grafico. En el eje x las longitudes
      #en el eje Y las latitudes. Obtenemos los valores del dataframe correspondiente
      coord_quickmap(xlim = range(tabla$lon),ylim=range(tabla$lat),expand = FALSE)+
      
      #Los labels de cada grafico. El titulo se establece con la informacion del
      #nivel. El nombre de la escala es un argumento de la funcion
      labs(x="Longitud",y="Latitud",fill=nombre_escala,title = paste("Nivel",nivel[i],"hpa"))
    
    #Redefino el valor de mi tabla
    
    tabla = 0
    
  }
  
  #Utilizo una funcion de la libreria GridExtra para poner los graficos en una imagen
  #arrangeGrobs me permite utilizar grobs de modo de poder ingresar una lista de 
  #graficos. Con ncol y nrow establezco el numero de filas y columnas que se agregan
  #como argumento a la funcion
  #Top sera el titulo superior que ira en la imagen tambien un argumento de la funcion
  grafico_final = grid.arrange(arrangeGrob(grobs = graficos,
                                           ncol = columnas,
                                           nrow = filas,
                                           top = titulo_grafico))
  
  #La funcion me devuelve los graficos que pedi
  
  return(grafico_final)
  
}

#Genero los graficos con la funcion con las caracteristicas que deseo
imagen_1 = campo_medio_mensual(archivo_ncdf4=archivo_temp,
                               variable_ncdf4 ="air",
                               dimensiones_array= c(21,33,6,124),
                               nivel=c(1000,850,500),
                               titulo_grafico = "Campos medios mensuales de Temperatura para Diciembre",
                               columnas=3,
                               colores = c("YlOrRd","BuGn","Blues"),
                               nombre_escala = "Temperatura [Â°C]",
                               celcius=TRUE,
                               altura_guide = 10)

imagen_2 = campo_medio_mensual(archivo_ncdf4=archivo_altura,
                               variable_ncdf4 ="hgt",
                               dimensiones_array= c(21,33,6,124),
                               nivel=c(1000,850,500),
                               titulo_grafico = "Campos medios mensuales de Altura Geopotencial para Diciembre",
                               columnas=3,
                               nombre_escala="Altura geopotencial [mgp]",
                               altura_guide = 10)


#Exporto graficos

ggsave("campos_medios_temperatura.png",imagen_1,dpi=300,height = 5,width = 11)
ggsave("campos_medios_altura.png",imagen_2,dpi=300,height = 6 , width = 14)


# ITEM C ------------------------------------------------------------------

#Funcion que genera los graficos para el item C

#Esta es una funcion que permite crear campos para un determinado dia a una
#determinada hora.

#Argumentos:

#archivo_nc: Le ingresamos la direccion de nuestro archivo ncdf4

#variable: Le ingresamos la variable de nuestro archivo ncdf4.Como un character

#nivel: Le ingresamos el nivel en que deseemos trabajar. Solo se permite uno.

#dias: Le ingresamos los dias del mes en formato numerico como vector

#mi_hora: Le ingresamos la hora para la cual queremos hacer el analisis

#titulo_grafico: Ingresamos el titulo del grafico que ira en la parte superior
#como character. Por defecto es sin titulo.

#nombre_escala: Le ingresamos el nombre de nuestra variable que ira por encima
#de la leyenda o guia.Por defecto "Sintitulo"

#paleta_colores: Ingresamos la paleta de colores con la que trabajaremos en los
#graficos

#altura_guide: La altura de la barra leyenda al borde de nuestro grafico.
#Por defecto es 15.

#ancho_guide: El ancho de la barra leyenda al borde de nuestro grafico.
#Por defecto es 2.



campos_diarios = function(archivo_nc,
                          variable,
                          nivel,
                          dias,
                          mi_hora,
                          titulo_grafico="SinTitulo",
                          nombre_escala="Sintitulo",
                          paleta_colores="YlGnBu",
                          altura_guide=15,
                          ancho_guide=2,
                          celcius = FALSE){
  
  #######PROCESAMIENTO DE DATOS########################
  
  #Leo y almaceno mi archivo ncdf4 tomando el nivel que requiero utilizando la
  #libreria de metR.
  nc = ReadNetCDF(archivo_nc,vars = variable,subset=list(level=nivel))
  
  ###LISTA QUE CONTIENE DATAFRAMES#########
  
  #Genero una lista en la que voy a almacenar los dataframes para graficar
  #Uno por cada dia
  
  dataframes = list()
  
  #Un ciclo que genera una lista en la que cada contenedor tiene dataframes
  #uno por cada dia
  
  for (i in 1:length(dias)){
    #Del dataframe con el que estaba operando y mediante el uso de la libreria lubridate. 
    #Pido de mi variable tiempo el dia y hora que solicite. Con esta nueva informacion 
    #genero un dataframe que solo tiene datos del dia y hora solicitado.
    
    df = nc[day(nc$time) == dias[i] & hour(nc$time) == mi_hora,]
    
    #Se realiza un ajuste de modo de convertir las longitudes a una escala comprensible
    #mediante la libreria metR
    
    df$lon = ConvertLongitude(df$lon)
    
    #Genero una nueva columna en la asigno la fecha y hora con la que estoy
    #trabajando para luego utilizarla para separar graficos con facet_wrap
    
    df$fecha_de_mi_dia = paste0(dias[i],"/12/2013 ",mi_hora," UTC")
    
    #Almaceno el dataframe que genere en un contenedor de mi lista
    dataframes[[i]] = df
    
    #Reedefino mi dataframe 
    df = 0
    
    #Este proceso se repite la cantidad de dias que pedi
  }
  
  
  #####CONCATENO DATAFRAMES##################
  
  #Genero un dataframe vacio donde almacenare dataframes de cada dia correspondiente 
  #de ser necesario
  
  varios_df = data.frame()
  
  #Genero un ciclo que va a concatener dataframes en caso de ser necesario
  
  for(i in 1:length(dataframes)){
    
    #Tomo el contenedor i de mi lista
    tabla = get("dataframes")[[i]]
    
    #Concateno los dataframes por filas de modo de poner uno debajo del otro
    varios_df = rbind(varios_df,tabla)
  }
  
  
  #####CARGAMOS EL MAPA PARA LOS CONTORNOS######
  
  #Cargamos un dataframe con datos del mapa mundial
  mapa = map_data("world")
  
  #Generamos los contornos con ggplot2 del dataframe de mapa
  
  mi_mapa = geom_path(data = mapa,aes(long,lat,group=group),size=0.1)
  
  #Esta seccion de la funcion se encarga de definir un nombre a la columna con la variable a operar
  #esto es debido a que de otra manera no es posible llamarla en la seccion de graficado ya que 
  #al cambiar de archivo ncdf4 nuestra variable puede cambiar de nombre. Al hacer esto generalizamos
  #el problema
  
  #La posicion en la que la columna de nuestro dataframe tiene el mismo nombre que nuestra variable
  
  posicion_correcta = which(colnames(varios_df) == as.character(variable))
  
  #Reemplazamos el nombre de nuestra columna por parametro.
  colnames(varios_df)[posicion_correcta] = "parametro"
  
  #Si condicion es TRUE cambia la variable a grados celcius
  
  if(celcius == TRUE){
    varios_df$parametro = varios_df$parametro - 273
  }
  ######GRAFICADO#############
  #Graficamos con la liberia ggplot2
  
  grafico = ggplot(varios_df,mapping=aes(x=lon,y=lat))+
    
    #Relleno los contornos que voy a estar generando. 
    #Con fill stat level nos aseguramos de que la escala de color que
    #se utilice sea discreta.
    geom_contour_fill(aes(z=parametro,fill=stat(level)))+
    
    #Los contornos que voy a estar generando con mi variable. 
    #Son de color negro.
    geom_contour(aes(z=parametro),color="black",size=0.2)+
    
    #Indicamos la paleta de colores que voy a utilizar. Es un argumento de la 
    #funcion. Ademas de la altura y ancho de la leyenda. La escala de colores
    #esta discretizada
    scale_fill_distiller(palette =paleta_colores ,direction = 1,super=ScaleDiscretised,
                         guide=guide_colorsteps(barheight=altura_guide,
                                                barwidth=ancho_guide))+
    
    #Generamos una capa en la que agregamos los contornos del mapa
    mi_mapa+
    
    #Agregamos los limites al grafico
    coord_quickmap(xlim = range(varios_df$lon),ylim=range(varios_df$lat),expand = FALSE)+
    
    #Nos encargamos de realizar graficos por facetas, en este caso segun varia
    #la fecha. La cantidad de columnas es la misma que la cantidad de dias
    facet_wrap(~fecha_de_mi_dia, ncol =length(dias)) +
    
    #Asignamos los labels a las imagenes. El nombre de la escala y el titulo del
    #grafico son argumentos de la funcion
    labs(x="Longitud",y="Latitud",fill=nombre_escala,title = titulo_grafico)
  
  
  #Devolvemos el graficado diario deseado
  
  return(grafico)
  
}

#Utilizo la funcion para generar el campo diario de temperatura para los dias
#10,15,20 y 25 para las 18 hs
imagen_3 = campos_diarios(archivo_nc = archivo_temp,variable = "air",
                          nivel = 1000,
                          dias = c(10,15,20,25),
                          mi_hora = 18,
                          paleta_colores = "YlOrRd",
                          titulo_grafico = "Campos diarios de Temperatura en el nivel de 1000 hpa",
                          nombre_escala = "Temperatura [Â°C]",
                          celcius=TRUE)



imagen_4 = campos_diarios(archivo_nc = archivo_altura,variable = "hgt",
                          nivel = 1000,
                          dias = c(10,15,20,25),
                          mi_hora = 18,
                          titulo_grafico = "Campos diarios de Altura Geopotencial en nivel de 1000 hpa",
                          nombre_escala = "Altura geopotencial [mgp]")

ggsave("campos_diarios_temperatura.png",imagen_3,height = 6 , width = 12,dpi=300)     
ggsave("campos_diarios_geopotencial.png",imagen_4,height = 6,width = 12,dpi=300)


# ITEM D ------------------------------------------------------------------

#La siguiente funcion grafica las marchas de temperatura. Marcha de temperatura
#maximas y marcha de temperatura minima para un punto en el mapa.


#Los argumentos de la funcion:

#archivo_ncdf4 : Se ingresa la direccion del archivo ncdf4

#variable_archivo: Se le ingresa la variable del archivo ncdf4 por defecto es
#air

#dimensiones: Se le ingresa las dimensiones con las que se va a trabajar.
#lonxlatxlevelxtiempo. Solo se trabaja con 5 dimensiones.

#latitud: Es la latitud a la que se encuentra el punto deseado. En caso de no
#encontrarse el punto exacto, se encuentra el mas cercano. Por defecto es 32Â°S

#longitud: Es la longitud a la que se encuentra el punto deseado. En caso de no
#encontrarse el punto exacto, se encuentra la longitud mas cercana a esta. Por
#defecto es 68Â°O.

#celcius: Es una variable que me realiza un ajuste a los datos. Si estamos 
#trabajando con temperatura es util.

#periodo: Es el subtitulo que indicamos. Es el mismo para cada imagen. Por
#defecto sin titulo

#titulo: Es el titulo superior, por encima de las 2 marchas. Por defecto
#sin titulo

marchas_minimas_maximas = function(archivo_ncdf4,
                                   variable_archivo="air",
                                   dimensiones,
                                   latitud=-32,
                                   longitud=-68,
                                   celcius=FALSE,
                                   periodo = "sintitulo",
                                   titulo = "sintitulo"){
  
  #La funcion necesita leer el archivo ncdf4 que se le asigne como un array y luego
  #como un dataframe para obtener informacion de ciertas variables
  
  #Leo mi archivo como formato matriz utilizando la libreria metR
  archivo_matrix = ReadNetCDF(archivo_ncdf4,vars=variable_archivo,subset = list(level=1000),out = c("array"))
  
  #Redefino las dimensiones de mi array para operar segun lo que necesite
  datos_matriz = array(archivo_matrix[[1]],dim=dimensiones)
  
  #Opero para transformar los datos de temperatura si es necesario a datos celcius
  
  if(celcius==TRUE){
    datos_matriz = datos_matriz - 273
  }
  
  #Leo mi archivo como un dataframe en el nivel de 1000hpa
  archivo_data = ReadNetCDF(archivo_ncdf4,vars=variable_archivo,subset=list(level=1000))
  
  
  #Extraigo latitudes y longitudes desde mi dataframe que abri
  
  valores_latitud = unique(archivo_data$lat)
  valores_longitud = unique(archivo_data$lon)
  
  #Transformo los valores de longitud a una escala acorde utilizando metR
  valores_longitud = ConvertLongitude(valores_longitud)
  
  #Me devuelve la posicion de los valores de latitud y longitud mas cercanos
  #a los que le indique en la funcion. Calcula el modulo de la resta, o sea la
  #distancia y luego busca la distancia menor, o sea la mas proxima.
  
  latitud_cercana = which.min(abs(valores_latitud - latitud))
  longitud_cercana = which.min(abs(valores_longitud - longitud))
  
  
  #Extraigo los valores de latitud y longitud para luego indicarlas en el grafico
  #como el punto de reticula
  
  valor_lat = valores_latitud[latitud_cercana]
  valor_lon = valores_longitud[longitud_cercana]
  #Fijo una longitud y latitud. Obtengo una matriz de 2 dimensiones
  #4 datos de temperatura horarios para cada dia. 31 dias. 
  
  punto_del_mapa = datos_matriz[longitud_cercana,latitud_cercana,,,]
  
  #Obtengo la temperatura maxima de cada dia. Busca el maximo en la matriz
  #punto del mapa, lo busca por columnas.
  maximos = apply(punto_del_mapa,2,max)
  
  #Obtengo la temperatura minima de cada dia. Busca el minimo en la matriz
  #punto del mapa, lo busca por columnas.
  minimos = apply(punto_del_mapa,2,min)
  
  #Genero un dataframe con los datos de temperaturas maximas
  df_maximas = data.frame(Dias=seq(1:31),Temperaturas = maximos)
  
  #Genero un dataframe con los datos de temperaturas minimas
  df_minimas = data.frame(Dias=seq(1:31),Temperaturas = minimos)
  
  ######BREAKS####
  
  #Para delimitar cada cuanto se realizan los tickets del grafico
  #se busca el valor minimo de temperatura y el maximo de temperatura
  #para las temperaturas minimas. Se realiza una secuencia entre el
  #minimo y maximo cada 2.
  
  breaks_minimas = seq(round(min(df_minimas$Temperaturas)),round(max(df_minimas$Temperaturas)),2)
  
  #Analogo para las temperaturas maximas.
  
  breaks_maximas = seq(round(min(df_maximas$Temperaturas)),round(max(df_maximas$Temperaturas)),2)
  #####GRAFICADO#####
  
  #Grafico de maximas
  
  #Tomamos en el eje x los dias y en el eje Y las temperaturas del dataframe
  #de temperaturas maximas 
  
  graf_maximas = ggplot(data=df_maximas,mapping=aes(x=Dias,y=Temperaturas))+
    
    #Graficamos lineas de color naranja
    geom_line(color = "orangered2")+
    
    #Indicamos los labels de nuestro grafico de maximas
    labs(title="Marcha de Temperatura maximas",
         subtitle = periodo,x = "Dias",y="Temperatura [Â°C]")+
    
    #Marcamos un punto por cada dia. Cada valor de temperatura
    geom_point(size=1.3,alpha=1)+
    
    #Indicamos los breaks cada cuanto se marcan las etiquetas
    #En el eje X. 
    scale_x_continuous(breaks=seq(1,31,1))+
    
    #Indicamos los breaks de la variable en el eje Y
    scale_y_continuous(breaks = breaks_maximas)
  
  #Grafico de Minimas 
  
  #Tomamos el eje x para los Dias y el eje Y las temperaturas        
  graf_minimas = ggplot(data=df_minimas,mapping=aes(x=Dias,y=Temperaturas))+
    
    #Marcamos las lineas en un color celeste
    geom_line(color="deepskyblue3")+
    
    #Indicamos los labels de nuestro grafico
    labs(title="Marcha de Temperaturas minimas",
         subtitle = periodo,x = "Dias",y="Temperatura [Â°C]")+
    
    #Marcamos un punto por cada valor de temperatura.
    geom_point(size=1.3,alpha=1)+
    
    #Los tickets de breaks del eje x
    scale_x_continuous(breaks = seq(1,31,1))+
    
    #Los tickets de breaks del eje y
    scale_y_continuous(breaks = breaks_minimas)
  
  
  #Generamos un grafico donde se haya la marcha de temperatura
  #maximas y la marcha de minimas
  ploteo = grid.arrange(graf_maximas,
                        graf_minimas,nrow=2,ncol=1,
                        
                        #Aca se indica el titulo que ira por encima y
                        #el punto mas cercano al que se indico como 
                        #argumento
                        
                        top=paste(titulo,"Punto de reticula: (lat = ",valor_lat,
                                  " lon = ",valor_lon,")"))
  #Me devuelve las marchas
  return(ploteo)
  
}

#Utilizo la funcion para calcular las marchas para el punto de reticula
#mas cercano a mendoza
mendoza = marchas_minimas_maximas(archivo_ncdf4 = archivo_temp,
                                  variable_archivo = "air",
                                  dimensiones=c(21,33,1,4,31),
                                  latitud= -32.89,longitud = -68.8,
                                  celcius = TRUE,
                                  periodo = "Periodo: Diciembre de 2013",
                                  titulo = "Mendoza")

#Utilizo la funcion para calcular las marchas para el punto de reticula
#mas cercano a buenos aires

buenos_aires = marchas_minimas_maximas(archivo_ncdf4 = archivo_temp,
                                       variable_archivo = "air",
                                       dimensiones=c(21,33,1,4,31),
                                       latitud= -34,longitud = -58,
                                       celcius = TRUE,
                                       periodo = "Periodo: Diciembre de 2013",
                                       titulo = "Buenos aires ")

#Guardo las imagenes

ggsave("mendoza.png",mendoza,height = 6,width = 12,dpi=300)
ggsave("buenos_aires.png",buenos_aires,height = 6,width = 12,dpi=300)
