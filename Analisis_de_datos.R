# Primero hay que inicializar un Git repository
library(usethis)
use_git()
# Si salió todo bien, vas a tener que ver la 'Git' tab a tu derecha
# La conexión entre Rstudio y Git ya esta hecha

# Ahora tenemos que conectar este R Project con Github.
use_github()

# Comittear cambios
#1. Una vez que haces modificaciones en este archivo, guardalas clickeando en Save
#2. Ir a la tab de 'Git' en la box de la derecha. Tocar en el botón de Commit.
#3. Se te va a abrir una nueva ventana. Agregale una descripción al commit. 
#Toca en el botón 'Commit'.
#4. Tocar en el botón 'Close' y cierra la nueva ventana.
#5. Tus cambios ahora se van a ver reflejados en el proyecto Github

# Pullear cambios: Se puede hacer modificaciones a los archivos desde Github (no es muy usual).
#1. Ir a la tab de 'Git' en la caja de la derecha
#2. Tocar en Pull
#3. Vas a tener la nueva versión del archivo en local.

# Hay un botón 'Push' en la tab de Git. No termino de entender que hace. Algo relacionado con pushear
# HEAD to master... si haces cambios, los guardas y le das a Push, no se te actualiza el archivo en el que hicsite cambios.
# Ojota

#Paquetes utilizados
library(readxl)
library(tidyverse)
library(writexl)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ade4)
library(ggrepel)

#Seteo el working directory
setwd("C:/Users/lucia/Desktop/ESTADISTICA/Tesina")

#Lectura de datos
F2_1x8 <- read_excel("Datos/Datos para Tesina_version 3.xls", 
                     sheet = "Hoja1", skip = 1)
F2_18x1 <- read_excel("Datos/Datos para Tesina_version 3.xls", 
                      sheet = "Hoja2", skip = 1)
F2_1x5 <- read_excel("Datos/Datos para Tesina_version 3.xls", 
                     sheet = "Hoja3", skip = 1)

#Preguntar si todas las observaciones que tienen un '.' son NA
F2_1x8[F2_1x8 == "."] <- NA
F2_18x1[F2_18x1 == "."] <- NA
F2_1x5[F2_1x5 == "."] <- NA

#Creo dataframes con coordenadas para el círculo y para el origen de las flechas
theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
circle <- data.frame(xcircle = cos(theta), ycircle = sin(theta), stringsAsFactors = TRUE)

#Analizamos la cantidad de valores NA en cada dataframe
#h <- data.frame(names(which(colSums(is.na(F2_1x8)) > 0))) #76 columnas tienen NA
#names(which(colSums(is.na(F2_1x8)) == 0)) #11 columnas no tienen NA
#i <- data.frame(names(which(colSums(is.na(F2_18x1)) > 0))) #88 columnas tienen NA
#names(which(colSums(is.na(F2_18x1)) == 0)) #Ninguna columna no tiene NA
#j <- data.frame(names(which(colSums(is.na(F2_1x5)) > 0))) #53 columna tienen NA
#length(names(which(colSums(is.na(F2_1x5)) == 0))) #35 columnas no tienen NA

#Ojo con re-escribir archivos
#a1 <- data.frame(colnames(F2_1x8))
#a2 <- data.frame(colnames(F2_18x1))
#a3 <- data.frame(colnames(F2_1x5))
#write_xlsx(a1, "C:/Users/lucia/Desktop/ESTADISTICA/Tesina/a11.xlsx")
#write_xlsx(a2, "C:/Users/lucia/Desktop/ESTADISTICA/Tesina/a2.xlsx")
#write_xlsx(a3, "C:/Users/lucia/Desktop/ESTADISTICA/Tesina/a3.xlsx")

#-----------------------------
# ANALISIS EN VARIABLES CUANTI
#-----------------------------

# Se evalúa cuáles columnas son comunes entre las 3 bases
# Las variables cuantitativas en comun son:
# a/b (índice de absorbencia)
# AT (Acidez Titulable en base 2 y 3)
#VP (VIDA POScosecha)
# Altura
# Diametro (Diámetro en base 1 y 3)
# Firmeza
# Forma (Forma (A/D) en base 2)
# L (porcentaje de reflectancia)
# Planta (N° Plante en base 1 y 3)
# Peso
# pH (medido sobre el jugo de tomate homogeneizado)
# SS (Sólidos Solubles en base 3): porcentaje de glucosa más fructosa del jugo homogeneizado

#-------
# F2_1x8
#-------

#Selecciono solo las columnas que las 3 bases tengan en común
F2_1x8 <- F2_1x8 %>%
  dplyr::select("N° Planta", "Diámetro", "Altura", "Forma", "Peso",
                "VP", "Firmeza", "L", "a/b", "AT", "pH",
                "SS")

#Renombro columnas con distintos nombres a traves de las 3 bases
#y creo la variable indice de madurez
F2_1x8 <- F2_1x8 %>%
  dplyr::rename(`Planta` = `N° Planta`, #new name = old name
                `Diametro` = `Diámetro`)%>%
  dplyr::mutate(`IM`= SS/AT)

#Creo una nueva columna con el nombre de la familia de tomate
F2_1x8$flia <- rep("F2_1x8", nrow(F2_1x8))

#Elimino las filas con datos faltantes
#Esto debe hacerse una vez que tengo seleccionadas las variables,
#porque si no, me quedo sin observaciones
F2_1x8 <- drop_na(F2_1x8)
#Me quedo con 50 observaciones

# Elimino la variable IM
F2_1x8 <- F2_1x8 %>%
  select(-c("IM"))

#--------
# F2_18x1
#--------

#Selecciono solo las columnas que las 3 bases tengan en común
#y creo la variable Indice de Madurez (IM)
F2_18x1 <- F2_18x1 %>%
  dplyr::select("N° Planta", "Diametro", "Altura", "Forma (A/D)", "Peso",
                "Días","Firmeza", "L", "a/b", "Acidez Titulable", "pH",
                "SS")

#Renombro columnas con distintos nombres a traves de las 3 bases
F2_18x1 <- F2_18x1 %>%
  dplyr::rename(`Planta` = `N° Planta`,
                `Forma` = `Forma (A/D)`,
                `VP` = `Días`,
                `AT` = `Acidez Titulable`)

#Cambio el tipo de variable en caso de ser necesario
#y creo la nueva variable Indice de Madurez (IM)
str(F2_18x1)
F2_18x1 <- F2_18x1 %>%
  mutate_at(c("Firmeza", "L", "a/b", "AT", "pH",
              "SS"), as.numeric)
F2_18x1 <- F2_18x1%>%
  mutate(across(where(is.numeric), ~round(., 2)))%>%
  dplyr::mutate(`IM`= SS/AT)

#Creo una nueva columna con el nombre de la familia de tomate
F2_18x1$flia <- rep("F2_18x1", nrow(F2_18x1))

#Elimino las filas con datos faltantes
#Esto debe hacerse una vez que tengo seleccionadas las variables,
#porque si no, me quedo sin observaciones
F2_18x1 <- drop_na(F2_18x1)
#Me quedo con 60 observaciones

# Elimino la variable IM
F2_18x1 <- F2_18x1 %>%
  select(-c("IM"))


#-------
# F2_1x5
#-------

#Selecciono solo las columnas que las 3 bases tengan en común
F2_1x5 <- F2_1x5 %>%
  dplyr::select("Planta", "Diámetro", "Altura", "Forma", "Peso",
                "Vida Poscosecha","Firmeza", "L", "a/b", "Acidez Titulable", "pH",
                "Sólidos Solubles")

#Renombro columnas con distintos nombres a traves de las 3 bases
F2_1x5 <- F2_1x5 %>%
  dplyr::rename(`Diametro` = `Diámetro`,
                `VP` = `Vida Poscosecha`, 
                `AT` = `Acidez Titulable`,
                `SS` = `Sólidos Solubles`)%>%
  dplyr::mutate(`IM`= SS/AT)

#Creo una nueva columna con el nombre de la familia de tomate
F2_1x5$flia <- rep("F2_1x5", nrow(F2_1x5))

#Elimino las filas con datos faltantes
#Esto debe hacerse una vez que tengo seleccionadas las variables,
#porque si no, me quedo sin observaciones
F2_1x5 <- drop_na(F2_1x5)
#Me quedo con 36 observaciones

# Elimino la variable IM
F2_1x5 <- F2_1x5 %>%
  select(-c("IM"))

#---------------------------
# Union de las tres familias
#---------------------------
union12 <- full_join(x = F2_1x8, y = F2_18x1) #Joining, by = c("Planta", "Diametro", "Altura", "Forma", "Peso", "Firmeza", "L", "a/b", "AT", "pH", "SS", "flia")
union123 <- full_join(x = union12, y = F2_1x5) #Joining, by = c("Planta", "Diametro", "Altura", "Forma", "Peso", "Firmeza", "L", "a/b", "AT", "pH", "SS", "flia")
union <- union123
#Para hacer le analisis sin IM 
union2 <- union %>%
  select(-c("IM"))


#--------------------
# Analisis de cada variable
# con boxplots
#--------------------

#Tabla con la media y el desvio estandar de cada variable de cada grupo de tomates
F2_1x8 %>% select(-c("Planta", "flia")) %>%
  #Pivotting data
  pivot_longer(cols = everything()) %>%
  #Grouping by sun/sky
  group_by(name) %>% 
  #Caluclating mean and sg grouped by sun/sky
  summarise(
    mean = mean(value,na.rm = T),
    sd = sd(value,na.rm = T)
  )

F2_18x1 %>% select(-c("Planta", "flia")) %>%
  #Pivotting data
  pivot_longer(cols = everything()) %>%
  #Grouping by sun/sky
  group_by(name) %>% 
  #Caluclating mean and sg grouped by sun/sky
  summarise(
    mean = mean(value,na.rm = T),
    sd = sd(value,na.rm = T)
  )


F2_1x5 %>% select(-c("Planta", "flia")) %>%
  #Pivotting data
  pivot_longer(cols = everything()) %>%
  #Grouping by sun/sky
  group_by(name) %>% 
  #Caluclating mean and sg grouped by sun/sky
  summarise(
    mean = mean(value,na.rm = T),
    sd = sd(value,na.rm = T)
  )

#Uno los datos para poder graficar con mayor facilidad
datos <- rbind.data.frame(F2_1x8, F2_18x1, F2_1x5)

class(datos)
datos <- as.data.frame(datos) #Convertimos lo que antes era un tibble en un dataframe
datos <- datos %>%
  dplyr::select(-c("Planta")) #eliminamos la columna Planta ya que no la necesitamos
#Ojo con intentar poner a Planta como rownames, ha llevado a errores

datos <- datos %>%
  dplyr::mutate(`Generación` = case_when (
    flia == "F2_18x1" ~ "G2",
    flia == "F2_1x8" ~ "G1",
    flia == "F2_1x5" ~ "G3"))%>%
  dplyr::select(-c("flia"))

#Boxplot VP Vida Poscosecha para cada familia
vp <- ggplot(datos)+
  aes(x = factor(`Generación`), y=VP, fill = `Generación`)+
  geom_boxplot()+
  labs(
    #title = "Boxplot del diámetro para cada familia de tomates",
    y= "Vida Poscosecha",
    fill = "Generación")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #Asigno los colores manualmente. La idea es mantener estos colores para cada flia
  scale_color_manual(values = c("G2" = "#F8766D", "G3" = "#00BA38",
                                "G1" = "#619CFF"))

#Boxplot Diametro para cada familia
diam <- ggplot(datos)+
  aes(x = factor(`Generación`), y=Diametro, fill = `Generación`)+
  geom_boxplot()+
  labs(
    #title = "Boxplot del diámetro para cada familia de tomates",
    y= "Diámetro",
    fill = "Generación")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #Asigno los colores manualmente. La idea es mantener estos colores para cada flia
  scale_color_manual(values = c("G2" = "#F8766D", "G3" = "#00BA38",
                                "G1" = "#619CFF"))

#Boxplot Altura para cada familia
altura <-  ggplot(datos)+
  aes(x = factor(`Generación`), y=Altura, fill = `Generación`)+
  geom_boxplot()+
  labs(
    #title = "Boxplot del diámetro para cada familia de tomates",
    y= "Altura",
    fill = "Generación")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #Asigno los colores manualmente. La idea es mantener estos colores para cada flia
  scale_color_manual(values = c("G2" = "#F8766D", "G3" = "#00BA38",
                                "G1" = "#619CFF"))


#Boxplot Forma para cada familia
forma <-  ggplot(datos)+
  aes(x = factor(`Generación`), y=Forma, fill = `Generación`)+
  geom_boxplot()+
  labs(
    #title = "Boxplot del diámetro para cada familia de tomates",
    y= "Forma",
    fill = "Generación")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #Asigno los colores manualmente. La idea es mantener estos colores para cada flia
  scale_color_manual(values = c("G2" = "#F8766D", "G3" = "#00BA38",
                                "G1" = "#619CFF"))


#Boxplot Peso para cada familia
peso <-  ggplot(datos)+
  aes(x = factor(`Generación`), y=Peso, fill = `Generación`)+
  geom_boxplot()+
  labs(
    #title = "Boxplot del diámetro para cada familia de tomates",
    y= "Peso",
    fill = "Generación")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #Asigno los colores manualmente. La idea es mantener estos colores para cada flia
  scale_color_manual(values = c("G2" = "#F8766D", "G3" = "#00BA38",
                                "G1" = "#619CFF"))


#Boxplot Firmeza para cada familia
firmeza <-  ggplot(datos)+
  aes(x = factor(`Generación`), y=Firmeza, fill = `Generación`)+
  geom_boxplot()+
  labs(
    #title = "Boxplot del diámetro para cada familia de tomates",
    y= "Firmeza",
    fill = "Generación")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #Asigno los colores manualmente. La idea es mantener estos colores para cada flia
  scale_color_manual(values = c("G2" = "#F8766D", "G3" = "#00BA38",
                                "G1" = "#619CFF"))


#Boxplot de L para cada familia
L <-  ggplot(datos)+
  aes(x = factor(`Generación`), y=L, fill = `Generación`)+
  geom_boxplot()+
  labs(
    #title = "Boxplot del diámetro para cada familia de tomates",
    y= "Nivel de reflectancia (L)",
    fill = "Generación")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #Asigno los colores manualmente. La idea es mantener estos colores para cada flia
  scale_color_manual(values = c("G2" = "#F8766D", "G3" = "#00BA38",
                                "G1" = "#619CFF"))


#Boxplot indice de absorbencia para cada familia
ab <- ggplot(datos)+
  aes(x = factor(`Generación`), y=`a/b`, fill = `Generación`)+
  geom_boxplot()+
  labs(
    #title = "Boxplot del diámetro para cada familia de tomates",
    y= "Índice de absorbencia (a/b)",
    fill = "Generación")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #Asigno los colores manualmente. La idea es mantener estos colores para cada flia
  scale_color_manual(values = c("G2" = "#F8766D", "G3" = "#00BA38",
                                "G1" = "#619CFF"))


#Boxplot AT para cada familia
AT <- ggplot(datos)+
  aes(x = factor(`Generación`), y=AT, fill = `Generación`)+
  geom_boxplot()+
  labs(
    #title = "Boxplot del diámetro para cada familia de tomates",
    y= "Acidez Titulable",
    fill = "Generación")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #Asigno los colores manualmente. La idea es mantener estos colores para cada flia
  scale_color_manual(values = c("G2" = "#F8766D", "G3" = "#00BA38",
                                "G1" = "#619CFF"))

#Boxplot pH para cada familia
pH <- ggplot(datos)+
  aes(x = factor(`Generación`), y=pH, fill = `Generación`)+
  geom_boxplot()+
  labs(
    #title = "Boxplot del diámetro para cada familia de tomates",
    y= "pH",
    fill = "Generación")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #Asigno los colores manualmente. La idea es mantener estos colores para cada flia
  scale_color_manual(values = c("G2" = "#F8766D", "G3" = "#00BA38",
                                "G1" = "#619CFF"))

#Boxplot SS para cada familia
SS <- ggplot(datos)+
  aes(x = factor(`Generación`), y=SS, fill = `Generación`)+
  geom_boxplot()+
  labs(
    #title = "Boxplot del diámetro para cada familia de tomates",
    y= "Nivel de sólidos solubles",
    fill = "Generación")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  #Asigno los colores manualmente. La idea es mantener estos colores para cada flia
  scale_color_manual(values = c("G2" = "#F8766D", "G3" = "#00BA38",
                                "G1" = "#619CFF"))

dev.size()
#OBS: ggsave no toma la ruta del set working directory
ggsave("C:/Users/lucia/Desktop/ESTADISTICA/Tesina/Datos/DMFA/Boxplots univariados/Boxplot ab.png",
       plot = ab,
       width = 4.75,
       height = 4.5)

#La union de todos los graficos individuales fue hecha en Figma

#----------------------------------------
# Aplicacion del PCA ignorando los grupos
#----------------------------------------
union_pca <- union %>%
  select(-c("Planta")) #,"flia"
res.PCA <- PCA(union_pca[1:11],
               scale.unit = TRUE,
               ncp = 5,
               #quali.sup = 11, #no funciona bien
               axes = c(1,2))
#ggsave("Datos/Grafico de variables_PCA.png", 
#       plot = last_plot(),
#       width = 10,
#       height= 6,
#       dpi=300)

res.PCA #Aqui te explica dónde está guardada cada cosa
res.PCA$eig
res.PCA$var
res.PCA$call$centre
res.PCA$call$ecart.type
summary.PCA(res.PCA)

#Scree plot con el porcentaje de variancia explicada por cada autovalor
fviz_eig(res.PCA)

#Defino variables en las que conservaré el porcentaje de variancia explicada
#por cada dimensión del ACP
var_dim1_all <- round(res.PCA$eig[1,2],2)
var_dim2_all <- round(res.PCA$eig[2,2],2)
var_dim3_all <- round(res.PCA$eig[3,2],2)
var_dim4_all <- round(res.PCA$eig[4,2],2)

#Grafico de individuos (scores)
#Creo un data frame con las coordenadas de los individuos en los ejes de las CPs
coordenadas_ind <- as.data.frame(res.PCA$ind$coord) 

#Agrego la variable cualitativa (flia de tomate)
familia <- union_pca %>% select(c("flia"))
coordenadas_ind <- cbind.data.frame(coordenadas_ind,familia)

coordenadas_ind <- coordenadas_ind %>%
  dplyr::mutate(`Generación` = case_when (
    flia == "F2_18x1" ~ "G2",
    flia == "F2_1x8" ~ "G1",
    flia == "F2_1x5" ~ "G3"))


# CP1 vs CP2
coordenadas_ind %>%
  ggplot(aes(x = `Dim.1`, y = `Dim.2`, col = `Generación`))+
  geom_point() + #puntos
  geom_vline(xintercept = 0)+ #eje y
  geom_hline(yintercept=0)+ #eje x
  labs(title = "Gráfico de scores", 
       x = paste0("Dim 1 (", var_dim1_all, "%)"), 
       y = paste0("Dim 2 (", var_dim2_all, "%)"))+
  #Asigno los colores manualmente. La idea es mantener estos colores para cada flia
  scale_color_manual(values = c("G2" = "#F8766D",
                                "G3" = "#00BA38",
                                "G1" = "#619CFF"))+
  theme_bw()

# CP3 vs CP4
coordenadas_ind %>%
  ggplot(aes(x = `Dim.3`, y = `Dim.4`, col = `Generación`))+
  geom_point() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+
  labs(title = "Gráfico de scores", 
       x = paste0("Dim 3 (", var_dim3_all, "%)"), 
       y = paste0("Dim 4 (", var_dim4_all, "%)"))+
  scale_color_manual(values = c("G2" = "#F8766D",
                                "G3" = "#00BA38",
                                "G1" = "#619CFF"))+
  theme_bw()

#Grafico de variables (grafico de cargas)
#Opcion 1
plot(res.PCA, choix = "var", shadow = TRUE)

#Opcion 2
fviz_pca_var(res.PCA)

#Opcion 3
#Creo un dataframe con las coordenadas de las nuevas variables
coordenadas_var <- as.data.frame(res.PCA$var$coord) %>%
  rownames_to_column(var = "label")

#Me tengo que asegurar de que ya se hayan definido dataframes con coordenadas 
#para el círculo y para el origen de las flechas
theta
circle

ggplot(coordenadas_var, aes(x=Dim.1, y = Dim.2))+
  #geom_point(size = 0.05) +
  #geom_text(aes(label = label), data = coordenadas_var,
  #        hjust="left", 
  #        vjust="right",
  #        size=3.4)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(title = "Gráfico de cargas para todos los individuos", 
       x = paste0("Dim 1 (", var_dim1_all, "%)"), 
       y = paste0("Dim 2 (", var_dim2_all, "%)"))+
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") + #ABABAB
  coord_fixed()+
  geom_segment(aes(x = 0, y = 0, 
                   xend = Dim.1, 
                   yend = Dim.2),
               arrow = arrow(length=unit(0.2, 'cm')),
               color = '#000000', lwd = .5)+
  theme_bw()+
  #theme(panel.border = element_blank())+ 
  geom_text_repel(aes(label = label)) #esto te tira una separación random de las etiquetas,
#para hacerlo mas en detalle, ver el grafico en PCA de todos los datos juntos


ggplot(coordenadas_var, aes(x=Dim.3, y = Dim.4))+
  #geom_point(size = 0.05) +
  geom_text(aes(label = label), data = coordenadas_var,
            hjust="left", 
            vjust="right")+
  #        size=3.4)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(title = "Gráfico de cargas para todos los individuos", 
       x = paste0("Dim 3 (", var_dim3_all, "%)"), 
       y = paste0("Dim 4 (", var_dim4_all, "%)"))+
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") + #ABABAB
  coord_fixed()+
  geom_segment(aes(x = 0, y = 0, 
                   xend = Dim.3, 
                   yend = Dim.4),
               arrow = arrow(length=unit(0.2, 'cm')),
               color = '#000000', lwd = .5)+
  theme_bw()+
  #theme(panel.border = element_blank())+ 
  geom_text_repel(aes(label = label)) #esto te tira una separación random de las etiquetas,
#para hacerlo mas en detalle, ver el grafico en PCA de todos los datos juntos

dev.size()
#ggsave("aver.png", width = 6, height= 4.5)  



#----------------------------------------
# Interpretacion PCA ignorando los grupos
#----------------------------------------
#Gráfico de los individuos PC1 vs PC2
#Se observa un agrupamiento de los individuos de  
#la familia F2_18x1 y de la F2_1x8, particularmente se distinguen
#en el eje 1, lo que implica que tienen valores opuestos de 
# diametro, altura, peso, a/b y SS, puesto que éstas son las variables
#que más contribuyen a dicho eje.
# Los de la familia F2_1x5 no tienen un comportamiento tan claro.
#Parecieran tener valores más extremos en la dimensión 2, en comparación
#con las otras dos familias al menos. Esto significa que la familia F2_1x5
#tiene valores extremos en las variables L, a/b, AT y pH (puesto que 
#son las variables que más contribuyen a dicho eje).

#Grafico de los individuos PC3 vs PC4
#No se observa una clara distinción entre la familia F2_18x1 y F2_1x8
# En cuanto a la familia F2_1x5 pareciera tomar solo valor positivos
#en la dimensión 4.



#----------------------------------------
# Aplicación del ADL yuxtaponiendo verti
# calmente las 3 familias
#----------------------------------------
union_adl <- union123 %>%
  select(-c("Planta"))
str(union_adl)
library(writexl)
#write_xlsx(union_adl, "C:/Users/lucia/Desktop/ESTADISTICA/Tesina/Datos/union_adl.xlsx")
library(MASS)
## create the lda model
modelo_lda_tomates <- lda(formula = flia ~ ., data = union_adl)

## get the x,y coordinates for the LDA plot
tomates_lda_valores <- predict(modelo_lda_tomates)

## create a dataframe that has all the info we need to draw a graph
datos_tomates <- data.frame(X=tomates_lda_valores$x[,1],
                            Y=tomates_lda_valores$x[,2],
                            Familia=union_adl$flia #aqui estoy creando una columna llamada `Familia` que contiene a la columna flia del dataframe union_adl
)

#grafico de los puntos en el nuevo par de ejes que maximizan la separacion
#entre grupos
ggplot(data=datos_tomates, aes(x=X, y=Y)) +
  geom_point(aes(color=Familia)) +
  geom_vline(xintercept = 0)+ #eje y
  geom_hline(yintercept=0)+ #eje x
  labs(title = "Analisis Discriminante Linear", 
       x ="LD1", 
       y = "LD2")+
  #Asigno los colores manualmente. La idea es mantener estos colores para cada flia
  scale_color_manual(values = c("F2_18x1" = "#F8766D", "F2_1x5" = "#00BA38",
                                "F2_1x8" = "#619CFF"))+
  theme_bw()

#Tabla de las contribuciones de cada variable a cada uno de los ejes
variables_tomates_lda <- as.data.frame(modelo_lda_tomates$scaling)%>%
  rownames_to_column("variable")%>%
  mutate(LD1 = round(LD1,2),
         LD2 = round(LD2,2))
str(variables_tomates_lda)

round(variables_tomates_lda,2)

#Se definen data frames para poder dibujar las flechas
origin1 <- rep(0, nrow(variables_tomates_lda))
dd1 <- cbind.data.frame(variables_tomates_lda, xstart = origin, ystart = origin, stringsAsFactors = TRUE)

#Grafico de las contribuciones de las variables - NO SE USA
#No se va a utilizar ya que no se suele utilizar para la tecnica ADL
ggplot(variables_tomates_lda, aes(LD1, LD2)) +
  #geom_point() +
  geom_text(aes(label = label), data = variables_tomates_lda,
            hjust="left", vjust="right",
            size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= "LD 1", 
       y = "LD2", 
       title = "Gráfico de la contribución de cada variable a los ejes") +
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=variables_tomates_lda$LD1,
                   yend=variables_tomates_lda$LD2),
               data = dd1 , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw()
#+
#geom_text_repel(aes(label = label),
#                size= 4,
#                min.segment.length = 0.5, #only draw line segments that are longer than 0.5
#                seed = 42, #random seed for recreating the exact same layout
#box.padding = 0.5, #padding around the text label
#max.overlaps = Inf, #discard text labels that overlap too many other text labels or data points. asi, nunca va a descartar a labels que se superponen
#arrow = arrow(length = unit(0.010, "npc")), #render line segment as an arrow with grid::arrow()
#nudge_x = .15, #adjust the starting x position of the text label. 0 is the default value
#                nudge_y = -.01,
#                color = "#000000"
#)


#------------------------------
# PCA solo para el grupo F2_1x8
#------------------------------
res.PCA_8 <- PCA(F2_1x8 %>% select(-c("Planta","flia")), scale.unit = TRUE,
                 axes=c(1,2))
res.PCA_8$eig
res.PCA_8$var

#Scree plot con el porcentaje de variancia explicada por cada autovalor
fviz_eig(res.PCA_8)

#Defino variables en las que conservaré el porcentaje de variancia explicada
#por cada dimensión del ACP
var_dim1_8 <- round(res.PCA_8$eig[1,2],2)
var_dim2_8 <- round(res.PCA_8$eig[2,2],2)
var_dim3_8 <- round(res.PCA_8$eig[3,2],2)
var_dim4_8 <- round(res.PCA_8$eig[4,2],2)

#Grafico de individuos (scores)
#Creo un data frame con las coordenadas de los individuos en los ejes de las CPs
coordenadas_ind_8 <- as.data.frame(res.PCA_8$ind$coord)
#coordenadas_ind_8 <- cbind.data.frame(coordenadas_ind_8,F2_1x8$Planta)

#Agrego la variable cualitativa (flia de tomate)
familia_8 <- F2_1x8 %>% select(c("flia", "Planta"))
coordenadas_ind_8 <- cbind.data.frame(coordenadas_ind_8,familia_8)
# Cambio el nombre de la familia
coordenadas_ind_8 <- coordenadas_ind_8 %>%
  dplyr::mutate(`Generación` = case_when (
    flia == "F2_1x8" ~ "G1"))

str(coordenadas_ind_8)
coordenadas_ind_8 %>%
  ggplot(aes(x = `Dim.1`, y = `Dim.2`))+
  geom_point(color = "#619CFF") + #puntos
  geom_text(aes(label = Planta), check_overlap = TRUE)+
  #geom_label(aes(label = Planta))+
  geom_vline(xintercept = 0)+ #eje y
  geom_hline(yintercept=0)+ #eje x
  labs(title = "CP 1 vs CP 2 para la generación G1", 
       x = paste0("Dim 1 (", var_dim1_8, "%)"), 
       y = paste0("Dim 2 (", var_dim2_8, "%)"))+
  theme_bw()
coordenadas_ind_8 %>%
  ggplot(aes(x = `Dim.1`, y = `Dim.3`))+
  geom_point(color = "#619CFF") + #puntos
  geom_vline(xintercept = 0)+ #eje y
  geom_hline(yintercept=0)+ #eje x
  labs(title = "CP 1 vs CP 3 para para la generación G1", 
       x = paste0("Dim 1 (", var_dim1_8, "%)"), 
       y = paste0("Dim 3 (", var_dim3_8, "%)"))+
  theme_bw()

#Grafico de variables
#Creo un dataframe con las coordenadas de las nuevas variables
coordenadas_var_8 <- as.data.frame(res.PCA_8$var$coord) %>%
  rownames_to_column(var = "label")
#Los dataframes con coordenadas para el círculo y para el origen de las flechas
#ya fueron creados previamente

ggplot(coordenadas_var_8, aes(x=Dim.1, y = Dim.2))+
  #geom_point(size = 0.05) +
  #geom_text(aes(label = label), data = coordenadas_var_8,
  #        hjust="left", 
  #        vjust="right",
  #        size=3.4)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(title = "Gráfico de cargas para la generación G1", 
       x = paste0("Dim 1 (", var_dim1_8, "%)"), 
       y = paste0("Dim 2 (", var_dim2_8, "%)"))+
  #labs(x ="Dim 1 (35.60%)", y = "Dim 2 (19.83%)", title = "Gráfico de cargas para la flia F2_1x8") +
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") + #ABABAB
  coord_fixed()+
  geom_segment(aes(x = 0, y = 0, 
                   xend = Dim.1, 
                   yend = Dim.2),
               arrow = arrow(length=unit(0.2, 'cm')),
               color = '#000000', lwd = .5)+
  theme_bw()+
  #theme(panel.border = element_blank())+ 
  geom_text_repel(aes(label = label)) #esto te tira una separación random de las etiquetas,
#para hacerlo mas en detalle, ver el grafico en PCA de todos los datos juntos

ggplot(coordenadas_var_8, aes(x=Dim.1, y = Dim.3))+
  #geom_point(size = 0.05) +
  #geom_text(aes(label = label), data = coordenadas_var_8,
  #        hjust="left", 
  #        vjust="right",
  #        size=3.4)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(title = "Gráfico de cargas para la generación G1", 
       x = paste0("Dim 1 (", var_dim1_8, "%)"), 
       y = paste0("Dim 3 (", var_dim3_8, "%)"))+
  #labs(x ="Dim 1 (35.60%)", y = "Dim 3 (16.09%)", title = "Gráfico de cargas para la flia F2_1x8") +
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") + #ABABAB
  coord_fixed()+
  geom_segment(aes(x = 0, y = 0, 
                   xend = Dim.1, 
                   yend = Dim.3),
               arrow = arrow(length=unit(0.2, 'cm')),
               color = '#000000', lwd = .5)+
  theme_bw()+
  #theme(panel.border = element_blank())+ 
  geom_text_repel(aes(label = label)) #esto te tira una separación random de las etiquetas,
#para hacerlo mas en detalle, ver el grafico en PCA de todos los datos juntos

dev.size()  #9.2 x 5.46


#------------------------------
# PCA solo para el grupo F2_18x1
#------------------------------
res.PCA_18 <- PCA(F2_18x1 %>% select(-c("Planta","flia")), scale.unit = TRUE,
                  axes=c(1,2))

res.PCA_18$eig
res.PCA_18$var

summary(res.PCA_18)

#Defino variables en las que conservaré el porcentaje de variancia explicada
#por cada dimensión del ACP
var_dim1_18 <- round(res.PCA_18$eig[1,2],2)
var_dim2_18 <- round(res.PCA_18$eig[2,2],2)
var_dim3_18 <- round(res.PCA_18$eig[3,2],2)
var_dim4_18 <- round(res.PCA_18$eig[4,2],2)

#Scree plot con el porcentaje de variancia explicada por cada autovalor
fviz_eig(res.PCA_18)

#Grafico de individuos (scores)
#Creo un data frame con las coordenadas de los individuos en los ejes de las CPs
coordenadas_ind_18 <- as.data.frame(res.PCA_18$ind$coord) 

#Agrego la variable cualitativa (flia de tomate)
familia_18 <- F2_18x1 %>% select(c("flia"))
coordenadas_ind_18 <- cbind.data.frame(coordenadas_ind_18,familia_18)
# Cambio el nombre de la familia
coordenadas_ind_18 <- coordenadas_ind_18 %>%
  dplyr::mutate(`Generación` = case_when (
    flia == "F2_18x1" ~ "G2"))

#Gráfico de puntos
coordenadas_ind_18 %>%
  ggplot(aes(x = `Dim.1`, y = `Dim.2`))+
  geom_point(color = "#F8766D") + #puntos
  geom_vline(xintercept = 0)+ #eje y
  geom_hline(yintercept=0)+ #eje x
  labs(title = "CP 1 vs CP 2 para la generación G2", 
       x = paste0("Dim 1 (", var_dim1_18, "%)"), 
       y = paste0("Dim 2 (", var_dim2_18, "%)"))+
  theme_bw()
coordenadas_ind_18 %>%
  ggplot(aes(x = `Dim.1`, y = `Dim.3`))+
  geom_point(color = "#F8766D") + #puntos
  geom_vline(xintercept = 0)+ #eje y
  geom_hline(yintercept=0)+ #eje x
  labs(title = "CP 1 vs CP 3 para la la generación G2", 
       x = paste0("Dim 1 (", var_dim1_18, "%)"), 
       y = paste0("Dim 3 (", var_dim3_18, "%)"))+
  theme_bw()
#Grafico de variables
#Creo un dataframe con las coordenadas de las nuevas variables
coordenadas_var_18 <- as.data.frame(res.PCA_18$var$coord) %>%
  rownames_to_column(var = "label")
#Los dataframes con coordenadas para el círculo y para el origen de las flechas
#ya fueron creados previamente

ggplot(coordenadas_var_18, aes(x=Dim.1, y = Dim.2))+
  #geom_point(size = 0.05) +
  #geom_text(aes(label = label), data = coordenadas_var_8,
  #        hjust="left", 
  #        vjust="right",
  #        size=3.4)+
  geom_vline(xintercept=0, linetype = "dashed") +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(x =paste0("Dim 1 (", var_dim1_18, "%)"),
       y = paste0("Dim 2 (", var_dim2_18, "%)"), 
       title = "Gráfico de cargas para la generación G2") +
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  geom_segment(aes(x = 0, y = 0, 
                   xend = Dim.1, 
                   yend = Dim.2),
               arrow = arrow(length=unit(0.2, 'cm')),
               color = '#000000', lwd = .5)+
  theme_bw()+
  #theme(panel.border = element_blank())+ 
  geom_text_repel(aes(label = label)) #esto te tira una separación random de las etiquetas,
#para hacerlo mas en detalle, ver el grafico en PCA de todos los datos juntos

ggplot(coordenadas_var_18, aes(x=Dim.1, y = Dim.3))+
  #geom_point(size = 0.05) +
  #geom_text(aes(label = label), data = coordenadas_var_8,
  #        hjust="left", 
  #        vjust="right",
  #        size=3.4)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(title = "Gráfico de cargas para la generación G2", 
       x = paste0("Dim 1 (", var_dim1_18, "%)"), 
       y = paste0("Dim 3 (", var_dim3_18, "%)"))+
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") + #ABABAB
  coord_fixed()+
  geom_segment(aes(x = 0, y = 0, 
                   xend = Dim.1, 
                   yend = Dim.3),
               arrow = arrow(length=unit(0.2, 'cm')),
               color = '#000000', lwd = .5)+
  theme_bw()+
  #theme(panel.border = element_blank())+ 
  geom_text_repel(aes(label = label)) #esto te tira una separación random de las etiquetas,
#para hacerlo mas en detalle, ver el grafico en PCA de todos los datos juntos


dev.size()  #9.2 x 5.46

#------------------------------
# PCA solo para el grupo F2_1x5
#------------------------------
res.PCA_5 <- PCA(F2_1x5 %>% select(-c("Planta","flia")), scale.unit = TRUE,
                 axes=c(1,2))
res.PCA_5$eig
res.PCA_5$var
summary(res.PCA_5)

#Defino variables donde guardar el porcentaje de variancia explicado por cada eje
var_dim1_5 <- round(res.PCA_5$eig[1,2],2)
var_dim2_5 <- round(res.PCA_5$eig[2,2],2)
var_dim3_5 <- round(res.PCA_5$eig[3,2],2)
var_dim4_5 <- round(res.PCA_5$eig[4,2],2)

#Scree plot con el porcentaje de variancia explicada por cada autovalor
fviz_eig(res.PCA_5)

#Grafico de individuos (scores)
#Creo un data frame con las coordenadas de los individuos en los ejes de las CPs
coordenadas_ind_5 <- as.data.frame(res.PCA_5$ind$coord) 

#Agrego la variable cualitativa (flia de tomate)
familia_5 <- F2_1x5 %>% select(c("flia"))
coordenadas_ind_5 <- cbind.data.frame(coordenadas_ind_5,familia_5)

# Cambio el nombre de la familia
coordenadas_ind_5 <- coordenadas_ind_5 %>%
  dplyr::mutate(`Generación` = case_when (
    flia == "F2_1x5" ~ "G3"))

#Gráfico de puntos
coordenadas_ind_5 %>%
  ggplot(aes(x = `Dim.1`, y = `Dim.2`))+
  geom_point(color = "#00BA38") + #puntos
  geom_vline(xintercept = 0)+ #eje y
  geom_hline(yintercept=0)+ #eje x
  labs(title = "CP 1 vs CP 2 para la generación G3", 
       x = paste0("Dim 1 (", var_dim1_5, "%)"), 
       y = paste0("Dim 2 (", var_dim2_5, "%)"))+
  theme_bw()
coordenadas_ind_5 %>%
  ggplot(aes(x = `Dim.1`, y = `Dim.3`))+
  geom_point(color = "#00BA38") + #puntos
  geom_vline(xintercept = 0)+ #eje y
  geom_hline(yintercept=0)+ #eje x
  labs(title = "CP 3 vs CP 4 para la para la generación G3", 
       x = paste0("Dim 1 (", var_dim1_5, "%)"), 
       y = paste0("Dim 3 (", var_dim3_5, "%)"))+
  theme_bw()

#Grafico de variables
#Creo un dataframe con las coordenadas de las nuevas variables
coordenadas_var_5 <- as.data.frame(res.PCA_5$var$coord) %>%
  rownames_to_column(var = "label")
#Los dataframes con coordenadas para el círculo y para el origen de las flechas
#ya fueron creados previamente

ggplot(coordenadas_var_5, aes(x=Dim.1, y = Dim.2))+
  #geom_point(size = 0.05) +
  #geom_text(aes(label = label), data = coordenadas_var_8,
  #        hjust="left", 
  #        vjust="right",
  #        size=3.4)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x =paste0("Dim 1 (", var_dim1_5, "%)"),
       y = paste0("Dim 2 (", var_dim2_5, "%)"), 
       title = "Gráfico de cargas para la generación G3") +
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  geom_segment(aes(x = 0, y = 0, 
                   xend = Dim.1, 
                   yend = Dim.2),
               arrow = arrow(length=unit(0.2, 'cm')),
               color = '#000000', lwd = .5)+
  theme_bw()+
  #theme(panel.border = element_blank())+ 
  geom_text_repel(aes(label = label)) #esto te tira una separación random de las etiquetas,
#para hacerlo mas en detalle, ver el grafico en PCA de todos los datos juntos

ggplot(coordenadas_var_5, aes(x=Dim.1, y = Dim.3))+
  #geom_point(size = 0.05) +
  #geom_text(aes(label = label), data = coordenadas_var_8,
  #        hjust="left", 
  #        vjust="right",
  #        size=3.4)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x =paste0("Dim 1 (", var_dim1_5, "%)"),
       y = paste0("Dim 3 (", var_dim3_5, "%)"), 
       title = "Gráfico de cargas para la generación G3") +
  #labs(x ="Dim 1 (35.60%)", y = "Dim 3 (16.09%)", title = "Gráfico de cargas para la flia F2_1x8") +
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") + #ABABAB
  coord_fixed()+
  geom_segment(aes(x = 0, y = 0, 
                   xend = Dim.1, 
                   yend = Dim.3),
               arrow = arrow(length=unit(0.2, 'cm')),
               color = '#000000', lwd = .5)+
  theme_bw()+
  #theme(panel.border = element_blank())+ 
  geom_text_repel(aes(label = label)) #esto te tira una separación random de las etiquetas,
#para hacerlo mas en detalle, ver el grafico en PCA de todos los datos juntos


dev.size()  #9.2 x 5.46

#-------------------------
# Aplicacion de la Tecnica
# DMFA en variables cuanti
#-------------------------
#Utilizaremos a  la columna con el ID de la planta
# como rownames, ya que es algo comun en el paquete FactoMineR
class(union)
union <- as.data.frame(union) #Convertimos lo que antes era un tibble en un dataframe
union_dmfa <- union %>%
  dplyr::select(-c("Planta")) #eliminamos la columna Planta ya que no la necesitamos
#Ojo con intentar poner a Planta como rownames, ha llevado a errores

union_dmfa2 <- union_dmfa %>%
  dplyr::mutate(`Generación` = case_when (
    flia == "F2_18x1" ~ "G2",
    flia == "F2_1x8" ~ "G1",
    flia == "F2_1x5" ~ "G3"))%>%
  dplyr::select(-c("flia"))

#-------------------------------
# Comparación de las estructuras
# de correlación entre familias                                          
#-------------------------------
#Correlaciones general
runion <- cor(union_dmfa2[,-12]) #elimino la columna que tiene el nombre del grupo al que pertenecen (es a nivel general)
corrplot(runion)

#Correlaciones considerando los grupos
rF2_1x8 <- cor(F2_1x8[,-c(1,13)]) #elimino la variable Id planta y flia
corrplot(rF2_1x8,  
         #title = "Visualización de la matriz de correlación \n de la familia F2_1x8",
         mar=c(1,1,1,1), #"A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot.
         #cex.main = 1, #tamaño del titulo
         tl.col = 'black')
round(rF2_1x8,2)

rF2_18x1 <- cor(F2_18x1[,-c(1,13)]) #elimino la variable Id planta y flia
corrplot(rF2_18x1,
         #title = "Visualización de la matriz de correlación \n de la familia F2_18x1",
         mar=c(1,1,1,1), 
         #cex.main = 1,
         tl.col = 'black')

rF2_1x5 <- cor(F2_1x5[,-c(1,13)]) #elimino la variable Id planta y flia
corrplot(rF2_1x5,
         #title = "Visualización de la matriz de correlación \n de la familia F2_1x5",
         mar=c(1,1,1,1), 
         #cex.main = 1,
         tl.col = 'black')

dev.size()
#ggsave("C:/Users/lucia/Desktop/ESTADISTICA/Tesina/Datos/Corrplots de cada familia/corrplot_f2_1x8.png",
#       plot = last_plot(), 
#       width = 4.75, 
#       height= 4.5) 

#Sacar conclusiones sobre las estructuras de correlacion
#entre las familias

#---------
#Aplicación del DMFA per se
#---------
res.dmfa <- DMFA(union_dmfa2, num.fact = 12, ncp = 5, scale.unit = TRUE,
                 axes = c(1,2))
res.dmfa.3.4 <- DMFA(union_dmfa2, num.fact = 12, ncp = 5, scale.unit = TRUE,
                     axes = c(3,4))

summary(res.dmfa)
res.dmfa$eig
res.dmfa$var

#Defino variables donde guardar el porcentaje de variancia explicado por cada eje
var_dim1_dmfa <- round(res.dmfa$eig[1,2],2)
var_dim2_dmfa <- round(res.dmfa$eig[2,2],2)
var_dim3_dmfa <- round(res.dmfa$eig[3,2],2)
var_dim4_dmfa <- round(res.dmfa$eig[4,2],2)

# Gráfico 'Variables factor map (PCA)'
#Vemos que para cada variable, tenemos cuatro colores:
# una variable para el grupo 1(negro), otra para el grupo 2(rojo)
# otra para el grupo 3(verde) y una variable consenso(azul).
# Está muy dificil interpretar ya que se superponen muchas flechas. Más abajo 
#se hacen los gráficos individuales para cada grupo.
res.dmfa$var.partiel #coordenadas de las variables parciales
res.dmfa$var #coordenadas de las variables consenso

#Creo dataframes con las coordenadas de las variables parciales y 
#los nombres de las variables
vparc_F2_1x8 <- as.data.frame(res.dmfa$var.partiel$G1[,1:4]) %>%
  rownames_to_column("label")
vparc_F2_18x1 <- as.data.frame(res.dmfa$var.partiel$G2[,1:4])%>%
  rownames_to_column("label")
vparc_F2_1x5 <- as.data.frame(res.dmfa$var.partiel$G3[,1:4])%>%
  rownames_to_column("label")

# Guardo las variables parciales en tablas lindas para luego
# exportarlas a Excel
res.dmfa$var.partiel$G1

vparcG1 <- vparc_F2_1x8 %>% mutate(across(is.numeric, round, digits=2))%>%
  # Si aplico la funcion round a todo el data frame, me tira error ya que
  # tengo la columna con las variables, que es del tipo character
  dplyr::rename(`Dimensión 1` = `Dim.1`,
                `Dimensión 2` = `Dim.2`,
                `Dimensión 3` = `Dim.3`,
                `Dimensión 4` = `Dim.4`,
                `Variable` = `label`)
vparcG2 <- vparc_F2_18x1 %>% mutate(across(is.numeric, round, digits=2))%>%
  dplyr::rename(`Dimensión 1` = `Dim.1`,
                `Dimensión 2` = `Dim.2`,
                `Dimensión 3` = `Dim.3`,
                `Dimensión 4` = `Dim.4`,
                `Variable` = `label`)
vparcG3 <- vparc_F2_1x5 %>% mutate(across(is.numeric, round, digits=2))%>%
  dplyr::rename(`Dimensión 1` = `Dim.1`,
                `Dimensión 2` = `Dim.2`,
                `Dimensión 3` = `Dim.3`,
                `Dimensión 4` = `Dim.4`,
                `Variable` = `label`)
#library(openxlsx)
#getwd()
#write.xlsx(vparcG1, "VariablesParcialesG1.xlsx")
#write.xlsx(vparcG2, "VariablesParcialesG2.xlsx")
#write.xlsx(vparcG3, "VariablesParcialesG3.xlsx")

#Creo un dataframe con las coordenadas de las variables consenso
vconsenso <- as.data.frame(res.dmfa$var$coord[,1:4])%>%
  rownames_to_column("label")

#Se definen data frames para poder dibujar el círculo. Ya fueron definidas al pcpio
#theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
#circle <- data.frame(xcircle = cos(theta), ycircle = sin(theta), stringsAsFactors = TRUE)

#Se definen data frames para poder dibujar las flechas
origin <- rep(0, nrow(vparc_F2_1x8))
dd <- cbind.data.frame(vparc_F2_1x8, xstart = origin, ystart = origin, stringsAsFactors = TRUE)

#Gráfico con solo los puntos de las variables parciales del grupo 1x8
# Dim 1 y Dim 2
puntos_F2_1x8 <- ggplot(vparc_F2_1x8, aes(Dim.1, Dim.2)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vparc_F2_1x8,
  #          hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 1 (", var_dim1_dmfa, "%)"), 
       y = paste0("Dim 2 (", var_dim2_dmfa, "%)"), 
       title = "Gráfico de variables parciales para la generación G1") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vparc_F2_1x8$Dim.1, yend=vparc_F2_1x8$Dim.2),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw() +
  geom_text_repel(aes(label = label),
                  size= 4,
                  min.segment.length = 0.5, #only draw line segments that are longer than 0.5
                  seed = 42, #random seed for recreating the exact same layout
                  #box.padding = 0.5, #padding around the text label
                  #max.overlaps = Inf, #discard text labels that overlap too many other text labels or data points. asi, nunca va a descartar a labels que se superponen
                  #arrow = arrow(length = unit(0.010, "npc")), #render line segment as an arrow with grid::arrow()
                  #nudge_x = .15, #adjust the starting x position of the text label. 0 is the default value
                  nudge_y = -.01,
                  color = "#000000"
  )
# Dim 3 y Dim 4
puntos_F2_1x8_3y4 <- ggplot(vparc_F2_1x8, aes(Dim.3, Dim.4)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vparc_F2_1x8,
  #          hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 3 (", var_dim3_dmfa, "%)"), 
       y = paste0("Dim 4 (", var_dim4_dmfa, "%)"), 
       title = "Gráfico de variables parciales para la generación G1") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vparc_F2_1x8$Dim.3, yend=vparc_F2_1x8$Dim.4),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw() +
  geom_text_repel(aes(label = label),
                  size= 4,
                  min.segment.length = 0.5, #only draw line segments that are longer than 0.5
                  seed = 42, #random seed for recreating the exact same layout
                  #box.padding = 0.5, #padding around the text label
                  #max.overlaps = Inf, #discard text labels that overlap too many other text labels or data points. asi, nunca va a descartar a labels que se superponen
                  #arrow = arrow(length = unit(0.010, "npc")), #render line segment as an arrow with grid::arrow()
                  #nudge_x = .15, #adjust the starting x position of the text label. 0 is the default value
                  nudge_y = -.01,
                  color = "#000000"
  )
puntos_F2_1x8
puntos_F2_1x8_3y4
#Gráfico con solo los puntos de las variables parciales del grupo 18x1
# Dim 1 y Dim 2
puntos_F2_18x1 <- ggplot(vparc_F2_18x1, aes(Dim.1, Dim.2)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vparc_F2_1x8,
  #          hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 1 (", var_dim1_dmfa, "%)"), 
       y = paste0("Dim 2 (", var_dim2_dmfa, "%)"),
       title = "Gráfico de variables parciales para la generación G2") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vparc_F2_18x1$Dim.1, yend=vparc_F2_18x1$Dim.2),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw() +
  geom_text_repel(aes(label = label),
                  size= 4,
                  min.segment.length = 2, #only draw line segments that are longer than 0.5
                  seed = 42, #random seed for recreating the exact same layout
                  #box.padding = 0.5, #padding around the text label
                  #max.overlaps = Inf, #discard text labels that overlap too many other text labels or data points. asi, nunca va a descartar a labels que se superponen
                  #arrow = arrow(length = unit(0.010, "npc")), #render line segment as an arrow with grid::arrow()
                  #nudge_x = -.1, #adjust the starting x position of the text label. 0 is the default value
                  #nudge_y = .01, #-.01 .175
                  color = "#000000"
  )
# Dim 3 y Dim 4
puntos_F2_18x1_3y4 <- ggplot(vparc_F2_18x1, aes(Dim.3, Dim.4)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vparc_F2_1x8,
  #          hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 3 (", var_dim3_dmfa, "%)"), 
       y = paste0("Dim 4 (", var_dim4_dmfa, "%)"),
       title = "Gráfico de variables parciales para la generación G2") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vparc_F2_18x1$Dim.3, yend=vparc_F2_18x1$Dim.4),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw() +
  geom_text_repel(aes(label = label),
                  size= 4,
                  min.segment.length = 2, #only draw line segments that are longer than 0.5
                  seed = 42, #random seed for recreating the exact same layout
                  #box.padding = 0.5, #padding around the text label
                  #max.overlaps = Inf, #discard text labels that overlap too many other text labels or data points. asi, nunca va a descartar a labels que se superponen
                  #arrow = arrow(length = unit(0.010, "npc")), #render line segment as an arrow with grid::arrow()
                  #nudge_x = -.1, #adjust the starting x position of the text label. 0 is the default value
                  #nudge_y = .01, #-.01 .175
                  color = "#000000"
  )
puntos_F2_18x1
puntos_F2_18x1_3y4

#Gráfico con solo los puntos de las variables parciales del grupo 18x1
# Dim 1 y Dim 2
puntos_F2_1x5 <- ggplot(vparc_F2_1x5, aes(Dim.1, Dim.2)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vparc_F2_1x8,
  #          hjust="left", vjust="right",
  #          size=4)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 1 (", var_dim1_dmfa, "%)"), 
       y = paste0("Dim 2 (", var_dim2_dmfa, "%)"),
       title = "Gráfico de variables parciales para la generación G3") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vparc_F2_1x5$Dim.1, yend=vparc_F2_1x5$Dim.2),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw() +
  geom_text_repel(aes(label = label),
                  size= 4,
                  min.segment.length = 2, #only draw line segments that are longer than 0.5
                  seed = 42, #random seed for recreating the exact same layout
                  #box.padding = 0.5, #padding around the text label
                  #max.overlaps = Inf, #discard text labels that overlap too many other text labels or data points. asi, nunca va a descartar a labels que se superponen
                  #arrow = arrow(length = unit(0.010, "npc")), #render line segment as an arrow with grid::arrow()
                  nudge_x = 0, #adjust the starting x position of the text label. 0 is the default value
                  nudge_y = .175, #-.01 .175
                  color = "#000000"
  )
# Dim 3 y Dim 4
puntos_F2_1x5_3y4 <- ggplot(vparc_F2_1x5, aes(Dim.3, Dim.4)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vparc_F2_1x8,
  #          hjust="left", vjust="right",
  #          size=4)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 3 (", var_dim3_dmfa, "%)"), 
       y = paste0("Dim 4 (", var_dim4_dmfa, "%)"),
       title = "Gráfico de variables parciales para la generación G3") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vparc_F2_1x5$Dim.3, yend=vparc_F2_1x5$Dim.4),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw() +
  #geom_text_repel(aes(label = label))
  geom_text_repel(aes(label = label),
                  size= 4,
                  min.segment.length = 2, #only draw line segments that are longer than 0.5
                  seed = 42, #random seed for recreating the exact same layout
                  #box.padding = 0.5, #padding around the text label
                  #max.overlaps = Inf, #discard text labels that overlap too many other text labels or data points. asi, nunca va a descartar a labels que se superponen
                  #arrow = arrow(length = unit(0.010, "npc")), #render line segment as an arrow with grid::arrow()
                  nudge_x = 0, #adjust the starting x position of the text label. 0 is the default value
                  nudge_y = .175, #-.01 .175
                  color = "#000000"
  )
puntos_F2_1x5
puntos_F2_1x5_3y4

#Grafico de variables consenso

#Para distinguirlo de los otros gráficos, decidi ponerle una caja a las etiquetas
#en vez de utilizar la funcion geom_repel_text utilizo geom_repel_label

#Dim 1 y Dim 2
puntos_consenso <- ggplot(vconsenso, aes(Dim.1, Dim.2)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vparc_F2_1x8,
  #          hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 1 (", var_dim1_dmfa, "%)"), 
       y = paste0("Dim 2 (", var_dim2_dmfa, "%)"),
       title = "Gráfico de variables consenso") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vconsenso$Dim.1, yend=vconsenso$Dim.2),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw()+
  geom_label_repel(aes(label = label),
                   size= 4,
                   min.segment.length = 2, #only draw line segments that are longer than 0.5
                   seed = 42, #random seed for recreating the exact same layout
                   box.padding = 0.5, #padding around the text label
                   #max.overlaps = Inf, #discard text labels that overlap too many other text labels or data points. asi, nunca va a descartar a labels que se superponen
                   #arrow = arrow(length = unit(0.010, "npc")), #render line segment as an arrow with grid::arrow()
                   nudge_x = 0, #adjust the starting x position of the text label. 0 is the default value
                   nudge_y = .1, #-.01 .175
                   color = "#000000"
  )
# Dim 3 y Dim 4
puntos_consenso_3y4 <- ggplot(vconsenso, aes(Dim.3, Dim.4)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vparc_F2_1x8,
  #          hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 3 (", var_dim3_dmfa, "%)"), 
       y = paste0("Dim 4 (", var_dim4_dmfa, "%)"),
       title = "Gráfico de variables consenso") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vconsenso$Dim.3, yend=vconsenso$Dim.4),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw()+
  geom_label_repel(aes(label = label),
                   size= 4,
                   min.segment.length = 2, #only draw line segments that are longer than 0.5
                   seed = 42, #random seed for recreating the exact same layout
                   box.padding = 0.5, #padding around the text label
                   #max.overlaps = Inf, #discard text labels that overlap too many other text labels or data points. asi, nunca va a descartar a labels que se superponen
                   #arrow = arrow(length = unit(0.010, "npc")), #render line segment as an arrow with grid::arrow()
                   nudge_x = 0, #adjust the starting x position of the text label. 0 is the default value
                   nudge_y = .1, #-.01 .175
                   color = "#000000"
  )
puntos_consenso
puntos_consenso_3y4
gridExtra::grid.arrange(puntos_F2_18x1, puntos_F2_1x5, 
                        puntos_F2_1x8, puntos_consenso,
                        ncol = 2, nrow = 2)

dev.size()
puntos_F2_1x8
puntos_F2_1x8_3y4
puntos_F2_1x5
puntos_F2_1x5_3y4
puntos_F2_18x1
puntos_F2_18x1_3y4
puntos_consenso
puntos_consenso_3y4

#Guardar los gráficos con la función ggsave hace que queden super
#crisp pero no coincide con el estilo de los gráficos anteriores.
#Asi que voy a seguir guardandolos como Export>'Export as Image'
#ggsave("Datos/DMFA/Grafico de var parc G1 dim 1 dim 2.png", 
#       plot = last_plot(),
#       width = 10,
#       height= 6,
#       dpi=300)


# Gráfico 'Individuals factor map (PCA)'
#Gráfico de los individuos pintados de distinto color según su grupo de proveniencia
res.dmfa$ind$coord

individuos <- as.data.frame(round(res.dmfa$ind$coord[,1:4],2))
individuos <- cbind(individuos, union_dmfa2[,12])
individuos <- individuos %>%
  dplyr::rename(`Dim 1` = `Dim.1`,
                `Dim 2` = `Dim.2`,
                `Dim 3` = `Dim.3`,
                `Dim 4` = `Dim.4`,
                `Generación` = `union_dmfa2[, 12]`)
#library(openxlsx)
#getwd()
#write.xlsx(individuos, "individuos.xlsx")

ggplot(individuos, aes(x = `Dim 1`, y = `Dim 2`, color = Generación)) +
  geom_point() + #size = 2, alpha = 0.8
  geom_vline(xintercept = 0)+ #eje y
  geom_hline(yintercept=0)+ #eje x
  labs(title = "Gráfico de scores en DMFA",
       x = paste0("Dim 1 (", var_dim1_dmfa, "%)"), 
       y = paste0("Dim 2 (", var_dim2_dmfa, "%)"),
       color = "Generación") +
  theme_bw()

ggplot(individuos, aes(x = `Dim 3`, y = `Dim 4`, color = Generación)) +
  geom_point() + #size = 2, alpha = 0.8
  geom_vline(xintercept = 0)+ #eje y
  geom_hline(yintercept=0)+ #eje x
  labs(title = "Gráfico de scores en DMFA",
       x = paste0("Dim 3 (", var_dim3_dmfa, "%)"), 
       y = paste0("Dim 4 (", var_dim4_dmfa, "%)"),
       color = "Generación") +
  theme_bw()

# Guardamos las  contribuciones y el cos2 de los individuos, para 
# luego exportarlas a Excel
round(res.dmfa$ind$coord[,1:2],2)
res.dmfa$ind$coord
res.dmfa$ind$contrib
res.dmfa$ind$cos2

#Lo de abajo se puede modificar para contemplar las dimensiones 3 y 4 del AFMD
# y no la 1 y 2.
coord_ind <- as.data.frame(round(res.dmfa$ind$coord[,1:2],2))
coord_ind <- cbind(coord_ind, union_dmfa2[,12])
coord_ind <- coord_ind %>%
  dplyr::rename(`Dim 1 coord` = `Dim.1`,
                `Dim 2 coord` = `Dim.2`,
                `Generación` = `union_dmfa2[, 12]`)

contrib_ind <- as.data.frame(round(res.dmfa$ind$contrib[,1:2],2))
contrib_ind <- cbind(contrib_ind, union_dmfa2[,12])
contrib_ind <- contrib_ind %>%
  dplyr::rename(`Dim 1 contrib` = `Dim.1`,
                `Dim 2 contrib` = `Dim.2`,
                `Generación` = `union_dmfa2[, 12]`)

cos2_ind <- as.data.frame(round(res.dmfa$ind$cos2[,1:2],2))
cos2_ind <- cbind(cos2_ind, union_dmfa2[,12])
cos2_ind <- cos2_ind %>%
  dplyr::rename(`Dim 1 cos2` = `Dim.1`,
                `Dim 2 cos2` = `Dim.2`,
                `Generación` = `union_dmfa2[, 12]`)

#Unimos los 3 datasets, dejo una unica columna para `Generación`
tablaInd <- dplyr::bind_cols(coord_ind %>% select(-c(`Generación`)),
                             contrib_ind %>% select(-c(`Generación`)),
                             cos2_ind)
#library(openxlsx)
#getwd()
#setwd("C:/Users/lucia/Desktop/ESTADISTICA/Tesina/Datos/DMFA")
#write.xlsx(tablaInd, "TablaIndividuossssssss.xlsx")


#¿Cuál es el porcentaje de expicación del plano principal?
res.dmfa$eig
# 46.569%

# ¿Qué puede decir a partir del gráfico de las condiciones?
#Gráfico 'Projection of the groups': gráfico de las condiciones
#Los tres grupos aportan a la primera componente, y además, el comportamiento de
#las correlaciones de los dos grupos es bastante similar
#Los grupos F2_18x1 y F2_1x5 aportan más a la dimensión 2 (más que el grupo F2_1x8)
#¿Además, al estar cerca, esto nos da indicios de que tienen una estructura
#de correlación similar?
res.dmfa$group
res.dmfa$group$coord
grupos <- as.data.frame(round(res.dmfa$group$coord.n[,1:4],2))
grupos <- rownames_to_column(grupos, var = "Generación")
grupos <- grupos %>%
  dplyr::rename(`Dim 1` = `Dim.1`,
                `Dim 2` = `Dim.2`,
                `Dim 3` = `Dim.3`,
                `Dim 4` = `Dim.4`,
                `Generación` = `Generación`)

ggplot(grupos, aes(x = `Dim 1`, y = `Dim 2`, color = Generación)) +
  geom_point(size = 4) + #size = 2, alpha = 0.8
  geom_vline(xintercept = 0)+ #eje y
  geom_hline(yintercept=0)+ #eje x
  labs(title = "Gráfico de contribución de cada grupo en el AFMD",
       x = paste0("Dim 1 (", var_dim1_dmfa, "%)"), 
       y = paste0("Dim 2 (", var_dim2_dmfa, "%)"),
       color = "Generación") +
  theme_bw()
ggplot(grupos, aes(x = `Dim 3`, y = `Dim 4`, color = Generación)) +
  geom_point(size = 4) + #size = 2, alpha = 0.8
  geom_vline(xintercept = 0)+ #eje y
  geom_hline(yintercept=0)+ #eje x
  labs(title = "Gráfico de contribución de cada grupo en el AFMD",
       x = paste0("Dim 3 (", var_dim3_dmfa, "%)"), 
       y = paste0("Dim 4 (", var_dim4_dmfa, "%)"),
       color = "Generación") +
  theme_bw()

#Gráfico 'Biplot between axes 1 and 2 for group F2_18x1' por default
#te da uno solo, pero debería haber uno por grupo (o sea tres). Con la siguiente
#sentencia te da los dos gráficos:
plot(res.dmfa, choix = "var",
     label="all",
     xlim = NULL, ylim= NULL,
     title = NULL,
     cex = 0.7,
     cex.main = 0.8,
     cex.lab=1)
res.dmfa.3.4
plot(res.dmfa.3.4, choix = "var",
     label="all",
     xlim = NULL, ylim= NULL,
     title = NULL,
     cex = 0.7,
     cex.main = 0.8,
     cex.lab=1,
     axes = c(3,4))

#La unica forma es haciendo captura de pantalla

#Grafico de variables consenso
library(ade4)
s.corcircle(res.dmfa$var$coord[,1:2], 
            clabel=.8) #tamaño de la label)
s.corcircle(res.dmfa$var.partiel$F2_18x1[,1:2], grid = TRUE) #aqui es donde vemos que las 
#coordenadas toman valores demasiado bajos
compromiso <- as.data.frame(res.dmfa$var.partiel$F2_18x1[,1:2])

ggplot(compromiso, aes(Dim.1, Dim.2)) +
  geom_point() +
  geom_text(aes(label = rownames(compromiso)), data=compromiso,
            hjust="left", vjust="right",
            size=3)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  theme_bw()

#Comparamos los dos biplots anteriores y el grafico
#de variables consenso:
#En la familia F2_18x1 se observa que la correlación no es tan fuerte 
#entre Forma y AT, en comparación con la estructura consenso.
#No hay diferencias muy notorias en el resto de variables.

#En la familia F2_1x8, se observa menos correlación entre la 
#variable Firmeza y L, comparandola con la estructura consenso. También
#se observa una mayor correlación (dado la cercania)
#entre las variables a/b y SS que en la estructura consenso.

#En la familia F2_1x5, se percibe menos correlación entre a/b y SS,
# a su vez, tiene una mayor correlación entre SS y Forma. Y las variables
#Firmeza y L no están tan correlacionadas.


#-----------------------------
# ANALISIS EN VARIABLES CUALI
#-----------------------------

# Se evalúa cuáles columnas son comunes entre las 3 bases
# Las variables cualitativas en comun son:
#todas menos 271J2
#quitaremos ésta última además de las variables cuantitativas

#Es importante volver a cargar las bases, porque si no, los nombres
#de las variables no van a coincidir


#-------#
# F2_1x8#
#-------#

#Selecciono solo las columnas que las 3 bases tengan en común
F2_1x8 <- F2_1x8 %>%
  select(-c("N° Planta", "Diámetro", "Altura", "Forma", "Peso",
            "Firmeza", "L", "a/b", "AT", "pH",
            "SS", "VP"))

#Creo una nueva columna con el nombre de la familia de tomate
F2_1x8$flia <- rep("F2_1x8", nrow(F2_1x8))

#Elimino las filas con datos faltantes
#Esto debe hacerse una vez que tengo seleccionadas las variables,
#porque si no, me quedo sin observaciones
F2_1x8 <- drop_na(F2_1x8)
#Me quedo con 0 observaciones

#--------#
# F2_18x1#
#--------#

#Selecciono solo las columnas que las 3 bases tengan en común
F2_18x1 <- F2_18x1 %>%
  select(-c("N° Planta", "Diametro", "Altura", "Forma (A/D)", "Peso",
            "Firmeza", "L", "a/b", "Acidez Titulable", "pH",
            "SS", "Días", "271J2"))

#Creo una nueva columna con el nombre de la familia de tomate
#F2_18x1$flia <- rep("F2_18x1", nrow(F2_18x1))

#Elimino las filas con datos faltantes
#Esto debe hacerse una vez que tengo seleccionadas las variables,
#porque si no, me quedo sin observaciones
F2_18x1 <- drop_na(F2_18x1)
#Me quedo con 46 observaciones

#-------#
# F2_1x5#
#-------#

#Selecciono solo las columnas que las 3 bases tengan en común
F2_1x5 <- F2_1x5 %>%
  select(-c("Planta", "Diámetro", "Altura", "Forma", "Peso",
            "Firmeza", "L", "a/b", "Acidez Titulable", "pH",
            "Sólidos Solubles", "Vida Poscosecha", "271J2"))

#Creo una nueva columna con el nombre de la familia de tomate
#F2_1x5$flia <- rep("F2_1x5", nrow(F2_1x5))

#Elimino las filas con datos faltantes
#Esto debe hacerse una vez que tengo seleccionadas las variables,
#porque si no, me quedo sin observaciones
F2_1x5 <- drop_na(F2_1x5)
#Me quedo con 44 observaciones

#Union
#Dado que la base 1 queda sin observaciones con NA, solo uniré la base 2 y la 3
#Convertimos a todas las columnas al tipo factoro
F2_1x5 <- F2_1x5 %>% mutate(across(1:75, as.numeric))
F2_18x1 <- F2_18x1 %>% mutate(across(1:75, as.numeric))
str(F2_1x5)
str(F2_18x1)
union23 <- full_join(x = F2_18x1, y = F2_1x5)

#Sobre la base de datos union23 debería aplicar la tecnica DMFA 
#-------------------------#
# Aplicacion de la Tecnica#
# DMFA en variables cuali#
#-------------------------#

#Antes de aplicar la tecnica DMFA, necesitamos aplicar ciertas transformaciones
#a la matriz de datos para poder aplicarla.

#Cada columna y^l_q para el grupo l va a ser pesada por la propoción de individuos
#que no poseen la categoria q. En nuestro caso tenemos dos grupos: F2_18x1 y F2_1x5
#Comencemos trabajando para una sola variable: 363F en F2_18x1
#Debemos dividirla en dos:

aver <- as.data.frame(F2_18x1$`363F`)
colnames(aver) <- c("363F")
colnames(aver[,1])
variable_name <- paste0(colnames(aver[1]),"_0")
aver <- aver %>%
  mutate(`pastef(aver[,1])` = case_when(aver[,1] == 1 ~ 0,
                                        TRUE  ~ 1))

b <- count(aver, `363F`)
m <- ((b$n[1]+b$n[2])-b$n[1])/(b$n[1]+b$n[2])

b <- count(aver, `363F_0`)
m <- ((b$n[1]+b$n[2])-b$n[1])/(b$n[1]+b$n[2])












b <- count(F2_18x1, `142II`)
m <- ((b$n[1]+b$n[2])-b$n[1])/(b$n[1]+b$n[2])
F2_18x1$`363F` <- F2_18x1$`363F`*m
b <- count(F2_18x1, F2_18x1[,1])

#Nos encontramos con variables que tienen los mismos valores, por ejemplo:
#todas las filas de la variable `140 II` valen 1
prueba <- F2_18x1 %>% select(where(~n_distinct(.) > 1))
#Pasamos de 75 a 30 variables

prueba_1x5 <- F2_1x5 %>% select(where(~n_distinct(.) > 1))
#Pasamos de 75 a 27 variables

#Tenemos que volver a elegir columnas en común


l <- numeric()
for(i in 1:ncol(F2_18x1)){
  b <- count(F2_18x1, F2_18x1[,i]) #Contamos la cantidad de unidades en cada categoria
  l[i] <- ((b$n[1]+b$n[2])-b$n[1])/(b$n[1]+b$n[2]) #Guardamos el peso m^l_q para cada
  #columna de la base
}

for(i in 1:ncol(F2_18x1)){
  F2_18x1[,i] <- F2_18x1[,i]*l[i] 
}

Z_18x1 <- as.data.frame(scale(F2_18x1)) #Escalamos el primer grupo












DMFA(union, num.fact = 11, scale.unit = TRUE, ncp =5, axes= c(1,2))



#---------
# Intento antiguo
#---------

#---------#
#Variables en comun entre F2_1x8 y F2_1x5
#---------#
#Selecciono solo las columnas que las 3 bases tengan en común
F2_1x8_a <- F2_1x8 %>%
  select("N° Planta", "Diámetro", "Altura", "Forma", "Peso",
         "VP","Firmeza", "L", "a/b", "AT",
         "pH","SS","101J", "102HH", "112JJ","121JJ",
         "131J","140II","142II","146JJ","148II",
         "148J","150II","155II","159H","184J",
         "200H","208JJ","221JJ","251II","253F",
         "303II","305F","332F","340H","430F",
         "434H","470F","500HH")

#Renombro columnas con distintos nombres a traves de las 3 bases
F2_1x8_a <- F2_1x8_a %>%
  dplyr::rename(`Planta` = `N° Planta`,
                `Diametro` = `Diámetro`)
#Elimino las filas con datos faltantes
#Esto debe hacerse una vez que tengo seleccionadas las variables,
#porque si no, me quedo sin observaciones
F2_1x8_a <- drop_na(F2_1x8_a)
#Absolutamente todas las filas tienen NAs, salvo por 4.

#Selecciono solo las columnas que las 3 bases tengan en común
F2_1x5_a <- F2_1x5 %>%
  select("Planta", "Diámetro", "Altura", "Forma", "Peso", "Vida Poscosecha",
         "Firmeza", "L", "a/b", "Acidez Titulable", "pH",
         "Sólidos Solubles","101J", "102HH", "112JJ","121JJ",
         "131J","140II","142II","146JJ","148II",
         "148J","150II","155II","159H","184J",
         "200H","208JJ","221JJ","251II","253F",
         "303II","305F","332F","340H","430F",
         "434H","470F","500HH")

#Renombro columnas con distintos nombres a traves de las 3 bases
F2_1x5_a <- F2_1x5_a %>%
  dplyr::rename(`Diametro` = `Diámetro`,
                `VP` = `Vida Poscosecha`,
                `AT` = `Acidez Titulable`,
                `SS` = `Sólidos Solubles`)
#Elimino las filas con datos faltantes
#Esto debe hacerse una vez que tengo seleccionadas las variables,
#porque si no, me quedo sin observaciones
F2_1x5_a <- drop_na(F2_1x5_a)
#Me quedo solo con 23 observaciones


#----------#
#Variables en comun entre F2_1x8 y F2_18x1
#----------#
#Selecciono solo las columnas que las 3 bases tengan en común
F2_1x8_b <- F2_1x8 %>%
  select("N° Planta", "Diámetro", "Altura", "Forma", "Peso",
         "Firmeza", "L", "a/b", "AT","pH","SS",
         "137H", "143-144-145JJ","156-159II","159-158-157-156H",
         "177J","183II","184-183J","188-189-191JJ","201H",
         "213-214JJ","230F","232-231II","236II","246F","249F",
         "271J...22","277J","282II","283F","293H","315J",
         "351F","359J","363F","376H","435II","452JJ","468JJ",
         "496JJ")

#Renombro columnas con distintos nombres a traves de las 3 bases
F2_1x8_b <- F2_1x8_b %>%
  dplyr::rename(`Planta` = `N° Planta`,
                `Diametro` = `Diámetro`)
#Elimino las filas con datos faltantes
#Esto debe hacerse una vez que tengo seleccionadas las variables,
#porque si no, me quedo sin observaciones
F2_1x8_b <- drop_na(F2_1x8_b)
#Absolutamente todas las filas tienen NAs, salvo por 1.

#Selecciono solo las columnas que las 3 bases tengan en común
F2_18x1_b <- F2_18x1 %>%
  select("N° Planta", "Diametro", "Altura", "Forma (A/D)", "Peso",
         "Firmeza", "L", "a/b", "Acidez Titulable","pH","SS",
         "137H", "143-144-145JJ","156-159II","159-158-157-156H",
         "177J","183II","184-183J","188-189-191JJ","201H",
         "213-214JJ","230F","232-231II","236II","246F","249F",
         "271J","277J","282II","283F","293H","315J",
         "351F","359J","363F","376H","435II","452JJ","468JJ",
         "496JJ")

#Renombro columnas con distintos nombres a traves de las 3 bases
F2_18x1_b <- F2_18x1_b %>%
  dplyr::rename(`Forma` = `Forma (A/D)`,
                `Planta` = `N° Planta`,
                `AT` = `Acidez Titulable`)
#Elimino las filas con datos faltantes
#Esto debe hacerse una vez que tengo seleccionadas las variables,
#porque si no, me quedo sin observaciones
F2_18x1_b <- drop_na(F2_18x1_b)
#Me quedan 44 observaciones sin NAs

#Union
union12 <- full_join(x = F2_1x8_b, y = F2_18x1_b)
union13 <- full_join(x = F2_1x8_a, y = F2_1x5_a)


#---------------
# CONDICION 9
# Simulación del
# artículo DMFA
# sebastian le entero
#---------------
library(MASS)
# Definir el número de observaciones
n <- 100

# Definir el vector de medias
mean_vec <- rep(0, 7)  # Todos los valores medios son 0

#---------#
# GRUPO 1 #
#---------#
# Definir la matriz de correlaciones
cor_mat <- matrix(c(
  1, .976, .184, -.102, -.026, .017,.011,
  .976,1, .005,-.285,-.119,.066,.046,
  .184,.005,1,.953,.029,.055,.051,
  -.102,-.285,.953,1,.056,-.015,.012,
  -.026,-.119,.029,.056,1,-.088,.116,
  .017,.066,.055,-.015,-.088,1,-.130,
  .011,.046,.051,.012,.116,-.130,1), nrow = 7, ncol = 7)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat) #Lo es

# Calcular la matriz de varianzas
variances <- diag(cor_mat)  # Obtener las varianzas de la diagonal de la matriz de correlación
D <- diag(sqrt(variances))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas

# Calcular la matriz de covarianza
cov_mat <- D %*% cor_mat %*% D

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo1 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat, empirical = TRUE)

# Convertir los datos generados en un data frame
grupo1 <- as.data.frame(grupo1)

# Verificar la correlación entre las variables
cor(grupo1)
cov(grupo1)

#---------#
# GRUPO 2 #
#---------#
# Definir la matriz de correlaciones
cor_mat2 <- matrix(c(
  1,.957,.201,-.115,.043,.455,-.343,
  .957,1,.245,-.067,-.084,.342,-.471,
  .201,.245,1,.931,-.043,.104,-.086,
  -.115,-.067,.931,1,-.132,-.091,-.042,
  .043,-.084,-.043,-.132,1,.881,.912,
  .455,.342,.104,-.091,.881,1,.652,
  -.343,-.471,-.086,-.042,.912,.652,1
), nrow = 7, ncol = 7)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat2) #Lo es

# Calcular la matriz de varianzas
variances2 <- diag(cor_mat2)  # Obtener las varianzas de la diagonal de la matriz de correlación
D2 <- diag(sqrt(variances2))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas

# Calcular la matriz de covarianza
cov_mat2 <- D2 %*% cor_mat2 %*% D2 #desvios por la matriz de correlacion = matriz de var y covar

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo2 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat2, empirical = TRUE)

# Convertir los datos generados en un data frame
grupo2 <- as.data.frame(grupo2)

# Verificar la correlación entre las variables
cor(grupo2)

datos_simulados <- rbind.data.frame(grupo1, grupo2)
datos_simulados$flia <- c(rep('grupo1',100), rep('grupo2',100))

DMFA(datos_simulados, num.fact =8, scale.unit = TRUE)
b <- DMFA(datos_simulados, num.fact =8, scale.unit = TRUE, axes = c(1, 2)) #ejes 1 y 2
d <- DMFA(datos_simulados, num.fact =8, scale.unit = TRUE, axes = c(3,4)) #ejes 3 y 4
b$eig
b$var
d$var
plot(b, choix = "var",
     label="all",
     xlim = NULL, ylim= NULL,
     title = NULL,
     cex = 0.7,
     cex.main = 0.8,
     cex.lab=1)

#-------------------------
# Simulación del articulo
# del DMFA categorico para
# variables cuantitativas
# DAN MAL
#-------------------------
library(MASS)
# Definir el número de observaciones
n <- 100

# Definir el vector de medias
mean_vec <- rep(0, 4)  # Todos los valores medios son 0

#---------#
# GRUPO 1 #
#---------#
# Definir la matriz de correlaciones
cor_mat <- matrix(c(
  1, .756, -.756, .109,
  .756,1, -.513, .109,
  -.756,-.513,1,-.109,
  .109,.109,-.109,1), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat) #Lo es

# Calcular la matriz de varianzas
variances <- diag(cor_mat)  # Obtener las varianzas de la diagonal de la matriz de correlación
D <- diag(sqrt(variances))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas

# Calcular la matriz de covarianza
cov_mat <- D %*% cor_mat %*% D

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo1 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat, empirical = TRUE)

# Convertir los datos generados en un data frame
grupo1 <- as.data.frame(grupo1)

# Verificar la correlación entre las variables
cor(grupo1)
summary(grupo1)
cov(grupo1)

#---------#
# GRUPO 2 #
#---------#
# Definir la matriz de correlaciones
cor_mat2 <- matrix(c(
  1, .657, .029, -.845,
  .657,1 , -.314, -.507,
  .029, -.314, 1, .169,
  -.845, -.507, .169, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat2) #Lo es

# Calcular la matriz de varianzas
variances2 <- diag(cor_mat2)  # Obtener las varianzas de la diagonal de la matriz de correlación
D2 <- diag(sqrt(variances2))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas
#Se está asumienod que todas las variables tienen desvio estándar = 1

# Calcular la matriz de covarianza
cov_mat2 <- D2 %*% cor_mat2 %*% D2 #desvios por la matriz de correlacion = matriz de var y covar

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo2 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat2, empirical = TRUE)

# Convertir los datos generados en un data frame
grupo2 <- as.data.frame(grupo2)

# Verificar la correlación entre las variables
cor(grupo2)

datos_simulados <- rbind.data.frame(grupo1, grupo2)
datos_simulados$flia <- c(rep('grupo1',100), rep('grupo2',100))

library(FactoMineR)
DMFA(datos_simulados, num.fact =5, scale.unit = FALSE)
b <- DMFA(datos_simulados, num.fact =5, axes = c(1, 2)) #ejes 1 y 2
d <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(3,4)) #ejes 3 y 4

b$eig
b$var
plot(d, choix = "var",
     label="all",
     xlim = NULL, ylim= NULL,
     title = NULL,
     cex = 0.7,
     cex.main = 0.8,
     cex.lab=1)

#Creo dataframes con las coordenadas de las variables parciales y los nombres de las variables
vparc_g1 <- as.data.frame(b$var.partiel$grupo1[,1:2]) %>%
  rownames_to_column("label")
vparc_g2 <- as.data.frame(b$var.partiel$grupo2[,1:2]) %>%
  rownames_to_column("label")

#Creo un dataframe con las coordenadas de las variables consenso
vconsenso <- as.data.frame(b$var$coord[,1:2])%>%
  rownames_to_column("label")











#Gráfico con solo los puntos de las variables parciales del grupo 1
origin <- rep(0, nrow(vparc_g1))
dd <- cbind.data.frame(vparc_g1, xstart = origin, ystart = origin, stringsAsFactors = TRUE)

ggplot(vparc_g1, aes(Dim.1, Dim.2)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vparc_F2_1x8,
  #          hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= "Dim 1 (53.19%)", y = "Dim 2 (24.79%)", title = "Gráfico de variables parciales del grupo 1") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vparc_g1$Dim.1, yend=vparc_g1$Dim.2),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw() +
  geom_text_repel(aes(label = label))

#Gráfico con solo los puntos de las variables parciales del grupo 2
origin <- rep(0, nrow(vparc_g2))
dd <- cbind.data.frame(vparc_g2, xstart = origin, ystart = origin, stringsAsFactors = TRUE)

ggplot(vparc_g2, aes(Dim.1, Dim.2)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vparc_F2_1x8,
  #          hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= "Dim 1 (53.19%)", y = "Dim 2 (24.79%)", title = "Gráfico de variables parciales del grupo 2") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vparc_g2$Dim.1, yend=vparc_g2$Dim.2),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw() +
  geom_text_repel(aes(label = label))

ggplot(vconsenso, aes(Dim.1, Dim.2)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vconsenso,
  #          #hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= "Dim 1 (53.19%)", y = "Dim 2 (24.79%)", title = "Gráfico de variables consenso") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vconsenso$Dim.1, yend=vconsenso$Dim.2),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw()+
  geom_label_repel(aes(label = label))


#---------------------------
# CONDICION 7
# Simulación de nuevas 
# estructuras de correlación
# Sebastian Le primera submatriz
#---------------------------
library(MASS)
# Definir el número de observaciones
n <- 100

# Definir el vector de medias
mean_vec <- rep(0, 4)  # Todos los valores medios son 0

#---------#
# GRUPO 1 #
#---------#
# Definir la matriz de correlaciones
cor_mat1 <- matrix(c(
  1, .976, .184, -.102,
  .976, 1 , .005, -.285,
  .184, .005, 1, .953,
  -.102, -.285, .953, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat1) #Lo es

# Calcular la matriz de varianzas
variances <- diag(cor_mat1)  # Obtener las varianzas de la diagonal de la matriz de correlación
D <- diag(sqrt(variances))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas

# Calcular la matriz de covarianza
cov_mat1 <- D %*% cor_mat1 %*% D

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo1 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat1, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo1 <- as.data.frame(grupo1)

# Verificar la correlación entre las variables
cor(grupo1)
summary(grupo1)
cov(grupo1)

#---------#
# GRUPO 2 #
#---------#
# Definir la matriz de correlaciones
cor_mat2 <- matrix(c(
  1, .957, .201, -.115,
  .957, 1 , .245, -.067,
  .201, .245, 1, .931,
  -.115, -.067, .931, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat2) #Lo es

# Calcular la matriz de varianzas
variances2 <- diag(cor_mat2)  # Obtener las varianzas de la diagonal de la matriz de correlación
D2 <- diag(sqrt(variances2))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas
#Se está asumienod que todas las variables tienen desvio estándar = 1

# Calcular la matriz de covarianza
cov_mat2 <- D2 %*% cor_mat2 %*% D2 #desvios por la matriz de correlacion = matriz de var y covar

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo2 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat2, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo2 <- as.data.frame(grupo2)

# Verificar la correlación entre las variables
cor(grupo2)

datos_simulados <- rbind.data.frame(grupo1, grupo2)
datos_simulados$flia <- c(rep('grupo1',100), rep('grupo2',100))

library(FactoMineR)
DMFA(datos_simulados, num.fact =5, scale.unit = FALSE)
b <- DMFA(datos_simulados, num.fact =5, scale.unit = FALSE, axes = c(1, 2)) #ejes 1 y 2
d <- DMFA(datos_simulados, num.fact =5, scale.unit = FALSE, axes = c(3,4)) #ejes 3 y 4

b$eig
b$var
plot(d, choix = "var",
     label="all",
     xlim = NULL, ylim= NULL,
     title = NULL,
     cex = 0.7,
     cex.main = 0.8,
     cex.lab=1)

#Creo dataframes con las coordenadas de las variables parciales y los nombres de las variables
vparc_g1 <- as.data.frame(b$var.partiel$grupo1[,1:2]) %>%
  rownames_to_column("label")
vparc_g2 <- as.data.frame(b$var.partiel$grupo2[,1:2]) %>%
  rownames_to_column("label")

#Creo un dataframe con las coordenadas de las variables consenso
vconsenso <- as.data.frame(b$var$coord[,1:2])%>%
  rownames_to_column("label")


#---------------------------
# CONDICION 5
# Simulación de nuevas 
# estructuras de correlación
# Sebastian Le segunda submatriz
#---------------------------
library(MASS)
# Definir el número de observaciones
n <- 100

# Definir el vector de medias
mean_vec <- rep(0, 4)  # Todos los valores medios son 0

#---------#
# GRUPO 1 #
#---------#
# Definir la matriz de correlaciones
cor_mat1 <- matrix(c(
  1, .056, -.015, .012,
  .056, 1 , -.088, .116,
  -.015, -.088, 1, -.130,
  .012, .116, -.130, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat1) #Lo es

# Calcular la matriz de varianzas
variances <- diag(cor_mat1)  # Obtener las varianzas de la diagonal de la matriz de correlación
D <- diag(sqrt(variances))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas

# Calcular la matriz de covarianza
cov_mat1 <- D %*% cor_mat1 %*% D

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo1 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat1, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo1 <- as.data.frame(grupo1)

# Verificar la correlación entre las variables
cor(grupo1)
summary(grupo1)
cov(grupo1)

#---------#
# GRUPO 2 #
#---------#
# Definir la matriz de correlaciones
cor_mat2 <- matrix(c(
  1, -.132, -.091, -.042,
  -.132, 1 , .881, .912,
  -.091, .881, 1, .652,
  -.042, .912, .652, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat2) #Lo es

# Calcular la matriz de varianzas
variances2 <- diag(cor_mat2)  # Obtener las varianzas de la diagonal de la matriz de correlación
D2 <- diag(sqrt(variances2))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas
#Se está asumienod que todas las variables tienen desvio estándar = 1

# Calcular la matriz de covarianza
cov_mat2 <- D2 %*% cor_mat2 %*% D2 #desvios por la matriz de correlacion = matriz de var y covar

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo2 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat2, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo2 <- as.data.frame(grupo2)

# Verificar la correlación entre las variables
cor(grupo2)

datos_simulados <- rbind.data.frame(grupo1, grupo2)
datos_simulados$flia <- c(rep('grupo1',100), rep('grupo2',100))

library(FactoMineR)
DMFA(datos_simulados, num.fact =5, scale.unit = FALSE)
b <- DMFA(datos_simulados, num.fact =5, axes = c(1, 2)) #ejes 1 y 2
d <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(3,4)) #ejes 3 y 4



#---------------------------
# CONDICION 1
# Simulación de nuevas 
# estructuras de correlación
# dos grupos no correlacionados 
# por dentro
#---------------------------
library(MASS)
# Definir el número de observaciones
n <- 100

# Definir el vector de medias
mean_vec <- rep(0, 4)  # Todos los valores medios son 0

#---------#
# GRUPO 1 #
#---------#
# Definir la matriz de correlaciones
cor_mat1 <- matrix(c(
  1, .056, -.015, .012,
  .056, 1 , -.088, .116,
  -.015, -.088, 1, -.130,
  .012, .116, -.130, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat1) #Lo es

# Calcular la matriz de varianzas
variances <- diag(cor_mat1)  # Obtener las varianzas de la diagonal de la matriz de correlación
D <- diag(sqrt(variances))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas

# Calcular la matriz de covarianza
cov_mat1 <- D %*% cor_mat1 %*% D

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo1 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat1, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo1 <- as.data.frame(grupo1)

# Verificar la correlación entre las variables
cor(grupo1)
summary(grupo1)
cov(grupo1)

#---------#
# GRUPO 2 #
#---------#
# Definir la matriz de correlaciones
cor_mat2 <- matrix(c(
  1, -.132, -.091, -.042,
  -.132, 1 , .181, .129,
  -.091, .181, 1, .256,
  -.042, .129, .256, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat2) #Lo es

# Calcular la matriz de varianzas
variances2 <- diag(cor_mat2)  # Obtener las varianzas de la diagonal de la matriz de correlación
D2 <- diag(sqrt(variances2))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas
#Se está asumienod que todas las variables tienen desvio estándar = 1

# Calcular la matriz de covarianza
cov_mat2 <- D2 %*% cor_mat2 %*% D2 #desvios por la matriz de correlacion = matriz de var y covar

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo2 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat2, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo2 <- as.data.frame(grupo2)

# Verificar la correlación entre las variables
cor(grupo2)

datos_simulados <- rbind.data.frame(grupo1, grupo2)
datos_simulados$flia <- c(rep('grupo1',100), rep('grupo2',100))

library(FactoMineR)
DMFA(datos_simulados, num.fact =5, scale.unit = FALSE)
b <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(1, 2)) #ejes 1 y 2
d <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(3,4)) #ejes 3 y 4




#---------------------------
# CONDICION 2
# SEGUNDA ITERACION
# Simulación de nuevas 
# estructuras de correlación
# dos grupos no correlacionados 
# por dentro
#---------------------------
library(MASS)
# Definir el número de observaciones
n <- 100

# Definir el vector de medias
mean_vec <- rep(0, 4)  # Todos los valores medios son 0

#---------#
# GRUPO 1 #
#---------#
# Definir la matriz de correlaciones
cor_mat1 <- matrix(c(
  1, 0.02, -0.04, 0.05,
  0.02, 1, -0.03, 0.01,
  -0.04, -0.03, 1, -0.02,
  0.05, 0.01, -0.02, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat1) #Lo es

# Calcular la matriz de varianzas
variances <- diag(cor_mat1)  # Obtener las varianzas de la diagonal de la matriz de correlación
D <- diag(sqrt(variances))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas

# Calcular la matriz de covarianza
cov_mat1 <- D %*% cor_mat1 %*% D

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo1 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat1, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo1 <- as.data.frame(grupo1)

# Verificar la correlación entre las variables
cor(grupo1)
summary(grupo1)
cov(grupo1)

#---------#
# GRUPO 2 #
#---------#
# Definir la matriz de correlaciones
cor_mat2 <- matrix(c(
  1, -0.06, 0.01, -0.03,
  -0.06, 1, 0.07, -0.02,
  0.01, 0.07, 1, -0.05,
  -0.03, -0.02, -0.05, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat2) #Lo es

# Calcular la matriz de varianzas
variances2 <- diag(cor_mat2)  # Obtener las varianzas de la diagonal de la matriz de correlación
D2 <- diag(sqrt(variances2))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas
#Se está asumienod que todas las variables tienen desvio estándar = 1

# Calcular la matriz de covarianza
cov_mat2 <- D2 %*% cor_mat2 %*% D2 #desvios por la matriz de correlacion = matriz de var y covar

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo2 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat2, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo2 <- as.data.frame(grupo2)

# Verificar la correlación entre las variables
cor(grupo2)

datos_simulados <- rbind.data.frame(grupo1, grupo2)
datos_simulados$flia <- c(rep('grupo1',100), rep('grupo2',100))

library(FactoMineR)
DMFA(datos_simulados, num.fact =5, scale.unit = FALSE)
b <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(1, 2)) #ejes 1 y 2
d <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(3,4)) #ejes 3 y 4


#---------------------------
# CONDICION 3
# Simulación de nuevas 
# estructuras de correlación
# un grupo alta y positivamente correlacionado 
# por dentro y otro grupo no correlacionado
#---------------------------
library(MASS)
# Definir el número de observaciones
n <- 100

# Definir el vector de medias
mean_vec <- rep(0, 4)  # Todos los valores medios son 0

#---------#
# GRUPO 1 #
#---------#
# Definir la matriz de correlaciones
cor_mat1 <- matrix(c(
  1, 0.976, 0.953, 0.957,
  0.976, 1, 0.931, 0.881,
  0.953, 0.931, 1, 0.912,
  0.957, 0.881, 0.912, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat1) #Lo es

# Calcular la matriz de varianzas
variances <- diag(cor_mat1)  # Obtener las varianzas de la diagonal de la matriz de correlación
D <- diag(sqrt(variances))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas

# Calcular la matriz de covarianza
cov_mat1 <- D %*% cor_mat1 %*% D

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo1 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat1, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo1 <- as.data.frame(grupo1)

# Verificar la correlación entre las variables
cor(grupo1)
summary(grupo1)
cov(grupo1)

#---------#
# GRUPO 2 #
#---------#
# Definir la matriz de correlaciones
cor_mat2 <- matrix(c(
  1, .056, -.015, .012,
  .056, 1 , -.088, .116,
  -.015, -.088, 1, -.130,
  .012, .116, -.130, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat2) #Lo es

# Calcular la matriz de varianzas
variances2 <- diag(cor_mat2)  # Obtener las varianzas de la diagonal de la matriz de correlación
D2 <- diag(sqrt(variances2))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas
#Se está asumienod que todas las variables tienen desvio estándar = 1

# Calcular la matriz de covarianza
cov_mat2 <- D2 %*% cor_mat2 %*% D2 #desvios por la matriz de correlacion = matriz de var y covar

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo2 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat2, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo2 <- as.data.frame(grupo2)

# Verificar la correlación entre las variables
cor(grupo2)

datos_simulados <- rbind.data.frame(grupo1, grupo2)
datos_simulados$flia <- c(rep('grupo1',100), rep('grupo2',100))

library(FactoMineR)
DMFA(datos_simulados, num.fact =5, scale.unit = FALSE)
b <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(1, 2)) #ejes 1 y 2
d <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(3,4)) #ejes 3 y 4






#---------------------------
# CONDICION 4
# Simulación de nuevas 
# estructuras de correlación
# dos grupos alta y positivamente correlacionados 
# por dentro
#---------------------------
library(MASS)
# Definir el número de observaciones
n <- 100

# Definir el vector de medias
mean_vec <- rep(0, 4)  # Todos los valores medios son 0

#---------#
# GRUPO 1 #
#---------#
# Definir la matriz de correlaciones
cor_mat1 <- matrix(c(
  1, .976, .953, .957,
  .976, 1 , .931, .881,
  .953, .931, 1, .912,
  .957, .881, .912, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat1) #Lo es

# Calcular la matriz de varianzas
variances <- diag(cor_mat1)  # Obtener las varianzas de la diagonal de la matriz de correlación
D <- diag(sqrt(variances))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas

# Calcular la matriz de covarianza
cov_mat1 <- D %*% cor_mat1 %*% D

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo1 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat1, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo1 <- as.data.frame(grupo1)

# Verificar la correlación entre las variables
cor(grupo1)
summary(grupo1)
cov(grupo1)

#---------#
# GRUPO 2 #
#---------#
# Definir la matriz de correlaciones
cor_mat2 <- matrix(c(
  1, .856, .796, .935,
  .856, 1 , .921, .9,
  .796, .921, 1, .879,
  .935, .9, .879, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat2) #Lo es

# Calcular la matriz de varianzas
variances2 <- diag(cor_mat2)  # Obtener las varianzas de la diagonal de la matriz de correlación
D2 <- diag(sqrt(variances2))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas
#Se está asumienod que todas las variables tienen desvio estándar = 1

# Calcular la matriz de covarianza
cov_mat2 <- D2 %*% cor_mat2 %*% D2 #desvios por la matriz de correlacion = matriz de var y covar

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo2 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat2, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo2 <- as.data.frame(grupo2)

# Verificar la correlación entre las variables
cor(grupo2)

datos_simulados <- rbind.data.frame(grupo1, grupo2)
datos_simulados$flia <- c(rep('grupo1',100), rep('grupo2',100))

library(FactoMineR)
DMFA(datos_simulados, num.fact =5, scale.unit = FALSE)
b <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(1, 2)) #ejes 1 y 2
d <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(3,4)) #ejes 3 y 4




#---------------------------
# Simulación de nuevas 
# estructuras de correlación
# un grupos alta y negativamente correlacionado 
# por dentro y otro grupo no correlacionado
#---------------------------
library(MASS)
# Definir el número de observaciones
n <- 100

# Definir el vector de medias
mean_vec <- rep(0, 4)  # Todos los valores medios son 0

#---------#
# GRUPO 1 #
#---------#
# Definir la matriz de correlaciones
cor_mat1 <- matrix(c(
  1, -0.5, -0.3, -0.4,
  -0.5, 1, -0.2, -0.3,
  -0.3, -0.2, 1, -0.1,
  -0.4, -0.3, -0.1, 1
), nrow = 4, ncol = 4)


#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat1) #Lo es
matrixcalc::is.positive.semi.definite(cor_mat1)

# Calcular la matriz de varianzas
variances <- diag(cor_mat1)  # Obtener las varianzas de la diagonal de la matriz de correlación
D <- diag(sqrt(variances))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas

# Calcular la matriz de covarianza
cov_mat1 <- D %*% cor_mat1 %*% D

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo1 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat1, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo1 <- as.data.frame(grupo1)

# Verificar la correlación entre las variables
cor(grupo1)
summary(grupo1)
cov(grupo1)

#---------#
# GRUPO 2 #
#---------#
# Definir la matriz de correlaciones
cor_mat2 <- matrix(c(
  1, .056, -.015, .012,
  .056, 1 , -.088, .116,
  -.015, -.088, 1, -.130,
  .012, .116, -.130, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat2) #Lo es

# Calcular la matriz de varianzas
variances2 <- diag(cor_mat2)  # Obtener las varianzas de la diagonal de la matriz de correlación
D2 <- diag(sqrt(variances2))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas
#Se está asumienod que todas las variables tienen desvio estándar = 1

# Calcular la matriz de covarianza
cov_mat2 <- D2 %*% cor_mat2 %*% D2 #desvios por la matriz de correlacion = matriz de var y covar

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo2 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat2, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo2 <- as.data.frame(grupo2)

# Verificar la correlación entre las variables
cor(grupo2)

datos_simulados <- rbind.data.frame(grupo1, grupo2)
datos_simulados$flia <- c(rep('grupo1',100), rep('grupo2',100))

library(FactoMineR)
DMFA(datos_simulados, num.fact =5, scale.unit = FALSE)
b <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(1, 2)) #ejes 1 y 2
d <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(3,4)) #ejes 3 y 4



#---------------------------
# CONDICION 6
# Simulación de nuevas 
# estructuras de correlación
# un grupos moderada y negativamente correlacionado 
# por dentro y otro grupo alta y positivamente correlacionado
#---------------------------
library(MASS)
# Definir el número de observaciones
n <- 100

# Definir el vector de medias
mean_vec <- rep(0, 4)  # Todos los valores medios son 0

#---------#
# GRUPO 1 #
#---------#
# Definir la matriz de correlaciones
cor_mat1 <- matrix(c(
  1, -0.5, -0.3, -0.4,
  -0.5, 1, -0.2, -0.3,
  -0.3, -0.2, 1, -0.1,
  -0.4, -0.3, -0.1, 1
), nrow = 4, ncol = 4)


#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat1) #Lo es
matrixcalc::is.positive.semi.definite(cor_mat1)

# Calcular la matriz de varianzas
variances <- diag(cor_mat1)  # Obtener las varianzas de la diagonal de la matriz de correlación
D <- diag(sqrt(variances))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas

# Calcular la matriz de covarianza
cov_mat1 <- D %*% cor_mat1 %*% D

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo1 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat1, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo1 <- as.data.frame(grupo1)

# Verificar la correlación entre las variables
cor(grupo1)
summary(grupo1)
cov(grupo1)

#---------#
# GRUPO 2 #
#---------#
# Definir la matriz de correlaciones
cor_mat2 <- matrix(c(
  1, .856, .796, .935,
  .856, 1 , .921, .9,
  .796, .921, 1, .879,
  .935, .9, .879, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat2) #Lo es

# Calcular la matriz de varianzas
variances2 <- diag(cor_mat2)  # Obtener las varianzas de la diagonal de la matriz de correlación
D2 <- diag(sqrt(variances2))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas
#Se está asumienod que todas las variables tienen desvio estándar = 1

# Calcular la matriz de covarianza
cov_mat2 <- D2 %*% cor_mat2 %*% D2 #desvios por la matriz de correlacion = matriz de var y covar

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo2 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat2, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo2 <- as.data.frame(grupo2)

# Verificar la correlación entre las variables
cor(grupo2)

datos_simulados <- rbind.data.frame(grupo1, grupo2)
datos_simulados$flia <- c(rep('grupo1',100), rep('grupo2',100))

library(FactoMineR)
DMFA(datos_simulados, num.fact =5, scale.unit = FALSE)
b <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(1, 2)) #ejes 1 y 2
d <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(3,4)) #ejes 3 y 4



#---------------------------
# CONDICION 8
# Simulación de nuevas 
# estructuras de correlación
# dos grupos moderada y negativamente correlacionados 
#---------------------------
library(MASS)
# Definir el número de observaciones
n <- 100

# Definir el vector de medias
mean_vec <- rep(0, 4)  # Todos los valores medios son 0

#---------#
# GRUPO 1 #
#---------#
# Definir la matriz de correlaciones
cor_mat1 <- matrix(c(
  1, .81, -0.82, -.80,
  .81, 1, -.91, -0.80,
  -0.82, -.91, 1, .86,
  -.80, -0.80, .86, 1
), nrow = 4, ncol = 4)


#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat1) #Lo es
matrixcalc::is.positive.semi.definite(cor_mat1)

# Calcular la matriz de varianzas
variances <- diag(cor_mat1)  # Obtener las varianzas de la diagonal de la matriz de correlación
D <- diag(sqrt(variances))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas

# Calcular la matriz de covarianza
cov_mat1 <- D %*% cor_mat1 %*% D

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo1 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat1, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo1 <- as.data.frame(grupo1)

# Verificar la correlación entre las variables
cor(grupo1)
summary(grupo1)
cov(grupo1)

#---------#
# GRUPO 2 #
#---------#
# Definir la matriz de correlaciones
cor_mat2 <- matrix(c(
  1, .81, -0.83, -.79,
  .81, 1, -.901, -0.83,
  -0.83, -.901, 1, .845,
  -.79, -0.83, .845, 1
), nrow = 4, ncol = 4)

#Verificar si la matriz de correlaciones es positiva
matrixcalc::is.positive.definite(cor_mat2) #Lo es

# Calcular la matriz de varianzas
variances2 <- diag(cor_mat2)  # Obtener las varianzas de la diagonal de la matriz de correlación
D2 <- diag(sqrt(variances2))  # Crear la matriz diagonal D con las raíces cuadradas de las varianzas
#Se está asumienod que todas las variables tienen desvio estándar = 1

# Calcular la matriz de covarianza
cov_mat2 <- D2 %*% cor_mat2 %*% D2 #desvios por la matriz de correlacion = matriz de var y covar

# Generar datos multivariados con mvrnorm
set.seed(123)  # Para reproducibilidad
grupo2 <- mvrnorm(n = n, mu = mean_vec, Sigma = cov_mat2, empirical = FALSE)

# Convertir los datos generados en un data frame
grupo2 <- as.data.frame(grupo2)

# Verificar la correlación entre las variables
cor(grupo2)

datos_simulados <- rbind.data.frame(grupo1, grupo2)
datos_simulados$flia <- c(rep('grupo1',100), rep('grupo2',100))

library(FactoMineR)
DMFA(datos_simulados, num.fact =5, scale.unit = FALSE)
b <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(1, 2)) #ejes 1 y 2
d <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(3,4)) #ejes 3 y 4



#-------------------------------
# Sintaxis necesaria para el caso
# en donde tengo datos simulados y ya apliqué el AFMD
#--------------------------------
b <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(1, 2)) #ejes 1 y 2
d <- DMFA(datos_simulados, num.fact =5, scale.unit = TRUE, axes = c(3,4)) #ejes 3 y 4

d$eig
b$eig
b$var
var_dim1 <- round(b$eig[1,2],2)
var_dim2 <- round(b$eig[2,2],2)
var_dim3 <- round(b$eig[3,2],2)
var_dim4 <- round(b$eig[4,2],2)

#Biplot para cada grupo
plot(b, choix = "var",
     label="all",
     xlim = NULL, ylim= NULL,
     title = NULL,
     cex = 0.7,
     cex.main = 0.8,
     cex.lab=1)

#Creo dataframes con las coordenadas de las variables parciales y los nombres de las variables
vparc_g1 <- as.data.frame(b$var.partiel$grupo1[,1:4]) %>%
  rownames_to_column("label")
vparc_g2 <- as.data.frame(b$var.partiel$grupo2[,1:4]) %>%
  rownames_to_column("label")

#Creo un dataframe con las coordenadas de las variables consenso
vconsenso <- as.data.frame(b$var$coord[,1:4])%>%
  rownames_to_column("label")


#Gráfico con solo los puntos de las variables parciales del grupo 1
origin <- rep(0, nrow(vparc_g1))
dd <- cbind.data.frame(vparc_g1, xstart = origin, ystart = origin, stringsAsFactors = TRUE)
theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
circle <- data.frame(xcircle = cos(theta), ycircle = sin(theta), stringsAsFactors = TRUE)

ggplot(vparc_g1, aes(Dim.1, Dim.2)) +
  #geom_point() +
  #geom_text(aes(label = label), 
  #          hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 1 (", var_dim1, "%)"),
       y = paste0("Dim 2 (", var_dim2, "%)"),
       title = "Gráfico de variables parciales del grupo 1") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vparc_g1$Dim.1, yend=vparc_g1$Dim.2),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw() +
  geom_text_repel(aes(label = label))

ggplot(vparc_g1, aes(Dim.3, Dim.4)) +
  #geom_point() +
  #geom_text(aes(label = label), 
  #          hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 3 (", var_dim3, "%)"),
       y = paste0("Dim 4 (", var_dim4, "%)"),
       title = "Gráfico de variables parciales del grupo 1") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vparc_g1$Dim.3, yend=vparc_g1$Dim.4),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw() +
  geom_text_repel(aes(label = label))

#Gráfico con solo los puntos de las variables parciales del grupo 2
origin <- rep(0, nrow(vparc_g2))
dd <- cbind.data.frame(vparc_g2, xstart = origin, ystart = origin, stringsAsFactors = TRUE)

ggplot(vparc_g2, aes(Dim.1, Dim.2)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vparc_F2_1x8,
  #          hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 1 (", var_dim1, "%)"), y = paste0("Dim 2 (", var_dim2, "%)"), title = "Gráfico de variables parciales del grupo 2") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vparc_g2$Dim.1, yend=vparc_g2$Dim.2),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw() +
  geom_text_repel(aes(label = label))
ggplot(vparc_g2, aes(Dim.3, Dim.4)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vparc_F2_1x8,
  #          hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 3 (", var_dim3, "%)"), y = paste0("Dim 4 (", var_dim4, "%)"), title = "Gráfico de variables parciales del grupo 2") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vparc_g2$Dim.3, yend=vparc_g2$Dim.4),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw() +
  geom_text_repel(aes(label = label))

ggplot(vconsenso, aes(Dim.1, Dim.2)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vconsenso,
  #          #hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 1 (", var_dim1, "%)"), y = paste0("Dim 2 (", var_dim2, "%)"), title = "Gráfico de variables consenso") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vconsenso$Dim.1, yend=vconsenso$Dim.2),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw()+
  geom_label_repel(aes(label = label))

ggplot(vconsenso, aes(Dim.3, Dim.4)) +
  #geom_point() +
  #geom_text(aes(label = label), data = vconsenso,
  #          #hjust="left", vjust="right",
  #          size=3.5)+
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(x= paste0("Dim 3 (", var_dim3, "%)"), y = paste0("Dim 4 (", var_dim4, "%)"), title = "Gráfico de variables consenso") +
  #Le sumamos el circulo
  geom_path(mapping = aes(xcircle, ycircle), data = circle, color = "black") +
  coord_fixed()+
  #Le sumamos las flechas
  geom_segment(aes(x=xstart, y=ystart, xend=vconsenso$Dim.3, yend=vconsenso$Dim.4),
               data = dd , arrow = arrow(length=unit(0.3, 'cm')),
               color = '#000000', lwd = .5)+
  #Le sumamos un tema
  theme_bw()+
  geom_label_repel(aes(label = label))
















#----------------------------------------------
# Sintaxis necesaria para aplicar
# APC sobre todo el conjunto de datos simulados
#----------------------------------------------
datos_para_pca <- datos_simulados %>%
  dplyr::select(-c("flia"))

res.PCA.s <- PCA(datos_para_pca,
                 scale.unit = FALSE,
                 #ncp = 5,
                 #quali.sup = 11, #no funciona bien
                 axes = c(1,2))
res.PCA.s$eig
res.PCA.s$var

#Defino variables en las que conservaré el porcentaje de variancia explicada
#por cada dimensión del ACP
var_dim1_s <- round(res.PCA.s$eig[1,2],2)
var_dim2_s <- round(res.PCA.s$eig[2,2],2)
var_dim3_s <- round(res.PCA.s$eig[3,2],2)
var_dim4_s <- round(res.PCA.s$eig[4,2],2)


#Grafico de individuos (scores)
#Creo un data frame con las coordenadas de los individuos en los ejes de las CPs
coordenadas_ind <- as.data.frame(res.PCA.s$ind$coord) 

#Agrego la variable cualitativa (flia de tomate)
familia <- datos_simulados %>% select(c("flia"))
coordenadas_ind <- cbind.data.frame(coordenadas_ind,familia)

# CP1 vs CP2
coordenadas_ind %>%
  ggplot(aes(x = `Dim.1`, y = `Dim.2`, col = flia))+
  geom_point() + #puntos
  geom_vline(xintercept = 0)+ #eje y
  geom_hline(yintercept=0)+ #eje x
  labs(title = "CP 1 vs CP 2", 
       x = paste0("Dim 1 (", var_dim1_s, "%)"), 
       y = paste0("Dim 2 (", var_dim2_s, "%)"))+
  theme_bw()

# CP3 vs CP4
coordenadas_ind %>%
  ggplot(aes(x = `Dim.3`, y = `Dim.4`, col = flia))+
  geom_point() +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept=0)+
  labs(title = "CP3 vs CP4",  
       x = paste0("Dim 3 (", var_dim3_s, "%)"), 
       y = paste0("Dim 4 (", var_dim4_s, "%)"))+
  theme_bw()


#-------------------------------------------
# Sintaxis necesaria para aplicar
# APC sobre cada grupo del conjunto simulado
#-------------------------------------------
grupo1_pca <- datos_simulados %>%
  dplyr::filter(flia == "grupo1") %>%
  dplyr::select(-c("flia"))

res.PCA.s1 <- PCA(grupo1_pca,
                  scale.unit = FALSE,
                  #ncp = 5,
                  #quali.sup = 11, #no funciona bien
                  axes = c(3,4))
res.PCA.s1$eig
res.PCA.s1$var

grupo2_pca <- datos_simulados %>%
  dplyr::filter(flia == "grupo2") %>%
  dplyr::select(-c("flia"))

res.PCA.s2 <- PCA(grupo2_pca,
                  scale.unit = FALSE,
                  #ncp = 5,
                  #quali.sup = 11, #no funciona bien
                  axes = c(3,4))
res.PCA.s2$eig
res.PCA.s2$var


#-----------------------------------------
# Aplicación del ADL sobre datos simulados
#-----------------------------------------
str(datos_simulados)

## create the lda model
modelo_lda_simulados <- lda(formula = flia ~ ., data = datos_simulados)

## get the x,y coordinates for the LDA plot
simulados_lda_valores <- predict(modelo_lda_simulados)

## create a dataframe that has all the info we need to draw a graph
datos_sim <- data.frame(X=simulados_lda_valores$x[,1],
                        Y=rep(0,length(simulados_lda_valores$x[,1])),
                        Familia=datos_simulados$flia #aqui estoy creando una columna llamada `Familia` que contiene a la columna flia del dataframe union_adl
)

ggplot(data=datos_sim, aes(x=X, y= Y)) +
  geom_point(aes(color=Familia)) +
  theme_bw()
#Tabla de las contribuciones de cada variable a cada uno de los ejes
variables_simulados_lda <- as.data.frame(modelo_lda_simulados$scaling)%>%
  rownames_to_column("variable")%>%
  mutate(LD1 = round(LD1,2),
         LD2 = round(LD2,2))
str(variables_simulados_lda)


