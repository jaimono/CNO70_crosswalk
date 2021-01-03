# =============================================================== #
# Title:                Cleaning CAPITULO H - Uso de tiempo e ingresos
# Author:               Jaime Montana
# Date:                 2-1-2021
# Description:          Extracts the infromation from the PDF (ISCO88_AC, CNO70_5D, ISCO88_4D, SENA2003_4D)
# =============================================================== #

library(stringr)
library(tidyr)
library(pdfsearch)

### Load functions required

na.pad <- function(x,len){
        x[1:len]
}

makePaddedDataFrame <- function(l,...){
        maxlen <- max(sapply(l,length))
        data.frame(lapply(l,na.pad,len=maxlen),...)
}


#### Read data from PDF searching the keywords.
# Note the loop is over the 19 pages of index of ocupations, taking the 4 digits.

data <- keyword_search("01_raw/CIUO-88AC.pdf",
                       keyword = c('Descripción'),
                       path = TRUE,
                       surround_lines = FALSE)

CIUO88AC <- c()

for(i in 1:19){
        extract <- str_extract_all(data$line_text[i],"[ ]([0-9]{4})[ ]")[[1]]        
        CIUO88AC <- c(CIUO88AC,extract)
        
}

#### Read data from PDF searching the keywords. "Correlativa: CIUO88OIT:"
# Note if I search only  "Correlativa: the terms are 401, which coincide with the
# extracted in the index at the beginning of the document.

data <- keyword_search("01_raw/CIUO-88AC.pdf",
                       keyword = c('Correlativa:'),
                       path = TRUE,
                       surround_lines = FALSE)

## Remove string in selection

end    <- stringr::str_locate(data$line_text,"Correlativa: CIUO88OIT:")[,2]
data <- substr(data$line_text, end-10, 100000L)

pattern_loc <- stringr::str_locate(data,"Subgrupo")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 1))
pattern_loc <- stringr::str_locate(data,"SUBGRUPO")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 1))
pattern_loc <- stringr::str_locate(data,"SUBGRUPO")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 1))
pattern_loc <- stringr::str_locate(data,"  CIUO-88 A.C. ")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 4))
# remove "Directores"
pattern_loc <- stringr::str_locate(data,"Directores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Dirigentes"
pattern_loc <- stringr::str_locate(data,"Dirigentes")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Otros"
pattern_loc <- stringr::str_locate(data,"Otros")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Coordinadores"
pattern_loc <- stringr::str_locate(data,"Coordinadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Meteorólogos"
pattern_loc <- stringr::str_locate(data,"Meteorólogos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Químicos"
pattern_loc <- stringr::str_locate(data,"Químicos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Geólogos"
pattern_loc <- stringr::str_locate(data,"Geólogos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Estadísticos"
pattern_loc <- stringr::str_locate(data,"Estadísticos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Ingenieros"
pattern_loc <- stringr::str_locate(data,"Ingenieros")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Arquitectos"
pattern_loc <- stringr::str_locate(data,"Arquitectos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Patólogos"
pattern_loc <- stringr::str_locate(data,"Patólogos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Odontólogos"
pattern_loc <- stringr::str_locate(data,"Odontólogos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Médicos"
pattern_loc <- stringr::str_locate(data,"Médicos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Optómetras"
pattern_loc <- stringr::str_locate(data,"Optómetras")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Fonoaudiólogos"
pattern_loc <- stringr::str_locate(data,"Fonoaudiólogos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Enfermeros"
pattern_loc <- stringr::str_locate(data,"Enfermeros")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Nutricionistas"
pattern_loc <- stringr::str_locate(data,"Nutricionistas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Profesores"
pattern_loc <- stringr::str_locate(data,"Profesores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Profesionales"
pattern_loc <- stringr::str_locate(data,"Profesionales")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Filólogos"
pattern_loc <- stringr::str_locate(data,"Filólogos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Filósofos"
pattern_loc <- stringr::str_locate(data,"Filósofos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Sociólogos"
pattern_loc <- stringr::str_locate(data,"Sociólogos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Bibliotecarios"
pattern_loc <- stringr::str_locate(data,"Bibliotecarios")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Especialistas"
pattern_loc <- stringr::str_locate(data,"Especialistas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Consejeros"
pattern_loc <- stringr::str_locate(data,"Consejeros")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Psicólogos"
pattern_loc <- stringr::str_locate(data,"Psicólogos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Inspectores"
pattern_loc <- stringr::str_locate(data,"Inspectores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Técnicos"
pattern_loc <- stringr::str_locate(data,"Técnicos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Compositores"
pattern_loc <- stringr::str_locate(data,"Compositores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Coreógrafos"
pattern_loc <- stringr::str_locate(data,"Coreógrafos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Publicistas"
pattern_loc <- stringr::str_locate(data,"Publicistas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Electrotécnicos"
pattern_loc <- stringr::str_locate(data,"Electrotécnicos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Delineantes"
pattern_loc <- stringr::str_locate(data,"Delineantes")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Operadores"
pattern_loc <- stringr::str_locate(data,"Operadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Pilotos"
pattern_loc <- stringr::str_locate(data,"Pilotos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Controladores"
pattern_loc <- stringr::str_locate(data,"Controladores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Higienistas"
pattern_loc <- stringr::str_locate(data,"Higienistas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Actores"
pattern_loc <- stringr::str_locate(data,"Actores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Capitanes"
pattern_loc <- stringr::str_locate(data,"Capitanes")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Curanderos"
pattern_loc <- stringr::str_locate(data,"Curanderos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Parteras"
pattern_loc <- stringr::str_locate(data,"Parteras")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Asistentes"
pattern_loc <- stringr::str_locate(data,"Asistentes")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Instructores"
pattern_loc <- stringr::str_locate(data,"Instructores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Agentes"
pattern_loc <- stringr::str_locate(data,"Agentes")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Representantes"
pattern_loc <- stringr::str_locate(data,"Representantes")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Tasadores"
pattern_loc <- stringr::str_locate(data,"Tasadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Funcionarios"
pattern_loc <- stringr::str_locate(data,"Funcionarios")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Músicos"
pattern_loc <- stringr::str_locate(data,"Músicos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Recreadores"
pattern_loc <- stringr::str_locate(data,"Recreadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Secretarios"
pattern_loc <- stringr::str_locate(data,"Secretarios")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Auxiliares"
pattern_loc <- stringr::str_locate(data,"Auxiliares")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Encargados"
pattern_loc <- stringr::str_locate(data,"Encargados")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Empleados"
pattern_loc <- stringr::str_locate(data,"Empleados")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Codificadores"
pattern_loc <- stringr::str_locate(data,"Codificadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Receptores"
pattern_loc <- stringr::str_locate(data,"Receptores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Prestamistas"
pattern_loc <- stringr::str_locate(data,"Prestamistas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Cobradores"
pattern_loc <- stringr::str_locate(data,"Cobradores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Recepcionistas"
pattern_loc <- stringr::str_locate(data,"Recepcionistas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Escribientes"
pattern_loc <- stringr::str_locate(data,"Escribientes")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Atletas"
pattern_loc <- stringr::str_locate(data,"Atletas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Revisores"
pattern_loc <- stringr::str_locate(data,"Revisores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Guías"
pattern_loc <- stringr::str_locate(data,"Guías")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Meseros"
pattern_loc <- stringr::str_locate(data,"Meseros")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Trabajadores"
pattern_loc <- stringr::str_locate(data,"Trabajadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Acompañantes"
pattern_loc <- stringr::str_locate(data,"Acompañantes")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Personal"
pattern_loc <- stringr::str_locate(data,"Personal")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Guardianes"
pattern_loc <- stringr::str_locate(data,"Guardianes")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Avicultores"
pattern_loc <- stringr::str_locate(data,"Avicultores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Criadores"
pattern_loc <- stringr::str_locate(data,"Criadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Pescadores"
pattern_loc <- stringr::str_locate(data,"Pescadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Cazadores"
pattern_loc <- stringr::str_locate(data,"Cazadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Obreros"
pattern_loc <- stringr::str_locate(data,"Obreros")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Tronzadores"
pattern_loc <- stringr::str_locate(data,"Tronzadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Operarios"
pattern_loc <- stringr::str_locate(data,"Operarios")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Instaladores"
pattern_loc <- stringr::str_locate(data,"Instaladores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Electricistas"
pattern_loc <- stringr::str_locate(data,"Electricistas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Fontaneros"
pattern_loc <- stringr::str_locate(data,"Fontaneros")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Cristaleros"
pattern_loc <- stringr::str_locate(data,"Cristaleros")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Vendedores"
pattern_loc <- stringr::str_locate(data,"Vendedores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Carpinteros"
pattern_loc <- stringr::str_locate(data,"Carpinteros")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Pintores"
pattern_loc <- stringr::str_locate(data,"Pintores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Soldadores"
pattern_loc <- stringr::str_locate(data,"Soldadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Montadores"
pattern_loc <- stringr::str_locate(data,"Montadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Chapistas"
pattern_loc <- stringr::str_locate(data,"Chapistas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Aparejadores"
pattern_loc <- stringr::str_locate(data,"Aparejadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Herramentistas"
pattern_loc <- stringr::str_locate(data,"Herramentistas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Pulidores"
pattern_loc <- stringr::str_locate(data,"Pulidores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Mecánicos"
pattern_loc <- stringr::str_locate(data,"Mecánicos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Fabricantes"
pattern_loc <- stringr::str_locate(data,"Fabricantes")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Estereotipistas"
pattern_loc <- stringr::str_locate(data,"Estereotipistas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Grabadores"
pattern_loc <- stringr::str_locate(data,"Grabadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Impresores"
pattern_loc <- stringr::str_locate(data,"Impresores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Sopladores"
pattern_loc <- stringr::str_locate(data,"Sopladores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Grabadores"
pattern_loc <- stringr::str_locate(data,"Grabadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Artesanos"
pattern_loc <- stringr::str_locate(data,"Artesanos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Floristas"
pattern_loc <- stringr::str_locate(data,"Floristas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Panaderos"
pattern_loc <- stringr::str_locate(data,"Panaderos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Preparadores"
pattern_loc <- stringr::str_locate(data,"Preparadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Tejedores"
pattern_loc <- stringr::str_locate(data,"Tejedores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Peleteros"
pattern_loc <- stringr::str_locate(data,"Peleteros")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Tapiceros"
pattern_loc <- stringr::str_locate(data,"Tapiceros")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Sastres"
pattern_loc <- stringr::str_locate(data,"Sastres")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Catadores"
pattern_loc <- stringr::str_locate(data,"Catadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Bordadores"
pattern_loc <- stringr::str_locate(data,"Bordadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Zapateros"
pattern_loc <- stringr::str_locate(data,"Zapateros")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Ebanistas"
pattern_loc <- stringr::str_locate(data,"Ebanistas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Ajustadores"
pattern_loc <- stringr::str_locate(data,"Ajustadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Perforadores"
pattern_loc <- stringr::str_locate(data,"Perforadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Patronistas"
pattern_loc <- stringr::str_locate(data,"Patronistas")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Ensambladores"
pattern_loc <- stringr::str_locate(data,"Ensambladores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Guardafrenos"
pattern_loc <- stringr::str_locate(data,"Guardafrenos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Conductores"
pattern_loc <- stringr::str_locate(data,"Conductores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Ayudantes"
pattern_loc <- stringr::str_locate(data,"Ayudantes")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Porteros"
pattern_loc <- stringr::str_locate(data,"Porteros")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Vigilantes"
pattern_loc <- stringr::str_locate(data,"Vigilantes")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Recolectores"
pattern_loc <- stringr::str_locate(data,"Recolectores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Barrenderos"
pattern_loc <- stringr::str_locate(data,"Barrenderos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Lectores"
pattern_loc <- stringr::str_locate(data,"Lectores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Lavanderos"
pattern_loc <- stringr::str_locate(data,"Lavanderos")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "Lavadores"
pattern_loc <- stringr::str_locate(data,"Lavadores")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))
# remove "4 Cultivo permanente"
pattern_loc <- stringr::str_locate(data,"4 Cultivo permanente")[,1]
data <- ifelse(is.na(pattern_loc),data,substr(data, 1, pattern_loc - 6))

pattern_loc <- stringr::str_locate(data,"SENA2003: ")[,2]
# Create vector  with the matches for CIUO88
CIUO88OIT <- substr(data,pattern_loc+1,pattern_loc+4)
data <- substr(data,pattern_loc+5,1000L)

## Organize the data in a DF

# Create empty DF
correspondencia <- makePaddedDataFrame(list(CIUO88_AC  =  "<NA>",
                                            CIUO88OIT  =  "<NA>",
                                            CNO70      =  "<NA>",
                                            SENA2003   =  "<NA>"))
correspondencia <-correspondencia[-1,]

size <- length(data)

for (i in 1:size) {
        ocup <-         makePaddedDataFrame(list(CIUO88_AC          =     CIUO88AC[i],
                                                 CIUO88OIT          =     ifelse(CIUO88OIT[i]=="xxxx","<NA>",CIUO88OIT[i]),
                                                 CNO70              =     str_extract_all(data[i],"[ ]([0-9]{5})[ ]")[[1]],
                                                 SENA2003           =     str_extract_all(data[i],"[ ]([0-9]{4})[ ]")[[1]])) %>% 
                fill(CIUO88_AC)
        
        correspondencia <- rbind(correspondencia, ocup)
        
}

correspondencia <- correspondencia %>% filter_all(any_vars(!is.na(.)))


saveRDS(correspondencia, "02_data/extractPDF.rds")

rm(list = ls())


