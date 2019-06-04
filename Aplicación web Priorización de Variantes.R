if (!require('shiny'))
  install.packages('shiny')
library("shiny")
if (!require('dplyr'))
  install.packages('dplyr')
library("dplyr")
if (!require('vcfR'))
  install.packages('vcfR')
library("vcfR")
if (!require('DT'))
  install.packages('DT')
library("DT")
options(shiny.maxRequestSize = 7168*1024^2) #Tamaño maximo archivo = 7 gb
ui <- fluidPage(
   
   titlePanel("Aplicación web para la priorización de variantes"),
   
   tabsetPanel(
     tabPanel(
       title = "Subida de archivos",
       fluidRow(
         h3("Sube tus archivos a la categoría correspondiente"),
         p("En ambas categorías, el archivo debe ser tipo *.VCF y como máximo debe tener un tamaño de 7 Gb")
       ),
       fluidRow(
         column(6,
       h4("Indice"),
       fileInput("indice",label = "Fichero para caso Indice", multiple = F),
       p("Solo se admite un único fichero"),
       br(),
       textOutput("proc")
       ),
       
       column(3,
       h4("Trio"),
       fileInput("padre", "Fichero de Trio - padre", multiple = F),
       fileInput("madre", "Fichero de Trio - madre", multiple = F),
       fileInput("hijo", "Fichero de Trio - hijo", multiple = F),
       p("Se debe aportar el fichero correspondiente en cada cuadro")),
       column(3,
              br(), br(), br(),
       selectInput("herencia",label = "Selecciona modelo de herencia",
                   choices = c("Autosómica dominante"= "ad","Autosómica recesiva"="ar",
                      "Ligada a X dominante"= "xd", "Ligada a X recesiva" = "xr")),
       selectInput("origen", "Selecciona de donde viene la variante",
                   choices = c("De novo" = "dn", "Heredada" = "her")),
       checkboxGroupInput("het", label = "Selecciona tipo de heterocigoto, si procede",
                          choiceNames = c("Heterocigoto", "Heterocigoto compuesto"),
                          choiceValues = c("h", "hc")),
       h5("Si se selecciona herencia ligada a X, es necesario 
          especificar el sexo del hijo."),
       selectInput("sex", label = "Selecciona el sexo",
                   choices = c("Masculino" = "M", "Femenino" = "F")),
       textOutput("proc_her")
       ),
       
       fluidRow(
       column(10,
       p("Cuando se hayan subido los archivos ('Uploaded complete') 
         y completado el formulario si es un caso familiar/trio, 
         clicar en el botón 'Procesar archivos'"),
       br(),       
       actionButton("lectura","Procesar archivos"),
       br()
         )
       )
     )
     ),
     

     tabPanel(
       title = "Priorización",
      fluidRow(
         column(3,
       #Nombre del gen
       h4("Nombre del gen afectado:"),
       textInput("nombre_gen", label = "Introduce el nombre del gen", 
                 value =  ""
       )),
       
       #Proteína
       column(3,
       h4("Proteína afectada:"),
       textInput("uniprot", label = "Introduce nombre de proteína (Uniprot)", 
                   value = ""
       )),
       
       #Impacto en la proteína
       column(3,
       h4("Impacto en la proteína"),
       selectInput("impacto", label = "Selecciona el impacto en la proteína",
                   choices = c( "Todos"="all","Bajo"="LOWER", "Moderado"= "MODERATE", 
                                "Alto" ="HIGH","Modificadora"= "MODIFIER"),
                   selected = "all"
       )),
       
       #Tipo de variante
       column(3,
       h4("Tipo de variante:"),
       selectInput("tipo_variante",label = "Selecciona tipo de variante",
                   choices = c("Todos" = "all" , "SNV" = "snv","INDEL" = "indel",
                               "Multi-SNV"= "multi-snv", "Multi-INDEL" = "multi-indel",
                               "Mezcla"= "mixed"),
                   selected = "all", multiple = FALSE
       )),
       
       #Genotipo
       column(3,
       h4("Genotipo"),
       textInput("genot",label="Introduce genotipo", 
                   value =""
       )),
       
      #Enfermedad asociada
      column(3,
      h4("Enfermedad asociada"),
      textInput("enfermedad", label = "Escribe una enfermedad",
                   value = ""
       )),
       
      #Efecto clínico
      column(3,
      h4("Efecto clínico"),
      selectInput("efecto", label = "Introduce el efecto buscado",
                  choices = c("Todos"= "all","Significado incierto" = "Uncertain significance" ,
                           "No proporcionado" = "not provided","Benigno" = "Benign",
                           "Probablemente benigno" = "Likely benign", 
                           "Problemente maligno" = "Likely malignant", 
                           "Maligno" = "Malignant"),
                 selected = "all"
      ))
      ),
     fluidRow(  
      #F. europea
      column(3,
      h4("Frecuencia europea"),
      h5("Introduce un valor para el filtrado"),
      selectInput("comp_f.eu", label="Selecciona signo",
                 choices = c("Igual"= "=", "Mayor o igual" = ">=", "Menor o igual" = "<="),
                 selected = "="
      ),
      numericInput("f.eu", label = "Introduce un valor numérico",value = 0, min = 0, max = 1, step = 0.01
      ),
        
      h5("Introduce un intervalo"),
      numericInput("f.eu_inf", label = "Introduce límite inferior",value = 0, min = 0, max = 1, step = 0.01
      ),
      numericInput("f.eu_sup", label = "Introduce límite superior",value = 0, min = 0, max = 1, step = 0.01
      )),
       
      #Qual
      column(3,
      h4("QUAL:"),
      h5("Introduce un valor para el filtrado"),
      selectInput("compQUAL", label="Selecciona signo",
                  choices = c("Igual"= "=", "Mayor o igual" = ">=", "Menor o igual" = "<="),
                  selected = "="
      ),
      numericInput("QUAL", label = "Introduce un valor numérico",value = 0, step = 5, min= 0
      ),
      h5("Introduce un intervalo"),
      numericInput("QUAL_inf", label = "Introduce límite inferior",value = 0, step = 5, min= 0),
      numericInput("QUAL_sup", label = "Introduce límite superior",value = 0, step = 5, min= 0)
      ),
      
      #DP
      column(3,
             h4("DP:"),
             h5("Introduce un valor para el filtrado"),
             selectInput("compDP", label="Selecciona signo",
                         choices = c("Igual"= "=", "Mayor o igual" = ">=", "Menor o igual" = "<="),
                         selected = "="
             ),
             numericInput("DP", label = "Introduce un valor numérico",value = 0, step = 5, min= 0
             ),
             h5("Introduce un intervalo"),
             numericInput("DP_inf", label = "Introduce límite inferior",value = 0, step = 5, min= 0),
             numericInput("DP_sup", label = "Introduce límite superior",value = 0, step = 5, min= 0)
      ),
    #Descarga criterios de busqueda
    column(3,
           downloadButton("prior",label = "Guardar parámetros"),
           br(), br(),
    #Ejecutar priorización
           actionButton("run", label= "Priorización de variantes")
           )
     ),
    fluidRow(
      h4(strong("Información sobre los parámetros de priorización: ")),
      p("A continuación se explican los parámetros disponibles para el 
        filtrado de las variantes"),
      p(strong("Nombre del gen:"),"Se corresponde con la columna GENEINFO
        de la tabla de resultados. En este campo se debe introducir el nombre de un gen o 
        una lista de nombres de genes separados por comas (,)"),
      p(strong("Proteína afectada:"),"Se corresponde con la columna db_NSFP_Uniprot_acc
        de la tabla de resultados. En este campo  se debe introducir el nombre en uniprot 
        de la proteína. "),
      p(strong("Impacto en la proteína:"), "Se corresponde con el tercer valor de la 
        columna vep. Indica que tipo de cambio produce en la proteína final"),
      p(strong("Tipo de variante:"), "Se corresponde con la columna variant_type. Indica
        qué tipo de variante es."),
      p(strong("Genotipo:"),"Correpondiente con el primer campo de la columna gt. Indica
        el modo de herencia de la variante. Ejemplo: 0/1"),
      p("Los alelos aparecen codificados como números, donde 0 es el alelo Wild-type / salvaje 
        / referencia. El separador puede ser: '/' para genotipo no en fase (genotype unphased) 
        y '|' para genotipo en fase (genotype phased)."),
      p(strong("Enfermedad asociada:"), "Se corresponde con la columna CLNDN. En este campo se
        debe introducir el nombre de la enfermedad separado por '_'. "),
      p(strong("Efecto clínico:"), "Se corresponde con la columna CLNSIG. Indica si la 
        variante es beninga o maligna."),
      p(strong("Frecuencia europea:"), "Se corresponde con la columna AF_nfe. Indica la 
        frecuencia de la variante en la población europea. El rango de valores varía entre
        0 y 1."),
      p(strong("QUAL:"),"Se correponde con la columna de igual nombre. Indica la calidad.
        Hay que introducir valores númericos"),
      p(strong("DP:"),"Se corresponde con el segundo valor de la columna gt. Indica la 
        la profundidad de lectura (deep read)")
    )
     
    ),
    tabPanel(
      title = "Resultados",
      br(),
      #Descarga resultados
      downloadButton("desc_res_indice", label = "Guardar resultado (INDICE)"),
      downloadButton("desc_res_m", label = "Guardar resultado (MADRE)"),
      downloadButton("desc_res_p", label = "Guardar resultado (PADRE)"),
      downloadButton("desc_res_h", label = "Guardar resultado (HIJO)"),
      br(), br(),
      #Tablas Resultados
      textOutput("res_i"),
      br(),
      dataTableOutput("resultado.indice"),
      br(),
      textOutput("res_m"),
      br(),
      dataTableOutput("resultado.madre"),
      br(),
      textOutput("res_p"),
      br(),
      dataTableOutput("resultado.padre"),
      br(),
      textOutput("res_h"),
      br(),
      dataTableOutput("resultado.hijo"),
      br(),
      
      h4(strong("Explicación del significado de las columnas mostradas los resultados")),
      p(strong("CHROM:"), "se indica el cromosoma donde se encuentra la variante."),
      p(strong("POS:"), "indica la posición dentro del cromosoma."),
      p(strong("ID:"), "indica el identificador de la variante (código rs{número})"),
      p(strong("REF:"), "contiene la base o bases que se encuentran en esa posición originalmente."),
      p(strong("ALT:"), "indica por que base o bases se han cambiado las originales."),
      p(strong("QUAL:"), "es un número que es una medida de la calidad asociada con la inferencia de los alelos."),
      p(strong("Genotype:"), "indica el genotipo (ver definición en la pestaña de Priorización"),
      p(strong("Transcript:"),"indica el código del transcrito"),
      p(strong("Impact:"),"indica el impacto sobre la proteína final. Puede ser: alto (high), moderado (moderate), bajo (low)
        o modificador (modifier)"),
      p(strong("Consecuence:"),"hace referencia a que tipo de consecuencia tiene para la proteína"),
      p(strong("Variant_type:"),"indica que tipo de variante es. Puede ser: SNV, INDEL, multi-SNV, multi-INDEL o mezcla (mixed)"),
      p(strong("cDNA:"),"especifica la posición relativa dentro de la hebra complementaria de ADN"),
      p(strong("Exon:"),"especifica en que exón. El formato de este campo es: número de exon afectado/número total de exones en el gen"),
      p(strong("Intron:"),"especifica en que intrón El formato de este campo es: número de intrón afectado/número total de intrones en el gen"),
      p(strong("AF_nfe:"),"indica la frecuencia de la variante en la población europea"),
      p(strong("dbNSFP_Uniprot_acc:"),"indica el nombre de la entrada de la proteína en la página Uniprot."),
      p(strong("CLNSIG:"),"es un indicador del significado clínico de la variante, en otras palabras, si la variante es benigna o maligna"),
      p(strong("CLNDN:"),"especifica con que enfermedad está asociada la variante"),
      p(strong("CLNDISBD:"),"Indica el código de la enfermedad asociada para diferentes bases de datos clínicas, como por ejemplo MedGen"),
      p(strong("n_alt_alleles:"),"especifica cuantos alelos alternativos existen."),
      p(strong("dbNSFP_MutationTaster_pred:"),"contiene la predicción del efecto de la variante mediante el algoritmo MutationTaster. Link al artículo:
         https://www.nature.com/articles/nmeth.2890"),
      p(strong("dbNSFP_Polyphen2_HDIV_pred:"),"muestra la predicción del posible impacto de la sustitución de aminoácidos en la estrutura y 
        función de las proteínas humanas. En esta versión se compiló usando los efectos dañinos conocidos sobre la función molecular que 
        causan enfermedades medelianas. Link: http://genetics.bwh.harvard.edu/pph2/dokuwiki/overview"),
      p(strong("dbNSFP_Polyphen2_HVAR_pred:"),"muestra la predicción del posible impacto de la sustitución de aminoácidos en la estrutura y 
        función de las proteínas humanas. En este caso, se emplearon todas las mutaciones causantes de enfermedades en el humano presentes en Uniprot, y 
        todos los Polimorfismos de Nucleotido Único (SNPs) no asociados a enfermedades, los cuales se trataron como no dañinos.
        Link: http://genetics.bwh.harvard.edu/pph2/dokuwiki/overview"),
      p(strong("dbNSFP_SIFT_pred:")," usando el algoritmo SIFT se predice como va a afectar una sustitución de aminoácidos a la función de la proteína, basada en la
        homología de la secuencia y las propiedades físicas de los aminoácidos."),
      p(strong("dbNSFP_LRT_pred:"),"Da el resultado de una prueba de verosímilutud para las posiciones de aminoácidos conservados. 
        Link: http://www.genetics.wustl.edu/jflab/lrt_query.html")
    )
   )
)

# Define server logic SERVER
server <- function(input, output){
  #Importación Archivos
observeEvent(input$lectura,{
   #INDICE
   if (is.null(input$indice) == FALSE){
   withProgress(message = "Procesando el archivo...", value = 0, {
   archivo_input<-input$indice
   vcf<-read.vcfR(archivo_input$datapath)
   info_basica<-as.data.frame(vcf@fix[,-8])
   gt<-as.data.frame(vcf@gt[,2])
   names(gt)<-c("gt")
   incProgress(amount = 0.2, detail = "Archivo vcf leído. Leyendo anotaciones")
   info_basica<-info_basica[,1:7]
   anot<-extract_info_tidy(vcf, info_types = F)
   cat("Tabla info creada", "\n")
   incProgress(0.6, detail = "Anotaciones leídas. Creando tabla para la priorización")
   datos<-cbind(info_basica,gt,anot[,-1])
   datos$gt<- as.character(datos$gt)
   genotype<-character()
   dp<- numeric()
   for (i in 1:nrow(datos)){
     gt_split<-strsplit(datos$gt[i],":")
     genotype[i]<-gt_split[[1]][1]
     dp[i]<-gt_split[[1]][2]
   }
   datos$genotype<-genotype
   datos$DP<-dp
   cat("vep")
   consecuencia<-character()
   impacto<-character()
   exon<- character()
   intron<- character()
   cDNA<-character()
   for (i in 1:nrow(datos)){
       vep_split<-strsplit(datos$vep[i],"\\|")
       consecuencia[i]<-vep_split[[1]][2]
       impacto[i]<-vep_split[[1]][3]
       exon[i]<-vep_split[[1]][9]
       intron[i]<-vep_split[[1]][10]
   }
   datos$consequence<-consecuencia
   datos$impact<-impacto
   datos$exon<-exon
   datos$intron<-intron
   transcritos<-character()
   gen.nombre<-character()
   for (i in 1:nrow(datos)){
     EFF_split<-strsplit(datos$EFF[i],"\\|")
     transcritos[i]<-EFF_split[[1]][9]
     gen.nombre[i]<-EFF_split[[1]][6]
     cDNA[i]<-EFF_split[[1]][4]
   }
   datos$gene<-gen.nombre
   datos$transcript<- transcritos
   datos$cDNA<- cDNA
   cat("Tabla datos del vcf creada", "\n")
   datos_variantes<-filter(datos, datos$ORIGIN == 1)
   datos_variantes<-select(datos,CHROM, POS, ID, REF, ALT, QUAL, DP, genotype, gene, transcript,impact, consequence, variant_type,cDNA, exon, intron, AF_nfe, dbNSFP_Uniprot_acc, CLNSIG, CLNDN, CLNDISDB,n_alt_alleles, dbNSFP_MutationTaster_pred, dbNSFP_Polyphen2_HDIV_pred,dbNSFP_Polyphen2_HVAR_pred, dbNSFP_SIFT_pred, dbNSFP_LRT_pred)
   datos_variantes$QUAL<-as.numeric(as.character(datos_variantes$QUAL))
   datos_variantes$AF_nfe<-as.numeric(datos_variantes$AF_nfe)
   cat("Tabla con columnas seleccionadas creada", "\n")
   incProgress(0.2, detail = "Tabla creada correctamente")
   })
     output$proc<- renderText("Se ha procesado correctamente el archivo")
   }
  else #Carga trios
    {
     #Trio
     if (is.null(input$padre) == FALSE & 
         is.null(input$padre) == FALSE & 
       is.null(input$hijo) == FALSE)
       {
       withProgress(message = "Procesando los archivos...", value = 0, { 
       #MADRE
       cat("Inicio del procesamiento del archivo Trio - madre","\n")
       archivo_madre<-input$madre
       incProgress(0.01, detail = "Iniciando lectura y procesamiento del archivo Trio-Madre. Leyendo archivo VCF")
       vcf<- read.vcfR(archivo_madre$datapath)
       cat("Archivo vcf correspondiente con madre leido","\n")
       info_basica<-as.data.frame(vcf@fix[,-8])
       gt<-as.data.frame(vcf@gt[,2])
       names(gt)<-c("gt")
       incProgress(0.09, detail = "Archivo madre leído. Extrayendo las anotaciones")
       anot<-extract_info_tidy(vcf, info_types = F)
       cat("Tabla info madre creada","\n")
       incProgress(0.1, detail = "Anotaciones extraídas. Creando tabla para la priorización")
       datos<-cbind(info_basica,gt,anot[,-1])
       datos$gt<- as.character(datos$gt)
       genotype<-character()
       dp<- numeric()
       for (i in 1:nrow(datos)){
         gt_split<-strsplit(datos$gt[i],":")
         genotype[i]<-gt_split[[1]][1]
         dp[i]<-gt_split[[1]][2]
       }
       datos$genotype<-genotype
       datos$DP<-dp
       consecuencia<-character()
       impacto<-character()
       exon<- character()
       intron<- character()
       cDNA<-character()
       for (i in 1:nrow(datos)){
           vep_split<-strsplit(datos$vep[i],"\\|")
           consecuencia[i]<-vep_split[[1]][2]
           impacto[i]<-vep_split[[1]][3]
           exon[i]<-vep_split[[1]][9]
           intron[i]<-vep_split[[1]][10]
       }
       datos$consequence<-consecuencia
       datos$impact<-impacto
       datos$exon<-exon
       datos$intron<-intron
       gen.nombre<-character()
       transcritos<-character()
       gen.nombre<-character()
       for (i in 1:nrow(datos)){
         EFF_split<-strsplit(datos$EFF[i],"\\|")
         transcritos[i]<-EFF_split[[1]][9]
         gen.nombre[i]<-EFF_split[[1]][6]
         cDNA[i]<-EFF_split[[1]][4]
       }
       datos$gene<-gen.nombre
       datos$transcript<- transcritos
       datos$cDNA<- cDNA
       cat("Tabla datos del vcf creada", "\n")
       datos_madre<-filter(datos, datos$ORIGIN == 1)
       datos_madre<-select(datos,CHROM, POS, ID, REF, ALT, QUAL, DP, genotype, gene, transcript,impact, consequence, variant_type,cDNA, exon, intron, AF_nfe, dbNSFP_Uniprot_acc, CLNSIG, CLNDN, CLNDISDB,n_alt_alleles, dbNSFP_MutationTaster_pred, dbNSFP_Polyphen2_HDIV_pred,dbNSFP_Polyphen2_HVAR_pred, dbNSFP_SIFT_pred, dbNSFP_LRT_pred)
       datos_madre$QUAL<-as.numeric(as.character(datos_madre$QUAL))
       datos_madre$AF_nfe<-as.numeric(datos_madre$AF_nfe)
       incProgress(0.05, detail = "Tabla creada para Trio-Madre. Iniciando lectura y procesamiento padre")
       cat("Fin del procesamiento del archivo Trio - madre","\n")
       
       #PADRE
       cat("Inicio del procesamiento del archivo Trio - padre","\n")
       incProgress(0.01, detail = "Iniciando lectura y procesamiento del archivo Trio-Padre. Leyendo archivo VCF")
       archivo_padre<-input$padre
       vcf<- read.vcfR(archivo_padre$datapath)
       cat("Archivo vcf correspondiente con padre leido","\n")
       info_basica<-as.data.frame(vcf@fix[,-8])
       gt<-as.data.frame(vcf@gt[,2])
       names(gt)<-c("gt")
       incProgress(0.09, detail = "Archivo padre leído. Extrayendo las anotaciones")
       anot<-extract_info_tidy(vcf, info_types = F)
       cat("Tabla info padre creada","\n")
       incProgress(0.1, detail = "Anotaciones extraídas. Creando tabla para la priorización")
       datos<-cbind(info_basica,gt,anot[,-1])
       datos$gt<- as.character(datos$gt)
       genotype<-character()
       dp<- numeric()
       for (i in 1:nrow(datos)){
         gt_split<-strsplit(datos$gt[i],":")
         genotype[i]<-gt_split[[1]][1]
         dp[i]<-gt_split[[1]][2]
       }
       datos$genotype<-genotype
       datos$DP<-dp
       consecuencia<-character()
       impacto<-character()
       exon<- character()
       intron<- character()
       cDNA<-character()
       for (i in 1:nrow(datos)){
           vep_split<-strsplit(datos$vep[i],"\\|")
           consecuencia[i]<-vep_split[[1]][2]
           impacto[i]<-vep_split[[1]][3]
           exon[i]<-vep_split[[1]][9]
           intron[i]<-vep_split[[1]][10]
       }
       datos$consequence<-consecuencia
       datos$impact<-impacto
       datos$exon<-exon
       datos$intron<-intron
       transcritos<-character()
       gen.nombre<-character()
       for (i in 1:nrow(datos)){
         EFF_split<-strsplit(datos$EFF[i],"\\|")
         transcritos[i]<-EFF_split[[1]][9]
         gen.nombre[i]<-EFF_split[[1]][6]
         cDNA[i]<-EFF_split[[1]][4]
       }
       datos$gene<-gen.nombre
       datos$transcript<- transcritos
       datos$cDNA<- cDNA
       cat("Tabla datos del vcf creada", "\n")
       datos_padre<-filter(datos, datos$ORIGIN == 1)
       datos_padre<-select(datos, CHROM, POS, ID, REF, ALT, QUAL, DP , genotype, gene, transcript,impact, consequence, variant_type,cDNA, exon, intron, AF_nfe, dbNSFP_Uniprot_acc, CLNSIG, CLNDN, CLNDISDB,n_alt_alleles, dbNSFP_MutationTaster_pred, dbNSFP_Polyphen2_HDIV_pred,dbNSFP_Polyphen2_HVAR_pred, dbNSFP_SIFT_pred, dbNSFP_LRT_pred)
       datos_padre$QUAL<-as.numeric(as.character(datos_padre$QUAL))
       datos_padre$AF_nfe<-as.numeric(datos_padre$AF_nfe)
       incProgress(0.05, detail = "Tabla creada para Trio-Padre. Iniciando lectura y procesamiento Trío-Hijo")
       cat("Fin del procesamiento del archivo Trio - padre","\n")
       
       #HIJO
       cat("Inicio del procesamiento del archivo Trio - hijo","\n")
       incProgress(0.01, detail = "Iniciando lectura y procesamiento del archivo Trio-Hijo. Leyendo archivo VCF")
       archivo_hijo<-input$hijo
       vcf<- read.vcfR(archivo_hijo$datapath)
       cat("Archivo vcf correspondiente con hijo leido","\n")
       info_basica<-as.data.frame(vcf@fix[,-8])
       gt<-as.data.frame(vcf@gt[,2])
       names(gt)<-c("gt")
       incProgress(0.09, detail = "Archivo hijo leído. Extrayendo las anotaciones")
       anot<-extract_info_tidy(vcf, info_types = F)
       cat("Tabla info hijo creada","\n")
       incProgress(0.1, detail = "Anotaciones extraídas. Creando tabla para la priorización")
       datos<-cbind(info_basica,gt,anot[,-1])
       datos$gt<- as.character(datos$gt)
       genotype<-character()
       dp<- numeric()
       for (i in 1:nrow(datos)){
         gt_split<-strsplit(datos$gt[i],":")
         genotype[i]<-gt_split[[1]][1]
         dp[i]<-gt_split[[1]][2]
       }
       datos$genotype<-genotype
       datos$DP<-dp
       consecuencia<-character()
       impacto<-character()
       exon<- character()
       intron<- character()
       cDNA<-character()
       for (i in 1:nrow(datos)){
           vep_split<-strsplit(datos$vep[i],"\\|")
           consecuencia[i]<-vep_split[[1]][2]
           impacto[i]<-vep_split[[1]][3]
           exon[i]<-vep_split[[1]][9]
           intron[i]<-vep_split[[1]][10]
       }
       datos$consequence<-consecuencia
       datos$impact<-impacto
       datos$exon<-exon
       datos$intron<-intron
       transcritos<-character()
       gen.nombre<-character()
       for (i in 1:nrow(datos)){
         EFF_split<-strsplit(datos$EFF[i],"\\|")
         transcritos[i]<-EFF_split[[1]][9]
         gen.nombre[i]<-EFF_split[[1]][6]
         cDNA[i]<-EFF_split[[1]][4]
       }
       datos$gene<-gen.nombre
       datos$transcript<- transcritos
       datos$cDNA<- cDNA
       cat("Tabla datos del vcf creada", "\n")
       datos_hijo<-filter(datos, datos$ORIGIN == 1)
       datos_hijo<-select(datos, CHROM, POS, ID, REF, ALT, QUAL, DP, genotype, gene, transcript,impact, consequence, variant_type, cDNA, exon, intron, AF_nfe, dbNSFP_Uniprot_acc, CLNSIG, CLNDN, CLNDISDB,n_alt_alleles, dbNSFP_MutationTaster_pred, dbNSFP_Polyphen2_HDIV_pred,dbNSFP_Polyphen2_HVAR_pred, dbNSFP_SIFT_pred, dbNSFP_LRT_pred)
       datos_hijo$QUAL<-as.numeric(as.character(datos_hijo$QUAL))
       datos_hijo$AF_nfe<-as.numeric(datos_hijo$AF_nfe)
       incProgress(0.05, detail = "Tabla creada para Trio-Hijo. Aplicando modelo de herencia")
       cat("Fin del procesamiento del archivo Trio - hijo","\n")
       
       cat("Todos los archivos han sido procesados", "\n")
       
       #Según lo escogido en herencia y origen de variante:
       
       datos_hijo$ID<- as.character(datos_hijo$ID)
       datos_madre$ID<- as.character(datos_madre$ID)
       datos_padre$ID<- as.character(datos_padre$ID)
       
       #AUTOSOMICA DOMINANTE
       if (input$herencia == "ad"){ 
           nfilas_hijo <- grep("0/1", datos_hijo$genotype)
           rs_hijo <- datos_hijo$ID[nfilas_hijo]
           if (length(rs_hijo) != 0){
             rs_padre <- datos_padre$ID
             rs_madre <- datos_madre$ID
             rs_hijo_padre<-setdiff(rs_hijo,rs_padre)
             rs_hijo_madre<-setdiff(rs_hijo,rs_madre)
             rs_hijo_final<-c(rs_hijo_madre,rs_hijo_padre)
             rs_hijo_final<- unique(rs_hijo_final)
             padre_variantes <- data.frame()
             hijo_variantes<- data.frame()
             madre_variantes <- data.frame()
             for (i in 1:length(rs_hijo_final)){
               fila<-subset(datos_hijo, datos_hijo$ID == rs_hijo_final[i])
               hijo_variantes<- rbind(hijo_variantes, fila)
             }
           }
          }
       if(input$origen == "her"){ 
         genot2<- strsplit("0|1","\\|")
         nfilas.genot<-numeric()
         for (i in 1:nrow(datos_hijo)) {
           genotype_datos<- strsplit(datos_hijo$genotype[i], "\\|")
           if(genotype_datos[[1]][1] == genot2[[1]][1]  &  genotype_datos[[1]][2] == genot2[[1]][2]){
             nfilas.genot <- c(i, nfilas.genot)}}
             rs_hijo<- datos_hijo$ID[nfilas.genot]
             nfilas_padre<- grep( "0/1", datos_padre$genotype)
             rs_padre<- datos_madre$ID[nfilas_padre]
             nfilas_madre<- grep( "0/1", datos_madre$genotype)
             rs_madre<- datos_madre$ID[nfilas_madre]
             rs_hijo_padre <- intersect(rs_hijo, rs_padre)
             rs_hijo_madre <- intersect (rs_hijo, rs_madre)
             rs_hijo_final<-c(rs_hijo_madre,rs_hijo_padre)
             rs_hijo_final<- unique(rs_hijo_final)
             padre_variantes <- data.frame()
             hijo_variantes<- data.frame()
             madre_variantes <- data.frame()
             for (i in 1:length(rs_hijo_final)){
               fila<-subset(datos_hijo, datos_hijo$ID == rs_hijo_final[i])
               hijo_variantes<- rbind(hijo_variantes, fila)
             }
             for (i in 1:length(rs_hijo_madre)){
               fila<-subset(datos_madre, datos_madre$ID == rs_hijo_madre[i])
               madre_variantes<- rbind(madre_variantes, fila)
             }
             for (i in 1:length(rs_hijo_padre)){
               fila<-subset(datos_padre, datos_padre$ID == rs_hijo_padre[i])
               padre_variantes<- rbind(padre_variantes, fila)
             }
       }
       
       #AUTOSOMICA RECESIVA
       if (input$herencia == "ar"){ 
         if(input$origen == "dn"){
           if (is.null(input$het) == TRUE){
           nfilas_hijo <- grep("1/1", datos_hijo$genotype)
           rs_hijo<- datos_hijo$ID[nfilas_hijo]
           if (length(rs_hijo) != 0){
             nfilas_padre<- grep( "0/1", datos_padre$genotype)
             rs_padre<- datos_padre$ID[nfilas_padre]
             nfilas_madre<- grep( "0/1", datos_madre$genotype)
             rs_madre<- datos_madre$ID[nfilas_madre]
             rs_hijo_padre <- intersect(rs_hijo, rs_padre)
             rs_hijo_madre <- intersect (rs_hijo, rs_madre)
             padre_variantes <- data.frame()
             hijo_variantes<- data.frame()
             madre_variantes <- data.frame()
             rs_hijo_final<-c(rs_hijo_madre,rs_hijo_padre)
             rs_hijo_final<- unique(rs_hijo_final)
             for (i in 1:length(rs_hijo_final)){
               fila<-subset(datos_hijo, datos_hijo$ID == rs_hijo_final[i])
               hijo_variantes<- rbind(hijo_variantes, fila)
             }
             for (i in 1:length(rs_hijo_madre)){
               fila<-subset(datos_madre, datos_madre$ID == rs_hijo_madre[i])
               madre_variantes<- rbind(madre_variantes, fila)
             }
             for (i in 1:length(rs_hijo_padre)){
               fila<-subset(datos_padre, datos_padre$ID == rs_hijo_padre[i])
               padre_variantes<- rbind(padre_variantes, fila)
             }
           }
           }else{
             if (input$het =="h"){
               nfilas_hijo <- grep("0/1", datos_hijo$genotype)
               rs_hijo<- datos_hijo$ID[nfilas_hijo]
               madre_variantes<- data.frame()
               padre_variantes <- data.frame()
               variantes_hijo<- data.frame()
               if (length(rs_hijo) != 0){
                 nfilas_padre<- grep( "0/1", datos_padre$genotype)
                 rs_padre<- datos_padre$ID[nfilas_padre]
                 nfilas_madre<- grep( "0/1", datos_madre$genotype)
                 rs_madre<- datos_madre$ID[nfilas_madre]
                 rs_hijo_padre <- setdiff(rs_hijo, rs_padre)
                 rs_hijo_madre <- setdiff (rs_hijo, rs_madre)
                 rs_hijo_final<-c(rs_hijo_madre,rs_hijo_padre)
                 rs_hijo_final<- unique(rs_hijo_final)
                 for (i in 1:length(rs_hijo_final)){
                   fila<-subset(datos_hijo,  rs_hijo_final[i] == datos_hijo$ID)
                   hijo_variantes<- rbind(hijo_variantes, fila)
                 }
               }
             }
             if (input$het == "hc"){
               tabla_genes<-table(datos_hijo$gene)
               genes2<-names(subset(tabla_genes, tabla_genes>1))[-1]
               rs_hijo <- character()
               hijo_variantes<- data.frame()
               padre_variantes<- data.frame()
               madre_variantes<- data.frame()
               for (i in 1:length(genes2)){
                 rs<-subset(datos_hijo, datos_hijo$gene == genes2[i] & datos_hijo$genotype =="0/1")
                 rs_hijo <- c(hijo_variantes, fila$id)
               }
               if (length(rs_hijo) !=0){
               nfilas_padre<- grep( "0/1", datos_padre$genotype)
               rs_padre<- datos_padre$ID[nfilas_padre]
               nfilas_madre<- grep( "0/1", datos_madre$genotype)
               rs_madre<- datos_madre$ID[nfilas_madre]
               rs_hijo_padre <- setdiff(rs_hijo, rs_padre)
               rs_hijo_madre <- setdiff (rs_hijo, rs_madre)
               rs_hijo_final<- c(rs_hijo_madre,rs_hijo_padre)
               rs_hijo_final<- unique(rs_hijo_final)
               for (i in 1:length(rs_hijo_final)){
                 fila<-subset(datos_hijo,  rs_hijo_final[i] == datos_hijo$ID)
                 hijo_variantes<- rbind(hijo_variantes, fila)
               }
             }
             }
           }
         }
         if(input$origen == "her"){
           if (is.null(input$het) == TRUE){
           genot2<- strsplit("1|1","\\|")
           nfilas.genot<-numeric()
           for (i in 1:nrow(datos_hijo)) {
             genotype_datos<- strsplit(datos_hijo$genotype[i], "\\|")
             if(genotype_datos[[1]][1] == genot2[[1]][1]  &  genotype_datos[[1]][2] == genot2[[1]][2]){
               nfilas.genot <- c(i, nfilas.genot)}}
           rs_hijo<- datos_hijo$ID[nfilas.genot]
           if (length(rs_hijo) != 0){
             nfilas_padre<- grep( "0/1", datos_padre$genotype)
             rs_padre<- datos_padre$ID[nfilas_padre]
             nfilas_madre<- grep( "0/1", datos_madre$genotype)
             rs_madre<- datos_madre$ID[nfilas_madre]
             rs_hijo_padre <- intersect(rs_hijo, rs_padre)
             rs_hijo_madre <- intersect (rs_hijo, rs_madre)
             rs_hijo_final<-c(rs_hijo_madre,rs_hijo_padre)
             rs_hijo_final<- unique(rs_hijo_final)
             padre_variantes <- data.frame()
             hijo_variantes<- data.frame()
             madre_variantes <- data.frame()
             for (i in 1:length(rs_hijo_final)){
               fila<-subset(datos_hijo, datos_hijo$ID == rs_hijo_final[i])
               hijo_variantes<- rbind(hijo_variantes, fila)
             }
             for (i in 1:length(rs_hijo_madre)){
               fila<-subset(datos_madre, datos_madre$ID == rs_hijo_madre[i])
               madre_variantes<- rbind(madre_variantes, fila)
             }
             for (i in 1:length(rs_hijo_padre)){
               fila<-subset(datos_padre, datos_padre$ID == rs_hijo_padre[i])
               padre_variantes<- rbind(padre_variantes, fila)
             }
           }
           }else{
             if (input$het =="h"){
               nfilas_hijo <- grep("0/1", datos_hijo$genotype)
               rs_hijo<- datos_hijo$ID[nfilas_hijo]
               madre_variantes<- data.frame()
               padre_variantes <- data.frame()
               variantes_hijo<- data.frame()
               if (length(rs_hijo) != 0){
                 nfilas_padre<- grep( "0/1", datos_padre$genotype)
                 rs_padre<- datos_padre$ID[nfilas_padre]
                 nfilas_madre<- grep( "0/1", datos_madre$genotype)
                 rs_madre<- datos_madre$ID[nfilas_madre]
                 rs_hijo_padre <- intersect(rs_hijo, rs_padre)
                 rs_hijo_madre <- intersect (rs_hijo, rs_madre)
                 rs_hijo_final<-c(rs_hijo_madre,rs_hijo_padre)
                 rs_hijo_final<- unique(rs_hijo_final)
                 for (i in 1:length(rs_hijo_madre)){
                   fila<-subset(datos_madre, datos_madre$ID == rs_hijo_madre[i])
                   madre_variantes<- rbind(madre_variantes, fila)
                 }
                 for (i in 1:length(rs_hijo_padre)){
                   fila<-subset(datos_padre, datos_padre$ID == rs_hijo_padre[i])
                   padre_variantes<- rbind(padre_variantes, fila)
                 }
                 for (i in 1:length(rs_hijo_final)){
                   fila<-subset(datos_hijo,  rs_hijo_final[i] == datos_hijo$ID)
                   hijo_variantes<- rbind(hijo_variantes, fila)
                 }
               }
             }
             if (input$het == "hc"){
               tabla_genes<-table(datos_hijo$gene)
               genes2<-names(subset(tabla_genes, tabla_genes>1))[-1]
               hijo_variantes<- data.frame()
               padre_variantes<- data.frame()
               madre_variantes<- data.frame()
               for (i in 1:length(genes2)){
                 rs<-subset(datos_hijo, datos_hijo$gene == genes2[i] & datos_hijo$genotype[i] =="0/1")
                 rs_hijo <- c(hijo_variantes, rs$id)
               }
               if (length(rs_hijo) != 0){
                 nfilas_padre<- grep( "0/1", datos_padre$genotype)
                 rs_padre<- datos_padre$ID[nfilas_padre]
                 nfilas_madre<- grep( "0/1", datos_madre$genotype)
                 rs_madre<- datos_madre$ID[nfilas_madre]
                 rs_hijo_padre <- intersect(rs_hijo, rs_padre)
                 rs_hijo_madre <- intersect (rs_hijo, rs_madre)
                 rs_hijo_final<- c(rs_hijo_madre,rs_hijo_padre)
                 rs_hijo_final<- unique(rs_hijo_final)
                 for (i in 1:length(rs_hijo_final)){
                   fila<-subset(datos_hijo,  rs_hijo_final[i] == datos_hijo$ID)
                   hijo_variantes<- rbind(hijo_variantes, fila)
                 }
               }
             }
           }
         }
       }
       
       #LIGADA A X DOMINANTE
       if (input$herencia == "xd"){ 
         if(input$origen == "dn"){
           if(input$sex == "M"){
             nfilas_hijo <- grep("1/1", datos_hijo$genotype)
             rs_hijo <- datos_hijo$ID[nfilas_hijo]
             if (length(rs_hijo) != 0){
               rs_madre<- datos_madre$ID
               rs_hijo_madre<-setdiff(rs_hijo,rs_madre)
               hijo_variantes<- data.frame()
               for (i in 1:length(rs_hijo_madre)){
                 fila<-subset(datos_hijo, datos_hijo$ID == rs_hijo_madre[i])
                 hijo_variantes<- rbind(hijo_variantes, fila)
               }
               madre_variantes <- data.frame()
               padre_variantes <- data.frame()
             }
           }
           else{
             nfilas_hijo <- grep("0/1", datos_hijo$genotype)
             rs_hijo <- datos_hijo$ID[nfilas_hijo]
             if (length(rs_hijo) != 0){
               rs_padre<- datos_padre$ID
               rs_madre<- datos_madre$ID
               rs_hijo_padre<-setdiff(rs_hijo,rs_padre)
               rs_hijo_madre<-setdiff(rs_hijo,rs_madre)
               rs_hijo_final<-c(rs_hijo_madre,rs_hijo_padre)
               rs_hijo_final<- unique(rs_hijo_final)
               hijo_variantes<- data.frame()
               for (i in 1:length(rs_hijo_final)){
                 fila<-subset(datos_hijo, datos_hijo$ID == rs_hijo_final[i])
                 hijo_variantes<- rbind(hijo_variantes, fila)
               }
               madre_variantes <- data.frame()
               padre_variantes <- data.frame()
             }
           }
         }
         if(input$origen == "her"){
           if(input$sex == "M"){
             nfilas_hijo <- grep("1/1", datos_hijo$genotype)
             nfilas_madre <- grep("0/1", datos_madre$genotype)
             rs_hijo <- datos_hijo$ID[nfilas_hijo]
             if (length(rs_hijo) != 0){
               rs_madre<- datos_madre$ID[nfilas_madre]
               rs_hijo_madre<- intersect(rs_hijo,rs_madre)
               padre_variantes <- data.frame()
               hijo_variantes<- data.frame()
               madre_variantes <- data.frame()
               for (i in 1:length(rs_hijo_madre)){
                 fila<-subset(datos_hijo, datos_hijo$ID == rs_hijo_madre[i])
                 hijo_variantes<- rbind(hijo_variantes, fila)
               }
               for (i in 1:length(rs_hijo_madre)){
                 fila<-subset(datos_madre, datos_madre$ID == rs_hijo_madre[i])
                 madre_variantes<- rbind(madre_variantes, fila)
               }
             }
           }
           else{ 
             nfilas_hijo <- grep("0/1", datos_hijo$genotype)
             rs_hijo<- datos_hijo$ID[nfilas_hijo]
             if (length(rs_hijo) != 0){
               nfilas_padre<- grep( "1/1", datos_padre$genotype)
               rs_padre<- datos_padre$ID[nfilas_padre]
               nfilas_madre<- grep( "0/1", datos_madre$genotype)
               rs_madre<- datos_madre$ID[nfilas_madre]
               rs_hijo_padre <- intersect(rs_hijo, rs_padre)
               rs_hijo_madre <- intersect (rs_hijo, rs_madre)
               rs_hijo_final<-c(rs_hijo_madre,rs_hijo_padre)
               rs_hijo_final<- unique(rs_hijo_final)
               padre_variantes <- data.frame()
               hijo_variantes<- data.frame()
               madre_variantes <- data.frame()
               for (i in 1:length(rs_hijo_final)){
                 fila<-subset(datos_hijo, datos_hijo$ID == rs_hijo_final[i])
                 hijo_variantes<- rbind(hijo_variantes, fila)
               }
               for (i in 1:length(rs_hijo_madre)){
                 fila<-subset(datos_madre, datos_madre$ID == rs_hijo_madre[i])
                 madre_variantes<- rbind(madre_variantes, fila)
               }
               for (i in 1:length(rs_hijo_padre)){
                 fila<-subset(datos_padre, datos_padre$ID == rs_hijo_padre[i])
                 padre_variantes<- rbind(padre_variantes, fila)
               }
             }
           }
         }
       }
       
       #LIGADA A X RECESIVA
       if (input$herencia == "xr"){ 
         if(input$origen == "dn"){
           if(input$sex == "M"){
             nfilas_hijo <- grep("1/1", datos_hijo$genotype)
             rs_hijo <- datos_hijo$ID[nfilas_hijo]
             if (length(rs_hijo) != 0){
               rs_madre<- datos_madre$ID
               rs_hijo_madre<-setdiff(rs_hijo,rs_madre)
               padre_variantes <- data.frame()
               hijo_variantes<- data.frame()
               madre_variantes <- data.frame()
               for (i in 1:length(rs_hijo_madre)){
                 fila<-subset(datos_hijo, datos_hijo$ID == rs_hijo_madre[i])
                 hijo_variantes<- rbind(hijo_variantes, fila)
               }
             }
           }
           else{
             if (is.null(input$het) == TRUE){
             nfilas_hijo <- grep("1/1", datos_hijo$genotype)
             nfilas_madre <- grep("0/1", datos_madre$genotype)
             rs_hijo <- datos_hijo$ID[nfilas_hijo]
             if (length(rs_hijo) != 0){
               rs_madre<- datos_madre$ID[nfilas_madre]
               rs_hijo_madre<- intersect(rs_hijo,rs_madre)
               padre_variantes <- data.frame()
               hijo_variantes<- data.frame()
               madre_variantes <- data.frame()
               for (i in 1:length(rs_hijo_madre)){
                 fila<-subset(datos_hijo, datos_hijo$ID == rs_hijo_madre[i])
                 hijo_variantes<- rbind(hijo_variantes, fila)
               }
               for (i in 1:length(rs_hijo_madre)){
                 fila<-subset(datos_madre, datos_madre$ID == rs_hijo_madre[i])
                 madre_variantes<- rbind(madre_variantes, fila)
               }
             }
           }else{
             if (input$het =="h"){
               nfilas_hijo <- grep("0/1", datos_hijo$genotype)
               rs_hijo<- datos_hijo$ID[nfilas_hijo]
               madre_variantes<- data.frame()
               padre_variantes <- data.frame()
               variantes_hijo<- data.frame()
               if (length(rs_hijo) != 0){
                 nfilas_padre<- grep( "1/1", datos_padre$genotype)
                 rs_padre<- datos_padre$ID[nfilas_padre]
                 nfilas_madre<- grep( "0/1", datos_madre$genotype)
                 rs_madre<- datos_madre$ID[nfilas_madre]
                 rs_hijo_padre <- setdiff(rs_hijo, rs_padre)
                 rs_hijo_madre <- setdiff (rs_hijo, rs_madre)
                 rs_hijo_final<-c(rs_hijo_madre,rs_hijo_padre)
                 rs_hijo_final<- unique(rs_hijo_final)
                 for (i in 1:length(rs_hijo_final)){
                   fila<-subset(datos_hijo,  rs_hijo_final[i] == datos_hijo$ID)
                   hijo_variantes<- rbind(hijo_variantes, fila)
                 }
               }
             }
             if (input$het == "hc"){
               tabla_genes<-table(datos_hijo$gene)
               genes2<-names(subset(tabla_genes, tabla_genes>1))[-1]
               rs_hijo <- character()
               hijo_variantes<- data.frame()
               padre_variantes<- data.frame()
               madre_variantes<- data.frame()
               for (i in 1:length(genes2)){
                 rs<-subset(datos_hijo, datos_hijo$gene == genes2[i] & datos_hijo$genotype =="0/1")
                 rs_hijo <- c(hijo_variantes, fila$id)
               }
               if (length(rs_hijo) !=0){
                 nfilas_padre<- grep( "1/1", datos_padre$genotype)
                 rs_padre<- datos_padre$ID[nfilas_padre]
                 nfilas_madre<- grep( "0/1", datos_madre$genotype)
                 rs_madre<- datos_madre$ID[nfilas_madre]
                 rs_hijo_padre <- setdiff(rs_hijo, rs_padre)
                 rs_hijo_madre <- setdiff (rs_hijo, rs_madre)
                 rs_hijo_final<- c(rs_hijo_madre,rs_hijo_padre)
                 rs_hijo_final<- unique(rs_hijo_final)
                 for (i in 1:length(rs_hijo_final)){
                   fila<-subset(datos_hijo,  rs_hijo_final[i] == datos_hijo$ID)
                   hijo_variantes<- rbind(hijo_variantes, fila)
                 }
               } 
             }
           }
           }
         }
         if(input$origen == "her"){
           if(input$sex == "M"){
             nfilas_hijo <- grep("1/1", datos_hijo$genotype)
             hijo_variantes <- datos_hijo[nfilas_hijo]
             if (length(rs_hijo) != 0){
               rs_madre<- datos_madre$ID
               rs_hijo_madre<-setdiff(rs_hijo,rs_madre)
               padre_variantes <- data.frame()
               madre_variantes <- data.frame()
               for (i in 1:length(rs_hijo_madre)){
                 fila<-subset(datos_madre, datos_madre$ID == rs_hijo_madre[i])
                 madre_variantes<- rbind(madre_variantes, fila)
               }
             }
             
           }
           else{ 
             if (is.null(input$het) == TRUE){
             nfilas_hijo <- grep("1/1", datos_hijo$genotype)
             nfilas_madre <- grep("0/1", datos_madre$genotype)
             rs_hijo <- datos_hijo$ID[nfilas_hijo]
             if (length(rs_hijo) != 0){
               rs_madre<- datos_madre$ID[nfilas_madre]
               rs_hijo_madre<- intersect(rs_hijo,rs_madre)
               padre_variantes <- data.frame()
               hijo_variantes<- data.frame()
               madre_variantes <- data.frame()
               for (i in 1:length(rs_hijo_madre)){
                 fila<-subset(datos_hijo, datos_hijo$ID == rs_hijo_madre[i])
                 hijo_variantes<- rbind(hijo_variantes, fila)
               }
               for (i in 1:length(rs_hijo_madre)){
                 fila<-subset(datos_madre, datos_madre$ID == rs_hijo_madre[i])
                 madre_variantes<- rbind(madre_variantes, fila)
               }
             }
           }else{
             if (input$het =="h"){
               nfilas_hijo <- grep("0/1", datos_hijo$genotype)
               rs_hijo<- datos_hijo$ID[nfilas_hijo]
               madre_variantes<- data.frame()
               padre_variantes <- data.frame()
               variantes_hijo<- data.frame()
               if (length(rs_hijo) != 0){
                 nfilas_padre<- grep( "0/1", datos_padre$genotype)
                 rs_padre<- datos_padre$ID[nfilas_padre]
                 nfilas_madre<- grep( "0/1", datos_madre$genotype)
                 rs_madre<- datos_madre$ID[nfilas_madre]
                 rs_hijo_padre <- intersect(rs_hijo, rs_padre)
                 rs_hijo_madre <- intersect (rs_hijo, rs_madre)
                 rs_hijo_final<-c(rs_hijo_madre,rs_hijo_padre)
                 rs_hijo_final<- unique(rs_hijo_final)
                 for (i in 1:length(rs_hijo_madre)){
                   fila<-subset(datos_madre, datos_madre$ID == rs_hijo_madre[i])
                   madre_variantes<- rbind(madre_variantes, fila)
                 }
                 for (i in 1:length(rs_hijo_padre)){
                   fila<-subset(datos_padre, datos_padre$ID == rs_hijo_padre[i])
                   padre_variantes<- rbind(padre_variantes, fila)
                 }
                 for (i in 1:length(rs_hijo_final)){
                   fila<-subset(datos_hijo,  rs_hijo_final[i] == datos_hijo$ID)
                   hijo_variantes<- rbind(hijo_variantes, fila)
                 }
               } 
             }
             if (input$het == "hc"){
               tabla_genes<-table(datos_hijo$gene)
               genes2<-names(subset(tabla_genes, tabla_genes>1))[-1]
               rs_hijo <- character()
               hijo_variantes<- data.frame()
               padre_variantes<- data.frame()
               madre_variantes<- data.frame()
               for (i in 1:length(genes2)){
                 rs<-subset(datos_hijo, datos_hijo$gene == genes2[i] & datos_hijo$genotype[i] =="0/1")
                 rs_hijo <- c(hijo_variantes, fila$id)
               }
               if (length(rs_hijo) != 0){
                 nfilas_padre<- grep( "0/1", datos_padre$genotype)
                 rs_padre<- datos_padre$ID[nfilas_padre]
                 nfilas_madre<- grep( "0/1", datos_madre$genotype)
                 rs_madre<- datos_madre$ID[nfilas_madre]
                 rs_hijo_padre <- intersect(rs_hijo, rs_padre)
                 rs_hijo_madre <- intersect (rs_hijo, rs_madre)
                 rs_hijo_final<- c(rs_hijo_madre,rs_hijo_padre)
                 rs_hijo_final<- unique(rs_hijo_final)
                 for (i in 1:length(rs_hijo_final)){
                   fila<-subset(datos_hijo,  rs_hijo_final[i] == datos_hijo$ID)
                   hijo_variantes<- rbind(hijo_variantes, fila)
                 }
               }
             }
           }
         }
         }
       }

       incProgress(0.25, detail = "Modelo de herencia aplicado.")
       })
       output$proc_her<- if (nrow(hijo_variantes) == 0){
         renderText("No hay variantes que coincidan con el modelo de herencia seleccionado")
       }else{
         renderText("El modelo de herencia se ha aplicado correctamente")
       }
     }
     
    }
   ##Filtros
observeEvent(input$run,{
  if (is.null(input$indice) == FALSE){
  output$res_i <- renderText("Resultado de la priorización para caso Índice")
  res_indice<- reactive({
   if (input$nombre_gen != ""){
     if (length(grep(",", input$nombre_gen)) == 0){
       cat("El gen introducido es: ", input$nombre_gen, "\n")
       nfilas.gen<-grep(input$nombre_gen, datos_variantes$gene)
       datos_variantes<-datos_variantes[nfilas.gen,]
     }else{
       cat("Los nombres de genes introducidos son: ", input$nombre_gen, "\n")
       #nombre_gen<-as.character(input$nombre_gen)
       gen<-strsplit(input$nombre_gen, ",")
       nfilas.gen<-numeric()
       for (i in 1:length(gen[[1]])){
         nfilas.gen.split<-grep(gen[[1]][i],datos_variantes$gene)
         nfilas.gen<-c(nfilas.gen,nfilas.gen.split)
       }
       datos_variantes<-datos_variantes[nfilas.gen,]
     }
   }

 #Proteina
   if (input$uniprot != ""){
     cat("El nombre de la proteína introducido es: ", input$uniprot, "\n")
     nfilas.prot<-grep(input$uniprot, datos_variantes$dbNSFP_Uniprot_acc)
     datos_variantes<-datos_variantes[nfilas.prot,]
   }
    
 #Impacto proteína
   if (input$impacto != "all"){
     cat("El impacto en la proteína seleccionado es: ", input$impacto, "\n")
     nfilas.impacto<-numeric()
     for (i in 1:nrow(datos_variantes)){
         nfilas.impacto<-grep(input$impacto, datos_variantes$impact)
         datos_variantes<-datos_variantes[nfilas.impacto,]   
     }}

 #Tipo variante
   if (input$tipo_variante != "all"){
     cat("El tipo de variante escogido es: ",input$tipo_variante, "\n")
     datos_variantes<- filter(datos_variantes, datos_variantes$variant_type == input$tipo_variante)
   }

 #QUAL
   if (input$QUAL != 0){
     cat("El filtrado para QUAL introducido es: ", paste(input$compQUAL,input$QUAL,sep = ""), "\n")
     if (input$compQUAL == "="){
       datos_variantes<- filter(datos_variantes, datos_variantes$QUAL == input$QUAL)}
     if (input$compQUAL == "<="){
       datos_variantes<- filter(datos_variantes, datos_variantes$QUAL <= input$QUAL)}
     if (input$compQUAL == ">="){
       datos_variantes<- filter(datos_variantes, datos_variantes$QUAL >= input$QUAL)}
   }
   if (input$QUAL_sup !=0){
     cat("El intervalo introducido para QUAL es: ", "[",input$QUAL_inf,",",input$QUAL_sup,"]","\n")
     datos_variantes<- filter(datos_variantes, datos_variantes$QUAL <= input$QUAL_sup & datos_variantes$QUAL >= input$QUAL_inf)
   }

 #Genotipo
    if (input$genot != ""){
      cat("El nombre del genotipo introducido es: ", input$genot, "\n")
      if (length(strsplit(input$genot,"\\|")[[1]]) != 1){
        genot2<- strsplit(input$genot,"\\|")
        nfilas.genot<-numeric()
        for (i in 1:nrow(datos_variantes)) {
          genotype_datos<- strsplit(datos_variantes$genotype[i], "\\|")
          if(genotype_datos[[1]][1] == genot2[[1]][1]  &  genotype_datos[[1]][2] == genot2[[1]][2]){
            nfilas.genot <- c(i, nfilas.genot)}
        }         
        datos_variantes<-datos_variantes[nfilas.genot,]
      }else{
        nfilas.genot<-grep(input$genot, datos_variantes$genotype)
        datos_variantes<-datos_variantes[nfilas.genot,]
      }
    }
    
 #F. europea
   if (input$f.eu != 0){
     cat("El filtrado para frecuencia europea introducido es: ", paste(input$comp_f.eu,input$f.eu,sep = ""), "\n")
     if (input$comp_f.eu == "="){
       datos_variantes<- filter(datos_variantes, datos_variantes$AF_nfe == input$f.eu)}
     if (input$comp_f.eu == "<="){
       datos_variantes<- filter(datos_variantes, datos_variantes$AF_nfe <= input$f.eu)}
     if (input$comp_f.eu == ">="){
       datos_variantes<- filter(datos_variantes, datos_variantes$AF_nfe >= input$f.eu)}
   }
   if (input$f.eu_sup !=0){
     cat("El intervalo introducido para frecuencia europea es: ", "[",input$f.eu_inf,",",input$f.eu_sup,"]","\n")
     datos_variantes<- filter(datos_variantes, datos_variantes$AF_nfe <= input$f.eu_sup & datos_variantes$AF_nfe >= input$f.eu_inf)
   }

 #Enfermedad asociada
   if (input$enfermedad != ""){
     cat("El nombre de la enfermedad asociada introducido es: ", input$enfermedad, "\n")
     nfilas.enfermedad<-grep(input$enfermedad, datos_variantes$CLNDN)
     datos_variantes<-datos_variantes[nfilas.enfermedad,]
   }

 #Efecto clinico
   if (input$efecto != "all"){
     cat("El valor seleccionado para el efecto es: ", input$efecto, "\n")
     datos_variantes<-filter(datos_variantes, datos_variantes$CLNSIG == input$efecto)
   } 

 #DP
   if (input$DP != 0){
     cat("El filtrado para DP introducido es: ", paste(input$compDP,input$DP,sep = ""), "\n")
     if (input$compDP == "="){
       datos_variantes<- subset(datos_variantes, DP == input$DP)
     }
     if (input$compDP == "<="){
       datos_variantes<- subset(datos_variantes, DP <= input$DP)
     }
     if (input$compDP == ">="){
       datos_variantes<- subset(datos_variantes, DP >= input$DP)
     }
   }
  if (input$DP_sup !=0){
   cat("El intervalo introducido para DP es: ", "[",input$DP_inf,",",input$DP_sup,"]","\n")
   datos_variantes<- subset(datos_variantes, DP >= input$DP_inf & DP <= input$DP_sup) 
  }
    
    return(datos_variantes)})
  output$resultado.indice<-DT::renderDataTable(DT::datatable({res_indice()}))
  }else{
  output$res_m <- renderText("Resultado de la priorización para caso Trio - Madre")
  res_madre<- reactive({
    if (input$nombre_gen != ""){
      if (length(grep(",", input$nombre_gen)) == 0){
        cat("El gen introducido es: ", input$nombre_gen, "\n")
        nfilas.gen<-grep(input$nombre_gen, madre_variantes$gene)
        madre_variantes<-madre_variantes[nfilas.gen,]
      }else{
        cat("Los nombres de genes introducidos son: ", input$nombre_gen, "\n")
        #nombre_gen<-as.character(input$nombre_gen)
        gen<-strsplit(input$nombre_gen, ",")
        nfilas.gen<-numeric()
        for (i in 1:length(gen[[1]])){
          nfilas.gen.split<-grep(gen[[1]][i],madre_variantes$gene)
          nfilas.gen<-c(nfilas.gen,nfilas.gen.split)
        }
        madre_variantes<-madre_variantes[nfilas.gen,]
      }
    }
    
    #Proteina
    if (input$uniprot != ""){
      cat("El nombre de la proteína introducido es: ", input$uniprot, "\n")
      nfilas.prot<-grep(input$uniprot, madre_variantes$dbNSFP_Uniprot_acc)
      madre_variantes<-madre_variantes[nfilas.prot,]
    }
    
    #Impacto proteína
    if (input$impacto != "all"){
      cat("El impacto en la proteína seleccionado es: ", input$impacto, "\n")
      nfilas.impacto<-numeric()
      for (i in 1:nrow(madre_variantes)){
          nfilas.impacto<-grep(input$impacto, madre_variantes$impact)
          madre_variantes<-madre_variantes[nfilas.impacto,]   
        }}
    
    #Tipo variante
    if (input$tipo_variante != "all"){
      cat("El tipo de variante escogido es: ",input$tipo_variante, "\n")
      madre_variantes<- filter(madre_variantes, madre_variantes$variant_type == input$tipo_variante)
    }
    
    #QUAL
    if (input$QUAL != 0){
      cat("El filtrado para QUAL introducido es: ", paste(input$compQUAL,input$QUAL,sep = ""), "\n")
      if (input$compQUAL == "="){
        madre_variantes<- filter(madre_variantes, madre_variantes$QUAL == input$QUAL)}
      if (input$compQUAL == "<="){
        madre_variantes<- filter(madre_variantes, madre_variantes$QUAL <= input$QUAL)}
      if (input$compQUAL == ">="){
        madre_variantes<- filter(madre_variantes, madre_variantes$QUAL >= input$QUAL)}
    }
    if (input$QUAL_sup !=0){
      cat("El intervalo introducido para QUAL es: ", "[",input$QUAL_inf,",",input$QUAL_sup,"]","\n")
      madre_variantes<- filter(madre_variantes, madre_variantes$QUAL <= input$QUAL_sup & madre_variantes$QUAL >= input$QUAL_inf)
    }
    
    #Genotipo
    if (input$genot != ""){
      cat("El nombre del genotipo introducido es: ", input$genot, "\n")
      if (length(strsplit(input$genot,"\\|")[[1]]) != 1){
        genot2<- strsplit(input$genot,"\\|")
        nfilas.genot<-numeric()
        for (i in 1:nrow(madre_variantes)) {
          genotype_datos<- strsplit(madre_variantes$genotype[i], "\\|")
          if(genotype_datos[[1]][1] == genot2[[1]][1]  &  genotype_datos[[1]][2] == genot2[[1]][2]){
            nfilas.genot <- c(i, nfilas.genot)}
        }         
        madre_variantes<-madre_variantes[nfilas.genot,]
      }else{
        nfilas.genot<-grep(input$genot, madre_variantes$genotype)
        madre_variantes<-madre_variantes[nfilas.genot,]
      }
    }
    
    #F. europea
    if (input$f.eu != 0){
      cat("El filtrado para frecuencia europea introducido es: ", paste(input$comp_f.eu,input$f.eu,sep = ""), "\n")
      if (input$comp_f.eu == "="){
        madre_variantes<- filter(madre_variantes, madre_variantes$AF_nfe == input$f.eu)}
      if (input$comp_f.eu == "<="){
        madre_variantes<- filter(madre_variantes, madre_variantes$AF_nfe <= input$f.eu)}
      if (input$comp_f.eu == ">="){
        madre_variantes<- filter(madre_variantes, madre_variantes$AF_nfe >= input$f.eu)}
    }
    if (input$f.eu_sup !=0){
      cat("El intervalo introducido para frecuencia europea es: ", "[",input$f.eu_inf,",",input$f.eu_sup,"]","\n")
      madre_variantes<- filter(madre_variantes, madre_variantes$AF_nfe <= input$f.eu_sup & madre_variantes$AF_nfe >= input$f.eu_inf)
    }
    
    #Enfermedad asociada
    if (input$enfermedad != ""){
      cat("El nombre de la enfermedad asociada introducido es: ", input$enfermedad, "\n")
      nfilas.enfermedad<-grep(input$enfermedad, madre_variantes$CLNDN)
      madre_variantes<-madre_variantes[nfilas.enfermedad,]
    }
    
    #Efecto clinico
    if (input$efecto != "all"){
      cat("El valor seleccionado para el efecto es: ", input$efecto, "\n")
      madre_variantes<-filter(madre_variantes, madre_variantes$CLNSIG == input$efecto)
    } 
    
    #DP
    if (input$DP != 0){
      cat("El filtrado para DP introducido es: ", paste(input$compDP,input$DP,sep = ""), "\n")
      if (input$compDP == "="){
        madre_variantes<- subset(madre_variantes, DP == input$DP)
      }
      if (input$compDP == "<="){
        madre_variantes<- subset(madre_variantes, DP <= input$DP)
      }
      if (input$compDP == ">="){
        madre_variantes<- subset(madre_variantes, DP >= input$DP)
      }
    }
    if (input$DP_sup !=0){
      cat("El intervalo introducido para DP es: ", "[",input$DP_inf,",",input$DP_sup,"]","\n")
      madre_variantes<- subset(madre_variantes, DP >= input$DP_inf & DP <= input$DP_sup) 
    }
    
    return(madre_variantes)})
  output$resultado.madre<-DT::renderDataTable(DT::datatable({res_madre()}))
  output$res_p <- renderText("Resultado de la priorización para caso Trio - Padre")
  res_padre<-reactive({
    if (input$nombre_gen != ""){
      if (length(grep(",", input$nombre_gen)) == 0){
        cat("El gen introducido es: ", input$nombre_gen, "\n")
        nfilas.gen<-grep(input$nombre_gen, padre_variantes$gene)
        padre_variantes<-padre_variantes[nfilas.gen,]
      }else{
        cat("Los nombres de genes introducidos son: ", input$nombre_gen, "\n")
        #nombre_gen<-as.character(input$nombre_gen)
        gen<-strsplit(input$nombre_gen, ",")
        nfilas.gen<-numeric()
        for (i in 1:length(gen[[1]])){
          nfilas.gen.split<-grep(gen[[1]][i],padre_variantes$gene)
          nfilas.gen<-c(nfilas.gen,nfilas.gen.split)
        }
        padre_variantes<-padre_variantes[nfilas.gen,]
      }
    }
    
    #Proteina
    if (input$uniprot != ""){
      cat("El nombre de la proteína introducido es: ", input$uniprot, "\n")
      nfilas.prot<-grep(input$uniprot, padre_variantes$dbNSFP_Uniprot_acc)
      padre_variantes<-padre_variantes[nfilas.prot,]
    }
    
    #Impacto proteína
    if (input$impacto != "all"){
      cat("El impacto en la proteína seleccionado es: ", input$impacto, "\n")
      nfilas.impacto<-numeric()
      for (i in 1:nrow(padre_variantes)){
          nfilas.impacto<-grep(input$impacto, padre_variantes$impact)
          padre_variantes<-padre_variantes[nfilas.impacto,]   
        }}
    
    #Tipo variante
    if (input$tipo_variante != "all"){
      cat("El tipo de variante escogido es: ",input$tipo_variante, "\n")
      padre_variantes<- filter(padre_variantes, padre_variantes$variant_type == input$tipo_variante)
    }
    
    #QUAL
    if (input$QUAL != 0){
      cat("El filtrado para QUAL introducido es: ", paste(input$compQUAL,input$QUAL,sep = ""), "\n")
      if (input$compQUAL == "="){
        padre_variantes<- filter(padre_variantes, padre_variantes$QUAL == input$QUAL)}
      if (input$compQUAL == "<="){
        padre_variantes<- filter(padre_variantes, padre_variantes$QUAL <= input$QUAL)}
      if (input$compQUAL == ">="){
        padre_variantes<- filter(padre_variantes, padre_variantes$QUAL >= input$QUAL)}
    }
    if (input$QUAL_sup !=0){
      cat("El intervalo introducido para QUAL es: ", "[",input$QUAL_inf,",",input$QUAL_sup,"]","\n")
      padre_variantes<- filter(padre_variantes, padre_variantes$QUAL <= input$QUAL_sup & padre_variantes$QUAL >= input$QUAL_inf)
    }
    
    #Genotipo
    if (input$genot != ""){
      cat("El nombre del genotipo introducido es: ", input$genot, "\n")
      if (length(strsplit(input$genot,"\\|")[[1]]) != 1){
        genot2<- strsplit(input$genot,"\\|")
        nfilas.genot<-numeric()
        for (i in 1:nrow(padre_variantes)) {
          genotype_datos<- strsplit(padre_variantes$genotype[i], "\\|")
          if(genotype_datos[[1]][1] == genot2[[1]][1]  &  genotype_datos[[1]][2] == genot2[[1]][2]){
            nfilas.genot <- c(i, nfilas.genot)}
        }         
        padre_variantes<-padre_variantes[nfilas.genot,]
      }else{
        nfilas.genot<-grep(input$genot, padre_variantes$genotype)
        padre_variantes<-padre_variantes[nfilas.genot,]
      }
    }
    
    #F. europea
    if (input$f.eu != 0){
      cat("El filtrado para frecuencia europea introducido es: ", paste(input$comp_f.eu,input$f.eu,sep = ""), "\n")
      if (input$comp_f.eu == "="){
        padre_variantes<- filter(padre_variantes, padre_variantes$AF_nfe == input$f.eu)}
      if (input$comp_f.eu == "<="){
        padre_variantes<- filter(padre_variantes, padre_variantes$AF_nfe <= input$f.eu)}
      if (input$comp_f.eu == ">="){
        padre_variantes<- filter(padre_variantes, padre_variantes$AF_nfe >= input$f.eu)}
    }
    if (input$f.eu_sup !=0){
      cat("El intervalo introducido para frecuencia europea es: ", "[",input$f.eu_inf,",",input$f.eu_sup,"]","\n")
      padre_variantes<- filter(padre_variantes, padre_variantes$AF_nfe <= input$f.eu_sup & padre_variantes$AF_nfe >= input$f.eu_inf)
    }
    
    #Enfermedad asociada
    if (input$enfermedad != ""){
      cat("El nombre de la enfermedad asociada introducido es: ", input$enfermedad, "\n")
      nfilas.enfermedad<-grep(input$enfermedad, padre_variantes$CLNDN)
      padre_variantes<-padre_variantes[nfilas.enfermedad,]
    }
    
    #Efecto clinico
    if (input$efecto != "all"){
      cat("El valor seleccionado para el efecto es: ", input$efecto, "\n")
      padre_variantes<-filter(padre_variantes, padre_variantes$CLNSIG == input$efecto)
    } 
    
    #DP
    if (input$DP != 0){
      cat("El filtrado para DP introducido es: ", paste(input$compDP,input$DP,sep = ""), "\n")
      if (input$compDP == "="){
        padre_variantes<- subset(padre_variantes, DP == input$DP)
      }
      if (input$compDP == "<="){
        padre_variantes<- subset(padre_variantes, DP <= input$DP)
      }
      if (input$compDP == ">="){
        padre_variantes<- subset(padre_variantes, DP >= input$DP)
      }
    }
    if (input$DP_sup !=0){
      cat("El intervalo introducido para DP es: ", "[",input$DP_inf,",",input$DP_sup,"]","\n")
      padre_variantes<- subset(padre_variantes, DP >= input$DP_inf & DP <= input$DP_sup) 
    }
    
    return(padre_variantes)})
  output$resultado.padre<-DT::renderDataTable(DT::datatable({res_padre()}))
  output$res_h <- renderText("Resultado de la priorización para caso Trio - Hijo")
  res_hijo<-reactive({
    if (input$nombre_gen != ""){
      if (length(grep(",", input$nombre_gen)) == 0){
        cat("El gen introducido es: ", input$nombre_gen, "\n")
        nfilas.gen<-grep(input$nombre_gen, hijo_variantes$gene)
        hijo_variantes<-hijo_variantes[nfilas.gen,]
      }else{
        cat("Los nombres de genes introducidos son: ", input$nombre_gen, "\n")
        #nombre_gen<-as.character(input$nombre_gen)
        gen<-strsplit(input$nombre_gen, ",")
        nfilas.gen<-numeric()
        for (i in 1:length(gen[[1]])){
          nfilas.gen.split<-grep(gen[[1]][i],hijo_variantes$gene)
          nfilas.gen<-c(nfilas.gen,nfilas.gen.split)
        }
        hijo_variantes<-hijo_variantes[nfilas.gen,]
      }
    }
    
    #Proteina
    if (input$uniprot != ""){
      cat("El nombre de la proteína introducido es: ", input$uniprot, "\n")
      nfilas.prot<-grep(input$uniprot, hijo_variantes$dbNSFP_Uniprot_acc)
      hijo_variantes<-hijo_variantes[nfilas.prot,]
    }
    
    #Impacto proteína
    if (input$impacto != "all"){
      cat("El impacto en la proteína seleccionado es: ", input$impacto, "\n")
      nfilas.impacto<-numeric()
      for (i in 1:nrow(hijo_variantes)){
          nfilas.impacto<-grep(input$impacto, hijo_variantes$impact)
          hijo_variantes<-hijo_variantes[nfilas.impacto,]   
        }}
    
    #Tipo variante
    if (input$tipo_variante != "all"){
      cat("El tipo de variante escogido es: ",input$tipo_variante, "\n")
      hijo_variantes<- filter(hijo_variantes, hijo_variantes$variant_type == input$tipo_variante)
    }
    
    #QUAL
    if (input$QUAL != 0){
      cat("El filtrado para QUAL introducido es: ", paste(input$compQUAL,input$QUAL,sep = ""), "\n")
      if (input$compQUAL == "="){
        hijo_variantes<- filter(hijo_variantes, hijo_variantes$QUAL == input$QUAL)}
      if (input$compQUAL == "<="){
        hijo_variantes<- filter(hijo_variantes, hijo_variantes$QUAL <= input$QUAL)}
      if (input$compQUAL == ">="){
        hijo_variantes<- filter(hijo_variantes, hijo_variantes$QUAL >= input$QUAL)}
    }
    if (input$QUAL_sup !=0){
      cat("El intervalo introducido para QUAL es: ", "[",input$QUAL_inf,",",input$QUAL_sup,"]","\n")
      hijo_variantes<- filter(hijo_variantes, hijo_variantes$QUAL <= input$QUAL_sup & hijo_variantes$QUAL >= input$QUAL_inf)
    }
    
    #Genotipo
    if (input$genot != ""){
      cat("El nombre del genotipo introducido es: ", input$genot, "\n")
      if (length(strsplit(input$genot,"\\|")[[1]]) != 1){
        genot2<- strsplit(input$genot,"\\|")
        nfilas.genot<-numeric()
        for (i in 1:nrow(hijo_variantes)) {
          genotype_datos<- strsplit(hijo_variantes$genotype[i], "\\|")
          if(genotype_datos[[1]][1] == genot2[[1]][1]  &  genotype_datos[[1]][2] == genot2[[1]][2]){
            nfilas.genot <- c(i, nfilas.genot)}
        }         
        hijo_variantes<-hijo_variantes[nfilas.genot,]
      }else{
        nfilas.genot<-grep(input$genot, hijo_variantes$genotype)
        hijo_variantes<-hijo_variantes[nfilas.genot,]
      }
    }
    
    #F. europea
    if (input$f.eu != 0){
      cat("El filtrado para frecuencia europea introducido es: ", paste(input$comp_f.eu,input$f.eu,sep = ""), "\n")
      if (input$comp_f.eu == "="){
        hijo_variantes<- filter(hijo_variantes, hijo_variantes$AF_nfe == input$f.eu)}
      if (input$comp_f.eu == "<="){
        hijo_variantes<- filter(hijo_variantes, hijo_variantes$AF_nfe <= input$f.eu)}
      if (input$comp_f.eu == ">="){
        hijo_variantes<- filter(hijo_variantes, hijo_variantes$AF_nfe >= input$f.eu)}
    }
    if (input$f.eu_sup !=0){
      cat("El intervalo introducido para frecuencia europea es: ", "[",input$f.eu_inf,",",input$f.eu_sup,"]","\n")
      hijo_variantes<- filter(hijo_variantes, hijo_variantes$AF_nfe <= input$f.eu_sup & hijo_variantes$AF_nfe >= input$f.eu_inf)
    }
    
    #Enfermedad asociada
    if (input$enfermedad != ""){
      cat("El nombre de la enfermedad asociada introducido es: ", input$enfermedad, "\n")
      nfilas.enfermedad<-grep(input$enfermedad, hijo_variantes$CLNDN)
      hijo_variantes<-hijo_variantes[nfilas.enfermedad,]
    }
    
    #Efecto clinico
    if (input$efecto != "all"){
      cat("El valor seleccionado para el efecto es: ", input$efecto, "\n")
      hijo_variantes<-filter(hijo_variantes, hijo_variantes$CLNSIG == input$efecto)
    } 
    
    #DP
    if (input$DP != 0){
      cat("El filtrado para DP introducido es: ", paste(input$compDP,input$DP,sep = ""), "\n")
      if (input$compDP == "="){
        hijo_variantes<- subset(hijo_variantes, DP == input$DP)
      }
      if (input$compDP == "<="){
        hijo_variantes<- subset(hijo_variantes, DP <= input$DP)
      }
      if (input$compDP == ">="){
        hijo_variantes<- subset(hijo_variantes, DP >= input$DP)
      }
    }
    if (input$DP_sup !=0){
      cat("El intervalo introducido para DP es: ", "[",input$DP_inf,",",input$DP_sup,"]","\n")
      hijo_variantes<- subset(hijo_variantes, DP >= input$DP_inf & DP <= input$DP_sup) 
    }
    
    return(hijo_variantes)})
  output$resultado.hijo<-DT::renderDataTable(DT::datatable({res_hijo()}))
  }

 #BOTONES DESCARGA 
  output$desc_res_indice <- downloadHandler(
    filename = "Resultado_priorización_índice.csv",
    content =  function(file){
      write.csv(res_indice() , file)
    },
    contentType = "text/plain"
  )
  
  output$desc_res_m <- downloadHandler(
    filename = "Resultado_priorización_madre.csv",
    content =  function(file){
      write.csv(res_madre() , file)
    },
    contentType = "text/plain"
  )
  
  output$desc_res_padre <- downloadHandler(
    filename = "Resultado_priorización_padre.csv",
    content =  function(file){
      write.csv(res_padre() , file)
    },
    contentType = "text/plain"
  )
  
  output$desc_res_h <- downloadHandler(
    filename = "Resultado_priorización_hijo.csv",
    content =  function(file){
      write.csv(res_hijo() , file)
    },
    contentType = "text/plain"
  )
})    
})
 
#BOTON DESCARGA PARAMETROS USADOS
 output$prior<- downloadHandler(
  filename = "Parámetros_priorización.csv",
  content =  function(file){
    priorizacion<- matrix( c("Genes",input$nombre_gen,
                                  "Proteina", input$uniprot,
                                  "Impacto en la proteína", input$impacto,
                                  "Tipo de variante", input$tipo_variante,
                                  "Genotipo", input$genot,
                                  "Enfermedad asociada", input$enfermedad,
                                  "Efecto clínico", input$efecto,
                                  "Comparar QUAL",paste(input$compQUAL,input$QUAL),
                                  "Intervalo QUAL", paste(input$QUAL_inf,",",input$QUAL_sup),
                                  "Comparar DP",paste(input$compDP,input$DP),
                                  "Intervalo DP", paste(input$DP_inf,",",input$DP_sup),
                                  "Comparar frecuencia europea",paste(input$comp_f.eu,input$f.eu),
                                  "Intervalo frecuencia europea", paste(input$f.eu_inf,",",input$f.eu_sup)
                             ) ,ncol=2, nrow=13, byrow = T)
    write.csv(priorizacion,file, row.names = F)
  },
  contentType = "text/plain"
)

}

# Run the application 
shinyApp(ui = ui, server = server)
