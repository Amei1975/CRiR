library(shiny)

setwd("C:/R/CRiR")
incidencia <- read.csv2("incidencia.csv")
mortalitat <- read.csv2("mortalitat.csv")
poblacio <- read.csv2("poblacio.csv")
ltumors <- read.csv2("Llistat_Tumors.csv")
ltumors.list <- as.list(ltumors$ICDO)
names(ltumors.list) <- ltumors$CAT

# Definicion UI
ui <- fluidPage(
   
   # Titulo aplicacion
   titlePanel("RCT - Calculo de Tasas"),
   
   # Sidebar 
   sidebarLayout(
      sidebarPanel(
         selectInput("zona", "Àrea", c("Tarragona"="P4300","RS Camp de Tarragona"="R0002","RS Terres del Ebre"="R0003")),
         selectInput("icdo", "Tipus Tumoral",choices=ltumors.list, 100),
         radioButtons("sexe", "Sexe", c("Home"=1,"Dona"=2,"Ambdos sexes"=0),1,inline=TRUE),
         numericInput("anyi", "Any Inici",2008),
         numericInput("anyf", "Any Final",2011),
         radioButtons("tipus", "Incidencia/Mortalitat", c("Incidencia"="I","Mortalidad"="M"),"I",inline=TRUE),
         radioButtons("estandard", "Ajustament", c("Mundial"=1,"Europeu"=2),1,inline=TRUE),
         radioButtons("tcalcul", "Tipus de càlcul", c("Global"=1,"Any a any"=2),1,inline=TRUE),
         actionButton("calcul","Calcul")
         ),
        # Show a plot of the generated distribution
      mainPanel(
         verbatimTextOutput("result")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$result <- renderPrint({
    if (input$calcul==0) return(invisible(NULL))
    isolate({
      if (input$tcalcul==1) indicadors(input$zona, input$icdo, input$sexe, input$anyi, input$anyf, input$tipus, 1, 18, input$estandard,"CAT")
      if (input$tcalcul==2) indicadors.any(input$zona, input$icdo, input$sexe, input$anyi, input$anyf, input$tipus, 1, 18, input$estandard,"CAT")
      })
    })
}

# Funciones utilizadas

indicadors <- function(zona="P4300", icdo, sexe, anyi=2008, anyf=2011, tipus="I", edin=1, edfn=18, estandard=1, idioma="CAT")
{
  if (idioma=="CAT")
  {
    valoricdo <- ltumors[ltumors$ICDO==icdo, "CAT"]
  }
  if (idioma=="ESP")
  {
    valoricdo <- ltumors[ltumors$ICDO==icdo, "ESP"]
  }
  if (idioma=="ENG")
  {
    valoricdo <- ltumors[ltumors$ICDO==icdo, "ENG"]
  }
  valorn <- nany(zona, icdo, sexe, anyi, anyf, tipus, edin, edfn)	
  valortb <- tb(zona, icdo, sexe, anyi, anyf, tipus, edin, edfn)
  valorta <- ta(zona, icdo, sexe, anyi, anyf, tipus, edin, edfn,estandard)
  cat("-------------------------------------------------- -------- -------- --------\n")
  cat("TIPUS TUMORAL:                                        N/ANY       TB       TA\n")
  cat("-------------------------------------------------- -------- -------- --------\n")
  cat(format(as.character(valoricdo), width=50),format(round(valorn,1),width=8),format(round(valortb,1),width=8),format(round(valorta,1),width=8))
  }

indicadors.any <- function(zona="P4300", icdo, sexe, anyi=2008, anyf=2011, tipus="I", edin=1, edfn=18, estandard=1, idioma="CAT")
{
  if (idioma=="CAT")
  {
    valoricdo <- ltumors[ltumors$ICDO==icdo, "CAT"]
  }
  if (idioma=="ESP")
  {
    valoricdo <- ltumors[ltumors$ICDO==icdo, "ESP"]
  }
  if (idioma=="ENG")
  {
    valoricdo <- ltumors[ltumors$ICDO==icdo, "ENG"]
  }
  cat("----- -------- -------- --------\n")
  cat(" ANY:        N       TB       TA\n")
  cat("----- -------- -------- --------\n")
  for(i in anyi:anyf) {
    valorn <- nany(zona, icdo, sexe, i, i, tipus, edin, edfn)	
    valortb <- tb(zona, icdo, sexe, i, i, tipus, edin, edfn)
    valorta <- ta(zona, icdo, sexe, i, i, tipus, edin, edfn,estandard)   
    cat(format(i, width=5),format(round(valorn,1),width=8),format(round(valortb,1),width=8),format(round(valorta,1),width=8),"\n")
  }
}

ntotal <- function(zona="P4300", icdo, sexe, anyi, anyf, tipus="I", edin=1, edfn=18)
{
  a<-4+edin
  b<-4+edfn	
  if(tipus=="I")
  {
    if (edin == 1 & edfn == 18) 
    {
      b <-23
    }
    n <- nrow(as.matrix(incidencia[incidencia$ZONA==zona & incidencia$ICDO==icdo & incidencia$SEXE==sexe & incidencia$ANY>=anyi & incidencia$ANY<=anyf,a:b]))
    if (n > 0) {casos <- incidencia[incidencia$ZONA==zona & incidencia$ICDO==icdo & incidencia$SEXE==sexe & incidencia$ANY>=anyi & incidencia$ANY<=anyf,a:b]}
    if (n == 0) { casos <- 0 }
  }
  if(tipus=="M")
  {
    n <- nrow(as.matrix(mortalitat[mortalitat$ZONA==zona & mortalitat$ICDO==icdo & mortalitat$SEXE==sexe & mortalitat$ANY>=anyi & mortalitat$ANY<=anyf,a:b]))
    if (n > 0) {casos <- mortalitat[mortalitat$ZONA==zona & mortalitat$ICDO==icdo & mortalitat$SEXE==sexe & mortalitat$ANY>=anyi & mortalitat$ANY<=anyf,a:b]}
    if (n == 0) { casos <- 0 }
  }
  sum(casos)
}

nany <- function(zona="P4300", icdo, sexe, anyi, anyf, tipus="I", edin=1, edfn=18)
{
  anys <- (anyf-anyi)+1
  ntotal(zona, icdo, sexe, anyi, anyf, tipus, edin, edfn)/anys
}

tb <- function(zona="P4300", icdo, sexe, anyi, anyf, tipus="I", edin=1, edfn=18)
{
  a<-4+edin
  b<-4+edfn
  casos <- ntotal(zona, icdo, sexe, anyi, anyf, tipus, edin, edfn)
  pob <- poblacio[poblacio$ZONA==zona & poblacio$SEXE==sexe & poblacio$ANY>=anyi & poblacio$ANY<=anyf & poblacio$MES=="J",a:b]
  casos/sum(pob)*100000
}

   ta <- function(zona="P4300", icdo, sexe, anyi, anyf, tipus="I", edin=1, edfn=18, estandard=1)
   {
     if (sexe ==1 | sexe == 0) 
     {
       edats <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
       edats[edin:edfn] <- 1
       if(estandard == 1) 
       {
         w <- matrix(c(0.12, 0.1, 0.09, 0.09, 0.08, 0.08, 0.06, 0.06, 0.06, 0.06, 0.05, 0.04, 0.04, 0.03, 0.02, 0.01, 0.005, 0.005),nrow = 18, ncol = 1)
       }
       if(estandard == 2) 
       {
         w <- matrix(c(0.08, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, 0.01),nrow = 18, ncol = 1)
       }
       k <- sum(w[edin:edfn])
       w <- edats * w * (1/k)
       tasa <- tasa(zona, icdo, 1, anyi, anyf, tipus)
       if(tipus=="I")
       {
         mis <- incidencia[incidencia$ZONA==zona & incidencia$ICDO==icdo & incidencia$SEXE==1 & incidencia$ANY>=anyi & incidencia$ANY<=anyf,23]
         mis <- sum(mis)
       }
       if(tipus=="M")
       {
         mis <- 0
       }
       tah <- as.matrix(tasa) %*% w
       tot <- ntotal(zona, icdo, 1, anyi, anyf, tipus, 1, 18)
       if (tah>0) {tah <- tah * (tot/(tot - mis))}
     }
     if (sexe == 2 | sexe == 0) 
     {
       edats <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
       edats[edin:edfn] <- 1
       if(estandard == 1) 
       {
         w <- matrix(c(0.12, 0.1, 0.09, 0.09, 0.08, 0.08, 0.06, 0.06, 0.06, 0.06, 0.05, 0.04, 0.04, 0.03, 0.02, 0.01, 0.005, 0.005),nrow = 18, ncol = 1)
       }
       if(estandard == 2) 
       {
         w <- matrix(c(0.08, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.07, 0.06, 0.05, 0.04, 0.03, 0.02, 0.01, 0.01),nrow = 18, ncol = 1)
       }
       k <- sum(w[edin:edfn])
       w <- edats * w * (1/k)
       tasa <- tasa(zona, icdo, 2, anyi, anyf, tipus)
       if(tipus=="I")
       {
         mis <- incidencia[incidencia$ZONA==zona & incidencia$ICDO==icdo & incidencia$SEXE==2 & incidencia$ANY>=anyi & incidencia$ANY<=anyf,23]
         mis <- sum(mis)
       }
       if(tipus=="M")
       {
         mis <- 0
       }
       tam <- as.matrix(tasa) %*% w
       tot <- ntotal(zona, icdo, 2, anyi, anyf, tipus, 1, 18)
       if (tam>0) {tam <- tam * (tot/(tot - mis))}
     }
     if(sexe == 1) { ta <- tah}
     if(sexe == 2) { ta <- tam}
     if(sexe == 0) { ta <- (tah+tam)/2 }
     if(is.na(ta)) { ta<- 0 }
     ta
   }
   
   tasa <- function(zona="P4300", icdo, sexe, anyi, anyf, tipus="I")
   {
     pob <- poblacio[poblacio$ZONA==zona & poblacio$SEXE==sexe & poblacio$ANY>=anyi & poblacio$ANY<=anyf & poblacio$MES=="J",5:22]
     aux <- rep(0,dim(pob)[1])
     pob <- aggregate(pob,list(aux=aux),sum)
     pob <- pob[2:19]
     rm(aux) 
     if(tipus=="I")
     {
       n <- nrow(incidencia[incidencia$ZONA==zona & incidencia$ICDO==icdo & incidencia$SEXE==sexe & incidencia$ANY>=anyi & incidencia$ANY<=anyf,5:22])
       if (n > 0) {casos <- incidencia[incidencia$ZONA==zona & incidencia$ICDO==icdo & incidencia$SEXE==sexe & incidencia$ANY>=anyi & incidencia$ANY<=anyf,5:22]}	
       if (n == 0) { casos <- t(rep(0,18)) }
     }
     if(tipus=="M")
     {
       n <- nrow(mortalitat[mortalitat$ZONA==zona & mortalitat$ICDO==icdo & mortalitat$SEXE==sexe & mortalitat$ANY>=anyi & mortalitat$ANY<=anyf,5:22])
       if (n > 0) {casos <- mortalitat[mortalitat$ZONA==zona & mortalitat$ICDO==icdo & mortalitat$SEXE==sexe & mortalitat$ANY>=anyi & mortalitat$ANY<=anyf,5:22]}
       if (n == 0) { casos <- t(rep(0,18)) }
     }
     if (n>0) {
       aux <- rep(0,n)
       casos <- aggregate(casos,list(aux=aux),sum)
       casos <- casos[2:19]
       rm(aux)
     } 
     tasa <- (casos/pob)*100000
     tasa 
   }
   


# Ejecucion de la aplicacion 
   
shinyApp(ui = ui, server = server)

