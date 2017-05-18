library(shiny)

setwd("C:/Users/LENOVO/Desktop/Curso/CRiR/CRiR")
incidencia <- read.csv2("incidencia.csv")
mortalitat <- read.csv2("mortalitat.csv")
poblacio <- read.csv2("poblacio.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("RCT - Calculo de Tasas"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         numericInput("icdo", "Tipo Tumoral", 100),
         numericInput("sexe", "Sexe", 1),
         numericInput("anyi", "Any Inici",2003),
         numericInput("anyf", "Any Final",2007),
         actionButton("calcula","Calcula")
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
    if (input$calcula==0) return(invisible(NULL))
    isolate({
      ta("P4300", input$icdo, input$sexe, input$anyi, input$anyf, "I", 1, 18, 1)
      })
    })
  
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

# Run the application 
shinyApp(ui = ui, server = server)

