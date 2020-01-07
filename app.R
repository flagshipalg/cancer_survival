library(survival)
library(survminer)
require("survival")
library(shiny)
setwd("d:/yd/survival_3/")
cancer = read.csv("tumor.csv")


cancer1 = mutate(cancer, MATH_group=ifelse(cancer$MATH>50, "high","low"))
cancer2=mutate(cancer1, TMB.Score_group=ifelse(cancer1$TMB.Score>15, "high","low"))

vars <- names(cancer2)


#cancer2$Cancer.Type
#a <- unique(cancer2$Cancer_Type)
Type <- c("Breast Cancer", "Esophagogastric Cancer",
          "Bladder Cancer", "Non-Small Cell Lung Cancer",
          "Glioma", "Head and Neck Cancer",
          "Cancer of Unknown Primary", "Renal Cell Carcinoma",
          "Melanoma", "Colorectal Cancer",
          "Skin Cancer, Non-Melanoma")



ui <- pageWithSidebar(
  headerPanel("Survival Curve"),
  sidebarPanel(
    selectInput('tumor_type', 'tumor type',Type),
    selectInput('categrey', 'calsses', vars, selected=vars[21])
  ),
  mainPanel(
    plotOutput('plot1')
  )
  
)

server <- function(input, output){
  
  output$plot1 <- renderPlot({
    
    tumor<-subset(cancer2, Cancer_Type==input$tumor_type)
    a <- tumor[, input$categrey]
    fit<-survfit(Surv(tumor$OS, tumor$OS_Status)~a, data=tumor)

    plot(fit)

    
  })
  
}


shinyApp(ui=ui, server=server)

