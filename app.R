library(shiny)
library(shinythemes)
rm(list=ls())

ui <- fluidPage(
  
  theme = shinytheme("superhero"),
  # img(src = "image.jpg"),
  h1("Estimez votre patrimoine, actuel et futur"),
  # h3(""),
  span("Cette page est associée au billet de blog: ", style="size-font:2.4em;"),
  span(a("Gérer son patrimoine commence par un grand inventaire", 
         href="https://direr.netlify.app/post/inventaire/"),
       style="size:2.4em;"),
  br(),
  span("Auteur: Alexis Direr", style="size-font:2.4em;"),
  br(),
  span("R code: ", style="size-font:2.4em;"),
  span(a("https://github.com/Direr/blog_01", href="https://github.com/Direr/blog_01"), style="size:2.4em;"),
  br(),
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId ="w0", 
        label = "Valeur présente du patrimoine :",
        min = 0, max = 1000000, value = 20000),
      
      sliderInput("ep0", 
        label = "Épargne annuelle :",
        min = 0, max = 40000, value = 12000),
      
      sliderInput("prs", 
        label = "Taux de progression annuel de l'épargne :",
        min = 0, max = .1, value = .02),
      
      sliderInput("rr", 
        label = "Rendement annuel de l'épargne :",
        min = 0, max = .1, value = .02),
      
      sliderInput("na", 
        label = "Durée du placement en années :",
        min = 2, max = 50, value = 15),
    ),
    
    mainPanel(
      
      plotOutput("wealth")
      
    ),
    
  ),
                                                                                                                                                                                                                                                                                                                                                                                                    
  "Le patrimoine couvre l\'ensemble des composantes de la richesse : livrets et comptes bancaires, 
  épargne retraite, contrats d\'assurance vie, immobilier (valeur de marché nette de l\'endettement résiduel), compte titres, ...",
  br(),
  "Le rendement annuel de l\'épargne est une moyenne pondérée des rendements prévus sur les différentes catégories du patrimoine.",
  br(),
  "L\'épargne annuelle est la différence entre les revenus annuels nets de toute fiscalité et les dépenses annuelles."
  
)

server <- function(input, output) {
  
  output$wealth <- renderPlot({
    
    library(ggplot2)
    
    ##### calcul des richesses actualisées à toutes les dates
    wealthy = data.frame(
      annee=numeric(),
      total_wealth=numeric(),
      fi_wealth=numeric(),
      human_cap=numeric()
    )
    
    fin <- input$w0
    for (t in 1:(input$na+1)) {                 # t=1 correspond à la date 0
      if (t>1) {
        fin <- (1+input$rr)*fin + input$ep0*(1+input$prs)^(t-1) # w capitalisé jusqu'à t
      }
      hum <- 0
      if (t < input$na + 1) {
        for (j in t : input$na) {
          hum <- hum + (input$ep0*(1+input$prs)^j)*(1+input$rr)^(t-j-1) # w actualisée
        }
      }
      wealthy <- rbind(wealthy,
         data.frame(
           annee = t-1,
           total_wealth = fin+hum,
           fi_wealth = fin,
           human = hum)
      )
    }
    
    ggplot(wealthy, aes(x=annee)) +
      geom_area(aes(y=total_wealth, fill="richesse future actualisée")) +
      geom_area(aes(y=fi_wealth, fill="richesse acquise")) +
      scale_fill_manual(name="", 
                        values = c("richesse future actualisée"="lightblue", 
                                   "richesse acquise"="MediumSlateBlue")) +
      labs(x="Années", y="") +
      theme(legend.position="bottom",
            text = element_text(size=20)) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
    
  })
  
}
shinyApp(ui = ui, server = server)
