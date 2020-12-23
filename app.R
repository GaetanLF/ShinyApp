#############################################
#                                           #
#           SHINY APP MADE BY               #
#    MEHDI FERHAT, GAETAN LE FLOCH AND      #
#            ALEXIS VIGNARD                 #
#                                           #
#############################################

#library(shiny)
#library(rsconnect)
library(FactoMineR)

#############################################
#      Firstly we load dataframes.          #
#############################################

# Dataframe from the third exercise
pathfile = "universite.csv"
dataUniv <- read.table(pathfile,header=T,sep=";")
rownames(dataUniv) = dataUniv$X
dataUniv = dataUniv[2:13]



#############################################
#    The ui variable denotes the user       #
# interface, e.g. what will the user see on #
#             his/her screen.               #
#############################################

ui <- fluidPage(
  

  
  checkboxGroupInput(inputId="Supp",
                     label="Indiquez les variables supplémentaires",
                     choices=colnames(dataUniv),
                     selected=colnames(dataUniv)[7:12],
                     inline=T,
                     choiceNames = colnames(dataUniv), # Asks the user for supplementary variables
                     choiceValues = 1:ncol(dataUniv)), 
  
  
  plotOutput(outputId = "ACGraph"), # Plots the 2D corresponding analysis.
  
  
  tableOutput(outputId = "cos2row"), # Shows cos2 from the 2D factorial plan
  tableOutput(outputId = "cos2col"),
  tableOutput(outputId = "cos2suppcol"),
  
  h1("Exercice 3"),
  p("Les réponses développées sur l'application sont cohérentes si vous sélectionnez les mêmes variables supplémentaires que nous."),
  h3("Question 1"),
  p("Ici, le plan factoriel explique 86.23% de l'inertie."),
  h3("Question 2"),
  p("Les études de lettres et de langues sont bien représentées, de même que les femmes en licence. Pour les masters et les doctorats,
    c'est un peu plus compliqué. Globalement, les points représentant les femmes et les études de langues sont assez proches.
    Nous pouvons raisonnablement penser que les femmes sont plus attirées par les études de lettres, au moins en licence."),
  h3("Question 3"),
  p("Au total, les hommes sont très bien représentés sur le plan factoriel. Il en est de même pour chaque niveau de diplôme où les cos2
    ne sont jamais en-dessous de 60%. Au niveau des sciences, seules les sciences humaines et les sciences fondamentales sont bien
    représentées ; les sciences économiques le sont aussi un peu dans une certaine mesuree, mais la SVT est mal représentée."),
  p("Sur la comparaison Hommes/Sciences, ils sont très proches des sciences fondamentales et, dans une certaine mesure, des sciences
    économiques (même si rappelons-le les sciences éco ne sont pas très bien représentées). Nous pouvons affirmer que les hommes sont
    attirés, surtout au niveau master, par les sciences fondamentales. Par ailleurs, les hommes sont très éloignés de STAPS, mais tout
    de même plus proches que les femmes."),
  h3("Question 4"),
  p("Le plan factoriel représente extrêmement bien les études d'AES (les cos2 sont presque égaux à 1 en les additionnant). Les licences
    sont assez proches d'AES et extrêmement bien représentées aussi. Les masters dans leur ensemble sont bien représentés, tout comme
    les doctorats, et ils sont tous les deux très éloignés de AES. Nous pouvons affirmer que ces études sont plutôt courtes."),
  h3("Question 5"),
  p("Le premier axe, de la dimension 1, pourrait séparer les études littéraires des études scientifiques : le point le plus à gauche
    représente les études de langues et le point le plus à droite représente les sciences fondamentales. Le deuxième axe, lui,
    pourrait séparer les études plutôt longues de celles plutôt courtes, comme en témoigne essentiellement le placement des points
    Licence, Master et Doctorat sur le plan factoriel."),
  h3("Question 6"),
  p("On ne peut pas penser grand chose de la proximité entre ces deux points : les femmes en master sont mal représentées (~0.47)
    et les études de SVT également (~0.42) : ces points ne sont pas interprétables et nous ne pouvons conclure à une supposée 
    proximité."),
  h3("Question 7"),
  p("Lettres et arts sont extrêmement bien représentées (~0.91) et les femmes en licence également (~0.97). Il y a une très forte
    proximité entre ces deux catégories.")
  
  
  
) # End of ui component
  
  
#############################################
#    The server is the deep side of the     #
#       shiny app, all our operations       #
#            are handling here.             #
#############################################

server <- function(input,output) {
  
  Ex3CA <- function(){ # Ex3CA is handling the analysis
    
    SupCol <- match(input$Supp,colnames(dataUniv))
    Univ.CA <- CA(dataUniv, col.sup=SupCol) # This function gives us the CA.
    return(Univ.CA)
  }
  
  output$ACGraph <- renderPlot({ 
    req(Ex3CA()) # Requests the function Ex3PCA in order to make the graph sent to the ACGraph component in ui.
     })
  
  output$cos2row <- renderTable( # Sent cos2row values from the factorial plan in the app
    {
      U = Ex3CA()
      U$row$cos2[,1:2]
    },rownames=TRUE
  )
  
  output$cos2col <- renderTable( # Sent cos2col values in the app
    {
      U = Ex3CA()
      U$col$cos2[,1:2]
    },rownames=TRUE
  )
  
  output$cos2suppcol <- renderTable( # Sent cos2suppcol values in the app
    {
      U = Ex3CA()
      U$col.sup$cos2[,1:2]
    },rownames=TRUE
  )
  
} # End of server component
  
# https://vimeo.com/rstudioinc/review/131218530/212d8a5a7a/#t=51m
  
#############################################
#    We make the link between the user      #
#   interface and the server.That's the     #
#         end of our application.           #
#############################################
shinyApp(ui=ui,server=server)