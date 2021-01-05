#############################################
#                                           #
#           SHINY APP MADE BY               #
#    MEHDI FERHAT, GAETAN LE FLOCH AND      #
#            ALEXIS VIGNARD                 #
#                                           #
#############################################

#library(shiny)
library(rsconnect)
library(FactoMineR)

#############################################
#      Firstly we load dataframes.          #
#############################################

# Dataframe from the first exercise

pathfile="bdf.csv"
data <- read.csv(pathfile,sep=":",header=T)
data2 = data[,c("COEF","DOMTRAV","TYPMEN2","CC","REVTOT","DIPLOPR","DIPLOCJ")]
dataINSEE = data2[complete.cases(data2),]
dataINSEE$DOMTRAV = factor(dataINSEE$DOMTRAV)
dataINSEE$TYPMEN2 = factor(dataINSEE$TYPMEN2)
dataINSEE$CC = factor(dataINSEE$CC)
dataINSEE$DIPLOPR = factor(dataINSEE$DIPLOPR)
dataINSEE$DIPLOCJ = factor(dataINSEE$DIPLOCJ)
dataINSEE$COEF = dataINSEE$COEF*1000 # Retraitement du coefficient de pondération.
dataINSEE = dataINSEE[rep(row.names(dataINSEE),dataINSEE$COEF),2:ncol(dataINSEE)]
lim = round(nrow(dataINSEE)*0.8,0)
trainData <- dataINSEE[1:lim,]
testData <- dataINSEE[(lim+1):nrow(dataINSEE),]

# Dataframe from the second exercise

pathfile="villes.txt"
dataVilles <- read.table(pathfile,header=T)

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
                 
tags$head( # Some aditional CSS
  tags$link(rel="stylesheet",type="text/css",href="bootstrap-simplex.css")
  
),
tabsetPanel(
  
  tabPanel("Accueil", # We present our application
           titlePanel("Accueil"),
           img(src="UP1.png"),
           p(div(HTML("<em>Cette application a été développée par Mehdi FERHAT, Gaëtan LE FLOCH et Alexis VIGNARD.</em>"))),
           p(div(HTML("<em>Ecole d'Economie de la Sorbonne, 2020.</em>"))),
           h2("Bienvenue sur notre application"),
           p("A travers les trois exercices proposés dans les onglets, nous vous proposons de découvrir trois aspects de l'analyse de
             données tels qu'indiquées dans le second contrôle continu effectué dans le cadre du Master 1 Econométrie, Statistiques de
             l'Université Paris 1 Panthéon-Sorbonne lors du cours de Mr. Joseph RYNKIEWICZ. Ainsi, vous aurez l'opportunité de modifier certaines valeurs pour tester la sensibilité de nos
             algorithmes et la robustesse des interprétations que nous vous proposons. Notez néanmoins que les chiffres et interprétations que nous vous proposons doivent être mis en perspective
             avec les paramètres fixés par défaut dans cette application Shiny. Si vous les modifiez, les réponses ne sont plus cohérentes."),
           p("Si vous repérez un dysfonctionnement ou une faute, nous vous remercions par avance de bien vouloir nous contacter sur : 
             gaetan.le-floch[at]etu.univ-paris1.fr. Veuillez noter que certaines figures peuvent mettre du temps pour apparaître si vous
             utilisez le fichier brut disponible sur Github. Vous ne devriez pas avoir ce problème si vous accédez à l'application par
             shinyapps.io."),
           p(div(HTML("En cas de besoin, vous pouvez accéder au <a href='https://github.com/GaetanLF/ShinyApp' target='_blank'
                      >répertoire Github</a>."))),
           p("Enfin, veuillez noter que cette application est à but purement illustratif : ici, vous ne retrouverez pas nécessairement les
             détails techniques qui nous ont fait choisir un modèle ou un paramètre particulier."),
           p("Nous vous souhaitons une bonne exploration !")),
  
  tabPanel("Exercice 1", # First exercise.
           titlePanel("Exercice 1 : le recours aux employés de maison"),
           
           mainPanel(
           
           fluidRow(column(12,p("Pour cet exercice, nous vous proposons d'étudier la régression logistique mise en
                                          place pour évaluer les critères d'emploi d'un employé de maison. Visualisons les 
                                probabilités conditionnelles selon le nombre de variables incluses dans le modèle."))),
           fluidRow(column(12,p(div(HTML("<b>Attention, le graphique peut mettre beaucoup de temps à s'afficher.</b>"))))),
                     
                     fluidRow(align="center",
                       column(12,wellPanel(
                         checkboxGroupInput(inputId="RegLog",
                                            label="Indiquez les variables à prendre en compte.",
                                            choices=colnames(trainData)[2:ncol(trainData)],
                                            selected=colnames(trainData)[2:ncol(trainData)],
                                            inline=T
                                            )))), # End of wellPanel's row containing the input.
           
           fluidRow(column(12,plotOutput(outputId = "ProC")))
           
           )), # End tabPanel from Exercice 1
  
  tabPanel("Exercice 2",
           titlePanel("Exercice 2 : températures dans les villes françaises"),
           
           
           
           mainPanel(align="center",fluidRow(
             
             column(12,h2("Partie I : l'analyse en composantes principales")),
             
             column(12,wellPanel(
               checkboxGroupInput(inputId="QuantiSupp",
                                  label="Indiquez les variables supplémentaires",
                                  choices=colnames(dataVilles),
                                  selected=colnames(dataVilles)[13:16],
                                  inline=T),
               radioButtons(inputId="Habillage",
                            label="Pour le graphique des individus statistiques, veuillez choisir le mois d'intérêt.",
                            choices = colnames(dataVilles)[1:12],
                            selected = colnames(dataVilles)[1:12][1],
                            inline=T))),
             
             column(6,plotOutput(outputId="PCAVar")),
             column(6,plotOutput(outputId = "PCAind")),
             
             column(12,h3("Question 1")),
             column(12,p("Ici, le plan factoriel explique 98.81% de l'inertie. Ainsi, les deux dimensions sont largement suffisantes pour une
             analyse cohérente vu qu'elle contiennent la quasi-totalité de l'information.")),
             column(12,h3("Question 2")),
             column(12,p("Tout d'abord, sur le cercle des corrélations, les corrélations sont positives et plutôt élevées, (comprises entre 0.6 et 0.9).
             Nous devinons que l'axe 1 séparera les villes froides (vers la gauche de l’axe), et les villes chaudes (vers la droite de l’axe) 
             les villes à gauches de l'axe 1 (avec de faibles coordonnées) auront des températures faibles au mois de mars. Celles vers le 
             centre de l'axe auront des températures moyennes en mars, et celles vers la droite auront des températures élevées au mois de mars. 
             Attention, il s'agira bien de températures relatives aux autres villes : quand on dit que la température d'une ville est faible, 
             c'est à dire qu'elle est faible comparativement aux autres villes.")),
             column(12,p("Pour l'axe 2, les corrélations du cercle de corrélations alternent entre approximativement -0.5 et +0.6.
              Nous devinons que cet axe concernera plutôt l’amplitude thermique au long de l’année : On en haut du graphique les villes 
                         où il fait chaud l’hiver et froid l’été, et en bas du graphique les villes où il fait chaud l’été et froid l’hiver. Notons 
                         encore une fois qu’il s’agira bien de températures relatives aux autres villes. Ainsi, en haut du graphique l’amplitude thermique des 
                         villes concernée sera faible, alors qu’en bas du graphique elle sera élevée.")),
             column(12,h3("Question 3")),
             column(12,p("Il s’agit de la meilleure représentation possible des individus avec notre jeu de données (98% d’inertie). Les individus « villes » sont
             bien représentés sur le premier plan factoriels car ils sont couchés sur le premier axe factoriel qui discrimine au mieux les observations.")),
             column(12,h3("Question 4")),
             column(12,p("Strasbourg et Lille semblent complètement opposés à Nice et Marseille sur le premier axe, leur températures moyennes sont très différentes tout au long de l’année. En effet, comme dit précédemment, l’axe 1 concerne les différences de températures.
              Il fera plutôt chaud tout au long de l’année à Nice et Marseille, alors qu’il fera plutôt froid à Lille et Strasbourg.")),
             column(12,p("Nous pouvons confirmer cette opposition en étudiant l’évolution de leurs températures selon les mois de l’année :
              Nous pouvons modifier l’habillage du graphique des individus pour nous donner une idée du comportement de la température en Janvier (pour représenter l’hiver) et en Juillet pour représenter l’été.
                         On conclut que Lille et Strasbourg s’opposent bien totalement à Montpellier et Marseille qui restent toujours chaudes tout au long de l’année (relativement aux autres villes).")),
             column(12,h3("Question 5")),
             column(12,p("Brest, Rennes et Nantes sont assez similaire comme vu dans les graphiques précédents : leurs températures sont très variables avec la saison (été ou hiver) : Il fera, relativement aux autres villes,  chaud en hiver et froid en été.
             On peut les opposer à Vichy Clermont, paris et Grenoble où il fera relativement plus chaud en été qu’en hiver.")),
             
             
             
             column(12,h2("Partie II : le clustering")),
             column(12,wellPanel(sliderInput(inputId="NClust",
                                             label="Choisissez le nombre de classes à prendre en compte dans l'algorithme des k-means : ",
                                             min=2,
                                             max=6,
                                             value=3
             ))),
             
             column(6,plotOutput(outputId="HCPlot"),),
             column(6,plotOutput(outputId = "KPlot")),
             column(12,h3("Question 1")),
             column(12,p("Il nous paraît naturel d’utiliser trois clusters pour l'algorithme des k-means. 
                         Cette classification correspond exactement à celle proposée par le clustering hiérarchique et elle demeure
                         cohérente. De plus, 3 classes semble être un bon compromis entre le nombre de clusters et la maximisation de la part de la 
                         variance inter-classes dans la variance totale.")),
             column(12,h3("Question 2")),
             column(12,p("Le dendogramme semble confirmer notre intuition avec les k-means : nous avons un cluster pour les villes du Sud, un cluster pour les villes du 
                         littoral breton et un dernier pour les villes soumises à un climat plus continental."))
             
           )) # Close fluidrow and Mainpanel from exercise 2
  
        ), # Close Tabpanel from exercise 2

  tabPanel("Exercice 3",
            titlePanel("Exercice 3 : Analyse des filières universitaires"),
           
            mainPanel(align="center",fluidRow(
                        column(12,wellPanel(
                              checkboxGroupInput(inputId="Supp",
                                                 label="Indiquez les variables supplémentaires",
                                                 choices=colnames(dataUniv),
                                                 selected=colnames(dataUniv)[7:12],
                                                 inline=T,
                                                 choiceNames = colnames(dataUniv), # Asks the user for supplementary variables
                                                 choiceValues = 1:ncol(dataUniv))))), # End of wellPanel
                              
                              
                        fluidRow(column(12,plotOutput(outputId = "ACGraph"))), # Plots the 2D corresponding analysis.
                        
                        
                        fluidRow(column(12,tableOutput(outputId = "cos21"))), # Shows cos2 from the 2D factorial plan
                        fluidRow(column(12,tableOutput(outputId = "cos22"))),
                        fluidRow(column(12,h3("Question 1"))),
                        fluidRow(column(12,p("Ici, le plan factoriel explique 86.23% de l'inertie. Ainsi, les deux dimensions du plan contiennent la grande majorité
                          de l'information."))),
                        fluidRow(column(12,h3("Question 2"))),
                        fluidRow(column(12,p("Les études de lettres et de langues sont bien représentées, de même que les femmes en licence. Pour les masters et les doctorats,
                                c'est un peu plus compliqué. Globalement, les points représentant les femmes et les études de langues sont assez proches.
                                Nous pouvons raisonnablement penser que les femmes sont plus attirées par les études de lettres, au moins en licence."))),
                        fluidRow(column(12,h3("Question 3"))),
                        fluidRow(column(12,p("Au total, les hommes sont très bien représentés sur le plan factoriel. Il en est de même pour chaque niveau de diplôme où les cos2
                                ne sont jamais en-dessous de 60%. Au niveau des sciences, seules les sciences humaines et les sciences fondamentales sont bien
                                représentées ; les sciences économiques le sont aussi un peu dans une certaine mesuree, mais la SVT est mal représentée."))),
                        fluidRow(column(12,p("Sur la comparaison Hommes/Sciences, ils sont très proches des sciences fondamentales et, dans une certaine mesure, des sciences
                                économiques (même si rappelons-le les sciences éco ne sont pas très bien représentées). Nous pouvons affirmer que les hommes sont
                                attirés, surtout au niveau master, par les sciences fondamentales. Par ailleurs, les hommes sont très éloignés de STAPS, mais tout
                                de même plus proches que les femmes."))),
                        fluidRow(column(12,h3("Question 4"))),
                        fluidRow(column(12,p("Le plan factoriel représente extrêmement bien les études d'AES (les cos2 sont presque égaux à 1 en les additionnant). Les licences
                                sont assez proches d'AES et extrêmement bien représentées aussi. Les masters dans leur ensemble sont bien représentés, tout comme
                                les doctorats, et ils sont tous les deux très éloignés de AES. Nous pouvons affirmer que ces études sont plutôt courtes."))),
                        fluidRow(column(12,h3("Question 5"))),
                        fluidRow(column(12,p("Le premier axe, de la dimension 1, pourrait séparer les études littéraires des études scientifiques : le point le plus à gauche
                                représente les études de langues et le point le plus à droite représente les sciences fondamentales. Le deuxième axe, lui,
                                pourrait séparer les études plutôt longues de celles plutôt courtes, comme en témoigne essentiellement le placement des points
                                Licence, Master et Doctorat sur le plan factoriel."))),
                        fluidRow(column(12,h3("Question 6"))),
                        fluidRow(column(12,p("On ne peut pas penser grand chose de la proximité entre ces deux points : les femmes en master sont mal représentées (~0.47)
                                et les études de SVT également (~0.42) : ces points ne sont pas interprétables et nous ne pouvons conclure à une supposée 
                                proximité."))),
                        fluidRow(column(12,h3("Question 7"))),
                        fluidRow(column(12,p("Lettres et arts sont extrêmement bien représentées (~0.91) et les femmes en licence également (~0.97). Il y a une très forte
                                proximité entre ces deux catégories.")))
                        
            ) # Close mainPanel from exercise 3
  
           
  ) # End of exercise 3
  
) # End of tabsetpanel  
  
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
  
  Ex2PCA <- function(){
    SupCol <- match(input$QuantiSupp,colnames(dataVilles))
    City.PCA <- PCA(dataVilles,quanti.sup=SupCol,graph=FALSE)
    return(City.PCA)
  }
  
  Ex2KM <- function(){
    Ngroup = input$NClust
    Km = kmeans(dataVilles,Ngroup)
    return(Km)
  }
  
  Ex1RegLog <- function(){ # Performs the logistic regression.
    vname = input$RegLog
    mod = glm(formula = DOMTRAV~.,data=trainData[,vname],family="binomial" )
    logit.coeffs <- data.frame(ID=rep(1:length(mod$coefficients)))
    logit.coeffs$Coefficients <- mod$coefficients
    logit.coeffs$Prob <- exp(logit.coeffs$Coefficients)/(1+exp(logit.coeffs$Coefficients))
    return(logit.coeffs)
  }
  
  output$ProC <- renderPlot({
    logit.coeffs = Ex1RegLog()
    attach(logit.coeffs)
    plot(ID[2:length(logit.coeffs$ID)],Prob[2:length(logit.coeffs$Prob)],
         xlab="Identifiant de la variable",ylab="Probabilité conditionnelle",
         main="Probabilités conditionnelles",ylim=c(0,1))
    detach(logit.coeffs)
  })
  
  output$ACGraph <- renderPlot({ 
    req(Ex3CA()) # Requests the function Ex3PCA in order to make the graph sent to the ACGraph component in ui.
     })
  
  output$cos21 <- renderTable( # Sent cos2 values from the factorial plan in the app
    {
      U = Ex3CA()
      t(rbind(U$row$cos2[,1:2],U$col$cos2[,1:2],U$col.sup$cos2[,1:2]))[,1:9]
    },rownames=TRUE
  )
  
  output$cos22 <- renderTable( # Second part of cos2
    {
      U = Ex3CA()
      t(rbind(U$row$cos2[,1:2],U$col$cos2[,1:2],U$col.sup$cos2[,1:2]))[,10:21]
    },rownames=TRUE
  )
  
  output$PCAVar <- renderPlot({
    
    U = Ex2PCA()
    plot(U,choix="var", habillage=match(input$Habillage,colnames(dataVilles)), cex=0.7)
  })
  
  output$PCAind <- renderPlot({
    U = Ex2PCA()
    plot(U,choix="ind", habillage=match(input$Habillage,colnames(dataVilles)), cex=0.7)
  })
  
  output$HCPlot <- renderPlot({
    hc <- hclust(dist(dataVilles),method='ward.D')
    plot(hc)
  })
  
  output$KPlot <- renderPlot({
    K <- Ex2KM()
    dataVilles2 = dataVilles
    dataVilles2$Clust = K$cluster
    colorsClust <- c("green","blue","red","black","purple","yellow")
    plot(x=dataVilles$longi,y=dataVilles$lati,main="Représentation des villes par position géographique et par 
         cluster",xlab="Longitude",ylab="Latitude",col=colorsClust[dataVilles2$Clust])
    text(dataVilles$longi, dataVilles$lati, labels=rownames(dataVilles), cex= 0.7, pos=2)
  })
  

} # End of server component
  
# https://vimeo.com/rstudioinc/review/131218530/212d8a5a7a/#t=51m
  
#############################################
#    We make the link between the user      #
#   interface and the server.That's the     #
#         end of our application.           #
#############################################
shinyApp(ui=ui,server=server)