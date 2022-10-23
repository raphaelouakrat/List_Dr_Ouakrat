
library (shiny)
library (shinydashboard)

ui = dashboardPage (
  # Header
  dashboardHeader (title = "Liste Passage Dr. OUAKRAT" 
                   # ,dropdownMenu (type = "notifications",
                   #               notificationItem (
                   #                 text = "message a mettre",
                   #                 icon ("users")
                   #               )
                   # )
  ),
  
  # Sidebar
  dashboardSidebar (
    sidebarMenu (
      menuItem ("Liste", tabName = "liste", icon = icon ("dashboard")),
      menuItem ("Widgets", tabName = "widgets", icon = icon ("th"))
    )
  ), 
  
  # Body
  dashboardBody (
    tabItems (
      # First tab content
      tabItem (tabName = "liste",
               fluidRow (
                 column (3), 
                 column (9, 
                         h2 (paste0 ("Liste de passage pour le ", 
                                     format (Sys.Date (), format = "%d/%m/%Y")))
                 )
               ), 
               
               fluidRow (
                box (textInput ("nom.patient", "Veuillez entrer votre nom"), 
                     width = 7), 
                column (3, 
                        selectInput (inputId = "numero_choisi", 
                                     label = "Quel est le rang souhaite ?", 
                                     choices = 1:25, 
                                     selected = 1)
                        ), 
                column (1, actionButton ("boutton", "Enregistrer"))
              ), 
              
              fluidRow (
                box (textOutput ("message"))
              ), 
              
              fluidRow (  
                column (1), 
                box (
                  title = "Liste",
                  dataTableOutput ("liste.actuelle"),
                  width = 10
                  )
                )
              ),
      
      # Second tab content
      tabItem (tabName = "widgets", 
               fluidRow (
                 column (3), 
                 column (6, 
                         h2 ("Onglet reserve au Medecin")
                 )
               ), 
               
               fluidRow (
                 passwordInput ("motdepasse", "Password"), 
                 actionButton ("boutton_medecin", "Valider")
               ), 
               
               uiOutput ("page_medecin")
      )
    )
  )
)

server = function (input, output) 
{
  output$liste.actuelle = renderDataTable (read.csv2 ("www/liste.csv"))
  
  observeEvent (input$boutton, 
                {
                  liste = read.csv2 ("www/liste.csv")
                  statut = read.csv2 ("www/mdp.csv")[1, 2]
                  if (statut == "OUVERT")
                  {
                    flag = T
                    
                    if (input$nom.patient == "")
                    {
                      flag = F
                      output$message = renderText ("Veuillez entrer un nom avant de cliquer sur le bouton Enregistrer")
                    }
                    
                    if (any (input$nom.patient %in% liste$Noms))
                    {
                      flag = F
                      output$message = renderText (paste0 ("Ce nom a deja un rendez vous de prevu au rang ", 
                                                           liste$Numero[which (liste$Noms == input$nom.patient)[1]]))
                    }
                    
                    if (flag)
                    {
                      buffer = which (!(1:25 %in% liste$Numero) & 1:25 >= input$numero_choisi)[1]
                      liste = rbind (liste, 
                                     data.frame (Numero = buffer, 
                                                 Noms = input$nom.patient))
                      liste = liste[order (liste$Numero), ]
                      write.csv2 (liste, "www/liste.csv", row.names = F)
                    }
                  }
                  
                  if (statut == "FERME")
                  {
                    output$message = renderText ("La liste est fermee pour l'instant, revenez plus tard")
                  }
                  output$liste.actuelle = renderDataTable (read.csv2 ("www/liste.csv"))
                }
  )
  
  observeEvent (input$boutton_medecin, 
                {
                  mdp = read.csv2 ("www/mdp.csv")[1, 1]
                  if (mdp == input$motdepasse)
                  {
                    output$page_medecin = renderUI (
                      list (
                        box (
                          fluidRow (
                            column (1), 
                            column (1, actionButton ("ouvrir", "ouvrir la liste")), 
                            column (1), 
                            column (1, actionButton ("fermer", "fermer la liste")), 
                            column (1), 
                            column (1, actionButton ("vider", "vider la liste")), 
                            column (1), 
                            column (1, actionButton ("inserer", "inserer un nom")), 
                            column (1), 
                            column (1, actionButton ("retirer", "retirer un nom"))
                          ), 
                          
                          fluidRow (
                            column (7,
                                    textInput ("nom.patient.inserer", "Nom a inserer")), 
                            column (1), 
                            column (3, 
                                    selectInput (inputId = "numero_inser_retir", 
                                                 label = "Quel est le rang a inserer ou a retirer ?", 
                                                 choices = 1:25, 
                                                 selected = 1))
                          ), 
                          
                          width = 12
                        )
                      )
                    )
                  }
                }
  )
  
  observeEvent (input$ouvrir, 
                {
                  mdp = read.csv2 ("www/mdp.csv")
                  mdp$statut = "OUVERT"
                  write.csv2 (mdp, "www/mdp.csv", row.names = F)
                  output$liste.actuelle = renderDataTable (read.csv2 ("www/liste.csv"))
                }
  )
  
  observeEvent (input$fermer, 
                {
                  mdp = read.csv2 ("www/mdp.csv")
                  mdp$statut = "FERME"
                  write.csv2 (mdp, "www/mdp.csv", row.names = F)
                  output$liste.actuelle = renderDataTable (read.csv2 ("www/liste.csv"))
                }
  )
  
  observeEvent (input$vider, 
                {
                  liste = read.csv2 ("www/liste.csv")
                  liste = liste[0, ]
                  write.csv2 (liste, "www/liste.csv", row.names = F)
                  output$liste.actuelle = renderDataTable (read.csv2 ("www/liste.csv"))
                }
  )
  
  observeEvent (input$inserer, 
                {
                  liste = read.csv2 ("www/liste.csv")
                  liste = rbind (data.frame (Numero = input$numero_inser_retir, 
                                             Noms = input$nom.patient.inserer), 
                                 liste)
                  liste = liste[order (liste$Numero), ]
                  while (length (unique (liste$Numero)) < nrow (liste))
                  {
                    buffer = as.numeric (names (table (liste$Numero))[which (as.numeric (table (liste$Numero)) > 1)][1])
                    buffer = which (liste$Numero == buffer)[2]
                    val = as.numeric (liste$Numero[buffer]) + 1
                    print (1)
                    liste$Numero[buffer] = val
                  }
                  write.csv2 (liste, "www/liste.csv", row.names = F)
                  output$liste.actuelle = renderDataTable (read.csv2 ("www/liste.csv"))
                }
  )
  
  observeEvent (input$retirer, 
                {
                  liste = read.csv2 ("www/liste.csv")
                  if (any (liste$Numero == input$numero_inser_retir))
                  {
                    liste = liste[-which (liste$Numero == input$numero_inser_retir), ]
                  }
                  write.csv2 (liste, "www/liste.csv", row.names = F)
                  output$liste.actuelle = renderDataTable (read.csv2 ("www/liste.csv"))
                }
  )
}

shinyApp (ui, server)
