library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(DT)
library(scales)

# ---- upload data
data_file <- "statconsul_full_data.xlsx"
transactions <- read_excel(data_file, sheet = "Transactions")
personnel <- read_excel(data_file, sheet = "Personnel")
clients <- read_excel(data_file, sheet = "Clients")

# Convert date into classes
transactions <- transactions %>%
  mutate(Date = as.Date(Date))

# Helper to format in millions "x.xx M"
fmt_M <- function(x) {
  if (is.na(x) || length(x)==0) return(NA_character_)
  paste0(ifelse(abs(x) >= 1e6, round(x/1e6, 2), round(x/1e3,1)), ifelse(abs(x) >= 1e6, " M", " k"))
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "DeepStat-Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Analyse", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Données", tabName = "data", icon = icon("table")),
      menuItem("AI Agent", tabName = "robot", icon = icon("robot"))
    ),
    hr(),
    dateRangeInput("date_range", "Période", start = min(transactions$Date), end = max(transactions$Date)),
    selectInput("activite", "Activité", choices = c("All", sort(unique(transactions$Activite))), selected = "All"),
    selectInput("lieu", "Lieu", choices = c("All", sort(unique(transactions$Lieu))), selected = "All"),
    sliderInput("montant", "Montant (FCFA)", min = min(transactions$Montant), max = max(transactions$Montant),
                value = c(min(transactions$Montant), max(transactions$Montant)), step = 1000)
  ), # End of the sidebar 
  dashboardBody(
   # tags$head(tags$style(HTML('.small-box {height: 110px} .value-box-label {font-size:13px}'))),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("vb_total_amount", width = 3),
                valueBoxOutput("vb_n_ops", width = 3),
                valueBoxOutput("vb_n_clients", width = 3),
                valueBoxOutput("vb_n_personnel", width = 3)
              ),
              fluidRow(
                box(title = "Montant par activité", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("plot_activity_pie", height = "330px"), collapsible = TRUE, collapsed = FALSE),
                box(title = "Montant par lieu (top)", status = "primary", solidHeader = TRUE, width = 6,
                    plotlyOutput("plot_location_bar", height = "330px"))
              ),
              fluidRow(
                box(title = "Carte des opérations", status = "success", solidHeader = TRUE, width = 12,
                    leafletOutput("map_ops", height = 420), collapsible = TRUE, collapsed = FALSE)
              )
      ),
      tabItem(tabName = "analysis",
              fluidRow(
                box(title = "Évolution temporelle des montants", width = 12, status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_time", height = "350px"))
              ),
              fluidRow(
                box(title = "Répartition Sexes (Clients/Personnel)", width = 6, status = "info", solidHeader = TRUE,
                     plotlyOutput("plot_sex_client_personnel", height = "350px")),
                box(title = "Top 10 clients par montant", width = 6, status = "info", solidHeader = TRUE,
                    plotlyOutput("plot_top_clients", height = "350px"))
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Table transactions (filtrable)", width = 12, status = "warning", solidHeader = TRUE,
                    DTOutput("dt_transactions"))
              ),
              fluidRow(
                box(title = "Personnels", width = 6, status = "warning", solidHeader = TRUE,
                    DTOutput("dt_personnel")),
                box(title = "Clients", width = 6, status = "warning", solidHeader = TRUE,
                    DTOutput("dt_clients"))
              )
      ),
      tabItem(
        tabName = "robot",
        fluidRow(
          box(title="Agent conversationnel de DeepStat", width=12, status="info", solidHeader = TRUE, collapsible=TRUE, collapsed = FALSE,
              textInput(inputId = "user_input", "Posez votre question", "Qu'est ce que DeepStat ?"),
              actionButton(inputId = "send_button",label = "Envoyer", width = "80px", icon = icon("paper-plane"), class = "btn-success"),
              br(),
              br(),
              br(),
              hr(),
              textOutput("text_ai"))
        )
      )
    )
  ) # End of the body
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered <- reactive({
    df <- transactions
    # date range: ensure Date class
    start <- as.Date(input$date_range[1])
    end <- as.Date(input$date_range[2])
    if (!is.na(start) && !is.na(end)) {
      df <- df %>% filter(Date >= start & Date <= end)
    }
    if (!is.null(input$activite) && input$activite != "All") {
      df <- df %>% filter(Activite == input$activite)
    }
    if (!is.null(input$lieu) && input$lieu != "All") {
      df <- df %>% filter(Lieu == input$lieu)
    }
    df <- df %>% filter(Montant >= input$montant[1], Montant <= input$montant[2])
    df
  })
  
  # Value boxes
  output$vb_total_amount <- renderValueBox({
    total_amt <- sum(filtered()$Montant, na.rm = TRUE)
    valueBox(
      value = fmt_M(total_amt),
      subtitle = "Montant total",
      icon = icon("coins"),
      color = "green"
    )
  })
  output$vb_n_ops <- renderValueBox({
    valueBox(
      value = format(nrow(filtered()), big.mark = " "),
      subtitle = "Nombre d'opérations",
      icon = icon("tasks"),
      color = "blue"
    )
  })
  output$vb_n_clients <- renderValueBox({
    valueBox(
      value = format(length(unique(filtered()$Client)), big.mark = " "),
      subtitle = "Clients distincts",
      icon = icon("handshake"),
      color = "purple"
    )
  })
  output$vb_n_personnel <- renderValueBox({
    valueBox(
      value = format(length(unique(filtered()$Personnel)), big.mark = " "),
      subtitle = "Personnel",
      icon = icon("users"),
      color = "yellow"
    )
  })
  
  # Pie: montant by activity
  output$plot_activity_pie <- renderPlotly({
    df <- filtered() %>% group_by(Activite) %>% summarise(Montant = sum(Montant, na.rm=TRUE)) %>% arrange(desc(Montant))
    if(nrow(df)==0) return(NULL)
    plot_ly(df, labels=~Activite, values=~Montant, type='pie') %>% layout(margin=list(l=0,r=0,t=30,b=0))
  })
  
  # Bar: montant by location
  output$plot_location_bar <- renderPlotly({
    df <- filtered() %>% group_by(Lieu) %>% summarise(Montant = sum(Montant, na.rm=TRUE)) %>% arrange(desc(Montant))
    if(nrow(df)==0) return(NULL)
    df_top <- head(df, 10)
    plot_ly(df_top, x=~reorder(Lieu, Montant), y=~Montant, type='bar', marker=list(color='steelblue')) %>%
      layout(xaxis=list(title="Lieu"), yaxis=list(title="Montant"))
  })
  
  # Map
  output$map_ops <- renderLeaflet({
    df <- filtered()
    if(nrow(df)==0){
      # empty map
      leaflet() %>% addTiles()
    } else {
      leaflet(df) %>% addTiles() %>%
        addCircleMarkers(~Longitude, ~Latitude,
                         radius = ~pmin(30, sqrt(Montant)/500),
                         color = "red",
                         fillOpacity = 0.6,
                         popup = ~paste0("<b>", Client, "</b><br>",
                                         "Montant: ", format(Montant, big.mark=" "), " FCFA<br>",
                                         "Activité: ", Activite, "<br>",
                                         "Date: ", Date))
    } 
  })
  
  # Time series
  output$plot_time <- renderPlotly({
    df <- filtered() %>% group_by(Date) %>% summarise(Montant = sum(Montant, na.rm=TRUE)) %>% arrange(Date)
    if(nrow(df)==0) return(NULL)
    plot_ly(df, x=~Date, y=~Montant, type='scatter', mode='lines+markers') %>%
      layout(yaxis=list(title="Montant (FCFA)"))
  })
  
  # Sex distribution plot (clients vs personnel)
  output$plot_sex_client_personnel <- renderPlotly({
    # clients distribution (from Clients sheet)
    cl <- clients %>% group_by(Sexe) %>% summarise(n = n())
    per <- personnel %>% group_by(Sexe) %>% summarise(n = n())
    dfc <- cl %>% mutate(Source = "Clients") %>% rename(Sex=Sexe)
    dfp <- per %>% mutate(Source = "Personnel") %>% rename(Sex=Sexe)
    dfb <- bind_rows(dfc %>% rename(count = n), dfp %>% rename(count = n))
    plot_ly(dfb, x=~Source, y=~count, color=~Sex, type='bar', barmode='stack') %>%
      layout(yaxis=list(title="Nombre"))
  })
  
  # Top 10 clients by montant (from filtered transactions)
  output$plot_top_clients <- renderPlotly({
    df <- filtered() %>% group_by(Client) %>% summarise(Montant = sum(Montant, na.rm=TRUE)) %>% arrange(desc(Montant))
    if(nrow(df)==0) return(NULL)
    df_top <- head(df, 10)
    plot_ly(df_top, x=~Montant, y=~reorder(Client, Montant), type='bar', orientation='h', marker=list(color='darkorange')) %>%
      layout(xaxis=list(title="Montant (FCFA)"), yaxis=list(title="Client"))
  })
  
  # DataTables
  output$dt_transactions <- renderDT({
    df <- filtered() %>% arrange(desc(Date))
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  output$dt_personnel <- renderDT({
    datatable(personnel, options = list(pageLength = 10))
  })
  output$dt_clients <- renderDT({
    datatable(clients, extensions = "Buttons", options = list(
      dom = 'Bfrtip',       # boutons + filtre + tableau
      pageLength = 10,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')  # les boutons
      ))
  })
  
  # Response of the AI agent
  ###############################################################
  
  # Call the API
  call_ai_api <- function(history){
    api_key = "" # renseigner votre clé d'api
    if(identical(api_key,"")){
      stop("Veuillez rensigner votre clé d'api !!!")
    }
    
    response <- httr::POST(
      url = "https://api.groq.com/openai/v1/chat/completions",
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(
        list(
          model = "llama3-8b-8192",
          messages = history,
          temperature = 0.8
        ),
        auto_unbox = TRUE
      ),
      encode = "json"
    )
    
    if(httr::status_code(response) != 200){
      return("Erreur !!! \n Veuillez vérifier votre requête, ou essayer à nouveau plus tard.")
    }
    
    # Get the content of the response
    parsed_resp <- httr::content(response, as = "parsed", encoding = "UTF-8")
    parsed_resp$choices[[1]]$message$content
  }
  
  
  ### format the user's request
  send_user_input <- function(user_input, recent_history = NULL){
    ## TODO: add a specific context with user role system to tell the agent who he is
    
    context <- "Tu es l'assistant intelligent de la structure DeepStat. 
    DeepStat est un bureau d'étude basé dans la ville de Yaoundé au Cameroun. 
    Elle propose différents services notamment la réalisation d'études statistiques, 
    les enquêtes, des formations en rapport avec la data, des benchmarks et autres.
    Tu as été intégré dans ce tableau de bord qui présente globalement la situation des 
    différentes ventes de la structure depuis sa création en 2013 et également la gestion de son personnel.
    Ton rôle est de répondre aux différentes questions des utilisateurs concernant la structure, ses services, et son fonctionnement.
    Réponds de manière professionnelle et claire.
    "
    
    # add the context as a system message
    history <- list(list(role="system", content = context))
    
    #history <- list(list(role="user", content = user_input))
    # add the user's message
    history <- append(history, list(list(role="user", content = user_input)))
    
    # Get the response
    api_resp <- call_ai_api(history)
    
    # return the response and the updated history
    list(
      response = api_resp,
      updated_history = append(history, list(list(role="assistant", content=api_resp)))
    )
  }
  
  # Hear for a click on the send btn
  observeEvent(input$send_button, {
    question <- input$user_input
    
    if(nchar(question) > 0){
     
      result <- send_user_input(question)
      
      response <- result$response
      
      output$text_ai <- renderText({
        response
      })
      
      # clear the field
      updateTextInput(session, "user_input", value = "")
      
    }
  })
  
 
}

shinyApp(ui, server)
