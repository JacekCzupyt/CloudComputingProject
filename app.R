library(shiny)
library(tidyverse)
library(jsonlite)
library(shinythemes)
library(lubridate)
library(dplyr)
library(png)
library(ggplot2)
library(ggjoy)
library(shinyjs)
options(stringsAsFactors = FALSE)

library(DBI)
library(RPostgres)

library(paws.management)
library(httr)

region = content(GET("http://169.254.169.254/latest/meta-data/placement/region"))
Sys.setenv(AWS_REGION = region)

ssm_ps <- ssm()

l <- ssm_ps$get_parameters_by_path("/rds")$Parameters
l <- lapply(l, function(x) c(x$Name, x$Value))
names(l) <- lapply(l, function(x) x[1])
l <- lapply(l, function(x) x[2])

db_table_name = l[["/rds/table_name"]]


conn <- dbConnect(
  drv = RPostgres::Postgres(),
  dbname = l[["/rds/database_name"]],
  host = l[["/rds/database_endpoint"]],
  port= l[["/rds/database_port"]],
  user = l[["/rds/database_user"]],
  password = l[["/rds/database_password"]])


if(!dbExistsTable(conn, db_table_name)){
  table_columns <- c('id', 'userid', 'updatedat', 'name', 'content')
  table_columns_types <- c('serial', 'int', 'timestamp default current_timestamp', 'varchar(255)', 'jsonb')
  names(table_columns_types) <- table_columns
  dbCreateTable(conn, db_table_name, table_columns_types)
}
print(dbGetQuery(conn, str_interp("SELECT id, updatedat, name FROM ${db_table_name} WHERE userid = 42")))

ui <- fluidPage(theme = shinytheme("yeti"),
                
                useShinyjs(),
                sidebarLayout(
                  sidebarPanel(width=4,
                               titlePanel("SpotiData - sprawdź swoje statystyki słuchania platformy Spotify", windowTitle = "SpotiData"),
                               hr(),
                               actionButton("zima", "Zima", width = "110px"),
                               actionButton("wiosna", "Wiosna", width = "110px"),
                               br(),
                               actionButton("lato", "Lato", width = "110px"),
                               actionButton("jesien", "Jesień", width = "110px"),
                               br(),
                               actionButton("resetdat", "Reset dat", width = "224px"),
                               # hr(),
                               # dateRangeInput("daterange1", "Zakres dat:",
                               #                start = "2018-12-01",
                               #                end   = "2020-01-31",
                               #                language = "pl",
                               #                weekstart = 1,
                               #                separator = " do "),
                               hr(),
                               fileInput('files', 'Załaduj swoje dane:', multiple = TRUE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv", ".json"),
                                         buttonLabel = "Wgraj pliki",
                                         placeholder = "Brak pliku",
                                         width = "220px"),
                               tags$script('
                                     pressedKeyCount = 0;
                                     $(document).on("keydown", function (e) {
                                      Shiny.onInputChange("pressedKey", pressedKeyCount++);
                                      Shiny.onInputChange("key", e.which);
                                      });'),
                               actionButton("save_file", "Zapisz plik", width = '100px', disabled=T),
                               hr(),
                               uiOutput('file_list'),
                               hr(),
                               htmlOutput("Opis")
                  ),
                  mainPanel(
                    plotOutput("distPlot",click="click", height = "672px", hover = "hover")
                  )
                )
)

server <- function(input, output, session) {
  
  
  selected_spotidane <- reactiveValues(    #po prostu zbior wartosci reaktywnych - nie sugeruj sie nazwa
    selected = character(),     #nazwa artysty zebrana przez klikniecie na pierwszym wykresie
    x1  = tibble(),      #ramka danych tworzona do przedstawienia pierwszego wykresu
    choices  = tibble(),  #zbior wszystkich artystow, ktorzy byli wyswietlani 
    clicked = numeric(),   #potrzebne do sczytania wspolrzednej y klikniecia mysza na pierwszym wykresie
    click = FALSE, # flaga potrzebna do klikania i odklikiwania artystow - wazna!! decyduje ktory wykres sie wyswietla 
    comeback_possible = FALSE, # do powrotu z drugiego wykresu
    maxvalue = numeric(), #tez do powrotu - okresla polozenie guzika
    begin_date = date("2018-01-01"),   #chyba jasne - paczatkowa data zakresu
    end_date = date("2022-12-31"),   #koncowa
    arrow_index = 0,   #wylicza jaka wartosc nalezy dodać do '1:20' aby byli wyswieltani artysci z zakresu 1+array_index:20+array_index
    keep_range = TRUE,  #czy trzymac zakres skali na pierwszym wykresie
    get_range = TRUE, # czy pobrac zakres skali z pierwszego wykresu
    max_value = numeric(),#potrzebne do okreslenia rozpietosci pierwszego wykresu
    twentieth = numeric(), # do okreslenia na jakiej wysokosci ma byc obrazek
    first_plot = TRUE, #flaga uzyta tylko przy pierwszym wczytywaniu wykresu
    x = FALSE,  #za duzo tych flag, ale dziala - potrzebne bo przy pierwszym wczytaniu plikow zmienia sie daterange, co utrudnia
    window = c(FALSE, TRUE, FALSE), #ktore okno na dole ma sie wyswietlic?
    hover_read = FALSE,   # czy zczytywać dane z hovera - true gdy ma to sens
    hover = c(-0.3, -0.3),  # dane odnosnie myszki
    test = character()
  )
  
  spotidane <-reactiveValues(  #tez nie sugerowac sie nazwa: reaktywne tylko ze inne 
    data = data.frame(),   #ramka danych zawierajaca informacje z wczytanych plikow
    toBind = data.frame()  #tymczasowa ramka wykorzystywana przy wczytywaniu danych
  )
  
  #### obserwatorzy inputow
  observeEvent(input$zima, {
    selected_spotidane$begin_date <- date(format(date("2018-12-22"),"%Y-%m-%d"))
    selected_spotidane$end_date <- date(format(date("2019-03-20"),"%Y-%m-%d"))
    updateDateRangeInput(session, "daterange1", start =  date("2018-12-22"), end = date("2019-03-20"))
    selected_spotidane$arrow_index <- 0
    if(!selected_spotidane$x){
      selected_spotidane$x <- TRUE
    }
    else{
      selected_spotidane$get_range <- TRUE
      selected_spotidane$keep_range <- FALSE
    }
    
  })
  observeEvent(input$wiosna, {
    selected_spotidane$begin_date <- date("2019-03-21")
    selected_spotidane$end_date <- date("2019-06-30")
    updateDateRangeInput(session, "daterange1", start =  date("2019-03-21"), end = date("2019-06-21"))
    selected_spotidane$arrow_index <- 0
    if(!selected_spotidane$x){
      selected_spotidane$x <- TRUE
    }
    else{
      selected_spotidane$get_range <- TRUE
      selected_spotidane$keep_range <- FALSE
    }
  })
  observeEvent(input$lato, {
    selected_spotidane$begin_date <- date(format(date("2019-07-01"),"%Y-%m-%d"))
    selected_spotidane$end_date <- date(format(date("2019-09-30"),"%Y-%m-%d"))
    updateDateRangeInput(session, "daterange1", start =  date("2019-06-22"), end = date("2019-09-22"))
    selected_spotidane$arrow_index <- 0
    if(!selected_spotidane$x){
      selected_spotidane$x <- TRUE
    }
    else{
      selected_spotidane$get_range <- TRUE
      selected_spotidane$keep_range <- FALSE
    }
  })
  observeEvent(input$jesien, {
    selected_spotidane$begin_date <- date("2019-10-01")
    selected_spotidane$end_date <- date("2019-12-31")
    updateDateRangeInput(session, "daterange1", start =  date("2019-09-23"), end = date("2019-12-21"))
    selected_spotidane$arrow_index <- 0
    if(!selected_spotidane$x){
      selected_spotidane$x <- TRUE
    }
    else{
      selected_spotidane$get_range <- TRUE
      selected_spotidane$keep_range <- FALSE
    }
  })
  observeEvent(input$resetdat, {
    selected_spotidane$begin_date <- date("2018-12-01") 
    selected_spotidane$end_date <- date("2020-01-31")
    updateDateRangeInput(session, "daterange1", start =  date("2018-12-01"), end = date("2020-01-31"))
    selected_spotidane$arrow_index <- 0
    if(!selected_spotidane$x){
      selected_spotidane$x <- TRUE
    }
    else{
      selected_spotidane$get_range <- TRUE
      selected_spotidane$keep_range <- FALSE
    }
  })
  
  observeEvent(input$daterange1, {
    selected_spotidane$begin_date <- input$daterange1[1]
    selected_spotidane$end_date <- input$daterange1[2]
    selected_spotidane$arrow_index <- 0
    if(!selected_spotidane$x){
      selected_spotidane$x <- TRUE
    }
    else{
      selected_spotidane$get_range <- TRUE
      selected_spotidane$keep_range <- FALSE
    }
    
  })
  observeEvent(input$pressedKey, {
    
    if(!selected_spotidane$click){
      if(input$key %in% c(38, 40)){
        tmp <- selected_spotidane$arrow_index + input$key -39
        if(tmp >=0){
          selected_spotidane$arrow_index <- tmp
          selected_spotidane$first_plot <- FALSE
          selected_spotidane$get_range <- FALSE
          selected_spotidane$keep_range <- TRUE
        }
      }
      
    }
    else{
      if(input$key == 39){ 
        if(which(selected_spotidane$window)==1){
          selected_spotidane$window <- c(FALSE, TRUE, FALSE)
        }
        else if(which(selected_spotidane$window)==2){
          selected_spotidane$window <- c(FALSE, FALSE, TRUE)
        }
      }
      else if(input$key == 37){
        if(which(selected_spotidane$window)==3){
          selected_spotidane$window <- c(FALSE, TRUE, FALSE)
        }
        else if(which(selected_spotidane$window)==2){
          selected_spotidane$window <- c(TRUE, FALSE, FALSE)
        }
      }
    }
    
  })
  
  observeEvent(input$hover,{
    if(selected_spotidane$hover_read){
      selected_spotidane$hover[1] <- input$hover$x
      selected_spotidane$hover[2] <- input$hover$y
    }
  })
  
  observeEvent(input$click,{
    if(!selected_spotidane$click) {#w przypadku gdy wyswietlany jest 1. wykres
      selected_spotidane$click = TRUE
      #zapisanie wykonawcy jaki ma byc wyswietlany w drugim oknie - o ile click$y występuje
      selected_spotidane$clicked[1] <-ifelse(!is.null(input$click$y), input$click$y, selected_spotidane$clicked[1])
    }
    else{#gdy drugi wykres jest wyswietlany, zbieramy klikniecie dla przycisku powróć
      if(which(selected_spotidane$window)==1){
        if(input$click$x>0 && input$click$x<3){
          if(input$click$y<selected_spotidane$maxvalue*1.08 && input$click$y>selected_spotidane$maxvalue*1.02){
            selected_spotidane$click = FALSE
            selected_spotidane$window = c(FALSE, TRUE, FALSE) #reset informacji ktory wykres ma byc wyswietlony na drugim poziomie
          }
        }
        
        
      }
      else if(which(selected_spotidane$window)==2){
        
        
      }
      else if(which(selected_spotidane$window)==3){
        
      }
    }
    
    selected_spotidane$first_plot <- FALSE
  })
  ######
  observeEvent(input$save_file,{
    tryCatch({
      # browser()
      file_data = as.character(toJSON(spotidane$data))
      
      dbAppendTable(conn, db_table_name, data.frame(userid=42, name=input$files$name, content=file_data))
      session$reload()
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
  })
  
  ######
  # logika wczytywania plikow
  #
  observeEvent(input$files,{
    tryCatch(
      {
        l <- loadFile(input$files, spotidane, selected_spotidane)
        spotidane <- l$spotidane
        selected_spotidane <- l$selected_spotidane
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    #####
    #Wczytywanie obu wykresow - ggplot
    
    output$distPlot <- plotrender(spotidane, selected_spotidane)
  })
  output$Opis <- renderUI({
    req(input$files)
    if(!selected_spotidane$click){
      HTML(paste("WSKAZÓWKA:", "Użyj strzałek na klawiaturze", "   - sprawdź co się stanie!", sep="<br/>"))
    }
    else{
      if(selected_spotidane$window[1]){HTML(paste("WSKAZÓWKA:", "Aby dowiedzieć się więcej,", "najedź na punkt", sep="<br/>"))} #dla wykresu Pawła
      else{
        
        HTML(paste("WSKAZÓWKA:", "Użyj strzałek na klawiaturze", "   - tym razem na boki!", sep="<br/>"))
      }
    }
  })
  
  output$file_list <- renderUI({

    file_list_item <- function(x){
      observeEvent(input[[paste0("button", x['id'])]], {
        loadFile(dbGetQuery(conn, str_interp('SELECT content FROM ${db_table_name} WHERE id = ${x["id"]}')), spotidane, selected_spotidane)
        output$distPlot <- plotrender(spotidane, selected_spotidane)
      })

      observeEvent(input[[paste0("delete", x['id'])]], {
        dbSendStatement(conn, str_interp('DELETE FROM ${db_table_name} WHERE id = ${x["id"]}'))
        session$reload()
      })
      tags$tr(
        tags$td(
          actionLink(paste0("button", x['id']), str_interp(paste("${x['name']}", "${x['updatedat']} (UTC)", sep="\n")))
        ),
        tags$td(
          actionButton(paste0("delete", x['id']), "", icon = icon("trash"))
          )
      )
    }

    tags$table(
      apply(dbGetQuery(
        conn, str_interp("SELECT id, updatedat, name FROM ${db_table_name} WHERE userid = 42")
      ), 1, function(x) file_list_item(x))
    )
  })
  
  
}


shinyApp(ui = ui, server = server)


