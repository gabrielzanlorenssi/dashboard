#--- libraries
library(shinydashboard)
library(shiny)
library(tidyverse)

#--- data
escolas <- read_csv("data/escola_segura.csv")

options1 <- c("Caruaru (PE)", "Porto Velho (RO)", "São Paulo (SP)")
options2 <- c("Todas as escolas", unique(escolas$`Qual o nome da sua escola? (EXEMPLOS)`))

texto = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed lacus ante, mattis sit amet rutrum et, ornare ac nibh. Fusce et faucibus purus. \n Sed maximus mollis quam sed ornare. Aliquam laoreet tortor et imperdiet faucibus. Nam vel diam vel sapien fermentum condimentum. Integer porta sodales risus. Nulla placerat ultrices neque id volutpat. Phasellus at massa nec diam gravida facilisis. Nulla eros elit, varius at nibh eget, accumsan sodales tortor. Donec finibus at orci eget accumsan. Praesent purus turpis, placerat sit amet ligula non, tempor aliquet nulla. Suspendisse accumsan diam euismod eros convallis, a posuere libero hendrerit. Ut congue metus non aliquam porttitor.
Cras ultricies, nisl a sodales fermentum, nisl felis luctus ante, vitae consectetur neque nibh in ex. Cras condimentum sapien sit amet dui vestibulum cursus. \n Nullam quam odio, condimentum euismod nunc eget, porta sollicitudin tortor. Vivamus eget nulla nisi. Nullam sed dignissim metus, at condimentum libero. In eu rutrum nulla. Vivamus dapibus ipsum id urna pretium auctor. Phasellus turpis odio, tempus eget consectetur pellentesque, suscipit sed nibh. In ac sapien sodales, vulputate sapien at, pulvinar nisi. Interdum et malesuada fames ac ante ipsum primis in faucibus. In vitae ex hendrerit tellus interdum dignissim. Nullam volutpat nibh in molestie tincidunt. Donec iaculis dolor tellus, vel tincidunt dolor tempus in. Nulla feugiat leo vitae porta ultricies."

#--- dashboard

dashboardPage(
    skin="green",
    #-- header
    dashboardHeader(title="Escola Segura - Instituto Gesto", titleWidth = 450),
    #-- sidebar
    dashboardSidebar(menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                     selectInput("variable1", "Selecione o município:",
                                 options1),
                     selectInput("variable2", "Selecione a escola:",
                                 options2),
                     sliderInput("slider", "Slider", 1, 100, 50),
                     menuItem("Download", tabName = "download", icon = icon("th")),
                     menuItem("Como usar", tabName = "instructions", icon = icon("dashboard"))),
    #-- body
    dashboardBody(
        tabItems(tabItem(tabName = "dashboard",
                h2("Caruaru (PE)"),
        fluidRow(
            # A static valueBox
            valueBox(36, "Casos de covid-19 em 2021", icon = icon("credit-card")),
            
            # Dynamic valueBoxes
            valueBoxOutput("progressBox"),
            
            valueBoxOutput("approvalBox")
        )),
        tabItem(tabName = "download",
                h2("Downloads"),
                radioButtons('format', 'Formato do documento', c('PDF', 'HTML', 'Word'),
                             inline = TRUE),
                downloadButton("downloadReport", "Baixe um relatório do município"),
                br(),
                radioButtons('formatDoc', 'Formato da tabela', c('Excel', 'CSV'),
                             inline = TRUE),
                downloadButton("downloadDatabase", "Baixe os dados do município como tabela")),
        tabItem(tabName = "instructions",
                h2("Como usar"),
                p(texto))
    )))
