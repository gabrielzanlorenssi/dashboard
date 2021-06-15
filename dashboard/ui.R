#--- libraries
library(shinydashboard)
library(shiny)
library(tidyverse)
library(shinymanager)
library(plotly)
library(DT)

#--- data
escolas <- read_csv("./data/escola_segura.csv")

options1 <- c("Caruaru (PE)", "Porto Velho (RO)", "São Paulo (SP)")
options2 <- c("ESCOLA MUNICIPAL JOÃO E MARIA", unique(escolas$`Qual o nome da sua escola? (EXEMPLOS)`))

texto = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed lacus ante, mattis sit amet rutrum et, ornare ac nibh. Fusce et faucibus purus. \n Sed maximus mollis quam sed ornare. Aliquam laoreet tortor et imperdiet faucibus. Nam vel diam vel sapien fermentum condimentum. Integer porta sodales risus. Nulla placerat ultrices neque id volutpat. Phasellus at massa nec diam gravida facilisis. Nulla eros elit, varius at nibh eget, accumsan sodales tortor. Donec finibus at orci eget accumsan. Praesent purus turpis, placerat sit amet ligula non, tempor aliquet nulla. Suspendisse accumsan diam euismod eros convallis, a posuere libero hendrerit. Ut congue metus non aliquam porttitor.
Cras ultricies, nisl a sodales fermentum, nisl felis luctus ante, vitae consectetur neque nibh in ex. Cras condimentum sapien sit amet dui vestibulum cursus. \n Nullam quam odio, condimentum euismod nunc eget, porta sollicitudin tortor. Vivamus eget nulla nisi. Nullam sed dignissim metus, at condimentum libero. In eu rutrum nulla. Vivamus dapibus ipsum id urna pretium auctor. Phasellus turpis odio, tempus eget consectetur pellentesque, suscipit sed nibh. In ac sapien sodales, vulputate sapien at, pulvinar nisi. Interdum et malesuada fames ac ante ipsum primis in faucibus. In vitae ex hendrerit tellus interdum dignissim. Nullam volutpat nibh in molestie tincidunt. Donec iaculis dolor tellus, vel tincidunt dolor tempus in. Nulla feugiat leo vitae porta ultricies."

textoDownload = HTML(paste0("<p>Nesta seção, é possível baixar os gráficos em formato de relatório, que pode ser impresso.<br>",
                     "Os dados também podem ser baixados em formato de tabela."))

#--- inactivity
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

#--- credenciais
credentials <- data.frame(
    user = c("admin", "caruaru", "porto_velho", "g"),
    password = c("123456", "123456", "123456", "1"), 
    admin = c(T, F, F, F),
    stringsAsFactors = FALSE
)

#--- dashboard

secure_app(language="pt-BR", head_auth = tags$script(inactivity),
           dashboardPage(
    skin="green",
    #-- header
    dashboardHeader(title="Escola Segura - Instituto Gesto", titleWidth = 450),
    #-- sidebar
    dashboardSidebar(br(),
                     sidebarMenu(
                     menuItem("Visão do município", tabName = "dashboard", icon = icon("city")),
                     menuItem("Visão da escola", tabName = "escola", icon = icon("school")),
                     #selectInput("variable1", "Selecione o município:",
                     #            options1),
                     selectInput("variable2", "Selecione a escola:",
                                 options2),
                     # dateRangeInput('dateRange',
                     #                label = 'Selecione uma data (opcional):',
                     #                start = as.Date("2021-01-01"),
                     #                format = "dd/mm/yy",
                     #                language = "pt-BR",
                     #                startview = "month",
                     #                end = Sys.Date()
                     # ),
                     br(),
                     menuItem("Download", tabName = "download", icon = icon("download")),
                     br(),
                     menuItem("Como usar", tabName = "instr", icon = icon("question-circle")))),
    #-- body
    dashboardBody(
        tabItems(tabItem(tabName = "dashboard",
                h2("Caruaru (PE)"),
        fluidRow(
            # A static valueBox
            valueBox(360, "Casos registrados em escolas"),
            
            # Dynamic valueBoxes
            valueBoxOutput("progressBox"),
            
            valueBoxOutput("approvalBox")
        ),
        fluidRow(
            box(
                title = textOutput("text_user"), status = "warning", solidHeader = TRUE,
                p("População: 356.000"),
                p("Número de escolas: 356"),
                p("Número de estudantes: 15.680"),
                p("Número de funcionários e professores: 1.325"),
                p("Casos no município: 15.607"),
                p("Média móvel nos últimos sete dias: 35 casos"),
                p("Variação nos últimos sete dias: +15%")
            )
           
        ),
        fluidRow(box(plotlyOutput('plot1'), title="Casos por local da escola"),
                box(plotlyOutput('plot2'), title="Casos por membro da comunidade")),
        fluidRow(box(plotlyOutput('plot4'), title="Casos diários: local"),
                 box(plotlyOutput('plot3'), title="Casos diários: membro")),
        h3('Casos por escola:'),
        radioButtons(inputId="radio1", label="", choices=c("Números absoultos", "Valores percentuais")),
        fluidRow(DTOutput('tbl'))),
        tabItem(tabName = "escola",
                h2("NOME DA ESCOLA AQUI"),
                fluidRow(valueBox(25, "Casos registrados na escola"),
                         valueBox(5, "Nos últimos 14 dias"),
                         valueBox("36%", "de adesão no preenchimento")),
                fluidRow( box(
                    title = "Visão da escola", status = "danger", solidHeader = TRUE,
                    strong(p("ESCOLA ESTADUAL GRACCHO CARDOSO")),
                    strong(p("Escola Rural")),
                    p("Total de estudantes: 867"),
                    p("Total de funcionários e professores: 85"),
                    p("Nota no IDEB: 6,5"),
                    p("Total de casos: 55 casos"),
                    p("Casos nos últimos 14 dias: 10 casos")
                ))), 
        #-- downloads
        tabItem(tabName = "download",
                h2("Download dos dados"),
                br(),
                br(),
                p(textoDownload),
                br(),
                br(),
                br(),
                br(),
                radioButtons('format', 'Formato do documento', c('PDF', 'HTML', 'Word'),
                             inline = TRUE),
                downloadButton("downloadReport", "Baixe um relatório do município"),
                br(),
                br(),
                br(),
                br(),
                radioButtons('formatDoc', 'Formato da tabela', c('Excel', 'CSV'),
                             inline = TRUE),
                downloadButton("downloadDatabase", "Baixe os dados do município como tabela")),
        tabItem(tabName = "instr",
                h2("Como usar"),
                br(),
                p(texto),   
                br(),
                h3("Perguntas frequentes"),
                br(),
                strong(p("Como o índice de adesão ao preenchimento é calculado?")),
                p("Cada escola deve preencher diariamente o questionário, ainda que não tenha casos registrados. ETC ETC ETC"))
    ))))


