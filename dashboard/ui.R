#--- libraries
library(shinydashboard)
library(shiny)
library(tidyverse)
library(shinymanager)
library(plotly)
library(DT)

#--- data
escolas <- read_csv("data/escola_segura.csv")
escolas01 <- read_rds("escolas01.rds")

options1 <- str_replace_all(escolas01$NO_ENTIDADE, "[[:punct:]]", "")
options2 <- c("ESCOLA MUNICIPAL ABCDE", unique(escolas[[4]]))

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
credentials <- read_excel('credentials.xlsx')

#--- dashboard

secure_app(language="pt-BR", head_auth = tags$script(inactivity),
           dashboardPage(
    skin="green",
    #-- header
    dashboardHeader(title="Escola Segura - Instituto Gesto", titleWidth = 450),
    #-- sidebar
    dashboardSidebar(br(),
                     sidebarMenu(
                     menuItem(textOutput("munReferencia", inline=TRUE), tabName = "dashboard", icon = icon("city")),
                     menuItem("Visão da escola", tabName = "escola", icon = icon("school")),
                     selectInput("variable2", "Selecione a escola:", options1),
                     br(),
                     menuItem("Download", tabName = "download", icon = icon("download")),
                     br(),
                     menuItem("Como usar", tabName = "instr", icon = icon("question-circle")))),

# Body --------------------------------------------------------------------
    dashboardBody(
        #-- dashboard
        tabItems(tabItem(tabName = "dashboard",
                h2(strong(textOutput("munOutput"))),
            br(),
        fluidRow(
            valueBox(50, "Casos registrados em escolas", color='green'),
            valueBoxOutput("progressBox"),
            valueBoxOutput("approvalBox")
        ),
        fluidRow(
            box(
                title = "Perfil", status = "warning", solidHeader = TRUE,
                p(textOutput("popText", inline=TRUE)),
                p("Número de escolas: 356"),
                p("Número de estudantes: 15.680"),
                p("Número de professores: 400"),
                p("Demais profissionais: 150")
            ),
               box(
                title = "Casos reportados", status = "warning", solidHeader = TRUE,
                p("Estudantes: 25"),
                p("Professores: 10"),
                p("Demais profissionais: 5"),
                p("Casos suspeitos: 30"),
                p("Casos confirmados: 20")
            )       
        ),
        fluidRow(box(plotlyOutput('plot1'), title="Casos por local da escola"),
                box(plotlyOutput('plot2'), title="Casos por membro da comunidade")),
        fluidRow(box(plotlyOutput('plot4'), title="Casos diários: local"),
                 box(plotlyOutput('plot3'), title="Casos diários: membro")),
        h3('Casos por escola:'),
        radioButtons(inputId="radio1", label="", choices=c("Números absoultos", "Valores percentuais")),
        fluidRow(DTOutput('tbl'))),
        #-- escola
        tabItem(tabName = "escola",
                h2(textOutput("nomeEscola1")),
                fluidRow(valueBox(25, "Casos registrados na escola", color = 'green'),
                         valueBox(5, "Nos últimos 14 dias", color = 'green'),
                         valueBox("36%", "de adesão no preenchimento", color = 'green')),
                fluidRow( box(
                    title = "Visão da escola", status = "danger", solidHeader = TRUE,
                    strong(textOutput("nomeEscola2")),
                    strong(p("Escola Rural")),
                    p("Total de estudantes: 867"),
                    p("Total de professores: 45"),
                    p("Demais profissionais: 40"),
                    p("Nota no IDEB: 6,5"),
                    p("Total de casos: 55 casos"),
                    p("Casos nos últimos 14 dias: 10 casos")
                ))), 
        #-- downloads
        tabItem(tabName = "download",
                h2("Download dos dados"),
                br(),
                br(),
                p("Nesta seção, é possível baixar o dados por município, em formato de tabela, tal como consta na planilha original do Google Forms utilizado para o preenchimento."),
                #p("Também é possível baixar um relatório, que reúne as informações apresentadas na Dashboard do município."),
                br(),
                br(),
                br(),
                br(),
                #radioButtons('format', 'Formato do documento', c('PDF', 'HTML', 'Word'),
                #             inline = TRUE),
                #downloadButton("downloadReport", "Baixe um relatório do município"),
                #br(),
                #br(),
                #br(),
                #br(),
                radioButtons('formatDoc', 'Formato da tabela', choiceValues=c('.csv'), choiceNames=c("CSV"),
                             inline = TRUE),
                downloadButton("downloadDatabase", "Baixe os dados do município como tabela")),
        #--- como usar
        tabItem(tabName = "instr",
                h2("Como usar"),
                br(),
                p("Inserir texto aqui mencionando o propósito da dashboard e instruções gerais de uso."),   
                br(),
                h3("Perguntas frequentes"),
                br(),
                strong(p("Como o índice de adesão ao preenchimento é calculado?")),
                p("Cada escola deve preencher diariamente o questionário, ainda que não tenha casos registrados. O percentual de adesão indica a frequência de preenchimento, considerando apenas os dias úteis"),
                br(),
                strong(p("Como baixar um gráfico?")),
                p("Todos os gráficos tem uma opção para baixar a imagem individualmente, como um arquivo png."),
                br(),
                strong(p("Adicionar mais perguntas aqui")))
    ))))


