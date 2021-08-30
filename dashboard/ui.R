#--- libraries
library(shinydashboard)
library(shiny)
library(dplyr)
library(readr)
library(shinymanager)
library(lubridate)
library(readxl)
library(plotly)
library(DT)
library(stringr)


#--- data
escolas <- read_csv("data/escola_segura.csv")

escolas01 <- read_rds("escolas01.rds") %>% 
    mutate(id=paste0(NO_ENTIDADE, " (", CO_ENTIDADE, ")")) %>% 
    left_join(read.csv("simulacao.csv")[,c(1,4:11)], by="CO_ENTIDADE") %>% 
    mutate(TOTAL = DS+DC+ES+EC+PS+PC,
           CONF = DC+EC+PC,
           SUSP = DS+ES+PS,
           D = DC+DS,
           E = ES+EC,
           P = PS+PC) %>% 
    mutate(NO_ENTIDADE = iconv(NO_ENTIDADE, to="UTF-8"),
           id = iconv(id, to="UTF-8"))

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
               dashboardHeader(title=imageOutput("image"), titleWidth = 450),
               #-- sidebar
               dashboardSidebar(br(),
                                sidebarMenu(
                                    menuItem("Guia de utilização", tabName = "instr", icon = icon("question-circle")),
                                    br(),
                                    menuItem(textOutput("munReferencia", inline=TRUE), tabName = "dashboard", icon = icon("city")),
                                    br(),
                                    menuItem("Visão da escola", tabName = "escola", icon = icon("school")),
                                    uiOutput('options'),
                                    br(),
                                    menuItem("Download", tabName = "download", icon = icon("download")),
                                    br()
                                    )),
               
               # Body --------------------------------------------------------------------
               dashboardBody(
                   #-- dashboard
                   tabItems(tabItem(tabName = "dashboard",
                                    h2(strong(textOutput("munOutput"))),
                                    br(),
                                    fluidRow(
                                        box(
                                            title = "Perfil", color="green", solidHeader = TRUE,
                                            p(textOutput("popText", inline=TRUE)),
                                            p(textOutput("escolasText", inline=TRUE)),
                                            p("Estudantes na rede: 5.320 (Censo 2020)"),
                                            p("Professores na rede: 532 (Censo 2020)"),
                                            p("Demais profissionais: 532 (Censo 2020)")
                                        )
                                    ),
                                    fluidRow(
                                        valueBoxOutput("firstCityBox"),
                                        valueBoxOutput("secondCityBox"),
                                        valueBoxOutput("thirdCityBox")
                                    ),
                                    fluidRow(
                                        valueBoxOutput("fourthCityBox"),
                                        valueBoxOutput("fifthCityBox"),
                                        valueBoxOutput("sixthCityBox")
                                    ),
                                    br(),
                                    h4("Selecione o período dos gráficos:"),
                                    radioButtons(inputId="radio2", label="", choices=c("Acumulado (desde o início)", "Últimos 14 dias")),
                                    br(),
                                    fluidRow(box(plotlyOutput('plot1'), title="Casos por tipo de caso"),
                                             box(plotlyOutput('plot2'), title="Casos por membro da comunidade")),
                                    fluidRow(box(plotlyOutput('plot5'), title="Casos por local da escola"),
                                             box(plotlyOutput('plot6'), title="Casos recentes na escola")),
                                    fluidRow(box(plotlyOutput('plot3'), title="Casos diários em escolas"),
                                             box(plotlyOutput('plot4'), title="Turmas fechadas em escolas")),
                                    h3('Casos por escola:'),
                                    radioButtons(inputId="radio1", label="", choices=c("Todos", "Estudantes", "Professores", "Demais profissionais")),
                                    fluidRow(DTOutput('tbl')),
                                    h3('Escolas e turmas fechadas:'),
                                    radioButtons(inputId="radio2", label="", choices=c("Todas", "Urbanas", "Rurais")),
                                    fluidRow(DTOutput('tbl2'))),
                            #-- escola
                            tabItem(tabName = "escola",
                                    h2(textOutput("nomeEscola1")),
                                    fluidRow(box(
                                        title = "Visão da escola", status = "danger", solidHeader = TRUE,
                                        strong(textOutput("nomeMuni2")),
                                        br(),
                                        p(textOutput("codigoINEP", inline=TRUE)),
                                        p(textOutput("localizacao", inline=TRUE)),
                                        p("Estudantes na escola: 532 (Censo 2020)"),
                                        p("Professores na escola: 32 (Censo 2020)"),
                                        p("Demais profissionais: 32 (Censo 2020)")
                                    )),
                                    fluidRow(valueBoxOutput("firstSchoolBox"),
                                             valueBoxOutput("secondSchoolBox"),
                                             valueBoxOutput("thirdSchoolBox")),
                                    fluidRow(DTOutput('tblPrincipal2'))), 
                            #-- downloads
                            tabItem(tabName = "download",
                                    h2("Download dos microdados"),
                                    br(),
                                    br(),
                                    p("Nesta seção, é possível baixar o dados por município, em formato de tabela, tal como consta na planilha original do Google Forms utilizado para o preenchimento. Os botões para download ficam na parte inferior da tabela."),
                                    #p("Também é possível baixar um relatório, que reúne as informações apresentadas na Dashboard do município."),
                                    fluidRow(DTOutput('tblPrincipal'))),
                            #--- como usar
                            tabItem(tabName = "instr",
                    h2('Painel Escola Segura'),
                    h3('Guia de Utilização'),    

    p('Olá Gestor(a),'),
    p("Para facilitar o monitoramento de casos de COVID-19 em sua unidade escolar e rede municipal, o Instituto Gesto desenvolveu a ferramenta Painel Escola Segura."),
    p("O Painel é uma ferramenta colaborativa para auxiliar a secretaria de educação de sua rede a identificar novos casos e estabelecer uma comunicação direta entre os gestores escolares e a comunidade."),
    p("Os dados deste painel são fornecidos por vocês, gestores escolares e da secretaria municipal de educação, através da Ferramenta para notificação de casos."),
    p("A comunicação próxima e feita por pares é necessária para criarmos uma ponte direta e empática entre gestores e a comunidade e o mais importante - confiança com base em dados e estatística para uma melhor tomada de decisões estratégicas."),
    br(),
    h3("METODOLOGIA"),
    br(),
    p("As perguntas da Ferramenta para notificação de casos foram criadas para assegurar a notificação de casos suspeitos ou confirmados na escola."),
    p("Para mantermos o painel atualizado, é importante que o formulário seja preenchido diariamente pela unidade escolar, mesmo que não haja casos suspeitos ou confirmados na escola. Lembre-se de manter o acompanhamento dos casos suspeitos, notificando sua confirmação caso necessário."),
    p("Os dados respondidos através da Ferramenta de notificação sincronizam automaticamente no Painel Escola Segura.")

                   )))))

