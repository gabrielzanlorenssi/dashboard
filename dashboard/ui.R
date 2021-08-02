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
               dashboardHeader(title="Escola Segura - Instituto Gesto - teste", titleWidth = 450),
               #-- sidebar
               dashboardSidebar(br(),
                                sidebarMenu(
                                    menuItem(textOutput("munReferencia", inline=TRUE), tabName = "dashboard", icon = icon("city")),
                                    menuItem("Visão da escola", tabName = "escola", icon = icon("school")),
                                    uiOutput('options'),
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
                                    h2("Instruções"),
                                    br(),
                                    h3("Sobre o painel Escola Segura"),
                                    br(),
                                    p("O painel Escola Segura, desenvolvida pelo Instituto Gesto, é uma plataforma para o monitoramento de casos de covid-19 na comunidade escolar, seja em uma escola em específico ou em toda uma rede de ensino."),
                                    p("Segundo o Dr. Márcio Bittencourt, as medidas pouco custosas são por vezes as mais efetivas para prevenção da covid-19. O painel é baseado na conclusão de estudo publicado na revista Science de medidas com maiores resultados na prevenção do contágio em escolas. Por ordem, as medidas são:"),
                                    tags$ul(tags$li("Monitoramento de casos"),
                                            tags$li("Máscara no professor"),
                                            tags$li("Máscara no aluno"),
                                            tags$li("Cancelamento de atividades extracurriculares"),
                                            tags$li("Distanciamento e aulas ao ar livre")),
                                    p("O monitoramento de casos é uma medida pouco custosa e com bons resultados. Esta plataforma interativa é um instrumento para as redes escolares realizarem um monitoramento rápido e eficaz, ajudando na tomada de decisões e no seguimento das atividades escolares."),
                                    br(),
                                    h3("Versão de testes"),
                                    p("A versão atual do painel é uma versão de testes e nem todas as funcionalidades estão disponíveis. Com o preenchimento dos dados pelas escolas, estas opções estarão ativas."),
                                    tags$ul(tags$li("Os dados na seção download é igual para todas as cidades."),
                                            tags$li("Os dados das escolas são simulações. Nenhum dado sobre casos, adesão e turmas fechadas é real por enquanto."),
                                            tags$li("Os gráficos de linha ao longo do tempo apresentam dados provisórios."),
                                            tags$li("Novas informações ou visualizações podem ser adicionadas ou já estão em desenvolvimento. O que é exibido no painel é o que já está consolidado.")),
                                    h3("Perguntas frequentes"),
                                    br(),
                                    strong(p("Como baixar gráficos e tabelas?")),
                                    p("Os gráficos possuem uma aba dentro deles com a opção de baixar a imagem em png. Uma opção alternativa é tirar uma captura de tela (print screen) da imagem desejada. Clique aqui para ver instruções de como tirar uma captura de tela no Windows."),
                                    p("As tabelas possuem três opções para serem baixadas, com três botões no canto inferior. Copy permite copiar a tabela, que pode ser colada em outro lugar usando o atalho CTRL + V. CSV permite baixar a tabela como um arquivo separado vírgulas, que um formato para um uso mais avançado dos dados. Excel é o formato mais tradicional, e baixa o arquivo como uma planilha que pode ser aberta no Excel ou editores gratuitos de planilhas"),
                                    br(),
                                    strong(p("Como o índice de adesão ao preenchimento é calculado?")),
                                    p("Cada escola deve preencher diariamente o questionário, ainda que não tenha casos registrados ou turmas fechadas. Isto é importante para mensurar se um número baixo de casos está relacionado com o baixo preenchimento da ferramenta. O percentual de adesão indica a frequência de preenchimento na escola ou na rede (dependendo da visão selecionada), considerando apenas os dias úteis nos últimos 14 dias"),
                                    br(),
                                    strong(p("Tem mais alguma dúvida?")))
                   ))))

