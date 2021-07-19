#--- data


data.entry() <- read_csv("data/escola_segura.csv")

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



options2 <- c("ESCOLA MUNICIPAL ABCDE", unique(escolas[[4]]))

credentials <- read_excel('credentials.xlsx')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
# Autenticação ------------------------------------------------------------

    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
# Municípios --------------------------------------------------------------

    creds_reactive <- reactive({
        reactiveValuesToList(res_auth)
    })

    mydata <- reactive({
        escolas01 %>% 
        filter(CO_UF %in% creds_reactive()$ibge |
                       CO_MUNICIPIO %in% creds_reactive()$ibge) })
    
    output$options <- renderUI({selectInput("variable2", "Selecione a escola:", mydata()$id)})
    
    #-- n escolas
    output$escolasText <- renderText({nrow(mydata()) -> x
        paste0("Número de escolas na rede: ", x)})
    
    #-- infos das escolas
    output$codigoINEP <- renderText({mydata() %>% 
            filter(id == input$variable2) %>% 
            pull(CO_ENTIDADE)->x
            paste0("Código INEP: ", x)})
    
    output$localizacao <- renderText({mydata() %>% 
            filter(id == input$variable2) %>% 
            pull(TP_LOCALIZACAO)->x
        paste0("Escola ", x)})
    
    #-- nome da escola
    output$nomeEscola1 <- renderText({str_replace(input$variable2, "[(].*", "")})
    output$nomeEscola2 <- renderText({str_replace(input$variable2, "[(].*", "")})
    
    #-- criar nome da cidade
    output$munOutput <- renderText({creds_reactive()$cidade})
    output$nomeMuni2 <- renderText({creds_reactive()$cidade})
    

    #-- criar populacao
    output$popText <- renderText({paste0("População: ", format(creds_reactive()$pop, big.mark=".", decimal.mark=",") )})
        
    #-- criar estado / municipio
    output$munReferencia <- renderText({paste0("Visão do ", creds_reactive()$tipo)})


# Box ---------------------------------------------------------------------

    #--- box de municipios
    output$firstCityBox <- renderValueBox({
        v<-sum(mydata()$TOTAL, na.rm=T)
        valueBox(v, "Casos registrados em escolas",
                 color = ifelse(v>=20, "red", "green"))})

    output$secondCityBox <- renderValueBox({
        v<-sum(mydata()$TF, na.rm=T)
        valueBox(v, "Turmas fechadas",
            color = ifelse(v>=20, "red", "green"))})
    
    output$thirdCityBox <- renderValueBox({
        v<-mean(mydata()$AD, na.rm=T)
        valueBox(paste0(round(v), "%"), "Adesão ao preenchimento nos últimos 14 dias",
            color = ifelse(v<=20, "red", "green"))})
    
    #--
    output$fourthCityBox <- renderValueBox({
        v<-sum(mydata()$CONF, na.rm=T)/sum(mydata()$TOTAL, na.rm=T)
        valueBox(paste0(round(v*100), "%"), "de casos confirmados",
                 color = "orange")})
    
    output$fifthCityBox <- renderValueBox({
        d <- mydata() %>% filter(TP_LOCALIZACAO=="Urbana")
        v=sum(d$TOTAL, na.rm=T)/sum(mydata()$TOTAL, na.rm=T)
        valueBox(paste0(round(v*100), "%"), "em áreas urbanas",
                 color = "orange")})
    
    output$sixthCityBox <- renderValueBox({
        v=sum(mydata()$E, na.rm=T)/sum(mydata()$TOTAL, na.rm=T)
        valueBox(paste0(round(v*100), "%"), "entre estudantes",
                 color = "orange")})
    
    #-- box school
    output$firstSchoolBox <- renderValueBox({
        v<-mydata() %>% 
            filter(id == input$variable2) %>% 
            pull(TOTAL)
        valueBox(paste0(round(v*100), "%"), "Casos registrados na escola",
                 color = "orange")})
    
    output$secondSchoolBox <- renderValueBox({
        v<-mydata() %>% 
            filter(id == input$variable2) %>% 
            pull(AD)
        valueBox(paste0(v, "%"), "Adesão ao preenchimento",
                 color = "orange")})
    
    output$thirdSchoolBox <- renderValueBox({
        v<-mydata() %>% 
            filter(id == input$variable2) %>% 
            pull(TF)
        valueBox(v, "Turmas fechadas",
                 color = "orange")})
    
# Plots -------------------------------------------------------------------

    #-- grafico 1
    
    labels1 = c('Confirmados', 'Suspeitos')
    colors1 = c("#CB2B23", "#5F1E88")
    
    output$plot1 <- renderPlotly({
        
        a <- sum(mydata()$CONF, na.rm=T)
        b <- sum(mydata()$SUSP, na.rm=T)
        
        plot_ly(type='pie', labels=labels1, values=c(a,b), hole=0.6,
                   textinfo='label+percent',
                   insidetextorientation='radial',
                marker = list(colors = colors1))
        
    })
    
    #-- grafico 2
    
    
    labels2 = c('Estudante', 'Professores', 'Demais profissionais')
    colors2 = c("#CB8B23", "#2A2C8B", "#157A7A")
    
    output$plot2<- renderPlotly({
        a <- sum(mydata()$E, na.rm=T)
        b <- sum(mydata()$P, na.rm=T)
        c <- sum(mydata()$D, na.rm=T)
        
        plot_ly(type='pie', labels=labels2, values=c(a,b,c), hole=0.6,
                            textinfo='label+percent',
                            insidetextorientation='radial',
                hoverinfo = 'text',
                text = ~paste('Casos: ', c(a,b,c)),
                marker = list(colors = colors2)) %>% 
            layout(
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    #-- grafico 5
    labels5 = c('Rural', 'Urbano')
    colors5 = c("#AA8E39", "#983351")
    
    output$plot5<- renderPlotly({
        b <- mydata() %>% filter(TP_LOCALIZACAO=="Urbana")
        a <- mydata() %>% filter(TP_LOCALIZACAO=="Rural")
        
        plot_ly(type='pie', labels=labels5, values=c(sum(a$TOTAL,na.rm=T),
                                                    sum(b$TOTAL,na.rm=T)), hole=0.6,
                textinfo='label+percent',
                insidetextorientation='radial',
                hoverinfo = 'text',
                text = ~paste('Casos: ', c(sum(a$TOTAL),
                                           sum(b$TOTAL))),
                marker = list(colors = colors5)) %>% 
            layout(
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    #-- grafico 6
    labels6 = c('Últimos 14 dias', 'Casos antigos')
    values6 = c(30,10)
    colors6 = c("#1a1a1a", "#c5c5c5")
    
    output$plot6<- renderPlotly({
        
        plot_ly(type='pie', labels=labels6, values=values6, hole=0.6,
                textinfo='label+percent',
                insidetextorientation='radial',
                hoverinfo = 'text',
                text = ~paste('Casos: ', values6),
                marker = list(colors = colors6)) %>% 
            layout(
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    
# Lines -------------------------------------------------------------------

    dates <- dmy('01-01-2021')+1:30
    c_alunos <- abs(round(rnorm(30,15,5)))
    data <- data.frame(dates, c_alunos)
    
    output$plot3 <- renderPlotly({
        plot_ly(data, x = ~dates, mode="lines") %>% 
        add_trace(y = ~c_alunos, name = 'Total', line = list(color = "#000000")) %>% 
        layout(yaxis = list(title = 'Número de casos'))    
    })
    
    c_turmas <- abs(round(rnorm(30,1,1)))
    data2 <- data.frame(dates, c_turmas)
    
    output$plot4 <- renderPlotly({
        plot_ly(data2, x = ~dates, mode="lines") %>% 
            add_trace(y = ~c_turmas, name = 'Total', line = list(color = "#000000")) %>% 
            layout(yaxis = list(title = 'Turmas fechadas'))    
    })
    

# Tabelas -----------------------------------------------------------------

    tabela <- reactive({
        switch(input$radio1,
               "Todos" = mydata()[,c(1,2,5,15,16,17)],
               "Estudantes" = mydata()[,c(1,2,5,19,13,12)],
               "Professores" = mydata()[,c(1,2,5,20,9,8)],
               "Demais profissionais" = mydata()[,c(1,2,5,18,11,10)])
    })
    
    output$tbl = renderDT(
        tabela(),
        extensions='Buttons',
        class = "display",
        colnames = c("INEP", "Escola", "Local", "Total", "Conf.", "Susp."),
         options = list(lengthChange = FALSE, 
         paging = TRUE,
                                searching = TRUE,
                                fixedColumns = TRUE,
                                autoWidth = TRUE,
                                ordering = TRUE,
                                dom = 'tB',
                                buttons = c('copy', 'csv', 'excel'),
        language=list(url="https://cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese.json"))) 
    
    output$tbl = renderDT(
        tabela(),
        extensions='Buttons',
        class = "display",
        colnames = c("INEP", "Escola", "Local", "Total", "Conf.", "Susp."),
        options = list(lengthChange = FALSE, 
                       paging = TRUE,
                       searching = TRUE,
                       fixedColumns = TRUE,
                       autoWidth = TRUE,
                       ordering = TRUE,
                       dom = 'tB',
                       buttons = c('copy', 'csv', 'excel'),
                       language=list(url="https://cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese.json")))  
    
#-- tabela principal
    output$tblPrincipal = renderDT(
        escolas[,c(2:15)],
        extensions='Buttons',
        class = "display",
        options = list(lengthChange = FALSE, 
                       paging = TRUE,
                       searching = TRUE,
                       fixedColumns = TRUE,
                       autoWidth = TRUE,
                       ordering = TRUE,
                       scrollX = TRUE,
                       dom = 'tB',
                       buttons = c('copy', 'csv', 'excel'),
                       language=list(url="https://cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese.json")))
    
    
    output$tblPrincipal2 = renderDT(
        escolas[,c(6:15)],
        extensions='Buttons',
        class = "display",
        options = list(lengthChange = FALSE, 
                       paging = TRUE,
                       searching = TRUE,
                       fixedColumns = TRUE,
                       autoWidth = TRUE,
                       ordering = TRUE,
                       scrollX = TRUE,
                       dom = 'tB',
                       buttons = c('copy', 'csv', 'excel'),
                       language=list(url="https://cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese.json")))
    
#--    
})


                            