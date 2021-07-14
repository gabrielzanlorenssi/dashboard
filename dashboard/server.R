#--- data
escolas <- read_csv("data/escola_segura.csv")

options2 <- c("ESCOLA MUNICIPAL ABCDE", unique(escolas[[4]]))

credentials <- read_excel('credentials.xlsx')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$progressBox <- renderValueBox({
        valueBox(
            paste0(108), "Casos em escolas nos últimos 14 dias",
            color = "green"
        )
    })
    
    output$approvalBox <- renderValueBox({
        valueBox(
            "80%", "de adesão ao questionário",
            color = "green"
        )
    })


# Download relatorio ------------------------------------------------------
    
    # output$downloadReport <- downloadHandler(
    #     filename = function() {
    #         paste('my-report', sep = '.', switch(
    #             input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    #         ))
    #     },
    #     
    #     content = function(file) {
    #         src <- normalizePath('report.Rmd')
    #         
    #         # temporarily switch to the temp dir, in case you do not have write
    #         # permission to the current working directory
    #         owd <- setwd(tempdir())
    #         on.exit(setwd(owd))
    #         file.copy(src, 'report.Rmd', overwrite = TRUE)
    #         
    #         library(rmarkdown)
    #         out <- render('report.Rmd', switch(
    #             input$format,
    #             PDF = pdf_document(), HTML = html_document(), Word = word_document()
    #         ))
    #         file.rename(out, file)
    #     }
    # )

# --- selecao da escola ---

output$nomeEscola1 <- renderText({input$variable2})
output$nomeEscola2 <- renderText({input$variable2})

# Download dos dados ------------------------------------------------------
        
    # output$downloadDatabase <- downloadHandler(
    #     file = function() {
    #         paste("tabela_", Sys.Date(), input$formatDoc, sep = "")
    #     },
    #     content = function(file) {
    #         write.csv2(escolas[,c(1:15)], file, row.names = FALSE, fileEncoding = "latin1")
    #     }
    # )
    

# Autenticação ------------------------------------------------------------

    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
# Municípios --------------------------------------------------------------

    creds_reactive <- reactive({
        reactiveValuesToList(res_auth)
    })
    
    #-- criar nome da cidade
    output$munOutput <- renderText({creds_reactive()$cidade})
    
    #-- criar populacao
    output$popText <- renderText({paste0("População: ", format(creds_reactive()$pop, big.mark=".", decimal.mark=",") )})
        
    #-- criar estado / municipio
    output$munReferencia <- renderText({paste0("Visão do ", creds_reactive()$tipo)})
    
    
# Plots -------------------------------------------------------------------

    labels = c('Confirmados', 'Suspeitos')
    values = c(55, 20)
    colors1 = c("#CB2B23", "#5F1E88")
    
    output$plot1 <- renderPlotly({
        plot_ly(type='pie', labels=labels, values=values, hole=0.6,
                   textinfo='label+percent',
                   insidetextorientation='radial',
                marker = list(colors = colors1))  
    })
    
    labels2 = c('Estudante', 'Professores', 'Demais profissionais')
    values2 = c(65, 25, 18)
    colors2 = c("#CB8B23", "#2A2C8B", "#157A7A")
    
    output$plot2<- renderPlotly({
        plot_ly(type='pie', labels=labels2, values=values2, hole=0.6,
                            textinfo='label+percent',
                            insidetextorientation='radial',
                hoverinfo = 'text',
                text = ~paste('Casos: ', values2),
                marker = list(colors = colors2)) %>% 
            layout(
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })

# Lines -------------------------------------------------------------------

    
    set.seed(1234)
    
    dates <- dmy('01-01-2021')+1:30
    
    c_alunos <- abs(round(rnorm(30,15,5)))
    c_professores <- abs(round(rnorm(30,3,2)))
    c_funcionarios <- abs(round(rnorm(30,2,1)))
    
    data <- data.frame(dates, c_alunos, c_professores, c_funcionarios)
    
    
    output$plot3 <- renderPlotly({
        plot_ly(data, x = ~dates, mode="lines") %>% 
        add_trace(y = ~c_alunos, name = 'Estudantes', line = list(color = "#CB8B23")) %>% 
        add_trace(y = ~c_professores, name = 'Professores',  line = list(color = "#2A2C8B")) %>% 
        add_trace(y = ~c_funcionarios, name = 'Funcionários',  line = list(color = "#157A7A")) %>% 
        layout(yaxis = list(title = 'Número de casos'))    
    })
    

    c_rural <- abs(round(rnorm(30,25,7)))
    c_urbano <- abs(round(rnorm(30,4,1)))
    
    data2 <- data.frame(dates, c_rural, c_urbano)

    output$plot4 <- renderPlotly({
        plot_ly(data2, x = ~dates, mode="lines") %>% 
            add_trace(y = ~c_urbano, name = 'Confirmados', line=list(color= "#CB2B23")) %>% 
            add_trace(y = ~c_rural, name = 'Suspeitos', line=list(color="#5F1E88")) %>% 
            layout(yaxis = list(title = 'Número de casos'))    
    })
    

# Tabelas -----------------------------------------------------------------

    Suspeitos <- c(12,3,5,6,7,10,2,0,0,6)
    Confirmados <- c(5,0,2,1,8,0,3,1,0,2)
    ce_func <- c(3,1,0,3,2,0,1,0,0,1)
    
    tabela <- data.frame(Escola=options2[2:11], Confirmados, Suspeitos) %>% 
        mutate(Total = Confirmados+Suspeitos)
    
    
    output$tbl = renderDT(
        tabela,
        extensions='Buttons',
        class = "display",
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
    
#--    
})


                            