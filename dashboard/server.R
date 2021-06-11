library(shiny)
library(shinydashboard)
library(lubridate)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$progressBox <- renderValueBox({
        valueBox(
            paste0(108), "Casos em escolas nos últimos 14 dias", icon = icon("list"),
            color = "red"
        )
    })
    
    output$approvalBox <- renderValueBox({
        valueBox(
            "80%", "Casos em escolas estão ativos", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "red"
        )
    })


# Download relatorio ------------------------------------------------------
    
    output$downloadReport <- downloadHandler(
        filename = function() {
            paste('my-report', sep = '.', switch(
                input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
        },
        
        content = function(file) {
            src <- normalizePath('report.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            
            library(rmarkdown)
            out <- render('report.Rmd', switch(
                input$format,
                PDF = pdf_document(), HTML = html_document(), Word = word_document()
            ))
            file.rename(out, file)
        }
    )
    

# Download dos dados ------------------------------------------------------

    output$downloadData <- downloadHandler(
        filename = function() {
            paste("caruaru_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(escolas, file, row.names = FALSE)
        }
    )


# Autenticação ------------------------------------------------------------

    res_auth <- secure_server(
        check_credentials = check_credentials(credentials)
    )
    
    output$auth_output <- renderPrint({
        reactiveValuesToList(res_auth)
    })
    
# Plots -------------------------------------------------------------------

    labels = c('Rural', 'Urbano')
    values = c(450, 45)
    
    output$plot1 <- renderPlotly({
        plot_ly(type='pie', labels=labels, values=values, 
                   textinfo='label+percent',
                   insidetextorientation='radial')
    })
    
    labels2 = c('Aluno', 'Professor', 'Funcionário')
    values2 = c(135, 25, 10)
    
    output$plot2<- renderPlotly({
        plot_ly(type='pie', labels=labels2, values=values2, 
                            textinfo='label+percent',
                            insidetextorientation='radial')    
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
        add_trace(y = ~c_alunos, name = 'Alunos') %>% 
        add_trace(y = ~c_professores, name = 'Professores') %>% 
        add_trace(y = ~c_funcionarios, name = 'Funcionários') %>% 
        layout(yaxis = list(title = 'Número de casos'))    
    })
    
    
    
    c_rural <- abs(round(rnorm(30,25,7)))
    c_urbano <- abs(round(rnorm(30,4,1)))
    
    data2 <- data.frame(dates, c_rural, c_urbano)
    
    
    output$plot4 <- renderPlotly({
        plot_ly(data2, x = ~dates, mode="lines") %>% 
            add_trace(y = ~c_urbano, name = 'Urbano') %>% 
            add_trace(y = ~c_rural, name = 'Rural') %>% 
            layout(yaxis = list(title = 'Número de casos'))    
    })
    

# Tabelas -----------------------------------------------------------------

    ce_alunos <- c(12,3,5,6,7,10,2,0,0,6)
    ce_prof <- c(5,0,2,1,8,0,3,1,0,2)
    ce_func <- c(3,1,0,3,2,0,1,0,0,1)
    
    tabela <- data.frame(Escola=options2[2:11], ce_alunos, ce_prof, ce_func) %>% 
        mutate(Total = ce_alunos+ce_prof+ce_func)
    
    
    output$tbl = renderDT(
        tabela, options = list(lengthChange = FALSE, language=list(url="https://cdn.datatables.net/plug-ins/1.10.19/i18n/Portuguese.json")))    
    
#--    
})
