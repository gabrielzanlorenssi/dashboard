library(shiny)
library(shinydashboard)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$progressBox <- renderValueBox({
        valueBox(
            paste0(8 + input$count, " casos"), "Nos últimos 14 dias", icon = icon("list"),
            color = "red"
        )
    })
    
    output$approvalBox <- renderValueBox({
        valueBox(
            "80%", "Casos ativos", icon = icon("thumbs-up", lib = "glyphicon"),
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

    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    output$res_auth <- renderPrint({
        reactiveValuesToList(result_auth)
    })            

#--    
})
