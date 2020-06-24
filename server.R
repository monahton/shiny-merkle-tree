# ----------------------------
# Description: Demonstration of the User Intefrace capable to verify hardware configuration
# Author: Vladimir Zhbanko
# Date: 2019-06-05
# ----------------------------

library(shiny)
library(DT)
library(lubridate)
library(tidyverse)
library(digest)
# functions that read API's
source("/home/myself/r-studio/shiny-merkle-tree/R/bring_element.R")
source("/home/myself/r-studio/shiny-merkle-tree/R/bring_element_hash.R")
source("/home/myself/r-studio/shiny-merkle-tree/R/bring_group_hash.R")
source("/home/myself/r-studio/shiny-merkle-tree/R/bring_unit_hash.R")

# =======================================
# DATA GATHERING AND PREPROCESSING      #
# =======================================  

# Server Logic
shinyServer(function(input, output, session) {
    
    ## **-------------- **
    # reset to original values when pressing the button //eraze field
    observeEvent(input$reset, {
        updateTextInput(session, inputId = "Prt1G1", value = "")
        updateTextInput(session, inputId = "Prt2G1", value = "")
        updateTextInput(session, inputId = "Prt3G1", value = "")
        updateTextInput(session, inputId = "Prt4G1", value = "")
        
        updateTextInput(session, inputId = "Hsh1G1", value = "")
        updateTextInput(session, inputId = "Hsh2G1", value = "")
        updateTextInput(session, inputId = "Hsh3G1", value = "")
        updateTextInput(session, inputId = "Hsh4G1", value = "")
        
        updateTextInput(session, inputId = "actG1", value = "")
        updateTextInput(session, inputId = "reqG1", value = "")
        
        updateTextInput(session, inputId = "Prt1G2", value = "")
        updateTextInput(session, inputId = "Prt2G2", value = "")
        updateTextInput(session, inputId = "Prt3G2", value = "")
        updateTextInput(session, inputId = "Prt4G2", value = "")
        
        updateTextInput(session, inputId = "Hsh1G2", value = "")
        updateTextInput(session, inputId = "Hsh2G2", value = "")
        updateTextInput(session, inputId = "Hsh3G2", value = "")
        updateTextInput(session, inputId = "Hsh4G2", value = "")
        
        updateTextInput(session, inputId = "actG2", value = "")
        updateTextInput(session, inputId = "reqG2", value = "")        
        
        updateTextInput(session, inputId = "Prt1G3", value = "")
        updateTextInput(session, inputId = "Prt2G3", value = "")
        updateTextInput(session, inputId = "Prt3G3", value = "")
        updateTextInput(session, inputId = "Prt4G3", value = "")
        
        updateTextInput(session, inputId = "Hsh1G3", value = "")
        updateTextInput(session, inputId = "Hsh2G3", value = "")
        updateTextInput(session, inputId = "Hsh3G3", value = "")
        updateTextInput(session, inputId = "Hsh4G3", value = "")
        
        updateTextInput(session, inputId = "actG3", value = "")
        updateTextInput(session, inputId = "reqG3", value = "")
        
        updateTextInput(session, inputId = "actU", value = "")
        updateTextInput(session, inputId = "reqU", value = "")
        
    })
    ## **-------------- **
    
    ## **-------------- **
    # filled values stored as reactive functions
    # G1 ===
    Prt1G1 <- reactive({ input$Prt1G1 })
    Prt2G1 <- reactive({ input$Prt2G1 })
    Prt3G1 <- reactive({ input$Prt3G1 })
    Prt4G1 <- reactive({ input$Prt4G1 })
    
    actG1 <- reactive({input$actG1})
    reqG1 <- reactive({input$reqG1})
    # G2 ===
    Prt1G2 <- reactive({ input$Prt1G2 })
    Prt2G2 <- reactive({ input$Prt2G2 })
    Prt3G2 <- reactive({ input$Prt3G2 })
    Prt4G2 <- reactive({ input$Prt4G2 })

    actG2 <- reactive({input$actG2})
    reqG2 <- reactive({input$reqG2})
    # G3 ===
    Prt1G3 <- reactive({ input$Prt1G3 })
    Prt2G3 <- reactive({ input$Prt2G3 })
    Prt3G3 <- reactive({ input$Prt3G3 })
    Prt4G3 <- reactive({ input$Prt4G3 })

    actG3 <- reactive({input$actG3})
    reqG3 <- reactive({input$reqG3})
    # actual signatures of the entire unit
    actU <- reactive({input$actU})
    reqU <- reactive({input$reqU})
    ## **-------------- **
    
    ## **-------------- **
    ## Merkle trees calculations
    # calculate actual signatures of G1
    Hsh1G1 <- reactive({ Prt1G1() %>% digest(algo="sha1", serialize=F) })
    Hsh2G1 <- reactive({ Prt2G1() %>% digest(algo="sha1", serialize=F) })
    Hsh3G1 <- reactive({ Prt3G1() %>% digest(algo="sha1", serialize=F) })
    Hsh4G1 <- reactive({ Prt4G1() %>% digest(algo="sha1", serialize=F) })
    # calculate actual signatures of G2
    Hsh1G2 <- reactive({ Prt1G2() %>% digest(algo="sha1", serialize=F) })
    Hsh2G2 <- reactive({ Prt2G2() %>% digest(algo="sha1", serialize=F) })
    Hsh3G2 <- reactive({ Prt3G2() %>% digest(algo="sha1", serialize=F) })
    Hsh4G2 <- reactive({ Prt4G2() %>% digest(algo="sha1", serialize=F) })
    # calculate actual signatures of G3
    Hsh1G3 <- reactive({ Prt1G3() %>% digest(algo="sha1", serialize=F) })
    Hsh2G3 <- reactive({ Prt2G3() %>% digest(algo="sha1", serialize=F) })
    Hsh3G3 <- reactive({ Prt3G3() %>% digest(algo="sha1", serialize=F) })
    Hsh4G3 <- reactive({ Prt4G3() %>% digest(algo="sha1", serialize=F) })
    # Object GR1 will contain the resulting 'hash' of the group
    GR1 <- reactive({ c(Hsh1G1(), Hsh2G1(), Hsh3G1(), Hsh4G1()) %>% as.character() %>% digest(algo="sha1", serialize=F) })
    # Object GR2 will contain the resulting 'hash' of the group
    GR2 <- reactive({ c(Hsh1G2(), Hsh2G2(), Hsh3G2(), Hsh4G2()) %>% as.character() %>% digest(algo="sha1", serialize=F) })
    # Object GR3 will contain the resulting 'hash' of the group
    GR3 <- reactive({ c(Hsh1G3(), Hsh2G3(), Hsh3G3(), Hsh4G3()) %>% as.character() %>% digest(algo="sha1", serialize=F) })
    # same for units
    U <- reactive({ c(GR1(), GR2(), GR3()) %>% as.character() %>% digest(algo="sha1", serialize=F) })
    ## **-------------- **

    ## **-------------- **
    # create functions calls that reads API to bring configuration from the OEM's API Container
    get_P1G1 <- function(){ bring_element(1, 1)}    
    get_P2G1 <- function(){ bring_element(2, 1)}    
    get_P3G1 <- function(){ bring_element(3, 1)}    
    get_P4G1 <- function(){ bring_element(4, 1)}    
    
    get_P1G2 <- function(){ bring_element(1, 2)}    
    get_P2G2 <- function(){ bring_element(2, 2)}    
    get_P3G2 <- function(){ bring_element(3, 2)}    
    get_P4G2 <- function(){ bring_element(4, 2)}    
    
    get_P1G3 <- function(){ bring_element(1, 3)}
    get_P2G3 <- function(){ bring_element(2, 3)}    
    get_P3G3 <- function(){ bring_element(3, 3)}    
    get_P4G3 <- function(){ bring_element(4, 3)} 
    
    # functions calls to read parts hash codes from the OEM's API Container
    get_P1G1h <- function(){ bring_element_hash(1, 1)}    
    get_P2G1h <- function(){ bring_element_hash(2, 1)}    
    get_P3G1h <- function(){ bring_element_hash(3, 1)}    
    get_P4G1h <- function(){ bring_element_hash(4, 1)}    
    
    get_P1G2h <- function(){ bring_element_hash(1, 2)}    
    get_P2G2h <- function(){ bring_element_hash(2, 2)}    
    get_P3G2h <- function(){ bring_element_hash(3, 2)}    
    get_P4G2h <- function(){ bring_element_hash(4, 2)}    
    
    get_P1G3h <- function(){ bring_element_hash(1, 3)}
    get_P2G3h <- function(){ bring_element_hash(2, 3)}    
    get_P3G3h <- function(){ bring_element_hash(3, 3)}    
    get_P4G3h <- function(){ bring_element_hash(4, 3)}   
    ## **-------------- **
    # create functions calls that reads API to bring group hash from the OEM's
    get_hashG1 <- function(){ bring_group_hash(1)}
    get_hashG2 <- function(){ bring_group_hash(2)}
    get_hashG3 <- function(){ bring_group_hash(3)}
    ## **-------------- **
    # create functions calls that reads API to bring unit hash from the OEM's
    get_U <- function(){ bring_unit_hash()}
    ## **-------------- **
    
    # create functions to check obtained signatures compared to recieved ones
    check_P1G1 <- function(){if (Hsh1G1() == get_P1G1h())  { js$backgroundCol("Hsh1G1","#32CD32")} else { js$backgroundCol("Hsh1G1","red")}}
    check_P2G1 <- function(){if (Hsh2G1() == get_P2G1h())  { js$backgroundCol("Hsh2G1","#32CD32")} else { js$backgroundCol("Hsh2G1","red")}}
    check_P3G1 <- function(){if (Hsh3G1() == get_P3G1h())  { js$backgroundCol("Hsh3G1","#32CD32")} else { js$backgroundCol("Hsh3G1","red")}}
    check_P4G1 <- function(){if (Hsh4G1() == get_P4G1h())  { js$backgroundCol("Hsh4G1","#32CD32")} else { js$backgroundCol("Hsh4G1","red")}}
    
    check_P1G2 <- function(){if (Hsh1G2() == get_P1G2h())  { js$backgroundCol("Hsh1G2","#32CD32")} else { js$backgroundCol("Hsh1G2","red")}}
    check_P2G2 <- function(){if (Hsh2G2() == get_P2G2h())  { js$backgroundCol("Hsh2G2","#32CD32")} else { js$backgroundCol("Hsh2G2","red")}}
    check_P3G2 <- function(){if (Hsh3G2() == get_P3G2h())  { js$backgroundCol("Hsh3G2","#32CD32")} else { js$backgroundCol("Hsh3G2","red")}}
    check_P4G2 <- function(){if (Hsh4G2() == get_P4G2h())  { js$backgroundCol("Hsh4G2","#32CD32")} else { js$backgroundCol("Hsh4G2","red")}}
    
    check_P1G3 <- function(){if (Hsh1G3() == get_P1G3h())  { js$backgroundCol("Hsh1G3","#32CD32")} else { js$backgroundCol("Hsh1G3","red")}}
    check_P2G3 <- function(){if (Hsh2G3() == get_P2G3h())  { js$backgroundCol("Hsh2G3","#32CD32")} else { js$backgroundCol("Hsh2G3","red")}}
    check_P3G3 <- function(){if (Hsh3G3() == get_P3G3h())  { js$backgroundCol("Hsh3G3","#32CD32")} else { js$backgroundCol("Hsh3G3","red")}}
    check_P4G3 <- function(){if (Hsh4G3() == get_P4G3h())  { js$backgroundCol("Hsh4G3","#32CD32")} else { js$backgroundCol("Hsh4G3","red")}}
    
    check_G1 <- function(){if (actG1() == get_hashG1())  { js$backgroundCol("actG1","#32CD32")} else { js$backgroundCol("actG1","red")}}
    check_G2 <- function(){if (actG2() == get_hashG2())  { js$backgroundCol("actG2","#32CD32")} else { js$backgroundCol("actG2","red")}}
    check_G3 <- function(){if (actG3() == get_hashG3())  { js$backgroundCol("actG3","#32CD32")} else { js$backgroundCol("actG3","red")}}
    
    check_U <- function(){if (actU() == get_U())  { js$backgroundCol("actU","#32CD32")} else { js$backgroundCol("actU","red")}}
    
    ## **-------------- **
    # bring actual configuration and signatures with API
    observeEvent(input$conf, {
        ## code below will be executed using button 'config'
        updateTextInput(session, inputId = "Prt1G1", value = get_P1G1())
        updateTextInput(session, inputId = "Prt2G1", value = get_P2G1())
        updateTextInput(session, inputId = "Prt3G1", value = get_P3G1())
        updateTextInput(session, inputId = "Prt4G1", value = get_P4G1())
        
        updateTextInput(session, inputId = "Prt1G2", value = get_P1G2())
        updateTextInput(session, inputId = "Prt2G2", value = get_P2G2())
        updateTextInput(session, inputId = "Prt3G2", value = get_P3G2())
        updateTextInput(session, inputId = "Prt4G2", value = get_P4G2())
        
        updateTextInput(session, inputId = "Prt1G3", value = get_P1G3())
        updateTextInput(session, inputId = "Prt2G3", value = get_P2G3())
        updateTextInput(session, inputId = "Prt3G3", value = get_P3G3())
        updateTextInput(session, inputId = "Prt4G3", value = get_P4G3())
        
        updateTextInput(session, inputId = "reqG1", value = get_hashG1())
        updateTextInput(session, inputId = "reqG2", value = get_hashG2())
        updateTextInput(session, inputId = "reqG3", value = get_hashG3())
        
        updateTextInput(session, inputId = "reqU", value = get_U())
    })
    ## **-------------- **
    
    ## **-------------- **
    # actual configuration, generation of signatures
    observeEvent(input$sign, {
        ## code below will be executed using button check
        # we must explicitly update values in the text field everytime
        updateTextInput(session, inputId = "Hsh1G1", value = Hsh1G1())
        updateTextInput(session, inputId = "Hsh2G1", value = Hsh2G1())
        updateTextInput(session, inputId = "Hsh3G1", value = Hsh3G1())
        updateTextInput(session, inputId = "Hsh4G1", value = Hsh4G1())

        updateTextInput(session, inputId = "Hsh1G2", value = Hsh1G2())
        updateTextInput(session, inputId = "Hsh2G2", value = Hsh2G2())
        updateTextInput(session, inputId = "Hsh3G2", value = Hsh3G2())
        updateTextInput(session, inputId = "Hsh4G2", value = Hsh4G2())
        
        updateTextInput(session, inputId = "Hsh1G3", value = Hsh1G3())
        updateTextInput(session, inputId = "Hsh2G3", value = Hsh2G3())
        updateTextInput(session, inputId = "Hsh3G3", value = Hsh3G3())
        updateTextInput(session, inputId = "Hsh4G3", value = Hsh4G3())
        
        updateTextInput(session, inputId = "actG1", value = GR1())
        
        updateTextInput(session, inputId = "actG2", value = GR2())
        
        updateTextInput(session, inputId = "actG3", value = GR3())
        
        updateTextInput(session, inputId = "actU", value = U())

    })
    ## **-------------- **
    
    ## **-------------- **
    observeEvent(input$Prt1G1, { 
        check_P1G1()
        Hsh1G1()
        })
    observeEvent(input$Prt2G1, { check_P2G1() })
    observeEvent(input$Prt3G1, { check_P3G1() })
    observeEvent(input$Prt4G1, { check_P4G1() })

    observeEvent(input$Prt1G2, { check_P1G2() })
    observeEvent(input$Prt2G2, { check_P2G2() })
    observeEvent(input$Prt3G2, { check_P3G2() })
    observeEvent(input$Prt4G2, { check_P4G2() })

    observeEvent(input$Prt1G3, { check_P1G3() })
    observeEvent(input$Prt2G3, { check_P2G3() })
    observeEvent(input$Prt3G3, { check_P3G3() })
    observeEvent(input$Prt4G3, { check_P4G3() })

    observeEvent(input$actG1, { check_G1() })
    #observeEvent(input$reqG1, { check_G1() })
    observeEvent(input$actG2, { check_G2() })
    #observeEvent(input$reqG2, { check_G2() })
    observeEvent(input$actG3, { check_G3() })
    #observeEvent(input$reqG3, { check_G3() })

    observeEvent(input$actU, { check_U() })
    #observeEvent(input$reqU, { check_U() })
    ## **-------------- **

    ## **-------------- **    
    # prepare reactive data object for the visual plot object that will highlight the result 
    ScoreJoined <- reactive({
        #A <- DF() %>% select(2:11) %>% t() %>% as.vector()
        #B <- ScoreNew() %>% t() %>% as.vector()
        #ScoreJoined <- data.frame(Param = A, Index = 1:10, Anomaly = B)
    })
    ## **-------------- **
    
    ## **-------------- **
    # Object DF will contain the resulting 'input' of the users (created for DEMO purposes)
    DF <- reactive({
        # building into the dataframe
        DF <- data.frame(DateCheck = as.POSIXct(Sys.time()+100),
                         Prt1G1 = Prt1G1(), Prt2G1 = Prt2G1(), Prt3G1 = Prt3G1(), Prt4G1 = Prt4G1(),
                         Prt1G1 = Prt1G2(), Prt2G2 = Prt2G2(), Prt3G1 = Prt3G2(), Prt4G2 = Prt4G2(),
                         Prt1G3 = Prt1G3(), Prt2G3 = Prt2G3(), Prt3G3 = Prt3G3(), Prt4G3 = Prt4G3(),
                         stringsAsFactors = F)
    })
    ## **-------------- **
    
    
    
    ## **-------------- **
    # object to keep reactive values on the table responses
    RESP <- eventReactive(input$submit, {
        # joining checks in one table
        if(!exists("OUT")){OUT <<- DF()} else {
        #    
            ADD <<- bind_cols(DF(), ScoreNew())
            OUT <<- bind_rows(OUT, ADD)
        }
        RESP <- OUT
    })
    ## **-------------- **
    
    ## **-------------- **
    # output to download file if needed
    output$downloadTable <- downloadHandler(
        filename = function() { 
            paste("Data-", Sys.Date(), '.csv', sep='') 
        },
        content = function(file) {
            write.csv(RESP(), file)
        }
    )
    ## **-------------- **
    
    ## ****-------------- ****   
    # output the results of the data input
    output$table <- renderTable({ DF()  })
    ## ****-------------- ****   
    
    ## ****-------------- ****   
    # output the results of the anomaly rating
    output$mse <- renderTable({ ScoreNew()  })
    
    ## ****-------------- ****   
    # Visualize responses in the interactive tables, refresh when some buttons are pressed...
    output$responses <- DT::renderDataTable({  RESP()  })     
    ## ****-------------- ****     
    
    
})