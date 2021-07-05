
#library(promises)
#library(future)
#plan(multiprocess)
library(data.table)
library(alfred)
library(shinyjs)
library(ecm)
library(zoo)
library(shinybusy)
library(TTR)
library(caTools)
library(lubridate)
library(plotly)
library(shiny)
library(shinyBS)
library(shinythemes)
library(shinydashboard)
library(shinyalert)
library(shinycssloaders)
library(shinyWidgets)
library(dplyr)
#library(Rsolnp)
#library(pagedown)


#*RStudio*
#global_wd<<-"P:/NEWTREE/Amadeus/CSE_PDE_DHO_FSL/3_FSL/Publications/Economic_Data_Revisions/"
#*Server*
#global_wd<<-"/srv/shiny-server/aams/"
#*For Docker*
#global_wd<<-"/srv/shiny-server/"
#For Shinyapps
#global_wd<<-"~/eco_shiny/"

#*Remove this for Docker*
#setwd(global_wd)


glossary<-as.data.frame(cbind(
  c("Industrial Production","Nonfarm Employees","New Orders Durable Goods","Retail Sales","US Leading Index","Temporary Help Services","Gross Domestic Product","CPI Urban Consumers"),
  c("INDPRO","PAYEMS","DGORDER","RSXFS","USSLIND","TEMPHELPS","GDP","CPIAUCSL")
),stringsAsFactors=F)
names(glossary)<-c("name","mnemonic")
glossary<<-glossary



dbHeader <- dashboardHeader(tags$li(class = "dropdown",  column(actionLink("log_off_button", "Logout", icon("power-off")),width=12), align="right"))
dbHeader$children[[2]]$children <-  tags$a(href='https://www.amadeus.ch',
                                           tags$img(src='AMADEUS_logo_Pantone.svg',height='40'))


#dbHeader$children[[3]]$children <-  tags$li(class = "dropdown", actionButton("home", "Home"), align="right")
#dbHeader$children[[3]]$children <-  div(tags$img(src=logo_path, align="right", height='50px'))
#dbHeader$children[[3]]$children <-  tags$li(class = "dropdown", column(actionLink("log_off_button", "Logout", icon("power-off")),width=12), align="right")





## app.R ##
library(shiny)
library(shinydashboard)
#library(semantic.dashboard)
ui <- dashboardPage(title="Economic Indicators Revision Model",skin="black",
#ui <- dashboardPage(title="Amadeus Allocation Model",
                    dbHeader,
                    dashboardSidebar(collapsed = F,
                      #actionButton("toggleSidebar", "Toggle sidebar"),
                        sidebarMenuOutput("menu"),
                        column(htmlOutput("sidebar_caption0"),width=12),
                        tags$style("#sidebar_caption0 {font-size:14px;color:#B8C7CE}"),
                        column(htmlOutput("sidebar_caption2"),width=12),
                        HTML("<br><br>"),
                        tags$style("#sidebar_series_name {font-size:16px;color:#ffffff}"),     
                        column(htmlOutput("sidebar_series_name"),width=12),  
                      selectizeInput("select_alfred_time_series", "Select Time Series",options=list(create=T,placeholder = 'Select or insert'), choices = glossary$mnemonic,selected="INDPRO",width="100%"),
                      #HTML("<br>"),                      
                      actionButton("load_alfred", "1) Load Archival Data",width="85%",style = "align: left;color: white; background-color: #dd0400"),
                      selectizeInput("select_vintages_compare", "2) Select Vintages to compare",options=list(create=F,placeholder = 'Select vintages'),multiple=T, choices = c(""),selected="",width="100%"),                      
                      radioButtons(
                        "standardize_rebase",
                        "Standardize/Index Comparison",
                        choices = c("Raw","Indexed"),
                        selected = "Indexed",
                        inline = FALSE,
                        width = NULL,
                        choiceNames = c("Raw","Indexed"),
                        choiceValues = c("Raw","Indexed")
                      ),
                      #actionButton("select_vintages_compare_trigger", "2) Load Vintage Comparison",width="85%",style = "align: left;color: white; background-color: #3b5171"),                      
                        tags$style("#sidebar_caption1 {font-size:14px;color:#B8C7CE}"),
                        column(htmlOutput("sidebar_caption1"),width=12),
                      actionButton("load_crisis_charts2", "3) Load Covid Crisis",width="85%",style = "color: white; background-color: #3b5171"),
                      actionButton("load_crisis_charts3", "4) Load 2015 Flash Crash",width="85%",style = "color: white; background-color: #3b5171"),
                      actionButton("load_crisis_charts4", "5) Load Great Financial Crisis",width="85%",style = "color: white; background-color: #3b5171"),
                      actionButton("load_crisis_charts5", "6) Load Dotcom Bubble",width="85%",style = "color: white; background-color: #3b5171"),
                      actionButton("load_crisis_charts6", "7) Load Asian Fin. Crisis",width="85%",style = "color: white; background-color: #3b5171"),
                      actionButton("load_crisis_charts7", "8) Load Black Monday",width="85%",style = "color: white; background-color: #3b5171"),
                      actionButton("load_crisis_charts8", "9) Load 1982 Bear Market",width="85%",style = "color: white; background-color: #3b5171"),
                      actionButton("load_crisis_charts9", "10) Load 1974 Crash",width="85%",style = "color: white; background-color: #3b5171"),
                      actionButton("load_crisis_charts10", "11) Load Kennedy Slide",width="85%",style = "color: white; background-color: #3b5171"),
                      tags$hr()
                    ),
                    dashboardBody(
                      tags$head(tags$style(HTML('
                              .popover-title{
                              color: #428bca;
                              font-size: 12px;
                              background-color: ##FAFAFA;
                              opacity: 0.8;
                              }
                              .popover-header{ 
                              background: ##FAFAFA; 
                              opacity: 0.8;
                              }
                              .popover-content{ 
                              background: ##FAFAFA; 
                              color: #428bca;
                              font-size: 11px;
                              opacity: 0.8;
                              }
                              '))),
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------                      
                      ## changing theme
                      #shinyDashboardThemes(
                      #  theme = "theme_blue_gradient"
                      #),
                      #theme_blue_gradient,

                      useShinyalert(),
                      useShinyjs(),
                      titlePanel("Amadeus Economic Revisions Model",
                                 tags$head(tags$link(rel = "icon", type = "image/png", href = "tree_logo.png"),
                                           tags$title("AllocationModel"),
                                           tags$style(HTML("
                                              /* body */
                                              .content-wrapper, .right-side {
                                              background-color: #ffffff;
                                             }"))
                                           )),       
                                              #background-color: #343e48;



                      fluidPage(
                        fluidRow(
                          add_busy_spinner(spin = "double-bounce", color = "#dd0400"),
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fluidRow(
                            fluidRow(
                                     fluidRow(
                                       box(
                                       column(
                                         tags$style("#welcome_text {font-size:16px;color:#04103b}"),
                                         tags$style("#welcome_text_warning {font-size:14px;color:#dd0400}"),
                                         column(uiOutput("welcome_text"),width=12),
                                         column(uiOutput("alfred_link"),width=12),
                                         column(uiOutput("amadeus_link"),width=12),
                                         column(uiOutput("welcome_text_warning"),width=12)
                                         
                                         
                                        ,width=12),
                                       width="100%")
                                     ),
                                     fluidRow(
                                       column(box(plotlyOutput("raw_data_chart", width = "100%",height="550px")%>% withSpinner(color="#04103b"),width="100%",height = "600px"),width = 6),
                                       column(box(plotlyOutput("revised_unrevised", width = "100%",height="550px")%>% withSpinner(color="#04103b"),width="100%",height = "600px"),width = 6)
                                     ),
                                     fluidRow(
                                       column(box(plotlyOutput("raw_data_comparison_chart", width = "100%",height="550px")%>% withSpinner(color="#04103b"),width="100%",height = "600px"),width = 12),
                                       tags$style("#back_to_top6 {font-size:14px;color:#3c8dbc}"),
                                       column(actionLink("back_to_top6", "Go back to top", icon("arrow-up")),width=12, align="center")
                                     ),
                                     fluidRow(
                                       column(box(plotlyOutput("covid_crisis", width = "100%",height="550px")%>% withSpinner(color="#04103b"),width="100%",height = "600px"),width = 6),
                                       column(box(plotlyOutput("flash_crash_2015", width = "100%",height="550px")%>% withSpinner(color="#04103b"),width="100%",height = "600px"),width = 6),
                                       tags$style("#back_to_top5 {font-size:14px;color:#3c8dbc}"),
                                       column(actionLink("back_to_top5", "Go back to top", icon("arrow-up")),width=12, align="center")
                                     ),     
                                     fluidRow(
                                       column(box(plotlyOutput("crisis_2008_beginning", width = "100%",height="550px")%>% withSpinner(color="#04103b"),width="100%",height = "600px"),width = 6),
                                       column(box(plotlyOutput("crisis_2008_end", width = "100%",height="550px")%>% withSpinner(color="#04103b"),width="100%",height = "600px"),width = 6),
                                       tags$style("#back_to_top4 {font-size:14px;color:#3c8dbc}"),
                                       column(actionLink("back_to_top4", "Go back to top", icon("arrow-up")),width=12, align="center")
                                     ),                                     
                                     fluidRow(
                                       column(box(plotlyOutput("dotcom_crisis", width = "100%",height="550px")%>% withSpinner(color="#04103b"),width="100%",height = "600px"),width = 6),
                                       column(box(plotlyOutput("asian_financial_crisis", width = "100%",height="550px")%>% withSpinner(color="#04103b"),width="100%",height = "600px"),width = 6),
                                       tags$style("#back_to_top3 {font-size:14px;color:#3c8dbc}"),
                                       column(actionLink("back_to_top3", "Go back to top", icon("arrow-up")),width=12, align="center")
                                     ),
                                     fluidRow(
                                       column(box(plotlyOutput("black_monday", width = "100%",height="550px")%>% withSpinner(color="#04103b"),width="100%",height = "600px"),width = 6),
                                       column(box(plotlyOutput("crisis_1982", width = "100%",height="550px")%>% withSpinner(color="#04103b"),width="100%",height = "600px"),width = 6),
                                       tags$style("#back_to_top2 {font-size:14px;color:#3c8dbc}"),
                                       column(actionLink("back_to_top2", "Go back to top", icon("arrow-up")),width=12, align="center")
                                     ),
                                     fluidRow(
                                       column(box(plotlyOutput("crisis_1974", width = "100%",height="550px")%>% withSpinner(color="#04103b"),width="100%",height = "600px"),width = 6),
                                       column(box(plotlyOutput("kennedy_slide", width = "100%",height="550px")%>% withSpinner(color="#04103b"),width="100%",height = "600px"),width = 6),
                                       tags$style("#back_to_top {font-size:18px;color:#3c8dbc}"),
                                       column(actionLink("back_to_top", "Go back to top", icon("arrow-up")),width=12, align="center")
                                     )
                            )
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                          )
                        )
                      ) 
                    )
)

#------------------------------------------->>>
server <- function(input, output,session) 
{ 
  output$revised_unrevised <- renderPlotly({validate(need(input$getImage, ""))})
  output$raw_data_chart <- renderPlotly({validate(need(input$getImage, ""))})
  output$raw_data_comparison_chart <- renderPlotly({validate(need(input$getImage, ""))})
  
  
  output$covid_crisis <- renderPlotly({validate(need(input$getImage, ""))})
  output$flash_crash_2015 <- renderPlotly({validate(need(input$getImage, ""))})
  output$crisis_2008_beginning <- renderPlotly({validate(need(input$getImage, ""))})
  output$crisis_2008_end <- renderPlotly({validate(need(input$getImage, ""))})
  output$dotcom_crisis <- renderPlotly({validate(need(input$getImage, ""))})
  output$asian_financial_crisis <- renderPlotly({validate(need(input$getImage, ""))})
  output$crisis_1982 <- renderPlotly({validate(need(input$getImage, ""))})
  output$crisis_1974 <- renderPlotly({validate(need(input$getImage, ""))})
  output$black_monday <- renderPlotly({validate(need(input$getImage, ""))})
  output$kennedy_slide <- renderPlotly({validate(need(input$getImage, ""))})
  
  
  
  observeEvent(input$load_alfred, {
    output$revised_unrevised <- renderPlotly({validate(need(input$getImage, ""))})
    output$raw_data_chart <- renderPlotly({validate(need(input$getImage, ""))})
    output$raw_data_comparison_chart <- renderPlotly({validate(need(input$getImage, ""))})
    
    output$covid_crisis <- renderPlotly({validate(need(input$getImage, ""))})
    output$flash_crash_2015 <- renderPlotly({validate(need(input$getImage, ""))})
    output$crisis_2008_beginning <- renderPlotly({validate(need(input$getImage, ""))})
    output$crisis_2008_end <- renderPlotly({validate(need(input$getImage, ""))})
    output$dotcom_crisis <- renderPlotly({validate(need(input$getImage, ""))})
    output$asian_financial_crisis <- renderPlotly({validate(need(input$getImage, ""))})
    output$crisis_1982 <- renderPlotly({validate(need(input$getImage, ""))})
    output$crisis_1974 <- renderPlotly({validate(need(input$getImage, ""))})
    output$black_monday <- renderPlotly({validate(need(input$getImage, ""))})
    output$kennedy_slide <- renderPlotly({validate(need(input$getImage, ""))})
  })
  
  updateDateInput(session,"run_backtest_st_date", "Simulation Start", min = "1980-06-01", max = Sys.Date())
  updateDateInput(session,"run_backtest_ed_date", "Simulation End", min = "1999-06-01", max = Sys.Date())
  
  
  #*************************************************************************************************************************#
  #-------------------------------------------------------------------------------------------------------------------------#
  #                                                                                                                         #
  #                                             Login Page                                                                  #
  #                                                                                                                         #
  #-------------------------------------------------------------------------------------------------------------------------#
  #*************************************************************************************************************************#    
  
  
  #Page 1
  # Login Page
  #--------------------------------------------------------------------------------------------
  vals <- reactiveValues(data = NULL)
  dataModal <- function(failed = FALSE) {
    modalDialog(
      span('Insert login'),
      textInput("login_user_input", "User",
                placeholder = 'username',width="100%"
      ),
      passwordInput("login_password_input", "Password",
                    placeholder = 'password',width="100%"
      ),
      if (failed)
        div(tags$b("Invalid name or password", style = "color: red;")),
      
      footer = tagList(
        #modalButton("Cancel"),
        column(actionButton("login_ok", "OK",width="100%",style = "color: white; background-color: #04103b"),width=12)
      ),
      easyClose = FALSE, fade = TRUE)
  }
  
  # Show modal when button is clicked.
  showModal(dataModal())
  
  observeEvent(input$log_off_button, {
    showModal(dataModal_logout())
    session$close()
  })
  
  dataModal_logout <- function(failed = FALSE) {
    modalDialog(
      span('You ve been logged out successfully'),
      footer = tagList(
        #modalButton("Cancel"),
        column(span(),width=12)
      ),
      easyClose = FALSE, fade = TRUE)
  }

  observeEvent(input$login_ok, {
    # Check that data object exists and is data frame.
    #sqlstr<-paste("select * from fdsl.login where login_name = '",input$login_user_input,"' and login_password = '", input$login_password_input,"'",sep="")
    #df<-dbGetQuery(pool,sqlstr)    
    df<-as.data.frame("Free")
    if(input$login_user_input=="alfred" & input$login_password_input == "alfred")
    {
      removeModal()
    }else{
      #removeModal()
      shinyalert("Failed", "Logged", type = "error")  
    }
    
  })


#-------------------------------------------------------------------------------------------------------------------------   
  

  observeEvent(input$back_to_top, {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    runjs('window.scrollTo(0, 0);')
  })
  observeEvent(input$back_to_top1, {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    runjs('window.scrollTo(0, 0);')
  })
  observeEvent(input$back_to_top2, {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    runjs('window.scrollTo(0, 0);')
  })
  observeEvent(input$back_to_top3, {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    runjs('window.scrollTo(0, 0);')
  })
  observeEvent(input$back_to_top4, {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    runjs('window.scrollTo(0, 0);')
  })
  observeEvent(input$back_to_top5, {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    runjs('window.scrollTo(0, 0);')
  })
  observeEvent(input$back_to_top6, {
    shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    runjs('window.scrollTo(0, 0);')
  })
  
  output$sidebar_series_name <- renderText({
      HTML(paste0(as.character(glossary$name[glossary$mnemonic==input$select_alfred_time_series]) , "<br>"          ))
  })
  
  output$sidebar_caption0 <- renderText({
    HTML(paste0("","Please select a time series to load Archival Economic Data from ALFRED and visualize it for the various Financial Crisis our model is covering. If the time series, you would like to check is not available, just clear the select field and insert a valid ALFRED mnemonic." , "<br>", "<br>","Selected Indicator:"
    ))
  })
  
  output$sidebar_caption1 <- renderText({
    HTML(paste0("","Please click here to visualize a crisis and scroll down for the result." , "<br>"
    ))
  })
  
  output$welcome_text <- renderText({
    HTML(paste0("","This application visualizes economic indicators alongside their vintages to illustrate the impact of data revisions. It thereby focuses on times of financial turmoil (stock market crashes and financial crisis) and the respective turning points." , "<br>",
                "For additional information and the background and motivation behind this application, please visit the paper published on our website." , "<br>",
                "All data is sourced from ALFRED, the Archival Economic Data platform of the Federal Reserve Bank of St Louis." , "<br>",
                "For more information on ALFRED, please visit the website. ALFRED currently covers more than 800.000 time series from 9 categories of which we have only included a handfull in the select field. However, you can obtain the mnemonics from further time series on ALFRED and insert them in the select input to run the model." , "<br>"
    ))
  })
  
  output$welcome_text_warning <- renderText({
    HTML(paste0("","Please Note: ","<br>",
                "- Some of the datasets covered by this model are quite big. So please be patient if it takes a while to load." , "<br>",
                "- To make all vintages comparable, we index the data to 1 for all time series displayed." , "<br>",
                "- This apps loads Archival Economic Data directly from ALFRED. Some of these datasets are quite big and loading it may take a while. Please excuse any inconvenience caused by this."
    ))
  })
  
  output$alfred_link <- renderUI({
    a("Click here to visit ALFRED for more information and data", href="https://alfred.stlouisfed.org/", target="_blank")
  })
  
  output$amadeus_link <- renderUI({
    a("See our paper", href="https://www.amadeus.ch/en/news", target="_blank")
  })
  
 
  
#-------------------------------------------------------------------------------------------------------------------------       
  #Load Data
  observeEvent(input$load_alfred, {
    tryCatch({  
    show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Loading archival data. For long time series this will take a while as the datasets are quite big.")
      #alfred<<-get_alfred_series("INDPRO", "INDPRO")  
      alfred<-get_alfred_series(input$select_alfred_time_series, input$select_alfred_time_series)  
      names(alfred)[3]<-"mnemonic"
      alfred<-alfred[alfred$date>=min(alfred$realtime_period),]
      alfred$realtime_period<-as.character(alfred$realtime_period)
      alfred<-as.data.table(alfred)
      alfred<-alfred %>% group_by(realtime_period) %>% mutate(pop=mnemonic/head(mnemonic,1))
      alfred$yoy<-ifelse(alfred$realtime_period==lagpad(alfred$realtime_period,k=12),alfred$mnemonic/lagpad(alfred$mnemonic,k=12)-1,NA)

      alfred<<-alfred

    remove_modal_spinner(session = getDefaultReactiveDomain())  
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  })
  
  observeEvent(input$load_alfred, {
      tryCatch({  
        vintages<-unique(alfred$realtime_period)
        vintages<-sort(vintages, decreasing = T)
        updateSelectizeInput(session, 'select_vintages_compare', choices =vintages,selected=c(head(vintages,1),tail(head(vintages,2),1)), server = TRUE)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    })
  #First Chart Raw Data
  observeEvent(input$load_alfred, {
    isolate({
      show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Adjusting the data and loading visualization.")     
      output$raw_data_chart<-renderPlotly({
        tryCatch({  
          unrevised<-alfred %>% group_by(realtime_period) %>% do(tail(., 1))
          unrevised$id<-"Unrevised"
          today<-alfred[alfred$realtime_period==max(alfred$realtime_period),]
          today$id<-"Most Recent"
          unr<-rbind(unrevised,today)
          unr<-unr[!is.na(unr$pop) & unr$date>=min(unrevised$date),]
          cols<-c("#dd0400","#04103b")
          
          m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
          p<-plot_ly(today,x=~as.Date(date),y=~mnemonic,line = list(color = '#dd0400'),type="scatter",mode="line")%>%
            layout(margin=m,title="Most Recent Time Series (Unadjusted)",xaxis = list(title=""), yaxis = list(title="",tickformat =".0"),legend = list(orientation = "h",xanchor = "center",x = 0.5))
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      })
      remove_modal_spinner(session = getDefaultReactiveDomain())  
    })
  })
  
  #First Chart Raw Data
  observeEvent(input$select_vintages_compare, {
    isolate({
      tryCatch({  
      output$raw_data_comparison_chart<-renderPlotly({

          rtp12_suba<-alfred[as.character(alfred$realtime_period) %in% head(input$select_vintages_compare,20),]
        
          rel_dates<-alfred[,c("date","realtime_period")] %>% group_by(realtime_period) %>% do(tail(., 1))
          rel_dates<-rel_dates %>% group_by(date) %>% do(tail(., 1))
          names(rel_dates)<-c("date","rel_date")
          rtp12_suba<-merge(rtp12_suba,rel_dates,by="date")
          rtp12_suba$rel_date[rtp12_suba$date==max(rtp12_suba$date)]<-min(alfred$realtime_period[alfred$date==max(rtp12_suba$date)])
          
          rtp12_suba<-rtp12_suba %>% group_by(realtime_period) %>% mutate(pop=mnemonic/head(mnemonic,1))
          
          col_aq2<-c("#04103b","#3b5171")
          cols = colorRampPalette(col_aq2[1:2])(length(unique(rtp12_suba$realtime_period))-1)
          cols<-c(cols,"#dd0400")
          
          if(input$standardize_rebase=="Indexed")
          {
            m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
            p<-plot_ly(rtp12_suba,x=as.Date(rtp12_suba$rel_date),y=round(rtp12_suba$pop,4),color=rtp12_suba$realtime_period,colors=cols,type="scatter",mode="line")  %>%
              layout(margin=m,title="Flexible Comparison (Standardized/Indexed)",xaxis = list(title=""), yaxis = list(title="",tickformat =".0"),legend = list(orientation = "h",xanchor = "center",x = 0.5))
          }else{
            m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
            p<-plot_ly(rtp12_suba,x=as.Date(rtp12_suba$rel_date),y=round(rtp12_suba$mnemonic,4),color=rtp12_suba$realtime_period,colors=cols,type="scatter",mode="line")  %>%
              layout(margin=m,title="Flexible Comparison Raw Data as Reported",xaxis = list(title=""), yaxis = list(title="",tickformat =".0"),legend = list(orientation = "h",xanchor = "center",x = 0.5))
          }

      })
      #runjs('document.getElementById("revised_unrevised").scrollIntoView();')
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    })
  })
  
  
  
  #First Chart Unrevised vs Revised Time Series
  observeEvent(input$load_alfred, {
    isolate({
      show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Adjusting the data and loading visualization.")     
      output$revised_unrevised<-renderPlotly({
        tryCatch({  

        unrevised<-alfred %>% group_by(realtime_period) %>% do(tail(., 1))
        unrevised$id<-"Unrevised"
        today<-alfred[alfred$realtime_period==max(alfred$realtime_period),]
        today$id<-"Most Recent"
        unr<-rbind(unrevised,today)
        unr<-unr[!is.na(unr$pop) & unr$date>=min(unrevised$date),]
        cols<-c("#dd0400","#04103b")

        
        m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
        p<-plot_ly(unr,x=~as.Date(date),y=~pop,color=~id,colors=cols,type="scatter",mode="line")%>%
          layout(margin=m,title="Latest vs Revised Data (Rebased & Standardized)",xaxis = list(title=""), yaxis = list(title="",tickformat =".0"),legend = list(orientation = "h",xanchor = "center",x = 0.5))
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      })
      remove_modal_spinner(session = getDefaultReactiveDomain())  
    })
  })
  
  
  #1974 Crisis
  observeEvent(input$load_crisis_charts9, {
        isolate({
          show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Slicing the data and loading visualization.")      
          output$crisis_1974<-renderPlotly({
            tryCatch({  
              rtp12<-alfred[as.character(year(alfred$realtime_period)) %in% c("1972","1973") | as.character((alfred$realtime_period)) == max(alfred$realtime_period),]
              #rtp12<-rtp12%>%group_by(realtime_period)%>%mutate(yoy=indpro/lagpad(indpro,k=12)-1)
              rtp12_sub<-rtp12[rtp12$date>as.Date("1972-01-01") & rtp12$date<=as.Date("1974-01-01"),]
              rtp12_sub<-rtp12_sub %>% group_by(realtime_period) %>% mutate(pop=mnemonic/head(mnemonic,1))
              
              rel_dates<-rtp12_sub[,c("date","realtime_period")] %>% group_by(realtime_period) %>% do(tail(., 1))
              rel_dates<-rel_dates %>% group_by(date) %>% do(tail(., 1))
              names(rel_dates)<-c("date","rel_date")
              rtp12_sub<-merge(rtp12_sub,rel_dates,by="date")
              rtp12_sub$rel_date[rtp12_sub$date==max(rtp12_sub$date)]<-min(alfred$realtime_period[alfred$date==max(rtp12_sub$date)])
              
              col_aq2<-c("#04103b","#3b5171")
              cols = colorRampPalette(col_aq2[1:2])(length(unique(rtp12_sub$realtime_period))-1)
              cols<-c(cols,"#dd0400")
              
              
              m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
              p<-plot_ly(rtp12_sub,x=as.Date(rtp12_sub$rel_date),y=round(rtp12_sub$pop,4),color=rtp12_sub$realtime_period,colors=cols,type="scatter",mode="line")  %>%
                layout(margin=m,title="1974 Stock Market Crash",xaxis = list(title=""), yaxis = list(title="",tickformat =".0"),legend = list(orientation = "h",xanchor = "center",x = 0.5))

            }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
          })     
          runjs('document.getElementById("crisis_1974").scrollIntoView();')
          shinyjs::addClass(selector = "body", class = "sidebar-collapse")
          remove_modal_spinner(session = getDefaultReactiveDomain())  
        })
  })
  
  #Kennedy Slide
  observeEvent(input$load_crisis_charts10, {
    isolate({
      show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Slicing the data and loading visualization.")      
      output$kennedy_slide<-renderPlotly({
        tryCatch({  
          rtp12<-alfred[as.character(year(alfred$realtime_period)) %in% c("1961","1962") | as.character((alfred$realtime_period)) == max(alfred$realtime_period),]
          #rtp12<-rtp12%>%group_by(realtime_period)%>%mutate(yoy=indpro/lagpad(indpro,k=12)-1)
          rtp12_sub<-rtp12[rtp12$date>as.Date("1961-12-01") & rtp12$date<=as.Date("1963-01-01"),]
          rtp12_sub<-rtp12_sub %>% group_by(realtime_period) %>% mutate(pop=mnemonic/head(mnemonic,1))
          
          rel_dates<-rtp12_sub[,c("date","realtime_period")] %>% group_by(realtime_period) %>% do(tail(., 1))
          rel_dates<-rel_dates %>% group_by(date) %>% do(tail(., 1))
          names(rel_dates)<-c("date","rel_date")
          rtp12_sub<-merge(rtp12_sub,rel_dates,by="date")
          rtp12_sub$rel_date[rtp12_sub$date==max(rtp12_sub$date)]<-min(alfred$realtime_period[alfred$date==max(rtp12_sub$date)])
          
          col_aq2<-c("#04103b","#3b5171")
          cols = colorRampPalette(col_aq2[1:2])(length(unique(rtp12_sub$realtime_period))-1)
          cols<-c(cols,"#dd0400")
          
          
          m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
          p<-plot_ly(rtp12_sub,x=as.Date(rtp12_sub$rel_date),y=round(rtp12_sub$pop,4),color=rtp12_sub$realtime_period,colors=cols,type="scatter",mode="line")  %>%
            layout(margin=m,title="Kennedy Slide of 1962",xaxis = list(title=""), yaxis = list(title="",tickformat =".0"),legend = list(orientation = "h",xanchor = "center",x = 0.5))
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      })     
      
      runjs('document.getElementById("kennedy_slide").scrollIntoView();')
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      remove_modal_spinner(session = getDefaultReactiveDomain())  
    })
  })
 
  
  #Black Monday
  observeEvent(input$load_crisis_charts7, {
    isolate({
      show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Slicing the data and loading visualization.")      
      output$black_monday<-renderPlotly({
        tryCatch({ 
          rtp12<-alfred[as.character(year(alfred$realtime_period)) %in% c("1987") |as.character((alfred$realtime_period)) == max(alfred$realtime_period),]
          #rtp12<-rtp12%>%group_by(realtime_period)%>%mutate(yoy=indpro/lagpad(indpro,k=12)-1)
          rtp12_sub<-rtp12[rtp12$date>as.Date("1987-01-01") & rtp12$date<=as.Date("1988-01-01"),]
          rtp12_sub<-rtp12_sub %>% group_by(realtime_period) %>% mutate(pop=mnemonic/head(mnemonic,1))
          
          rel_dates<-rtp12_sub[,c("date","realtime_period")] %>% group_by(realtime_period) %>% do(tail(., 1))
          rel_dates<-rel_dates %>% group_by(date) %>% do(tail(., 1))
          names(rel_dates)<-c("date","rel_date")
          rtp12_sub<-merge(rtp12_sub,rel_dates,by="date")
          rtp12_sub$rel_date[rtp12_sub$date==tail(rtp12_sub$date,1)]<-min(alfred$realtime_period[alfred$date==tail(rtp12_sub$date,1)])
          
          col_aq2<-c("#04103b","#3b5171")
          cols = colorRampPalette(col_aq2[1:2])(length(unique(rtp12_sub$realtime_period))-1)
          cols<-c(cols,"#dd0400")
          
          
          m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
          p<-plot_ly(rtp12_sub,x=as.Date(rtp12_sub$rel_date),y=round(rtp12_sub$pop,4),color=rtp12_sub$realtime_period,colors=cols,type="scatter",mode="line")  %>%
            layout(margin=m,title="Black Monday",xaxis = list(title=""), yaxis = list(title="",tickformat =".1"),legend = list(orientation = "h",xanchor = "center",x = 0.5))

        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      })     
      runjs('document.getElementById("black_monday").scrollIntoView();')
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      remove_modal_spinner(session = getDefaultReactiveDomain())  
    })
  })  
     
  #1982 Crisis
  observeEvent(input$load_crisis_charts8, {
    isolate({
      show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Slicing the data and loading visualization.")      
      output$crisis_1982<-renderPlotly({
        tryCatch({ 
        rtp12<-alfred[as.character(year(alfred$realtime_period)) %in% c("1981","1982","1983") |as.character((alfred$realtime_period)) == max(alfred$realtime_period),]
        #rtp12<-rtp12%>%group_by(realtime_period)%>%mutate(yoy=indpro/lagpad(indpro,k=12)-1)
        rtp12_sub<-rtp12[rtp12$date>as.Date("1980-01-01") & rtp12$date<=as.Date("1984-01-01"),]
        rtp12_sub<-rtp12_sub %>% group_by(realtime_period) %>% mutate(pop=mnemonic/head(mnemonic,1))
        
        rel_dates<-rtp12_sub[,c("date","realtime_period")] %>% group_by(realtime_period) %>% do(tail(., 1))
        rel_dates<-rel_dates %>% group_by(date) %>% do(tail(., 1))
        names(rel_dates)<-c("date","rel_date")
        rtp12_sub<-merge(rtp12_sub,rel_dates,by="date")
        rtp12_sub$rel_date[rtp12_sub$date==tail(rtp12_sub$date,1)]<-min(alfred$realtime_period[alfred$date==tail(rtp12_sub$date,1)])
        
        col_aq2<-c("#04103b","#3b5171")
        cols = colorRampPalette(col_aq2[1:2])(length(unique(rtp12_sub$realtime_period))-1)
        cols<-c(cols,"#dd0400")
        
        
        m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
        p<-plot_ly(rtp12_sub,x=as.Date(rtp12_sub$rel_date),y=round(rtp12_sub$pop,4),color=rtp12_sub$realtime_period,colors=cols,type="scatter",mode="line")  %>%
          layout(margin=m,title="1982 Slowdown",xaxis = list(title=""), yaxis = list(title="",tickformat =".1"),legend = list(orientation = "h",xanchor = "center",x = 0.5))
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      })     
      runjs('document.getElementById("crisis_1982").scrollIntoView();')
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      remove_modal_spinner(session = getDefaultReactiveDomain())  
    })
  })  
  
  
  #Dotcom Crisis
  observeEvent(input$load_crisis_charts5, {
    isolate({
      show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Slicing the data and loading visualization.")      
      output$dotcom_crisis<-renderPlotly({
        tryCatch({ 
        rtp12<-alfred[as.character(year(alfred$realtime_period)) %in% c("2000") |as.character((alfred$realtime_period)) == max(alfred$realtime_period),]
        #rtp12<-rtp12%>%group_by(realtime_period)%>%mutate(yoy=indpro/lagpad(indpro,k=12)-1)
        rtp12_sub<-rtp12[rtp12$date>as.Date("1999-01-01") & rtp12$date<=as.Date("2001-01-01"),]
        rtp12_sub<-rtp12_sub %>% group_by(realtime_period) %>% mutate(pop=mnemonic/head(mnemonic,1))
        
        rel_dates<-rtp12_sub[,c("date","realtime_period")] %>% group_by(realtime_period) %>% do(tail(., 1))
        rel_dates<-rel_dates %>% group_by(date) %>% do(tail(., 1))
        names(rel_dates)<-c("date","rel_date")
        rtp12_sub<-merge(rtp12_sub,rel_dates,by="date")
        rtp12_sub$rel_date[rtp12_sub$date==tail(rtp12_sub$date,1)]<-min(alfred$realtime_period[alfred$date==tail(rtp12_sub$date,1)])
        
        col_aq2<-c("#04103b","#3b5171")
        cols = colorRampPalette(col_aq2[1:2])(length(unique(rtp12_sub$realtime_period))-1)
        cols<-c(cols,"#dd0400")
        
        m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
        p<-plot_ly(rtp12_sub,x=as.Date(rtp12_sub$rel_date),y=round(rtp12_sub$pop,4),color=rtp12_sub$realtime_period,colors=cols,type="scatter",mode="line")  %>%
          layout(margin=m,title="Dotcom Bubble",xaxis = list(title=""), yaxis = list(title="",tickformat =".0"),legend = list(orientation = "h",xanchor = "center",x = 0.5))
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      })     
      runjs('document.getElementById("dotcom_crisis").scrollIntoView();')
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      remove_modal_spinner(session = getDefaultReactiveDomain())  
    })
  })  
  
  #2008 Crisis Beginning
  observeEvent(input$load_crisis_charts4, {
    isolate({
      show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Slicing the data and loading visualization.")      
      show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Slicing the data and loading visualization.")      
      output$crisis_2008_beginning<-renderPlotly({
        tryCatch({ 
        rtp12<-alfred[as.character(year(alfred$realtime_period)) %in% c("2007","2008") |as.character((alfred$realtime_period)) == max(alfred$realtime_period),]
        #rtp12<-rtp12%>%group_by(realtime_period)%>%mutate(yoy=indpro/lagpad(indpro,k=12)-1)
        rtp12_sub<-rtp12[rtp12$date>as.Date("2006-01-01") & rtp12$date<=as.Date("2009-01-01"),]
        rtp12_sub<-rtp12_sub %>% group_by(realtime_period) %>% mutate(pop=mnemonic/head(mnemonic,1))
        
        rel_dates<-rtp12_sub[,c("date","realtime_period")] %>% group_by(realtime_period) %>% do(tail(., 1))
        rel_dates<-rel_dates %>% group_by(date) %>% do(tail(., 1))
        names(rel_dates)<-c("date","rel_date")
        rtp12_sub<-merge(rtp12_sub,rel_dates,by="date")
        rtp12_sub$rel_date[rtp12_sub$date==tail(rtp12_sub$date,1)]<-min(alfred$realtime_period[alfred$date==tail(rtp12_sub$date,1)])
        
        col_aq2<-c("#04103b","#3b5171")
        cols = colorRampPalette(col_aq2[1:2])(length(unique(rtp12_sub$realtime_period))-1)
        cols<-c(cols,"#dd0400")
        
        m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
        p<-plot_ly(rtp12_sub,x=as.Date(rtp12_sub$rel_date),y=round(rtp12_sub$pop,4),color=rtp12_sub$realtime_period,colors=cols,type="scatter",mode="line")  %>%
          layout(margin=m,title="Great Financial Crisis Beginning",xaxis = list(title=""), yaxis = list(title="",tickformat =".0"),legend = list(orientation = "h",xanchor = "center",x = 0.5))

      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      })     
      runjs('document.getElementById("crisis_2008_beginning").scrollIntoView();')
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      remove_modal_spinner(session = getDefaultReactiveDomain())  
    })
  })    
  
  #2008 Crisis End
  observeEvent(input$load_crisis_charts4, {
    isolate({
      show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Slicing the data and loading visualization.")      
      output$crisis_2008_end<-renderPlotly({
        tryCatch({ 
        rtp12<-alfred[as.character(year(alfred$realtime_period)) %in% c("2009") |as.character((alfred$realtime_period)) == max(alfred$realtime_period),]
        #rtp12<-rtp12%>%group_by(realtime_period)%>%mutate(yoy=indpro/lagpad(indpro,k=12)-1)
        rtp12_sub<-rtp12[rtp12$date>as.Date("2009-01-01") & rtp12$date<=as.Date("2010-01-01"),]
        rtp12_sub<-rtp12_sub %>% group_by(realtime_period) %>% mutate(pop=mnemonic/head(mnemonic,1))
        
        rel_dates<-rtp12_sub[,c("date","realtime_period")] %>% group_by(realtime_period) %>% do(tail(., 1))
        rel_dates<-rel_dates %>% group_by(date) %>% do(tail(., 1))
        names(rel_dates)<-c("date","rel_date")
        rtp12_sub<-merge(rtp12_sub,rel_dates,by="date")
        rtp12_sub$rel_date[rtp12_sub$date==tail(rtp12_sub$date,1)]<-min(alfred$realtime_period[alfred$date==tail(rtp12_sub$date,1)])
        
        col_aq2<-c("#04103b","#3b5171")
        cols = colorRampPalette(col_aq2[1:2])(length(unique(rtp12_sub$realtime_period))-1)
        cols<-c(cols,"#dd0400")
        
        m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
        p<-plot_ly(rtp12_sub,x=as.Date(rtp12_sub$rel_date),y=round(rtp12_sub$pop,4),color=rtp12_sub$realtime_period,colors=cols,type="scatter",mode="line")  %>%
          layout(margin=m,title="Great Financial Crisis Recovery",xaxis = list(title=""), yaxis = list(title="",tickformat =".0"),legend = list(orientation = "h",xanchor = "center",x = 0.5))

        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      })     
      runjs('document.getElementById("crisis_2008_end").scrollIntoView();')
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      remove_modal_spinner(session = getDefaultReactiveDomain())  
    })
  })  
  
  #Summer 2015
  observeEvent(input$load_crisis_charts3, {
    isolate({
      show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Slicing the data and loading visualization.")      
      output$flash_crash_2015<-renderPlotly({
        tryCatch({ 
          rtp12<-alfred[as.character(year(alfred$realtime_period)) %in% c("2015") |as.character((alfred$realtime_period)) == max(alfred$realtime_period),]
          #rtp12<-rtp12%>%group_by(realtime_period)%>%mutate(yoy=indpro/lagpad(indpro,k=12)-1)
          rtp12_sub<-rtp12[rtp12$date>as.Date("2015-01-01") & rtp12$date<=as.Date("2016-01-01"),]
          rtp12_sub<-rtp12_sub %>% group_by(realtime_period) %>% mutate(pop=mnemonic/head(mnemonic,1))
          
          rel_dates<-rtp12_sub[,c("date","realtime_period")] %>% group_by(realtime_period) %>% do(tail(., 1))
          rel_dates<-rel_dates %>% group_by(date) %>% do(tail(., 1))
          names(rel_dates)<-c("date","rel_date")
          rtp12_sub<-merge(rtp12_sub,rel_dates,by="date")
          rtp12_sub$rel_date[rtp12_sub$date==tail(rtp12_sub$date,1)]<-min(alfred$realtime_period[alfred$date==tail(rtp12_sub$date,1)])
          
          col_aq2<-c("#04103b","#3b5171")
          cols = colorRampPalette(col_aq2[1:2])(length(unique(rtp12_sub$realtime_period))-1)
          cols<-c(cols,"#dd0400")
          
          m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
          p<-plot_ly(rtp12_sub,x=as.Date(rtp12_sub$rel_date),y=round(rtp12_sub$pop,4),color=rtp12_sub$realtime_period,colors=cols,type="scatter",mode="line")  %>%
            layout(margin=m,title="2015 Flash Crash",xaxis = list(title=""), yaxis = list(title="",tickformat =".0"),legend = list(orientation = "h",xanchor = "center",x = 0.5))

        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      })     
      runjs('document.getElementById("flash_crash_2015").scrollIntoView();')
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      remove_modal_spinner(session = getDefaultReactiveDomain())  
    })
  })      
  
  #Covid Crisis
  observeEvent(input$load_crisis_charts2, {
    isolate({
      show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Slicing the data and loading visualization.")      
      output$covid_crisis<-renderPlotly({
        tryCatch({ 
          rtp12<-alfred[as.character(year(alfred$realtime_period)) %in% c("2020") |as.character((alfred$realtime_period)) == max(alfred$realtime_period),]
          #rtp12<-rtp12%>%group_by(realtime_period)%>%mutate(yoy=indpro/lagpad(indpro,k=12)-1)
          rtp12_sub<-rtp12[rtp12$date>as.Date("2019-01-01") & rtp12$date<=as.Date("2021-01-01"),]
          rtp12_sub<-rtp12_sub %>% group_by(realtime_period) %>% mutate(pop=mnemonic/head(mnemonic,1))
          
          rel_dates<-rtp12_sub[,c("date","realtime_period")] %>% group_by(realtime_period) %>% do(tail(., 1))
          rel_dates<-rel_dates %>% group_by(date) %>% do(tail(., 1))
          names(rel_dates)<-c("date","rel_date")
          rtp12_sub<-merge(rtp12_sub,rel_dates,by="date")
          rtp12_sub$rel_date[rtp12_sub$date==tail(rtp12_sub$date,1)]<-min(alfred$realtime_period[alfred$date==tail(rtp12_sub$date,1)])
          
          col_aq2<-c("#04103b","#3b5171")
          cols = colorRampPalette(col_aq2[1:2])(length(unique(rtp12_sub$realtime_period))-1)
          cols<-c(cols,"#dd0400")
          
          m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
          p<-plot_ly(rtp12_sub,x=as.Date(rtp12_sub$rel_date),y=round(rtp12_sub$pop,4),color=rtp12_sub$realtime_period,colors=cols,type="scatter",mode="line")  %>%
            layout(margin=m,title="Covid Crisis",xaxis = list(title=""), yaxis = list(title="",tickformat =".0"),legend = list(orientation = "h",xanchor = "center",x = 0.5))

        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      })     
      runjs('document.getElementById("covid_crisis").scrollIntoView();')
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      remove_modal_spinner(session = getDefaultReactiveDomain()) 
    })
  })     
  
  #Asian Financial Crisis
  observeEvent(input$load_crisis_charts6, {
    isolate({
      show_modal_spinner(spin = "semipolar",color = "#dd0400",text = "Slicing the data and loading visualization.")      
      output$asian_financial_crisis<-renderPlotly({
        tryCatch({ 
          rtp12<-alfred[as.character(year(alfred$realtime_period)) %in% c("1997") |as.character((alfred$realtime_period)) == max(alfred$realtime_period),]
          #rtp12<-rtp12%>%group_by(realtime_period)%>%mutate(yoy=indpro/lagpad(indpro,k=12)-1)
          rtp12_sub<-rtp12[rtp12$date>as.Date("1997-01-01") & rtp12$date<=as.Date("1998-01-01"),]
          rtp12_sub<-rtp12_sub %>% group_by(realtime_period) %>% mutate(pop=mnemonic/head(mnemonic,1))
          
          rel_dates<-rtp12_sub[,c("date","realtime_period")] %>% group_by(realtime_period) %>% do(tail(., 1))
          rel_dates<-rel_dates %>% group_by(date) %>% do(tail(., 1))
          names(rel_dates)<-c("date","rel_date")
          rtp12_sub<-merge(rtp12_sub,rel_dates,by="date")
          rtp12_sub$rel_date[rtp12_sub$date==tail(rtp12_sub$date,1)]<-min(alfred$realtime_period[alfred$date==tail(rtp12_sub$date,1)])
          
          col_aq2<-c("#04103b","#3b5171")
          cols = colorRampPalette(col_aq2[1:2])(length(unique(rtp12_sub$realtime_period))-1)
          cols<-c(cols,"#dd0400")
          
          m <- list(l = 10,r = 10,b = 20,t = 50,pad = 4)
          p<-plot_ly(rtp12_sub,x=as.Date(rtp12_sub$rel_date),y=round(rtp12_sub$pop,4),color=rtp12_sub$realtime_period,colors=cols,type="scatter",mode="line")  %>%
            layout(margin=m,title="Asian Financial Crisis",xaxis = list(title=""), yaxis = list(title="",tickformat =".1"),legend = list(orientation = "h",xanchor = "center",x = 0.5))

        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      })     
      runjs('document.getElementById("asian_financial_crisis").scrollIntoView();')
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      remove_modal_spinner(session = getDefaultReactiveDomain())  
    })
  })      
  
    
#-------------------------------------------------------------------------------------------------------------------------   
}
# Return a Shiny app object
shinyApp(ui = ui, server = server)



#sudo cp -r ~/AAMS/aams/ /srv/shiny-server/
#Am@zon2019*

