library(stringr)
library(utf8)
library(ggplot2)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(rsconnect)
library(dplyr)
library(writexl)
options(shiny.maxRequestSize=30*1024^2) 
runApp(
  list(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar( 
        fileInput('file1', 'Հիմնական դատան',
                  accept = c(".xlsx")),
        fileInput('file2', 'Բոլոր կոդերով դատան',
                  accept = c(".xlsx")),
        tabsetPanel(id = 'first_ch',
          
        
        
        
        tabPanel("Որոնում", fluid=TRUE,
        selectInput(inputId = "Expimp",label="Ներմուծում թե Արտահանում",
                    choices= c("Արտահանում","Ներմուծում")),
        tabsetPanel(id = "tabs",
          tabPanel("Տարեկան", fluid = TRUE,
                   
                   
                   selectInput(inputId = "year",label="Ընտրել տարին",
                               choices= c("2007","2008","2009","2010","2011",
                                          "2012","2013","2014","2015",
                                          "2016","2017","2018"),multiple = TRUE)
                   
                   
                   
                   
          ),
          tabPanel("Ամսեկան", fluid = TRUE,
                   selectInput(inputId =  "Monthly", label = "Ընտրեք ներկայացման տեսակը",
                               choices =c("Գումարային","Ամեն ամսյա")),
                   selectInput(inputId = "year_m",label="Ընտրել տարին",
                               choices= c("2007","2008","2009","2010","2011",
                                          "2012","2013","2014","2015",
                                          "2016","2017","2018"),multiple = TRUE),
                   selectInput(inputId = "month_m", label = "Ընտրել Ամիսը",
                               choices = c("Հունվար","Փետրվար","Մարտ","Ապրիլ","Մայիս","Հունիս","Հուլիս"
                                           ,"Օգոստոս","Սեպտեմբեր",
                                                  "Հոկտեմբեր","Նոյեմբեր","Դեկտեմբեր"),multiple = TRUE)
                   # textInput(inputId = "code_m", label = "Ընտրել ապրանքի կոդը")
                   
                   
          ),
          tabPanel("Եռամսյակային", fluid = TRUE,
                   
                   selectInput(inputId = "year_t",label="Ընտրել տարին",
                               choices= c("2007","2008","2009","2010","2011",
                                          "2012","2013","2014","2015",
                                          "2016","2017","2018"),multiple = TRUE),
                   selectInput(inputId = "three",label="Ընտրել եռամսյակ",
                               choices= c("Առաջին","Երկրորդ","Երրորդ","Չորրորդ"))
                   
                   
                   
                   
          ),
          textInput(inputId = "code", label = "Ընտրել ապրանքի կոդը")
        )
        
      ),
      tabPanel("Գրաֆիկներ",
               selectizeInput("names_p", "Name of product",choices=NULL, options = list(maxItems=1)),
               selectInput("variables_p", "Choose variable", choices = c("Export_in_tonnas",
                                                                         "Export","Import_in_tonnas","Import")))),
      actionButton(
        inputId = "submit_loc",
        label = "Submit"
      ),
      
      br(),
      br(),
      downloadButton("downloadData", "Download",class="butt1")),
      
      dashboardBody(
        # fluidRow(
        # tags$hr(),
        dataTableOutput("table"),
        plotOutput("plot1")
        # )
      ))
    ,
    server = shinyServer(function(input, output,session) {
      year_agg <- function(df, Year,main){
        ####
        titles <- main[is.na(main$ID),'Name']
        counter <- 1
        for (i in c(1:nrow(titles))){
          titles[i,'Group'] = counter
          counter = counter +1
        }
        #####
        df_Year <- df[df$Year == Year,]
        agg_df_Year <- df_Year %>%
          group_by(ID) %>%
          summarise(Export_in_tonnas = sum(Export_in_tonnas),
                    Export = sum(Export),
                    Import_in_tonnas = sum(Import_in_tonnas),
                    Import = sum(Import))
        polufinal_Year_1 <- left_join(main, agg_df_Year, by = "ID")
        polufinal_Year_1['Year'] <- Year
        counter <- 0
        for (i in c(1:nrow(polufinal_Year_1))){
          polufinal_Year_1[i,'Group'] = counter
          
          if (is.na(polufinal_Year_1[i,'ID'])){
            counter = counter + 1
          }
        }
        polufinal_Year_1[is.na(polufinal_Year_1$ID),'Group']<- NA
        polufinal_Year_1[3:6][is.na(polufinal_Year_1[3:6])] <- 0
        
        
        agg_total_Year <-polufinal_Year_1 %>%
          group_by(Group)  %>%
          summarise(Total_Export_in_tonnas = sum(Export_in_tonnas),
                    Total_Export = sum(Export),
                    Total_Import_in_tonnas = sum(Import_in_tonnas),
                    Total_Import = sum(Import))
        agg_total_Year <- agg_total_Year[complete.cases(agg_total_Year), ]
        
        
        polufinal_Year_2 <- left_join(titles, agg_total_Year, by = "Group")
        polufinal_Year_2['Group'] <- NULL
        final_Year <- left_join(polufinal_Year_1,polufinal_Year_2, by = "Name")
        
        for (i in c(1:nrow(final_Year))){
          if (is.na(final_Year[i,'ID'])){
            final_Year[i,3:6] <- final_Year[i,9:12]
          }
        }
        final_Year <- final_Year[,-c(8:12)]
        year<- final_Year$Year[1]
        final_Year$Year <- NULL
        colnames(final_Year) <- c("Name","ID",paste0(colnames(final_Year[,c(3:6)]), ".",year))
        
        return (final_Year)
      }
      get_main <- function(){
        ########Reading the main file
        inFile2 <- input$file2
        main  <- readxl::read_xlsx(inFile2$datapath ,sheet = 1, col_names = FALSE) 
        colnames(main) <-  c('Name','ID')
        main$ID<-as.numeric(main$ID)
        
        main[main$ID==9701 & !(is.na(main$ID)),'Name']  <- "Ձեռքով արված նկար"
        main[main$ID==9702 & !(is.na(main$ID)),'Name']  <- "Փորագրանկարի, Էստամպի, վինատպության բնօրինակներ"
        main[main$ID==9704 & !(is.na(main$ID)),'Name']  <- "Քանդակների, արձանիկների բնօրինակներ	"
        main[main$ID==9703 & !(is.na(main$ID)),'Name']  <- "Նամականիշ կամ պետական տուրքի դրոշմանիշ"
        main[main$ID==9706 & !(is.na(main$ID)),'Name']  <- "Հավաքածու կամ հավաքածուի առարկաներ"
        main[main$ID==9705 & !(is.na(main$ID)),'Name']  <- "Հնաոճ իրեր 100ից ավելի տարիքով"
        return (main)
      }
      get_columns <- function(df,exp_imp){
        if (exp_imp=="Արտահանում"){
          return(df[,c("Name","ID",grep('Export', colnames(df), value=TRUE))])
        }
        else if (exp_imp=="Ներմուծում"){
          return(df[,c("Name","ID",grep('Import', colnames(df), value=TRUE))])
        }
        
      }
      tremester_periods <- function(df, quart){
        if (quart=="Առաջին"){
          df1 <- df[df$Period %in% c(1,2,3),]
        }
        else if (quart=="Երկրորդ"){
          df1 <- df[df$Period %in% c(4,5,6),]
        }
        else if (quart=="Երրորդ"){
          df1 <- df[df$Period %in% c(7,8,9),]
        }
        else if (quart=="Չորրորդ"){
          df1 <- df[df$Period %in% c(10,11,12),]
        }
        return (df1)
      }
      choice <- reactive({
        inFile2 <- input$file2
        
        if (is.null(inFile2))
          return(NULL)
        main1 <- get_main()
        choices <- unique(main1$Name)
        return (choices)
      })
      
      my_data <- reactive({
        
        inFile1 <- input$file1
        
        if (is.null(inFile1))
          return(NULL)
        inFile2 <- input$file2
        
        if (is.null(inFile2))
          return(NULL)
        df  <- readxl::read_xlsx(inFile1$datapath, sheet = "Armstat", col_names = TRUE) #Reading the file
        colnames(df) <- c('Name','ID','Year','Period','Export_in_tonnas',
                          'Export','Import_in_tonnas','Import')
        
        df[,2:8] <- apply(df[,2:8],2,as.numeric)
        df$Export <- df$Export/1000
        df$Import <- df$Import/1000
        #Reading the main file
        main <- get_main()
        ###
        

        if(input$tabs=="Տարեկան"){
        
        
        df$Period <- NULL
        agg1 <- df
        years <- as.numeric(input$year)
        fin_y <- main
        for (year in years){
          temp <- year_agg(agg1, year,main)
          fin_y <- inner_join(fin_y, temp, by = c("Name","ID"))
        }
        
        fin_y1 <- get_columns(fin_y,input$Expimp)
        return (fin_y1)
        }
        else if(input$tabs=="Ամսեկան"){
          month_per <- data.frame(Month = c("Հունվար","Փետրվար","Մարտ","Ապրիլ","Մայիս","Հունիս","Հուլիս"
                                            ,"Օգոստոս","Սեպտեմբեր",
                                            "Հոկտեմբեր","Նոյեմբեր","Դեկտեմբեր"),
                                  Period = rep(1:12))
          
          if (input$Monthly == "Գումարային"){
          
          per <- month_per[month_per$Month==input$month_m,"Period"]
          agg1 <- df[df$Period <= per,]
          years <- as.numeric(input$year_m)
          agg1$Period <- NULL
          fin_m <- main
          for (year in years){
            temp <- year_agg(agg1, year,main)
            fin_m <- inner_join(fin_m, temp, by = c("Name","ID"))
          }
          fin_m1 <- get_columns(fin_m,input$Expimp)
          return (fin_m1)
          }
          if (input$Monthly == "Ամեն ամսյա"){
            
            per <- month_per[month_per$Month %in% input$month_m,"Period"]
            # df$Year %in% as.numeric(input$year_m) 
            agg2 <- df[df$Period %in% per &
                         df$Year %in% as.numeric(input$year_m),]
            # fin_m1 <- get_columns(agg2,input$Expimp)
            
            return (agg2)
          }
          
        }
        else if(input$tabs=="Եռամսյակային"){
          agg1 <- tremester_periods(df, input$three)
          agg1$Period<-NULL
          fin_t <- main
          years <- as.numeric(input$year_t)
          for (year in years){
            temp <- year_agg(agg1, year,main)
            fin_t <- inner_join(fin_t, temp, by = c("Name","ID"))
          }
          fin_t1 <- get_columns(fin_t,input$Expimp)
          return (fin_t1)
          
        }
      })
      
      agg_data <- reactive({
        df <- my_data()
        if (input$code == ""){
          return (df)
        }
        else {
          
          text <- input$code
          if  (grepl(',', text)){
            code1 <- unlist(strsplit(text,','))
          } else if (grepl('-',text)){
            code1 <- unlist(strsplit(text,'-'))
            code1 <- seq(as.numeric(code1[1]),as.numeric(code1[2]))
          }
          else {
            code1 <- as.numeric(text)
          }
          
          return (df[df$ID %in% code1,  ])
        }
        
      })
      plot_data <- reactive({
        inFile1 <- input$file1
        
        if (is.null(inFile1))
          return(NULL)
        inFile2 <- input$file2
        
        if (is.null(inFile2))
          return(NULL)
        df  <- readxl::read_xlsx(inFile1$datapath, sheet = 1, col_names = TRUE) #Reading the file
        colnames(df) <- c('Name','ID','Year','Period','Export_in_tonnas',
                          'Export','Import_in_tonnas','Import')
        
        df[,2:8] <- apply(df[,2:8],2,as.numeric)
        
        df$Export <- df$Export/1000
        df$Import <- df$Import/1000
        main  <- readxl::read_xlsx(inFile2$datapath ,sheet = 1, col_names = FALSE) 
        colnames(main) <-  c('Name','ID')
        main$ID<-as.numeric(main$ID)
        
        main[main$ID==9701 & !(is.na(main$ID)),'Name']  <- "Ձեռքով արված նկար"
        main[main$ID==9702 & !(is.na(main$ID)),'Name']  <- "Փորագրանկարի, Էստամպի, վինատպության բնօրինակներ"
        main[main$ID==9704 & !(is.na(main$ID)),'Name']  <- "Քանդակների, արձանիկների բնօրինակներ	"
        main[main$ID==9703 & !(is.na(main$ID)),'Name']  <- "Նամականիշ կամ պետական տուրքի դրոշմանիշ"
        main[main$ID==9706 & !(is.na(main$ID)),'Name']  <- "Հավաքածու կամ հավաքածուի առարկաներ"
        main[main$ID==9705 & !(is.na(main$ID)),'Name']  <- "Հնաոճ իրեր 100ից ավելի տարիքով"
        rep_main <- data.frame("Name" = rep(main$Name, 11),"ID"=rep(main$ID, 11))
        rep_main$Year <- 0
        year_1 <- data.frame("Year"=c(2007:2017), "i"=c(1:11))
        for (i in c(0:10)){
          rep_main$Year[(i *1244): ((i+1)*1244)] <- year_1[i+1,"Year"]
        }
        titles <- main[is.na(main$ID),'Name']
        counter <- 1
        for (i in c(1:nrow(titles))){
          titles[i,'Group'] = counter
          counter = counter +1
        }
        test1 <- df %>%
          filter(Year != 2018) %>%
          group_by(ID,Year) %>%
          summarise(Export_in_tonnas = sum(Export_in_tonnas),
                    Export = sum(Export),
                    Import_in_tonnas = sum(Import_in_tonnas),
                    Import = sum(Import))
        
        
        
        test2 <- left_join(rep_main, test1, by = c("ID","Year"))
        test2[4:7][is.na(test2[4:7])] <- 0
        counter <- 0
        
        for (i in c(1:nrow(test2))){
          if ((i-1)%%1244==0) {
            counter = 0
          }
          if (is.na(test2[i,'ID'])) {
            counter = counter +1
          }
          test2[i,'Group'] = counter
        }
        
        unique(test2$Group)
        
        cat <- test2 %>%
          group_by(Group, Year) %>%
          summarise(Export_in_tonnas1 = sum(Export_in_tonnas),
                    Export1 = sum(Export),
                    Import_in_tonnas1 = sum(Import_in_tonnas),
                    Import1 = sum(Import))
        
        test3 <- left_join(titles, cat, by = "Group")
        test3['Group'] <- NULL
        test2$Name <- as.character(test2$Name)
        final <- left_join(test2,test3, by = c("Name","Year"))
        
        for (i in c(1:nrow(final))){
          if (is.na(final[i,'ID'])){
            final[i,4:7] <- final[i,9:12]
          }
        }
        final <- final[,-c(8:12)]
        return (final)
      })
      observe({
        updateSelectizeInput(session,"names_p", choices=choice())
      })
      observeEvent(
        eventExpr = input[["submit_loc"]],
        handlerExpr = {
          if (input$first_ch=="Որոնում"){
          output$table <- renderDataTable({
            
            agg_data();
            })}
          else if (input$first_ch == "Գրաֆիկներ"){
            
            output$plot1 <- renderPlot({
              plot_dat1 <- plot_data()
              
              names <- as.character(input$names_p)
              variables <- as.character(input$variables_p)
              variables <- c(variables, "Year","Name")
              if (!(is.na(names))){
                temp  <- plot_dat1[plot_dat1$Name %in% c(names),variables]
                # print(colnames(temp))
                ggplot(temp) +
                  geom_point(aes(x=temp[,2],y=temp[,1]), color="Red")+
                  geom_line(aes(x=temp[,2],y=temp[,1])) + 
                  scale_x_discrete(name ="Year",limits=c(2007:2017)) + labs(title=input$names_p)+
                  ylab(as.character(input$variables_p))
              }
              
            })
          }
            
           })
      
      output$downloadData <- downloadHandler(
        filename = function() {
          ("filtered.xlsx")
        },
        content = function(file) {
          write_xlsx(agg_data(), file)
        }
      )
      
    })
  ))


