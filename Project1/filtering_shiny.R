library(stringr)
library(utf8)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(rsconnect)
library(dplyr)
library(writexl)

runApp(
  list(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar( fileInput('file1', 'Data from Armstat',
                                  accept = c(".xlsx")),
                        fileInput('file2', 'Main file',
                                  accept = c(".xlsx")),
                        selectInput(inputId = "year",label="Choose year",
                                    choices= "waiting for data",multiple = TRUE),
                        selectInput(inputId = "Expimp",label="Select type of output",
                                    choices= c("Export","Import", "All", "Decomposition","Import Decomposition")),
                       
                        actionButton(
                          inputId = "submit_loc",
                          label = "Submit"
                        ),
                        br(),
                        br(),
                        downloadButton("downloadData", "Download",class="butt1")
                        # tags$head(tags$style(".butt1{background-color:blue;} .butt1{color: black;} .butt1{font-family: Courier New} ,butt1{align: center}"))
                        ),
      
      dashboardBody(
        # fluidRow(
        # tags$hr(),
        dataTableOutput("table"),
        textOutput("txt")
        # )
      ))
    ,
    server = shinyServer(function(input, output,session) {
      choice <- reactive({
        inFile1 <- input$file1
        if (is.null(inFile1))
          return(NULL)
        df  <- readxl::read_xlsx(inFile1$datapath, sheet = "Armstat", col_names = TRUE) #Reading the file
        colnames(df) <- c('Name','ID','Year','Period','Export_in_tonnas',
                          'Export','Import_in_tonnas','Import')
        choices <- unique(df$Year)
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
        choices <- unique(df$Year)
        df$Period <- NULL
        df[,2:7] <- apply(df[,2:7],2,as.numeric)
        df$Export <- df$Export/1000
        df$Import <- df$Import/1000
        #Reading the main file
        main  <- readxl::read_xlsx(inFile2$datapath, sheet = "Sheet1", col_names = FALSE) 
        colnames(main) <-  c('Name','ID')
        main$ID<-as.numeric(main$ID)
        ###
        
        #Seperating titles and assigning groups to it
        titles <- main[is.na(main$ID),'Name']
        counter <- 1
        for (i in c(1:nrow(titles))){
          titles[i,'Group'] = counter
          counter = counter +1
        }
        
        ##
        year_agg <- function(df, Year){
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
              counter = counter +1
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


          return (final_Year)
        }
        
        years <- as.numeric(input$year)
        final_year1 <- year_agg(df,years[1])
        final_year2 <- year_agg(df,years[2])
        first_output <- inner_join(final_year1, final_year2,
                                   by = c("Name","ID"),  suffix = c(paste0(".",years[1]), paste0(".",years[2])))
        first_output[,'Year.2017'] <- NULL
        first_output[,'Year.2018'] <- NULL
        if (input$Expimp == "All") {
          all <- inner_join(final_year1, final_year2, by = c("Name","ID"),  suffix = c(paste0(".",years[1]), paste0(".",years[2])))
          all[,paste0("Year.",years[1])] <- NULL
          all[,paste0("Year.",years[2])] <- NULL
          all <- rbind(c("Ընդամենը","",colSums(all[is.na(all$ID),c(3:10)]),"-"),all)
          return (all)
        }

        if (input$Expimp=="Decomposition"){
          dec1 <- first_output[first_output$ID %in% c(2603,2402,7108,2208,7607,7202,7102,7402),]
          dec1$price_growth <- dec1$Export.2018 - dec1$Export.2017                                            
          dec1$mas_growth <- dec1$Export_in_tonnas.2018 - dec1$Export_in_tonnas.2017
          dec1$quant_factor <- (dec1$Export.2017*dec1$mas_growth*100)/(dec1$Export_in_tonnas.2017*dec1$price_growth)
          dec1$price_factor <- 100-dec1$quant_factor
          dec1$pf_growth <- dec1$price_factor*dec1$price_growth/100
          dec1$qf_growth <- dec1$price_growth - dec1$pf_growth
          dec_final <- dec1[order(-dec1$Export.2018),c("Name","ID","Export_in_tonnas.2018","Export.2018","mas_growth","price_growth","qf_growth","pf_growth")]
          dec_final <- rbind(dec_final, c("Ընդամենը","",colSums(dec_final[,-c(1,2)])))
          dec_final[,-c(1,2)] <- round(apply(dec_final[,-c(1,2)],2,as.numeric),5)
          
          return (dec_final)
        }
        if (input$Expimp == "Import Decomposition"){
          imp1 <- first_output[first_output$ID %in% c(2711,2710,8703,8517,8471,8704,
                                                      8429,8431,3004,3102,7102,7108,2401,7601,1001,4810) ,]
          
          imp1$Group <- c(1,1,2,2,2,2,2,2,3,3,4,4,4,4,4,4)
          titles_imp <- c("ՎԱՌԵԼԻՔՆԵՐ", "ՄԵՔԵՆԱՆԵՐ, ՍԱՐՔԱՎՈՐՈՒՄՆԵՐ ԵՎ ՏՐԱՆՍՊՈՐՏԱՅԻՆ ՄԻՋՈՑՆԵՐ",
                          "ՔԻՄԻԱԿԱՆ ԾԱԳՄԱՆ ԱՊՐԱՆՔՆԵՐ","ՀՈՒՄՔԱՅԻՆ ԱՊՐԱՆՔՆԵՐ")
          totals <- imp1 %>%
            group_by(Group) %>%
            summarise(Import_in_tonnas.2017 = sum(Import_in_tonnas.2017),
                      Import.2017 = sum(Import.2017),
                      Import_in_tonnas.2018 = sum(Import_in_tonnas.2018),
                      Import.2018 = sum(Import.2018))
          totals$ID <- NA
          totals$Name <- titles_imp
          imp1 <- imp1[,c("Name","ID","Import_in_tonnas.2018","Import.2018","Import_in_tonnas.2017","Import.2017","Group")]
          
          imp2 <- rbind(imp1,totals)
          imp2 <- imp2[order(imp2$Group,-imp2$Import_in_tonnas.2018),]
          imp2$mass_growth <- imp2$Import_in_tonnas.2018-imp2$Import_in_tonnas.2017
          imp2$price_growth <- imp2$Import.2018 - imp2$Import.2017
          imp2$Group<-NULL
          options(scipen=999)
          return (imp2)
        }

        join_and_output <- function(df1, df2, exp_imp){

          final <- inner_join(df1, df2, by = c("Name","ID"),  suffix = c(".2017", ".2018"))
          if(exp_imp == "Export"){
            final$Abs_Growth <- final$Export.2018 - final$Export.2017
            final$Pct_Growth <- final$Abs_Growth * 100 / final$Export.2017
          }
          else if(exp_imp == "Import"){
            final$Abs_Growth <- final$Import.2018 - final$Import.2017
            final$Pct_Growth <- final$Abs_Growth * 100 / final$Import.2017
          }


          groups <- final[is.na(final$ID),c(1,13)]
          groups_sort <- groups[order(groups$Abs_Growth),]
          groups_sort['ord']<-order(groups_sort$Abs_Growth)
          final <- left_join(final,groups_sort[,c(1,3)],by="Name")
          for (i in c(1:nrow(final))){
            if (is.na(final[i,'ID'])){
              counter <- final[i,'ord']
            }
            final[i,'ord'] = counter
          }
          counter <- 0
          for (i in c(1:nrow(final))){
            if (is.na(final[i,'ID'])){
              counter = counter + 1
              final[i,'ord2'] <- counter
              counter = counter + 1
            } 
            else {
              final[i,'ord2'] <- counter
            }
          }
          final <- final[order(-final$ord, final$ord2, -final[,paste0(exp_imp,".2018")]),]
          counter <- 1
          for (i in c(1:nrow(final))){
            if (is.na(final[i,'ID'])){
              final[i,'rank_price'] <- 0
              counter <- 1
            } 
            else {
              final[i,'rank_price'] <- counter
              counter = counter +1
            }
          }
          final <- final[order(-final$ord, final$ord2, -final$Abs_Growth),]
          counter <- 1
          for (i in c(1:nrow(final))){
            if (is.na(final[i,'ID'])){
              final[i,'rank_abs_growth'] <- 0
              counter <- 1
            } 
            else {
              final[i,'rank_abs_growth'] <- counter
              counter = counter +1
            }
          }
          final <- final[final$rank_abs_growth<=10 | final$rank_price<=10,]
          final <- final[,-c(15:18)]
          options(scipen=999)
          final[!(is.finite(final$Pct_Growth)) | (final$Pct_Growth >= 1000),"Pct_Growth"] <- '-'
          # vars <-assign_var(final,exp_imp)
          # print(vars)
          final <- final[,c("Name","ID",paste0(exp_imp,".2018"),paste0(exp_imp,".2017"),"Abs_Growth","Pct_Growth")]
          final <- final[final[,3]>0 & final[,4] >0,]
          final <- rbind(c("Ընդամենը","",colSums(final[is.na(final$ID),c(3:5)]),"-"),final)
          final[1,'Pct_Growth'] = as.numeric(final[1,'Abs_Growth']) /as.numeric(final[1,c(4)])
          return(final)
        }

        final <- join_and_output(final_year1,final_year2,input$Expimp)




        return (final)
        
      })
      output$txt <- renderText({
        "Instructions:"
      })
      observe({
        updateSelectInput(session,"year",choices=choice())
      })
      observeEvent(
        eventExpr = input[["submit_loc"]],
        handlerExpr = {
          
          output$table <- renderDataTable({
          my_data()
      }) })
      output$downloadData <- downloadHandler(
        filename = function() {
            ("untitled.xlsx")
        },
        content = function(file) {
          write_xlsx(my_data(), file)
        }
      )
      
    })
  ))

