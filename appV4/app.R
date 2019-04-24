#0- Prerequisites----
{if (!require("shiny"))         install.packages("shiny")
  if (!require("shinyjs"))      install.packages("shinyjs")
  if (!require("tidyverse"))    install.packages("tidyverse")
  if (!require("plotly"))       install.packages("plotly")
  if (!require("plm"))          install.packages("plm")
  if (!require("DT"))           install.packages("DT")
  if (!require("pivottabler"))  install.packages("pivottabler")
  if (!require("scales"))       install.packages("scales")
  if (!require("rlang"))        install.packages("rlang")
  if (!require("goeveg"))       install.packages("goeveg")
  }

library(shiny); library(shinythemes);library(shinyjs); library(tidyverse); library(plotly); library(plm)
library(DT); library(pivottabler); library(scales); library(rlang); library(goeveg)


#- I - USER INTERFACE ----

ui<-fluidPage(
  theme=shinytheme("flatly"),
   
  titlePanel("An application for plotting and Summary stats"),
      tabsetPanel(
        
      #0- The datset----
        tabPanel( title= "The DataSet",
                  
                  #Select_DataBase
                  fileInput("selected_DB",label="Choose a 'csv' extension Dataset" ),
                  
                  #Mutate expression
                  
                  textAreaInput("transform_dataset", "Write the command", 
                      width="100px", height="100px", resize=NULL,
                      placeholder ="dataset %>% "),
                  
                  submitButton(text="Submit"),
                  
                  #Exhibit the Dataset
                  tabsetPanel(
                    DTOutput("DaTaset")
                    )#tabsetpanel
                  )
                  
                  , 
        
      #1- Plot_inputs & outputs_Panel----
      
        tabPanel(title="Plotting Panel",
                 
          #The Options
          sidebarPanel(width=3,
              helpText("Please enter your plotting command & refer to dataset as dataset() or 
                       Or Use the interface"),
              
              #1-1- Using the Interface choices:  
                #Select_variables
                uiOutput("axe_x"), #( given the database==>reactive input==>comes from output and is Ui object: uiOutput)
                uiOutput("axe_y"),
                
                #Select_filters:
                
                textInput("quickfilter_expression", label="Enter a filtering expression"),
           
                #Slect_plottype
                  #which geom
                  selectInput(inputId = "selected_geom", label="Choose a plot type" ,
                              choices= c("geom_path","geom_point","geom_boxplot","geom_density", "geom_smooth") ), 
                
                  #which color
                  uiOutput("color"),
                
                  #which size
                  uiOutput("size"),
                  #which alpha
                  uiOutput("alpha"),
                
                #Rescaling_button
                checkboxInput("rescale",label="clic to rescale all Y varaibles" ), 
                
                #Button d'actualisation
                submitButton(text="Submit")),
          
               
          
          #2- The Plot Area:
          mainPanel(
            
             #Manual entry: when users choose to Write the plotting commands 
              
                textAreaInput("plot_expression", "Write the plotting command", 
                          width="100px", height="100px", resize=NULL,
                          placeholder = "ggplot(dataset())+geom_point(aes(x=Year, y=Year)) {or dataset()[input$DaTaset_rows_all,]}"),
                br(),br(),
                
              #Exhibiting The Plot 
              
                  plotlyOutput("reactiveplot"), 
              
               #Dowload The Plot
                  fluidRow(
                  selectInput("download_format","choose the format of download", choices=list("png","html"), multiple=FALSE),
                  downloadButton('PlotDownload')),
              
                #Accessing events
                  verbatimTextOutput("selected")
            )
            ),
                
      #2- Summary statistics_inputs & output_Panel----
        
        tabPanel(title="Summary Statistics Panel",
                 
        #The Options:
          sidebarPanel(
                #selet rows, columns and calculations:
                uiOutput("rows"),
                uiOutput("columns"),
                uiOutput("pivot_var"),
                uiOutput("calculations"),
                
                submitButton(text="Submit")
        ),
        #Exhibiting The Summary Statistics: as pivottable or as Datatable
        
        mainPanel(
        pivottablerOutput("pivottable_asdataframe")
        #DTOutput("pivottable_asdataframe")
        )
      )
      
      
    )
  )
  


    #server function mentions two arguments: output is a list-like object that stores instructions for building the R objects in your app.
    #input is a second list-like object. It stores the current values of all of the widgets in your app. 
    #These values will be saved under the names that you gave the widgets in your ui.here "var" and "range" as input$var and input$range
    #slider widget has two values (a min and a max)=> input$range will contain a vector of length two



#- II - SERVER ----

server<-function(input, output){
  options(shiny.maxRequestSize=30*1024^2) 
  
  #0-reactive dta on selected DB----
        dataset<-reactive({ 
                            #Reads only csv files (should check if (input$selected_DB$datapath is csv or other kind)
                            dataset<-read.csv(input$selected_DB$datapath)
                            
                            data_post_mutation<-dataset
                            if(input$transform_dataset!=""){expr<-input$transform_dataset
                              data_post_mutation<-eval(parse_expr(expr))}
                            
                            data_post_mutation %>% mutate(NONE="NONE")
        
                          }) 
                                  #reactive ++ to read it each time it changed (inout$selected_DB) and store it reactively
                                  #we read the path "file.csv"
        
        #0-1- Rendering intractive(not only reactive) dta through datatable of DT package----
        
        output$DaTaset<-renderDT({ 
          
          datatable(dataset(), class="display cell-border compact",
                   selection = list(target='row+column'),
                   filter='top',
                   editable = TRUE,
                   extensions = c('FixedColumns',"FixedHeader"),
                   options = list(scrollX = TRUE,
                                  #paging=FALSE,
                                  scrollY=TRUE,
                                  stateSave=TRUE,#so that any transformation by the user will be saved==>and called back lately using syntax:dataset()[input$dataset_rows_all,]
                                  fixedHeader=TRUE#this is NOT COMPATIBLE WITH SCROLLING since the scorllig splits the data//https://datatables.net/download/compatibility
                                  #fixedColumns = list(leftColumns =4 , rightColumns = 0)autoWidth = TRUE,columnDefs = list(list(width = '1%', targets = list(1:ncol(data_post_mutation))))
                                  )) %>% formatStyle(columns = names(dataset()),
                                                    fontFamily = "times",
                                                    fontSize = "14px",
                                                    #color = '#ed1c16',fontWeight = 'bold',paddingRight = "1em",borderRightWidth = "1px",borderRightStyle = "solid",borderRightColor = "white",borderBottomColor = "rgb(255, 255, 255)",#borderBottomStyle = "solid",#borderBottomWidth = "1px",#borderCollapse = "collapse",#verticalAlign = "middle",
                                                    textAlign = "center",
                                                    wordWrap = "break-word"
                                                    #backgroundColor = 'white'
                                                    )

          })
    
  #1-Plotting function---- 
    #1-1- Reading plot arguments ----
        #Reading reactive plot arguments (dataset and its variables(axes, color, regions, and countries))
          #pour alimenter l'interface UI using uiOutput("")
        
        
        output$axe_x<-renderUI({
          selectInput(inputId = "selected_var1", label=p("Choose a Variable for X axis"), 
                      choices= names(dataset()), selected="Year")})
        output$axe_y<-renderUI({
          selectInput(inputId = "selected_var2", label=p("Choose Variables for Y axis"), 
                      choices= names(dataset()), multiple=T)})
        
        output$color<-renderUI({
          selectInput(inputId = "selected_color", label=p("Choose a color"), names(dataset()) , selected=NULL)})
        output$size<-renderUI({
          selectInput(inputId = "selected_size", label=p("Choose a size"), names(dataset()) , selected=NULL)})
         
        
    #1-2- Making the Plot----
       
      #1-2-1- Creating a reactive Plot----
        Plot<-  reactive({
          
          #If user choosed to enter his own plot commands:read the expression and evaluate it: so easy
          
          if (input$plot_expression!=""){expr<-input$plot_expression
                                          
                                          eval(parse_expr(expr))}
          
          #If user choosed to use the interface
 
          else{ #do the entire work below
          
          #read the dataset ( the instance resulting from O-1 section: renderDT, thanks to option saveState=TRUE )
          dta<-dataset()[input$DaTaset_rows_all,]
      
        #A- Reading (transfoming) inputs to be used by dplyr functions(ggplot, filter,etc)

              x<-sym(input$selected_var1);color<-sym(input$selected_color);size<-sym(input$selected_size)# transforming character arguments to symbols! 
              geom<-eval(sym(input$selected_geom))
         
        #B- Rescaling: to [0,1]
          
            #Y variables: many Y, many dta, many geom: only for rescaling 
            L_y<- as.list(input$selected_var2)
            L_y<-lapply(L_y, FUN =sym )
            
            if (input$rescale==TRUE){for (i in 1: length(L_y)) {
              dta[, as.character(L_y[[i]])]<-rescale(dta[, as.character(L_y[[i]])], 
                                                     from=range(dta[, as.character(L_y[[i]])],na.rm=T),
                                                     to=c(0,1))}                                            
                                            }
                  
       #C- Building the Plot: A new way ( To identify linetype and shape as variables with the var they reprseent)
            #filtering:
            if(input$quickfilter_expression!=""){
              
              expr<-input$quickfilter_expression
              
              dta<-dta %>% filter(!!parse_expr(expr))}
            
            #transforming to long format
            c1<-c(input$selected_var1, input$selected_color,input$selected_size); n1=length(c1)+1
            c2<-c(input$selected_var1, input$selected_color,input$selected_size, input$selected_var2); n2=length(c2)
            dta1<-dta[,c2]
             
            dta_long<-gather(dta1,key=Indicator, value=Value, n1:n2) %>% filter(!is.na(Value))
            
            #the plot
            gg<-ggplot(dta_long, mapping=aes(x=!!x, y=Value,color=!!color))+geom(aes(linetype=Indicator, shape=Indicator))+theme_bw()
            labs(y=paste(L_y, collapse=""))
            gg
          }#fin else
})

      #Creating the Plot Output (seperated form rendering) : To download it easily----
      output$reactiveplot<-renderPlotly ({ggplotly(Plot()) %>% layout(dragmode="select") })
      
      #Accesing events
        output$selected<-renderPrint({
          
          d<-event_data("plotly_selected")
          if (is.null(d)){"selected events appear here(unselected to clear)"} else d
          
        })
      
    #1-3- Downloading the plot----
        
        output$PlotDownload<-downloadHandler(
          
          filename =function(){
            
          "test"  
            
          },
 
          content= function(file) {
          
           #as.html
            if(input$download_format=="html"){
          name<-paste0(
            paste0(paste0(Plot()$labels$y, collapse=" & "),Plot()$labels$x),
            ".html")
          z<-htmlwidgets::saveWidget(as_widget(ggplotly(Plot())),name,
                                      selfcontained = FALSE)
          z}  
          
          #as.png
            if (input$download_format=="png"){
          png(file);print(Plot());dev.off()}
          
         
            
          })
          
        
        
        
  #2- Summary stats function----
      
    #2-1- Reading-reactively- Pivot arguments
      
        output$rows<-renderUI({selectInput("selected_rows", label="choose the rows to tabulate", names( dataset() ), multiple = T )})
        output$columns<-renderUI({selectInput("selected_columns", label="choose the columns to tabulate", names( dataset() ), multiple = T )})
        output$pivot_var<-renderUI({selectInput("selected_pivot_var", label="choose the vars to be considered in calculations", names( dataset() ), multiple = T  )})
        
        output$calculations<-renderUI({selectizeInput("selected_calculation", label="choose summary stats", 
                            choices = c("n", "n_distinct", "mean", "sd", "min", "max","range","median", "cv"), 
                            multiple = T )})#no need for it being interactive..
    
    #2-2- Building reactive PivoTable
      #Creer  PivotTable reactive:
      
      PivoTable<-reactive({
        
        #Reading the Dataset (the instance as modified by user)
        
        dta<-dta<-dataset()[input$DaTaset_rows_all,]  
        nrows=length(input$selected_rows); ncols=length(input$selected_columns); 
        ncalculations=length(input$selected_calculation); nvars<-length(input$selected_pivot_var)
        
                          #calculation<-paste0(input$selected_calculation, "(" , input$selected_pivot_var , ", na.rm=TRUE)" )
                          #if (input$selected_calculation=="n") {calculation= paste0(input$selected_calculation, "()")}
                          #pt<-qpvt(dataFrame = dta, columns=input$selected_columns, rows=input$selected_rows,
                          #           calculations =calculation)
        
        #construisons un vecteur cahracter des calculs (multiplions les calculs avel les vars)
        c<-matrix(nrow=ncalculations,ncol=nvars)
        for (i in 1:ncalculations ){for (j in 1: nvars) {
            c[i, j]<-paste0(input$selected_calculation[[i]], "(" , input$selected_pivot_var[[j]] , ", na.rm=TRUE)" )
            if (input$selected_calculation[[i]]=="n") {c[i,j]= "n()"}}}
        
        calculation<-as.vector(c)
        
        #Constrisons la PivotTable: usual
        pt <- PivotTable$new()
        
        pt$addData( dta)
        
        if (ncols!=0){for (i in 1: ncols){
        pt$addColumnDataGroups(input$selected_columns[[i]])
        }}
        
        if (nrows!=0){##very essential condition hhh
        for (i in 1: nrows){
        pt$addRowDataGroups(input$selected_rows[[i]])
        }}
        
        for (i in 1: length(calculation)){
        pt$defineCalculation(calculationName=calculation[i], summariseExpression=calculation[i])
        }
                        #Defining Theme
                        simpleBlueTheme <- list(
                        fontName="Verdana, Arial",headerBackgroundColor = "rgb(68, 114, 196)",headerColor = "rgb(255, 255, 255)",
                        cellBackgroundColor = "rgb(255, 255, 255)",cellColor = "rgb(0, 0, 0)",totalBackgroundColor = "rgb(186, 202, 233)",
                        totalColor = "rgb(0, 0, 0)",borderColor = "rgb(48, 84, 150)"
                         )
                        pt$theme <- simpleBlueTheme
      pt$evaluatePivot()
      
      pt<-pivottabler(pt)
      
      #pivot<-pt$asDataFrame()
        
        })
        
    #Rendering as pivottable
        output$pivottable_asdataframe<-renderPivottabler({PivoTable()})
        
      #Rendering PivoTable as dataframe (DT)...see below  
        
 
        
}



#- III- APPLICATION ----
A<-shinyApp(ui=ui, server=server)

A


#Appendix----

#runApp(A, display.mode = "showcase") this or run in command line: runApp("app-1-lesson4.R", display.mode = "showcase")

#Rendering PivoTable as dataframe (DT)
        
        #output$pivottable_asdataframe<-renderDT({
         #datatable(PivoTable())
          #       filter='top', extensions = c("FixedHeader"),
           #        options = list(scrollX = TRUE,
            #                      #paging=FALSE,
             #                     scrollY=TRUE,
              #                    fixedHeader=TRUE#this is NOT COMPATIBLE WITH SCROLLING since the scorllig splits the data//https://datatables.net/download/compatibility
               #                   #fixedColumns = list(leftColumns =4 , rightColumns = 0)autoWidth = TRUE,columnDefs = list(list(width = '1%', targets = list(1:ncol(data_post_mutation))))
                #                  ))
        
     
        #})
