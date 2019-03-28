library(shiny); library(tidyverse); library(plotly); library(DT); library(pivottabler); library(scales); library(rlang)
#dta<-read.csv("dta.csv") %>% select(-1); L<-as.list(names(dta)); names(L)<-names(dta)
#dta<-read.csv("Agriculture.csv") 
#dta<-dta; L<-as.list(names(dta)); names(L)<-names(dta)
#in applying to GGDC: no need for large format: 
#You can filter on indicator names and process(mutate, group by, etc) data within server.

plottypes<-list(geom_path=geom_path,geom_point=geom_point)

#Build UI----
##################################  FOR MORE FLEXIBLE UI allow for expressions like for 
##################################  Mutating data, or ...
ui<-fluidPage(
 
   
  titlePanel("An application for plotting and Summary stats"),
  
    
      tabsetPanel(
      #0- The datset
        tabPanel( title= "The DataSet",
                  
                  #Select_DataBase
                  
                  
                  fileInput("selected_DB",label="Choose a Dataset" ),
                  
                  #0 Mutation
                  
                  textInput("selected_mutate_expr", "Create a new variable"),
                  
                  submitButton(text="Submit"),
                    DTOutput("dataset")
                  )
                  
                  , 
        
      #1- Plot_options_Panel----
      
        tabPanel(title="Plotting Option",
          #The Options
          sidebarPanel(width=3,
                 helpText("You will select variables that will be dispalyed in the main panel"),
      
                #Select_variables
                uiOutput("axe_x"), #( given the database==>reactive input==>comes from output and is Ui object: uiOutput)
                uiOutput("axe_y"),
                
                #select_filters:
                
                textInput("quickfilter_expression", label="Enter a filtering expression"),
           
                #Slect_plottype
                  #which geom
                selectInput(inputId = "selected_geom", label="Choose a plot type" ,
                            choices= names(list(geom_path=geom_path,geom_point=geom_point)) ), 
                
                  #which color
                uiOutput("color"),
                
                  #which size
                uiOutput("size"),
                  #which alpha
                uiOutput("alpha"),
                
                #Button d'actualisation
                submitButton(text="Submit"),
                
                #Rescaling button
                checkboxInput("rescale",label="clic to rescale all Y varaibles" )
                
                 ),
          #The Plot:
          mainPanel(
            plotlyOutput("reactiveplot")
          )
          ),
                
      #2-Summary stats_options panels----
        
        tabPanel(title="Summary statistics options",
        #The Options:
          sidebarPanel(
                #selet rows, columns and calculations:
                uiOutput("rows"),
                uiOutput("columns"),
                uiOutput("pivot_var"),
                uiOutput("calculations"),
                
                submitButton(text="Submit")
        ),
        #The sumstats:
        mainPanel(
        pivottablerOutput("pivottable")
        )
      )
      
      
    )
  )
  


    #server function mentions two arguments: output is a list-like object that stores instructions for building the R objects in your app.
    #input is a second list-like object. It stores the current values of all of the widgets in your app. 
    #These values will be saved under the names that you gave the widgets in your ui.here "var" and "range" as input$var and input$range
    #slider widget has two values (a min and a max)=> input$range will contain a vector of length two

#Build server----

server<-function(input, output){
  
  #0-reactive dta on selected DB----
        dataset<-reactive({ dataset<-read.csv(input$selected_DB$datapath) }) 
                                  #reactive ++ to read it each time it changed (inout$selected_DB) and store it reactively
                                  #we read the path "file.csv"
        
        #rendering intractive(not only reactive) dta through datatable of DT package
        output$dataset<-renderDT({ 
          
          data_post_mutation<-dataset()
          if(input$selected_mutate_expr!=""){
            
            expr<-input$selected_mutate_expr
            
            data_post_mutation<-dataset() %>% mutate(!!parse_expr(expr))}
            
          
          datatable(data_post_mutation, class="display",
                   selection = list(target='row+column'),
                   filter='top',
                   editable = TRUE,
                   extensions = c('FixedColumns',"FixedHeader"),
                   options = list(scrollX = TRUE,
                                  #paging=FALSE,
                                  scrollY=TRUE,
                                  stateSave=TRUE,
                                  fixedHeader=TRUE
                                  #fixedColumns = list(leftColumns =4 , rightColumns = 0)autoWidth = TRUE,columnDefs = list(list(width = '1%', targets = list(1:ncol(data_post_mutation))))
                                  )) %>% formatStyle(columns = names(data_post_mutation),
                                                    fontFamily = "times",
                                                    fontSize = "14px",
                                                    #color = '#ed1c16',fontWeight = 'bold',paddingRight = "1em",borderRightWidth = "1px",borderRightStyle = "solid",borderRightColor = "white",borderBottomColor = "rgb(255, 255, 255)",#borderBottomStyle = "solid",#borderBottomWidth = "1px",#borderCollapse = "collapse",#verticalAlign = "middle",
                                                    textAlign = "center",
                                                    wordWrap = "break-word"
                                                    #backgroundColor = 'white'
                                                    )

          })
    
  #1-Plotting function---- 
    #1-1-Reading plot arguments ----
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
        
        
       
        
     #1-2-Making the Plot----
      output$reactiveplot<-renderPlotly ({
      
        #trasnforming inputs to be used by dplyr functions(ggplot, filter,etc)
      
        #dta<-dataset()
        #dta<-input$dataset_state #dta<-dataTableProxy("dataset") #dta<-reloadData(dta)
        
        
        dta<-dataset()[input$dataset_rows_all,]
        x<-sym(input$selected_var1)#au lieu de switch c est mieux
        color<-sym(input$selected_color)
        size<-sym(input$selected_size)
        
        
        geom<-switch(input$selected_geom, 
                    "geom_path"=plottypes[["geom_path"]],
                    "geom_point"=plottypes[["geom_point"]])
        
        #Y variables: many Y, many dta, many geom
        
        L_y<- as.list(input$selected_var2)->L_dta #making a list of Ys but just initializing L_dta
        L_y<-lapply(L_y, FUN =sym )
        
        #rescaling
        if (input$rescale==TRUE){for (i in 1: length(L_y)) {
          dta[, as.character(L_y[[i]])]<-rescale(dta[, as.character(L_y[[i]])], 
                                                 from=range(dta[, as.character(L_y[[i]])],na.rm=T),
                                                 to=c(0,1))
            }
        }
        
     #New Way----To identify linetype and shape as variables with the var they reprseent
           
        
          #filtering:
          if(input$quickfilter_expression!=""){
            
            expr<-input$quickfilter_expression
            
            dta<-dta %>% filter(!!parse_expr(expr))}
          
          #transforming to long format
          c1<-c(input$selected_var1, input$selected_color,input$selected_size); n1=length(c1)+1
          c2<-c(input$selected_var1, input$selected_color,input$selected_size, input$selected_var2); n2=length(c2)
           
        
           dta1<-dta[,c2]
           
           dta_long<-gather(dta1,key=Indicator, value=Value, n1:n2) %>% filter(!is.na(Value))
           
           gg<-ggplot(dta_long, mapping=aes(x=!!x, y=Value,color=!!color))+geom(aes(linetype=Indicator, shape=Indicator))+
           labs(title= "this is the plot", y=as.character(input$seleceted_var2))
           s<-ggplotly(gg)
           s
        
        
        
      })
        

        
  #2- Summary stats function----
    #Reading-reactively- Pivot arguments
      
        output$rows<-renderUI({selectInput("selected_rows", label="choose the rows to tabulate", names( dataset() ), multiple = T )})
        output$columns<-renderUI({selectInput("selected_columns", label="choose the rows to tabulate", names( dataset() ), multiple = T )})
        output$pivot_var<-renderUI({selectInput("selected_pivot_var", label="choose the vars to be considered in calculations", names( dataset() ) )})
        
        output$calculations<-renderUI({selectizeInput("selected_calculation", label="choose summary stats", 
                            choices = c("n", "n_distinct", "mean", "sd", "min", "max"), multiple = T )})#no need for it being interactive..
    
    #Building Pivot table
        output$pivottable<-renderPivottabler({
          
        calculation<-paste0(input$selected_calculation, "(" , input$selected_pivot_var , ", na.rm=TRUE)" )
        if (input$selected_calculation=="n") {calculation= paste0(input$selected_calculation, "()")}
        pivot<-qhpvt(dataFrame = dataset(), columns=input$selected_columns, rows=input$selected_rows,
                     calculations =calculation)
        
        })
        
        
        
        
        
}



#Buid App----
A<-shinyApp(ui=ui, server=server)
#runApp(A, display.mode = "showcase") this or run in command line: runApp("app-1-lesson4.R", display.mode = "showcase")
#?runApp
#A
#A<-function(dta){A}
#A(GGDC_large)


 