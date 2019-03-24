library(shiny); library(tidyverse); library(plotly); library(DT); library(pivottabler); library(scales)
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
  sidebarLayout(position="left",
    sidebarPanel(
      tabsetPanel(
      #0- The datset
        tabPanel( title= "The DataSet",
                  #Select_DataBase
                  fileInput("selected_DB",label="Choose a Dataset" )), 
        
      #1- Plot_options_Panel----
        tabPanel(title="Plotting Option",
      
                 helpText("You will select variables that will be dispalyed in the main panel"),
      
                #Select_variables
                uiOutput("axe_x"), #( given the database==>reactive input==>comes from output and is Ui object: uiOutput)
                uiOutput("axe_y"),
           
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
                
                #select_filters:
                
                  #which filter
                uiOutput("filter_var"), 
                uiOutput("filter_choices"),
                
                  #Regions
                uiOutput("regions"),
                
                  #Countries
                uiOutput("countries"),
                
                #Button d'actualisation
                submitButton(text="Submit"),
                
                #Rescaling button
                checkboxInput("rescale",label="clic to rescale all Y varaibles" )
                
                 ),
                
      #2-Summary stats_options panels----
      
        tabPanel(title="Summary statistics options",
                #selet rows, columns and calculations:
                uiOutput("rows"),
                uiOutput("columns"),
                uiOutput("pivot_var"),
                uiOutput("calculations"),
                
                submitButton(text="Submit")
        )
      )
      
      ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel(title= "The Plot", plotlyOutput("reactiveplot")),
        tabPanel(title= "The DataSet", DTOutput("dataset")),
        tabPanel(title= "Summary Stats",  pivottablerOutput("pivottable"))
        )
      
      
      )
      #dataTableOutput("axes")
    )
  )
  


    #server function mentions two arguments: output is a list-like object that stores instructions for building the R objects in your app.
    #input is a second list-like object. It stores the current values of all of the widgets in your app. 
    #These values will be saved under the names that you gave the widgets in your ui.here "var" and "range" as input$var and input$range
    #slider widget has two values (a min and a max)=> input$range will contain a vector of length two

#Build server----

server<-function(input, output){
  
  #0-reactive dta on selected DB
        dataset<-reactive({ dataset<-read.csv(input$selected_DB$datapath) }) 
                                  #reactive ++ to read it each time it changed (inout$selected_DB) and store it reactively
                                  #we read the path "file.csv"
        
        #rendering intractive(not only reactive) dta through datatable of DT package
        output$dataset<-renderDT({ datatable(dataset(), class="display cell-border",
                                         selection = list(target='row+column'),
                                         filter='top',
                                         editable = TRUE,
                                         options = list(scrollX = TRUE, 
                                                        scrollY=TRUE,
                                                        stateSave=TRUE) )})
    
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
        
        output$filter_var<-renderUI({
          selectInput(inputId = "selected_filter", label="Chose the variable to filter on", names(dataset()))})
        
        output$color<-renderUI({
          selectInput(inputId = "selected_color", label=p("Choose a color"), names(dataset()) , selected=NULL)})
        
        output$size<-renderUI({
          selectInput(inputId = "selected_size", label=p("Choose a size"), names(dataset()) , selected=NULL)})
        
        #reactive filter_choices on selected filter var:
        Filter<- reactive({Filter<-as.list(unique(dataset()[input$selected_filter]))})# can't use unique(dataset()$input$selected_filter, because input$selected_filter is a character
        
        output$filter_choices<-renderUI({
         selectInput("selected_filter_choices", label="Chose the filter values",choices=Filter(), multiple = T)})
        
        #region and country filters:
        output$regions<-renderUI({
           selectInput(inputId = "selected_region", label=p("Choose a region"), choices= as.list(unique(dataset()[3])), multiple = T )})
        
        output$countries<-renderUI({
          selectInput(inputId = "selected_area", label=p("Choose a coutry"), choices= as.list(unique(dataset()[2])), multiple=T )})
    
    
    
     #1-2-Making the Plot----
      output$reactiveplot<-renderPlotly ({
      
        #trasnforming inputs to be used by dplyr functions(ggplot, filter,etc)
      
        #dta<-dataset()
        #dta<-input$dataset_state
        dta<-dataTableProxy("dataset")
        dta<-reloadData(dta)
        dta<-dataset()[input$dataset_rows_all,]
        
        color<-sym(input$selected_color)
        size<-sym(input$selected_size)
        filter_var<-sym(input$selected_filter)
        x<-sym(input$selected_var1)#au lieu de switch c est mieux
        geom<-switch(input$selected_geom, 
                    "geom_path"=plottypes[["geom_path"]],
                    "geom_point"=plottypes[["geom_point"]])
        
        #Y variables: many Y, many dta, many geom
        
        L_dta<- as.list(input$selected_var2)->L_y
        L_y<-lapply(L_y, FUN =sym )
        
        #rescaling
        if (input$rescale==TRUE){for (i in 1: length(L_y)) {
          dta[, as.character(L_y[[i]])]<-rescale(dta[, as.character(L_y[[i]])], 
                                                 from=range(dta[, as.character(L_y[[i]])],na.rm=T),
                                                 to=c(0,1))
            }
          }
        
        #filtering regions and countries(names(dta)[c(2,4)]
        if (!is.null(input$selected_area)|!is.null(input$selected_region)){
          dta<-dta %>% filter(!!sym(names(dta)[2])%in%input$selected_area|
                               !!sym(names(dta)[3])%in%input$selected_region)}
        
        
        #filtering on selected_filter_var in selected_filter_choices
        if(!is.null(input$selected_filter_choices)) { dta<-dta %>% filter(!!filter_var%in%input$selected_filter_choices) }
        
        #creating the plot
          #filtering NAs
          for (i in 1: length(L_dta)){L_dta[[i]]<-filter (dta,!is.na(!!sym(input$selected_var2[i])))}#after filtering regions ans countries
          
          #creating geoms
          L_geom<-L_dta
          for (i in 1: length(L_geom)){
            
            L_geom[[i]]<-geom(data=L_dta[[i]],
                              mapping=aes(x=!!x, y=!!L_y[[i]],color=!!color), size=1/4, 
                              linetype=i, shape=i, alpha=0.4 )
            
            
            if(input$selected_size!="X" ){
            L_geom[[i]]<-geom(data=L_dta[[i]],
                              mapping=aes(x=!!x, y=!!L_y[[i]],color=!!color, size=!!size), 
                              linetype=i, shape=i,  alpha=0.4 )}
            
            }
        
        gg<-ggplot()
        for (i in 1: length(L_geom)){gg<-gg+L_geom[[i]]}
        
        s<-ggplotly(gg+theme_bw())
        s
      })
        

        
        
        
        
        
        
        
        
        
  #2- Summary stats function
    #Reading-reactively- Pivot arguments
      
        output$rows<-renderUI({selectInput("selected_rows", label="choose the rows to tabulate", names( dataset() ), multiple = T )})
        output$columns<-renderUI({selectInput("selected_columns", label="choose the rows to tabulate", names( dataset() ), multiple = T )})
        output$pivot_var<-renderUI({selectInput("selected_pivot_var", label="choose the vars to be considered in calculations", names( dataset() ) )})
        
        output$calculations<-renderUI({selectInput("selected_calculation", label="choose summary stats", 
                            choices = c("n", "n_distinct", "mean", "sd", "min", "max") )})#no need for it being interactive..
    
    #Building Pivot table
        output$pivottable<-renderPivottabler({
          
        calculation<-paste0(input$selected_calculation, "(" , input$selected_pivot_var , ", na.rm=TRUE)" )
        if (input$selected_calculation=="n") {calculation= paste0(input$selected_calculation, "(,na.rm=TRUE)")}
        pivot<-qhpvt(dataFrame = dataset(), columns=input$selected_columns, rows=input$selected_rows,
                     calculations =calculation)
        
        })
        
        
        
        
        
}



#Buid App----
A<-shinyApp(ui=ui, server=server)
#runApp(A, display.mode = "showcase") this or run in command line: runApp("app-1-lesson4.R", display.mode = "showcase")
#?runApp
# A
#A<-function(dta){A}
#A(GGDC_large)
