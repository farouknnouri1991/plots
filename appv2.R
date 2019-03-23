library(shiny); library(tidyverse); library(plotly)
#dta<-read.csv("dta.csv") %>% select(-1); L<-as.list(names(dta)); names(L)<-names(dta)
#dta<-read.csv("Agriculture.csv") 
#dta<-dta; L<-as.list(names(dta)); names(L)<-names(dta)
#in applying to GGDC: no need for large format: 
#You can filter on indicator names and process(mutate, group by, etc) data within server.

plottypes<-list(geom_path=geom_path,geom_point=geom_point)

#Build UI----
ui<-fluidPage(
 
   
  titlePanel("Lesson 4"),
  sidebarLayout(position="left",
    sidebarPanel(
      
      helpText("You will select a variable that will be dispalyed in the main panel"),
      
      #Select_DataBase
      
      selectInput(inputId = "selected_DB", label=p("Choose a Variable for Y axis"), choices= list("GGDC_PPP_large", "Agriculture")),
      
      #Select_variables

      uiOutput("axe_x"), #( given the database==>reactive input==>comes from output and is Ui object: uiOutput)
      uiOutput("axe_y"),
      
 
      
      #plottype
      
        #which geom
      
      selectInput(inputId = "selected_geom", label=p("Choose a plot type", style="font-family:'times'") ,
                  choices= names(list(geom_path=geom_path,geom_point=geom_point)) ), 
      
        #which color
      
      uiOutput("color"),
      
        #which size
      
      #filters:
      
        #Regions
      
      uiOutput(("regions")),
      
        #Countries
      
      uiOutput(("countries")),
      
      
      #Button d'actualisation
      
      submitButton(text="Submit")
      
    
    ),
    
    
    mainPanel(
      #textOutput("reactiveVars"),
      
      plotlyOutput("reactiveplot")
      #dataTableOutput("dta")
      #dataTableOutput("axes")
    )
  )
  
)

    #server function mentions two arguments: output is a list-like object that stores instructions for building the R objects in your app.
    #input is a second list-like object. It stores the current values of all of the widgets in your app. 
    #These values will be saved under the names that you gave the widgets in your ui.here "var" and "range" as input$var and input$range
    #slider widget has two values (a min and a max)=> input$range will contain a vector of length two

#Build server----

server<-function(input, output){
  
    #output$reactiveVars<-renderText(paste("Vous avez selectionne les variable: ", input$selected_var1, input$selected_var2))
    
    
    #Reading dataset (to propose the axis in UI, otherwise I can make it in reactiveplot)----
    
        #reading data interactively and ist variables(axes, color, regions, and countries)
        #Pour alimenter l'interface UI using uiOutput("")
  
            dta<-reactive({ read.csv(paste0(input$selected_DB,".csv")) })#reactive ++ to read it each time it changed (inout$selected_DB) and store it reactively
            
            output$dta<-renderDataTable({ 
              
              dta()
             
              })
            
            output$axe_x<-renderUI({
             
              selectInput(inputId = "selected_var1", label=p("Choose a Variable for X axis"), 
                          choices= names(dta()), selected="Year")
            })
        
            output$axe_y<-renderUI({
              selectInput(inputId = "selected_var2", label=p("Choose Variables for Y axis"), 
                          choices= names(dta()), multiple=T)
            })
            
            output$color<-renderUI({
              selectInput(inputId = "selected_color", label=p("Choose a color"), names(dta())[2:4] , selected=NULL)
            })
            
            output$regions<-renderUI({
               selectInput(inputId = "selected_region", label=p("Choose a region"), choices= as.list(unique(dta()[3])), multiple = T )
            })
            
            output$countries<-renderUI({
              selectInput(inputId = "selected_area", label=p("Choose a coutry"), choices= as.list(unique(dta()[2])), multiple=T )
            })
        
    
    
     #Making the Plot----
      output$reactiveplot<-renderPlotly ({
      
      #reading inputs
      
        dta<-dta()
        color<-sym(input$selected_color)
        #----
        
        x<-sym(input$selected_var1)#au lieu de switch c est mieux

        L_dta<- as.list(input$selected_var2)->L_y
        L_y<-lapply(L_y, FUN =sym )
        
        
       
        
        #-----
        geom<-switch(input$selected_geom, 
                    "geom_path"=plottypes[["geom_path"]],
                    "geom_point"=plottypes[["geom_point"]])
        
        
       
        
        #if (length(input$selected_var2)<2){z<-y} else{z<-sym(input$selected_var2[2])}
        
        
      
      #filtering regions and countries
        
        if (!is.null(input$selected_area)|!is.null(input$selected_region)){
          dta<-dta %>% filter(!!sym(names(dta)[2])%in%input$selected_area|
                               !!sym(names(dta)[3])%in%input$selected_region)
          }
       
        #dta1<-dta %>% filter(!is.na(!!y))
       
        #dta2<-dta %>% filter(!is.na(!!z))
        
       
        
        #creating the plot
        
        for (i in 1: length(L_dta)){L_dta[[i]]<-filter (dta,!is.na(!!sym(input$selected_var2[i])))}#after filtering regions ans countries
        
        L_geom<-L_dta
        for (i in 1: length(L_geom)){L_geom[[i]]<-geom(data=L_dta[[i]],mapping=aes(x=!!x, y=!!L_y[[i]],color=!!color), size=1/4)}
        
        gg<-ggplot()
        for (i in 1: length(L_geom)){gg<-gg+L_geom[[i]]}
        
        s<-ggplotly(gg)
        s
      })
    
    
    }



#Buid App----
A<-shinyApp(ui=ui, server=server)
#runApp(A, display.mode = "showcase") this or run in command line: runApp("app-1-lesson4.R", display.mode = "showcase")
#?runApp
# A
#A<-function(dta){A}
#A(GGDC_large)
