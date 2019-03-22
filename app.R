library(shiny); library(tidyverse); library(plotly)
 dta<-read.csv("dta.csv") %>% select(-1); L<-as.list(names(dta)); names(L)<-names(dta)
 
 plottypes<-list(geom_path=geom_path,geom_point=geom_point)

 ggplot(dta, aes(x=Year, y=Emploi_nb_FAO))+plottypes[["geom_point"]]()
 
#A function to graph
  graph<-function(data,x,y,region, color, geom){


      if (class(substitute(x))=="character") { x<-sym(x)} ; if (class(substitute(x))=="call") { x<-sym(eval(x))} 
      x<-enquo(x)  
        
      if (class(substitute(y))=="character") {y<-sym(y)}  ; if (class(substitute(y))=="call") { y<-sym(eval(y))} 
      y<-enquo(y)
      
      if (class(substitute(color))=="character"){color<-sym(color)};  if (class(substitute(color))=="call"){ color<-sym(eval(color))}  
      color<-enquo(color)
      
      if (!missing(region)){data<-data %>% dplyr::filter(Region==region, !is.na(!!y))}
      
      gg<-ggplot(data, mapping=(aes(x=!!x, y=!!y)))+
         geom((aes(color=!!color)))
              
      ggplotly(gg) 
      
      }


#Build UI----
ui<-fluidPage(
 
  
  names(L)<-names(dta),
  titlePanel("Lesson 4"),
  sidebarLayout(position="left",
    sidebarPanel(
      
      helpText("You will select a variable that will be dispalyed in the main panel"),
      
      #variables:
      selectInput(inputId = "selected_var1", label=p("Choose a Variable for X axis", style="font-family:'times'"), choices= L, selected = "Year" ),
      selectInput(inputId = "selected_var2", label=p("Choose a Variable for Y axis", style="font-family:'times'"), choices= L, selected="Year" ),
      
      #filters: 
        #Regions
      checkboxGroupInput(inputId = "selected_region", label=p("Choose a Variable for X axis"), choices= as.list(unique(dta$Region)) ),

      #plottype
      #which geom
      selectInput(inputId = "selected_geom", label=p("Choose a plot type", style="font-family:'times'") ,
                  choices= names(plottypes) )
      
     
      
    
    ),
    
    
    mainPanel(
      #textOutput("reactiveVars"),
      #textOutput("reactiveRange"), 
      plotlyOutput("reactiveplot")
    )
  )
  
)

#server function mentions two arguments: output is a list-like object that stores instructions for building the R objects in your app.
#input is a second list-like object. It stores the current values of all of the widgets in your app. 
#These values will be saved under the names that you gave the widgets in your ui.here "var" and "range" as input$var and input$range
#slider widget has two values (a min and a max)=> input$range will contain a vector of length two

#Build server----
server<-function(input, output){
  
    output$reactiveVars<-renderText(paste("Vous avez selectionne les variable: ", input$selected_var1, input$selected_var2))
    output$reactiveRange<-renderText(paste("la variable apparaitrea entre", 
                                         input$selected_range[1], "and",  input$selected_range[2]))
    
    
    output$reactiveplot<-renderPlotly ({
      
      
      
      x<-sym(input$selected_var1)#au lieu de switch c est mieux
      
      y<-sym(input$selected_var2)
      
      region<-input$selected_region
      
      dta<-dta %>% filter(!is.na(!!y), Region %in% region) 
      
      
      
      geom<-switch(input$selected_geom, 
                  "geom_path"=plottypes[["geom_path"]],
                  "geom_point"=plottypes[["geom_point"]])
     
      gg<-ggplot(data=dta)+
      geom(mapping=aes(x=!!x, y=!!y, color=Area_code), size=1/4)
      
      s<-ggplotly(gg)
      s
      })
    
    
    }



#Buid App----
A<-shinyApp(ui=ui, server=server)
#runApp(A, display.mode = "showcase") this or run in command line: runApp("app-1-lesson4.R", display.mode = "showcase")
#?runApp
