library(shiny)
library(tidyverse)
library(tabulizer)
library(reshape2)
library(zip)

ui <- 
  fluidPage(
    titlePanel("Please Upload HPLC Data"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Choose PDF File to Upload",
                  accept = c(".pdf")),
        downloadButton("Download_name", "Download name-Contrast-table"),
        downloadButton("Download_csv", "Download Data CSV"),
        width = 8),
        mainPanel(
          tableOutput("contents"),
        )
      )
    )

##

server <- function(input, output){
  
  state <- reactiveValues()
    
  
  observe({
    req(input$file)
    inFile <- input$file
    
    state$inFile <- inFile
    
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".pdf", sep=""))
    
    
    out <- paste(inFile$datapath, ".pdf", sep = "")
    TAH2211 <- extract_tables(out)
    #final is variable
    final <- do.call(rbind, TAH2211[1:length(TAH2211)])
    final <- as.data.frame(final)
    colnames(final) <-
      c("No",
        "SampleName",
        "Vial",
        "Inj",
        "Name",
        "RT",
        "RRT",
        "Area",
        "Tailing",
        "Plate")
    del_data <- which(final$SampleName == "SampleName" | final$SampleName == "")
    final <- final[-del_data, ]
    final <- subset(final, select = -c(RRT, Tailing, Plate))
    final[,c(8,9,10)] <- NULL

    state$final <- final

  })
  output$Download_name <- downloadHandler(
    filename = function(){
      paste0("name-Contrast-table.csv")
    },
    content = function(file){
      samplename <- as.character(state$final$SampleName) %>% unique()
      samplename_1 <- as.data.frame(samplename) %>% 
        mutate(No = seq_along(samplename))
      write.csv(samplename_1, file, row.names = F)
      state$samplename <- samplename
    }
  )
  
  output$Download_csv <- downloadHandler(
    filename = function(){
      paste0(state$inFile$name, ".zip")
    },
    content = function(file){
      
      #go to a temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;
    
      output <- list()
      serial <- 1
      for (i in state$samplename) {
        output[[i]]  <- state$final %>%
          gather(key = "name", value = "value", RT:Area) %>%
          filter(SampleName == i) %>%
          unite("New_Name",
                c("No", "Name"),
                sep = "_",
                remove = T) %>%
          pivot_wider(names_from = New_Name, values_from = value) %>% 
          #pivot_wider = spread , is new tidyr package
          #spread(key = "New_Name", value = "value") %>%
          arrange(desc(name))
        filename <- paste0("HPLC_", serial, ".csv")
        write.csv(output[[i]], filename, row.names = F)
        serial <- serial + 1
        files <- c(filename, files)
      }
      #create the zip file
      zip(file,files=files)
    },
    contentType = "application/zip"
  )
  
  output$contents <- renderTable({
    print(state$final)
  })
}
   
shinyApp(ui = ui, server = server)  

runApp(shinyApp(ui = ui, server = server), launch.browser = T)
