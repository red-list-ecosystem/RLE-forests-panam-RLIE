#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(tidyr)
require(dplyr)
require(DT)
require(ggplot2)
require(plotly)

if (!file.exists("20181123_MacrogroupsCountry.rda"))
  download.file("https://figshare.com/ndownloader/files/13874333","20181123_MacrogroupsCountry.rda")

load("20181123_MacrogroupsCountry.rda")

rle.categories <- c("CO","CR","EN","VU","NT","LC","DD","NE")
rle.weights <- c(5:0,NA,NA)
rle.colors <- c("black","red","orange","yellow","palegreen","green","grey","white")
names(rle.weights) <- rle.categories
names(rle.colors) <- rle.categories


threat.score <- function(x,w=c("CO"=5,"CR"=4,"EN"=3,"VU"=2,"NT"=1,"LC"=0,"DD"=NA,"NE"=NA)) {
  if (all(x %in% names(w))) {
    y <- table(factor(x,levels=names(w)))
    z <- weighted.mean(w,y,na.rm=T)
    return(z)
  } else {
    error("Mismatch between category codes and names")
  }
}

RLIe <- function(x,w=c("CO"=5,"CR"=4,"EN"=3,"VU"=2,"NT"=1,"LC"=0,"DD"=NA,"NE"=NA)) {
  TS <- threat.score(x,w)
  1-(TS/max(w,na.rm = T))
}

data <-
  Macrogroups.Country %>%
  group_by(Country) %>%
  summarise(`Threat score`=threat.score(Overall.Category),
            CR=sum(Overall.Category %in% "CR"),
            EN=sum(Overall.Category %in% "EN"),
            VU=sum(Overall.Category %in% "VU"),
            NT=sum(Overall.Category %in% "NT"),
            LC=sum(Overall.Category %in% "LC"),
            DD=sum(Overall.Category %in% "DD"),
            ## NE=sum(Overall.Category %in% "LE"),
            `RLI of ecosystems`=RLIe(Overall.Category))

df <- data %>% arrange(`Threat score`) %>% mutate(order=order(`Threat score`))

opts <- c("Red List Index of ecosystems","Threat score")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title

    titlePanel( div(column(width = 3, tags$a(href='http://apps.global-ecosystems.org',
                                             tags$img(src='logo.png', width = 220))),
                    column(width = 9, h1("Forest Macrogroups of the Americas"),h2("Red List Index of Ecosystems")
                           )),
                windowTitle="RLI-ecosystems-forest-Americas"
    ),
    # Sidebar with a select input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId="index",
                      label="Index to show", opts),
          plotlyOutput("whatPlot"),
          h4("You can highlight countries by selecting from the table."),
          verbatimTextOutput('x4')
                  ),

        # Show plot of scores/indices
        mainPanel(
          h3("Data and methods"),
          p("The table is based on the IUCN Red List of Ecosystem assessments of the forest macrogroups of the Americas (Ferrer-Paris et al. 2019). Details of the assessments are available in the ",a("IUCN RLE assessments database",href="https://assessments.iucnrle.org/systematics/8"),"."),
          p("The ",strong("Red List Index of Ecosystems (RLIE)")," was described by Rowland et al. (2020). The average risk of ecosystem collapse increases as the index value decreases. This index is documented by the:",a("Biodiversity Indicators Partnership",href="https://www.bipindicators.net/indicators/red-list-index-of-ecosystems"),"."),
          p("The ",strong("Threat score")," is calculated according to Ferrer-Paris et al. (2019). Higher values mean higher level of threat."),
          DTOutput("Table"),
          p("The columns represent the number of ecosystems in each category:",
          strong("CR")," Critically Endangered, ",
          strong("EN")," Endangered, " ,
          strong("VU")," Vulnerable, " ,
          strong("NT")," Near Threatened, " ,
          strong("LC")," Least Concern, " ,
          ## strong("NE")," Not Evaluated, " ,
          strong("DD")," Data Deficient. "),
          h3("References"),
          p("Ferrer-Paris, J.R., Zager, I., Keith, D.A., Oliveira-Miranda, M.A., Rodríguez, J.P, Josse, C., González-Gil, M., Miller, R.M., Zambrana-Torrelio, C., Barrow, E., 2019. An ecosystem risk assessment of temperate and tropical forests of the Americas with an outlook on future conservation strategies. Conserv. Lett. 12. ",a("DOI:10.1111/conl.12623", href="https://doi.org/10.1111/conl.12623")),
          p("Ferrer-Paris, José R. (2018): IUCN Red List of Ecosystem assessment results for IVC Forest Macrogroups in the Americas region. figshare. Dataset. ",a("DOI:10.6084/m9.figshare.7488872.v1",href="https://doi.org/10.6084/m9.figshare.7488872.v1")),
          p("Rowland JA, Bland LM, Keith DA, Juffe‐Bignoli D, Burgman MA, Etter A, Ferrer‐Paris JR, Miller RM, Skowno AL, Nicholson E. 2020. Ecosystem indices to support global biodiversity conservation. Conservation Letters 13:311.", a("DOI:10.1111/conl.12680",href="http://dx.doi.org/10.1111/conl.12680")),
          h3("Credits"),
          p("This app was created by ",
            a("José R. Ferrer-Paris",href="https://github.com/jrfep")," from UNSW ",
            a("Centre for Ecosystem Science",href="https://www.ecosystem.unsw.edu.au/"),
            " and UNSW Data Science Hub. ",
            a("Source code.",href="https://github.com/red-list-ecosystem/RLE-forests-panam-RLIE")),
          p("More apps ",a("here.",href="http://apps.global-ecosystems.org/"))
        )
    )
)

# Define server logic required to draw plot
server <- function(input, output) {

    output$whatPlot <- renderPlotly({
      s = input$Table_rows_selected
      if (length(s)) {
        slc <- df %>% slice(s) %>% pull(Country)
      } else {
        slc <- ""
      }

      p <- ggplot({df %>% mutate(selected=Country %in% slc)}) 

      if(input$index=="Threat score") {
        pp <- p + geom_point(aes(y=`Threat score`,x=order, colour=selected,label=Country)) + ylim(0,5) +
          xlab("Countries from lowest to highest risk") +
          annotate("text", x = 12, y = c(0,5), label = c("All Least Concern","All Collapsed"),
                   hjust=0, color=c("darkgreen","black"))
      } else {
        pp <- p + geom_point(aes(y=`RLI of ecosystems`,x=order, colour=selected,label=Country))  +
          ylab("Red List Index of Ecosystems") + ylim(0,1) +
          xlab("Countries from lowest to highest risk") +
          annotate("text", x = 12, y = c(1,0), label = c("All Least Concern","All Collapsed"),
                   hjust=0,color=c("darkgreen","black"))
      }
      ggplotly(pp +  theme_minimal()+theme(legend.position='none') )
      })

    output$Table <- DT::renderDataTable({

        datatable(df %>% select(Country,`RLI of ecosystems`,`Threat score`,CR,EN,VU,NT,LC,DD)) %>%
        formatRound(c(2:3), 2) %>%
        formatStyle(columns = c(2:10), 'text-align' = 'center') %>%
        formatStyle(
          'CR',
          color = styleInterval(c(0.5), c('black', 'white')),
          backgroundColor = styleInterval(0.5, c('white', 'red'))
        ) %>%
        formatStyle(
          'EN',
          color = styleInterval(c(0.5), c('black', 'white')),
          backgroundColor = styleInterval(0.5, c('white', 'orange'))
        ) %>%
        formatStyle(
          'VU',
          backgroundColor = styleInterval(0.5, c('white', 'yellow'))
        ) %>%
        formatStyle(
          'NT',
          color = styleInterval(c(0.5), c('black', 'white')),
          backgroundColor = styleInterval(0.5, c('white', 'green'))
        ) %>%
        formatStyle(
          'LC',
          color = styleInterval(c(0.5), c('black', 'white')),
          backgroundColor = styleInterval(0.5, c('white', 'darkgreen'))
        )%>%
        formatStyle(
          'DD',
          color = styleInterval(c(0.5), c('black', 'white')),
          backgroundColor = styleInterval(0.5, c('white', 'grey'))
        )

    })
    # print the selected indices
    output$x4 = renderPrint({
      s = input$Table_rows_selected
      if (length(s)) {
        cat('Selected countries are:\n\n')
        slc <- df %>% slice(s) %>% pull(Country)
        cat(slc, sep = ', ')
      }
    })

}

# Run the application
shinyApp(ui = ui, server = server)
