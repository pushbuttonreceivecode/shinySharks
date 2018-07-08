#ui code...

library(shiny)
library(shinythemes)
library(DT)
library(plotly)
library(shinyjqui)

source("utilities.R")

ui = function(){
  fixedPage(
    title = "Shark Attack Statistics",
    titlePanel("Shark Attack Statistics"),
    theme = shinytheme("darkly"),
    includeCSS("www/styles.css"),
    tabsetPanel(
      id="mainTabset",
      tabPanel(
        "World Map",
        fluidRow(
          column(
            12,
            div(
              class = "dash-card",
              h1("Global Reported Human-Shark Interactions"),
              jqui_resizable(plotlyOutput(
                "map"
              )),
              htmlOutput(
                "mapText"
              )
            )
          )
        )
      ),
      tabPanel(
        "Characteristics",
        class="char-bg",
        fluidRow(
          column(
            12,
            sliderInput("year", "Range in Years: ", width = "100%", min = 0, max = 2018, value = c(0,2018), sep = ""),
            danger("Year zero incidents have uncertain dates of occurrence and may have actually taken place more recently.")
          )
        ),
        fixedRow(
          column(
            4,
            div(
              class = "dash-card",
              h1("Fatal or Non-Fatal"),
              plotlyOutput(
                "yearFatal"
              ),
              htmlOutput(
                "fatalText"
              )
            )
          ),
          column(
            4,
            div(
              class = "dash-card",
              h1("Type of Incident"),
              plotlyOutput(
                "yearType"
              ),
              htmlOutput(
                "typeText"
              )
            )
          ),
          column(
            4,
            div(
              class = "dash-card",
              h1("Gender"),
              plotlyOutput(
                "yearSex"
              ),
              htmlOutput(
                "sexText"
              )
            )
          ),
          column(
            12,
            div(
              class = "dash-card",
              h1("Age"),
              plotlyOutput(
                "yearAge"
              ),
              htmlOutput(
                "centralAge"
              )
            )
          ),
          column(
            12,
            div(
              class = "dash-card",
              h1("Shark Species"),
              plotlyOutput(
                "yearSpecies"
              ),
              br(),
              actionButton("showSpeciesInfo", "Show Species Wikipedia Links"),
              actionButton("hideSpeciesInfo", "Hide Species Wikipedia Links"),
              htmlOutput(
                "speciesText"
              ),
              div(
                id = "moreSpeciesInfo",
                class = "compact",
                h2("More Information:"),
                DT::dataTableOutput(
                  "speciesTable"
                ),
                style = "display:none;"
              )
            )
          )
        )
      ),
      tabPanel(
        "Explore Data",
        div(
          class = "dash-card",
          DT::dataTableOutput(
            "data"
          )
        )
      )
    ),
    h1("Data Source:"),
    p(
      a(
        "Global Shark Attack File", 
        href = "http://www.sharkattackfile.net/incidentlog.htm",
        target = "_blank"
      )
    ),
    warn("Data cleaned, anonymized, recoded, and extended to suit my own purposes."),
    infolink("You can find my cleaned data set on", "GitHub", "https://github.com/pushbuttonreceivecode/shinySharks")
  )
}
