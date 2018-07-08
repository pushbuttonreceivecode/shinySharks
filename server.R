#server code...

library(shiny)
library(shinythemes)
library(DT)
library(plotly)
library(RColorBrewer)

source("utilities.R")

server = function(input, output){
  #NOTE TO SELF: UTF-8 encoding is required on Linux
  #OpenOffice default is some other format - be wary and save as right format!
  sharks = read.csv(paste0(getwd(), "/www/sharks.csv"), stringsAsFactors = FALSE)
  countries = read.csv(paste0(getwd(), "/www/sharks_geocoded_countries_clean.csv"), stringsAsFactors = FALSE)
  
  #**************************************
  #analyses...
  #**************************************
  
  #only include the *sure* yes/no cases, there is
  #not enough context here to fix the few bad values...
  fatalAttacksPerYear = data.frame(xtabs(~Year+Fatal, subset(sharks, sharks$Fatal %in% c("Y","N"))))
  
  #type of interaction
  type = data.frame(xtabs(~Year+Type, sharks, exclude = c(NA, 0, "Invalid")))
  
  #weed out the non-numeric age values...
  age = data.frame(xtabs(~Year+as.numeric(Age), sharks, exclude = c(NA, 0)))
  names(age) = c("Year", "Age", "Count")
  
  #gender of human involved in the interaction...
  sex = data.frame(xtabs(~Year+Sex, subset(sharks, sharks$Sex %in% c("F","M"))))
  
  #activity they were partaking in during the interaction...
  #TODO
  
  #injury sustained by the individual...
  #TODO
  
  #species of shark
  species = data.frame(xtabs(~Year+Species, sharks, exclude = c(NA, "")))
  speciesMoreInfo = read.csv(paste0(getwd(), "/www/species_info.csv"), stringsAsFactors = FALSE)
  
  #**************************************
  #end analyses
  #**************************************
  
  output$map = renderPlotly({
    d = countries
    d %>%
      plot_geo(
        locationmode = 'country names', sizes = c(1, 50), color = I("black")
      ) %>%
      add_trace(
        z = ~Count, locations = ~Country, 
        color = ~Count, colors = 'YlOrRd', marker = list(line = list(color = toRGB("grey"), width = 0.5)), 
        text = ~paste(Country, "<br>Human-Shark Interactions: ", Count)
      ) %>%
      layout(
        showlegend = FALSE,
        geo = list(
          showcountries = TRUE,
          scope = "world",
          showframe = TRUE,
          showcoastlines = TRUE,
          projection = list(type = 'Mercator')
        )
      )
  })
  
  output$mapText = renderPrint({
    div(
      hr(),
      p(tags$b("n = ", sum(countries$Count))),
      info("Counts include interactions from all years of available data.")
    )
  })
  
  output$yearFatal = renderPlotly({
    s = subset(fatalAttacksPerYear, fatalAttacksPerYear$Year %in% c(input$year[1]:input$year[2]))
    #sum and aggregate
    d = as.data.frame(rbind("Non-fatal" = sum(subset(s, s$Fatal == "N")["Freq"]), "Fatal" = sum(subset(s, s$Fatal == "Y")["Freq"])))
    names(d) = "Fatal"
    plot_ly(
      d, 
      values = ~Fatal,
      labels = ~rownames(d),
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      text = ~paste0(rownames(d), "<br>", Fatal),
      marker = list(colors = c('rgb(100,0,0)', 'rgb(200,100,0)'), line = list(color = '#FFFFFF', width = 1)
      ),
      showlegend = TRUE,
      type = 'pie'
      ) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })
  
  output$fatalText = renderPrint({
    s = subset(fatalAttacksPerYear, fatalAttacksPerYear$Year %in% c(input$year[1]:input$year[2]))
    d = as.data.frame(rbind("No" = sum(subset(s, s$Fatal == "N")["Freq"]), "Yes" = sum(subset(s, s$Fatal == "Y")["Freq"])))
    names(d) = "Fatal"
    div(
      hr(),
      p(tags$b("n = ", sum(d$Fatal)))
    )
  })
  
  output$yearType = renderPlotly({
    s = subset(type, type$Year %in% c(input$year[1]:input$year[2]))
    d = as.data.frame(
      rbind(
        "Boating" = sum(subset(s, s$Type == "Boating")["Freq"]), 
        "Sea Disaster" = sum(subset(s, s$Type == "Sea Disaster")["Freq"]), 
        "Questionable" = sum(subset(s, s$Type == "Questionable")["Freq"]), 
        "Unprovoked" = sum(subset(s, s$Type == "Unprovoked")["Freq"]), 
        "Provoked" = sum(subset(s, s$Type == "Provoked")["Freq"])
      )
    )
    names(d) = "Type"
    plot_ly(
      d, 
      values = ~Type,
      labels = ~rownames(d),
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#000000'),
      hoverinfo = 'text',
      text = ~paste0('Type of Incident: ', rownames(d), "<br>", Type),
      marker = list(colors = brewer.pal(5, 'YlOrRd'), line = list(color = '#FFFFFF', width = 1)
      ),
      showlegend = TRUE,
      type = 'pie'
    ) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })
  
  output$typeText = renderPrint({
    s = subset(type, type$Year %in% c(input$year[1]:input$year[2]))
    div(
      hr(),
      p(tags$b("n = ", sum(s$Freq)))
    )
  })
  
  output$yearSex = renderPlotly({
    s = subset(sex, sex$Year %in% c(input$year[1]:input$year[2]))
    d = as.data.frame(
      rbind(
        "Male" = sum(subset(s, s$Sex == "M")["Freq"]), 
        "Female" = sum(subset(s, s$Sex == "F")["Freq"])
      )
    )
    names(d) = "Sex"
    plot_ly(
      d, 
      values = ~Sex, 
      labels = ~rownames(d), 
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      text = ~paste0('Gender of Person in the Incident: ', rownames(d), "<br>", Sex),
      marker = list(colors = c('rgb(100,0,0)', 'rgb(200,100,0)'), line = list(color = '#FFFFFF', width = 1)
      ),
      showlegend = TRUE,
      type = 'pie'
      ) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })
  
  output$sexText = renderPrint({
    s = subset(sex, sex$Year %in% c(input$year[1]:input$year[2]))
    div(
      hr(),
      p(tags$b("n = ", sum(s$Freq)))
    )
  })
  
  output$yearAge = renderPlotly({
    s = subset(age, age$Year %in% c(input$year[1]:input$year[2]) & age$Count != 0)
    d = as.data.frame(
      aggregate(s$Count, by = list(Category=s$Age), FUN = sum)
    )
    names(d) = c("Age", "Count")
    myc = c(brewer.pal(9, 'YlOrRd'),brewer.pal(9, 'YlOrRd'),brewer.pal(9, 'YlOrRd'),brewer.pal(9, 'YlOrRd'),brewer.pal(9, 'YlOrRd'),
            brewer.pal(9, 'YlOrRd'),brewer.pal(9, 'YlOrRd'),brewer.pal(9, 'YlOrRd'),brewer.pal(9, 'YlOrRd'),brewer.pal(9, 'YlOrRd'),
            brewer.pal(9, 'YlOrRd'),brewer.pal(9, 'YlOrRd'),brewer.pal(9, 'YlOrRd'))
    plot_ly(
      d
    ) %>%
      add_trace(
        x = ~Age,
        y = ~Count, 
        hoverinfo = 'text',
        text = ~paste0('Age of Person in the Incident: ', Age, " Years<br>", Count),
        marker = list(color = myc[1:length(d$Age)], line = list(color = '#FFFFFF', width = 1)
        ),
        showlegend = TRUE,
        type = 'bar'
      ) %>%
      layout(
        
      )
  })
  
  output$centralAge = renderPrint({
    s = subset(age, age$Year %in% c(input$year[1]:input$year[2]) & age$Count != 0)
    div(
      hr(),
      h2("Measures of Central Tendency:"),
      p(tags$b("n = ", length(s$Age))),
      p(tags$b("Mean age: ", round(mean(as.numeric(s$Age)),2))),
      p(tags$b("Median age: ", median(as.numeric(s$Age)))),
      p(tags$b("Mode of age: ", statsMode(as.numeric(s$Age)))),
      danger("Age not reported for many incidents.")
    )
  })
  
  output$yearSpecies = renderPlotly({
    s = subset(species, species$Year %in% c(input$year[1]:input$year[2]))
    d = as.data.frame(
      aggregate(s$Freq, by = list(Category=s$Species), FUN = sum)
    )
    names(d) = c("Species", "Count")
    myc = c(brewer.pal(9, 'Spectral'),brewer.pal(9, 'Spectral'),brewer.pal(9, 'Spectral'),brewer.pal(9, 'Spectral'),brewer.pal(9, 'Spectral'),brewer.pal(9, 'Spectral'),brewer.pal(9, 'Spectral'))
    plot_ly(
      d, 
      values = ~Count, 
      labels = ~Species, 
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#000000'),
      hoverinfo = 'text',
      text = ~paste0('Shark Species: ', Species, "<br>", Count),
      marker = list(colors = myc[1:length(d$Species)], line = list(color = '#FFFFFF', width = 0.5)
      ),
      showlegend = TRUE,
      type = 'pie'
    ) %>%
      layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })
  
  output$speciesText = renderPrint({
    s = subset(species, species$Year %in% c(input$year[1]:input$year[2]))
    div(
      hr(),
      p(tags$b("n = ", sum(s$Freq))),
      danger("Shark species not determined for many incidents.")
    )
  })
  
  output$speciesTable = renderDataTable({
    DT::datatable(speciesMoreInfo, escape = c(TRUE, FALSE))
  })
  
  observeEvent(input$hideSpeciesInfo,{
    jqui_hide("#moreSpeciesInfo", effect = "clip")
  })
  observeEvent(input$showSpeciesInfo,{
    jqui_show("#moreSpeciesInfo", effect = "clip")
  })
  
  output$data = DT::renderDataTable({
    #can pass options to DataTable via "options" list...
    #https://rstudio.github.io/DT/
    #useful for example:
    #https://datatables.net/examples/basic_init/scroll_x.html
    d = sharks %>% select("Year", "Country", "Area", "Location", "Activity", "Injury", "Fatal", "Sex", "Age", "Species")
    DT::datatable(d, options = list("scrollX" = "true"))
  })
  
}