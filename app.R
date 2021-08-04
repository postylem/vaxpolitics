# This is a shiny app for visualizing COVID vaccinations vs 2020 presidential politics by state

library(shiny)
library(tidyverse)
# library(docstring)
library(ggrepel) # for repelling labels with geom_label_repel()

get_vaxpolitics_data <- function() {
    #' Read in election and vaccine data
    #'
    #' Reads in the elections data and the vaccinations data
    #' from two separate csv files (hardcoded).
    #' @return a list containing
    #' `df`: the joined dataframe;
    #' `vaxcolnames`: the columns that came from the vaccination data;
    #' `vaxfileinfo`: the first two lines of the vaccination file (date info).
    #' `cdtcolnames`: the columns that came from the cases/deaths/testing data;
    #' `cdtfileinfo`: the first two lines of the cases/deaths/testing file (date info).
    elections_file <- 'data/1976-2020-president.csv'
    elections_raw <- read_csv(elections_file)
    elections <- elections_raw %>%
        filter(year==2020) %>%
        mutate(`%vote`=candidatevotes/totalvotes*100) %>%
        filter(party_simplified %in% c('DEMOCRAT','REPUBLICAN')) %>%
        select(state, state_po, candidate, party_simplified,
               totalvotes, candidatevotes, `%vote`) %>%
        pivot_wider(names_from = party_simplified, values_from = -c(state,state_po,party_simplified))
    election_states<- elections$state

    # VAX DATA
    vaccinations_file <- 'data/covid19_vaccinations_in_the_united_states.csv'
    vaccination_file_info <- readLines(file(vaccinations_file,'r'),2)
    # read in vaccinations data
    vaccinations_raw <- read_csv(vaccinations_file, skip = 2)
    vaccinations <- vaccinations_raw %>%
        mutate(state = `State/Territory/Federal Entity` %>% str_to_upper()) %>%
        select(-`State/Territory/Federal Entity`)
    # Hand adjust to make the states names all match... it's just New York that was a problem
    # note that NYC does not appear separately in this data, so this renaming isn't ambiguous
    vaccinations$state <- gsub("NEW YORK STATE", "NEW YORK", vaccinations$state)
    vaccination_colnames <- colnames(vaccinations)
    vacc_states <- vaccinations$state

    # CASES/DEATHS/TESTING DATA
    cdt_file <- 'data/united_states_covid19_cases_deaths_and_testing_by_state.csv'
    cdt_file_info <- readLines(file(cdt_file,'r'),2)
    # read in cdt data
    cdt_raw <- read_csv(cdt_file, skip = 2)
    cdt <- cdt_raw %>%
        mutate(state = `State/Territory` %>% str_to_upper()) %>%
        select(-`State/Territory`)
    # TODO: there's a problem in this data: NEW YORK and NEW YORK CITY are separate...
    # For now, I am just using the row labeled New York, and ignoring New York CIty
    cdt_colnames <- colnames(cdt)
    cdt$state <- sub("NEW YORK*", "NEW YORK", cdt$state, fixed=T)
    cdt_states <- cdt$state


    # should be 51 'states' (the states plus DC)
    states <- intersect(vacc_states, election_states)

    vaxelect <- inner_join(elections,vaccinations) %>% select(state, everything())
    cdtelect <- inner_join(elections,cdt) %>% select(state, everything())
    # it's nicer to have state names in title case if I want to use them
    vaxelect$state <- vaxelect$state %>% str_to_title() %>% gsub("Of", "of", .)
    cdtelect$state <- vaxelect$state %>% str_to_title() %>% gsub("Of", "of", .)

    return(list("vaxelect_df" = vaxelect,
                "vaxcolnames" = vaccination_colnames,
                "vaxfileinfo" = vaccination_file_info,
                "cdtelect_df" = cdtelect,
                "cdtcolnames" = cdt_colnames,
                "cdtfileinfo" = cdt_file_info))
}

plot_vaxelect <- function(data, xcolumn_str, ycolumn_str) {
    #' Makes vaccination vs election result plot
    #'
    #' Uses ggplot to make a plot of vaccination metric vs election result
    #'
    #' @param data a dataframe with one observation per state,
    #' and columns for vaccination metrics and election results
    #' @param xcolumn_str the df column name for horizontal axis, as a string
    #' @param ycolumn_str the df column name for vertical axis, as a string
    #' @returns the plot

    xcolumn <- sym(xcolumn_str)
    ycolumn <- sym(ycolumn_str)
    gs.pal <- colorRampPalette(c("blue","red"),bias=.1,space="rgb")
    plot.Vax_vs_Trumpvote <- data %>%
        filter(state!="District of Columbia") %>%
        ggplot(aes(y=as.numeric(!!ycolumn), color=factor(!!xcolumn), x=!!xcolumn)) +
        # geom_label(
        #     aes(label=round(!!xcolumn,1)),
        #     alpha=.5, label.size = 0) +
        geom_point(size=3,alpha=0.85) +
        geom_label_repel(
            aes(label=state_po), alpha=.85, min.segment.length = 0,
            label.size=.75, label.padding = .2) +
        scale_color_manual(values=gs.pal(51)) +
        scale_fill_manual(values=gs.pal(51)) +
        xlab("Percent vote for Trump, 2020") +
        ylab(ycolumn_str) +
        theme(legend.position = "none")
    return(plot.Vax_vs_Trumpvote)
}

vaxpolitics_data <- get_vaxpolitics_data()

vaxelect <- vaxpolitics_data$vaxelect_df
vaccination_column_choices <- vaxpolitics_data$vaxcolnames[vaxpolitics_data$vaxcolnames != 'state']

cdtelect <- vaxpolitics_data$cdtelect_df
cdt_column_choices <- vaxpolitics_data$cdtcolnames[vaxpolitics_data$cdtcolnames != 'state']

metrics_choices = c("metrics proportional to population", "all metrics")

vaxmetric_choices_all <- vaccination_column_choices
vaxmetric_choices_prop <- vaccination_column_choices %>% grep("(.*per\ .*$)|(Percent.*$)", ., value=T)
# We could also included only nonproportional metrics, but seems pointless
# vaxmetric_choices_nonprop <- setdiff(vaxmetric_choices_all,vaxmetric_choices_prop)

# remove the % columns like Total % Positive, which are non-numeric
cdt_choices_all <- cdt_column_choices[!grepl("%", cdt_column_choices)]
cdt_choices_prop <- cdt_choices_all %>% grep("(.*per\ .*$)", ., value=T)

# Define UI for application
ui <- fluidPage(
    # Application title
    titlePanel("US COVID stats vs. 2020 presidential politics"),

    # Vertical layout for page with
    verticalLayout(
        div(
            h3("Vaccination rates vs politics"),
            # Vaccination metrics set selector with a radio button
            # The population-proportional subset of metrics are more easy to interpret,
            # but I want to leave the option to look at all the metrics provided by the CDC
            p("Select which vaccination metric you are interested in, and see it plotted against 2020 presidential politics."),
            p("By default only the metrics that are proportional to population (like, 'Doses Delivered per 100K') are included. You can choose to include all metrics instead if you want."),
            radioButtons(
                inputId = "vaxmetric_selector",
                label = "Choose set of metrics (changes the contents of the boxes below):",
                choices = metrics_choices,
                inline = TRUE,
                selected = metrics_choices[1]
            ),
            selectInput(
                inputId = "vaxmetric_choice",
                label = "Select a vaccination metric (to plot on y-axis)",
                width="100%",
                size = 5,
                selectize = F,
                choices = vaccination_column_choices,
                selected = "Percent of Total Pop with at least One Dose by State of Residence",
                # We can initialize this as NULL because
                # we will update in the server function.
                # choices = NULL
            ),
        ),
        plotOutput("vaxelectPlot"),
        div(
            p("correlation coefficient r = ", textOutput("vaxelect_corrText", inline=T)),
            align="center"
            ),
        div(
            h3("Cases/Testing/Deaths vs politics"),
            p("Select which vaccination metric you are interested in, and see it plotted against 2020 presidential politics."),
            p("By default only the metrics that are proportional to population (like, '7-Day Cases Rate per 100000') are included. You can choose to include all metrics instead, if you want, by selecting 'all metrics' with the radio button above."),
            selectInput(
                inputId = "cdt_choice",
                label = "Select a cases/testing/deaths metric (to plot on y-axis)",
                width="100%",
                size = 5,
                selectize = F,
                choices = cdt_choices_all,
                selected = "7-Day Cases Rate per 100000"
            ),
        ),
        plotOutput("cdtelectPlot"),
        div(
            p("correlation coefficient r = ", textOutput("cdtelect_corrText", inline=T)),
            align="center"
        ),
        div(
            h2("Data sources"),
            p("Vaccination data from CDC, downloaded from here:",
              a(vaxpolitics_data$vaxfileinfo[1],
                href="https://covid.cdc.gov/covid-data-tracker/#vaccinations", .noWS = "after"), ".",
              em(vaxpolitics_data$vaxfileinfo[2], .noWS = "after"), "."),
            p("Cases/testing/deaths data from CDC, downloaded from here:",
              a(vaxpolitics_data$cdtfileinfo[1],
                href="https://covid.cdc.gov/covid-data-tracker/#cases_casesper100klast7days", .noWS = "after"), ".",
              em(vaxpolitics_data$cdtfileinfo[2], .noWS = "after"), "."),
            p("Election data downloaded from",
              a("MIT EDSL", href="https://electionlab.mit.edu/"),
              "downloaded here:",
              a("MIT Election Data and Science Lab, 2017, U.S. President 1976--2020",
                href="https://doi.org/10.7910/DVN/42MVDX"))
        ),
        div(hr(),
            p("This little app was written in R by",
              a("jacob", href="https://jahoo.github.io", .noWS = "after"), ".",
            "Source code",
              a("here", href="https://github.com/postylem/vaxpolitics", .noWS = "after"), ".")
        )
    )
)

# Define server logic required to get the selection and draw the plot
server <- function(input, output, session) {

    # Whatever is inside `observeEvent()` will be triggered each time the first
    # argument undergoes a change in value. In this case, that means whenever
    # the user changes the radio button value.
    observeEvent(input$vaxmetric_selector,
                 {
                     if (input$vaxmetric_selector == "all metrics") {
                         vaxmetric_choices <- vaxmetric_choices_all
                         cdt_choices <- cdt_choices_all
                     }
                     else {
                         vaxmetric_choices <- vaxmetric_choices_prop
                         cdt_choices <- cdt_choices_prop
                     }
                     updateSelectInput(
                         session,
                         inputId = "vaxmetric_choice",
                         choices = vaxmetric_choices,
                         selected = "Percent of Total Pop with at least One Dose by State of Residence")

                     output$vaxelectPlot <- renderPlot({
                         plot_vaxelect(vaxelect,
                                       "%vote_REPUBLICAN",
                                       input$vaxmetric_choice)
                     })

                     output$vaxelect_corrText <- renderText({
                         round(cor(
                             as.numeric(unlist(vaxelect[input$vaxmetric_choice])),
                             vaxelect$`%vote_REPUBLICAN`),
                             4)
                     })

                     updateSelectInput(
                         session,
                         inputId = "cdt_choice",
                         choices = cdt_choices,
                         selected = "7-Day Cases Rate per 100000")

                     output$cdtelectPlot <- renderPlot({
                         plot_vaxelect(cdtelect,
                                       "%vote_REPUBLICAN",
                                       input$cdt_choice)
                     })

                     output$cdtelect_corrText <- renderText({
                         round(cor(
                             as.numeric(unlist(cdtelect[input$cdt_choice])),
                             cdtelect$`%vote_REPUBLICAN`),
                             4)
                     })

    })
}

# Run the application
shinyApp(ui = ui, server = server)
