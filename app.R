library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(alfred)
library(gridExtra)
library(quantmod)
library(zoo)
library(forecast)
library(scales)
library(hockeystick)

#https://rstudio.github.io/shinydashboard/structure.html

body <- dashboardBody(
        tabItems(
                tabItem(tabName = "dashboard",
                        fluidRow(
                                box(plotOutput("grapha1", width = "100%")),
                                box(plotOutput("grapha2", width = "100%")),
                                box(plotOutput("grapha3", width = "100%")),
                                box(plotOutput("grapha4", width = "100%")),
                                box(plotOutput("grapha5", width = "100%")),
                                box(plotOutput("grapha6", width = "100%")),
                                box(plotOutput("grapha7", width = "100%")),
                                box(plotOutput("grapha8", width = "100%")),
                                box(plotOutput("grapha9", width = "100%"))
                        )
                ),
                
                tabItem(tabName = "econ",
                        fluidRow(
                                box(plotOutput("graphb1", width = "100%")),
                                box(plotOutput("graphb2", width = "100%")),
                                box(plotOutput("graphb3", width = "100%")),
                                box(plotOutput("graphb4", width = "100%")),
                                box(plotOutput("graphb5", width = "100%")),
                                box(plotOutput("graphb6", width = "100%")),
                                box(plotOutput("graphb7", width = "100%")),
                                box(plotOutput("graphb8", width = "100%"))
                                
                        )
                ),
                
                tabItem(tabName = "climate",
                        fluidRow(
                                box(plotOutput("graphc1", width = "100%")),
                                box(plotOutput("graphc2", width = "100%")),
                                box(plotOutput("graphc3", width = "100%")),
                                box(plotOutput("graphc4", width = "100%")),
                                box(plotOutput("graphc5", width = "100%")),
                                box(plotOutput("graphc6", width = "100%")),
                                box(plotOutput("graphc7", width = "100%")),
                                box(plotOutput("graphc8", width = "100%"))
                                
                        )
                ),
                
                tabItem(tabName = "info",
                        h2("INFO"),
                        hr(),
                        h5("App by Jose Ramon Pineda (2022) in Shiny."),
                        hr(),
                        h3("Sources"),
                        hr(),
                        fluidRow("- St. Louis Fed, Federal Reserve Economic Data, https://fred.stlouisfed.org/"),
                        fluidRow("- NOAA's Goddard Institute for Space Studies, https://data.giss.nasa.gov/gistemp/"),
                        fluidRow("- Johns Hopkins Coronavirus Resource Center, https://github.com/CSSEGISandData"),
                        fluidRow("- R Package: alfred, https://cran.r-project.org/web/packages/alfred/"),
                        fluidRow("- R Package: hockeystick, https://cran.r-project.org/web/packages/hockeystick/index.html"),
                        hr(),
                        h5("Code for the app can be found here: https://github.com/joram88/Dashboard")
                )

        )
)


ui <- dashboardPage(skin = "yellow",
                    header = dashboardHeader(title = "JRP's Dashboard"),
                    sidebar = dashboardSidebar(sidebarMenu(id = "tabs", style = "position:fixed;width:220px;",
                            menuItem("Main Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                            menuItem("Econ (WIP)", tabName = "econ", icon = icon("th")),
                            menuItem("Climate (WIP)", tabName = "climate", icon = icon("th")),
                            menuItem("Info (WIP)", tabName = "info", icon = icon("question")),
                            fluidPage(checkboxInput("Historical", "Historical Data", FALSE)),
                            fluidPage(checkboxInput("Forecast", "Forecast (WIP)", FALSE))
                    )),
                    body = body
)

server <- function(input, output) {
        
        raw_usa <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
        
        TradeBalance <- get_fred_series("BOPGSTB", series_name = "Trade Balance")
        CPI <- get_fred_series("MEDCPIM158SFRBCLE", series_name = "CPI")
        GDP <- get_fred_series("A191RL1Q225SBEA", series_name = "GDP")
        UNRATE <- get_fred_series("UNRATE", series_name = "Unemployment Rate")
        MEXP <- get_fred_series("DEXMXUS", series_name = "MEX/USD Exchange Rate")
        RGDP <- get_fred_series("GDPC1", series_name = "Real Domestic Product")
        HOME <- get_fred_series("QUSR368BIS", series_name = "Real Residential Property Prices")
        
        getSymbols("^GSPC", src = "yahoo")
        #SP500 <- tail(GSPC$GSPC.Close, 60)
        SP500 <- data.frame(date=index(GSPC), coredata(GSPC))[,c("date", "GSPC.Close")]
        
        getSymbols("^W5000", src = "yahoo")
        WILL <- data.frame(date=index(W5000), coredata(W5000))[,c("date", "W5000.Close")]
        
        buff <- merge(x = WILL, y = RGDP, all.x = TRUE)
        
        buff  <- buff %>% 
                mutate(rgdpi = na.spline(buff[3], 1:nrow(buff), na.rm = FALSE)) %>% 
                mutate(buff = W5000.Close / rgdpi)
        
        #Denver Covid
        
        denver <- c("8001", "8005", "8014", "8019", "8031", "8035", "8039", "8047", "8059", "8093")
        
        raw_denvmetro <- raw_usa %>% 
                filter(FIPS %in% denver)
        
        denver_counties <- gather(raw_denvmetro, key = "Date", value="Value", -c(Admin2, UID, iso2, iso3, code3, FIPS,
                                                                                 Province_State, Country_Region, Lat,
                                                                                 Long_, Combined_Key))
        
        denver_counties <- denver_counties %>% 
                select(Admin2, Combined_Key, Date, Value) %>% 
                arrange(Admin2, Combined_Key)
        
        denver_counties$Date <- mdy(denver_counties$Date)
        
        denver_counties <- denver_counties %>%  
                group_by(Admin2, Date) %>% 
                summarize(Value=sum(Value)) %>% 
                select(Admin2, Date, Value) %>% 
                mutate(new_cases = Value - lag(Value))
        
        denver_counties$Week <- week(denver_counties$Date)
        
        denver_pop <- 715522
        
#GRAPHS
        
        output$grapha1 <- renderPlot({
 
                
                # S&P500
                
                if(input$Forecast == TRUE){
                        SP500_ts <- as.ts(SP500[2])
                        SP500_AR <- auto.arima(SP500_ts)
                        SP500_for <- forecast(SP500_AR, h = 30)
                        SP500_model <- data.frame(Value=as.numeric(SP500_for$mean, check.names = FALSE))
                        SP500_model$date <- last(SP500$date)+ 1 : 30
                }

                
                if(input$Historical == FALSE){
                        SP500 <- slice(SP500, tail(row_number(), 90))
                }

                
                p1 <-   ggplot()+
                        geom_line(data = SP500, mapping = aes(x = date, y = GSPC.Close))+
                        ylab("Index")+
                        xlab("")+
                        labs(title = "S&P500")+
                        scale_x_date(date_labels = "%b %Y")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p1 <- p1+
                                geom_line(data = SP500_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                }
                
                p1
        })
        
        output$grapha2 <- renderPlot({
                
                #Buffett Indicator
                
                if(input$Forecast == TRUE){
                        buff_ts <- as.ts(buff['buff'])
                        buff_AR <- auto.arima(buff_ts)
                        buff_for <- forecast(buff_AR, h = 30)
                        buff_model <- data.frame(Value=as.numeric(buff_for$mean, check.names = FALSE))
                        buff_model$date <- last(buff$date) + 1 : 30
                }
                
                if(input$Historical == FALSE){
                        buff <- slice(buff, tail(row_number(), 120))
                }
                
                p2 <- buff %>% 
                        ggplot(mapping = aes(x = date, y = buff))+
                        geom_line()+
                        geom_hline(yintercept = 1, color = "red")+
                        ylab("Ratio")+
                        xlab("")+
                        labs(title = "Buffett Indicator", subtitle = "Mkt Capitalizaton / Real GDP")+
                        scale_x_date(date_labels = "%b %Y")+
                        scale_y_continuous(labels = percent)+
                        theme_minimal()
                        #theme(axis.text.x = element_text(angle = 45))
                
                if(input$Forecast == TRUE){
                        p2 <- p2+
                                geom_line(data = buff_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                }
                
                p2
        })
        
        output$grapha3 <- renderPlot({
                
                #Denver Covid
                
                if(input$Forecast == TRUE){
                        denver_counties_ts <- as.ts(denver_counties['new_cases'])
                        denver_counties_AR <- auto.arima(denver_counties_ts)
                        denver_counties_for <- forecast(denver_counties_AR, h = 30)
                        denver_counties_model <- data.frame(Value=as.numeric(denver_counties_for$mean, check.names = FALSE))
                        denver_counties_model$date <- last(denver_counties$Date)+ 1 : 30
                }
                
                if(input$Historical == FALSE){
                        denver_counties <- slice(denver_counties, tail(row_number(), 120))
                }
                else{
                        denver_counties <- filter(denver_counties, Date > "2020-11-01")
                }
                
                p3 <- denver_counties %>%
                        mutate(all_week = floor_date(Date, "week")) %>%
                        filter(Admin2 == "Denver") %>% 
                        group_by(all_week) %>% 
                        summarize(avg_cases = mean(new_cases))%>% 
                        mutate(per_capita = avg_cases / (denver_pop/100000)) %>%
                        ggplot(mapping = aes(x = all_week, y = per_capita))+
                        geom_point()+
                        geom_line(alpha = 0.8, color = "blue")+
                        geom_hline(yintercept =  35, color = 'red')+
                        xlab("")+
                        ylab("Cases per 100K")+
                        labs(title = "Covid Cases in Denver", subtitle = "Aggregated by Week")+
                        scale_x_date(date_labels = "%b %Y")+
                        theme_minimal()

                if(input$Forecast == TRUE){
                        p3 <- p3+
                                geom_line(data = denver_counties_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                }
                
                p3
                
        })
        
        output$grapha4 <- renderPlot({
                
                #Real Residential Prices
                
                if(input$Forecast == TRUE){
                        HOME_ts <- as.ts(HOME['Real Residential Property Prices']/100, frequency = 4)
                        HOME_AR <- auto.arima(HOME_ts)
                        HOME_for <- forecast(HOME_AR, h = 9)
                        HOME_model <- data.frame(Value=as.numeric(HOME_for$mean, check.names = FALSE))
                        HOME_model$date <- last(HOME$date) %m+% months(seq(3,27, by = 3))
                }
                
                if(input$Historical == FALSE){
                        HOME <- slice(HOME, tail(row_number(), 8))
                }
                
                p4 <- HOME %>% 
                        ggplot(mapping = aes(x = date, y = `Real Residential Property Prices`/100))+
                        geom_bar(stat = "identity")+
                        scale_y_continuous(labels = percent)+
                        ylab("Property Price Change")+
                        xlab("")+
                        scale_x_date(date_labels = "%Y")+
                        labs(title = "Real Residential Property Prices Change", subtitle = "Quarterly Data")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p4 <- p4+
                                geom_bar(data = HOME_model, stat = "identity", aes(x = date, y = Value, fill = "red"))+
                                theme(legend.position = "none")
                }
                
                p4
                
        })
         
        
        output$grapha5 <- renderPlot({
                
                #Trade Balance
                
                if(input$Forecast == TRUE){
                        TradeBalance_ts <- as.ts(TradeBalance['Trade Balance'])
                        TradeBalance_AR <- auto.arima(TradeBalance_ts)
                        TradeBalance_for <- forecast(TradeBalance_AR, h = 4)
                        TradeBalance_model <- data.frame(Value=as.numeric(TradeBalance_for$mean, check.names = FALSE))
                        TradeBalance_model$date <- last(TradeBalance$date) %m+% months(0:3)
                }
                
                if(input$Historical == FALSE){
                        TradeBalance <- slice(TradeBalance, tail(row_number(), 24))
                }
                
                p5 <- TradeBalance %>% 
                        ggplot(mapping = aes(x = date, y = `Trade Balance`))+
                        geom_line()+
                        geom_hline(yintercept = 0, color = "red")+
                        ylab("Trade Balance")+
                        xlab("")+
                        labs(title = "US Trade Balance")+
                        scale_x_date(date_labels = "%b %Y")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p5 <- p5+
                                geom_line(data = TradeBalance_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                }
                
                p5
        })
        
        output$grapha6 <- renderPlot({
                
                #Mexican Peso value vs USD
                
                if(input$Forecast == TRUE){
                        MEXP_ts <- as.ts(MEXP[2])
                        MEXP_AR <- auto.arima(MEXP_ts)
                        MEXP_for <- forecast(MEXP_AR, h = 30)
                        MEXP_model <- data.frame(Value=as.numeric(MEXP_for$mean, check.names = FALSE))
                        MEXP_model$date <- last(MEXP$date) + 1 : 30
                }
                
                if(input$Historical == FALSE){
                        MEXP <- slice(MEXP, tail(row_number(), 90))
                }
                
                p6 <- MEXP %>% 
                        ggplot(mapping = aes(x = date, y = `MEX/USD Exchange Rate`))+
                        geom_line()+
                        ylab("Exchange Rate")+
                        xlab("")+
                        labs(title = "USD/MEX Exchange Rate")+
                        scale_x_date(date_labels = "%b %Y")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p6 <- p6+
                                geom_line(data = MEXP_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                }
                
                p6
                
        })
        
        output$grapha7 <- renderPlot({
                
                #Unemployment Rate
                
                if(input$Forecast == TRUE){
                        UNRATE_ts <- as.ts(UNRATE['Unemployment Rate']/100)
                        UNRATE_AR <- auto.arima(UNRATE_ts)
                        UNRATE_for <- forecast(UNRATE_AR, h = 4)
                        UNRATE_model <- data.frame(Value=as.numeric(UNRATE_for$mean, check.names = FALSE))
                        UNRATE_model$date <- last(UNRATE$date) %m+% months(0:3)
                }
                
                if(input$Historical == FALSE){
                        UNRATE <- slice(UNRATE, tail(row_number(), 24))
                }
                
                p7 <- UNRATE %>% 
                        mutate(r = `Unemployment Rate`/100) %>% 
                        ggplot(mapping = aes(x = date, y = r))+
                        geom_line()+
                        scale_y_continuous(labels = percent)+
                        ylab("")+
                        xlab("")+
                        labs(title = "Unemployment Rate")+
                        scale_x_date(date_labels = "%b %Y")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p7 <- p7+
                                geom_line(data = UNRATE_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                }
                
                p7
                
        })
        
        output$grapha8 <- renderPlot({
                
                #CPI
                
                if(input$Forecast == TRUE){
                        CPI_ts <- as.ts(CPI['CPI']/100)
                        CPI_AR <- auto.arima(CPI_ts)
                        CPI_for <- forecast(CPI_AR, h = 4)
                        CPI_model <- data.frame(Value=as.numeric(CPI_for$mean, check.names = FALSE))
                        CPI_model$date <- last(CPI$date) %m+% months(1:4)
                }
                
                if(input$Historical == FALSE){
                        CPI <- slice(CPI, tail(row_number(), 24))
                }
                
                p8 <- CPI %>% 
                        mutate(r = CPI/100) %>% 
                        ggplot(mapping = aes(x = date, y = r))+
                        geom_bar(stat = "identity")+
                        scale_y_continuous(labels = percent)+
                        ylab("Inflation Rate")+
                        xlab("")+
                        labs(title = "Median CPI", subtitle = "Percent Change annualized and Seasonally Adjusted")+
                        scale_x_date(date_labels = "%Y")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p8 <- p8+
                                geom_bar(data = CPI_model, stat = "identity", aes(x = date, y = Value, fill = "red"))+
                                theme(legend.position = "none")
                }
                
                p8
                
        })
        
        output$grapha9 <- renderPlot({
                
                #GDP
                
                if(input$Forecast == TRUE){
                        GDP_ts <- as.ts(GDP['GDP']/100)
                        GDP_AR <- auto.arima(GDP_ts)
                        GDP_for <- forecast(GDP_AR, h = 3)
                        GDP_model <- data.frame(Value=as.numeric(GDP_for$mean, check.names = FALSE), 
                                                lower = as.numeric(GDP_for$lower[,1]),
                                                upper = as.numeric(GDP_for$upper[,1]))
                        GDP_model$date <- last(GDP$date) %m+% months(seq(3,9, by = 3))
                        GDP_model[1:3] <- round(GDP_model[1:3], digits = 3)
                        
                } 
                
                if(input$Historical == FALSE){
                        GDP <- slice(GDP, tail(row_number(), 8))
                }
                
                p9 <- GDP %>% 
                        slice(tail(row_number(), 12)) %>% 
                        mutate(GDP = GDP/100) %>% 
                        ggplot(mapping = aes(x = date, y = GDP))+
                        geom_bar(stat = "identity")+
                        geom_text(aes(label = GDP*100, y = ifelse(GDP > 0, GDP + 0.05, GDP - 0.05)))+
                        scale_y_continuous(labels = percent)+
                        ylab("US GDP growth rate")+
                        xlab("")+
                        scale_x_date(date_labels = "%Y")+
                        labs(title = "GDP Growth Rate")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p9 <- p9+
                                geom_bar(data = GDP_model, stat = "identity", aes(x = date, y = Value, fill = "red"))+
                                geom_text(data = GDP_model, aes(label = Value*100, y = ifelse(Value > 0, Value + 0.05, Value - 0.05)))+
                                #geom_line(data = GDP_model, stat="smooth",method = "lm", formula = y ~ 0 + I(1/x) + I((x-1)/x), aes(x = date, y = lower), se = FALSE, alpha = 0.5, color = "blue")+
                                #geom_line(data = GDP_model, stat="smooth",method = "lm", formula = y ~ 0 + I(1/x) + I((x-1)/x), aes(x = date, y = upper), se = FALSE, alpha = 0.5, color = "blue")+
                                theme(legend.position = "none")
                                
                                
                }
                
                p9
        })
        
        #PINF <- Price Inflation by TOP 10 or 15 goods
        
        FFR <- get_fred_series("DFF", series_name = "FFR")
        INDPRO <- get_fred_series("INDPRO", series_name = "INDPRO")
        PPI <- get_fred_series("PPIACO", series_name = "PPI")
        TRE <- get_fred_series("T10Y2Y", series_name = "TRE")
        LABFOR <- get_fred_series("CIVPART", series_name = "LFP")
        VIX <- get_fred_series("VIXCLS", series_name = "VIX")
        PCON <- get_fred_series("DPCERE1Q156NBEA", series_name = "PCE")
        CRUDE <- get_fred_series("DCOILWTICO", series_name = "WTI")
        
        
        
        output$graphb1 <- renderPlot({
                
                
                #GDP
                
                if(input$Forecast == TRUE){
                        FFR_ts <- as.ts(FFR['FFR'])
                        FFR_AR <- auto.arima(FFR_ts)
                        FFR_for <- forecast(FFR_AR, h = 100)
                        FFR_model <- data.frame(Value=as.numeric(FFR_for$mean, check.names = FALSE), 
                                                lower = as.numeric(FFR_for$lower[,1]),
                                                upper = as.numeric(FFR_for$upper[,1]))
                        FFR_model$date <- last(FFR$date) + 0 : 99
                        
                } 
                
                if(input$Historical == FALSE){
                        FFR <- slice(FFR, tail(row_number(), 1000))
                }
                
                p10 <- FFR %>% 
                        ggplot(mapping = aes(x = date, y = FFR))+
                        geom_line()+
                        ylab("Rate")+
                        xlab("")+
                        scale_x_date(date_labels = "%b %Y")+
                        labs(title = "Federal Reserve Federal Funds Rate")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p10 <- p10+
                                geom_line(data = FFR_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p10
                
                
                })
        
        
        output$graphb2 <- renderPlot({
                
                #Industrial Production
                
                if(input$Forecast == TRUE){
                        INDPRO_ts <- as.ts(INDPRO['INDPRO'])
                        INDPRO_AR <- auto.arima(INDPRO_ts)
                        INDPRO_for <- forecast(INDPRO_AR, h = 6)
                        INDPRO_model <- data.frame(Value=as.numeric(INDPRO_for$mean, check.names = FALSE), 
                                                lower = as.numeric(INDPRO_for$lower[,1]),
                                                upper = as.numeric(INDPRO_for$upper[,1]))
                        INDPRO_model$date <- last(INDPRO$date) %m+% months(0:5)
                        
                } 
                
                if(input$Historical == FALSE){
                        INDPRO <- slice(INDPRO, tail(row_number(), 24))
                }
                
                p11 <- INDPRO %>% 
                        ggplot(mapping = aes(x = date, y = INDPRO))+
                        geom_line()+
                        ylab("Index")+
                        xlab("")+
                        scale_x_date(date_labels = "%b %Y")+
                        labs(title = "Industrial Production")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p11 <- p11+
                                geom_line(data = INDPRO_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p11
                
                })
        
        output$graphb3 <- renderPlot({
                
                #Producer Price Index
                
                if(input$Forecast == TRUE){
                        PPI_ts <- as.ts(PPI['PPI'])
                        PPI_AR <- auto.arima(PPI_ts)
                        PPI_for <- forecast(PPI_AR, h = 6)
                        PPI_model <- data.frame(Value=as.numeric(PPI_for$mean, check.names = FALSE), 
                                                   lower = as.numeric(PPI_for$lower[,1]),
                                                   upper = as.numeric(PPI_for$upper[,1]))
                        PPI_model$date <- last(PPI$date) %m+% months(0:5)
                        
                } 
                
                if(input$Historical == FALSE){
                        PPI <- slice(PPI, tail(row_number(), 24))
                }
                
                p12 <- PPI %>% 
                        ggplot(mapping = aes(x = date, y = PPI))+
                        geom_line()+
                        ylab("Index")+
                        xlab("")+
                        scale_x_date(date_labels = "%b %Y")+
                        labs(title = "Producer Price Index")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p12 <- p12+
                                geom_line(data = PPI_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p12
                
                
        })
        
        output$graphb4 <- renderPlot({
                
                #Treasuries
                
                if(input$Forecast == TRUE){
                        TRE_ts <- as.ts(TRE['TRE'])
                        TRE_AR <- auto.arima(TRE_ts)
                        TRE_for <- forecast(TRE_AR, h = 100)
                        TRE_model <- data.frame(Value=as.numeric(TRE_for$mean, check.names = FALSE), 
                                                lower = as.numeric(TRE_for$lower[,1]),
                                                upper = as.numeric(TRE_for$upper[,1]))
                        TRE_model$date <- last(TRE$date) + 0 : 99
                        
                } 
                
                if(input$Historical == FALSE){
                        TRE <- slice(TRE, tail(row_number(), 24))
                }
                
                p13 <- TRE %>% 
                        ggplot(mapping = aes(x = date, y = TRE))+
                        geom_line()+
                        ylab("Percent")+
                        xlab("")+
                        scale_x_date(date_labels = "%b %Y")+
                        labs(title = "10YR Treasury - 2YR Treasury")+
                        geom_hline(yintercept = 0, color = "red")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p13 <- p13+
                                geom_line(data = TRE_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p13
                
        })
        output$graphb5 <- renderPlot({
                
                #Labor Force
                
                if(input$Forecast == TRUE){
                        LABFOR_ts <- as.ts(LABFOR['LFP'])
                        LABFOR_AR <- auto.arima(LABFOR_ts)
                        LABFOR_for <- forecast(LABFOR_AR, h = 6)
                        LABFOR_model <- data.frame(Value=as.numeric(LABFOR_for$mean, check.names = FALSE), 
                                                lower = as.numeric(LABFOR_for$lower[,1]),
                                                upper = as.numeric(LABFOR_for$upper[,1]))
                        LABFOR_model$date <- last(LABFOR$date) %m+% months(0:5)
                        
                } 
                
                if(input$Historical == FALSE){
                        LABFOR <- slice(LABFOR, tail(row_number(), 24))
                }
                
                p14 <- LABFOR %>% 
                        ggplot(mapping = aes(x = date, y = LFP))+
                        geom_line()+
                        ylab("Participation Rate")+
                        xlab("")+
                        scale_x_date(date_labels = "%b %Y")+
                        labs(title = "Labor Force Participation")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p14 <- p14+
                                geom_line(data = LABFOR_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p14
        })
        output$graphb6 <- renderPlot({
                
                #VIX
                
                if(input$Forecast == TRUE){
                        VIX_ts <- as.ts(VIX['VIX'])
                        VIX_AR <- auto.arima(VIX_ts)
                        VIX_for <- forecast(VIX_AR, h = 100)
                        VIX_model <- data.frame(Value=as.numeric(VIX_for$mean, check.names = FALSE), 
                                                   lower = as.numeric(VIX_for$lower[,1]),
                                                   upper = as.numeric(VIX_for$upper[,1]))
                        VIX_model$date <- last(VIX$date) + 0 : 99
                        
                } 
                
                if(input$Historical == FALSE){
                        VIX <- slice(VIX, tail(row_number(), 24))
                }
                
                p15 <- VIX %>% 
                        ggplot(mapping = aes(x = date, y = VIX))+
                        geom_line()+
                        ylab("Index")+
                        xlab("")+
                        scale_x_date(date_labels = "%b %Y")+
                        labs(title = "VIX")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p15 <- p15+
                                geom_line(data = VIX_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p15
        })
        output$graphb7 <- renderPlot({
                
                #Personal Consumption  Expenditure
                
                if(input$Forecast == TRUE){
                        PCON_ts <- as.ts(PCON['PCE'])
                        PCON_AR <- auto.arima(PCON_ts)
                        PCON_for <- forecast(PCON_AR, h = 6)
                        PCON_model <- data.frame(Value=as.numeric(PCON_for$mean, check.names = FALSE), 
                                                lower = as.numeric(PCON_for$lower[,1]),
                                                upper = as.numeric(PCON_for$upper[,1]))
                        PCON_model$date <- last(PCON$date) %m+% months(0:5)
                        
                } 
                
                if(input$Historical == FALSE){
                        PCON <- slice(PCON, tail(row_number(), 24))
                }
                
                p16 <- PCON %>% 
                        ggplot(mapping = aes(x = date, y = PCE))+
                        geom_line()+
                        ylab("Index")+
                        xlab("")+
                        scale_x_date(date_labels = "%b %Y")+
                        labs(title = "Personal Consumption Expenditure")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p16 <- p16+
                                geom_line(data = PCON_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p16
                
        })
        output$graphb8 <- renderPlot({
                
                #Crude Oil WTI
                
                if(input$Forecast == TRUE){
                        CRUDE_ts <- as.ts(CRUDE['WTI'])
                        CRUDE_AR <- auto.arima(CRUDE_ts)
                        CRUDE_for <- forecast(CRUDE_AR, h = 100)
                        CRUDE_model <- data.frame(Value=as.numeric(CRUDE_for$mean, check.names = FALSE), 
                                                 lower = as.numeric(CRUDE_for$lower[,1]),
                                                 upper = as.numeric(CRUDE_for$upper[,1]))
                        CRUDE_model$date <- last(CRUDE$date) + 0 : 99
                        
                } 
                
                if(input$Historical == FALSE){
                        CRUDE <- slice(CRUDE, tail(row_number(), 24))
                }
                
                p17 <- CRUDE %>% 
                        ggplot(mapping = aes(x = date, y = WTI))+
                        geom_line()+
                        ylab("Price")+
                        xlab("")+
                        scale_x_date(date_labels = "%b %Y")+
                        labs(title = "Crude Oil (WTI)")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p17 <- p17+
                                geom_line(data = CRUDE_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p17
                
        })
    
        carbon <- get_carbon()
        emissions <- get_emissions()
        
        emissions <- emissions %>% 
                filter(iso_code == "USA")
                
        
        hurr <- get_hurricanes()
        sealevel <- get_sealevel()
        temps <- get_temp()
        
        temps <- temps %>% 
                select(-c(MAM, DJF, `J-D`, `D-N`, JJA, SON)) %>% 
                gather(key = month, anomaly, 2:13 ) %>% 
                mutate(yr = substr(Year, 1,4)) %>% 
                mutate(date = paste(1, month, yr, sep = " ")) %>% 
                mutate(date = dmy(date))
        
        seaice <- get_seaice()

        
        output$graphc1 <- renderPlot({
                
                #CO2 global
                
                if(input$Forecast == TRUE){
                        carbon_ts <- as.ts(carbon['trend'])
                        carbon_AR <- auto.arima(carbon_ts)
                        carbon_for <- forecast(carbon_AR, h = 12)
                        carbon_model <- data.frame(Value=as.numeric(carbon_for$mean, check.names = FALSE), 
                                                  lower = as.numeric(carbon_for$lower[,1]),
                                                  upper = as.numeric(carbon_for$upper[,1]))
                        carbon_model$date <- last(carbon$date) %m+% months(0:11)
                        
                } 
                
                if(input$Historical == FALSE){
                        carbon <- slice(carbon, tail(row_number(), 24))
                }
                
                p18 <- carbon %>% 
                        ggplot(mapping = aes(x = date, y = trend))+
                        geom_line()+
                        ylab("ppm")+
                        xlab("")+
                        scale_x_date(date_labels = "%b %Y")+
                        labs(title = "Global CO2 ppm")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p19 <- p19+
                                geom_line(data = carbon_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p18
                
                
                
        })
        
        output$graphc2 <- renderPlot({
                
                #USA Emissions
                
                if(input$Forecast == TRUE){
                        emissions_ts <- as.ts(emissions['co2'])
                        emissions_AR <- auto.arima(emissions_ts)
                        emissions_for <- forecast(emissions_AR, h = 4)
                        emissions_model <- data.frame(Value=as.numeric(emissions_for$mean, check.names = FALSE), 
                                                   lower = as.numeric(emissions_for$lower[,1]),
                                                   upper = as.numeric(emissions_for$upper[,1]))
                        emissions_model$year <- last(emissions$year) + 0:3
                        
                } 
                
                if(input$Historical == FALSE){
                        emissions <- slice(emissions, tail(row_number(), 24))
                }
                
                p19 <- emissions %>% 
                        ggplot(mapping = aes(x = year, y = co2))+
                        geom_line()+
                        ylab("Gigatons")+
                        xlab("")+
                        #scale_x_date(date_labels = "%Y")+
                        labs(title = "US CO2 Total Emissions")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p19 <- p19+
                                geom_line(data = emissions_model, aes(x = year, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p19
                
                
        })
        
        output$graphc3 <- renderPlot({
                
                #Hurricanes
                
                if(input$Forecast == TRUE){
                        hurr_ts <- as.ts(hurr['RevisedACE'])
                        hurr_AR <- auto.arima(hurr_ts)
                        hurr_for <- forecast(hurr_AR, h = 7)
                        hurr_model <- data.frame(Value=as.numeric(hurr_for$mean, check.names = FALSE), 
                                                      lower = as.numeric(hurr_for$lower[,1]),
                                                      upper = as.numeric(hurr_for$upper[,1]))
                        hurr_model$date <- last(hurr$Year) + 0:6
                        
                } 
                
                if(input$Historical == FALSE){
                        hurr <- slice(hurr, tail(row_number(), 24))
                }
                
                p20 <- hurr %>% 
                        ggplot(mapping = aes(x = Year, y = RevisedACE))+
                        geom_line()+
                        ylab("Gigatons")+
                        xlab("")+
                        #scale_x_date(date_labels = "%Y")+
                        labs(title = "North American Hurricane Basin", subtitle = "Accumulated Cyclon Energy (ACE)")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p20 <- p20+
                                geom_line(data = hurr_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p20
                
        })
        output$graphc4 <- renderPlot({
                
                #Sealevels
                
                if(input$Forecast == TRUE){
                        sealevel_ts <- as.ts(sealevel['gmsl'])
                        sealevel_AR <- auto.arima(sealevel_ts)
                        sealevel_for <- forecast(sealevel_AR, h = 6)
                        sealevel_model <- data.frame(Value=as.numeric(sealevel_for$mean, check.names = FALSE), 
                                                 lower = as.numeric(sealevel_for$lower[,1]),
                                                 upper = as.numeric(sealevel_for$upper[,1]))
                        sealevel_model$date <- last(sealevel$date) + 0:5
                        
                } 
                
                if(input$Historical == FALSE){
                        sealevel <- slice(sealevel, tail(row_number(), 24))
                }
                
                p21 <- sealevel %>% 
                        ggplot(mapping = aes(x = date, y = gmsl))+
                        geom_line()+
                        ylab("Global Mean Sea Level")+
                        xlab("")+
                        scale_x_date(date_labels = "%Y")+
                        labs(title = "Global Mean Sea Levels")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p21 <- p21+
                                geom_line(data = sealevel_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p21
                
                
        })
        
        
        output$graphc5 <- renderPlot({
                
                #Temperature Anomalies
                
                if(input$Forecast == TRUE){
                        temps_ts <- as.ts(temps['anomaly'])
                        temps_AR <- auto.arima(temps_ts)
                        temps_for <- forecast(temps_AR, h = 6)
                        temps_model <- data.frame(Value=as.numeric(temps_for$mean, check.names = FALSE), 
                                                     lower = as.numeric(temps_for$lower[,1]),
                                                     upper = as.numeric(temps_for$upper[,1]))
                        temps_model$date <- last(temps$date) + 0:5
                        
                } 
                
                if(input$Historical == FALSE){
                        temps <- slice(temps, tail(row_number(), 24))
                }
                
                p22 <- temps %>% 
                        ggplot(mapping = aes(x = date, y = anomaly))+
                        geom_line()+
                        ylab("Temperature Anomaly")+
                        xlab("")+
                        scale_x_date(date_labels = "%Y")+
                        labs(title = "Global Temperature Anomaly")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p22 <- p22+
                                geom_line(data = temps_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p22
                
        })
        
        output$graphc6 <- renderPlot({
                
                #Seaice
                
                if(input$Forecast == TRUE){
                        seaice_ts <- as.ts(seaice['extent'])
                        seaice_AR <- auto.arima(seaice_ts)
                        seaice_for <- forecast(seaice_AR, h = 6)
                        seaice_model <- data.frame(Value=as.numeric(seaice_for$mean, check.names = FALSE), 
                                                  lower = as.numeric(seaice_for$lower[,1]),
                                                  upper = as.numeric(seaice_for$upper[,1]))
                        seaice_model$date <- last(seaice$date) + 0:5
                        
                } 
                
                if(input$Historical == FALSE){
                        seaice <- slice(seaice, tail(row_number(), 24))
                }
                
                p23 <- seaice %>% 
                        ggplot(mapping = aes(x = date, y = extent))+
                        geom_line()+
                        ylab("Sea Ice")+
                        xlab("")+
                        scale_x_date(date_labels = "%Y")+
                        labs(title = "Sea Ice Level")+
                        theme_minimal()
                
                if(input$Forecast == TRUE){
                        p23 <- p23+
                                geom_line(data = seaice_model, aes(x = date, y = Value, color = "red"))+
                                theme(legend.position = "none")
                        
                }
                
                p23
                
        })
        
        output$graphc7 <- renderPlot({plot_paleo()})
        
        output$graphc8 <- renderPlot({warming_stripes()})
        
}


shinyApp(ui, server)
