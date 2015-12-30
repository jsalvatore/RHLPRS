#############################################################################################################################
# googleAnalyticsDataGrabber.R
#
# Google Analytics only allows pulls of 90 days at a time - this function helps shorten the time and
# to grab more than 90 days data
#
# Joe Salvatore
# www.jsalvatore.com
# 2015
#############################################################################################################################

require("RGoogleAnalytics")

# create an iterator in multiple of 90...
iterator <- rev(seq(0,540, by = 90))

# blank df
ga.data.final <- data.frame()

# grab data
for (i in 1:length(iterator)) {
  start.date <- Sys.Date() - iterator[i] + 1
  end.date <- Sys.Date() - iterator[(i + 1)]
  
  query.list <- Init(
    start.date = start.date,
    end.date = end.date,
    dimensions = "ga:date, ga:medium",
    metrics = "ga:sessions, ga:newUsers",
    max.results = 10000,
    sort = "-ga:date",
    table.id = "ga:77093466"
  )
  
  
  # Create the Query Builder object so that the query parameters are validated
  ga.query <- QueryBuilder(query.list)
  
  
  
  # Extract the data and store it in a data-frame
  ga.data <- GetReportData(ga.query, token, split_daywise = T)
  
  
  #  A helper funciton to make sense of the random mediums returned from the GA reporting API
  defaultChannelGrouping <- function(x) {
    ifelse(x == "(none)", "Direct",
           ifelse(
             x == "affiliate", "Affiliate",
             ifelse(
               x == "cpc", "Paid Search",
               ifelse(
                 x == "display", "Display",
                 ifelse(
                   x == "email", "Email",
                   ifelse(
                     x == "organic", "Organic Search",
                     ifelse(
                       x == "referral", "Referral",
                       ifelse(
                         x == "sms", "SMS Text",
                         ifelse(x == "social", "Social", "Other")
                       )
                     )
                   )
                 )
               )
             )
           ))
    
  }
  
  # apply the previous function
  ga.data$medium <- defaultChannelGrouping(ga.data$medium)
  
  ga.data.final <- rbind(ga.data.final, ga.data)
  
}