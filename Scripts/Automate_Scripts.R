library(taskscheduleR)

# Schedule Data Scraping
taskscheduler_create(taskname = "Scrape_UFC_Data",
                     rscript = "E:/School/R Work/UFC-Sports-Betting-Model/Scripts/Data_Scraping.R",
                     schedule = "WEEKLY",
                     starttime = "09:00",
                     startdate = "03/15/2021",
                     days = "MON")
# Schedule Data Cleaning
taskscheduler_create(taskname = "Clean_UFC_Data",
                     rscript = "E:/School/R Work/UFC-Sports-Betting-Model/Scripts/Data_Cleaning.R",
                     schedule = "WEEKLY",
                     starttime = "09:00",
                     startdate = "03/15/2021",
                     days = "MON")

# Schedule Future Card Predictions
taskscheduler_create(taskname = "Predict_UFC_Data",
                     rscript = "E:/School/R Work/UFC-Sports-Betting-Model/Scripts/Future_Card.R",
                     schedule = "WEEKLY",
                     starttime = "09:00",
                     startdate = "03/15/2021",
                     days = "MON")

# Schedule Plots
taskscheduler_create(taskname = "Plot_UFC_Data",
                     rscript = "E:/School/R Work/UFC-Sports-Betting-Model/Scripts/README_Plots.R",
                     schedule = "WEEKLY",
                     starttime = "09:00",
                     startdate = "03/15/2021",
                     days = "MON")

taskscheduler_delete(taskname = "Scrape_UFC_Data")
taskscheduler_delete(taskname = "Clean_UFC_Data")
taskscheduler_delete(taskname = "Predict_UFC_Data")
taskscheduler_delete(taskname = "Plot_UFC_Data")