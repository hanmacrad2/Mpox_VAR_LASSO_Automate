#**********************************
#DATA 
#*********************************
#LIBRARIRES
library(VARLASSOMpox)
FOLDER = "C:/Users/h2cra/OneDrive/Documents/GitHub/Mpox_VAR_LASSO_Automate/"
source(paste0(FOLDER, 'libs_required.R'))

#DATA FOLDER 
DATA_FOLDER <- "C:/Users/h2cra/OneDrive/Documents/UCSD/PROJECTS/Project_2_Mpox/Data/DATA_GOLDEN_CDC/"
file_name = 'data_mpox.xlsx' 
data_mpox = readxl::read_excel(path = paste0(DATA_FOLDER, file_name))


#*********************************************************
#2. DATA ORIGINAL (NEEDED FOR PLOTTING FORECASTS)
list_jur = c("NewYorkCity", "Texas", "LA", "Florida", "Illinois", "Georgia",
             "SanDiego", "Washington")

#list_jur = c("NewYorkCity", "Texas", "LA", "Florida", "Illinois", "Georgia",
#             "SanDiego", "Washington", "NewJersey")

list_jur = c("NewYorkCity", "Texas", "LA", "Florida", "Illinois", "Georgia",
             "SanDiego", "Washington", "NewJersey", "Colorado")

#list_jur = c("NewYorkCity", "Texas", "LA", "Florida", "Illinois", "Georgia",
#             "SanDiego", "Washington", "NewJersey", "Colorado", "NorthCarolina", "Arizona")

data_mpox_model = data_mpox %>% filter(Jurisdiction %in% list_jur)
length(unique(data_mpox_model$Jurisdiction))


#*********************************************************
#3.FORMAT DATA FOR MODEL
df_smooth = GET_SMOOTH_DATA(data_mpox)

#*FILTER PERIOD & JURISDICTIONS OF INTEREST
df_model_smooth = df_smooth %>% filter(Jurisdiction %in% list_jur)
df_model_smooth = df_model_smooth %>% filter(Year >= START_YEAR_INTEREST)


#**********************************************************
#* 4. TRAIN TEST SPLIT FOR MODEL FITTING

#i.SELECT TRAINING END WEEK
TRAIN_WEEK = 84 #end of 2023
print(paste0('TRAIN_WEEK: ', TRAIN_WEEK))

#TRAIN_WEEK = 135 #end of 2024
#print(paste0('TRAIN_WEEK: ', TRAIN_WEEK))

#ii. SPLIT into train & test sets 
list_data = get_train_test_data(data_mpox_model, TRAIN_WEEK)
train_data = list_data$train_data
future_data = list_data$future_data

#iii. FORMAT split data into long data (jurisdictions == columns, rows == time)
train_data_ts = GET_TS_DATA(train_data)
future_data_ts = GET_TS_DATA(future_data)
data_ts = GET_TS_DATA(data_mpox_model)

#iv. FORMAT DATA FOR PLOT
data_model_24 = data_mpox_model %>% filter(Year == '2024')
data_ts_24 = GET_TS_DATA(data_model_24)

#WEEK NUMBER
WEEK_FORECAST = TRAIN_WEEK + 2
#WEEK_END = 169

#WEEK_END = 135
WEEK_END = 130
data_24_forecast_start = data_mpox_model %>% filter(Week_Number >= WEEK_FORECAST)
data_24_forecast_start = data_24_forecast_start %>% filter(Week_Number <= WEEK_END)
data_24_ts_forecast_start = GET_TS_DATA(data_24_forecast_start)

#**************************************
#* SMOOTH DATA FOR MODEL

#i. TEST-TRAIN SPLIT
list_data_smooth = get_train_test_data(df_model_smooth, TRAIN_WEEK)
train_data_smooth = list_data_smooth$train_data
future_data_smooth = list_data_smooth$future_data

#ii. FORMAT TIME-SERIES DATA -> INTO LONG DATA (jurisdictions == columns, rows == time)
train_data_ts_smooth = GET_TS_DATA(train_data_smooth)
future_data_ts_smooth = GET_TS_DATA(future_data_smooth)
data_tot_ts_smooth = GET_TS_DATA(df_model_smooth)

#PLOT 2023 & 2024 Data 
data_ts_23_24 = data_ts %>% filter(Week_Number > 32)


