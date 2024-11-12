# Create a separate CSV file for each continent, which is mentioned in the dataset file. The CSV
# file that will be generated, should contain statistics of that respective continent i.e. it should
# contain the following details of the continent:
#
# 1. Total no. of countries from that continent whose data is present in the dataset file
# 2. Countries whose HDI improved in the year 2022 w.r.t. the year 2021
# 3. Countries whose HDI deteriorated in the year 2022 w.r.t. the year 2021
# 4. Countries whose HDI remained constant in the year 2022 w.r.t. the year 2021


data <- read.csv('cleaned_hdi_with_calculations.csv')
# str(data)

# find out all the continents present in the dataset file using factors()

fac_cont <- factor(data$Continent)
cont_list <- c(levels(fac_cont))
# print(cont_list)
# print(length(cont_list))  # 7

# creating a vector to store the count values i.e. values/counts of countries of each
# continent whose hdi has improved/deterioraated/remained constant

count <- c(0, 0, 0)   # creating a counter i.e. a vector that will act as a counter

# filtering data
for(i in 1:length(cont_list)){
  filtered_data <- subset(data, data$Continent == cont_list[i])
  count <- c(0, 0, 0)   # reinitializing counter to 0

# counting statistics
  for(j in 1:nrow(filtered_data)){

    total_countries_on_continent <- nrow(filtered_data)

    if(filtered_data$Change_HDI_2022_21[j] > 0){
      count[1] <- count[1] + 1
    }
    if(filtered_data$Change_HDI_2022_21[j] < 0){
      count[2] <- count[2] + 1
    }
    if(filtered_data$Change_HDI_2022_21[j] == 0){
      count[3] <- count[3] + 1
    }
  }
# transferring the data to a separate CSV file
# print(count)

col_names = c("Total Countries on the Continent (which are present in the dataset)", "Countries with improved HDI in 2022", "Countries with deteriorated HDI in 2022", "Countries whose HDI remained constant in 2022")
file_data <- c(total_countries_on_continent, count[1], count[2], count[3])

df <- data.frame(col_names, file_data)
write.csv(df, paste0("Continent_", cont_list[i], ".csv"))
}
