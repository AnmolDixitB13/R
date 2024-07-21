# application of bar plots (sub divided bar plots)
# matrix or array -> bar plot

# A csv file contains a list of employees of 5 companies & the schemes they have chosen. There are 3 schemes - S1, S2 & S3
# & an employee from an organization can choose only 1 scheme. Represent the info in the form of bar plots (with company
# wise distrinution plot & scheme wise distribution plot). 

data <- read.csv('pie_bar_graph_dataset.csv')
# data

#############  Find out the no. of companies & the schemes present in the dataset file  ###########

fac_companies <- levels(factor(data$Company_Name))
fac_schemes <- levels(factor(data$Scheme_Opted))

n_companies <- length(fac_companies)
n_schemes <- length(fac_schemes)

fac_companies  # 'A''B''C''D''E'
fac_schemes    # 'S1''S2''S3'

n_companies   # 5
n_schemes     # 3


########################    Segregating data into a matrix/array    ############################

# count <- matrix(0, nrow = n_companies, ncol = n_schemes, dimnames = list(c("A", "B", "C", "D", "E"), c("S1", "S2", "S3"))) 

count <- array(0, dim = c(n_companies, n_schemes), dimnames = list(c("A", "B", "C", "D", "E"), c("S1", "S2", "S3"))) 
#     count[i, j] <- nrow(filtered_data) 

# works well with both matrix & array

for(i in 1:n_companies){
  for(j in 1:n_schemes){
    filtered_data <- subset(data, data$Company_Name == fac_companies[i] & data$Scheme_Opted == fac_schemes[j])
    # print(filtered_data)
    count[i, j] <- nrow(filtered_data)
  }
}

###############################    Displaying the data graphically    #################################

count # Display the resulting array

colours = c("red", "cyan", "blue", "green", "magenta")
barplot(count, xlab = 'Scheme', ylab = 'Quantity', main = 'Scheme Distribution among Companies', beside = FALSE, col = colours)
legend('topright', fac_companies, fill = colours)
# working fine

barplot(t(count), xlab = 'Company', ylab = 'Scheme Quantity', main = 'Company Distribution among Schemes', beside = FALSE, col = colours)
legend('topright', fac_companies, fill = colours)

