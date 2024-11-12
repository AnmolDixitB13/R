# matrix
# marks of 4 students in 3 subjects, find total, percent, display entire record together

stud_names <- c('abc', 'def', 'ghi', 'jkl')
sub_names <- c('A', 'B', 'C')
scores <- c(85, 57, 52, 23, 23, 41, 98, 80, 76, 35, 87, 68)

marks <- matrix(scores, nrow <- length(stud_names), ncol = length(sub_names), dimnames = list(stud_names, sub_names), byrow = TRUE)
# do not use <- with nrow, ncol, dimnames because it throws error. Prefer using = as assignment operator.

print(marks)


#sum(marks[1,]) # finds sum of the elements of the 1st row

# nrow(marks) # 4
# ncol(marks) # 3
# dimnames(marks) # 'abc''def''ghi''jkl', 'A''B''C'

total <- c()
percent <- c()

#################################   Totalling marks & finding %    ####################################
for(i in 1:nrow(marks)){
# append(total, sum(marks[i,])) # produces null o/p
temp_sum <- sum(marks[i,])

total[i] <- temp_sum
percent[i] <- (temp_sum/300)*100
}

mat_tot <- matrix(total, nrow = length(stud_names), byrow = FALSE)
mat_per <- matrix(percent, nrow = length(stud_names), byrow = FALSE)

colnames(mat_tot) = c("Total Marks")
colnames(mat_per) = "Percent"

#################################   Subject Wise Grading    ####################################

mat_grade <- matrix("N", nrow = length(stud_names), ncol = length(sub_names))
colnames(mat_grade) = c("Sub1", "Sub2", "Sub3")

# colnames_mat_grade <- c()
# for(i in 1:length(sub_names)){
# colnames_mat_grade[i] <- sprintf("Sub %d", i)}

#mat_grade <- matrix("N", nrow = length(stud_names), ncol = length(sub_names), dimnames = list(colnames_mat_grade))

for(i in 1:nrow(marks)){
  for(j in 1:ncol(marks)){
    ifelse(marks[i, j] > 80 & marks[i, j] <= 100, mat_grade[i, j] <- 'A', ifelse(marks[i, j] > 60 & marks[i, j] <= 80, mat_grade[i, j] <- 'B', ifelse(marks[i, j] > 40 & marks[i, j] <= 60, mat_grade[i, j] <- 'C', ifelse(marks[i, j] > 30 & marks[i, j] <= 40, mat_grade[i, j] <- 'D', mat_grade[i, j] <- 'F'))))
  }
}
# mat_grade[i, j] = 'A' throws error ' unexpected = ', use <- instead of =

print(mat_grade)



###########################################   Overall Percent & Overall Pass Status  ###############################################

# overall_pass_status <- c("")
mat_overall_pass_status <- matrix("PStat", nrow = nrow(marks), byrow = FALSE)

mat_overall_grade <- matrix("N", nrow = nrow(marks), ncol = 1)

for(i in 1:nrow(marks)){
  all_pass  = TRUE
  for(j in 1:ncol(marks)){
  if(mat_grade[i, j]=='F'){
    mat_overall_grade[i] <- 'F'
    mat_overall_pass_status[i] <- 'F'
    all_pass  = FALSE
  }
  if(all_pass){
    mat_overall_pass_status[i] <- 'P'
    ifelse(mat_per[i] > 80 & mat_per[i] <= 100, mat_overall_grade[i] <- 'A', ifelse(mat_per[i] > 60 & mat_per[i] <= 80, mat_overall_grade[i] <- 'B', ifelse(mat_per[i] > 40 & mat_per[i] <= 60, mat_overall_grade[i] <- 'C', ifelse(mat_per[i] > 30 & mat_per[i] <= 40, mat_overall_grade[i] <- 'D', mat_overall_grade[i] <- 'F'))))
}}}

colnames(mat_overall_grade) = c("Overall Grade")
colnames(mat_overall_pass_status) = c("Overall Pass Status")

# if F in subject, overall grade = F, else compare & assign

# if colnames = c("Percent") was written within matrix() we would get error unused arguments

# print(total)
# print(percent)

print("----------------------------------------------------------------")
print(cbind(marks, mat_grade, mat_tot, mat_per, mat_overall_grade, mat_overall_pass_status))
