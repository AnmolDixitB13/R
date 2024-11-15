# Accept employee name, monthly salary, year, month, leaves ?
# Calculate annual ctc & monthly salary after leave deduction, ppf deduction and tax deduction
# display the results

#____________________________________________________________________________________________________

#------------ Functions Section / All the necessary functions are defined here ------------

# advice for R programming
# prefer using multiple if statements or ifelse() instead of if-else because else often throws
error in if - else 

daysInMonth <- function(month)
{
  if(tolower(month)=='jan' | tolower(month)== 'january' | tolower(month)=='mar' | 
tolower(month)== 'march' |
  tolower(month)=='may' | tolower(month)== 'jul' | tolower(month)=='july' | tolower(month)== 'aug' 
| tolower(month)=='august'
   | tolower(month)== 'oct' | tolower(month)=='october' | tolower(month)== 'dec' | tolower(month)=='december'){
    return (31L) ;}

  if(tolower(month)=='apr' | tolower(month)== 'april' | tolower(month)=='jun' | 
tolower(month)== 'june' | tolower(month)=='sep'
  | tolower(month)== 'september' | tolower(month)=='nov' | tolower(month)== 'november'){
    return (30L) ;}

  return (0) ; # in case of typing errors in the name of the month
}

leap <- function(year){
  if (year%%4 != 0){
   #print("Year ", year, " is not a leap year. (Code executed from first if statement)")
   return(28L) ; }

  else if (year%%100!=0){
   #print("Year ", year, " is a leap year. (Code executed from first elif statement)")
   return(29L) ; }

  else if (year%%100 == 0 & year%%400 == 0){
    #print("Year ", year, " is a leap year. (Code executed from second elif statement)")
    return(29L) ; }
  else{
    #print("Year ", year, " is not a leap year.")
    return(28L) ; }
}


leaveDeduction <- function(monthly_salary, leave_days, total_days_in_month)
{
oneDaySal = monthly_salary/total_days_in_month
leaveDaysSalDeduction = oneDaySal*leave_days

return (leaveDaysSalDeduction) ;
}

regularDeductions <- function(monthly_salary, leave_days, leaveDaysSalDeduction, leaves)
  {
    ppfPer = 3
    annualCTC <- monthly_salary*12
    newMonthlySal <- monthly_salary - leaveDaysSalDeduction
    ppfDeduction = (ppfPer*monthly_salary)/100


    if(monthly_salary <= 200000){
      taxPer <- 2
    }
    else if(monthly_salary > 200000 & monthly_salary <= 500000){
      taxPer <- 8
    }
    else{
      taxPer <- 10
    }

    taxDeduction = (newMonthlySal*taxPer)/100

    finalMonthlySalAfterAllDeductions = newMonthlySal - ppfDeduction - taxDeduction

    print("\n\n")
    print(paste0("-------------------Salary Slip for ", year, " ", month, "-------------------"))
    print(paste0("Employee Name: ", name))
    print(paste0("Monthly Salary: ", monthly_salary))
    print(paste0("Annual CTC: ", annualCTC))
    print(paste0("Leaves Taken: ", toupper(leaves)))

    if(tolower(leaves) == 'y'){
      print(paste0("Leave Days: ", leave_days))  }

    if(tolower(leaves)=='n'){
       print("Leave Days: --") }

    if (leaveDaysSalDeduction !=0) {
       print(paste0("Leave Deduction: ", leaveDaysSalDeduction)) }

    if(leaveDaysSalDeduction==0) {
      print(paste0("Leave Deduction: --")) }

    print(paste0("PF %: ", ppfPer))
    print(paste0("PF Deduction: ", ppfDeduction))
    print(paste0("Tax %: ", taxPer))
    print(paste0("Tax Deduction: ", taxDeduction))
    print(paste0("Final Monthly Salary: ", finalMonthlySalAfterAllDeductions))
    #name, month, year, monthly, annual, leaves, leavededuction, ppfper, ppfded, taxper, taxded, final

  }

#--------------------------- I/P Section --------------------------

while(TRUE){
  name <- readline("Enter the employee name:\t")

  if (!is.na(as.numeric(name))){ print("Name can't be purely numerical. Re - enter the name correctly.")
    next
  }

  else if(nchar(name) == 0){ print("Name can't be empty. Enter the name of the employee.")
  next
  }

  else{
  break
  }

} # end of while ()

while(1){monthly_salary <- as.numeric(readline(paste0("Enter the monthly salary of the employee ", name, " :\t")))

if(monthly_salary < 0){
  print("Salary can't be nagative. Re - enter the salary correctly.")
  next
}
else{
  break
}
}

year <- as.integer(readline(paste0("Enter the year for which the monthly salary of ", name, " has to be calculated:\t")))
month <- readline(paste0("Enter the month of the year ", year, " for which the monthly salary of ", name, " has to be calculated (jan/january...):\t"))
# typeof(year)

while (1){
leaves <- as.character(readline(paste0("Did the employee ", name," take any leave(s) in the month ", month, " and the year ", year, " ? Enter Y/N or y/n:\t")))
if(tolower(leaves) == 'y'| tolower(leaves) == 'n'){
  break
}else{
  print("Invalid option entered. Re enter Y/N or y/n suitably.")
  next
}
}

if(tolower(leaves)=='y'){

  if(tolower(month)=='feb' | tolower(month)=='february'){
    total_days_in_month <- leap(year)
  }

  else{
    total_days_in_month <- daysInMonth(month)
  }

  while (1){
  leave_days <- as.integer(readline("Enter the no. of leaves for the employee for this period:\t"))
  if(leave_days >= 0 & leave_days < total_days_in_month){
    break
  }
  else if(leave_days < 0){
    print("Leave days can't be negative. Re enter the leave days correctly.")
    next
  }

  else if(leave_days > total_days_in_month){
    print("Leave days can't be greater than no. of days in the month. Re enter the leave days correctly.")
    next
  }else{
  break}
}
# end of while() loop
}# end of if statement

#using else here throws error, so I used if
if(tolower(leaves)=='n') {
  leave_days <- 0
}

#---------------- Calling the suitable functions -----------------

# Here, while calling the suitable functions, I had previously used if-else, but since else was
# throwing error, I then used 2 separate if statements instead of if-else
  
if(leave_days!=0){
  leaveDaysSalDeduction <- leaveDeduction(monthly_salary, leave_days, total_days_in_month)
  print("executed here")}
  regularDeductions(monthly_salary, leave_days, leaveDaysSalDeduction, leaves)


if (leave_days==0) {
  leaveDaysSalDeduction <- 0
  regularDeductions(monthly_salary, leave_days, leaveDaysSalDeduction, leaves)
  }
