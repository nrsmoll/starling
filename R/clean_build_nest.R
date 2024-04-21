# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#' @description Clean infection data, to prepare for data-linkage
#'
#' @param data The dataset (make sure dates are in date format).
#' @param id The dataset (make sure dates are in date format).
#' @param diagnosis The column with the diagnosis listed
#' @param firstname First Name. If there is a middle name, it will be removed during cleaning.
#' @param surname Last name. All non-alphanumeric characters will be removed and everything becomes lower case
#' @param dob The date of birth (make sure dates are in date format).
#' @param age Age, if it has been pre-specified in the dataset.
#' @param medicare Medicare number. A medicare number with 10 and 11 numbers will have been created
#' @param postcode Post code, no restriction on the number of digits
#' @param fn First Nations Status
#' @param lattitude Lattitude, as numeric
#' @param longitude Longitude, as numeric
#' @param onset_date Onset date of the illness. Commonly the date of diagnosis (date the lab test was done), but can be the date of first symptom. Must be in date format.
#' @param admission_date Admission date. Typically, this should be later than the date of onset, but there are times when the disease is diagnosed in hospital.
#' @param icu_date ICU date. Typically, this should be later than the date of onset and admission, but there are times when the disease is diagnosed in ICU.
#' @param genomics Genomics variables
#' @param dod Variable representing date of death
#' @param died Variable representing death
#'
#' @return A dataset that is cleaned ready for linkage
#' @export

clean_build_nest <- function(data,
                           id=NULL,
                           diagnosis=NULL,
                           firstname=NULL,
                           surname=NULL,
                           dob=NULL,
                           age=NULL,
                           medicare=NULL,
                           postcode=NULL,
                           gender=NULL,
                           fn=NULL,
                           latitude=NULL,
                           longitude=NULL,
                           onset_date=NULL,
                           admission_date=NULL,
                           discharge_date=NULL,
                           hospital=NULL,
                           icd_code=NULL,
                           diagnosis_description=NULL,
                           icu_date=NULL,
                           icu_hours=NULL,
                           dialysis=NULL,
                           genomics=NULL,
                           dod=NULL,
                           died=NULL) {


  age2breaks <- c(0,60,500)
  age2labels <- c("Younger", "Older")

  age4breaks <- c(0,18,60,80,500)
  age4labels <- c("0-17","18-59","60-79", "80+")

  age5breaks <- c(0,5,18,60,80,500)
  age5labels <- c("0-4", "5-17","18-59","60-79", "80+")

  age6breaks <- c(0,5,18,40,60,80,500)
  age6labels <- c("0-4","5-17","18-39","40-59","60-79", "80+")


  age11breaks <- c(0,10,20,30,40,50,60,70,80,90,100,500)
  age11labels <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69",
                   "70-79","80-89","90-99", "100+")

  age18breaks <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,500)
  age18labels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                   "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                   "70-74","75-79","80-84","85+")

  age19breaks <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,500)
  age19labels <- c("0-1","1-4","5-9","10-14","15-19","20-24","25-29","30-34",
                   "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                   "70-74","75-79","80-84","85+")

  age21breaks <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,500)
  age21labels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
                   "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
                   "70-74","75-79","80-84","85-89", "90-94", "95-99", "100+")


  if (class(data)!="data.frame"){
    print("There is no data frame selected")
  }
  if (is.null(id)){
    print("There is no ID selected. You will need this for data-linkage")
  } else {
    data <- data_rename(data, id, "id")
    var_label(data$id) <-  "ID"
  }

  if (!is.null(diagnosis)){
    data <- data_rename(data, diagnosis, "dx")
    var_label(data$dx) <-  "Pathogen"
  }

  if (is.null(firstname)){
    print("There is no first name variable selected. You will need first name, last name, date of birth, gender and date of birth for some data-linkage.")
  } else {
    data <- data_rename(data, {{ firstname }}, "firstname")
    data <- data %>% mutate(firstname = str_split(tolower(firstname), " ", simplify = TRUE)[,1], #to only take the first name, and not middle name
                            firstname = str_replace_all(firstname, "[^[:alnum:]]", ""))
    var_label(data$firstname) <-  "First Name"
  }

  if (is.null(surname)){
    print("There is no surname variable selected. You will need first name, last name, date of birth, gender and date of birth for some data-linkage.")
  } else {
    data <- data_rename(data, {{ surname }}, "surname")
    data <- data %>% mutate(surname = tolower(surname),
                            surname = str_replace_all(surname, "[^[:alnum:]]", ""))
    var_label(data$surname) <-  "Surname"
  }

  if (is.null(dob)){
    print("There is no date of birth variable selected. You will need first name, last name, date of birth, gender and date of birth for some data-linkage.")
  } else {
    data <- data_rename(data, {{ dob }}, "dob")
    data <- data %>% mutate(dob_yr = lubridate::year(dob),
                            dob_mo = lubridate::month(dob),
                            dob_day = lubridate::day(dob))
    var_label(data$dob) <-  "Date of Birth"
  }

  if (!is.null(firstname) & !is.null(surname)){
    data <- data %>% mutate(firstname_surname = paste(firstname, surname))
  }

  if (!is.null(firstname) & !is.null(surname) & !is.null(dob)){
    data <- data %>% mutate(firstname_surname_dob = paste(firstname, surname, dob))
  }

  if (is.null(age)){
    print("There is no age variable selected. Age will be created using onset date minus date of birth if both available.")
  } else {
    data <- data_rename(data, age, "age")
    var_label(data$age) <-  "Age"
  }

  if (!is.null(postcode)){
    data <- data_rename(data, postcode, "postcode")
    var_label(data$postcode) <-  "Postcode"
  }


  if (!is.null(medicare)){
    data <- data_rename(data, {{ medicare }}, "medicare")
    var_label(data$medicare) <-  "Medicare Number"
    data <- data %>% mutate(medicare10 = substr(medicare, 1, 10),
                            medicare11 = str_replace_all(medicare, " ", ""))
    var_label(data$medicare10) <-  "Medicare Number (10 digits)"
    var_label(data$medicare11) <-  "Medicare Number (11 digits)"
  }

  if (!is.null(gender)){
    data <- data_rename(data, {{ gender }}, "gender")
    var_label(data$gender) <-  "Gender"
  }
  if (!is.null(fn)){
    data <- data_rename(data, {{ fn }}, "fn")
    var_label(data$fn) <-  "First Nations Status"
  }

  if (is.null(onset_date)){
    print("There is no onset of disease date variable selected. Expected to be absent in datasets of admissions only.")
  } else {
    data <- data_rename(data, {{ onset_date }}, "onset_date")
    data <- data %>% mutate(onset_iso_week = epiweek(onset_date),
                            onset_year_iso_week = as.numeric(paste0(year(onset_date), ".", epiweek(onset_date))),
                            onset_q = lubridate::quarter(onset_date, type = "year.quarter"),
                            onset_m = format(onset_date, format = "%b-%Y"))
    var_label(data$onset_date) <-  "Onset Date"
    var_label(data$onset_iso_week) <-  "ISO Week"
    var_label(data$onset_year_iso_week) <-  "Year ISO Week"
    var_label(data$onset_q) <-  "Onset Quarter"
    var_label(data$onset_m) <-  "Onset Month"
  }

  if (is.null(age) & !is.null(dob) & !is.null(onset_date)){
    data <- data %>% mutate(age = as.numeric(round((onset_date - dob)/365.25, digits = 1), na.rm = TRUE))
  }

  if (!is.null(data$age)){
    data <- data %>% mutate(
      age2cat = cut(age, breaks = age2breaks, labels = age2labels, ordered_result = TRUE),
      age4cat = cut(age, breaks = age4breaks, labels = age4labels, ordered_result = TRUE),
      age5cat = cut(age, breaks = age5breaks, labels = age5labels, ordered_result = TRUE),
      age6cat = cut(age, breaks = age6breaks, labels = age6labels, ordered_result = TRUE),
      age11cat = cut(age, breaks = age11breaks, labels = age11labels, ordered_result = TRUE),
      age19cat = cut(age, breaks = age19breaks, labels = age19labels, ordered_result = TRUE),
      age21cat = cut(age, breaks = age21breaks, labels = age21labels, ordered_result = TRUE))

    var_label(data$age2cat) <-  "Binary Age Category"
    var_label(data$age4cat) <-  "Age Categories"
    var_label(data$age5cat) <-  "Age Categories"
    var_label(data$age6cat) <-  "Age Categories"
    var_label(data$age11cat) <-  "Age Categories"
    var_label(data$age19cat) <-  "Age Categories"
    var_label(data$age21cat) <-  "WHO Age Category"
  }


  if (!is.null(admission_date)){
    data <- data_rename(data, {{ admission_date }}, "admission_date")
    data <- data %>% mutate(admission_iso_week = epiweek(admission_date),
                            admission_year_iso_week = as.numeric(paste0(year(admission_date), ".", epiweek(admission_date))),
                            admission_q = quarter(admission_date, type = "year.quarter"),
                            admission_m = format(admission_date, format = "%b-%Y"),
                            admission = factor(case_when(is.na(admission_date) ~ 0,
                                                         TRUE == "Yes" ~ 1), levels = c("0", "1"), labels = c("No Admission", "Admission")))
    var_label(data$admission_date) <-  "admission Date"
    var_label(data$admission_iso_week) <-  "Admission ISO Week"
    var_label(data$admission_year_iso_week) <-  "Admission Year ISO Week"
    var_label(data$admission_q) <-  "Admission Quarter"
    var_label(data$admission_m) <-  "Admission Month"
    var_label(data$admission) <-  "Admission"
  }

  if (!is.null(discharge_date)){
    data <- data_rename(data, {{ discharge_date }}, "discharge_date")
  }

  if (!is.null(admission_date) & !is.null(discharge_date)){
    data <- data %>% mutate(los = as.numeric(discharge_date - admission_date))
    var_label(data$los) <-  "Length of Stay (days)"
  }

  if (!is.null(hospital)){
    data <- data_rename(data, {{ hospital }}, "hospital")
  }

  if (!is.null(icd_code)){
    data <- data_rename(data, {{ icd_code }}, "icd_code")
  }

  if (!is.null(diagnosis_description)){
    data <- data_rename(data, {{ diagnosis_description }}, "diagnosis_description")
  }

  if (!is.null(icu_date)){
    data <- data_rename(data, {{ icu_date }}, "icu_date")
    data <- data %>% mutate(icu_admission = factor(case_when(is.na(icu_date) ~ 0,
                                                             TRUE == "Yes" ~ 1), levels = c("0", "1"), labels = c("No Admission", "Admission")))
  }

  if (!is.null(icu_hours)){
    data <- data_rename(data, {{ icu_hours }}, "icu_hours")
  }

  if (!is.null(dialysis)){
    data <- data_rename(data, {{ dialysis }}, "dialysis")
  }

  if (!is.null(genomics)){
    data <- data_rename(data, {{ genomics }}, "genomics")
    var_label(data$genomics) <-  "Genomics"
  }

  if (!is.null(dod)){
    data <- data_rename(data, {{ dod }}, "dod")
    var_label(data$died) <-  "Date of Death"
  }

  if (is.null(died) & !is.null(dod)){ #if there is no died variable
    data <- data %>% mutate(died = factor(case_when(is.na(dod) ~ 0,
                                                    TRUE == "Yes" ~ 1), levels = c("0", "1"), labels = c("Alive", "Died")))
    var_label(data$died) <-  "Death"
  }

  if (!is.null(data$gender) & !is.null(data$postcode) & !is.null(data$dob_yr)){ #block 1 creation
    data <- data %>% mutate(block1 = paste(gender, postcode, dob_yr),
                            block2 = paste(postcode, dob_yr),
                            block3 = paste(gender, dob_yr))
  } else if (!is.null(data$dob_yr) & !is.null(data$postcode) ) {
    data <- data %>% mutate(block2 = paste(postcode, dob_yr))
  } else if (!is.null(data$dob_yr) & !is.null(data$gender) ) {
    data <- data %>% mutate(block3 = paste(gender, dob_yr))
  }


  return(data)
}






