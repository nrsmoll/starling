## code to prepare `DATASET` dataset goes here

dx_data <- structure(data.frame(cbind(c(1, 2, 6, 9),
                                      c("COVID-19", "Campylobacter", "Shigella", "RSV"),
                                      c("Nicolas", "Hadley", "Tom", "Megan"),
                                      c("Smoll", "Wickham", "Barker", "Juniper"),
                                      c("06-06-1954", "12-06-1922", "31-10-2015", "12-5-2001"),
                                      c(12345678902, 09876543215, 56565479012, 56725479034),
                                      c("Male", "Female", "Male", "Female"),
                                      c(4560, 4560, 4560, 4560),
                                      c("ATSI", "Neither", "Torres Strait Islander", "None"),
                                      c("06-06-2024", "16-01-2024", "31-12-2023", "23-4-2021"))),
                     names = c("identity", "disease_name", "first_name", "surname", "date_of_birth",
                               "medicare_no", "gender", "postcode", "indigenous_status", "diagnosis_date"))

dx_data$date_of_birth <- as.Date(dx_data$date_of_birth, format = '%d-%m-%Y')
dx_data$medicare_no <- as.numeric(dx_data$medicare_no)
dx_data$diagnosis_date <- as.Date(dx_data$diagnosis_date, format = '%d-%m-%Y')
usethis::use_data(dx_data, overwrite = TRUE)

hosp_data <- structure(data.frame(cbind(c(304056, 403030, 723434, 723434),
                                        c("Nicholas", "Hadley", "Tom", "Tom", "Tom"),
                                        c("Smoll", "Whickam", "Barker", "Barker", "Barker"),
                                        c("06-06-1954", "12-06-1922", "31-10-2015", "31-10-2023", "31-10-2023"),
                                        c(12345678902, 09876543215, 56565479012, 56565479012, 56565479012),
                                        c("Male", "Female", "Male", "Male", "Male"),
                                        c(4560, 4560, 4560, 4560, 4560),
                                        c("ATSI", "Neither", "Torres Strait Islander", "Torres Strait Islander", "Torres Strait Islander"),
                                        c("R06.02", "A09", "A09", "A09", "I46.9"),
                                        c("04-06-2024", "28-01-2024", "05-01-2024", "12-01-2024", "25-03-2024"),
                                        c("09-06-2024", "2-02-2024", "10-01-2024", "18-01-2024", "29-03-2024"))),
                       names = c("patient_id", "firstname", "last_name", "birth_date", "medicare_number", "sex", "zip_codes", "cultural_heritage","icd_codes", "date_of_admission", "date_of_discharge")  )

hosp_data$birth_date <- as.Date(hosp_data$birth_date, format = '%d-%m-%Y')
hosp_data$medicare_number <- as.numeric(hosp_data$medicare_number)
hosp_data$date_of_admission <- as.Date(hosp_data$date_of_admission, format = '%d-%m-%Y')
hosp_data$date_of_discharge <- as.Date(hosp_data$date_of_discharge, format = '%d-%m-%Y')
usethis::use_data(hosp_data, overwrite = TRUE)
