# https://docs.ropensci.org/charlatan/articles/charlatan.html
# https://github.com/trinker/wakefield
# cat(wakefield::variables(), sep = ",\n")

fakedata <-
  
  wakefield::r_data_frame(
    n = 250,
    
    id,
    name,
    sex,
    age(x = 25:73, name = "Age"),
    race,
    
    answer(
      x = c("Absent", "Present"),
      prob = c(.8, .2),
      name = "PreinvasiveComponent"
    ),
    
    answer(
      x = c("Absent", "Present"),
      prob = c(.8, .2),
      name = "LVI"
    ),
    
    answer(
      x = c("Absent", "Present"),
      prob = c(.7, .3),
      name = "PNI"
    ),
    
    date_stamp(random = TRUE, name = "LastFollowUpDate"),
    death(prob = c(.3, .7)),
    group,
    
    level(
      x = 1:3,
      prob = c("0.3", "0.3", "0.4"),
      name = "Grade"
    ),
    
    level(
      x = 1:4,
      prob = c("0.1", "0.2", "0.4", "0.3"),
      name = "TStage"
    ),
    
    answer(
      x = c("Absent", "Present"),
      prob = c("0.6", "0.4"),
      name = "LymphNodeMetastasis"
    ),
    valid,
    
    Smoker = valid,
    
    grade_level(
      x = c("low", "moderate", "high"),
      prob = c("0.3", "0.3", "0.4")
    )
    
    # animal,
    # area,
    # car,
    # children,
    # coin,
    # color,
    # dice,
    # dna,
    # dob,
    # dummy,
    # education,
    # employment,
    # eye,
    # grade,
    # hair,
    # height,
    # income,
    # internet_browser,
    # iq,
    # language,
    # likert
    # lorem_ipsum,
    # marital,
    # military,
    # month,
    # normal,
    # political,
    # religion,
    # sat,
    # sentence,
    # sex_inclusive,
    # smokes,
    # speed,
    # state,
    # string,
    # upper,
    # year,
    # zip_code
  ) %>%
  
  r_na(x = .,
       prob = .005) %>%
  
  mutate(
    LVI = factor(LVI, ordered = TRUE),
    PNI = factor(PNI, ordered = TRUE),
    PreinvasiveComponent = factor(PreinvasiveComponent, ordered = TRUE),
    Grade = factor(Grade, ordered = TRUE),
    TStage = factor(TStage, ordered = TRUE),
    LymphNodeMetastasis = factor(LymphNodeMetastasis, ordered = TRUE)
  ) %>%
  
  mutate(SurgeryDate = LastFollowUpDate - sample(
    x = 180:3600,
    size = 250,
    replace = TRUE
  )) %>%
  
  mutate(LastFollowUpDate  =
           case_when(Death == FALSE ~  Sys.Date() - 10,
                     TRUE ~ LastFollowUpDate))

## Reordering fakedata$LVI
# fakedata$LVI <- factor(fakedata$LVI, levels=c("Absent", "Present"), ordered = TRUE)


fakedata %>%
  plot(palette = "Set1")


rio::export(
  x = fakedata,
  file = here::here("data", "mydata.xlsx"),
  format = "xlsx"
)
