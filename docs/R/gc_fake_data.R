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
    
    level(
      x = 1:3,
      prob = c("0.1", "0.4", "0.5"),
      name = "Anti-X-intensity"
    ),
    
    
    level(
      x = 1:3,
      prob = c("0.3", "0.4", "0.3"),
      name = "Anti-Y-intensity"
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
  
  dplyr::mutate(SurgeryDate = LastFollowUpDate - c(
    sample(
      x = 90:360,
      size = 150,
      replace = TRUE
    ),
    sample(
      x = 360:1080,
      size = 75,
      replace = TRUE
    ),
    sample(
      x = 1080:1800,
      size = 25,
      replace = TRUE
    )
  )) %>%
  
  wakefield::r_na(x = .,
                  prob = .005) %>%
  
  dplyr::mutate(
    Age = as.numeric(Age),
    LVI = factor(LVI, ordered = TRUE),
    PNI = factor(PNI, ordered = TRUE),
    PreinvasiveComponent = factor(PreinvasiveComponent, ordered = TRUE),
    Grade = factor(Grade, ordered = TRUE),
    TStage = factor(TStage, ordered = TRUE),
    LymphNodeMetastasis = factor(LymphNodeMetastasis, ordered = TRUE)
  )


fakedata <- fakedata %>%
  dplyr::mutate(
    DeathTime =
      case_when(
        LastFollowUpDate - SurgeryDate <= 360 ~ "Within1Year",
        TRUE ~ "MoreThan1Year"
      )
  )


fakedata$LVI[fakedata$DeathTime == "Within1Year"] <-
  
  sample(
    x = c("Absent", "Present"),
    prob = c(.5, .5),
    size = sum(fakedata$DeathTime == "Within1Year"),
    replace = TRUE
  )

fakedata$TStage[fakedata$DeathTime == "Within1Year"] <-
  
  sample(
    x = c(1:4),
    prob = c(.1, .2, .2,.5),
    size = sum(fakedata$DeathTime == "Within1Year"),
    replace = TRUE
  )

fakedata %>%
  plot(palette = "Set1")

rio::export(
  x = fakedata,
  file = here::here("data", "mydata.xlsx"),
  format = "xlsx"
)
