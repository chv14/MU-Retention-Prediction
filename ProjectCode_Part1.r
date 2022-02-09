# ========================================= PART 1 ======================================

# Load Necessary Packages and Read in Data ----------------------------------------------
if(require(pacman) == FALSE) install.packages("pacman")
pacman::p_load(tidyverse, magrittr, DataExplorer, lubridate, dplyr, stringr, naniar, lares, fastDummies, readxl)

df = read.csv("/Users/chv/Documents/ISA 491/ISA 491 Project/domestic_data.csv", stringsAsFactors = T)
source("/Users/chv/Documents/ISA 491/Module 2 - Overview of the Data Mining Process/data summary.r")

# Filter Time Range ---------------------------------------------------------------------
df %<>% filter(DateFrom %in% (2010:2017))

# Filter out International Students -----------------------------------------------------
# Filter out based on Application Type, Flag, Region, Race 
df %<>% filter(!ApplicationType %in% "OI" & # No missing value for ApplicationType
                      !InternationalFlag %in% c("International", "international") &
                      !MCFlag %in% "International" &
                      !USStateRegion %in% "International" &
                      !Race %in% "NC" &
                      !OneRace %in% c("NC", "Non-Resident Alien") &
                      !FullRace %in% "Non-Resident Alien")
df$ApplicationType <- droplevels(df$ApplicationType)
    
# Filter out based on visa type
df %<>% filter(((Visa.Type %in% c("RA - Resident Alien", "Asylee", "")) | is.na(Visa.Type)) &
               (VisaType %in% c("RA", "NV", "XX", "")) | is.na(VisaType))

# Filter out international students using TOEFL and IELTS
df %<>% filter_at(vars(TCP1, TCP2, TCP3, TCP4, TCPT, 	
                                   TIBL, TIBR, TIBS, TIBT, TIBW, 
                                   TOEFL.Listening, TOEFL.Reading, TOEFL.Speaking, 
                                   TOEFL.Structure.Written.Expression),
                                all_vars(is.na(.))) # all_vars(is.na(.)) means all the variables listed need to be NA

df %<>% filter_at(vars(IELL, IELO, IELR, IELS, IELW,
                       IELTS.Listening, IELTS.Overall.Band.Score, IELTS.Reading, IELTS.Speaking, IELTS.Writing),
                  all_vars(is.na(.))) # all_vars(is.na(.)) means all the variables listed need to be NA

# Remove variables about TOEFL, IELTS, Visa Type, and Flag
df %<>% select(-c(TCP1, TCP2, TCP3, TCP4, TCPT, 	
                  TIBL, TIBR, TIBS, TIBT, TIBW, 
                  TOEFL.Listening, TOEFL.Reading, TOEFL.Speaking, 
                  TOEFL.Structure.Written.Expression,
                  IELL, IELO, IELR, IELS, IELW,
                  IELTS.Listening, IELTS.Overall.Band.Score, IELTS.Reading, IELTS.Speaking, IELTS.Writing,
                  Visa.Type, VisaType,
                  MCFlag, InternationalFlag))

# Remove Variables -----------------------------------------------------------------------
# Identify the variables that have all missing values (columns that are entirely N/A) and remove those variables
df %<>% select(-colnames(df[which(lapply(df, function(x) sum(is.na(x))) == nrow(df))]))

# Remove variables based on plot_missing() and redundant variables

df %<>% select(-c(# Contribute nothing
                  tag, ZipCode, Zip, Zipcode, Zip5, Phone,
                  CEEB1, CEEB2, CEEB3, CEEB4, CEEB6, CEEB7, CEEB8, CEEB9, CEEB10, CEEB5,
                  HighSchoolCode, hs.code, HS.Code,
                  TermCode,
                  Decision, # This variable has only 1 level
                  
                  # Too many missing values and duplicated
                  Retained_recode, RoundedGPA,
                  ACTMaxComposite, ACTMaxEnglish, ACTMaxMath, ACTMaxReading, ACTMaxSciReasoning, ACTWritingMax, Super.ACT, Act25, Act75,
                  SAT.R.ERW, SAT.R.Math, Satm25, Satm75, Satv25, Satv75, 
                  SATWRSC, SATVerbal, SATComp,
                  ACTEquivalent,
                  ApplicationDate_Formatted, ConfirmedDate, ConfirmDate_Formatted, DecisionDate_Formatted, DecisionDateFormatted,
                  ACTSci, ACTRdng, ACTMath, ACTEng, ACTComposite, ACTWRSC, # Kept ACTBest
                  
                  ACTChoice, RankPercent, ClassRank, ClassSize, # Rank and size in 2018 missing too much
                  WhichTestBest, Dec,
                  
                  # 2018 data have too little MiamiRanks data
                  MiamiRanks, 
                  FirstSchool, SecondSchool, ThirdSchool, FourthSchool, FifthSchool,SixthSchool, SeventhSchool, EighthSchool, TenthSchool, 
                  
                  ## Variables that 2018 data doesn't have
                  InitialContact, EduNbrhd, Permanent.Home, College.Since.9th.Grade, Primary_Parent_Occupation, Sec_Parent_Occupation,
                  Com.App.Acad.Int,
                  Lang, Math, MeritGPA, DataFrom, FSBDirect,
                  
                  GPAScale, OriginalGPA, GPAOrig, # Keep GPA because it's been re-scale on a 4.0 scale
                  County, CountyCode, # Keep CountyDesc has the least missing values
                  IntlTestScoreThresholdFlag, Intl.Scholarship, ACEFlag, # Related to international students
                  AIDY_Code, ConCode, Harrison, MAI, Geomkt, StudentType, MeritGPAThresholdFlag, # Not sure this vars are about
                  
                  Status, ConfirmCode, HSClust, EER, Division
              ))

# Identify if the student took SAT or not ---------------------------------
df$SAT <- NA
for (i in 1:nrow(df)) {
  if (is.na(df$SATMath[i]) == TRUE) {
    df$SAT[i] <- "No"
  } else {
    df$SAT[i] <- "Yes"
  }
}
df$SAT = as.factor(df$SAT)

# Remove SATMath
df %<>% select(-SATMath)

# Merging Race Variables --------------------------------------------------
## Merge race to analyze students based on if they are White or not 
levels(df$WH_Race)
levels(df$Race)
levels(df$FullRace)
levels(df$AlmostFullRace)
levels(df$OneRace)

df$WH_Race[df$WH_Race == ""] <- NA
df$Race[df$Race == ""] <- NA
df$FullRace[df$FullRace == ""] <- NA
df$AlmostFullRace[df$AlmostFullRace == ""] <- NA
df$OneRace[df$OneRace == ""] <- NA

# Create columns Race_WH to identify if students are White
df$Race_WH <- NA

levels(df$Race_WH) <- c(levels(df$Race_WH), c("Yes", "No", "Unknown")) # Add new level

df$Race_WH[df$WH_Race == "WH" | df$Race %in% c("White", "WH", "5") | 
              df$FullRace %in% c("WH", "White") | df$AlmostFullRace == "WH" | 
              df$OneRace %in% c("WH", "White")] <- "Yes"

df$Race_WH[df$OneRace %in% c("UK", "Unknown") | df$Race %in% c("UK", "8") | df$FullRace == "UK"] <- "Unknown"
df$Race_WH[is.na(df$Race_WH) == T] <- "No"

df$Race_WH = as.factor(df$Race_WH)

## Remove all race variables except for Race_WH
df %<>% select(-c(Race, AI_Race, AS_Race, BL_Race, HS_Race,
                  PI_Race, WH_Race, OneRace, FullRace, 
                  FullEthn.Race, AlmostFullRace, FullEthn,
                  Ethn_HS_YN, Hispanic))

## Merge 2 Special Consideration columns and only replace N/A values with the other columns value
df$SpecialConsideration[df$SpecialConsideration == ""] <- NA
df$Special.Consideration[df$Special.Consideration == ""] <- NA
df["SpecialConsideration"] = coalesce(df$SpecialConsideration, df$Special.Consideration)
df %<>% select(-Special.Consideration) # Remove one column

levels(df$SpecialConsideration) <- c(levels(df$SpecialConsideration), c("No", "Yes")) # Add new level ("No")
df$SpecialConsideration[is.na(df$SpecialConsideration == T)]  <- "No" # Change NA values to No
df$SpecialConsideration[df$SpecialConsideration != "No"] <- "Yes"
df$SpecialConsideration <- droplevels(df$SpecialConsideration)

# Fix Citizenship ---------------------------------------------------------
levels(df$Citizen)
levels(df$Citizen) <- c(levels(df$Citizen), "Unknown") # Add new level "Unknown"
df$Citizen[is.na(df$Citizen == T) | df$Citizen %in% c("","D")]  <- "Unknown"
df$Citizen <- droplevels(df$Citizen)

# Remove citizenship2 col
df %<>% select(-c(Citizenship, Citizenship2))

# Fix Parent Edu Level ----------------------------------------------------
df$Parent.1.Education.Level[df$Parent.1.Education.Level == ""]  <- NA
df$Parent.2.Educational.Level[df$Parent.2.Educational.Level == ""]  <- NA
df$Parent1Degree[df$Parent1Degree == ""]  <- NA
df$Parent2Degree[df$Parent2Degree == ""]  <- NA

# Identify if one of the parents have bachelor degree or above
df$Parent_College <- NA
for (i in 1:nrow(df)) {
  if (df$Parent.1.Education.Level[i] %in% c("Graduate school", "Graduated from college/university") | 
      df$Parent1Degree[i] %in% "College" | 
      df$Parent.2.Educational.Level[i] %in% c("Graduate school", "Graduated from college/university") |
      df$Parent2Degree[i] %in% "College") {
    df$Parent_College[i] <- "Yes"
  } else if ((is.na(df$Parent.1.Education.Level[i])) == T & (is.na(df$Parent.2.Educational.Level[i])) == T &
              (is.na(df$Parent1Degree[i])) == T & (is.na(df$Parent2Degree[i])) == T) {
    df$Parent_College[i] <- "Unknown"
  } else {
    df$Parent_College[i] <- "No"
  }
}

df$Parent_College = as.factor(df$Parent_College)

# Remove parent_level and parent_degree cols
df %<>% select(-c(Parent.1.Education.Level, Parent.2.Educational.Level,
                  Parent1Degree, Parent2Degree))

# Collapse Major ----------------------------------------------------------
# Read in major description
major_code = read_excel("/Users/chv/Documents/ISA 491/ISA 491 Project/Major Codes and Descriptions.xlsx")

# Merge 2 df, keep the left one
df = left_join(df, major_code, by = c("Major" = "STVMAJR_CODE"))
for (i in 1:nrow(df)) {
  if(is.na(df$STVMAJR_DESC[i]) == TRUE) {
    df$STVMAJR_DESC[i] <- df$Major[i]
  }
}

df = select(df, -Major)

names(df)[names(df) == 'STVMAJR_DESC'] <- 'Major' # Rename variable

# Coding "56" and "Undeclared - XX" as Undeclared 
df$Major <- str_replace_all(string = df$Major, pattern = "56", replacement = "Undeclared")
df$Major <- str_replace_all(string = df$Major, pattern = "[A-Z]{2}56", replacement = "Undeclared")
df$Major <- str_replace_all(string = df$Major, pattern = "[A-Z]{2}Undeclared", replacement = "Undeclared")
df$Major <- str_replace_all(string = df$Major, pattern = "Undeclared.*", replacement = "Undeclared")

df$Major[df$Major %in% c("Strategic Communication", "Communication Design", "Communication Pre-Major")] <- "Communication"
df$Major[df$Major %in% c("Management Information Systems", "Information Systems")] <- "Information Systems and Analytics"


# Sort top10 majors
head(sort(table(df$Major), decreasing = T), 10)

top10_major <- head(sort(table(df$Major), decreasing = T), 10) %>% 
  as.data.frame()

for (i in 1: nrow(df)) {
  if ((df$Major[i] %in% top10_major$Var1) == FALSE) {
    df$Major[i] <- "Others"
  } else if ((df$Major[i] %in% top10_major$Var1) == TRUE) {
    df$Major[i] <- df$Major[i]
  }
}

df$Major %<>% as.factor()
levels(df$Major)

# Fixing SuppMajors -------------------------------------------------------
# Decide if the students have one or multiple majors
levels(df$SuppMajor1)
table(is.na(df$SuppMajor2))
table(is.na(df$SuppMajor3))

df$SuppMajor1[df$SuppMajor1 == ""] <- NA
df$SuppMajor2[df$SuppMajor2 == ""] <- NA
df$SuppMajor3[df$SuppMajor3 == ""] <- NA

df$OneMajor <- NA
for (i in 1:nrow(df)) {
  if (is.na(df$SuppMajor1[i]) == FALSE & is.na(df$SuppMajor2[i]) == TRUE & is.na(df$SuppMajor3[i]) == TRUE) {
    df$OneMajor[i] <- "Yes" # only one major
  } else if (is.na(df$SuppMajor1[i]) == TRUE) {
    df$OneMajor[i] <- "Unknown"
  } else {
    df$OneMajor[i] <- "No" # more than one major
  }
}

df$OneMajor = as.factor(df$OneMajor)

# Remove suppMajor1, suppMajor2, suppMajor3 
df %<>% select(-c(SuppMajor1, SuppMajor2, SuppMajor3))

# Fix Date ----------------------------------------------------------------
df$ConfirmDate %<>% ymd()
df$ApplicationDate = as.Date(df$ApplicationDate, format = "%m/%d/%Y")
df$DecisionDate = as.Date(df$DecisionDate, format = "%m/%d/%Y")

# Find the difference between ApplicationDate and DecisionDate
x<-interval(ymd(df$ApplicationDate), ymd(df$DecisionDate))
x<-x %/% days(1)
df$DecisionLength<-x
df$DecisionLength = ifelse(df$DecisionLength < 0, 0, df$DecisionLength)

# Remove ApplicationDate, ConfirmDate and DecisionDate
df %<>% select(-c(ApplicationDate, DecisionDate, ConfirmDate))

# Recode Variables --------------------------------------------------------
# Recode FirstGen 
df$FirstGen = ifelse(is.na(df$FirstGen) | df$FirstGen == "", "No", "Yes")
df$FirstGen = as.factor(df$FirstGen)
str(df$FirstGen)

# Recode Bridges
df$Bridges = ifelse(is.na(df$Bridges) | df$Bridges == "", "No", "Yes")
df$Bridges = as.factor(df$Bridges)
str(df$Bridges)

# Recode AlumniConnection
levels(df$AlumniConnection)
levels(df$AlumniConnection) <- c(levels(df$AlumniConnection), c("Yes", "No")) # Add new levels
df$AlumniConnection[(is.na(df$AlumniConnection) == T) | (df$AlumniConnection == "")] <- "No"
df$AlumniConnection[df$AlumniConnection != "No"] <- "Yes"
df$AlumniConnection <- droplevels(df$AlumniConnection)

# Recode DecisionType
levels(df$DecisionType) <- c(levels(df$DecisionType), c("Others", "Unknown")) # Add new levels
df$DecisionType[df$DecisionType == "" | is.na(df$DecisionType == T)]  <- "Unknown" # Change "" and NA values to Unknown
df$DecisionType[df$DecisionType %in% c("DN", "DQ", "HN", "HQ", "HY", "N", "S", "SQ", "SY")]  <- "Others"
df$DecisionType <- droplevels(df$DecisionType)
str(df$DecisionType)

# Recode ON (Other Notion Score)
df$ON = as.factor(df$ON)
levels(df$ON) <- c(levels(df$ON), c("Unknown")) # Add new level
df$ON[is.na(df$ON) == T]  <- "Unknown" # Change NA values to Unknown
df$ON <- droplevels(df$ON)
df = select(df, -ON)

# Recode HsType
levels(df$HsType) <- c(levels(df$HsType), c("Others")) # Add new level
df$HsType[df$HsType == "1"]  <- "Public"
df$HsType[df$HsType == "2"]  <- "Private Secular"
df$HsType[df$HsType == "3"]  <- "Religious"
df$HsType[df$HsType == "4"]  <- "Religious"
df$HsType[df$HsType %in% c("Charter", "Home School")]  <- "Others"
df$HsType[is.na(df$HsType == T) | df$HsType == ""]  <- "Unknown"
df$HsType <- droplevels(df$HsType)

# Recode Housing variable
levels(df$Housing) <- c(levels(df$Housing), c("Unknown")) # Add new level
df$Housing[is.na(df$Housing) == T] <- "Unknown"

# Recode StateResidency
df$StateResidency[df$StateResidency == "Z"] <- "N"
df$StateResidency <- droplevels(df$StateResidency)

# Recode retained (our target variable)
names(df)[names(df) == 'retained'] <- 'Target' # Rename variable as "Target"
df$Target = as.factor(df$Target)
levels(df$Target) <- c(levels(df$Target), "Yes") # Add new level ("Yes")
levels(df$Target) <- c(levels(df$Target), "No") # Add new level ("No")
df$Target[df$Target == "1"]  <- "No"
df$Target[df$Target == "0"]  <- "Yes" 
df$Target <- droplevels(df$Target)

# Filter rows missing Target
df %<>% filter(is.na(df$Target) != T)

# Fix DisciplinaryQuestions -------------------------------------------------
# Identify if the students ever flagged for disciplinary questions
df$Question[df$Question == ""]  <- NA
df$DisciplinaryQuestion1[df$DisciplinaryQuestion1 == ""]  <- NA

df$Disciplinary <- NA
levels(df$Target) <- c(levels(df$Target), c("Yes", "No")) # Add new levels
df$Disciplinary[(is.na(df$Question) == TRUE) & (is.na(df$DisciplinaryQuestion1) == TRUE)] <- "No"
df$Disciplinary[is.na(df$Question) == FALSE | is.na(df$DisciplinaryQuestion1) == FALSE] <- "Yes"
df$Disciplinary = as.factor(df$Disciplinary)

# Remove Question, DisciplinaryQuestion1
df %<>% select(-c(Question, DisciplinaryQuestion1))

# Fix NationDesc ----------------------------------------------------
# Recode NationDesc
df$NationDesc[is.na(df$NationDesc == T)]  <- "United States"

# Identify US nation or non-US
levels(df$NationDesc)
df$Nation <- NA

for (i in 1:nrow(df)) {
  if (df$NationDesc[i] == "United States" ){
    df$Nation[i] <- "US"
  } else {
    df$Nation[i] <- "Non-US"
  }
}
df$Nation = as.factor(df$Nation)

# Remove NationDesc
df %<>% select(-NationDesc)

# Fix HomeState -----------------------------------------------------------
levels(df$HomeState)

# Top 5 states: OH    IL    MI    IN    PA
head(sort(table(df$HomeState), decreasing = T), 5)

levels(df$HomeState) <- c(levels(df$HomeState), c("Unknown")) # Add new level
df$HomeState[df$HomeState == "" | is.na(df$HomeState) == T] <- "Unknown"
df$HomeState = as.factor(df$HomeState)
df$HomeState <- droplevels(df$HomeState)

# Remove
df %<>% select(-c(OhioCountyRegion, 
                  USStateRegion, CountyDesc))

# Fix HighSchoolState -------------------------------------------------------
levels(df$HighSchoolState)

# Top 5 states: OH    IL    IN    MI    PA
head(sort(table(df$HighSchoolState), decreasing = T), 5)
levels(df$HighSchoolState) <- c(levels(df$HighSchoolState), c("Unknown")) # Add new level
df$HighSchoolState[df$HighSchoolState == "" | is.na(df$HighSchoolState) == T] <- "Unknown"
df$HighSchoolState = as.factor(df$HighSchoolState)
df$HighSchoolState <- droplevels(df$HighSchoolState)

# Fix Concentration -------------------------------------------------------
levels(df$Concentration)

# Top concentration:
head(sort(table(df$Concentration), decreasing = T), 5)

levels(df$Concentration) <- c(levels(df$Concentration), c("Unknown")) # Add new level
df$Concentration[df$Concentration == "" | is.na(df$Concentration) == T] <- "Unknown"
df$Concentration <- droplevels(df$Concentration)

# Create Dummies ----------------------------------------------------------
introduce(df)
data.summary(df)

df$DecisionType <- relevel(df$DecisionType,"Others")
df$Major <- relevel(df$Major,"Others")
df$HsType <- relevel(df$HsType,"Others")

# Dummy for top levels in HomeState, HighSchoolState, Concentration, 
# Housing, ON, Race_WH, Citizen, Parent_College
## HomeState
df$HomeState_OH = ifelse(df$HomeState == "OH", 1, 0)
df$HomeState_IL = ifelse(df$HomeState == "IL", 1, 0)
df$HomeState_MI = ifelse(df$HomeState == "MI", 1, 0)
df$HomeState_IN = ifelse(df$HomeState == "IN", 1, 0)
df$HomeState_PA = ifelse(df$HomeState == "PA", 1, 0)
df = select(df, -HomeState)

## HighSchoolState
df$HighSchoolState_OH = ifelse(df$HighSchoolState == "OH", 1, 0)
df$HighSchoolState_IL = ifelse(df$HighSchoolState== "IL", 1, 0)
df$HighSchoolState_MI = ifelse(df$HighSchoolState == "MI", 1, 0)
df$HighSchoolState_IN = ifelse(df$HighSchoolState == "IN", 1, 0)
df$HighSchoolState_PA = ifelse(df$HighSchoolState == "PA", 1, 0)
df = select(df, -HighSchoolState)

## Concentration
df$Concentration_BB = ifelse(df$Concentration == "BB", 1, 0)
df$Concentration_PM = ifelse(df$Concentration == "PM", 1, 0)
df = select(df, -Concentration)

## Housing
df$Housing_ON = ifelse(df$Housing == "On Campus", 1, 0)
df = select(df, -Housing)

## Race_WH
df$Race_WH_Yes = ifelse(df$Race_WH == "Yes", 1, 0)
df = select(df, -Race_WH)

## Citizen
df$Citizen_Y = ifelse(df$Citizen == "Y", 1, 0)
df = select(df, -Citizen)

## OneMajor
df$OneMajor_Yes = ifelse(df$OneMajor == "Yes", 1, 0)
df = select(df, -OneMajor)

df <- dummy_columns(df, select_columns = c("Gender", "StateResidency", "FirstGen", "AlumniConnection", "ApplicationType",
                                           "SpecialConsideration", "DecisionType", "Major", "HsType",
                                           "Bridges", "SAT", "Parent_College",
                                           "Disciplinary", "Nation"),
                    remove_first_dummy = TRUE, 
                    remove_selected_columns = TRUE)

# Imputation --------------------------------------------------------------
plot_missing(df)

df$GPA[is.na(df$GPA)]<-median(df$GPA, na.rm=TRUE)
df$AcadRS[is.na(df$AcadRS)]<-median(df$AcadRS, na.rm=TRUE)
df$M_DecisionLength<-ifelse(is.na(df$DecisionLength), 1, 0)
df$DecisionLength[is.na(df$DecisionLength)]<-median(df$DecisionLength, na.rm=TRUE)

plot_missing(df)

# Tidy column names
names(df) <- gsub(" ", "_", names(df))

saveRDS(df, "/Users/chv/Documents/ISA 491/ISA 491 Project/domestic.clean.RDS")

