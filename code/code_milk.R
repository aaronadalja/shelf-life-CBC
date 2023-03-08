# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# Consumer willingness to pay for shelf life of high-temperature-short-time 
# (HTST)-pasteurized fluid milk: Implications for smart labeling and food 
# waste reduction
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# 1). Preliminaries
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      
      # 1.1). Load packages 
        rm(list=ls())
        want <- c("tidyverse", "magrittr", "tidyselect", "stringr",
                  "gmnl", "mlogit", "dplyr", "prcomp", "gtsummary", "xtable")
        need <- want[!(want %in% installed.packages()[,"Package"])]
        if (length(need)) install.packages(need)
        lapply(want, function(i) require(i, character.only=TRUE))
        rm(want, need)

      # 1.2). Working directories
        dir <- list()
        dir$root <- dirname(getwd())
        dir$data <- paste(dir$root, "/data", sep = "")
        dir$output <- paste(dir$root, "/output", sep = "")
        dir$clean_data <- paste(dir$output, "/clean_data", sep = "")
        dir$tables <- paste(dir$output, "/tables", sep = "")
        
      # 1.3). Call the functions file 
        
        source("functions.R")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# 2). Loading and cleaning the data
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      
      # 2.1). Load the data
        raw <- read.csv(paste(dir$data,"/milk_full.csv", sep = ""), sep = ";" )
        design <- read.csv(paste(dir$data,"/design_full.csv", sep = ""))
        regions_matrix <- read.csv(paste(dir$data,"/regions.csv", sep = ""))
        hincome_matrix <- read.csv(paste(dir$data,"/household_income.csv", sep = ""))
        
      # 2.2). Assign simplified scales to the required variables: state and household income. 
        raw <- full_join(raw,regions_matrix,by="state")
        raw <- full_join(raw,hincome_matrix,by="hhinc")
        
      # 2.3). Concatenate ethnicity into one column
        raw$Ethnicity <- paste(raw$white, raw$middle_east, raw$black, raw$native, raw$asian, raw$pacific, raw$hispanic, raw$race_othr_txt)
        raw$Ethnicity <- as.factor(raw$Ethnicity)
        
      # 2.4). Rename the columns that contain perceptions information
        raw <- raw %>% relocate(date_quality, .before = discard_afterdate)
        raw <- raw %>% relocate(best_sell, .before = discard_afterdate)
        
        raw_new_names <- c("upmil", "dqua", "besel",                    # Labeling perceptions
                           "daft", "don", "dfin", "dsmel", "dtast",     # Discard perceptions
                            "fsig", "fbad", "fwron", "fcons", "fhand",   # Foodwaste perceptions
                            "shsoc", "promsu")                           # Social currency perceptions
        
        colnames(raw)[65:79] <- c(raw_new_names)
        #colnames(raw)
        
      # 2.5). Set Likert data to numberic (use the factorise function)
        
        raw[,c(raw_new_names)] <- sapply(raw[,c(raw_new_names)],factorise) # Apply the factorise function, raw[65:79]
        raw[,c(raw_new_names)][is.na(raw[,c(raw_new_names)])] <- 0 # If needed: Set the NA values to 0
      
      # 2.6). Calcuate PCA loads for perceptions data
        
        # 2.6.1). Create a correlation matrix
          pca_data <- subset(raw, select = c(raw_new_names))
          cormat <- round(cor(pca_data), 2) # Create a correlation matrix
          cormat
        
        # 2.6.2). Create a list for each subset of data                           
          pca_list <- list()
          pca_list$labelling <- raw[,c("upmil", "dqua", "besel")]                           #d[65:67]
          pca_list$discard <- raw[,c("daft", "don", "dfin", "dsmel", "dtast")]              #d[68:72] all
          pca_list$foodwaste <-  raw[,c("fsig", "fbad", "fwron", "fcons", "fhand")]         #d[73:77]
          pca_list$social <- raw[,c("shsoc", "promsu")]                                     #d[78:79]
          
        # 2.6.3). Run the pca function
          pca_comp <- lapply(pca_list, prcomp)
        
        # 2.6.4). Extract the PCA scores
             
          pca_loadings <- list()
          out <- lapply(1:length(pca_list), function (i){
            print (i)
            
            pca_loadings[[i]] <- pca_comp[[i]]$x[,1]
            
          })
        
        # 2.6.5). Add the pca_scores to the main dataset
          pca_data_frame <- as.data.frame(out, col.names = c("labelling", "discard", "foodwaste", "social"))
          raw <- cbind(raw, pca_data_frame)
        
      # 2.7). Select complete answers
        raw <- raw %>% drop_na(C1)
        
      # 2.8). Take the raw qualtrics data and create a "long" dataset with one row for each of the four conjoint questions ###
      # The new columns created are called "question" and "answer".  Note that we remove the "C" from the question values and convert it to numeric for subsequent join ###
        tmp.long <- raw %>% pivot_longer(cols = C1:C4, names_to = "question", names_prefix = "C", names_transform = list(question=as.integer), values_to = "answer")
    
      # 2.9). Merge the "long" dataset with the design matrix by question set and by question to create final "long" dataset with 12 rows per respondent ###
      # Also create a new variable called "select" that equals 1 when the respondents choice answer equals the choice option for a given question.
        final.long <- tmp.long %>% left_join(design, by=c("vers_cbconjoint" = "question_set", "question" = "question")) %>% mutate(select = as.integer(answer==choice))

      #2.10).  Export the final dataset 
        write_csv(final.long, file = paste(dir$clean_data,"/final_long.csv",sep = "")) 

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# 3). Process the data and clean it for the mlogit.data
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      
      # 3.1). Load the data
        choicedata <- read.csv(paste(dir$clean_data,"/final_long.csv", sep = ""), header=T, na.strings=c("","NA"))
      
      # 3.2). Recode the attribute variables. 
      
        # 3.2.1). Milk type: 1 is organic and 2 is conventional. Set 2 as 0. 
          choicedata$Milk.type <- ifelse(choicedata$Milk.type == 2, 0, 1)
        
        # 3.2.2). Date label type: 1 is static and 2 is smart. Set 2 as 1. 
          choicedata$Date.label.type <- ifelse(choicedata$Date.label.type == 2, 1, 0)
      
        # 3.2.3). Additional information label: 1 is ecolable and 2 is none. Set 2 as 0. 
          choicedata$Additional.information.label <- ifelse(choicedata$Additional.information.label == 2, 0, 1)
      
        # 3.2.3). Date on label: 1 is tomorrow, 2 is 7 days from today, 3 is 18 days from today, and 4 is 30 days from today. Set: 1 as 1, 2 as 7, 3 as 18, and 4 as 30. 
                # Price: 1 is $2.55, 2 is $3.28, 3 is $ 4.39, 4 is $5.31, 5 is $6.28, and 6 is $7.20. 
      
          # Set the variables as characters
            choicedata$Date.on.label<- as.character(choicedata$Date.on.label)
            choicedata$Price<- as.character(choicedata$Price)
      
          # Recode the variables
            choicedata <- choicedata %>% mutate(Date.on.label=recode(Date.on.label, "1"="1", "2"="7", "3"="18", "4"="30"),
                                          Price = recode(Price,"1"="2.55", "2"="3.28", "3"="4.39", "4"="5.31", "5"="6.28", "6"="7.20" ))
      
          # Set the variables as numeric
            choicedata$Date.on.label <- as.numeric(choicedata$Date.on.label)
            choicedata$Price <- as.numeric(choicedata$Price)
      
      # 3.3). Rename/edit variables to use later on mlogit.data()
        
        # Rename column names to alternatives and choice. 
          colnames(choicedata)[106] <- "alternatives"
          colnames(choicedata)[112] <- "choice"
          
        # Change the opt-out option to empty and drop NAs (needed for mlogit.data)
          choicedata$choice[choicedata$answer == 4] <- ""
      
        # Create unique identifiers: Concatenate ID + Question.
          df_ID <-choicedata %>% group_by(prolific_pid) %>% summarise(n())
          df_ID$number <- seq(1:nrow(df_ID))
          df_ID$`n()` <- NULL
          choicedata <- merge(choicedata,df_ID,by="prolific_pid")
          colnames(choicedata)[ncol(choicedata)] <- "ID"
          choicedata$Idquestion <- paste0(choicedata$ID,"", choicedata$question)
      
        # Set choice as logical (needed for mlogit.data)
          choicedata$choice<- as.numeric(choicedata$choice)
          choicedata$choice<- as.logical(choicedata$choice)
      
        # Save the new dataset
          write_csv(choicedata, file = paste(dir$clean_data,"/choicedata.csv",sep = "")) 

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =      
# 4). Run the main model
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      
      # 4.1). Define the data for the choice model
        data <- mlogit.data(data = choicedata, choice="choice", shape="long", alt.var="alternatives", id.var="ID", chid.var="Idquestion")
        View(as.data.frame(data))
      
      # 4.2). Run the main model - Results are shown in Table 7. 
        set.seed(12345)
        mixmilktrt <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                           Milk.type:group + Date.label.type:group + Additional.information.label:group + Date.on.label:group +
                           Date.label.type:Additional.information.label + Date.label.type:Additional.information.label:group| -1,
                           data = data, 
                           model = 'mixl', 
                           panel = TRUE,
                           ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                           R = 500, 
                           halton = NULL)
      
        summary(mixmilktrt)
      
      # 4.3). Generate WTP - Results are shown in Table 7. 
        wtp.gmnl(mixmilktrt, wrt = "Price")

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =    
# 5). Characterizing heterogeneity - Results are shown in Table 8 & Appendix 1. 
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
      
      # 5.1). Mixl milk: interaction with age
        mixmilk_age <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                            Milk.type:age + Date.label.type:age + Additional.information.label:age + Date.on.label:age| -1,
                            data = data, 
                            model = 'mixl', 
                            panel = TRUE,
                            ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                            R = 500, 
                            halton = NULL)
        
        summary(mixmilk_age)
      
      # 5.2). Mixl milk: interaction with gender
        mixmilk_gender <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                               Milk.type:gender + Date.label.type:gender + Additional.information.label:gender + Date.on.label:gender | -1,
                               data = data, 
                               model = 'mixl', 
                               panel = TRUE,
                               ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                               R = 500, halton = NULL)
        
        summary(mixmilk_gender)
      
      # 5.3). Mixl milk: interaction with education
        mixmilk_edu <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                            Milk.type:edu + Date.label.type:edu + Additional.information.label:edu + Date.on.label:edu| -1,
                            data = data, 
                            model = 'mixl', 
                            panel = TRUE,
                            ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                            R = 500, 
                            halton = NULL)
      
        summary(mixmilk_edu)
      
      # 5.4). Mixl milk: interaction with milk purchase frequency
        mixmilk_purch_freq <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                   Milk.type:milk_purch_freq + Date.label.type:milk_purch_freq + Additional.information.label:milk_purch_freq + Date.on.label:milk_purch_freq| -1,
                                   data = data,
                                   model = 'mixl',
                                   panel = TRUE,
                                   ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                                   R = 500, 
                                   halton = NULL,
                                   na.action = na.exclude)
      
        summary(mixmilk_purch_freq)
      
      # 5.5). Mixl milk: interaction with milk consumption rate
        mixmilk_cons_freq <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                  Milk.type:milk_cons_freq + Date.label.type:milk_cons_freq + Additional.information.label:milk_cons_freq + Date.on.label:milk_cons_freq| -1,
                                  data = data,
                                  model = 'mixl',
                                  panel = TRUE,
                                  ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                                  R = 500, 
                                  halton = NULL,
                                  na.action = na.exclude)
        
        summary(mixmilk_cons_freq)
      
      # 5.6). Mixl milk: Milk fat content
        mixmilk_fat_cont <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                 Milk.type:milk_fat + Date.label.type:milk_fat + Additional.information.label:milk_fat + Date.on.label:milk_fat| -1,
                                 data = data,
                                 model = 'mixl',
                                 panel = TRUE,
                                 ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                                 R = 500, 
                                 halton = NULL,
                                 na.action = na.exclude)
      
        summary(mixmilk_fat_cont)
      
      # 5.7). Mixl milk: Milk size
        mixmilk_size <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                             Milk.type:milk_size + Date.label.type:milk_size + Additional.information.label:milk_size + Date.on.label:milk_size| -1,
                             data = data,
                             model = 'mixl',
                             panel = TRUE,
                             ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                             R = 500, 
                             halton = NULL,
                             na.action = na.exclude)
      
        summary(mixmilk_size)
      
      # 5.8). Mixl milk: Lactose free
        mixmilk_lactose_free <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                     Milk.type:lactose_free + Date.label.type:lactose_free + Additional.information.label:lactose_free + Date.on.label:lactose_free| -1,
                                     data = data,
                                     model = 'mixl',
                                     panel = TRUE,
                                     ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                                     R = 500, 
                                     halton = NULL,
                                     na.action = na.exclude)
      
        summary(mixmilk_lactose_free)
      
      # 5.9). Mixl milk: Non dairy
        mixmilk_non_dairy <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                  Milk.type:non_dairy + Date.label.type:non_dairy + Additional.information.label:non_dairy + Date.on.label:non_dairy| -1,
                                  data = data,
                                  model = 'mixl',
                                  panel = TRUE,
                                  ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                                  R = 500, 
                                  halton = NULL,
                                  na.action = na.exclude)
        
        summary(mixmilk_non_dairy)
      
      # 5.10). Mixl milk: online buying frequency
        mixmilk_online_freq <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                    Milk.type:online_freq + Date.label.type:online_freq + Additional.information.label:online_freq + Date.on.label:online_freq| -1,
                                    data = data,
                                    model = 'mixl',
                                    panel = TRUE,
                                    ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                                    R = 500, 
                                    halton = NULL,
                                    na.action = na.exclude)
      
        summary(mixmilk_online_freq)
      
      # 5.11). Mixl milk: read label
        mixmilk_read_label_freq <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                        Milk.type:read_label_freq + Date.label.type:read_label_freq + Additional.information.label:read_label_freq + Date.on.label:read_label_freq| -1,
                                        data = data,
                                        model = 'mixl',
                                        panel = TRUE,
                                        ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                                        R = 500, 
                                        halton = NULL,
                                        na.action = na.exclude)
      
        summary(mixmilk_read_label_freq)
      
      # 5.12). Mixl milk: use of qr code
        mixmilk_use_qr_code <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                    Milk.type:use_qr_code + Date.label.type:use_qr_code + Additional.information.label:use_qr_code + Date.on.label:use_qr_code| -1,
                                    data = data,
                                    model = 'mixl',
                                    panel = TRUE,
                                    ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                                    R = 500, 
                                    halton = NULL,
                                    na.action = na.exclude)
      
        summary(mixmilk_use_qr_code)
      
      # 5.13). Mixl milk: hh income

        mixmilk_hhinc <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                               Milk.type:hhinc_reg + Date.label.type:hhinc_reg + Additional.information.label:hhinc_reg + Date.on.label:hhinc_reg| -1,
                               data = data,
                               model = 'mixl',
                               panel = TRUE,
                               ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                               R = 500, 
                               halton = NULL,
                               na.action = na.exclude)
      
        summary(mixmilk_hhinc)
      
      names(choicedata)
      
      # 5.14). Mixl milk: marital status 
        mixmilk_ms <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                           Milk.type:ms + Date.label.type:ms + Additional.information.label:ms + Date.on.label:ms| -1,
                           data = data,
                           model = 'mixl',
                           panel = TRUE,
                           ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                           R = 500, 
                           halton = NULL,
                           na.action = na.exclude)
      
        summary(mixmilk_ms)
      
      # 5.15). Mixl milk: children
        mixmilk_children <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                 Milk.type:children + Date.label.type:children + Additional.information.label:children + Date.on.label:children| -1,
                                 data = data,
                                 model = 'mixl',
                                 panel = TRUE,
                                 ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                                 R = 500, 
                                 halton = NULL,
                                 na.action = na.exclude)
      
        summary(mixmilk_children)
      
      # 5.16). Mixl milk: hh size 
        data$hhsize <- as.numeric(data$hhsize)
        mixmilk_hhsize <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                               Milk.type:hhsize + Date.label.type:hhsize + Additional.information.label:hhsize + Date.on.label:hhsize| -1,
                               data = data,
                               model = 'mixl',
                               panel = TRUE,
                               ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                               R = 500, 
                               halton = NULL,
                               na.action = na.exclude)
      
        summary(mixmilk_hhsize)
      
      # 5.17). Mixl milk: region
        mixmilk_region <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                               Milk.type:region + Date.label.type:region + Additional.information.label:region + Date.on.label:region| -1,
                               data = data,
                               model = 'mixl',
                               panel = TRUE,
                               ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                               R = 500, 
                               halton = NULL,
                               na.action = na.exclude)
      
        summary(mixmilk_region)
      
      # 5.18). Mixl milk: ethnicity
        mixmilk_eth <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                            Milk.type:Ethnicity + Date.label.type:Ethnicity + Additional.information.label:Ethnicity + Date.on.label:Ethnicity| -1,
                            data = data,
                            model = 'mixl',
                            panel = TRUE,
                            ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                            R = 500, 
                            halton = NULL,
                            na.action = na.exclude)
      
        summary(mixmilk_eth)
      
      # 5.19). Mixl milk: familiarity with food waste
        mixmilk_familiarity <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                    Milk.type:foodwaste_familiar + Date.label.type:foodwaste_familiar + Additional.information.label:foodwaste_familiar + Date.on.label:foodwaste_familiar| -1,
                                    data = data,
                                    model = 'mixl',
                                    panel = TRUE,
                                    ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                                    R = 500, 
                                    halton = NULL,
                                    na.action = na.exclude)
      
        summary(mixmilk_familiarity)
      
      # 5.20). Mixl milk: purchasing location
        mixmilk_purchloc <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                 Milk.type:milk_purch_loc + Date.label.type:milk_purch_loc + Additional.information.label:milk_purch_loc + Date.on.label:milk_purch_loc| -1,
                                 data = data,
                                 model = 'mixl',
                                 panel = TRUE,
                                 ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                                 R = 500, 
                                 halton = NULL,
                                 na.action = na.exclude)
      
        summary(mixmilk_purchloc)
        
      # 5.21). Mixl milk: foodwaste perception
        mixmilk_foodwaste <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                    Milk.type:foodwaste + Date.label.type:foodwaste + Additional.information.label:foodwaste + Date.on.label:foodwaste| -1,
                                  data = data,
                                  model = 'mixl',
                                  panel = TRUE,
                                  ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                                  R = 500, 
                                  halton = NULL,
                                  na.action = na.exclude)
        
        summary(mixmilk_foodwaste)
        
      # 5.22). Mixl milk: labelling perception
        mixmilk_labelling <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                    Milk.type:labelling + Date.label.type:labelling + Additional.information.label:labelling + Date.on.label:labelling| -1,
                                  data = data,
                                  model = 'mixl',
                                  panel = TRUE,
                                  ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                                  R = 500, 
                                  halton = NULL,
                                  na.action = na.exclude)
        
        summary(mixmilk_labelling)
        
      # 5.23). Mixl milk: social
        mixmilk_social <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                 Milk.type:social + Date.label.type:social + Additional.information.label:social + Date.on.label:social| -1,
                               data = data,
                               model = 'mixl',
                               panel = TRUE,
                               ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                               R = 500, 
                               halton = NULL,
                               na.action = na.exclude)
        
        summary(mixmilk_social)
        
      # 5.24). Mixl milk: discarding
        mixmilk_disc <- gmnl(choice ~ Price + Milk.type + Date.label.type + Additional.information.label + Date.on.label +
                                Milk.type:discard + Date.label.type:discard + Additional.information.label:discard + Date.on.label:discard| -1,
                              data = data,
                              model = 'mixl',
                              panel = TRUE,
                              ranp = c(Milk.type = 'n', Additional.information.label = 'n', Date.label.type = "n", Date.on.label = "n"), 
                              R = 500, 
                              halton = NULL,
                              na.action = na.exclude)
        
        summary(mixmilk_disc)
        
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 6). Descriptive data
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
         
      # 6.1). Demographic characteristics
        descriptives <- raw[-c(378, 240, 419, 99, 81, 481, 439),] # The GMNL algorithm discards respondents with the opt-out option. The total N was 498. 
        descriptives$hhsize <- as.numeric(descriptives$hhsize)
        descriptives$hhsize[is.na(descriptives$hhsize)] <- 0
        demo_milk <- descriptives %>% select(primary_shopper, group, age, gender, edu, ms, children, hhinc_reg , region, white, 
                                             middle_east, black, native, asian, pacific, hispanic, race_othr, hhsize)
        
        demo_table <- demo_milk %>% tbl_summary(by = group, missing = "no",
                                    type = hhsize ~ "continuous2",
                                    statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
            modify_caption("**Sample demographic characteristics**") %>%
            bold_labels() %>%
            add_overall()
        demo_table
          
        # Extract the table
        demo_table %>%
            as_gt() %>%
            gt::gtsave(filename = paste(dir$tables,"/Demographics.docx",sep=""))
          
      # 6.2). Behavioral characteristics 
        demo_milk_behaviors <- descriptives %>% select(group, milk_purch_freq, milk_cons_freq, milk_purch_loc, milk_fat, milk_size, lactose_free, non_dairy, nd_soy, nd_almond, 
                                                         nd_oat, nd_coconut, nd_rsn_vegan, nd_rsn_lactose, nd_rsn_shelflife, nd_rsn_inflammation, nd_rsn_antibiotic, online_freq, 
                                                         read_label_freq, use_qr_code, foodwaste_familiar)
          
          
        beh_table <- demo_milk_behaviors %>% tbl_summary(by = group, missing = "no",
                                              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
            modify_caption("**Sample behavioral characteristics**") %>%
            bold_labels() %>%
            add_overall()
        beh_table
          
        # Extract the table
          beh_table %>%
            as_gt() %>%
            gt::gtsave(filename = paste(dir$tables,"/Behaviors.docx",sep=""))
          
      # 6.3). Perceptions
          
        # Setting the perceptions data to numeric
          demo_milk_perceptions <- descriptives %>% select(group, upmil, dqua, besel, daft, don, dfin, dsmel, dtast, fsig, 
                                                        fbad, fwron, fcons, fhand, shsoc, promsu)
            
          demo_milk_perceptions$group <- as.factor(demo_milk_perceptions$group)
          demo_milk_perceptions[sapply(demo_milk_perceptions, is.character)] <- lapply(demo_milk_perceptions[sapply(demo_milk_perceptions, is.character)], as.numeric)
          demo_milk_perceptions[,c(raw_new_names)][is.na(demo_milk_perceptions[,c(raw_new_names)])] <- 0 # If needed: Set the NA values to 0
            
          # Generate the table
            perc_table <- demo_milk_perceptions %>% tbl_summary(by = group, missing = NULL,
                                                  type = list(upmil ~ "continuous2", dqua ~ "continuous2", besel ~ "continuous2", daft ~ "continuous2",
                                                              don ~ "continuous2", dfin ~ "continuous2", dsmel ~ "continuous2", dtast ~ "continuous2",
                                                              fsig ~ "continuous2", fbad ~ "continuous2", fwron ~ "continuous2", fcons ~ "continuous2",
                                                              fhand ~ "continuous2", shsoc ~ "continuous2", promsu ~ "continuous2"),
                                                  statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
              modify_caption("**Sample perceptions**") %>%
              bold_labels() %>%
              add_overall()
            perc_table
            
          # Extract the table
            perc_table %>%
              as_gt() %>%
              gt::gtsave(filename = paste(dir$tables,"/Perceptions.docx",sep=""))

      # The end
      