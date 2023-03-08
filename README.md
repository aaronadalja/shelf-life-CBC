milk_data_test.zip" has all the necessary elements to produce the results for  Consumer willingness to pay for shelf life of high-temperature-short-time 
(HTST)-pasteurized fluid milk: Implications for smart labeling and food 
waste reduction

You will find three main folders: "code", "data", and "output".

1). code: You will find three important files:  
	1.1). "code.proj" is and RProject document that contains all the necessary information 
		to produce the results for the mixed logit model, and descriptive data.  
        1.2). "code_R.R" is an RFile document that contains the code and annotations 
		to produce the results. It is divided into the following sections: 
			1). Preliminaries
			2). Loading and cleaning the data
			3). Process the data and clean it for the mlogit.data
			4). Run the main MIXL model
       			5). Characterizing heterogeneity
    			6). Descriptive data
	1.3). "functions.R" is and RFile document that contains all the required functions to run the code.
		We created a factorise function to transform Likert data to numeric.

2). data: You will find four datasets in this file: 
	2.1). "milk_full.csv" is a csv. excel file that contains all the data collected during the survey that
		was carried out on February 8, 2021.
	2.2). "design_full.csv" is a csv. excel file that contains the qualtrics design for each set of options
		presented during the experiment.  
	2.2). "regions_csv" is a csv. excel file that contains the key to aggregate the states into four main 
		regions. 
	2.3). "household_income" is a csv. excel file that contains the key to aggregate the household income 
		into four main variables. 

3) output: You will find two subfolders in this file: 
	3.1). "clean data" is a folder that contains the datasets after each cleaning process. 
	3.2). "tables" is a folder that will contain the descriptives tables. 

 
