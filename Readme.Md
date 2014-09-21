
Getting & Cleaning Data - Course Project
===


Hello there !

Here is a quick explanation about the run_analysis.R script submitted during the assignment.


Important : this script assumes that you downloaded and unzipped the package of data in your current working directory, respecting the original structure of the folders.


The Script is divided in 5 main parts :


1. First part : Introduction
===

Lines 1 - 9

Reminds the instructions for the assignment and remove all data currently used in your environment so that you can run the rest of the code efficiently


2. Second part : Merging the data
===

Lines 10 - 42

This chunk extract the raw data from the the different text files.

It combines : the measured data (trainX & testX) with the number identifiying the perticular subject (trainy, testy).

Finally, it names the columns of the "merged" data set, using the list of features available in the features.txt file.

This merged data set answers to the first question of the assignment.


3. Third part : Means and Standard Deviations
===

Lines 43 - 56

Here the grepl function help us to identify the feature names composed by "mean" or "std".

By subsetting the associated columns in the "merged" data set we obtain the "subset" data set that answers to the 2nd question of the assignment.


4. Fourth part : Naming the activities
===

Lines 57 - 72

This code extract the activity_labels.txt file in order to apply the right name accordingly with the associated numbers (see also the different activities in the Code Book) as asked in the 3rd question of the assignment.

The process is based on a "for loop".


5. Fifth part : Labelling the data set
===

Lines 73 - 79

There is no perticular code here : the labels were already applied during Part 2. The "names(subset)" line of code offers you the opportunity to assess whether this labelling is correct or not.


6. Sixth part : 
===

Lines 80 - 91

This last part answer to the fifth question of the assignment : the dplyr package allow to create the new dataset, aggregating the means and std by activity for each subjects of the set.