Source: forum thread
"https://class.coursera.org/getdata-011/forum/search?q=step+2#15-state-query=step+2"
author: Mohammed K. Barakat
#_----------------------



Greetings,

I came up with the below summary of what I think should go in each of the README and Codebook files based on what I read in the threads and in the lectures. I’d like to thank David Hood for his first post in this thread as most of this outline is extracted from his post.

Please let me know what you think. Is the below enough? Do we need to shuffle some contents from one file to the other?



What goes in the README.md:

    A brief description of the project (as described in the original README.txt)

    Give a brief description to each of the below two raw datasets:

        The inertia datasets (mention that they are not used directly in the script to produce the tidy data. However, they were used previously to produce the following type of dataset)

        The data set used in the script to produce the output tidy data (X,y,activity, features, subject files)

    Mention the scope of the script and the output tidy data. i.e. summarizing the mean and standard deviation (std) measurements per subject per activity by using the input dataset mentioned above.

    Mention the locations of the input (raw data) and output (tidy data). If your starting point to run the code is the unzipped "UCI HAR Dataset" in the working directory, and it should as per the project requirements, mention that in order to remove all doubt from a potential marker. Use something like "This script starts with the assumption that the Samsung data is available in an unzipped UCI HAR Dataset folder with the working directory pointing to this folder". Besides, I save the exported tidy data in the "UCI HAR Dataset" folder so that the marker will have it there after running the script.

    Make it clear for the marker to pre-install any package you used in the script. E.g. s/he may need to install the reshape2 package before running the script.

    A step-by-step explanation of how you approached the analysis. Breakdown of the requirements and what you did to complete each requirement. I.e. a descriptive translation of each line in the script. Example:

        Step 1 - read the X_test file…

        Step 2 – read the y_test file..

    Mention that you changed the activities into descriptive names. Give a list of the activities classes vs labels.

    If you changed the variable names, mention that and explain why you did that.

    Although the tidy data comprises the mean and std variables, mention the original variables (in the inertia datasets) and how they contributed to calculate the output mean and std variables in the tidy data.

    Guide the marker on how to read the tidy data file back into R by giving them the code for that. read.table(header=TRUE) {listing any settings you have changed from the default}".



What goes in the Codebook.md:

    A general description of the codebook and its contents.

    A list of the mean and std variables used in the tidy data showing the following details for each variable:

        Variable name (as in your tidy data)

        Variable class and/or number of characters/digits

        Variable unit (normalized for measurements)

        Description of the variable (as described in the original README and features_info)

        Variable standard values where applicable (e.g. list all activity labels)
