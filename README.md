# Master_Thesis
Codes for the Master Thesis to obtain the degree of Master in Computer Science with Specialization in Data Science and Technology at Delft University of Technology

# Title:  F-formations and Social Contact in Children: Exploring Dyadic Interactions and Modeling Groups

These are files to conduct the whole analysis for the thesis. Mind that there are 3 verisons of data for use. Also, features using with P(position only) or PO(position and orientation).


- children_fformation_extraction_130.ipynb

To explore the dataset and some preprocessing steps. Do the whole process for the day 1.30. Also the parameter choice and result visulation.

- children_extraction_alldays.ipynb

Do the whole extraction proces for all days

- idiap_test.ipynb

To verify the implementation of DS and affinity building on idiap test

- homophily_socialcontacts.ipynb

Calculate the social contact ratio as the original homophil effect paper. This file is to build csv file to conduct dydadic analysis as theirs.

- Ratio_individual_in_groups.ipynb

This file is to build the csv file for mixed effect study incoporating group properties: hl ratio, homophily degree and group size. The time one kid in a group/The time this kid in classroom ~ hl raio/homophily degree/group size * diagnosis

- plot_high_fre_ff.ipynb

This file is mainly used to plot all f-formation plots for the most frequent groups. Require the extracted f-formations are already stored.

- new_sc.R

R file to conduct mixed effect analysis, codes for results incluing group properties in the thesis are before line 41. The rest are for dyadic analysis

