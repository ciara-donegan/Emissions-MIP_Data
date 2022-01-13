README
This folder contains the global and regional time series data from various models.

fixed_axes.csv:
	-For the first column, enter in the first row which method you want to use in order to determine y axes 
	in the ouput file. The choices are:

	1. group
	-If set to group then you must create a group name in the first row of a column and add the variables in
	that group below it. The script will then make sure each variable in a group is plotted with the same y 
	axes. NOTE: do not put a variable in more than one group as it will default to the axes of the last group
	it is listed in. You can have as many groups as you want

	2. fixed
	-If set to fixed then the script will determine the min and max values across all regions or experiments
	(depending on what you are sorting each run by) and set the output y axes to those values

	3. neither
	-The script runs as normal

NOTE: make sure the order of the variables under 'vars' in var_master_list.csv and 'Variable' in variables.csv are
      are in the same order. All stand-alone variables (ie mmrbc, emibc...) should be listed before any combined 
      variables (ie net_rad, tot_s...) in variables.csv

combined_variables.csv
	-The column headers are the name of the ocmbined variable and the values below are the variables joined in
	 order to make the combined variable. You can only join two at a time, so if you wanted to combine four 
	 variables you would need some intermediate steps and put them to the left of the final column (see for
	 example tot_s, which is a combination of dry_s and wet_s. Make sure to add these intermediates to 
	 variables.csv too).