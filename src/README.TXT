There are 2 dirs:

1-) shiny-gui: 
The Simulator for boat rescue. 
In its current form it puts the BS to the same locations so that for many runs you can have the same simulation setting
You can change it to the clicked location go to server.R and search for "ADD BS ALWAYS TO THE CLICKED PT".
In the subdirs of "data" and "results" creates boat positions and simulations results respectively.


2-) process-results:
Here there are example data and result files (csv files) and script to process them.
After Simulator creates these csv files in data and results dirs copy
them to the same directory with the R script.

my_generate_latex_tables: Just calls the R script from the Linux commandline

my_get_latex_from_results-script-V1.R: You can use this script with RStudio. 
It create latex table for your paper in text files.

For example: 
sq-sim4bt11.txt: It is text file that contains 4 simulation results for Square CS Grid
with 11 boats generated at random positions.

