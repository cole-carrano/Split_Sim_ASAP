# Split_Sim_ASAP
For cross test simulations of ASAP assessment models with split indices

The file Sim_split_functions contains all the necessary functions for doing cross tests on ASAP models with split surveys. All of this code is in some way adapted from simASAP or other code from Chris Legault (@cmLegault), so thank you to him for providing the foundations for this. These functions were created originally for American plaice in the New England region, so many of the functions are specific to that model. Warning: There is probably a lot of hard coding in these functions, and maybe some other sloppy stuff. But it should work. Descriptions of each function below:

split_survey - This was directly from Chris Legault, I take no credit for this. Splits a survey at a given year into two separate indices

split_survey.2 - I ran into some issues when trying to use split_survey back to back on the same file. I believe the issues were in the comments section of the ASAP file. I had to create this for splitting a second survey after using the split_survey function once. If you get an error when using this that says something about "cut1", it's also related to the comments section. Check the number of spaces in the names of the survey data in the .dat file comments

sim_then_split_ASAP - This is a direct adaptation of Chris' simASAP function which simulates ASAP files and optionally fits them (cmlegault/simASAP). sim_then_split_ASAP follows the same steps for simulating the .dat files, but then takes the first two indices, splits them into four separate (hard coded to split at year 2009), and creates and optionally fits those new files. Needs both split_survey and split_survey.2 loaded

unsplit_survey - An adaptation of split_survey which I named "unsplit" instead of "combine" for some reason. This combines four split surveys into two full ones. What it actually does is create two new indices, combine the data from the split indices, and turn off the split indices in the file. Definitely has some hard coding, For example, I think it assumes two split indices which are listed in a specifc order: season1_old, season2_old, season1_new, season2_new.

sim_then_unsplit_ASAP - Just like sim_then_split_ASAP but instead of splitting indices after simulation it combines them using unsplit_survey. Watch out for hard coding of directories but more likely of survey numbers and
implied data structures of the ASAP files.

PlotSimSplitASAP- Minor edits to hard code some directories into the PlotSimASAP function from Chris

Plot_q_simASAP and Plot_isel_simASAP - both are functions to pull out catchability and survey selectivity of simulated data files for comparison. Creates a df with relevant values

sim_split_M - Special version of sim_then_splitASAP which creates a situation with a changing natural mortality (M) to test if the splitting of the index could estimate a change in survey catchability that aliases the change in M. Does the same thing as sim_then_splitASAP but adds a change in M to the operating model file before simulation.
