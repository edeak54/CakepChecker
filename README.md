# CakepChecker (DEPRECATED)
Program using RStudio to calculate the symmetrical value on a face then using machine learning to compare with good looking datasets, to see how good looking a face is.

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

before running program atleast once, uncomment the install.packages then run the program.
after installed running with the install.packages commented should be fine.

check attractiveness:

-put .jpg image to use in 'Face' folder.
-run code (select all lines first).
-done
(note: all .jpg in 'Face' will be run by program, remove ones you don't want).



add training data:
-open 'training_data.csv'.
-add the RAW image (meaning non preprocessed images, the ones you usually will use) to 'Dataset'.
-add text following the same format as above data(file_directory,axisMin = landmarks on face,Asym = how much asymmetry(lower = better).
(IMPORTANT : DONT USE THE PROCESSED FILES (GRAYSCALED) TO PUT INSIDE 'Dataset', USE RAW IMAGES ONLY)
(note : if you dont know what axisMin to put, run the program using the image and program will tell the axismin, 
modify only the asymmetry value)





base lines :
0.001 - 0.01 sgt cakep
0.01 - 0.02 cakep
0.03 - 0.04 mayan
0.05 - 0.06 neutral
0.07 - 0.09 b aja
0.1 - 0.4 jelek
0.5++ omg
