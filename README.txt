
Chiron Software :
This a pipeline desing for CHIRON optamized specifically for the SLICER mode.
- It runs on Windows 
- At the beginning of the procedure "sorting_hat" there is a collection of paths. Add your path within this collection.
  E.g. 'C:\Users\mrstu': ctparfn = 'C:\Users\mrstu\idlworkspace_yalecalibration\all_yale_files/ctio.par'
  Note the second path must point to the ctio.par 
- Change the default directory 'rootdir' variable within ctio.par
- Two paths MUST be within in working directory Path 1 points to the chiron procedures and Path 2 points to the exiting IDL packages 
  Note Path 1 must be first and path 2 second just so any overwritten procedures created for CHIRON are read.
