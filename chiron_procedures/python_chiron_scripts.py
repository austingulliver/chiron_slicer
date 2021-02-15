# To add a new cell, type '# %%'
# To add a new markdown cell, type '# %% [markdown]'
# %%

import glob
from astropy.io import fits
import numpy as np
from astropy.stats import mad_std
from datetime import date




def clean_cr_from_images( target, mode, ccd_sum):
    
    ####################################
    #1) Cosntants to be used
    ####################################
    
    limit_mad= 10 # a value need to be 'limit_mad' times bigger than the rest of the values to be identified as outlier 
    
    
    
    
    ####################################
    #1) Filter files according to input
    ####################################    
    target_files= []
    
    for file in glob.glob("chi*.fits"):
        fits_image_filename = file[file.find("\\")+1:] 
        
        with fits.open(fits_image_filename) as hdul:

            h_object= hdul[0].header['OBJECT'].strip().lower()
            h_decker= hdul[0].header['DECKER'].strip().lower()
            h_ccd_sum =  hdul[0].header['CCDSUM'].strip().lower()          
            
            if h_object == target and h_decker == mode and h_ccd_sum == ccd_sum:
                #print (" -->> "+h_object + "  "+ h_decker + " " +h_ccd_sum)
                n_columns = int( hdul[0].header['NAXIS1'] )
                n_rows = int( hdul[0].header['NAXIS2'])                
                target_files.append(fits_image_filename)
               
    
    files_num =len(target_files)
    if (files_num <=2 ):
        raise  Exception ("There are not enough file to clean them from CR")
    else:
        print ("Number of files found: " +str (files_num))
        #print (target_files)
    
    #################################################################
    # 2) Create Median Master bias + Master Median Absolute Deviation
    #################################################################
    
    data_cube= np.zeros((n_rows,n_columns,files_num))
    
    for idx in range (files_num):
        with fits.open(target_files[idx]) as hd:
            data = (hd[0].data).copy()            
            data_cube[:,:,idx ] = data

    master_median = np.median(data_cube, axis=2 )
    master_abs_mad = mad_std(data_cube, axis=2 )
    
    #################################################################
    # 2) Cleaning CR from every image + re writing on disk
    #################################################################   
    
    today = date.today() # To be used in all headers
    file_date = today.strftime("%b-%d-%Y")
    
    for f_idx in range (files_num):
        with fits.open(target_files[f_idx]) as hd0:
            #Header
            img_header=(hd0[0].header).copy()
            #cr-cleaned is a keyword used in down the road
            img_header['HISTORY']='CR-CLEANED : Cosmic Rays removed from this file on '+file_date+' by finding the MAD of each pixel in '+str(files_num)+ ' ' +target +' files.'


            #Data 
            img_data=(hd0[0].data).copy()            
            exclude = (img_data - master_median) / master_abs_mad > limit_mad
            img_data[exclude] = 0
            mask = np.multiply( exclude , master_median)
            img_data = np.add(img_data,  mask)    
            
        fits.writeto (target_files[f_idx], img_data, header=img_header, overwrite=True )
        print ("File : "+target_files[f_idx]+  "had "+str(np.count_nonzero(exclude))+"cr. It has been cleaned and overwritten successfully.")

        
        
            
            
        
       
    print(" - - - - -  End of Script - - - - - -  ")
            


print (mad_std('sssssss'))
# def __init__():
#     print(mad_std)

