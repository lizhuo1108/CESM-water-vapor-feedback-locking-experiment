# First step, run the control run (CTL), output the hourly specific humidity
Modify user_nl_cam:
```
fincl2='Q:I'
mfilt=1,4380
ndens=2,1
nhtfrq=0,-2
```
# After you get the model output, use cdo to combine Q from hourly model output into one file
```
cdo -select,name=Q /route/*.cam.h2.*.nc CTL_Q.nc
```
# Reference case
Create a new case and run it to longer timescale, such as 10 years. Make sure all other settings are same as in CTL
# Locked case
Find the year in CTL_Q.nc, say 0001. Create another case and modify user_nl_cam:
```
prescribed_wv_datapath = '/datapath/'
prescribed_wv_file = 'CTL_Q.nc'
prescribed_wv_type = 'CYCLICAL'
prescribed_wv_cycle_yr = '0001'
```
Next, put all the modified Fortran codes in .../SourceMods/src.cam/ and rebuild the CESM model:
```
case.build
```
If you have built (compiled) the model, clean all the built modules first:
```
case.clean --all
```
Then, run the model
# Then, run the model and compare the result to the reference case
