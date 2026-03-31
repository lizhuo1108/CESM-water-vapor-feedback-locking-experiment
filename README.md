# Brief introduction
Water vapor feedback locking experiment is based on the idea that you feed the radiation module in the model code with constant year-to-year water vapor content, say from a control experiment. In other words, only modify the radiation effects of water vapor. You can choose to do either longwave part, shortwave part, or both. A good example is given by "The Role of Water Vapor Feedback in Unperturbed Climate Variability and Global Warming" by Hall and Manabe (1999).

# Getting started
You need to be able to run the CESM model. Check the radiation code used in the model. Normally the atmospheric component-CAM used RRTMG scheme. 

# Codes
Includes the radiation codes written in Fortran (physpkg.F90; prescribed_wv.F90; radiation.F90). These codes are modified based on the origional codes from RRTMG. restart_physics.F90 is the code I write to read the water vapor content from the control run. 

# Author
Li Zhuo
