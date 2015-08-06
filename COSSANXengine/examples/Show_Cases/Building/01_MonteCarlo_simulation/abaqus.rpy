# -*- coding: mbcs -*-
#
# Abaqus/CAE Version 6.7-3 replay file
# Internal Version: 2007_08_06-15.53.37 79879
# Run by bg on Wed Sep 15 14:39:05 2010
#

# from driverUtils import executeOnCaeGraphicsStartup
# executeOnCaeGraphicsStartup()
#: Executing "onCaeGraphicsStartup()" in the site directory ...
from abaqus import *
from abaqusConstants import *
session.Viewport(name='Viewport: 1', origin=(1.36719, 1.36719), width=201.25, 
    height=135.625)
session.viewports['Viewport: 1'].makeCurrent()
from driverUtils import executeOnCaeStartup
executeOnCaeStartup()
execfile('generate_report.py', __main__.__dict__)
#: Model: /home/bg/workspace/COSSAN-X_SVN/COSSANXengine/examples/Use_Cases/BTMK/Building/01_MonteCarlo_simulation/Building.odb
#: Number of Assemblies:         1
#: Number of Assembly instances: 0
#: Number of Part instances:     1
#: Number of Meshes:             1
#: Number of Element Sets:       117
#: Number of Node Sets:          9
#: Number of Steps:              2
#* OdiError: Invalid frame index: 1
#* 
#* Available frame indices: 0 - 0
#* File "generate_report.py", line 21, in ?
#*     outputPosition=ELEMENT_CENTROID, variable=(('S', INTEGRATION_POINT, ((
