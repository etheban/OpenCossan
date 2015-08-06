# -*- coding: mbcs -*-
#
# Abaqus/CAE Version 6.8-2 replay file
# Internal Version: 2008_07_21-07.21.56 87172
# Run by ep on Wed Jul 27 15:48:22 2011
#

# from driverUtils import executeOnCaeGraphicsStartup
# executeOnCaeGraphicsStartup()
#: Executing "onCaeGraphicsStartup()" in the site directory ...
from abaqus import *
from abaqusConstants import *
session.Viewport(name='Viewport: 1', origin=(0.0, 0.0), width=615.912109375, 
    height=220.542846679688)
session.viewports['Viewport: 1'].makeCurrent()
session.viewports['Viewport: 1'].maximize()
from caeModules import *
from driverUtils import executeOnCaeStartup
executeOnCaeStartup()
openMdb(
    pathName='/home/ep/workspace/COSSAN-X_SVN/COSSANXengine/examples/FE_models/ABAQUS/MultistoryBuilding/Building.cae')
#: The model database "/home/ep/workspace/COSSAN-X_SVN/COSSANXengine/examples/FE_models/ABAQUS/MultistoryBuilding/Building.cae" has been opened.
session.viewports['Viewport: 1'].setValues(displayedObject=None)
p = mdb.models['Building'].parts['PART-1-1']
session.viewports['Viewport: 1'].setValues(displayedObject=p)
session.viewports['Viewport: 1'].view.setValues(nearPlane=82.3916, 
    farPlane=145.722, width=55.9102, height=38.9069, cameraPosition=(99.6044, 
    -48.2151, 49.7474), cameraUpVector=(-0.61617, 0.24684, 0.747933))
session.viewports['Viewport: 1'].view.setValues(nearPlane=83.0677, 
    farPlane=142.007, width=56.369, height=39.2262, cameraPosition=(90.7861, 
    67.4341, 11.7432), cameraUpVector=(-0.342532, -0.166763, 0.924587))
session.viewports['Viewport: 1'].view.setValues(nearPlane=82.8259, 
    farPlane=140.273, width=56.2049, height=39.112, cameraPosition=(-93.6729, 
    50.4948, 3.57012), cameraUpVector=(0.19992, -0.259591, 0.944799))
session.viewports['Viewport: 1'].view.setValues(nearPlane=84.5459, 
    farPlane=139.064, width=57.3721, height=39.9242, cameraPosition=(-103.473, 
    26.931, 14.7722), cameraUpVector=(0.353597, -0.194912, 0.914865))
a = mdb.models['Building'].rootAssembly
session.viewports['Viewport: 1'].setValues(displayedObject=a)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Side_wind')
session.viewports['Viewport: 1'].assemblyDisplay.setValues(loads=ON, bcs=ON, 
    predefinedFields=ON, connectors=ON)
mdb.models['Building'].loads['CFORCE-6'].setValues(cf2=-879.924, 
    distributionType=UNIFORM, field='')
mdb.models['Building'].loads['CFORCE-5'].setValues(cf2=-1690.42, 
    distributionType=UNIFORM, field='')
mdb.models['Building'].loads['CFORCE-4'].setValues(cf2=-1585.61, 
    distributionType=UNIFORM, field='')
mdb.models['Building'].loads['CFORCE-3'].setValues(cf2=-1459.76, 
    distributionType=UNIFORM, field='')
mdb.models['Building'].loads['CFORCE-2'].setValues(cf2=-1298.36, 
    distributionType=UNIFORM, field='')
mdb.models['Building'].loads['CFORCE-1'].setValues(cf2=-1397.82, 
    distributionType=UNIFORM, field='')
session.viewports['Viewport: 1'].view.setValues(nearPlane=81.8144, 
    farPlane=146.721, width=50.4904, height=38.6343, cameraPosition=(79.0887, 
    -82.3891, 41.4008), cameraUpVector=(-0.412431, 0.439125, 0.798166))
session.viewports['Viewport: 1'].view.setValues(nearPlane=82.7431, 
    farPlane=141.81, width=51.0635, height=39.0728, cameraPosition=(78.4478, 
    79.9656, 2.32292), cameraUpVector=(-0.416602, -0.0258184, 0.908723))
session.viewports['Viewport: 1'].view.setValues(nearPlane=80.8616, 
    farPlane=142.283, width=49.9023, height=38.1843, cameraPosition=(-84.5301, 
    62.3978, 23.5439), cameraUpVector=(0.560385, -0.0169462, 0.828059))
session.viewports['Viewport: 1'].view.setValues(nearPlane=80.8284, 
    farPlane=143.624, width=49.8818, height=38.1686, cameraPosition=(-83.9886, 
    24.5336, 70.8218), cameraUpVector=(0.757199, -0.276969, 0.591555))
session.viewports['Viewport: 1'].view.setValues(nearPlane=86.0242, 
    farPlane=138.146, width=53.0883, height=40.6221, cameraPosition=(-106.146, 
    5.4812, 26.8666), cameraUpVector=(0.490242, -0.0355648, 0.870861))
session.viewports['Viewport: 1'].view.setValues(nearPlane=87.0117, 
    farPlane=136.903, width=53.6978, height=41.0884, cameraPosition=(-107.447, 
    8.92121, 10.0901), cameraUpVector=(0.355973, -0.0231218, 0.93421))
session.viewports['Viewport: 1'].view.setValues(nearPlane=83.9137, 
    farPlane=139.71, width=51.7859, height=39.6255, cameraPosition=(-103.152, 
    27.479, 16.799), cameraUpVector=(0.384236, -0.149596, 0.911034))
session.viewports['Viewport: 1'].view.setValues(nearPlane=81.054, 
    farPlane=142.358, width=50.0211, height=38.2751, cameraPosition=(-92.6579, 
    48.2147, 28.8134), cameraUpVector=(0.42327, -0.281441, 0.861182))
session.viewports['Viewport: 1'].view.setValues(nearPlane=80.0575, 
    farPlane=143.747, width=49.4062, height=37.8046, cameraPosition=(-88.4142, 
    42.3983, 50.3864), cameraUpVector=(0.57508, -0.341502, 0.743411))
session.viewports['Viewport: 1'].view.setValues(nearPlane=79.9664, 
    farPlane=143.976, width=49.35, height=37.7616, cameraPosition=(-86.2627, 
    40.2103, 56.8447), cameraUpVector=(0.618889, -0.356235, 0.700052))
session.viewports['Viewport: 1'].view.setValues(nearPlane=79.6682, 
    farPlane=143.838, width=49.166, height=37.6208, cameraPosition=(-82.8976, 
    56.1695, 44.1076), cameraUpVector=(0.485549, -0.391107, 0.781843))
session.viewports['Viewport: 1'].view.setValues(nearPlane=80.4192, 
    farPlane=143.397, width=49.6295, height=37.9754, cameraPosition=(-90.0796, 
    40.3072, 48.99), cameraUpVector=(0.572267, -0.327016, 0.752045))
session.viewports['Viewport: 1'].assemblyDisplay.setValues(mesh=ON)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=SHADED)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(mesh=OFF)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=SHADED)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=HIDDEN)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=HIDDEN)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=HIDDEN)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=HIDDEN)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=HIDDEN)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=HIDDEN)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    renderStyle=WIREFRAME)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    renderStyle=WIREFRAME)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=HIDDEN)
session.viewports['Viewport: 1'].view.setValues(nearPlane=80.7925, 
    farPlane=142.949, width=49.8599, height=38.1519, cameraPosition=(-92.651, 
    39.7295, 43.3103), cameraUpVector=(0.549174, -0.281087, 0.787018))
session.viewports['Viewport: 1'].view.setValues(nearPlane=82.5302, 
    farPlane=141.212, width=31.2788, height=23.934, viewOffsetX=-5.36729, 
    viewOffsetY=2.38424)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=SHADED, 
    engineeringFeatures=ON)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=HIDDEN)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    renderStyle=WIREFRAME)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=SHADED)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=HIDDEN)
session.viewports['Viewport: 1'].assemblyDisplay.geometryOptions.setValues(
    geometryHiddenEdges=ON)
session.viewports['Viewport: 1'].assemblyDisplay.geometryOptions.setValues(
    highlightMode=ISOLINES)
session.viewports['Viewport: 1'].assemblyDisplay.geometryOptions.setValues(
    highlightMode=STIPPLED)
session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
    meshVisibleEdges=ALL)
session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
    meshVisibleEdges=FREE)
session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
    meshVisibleEdges=FEATURE)
session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
    meshVisibleEdges=EXTERIOR)
session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
    meshVisibleEdges=ALL)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    renderBeamProfiles=ON)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    renderBeamProfiles=OFF)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    renderBeamProfiles=ON)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=SHADED)
session.viewports['Viewport: 1'].assemblyDisplay.geometryOptions.setValues(
    datumCoordSystems=OFF)
session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
    nonBottomUpGeom=ON)
session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
    nonBottomUpGeom=OFF)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    otherSymbolSize=10)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    arrowSymbolSize=10)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    arrowSymbolSize=25)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    otherSymbolSize=6, arrowSymbolSize=14)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    arrowSymbolSize=8)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    arrowSymbolSize=6)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    otherSymbolSize=2)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    otherSymbolSize=6)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    arrowSymbolSize=10)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    faceSymbolDensity=2)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    faceSymbolDensity=5)
session.viewports['Viewport: 1'].assemblyDisplay.adaptiveMeshConstraintOptions.setValues(
    displacementAdaptiveMeshConstraint=OFF, velocityAdaptiveMeshConstraint=OFF)
session.viewports['Viewport: 1'].assemblyDisplay.adaptiveMeshConstraintOptions.setValues(
    displacementAdaptiveMeshConstraint=ON, velocityAdaptiveMeshConstraint=ON)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    faceSymbolDensity=1, edgeSymbolDensity=1)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    faceSymbolDensity=4, edgeSymbolDensity=4)
session.viewports['Viewport: 1'].assemblyDisplay.interactionOptions.setValues(
    surfaceContact=OFF, selfContact=OFF, elasticFoundation=OFF, 
    filmCondition=OFF, radiationAmbient=OFF, concentratedFilmCondition=OFF, 
    concentratedRadiationToAmbient=OFF, incidentWave=OFF, cavityRadiation=OFF, 
    acousticImpedance=OFF, actuatorSensor=OFF, cyclicSymmetry=OFF)
session.viewports['Viewport: 1'].view.setValues(nearPlane=80.6428, 
    farPlane=143.099, width=51.6804, height=39.5448, viewOffsetX=0.958094, 
    viewOffsetY=1.59436)
session.viewports['Viewport: 1'].view.setValues(nearPlane=83.1152, 
    farPlane=140.81, width=53.2648, height=40.7573, cameraPosition=(-101.957, 
    21.8757, 33.3804), cameraUpVector=(0.538318, -0.0800939, 0.838927), 
    viewOffsetX=0.987468, viewOffsetY=1.64324)
session.viewports['Viewport: 1'].view.setValues(nearPlane=80.8118, 
    farPlane=142.655, width=51.7887, height=39.6278, cameraPosition=(-93.2395, 
    46.2547, 30.8461), cameraUpVector=(0.486465, -0.196576, 0.851299), 
    viewOffsetX=0.960102, viewOffsetY=1.5977)
session.viewports['Viewport: 1'].view.setValues(nearPlane=81.5816, 
    farPlane=141.958, width=52.282, height=40.0053, cameraPosition=(-96.2901, 
    40.6728, 29.4355), cameraUpVector=(0.465995, -0.210832, 0.859301), 
    viewOffsetX=0.969248, viewOffsetY=1.61292)
session.viewports['Viewport: 1'].view.setValues(nearPlane=82.4187, 
    farPlane=141.348, width=52.8184, height=40.4158, cameraPosition=(-100.337, 
    29.0414, 31.4977), cameraUpVector=(0.516513, -0.116252, 0.848351), 
    viewOffsetX=0.979193, viewOffsetY=1.62947)
session.viewports['Viewport: 1'].view.setValues(nearPlane=85.5771, 
    farPlane=139.05, width=54.8424, height=41.9646, cameraPosition=(-103.327, 
    -7.52647, 40.5131), cameraUpVector=(0.590407, 0.136336, 0.795508), 
    viewOffsetX=1.01672, viewOffsetY=1.69191)
session.viewports['Viewport: 1'].view.setValues(nearPlane=80.6564, 
    farPlane=143.257, width=51.689, height=39.5517, cameraPosition=(-95.0802, 
    31.6122, 45.762), cameraUpVector=(0.534105, -0.371611, 0.759366), 
    viewOffsetX=0.958259, viewOffsetY=1.59463)
session.viewports['Viewport: 1'].view.setValues(nearPlane=82.6829, 
    farPlane=140.819, width=52.9877, height=40.5455, cameraPosition=(-99.1699, 
    37.2043, 21.6266), cameraUpVector=(0.42807, -0.141709, 0.892566), 
    viewOffsetX=0.982336, viewOffsetY=1.6347)
session.viewports['Viewport: 1'].view.setValues(nearPlane=81.8175, 
    farPlane=141.982, width=52.4331, height=40.1211, cameraPosition=(-98.8516, 
    30.2209, 35.8179), cameraUpVector=(0.530677, -0.180343, 0.828166), 
    viewOffsetX=0.972054, viewOffsetY=1.61759)
session.viewports['Viewport: 1'].view.setValues(nearPlane=82.3379, 
    farPlane=141.282, width=52.7666, height=40.3763, cameraPosition=(-99.5451, 
    34.0399, 26.9213), cameraUpVector=(0.467445, -0.15418, 0.870473), 
    viewOffsetX=0.978237, viewOffsetY=1.62788)
session.viewports['Viewport: 1'].assemblyDisplay.symbolOptions.setValues(
    arrowSymbolSize=7)
session.viewports['Viewport: 1'].view.setValues(nearPlane=82.6798, 
    farPlane=141.172, width=52.9861, height=40.544, cameraPosition=(-101.292, 
    25.0847, 32.5628), cameraUpVector=(0.48974, -0.232522, 0.840291), 
    viewOffsetX=0.982299, viewOffsetY=1.63464)
session.viewports['Viewport: 1'].view.setValues(nearPlane=82.6471, 
    farPlane=141.204, width=52.9652, height=40.528, cameraPosition=(-101.292, 
    25.0847, 32.5628), cameraUpVector=(0.533289, -0.0809279, 0.842053), 
    viewOffsetX=0.981911, viewOffsetY=1.63399)
session.viewports['Viewport: 1'].view.setValues(nearPlane=83.6853, 
    farPlane=140.631, width=53.6306, height=41.0371, cameraPosition=(-101.478, 
    8.82865, 43.5453), cameraUpVector=(0.629613, 0.0820946, 0.772559), 
    viewOffsetX=0.994246, viewOffsetY=1.65452)
session.viewports['Viewport: 1'].view.setValues(nearPlane=85.7216, 
    farPlane=138.381, width=54.9356, height=42.0357, cameraPosition=(-106.583, 
    6.69906, 23.3745), cameraUpVector=(0.473212, 0.075493, 0.877708), 
    viewOffsetX=1.01844, viewOffsetY=1.69478)
session.viewports['Viewport: 1'].view.setValues(nearPlane=81.6518, 
    farPlane=141.945, width=52.3275, height=40.04, cameraPosition=(-98.3726, 
    36.3401, 28.3401), cameraUpVector=(0.446983, -0.235786, 0.862908), 
    viewOffsetX=0.970088, viewOffsetY=1.61432)
session.viewports['Viewport: 1'].view.setValues(nearPlane=81.9966, 
    farPlane=141.6, width=52.5485, height=40.2091, cameraPosition=(-98.3726, 
    36.3401, 28.3401), cameraUpVector=(0.463667, -0.194989, 0.864287), 
    viewOffsetX=0.974185, viewOffsetY=1.62114)
session.viewports['Viewport: 1'].assemblyDisplay.loadOptions.setValues(
    concentratedForce=OFF, moment=OFF, pressure=OFF, pipePressure=OFF, 
    shellEdgeLoad=OFF, surfaceTraction=OFF, bodyForce=OFF, lineLoad=OFF, 
    gravity=OFF, boltLoad=OFF, pegLoad=OFF, rotationalInertialLoad=OFF, 
    coriolisForce=OFF, connectorForce=OFF, connectorMoment=OFF, 
    inertiaRelief=OFF, surfaceHeatFlux=OFF, bodyHeatFlux=OFF, 
    concentratedHeatFlux=OFF, concentratedPoreFluid=OFF, surfacePoreFluid=OFF, 
    concentratedCharge=OFF, concentratedCurrent=OFF, surfaceCharge=OFF, 
    surfaceCurrent=OFF, bodyCharge=OFF, bodyCurrent=OFF, inwardVolAccel=OFF, 
    concentratedConcentrationFlux=OFF, surfaceConcentrationFlux=OFF, 
    bodyConcentrationFlux=OFF, submodel=OFF)
session.viewports['Viewport: 1'].assemblyDisplay.loadOptions.setValues(
    concentratedForce=ON, moment=ON, pressure=ON, pipePressure=ON, 
    shellEdgeLoad=ON, surfaceTraction=ON, bodyForce=ON, lineLoad=ON, 
    gravity=ON, boltLoad=ON, pegLoad=ON, rotationalInertialLoad=ON, 
    coriolisForce=ON, connectorForce=ON, connectorMoment=ON, inertiaRelief=ON)
session.viewports['Viewport: 1'].assemblyDisplay.bcOptions.setValues(
    displacement=OFF, velocity=OFF, acceleration=OFF, symmetry=OFF, 
    connectorDisplacement=OFF, connectorVelocity=OFF, 
    connectorAcceleration=OFF, temperature=OFF, porePressure=OFF, 
    electricPotential=OFF, concentration=OFF, acousticPressure=OFF, 
    materialFlow=OFF, submodel=OFF)
session.viewports['Viewport: 1'].assemblyDisplay.bcOptions.setValues(
    displacement=ON, velocity=ON, acceleration=ON, symmetry=ON, 
    connectorDisplacement=ON, connectorVelocity=ON, connectorAcceleration=ON)
session.viewports['Viewport: 1'].lightOptions.setValues(ambientColor='#808080')
session.viewports['Viewport: 1'].lightOptions.setValues(materialShininess=50)
session.viewports['Viewport: 1'].lightOptions.setValues(materialShininess=95)
session.viewports['Viewport: 1'].lightOptions.lights[0].setValues(type=POINT)
session.viewports['Viewport: 1'].view.setValues(nearPlane=82.124, 
    farPlane=141.515, width=52.6301, height=40.2716, cameraPosition=(-99.1054, 
    34.2161, 28.8083), cameraUpVector=(0.469921, -0.189509, 0.862126), 
    viewOffsetX=0.975698, viewOffsetY=1.62366)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(mesh=ON, loads=OFF, 
    bcs=OFF, predefinedFields=OFF, connectors=OFF, engineeringFeatures=OFF)
session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
    meshTechnique=ON)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(mesh=OFF, loads=ON, 
    bcs=ON, predefinedFields=ON, connectors=ON)
session.viewports['Viewport: 1'].assemblyDisplay.meshOptions.setValues(
    meshTechnique=OFF)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Self_weight')
session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Initial')
session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Side_wind')
session.viewports['Viewport: 1'].assemblyDisplay.setValues(mesh=ON)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=SHADED)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(mesh=OFF)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=SHADED)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=SHADED)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    renderStyle=WIREFRAME)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=HIDDEN)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(mesh=ON)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=SHADED)
session.viewports['Viewport: 1'].enableMultipleColors()
session.viewports['Viewport: 1'].setColor(initialColor='#BDBDBD')
cmap = session.viewports['Viewport: 1'].colorMappings['Load']
cmap.updateOverrides(overrides={'CFORCE-2':(False, )})
session.viewports['Viewport: 1'].setColor(colorMapping=cmap)
session.viewports['Viewport: 1'].disableMultipleColors()
session.viewports['Viewport: 1'].enableMultipleColors()
session.viewports['Viewport: 1'].setColor(initialColor='#BDBDBD')
cmap = session.viewports['Viewport: 1'].colorMappings['Load']
cmap.updateOverrides(overrides={'CFORCE-2':(True, )})
session.viewports['Viewport: 1'].setColor(colorMapping=cmap)
session.viewports['Viewport: 1'].disableMultipleColors()
session.viewports['Viewport: 1'].enableMultipleColors()
session.viewports['Viewport: 1'].setColor(initialColor='#BDBDBD')
cmap = session.viewports['Viewport: 1'].colorMappings['Load']
session.viewports['Viewport: 1'].setColor(colorMapping=cmap)
session.viewports['Viewport: 1'].disableMultipleColors()
session.viewports['Viewport: 1'].enableMultipleColors()
session.viewports['Viewport: 1'].setColor(initialColor='#BDBDBD')
cmap = session.viewports['Viewport: 1'].colorMappings['Assembly']
session.viewports['Viewport: 1'].setColor(colorMapping=cmap)
session.viewports['Viewport: 1'].disableMultipleColors()
session.viewports['Viewport: 1'].enableMultipleColors()
session.viewports['Viewport: 1'].setColor(initialColor='#BDBDBD')
cmap = session.viewports['Viewport: 1'].colorMappings['Load']
session.viewports['Viewport: 1'].setColor(colorMapping=cmap)
session.viewports['Viewport: 1'].disableMultipleColors()
session.viewports['Viewport: 1'].enableMultipleColors()
session.viewports['Viewport: 1'].setColor(initialColor='#BDBDBD')
cmap = session.viewports['Viewport: 1'].colorMappings['Load']
session.viewports['Viewport: 1'].setColor(colorMapping=cmap)
session.viewports['Viewport: 1'].disableMultipleColors()
session.viewports['Viewport: 1'].enableMultipleColors()
session.viewports['Viewport: 1'].setColor(initialColor='#BDBDBD')
cmap = session.viewports['Viewport: 1'].colorMappings['Assembly']
session.viewports['Viewport: 1'].setColor(colorMapping=cmap)
session.viewports['Viewport: 1'].disableMultipleColors()
session.viewports['Viewport: 1'].setColor(globalTranslucency=True)
session.viewports['Viewport: 1'].setColor(globalTranslucency=False)
session.viewports['Viewport: 1'].setColor(translucency=0.4)
session.viewports['Viewport: 1'].setColor(translucency=0.2)
session.viewports['Viewport: 1'].setColor(translucency=0.1)
session.viewports['Viewport: 1'].setColor(translucency=0.1)
session.viewports['Viewport: 1'].setColor(translucency=0.2)
session.viewports['Viewport: 1'].setColor(translucency=0.4)
session.viewports['Viewport: 1'].setColor(translucency=0.5)
session.viewports['Viewport: 1'].setColor(translucency=0.7)
session.viewports['Viewport: 1'].setColor(translucency=0.8)
session.viewports['Viewport: 1'].setColor(translucency=0.8)
session.viewports['Viewport: 1'].setColor(translucency=0.7)
session.viewports['Viewport: 1'].setColor(translucency=0.6)
session.viewports['Viewport: 1'].setColor(translucency=0.5)
session.viewports['Viewport: 1'].setColor(translucency=0.4)
session.viewports['Viewport: 1'].setColor(translucency=0.3)
session.viewports['Viewport: 1'].setColor(translucency=0.3)
session.viewports['Viewport: 1'].setColor(globalTranslucency=True)
session.viewports['Viewport: 1'].setColor(translucency=0.4)
session.viewports['Viewport: 1'].setColor(translucency=0.7)
session.viewports['Viewport: 1'].setColor(translucency=0.8)
session.viewports['Viewport: 1'].setColor(translucency=1)
session.viewports['Viewport: 1'].setColor(translucency=0.9)
session.viewports['Viewport: 1'].setColor(translucency=0.8)
session.viewports['Viewport: 1'].setColor(translucency=0.6)
session.viewports['Viewport: 1'].setColor(translucency=0.4)
session.viewports['Viewport: 1'].setColor(translucency=0.3)
session.viewports['Viewport: 1'].setColor(translucency=0.2)
session.viewports['Viewport: 1'].setColor(translucency=0)
session.viewports['Viewport: 1'].setColor(translucency=0.1)
session.viewports['Viewport: 1'].setColor(translucency=0.2)
session.viewports['Viewport: 1'].setColor(translucency=0.3)
session.viewports['Viewport: 1'].setColor(translucency=0.4)
session.viewports['Viewport: 1'].setColor(translucency=0.5)
session.viewports['Viewport: 1'].setColor(translucency=0.6)
session.viewports['Viewport: 1'].setColor(translucency=0.8)
session.viewports['Viewport: 1'].setColor(translucency=1)
session.viewports['Viewport: 1'].setColor(translucency=0.9)
session.viewports['Viewport: 1'].setColor(translucency=0.8)
session.viewports['Viewport: 1'].setColor(translucency=0.6)
session.viewports['Viewport: 1'].setColor(translucency=0.6)
session.viewports['Viewport: 1'].setColor(globalTranslucency=False)
session.viewports['Viewport: 1'].setColor(globalTranslucency=True)
session.viewports['Viewport: 1'].setColor(globalTranslucency=False)
dg1= session.displayGroups['All']
session.viewports['Viewport: 1'].assemblyDisplay.setValues(
    visibleDisplayGroups=(dg1, ))
mdb.models['Building'].boundaryConditions['Disp-BC-1'].suppress()
mdb.models['Building'].boundaryConditions['Disp-BC-1'].resume()
mdb.models['Building'].boundaryConditions['Disp-BC-1'].suppress()
mdb.models['Building'].boundaryConditions['Disp-BC-1'].resume()
mdb.models['Building'].boundaryConditions['Disp-BC-2'].suppress()
mdb.models['Building'].boundaryConditions['Disp-BC-2'].resume()
session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Initial')
mdb.models['Building'].loads['CFORCE-1'].suppress()
mdb.models['Building'].loads['CFORCE-1'].resume()
mdb.models['Building'].loads['CFORCE-1'].suppress()
mdb.models['Building'].loads['CFORCE-1'].resume()
session.viewports['Viewport: 1'].enableMultipleColors()
session.viewports['Viewport: 1'].setColor(initialColor='#BDBDBD')
cmap = session.viewports['Viewport: 1'].colorMappings['Assembly']
session.viewports['Viewport: 1'].setColor(colorMapping=cmap)
session.viewports['Viewport: 1'].disableMultipleColors()
session.viewports['Viewport: 1'].enableMultipleColors()
session.viewports['Viewport: 1'].setColor(initialColor='#BDBDBD')
cmap = session.viewports['Viewport: 1'].colorMappings['Assembly']
session.viewports['Viewport: 1'].setColor(colorMapping=cmap)
session.viewports['Viewport: 1'].disableMultipleColors()
session.viewports['Viewport: 1'].view.setValues(nearPlane=83.204, 
    farPlane=140.435, width=53.3223, height=40.8012, cameraPosition=(-101.778, 
    29.8491, 22.2874), cameraUpVector=(0.426391, -0.160591, 0.890169), 
    viewOffsetX=0.98853, viewOffsetY=1.64501)
session.viewports['Viewport: 1'].assemblyDisplay.setValues(step='Side_wind')
session.viewports['Viewport: 1'].view.fitView()
session.viewports['Viewport: 1'].view.setValues(nearPlane=88.5545, 
    farPlane=143.462, width=37.513, height=28.7042, cameraPosition=(-104.385, 
    35.2015, 22.7353), cameraUpVector=(0.455978, -0.140891, 0.878768))
session.viewports['Viewport: 1'].view.setValues(nearPlane=88.7271, 
    farPlane=143.289, width=37.5861, height=28.7602, cameraPosition=(-104.385, 
    35.2015, 22.7353), cameraUpVector=(0.443086, -0.175639, 0.879105))
session.viewports['Viewport: 1'].view.setValues(nearPlane=89.322, 
    farPlane=142.793, width=37.8381, height=28.953, cameraPosition=(-106.877, 
    26.1, 25.4147), cameraUpVector=(0.479932, -0.129171, 0.867744))
session.viewports['Viewport: 1'].view.setValues(nearPlane=88.7386, 
    farPlane=143.262, width=37.591, height=28.7639, cameraPosition=(-105.228, 
    31.5629, 25.2224), cameraUpVector=(0.471307, -0.153079, 0.868583))
session.psOptions.setValues(paperSize=A4, logo=OFF, date=OFF, 
    resolution=DPI_300)
session.printToFile(
    fileName='/home/ep/workspace/COSSAN-X_SVN/COSSANXengine/examples/FE_models/ABAQUS/MultistoryBuilding/BuildingA', 
    format=PS, canvasObjects=(session.viewports['Viewport: 1'], ))
session.viewports['Viewport: 1'].view.setValues(nearPlane=92.2532, 
    farPlane=139.748, width=14.513, height=8.84738, viewOffsetX=-8.35108, 
    viewOffsetY=11.3488)
session.viewports['Viewport: 1'].view.setValues(nearPlane=93.4318, 
    farPlane=138.569, width=2.97116, height=1.81127, viewOffsetX=-12.6061, 
    viewOffsetY=11.3259)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.407, 
    farPlane=136.969, width=3.03398, height=1.84956, cameraPosition=(-109.612, 
    13.5875, 25.6291), cameraUpVector=(0.492243, -0.0794273, 0.866826), 
    viewOffsetX=-12.8726, viewOffsetY=11.5653)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.4002, 
    farPlane=136.975, width=3.03376, height=1.84943, viewOffsetX=-10.9536, 
    viewOffsetY=11.2525)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.3438, 
    farPlane=137.031, width=4.1965, height=2.55825, viewOffsetX=-10.9125, 
    viewOffsetY=11.2108)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.2777, 
    farPlane=137.097, width=4.19359, height=2.55648, viewOffsetX=-10.1251, 
    viewOffsetY=11.1277)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.1089, 
    farPlane=137.266, width=6.81647, height=4.15543, viewOffsetX=-10.1059, 
    viewOffsetY=11.1728)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.0018, 
    farPlane=137.373, width=6.80879, height=4.15075, viewOffsetX=-7.83802, 
    viewOffsetY=11.3991)
session.viewports['Viewport: 1'].view.setValues(nearPlane=94.5554, 
    farPlane=137.82, width=12.9822, height=7.91417, viewOffsetX=-8.67931, 
    viewOffsetY=11.3786)
session.viewports['Viewport: 1'].view.setValues(nearPlane=94.3525, 
    farPlane=138.023, width=12.9544, height=7.89718, viewOffsetX=-8.86409, 
    viewOffsetY=11.555)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.0018, 
    farPlane=137.373, width=8.01037, height=4.88325, viewOffsetX=-9.85006, 
    viewOffsetY=11.3921)
session.viewports['Viewport: 1'].view.setValues(nearPlane=94.8761, 
    farPlane=137.499, width=7.99977, height=4.87678, viewOffsetX=-8.69987, 
    viewOffsetY=11.7753)
session.viewports['Viewport: 1'].view.fitView()
session.viewports['Viewport: 1'].view.setValues(nearPlane=92.4344, 
    farPlane=142.345, width=35.843, height=25.3977)
session.viewports['Viewport: 1'].view.setValues(nearPlane=93.4279, 
    farPlane=141.352, width=28.8504, height=20.4428)
session.viewports['Viewport: 1'].view.setValues(nearPlane=93.7241, 
    farPlane=141.056, width=26.2173, height=18.5771)
session.viewports['Viewport: 1'].view.setValues(nearPlane=93.7131, 
    farPlane=141.067, width=26.2142, height=18.5749, viewOffsetX=-7.93226, 
    viewOffsetY=-4.74942)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.4442, 
    farPlane=139.336, width=12.2949, height=8.71193, viewOffsetX=-10.9128, 
    viewOffsetY=-4.69787)
session.viewports['Viewport: 1'].view.setValues(nearPlane=97.1862, 
    farPlane=138.097, width=12.5193, height=8.87096, cameraPosition=(-112.438, 
    14.4451, 10.8685), cameraUpVector=(0.373716, -0.0802412, 0.924066), 
    viewOffsetX=-11.112, viewOffsetY=-4.78361)
session.viewports['Viewport: 1'].view.setValues(nearPlane=97.0969, 
    farPlane=138.186, width=12.7631, height=9.04369, viewOffsetX=-11.1704, 
    viewOffsetY=-4.85477)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.4932, 
    farPlane=139.351, width=12.5523, height=8.89431, cameraPosition=(-110.372, 
    22.5958, 15.6187), cameraUpVector=(0.404137, -0.111523, 0.907874), 
    viewOffsetX=-10.9859, viewOffsetY=-4.77459)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.7453, 
    farPlane=139.098, width=10.4931, height=7.43522, viewOffsetX=-11.3837, 
    viewOffsetY=-4.51171)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.9515, 
    farPlane=138.941, width=10.5157, height=7.45123, cameraPosition=(-110.559, 
    22.2157, 14.5903), cameraUpVector=(0.395349, -0.113924, 0.911439), 
    viewOffsetX=-11.4082, viewOffsetY=-4.52142)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.9484, 
    farPlane=138.944, width=10.5154, height=7.45101, viewOffsetX=-11.2866, 
    viewOffsetY=-1.87801)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.9484, 
    farPlane=138.944, width=10.5154, height=7.45101, viewOffsetX=-11.2664, 
    viewOffsetY=0.575736)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.9484, 
    farPlane=138.944, width=10.5154, height=7.45101, viewOffsetX=-11.1755, 
    viewOffsetY=2.91976)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.9484, 
    farPlane=138.944, width=10.5154, height=7.45101, viewOffsetX=-11.0947, 
    viewOffsetY=5.68272)
session.viewports['Viewport: 1'].view.setValues(nearPlane=95.9484, 
    farPlane=138.944, width=10.5154, height=7.45101, viewOffsetX=-10.8018, 
    viewOffsetY=8.25616)
session.viewports['Viewport: 1'].view.setValues(nearPlane=92.3825, 
    farPlane=141.373, width=10.1246, height=7.17412, cameraPosition=(-101.22, 
    44.5809, 24.5028), cameraUpVector=(0.488898, -0.104473, 0.866063), 
    viewOffsetX=-10.4004, viewOffsetY=7.94932)
session.viewports['Viewport: 1'].view.setValues(nearPlane=92.7448, 
    farPlane=141.164, width=10.1643, height=7.20225, cameraPosition=(-104.66, 
    32.1521, 32.1821), cameraUpVector=(0.551802, -0.0657295, 0.831381), 
    viewOffsetX=-10.4412, viewOffsetY=7.98049)
session.viewports['Viewport: 1'].view.setValues(nearPlane=92.7398, 
    farPlane=141.169, width=10.1638, height=7.20186, viewOffsetX=-8.77109, 
    viewOffsetY=9.37801)
session.viewports['Viewport: 1'].view.setValues(nearPlane=92.7398, 
    farPlane=141.169, width=10.1638, height=7.20186, viewOffsetX=-9.33737, 
    viewOffsetY=8.77062)
session.viewports['Viewport: 1'].view.setValues(nearPlane=92.4593, 
    farPlane=141.335, width=10.1331, height=7.18011, cameraPosition=(-104.094, 
    28.2374, 38.7239), cameraUpVector=(0.591818, -0.0865197, 0.801415), 
    viewOffsetX=-9.30913, viewOffsetY=8.74409)
session.printToFile(
    fileName='/home/ep/workspace/COSSAN-X_SVN/COSSANXengine/examples/FE_models/ABAQUS/MultistoryBuilding/BuildingB', 
    format=PS, canvasObjects=(session.viewports['Viewport: 1'], ))
session.viewports['Viewport: 1'].view.setValues(nearPlane=91.991, 
    farPlane=141.804, width=19.0815, height=11.6324, viewOffsetX=-10.4364, 
    viewOffsetY=9.68827)
session.viewports['Viewport: 1'].view.setValues(nearPlane=90.6344, 
    farPlane=142.788, width=18.8001, height=11.4608, cameraPosition=(-99.4846, 
    42.8935, 35.0113), cameraUpVector=(0.542329, -0.172974, 0.822168), 
    viewOffsetX=-10.2825, viewOffsetY=9.54539)
session.viewports['Viewport: 1'].view.fitView()
session.printToFile(
    fileName='/home/ep/workspace/COSSAN-X_SVN/COSSANXengine/examples/FE_models/ABAQUS/MultistoryBuilding/BuildingC', 
    format=PS, canvasObjects=(session.viewports['Viewport: 1'], ))
session.viewports['Viewport: 1'].assemblyDisplay.setValues(renderStyle=SHADED)
session.viewports['Viewport: 1'].setColor(globalTranslucency=True)
session.viewports['Viewport: 1'].setColor(globalTranslucency=False)
session.viewports['Viewport: 1'].setColor(translucency=0.6)
session.viewports['Viewport: 1'].setColor(translucency=0.9)
session.viewports['Viewport: 1'].setColor(translucency=1)
session.viewports['Viewport: 1'].setColor(translucency=1)
session.viewports['Viewport: 1'].setColor(translucency=0.9)
session.viewports['Viewport: 1'].setColor(translucency=0.8)
session.viewports['Viewport: 1'].setColor(translucency=0.2)
session.viewports['Viewport: 1'].setColor(translucency=0.1)
session.viewports['Viewport: 1'].setColor(translucency=0)
session.viewports['Viewport: 1'].setColor(translucency=0)
session.psOptions.setValues(fontType=AS_DISPLAYED)
session.printToFile(
    fileName='/home/ep/workspace/COSSAN-X_SVN/COSSANXengine/examples/FE_models/ABAQUS/MultistoryBuilding/BuildingC', 
    format=PS, canvasObjects=(session.viewports['Viewport: 1'], ))
session.printToFile(
    fileName='/home/ep/workspace/COSSAN-X_SVN/COSSANXengine/examples/FE_models/ABAQUS/MultistoryBuilding/BuildingC', 
    format=SVG, canvasObjects=(session.viewports['Viewport: 1'], ))
session.viewports['Viewport: 1'].view.setValues(nearPlane=87.6063, 
    farPlane=147.121, width=50.1835, height=30.5926, cameraPosition=(-91.0154, 
    62.9289, 25.6353), cameraUpVector=(0.536155, -0.0933931, 0.838937))
session.viewports['Viewport: 1'].view.setValues(nearPlane=87.8677, 
    farPlane=146.852, width=50.3332, height=30.6839, cameraPosition=(-90.4553, 
    64.008, 24.5415), cameraUpVector=(0.530869, -0.0911369, 0.842539))
session.viewports['Viewport: 1'].view.setValues(nearPlane=88.2529, 
    farPlane=146.858, width=50.5539, height=30.8184, cameraPosition=(-98.5629, 
    49.3788, 29.9498), cameraUpVector=(0.46987, -0.236517, 0.85046))
session.viewports['Viewport: 1'].view.setValues(nearPlane=88.5684, 
    farPlane=146.731, width=50.7346, height=30.9286, cameraPosition=(-102.387, 
    37.2437, 36.8498), cameraUpVector=(0.554774, -0.164269, 0.815623))
session.viewports['Viewport: 1'].view.setValues(nearPlane=89.4738, 
    farPlane=146.139, width=51.2532, height=31.2448, cameraPosition=(-104.984, 
    35.1785, 30.1852), cameraUpVector=(0.504711, -0.15645, 0.848994))
session.viewports['Viewport: 1'].view.setValues(nearPlane=89.9149, 
    farPlane=145.853, width=51.5059, height=31.3988, cameraPosition=(-106.877, 
    27.6979, 32.8089), cameraUpVector=(0.537803, -0.109739, 0.835898))
session.viewports['Viewport: 1'].view.setValues(nearPlane=90.7011, 
    farPlane=145.314, width=51.9563, height=31.6733, cameraPosition=(-108.743, 
    24.3015, 28.9634), cameraUpVector=(0.503178, -0.12544, 0.85503))
session.printOptions.setValues(vpDecorations=OFF)
session.printToFile(
    fileName='/home/ep/workspace/COSSAN-X_SVN/COSSANXengine/examples/FE_models/ABAQUS/MultistoryBuilding/BuildingC', 
    format=SVG, canvasObjects=(session.viewports['Viewport: 1'], ))
session.printOptions.setValues(reduceColors=False)
session.printToFile(
    fileName='/home/ep/workspace/COSSAN-X_SVN/COSSANXengine/examples/FE_models/ABAQUS/MultistoryBuilding/BuildingC', 
    format=PNG, canvasObjects=(session.viewports['Viewport: 1'], ))
session.viewports['Viewport: 1'].view.setValues(nearPlane=94.6464, 
    farPlane=141.368, width=14.6609, height=8.93752, viewOffsetX=-11.7618, 
    viewOffsetY=9.49058)
session.printToFile(
    fileName='/home/ep/workspace/COSSAN-X_SVN/COSSANXengine/examples/FE_models/ABAQUS/MultistoryBuilding/BuildingD', 
    format=PNG, canvasObjects=(session.viewports['Viewport: 1'], ))
session.viewports['Viewport: 1'].view.setValues(nearPlane=94.5824, 
    farPlane=141.433, width=14.651, height=8.93147, viewOffsetX=-8.88418, 
    viewOffsetY=9.44829)
session.printToFile(
    fileName='/home/ep/workspace/COSSAN-X_SVN/COSSANXengine/examples/FE_models/ABAQUS/MultistoryBuilding/BuildingD', 
    format=PNG, canvasObjects=(session.viewports['Viewport: 1'], ))
