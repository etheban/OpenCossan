%% Tutorial Turbine Blade - Performing Neumann Analysis
%
% See also: http://cossan.cfd.liv.ac.uk/wiki/index.php/Turbine_Blade
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author:~Murat~Panayirci$ 

%% Load the Model - with NASTRAN Connector

run(fullfile(OpenCossan.getCossanRoot,'examples','Tutorials',...
                                   'TurbineBlade','TutorialTurbineBladeNastran'));

%% using Regular implementation (NASTRAN)

Xsfem = Neumann('Xmodel',Xmodel,'CyoungsmodulusRVs',{'RV1','RV2','RV3'},...
               'Nsimulations',30,'Norder',5);

% Fix the seed in order to generate same samples => to validate results
OpenCossan.resetRandomNumberGenerator(1);             
                   
Xout  = Xsfem.performAnalysis; 

Xout  = getResponse(Xout,'Sresponse','specific','MresponseDOFs',[150 3]);
display(Xout);

referenceMean = 0.9043;
referenceCoV  = 0.0942;

assert(abs(Xout.Vresponsemean-referenceMean)<1e-4,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference mean value does not match.')

assert(abs(Xout.Vresponsecov-referenceCoV)<1e-4,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference CoV value does not match.')
  

%% using Superelement (Nastsem) implementation (only available for NASTRAN)

Xsfem = Nastsem('Xmodel',Xmodel,'CyoungsmodulusRVs',{'RV1','RV2','RV3'},'Smethod','Neumann',...
             'Nsimulations',30,'Norder',4,'Vfixednodes',[1777 616 1779 4120 4276 4286]);                          
                       
% Fix the seed in order to generate same samples => to validate results
OpenCossan.resetRandomNumberGenerator(1);               
         
Xout  = Xsfem.performAnalysis;

Xout  = getResponse(Xout,'Sresponse','specific','MresponseDOFs',[150 3]);
display(Xout);

assert(abs(Xout.Vresponsemean-referenceMean)<1e-3,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference mean value does not match.')

assert(abs(Xout.Vresponsecov-referenceCoV)<1e-2,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference CoV value does not match.')


%% Load the Model - with ABAQUS Connector

run(fullfile(OpenCossan.getCossanRoot,'examples','Tutorials',...
                                   'TurbineBlade','TutorialTurbineBladeAbaqus'));

%% using Regular implementation (ABAQUS)

Xsfem = Neumann('Xmodel',Xmodel,'CyoungsmodulusRVs',{'RV1','RV2','RV3'},...
                'CstepDefinition',{'**BOUNDARY  ',...
       '     616, 1,6, 0.','    1777, 1,6, 0.','    4120, 1,6, 0.','    4276, 1,6, 0.',...
       '    4286, 1,6, 0.','*DLOAD, OP=NEW','ALL_MASSIVE_ELEMENTS, GRAV, 3.5e+08, 0., 1., 0.'},...
       'MconstrainedDOFs',[616.*ones(6,1) (1:6)'; 1777.*ones(6,1) (1:6)';...
                           4120.*ones(6,1) (1:6)'; 4276.*ones(6,1) (1:6)';...
                           4286.*ones(6,1) (1:6)'],'Nsimulations',30,'Norder',5);
                   
% Fix the seed in order to generate same samples => to validate results
OpenCossan.resetRandomNumberGenerator(1);      

Xout = Xsfem.performAnalysis;

Xout = getResponse(Xout,'Sresponse','specific','MresponseDOFs',[150 3]);
display(Xout);

assert(abs(Xout.Vresponsemean-referenceMean)<1e-4,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference mean value does not match.')

assert(abs(Xout.Vresponsecov-referenceCoV)<1e-4,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference CoV value does not match.')
