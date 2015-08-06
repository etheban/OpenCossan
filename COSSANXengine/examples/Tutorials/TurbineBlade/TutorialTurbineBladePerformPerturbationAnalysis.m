%% Tutorial Turbine Blade - Performing Perturbation Analysis
%
% See also: http://cossan.cfd.liv.ac.uk/wiki/index.php/Turbine_Blade
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author:~Murat~Panayirci$ 

%% Load the Model - with NASTRAN Connector

Sdirectory = fileparts(which('TutorialTurbineBladePerformPerturbationAnalysis'));
run(fullfile(Sdirectory,'TutorialTurbineBladeNastran'));
       
%% using Regular implementation (NASTRAN)
                   
Xsfem = Perturbation('Xmodel',Xmodel,'CyoungsmodulusRVs',{'RV1','RV2','RV3'});                                           
Xout  = Xsfem.performAnalysis;

Xout  = getResponse(Xout,'Sresponse','specific','MresponseDOFs',[150 3]);
display(Xout);

% verify results
referenceMean = 0.89798;
referenceCoV  = 0.098122;

assert(abs(Xout.Vresponsemean-referenceMean)<1e-4,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference mean value does not match.')

assert(abs(Xout.Vresponsecov-referenceCoV)<1e-4,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference CoV value does not match.')

%% using Componentwise implementation (only available for NASTRAN)

Xsfem = Perturbation('Xmodel',Xmodel,'CyoungsmodulusRVs',{'RV1','RV2','RV3'},'Simplementation','Componentwise');                                      
Xout  = Xsfem.performAnalysis;

Xout  = getResponse(Xout,'Sresponse','specific','MresponseDOFs',[150 3]);
display(Xout);

% verify results
assert(abs(Xout.Vresponsemean-referenceMean)<1e-4,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference mean value does not match.')

assert(abs(Xout.Vresponsecov-referenceCoV)<1e-4,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference CoV value does not match.')

%% using Superelement (Nastsem) implementation (only available for NASTRAN)

Xsfem = Nastsem('Xmodel',Xmodel,'CyoungsmodulusRVs',{'RV1','RV2','RV3'},...
                'Smethod','Perturbation','Vfixednodes',[1777 616 1779 4120 4276 4286]);                                  
Xout  = Xsfem.performAnalysis;

Xout  = getResponse(Xout,'Sresponse','specific','MresponseDOFs',[150 3]);
display(Xout);

% verify results
assert(abs(Xout.Vresponsemean-referenceMean)<1e-4,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference mean value does not match.')

assert(abs(Xout.Vresponsecov-referenceCoV)<1e-4,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference CoV value does not match.')

%% Load the Model - with ABAQUS Connector

run(fullfile(OpenCossan.getCossanRoot,'examples','Tutorials',...
                                   'TurbineBlade','TutorialTurbineBladeAbaqus'));

%% using Regular implementation (ABAQUS)
                   
Xsfem = Perturbation('Xmodel',Xmodel,'CyoungsmodulusRVs',{'RV1','RV2','RV3'},...
      'CstepDefinition',{'**BOUNDARY  ',...
       '     616, 1,6, 0.','    1777, 1,6, 0.','    4120, 1,6, 0.','    4276, 1,6, 0.',...
       '    4286, 1,6, 0.','*DLOAD, OP=NEW','ALL_MASSIVE_ELEMENTS, GRAV, 3.5e+08, 0., 1., 0.'},...
       'MconstrainedDOFs',[616.*ones(6,1) (1:6)'; 1777.*ones(6,1) (1:6)';...
                           4120.*ones(6,1) (1:6)'; 4276.*ones(6,1) (1:6)';...
                           4286.*ones(6,1) (1:6)']);                                     
                       
Xout = Xsfem.performAnalysis;

Xout = getResponse(Xout,'Sresponse','specific','MresponseDOFs',[150 3]);
display(Xout);

assert(abs(Xout.Vresponsemean-referenceMean)<1e-4,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference mean value does not match.')

assert(abs(Xout.Vresponsecov-referenceCoV)<1e-4,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference CoV value does not match.')

