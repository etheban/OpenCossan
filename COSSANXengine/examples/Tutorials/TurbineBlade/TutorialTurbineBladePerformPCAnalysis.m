%% Tutorial Turbine Blade - Performing P-C Analysis
%
% See also: http://cossan.cfd.liv.ac.uk/wiki/index.php/Turbine_Blade
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author:~Murat~Panayirci$ 

%% Load the Model - with NASTRAN Connector

Sdirectory = fileparts(which('TutorialTurbineBladePerformPCAnalysis'));
run(fullfile(Sdirectory,'TutorialTurbineBladeNastran'));

%% using Regular implementation (NASTRAN) - Galerkin Method

Xsfem = SfemPolynomialChaos('Xmodel',Xmodel,'Smethod','Galerkin',...
                          'CyoungsmodulusRVs',{'RV1','RV2','RV3'},'Norder',3);

Xout  = Xsfem.performAnalysis;

Xout  = getResponse(Xout,'Sresponse','specific','MresponseDOFs',[150 3]); %#ok<*NASGU>
display(Xout);

referenceMean = 0.9073;
referenceCoV  = 0.1012;

assert(abs(Xout.Vresponsemean-referenceMean)<1e-3,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference mean value does not match.')

assert(abs(Xout.Vresponsecov-referenceCoV)<1e-3,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference CoV value does not match.')

%% using Regular implementation (NASTRAN) - Guyan Method

Xsfem = SfemPolynomialChaos('Xmodel',Xmodel,'Smethod','Guyan','CyoungsmodulusRVs',{'RV1','RV2','RV3'},...
                            'Norder',3,'MmasterDOFs',[150 3]);

Xout  = Xsfem.performAnalysis;

Xout  = getResponse(Xout,'Sresponse','specific','MresponseDOFs',[150 3]);
display(Xout);

assert(abs(Xout.Vresponsemean-referenceMean)<1e-3,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference mean value does not match.')

assert(abs(Xout.Vresponsecov-referenceCoV)<1e-3,'CossanX:Tutorials:TutorialNeumann', ...
      'Reference CoV value does not match.')

