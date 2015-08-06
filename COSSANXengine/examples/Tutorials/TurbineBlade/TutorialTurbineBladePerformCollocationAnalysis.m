%% Tutorial Turbine Blade - Performing Collocation PC Analysis
%
%
% Note: For this analysis, the model is also created in this file
%       since one has to also define extractors for this case 
%       (therefore TutorialTurbineBladeNastran file is not loaded
%        in this case)
% 
% See also: http://cossan.cfd.liv.ac.uk/wiki/index.php/Turbine_Blade
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author:~Murat~Panayirci$ 

%% Create the input

% define the RVs
RV1=RandomVariable('Sdistribution','normal', 'mean',7e7,'std',7e6);    
RV2=RandomVariable('Sdistribution','normal', 'mean',7e7,'std',7e6); 
RV3=RandomVariable('Sdistribution','normal', 'mean',7e7,'std',7e6);       

Xrvs = RandomVariableSet('Cmembers',{'RV1','RV2','RV3'}); 
Xinp = Input('Sdescription','Xinput object');       
Xinp = add(Xinp,Xrvs);

%% Define the Injector

Sdirectory = fileparts(which('TutorialTurbineBladePerformCollocationAnalysis.m'));
Xinj = Injector('Sscanfilepath',fullfile(Sdirectory,'FEinputFiles'),'Sscanfilename','Nastran.cossan','Sfile','Nastran.inp');

%% Define the Connector

Xcon = Connector('Spredefinedtype','nastran_x86_64',...
                     'SmaininputPath',fullfile(Sdirectory,'FEinputFiles'),...
                     'Smaininputfile','Nastran.inp',...
                     'Sworkingdirectory',tempdir,...
                     'Lkeepsimulationfiles',false);
Xcon = add(Xcon,Xinj);

%% Define Extractor

Xresp1 = Response('Sname', 'XYstress', 'Sfieldformat', '%12e', ...
                 'Clookoutfor',{'0       158           0GRID CS  4 GP'},'Ncolnum',30,'Nrownum',1 );
Xresp2 = Response('Sname', 'VONMstress', 'Sfieldformat', '%12e', ...
                 'Clookoutfor',{'0       158           0GRID CS  4 GP'},'Ncolnum',118,'Nrownum',1 );
             
Xext  = Extractor('Sfile','Nastran.f06','CXresponse', {Xresp1 Xresp2});

Xcon = add(Xcon,Xext);   

%% Define the JobManager

Xjmi   = JobManagerInterface('Stype','gridengine');
Xeval  = Evaluator('Xconnector',Xcon,'CSmembers',{'Xcon'},'XJobManagerInterface',Xjmi,...
                   'LremoteInjectExtract',false,'CSqueues',{'pizzas64.q'},'Nconcurrent',4);
Xmodel = Model('Xevaluator',Xeval,'Xinput',Xinp);

%% Perform SFEM Analysis

Xsfem = SfemPolynomialChaos('Xmodel',Xmodel,'Smethod','Collocation','Sbasis','Legendre',...
                          'CyoungsmodulusRVs',{'RV1','RV2','RV3'},'Norder',2,'Nmaxdepth',2);

Xout = Xsfem.performAnalysis;

Xout = getResponse(Xout);
display(Xout);


%% Validate the results
%
% Note: following CoV values are not accurate since I selected Nmaxdepth
%       parameter above as 2 (it should be at least 4 to get accurate 
%       results, but then it takes too long to get results)
%
referenceMean1 = 4112.0128;
referenceCoV1  = 0.76565;

referenceMean2 = 34757.4406;
referenceCoV2  = 0.76526;

assert(abs(Xout.Vresponsemean(1)-referenceMean1)<1e-1,'CossanX:Tutorials:TutorialTurbineBlade:CollocationPC', ...
      'Reference mean value does not match.')

assert(abs(Xout.Vresponsecov(1)-referenceCoV1)<1e-1,'CossanX:Tutorials:TutorialTurbineBlade:CollocationPC', ...
      'Reference CoV value does not match.')
  
assert(abs(Xout.Vresponsemean(2)-referenceMean2)<1e-1,'CossanX:Tutorials:TutorialTurbineBlade:CollocationPC', ...
      'Reference mean value does not match.')

assert(abs(Xout.Vresponsecov(2)-referenceCoV2)<1e-1,'CossanX:Tutorials:TutorialTurbineBlade:CollocationPC', ...
      'Reference CoV value does not match.')


