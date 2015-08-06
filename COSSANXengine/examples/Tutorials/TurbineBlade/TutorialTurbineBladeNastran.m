%% Tutorial Turbine Blade - Creating the NASTRAN model
%
% See also: http://cossan.cfd.liv.ac.uk/wiki/index.php/Turbine_Blade
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author:~Murat~Panayirci$ 

%% Create the input

% define the RVs
RV1 = RandomVariable('Sdistribution','normal', 'mean',7e7,'std',7e6);    
RV2 = RandomVariable('Sdistribution','normal', 'mean',7e7,'std',7e6); 
RV3 = RandomVariable('Sdistribution','normal', 'mean',7e7,'std',7e6);       

Xrvs = RandomVariableSet('Cmembers',{'RV1','RV2','RV3'}); 
Xinp = Input('Sdescription','Xinput object');       
Xinp = add(Xinp,Xrvs);

%% Construct the Injector

Sdirectory = fileparts(which('TutorialTurbineBladeNastran.m'));
Xinj       = Injector('Sscanfilepath',fullfile(Sdirectory,'FEinputFiles'),...
                      'Sscanfilename','Nastran.cossan','Sfile','Nastran.inp');

%% Define Connector

Xcon = Connector('Spredefinedtype','nastran_x86_64',...
                 'SmaininputPath',fullfile(Sdirectory,'FEinputFiles'),...
                 'Smaininputfile','Nastran.inp', ...
                 'Sworkingdirectory',tempdir);
Xcon = add(Xcon,Xinj);

%% Define Model

Xeval  = Evaluator('Xconnector',Xcon);
Xmodel = Model('Xevaluator',Xeval,'Xinput',Xinp);

display(Xmodel);


