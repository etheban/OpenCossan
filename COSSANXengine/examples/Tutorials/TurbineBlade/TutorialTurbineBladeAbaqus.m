%% Tutorial Turbine Blade - Creating the ABAQUS model
%
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

%% Construct the Model

Sdirectory = fileparts(which('TutorialTurbineBladeAbaqus.m'));
Xinj       = Injector('Sscanfilepath',fullfile(Sdirectory,'FEinputFiles'),'Sscanfilename','Abaqus.cossan','Sfile','Abaqus.inp');
Xcon       = Connector('Spredefinedtype','abaqus',...
                     'SmaininputPath',fullfile(Sdirectory,'FEinputFiles'),...
                     'Smaininputfile','Abaqus.inp',...
                     'Sworkingdirectory',tempdir);
Xcon   = add(Xcon,Xinj);
Xjmi   = JobManagerInterface('Stype','gridengine');
Xeval  = Evaluator('Xconnector',Xcon,'CSmembers',{'Xcon'},'XJobManagerInterface',Xjmi,...
                   'LremoteInjectExtract',false,'CSqueues',{'pizzas64.q'},'Nconcurrent',4);
Xmodel = Model('Xevaluator',Xeval,'Xinput',Xinp);

display(Xmodel);

