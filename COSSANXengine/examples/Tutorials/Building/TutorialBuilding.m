%% TutorialBuilding
%
% This tutorial is use to define the model of a multi-storey building 
% 
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author: Edoardo-Patelli$ 

%% Create the Injector

Xi=Injector('Stype','scan','SscanFilePath',fullfile(OpenCossan.ScossanRoot,'examples','Tutorials','Connector','FEAP'),...
    'Sscanfilename','Ipatch.cossan','Sfile','Ipatch');


%% Extractor
%  Build extractor
Xresp = Response('Sname', 'OUT1', ...
    'Sfieldformat', '%13e', ...
    'Clookoutfor',{'N o d a l   D i s p l a c e m e n t s'}, ...
    'Ncolnum',45, ...
    'Nrownum',6 );
Xe=Extractor('Sdescription','Extractor for Opatch', ...
    'Sfile','Opatch', ...
    'Xresponse', Xresp);

%% Construct the connector
% create the connector

Xc=Connector('Stype','Feap',...
    'Sworkingdirectory','/tmp/',...
    'Smaininputpath',fullfile(OpenCossan.ScossanRoot,'examples','Tutorials','Connector','FEAP'),...
    'Smaininputfile','Ipatch','Lkeepfile',true);

% Add injector and extractor
Xc=add(Xc,Xi);
Xc=add(Xc,Xe);

%% Define Input

Xforce1 = RandomVariable('Sdistribution','uniform','lowerbound',2.5,'upperbound',3.5);
Xforce2 = RandomVariable('Sdistribution','uniform','lowerbound',5,'upperbound',7);
Xforce3 = RandomVariable('Sdistribution','uniform','lowerbound',2.5,'upperbound',3.5);

XforceSet = RandomVariableSet('Cmembers',{'Xforce1','Xforce2','Xforce3'},...
    'CXrv',{Xforce1,Xforce2,Xforce3});

Xfun1=Function('Sexpression','<&Xforce1&>+1');

Xinp = Input();
Xinp = add(Xinp,XforceSet);
Xinp = add(Xinp,Xfun1);

%% Test connector
% Run deterministic Analysis from Model
Xeval=Evaluator('Xconnector',Xc);
Xmdl=Model('Xinput',Xinp,'Xevaluator',Xeval);

% Show Model details
display(Xmdl)

%% Compute the Gradient of the model 
% Around the origin in standard normal space

% In order compute the gradient the static method gradient of the
% Sensitivity toolbox must be invoked
Xgfd=Sensitivity.gradientFiniteDifferences('Xtarget',Xmdl);
display(Xgfd)

Xg=Sensitivity.gradientMonteCarlo('Xtarget',Xmdl,'Nsimulations',7);
% The gradient output contains 
display(Xg)

% Gradient in a different reference point
Xg=Sensitivity.gradientMonteCarlo('Xtarget',Xmdl,'Nsimulations',7,'CnamesRandomVariable',{'RV_1' 'RV_3'},'VreferencePoint',[0 4]);
display(Xg)


Xg=Sensitivity.gradientMonteCarlo('Xtarget',Xmdl,'Nsimulations',50);

error=Xgfd.Vgradient+Xg.Vgradient


%% Test with a very large model 
RV=RandomVariable('Sdistribution','normal','mean',0,'std',1);
Xrvs2=RandomVariableSet('Xrv',RV,'Nrviid',250);
Xin = Input('Sdescription','TestGradient');
Xin = add(Xin,Xrvs2);

% Construct a Mio object
Xm2=Mio('Sdescription', 'Performance function', ...
                'Sscript','Moutput=Minput(:,1).^2+5*Minput(:,2)-0.1*Minput(:,3)+0.01*Minput(:,4);', ...
                'Liomatrix',true,...
                'Liostructure',false,...
				'Lfunction',false, ...
                'Cinputnames',{'RV_1' 'RV_2' 'RV_3' 'RV_4' 'RV_5'}, ...
                'Coutputnames',{'out1'}); % This flag specify if the .m file is a script or a function. 

% Construct the Evaluator
Xeval2 = Evaluator('Xmio',Xm2,'Sdescription','fist Evaluator');

Xmdl2=Model('Xinput',Xin,'Xevaluator',Xeval2);
Xg2=Sensitivity.gradientMonteCarlo('Xtarget',Xmdl2,'Nsimulations',7);
display(Xg2)
