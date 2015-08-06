%% Tutorial for the Sensitivity analysis
% The tutorial InfectionDynamicModel expains in very details how to use the
% Sensitivity Toolbox. For this reason the uses is invited to check the
% tutorial InfectionDynamicModel. 
%
% In this tutorial a very simplified model is considered.  
% 
% See Also: http://cossan.cfd.liv.ac.uk/wiki/index.php/Infection_Dynamic_Model
% 
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author: Edoardo-Patelli$ 


%% Problem setup
% In this examples we consider only 3 uniform random variables
Xrv1   = RandomVariable('Sdistribution','uniform','lowerbound',-1,'upperbound',1);
Xrv2   = RandomVariable('Sdistribution','uniform','lowerbound',-1,'upperbound',1);
Xrv3   = RandomVariable('Sdistribution','uniform','lowerbound',-1,'upperbound',1);
Xrvset = RandomVariableSet('Cmembers',{'Xrv1','Xrv2','Xrv3'},'CXrandomvariables',{Xrv1,Xrv2,Xrv3});
Xin    = Input('XrandomVariableSet',Xrvset);

% The model is defined using a Mio object
Xm = Mio('Sscript','for j=1:length(Tinput), Toutput(j).out1=Tinput(j).Xrv1^2+2*Tinput(j).Xrv2-Tinput(j).Xrv3; end', ...
         'Coutputnames',{'out1'},...
         'Cinputnames',{'Xrv1' 'Xrv2' 'Xrv3'},...
         'Liostructure',true,...
	     'Lfunction',false); 
     
Xev    = Evaluator('Xmio',Xm);
Xmdl   = Model('Xinput',Xin,'Xevaluator',Xev);

%% Global Sensitivity Analysis
% The global sensitivity analysis is independent of the reference points.
% There are different methods to estimate the global sensitivity analysis.

% The method upperBounds allows to estimate the upper bounds of teh total
% sensitivity indices

Xgs=GlobalSensitivityUpperBound('Xmodel',Xmdl,'Nbootstrap',3,'Nsamples',4);
display(Xgs)
Xsm = Xgs.computeIndices;
display(Xsm)

   