% function Xoptimum = testHLRF(Xobj)
% %TESTHLRF Summary of this function goes here
% %   Detailed explanation goes here
% 
% %% 2.   Algorithm
% icount  = 0;
% ireduction=4;
% while 1,
%      % TODO: BG,02-09-2008: Why is function dgdu in folder /COSSAN-X and
%      % not in /private folder?
%     [Vgradg0 g0 Nisim]  = dgdu(Toptim_problem.Xpm,Vu0);
%     gradg0_abs  = norm(Vgradg0);
%     Valpha      = Vgradg0 / gradg0_abs;
%     VB=(Vu0*Valpha')*Valpha;    % projection along the gradiend 
%     
%     Vu          =VB - g0/gradg0_abs * Valpha;
%    
%     if Toptions.Lenhanced
%             C=Vu0-VB;
%             D=Vu0-C/ireduction;
%             Vu=D - g0/gradg0_abs * Valpha;
%         if icount<=Nisim  
%             Sold=sign(C);
%         elseif (sign(C)~=Sold)
%             Sold=sign(C);
%             if ireduction<10
%                 ireduction=ireduction+1;
%             end
%         end
%     end
%     
%     if Toptions.Lverbose
%         OpenCossan.cossanDisp(['Evaluation point: ' num2str(Vu)]);
%         OpenCossan.cossanDisp(['gradient: ' num2str(Valpha)]);
%         OpenCossan.cossanDisp('');
%     end
%     
%     bet         = norm(Vu);
%     if abs( (bet - bet0) / bet0) < Toptions.dbeta,
%         Toutput.Sexitflag   = 'HL-RF converged';
%         break
%     elseif icount > Toptions.NMaxFunEvals,
%         Toutput.Sexitflag = 'HL-RF exceeded maximum number of function evaluations';
%         break
%     end
%     Vu0     = Vu;
%     bet0    = bet;
%     icount      = icount + Nisim;
% end


% %**************************************************************************
% %   Identification of design point
% %**************************************************************************
% % This Tutorial show how the so-called desing poin can be idenfied automatically
% % by COSSAN-X. 
% % 
% 
% %%  Define Input parameters
% % Random Variables
% Xrv1    = RandomVariable('Sdistribution','normal', 'mean',1,'std',0.5); 
% Xrv2    = RandomVariable('Sdistribution','normal', 'mean',-1,'std',2);
% Xrv3    = RandomVariable('Sdistribution','normal', 'mean',10,'std',0.5); 
% Xrv4    = RandomVariable('Sdistribution','normal', 'mean',10,'std',2);
% C       = Parameter('Sdescription','Capacity','value',3);
% 
% %  RandomVariableSets 
% Xrvs1   = RandomVariableSet('Cmembers',{'Xrv1','Xrv3','Xrv4'},'Cxrandomvariables',{Xrv1 Xrv3 Xrv4}); 
% Xrvs2   = RandomVariableSet('Cmembers',{'Xrv2'},'Cxrandomvariables',{Xrv2}); 
% 
% % Define Input
% Xin     = Input('CXmembers',{Xrvs1 Xrvs2 C},'CSmembers',{'Xrvs1' 'Xrvs2' 'C'});
% 
% %%  Construct Mio object
% Xm      = Mio('Sdescription','normalized demand', ...
%     'Spath',pwd,...
%     'Sfile','normalized_demand',...
%     'Liostructure',true,...
%     'Lfunction',true,...
%     'Cinputnames',{'Xrv1','Xrv2'},...
%     'Coutputnames',{'D'});            
% 
% %%  Construct evaluator
% Xeval   = Evaluator('Xmio',Xm,'Sdescription','Evaluator xmio');
% 
% %%   Define Physical Model
% Xmdl    = Model('Xevaluator',Xeval,'Xinput',Xin);
% 
% %% Define Probabilistic Model
% % Define Performance Function
% Xperf   = PerformanceFunction('Scapacity','C','Sdemand','D','Soutputname','Vg');
% % Construct a probmodel
% Xpm     = ProbabilisticModel('XModel',Xmdl,'XPerformanceFunction',Xperf);
% 
% %% Identify design point using
% % The design point is identified using Cobyla (default) 
% [XdesignPoint  Xoptimum] = Xpm.designPointIdentification;


%**************************************************************************
%   Identification of design point
%**************************************************************************
% This Tutorial show how the so-called desing poin can be idenfied automatically
% by COSSAN-X. 
% 

%%  Define Input parameters
% Random Variables
Xrv1    = RandomVariable('Sdistribution','normal', 'mean',0,'std',1); 
Xrv2    = RandomVariable('Sdistribution','normal', 'mean',0,'std',1);

C       = Parameter('Sdescription','Capacity','value',2);

%  RandomVariableSets 
Xrvs1   = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'}); 

% Define Input
Xin     = Input('CXmembers',{Xrvs1 C},'CSmembers',{'Xrvs1' 'C'});

%%  Construct Mio object
Xm      = Mio('Sdescription','normalized demand', ...
    'Sscript','for n=1:length(Tinput), Toutput(n).D=Tinput(n).Xrv1-2*Tinput(n).Xrv2;end',...
    'Liostructure',true,...
    'Cinputnames',{'Xrv1','Xrv2'},...
    'Coutputnames',{'D'});            

%%  Construct evaluator
Xeval   = Evaluator('Xmio',Xm,'Sdescription','Evaluator xmio');

%%   Define Physical Model
Xmdl    = Model('Xevaluator',Xeval,'Xinput',Xin);

%% Define Probabilistic Model
% Define Performance Function
Xperf   = PerformanceFunction('Scapacity','C','Sdemand','D','Soutputname','Vg');
% Construct a probmodel
Xpm     = ProbabilisticModel('XModel',Xmdl,'XPerformanceFunction',Xperf);

%% Identify design point using
% The design point is identified using Cobyla (default) 
[XdesignPoint  Xoptimum] = Xpm.designPointIdentification;
