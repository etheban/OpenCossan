%
%   Car body in white - optimization
%
%   In this example, optimization of the car-body in white is performed.
%   The objective function is the FRF around the first torsional frequency.
%   The design variables are the thicknesses of different shell elements of
%   the FE model of the car.
%
%   The physical model comprises a car body in white. In order to calculate
%   the FRF, a mode-base metamodel is used.
% 
% =========================================================================
% COSSAN - COmputational Stochastic Structural Analysis
% IfM, Chair of Engineering Mechanics, LFU Innsbruck, A
% Copyright 1993-2009 IfM
% =========================================================================

%% 1.   Data of thicknesses
%in this part, the values of the thickness of the different parts of the
%car body are defined. The different parts correspond to different property
%cards defined in a Nastran input file; these property cards refer to shell
%elements
%1.1.   Id number of the property cards
Vid_shells  = [11,17,19,53,71,79,82,84,86,88,91,98,100,101,102,105,107,109,110];
%1.2.   Values of thicknesses
Vthick  = [1,2,0.9,1.5,2,1.8,1.4,0.8,1.2,2.5,1.5,1.2,1.2,0.9,1,1.8,1.2,5,1.4];
%this vector contains the values of the thicknesses for each of the
%property cards

%% 2.   Create parameters of the model
%in this part of the code, the parameters of the model are created. These
%parameters correspond to the thicknesses defined above
NParameters         = length(Vid_shells);   %number of parameters
Xin                 = Input;                %empty input object
for i=1:NParameters,
    assignin('base',['pshell_' num2str(Vid_shells(i))],Parameter('value',Vthick(i)));
    Xin             = eval(['add(Xin,pshell_' num2str(Vid_shells(i)) ');']);
end

%% 3.   Construct Mio object and Model object to evaluate performance function
%3.1.   Mio Object
Xm  = Mio('Sdescription', 'objective function', ...
                'Spath','./', ...
                'Sfile','response', ...
                'Coutputnames',{'Evec_cl_1','Eval_cl_1'},...
                'Liostructure',true,...
				'Lfunction',true); % This flag specify if the .m file is a script or a function. 
            
%3.2.   Evaluator object
Xeval   = Evaluator('Xmio',Xm,'Sdescription','Xevaluator xmio');
%3.3.   Model object
Xmdl    = Model('Xevaluator',Xeval,'Xinput',Xin);

%% 4.   Create objective function
Xfun1   = Function('Sdescription','objective function', ...
    'Sexpression','apply(<&Xmdl&>,<&Xin&>)','Sfield','fobj');

%% 6.   Create initial solution
Xinisol = Parameter('Sdescription','initial solution','value',Vthick);

%% 7.   Create object Xoptprob
Xop     = OptimizationProblem;     %create empty object
Xop     = add(Xop,'Sdescription','Optimization problem','Xinput',Xin,...
    'Cdesignvariables',get(Xin,'Parameter'));
Xop     = add(Xop,'Xfunction_objectivefunction',Xfun1,...
    'Xparameter_initialsolution',Xinisol);
for i=1:length(Vid_shells),
    Xop     = eval(['addSideConstraint(Xop,''Xtarget'',pshell_' ...
        num2str(Vid_shells(i)) ...
        ',''lowerBound'','  num2str(0.9*Vthick(i))...
        ',''upperBound'','  num2str(1.1*Vthick(i)) ');']);
end

%% 8.   Create object Cobyla
Xcob    = Cobyla('LKeepFiles',false,'Nmax',500,'rho_ini',0.05,'rho_end',1e-4);

%% 9.   Solve optimization problem
Xout = apply(Xcob,'OptimizationProblem',Xop);

% % mult = 0.1;
% % 
% % 
% % CINEQ11a  = @(x)x(1)-1.0*(1+mult);
% % CINEQ11b  = @(x)1.0*(1-mult)-x(1);
% % CINEQ17a  = @(x)x(2)-2.0*(1+mult);
% % CINEQ17b  = @(x)2.0*(1-mult)-x(2);
% % CINEQ19a  = @(x)x(3)-0.9*(1+mult);
% % CINEQ19b  = @(x)0.9*(1-mult)-x(3);
% % CINEQ53a  = @(x)x(4)-1.5*(1+mult);
% % CINEQ53b  = @(x)1.5*(1-mult)-x(4);
% % CINEQ71a  = @(x)x(5)-2.0*(1+mult);
% % CINEQ71b  = @(x)2.0*(1-mult)-x(5);
% % CINEQ79a  = @(x)x(6)-1.8*(1+mult);
% % CINEQ79b  = @(x)1.8*(1-mult)-x(6);
% % CINEQ82a  = @(x)x(7)-1.4*(1+mult);% or 1.5??
% % CINEQ82b  = @(x)1.4*(1-mult)-x(7);
% % CINEQ84a  = @(x)x(8)-0.8*(1+mult);
% % CINEQ84b  = @(x)0.8*(1-mult)-x(8);
% % CINEQ86a  = @(x)x(9)-1.2*(1+mult);
% % CINEQ86b  = @(x)1.2*(1-mult)-x(9);
% % CINEQ88a  = @(x)x(10)-2.5*(1+mult);
% % CINEQ88b  = @(x)2.5*(1-mult)-x(10);
% % CINEQ91a  = @(x)x(11)-1.5*(1+mult);
% % CINEQ91b  = @(x)1.5*(1-mult)-x(11);
% % CINEQ98a  = @(x)x(12)-1.2*(1+mult);
% % CINEQ98b  = @(x)1.2*(1-mult)-x(12);
% % CINEQ100a  = @(x)x(13)-1.2*(1+mult);
% % CINEQ100b  = @(x)1.2*(1-mult)-x(13);
% % CINEQ101a  = @(x)x(14)-0.9*(1+mult);
% % CINEQ101b  = @(x)0.9*(1-mult)-x(14);
% % CINEQ102a  = @(x)x(15)-1.0*(1+mult);
% % CINEQ102b  = @(x)1.0*(1-mult)-x(15);
% % CINEQ105a  = @(x)x(16)-1.8*(1+mult);
% % CINEQ105b  = @(x)1.8*(1-mult)-x(16);
% % CINEQ107a  = @(x)x(17)-1.2*(1+mult);
% % CINEQ107b  = @(x)1.2*(1-mult)-x(17);
% % CINEQ109a  = @(x)x(18)-5.0*(1+mult);
% % CINEQ109b  = @(x)5.0*(1-mult)-x(18);
% % CINEQ110a  = @(x)x(19)-1.4*(1+mult);
% % CINEQ110b  = @(x)1.4*(1-mult)-x(19);


% 
% Tinput=@(x)struct('pshell_11',x(1),'pshell_17',x(2),...
%     'pshell_19',x(3),'pshell_53',x(4),...
%     'pshell_71',x(5),'pshell_79',x(6),...
%     'pshell_82',x(7),'pshell_84',x(8),...
%     'pshell_86',x(9),'pshell_88',x(10),...
%     'pshell_91',x(11),'pshell_98',x(12),...
%     'pshell_100',x(13),'pshell_101',x(14),...
%     'pshell_102',x(15),'pshell_105',x(16),...
%     'pshell_107',x(17),'pshell_109',x(18),...
%     'pshell_110',x(19));