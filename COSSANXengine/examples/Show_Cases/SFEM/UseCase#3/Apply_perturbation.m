%% Updated by HMP 20-Aug-2010

% example for Perturbation method : Building Model
% 3rd party solver : NASTRAN
%
% MODAL ANALYSIS 

%% Create the probabilistic model

RVKEE1=RandomVariable('Sdistribution','normal', 'mean',5e9,'std',5e8);
RVKEE2=RandomVariable('Sdistribution','normal', 'mean',3.5e10,'std',3.5e9);
RVKEE3=RandomVariable('Sdistribution','normal', 'mean',3.5e10,'std',3.5e9);
RVKEE4=RandomVariable('Sdistribution','normal', 'mean',2.5e10,'std',2.5e9);

RVM1=RandomVariable('Sdistribution','normal', 'mean',1500,'std',150); 
RVM2=RandomVariable('Sdistribution','normal', 'mean',2500,'std',250); 
RVM3=RandomVariable('Sdistribution','normal', 'mean',2500,'std',250); 
RVM4=RandomVariable('Sdistribution','normal', 'mean',2500,'std',250); 

Xrvs1 = RandomVariableSet('Cmembers',{'RVKEE1','RVKEE2','RVKEE3','RVKEE4',...
                           'RVM1','RVM2','RVM3','RVM4'}); 
Xinp1 = Input('Sdescription','Xinput object');       
Xinp1 = add(Xinp1,Xrvs1);

%% Construct the Model

test_con   = Connector('Stype','nastran_x86_64','Smaininputfile','dummy');
test_eval  = Evaluator('Xconnector',test_con);
test_model = Model('Xevaluator',test_eval,'xinput',Xinp1);

%% Perform SFEM Analysis

Sdirectory = pwd;
test = Perturbation('Xmodel',test_model,'Sanalysis','Modal','Sinputfile',[Sdirectory filesep 'BUILDING.dat'],...
                    'CdensityRVs',{'RVM1','RVM2','RVM3','RVM4'},...
                    'CyoungsmodulusRVs',{'RVKEE1','RVKEE2','RVKEE3','RVKEE4'});
                   
Xout = test.performAnalysis;    

getResponse(Xout,'Sresponse','specific','Nmode',2)


