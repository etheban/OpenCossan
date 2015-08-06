%% Updated by HMP 14-Oct-2010

% example for Perturbation method : Building Model
% 3rd party solver : NASTRAN

%% Create the probabilistic model

% define the RVs

RVKEE6=RandomVariable('Sdistribution','lognormal', 'mean',3.5e10,'std',3.5e9); 
RVKEE7=RandomVariable('Sdistribution','lognormal', 'mean',5e9,'std',5e8);

Xrvs1 = RandomVariableSet('Cmembers',{'RVKEE6','RVKEE7'}); 
Xinp1 = Input('Sdescription','Xinput object');       
Xinp1 = add(Xinp1,Xrvs1);

%% Construct the Model

test_con   = Connector('Stype','nastran_x86_64','Smaininputfile','dummy');
test_eval  = Evaluator('Xconnector',test_con);
test_model = Model('Xevaluator',test_eval,'xinput',Xinp1);

%% Perform SFEM Analysis

Sdirectory = pwd;
test = SfemPolynomialChaos('Xmodel',test_model,'Sinputfile',[Sdirectory filesep 'BUILDING_B.dat'],...
                     'CyoungsmodulusRVs',{'RVKEE6','RVKEE7'},'Simplementation','Componentwise');

Xout = test.performAnalysis;    

getResponse(Xout,'Sresponse','max')                      



