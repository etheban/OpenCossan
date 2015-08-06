%% Initialize additional variables used to test MonteCarlo
% Define RVs
try 
RV1=RandomVariable('Sdistribution','normal', 'mean',0,'std',1);
RV2=RandomVariable('Sdistribution','normal', 'mean',0,'std',1); 
Xpar=Parameter('value',1);
Xfun=Function('Sexpression','<&Xpar&>+<&RV1&>'); 

% Define the RVset
Xrvs1=RandomVariableSet('Cmembers',{'RV1', 'RV2'},'CXrv',{RV1, RV2}); 

% Define GaussianMixtureRandomVariableSet
SIGMA1=[0.8^2, 0.936*0.8*0.1;  0.936*0.8*0.1 0.1^2;];
SIGMA2=[0.1^2, 0.936*0.8*0.1;  0.936*0.8*0.1 0.8^2;];
SIGMA3=eye(2);
SIGMA=zeros(2,2,3);
SIGMA(:,:,1)=SIGMA1;SIGMA(:,:,2)=SIGMA2;SIGMA(:,:,3)=SIGMA3;
Xg = GaussianMixtureRandomVariableSet('Cmembers',{'RV3','RV4'},'MdataSet',[5 5; -5 -5; 0 0],'Mcovariance',SIGMA);


% Define RVs
RV5=RandomVariable('Sdistribution','uniform', 'mean',2,'std',1); 
RV6=RandomVariable('Sdistribution','uniform', 'mean',2,'std',1);  

% Define the RVset
Xrvs2=RandomVariableSet('Cmembers',{'RV5', 'RV6'},'CXrv',{RV5, RV6}); 

% Define Xinput
Xin = Input('Sdescription','Input for mio');
Xthreshold=Parameter('Sdescription','Define threshold','value',2);
Xin = add(Xin,Xrvs1);
Xin = add(Xin,Xpar);
Xin = add(Xin,Xfun);

Xin2 = Input('Sdescription','Input for mio');
Xin2 = add(Xin2,Xg);
Xin2 = add(Xin2,Xrvs2);
Xin2 = add(Xin2,Xthreshold);

% Construct a Mio object
Xm=Mio('Sdescription', 'Performance function', ...
    'Sscript','for j=1:length(Tinput), Toutput(j).out= Tinput(j).RV2; end', ...
    'Liostructure',true,...
    'Lfunction',false,...
    'Cinputnames',{'RV1','RV2'},...
    'Coutputnames',{'out'}); 

% Construct a Mio object
Xm2=Mio('Sdescription', 'Performance function', ...
    'Sscript','for j=1:length(Tinput), Toutput(j).out= Tinput(j).RV3; end', ...
    'Liostructure',true,...
    'Lfunction',false,...
    'Cinputnames',{'RV3','RV4'},...
    'Coutputnames',{'out'}); 
            
% Construct the Evaluator
Xeval = Evaluator('Xmio',Xm,'Sdescription','Evaluator for Mio Xm');
Xeval2 = Evaluator('Xmio',Xm2,'Sdescription','Evaluator for Mio Xm');


% Define a Model
Xmdl=Model('Xevaluator',Xeval,'Xinput',Xin);
Xmdl2=Model('Xevaluator',Xeval2,'Xinput',Xin2);

% Construct the performance function
%Xpf=PerformanceFunction('Sdemand','out','Scapacity','Xthreshold','SoutputName','Vg');
% DO NOT CALL "Xpf" THE PERFORMANCE FUNCTION!
Xperffun=PerformanceFunction('Sdemand','RV1','Scapacity','Xpar','SoutputName','Vg');
% Construct a ProbabilisticModel Object
Xpm=ProbabilisticModel('XModel',Xmdl,'XPerformanceFunction',Xperffun); %#ok<SNASGU>
Xpm2=ProbabilisticModel('XModel',Xmdl2,'XPerformanceFunction',Xperffun); %#ok<SNASGU>

Xg=Gradient('Vgradient',[0.095 0.05],'Cnames',{'RV1' 'RV2'});

catch ME
    disp(['Error in the model definition\n' ME.message]);
end
