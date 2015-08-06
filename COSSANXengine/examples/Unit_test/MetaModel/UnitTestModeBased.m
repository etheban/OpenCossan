function varargout=UnitTestModeBased

% Test constructor
Ntest=12;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% Prerequisites
itest = 1;
try
    %% Prepare a Test Problem
    RV1=RandomVariable('Sdistribution','normal', 'mean',0,'std',1);
    RV2=RandomVariable('Sdistribution','normal', 'mean',0,'std',1);
    %% Define the RVset
    % all RVs
    Xrvs = RandomVariableSet('CXrv',{RV1 RV2},'Cmembers',{'RV1' 'RV2'});
    
    %% Define Xinput
    Xin = Input('Sdescription','Input truss','XrandomVariableSet',Xrvs);
    
    %% Construct a Mio object
    % the mio will solve for the eigenvalues, eigenvectors and the mass matrix
    
    Xm=Mio('Sdescription', 'Simple model', ...
        'Sscript','Moutput=sum(Minput,2);', ...
        'Cinputnames',Xin.Cnames,...
        'Coutputnames',{'out'},...
        'Liostructure',false,...
        'Liomatrix',true,...
        'Lfunction',false); % This flag specify if the .m file is a script or a function.
    
    %%
    
    %% Construct the Evaluator
    Xeval = Evaluator('Xmio',Xm,'Sdescription','Evaluator xmio');
    
    %% create the Xmodel
    Xmdl=Model('Xevaluator',Xeval,'Xinput',Xin);
    % Test Model
    Xout=Xmdl.deterministicAnalysis;
    Cmess{itest}='Prerequisite created correctly';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create an empty object
% get public properties and Cmethods
itest = itest+1;

try
    Xobj = ModeBased;
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 3. Constructor with sub minimal inputs
% get public properties and Cmethods
itest =itest + 1;

try
    Xobj = ModeBased('Xfullmodel',Xmdl);
    display(Xobj)
catch ME
    if strcmp(ME.identifier,'openCOSSAN:ModeBased')
       Vtest(itest)=true; 
       Cmess{itest}=[ME.identifier ' -- ' ME.message];
    else
       Cmess{itest}=['It should fail in the constructor not somewhere else. ' ME.message];
    end
end

%% 4. Test Constructors
% Shall fail: no names of the eigenvalues and eigenvectors specified
itest =itest + 1;
try
    Xobj = ModeBased('Xfullmodel',Xmdl,...
        'Vmodes',[ 1 4 5], ...
        'Vmkmodes',[3 4 5],'Mmass0',rand(2));
    display(Xobj)
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 5. Prepare more meaningful model
itest=1+itest;
try
    mass1=RandomVariable('Sdistribution','normal', 'mean',1e0,'std',0.025*1e0);
    stiffness1=RandomVariable('Sdistribution','normal', 'mean',1e3,'std',0.025*1e3);
    
    %% Define the RVset
    % all RVs
    Xrvs1 = RandomVariableSet('Xrv',mass1,'Nrviid',33);
    Xrvs2 = RandomVariableSet('Xrv',stiffness1,'Nrviid',131);
    
    %% Define Xinput
    Xin = Input('Sdescription','Input truss');
    Xin = add(Xin,Xrvs1);
    Xin = add(Xin,Xrvs2);
    
    %% Construct a Mio object
    % the mio will solve for the eigenvalues, eigenvectors and the mass matrix
    Xm=Mio('Sdescription', 'Performance function', ...
        'Spath',fullfile(cossanRoot,'examples','Tutorials','metamodel'),  ...
        'Sfile','gen_truss', ...
        'Cinputnames',Xin.CnamesRandomVariable, ...
        'Coutputnames',{'mass','stiff','MPhi','Vlambda'}, ...
        'Liostructure',true,...
        'Lfunction',true); % This flag specify if the .m file is a script or a function.
    
   
    %% Construct the Evaluator
    Xeval = Evaluator('Xmio',Xm,'Sdescription','Evaluator xmio');
    
    %% create the Xmodel
    Xmdl=Model('Xevaluator',Xeval,'Xinput',Xin);
    Vtest(itest)=true;
    Cmess{itest}='Full model prepared';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6. Test Constructors
% This should fail because a random Property Name is passed
itest = 1+itest;
try
    Xobj = ModeBased('Xfullmodel',Xmdl,...
        'Vmodes',[ 1 4 5], ...
        'Vmkmodes',[3 4 5],'Cnamesmodalprop',{'Vlambda','MPhi'},'Mmass0',eye(99));
    display(Xobj)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 7. Test Constructors
% This should fail because a random Property Name is passed
itest = 1+itest;
try
    Xobj = ModeBased('Xfullmodel',Xmdl,...
        'Vmodes',[ 1 4 5], ...
        'Vmkmodes',[3 4 5],'Cnamesmodalprop',{'Idontexist','MPhi'},'Mmass0',eye(99));
    display(Xobj)
catch ME
    if strcmp(ME.identifier,'openCOSSAN:ModeBased')
       Vtest(itest)=true; 
       Cmess{itest}=[ME.identifier ' -- ' ME.message];
    else
       Cmess{itest}=['It should fail in the constructor not somewhere else. ' ME.message];
    end
    
end


%% 8. Test Constructors
% This should fail because a random Property Name is passed
itest = 1+itest;
try
    Xobj = ModeBased('Xfullmodel',Xmdl,...
        'Vmodes',[ 1 4 5], ...
        'Vmkmodes',[3 4 5],'Cnamesmodalprop',{'MPhi'},'Mmass0',eye(99));
    display(Xobj)
catch ME
    if strcmp(ME.identifier,'openCOSSAN:ModeBased')
       Vtest(itest)=true; 
       Cmess{itest}=[ME.identifier ' -- ' ME.message];
    else
       Cmess{itest}=['It should fail in the constructor not somewhere else. ' ME.message];
    end
    
end

%% 9. Test calibration
% This should fail since the number of calibration samples is not specified 
itest = 1+itest;
try
    Xobj = ModeBased('Xfullmodel',Xmdl,...
        'Vmodes',[ 1 4 5], ...
        'Vmkmodes',[3 4 5],'Cnamesmodalprop',{'Vlambda','MPhi'},'Mmass0',eye(99));
    Xobj=Xobj.calibrate;
    display(Xobj)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10. Test calibration
% This should fail since the model does not contain these outputs
itest = 1+itest;
try
    Xobj = ModeBased('Xfullmodel',Xmdl,...
        'Vmodes',[ 1 4 5], ...
        'Vmkmodes',[3 4 5],'Cnamesmodalprop',{'Vlambda','MPhi'},'Mmass0',eye(99));
    Xobj=Xobj.calibrate('Nsamples',10);
    display(Xobj)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11. Test Constructors
% This should work! A full model should be not required if the input/output
% data are available.
itest = 1+itest;
try
    load training_validation_IO; 
    Xout = Xmdl.deterministicAnalysis;
    Xmodes0 = Modes('MPhi',randn(99,20),'Vlambda',rand(20,1));
    Xobj = ModeBased( ...
        'XcalibrationInput',XtrainingInput, ...
        'XcalibrationOutput',XtrainingOutput, ...
        'XvalidationInput',XvalidationInput, ...
        'XvalidationOutput',XvalidationOutput,...
        'Mmass0',eye(99),...
        'Xmodes0',Xmodes0,...
        'Vmodes',[ 1 4 5], ...
        'Vmkmodes',[3 3 3]);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12. Test Constructors
% This should fail because the full model is not passed and the training
% data are not passed
itest = 1+itest;
try
    Xobj = ModeBased( ...
        'XvalidationInput',Xin, ...
        'XvalidationOutput',Xout);
    display(Xobj)
    Cmess{itest}='This should fail because the full model is not passed and the training data are not passed';
catch ME
       Vtest(itest)=true; 
       Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


close all;

if nargout>0
    % Export name of the UnitTest
    varargout{1}='ModeBased';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of ModeBased (' datestr(now) ')'])
    disp('--------------------------------------------------------------------')
    for i=1:length(Vtest)
        if Vtest(i)
            disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
        else
            disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
        end
    end
end
end
