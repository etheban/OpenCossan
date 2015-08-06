%% Unit Test for the RobustDesign object
% BG

function varargout=UnitTestRobustDesign

% Test constructor
Ntest=6;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1
% construct empty object and display
itest=1;
try
    Xrobde  = RobustDesign;
    display(Xrobde)
    Vtest(itest)=true;
    Cmess{itest}='Object created and display test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2
% Constructor with all required properties
itest=itest+1;
try
    % defined objects required for the Model
    Xvar1 = RandomVariable('Sdistribution','normal','mean',0,'std',1);
    Xrvset = RandomVariableSet('CXrandomvariables',{Xvar1},'Cmembers',{'Xvar1'});
    Xinp = Input('Xrvset',Xrvset);
    Xmio = Mio('Sscript','for i=1:length(Tinput); Toutput(i).out1= Tinput(i).Xvar1;end;',...
        'Cinputnames',{'Xvar1'},'Coutputnames',{'out1'});
    Xmodel = Model('Xevaluator',Evaluator('Xmio',Xmio),'Xinput',Xinp);
   
    
    Xdv1 = DesignVariable('value',1,'lowerbound',0.5,'upperbound',1.5);
    Xdv2 = DesignVariable('value',0,'lowerbound',-0.5,'upperbound',0.5);
    Xin = Input('CSmembers',{'Xdv1' 'Xdv2'},'CXmember',{Xdv1 Xdv2});
    
    Xobj = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out2=mean(Tinput(i).out1)+6*std(Tinput(i).out1); end', ...
                             'Cinputnames',{'out1'},'Coutputnames',{'out2'});
    
    XrobustDesign = RobustDesign('Sdescription','simple robust design', ...
        'XinnerLoopModel',Xmodel, ...
        'Xinput',Xin, ...
        'XobjectiveFunction',Xobj,...
        'Xsimulator',LatinHypercubeSampling('Nsamples',100),...
        'CSinnerLoopOutputNames',{'out1'},...
        'CdesignvariableMapping',{'Xdv1' 'Xvar1' 'std';'Xdv2' 'Xvar1' 'mean'});

    display(XrobustDesign)
    Vtest(itest)=true;
    Cmess{itest}='Object created and display test passed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 
% This test shall fail since the objective function is not passed
itest=itest+1;
try
    
    XrobustDesign = RobustDesign('Sdescription','simple robust design', ...
        'XinnerLoopModel',Xmodel, ...
        'Xinput',Xin, ...
        'Xsimulator',LatinHypercubeSampling('Nsamples',100),...
        'CSinnerLoopOutputNames',{'out1'},...
        'CdesignvariableMapping',{'Xdv1' 'Xvar1' 'std';'Xdv2' 'Xvar1' 'mean'});
    Cmess{itest}='This test shall fail since the objective function is not passed';
catch ME 
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 
% This test shall fail since out11 is not an output of the inner loop
itest=itest+1;
try
    
    XrobustDesign = RobustDesign('Sdescription','simple robust design', ...
        'XinnerLoopModel',Xmodel, ...
        'XobjectiveFunction',Xobj,...
        'Xinput',Xin, ...
        'Xsimulator',LatinHypercubeSampling('Nsamples',100),...
        'CSinnerLoopOutputNames',{'out11'},...
        'CdesignvariableMapping',{'Xdv1' 'Xvar1' 'std';'Xdv2' 'Xvar1' 'mean'});
    Cmess{itest}='This test shall fail since out11 is not an output of the inner loop';
catch ME 
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
% Perform analysis
itest=itest+1;
try
    OpenCossan.resetRandomNumberGenerator(1000); %important for the check of the results

    XrobustDesign = RobustDesign('Sdescription','simple robust design', ...
        'XinnerLoopModel',Xmodel, ...
        'Xinput',Xin, ...
        'XobjectiveFunction',Xobj,...
        'Xsimulator',LatinHypercubeSampling('Nsamples',100),...
        'CSinnerLoopOutputNames',{'out1'},...
        'CdesignvariableMapping',{'Xdv1' 'Xvar1' 'std';'Xdv2' 'Xvar1' 'mean'});
    Xoptimum=XrobustDesign.optimize('Xoptimizer',Cobyla);
    display(Xoptimum)
    assert(abs(Xoptimum.XdesignVariable.Mdata(1,end)-0.5000)<1.e-2,'Wrong result for Xdv1');
    assert(abs(Xoptimum.XdesignVariable.Mdata(2,end)+0.5000)<1.e-2,'Wrong result for Xdv2');
    Vtest(itest)=true;
    Cmess{itest} ='Apply RBOProblem';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
% Perform analysis using 2 objective functions
itest=itest+1;
try
    Xobj2 = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out2=std(Tinput(i).out1); end', ...
                             'Cinputnames',{'out1'},'Coutputnames',{'out3'});
    
    XrobustDesign = RobustDesign('Sdescription','simple robust design', ...
        'XinnerLoopModel',Xmodel, ...
        'Xinput',Xin, ...
        'CXobjectiveFunctions',{Xobj Xobj2},...
        'Xsimulator',LatinHypercubeSampling('Nsamples',100),...
        'CSinnerLoopOutputNames',{'out1'},...
        'CdesignvariableMapping',{'Xdv1' 'Xvar1' 'std';'Xdv2' 'Xvar1' 'mean'});
    Xoptimum=XrobustDesign.optimize('Xoptimizer',Cobyla);
    display(Xoptimum)
    assert(abs(Xoptimum.XdesignVariable.Mdata(1,end)-0.5000)<1.e-2,'Wrong result for Xdv1');
    assert(abs(Xoptimum.XdesignVariable.Mdata(2,end)+0.5000)<1.e-2,'Wrong result for Xdv2');
    Vtest(itest)=true;
    Cmess{itest} ='Apply RBOProblem using 2 objective functions';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
    
%% Show summary of the test
OpenCossan.cossanDisp('--------------------------------------------------------------------')
OpenCossan.cossanDisp([' Unit Test of RobustDesign (' datestr(now) ')'])
OpenCossan.cossanDisp('--------------------------------------------------------------------')
for i=1:length(Vtest)
    if Vtest(i)
        OpenCossan.cossanDisp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);  
    else
        OpenCossan.cossanDisp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
    end
end

if nargout>0
% Export name of the UnitTest
varargout{1}='RobustDesign';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end