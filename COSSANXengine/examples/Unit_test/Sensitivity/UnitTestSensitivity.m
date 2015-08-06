function varargout = UnitTestSensitivity
%UNITTESTEVALUATOR Summary of this function goes here
%   Detailed explanation goes here

% Test Sensitivity
% Perallocate memory

Ntest=32;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

StestPath = fullfile(OpenCossan.getCossanRoot,'examples','Unit_test','Sensitivity');

%% Setup the test problem 

Xrv1   = RandomVariable('Sdistribution','uniform','lowerbound',-1,'upperbound',1);
Xrv2   = RandomVariable('Sdistribution','uniform','lowerbound',-1,'upperbound',1);
Xrv3   = RandomVariable('Sdistribution','uniform','lowerbound',-1,'upperbound',1);
Xrvset = RandomVariableSet('Cmembers',{'Xrv1','Xrv2','Xrv3'},'CXrandomvariables',{Xrv1,Xrv2,Xrv3});
Xin    = Input('XrandomVariableSet',Xrvset);
Xm     = Mio('Cinputnames',{'Xrv1','Xrv2','Xrv3'}, ...
             'Coutputnames',{'Y'},...
             'Spath',StestPath,...
             'Sfile','testfunction.m',...
             'Lfunction',true,'Liomatrix',false,'Liostructure',false);
Xev    = Evaluator('Xmio',Xm);
Xmdl   = Model('Xinput',Xin,'Xevaluator',Xev);


%% testing sobolIndices

%% 1 Create an empty object
itest = itest+1;
try
    % It is possible. 
    Xobj = Sensitivity;
    display(Xobj)
    Cmess{itest}='Create an empty object';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 No model defined
itest = itest+1;
try
    Xsm = Sensitivity.sobolIndices('Nbootstrap',10);  
    display(Xsm)
    Cmess{itest}='No model defined, it should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 No Xsimulation defined
itest = itest+1;
try
    Xsm = Sensitivity.sobolIndices('Xmodel',Xmdl);
    display(Xsm)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 - should work
itest = itest+1;
try
    Xmc=MonteCarlo('Nsamples',5);
    Xsm = Sensitivity.sobolIndices('Xmodel',Xmdl,'Xsimulation',Xmc);
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 - Nbootstrap should be an integer
itest = itest+1;
try
    Xsm = Sensitivity.sobolIndices('Xmodel',Xmdl,'Xsimulation',Xmc,'Nbootstrap',0);
    display(Xsm);
    Vtest(itest)=true;
catch ME
   Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 - Defining CSinputNames 
itest = itest+1;
try
    Xsm = Sensitivity.sobolIndices('Xmodel',Xmdl,'Xsimulation',Xmc,'CSinputNames',{'Xrv1'});
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 - Defining nonexisting RV in CSinputNames 
itest = itest+1;
try
    Xmc=MonteCarlo('Nsamples',10);
    Xsm = Sensitivity.sobolIndices('Xmodel',Xmdl,'Xsimulation',Xmc,'CSinputNames',{'Xrv4'});
    display(Xsm);
    Cmess{itest}='Nonexisting RV defined within Csinputnames - it should provide error';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 8 - Using structure within MIO
itest = itest+1;
Xm = Mio('Sscript','for j=1:length(Tinput), Toutput(j).out1=Tinput(j).Xrv1^2+2*Tinput(j).Xrv2-Tinput(j).Xrv3; end', ...
         'Coutputnames',{'out1'},...
         'Cinputnames',{'Xrv1' 'Xrv2' 'Xrv3'},...
         'Liostructure',true,...
	     'Lfunction',false); 
Xev    = Evaluator('Xmio',Xm);
Xmdl   = Model('Xinput',Xin,'Xevaluator',Xev);
try
    Xsm = Sensitivity.sobolIndices('Xmodel',Xmdl,'Xsimulation',Xmc);
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% testing upperbounds

% 9 - should work
itest = itest+1;
try
    Xsm = Sensitivity.upperBounds('Xmodel',Xmdl,'Nbootstrap',3,'Nsamples',4);
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 10 - Nsamples is not an integer
itest = itest+1;
try
    Xsm = Sensitivity.upperBounds('Xmodel',Xmdl,'Nsamples',1.6);
    display(Xsm);
    Cmess{itest}='This should fail in the validateCossanInputs';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 11 - Nsamples is negative
itest = itest+1;
try
    Xsm = Sensitivity.upperBounds('Xmodel',Xmdl,'Nsamples',-6);
    display(Xsm);
    Cmess{itest}='This should fail in the validateCossanInputs';
catch ME
    Vtest(itest)=true;
     Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 12 - Nsamples is not defined
itest = itest+1;
try
    Xsm = Sensitivity.upperBounds('Xmodel',Xmdl);
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 13 - Defining nonexisting RV in CSinputNames 
itest = itest+1;
try
    Xsm = Sensitivity.sobolIndices('Xmodel',Xmdl,'Xsimulation',Xmc,'CSinputNames',{'Xrv4'});
    display(Xsm);
    Cmess{itest}='Nonexisting RV defined within Csinputnames - it should provide error';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 14 - This should work
itest = itest+1;
try
    Xsm = Sensitivity.sobolIndices('Xmodel',Xmdl,'Xsimulation',Xmc,'CSinputNames',{'Xrv1' 'Xrv2' 'Xrv3'});
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% testing randomBalanceDesign

% 15 -  should work
itest = itest+1;
try
    Xsm = Sensitivity.randomBalanceDesign('Xmodel',Xmdl,'Nbootstrap',1,'Nsamples',5);
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 16 - should work
itest = itest+1;
try
    Xsm = Sensitivity.randomBalanceDesign('Xmodel',Xmdl,'Nharmonics',1);
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% testing gradient MC

% 17 - No Xsamples defined
itest = itest+1;
try
    Xsm = Sensitivity.gradientMonteCarlo('Xtarget',Xmdl);
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 18 - should work
itest = itest+1;
try
    Xsm = Sensitivity.gradientMonteCarlo('Xtarget',Xmdl,'tolerance',0.2,'perturbation',0.1);
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 19 -  
itest = itest+1;
try
    Xsm = Sensitivity.gradientMonteCarlo('Xtarget',Xmdl,'perturbation',0.01);
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 20 - No Xmodel defined
itest = itest+1;
try
    Xsm = Sensitivity.gradientMonteCarlo('perturbation',0.01);
    display(Xsm);
    Cmess{itest}='No model defined, it should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 21 -  Vreferencepoint with wrong dimension
itest = itest+1;
try
    Xsm = Sensitivity.gradientMonteCarlo('Xtarget',Xmdl,'perturbation',0.01, ...
        'VreferencePoint',[0 0.1],'CSnames',{'Xrv1','Xrv3'});
    display(Xsm);
    % The VreferencePoint is not valid!
catch ME
    Vtest(itest)=true; 
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


% 22 - using Xsamples
itest = itest+1;
try
    Xin=Xin.sample('Nsamples',5);
    Xs=Xin.Xsamples;
    Xsm = Sensitivity.gradientMonteCarlo('Xtarget',Xmdl,'Xsamples',Xs);
    display(Xsm);
    Cmess{itest}='This should fail, only 1 sample can be used to define the reference point';
catch ME
    Vtest(itest)=true;  
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 23 - using Xsamples
itest = itest+1;
try
    Xin=Xin.sample('Nsamples',1);
    Xs=Xin.Xsamples;
    Xsm = Sensitivity.gradientMonteCarlo('Xtarget',Xmdl,'Xsamples',Xs);
    display(Xsm);
    Vtest(itest)=true;  
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% testing gradient FD

% 24 -  
itest = itest+1;
try
    Xg=Sensitivity.gradientFiniteDifferences('Xtarget',Xmdl,'Xsamples',Xs,'functionValue',0.5);
    display(Xg);
    Vtest(itest)=true;  
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Testing local MC
% 25 - No Xsamples defined
itest = itest+1;
try
    Xsm = Sensitivity.localMonteCarlo('Xtarget',Xmdl);
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 26 - should work
itest = itest+1;
try
    Xsm = Sensitivity.localMonteCarlo('Xtarget',Xmdl,'tolerance',0.2,'perturbation',0.1);
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 27  
itest = itest+1;
try
    Xsm = Sensitivity.localMonteCarlo('Xtarget',Xmdl,'perturbation',0.01);
    display(Xsm);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 28 - No Xmodel defined
itest = itest+1;
try
    Xsm = Sensitivity.localMonteCarlo('perturbation',0.01);
    display(Xsm);
    Cmess{itest}='No model defined, it should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 29 -  Vreferencepoint with wrong dimension
itest = itest+1;
try
    Xsm = Sensitivity.localMonteCarlo('Xtarget',Xmdl,'perturbation',0.01, ...
        'VreferencePoint',[0 0.1],'CSnames',{'Xrv1','Xrv3'});
    display(Xsm);
    Cmess{itest}='Should fail - The VreferencePoint with wrong dimension';
catch ME
    Vtest(itest)=true; 
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


% 30 - using Xsamples
itest = itest+1;
try
    Xin=Xin.sample('Nsamples',5);
    Xs=Xin.Xsamples;
    Xsm = Sensitivity.localMonteCarlo('Xtarget',Xmdl,'Xsamples',Xs);
    display(Xsm);
    Cmess{itest}='This should fail, only 1 sample can be used to define the reference point';
catch ME
    Vtest(itest)=true;  
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 31 - using Xsamples
itest = itest+1;
try
    Xin=Xin.sample('Nsamples',1);
    Xs=Xin.Xsamples;
    Xsm = Sensitivity.localMonteCarlo('Xtarget',Xmdl,'Xsamples',Xs);
    display(Xsm);
    Vtest(itest)=true;  
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% testing gradient FD

% 32 -  
itest = itest+1;
try
    Xin=Xin.sample('Nsamples',1);
    Xs=Xin.Xsamples;
    Xg=Sensitivity.localFiniteDifferences('Xtarget',Xmdl,'Xsamples',Xs,'functionValue',0.5);
    display(Xg)
    Vtest(itest)=true;  
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Finalize the test

if nargout>0
% Export name of the UnitTest
varargout{1}='Sensitivity';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of the Sensitivity (' datestr(now) ')'])
disp([' Successful: ' num2str(100*(sum(Vtest)/length(Vtest))) '%'])
disp('--------------------------------------------------------------------')
for i=1:length(Vtest)
    if Vtest(i)
        disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
    else
        disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
    end
end

end



