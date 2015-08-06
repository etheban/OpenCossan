function varargout=UnitTestNeuralNetwork
%% Unit test for NeuralNetwork
%% 0. Check library
% check if the library "libfann" is available
try
    testFann
catch ME
    if strfind(ME.message,'testFann usage: ')
        OpenCossan.cossanDisp('Fann Libraries and Mex correctly installed',1)
    else
        error('openCOSSAN:UnitTest:MetaModel:NeuralNetwork', ...
            ['\n\nFann Libraries or Mex file not installed correctly\nThe Unit Test can not be performed\n' ME.message])
    end
    
end

% Test constructor
Ntest=29;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% Prepare a Test Problem
RV=RandomVariable('Sdistribution','normal', 'mean',0,'std',1);

%% Define the RVset
% all RVs
Xrvs = RandomVariableSet('Xrv',RV,'Nrviid',10);

%% Define Xinput
Xin = Input('Sdescription','Input truss');
Xin = add(Xin,Xrvs);

%% Construct a Mio object
% the mio will solve for the eigenvalues, eigenvectors and the mass matrix

Xm=Mio('Sdescription', 'Simple model', ...
    'Sscript','Moutput=sum(Minput(:,2:5),2)+Minput(:,6).^2+prod(Minput(:,7:9),2)./Minput(:,10);', ...
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
Xpf=PerformanceFunction('Scapacity','RV_1','Sdemand','RV_2','SoutputName','vg');
Xprobmdl=ProbabilisticModel('Xmodel',Xmdl,'XperformanceFunction',Xpf);

%% 1 Create an empty object
% get public properties and Cmethods
itest = 1;

try
    Xobj = NeuralNetwork;
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 2. Constructor
clear Xobj
itest =itest + 1;
try
    Xobj = NeuralNetwork('Xfullmodel',Xmdl, ...
        'Sdescription','Unit Test 2', ...
        'Stype','Linear', ...
        'Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Constructor';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3. Constructor with wrong FieldName
clear Xobj
itest =itest + 1;
try
    Xobj = NeuralNetwork('Xfullmodel',Xmdl,...
        'Sunexisting','interaction', ...
        'Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'},'Lplot',true);
    display(Xobj)
    Cmess{itest}='Constructor with wrong FieldName. It should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4. Test Constructors
% This should fail because Sresponse does not exist in the model
clear Xobj
itest =itest + 1;
try
    Xobj = NeuralNetwork(...
        'Xfullmodel',Xmdl,...
        'Stype','linear', ...
        'Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'notexisting'});
    display(Xobj)
    Cmess{itest}='This should fail because Sresponse does not exist in the model';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5. Test Constructors
% Use a Probabilistic Model as a Full Model
clear Xobj
itest =itest + 1;
try
    Xobj = NeuralNetwork(...
        'Xfullmodel',Xprobmdl,...
        'Stype','linear', ...
        'Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'notexisting'});
    display(Xobj)
    Cmess{itest}='Number of layer should be computed automatically. ';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6. Test Constructors
% Number of layer should be computed automatically.
clear Xobj
itest =itest + 1;
try
    Xobj = NeuralNetwork('Xfullmodel',Xmdl,...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Number of layer should be computed automatically. ';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7. Constructor with calibration
% Try to add Xinput without Samples
clear Xobj
itest =itest + 1;
try
    Xobj = NeuralNetwork('Xfullmodel',Xmdl,...
        'XcalibrationInput',Xin, ...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=Xobj.calibrate;
    display(Xobj)
    Cmess{itest}='It should fail. No meaninful messages returned';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=ME.message ;
end

%% 8. Constructor with calibration
clear Xobj
itest =itest + 1;
try
    Xin=Xin.sample('Nsamples',20);
    Xobj = NeuralNetwork('Xfullmodel',Xmdl,...
        'XcalibrationInput',Xin, ...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=Xobj.calibrate;
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9. Constructor with calibration
clear Xobj
itest =itest + 1;
try
    Xin=Xin.sample('Nsamples',20);
    Xobj = NeuralNetwork('Xfullmodel',Xmdl,...
        'XcalibrationInput',Xin, ...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=Xobj.calibrate;
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Constructor with calibration. NN and generate samples from the Model. It should work';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10. Constructor with calibration
clear Xobj
itest =itest + 1;

try
    Xout=Xmdl.apply(Xin);
    Xobj = NeuralNetwork('Xfullmodel',Xmdl,...
        'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout, ...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=Xobj.calibrate;
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Constructor with calibration. NN and generate samples from the Model. It should work';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11. Test Constructors
% This should work! A full model should be not required if the input/output
% data are available.
clear Xobj
itest =itest + 1;
try
    Xobj = NeuralNetwork( ...
        'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout, ...
        'XvalidationInput',Xin, ...
        'XvalidationOutput',Xout, ...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=Xobj.calibrate;
    Xobj=Xobj.validate;
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='A full model should be not required if the input/output data are available. NN and generate samples from the Model. It should work';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12. Test Constructors
% This should fail because the full model is not passed and the training
% data are not passed
clear Xobj
itest =itest + 1;
try
    Xobj = NeuralNetwork( ...
        'XvalidationInput',Xin, ...
        'XvalidationOutput',Xout, ...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=Xobj.validate;
    display(Xobj)   
    Cmess{itest}='This should fail because the full model is not passed and the training data are not passed. NOTE BY MB: no, you can pass the calibration data as input of method calibrate!';
catch ME
     Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 13. Calibrate NN and generate samples from the Model.
% This should work
clear Xobj
itest =itest + 1;
try
    Xobj = NeuralNetwork( ...
        'Xfullmodel',Xmdl,...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'Xsimulator',MonteCarlo('Nsamples',200));
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='NN and generate samples from the Model. It should work';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 14. Training
% This should work
clear Xobj
itest =itest + 1;
try
    Xobj = NeuralNetwork( ...
        'Xfullmodel',Xmdl,...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout);
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Test Training. It should work';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 15. Test Validation
clear Xobj
itest =itest + 1;

try
    Xobj = NeuralNetwork( ...
        'Xfullmodel',Xmdl,...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'Xsimulator',MonteCarlo('Nsamples',200));
    Xobj=validate(Xobj,'Xsimulator',LatinHypercubeSampling('Nsamples',200));
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 16. Test Validation
% This should work
clear Xobj
itest = 1+itest;
try
    Xobj = NeuralNetwork( ...
        'Xfullmodel',Xmdl,...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=Xobj.calibrate('XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout);
    Xobj=Xobj.validate('XvalidationInput',Xin, ...
        'XvalidationOutput',Xout);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 17. Validation
% This should fail because wrong input arguments
clear Xobj
itest = 1+itest;
try
    Xobj = NeuralNetwork( ...
        'Xfullmodel',Xmdl,...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'XvalidationInput',Xin, ...
        'XvalidationOutput',Xout);
    display(Xobj)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 18. Validation
% This should fail because wrong input class
clear Xobj
itest = 1+itest;
try
    Xobj = NeuralNetwork( ...
        'Xfullmodel',Xmdl,...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'Xsimulator',Xin);
    display(Xobj)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 19. Validation
clear Xobj
itest = 1+itest;
try
    Xobj = NeuralNetwork( ...
        'Xfullmodel',Xmdl,...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout);
    Xobj=validate(Xobj,'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout);
    display(Xobj)
    Cmess{itest}='This should fail because wrong input arguments';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 20. Validation
itest = 1+itest;
clear Xobj
try
    Xobj = NeuralNetwork( ...
        'Xfullmodel',Xmdl,...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=validate(Xobj,'XvalidationInput',Xin, ...
        'XvalidationInput',Xout);
    display(Xobj)
    Cmess{itest}='This should fail because the object has not been calibrate';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 21. Test apply
clear Xobj
itest = 1+itest;
try
    Xobj = NeuralNetwork( ...
        'Xfullmodel',Xmdl,...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout);
    Xout=Xobj.apply(Xin);
    display(Xout)
    Vtest(itest)=true;
    Cmess{itest}='Test apply method with Input object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 22. Test apply
% This should work
clear Xobj
itest = 1+itest;
try
    Xobj = NeuralNetwork( ...
        'Xfullmodel',Xmdl,...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout);
    Xout=Xobj.apply(Xin.Xsamples);
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Test apply method with Samples object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 23  Test plot
clear Xobj
itest = itest+1;
try
    Xobj = NeuralNetwork( ...
        'Xfullmodel',Xmdl,...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    
    Xobj = Xobj.calibrate('Xsimulator',LatinHypercubeSampling('Nsamples',500));
    h1=Xobj.plotregression('Stype','calibration','SoutputName','out');
    Xobj = Xobj.validate('Xsimulator',MonteCarlo('Nsamples',20));
    h2=Xobj.plotregression('Stype','validation','SoutputName','out');
    close(h1)
    close(h2)
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 24
% Check the effect of passing a wrong argument to Stype
clear Xobj
itest = itest+1;
try
    Xobj = NeuralNetwork( ...
        'Xfullmodel',Xmdl,...
        'Stype',';)','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    display(Xobj)
    Cmess{itest}='This should fail because of a wrong Stype';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 25
clear Xobj
itest = itest+1;
try
    Xobj = NeuralNetwork( ...
        'Xfullmodel',Xmdl,...
        'Stype','HyperbolicTangent','Vnnodes',[10 3 2 1], ...
        'Coutputnames',{'out'});
    
    if ~Xobj.Lcalibrated
        Xobj=calibrate(Xobj,'XcalibrationInput',Xin, ...
            'XcalibrationOutput',Xout);
        if Xobj.Lcalibrated
            Vtest(itest)=true;
        else
            Cmess{itest}='The flag Lcalibrated is not set correctly. ';
        end
    else
        Cmess{itest}='The flag Lcalibrated is not set correctly. ';
    end
    
    if ~Xobj.Lvalidated % the response surface has not yet been validated
        Vtest(itest)=true;
        Xobj=validate(Xobj,'XvalidationInput',Xin, ...
            'XvalidationOutput',Xout);
        if Xobj.Lvalidated
            Vtest(itest)=true;
        else
            Cmess{itest}='The flag Lvalidated is not set correctly. ';
        end
    else
        Cmess{itest}=[Cmess{itest} 'The flag Lvalidated is not set correctly.'];
    end
    Cmess{itest}='Check the correct settings of the flags Ltrained and Lvalidated';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 26 Plot regression
% Test the method plotregression
itest = itest+1;
try
    Xobj.plotregression('Stype','calibration','Soutputname','out')
    Vtest(itest)=true;
    Cmess{itest}='Test the method plotregression';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 27 Plot regression
% wrong output name
itest = itest+1;
try
    Xobj.plotregression('Stype','calibration','Soutputname','f1')
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 28 Plot exportFigure
itest = itest+1;
try
    Xobj.plotregression('Stype','calibration','Soutputname','out','SfigureName','FigExported1')
    
    if ~exist(fullfile(OpenCossan.getCossanWorkingPath,'FigExported1.eps'),'file')
        error('Figure FigExported1 not exported') %#ok<ERTAG>
    end
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 29 Plot exportFigure
itest = itest+1;
try
    Xobj.plotregression('Stype','validation','Soutputname','out','SfigureName','FigExported2')
    if ~exist(fullfile(OpenCossan.getCossanWorkingPath,'FigExported2.eps'),'file')
        error('Figure FigExported2 not exported') %#ok<ERTAG>
    end
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

close all;

if nargout>0
    % Export name of the UnitTest
    varargout{1}='NeuralNetwork';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    %% Show summary of the test
    OpenCossan.cossanDisp('--------------------------------------------------------------------',1)
    OpenCossan.cossanDisp([' Unit Test of NeuralNetwork (' datestr(now) ')'],1)
    OpenCossan.cossanDisp([' Successful: ' num2str(100*(sum(Vtest)/length(Vtest))) '%'],1)
    OpenCossan.cossanDisp('--------------------------------------------------------------------',1)
    for i=1:length(Vtest)
        if Vtest(i)
            OpenCossan.cossanDisp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ],1);
        else
            OpenCossan.cossanDisp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ],1);
        end
    end
end
end
