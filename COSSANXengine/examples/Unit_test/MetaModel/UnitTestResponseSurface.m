function varargout=UnitTestResponseSurface

% Test constructor
Ntest=30;
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



%% 1 Create an empty object
% get public properties and Cmethods
itest = 1;

try
    Xobj = ResponseSurface;
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 2. Constructor
% get public properties and Cmethods
itest =itest + 1;

try
    Xobj = ResponseSurface('Xfullmodel',Xmdl,...
        'Stype','linear', ...
        'Coutputnames',{'out'});
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3. Test Constructors
% calibrate MetaModel by passing the number of samples to the calibrate
% method
itest =itest + 1;
try
    Xobj = ResponseSurface('Xfullmodel',Xmdl,...
        'Stype','linear');
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4. Test Constructors
% create a custom repsonse surface without specifying the maximum exponent
% or the custom model; this test should fail
itest =itest + 1;
try
    Xobj = ResponseSurface('Xfullmodel',Xmdl,...
        'Stype','custom');
    display(Xobj)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5. Test Constructors
% create a custom repsonse surface 
itest =itest + 1;
try
    Xobj = ResponseSurface('Xfullmodel',Xmdl,...
        'Stype','custom',...
        'NmaximumExponent',3);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6. Test Constructors
% This should fail because Sresponse does not exist in the model
itest = 1+itest;
try
    Xobj = ResponseSurface(...
        'Xfullmodel',Xmdl,...
        'Stype','interaction', ...
        'Coutputnames',{'notexisting'});
    display(Xobj)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7. Test Constructors
% This should fail because a random Property Name is passed
itest = 1+itest;
try
    Xobj = ResponseSurface(...
        'Snonsense','I have no meaning');
    display(Xobj)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8. Constructor with calibration
% Try to add Xinput without Samples
itest = 1+itest;
try
    Xobj = ResponseSurface('Xfullmodel',Xmdl,...
        'XcalibrationInput',Input, ...
        'Coutputnames',{'out'});
    Xobj=Xobj.calibrate;
    display(Xobj)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9. Constructor with calibration
itest = 1+itest;
try
    Xin=Xin.sample('Nsamples',15);
    Xobj = ResponseSurface('Xfullmodel',Xmdl,...
        'XcalibrationInput',Xin, ...
        'Coutputnames',{'out'});
    Xobj=Xobj.calibrate;
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 10. Test Constructors
% Try to add Xinput without Samples
itest = 1+itest;
try
    Xin=Xin.sample('Nsamples',15);
    Xout=Xmdl.apply(Xin);
    Xobj = ResponseSurface('Xfullmodel',Xmdl,...
        'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout, ...
        'Coutputnames',{'out'});
     Xobj=Xobj.calibrate;
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11. Test Constructors
% This should work! A full model should be not required if the input/output
% data are available.
itest = 1+itest;
try

    Xin=Xin.sample('Nsamples',15);
    Xout=Xmdl.apply(Xin);
    Xobj = ResponseSurface( ...
        'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout, ...
        'XvalidationInput',Xin, ...
        'XvalidationOutput',Xout, ...
        'Coutputnames',{'out'});
    Xobj=Xobj.calibrate;
    Xobj=Xobj.validate;
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
    Xobj = ResponseSurface( ...
        'XvalidationInput',Xin, ...
        'XvalidationOutput',Xout, ...
        'Coutputnames',{'out'});
    display(Xobj)
    Cmess{itest}='This should fail because the full model is not passed and the training data are not passed';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13. Test Constructors
% Test the custom type
itest = 1+itest;
try
    Xobj = ResponseSurface( ...
        'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout,...
        'Stype','custom', ...
        'Coutputnames',{'out'});
    display(Xobj)
    Cmess{itest}='This should fail because the custom model matrix/exponent is not passed';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 14. Test Constructors
% This should fail because the full model is not passed and the training
% data are not passed
itest = 1+itest;
try
    Xobj = ResponseSurface( ...
        'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout, ...
        'Stype','custom', ...
        'Mexponents',[pi, exp(1), 1],...
        'Coutputnames',{'out'});
    display(Xobj)
    Cmess{itest}='This should fail because the custom exponent is not an integer';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 15. Training
% This should work
itest = 1+itest;
try
    Xobj = ResponseSurface( ...
        'Xfullmodel',Xmdl,...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'Xsimulator',MonteCarlo('Nsamples',200));
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 16. Training
% This should work
itest = 1+itest;
try
    Xobj = ResponseSurface( ...
        'Xfullmodel',Xmdl,...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 17. Training
% This should fail because not enough samples are provided
itest = 1+itest;
try
    Xobj = ResponseSurface( ...
        'Xfullmodel',Xmdl,...
        'Stype','quadratic',...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout,'Xsimulator',MonteCarlo('Nsamples',5));
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 18. Test Constructors
% This should fail because the custom model has a wrong number of inputs
itest =itest + 1;
try
    Xobj = ResponseSurface('Xfullmodel',Xmdl,...
        'Stype','custom',...
        'Mexponents',[0 0 0;1 1 1;1 0 2],...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'Xsimulator',MonteCarlo('Nsamples',200));
    display(Xobj)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 19. Test Validation
itest = 1+itest;
try
    Xobj = ResponseSurface( ...
        'Xfullmodel',Xmdl,...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'Xsimulator',MonteCarlo('Nsamples',200));
    Xobj=validate(Xobj,'Xsimulator',LatinHypercubeSampling('Nsamples',200));
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 20. Test Validation
% This should work
itest = 1+itest;
try
    Xobj = ResponseSurface( ...
        'Xfullmodel',Xmdl,...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout);
    Xobj=validate(Xobj,'XvalidationInput',Xin, ...
        'XvalidationOutput',Xout);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 21. Validation
% This should fail because wrong input arguments
itest = 1+itest;
try
    Xobj = ResponseSurface( ...
        'Xfullmodel',Xmdl,...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout);
    Xobj=validate(Xobj,'XcalibrationInput',Xin, ...
        'XcalibrationInput',Xout);
    display(Xobj)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 22. Validation
% This should fail because the object has not been calibrate
itest = 1+itest;
try
    Xobj = ResponseSurface( ...
        'Xfullmodel',Xmdl,...
        'Coutputnames',{'out'});
    Xobj=validate(Xobj,'XvalidationInput',Xin, ...
        'XvalidationInput',Xout);
    display(Xobj)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 23. Test apply
itest = 1+itest;
try
    Xobj = ResponseSurface( ...
        'Xfullmodel',Xmdl,...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout);
    Xout=Xobj.apply(Xin);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 24. Test apply
% This should work
itest = 1+itest;
try
    Xobj = ResponseSurface( ...
        'Xfullmodel',Xmdl,...
        'Coutputnames',{'out'});
    Xobj=calibrate(Xobj,'XcalibrationInput',Xin, ...
        'XcalibrationOutput',Xout);
    Xout=Xobj.apply(Xin.Xsamples);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 25  Test plot
itest = itest+1;
try
    Xresp1 = ResponseSurface('Xfullmodel',Xmdl, ...
        'Stype','quadratic', ...
        'Coutputnames',{'out'});
    
    Xresp1 = Xresp1.calibrate('Xsimulator',LatinHypercubeSampling('Nsamples',500));
    Xresp1 = Xresp1.validate('Xsimulator',MonteCarlo('Nsamples',20));
    
    Xresp1.plotregression('Stype','calibration','Soutputname','out')
    Xresp1.plotregression('Stype','validation','Soutputname','out')
    
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 26
% Check the effect of passing a wrong argument to Stype
itest = itest+1;
try
    Xresp3 = ResponseSurface('Xfullmodel',Xmdl, ...
        'Stype',';-)', ...
        'Coutputnames',{'f1'});
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 27
% Check the correct settings of the flags Lcalibrated

itest = itest+1;
try
    Xobj = ResponseSurface('Xfullmodel',Xmdl, ...
        'Stype','quadratic', ...
        'Coutputnames',{'out'});
    Xin=Xin.sample('Nsamples',200);
    if ~Xobj.Lcalibrated
        Xobj=calibrate(Xobj,'XcalibrationInput',Xin);
        if Xobj.Lcalibrated
            Vtest(itest)=true;
        else
            Cmess{itest}='The flag Lcalibrated is not set correctly. ';
        end
    else
        Cmess{itest}='The flag Lcalibrated is not set correctly. ';
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 28
% Check the correct settings of the flags Lvalidated

itest = itest+1;
try
    if ~Xobj.Lvalidated % the response surface has not yet been validated
        Vtest(itest)=true;
        Xobj=validate(Xobj,'XvalidationInput',Xin);
        if Xobj.Lvalidated
            Vtest(itest)=true;
        else
            Cmess{itest}='The flag Lvalidated is not set correctly. ';
        end
    else
        Cmess{itest}=[Cmess{itest} 'The flag Lvalidated is not set correctly.'];
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 29 Plot regression
% Test the method plotregression
itest = itest+1;
try
    Xobj.plotregression('Stype','calibration','Soutputname','out') %fails, for some reason calibrationinput is empty, please check
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 30 Plot regression
% wrong output name
itest = itest+1;
try
    Xobj.plotregression('Stype','calibration','Soutputname','f1')
catch ME
    Vtest(itest)=true; 
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

close all;

if nargout>0
    % Export name of the UnitTest
    varargout{1}='ResponseSurface';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of ResponseSurface (' datestr(now) ')'])
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
end
