%% UNIT TEST for LineSampling
function varargout=UnitTestLineSampling

% TODO: Improve this unit test! (EP)

% create the model
run('Model_definition')

Ntest=22;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 test the constructor with various properties
itest = 1;
try

    Xls=LineSampling('Sdescription', 'LineSampling unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'Xgradient',Xg);
    display(Xls)
    Vtest(itest) = true;
    Cmess{itest}='Create empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
% 2
itest = itest+1;
try
    Xls=LineSampling('Sdescription', 'LineSampling unit test #1',...
        'Nlines',10,...
        'Nbatches',1,...
        'Xgradient',Xg);
        display(Xls)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
% 3
itest = itest+1;
try
        
    Xls=LineSampling('Sdescription', 'LineSampling unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'LexportSamples',true,...
        'Xgradient',Xg);
        display(Xls)
        Cmess{itest}='This should fail. PropertyName LexportSamples not allowed';
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
% 4
itest = itest+1;
try
    Xls=LineSampling('Sdescription', 'LineSampling unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'confLevel',0.9,...
        'Xgradient',Xg);
    display(Xls)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end 
% 5
itest = itest+1;
try
    Xls=LineSampling('Sdescription', 'LineSampling unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'Vset',1:0.5:4,...
        'Xgradient',Xg);
    display(Xls)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
% 6
itest = itest+1;
try
    Xls=LineSampling('Sdescription', 'LineSampling unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'NCfine',500,...
        'Xgradient',Xg);
        display(Xls)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 test the methods sample
itest = itest + 1;
try
   Xls=LineSampling('Sdescription', 'LineSampling unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'Vset',1:0.5:4,...
        'Xgradient',Xg);
    Xs = Xls.sample('Nlines',10);
    display(Xs)
    Cmess{itest}='This should fail. A Input object is required.';
catch ME 
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 test the methods sample
itest = itest + 1;
try
    
   Xls=LineSampling('Sdescription', 'LineSampling unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'Vset',1:0.5:4,...
        'Xgradient',Xg);
    Xs= Xls.sample('Nlines',10,'Xinput',Xin2);
    Cmess{itest}='This should fail. Input and Gradient does not match.';
    display(Xs)
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 test the methods sample
itest = itest + 1;
try
    
   Xls=LineSampling('Sdescription', 'LineSampling unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'Vset',1:0.5:4,...
        'Xgradient',Xg);
    Xs= Xls.sample('Nlines',10,'Xinput',Xin);
    assert(length(Xls.Vset)*10==Xs.Nsamples,'','wrong number of samples')
    display(Xs)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 test the methods sample
itest = itest + 1;
try
    
   Xls=LineSampling('Sdescription', 'LineSampling unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'Vset',1:0.5:4,...
        'Xgradient',Xg);
    Xs= Xls.apply(Xin);
    Cmess{itest}='This should fail. No methos apply requires a model.';
    display(Xs)
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11 test the methods sample
itest = itest + 1;
try
    
   Xls=LineSampling('Sdescription', 'LineSampling unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'Vset',1:0.5:4,...
        'Xgradient',Xg);
    Xout= Xls.apply(Xmdl);
    display(Xout)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12 test the methods sample
itest = itest + 1;
try
    
   Xls=LineSampling('Sdescription', 'LineSampling unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'Vset',1:0.5:4,...
        'Xgradient',Xg);
    Xout= Xls.computeFailureProbability(Xmdl);
    display(Xout)
    Cmess{itest}='This should fail';
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13 test the termination criteria Nsamples
itest = itest + 1;
try
    Xls=LineSampling('Sdescription', 'LineSampling unit test #4',...
        'Nlines',10,...
        'Nbatches',10,...
        'Xgradient',Xg);
    [Xopf Xout] = Xls.computeFailureProbability(Xpm); 
    assert(Xout.Nsamples == 6,...
        'Wrong nr. of simulation!');
    display(Xopf)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 14 test the termination criteria timeout
itest = itest + 1;
try
    Xls=LineSampling('Sdescription', 'LineSampling unit test #4',...
        'timeout',1,...
        'Nsamples',60000,...
        'Nbatches',30,...
        'Xgradient',Xg);
    tic,Xopf  = Xls.computeFailureProbability(Xpm);tmp=toc; 
    assert(logical(strfind(Xopf.SexitFlag,'Maximum execution time')), ...
        'Wrong exit flag.')

    Cmess{itest}=['Enlapsed time ' num2str(tmp)];
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 15 test the termination criteria CoV
itest = itest + 1;
try
    Xg=Sensitivity.gradientFiniteDifferences('Xtarget',Xpm,'Coutputname',{Xpm.XperformanceFunction.Soutputname});
    Xls=LineSampling('Sdescription', 'LineSampling unit test',...
        'cov',0.5,...
        'Nsamples',5000,...
        'Nbatches',10,...
        'Xgradient',Xg);
    Xopf = Xls.computeFailureProbability(Xpm);
    % with 50 samples the CoV is approx. , while the CoV should be
    % around 0.5
   assert(logical(strfind(Xopf.SexitFlag,'Target CoV level reached')), ...
        'Wrong exit flag.')
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 16 test NseedRandomNumberGenerator
itest = itest + 1;
try
     Xls=LineSampling('Sdescription', 'LineSampling unit test',...
        'cov',0.5,...
        'Nsamples',60,...
        'Nbatches',5,...
        'Xgradient',Xg,...
        'NseedRandomNumberGenerator',0);
    [Xopf Xout1] = Xls.computeFailureProbability(Xpm);
    Xls=LineSampling('Sdescription', 'LineSampling unit test',...
        'cov',0.5,...
        'Nsamples',60,...
        'Nbatches',10,...
        'Xgradient',Xg,...
        'NseedRandomNumberGenerator',0);
    [Xopf Xout2] = Xls.computeFailureProbability(Xpm);
    if any(any(Xout1.Mvalues - Xout2.Mvalues))
        Vtest(itest) = false;
        Cmess{itest} = 'The same samples should have been identical';
    else
        Vtest(itest) = true;
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 17 test RandomNumberGenerator #2
itest = itest + 1;
try
    Xstream = RandStream.getDefaultStream;
    Xstream.reset
     Xls=LineSampling('Sdescription', 'LineSampling unit test',...
        'cov',0.5,...
        'Nsamples',50,...
        'Nbatches',1,...
        'Xgradient',Xg,...
        'XRandomNumberGenerator',Xstream);
    [Xopf Xout1] = Xls.computeFailureProbability(Xpm);
    Xstream.reset
     Xls=LineSampling('Sdescription', 'LineSampling unit test',...
        'cov',0.5,...
        'Nsamples',50,...
        'Nbatches',1,...
        'Xgradient',Xg,...
        'XRandomNumberGenerator',Xstream);
    [Xopf Xout2] = Xls.computeFailureProbability(Xpm);
    if any(any(Xout1.Mvalues - Xout2.Mvalues))
        Vtest(itest) = false;
        Cmess{itest} = 'The same samples should have been identical';
    else
        Vtest(itest) = true;
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 18 Check LineSamplingOutput
itest = itest + 1;
try
    Xls=LineSampling('Sdescription', 'LineSampling unit test',...
        'Nsamples',60,...
        'Nbatches',10,...
        'Xgradient',Xg);
    [~, XlsOut] = Xls.computeFailureProbability(Xpm);
    assert(isa(XlsOut,'LineSamplingOutput'),'Wrong output object')
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 19 Check LineSamplingOutput
itest = itest + 1;
try
    Xls=LineSampling('Sdescription', 'LineSampling unit test',...
        'Nsamples',60,...
        'Nbatches',1,...
        'Xgradient',Xg);
    [~, XlsOut] = Xls.computeFailureProbability(Xpm);
    assert(XlsOut.Nsamples==60,'Wrong number of samples')
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 20 LineSampling with array of SensitivityMeasures
itest = itest + 1;
try
    Xls=Sensitivity.localFiniteDifferences('Xtarget',Xpm);
    % This should be of length 2
    display(Xls)
    
    Xls=LineSampling('Sdescription', 'LineSampling unit test',...
        'Nsamples',60,...
        'Nbatches',1,...
        'XlocalSensitivityMeasures',Xls);
    
    % This should fail


catch ME
     Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 21 LineSampling with array of SensitivityMeasures
itest = itest + 1;
try
    Xlsensitivity=LocalSensitivityMeasures('Cnames',{'RV1' 'RV2'},'Vmeasures',[3 2],'VreferencePoint',[0 0]);
    % This should be of length 2
    display(Xlsensitivity)
    
    Xls=LineSampling('Sdescription', 'LineSampling unit test',...
        'Nsamples',60,...
        'Nbatches',1,...
        'XlocalSensitivityMeasures',Xlsensitivity);
    Xpf=Xls.computeFailureProbability(Xpm);
    display(Xpf)
    Vtest(itest) = true;
catch ME
     
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 22 LineSampling limit case: high probability of failure
itest = itest + 1;
try
    capacity = Xin.Xparameters.Xpar_near.value;
    
    Xls=LineSampling('Sdescription', 'LineSampling unit test',...
        'Nlines',20,...
        'Vset',0:1:6,...
        'Nbatches',1,...
        'Ncfine',100);
    
    Xpf_near=Xls.computeFailureProbability(Xpm_near);
    % display(Xpf_near)
    pf_near = Xpf_near.pfhat;
    % exact value of the failure probability
    pf_exact = normcdf(-capacity/sqrt(2),0,1);
    % display(pf_exact)
    
    assert(abs(log10(pf_near/pf_exact))<1,...
        ['Value of failure probability more than one order of magnitude',...
        ' away from the exact value: the probability can be greater than',...
        ' 0.5 if the capacity is given smaller than the mean state']);
    
    Vtest(itest) = true;
catch ME
     
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 23 LineSampling limit case: low probability of failure
itest = itest + 1;
try

    capacity = Xin.Xparameters.Xpar_far.value;
    
    Xls=LineSampling('Sdescription', 'LineSampling unit test',...
        'Nlines',20,...
        'Vset',0:1:6,...
        'Nbatches',1,...
        'Ncfine',100);
    
    Xpf_far=Xls.computeFailureProbability(Xpm_far);
    % display(Xpf_far)
    pf_far = Xpf_far.pfhat;
    % exact value of the failure probability
    pf_exact = normcdf(-capacity/sqrt(2),0,1);
    % display(pf_exact)
    
    assert(log10(pf_far/pf_exact)<1,...
        ['Value of failure probability more than one order of magnitude',...
        ' away from the exact value: if no lines intersect the limit state',...
        ' the probability should be zero']);
    
    Vtest(itest) = true;
    
catch ME
    
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 24 LineSampling with array of SensitivityMeasures
itest = itest + 1;
try
    Xlsensitivity=LocalSensitivityMeasures('Cnames',{'RV5' 'RV2'},'Vmeasures',[3 2],'VreferencePoint',[0 0]);
    % This should be of length 2
    display(Xlsensitivity)
    
    Xls=LineSampling('Sdescription', 'LineSampling unit test',...
        'Nsamples',60,...
        'Nbatches',1,...
        'XlocalSensitivityMeasures',Xlsensitivity);
    Xpf=Xls.computeFailureProbability(Xpm);
    display(Xpf)

catch ME
     Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

if nargout>0
% Export name of the UnitTest
varargout{1}='LineSampling';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
else
end
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of LineSampling (' datestr(now) ')'])
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
