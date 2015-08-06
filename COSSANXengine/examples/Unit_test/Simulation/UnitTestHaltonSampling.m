%% UNIT TEST for HSS

function varargout=UnitTestHaltonSampling

% create the model
run('Model_definition')

Ntest=14;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);


%% 1 Empty object
itest = 1;
try
    Xhs=HaltonSampling;
    display(Xhs)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 test the constructor, Nsamples termination criteria
itest = itest + 1;
try
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10);
    Vtest(itest) = true;
    display(Xhs)
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 test the methods apply
itest = itest + 1;
try
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10);
    Xout = Xhs.apply(Xmdl);
    display(Xout)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 test the methods sample
itest = itest + 1;
try
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10);
    Xsample = Xhs.sample('Nsamples',100,'Xinput',Xin);
    assert(isa(Xsample,'Samples'),'Wrong output object')
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 test the termination criteria Nsamples
itest = itest + 1;
try
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10);
    Xout = Xhs.apply(Xmdl);
    assert(Xout.Nsamples== 10, ...
        'Wrong nr. of simulation!');
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 test the termination criteria timeout
itest = itest + 1;
try
    Xhs=HaltonSampling('Sdescription', 'HS unit test #5',...
        'timeout',2,...
        'Nsamples',50000,...
        'Nbatches',50,'Nskip',10,'Nleap',10);
    display(Xhs)
    tic; Xout = Xhs.apply(Xmdl); tmp = toc;
    
    assert(~isempty(strfind(Xout.SexitFlag,'time reached')),'Wrong exit flag')
    Cmess{itest} = ['Elapsed time counted with tic-toc is ' num2str(tmp) 's.'];
    display(Xout)
    Vtest(itest) = true;
catch ME
    Cmess{itest} = [ME.message ' Exit Flag ' Xout.SexitFlag];
end

%% 7 test the nr. of batches
itest = itest + 1;
try
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'Nsamples',10,...
        'Nbatches',10,'Nskip',10,'Nleap',100);
    Xout = Xhs.apply(Xmdl); % change this command after apply is fixed
    assert(Xout.Nsamples==1,'Wrong number of samples per batch');
    
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 test the nr. of batches #2
itest = itest + 1;
try
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'Nsamples',10,...
        'Nbatches',10,'Nskip',10,'Nleap',100);
    Xout = Xhs.apply(Xmdl);
    assert(Xout.Nsamples==1,...
        ['Samples in Xout should be 1. it is ' num2str(Xout.Nsamples)])
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 test NseedRandomNumberGenerator
itest = itest + 1;
try
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10,...
        'NseedRandomNumberGenerator',0);
    Xout1 = Xhs.apply(Xmdl);
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10,...
        'NseedRandomNumberGenerator',0);
    Xout2 = Xhs.apply(Xmdl);
    if any(any(Xout1.Mvalues - Xout2.Mvalues))
        Vtest(itest) = false;
        Cmess{itest} = 'The same samples should have been identical';
    else
        Vtest(itest) = true;
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 test RandomNumberGenerator #1
itest = itest + 1;
try
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10,...
        'RandomNumberGenerator',0);
    Xout1 = Xhs.apply(Xmdl);
    display(Xout1)
    Vtest(itest) = false;
    Cmess{itest} = 'This test should have trown an error, since a RandStream object is not passed.';
catch ME
    Cmess{itest} = ME.message;
    Vtest(itest) = true;
end

%% 11 test RandomNumberGenerator #2
itest = itest + 1;
try
    Xstream = RandStream.getDefaultStream;
    Xstream.reset
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10,...
        'XRandomNumberGenerator',Xstream);
    Xout1 = Xhs.apply(Xmdl);
    Xstream.reset
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10,...
        'XRandomNumberGenerator',Xstream);
    Xout2 = Xhs.apply(Xmdl);
    if any(any(Xout1.Mvalues - Xout2.Mvalues))
        Vtest(itest) = false;
        Cmess{itest} = 'The same samples should have been identical';
    else
        Vtest(itest) = true;
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12 test the termination criteria cov and method pf #1
itest = itest + 1;
try
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'cov',0.5,...
        'Nbatches',1,'Nskip',10,'Nleap',10);
    Xopf = Xhs.computeFailureProbability(Xmdl);  % a model is used instead of a  probabilistic model
    display(Xopf)
    Cmess{itest}='This should fail a model is used instead of a  probabilistic model ';
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13 test the termination criteria cov and method pf #2
itest = itest + 1;
try
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'cov',0.2,...
        'Nsamples',1000,...
        'Nbatches',100);
    Xpf = Xhs.computeFailureProbability(Xpm);
    assert(logical(strfind(Xpf.SexitFlag,'Target CoV level reached')),...
        'CoV termination criteria not used')
    Vtest(itest) = true;
    Cmess{itest}=['Obtained CoV: ' num2str(Xpf.cov)];
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 14 test Constructor
itest = itest + 1;
try
    Xhs=HaltonSampling('Sdescription', 'HS unit test #1',...
        'cov',0.4,'Nsamples',10000,'Nbatches',10,...
        'Lintermediateresults',true,'Nskip',20,'Nleap',100,'SscrambleMethod','clear');
    Xfp = Xhs.computeFailureProbability(Xpm);
    if isempty(strfind(Xfp.SexitFlag,'Target CoV level reached'))
        error('openCOSSAN:UnitTest',Xfp.SexitFlag)
    end
    Vtest(itest) = true;
    Cmess{itest}=['Obtained CoV: ' num2str(Xfp.cov)];
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


if nargout>0
    % Export name of the UnitTest
    varargout{1}='HaltonSampling';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of HaltonSampling (' datestr(now) ')'])
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
