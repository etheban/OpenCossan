%% UNIT TEST for Sobol' Sampling

function varargout=UnitTestSobolSampling

% create the model
run('Model_definition')

Ntest=12;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 test the constructor, Nsamples termination criteria
itest = 1;
try
    Xss=SobolSampling('Sdescription', 'SS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10);
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 test the methods apply
itest = itest + 1;
try
    Xout = Xss.apply(Xmdl);
    display(Xout)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 test the methods sample
itest = itest + 1;
try
    Xsample = Xss.sample('Nsamples',100,'Xinput',Xin);
    display(Xsample)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 test the termination criteria Nsamples
itest = itest + 1;
try
    Xss=SobolSampling('Sdescription', 'SS unit test #4',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10);
    Xout = Xss.apply(Xmdl);
    assert(Xout.Nsamples == 10, ...
        'Wrong nr. of simulation!');
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 test the termination criteria timeout
itest = itest + 1;
try
    Xss=SobolSampling('Sdescription', 'SS unit test #5',...
        'timeout',2,...
        'Nsamples',60000,...
        'Nbatches',1000,'Nskip',10,'Nleap',10);
    display(Xss)
    tic; Xout = Xss.apply(Xmdl); tmp = toc;
    display(Xout)
    assert(tmp<2.5, ... % 25% tolerance added to limit time
        'Execution took longer than the set timeout.')
    Vtest(itest) = true;
    Cmess{itest} = ['Elapsed time counted with tic-toc is ' num2str(tmp) 's.'];
    %     Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 test the nr. of batches
itest = itest + 1;
try
    Xss=SobolSampling('Sdescription', 'SS unit test #6',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',100);
    Xout = Xss.apply(Xmdl); % change this command after apply is fixed
    display(Xout)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 test the nr. of batches #2
itest = itest + 1;
try
    Xss=SobolSampling('Sdescription', 'SS unit test #7',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',100);
    Xout = Xss.apply(Xmdl); % change this command after apply is fixed
    display(Xout)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 test NseedRandomNumberGenerator
itest = itest + 1;
try
    Xss=SobolSampling('Sdescription', 'SS unit test #8',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10,...
        'NseedRandomNumberGenerator',0);
    Xout1 = Xss.apply(Xmdl);
    Xss=SobolSampling('Sdescription', 'SS unit test #8',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10,...
        'NseedRandomNumberGenerator',0);
    Xout2 = Xss.apply(Xmdl);
    if any(any(Xout1.Mvalues - Xout2.Mvalues))
        Vtest(itest) = false;
        Cmess{itest} = 'The same samples should have been identical';
    else
        Vtest(itest) = true;
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 test RandomNumberGenerator #1
itest = itest + 1;
try
    Xss=SobolSampling('Sdescription', 'SS unit test #9',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10,...
        'RandomNumberGenerator',0);
    Xout1 = Xss.apply(Xmdl);
    display(Xout1)
    Vtest(itest) = false;
    Cmess{itest} = 'This test should have trown an error, since a RandStream object is not passed.';
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 test RandomNumberGenerator #2
itest = itest + 1;
try
    Xstream = RandStream.getDefaultStream;
    Xstream.reset
    Xss=SobolSampling('Sdescription', 'SS unit test #10',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10,...
        'XRandomNumberGenerator',Xstream);
    Xout1 = Xss.apply(Xmdl);
    Xstream.reset
    Xss=SobolSampling('Sdescription', 'SS unit test #10',...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10,...
        'XRandomNumberGenerator',Xstream);
    Xout2 = Xss.apply(Xmdl);
    if any(any(Xout1.Mvalues - Xout2.Mvalues))
        Vtest(itest) = false;
        Cmess{itest} = 'The same samples should have been identical';
    else
        Vtest(itest) = true;
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11 test the termination criteria cov and method pf #1
itest = itest + 1;
try
    Xss=SobolSampling('Sdescription', 'SS unit test #11',...
        'cov',0.5,...
        'Nsamples',10,...
        'Nbatches',1,'Nskip',10,'Nleap',10);
    Xpf= Xss.computeFailureProbability(Xmdl);  % a perfomance function instead of a model or probabilistic model is passed
    display(Xpf)
catch ME
    Vtest(itest) = true;
    Cmess{itest}=ME.message ;
end

%% 12 test the termination criteria cov and method pf #2
itest = itest + 1;
try
    Xss=SobolSampling('Sdescription', 'SS unit test #12',...
        'cov',0.5,...
        'Nsamples',1000,...
        'Nbatches',50,'Nskip',10,'Nleap',10);
    Xfp = Xss.computeFailureProbability(Xpm);
    if isempty(strfind(Xfp.SexitFlag,'Target CoV level reached'))
        error('openCOSSAN:UnitTest',Xout.SexitFlag)
    end
    Cmess{itest}=['Obtained CoV: ' num2str(Xfp.cov)];
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



if nargout>0
    % Export name of the UnitTest
    varargout{1}='SobolSampling';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of SobolSampling (' datestr(now) ')'])
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
