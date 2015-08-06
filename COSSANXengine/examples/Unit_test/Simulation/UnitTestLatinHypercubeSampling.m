%% UNIT TEST for LHSS

function varargout=UnitTestLatinHypercubeSampling

% create the model
run('Model_definition')

Ntest=14;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);


%% 1 Empty object
itest = 1;
try
    Xlhs=LatinHypercubeSampling;
    display(Xlhs)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 test the constructor, Nsamples termination criteria
itest =itest+ 1;
try
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1);
    Vtest(itest) = true;
    display(Xlhs)
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 test the methods apply
itest = itest + 1;
try
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1);
    Xout = Xlhs.apply(Xmdl);
    display(Xout)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 test the methods sample
itest = itest + 1;
try
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1);
    Xsample = Xlhs.sample('Nsamples',100,'Xinput',Xin);
    assert(isa(Xsample,'Samples'),'Wrong output object')
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 test the termination criteria Nsamples
itest = itest + 1;
try
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #4',...
        'Nsamples',10,...
        'Nbatches',2);
    Xout = Xlhs.apply(Xmdl);
    assert(Xout.Nsamples== 5, ...
        'Wrong nr. of simulation!');
    Vtest(itest) = true;
    Cmess{itest}='Check Nsamples';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 test the termination criteria Nsamples and Criterion correlations
itest = itest + 1;
try
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #4',...
        'Nsamples',10,...
        'Nbatches',1,'Scriterion','correlation','cov',2,'timeout',0);
    display(Xlhs);
    Xfp = Xlhs.computeFailureProbability(Xpm);
    assert(Xfp.Nsamples== 10, ...
        'Wrong nr. of simulation!');
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 test the termination criteria timeout
itest = itest + 1;
try
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #5',...
        'timeout',2,...
        'Nsamples',60000,...
        'Nbatches',30);
    tic; Xout = Xlhs.apply(Xmdl); tmp = toc;
    assert(tmp<3,... %  tolerance added to limit time
        ['Execution took longer than the set timeout (' num2str(tmp) ')'])
    Vtest(itest) = true;
    display(Xout)
    Cmess{itest} = ['Elapsed time counted with tic-toc is ' num2str(tmp) 's.'];
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 test the nr. of batches
itest = itest + 1;
try
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #6',...
        'Nsamples',10,...
        'Nbatches',10);
    Xout = Xlhs.apply(Xmdl); % change this command after apply is fixed
    assert(Xout.Nsamples==1,'Wrong number of samples per batch');
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 test the nr. of batches #2
itest = itest + 1;
try
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #7',...
        'Nsamples',10,...
        'Nbatches',10);
    Xout = Xlhs.apply(Xmdl); 
    assert(Xout.Nsamples==1,...
        ['Samples in Xout should be 1. it is ' num2str(Xout.Nsamples)])
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 test NseedRandomNumberGenerator
itest = itest + 1;
try
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #8-1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'NseedRandomNumberGenerator',0);
    Xout1 = Xlhs.apply(Xmdl);
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #8-2',...
        'Nsamples',10,...
        'Nbatches',1,...
        'NseedRandomNumberGenerator',0);
    Xout2 = Xlhs.apply(Xmdl);
    if any(any(Xout1.Mvalues - Xout2.Mvalues))
        Vtest(itest) = false;
        Cmess{itest} = 'The same samples should have been identical';
    else
        Vtest(itest) = true;
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11 test RandomNumberGenerator #1
itest = itest + 1;
try
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #9',...
        'Nsamples',10,...
        'Nbatches',1,...
        'RandomNumberGenerator',0);
    Xout1 = Xlhs.apply(Xmdl);
    display(Xout1)
    Vtest(itest) = false;
    Cmess{itest} = 'This test should have trown an error, since a RandStream object is not passed.';
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12 test RandomNumberGenerator #2
itest = itest + 1;
try
    Xstream = RandStream.getDefaultStream;
    Xstream.reset
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #10-1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'XRandomNumberGenerator',Xstream);
    Xout1 = Xlhs.apply(Xmdl);
    Xstream.reset
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #10-2',...
        'Nsamples',10,...
        'Nbatches',1,...
        'XRandomNumberGenerator',Xstream);
    Xout2 = Xlhs.apply(Xmdl);
    if any(any(Xout1.Mvalues - Xout2.Mvalues))
        Vtest(itest) = false;
        Cmess{itest} = 'The same samples should have been identical';
    else
        Vtest(itest) = true;
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13 test the termination criteria cov and method pf #1
itest = itest + 1;
try
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #11',...
        'cov',0.5,...
        'Nsamples',12,...
        'Nbatches',1);
    Xopf = Xlhs.computeFailureProbability(Xmdl);  % a model is used instead of a  probabilistic model
    display(Xopf)
    Cmess{itest}='This should fail a model is used instead of a  probabilistic model ';
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 14 test the termination criteria cov and method pf #2
itest = itest + 1;
try
    Xlhs=LatinHypercubeSampling('Sdescription', 'LHS unit test #12',...
        'cov',0.5,...
        'Nsamples',1000,...
        'Nbatches',50);
    Xopf = Xlhs.computeFailureProbability(Xpm);
    assert(logical(strfind(Xopf.SexitFlag,'Target CoV level reached')),...
        'CoV termination criteria not used')
    Cmess{itest}=['Obtained CoV: ' num2str(Xopf.cov)];
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



if nargout>0
    % Export name of the UnitTest
    varargout{1}='LatinHypercubeSampling';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of LatinHypercubeSampling (' datestr(now) ')'])
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
