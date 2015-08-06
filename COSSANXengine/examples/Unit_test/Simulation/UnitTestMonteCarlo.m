%% UNIT TEST for MCS

function varargout=UnitTestMonteCarlo
% create the model
run('Model_definition')

Ntest=13;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 Empty Model
itest = 1;
try
    Xmc=MonteCarlo;
    display(Xmc)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 2 test the constructor, Nsamples termination criteria
itest =itest+1;
try
    Xmc=MonteCarlo('Sdescription', 'MC unit test #1',...
        'Nsamples',10,...
        'Nbatches',1);
    display(Xmc)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 test the methods apply
itest = itest + 1;
try
   Xmc=MonteCarlo('Sdescription', 'MC unit test #1',...
        'Nsamples',10,...
        'Nbatches',1);
    Xout = Xmc.apply(Xmdl);
    display(Xout)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 test the methods sample
itest = itest + 1;
try
   Xmc=MonteCarlo('Sdescription', 'MC unit test #1',...
        'Nsamples',10,...
        'Nbatches',1);
    Xsample = Xmc.sample('Nsamples',100,'Xinput',Xin);
    display(Xsample)
    assert(isa(Xsample,'Samples'),'Wrong output object')
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 test the termination criteria Nsamples
itest = itest + 1;
try
    Xmc=MonteCarlo('Sdescription', 'MC unit test #4',...
        'Nsamples',10,...
        'Nbatches',1);
    Xout = Xmc.apply(Xmdl); 
    assert(Xout.Nsamples== 10, ...
        'Wrong nr. of simulation!');
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 test the termination criteria timeout
itest = itest + 1;
try
    Xmc=MonteCarlo('Sdescription', 'MC unit test #5',...
        'timeout',2,...
        'Nsamples',60000,...
        'Nbatches',30);
    tic; Xout = Xmc.apply(Xmdl); tmp = toc;
    if isempty(strfind(Xout.SexitFlag,'Maximum execution time reached'))
        error('openCOSSAN:UnitTest',Xout.SexitFlag)
    end
    Vtest(itest) = true;
    display(Xout)
    Cmess{itest} = ['Elapsed time counted with tic-toc is ' num2str(tmp) 's.'];
%     Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 test the nr. of batches
itest = itest + 1;
try
    Xmc=MonteCarlo('Sdescription', 'MC unit test #6',...
        'Nsamples',10,...
        'Nbatches',10);
    Xout = Xmc.apply(Xmdl); % change this command after apply is fixed
    assert(Xout.Nsamples==1,'Wrong number of samples per batch');
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 test the nr. of batches #2
itest = itest + 1;
try
    Xmc=MonteCarlo('Sdescription', 'MC unit test #7',...
        'Nsamples',10,...
        'Nbatches',10);
    Xout = Xmc.apply(Xmdl); % change this command after apply is fixed
    assert(Xout.Nsamples==1,...
        'samples in Xout should be 1')
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 test NseedRandomNumberGenerator
itest = itest + 1;
try
    Xmc=MonteCarlo('Sdescription', 'MC unit test #8',...
        'Nsamples',10,...
        'Nbatches',1,...
        'NseedRandomNumberGenerator',0);
    Xout1 = Xmc.apply(Xmdl);
    Xmc=MonteCarlo('Sdescription', 'MC unit test #8',...
        'Nsamples',10,...
        'Nbatches',1,...
        'NseedRandomNumberGenerator',0);
    Xout2 = Xmc.apply(Xmdl);
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
    Xmc=MonteCarlo('Sdescription', 'MC unit test #9',...
        'Nsamples',10,...
        'Nbatches',1,...
        'RandomNumberGenerator',0);
    Xout1 = Xmc.apply(Xmdl);
    display(Xout1)
    Vtest(itest) = false;
    Cmess{itest} = 'This test should have trown an error, since a RandStream object is not passed.';
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11 test RandomNumberGenerator #2
itest = itest + 1;
try
    Xstream = RandStream.getDefaultStream;
    Xstream.reset
    Xmc=MonteCarlo('Sdescription', 'MC unit test #10',...
        'Nsamples',10,...
        'Nbatches',1,...
        'XRandomNumberGenerator',Xstream);
    Xout1 = Xmc.apply(Xmdl);
    Xstream.reset
    Xmc=MonteCarlo('Sdescription', 'MC unit test #10',...
        'Nsamples',10,...
        'Nbatches',1,...
        'XRandomNumberGenerator',Xstream);
    Xout2 = Xmc.apply(Xmdl);
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
    Xmc=MonteCarlo('Sdescription', 'MC unit test #11',...
        'cov',0.5,...
        'Nsamples',10,...
        'Nbatches',1);
    Xopf = Xmc.computeFailureProbability(Xpf);  % a perfomance function instead of a model or probabilistic model is passed    
    display(Xopf)
     Cmess{itest}='This should fail';
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13 test the termination criteria cov and method pf #2
itest = itest + 1;
try
    Xmc=MonteCarlo('Sdescription', 'MC unit test #12',...
        'Nsamples',1000,...
        'Nbatches',50,...
        'cov',0.5);
    Xopf = Xmc.computeFailureProbability(Xpm);
    % with 1000 samples the CoV is approx. 0.233, while the CoV should be
    % around 0.5
    assert(logical(strfind(Xopf.SexitFlag,'Target CoV level reached')),...
        'CoV termination criteria not used')
    Cmess{itest}=['Obtained CoV: ' num2str(Xopf.cov)];
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 14 test the termination criteria cov and method pf #2
itest = itest + 1;
try
    Xmc=MonteCarlo('Sdescription', 'MC unit test #12',...
        'Nsamples',10000,...
        'Nbatches',1);
    Xopf1 = Xmc.computeFailureProbability(Xpm);
    Xmc=MonteCarlo('Sdescription', 'MC unit test #12',...
        'Nsamples',10000,...
        'Nbatches',100);
    Xopf2 = Xmc.computeFailureProbability(Xpm);
    
    assert((Xopf1.pfhat-Xopf2.pfhat)>0.01*Xopf2.pfhat, ...
        ['The pf difference between no batches (' num2str(Xopf1.pfhat) ...
             ') and not batches (' num2str(Xopf2.pfhat) ') is too high']  )
    
    assert((Xopf1.cov-Xopf2.cov)>0.01*Xopf2.cov, ...
        ['The cov difference between no batches (' num2str(Xopf1.cov) ...
             ') and batches (' num2str(Xopf2.cov) ') is too high']  )
    Cmess{itest}='Batches test passed';
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


if nargout>0
% Export name of the UnitTest
varargout{1}='MonteCarlo';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of MonteCarlo (' datestr(now) ')'])
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
