%% UNIT TEST for SubSetS

function varargout=UnitTestSubsetSimulation

% create the model
run('Model_definition')

Ntest=13;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 empty object
itest = 1;
try
    Xss=SubSet;
    display(Xss)
    Vtest(itest) = true;
    Cmess{itest}='Create empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 test the constructor, Nsamples termination criteria
itest =itest+ 1;
try
    Xss=SubSet('Sdescription', 'SubSet unit test #1',...
        'Nsamples',10,...
        'Nbatches',1);
    display(Xss)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 test the constructor with non mandatory inputs
itest = itest + 1;
try
    Xss=SubSet('Sdescription', 'SubSet unit test #1',...
        'Nsamples',1000,...
        'Nbatches',1,...
        'LexportSamples',true);
        display(Xss)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 4
itest = itest + 1;
try
    Xss=SubSet('Sdescription', 'SubSet unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'Ninitialsamples',20);
    display(Xss)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 5
itest = itest + 1;
try
    Xss=SubSet('Sdescription', 'SubSet unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'target_pf',0.03);
    display(Xss)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 6
itest = itest + 1;
try
    Xss=SubSet('Sdescription', 'SubSet unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'Nmaxlevels',10);
    display(Xss)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 test the methods apply
itest = itest + 1;
try
    Xss=SubSet('Sdescription', 'SubSet unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'Nmaxlevels',10);
    Xout = Xss.apply(Xmdl);
    display(Xout)
    Cmess{itest}='This should fail. No methos apply for SubSet.';
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 8 test the termination criteria Nsamples
itest = itest + 1;
try
    Xss=SubSet('Sdescription', 'SubSet unit test #4',...
        'Nsamples',100,...
        'Ninitialsamples',100,...
        'Nbatches',2);
    Xopf = Xss.computeFailureProbability(Xpm); 
    assert(Xopf.Nsamples == 100,...
        'Wrong nr. of simulation!');
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 test the termination criteria timeout
itest = itest + 1;
try
    Xss=SubSet('Sdescription', 'SubSet unit test #5',...
        'timeout',2,...
        'Nsamples',60000,...
        'Nbatches',30);
    tic; Xopf = Xss.computeFailureProbability(Xpm); tmp = toc;
    display(Xopf)
    assert(tmp<2.5, ...
        'Execution took longer than the set timeout.')
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 test the termination criteria cov
itest = itest + 1;
try
    Xss=SubSet('Sdescription', 'SubSet unit test #12',...
        'cov',0.5,...
        'Nbatches',1000,...
        'Nbatches',100);
    % this should fail
    Xpf = Xss.computeFailureProbability(Xpm);
    display(Xpf)
    Cmess{itest}='Termination criteria not valid';
catch ME
    Vtest(itest) = true;
    Cmess{itest}=ME.message;
end

%% 11 test the nr. of batches
itest = itest + 1;
try
    Xss=SubSet('Sdescription', 'SubSet unit test #6',...
        'Nsamples',100,...
        'Nbatches',10);
    Xopf = Xss.computeFailureProbability(Xpm); 
    display(Xopf)
    assert(Xopf.Nbatches==10,...
    'wrong number of batches')
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12 test NseedRandomNumberGenerator
itest = itest + 1;
try
    Xss=SubSet('Sdescription', 'SubSet unit test #8',...
        'Nsamples',10,...
        'Nbatches',1,...
        'NseedRandomNumberGenerator',0);
    [~, Xout1] = Xss.computeFailureProbability(Xpm);
    Xss=SubSet('Sdescription', 'SubSet unit test #8',...
        'Nsamples',10,...
        'Nbatches',1,...
        'NseedRandomNumberGenerator',0);
    [~, Xout2] = Xss.computeFailureProbability(Xpm);
    if any(any(Xout1.Mvalues - Xout2.Mvalues))
        Vtest(itest) = false;
        Cmess{itest} = 'The same samples should have been identical';
    else
        Vtest(itest) = true;
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13 test RandomNumberGenerator #2
itest = itest + 1;
try
    Xstream = RandStream.getDefaultStream;
    Xstream.reset
    Xss=SubSet('Sdescription', 'SubSet unit test #10',...
        'Nsamples',10,...
        'Nbatches',1,...
        'XRandomNumberGenerator',Xstream);
    [~, Xout1] = Xss.computeFailureProbability(Xpm);
    Xstream.reset
    Xss=SubSet('Sdescription', 'SubSet unit test #10',...
        'Nsamples',10,...
        'Nbatches',1,...
        'XRandomNumberGenerator',Xstream);
    [~, Xout2] = Xss.computeFailureProbability(Xpm);
    if any(any(Xout1.Mvalues - Xout2.Mvalues))
        Vtest(itest) = false;
        Cmess{itest} = 'The same samples should have been identical';
    else
        Vtest(itest) = true;
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



if nargout>0
% Export name of the UnitTest
varargout{1}='SubSet';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of SubSet (' datestr(now) ')'])
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
