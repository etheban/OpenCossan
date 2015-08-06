%% UNIT TEST for Importance Sampling


function varargout=UnitTestImportanceSampling

% create the model
run('Model_definition')

Ntest=13;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1 Empty object
itest = 1;
try
    Xis=ImportanceSampling;
    display(Xis)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 test the constructor with CXrvset
itest =itest+ 1;
try
    XrvIS = RandomVariable('Sdistribution','uniform',...
        'lowerbound',1.9,'upperbound',2.1); 
    XrvsetIS = RandomVariableSet('Cmembers',{'XrvIS'},'CXrv',{XrvIS});
    Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',100,...
        'Nbatches',1,...
        'CXrvset',{XrvsetIS},...
        'Cmapping',{'XrvIS','RV2'});
    display(Xis)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 test the constructor with DesignPoint
itest = itest+1;
try
    Xdp  = DesignPoint('Sdescription','My design point',...
        'XProbabilisticModel',Xpm,...
        'VDesignPointPhysical',[0 2]);
    Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'XdesignPoint',Xdp);
    display(Xis)
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4  test the wrong constructor usage
% wrong cmapping passed
itest = itest+1;
try
    Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'CXrvset',{XrvsetIS},...
        'Cmapping',{'RV2','XrvIS','RV2'});
    display(Xis)
    Cmess{itest}='Wrong constructor wrong cmapping passed ';
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 inverted IS rv - real rv passed in cmapping 
itest = itest+1;
try
    Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'CXrvset',{XrvsetIS},...
        'Cmapping',{'RV2','XrvIS'});
        display(Xis)
        Cmess{itest}='Wrong constructor inverted IS rv - real rv passed in cmapping ';
catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 wrong object in CXrvset
itest = itest+1;
try
    Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',10,...
        'Nbatches',1,...
        'CXrvset',{Xdp},...
        'Cmapping',{'RV2','XrvIS'});
    display(Xis)
   Cmess{itest}='Wrong constructor  wrong object in CXrvset';

catch ME
    Vtest(itest) = true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 test method apply
itest = itest+1;
try
    Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',100,...
        'Nbatches',1,...
        'CXrvset',{XrvsetIS},...
        'Cmapping',{'XrvIS','RV2'});
    
    Xout = Xis.apply(Xmdl);
    % check that RV2 has been sampled with the IS distribution and not with
    % the original one
    tmp = Xout.getValues('Sname','RV2');
    if any(tmp>2.1) || any(tmp<1.9)
        error('Unit:Test','RV2 has not been sampled using the IS rv!')
    end
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 test method pf
itest = itest+1;
try
   Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',100,...
        'Nbatches',1,...
        'CXrvset',{XrvsetIS},...
        'Cmapping',{'XrvIS','RV2'});
    Xopf = Xis.computeFailureProbability(Xpm);
    assert(isa(Xopf,'FailureProbability'),...
        'Wrong object outputed by pf')
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 test the termination criteria timeout
itest = itest + 1;
try
    Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',60000,...
        'Nbatches',20,...
        'CXrvset',{XrvsetIS},...
        'timeout',2,...
        'Cmapping',{'XrvIS','RV2'});
    tic; Xout = Xis.apply(Xmdl); tmp = toc;
    if isempty(strfind(Xout.SexitFlag,'Maximum execution time reached'))
        error('openCOSSAN:UnitTest',Xout.SexitFlag)
    end
    display(Xout)
    Vtest(itest) = true;
    Cmess{itest}=['Timeout=2 s; Used time=' tmp ];
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 test the termination criteria cov
itest = itest + 1;
try
   Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',10000,...
        'Nbatches',10,...
        'CXrvset',{XrvsetIS},...
        'cov',0.5,...
        'Cmapping',{'XrvIS','RV2'});
    Xpf = Xis.computeFailureProbability(Xpm);  
    % with 1000 samples the CoV is approx. -insert chek when pf is working-,
    % while the CoV should be around 0.5
    assert(logical(strfind(Xpf.SexitFlag,'Target CoV level reached')),...
        'CoV termination criteria not used')
    Cmess{itest}=['Obtained CoV: ' num2str(Xpf.cov)];
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11 test the nr. of batches
itest = itest + 1;
try
    Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',10,...
        'Nbatches',10,...
        'CXrvset',{XrvsetIS},...
        'Cmapping',{'XrvIS','RV2'});
    Xout = Xis.apply(Xmdl); % change this command after apply is fixed
    assert(Xout.Nsamples==1,'Wrong number of samples per batch');
    Vtest(itest) = true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12 test NseedRandomNumberGenerator
itest = itest + 1;
try
    Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',100,...
        'Nbatches',1,...
        'CXrvset',{XrvsetIS},...
        'Cmapping',{'XrvIS','RV2'},...
        'NseedRandomNumberGenerator',0);
    Xout1 = Xis.apply(Xmdl);
    Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',100,...
        'Nbatches',1,...
        'CXrvset',{XrvsetIS},...
        'Cmapping',{'XrvIS','RV2'},...
        'NseedRandomNumberGenerator',0);
    Xout2 = Xis.apply(Xmdl);
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
    Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',100,...
        'Nbatches',1,...
        'CXrvset',{XrvsetIS},...
        'Cmapping',{'XrvIS','RV2'},...
        'XRandomNumberGenerator',Xstream);
    Xout1 = Xis.apply(Xmdl);
    Xstream.reset
    Xis=ImportanceSampling('Sdescription', 'IS unit test #1',...
        'Nsamples',100,...
        'Nbatches',1,...
        'CXrvset',{XrvsetIS},...
        'Cmapping',{'XrvIS','RV2'},...
        'XRandomNumberGenerator',Xstream);
    Xout2 = Xis.apply(Xmdl);
    if any(any(Xout1.Mvalues - Xout2.Mvalues))
        Vtest(itest) = false;
        Cmess{itest} = 'The same samples should have been identical';
    else
        Vtest(itest) = true;
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of ImportanceSampling (' datestr(now) ')'])
disp([' Successful: ' num2str(100*(sum(Vtest)/length(Vtest))) '%'])
disp('--------------------------------------------------------------------')
for i=1:length(Vtest)
    if Vtest(i)
        disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
    else
        disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
    end
end

if nargout>0
% Export name of the UnitTest
varargout{1}='ImportanceSampling';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
