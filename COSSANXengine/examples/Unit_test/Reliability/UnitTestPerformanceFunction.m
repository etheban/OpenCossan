function varargout = UnitTestPerformanceFunction
%UNITTESTEVALUATOR Summary of this function goes here
%   Detailed explanation goes here

% Test PerformanceFunction
% Perallocate memory

Ntest=11;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

% defined required objects
Xvar1 = RandomVariable('Sdistribution','normal','mean',0,'std',1);
Xrvset = RandomVariableSet('CXrandomvariables',{Xvar1},'Cmembers',{'Xvar1'});
Xvar2 = Parameter('Mvalue',3);
Xinp = Input('Xrvset',Xrvset,'Xparameter',Xvar2);
%generate some samples
Xinp = Xinp.sample('Nsamples',10);
Xmio = Mio('Sscript','for i=1:length(Tinput); Toutput(i).out1=Tinput(1).Xvar2 - Tinput(i).Xvar1;end;',...
    'Cinputnames',{'Xvar1' 'Xvar2'},'Coutputnames',{'out1'});
Xmodel = Model('Xevaluator',Evaluator('Xmio',Xmio),'Xinput',Xinp);
Xoutput = Xmodel.apply(Xinp);

%% 1 Create an empty object
itest = itest+1;
try
    Xpf = PerformanceFunction;
    display(Xpf)
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create a PerformanceFunction using capacity and demand
itest = itest+1;
try
    Xpf1  = PerformanceFunction('Scapacity','Xvar2','Sdemand','Xvar1','SoutputName','vg');
    display(Xpf1)
    Vtest(itest)=true;
    Cmess{itest}='Create an object using capacity - demand';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Create a PerformanceFunction using a Mio
itest = itest+1;
try
    Xpf2  = PerformanceFunction('Xmio',Xmio,'SoutputName','vg');
    display(Xpf2)
%     Vtest(itest)=true;
    Cmess{itest}=['Create an object usign a mio \n'...
        'In this case the property SoutputName is ignored, but no indications, i.e. warning, are given to the user'];
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 Create a PerformanceFunction using a Mio
itest = itest+1;
try
    Xpf2  = PerformanceFunction('Xmio',Xmio);
    display(Xpf2)
     Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 Create a PerformanceFunction with non-mandatory arguments
itest = itest+1;
try
    Xpf3 = PerformanceFunction('CXmio',{Xmio},'stdDeviationIndicatorFunction',0.1);
    display(Xpf3)
    Vtest(itest)=true;
    Cmess{itest}='Create an object usign a mio';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 Wrong objects passed to the constructor
itest = itest+1;
try
    Xobj = PerformanceFunction('Xmio',Xvar1,'stdDeviationIndicatorFunction',0.1);
    display(Xobj)
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 test constructor, non existing property names
itest = itest+1;
try
    Xobj = PerformanceFunction('Spippo','pinco','Sdemand','pallino','SoutputName','vg');
    display(Xobj)
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 test apply method with Xpf1
itest = itest+1;
try
    Xoutput_pf = Xpf1.apply(Xoutput);
    % in this case, the output of the Mio and the output of the perfomance
    % function should be the same
    Mout1 = Xoutput.getValues('Sname','out1');
    Mout2 = Xoutput_pf.getValues('Sname','vg');
    assert(~logical(sum(Mout1-Mout2)),'The values of out1 and vg are not the same!')
    Vtest(itest)=true;
    Cmess{itest}='apply of pf defined using capacity and demand';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 test apply method with Xpf2
itest = itest+1;
try
    % remove the output out1 from the SimulationData object
    Xoutput_apply = Xoutput.split('Cremove',{'out1'});
    Xoutput_pf = Xpf2.apply(Xoutput_apply);
    % in this case, the output of the Mio and the output of the perfomance
    % function should be the same
    Mout1 = Xoutput.getValues('Sname','out1');
    Mout2 = Xoutput_pf.getValues('Sname','out1');
    assert(~logical(sum(Mout1-Mout2)),'The values of out1 and vg are not the same!')
    Vtest(itest)=true;
    Cmess{itest}='apply of pf defined using mio';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 test apply - pf with name already in use
itest = itest+1;
try
    % Xoutput contains the output out1, that is also the output of the Mio
    % used in the PerfomanceFunction
    Xoutput_pf = Xpf2.apply(Xoutput);
    display(Xoutput_pf)
    Cmess{itest}='this test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.message];
end
%% 11 test apply - pf with name of not existing objects
itest = itest+1;
try
    Xobj = PerformanceFunction('Scapacity','pinco','Sdemand','pallino','SoutputName','vg');
    Xoutput_pf = Xobj.apply(Xoutput);
    display(Xoutput_pf)
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.message ];
end


%% finalize unit test
if nargout>0
% Export name of the UnitTest
varargout{1}='PerformanceFunction';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of the PerformanceFunction (' datestr(now) ')'])
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



