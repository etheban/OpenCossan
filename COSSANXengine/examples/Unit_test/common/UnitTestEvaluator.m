function varargout = UnitTestEvaluator
%UNITTESTEVALUATOR Summary of this function goes here
%   Detailed explanation goes here

% Test evaluator
% Perallocate memory

Ntest=15;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

%% 1 Create an empty object
itest = itest+1;
try
    Xobj = Evaluator;
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2
itest=itest+1;
try
    Xobj  = Evaluator('Sdescription','My Evaluator object');
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create Object with only description';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
    Xobj  = Evaluator('Xmio',true);
    display(Xobj)
    Cmess{itest}='This should fail, wrong property name lremoteinjectextract';
catch ME
    % This should fail, wrong property name
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
itest=itest+1;
try
    Xmio=Mio('Coutputnames',{'name1output'},'Cinputnames',{'name1input'},'Sscript','%do nothing');
    Xcon=Connector;
    Xobj  = Evaluator('Xmio',Xmio,'Xconnector',Xcon);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
itest=itest+1;
try
    Xmio=Mio;
    Xcon=Connector;
    Xobj  = Evaluator('Xconnector',Xcon,'Xmio',Xmio,'CSnames',{'Connector Name', 'Mio name'});
    % Check that the first Connector is Xcon
    assert(isa(Xobj.CXsolvers{1},'Connector'));
    display(Xobj)
     Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
itest=itest+1;
try
    Xmio=Mio;
    Xcon=Connector;
    Xobj  = Evaluator('CXmembers',{Xcon Xmio},'CSnames',{'Connector Name', 'Mio name'});
    % Check that the first Connector is Xcon
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7
itest=itest+1;
try
    Xmio=Mio;
    Xcon=Connector;
    Xobj  = Evaluator('CXmembers',{Xcon Xmio},'CSnames',{'Connector Name'});
    % Check that the first Connector is Xcon
    display(Xobj)
    Cmess{itest}='This should fail, wrong length of CSnames';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8
% Test jobManager
itest=itest+1;
try
    Xmio=Mio;
    Xcon=Connector;
    Xjmi = JobManagerInterface('Stype','GridEngine_matlab');
    Xobj  = Evaluator('CXmembers',{Xcon Xmio Xjmi},'CSnames',{'Connector Name' 'mio Name'});
    % Check that the first Connector is Xcon
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9
itest=itest+1;
try
    Xmio=Mio;
    Xcon=Connector;
    Xjmi = JobManagerInterface('Stype','GridEngine_matlab');
    Xobj  = Evaluator('CXmembers',{Xcon Xmio Xjmi Xcon Xmio},'CSnames',{'Connector Name' 'mio Name' 'Connector Name2' 'mio Name2'});
    % Check that the first Connector is Xcon
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10
itest=itest+1;
try
    Xmio1=Mio('CoutputNames',{'out1' 'out2'},'CinputNames',{'in1'},'Sscript','%do nothing');
    Xmio2=Mio('CoutputNames',{'out3' 'out4'},'CinputNames',{'out1'},'Sscript','%do nothing');
    Xobj  = Evaluator('CXmembers',{Xmio1 Xmio2},'CSnames',{'mio Name' 'mio Name2'});
    % Check that the first Connector is Xcon
    Xobj.Cinputnames
    Xobj.Coutputnames
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 11
% Deterministic Analysis
itest=itest+1;
try
    Xmio=Mio('Sscript','Toutput.out1=Tinput.Xpar','CoutputNames',{'out1'},'CinputNames',{'Xpar'});
    Xpar=Parameter('value',10.2);
    Xinput=Input('Xparameter',Xpar);
    Xobj  = Evaluator('CXmembers',{Xmio},'CSnames',{'mio Name'});
    % Check that the first Connector is Xcon
    Xout=Xobj.deterministicAnalysis(Xinput);
    assert(isa(Xout,'SimulationData'))
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12
% Deterministic Analysis Check execution oder
itest=itest+1;
try
    Xmio1=Mio('Sscript','Toutput.out1=1;', ...
    'Liostructure', true, ...
    'Coutputnames',{'out1';},...
    'Cinputnames',{'dummy'});
    
    Xmio2=Mio('Sscript','Toutput.out2=5;', ...
    'Liostructure', true, ...
    'Coutputnames',{'out2'},...
    'Cinputnames',{'out1'});
    Xrv=RandomVariable('Sdistribution','normal','mean',2,'std',0.2);
    Xrvset=RandomVariableSet('CXrandomvariables',{Xrv},'Cmembers',{'Xrv'});
    Xinput=Input('Xrvset',Xrvset);
    Xobj  = Evaluator('CXmembers',{Xmio1 Xmio2},'CSnames',{'Connector Name' 'mio Name'});
    % Check that the first Connector is Xcon
    Xout=Xobj.deterministicAnalysis(Xinput);
    
    assert(Xout.Tvalues.out2==5)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 13
% Apply Analysis Check execution oder
itest=itest+1;
try
    Xmio1=Mio('Sscript','Toutput.out1=1;', ...
    'Liostructure', true, ...
    'Coutputnames',{'out1';},...
    'Cinputnames',{'dummy'});
    
    Xmio2=Mio('Sscript','Toutput.out2=Tinput.out1+5;', ...
    'Liostructure', true, ...
    'Coutputnames',{'out2';},...
    'Cinputnames',{'out1'});

    Xrv=RandomVariable('Sdistribution','normal','mean',2,'std',0.2);
    Xrvset=RandomVariableSet('CXrandomvariables',{Xrv},'Cmembers',{'Xrv'});
    Xinput=Input('Xrvset',Xrvset);
    Xinput=Xinput.sample;
    
    Xobj  = Evaluator('CXmembers',{Xmio1 Xmio2},'CSnames',{'Connector Name' 'mio Name'});
    % Check that the first Connector is Xcon
    Xout=Xobj.apply(Xinput);
    
    assert(Xout.Tvalues.out2==6)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 14 Check InputNames
itest=itest+1;
try
    Xmio1=Mio('Sscript','Toutput.out1=1;', ...
    'Liostructure', true, ...
    'Coutputnames',{'out1' 'out2'},...
    'Cinputnames',{'X1' 'X2' 'X3'});
    
    Xmio2=Mio('Sscript','Toutput.out2=Tinput.out1+5;', ...
    'Liostructure', true, ...
    'Coutputnames',{'out3'},...
    'Cinputnames',{'X1' 'X2' 'X5' 'out1'});

    Xmio3=Mio('Sscript','Toutput.out2=Tinput.out1+5;', ...
    'Liostructure', true, ...
    'Coutputnames',{'out4'},...
    'Cinputnames',{'X6' 'out3'});

    
    Xobj  = Evaluator('CXmembers',{Xmio1 Xmio2 Xmio3},'CSnames',{'Name1' 'Name2' 'Name3'});
    
    % Input Names should be:
    Cinput={'X1' 'X2' 'X3' 'X5' 'X6'};
    assert(length(Cinput)==length(Xobj.Cinputnames),'Wrong input names')
    assert(all(strcmp(Cinput,Xobj.Cinputnames)),'Wrong input names')
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 15 Check OutpoutNames
itest=itest+1;
try
    Xmio1=Mio('Sscript','Toutput.out1=1;', ...
    'Liostructure', true, ...
    'Coutputnames',{'out1' 'out2'},...
    'Cinputnames',{'X1' 'X2' 'X3'});
    
    Xmio2=Mio('Sscript','Toutput.out2=Tinput.out1+5;', ...
    'Liostructure', true, ...
    'Coutputnames',{'out3'},...
    'Cinputnames',{'X1' 'X2' 'X5' 'out1'});

    Xmio3=Mio('Sscript','Toutput.out2=Tinput.out1+5;', ...
    'Liostructure', true, ...
    'Coutputnames',{'out4'},...
    'Cinputnames',{'X6' 'out3'});

    
    Xobj  = Evaluator('CXmembers',{Xmio1 Xmio2 Xmio3},'CSnames',{'Name1' 'Name2' 'Name3'});
    
    % Input Names should be:
    Coutput={'out1' 'out2' 'out3' 'out4' };
    assert(length(Coutput)==length(Xobj.Coutputnames),'Wrong output names')
    assert(all(strcmp(Coutput,Xobj.Coutputnames)),'Wrong output names')
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

if nargout>0
% Export name of the UnitTest
varargout{1}='Evaluator';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of the Evaluator (' datestr(now) ')'])
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



