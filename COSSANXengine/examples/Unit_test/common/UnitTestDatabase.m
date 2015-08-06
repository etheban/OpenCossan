function varargout = UnitTestDatabase
%UNITTESTMODEL Summary of this function goes here
%   Detailed explanation goes here

% Test evaluator
% Perallocate memory

Ntest=8;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

%% 1 Initialize database
itest = itest+1;
try
    DatabaseDriver('Susername','','Spassword','',...
                    'SdatabaseURL',fullfile('jdbc:sqlite:',userpath),...
                    'SdatabaseName','cossanx.db','SjdbcDriver','org.sqlite.JDBC')
    Vtest(itest)=true;
    Cmess{itest}='Initialize Database';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
   
%% 2 Test deterministic analyis
itest = itest+1;
try
    Xrv=RandomVariable('Sdistribution','normal','mean',2,'std',0.2);
    Xrvset=RandomVariableSet('CXrandomvariables',{Xrv},'Cmembers',{'Xrv'});
    Xinput=Input('Xrvset',Xrvset);
    
    Xmio1=Mio('Sscript','for n=1:length(Tinput),Toutput(n).out1=1+Tinput(n).Xrv;end', ...
    'Liostructure', true, ...
    'Coutputnames',{'out1';},...
    'Cinputnames',{'Xrv'});
    Xev=Evaluator('Xmio',Xmio1);
    Xmdl = Model('CXmembers',{Xev Xinput},'Sdescription','My Model');
    Xout=Xmdl.deterministicAnalysis;
    
    % Check the database
    assert(Xout.Nsamples==1)
    display(Xout)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 Create incomplete object
itest = itest+1;
try
    Xrv=RandomVariable('Sdistribution','normal','mean',2,'std',0.2);
    Xrvset=RandomVariableSet('CXrandomvariables',{Xrv},'Cmembers',{'Xrv'});
    Xinput=Input('Xrvset',Xrvset);
   
    
    Xmio1=Mio('Sscript','Toutput.out1=1;', ...
    'Liostructure', true, ...
    'Coutputnames',{'out1';},...
    'Cinputnames',{});
    Xev=Evaluator('Xmio',Xmio1);
    Xmdl = Model('CXmembers',{Xev Xinput},'Sdescription','My Model');
    Xinput=samples('Nsamples',10);
    Xout=Xmdl.apply(Xinput);
    assert(Xout.Nsamples==10)
    display(Xout)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 8 Create incomplete object
itest = itest+1;
try
    Xpar1=Parameter('value',2);
    Xinput=Input('Xparameter',Xpar1);
   
    
    Xmio1=Mio('Sscript','Toutput.out1=1;', ...
    'Liostructure', true, ...
    'Coutputnames',{'out1';},...
    'Cinputnames',{'Xrv' 'Xrv2'});
    Xev=Evaluator('Xmio',Xmio1);
    Xmdl = Model('CXmembers',{Xev Xinput},'Sdescription','My Model');
    Cmess{itest}='This should fail, not enough inputs defined';
    display(Xmdl)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

if nargout>0
% Export name of the UnitTest
varargout{1}='Model';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of the Model (' datestr(now) ')'])
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



