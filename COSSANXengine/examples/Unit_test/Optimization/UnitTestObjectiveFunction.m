% Unit testing of Optimization Toolbox
	
function varargout=UnitTestObjectiveFunction
	
Ntest=5;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

%% 1 Create an empty object

itest = itest+1;
try
    Xobj = ObjectiveFunction;
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create an object with input parameters

itest = itest+1;
try
    Xobj = ObjectiveFunction('Sscript','Toutput.out1=Tinput.x1-5*Tinput.x2;','Cinputnames',{'x1','x2'},...
                             'Coutputnames',{'out1'},'Liostructure',true);
    Cmess{itest}='Create an object with input parameters';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Check display

itest = itest+1;
try
    display(Xobj);
    Cmess{itest}='Checking display';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 No outputnames defined

itest = itest+1;
try
    Xobj = ObjectiveFunction('Sscript','Toutput.out1=Tinput.x1-5*Tinput.x2;','Cinputnames',{'x1','x2'}); %#ok<*NASGU>
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 No Sscript/Sfile provided

itest = itest+1;
try
    Xobj = ObjectiveFunction('Cinputnames',{'x1','x2'},'Coutputnames',{'out1'},'Liostructure',true);
    Cmess{itest}='Should fail - No Sscript/Sfile provided';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Finish the test

if nargout>0
% Export name of the UnitTest
varargout{1}='ObjectiveFunction';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of ObjectiveFunction (' datestr(now) ')'])
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
