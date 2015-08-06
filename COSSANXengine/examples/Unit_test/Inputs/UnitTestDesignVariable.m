function varargout = UnitTestDesignVariable

%%  Unit test for DESIGN VARIABLES

Ntest          = 9;
Vtest(1:Ntest) = false;
Cmess          = cell(Ntest,1);

%% 1 Create a continous Design Variable without bounds

itest=1;
try
    Xdv1  = DesignVariable('Sdescription','dummy','value',5);
    Vtest(itest)=true;
    Cmess{itest}='Create a continous design variable without bounds';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create a continous Design Variable with bounds

itest=itest+1;
try
    Xdv1  = DesignVariable('value',5,'lowerbound',1,'upperbound',10);
    Vtest(itest)=true;
    Cmess{itest}='Create a continous design variable with bounds';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Create a discrete Design Variable

itest=itest+1;
try
    Xdv2  = DesignVariable('value',5,'Vsupport',1:2:9);
    Vtest(itest)=true;
    Cmess{itest}='Create a discrete design variable';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 Add Design Variables to Xinput

itest=itest+1;
try
    Xin   = Input;
    Xin   = add(Xin,Xdv1);
    Xin   = add(Xin,Xdv2);
    Vtest(itest)=true;
    Cmess{itest}='Add Design Variables to Xinput';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 Remove Design Variable from Xinput

itest=itest+1;
try
    Xin   = remove(Xin,Xdv2); %#ok<*NASGU>
    Vtest(itest)=true;
    Cmess{itest}='Remove Design Variables from Xinput';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 Lowerbound higher than Upperbound - should fail

itest=itest+1;
try
    Xdv1  = DesignVariable('value',5,'lowerbound',15,'upperbound',10);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 Continous DV defined without specifiying its current value - should fail

itest=itest+1;
try
    Xdv1  = DesignVariable('lowerbound',1);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 Discrete DV defined without specifiying its current value - warning provided

itest=itest+1;
try
    Xdv1  = DesignVariable('Vsupport',1:10);
    Vtest(itest)=true;
    Cmess{itest}='Discrete DV defined without specifiying its current value';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 Discrete DV defined with its value outside of Vsupport - warning provided

itest=itest+1;
try
    Xdv1  = DesignVariable('value',13,'Vsupport',1:10);
    Vtest(itest)=true;
    Cmess{itest}='Discrete DV defined with its value outside of Vsupport';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Finish the test

if nargout>0
% Export name of the UnitTest
varargout{1}=['Unit Test of DESIGNVARIABLE (' datestr(now) ')'];
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else


%% Show summary of the test

disp('  ')
disp('  ')
disp('--------------------------------------------------------------------')
disp([' Unit Test of DESIGNVARIABLE (' datestr(now) ')'])
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








