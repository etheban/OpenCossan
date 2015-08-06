function varargout = UnitTestParameter

%%  Unit test for PARAMETERS

Ntest          = 6;
Vtest(1:Ntest) = false;
Cmess          = cell(Ntest,1);

%% Testing Random Variable Set

% 1
itest=1;
try
    Xpar1  = Parameter('value',5);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 2
itest=itest+1;
try
    Xin   = Input;
    Xin   = add(Xin,Xpar1);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 3
itest=itest+1;
try
    Xpar2  = Parameter('value',3,'Sdescription','dummy');
    Xin   = add(Xin,Xpar2);
    Xin   = remove(Xin,Xpar2);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 4
itest=itest+1;
try
    x1    = RandomVariable('Sdistribution','normal','mean',2,'std',0.4);
    Xfun1 = Function('Sdescription','function #1', 'Sexpression','<&Xpar&>/<&x1&>');
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


% 5
itest=itest+1;
try
    Xpar1  = Parameter('value',5);
    Xin   = Input;
    Xin = Xin.add(Xfun1);
    Xin = Xin.sample('Nsamples',25);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 6
itest=itest+1;
try
    Xpar  = Parameter('value',5);
    x1    = RandomVariable('Sdistribution','normal','mean',2,'std',0.4);
    xset1    = RandomVariableSet('Cmembers',{'*all*'});
    Xin   = Input;
    Xin   = add(Xin,Xpar);
    Xin   = add(Xin,xset1);
    Xin = Xin.add(Xfun1);
    Xin = Xin.sample('Nsamples',25);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



if nargout>0
% Export name of the UnitTest
varargout{1}=['Unit Test of PARAMETERS (' datestr(now) ')'];
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else


%% Show summary of the test

disp('  ')
disp('  ')
disp('--------------------------------------------------------------------')
disp([' Unit Test of PARAMETERS (' datestr(now) ')'])
disp('--------------------------------------------------------------------')

for i=1:length(Vtest)
    if Vtest(i)
        disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);  
    else
        disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
    end
end
end








