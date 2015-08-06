%% Unit Test for the Timer object
function varargout = UnitTestTimer

Ntest=8;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1. test the constructor
itest = 1;
try
    Xt=Timer;
    display(Xt)
    Vtest(itest)=true;    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2. test the constructor with varargin
itest = 1+itest;
try
    Xt=Timer('Sdescription','First Timer');
    display(Xt)
    Vtest(itest)=true;    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3. start new counter
itest = 1+itest;
try
    Xt.laptime
    display(Xt)
    Vtest(itest)=true;    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4. start new counter with varargin
itest = 1+itest;
try
    Xt.laptime('Sdescription','add more counter')
    display(Xt)
    Vtest(itest)=true;    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5. start new counter with varargin and varargout
itest = 1+itest;
try
    Ncounter=Xt.laptime('Sdescription','add more counter');
    display(Xt)
    display(Ncounter)
    Vtest(itest)=true;    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6. start new counter with varargin and varargout
itest = 1+itest;
try
    Vdelta=Xt.deltatime(Ncounter);
    display(Vdelta)
    Vtest(itest)=true;    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7. stop timer
itest = 1+itest;
try
    Xt.stoptime;
    display(Xt)
    Vtest(itest)=true;    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8. reset timer
itest = 1+itest;
try
    Xt.reset;
    display(Xt)
    Vtest(itest)=true;    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

if nargout>0
% Export name of the UnitTest
varargout{1}='Timer';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of Timer (' datestr(now) ')'])
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
