function varargout = UnitTestFunction

Ntest          = 12;
Vtest(1:Ntest) = false;
Cmess          = cell(Ntest,1);

%% Testing Random Variable Set

Xrv1=RandomVariable('Sdistribution','Normal','std',1,'mean',0);
Xrv2=RandomVariable('Sdistribution','Normal','std',2,'mean',10);
XrvSet=RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2});
% 1
itest=1;
try
    Xfun = Function;
    display(Xfun)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 2
itest=itest+1;
try
    Xfun = Function('Sexpression','<&Xrv1&>+<&Xrv2&>');
    display(Xfun)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 3
itest=itest+1;
try
    Xfun = Function('Cexpression','<&Xrv1&>+<&Xrv2&>+<&Xrv3&>');
    Cmess{itest}='This should fail!!! Wrong input';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 4
itest=itest+1;
try
    Xfun = Function('Snonsense','no available field');
    display(Xfun)
    Cmess{itest}='This should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 5 fixed
itest=itest+1;
try
    Xfun = Function('Sexpression','<&Xrv1&>+<&Xrv2&>');
     Xfun.evaluate 
    Vtest(itest)=false;
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 6 NON SENSE TEST
itest=itest+1;
try
    Xfun = Function('Sexpression','<&Xrv3&>+<&Xrv4&>');
    Xfun.evaluate 
    Vtest(itest)=false;
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


% 7
itest=itest+1;
try
    Xfun = Function('Sexpression','<&Xrv1&>+<&Xrv2&>');
    Vtest(itest)=false;
    Cmess{itest} = 'Bad number of output arguments';
    [COut1 COut2]=Xfun.evaluate;
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 8
itest=itest+1;
try
    Xfun = Function('Sexpression','<&Xrv3&>+<&Xrv4&>');
    [Cmembers Ctypes] = getMembers(Xfun);
    Vtest(itest)=true;
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


% 9
itest=itest+1;
try
    Xfun = Function('Sexpression','<&Xrv1&>+<&Xrv2&>');
    [Cmembers Ctypes] = getMembers(Xfun);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.message];
end

% 10
itest=itest+1;
try
    Xfun = Function('Sexpression','<&Xrv1&>+<&Xrv2&>');
    Xin   = Input;
    Xin   = add(Xin,XrvSet);
    Xin   = add(Xin,Xfun);
    Xin   = sample(Xin,'Nsamples',3);
    Cfun1 = evaluate(Xfun,Xin);
    display(Cfun1)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 11
%the purpose of this test is unclear
%what is 'Mucca' ?
itest=itest+1; 
try
    Xfun = Function('Sexpression','<&Xrv1&>+<&Xrv2&>');
    Xin   = Input;
    Xin   = add(Xin,XrvSet);
    Xin   = add(Xin,Xfun);
    Xin   = sample(Xin,'Nsamples',3);
    Cfun1 = evaluate(Xfun,'Mucca');
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 12
itest=itest+1;
try
    Xfun = Function('Sexpression','<&Xrv1&>+<&Xrv2&>');
    Xin   = Input;
    Xin   = add(Xin,Xfun);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ ME.message];
end


if nargout>0
% Export name of the UnitTest
varargout{1}='Unit Test of FUNCTIONS';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else


%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of the FUNCTIONS (' datestr(now) ')'])
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










