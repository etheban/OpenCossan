% Unit testing of Optimization Toolbox
	
function varargout=UnitTestConstraint
	
Ntest=9;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

%% 1 Create an empty object

itest = itest+1;
try
    Xobj = Constraint; %#ok<*NASGU>
    display(Xobj);
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Check display

itest = itest+1;
try
    Xobj = Constraint('Sscript','con1 = Tinput.x1 + Tinput.x2','Liostructure',true,...
                        'Cinputnames',{'x1','x2'},'Soutputname','con1');
    display(Xobj);
    Cmess{itest}='Checking display';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 No outputnames provided 

itest = itest+1;
try
    Xcons1 = Constraint('Sscript','con1 = Tinput.x1 + Tinput.x2','Liostructure',true,...
                        'Cinputnames',{'x1'});
    Cmess{itest}='should fail - no Coutputnames provided';             
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 No inputnames provided 

itest = itest+1;
try
    Xcons1 = Constraint('Sscript','con1 = Tinput.x1 + Tinput.x2','Coutputnames',{'con1'},'Liostructure',true);
    Cmess{itest}='should fail - no Cinputnames provided';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 wrong number of outputnames
itest = itest+1;
try
    Xcons1 = Constraint('Sscript','con1 = Tinput.x1 + Tinput.x2', ...
           'Cinputnames',{'x1','x2'}, ...
           'Coutputnames',{'con1' 'con2'},'Liostructure',true);
    Cmess{itest}='should fail - length(Coutputnames)>1';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 Creating an object with input parameters

itest = itest+1;
try
    Xcons1 = Constraint('Sscript','con1 = Tinput.x1 + Tinput.x2','Coutputnames',...
        {'con1'},'Liostructure',true,...
        'Cinputnames',{'x1','x2'});
    Vtest(itest)=true;
    Cmess{itest}='Creating object with input parameters'; 
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 7 Test method evaluate
itest = itest+1;
try
    Xcons1 = Constraint('Sscript','con1 = Tinput.x1 + Tinput.x2','Soutputname',...
        'con1','Liostructure',true,'Cinputnames',{'x1','x2'});
    [Vcon]=Xcons1.evaluate('Mreferencepoints',[2 1]);
    Cmess{itest}='It should fail, no OptimizationProblem defined'; 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 Test method evaluate
itest = itest+1;

try
    x1       = DesignVariable('value',10);
    x2       = DesignVariable('value',10);
    Xin      = Input('Xdesignvariable',x1,'Xdesignvariable',x2);
    Xobjfunc = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out1=Tinput(i).x1-5*Tinput(i).x2; end','Cinputnames',{'x1','x2'},...
        'Coutputnames',{'out1'},'Liostructure',true);
    
    Xcons1 = Constraint('Sscript','con1 = Tinput.x1 + Tinput.x2','Soutputname',...
        'con1','Liostructure',true,'Cinputnames',{'x1','x2'});
    
    XoptProb = OptimizationProblem('Xinput',Xin,'XobjectiveFunction',Xobjfunc,...
                               'VinitialSolution',[2 2],'Xconstraint',Xcons1);


    [Vcon]=Xcons1.evaluate('Mreferencepoints',[2 1],'XoptimizationProblem',XoptProb);
    Vtest(itest)=true;
    Cmess{itest}='Creating object with input parameters'; 
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 Test method evaluate
itest = itest+1;

try
    x1       = DesignVariable('value',10);
    x2       = DesignVariable('value',10);
    Xin      = Input('Xdesignvariable',x1,'Xdesignvariable',x2);
    Xobjfunc = ObjectiveFunction('Sscript','for i=1:length(Tinput); Toutput(i).out1=Tinput(i).x1-5*Tinput(i).x2; end','Cinputnames',{'x1','x2'},...
        'Coutputnames',{'out1'},'Liostructure',true);
    
    Xcons1 = Constraint('Sscript','for n=1:length(Tinput),Toutput(n).con1 = Tinput(n).x1 + Tinput(n).x2; end','Soutputname',...
        'con1','Liostructure',true,'Cinputnames',{'x1','x2'},'Lgradient',false);
    
    XoptProb = OptimizationProblem('Xinput',Xin,'XobjectiveFunction',Xobjfunc,...
                               'VinitialSolution',[2 2],'Xconstraint',Xcons1);

    [Vin,Veq,MinGrad,MeqGrad]=Xcons1.evaluate('Mreferencepoints',[2 1],...
        'XoptimizationProblem',XoptProb,'Lgradient',true);
    
    Vtest(itest)=true;
    Cmess{itest}='Creating object with input parameters'; 
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% Finish the test

if nargout>0
% Export name of the UnitTest
varargout{1}='Constraints';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of Constraints (' datestr(now) ')'])
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
