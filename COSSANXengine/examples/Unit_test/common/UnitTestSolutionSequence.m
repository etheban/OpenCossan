function varargout = UnitTestSolutionSequence
%UNITTESTEVALUATOR Summary of this function goes here
%  SolutionSequence is a subclass of Mio. Only the fields and methods specific
%  for this class are tested.

% Test solutionsequence
% Perallocate memory

Ntest=11;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

%% 1 Create an empty object
itest = itest+1;
try
    Xobj = SolutionSequence;
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Create an object using a file
itest = itest+1;
try
    Xdv1 = DesignVariable('Vvalues',1:10,'value',5);
    Xdv2 = DesignVariable('Vvalues',1:10,'value',5);
    Xpar = Parameter('value',1);
    Xin = Input('CXmembers',{Xdv1 Xdv2},'CSmembers',{'Xdv1' 'Xdv2'});
    Xobj = SolutionSequence('Sfile','FileForSolutionSequence', ...
                            'Spath',[OpenCossan.getCossanRoot '/examples/Unit_test/common/'],...
                            'Cinputnames',{'Xdv1','Xdv2'}, ...                            
                            'Coutputnames',{'out1'}, ...
                            'CprovidedObjectTypes',{'RandomVariable'},...
                            'Cobject2output',{'.mean'},...
                            'CobjectsNames',{'Xpar'},...
                            'CobjectsTypes',{'Parameter'},...
                            'CXobjects',{Xpar});                  
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create a SolutionSequence object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Create an object using a script
itest = itest+1;
try
    Xdv1 = DesignVariable('Vvalues',1:10,'value',5);
    Xdv2 = DesignVariable('Vvalues',1:10,'value',5);
    Xpar = Parameter('value',1);
    Xin = Input('CXmembers',{Xdv1 Xdv2},'CSmembers',{'Xdv1' 'Xdv2'});
    Xobj = SolutionSequence('Sscript','Xrv = RandomVariable(''Sdistribution'',''normal'',''mean'',varargin{1}+varargin{2}+Xpar.value,''std'',1); COSSANoutput{1} = Xrv;',...
                            'Cinputnames',{'Xdv1','Xdv2'}, ...                            
                            'Coutputnames',{'out1'}, ...
                            'CprovidedObjectTypes',{'RandomVariable'},...
                            'Cobject2output',{'.mean'},...
                            'CobjectsNames',{'Xpar'},...
                            'CobjectsTypes',{'Parameter'},...
                            'CXobjects',{Xpar});
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create a SolutionSequence object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 Create incomplete object (no Coutputnames)

itest = itest+1;
try
    Xdv1 = DesignVariable('Vvalues',1:10,'value',5);
    Xdv2 = DesignVariable('Vvalues',1:10,'value',5);
    Xin = Input('CXmembers',{Xdv1 Xdv2},'CSmembers',{'Xdv1' 'Xdv2'});
    Xobj = SolutionSequence('Sscript','Xrv = RandomVariable(''Sdistribution'',''normal'',''mean'',varargin{1}+varargin{2},''std'',1); COSSANoutput{1} = Xrv;',...
                            'Cinputnames',{'Xdv1','Xdv2'});
    display(Xobj)
    Cmess{itest}='This test shall fail since Coutputnames is not defined';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 5 Apply object using a file
itest = itest+1;
try
    Xdv1 = DesignVariable('Vvalues',1:10,'value',5);
    Xdv2 = DesignVariable('Vvalues',1:10,'value',5);
    Xpar = Parameter('value',1);
    Xin = Input('CXmembers',{Xdv1 Xdv2},'CSmembers',{'Xdv1' 'Xdv2'});
    Xobj = SolutionSequence('Sfile','FileForSolutionSequence', ...
                            'Spath',[OpenCossan.getCossanRoot '/examples/Unit_test/common/'],...
                            'Cinputnames',{'Xdv1','Xdv2'}, ...                            
                            'Coutputnames',{'out1'}, ...
                            'CprovidedObjectTypes',{'RandomVariable'},...
                            'Cobject2output',{'.mean'},...
                            'CobjectsNames',{'Xpar'},...
                            'CobjectsTypes',{'Parameter'},...
                            'CXobjects',{Xpar});
    Xout=Xobj.apply(Xin.getStructure);
    if Xout.Mvalues == 11 % mean value of computed rv shall be 11
        Vtest(itest)=true;
    end
    Cmess{itest} = 'Apply object using a file';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 Apply object using a script
itest = itest+1;
try
    Xdv1 = DesignVariable('Vvalues',1:10,'value',5);
    Xdv2 = DesignVariable('Vvalues',1:10,'value',5);
    Xpar = Parameter('value',1);
    Xin = Input('CXmembers',{Xdv1 Xdv2},'CSmembers',{'Xdv1' 'Xdv2'});
    Xobj = SolutionSequence('Sscript','Xrv = RandomVariable(''Sdistribution'',''normal'',''mean'',varargin{1}+varargin{2}+Xpar.value,''std'',1); COSSANoutput{1} = Xrv;',...
                            'Cinputnames',{'Xdv1','Xdv2'}, ...                            
                            'Coutputnames',{'out1'}, ...
                            'CprovidedObjectTypes',{'RandomVariable'},...
                            'Cobject2output',{'.mean'},...
                            'CobjectsNames',{'Xpar'},...
                            'CobjectsTypes',{'Parameter'},...
                            'CXobjects',{Xpar});
    Xout=Xobj.apply(Xin.getStructure);
    if Xout.Mvalues == 11 % mean value of computed rv shall be 11
        Vtest(itest)=true;
    end
    Cmess{itest} = 'Apply object using a script';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 7 Pass wrong CprovidedObjectTypes
itest = itest+1;
try
    Xdv1 = DesignVariable('Vvalues',1:10,'value',5);
    Xdv2 = DesignVariable('Vvalues',1:10,'value',5);
    Xpar = Parameter('value',1);
    Xin = Input('CXmembers',{Xdv1 Xdv2},'CSmembers',{'Xdv1' 'Xdv2'});
    Xobj = SolutionSequence('Sscript','Xrv = RandomVariable(''Sdistribution'',''normal'',''mean'',varargin{1}+varargin{2}+Xpar.value,''std'',1); COSSANoutput{1} = Xrv;',...
                            'Cinputnames',{'Xdv1','Xdv2'}, ...                            
                            'Coutputnames',{'out1'}, ...
                            'CprovidedObjectTypes',{'Input'},...
                            'Cobject2output',{'.mean'},...
                            'CobjectsNames',{'Xpar'},...
                            'CobjectsTypes',{'Parameter'},...
                            'CXobjects',{Xpar});
    Xout=Xobj.apply(Xin.getStructure);  
    Cmess{itest} = 'This test shall fail since the provided object is a RandomVariable (and not an Input)';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 Cobject2output does not provide values
itest = itest+1;
try
    Xdv1 = DesignVariable('Vvalues',1:10,'value',5);
    Xdv2 = DesignVariable('Vvalues',1:10,'value',5);
    Xpar = Parameter('value',1);
    Xin = Input('CXmembers',{Xdv1 Xdv2},'CSmembers',{'Xdv1' 'Xdv2'});
    Xobj = SolutionSequence('Sscript','Xrv = RandomVariable(''Sdistribution'',''normal'',''mean'',varargin{1}+varargin{2}+Xpar.value,''std'',1); COSSANoutput{1} = Xrv;',...
                            'Cinputnames',{'Xdv1','Xdv2'}, ...                            
                            'Coutputnames',{'out1'}, ...
                            'CprovidedObjectTypes',{'RandomVariable'},...
                            'Cobject2output',{'.Cpar{1,1}'},...
                            'CobjectsNames',{'Xpar'},...
                            'CobjectsTypes',{'Parameter'},...
                            'CXobjects',{Xpar});
    Xout=Xobj.apply(Xin.getStructure);  
    
    
    Cmess{itest} = 'This test returns the valus of the fiels of RandomVariable';
    Vtest(itest)=true;
catch ME
    
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 Do not pass type of object passed and of object which is output
itest = itest+1;
try
    Xdv1 = DesignVariable('Vvalues',1:10,'value',5);
    Xdv2 = DesignVariable('Vvalues',1:10,'value',5);
    Xpar = Parameter('value',1);
    Xin = Input('CXmembers',{Xdv1 Xdv2},'CSmembers',{'Xdv1' 'Xdv2'});
    Xobj = SolutionSequence('Sscript','Xrv = RandomVariable(''Sdistribution'',''normal'',''mean'',varargin{1}+varargin{2}+Xpar.value,''std'',1); COSSANoutput{1} = Xrv;',...
                            'Cinputnames',{'Xdv1','Xdv2'}, ...                            
                            'Coutputnames',{'out1'}, ... 
                            'Cobject2output',{'.mean'},...
                            'CobjectsNames',{'Xpar'},...
                            'CXobjects',{Xpar});
    Xout=Xobj.apply(Xin.getStructure); 
    if Xout.Mvalues == 11 % mean value of computed rv shall be 11
        Vtest(itest)=true;
    end
    Cmess{itest} = 'Apply object without defining the types of the involved objects';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 Pass the JobManager
itest = itest+1;
try
    Xdv1 = DesignVariable('Vvalues',1:10,'value',5);
    Xdv2 = DesignVariable('Vvalues',1:10,'value',5);
    Xpar = Parameter('value',1);
    Xin = Input('CXmembers',{Xdv1 Xdv2},'CSmembers',{'Xdv1' 'Xdv2'});
    
    Xjm = JobManagerInterface('Stype','GridEngine');
    Xg = JobManager('Sdescription','test #1',...
        'Squeue','pizzas64.q','Shostname','c810-cl21.uibk.ac.at',......
        'Xjobmanagerinterface',Xjm);
    
    Xobj = SolutionSequence('Sscript','Xrv = RandomVariable(''Sdistribution'',''normal'',''mean'',varargin{1}+varargin{2}+Xpar.value,''std'',1); COSSANoutput{1} = Xrv;',...
                            'Cinputnames',{'Xdv1','Xdv2'}, ...                            
                            'Coutputnames',{'out1'}, ... 
                            'Cobject2output',{'.mean'},...
                            'CobjectNames',{'Xpar'},...
                            'CXobjects',{Xpar},'XjobManager',Xg);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11 Pass the JobManagerInterface
itest = itest+1;
try
    Xdv1 = DesignVariable('Vvalues',1:10,'value',5);
    Xdv2 = DesignVariable('Vvalues',1:10,'value',5);
    Xpar = Parameter('value',1);
    Xin = Input('CXmembers',{Xdv1 Xdv2},'CSmembers',{'Xdv1' 'Xdv2'});
    
    Xjm = JobManagerInterface('Stype','GridEngine');
   
    Xobj = SolutionSequence('Sscript','Xrv = RandomVariable(''Sdistribution'',''normal'',''mean'',varargin{1}+varargin{2}+Xpar.value,''std'',1); COSSANoutput{1} = Xrv;',...
                            'Cinputnames',{'Xdv1','Xdv2'}, ...                            
                            'Coutputnames',{'out1'}, ... 
                            'Cobject2output',{'.mean'},...
                            'CobjectNames',{'Xpar'},...
                            'CXobjects',{Xpar},'Xjobmanagerinterface',Xjm,...
                            'Squeue','pizzas64.q','Shostname','c810-cl21.uibk.ac.at');
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


if nargout>0
% Export name of the UnitTest
varargout{1}='SolutionSequence';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
    %% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of the SolutionSequence (' datestr(now) ')'])
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



