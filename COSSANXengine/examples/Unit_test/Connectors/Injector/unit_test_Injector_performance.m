
nsamples = 1;

XEmod = RandomVariable('Sdistribution','normal','mean',2.1E+11,'std',2.1E+10);
Xnu = RandomVariable('Sdistribution','normal','mean',0.3,'std',0.03);
XP = RandomVariable('Sdistribution','normal','mean',10000,'std',1000);

% add the RandomVariable objects to a RandomVariableSet object
Xrvset = RandomVariableSet('Sdescription','random variable set used in the ABAQUS truss example',...
    'Cmembers',{'XEmod','XP','Xnu'},'CXrv',{XEmod,XP,Xnu});

% add the RandomVariableSet object to an Input object
Xi = Input;
Xi = add(Xi,Xrvset);
% create samples of the Input object
Xi = sample(Xi,'nsamples',nsamples);
%%
tic;
Xinj = Injector('Stype','scan','Spath','./','Sscanfilename','test.cossan','Sfile','test.inp');
time = toc;
display(['Elapsed time to create by scan an Injector from a file with 3000 identifiers:' num2mstr(time)]);
%%
tic;
inject(Xinj,get(Xi,'values'))
time = toc;
display(['Elapsed time to inject 3000 values:' num2str(time)]);

try 
    load('test.inp');
    if size(test) ~= [1000,3]
        error('openCOSSAN:unit_test:Injector','Unit test Injector failed')
    end
catch ME
    error('openCOSSAN:unit_test:Injector','Unit test Injector failed')
end

display('Unit test passed')