%% TUTORIALINPUT
% This turorial shows how to create and use an Input object
% The Input object is uses to generate samples of random variables, collection
% parameters, function and design variables
%
% See Also:  https://cossan.co.uk/wiki/index.php/@Input
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author:~Pierre~Beaurepaire$ 


%% Create additional object
% Now we create 4 different parameter objects that will be included in the
% Input object. Please refer to the documentation of the Parameter for more
% details
Xmat1   = Parameter('Sdescription','material 1 E','value',7E+7);
Xmat2   = Parameter('Sdescription','material 2 E','value',2E+7);
Xmat3   = Parameter('Sdescription','material 3 E','value',1E+4);
Xconfiguration  = Parameter('Sdescription','material configuration','Mvalue',unidrnd(3,16,16));

% Now we create RandomVariable and RandomVariableSet
x1  = RandomVariable('Sdistribution','normal','mean',2.763,'std',0.4);
x2  = RandomVariable('Sdistribution','normal','mean',1.25,'std',0.4);
Xrvs1 = RandomVariableSet('Cmembers',{'x1','x2'},'Xrv',[x1 x2]);

% Create a second RandomVariableSet
% Definition of Random Variables
x3  = RandomVariable('Sdistribution','uniform','lowerbound',0,'upperbound',10);
x4  = RandomVariable('Sdistribution','uniform','mean',5,'std',1);

Xrvs2   = RandomVariableSet('Cmembers',{'x3','x4'},'Xrv',[x3 x4]);

% Create RandomVariableSet with IID RandomVariable
Xrvs3 = RandomVariableSet('Xrv',x1,'Nrviid',10);
    
%% Create Functions
Xfun1   = Function('Sdescription','function #1', ...
    'Sexpression','<&x3&>+<&x4&>');
Xfun2   = Function('Sdescription','function #2', ...
    'Sexpression','<&Xmat3&>./<&x1&>');
Xfun3   = Function('Sdescription','function #2', ...
    'Sexpression','<&Xmat3&>+1');

%% Create an Input object that contains all the object already prepared
Xin=Input('Sdescription','My first Input'); % initialize Input object

% Add parameters to the input object
Xin = Xin.add('Sname','Xconfiguration','Xmember',Xconfiguration);
Xin = Xin.add('Xmember',Xmat1,'Sname','Xmat1');
Xin = Xin.add('Xmember',Xmat2,'Sname','Xmat2');
Xin = Xin.add('Xmember',Xmat3,'Sname','Xmat3');
% Add RandomVariable
Xin     = Xin.add('Xmember',Xrvs1,'Sname','Xrvs1');
Xin     = Xin.add('Xmember',Xrvs2,'Sname','Xrvs2');
Xin     = Xin.add('Xmember',Xrvs3,'Sname','Xrvs3');

% Add Functions
Xin = Xin.add('Xmember',Xfun1,'Sname','Xfun1');
Xin = Xin.add('Xmember',Xfun2,'Sname','Xfun2');
Xin = Xin.add('Xmember',Xfun3,'Sname','Xfun3');

%% Show summary of the Input object
display(Xin)

%% Generate samples from the Xinput object
Xin = Xin.sample; % Generate a single sample
display(Xin)

Xin = Xin.sample('Nsamples',20); % Generate 20 samples and replace the 
                                     % previous generated sample
display(Xin)

% Add additional samples to the previous sample                                     
Xin = Xin.sample('Nsamples',25,'Ladd',true);
display(Xin)

%% Using get and dependent field to retrieve information from Xinput
% get the list of the  RandomVariableSet
Cname=Xin.CnamesRandomVariableSet;
display('Name of the RandomVariableSet')
display(Cname')
% get the list of the Parameter
Cname=Xin.CnamesParameter;
display('Name of the Parameter')
display(Cname')
% get the list of Function
Cname=Xin.CnamesFunction;
display('Name of the Function')
display(Cname')
% get the list of StochasticProcess
Cname=Xin.CnamesStochasticProcess;
display('Name of the StochasticProcess')
display(Cname')
% get the list of all variables
Cname=Xin.Cnames;
display('Name of the Variable present in the Input')
display(Cname')

%% Retrieve values from the Input object
% recompute the values of the function
Vfvalues=get(Xin,'Xfunctionvalue');
display(Vfvalues)

% The function returns a cell array
Cvalue=Xin.getValues('Sname','Xfun1');
display(Cvalue)

% retrive the values of the input (as a structure)
Tstruct=Xin.getStructure;
display(Tstruct)

% or as a matrix (rvs and functions only)
Msamples=Xin.getSampleMatrix;
display(Msamples);

% retrieve default values of the Xinput (i.e. mean values of the rvs)
get(Xin,'defaultvalues')

OpenCossan.cossanDisp('End of the Tutorial, bye bye! ')

