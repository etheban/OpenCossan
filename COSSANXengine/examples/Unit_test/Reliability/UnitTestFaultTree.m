function varargout = UnitTestFaultTree
%UNITTESTEVALUATOR Summary of this function goes here
%   Detailed explanation goes here

% Test FaultTree
% Perallocate memory

Ntest=15;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

% define necessary objects
CnodeTypes={'Output','AND','Input','OR','Input','AND','Input','AND','Input','Input'};
CnodeNames={'TopEvent','AND gate 1','C','OR gate 1','A','AND gate 2','B','AND gate 3','B','D'};
VnodeConnections=[0 1 2 2 4 4 6 6 8 8];

%% 1. Create an empty object
itest = itest+1;
try
    Xobj = FaultTree;
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2. Create an object
itest = itest+1;
try
    Xobj = FaultTree('CnodeTypes',CnodeTypes,...
        'CnodeNames',CnodeNames,...
        'VnodeConnections',VnodeConnections) ;
    Vtest(itest)=true;
    Cmess{itest}='Create a fault tree object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3. wrong use of the constructor
%% the length of CnodeTypes CnodeNames and VnodeConnections are not the same
itest = itest+1;
try
    % the length of VnodeConnections is wrong
    Xobj = FaultTree('CnodeTypes',CnodeTypes,...
        'CnodeNames',CnodeNames,...
        'VnodeConnections',VnodeConnections(1:end-1)) ;
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4.
itest = itest+1;
try
    % the length of CnodeNames is wrong
    Xobj = FaultTree('CnodeTypes',CnodeTypes,...
        'CnodeNames',CnodeNames(1:end-1),...
        'VnodeConnections',VnodeConnections) ;
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5.
itest = itest+1;
try
    % the length of CnodeTypes is wrong
    Xobj = FaultTree('CnodeTypes',CnodeTypes(1:end-1),...
        'CnodeNames',CnodeNames,...
        'VnodeConnections',VnodeConnections) ;
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6. wrong node types are passed
% two Output nodes
itest = itest+1;
try
    CwrongNodeTypes={'Output','AND','Output','OR','Input','AND','Input','AND','Input','Input'};
    % the length of CnodeTypes is wrong
    Xobj = FaultTree('CnodeTypes',CwrongNodeTypes,...
        'CnodeNames',CnodeNames,...
        'VnodeConnections',VnodeConnections) ;
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7. wrong type
itest = itest+1;
try
    CwrongNodeTypes={'Output','AND','Pippo','OR','Input','Pluto','Input','AND','Input','Input'};
    % the length of CnodeTypes is wrong
    Xobj = FaultTree('CnodeTypes',CwrongNodeTypes,...
        'CnodeNames',CnodeNames,...
        'VnodeConnections',VnodeConnections) ;
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8. test methods
itest = itest+1;
% method findCutSets
try
    Xobj = FaultTree('CnodeTypes',CnodeTypes,...
    'CnodeNames',CnodeNames,...
    'VnodeConnections',VnodeConnections);
    Xobj = Xobj.findCutSets;
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9. method findMinimalCutSets
itest = itest+1;
try
    Xobj = Xobj.findMinimalCutSets;
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10. method removeNodes, addNodes
itest = itest+1;
try
    Xobj = Xobj.removeNodes('VnodeIndex',[10]);
    Xobj = Xobj.addNodes('CnodeTypes',{'AND' 'Input' 'Input'},...
        'CnodeNames',{'AND gate 4' 'D' 'E'},...
        'VnodeConnections',[8 10 10]);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 11. try to remove a non-existing node
itest = itest+1;
try
    Xobj = Xobj.removeNodes('VnodeIndex',[13]);
    display(Xobj)
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12. test plotTree method
itest = itest+1;
try
    Xobj.plotTree;
    Vtest(itest)=true;
    Cmess{itest}='';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13. export figure
itest = itest+1;
try
    Sfigurename='FigExported1.eps';
    Xobj.plotTree('SfigureName',Sfigurename)    
    if exist([OpenCossan.getCossanWorkingPath Sfigurename],'file')
        delete([OpenCossan.getCossanWorkingPath Sfigurename])
    else
        error('Figure FigExported1 not exported') %#ok<ERTAG>
    end
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 14. test the constructor and methods "suggested" in the reference manual
% Since it is the example of the reference manual, all the methods should
% work
itest = itest+1;
try
    XfaultTree=FaultTree('CnodeTypes',{'Input','Output'},...
        'CnodeNames',{'Event A','Out'},...
        'VnodeConnections',[2 0]);
    display(XfaultTree);
    Cmess{itest}='This should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 15. test the constructor and methods "suggested" in the reference manual
% Since it is the example of the reference manual, all the methods should
% work
itest = itest+1;
try
    XfaultTree=FaultTree('CnodeTypes',{'Output', 'AND', 'Input','Input'},...
        'CnodeNames',{'Top Event','AND' 'Event A','Event B'},...
        'VnodeConnections',[0 1 2 2]);
    display(XfaultTree)
    XfaultTree.findCutSets;
    XfaultTree.findMinimalCutSets;
    XfaultTree=XfaultTree.addNodes('CnodeTypes',{'Input'},'CnodeNames',{'Event C'},'VnodeConnections',[2]);
    XfaultTree.plotTree;
    XfaultTree=XfaultTree.addNodes('CnodeTypes',{'Input'},'CnodeNames',{'Event D'},'VnodeConnections',[2]);
    XfaultTree.plotTree;
    XfaultTree=XfaultTree.removeNodes('Vnodeindex',[3 4]) ;
    XfaultTree.plotTree;
    Vtest(itest)=true;
    Cmess{itest}='All reference manual examples are successfull';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

close all

%% finalize the test
if nargout>0
% Export name of the UnitTest
varargout{1}='FaultTree';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
    
%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of the FaultTree (' datestr(now) ')'])
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
return
