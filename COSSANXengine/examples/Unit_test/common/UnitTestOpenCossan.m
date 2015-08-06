function varargout = UnitTestOpenCossan
%% Unit Test for the OpenCossan object
%
% Author: Edoardo Patelli
% Institute for Risk and Uncertainty, University of Liverpool, UK
% email address: openengine@cossan.co.uk
% Website: http://www.cossan.co.uk

% =====================================================================
% This file is part of openCOSSAN.  The open general purpose matlab
% toolbox for numerical analysis, risk and uncertainty quantification.
%
% openCOSSAN is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License.
%
% openCOSSAN is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with openCOSSAN.  If not, see <http://www.gnu.org/licenses/>.
% =====================================================================

warning('OpenCossan:UnitTest','This unit test can only test some properties of the OpenCossan class')


% Initialize variables
Ntest=12;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

% Set the COSSANengine root path! 
% For instance, 
% ScossanRoot=%HOME/workspace/OpenCossan/trunk/COSSANXengine
%
% or 
%
% ScossanRoot=[userpath filesep COSSANXengine];




%% 1. 
global OPENCOSSAN

% Store OpenCossan object
XopenCossanBackup=OPENCOSSAN;
ScossanRoot=OpenCossan.getCossanRoot;
SexternalPath=OpenCossan.getCossanExternalPath;
SmatlabDatabasePath=OPENCOSSAN.SmatlabDatabasePath;

itest = 1;
try
    Xt=OpenCossan;
    display(Xt)
    Vtest(itest)=true;
    Cmess{itest}='Initialization of openCOSSAN';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2. 

itest = itest + 1;
try
    Xt = OpenCossan('ScossanRoot',ScossanRoot);
    Vtest(itest)=true;
    Cmess{itest}='Setting CossanRoot';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3. Check whether the global object has been created
itest = itest + 1;
try
    global OPENCOSSAN %#ok<REDEF,TLEV>
    assert(isa(OPENCOSSAN,'OpenCossan'),'Wrong class (%s)',class(OPENCOSSAN));
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4. Check static methods 

itest = itest + 1;
try
    Xt = OpenCossan('SexternalPath',SexternalPath);
    Vtest(itest)=true;
    Cmess{itest}='Setting SexternalPath';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5.

itest = itest + 1;
try
    Xt = OpenCossan('Smatlabdatabasepath',Smatlabdatabasepath);
    Vtest(itest)=true;
    Cmess{itest}='Setting SdatabasePath';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6. 

itest = itest + 1;
try
    Xt = OpenCossan('SdiaryFileName','dummyt.txt');
    Vtest(itest)=true;
    Cmess{itest}='setting SdiaryFileName';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7.

itest = itest + 1;
try
    Xt = OpenCossan('NverboseLevel',2);
    Vtest(itest)=true;
    Cmess{itest}='Setting NverboseLevel';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8.

itest = itest + 1;
try
    Xt = OpenCossan('NverboseLevel',5);
    Vtest(itest)=true;
    Cmess{itest}='Setting NverboseLevel higher than 4';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9.

itest = itest + 1;
try
    Xt = OpenCossan('Nseed',3);
    Vtest(itest)=true;
    Cmess{itest}='Setting Nseed';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 10. 

itest = itest + 1;
try
    Xt = setPath(Xt,'Scossanroot','/home/hmp/matlab/toolbox/COSSANXengine/');
    Vtest(itest)=true;
    Cmess{itest}='using setPath method';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 11. 

itest = itest + 1;
try
    OpenCossan.cossanDisp('hello',1);
    Vtest(itest)=true;
    Cmess{itest}='using cossanDisp method';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12. 

itest = itest + 1;
try
    Sout=getCossanRoot(Xt); %#ok<*NASGU>
    Vtest(itest)=true;
    Cmess{itest}='using getCossanRoot method';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Finish the test

if nargout>0
% Export name of the UnitTest
varargout{1}='OpenCossan';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
%% Show summary of the test

disp('--------------------------------------------------------------------')
disp([' Unit Test of OpenCossan Object (' datestr(now) ')'])
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
