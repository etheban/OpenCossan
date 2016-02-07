function varargout = OpenCossanApp(varargin)
% OPENCOSSANAPP MATLAB code for OpenCossanApp.fig
%      OPENCOSSANAPP, by itself, creates a new OPENCOSSANAPP or raises the existing
%      singleton*.
%
%      H = OPENCOSSANAPP returns the handle to a new OPENCOSSANAPP or the handle to
%      the existing singleton*.
%
%      OPENCOSSANAPP('CALLBACK',hObject,eventData,handles,...) calls the local
%      function named CALLBACK in OPENCOSSANAPP.M with the given input arguments.
%
%      OPENCOSSANAPP('Property','Value',...) creates a new OPENCOSSANAPP or raises the
%      existing singleton*.  Starting from the left, property value pairs are
%      applied to the GUI before OpenCossanApp_OpeningFcn gets called.  An
%      unrecognized property name or invalid value makes property application
%      stop.  All inputs are passed to OpenCossanApp_OpeningFcn via varargin.
%
%      *See GUI Options on GUIDE's Tools menu.  Choose "GUI allows only one
%      instance to run (singleton)".
%
% See also: GUIDE, GUIDATA, GUIHANDLES
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


% Edit the above text to modify the response to help OpenCossanApp

% Last Modified by GUIDE v2.5 14-Jan-2015 16:39:00

% Begin initialization code - DO NOT EDIT
gui_Singleton = 1;
gui_State = struct('gui_Name',       mfilename, ...
    'gui_Singleton',  gui_Singleton, ...
    'gui_OpeningFcn', @OpenCossanApp_OpeningFcn, ...
    'gui_OutputFcn',  @OpenCossanApp_OutputFcn, ...
    'gui_LayoutFcn',  [] , ...
    'gui_Callback',   []);
if nargin && ischar(varargin{1})
    gui_State.gui_Callback = str2func(varargin{1});
end

if nargout
    [varargout{1:nargout}] = gui_mainfcn(gui_State, varargin{:});
else
    gui_mainfcn(gui_State, varargin{:});
end
% End initialization code - DO NOT EDIT


% --- Executes just before OpenCossanApp is made visible.
function OpenCossanApp_OpeningFcn(hObject, eventdata, handles, varargin)
% This function has no output args, see OutputFcn.
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
% varargin   command line arguments to OpenCossanApp (see VARARGIN)

% Choose default command line output for OpenCossanApp
handles.output = hObject;

Sapps=matlab.apputil.getInstalledAppInfo;
mg = arrayfun(@(x)strcmp(x.name,'OpenCossanApp'),Sapps);

if isempty(mg)
    % Use local path since the Apps path is not available
    handles.OpenCossanData.SAppPath=pwd;
else
    handles.OpenCossanData.SAppPath=Sapps(mg).location;
end

% Always initialize variables (allows update)
initializeVariables(hObject, eventdata, handles);    
handles=guidata(hObject);

if exist(fullfile(handles.OpenCossanData.SAppPath,'OpenCossanData.mat'),'file')
    handles.OpenCossanData=load(fullfile(handles.OpenCossanData.SAppPath,'OpenCossanData.mat'));
    set(handles.textInformation,...
        'String',...
        'Welcome back to OpenCossanApp')   
end


try
    axis(handles.axesLogo);
    imshow('OpenCossanIcon2.png')
catch
    set(handles.axesLogo,'Visible','off');
end

if strcmp(handles.OpenCossanData.SourcePath,'N/A')||strcmp(handles.OpenCossanData.InstallationPath,'N/A')
   set(handles.pushbuttonManualInstall,'enable','off');
end

% Update (or initialize) display
set(handles.textInstallationPath,'string',handles.OpenCossanData.InstallationPath);
set(handles.textSourcePath,'string',handles.OpenCossanData.SourcePath);
set(handles.textOpenCossan,'string',handles.OpenCossanData.OpenCossanInstalledVersion);
set(handles.textExamples,'string',handles.OpenCossanData.ExamplesInstalledVersion);
set(handles.textDocs,'string',handles.OpenCossanData.DocsInstalledVersion);
set(handles.textAddOns,'string',handles.OpenCossanData.AddOnsInstalledVersion);
set(handles.textVersionApp,'string',handles.OpenCossanData.AppInstalledVersion);



% Update handles structure
guidata(hObject,handles)

% UIWAIT makes OpenCossanApp wait for user response (see UIRESUME)
% uiwait(handles.figure1);


% --- Outputs from this function are returned to the command line.
function varargout = OpenCossanApp_OutputFcn(hObject, eventdata, handles) %#ok<INUSL>
% varargout  cell array for returning output args (see VARARGOUT);
% hObject    handle to figure
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Get default command line output from handles structure
varargout{1} = handles.output;


% --- Executes on button press in Run.
function Run_Callback(hObject, eventdata, handles) %#ok<INUSL>
% hObject    handlePlease OpenCossan not found.  to Run (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

try
    addpath(fullfile(handles.OpenCossanData.InstallationPath,'COSSANXengine','src','common'));
    set(handles.textInformation,'String','Initialising OpenCossan')
    OpenCossan('ScossanRoot',handles.OpenCossanData.InstallationPath)
    close OpenCossanApp
catch
    set(handles.textInformation,'String','OpenCossan not found. Please check your installation path')
end


% --- Executes on button press in CheckUpdates.
function CheckUpdates_Callback(hObject, eventdata, handles) %#ok<INUSL>
% hObject    handle to CheckUpdates (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Main
handles.OpenCossanData.OpenCossanServerVersion=getRevision(handles.OpenCossanData.ServerSVNInfoFileOpenCossan,handles);

% Compare with the local version
if ~strcmp(handles.OpenCossanData.OpenCossanServerVersion,handles.OpenCossanData.OpenCossanInstalledVersion)
    set(handles.textOpenCossan,'string',['Current version: ' char(handles.OpenCossanData.OpenCossanInstalledVersion) ' Available: ' char(handles.OpenCossanData.OpenCossanServerVersion)])
    handles.OpenCossanData.updateCossanNeeded=1;
else
    handles.OpenCossanData.updateCossanNeeded=0;
end

% AddOns
handles.OpenCossanData.AddOnsServerVersion=getRevision(handles.OpenCossanData.ServerSVNInfoFileAddOns,handles);
% Compare with the local version
if ~strcmp(handles.OpenCossanData.AddOnsServerVersion,handles.OpenCossanData.AddOnsInstalledVersion)
    set(handles.textAddOns,'string',['Current version: ' char(handles.OpenCossanData.AddOnsInstalledVersion) ' Available: ' char(handles.OpenCossanData.AddOnsServerVersion)])
    handles.OpenCossanData.updateAddOnsNeeded=1;
else
    handles.OpenCossanData.updateAddOnsNeeded=0;
end


% Examples
handles.OpenCossanData.ExamplesServerVersion=getRevision(handles.OpenCossanData.ServerSVNInfoFileExamples,handles);
% Compare with the local version
if ~strcmp(handles.OpenCossanData.ExamplesServerVersion,handles.OpenCossanData.ExamplesInstalledVersion)
    set(handles.textExamples,'string',['Current version: ' char(handles.OpenCossanData.ExamplesInstalledVersion) ' Available: ' char(handles.OpenCossanData.ExamplesServerVersion)])
    handles.OpenCossanData.updateExamplesNeeded=1;
else
    handles.OpenCossanData.updateExamplesNeeded=0;
end

% Docs
handles.OpenCossanData.DocsServerVersion=getRevision(handles.OpenCossanData.ServerSVNInfoFileDocs,handles);
% Compare with the local version
if ~strcmp(handles.OpenCossanData.DocsServerVersion,handles.OpenCossanData.DocsInstalledVersion)
    set(handles.textDocs,'string',['Current version: ' char(handles.OpenCossanData.DocsInstalledVersion) ' Available: ' char(handles.OpenCossanData.DocsServerVersion)])
    handles.OpenCossanData.updateDocsNeeded=1;
else
    handles.OpenCossanData.updateDocsNeeded=0;
end

% CossanApp 
handles.OpenCossanData.AppServerVersion=getRevision(handles.OpenCossanData.ServerSVNInfoFileApp,handles);
% Compare with the local version
if ~strcmp(handles.OpenCossanData.AppServerVersion,handles.OpenCossanData.AppInstalledVersion)
    set(handles.textVersionApp,'string',['Current version: ' char(handles.OpenCossanData.AppInstalledVersion) ' Available: ' char(handles.OpenCossanData.AppServerVersion)])
    handles.OpenCossanData.updateAppNeeded=1;
else
    handles.OpenCossanData.updateAppNeeded=0;
end

if any([handles.OpenCossanData.updateCossanNeeded handles.OpenCossanData.updateDocsNeeded handles.OpenCossanData.updateExamplesNeeded handles.OpenCossanData.updateAddOnsNeeded])
    set(handles.textInformation,'String','One or more Update(s) are available. Set a package path, then press download.')
    set(handles.Download,'Enable','On');
end

% Update data
guidata(hObject,handles);

% --- Executes on button press in Download.
function Download_Callback(hObject, eventdata, handles) %#ok<INUSL>
% hObject    handle to Download (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


set(handles.textInformation,'String','Please be awere that IE and Safary are extracting  the zip files automatically. All the downloaded files must have the extension .zip');
drawnow()
        
if handles.OpenCossanData.updateCossanNeeded==1
    Sname=fullfile(handles.OpenCossanData.SourcePath,handles.OpenCossanData.InstallationFileNameOpenCossan);
    if exist(Sname,'file')
        delete(Sname);
    end
    
    url=[handles.OpenCossanData.URL handles.OpenCossanData.InstallationFileNameOpenCossan];
    web(url,'-browser');

   handles.OpenCossanData.OpenCossanInstalledVersion=getRevision(handles.OpenCossanData.ServerSVNInfoFileOpenCossan,handles);
end

% AddOns
if handles.OpenCossanData.updateAddOnsNeeded==1
    Sname=fullfile(handles.OpenCossanData.SourcePath,handles.OpenCossanData.InstallationFileNameAddOns);
    if exist(Sname,'file')
        delete(Sname);
    end
    
    url=[handles.OpenCossanData.URL handles.OpenCossanData.InstallationFileNameAddOns];
    web(url,'-browser');
    handles.OpenCossanData.AddOnsInstalledVersion=getRevision(handles.OpenCossanData.ServerSVNInfoFileAddOns,handles);
end

% Examples
if handles.OpenCossanData.updateExamplesNeeded==1
    Sname=fullfile(handles.OpenCossanData.SourcePath,handles.OpenCossanData.InstallationFileNameExamples);
    if exist(Sname,'file')
        delete(Sname);
    end
    
    url=[handles.OpenCossanData.URL handles.OpenCossanData.InstallationFileNameExamples];
    web(url,'-browser');
    
    handles.OpenCossanData.ExamplesInstalledVersion=getRevision(handles.OpenCossanData.ServerSVNInfoFileExamples,handles);
end

%% Docs
if handles.OpenCossanData.updateDocsNeeded==1
    Sname=fullfile(handles.OpenCossanData.SourcePath,handles.OpenCossanData.InstallationFileNameDocs);
    if exist(Sname,'file')
        delete(Sname);
    end
    
    url=[handles.OpenCossanData.URL handles.OpenCossanData.InstallationFileNameDocs];
    web(url,'-browser');

    handles.OpenCossanData.DocsInstalledVersion=getRevision(handles.OpenCossanData.ServerSVNInfoFileDocs,handles);
end

%% Enable install button
if any([handles.OpenCossanData.updateDocsNeeded handles.OpenCossanData.updateAddOnsNeeded ...
        handles.OpenCossanData.updateExamplesNeeded handles.OpenCossanData.updateCossanNeeded] )
   set(handles.Install,'Enable','On')
else
   set(handles.Install,'Enable','Off')
end

%% CossanApp
if handles.OpenCossanData.updateAppNeeded==1
    Sname=fullfile(handles.OpenCossanData.SourcePath,handles.OpenCossanData.InstallationFileNameApp);
    if exist(Sname,'file')
        delete(Sname);
    end
    
    url=[handles.OpenCossanData.URL handles.OpenCossanData.InstallationFileNameApp];
    web(url,'-browser');
    
    
    handles.OpenCossanData.AppInstalledVersion=getRevision(handles.OpenCossanData.ServerSVNInfoFileApp,handles);
       
    set(handles.textInformation,'String','Please download the new app and then press "Update App"')
    set(handles.CossanApp,'Enable','on')
else
    set(handles.CossanApp,'Enable','off')
end

guidata(hObject,handles);

% --- Executes on button press in Install.
function Install_Callback(hObject, eventdata, handles) %#ok<INUSL>
% hObject    handle to Install (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

if ~isdir(handles.OpenCossanData.InstallationPath)
     set(handles.textInformation,'String','Please define a valid installation path')
     guidata(hObject,handles);
     return
end

if ~isdir(handles.OpenCossanData.SourcePath)
     set(handles.textInformation,'String','Please define a valid package path')
     guidata(hObject,handles);
     return
end

LupdateOpenCossan=logical(handles.OpenCossanData.updateOpenCossanNeeded);
LupdateExamples=logical(handles.OpenCossanData.updateExamplesNeeded);
LupdateAddOns=logical(handles.OpenCossanData.updateAddOnsNeeded);

Stextupdate=InstallOpenCossan(handles,LupdateOpenCossan,LupdateExamples,LupdateAddOns);

set(handles.textInformation,'String',Stextupdate)

guidata(hObject,handles);

% --- Executes on button press in CossanApp.
function CossanApp_Callback(hObject, eventdata, handles)
% hObject    handle to CossanApp (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

Sfilename=fullfile(handles.OpenCossanData.SourcePath,handles.OpenCossanData.InstallationFileNameApp);

if exist(Sfilename,'file')
    set(handles.textInformation,'String','Updating the App')
else
    set(handles.textInformation,'String','Please download the app first')
end

guidata(hObject,handles)

% --- Executes when user attempts to close figure1.
function figure1_CloseRequestFcn(hObject, eventdata, handles)
% hObject    handle to figure1 (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

% Hint: delete(hObject) closes the figure
OpenCossanData = handles.OpenCossanData;
save(fullfile(handles.OpenCossanData.SAppPath,'OpenCossanData.mat'),'-struct','OpenCossanData');

delete(hObject);



% --- Executes on button press in pushbuttonInstallationPath.
function pushbuttonInstallationPath_Callback(hObject, eventdata, handles) %#ok<INUSL>
% hObject    handle to pushbuttonInstallationPath (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

handles.OpenCossanData.SourcePath=uigetdir;
set(handles.textSourcePath,'string',handles.OpenCossanData.SourcePath);

% Enable manual installation
if ~strcmp(handles.OpenCossanData.SourcePath,'N/A')||~strcmp(handles.OpenCossanData.InstallationPath,'N/A')
    set(handles.pushbuttonManualInstall,'enable','on');
end

if any([handles.OpenCossanData.updateOpenCossanNeeded handles.OpenCossanData.updateExamplesNeeded ...
        handles.OpenCossanData.updateAddOnsNeeded handles.OpenCossanData.updateDocsNeeded] )
   set(handles.Install,'Enable','On')  
else
   set(handles.Install,'Enable','Off')
end


drawnow()
guidata(hObject,handles)


function pushbuttonManualInstall_CreateFcn(hObject, eventdata, handles) %#ok<INUSD>
% hObject    handle to pushbuttonInstallationPath (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)


% --- Executes on button press in pushbuttonManualInstall.
function pushbuttonManualInstall_Callback(hObject, eventdata, handles) %#ok<INUSL>
% hObject    handle to pushbuttonReset (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

StextInfo='Manual Installation of: ';
% Check .gtz files availables
if exist(fullfile(handles.OpenCossanData.SourcePath,handles.OpenCossanData.InstallationFileNameOpenCossan),'file')
    LupdateOpenCossan=true;
    StextInfo=[StextInfo 'OpenCossan; '];
else
    LupdateOpenCossan=false;       
end
  
if exist(fullfile(handles.OpenCossanData.SourcePath,handles.OpenCossanData.InstallationFileNameExamples),'file')
    LupdateExamples=true;
    StextInfo=[StextInfo 'Examples; '];
else
    LupdateExamples=false;       
end

if exist(fullfile(handles.OpenCossanData.SourcePath,handles.OpenCossanData.InstallationFileNameAddOns),'file')
    LupdateAddOns=true;
    StextInfo=[StextInfo 'AddOns; '];
else
    LupdateAddOns=false;       
end

if all([LupdateOpenCossan LupdateExamples LupdateAddOns]==false)
    set(handles.textInformation,'String','No .zip files available!')
else
    set(handles.textInformation,'String',StextInfo)
    StextOut=InstallOpenCossan(handles,LupdateOpenCossan,LupdateExamples,LupdateAddOns);
    set(handles.textInformation,'String',StextOut)
end

drawnow()
guidata(hObject,handles)

% --- Executes on button press in pushbuttonDestination.
function pushbuttonDestination_Callback(hObject, eventdata, handles) %#ok<INUSL>
% hObject    handle to pushbuttonDestination (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)

%% WHERE COSSAN WILL BE INSTALLED
handles.OpenCossanData.InstallationPath=uigetdir;
set(handles.textInstallationPath,'string',handles.OpenCossanData.InstallationPath);

if ~strcmp(handles.OpenCossanData.SourcePath,'N/A')
    set(handles.pushbuttonManualInstall,'enable','on');
end

drawnow()
guidata(hObject,handles)


% --- Executes during object creation, after setting all properties.
function textSourcePath_CreateFcn(hObject, eventdata, handles) %#ok<INUSD>
% hObject    handle to textSourcePath (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called


% --- Executes on button press in pushbuttonReset.
function pushbuttonReset_Callback(hObject, eventdata, handles)
% hObject    handle to pushbuttonReset (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    structure with handles and user data (see GUIDATA)
if exist('OpenCossanData.mat','file')
    delete('OpenCossanData.mat')
end

initializeVariables(hObject, eventdata, handles);


function initializeVariables(hObject, eventdata, handles) %#ok<INUSL>
% hObject    handle to pushbuttonManualInstall (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
%
% This function initialize the variables stored in the matlab file
% OpenCossanData.mat 

handles.OpenCossanData.InstallationPath='N/A';
handles.OpenCossanData.SourcePath='N/A';
handles.OpenCossanData.OpenCossanInstalledVersion='N/A';
handles.OpenCossanData.ExamplesInstalledVersion='N/A';
handles.OpenCossanData.DocsInstalledVersion='N/A';
handles.OpenCossanData.AddOnsInstalledVersion='N/A';
handles.OpenCossanData.AppInstalledVersion='N/A';
handles.OpenCossanData.updateOpenCossanNeeded=1;
handles.OpenCossanData.updateExamplesNeeded=1;
handles.OpenCossanData.updateAddOnsNeeded=1;
handles.OpenCossanData.updateDocsNeeded=1;
handles.OpenCossanData.updateAppNeeded=0;
handles.OpenCossanData.InstallationFileNameOpenCossan='OpenCossan.zip';
handles.OpenCossanData.InstallationFileNameExamples='OpenCossanExamples.zip';
handles.OpenCossanData.InstallationFileNameDocs='OpenCossanDocs.zip';
handles.OpenCossanData.InstallationFileNameAddOns='OpenCossanAddOns.zip';
handles.OpenCossanData.InstallationFileNameApp='OpenCossanApp.mlappinstall';
handles.OpenCossanData.ServerSVNInfoPath='http://cossan.co.uk/svninfo/';
handles.OpenCossanData.ServerSVNInfoFileOpenCossan='svn_OpenCossan.zip.xml';
handles.OpenCossanData.ServerSVNInfoFileExamples='svn_OpenCossanExamples.zip.xml';
handles.OpenCossanData.ServerSVNInfoFileDocs='svn_OpenCossanDocs.zip.xml';
handles.OpenCossanData.ServerSVNInfoFileAddOns='svn_OpenCossanAddOns.zip.xml';
handles.OpenCossanData.ServerSVNInfoFileApp='svn_OpenCossanApp.mlappinstall.xml';
handles.OpenCossanData.URL='https://cossan.co.uk/svn/OpenCossan/branches/Archives/stable/';

%Get revision for the COSSAN APP. I am assuming that when the CossanAPP is
% used for the first time it is up-to-date 

try
    Crevision=getRevision(handles.OpenCossanData.ServerSVNInfoFileApp,handles);
    handles.OpenCossanData.AppInstalledVersion=Crevision;
    
    set(handles.textInformation,...
    'String',...
    'For the first use of OpenCossan, please define the installation path')
catch
     set(handles.textInformation,...
    'String',...
    ['Unable to connect with COSSAN server. Download the file ' handles.OpenCossanData.ServerSVNInfoPath handles.OpenCossanData.ServerSVNInfoFileApp])
end


guidata(hObject,handles)

hold=guidata(hObject);


function [Srevision]=getRevision(filename,handles)
% Private function to retrieve the revision number from the server

try 
    Xfile=xmlread([handles.OpenCossanData.ServerSVNInfoPath filename]);
    Xout=Xfile.getDocumentElement;
    Xelement=Xout.getElementsByTagName('entry');
    Xitem=Xelement.item(0);
    Srevision=char(Xitem.getAttribute('revision'));
catch
    warndlg({[' Problem retriving information from : ' handles.OpenCossanData.ServerSVNInfoPath filename]; ...
        'Please check that you can connect with the Cossan server'},'OpenCossanApp','replace')
    Srevision='N/A';
end

function [Stextupdate]=InstallOpenCossan(handles,LupdateOpenCossan,LupdateExamples,LupdateAddOns)
% This function is used to install OpenCossan using the Manual Installation
% button or the Automatic Installation

choice = questdlg('Pressing OK the existing installation of OpenCossan will be overwritten. Do you want to continue?', ...
    'Installing OpenCossan', ...
    'Yes sure','No thank you','I am not sure','I am not sure');
% Handle response
switch choice
    case 'Yes sure'
        
        Stextupdate='Install/Updating ... Please wait';
        set(handles.textInformation,'String',Stextupdate)
        drawnow();
        extractOpenCossan('SdestinationPath',char(handles.OpenCossanData.InstallationPath),...
            'Ldocumentation',LupdateOpenCossan,...
            'lexamples',LupdateExamples,...
            'laddons',LupdateAddOns,...
            'Sfolder',char(handles.OpenCossanData.SourcePath));
        Stextupdate='Installation completed';
        set(handles.textInformation,'String',Stextupdate)
        drawnow();
    case 'No thank you'
        Stextupdate='Installation cancelled';
    case 'I am not sure'
        %warndlg(['Never mind... I will do anyway, but on: ' tempdir]);
        %uiwait
        Stextupdate=['Never mind... I will do anyway, but on: ' tempdir];
        set(handles.textInformation,'String',Stextupdate)
        drawnow();
        extractOpenCossan('SdestinationPath',tempdir,...
            'Ldocumentation',LupdateOpenCossan,...
            'lexamples',LupdateExamples,...
            'laddons',LupdateAddOns,...
            'Sfolder',char(handles.OpenCossanData.SourcePath));
        Stextupdate='Installation simulated (installed on your tempdir)';
        set(handles.textInformation,'String',Stextupdate)
        drawnow();
end



% --- Executes during object creation, after setting all properties.
function textOpenCossan_CreateFcn(hObject, eventdata, handles) %#ok<INUSD>
% hObject    handle to textOpenCossan (see GCBO)
% eventdata  reserved - to be defined in a future version of MATLAB
% handles    empty - handles not created until after all CreateFcns called
