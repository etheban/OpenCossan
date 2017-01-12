classdef UnitTestCossanTutorials
    %UnitTestCossan This class Test and check all the Cossan Tutorials
    %   Detailed explanation goes here
    
    properties
        CtutorialList
        XtutorialDate
        CdocList
        XdocDate
        VmappingTutorialsDocs
        VoutdatedDocs     %List of outdated docs
        VmissingDocs      %List of missing docs
        VorphanDocs       %List of orphan docs 
    end
    
    methods
        
        function Xobj=UnitTestCossanTutorials
            % Initialize object
            Xobj=getTutorialList(Xobj);
            Xobj=getDocList(Xobj);
            Xobj=checkDocs(Xobj);
        end
        
        function Xobj=getTutorialList(Xobj)
            % Tutorial Folder List
            
            % Use dir because you can collect more information, including the file date
            L1 = dir(fullfile(OpenCossan.getCossanRoot,'examples','Tutorials'));
            
            % The first 2 are always '.' and '..'
            L1(1:2)=[];
            % Remove non folders
            L1(~[L1.isdir])=[];
            
            % Initialize variable
            TtutorialList=[];
            for n = 1:length(L1)
                L2= dir(fullfile(OpenCossan.getCossanRoot,'examples','Tutorials',L1(n).name,'Tutorial*.m'));
                % remove directories (~[L2.isdir])
                % Merge results
                TtutorialList=[TtutorialList; L2(~[L2.isdir])]; %#ok<AGROW>
            endXobj.XtutorialDate(Vpos)
            Ctmp=struct2cell(TtutorialList);
            
            Xobj.CtutorialList=Ctmp(1,:);
            Xobj.XtutorialDate=datetime(Ctmp(2,:));
            % Now TutorialList contains all the name of the tutorials and also
            % information about the creation date that can be compared with the date of
            % the documentation
            
            
        end
        
        function Xobj=getDocList(Xobj)
            Ctmp=struct2cell(dir(fullfile(OpenCossan.getCossanRoot,'docs','html','Tutorial*.html')));
            Xobj.CdocList=Ctmp(1,:);
            Xobj.XdocDate=datetime(Ctmp(2,:));
        end
        
        function Xobj=checkDocs(Xobj)
            % Reinitialize variable
            Xobj.VmissingDocs=true(length(Xobj.CtutorialList),1);
            
            Ctutorials=strrep(Xobj.CtutorialList(1,:),'.m','.html');
            Cdocs=strrep(Xobj.CdocList(1),'.m','.html');
           
                        
            for n = 1:size(Ctutorials,2)
                 Xobj.VmissingDocs(strcmp(Ctutorials(1,n),Cdocs))=false;
            end
                
            Xobj.VorphanDocs=true(length(Xobj.CdocList),1);
            
            Xobj.VmappingTutorialsDocs=zeros(length(Xobj.CdocList),1);
            
            for n = 1:size(Cdocs,2)
                 Vpos=find(strcmp(Cdocs(1),Ctutorials));
                 Xobj.VorphanDocs(Vpos)=false;                 
                 Xobj.VmappingTutorialsDocs(n)=Vpos;
                 % Check date
                 Xobj.VoutdatedDocs(n)=Xobj.XtutorialDate(Vpos)>Xobj.XdocDate(n);
            end                       
        end
    end
    end
end

