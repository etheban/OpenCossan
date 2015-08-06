%% Unit test for the TableExtractor

function varargout=UnitTestTableExtractor

Ntest=9;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

Xresp1 = Response('Sname', 'displacement1', ...
    'Sfieldformat', ['%f','%*s','%f','%f','%f','%*d','\n','%*s','%f','%f','%f','%*d'], ...
    'Clookoutfor',{'POINT ID =          18'}, ...
    'Ncolnum',1, ...
    'Nrownum',1,...
    'Nrepeat',3);
Xresp2 = Response('Sname', 'displacement2', ...
    'Sfieldformat', ['%f','%*s','%f','%f','%f','%*d','\n','%*s','%f','%f','%f','%*d'], ...
    'Clookoutfor',{'POINT ID =          28'}, ...
    'Ncolnum',1, ...
    'Nrownum',1,...
    'Nrepeat',3);

SworkDir = fullfile(OpenCossan.getCossanRoot,'/examples/Unit_test/Connectors/TableExtractor/');
%% 1 Method display
itest=1;
try
    Xex = TableExtractor('Sdescription', 'Table Extractor for Unit test',...
                'Sworkingdirectory',SworkDir, ...
                'Sfile','resultsResponse.txt', ....
                'Xresponse',Xresp1);
    display(Xex);
    Vtest(itest)=true;         
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 2 Check whether description is correct
itest=itest+1;
try
    Xex = TableExtractor('Sdescription', 'Table Extractor for Unit test',...
                'Sworkingdirectory',SworkDir, ...
                'Sfile','resultsResponse.txt', ....
                'Xresponse',Xresp1);
            
    if strcmp(Xex.Sdescription,'Table Extractor for Unit test')
        Vtest(itest)=true;  
    else
        Cmess{itest} = 'Sdescription not correct';
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Wrong file name passed
itest=itest+1;
try
    Xex = TableExtractor('Sdescription', 'Table Extractor for Unit test',...
                'Sworkingdirectory',SworkDir, ...
                'Sfile','resultsResponse1.txt', ....
                'Xresponse',Xresp1);
    Tout = extract(Xex);
    if isnan(Tout.(Xresp1.Sname))
    Vtest(itest)=true;  
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 File is in another directory than Sworkingdirectory

itest=itest+1;
try
    Xex = TableExtractor('Sdescription', 'Table Extractor for Unit test',...
                'Sworkingdirectory',SworkDir, ...
                'Sfile','resultsTable.txt', ....
                'Srelativepath','FilesToBeRead',...
                'Nheaderlines', 3, ...
                'Sdelimiter', ' ',...
                'Soutputname','displacement');
    Tout = extract(Xex);
    Vtest(itest)=true; 
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 Mandatory fields (Xresponse or Nheaderline,Sdelimiter and
%% Cresponsename) omitted

itest=itest+1;
try
    Xex = TableExtractor('Sdescription', 'Table Extractor for Unit test',...
                'Sworkingdirectory',SworkDir, ...
                'Sfile','resultsTable.txt', ....
                'Srelativepath','FilesToBeRead');
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 6 Mandatory fields (Nheaderline and Sdelimiter given, Soutputname) omitted

itest=itest+1;
try
    Xex = TableExtractor('Sdescription', 'Table Extractor for Unit test',...
                'Sworkingdirectory',SworkDir, ...
                'Sfile','resultsTable.txt', ....
                'Srelativepath','FilesToBeRead', ...
                'Nheaderlines', 3, ...
                'Sdelimiter', ' ');
    
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 7 Check whether values are extracted correctly (using 2 Response objects)

itest=itest+1;
try
Mcoord_target = [0.000000E+00,  5.000000E-03,  1.000000E-02];
Mdata_target =  [0.000000E+00,  2.779032E-06,  1.110885E-05;
                 0.000000E+00,  3.331650E-06,  1.449050E-05;
                 0.000000E+00, -1.591030E-05, -5.965713E-05;
                 0.000000E+00,  3.538517E-04,  1.327156E-03;
                 0.000000E+00,  8.607568E-05,  3.139196E-04;
                 0.000000E+00,  1.402515E-04,  5.257608E-04];

    Xex = TableExtractor('Sdescription', 'Table Extractor for Unit test',...
                'Sworkingdirectory',SworkDir, ...
                'Sfile','resultsResponse.txt', ....
                'Xresponse',[Xresp1, Xresp2]);
    Tout = extract(Xex);
    if all(Tout.displacement1.Mcoord==Mcoord_target) & ...
       all(Tout.displacement2.Mcoord==Mcoord_target) & ...     
       all(Tout.displacement1.Mdata==Mdata_target)  & ...
       all(Tout.displacement2.Mdata==Mdata_target)
            Vtest(itest)=true;
    else
       Cmess{itest} = 'Values not correctly read from file'; 
    end
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end 

%% 8 Check what happens if the Response objects are the same

itest=itest+1;
try
    Xex = TableExtractor('Sdescription', 'Table Extractor for Unit test',...
                'Sworkingdirectory',SworkDir, ...
                'Sfile','resultsResponse.txt', ....
                'Xresponse',[Xresp2, Xresp2]);
    Tout = extract(Xex);
    Cmess{itest}='This test shall fail since the response objects ar the same';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end 

%% 9 Check whether values are extracted correctly (using table specifications)

itest=itest+1;
try
    
Mcoord_target = [0., 5.E-03, 10.E-03, 15.E-03, 20.E-03, 25.E-03, 30.E-03, 35.E-03]; 
Mdata_target =  [0.,  112.956E-06,  600.312E-06,  1.22092E-03, 1.7666E-03, ...
                 2.41683E-03, 3.01616E-03, 3.44247E-03];               


    Xex = TableExtractor('Sdescription', 'Table Extractor for Unit test',...
                'Sworkingdirectory',SworkDir, ...
                'Sfile','resultsTable.txt', ....
                'Srelativepath','FilesToBeRead',...
                'Nheaderlines', 4, ...
                'Sdelimiter', ' ',...
                'Soutputname','displacement');
            
    Tout = extract(Xex);
    if all(Tout.displacement.Mcoord==Mcoord_target) & ...
        all(Tout.displacement.Mdata==Mdata_target)
            Vtest(itest)=true;
    else
       Cmess{itest} = 'Values not correctly read from file'; 
    end
    
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end 

if nargout>0
    % Export name of the UnitTest
    varargout{1}='TableExtractor';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of TableExtractor (' datestr(now) ')'])
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
