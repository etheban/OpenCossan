classdef UnitTestDataseries < matlab.unittest.TestCase
    % This is a unit test for the Dataseries Object
    %   This UnitTest should test all the interface of the Dataseries
    %   object.
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
    properties
        
    end
    
    
    methods (Test)
        function emptyObject(testCase)
            Xobj=Dataseries;
            display(Xobj);
            testCase.assertEqual(class(Xobj),'Dataseries');
        end
        
        function singleObject(testCase)
            myVdata=rand(1,10);
            myCoord=1:10;
            Xobj=Dataseries('Sdescription','myDescription','Mcoord',myCoord,'Vdata',myVdata,'Sindexname','myIndexName','Sindexunit','myIndexUnit');
            display(Xobj);
            
            %Check values
            testCase.verifyClass(Xobj,'Dataseries');
            testCase.assertEqual(Xobj.Vdata,myVdata);
            testCase.assertSubstring(Xobj.SindexName,'myIndexName');
            testCase.assertSubstring(Xobj.SindexUnit,'myIndexUnit');
            testCase.assertSubstring(Xobj.Sdescription,'myDescription');
            testCase.assertEqual(Xobj.Mcoord,myCoord);
        end
        
        function multiSamplesSingleObject(testCase)
            myCoord=1:10;
            Xobj=Dataseries('Sdescription','myDescription','Sindexname','myIndexName','Sindexunit','myIndexUnit');
            
            %Add samples
            myVdata2=rand(4,10);
            Xobj=Xobj.addData('Mdata',myVdata2);
            display(Xobj);
            
            %Check values
            testCase.verifyClass(Xobj,'Dataseries');
            testCase.assertEqual(Xobj(2,1).Vdata,myVdata2(2,:));
            testCase.assertEqual(Xobj(2,1).VdataLength,10);
            testCase.assertSubstring(Xobj(2,1).SindexName,'myIndexName');
            testCase.assertSubstring(Xobj(4,1).SindexUnit,'myIndexUnit');
            testCase.assertSubstring(Xobj(3,1).Sdescription,'myDescription');
            testCase.assertEqual(Xobj(2,1).Mcoord,myCoord);
            testCase.assertSize(Xobj(1).Vdata,size(myVdata2));
            testCase.assertEqual(Xobj(1).VdataLength,10);
            testCase.assertSubstring(Xobj(1).SindexName,'myIndexName');
            testCase.assertSubstring(Xobj(1).SindexUnit,'myIndexUnit');
            testCase.assertSubstring(Xobj(1).Sdescription,'myDescription');
            testCase.assertEqual(Xobj(1).Mcoord,myCoord);
        end
        
        function singleVectorObjects(testCase)
            myVdata1=rand(1,10);
            myVdata2=rand(1,20);
            myCoord1=1:10;
            myCoord2=1:20;
            Xobj1=Dataseries('Sdescription','myDescription1','Mcoord',myCoord1,'Vdata',myVdata1,'Sindexname','myIndexName1','Sindexunit','myIndexUnit1');
            Xobj2=Dataseries('Sdescription','myDescription2','Mcoord',myCoord2,'Vdata',myVdata2,'Sindexname','myIndexName2','Sindexunit','myIndexUnit2');
            
            Xobj=[Xobj1 Xobj2];
            display(Xobj);
            
            %Check values
            testCase.verifyClass(Xobj,'Dataseries');
            testCase.assertLength(Xobj,2);
            testCase.assertEqual(Xobj(1).Vdata,myVdata1);
            testCase.assertEqual(Xobj(2).Vdata,myVdata2);
            testCase.assertSubstring(Xobj(1,1).SindexName,'myIndexName1');
            testCase.assertSubstring(Xobj(1,2).SindexName,'myIndexName2');
            testCase.assertSubstring(Xobj(1,1).SindexUnit,'myIndexUnit1');
            testCase.assertSubstring(Xobj(1,2).SindexUnit,'myIndexUnit2');
            testCase.assertSubstring(Xobj(1,1).Sdescription,'myDescription1');
            testCase.assertSubstring(Xobj(1,2).Sdescription,'myDescription2');
            testCase.assertEqual(Xobj(1,1).Mcoord,myCoord1);
            testCase.assertEqual(Xobj(1,2).Mcoord,myCoord2);
            testCase.assertEqual(Xobj(1,1).Vdata,myVdata1);
            testCase.assertEqual(Xobj(1,2).Vdata,myVdata2);
        end
        
        function multisamplesVectorObjects(testCase)
            myVdata1=rand(8,10);
            myVdata2=rand(8,20);
            Xobj1=Dataseries('Sdescription','myDescription1','Sindexname','myIndexName1','Sindexunit','myIndexUnit1');
            Xobj2=Dataseries('Sdescription','myDescription2','Sindexname','myIndexName2','Sindexunit','myIndexUnit2');
            
            Xobj=[Xobj1 Xobj2];
            display(Xobj);
            % Add samples
            Xobj=Xobj.addData('Csamples',{myVdata1 myVdata2});
            
            %Check values
            testCase.verifyClass(Xobj,'Dataseries');
            testCase.assertEqual(Xobj(5,1).Vdata,myVdata1(5,:));
            testCase.assertEqual(Xobj(2,2).Vdata,myVdata2(2,:));
            testCase.assertEqual(Xobj(1).Vdata(3,4),myVdata1(3,4));
            testCase.assertEqual(Xobj(2).Vdata(2,1),myVdata2(2,1));
        end
        
        function catDataseries(testCase)
            myVdata1=rand(8,10);
            myVdata2=rand(8,20);
            myVdata3=rand(8,3);
            Xobj1=Dataseries('Sdescription','myDescription1','Mdata',myVdata1,'Sindexname','myIndexName1','Sindexunit','myIndexUnit1');
            Xobj2=Dataseries('Sdescription','myDescription2','Mdata',myVdata2,'Sindexname','myIndexName2','Sindexunit','myIndexUnit2');
            Xobj3=Dataseries('Sdescription','myDescription2','Mdata',myVdata3,'Sindexname','myIndexName2','Sindexunit','myIndexUnit2');
            
            Xobj123a=[Xobj1 Xobj2 Xobj3];
            display(Xobj123a);
            
            Xobj12=[Xobj1 Xobj2];
            Xobj123b=[Xobj12 Xobj3];
            display(Xobj123b);
            
            % Add samples
           
            %Check values
            testCase.assertEqual([Xobj123a.VdataLength],[10 20 3]);
            testCase.assertEqual([Xobj123b.VdataLength],[10 20 3]);
            testCase.assertEqual(Xobj123b(1).Vdata(3,4),myVdata1(3,4));
            testCase.assertEqual(Xobj123b(2).Vdata(2,1),myVdata2(2,1));
            testCase.assertEqual(Xobj123b(3).Vdata(2,1),myVdata3(2,1));
        end
        
        function addSamples(testCase)
            myVdata1=rand(8,10);
            myVdata2=rand(8,20);
            myVdata3=rand(8,3);
            Xobj1=Dataseries('Sdescription','myDescription1','Mdata',myVdata1,'Sindexname','myIndexName1','Sindexunit','myIndexUnit1');
            Xobj2=Dataseries('Sdescription','myDescription2','Mdata',myVdata2,'Sindexname','myIndexName2','Sindexunit','myIndexUnit2');
            Xobj3=Dataseries('Sdescription','myDescription2','Mdata',myVdata3,'Sindexname','myIndexName2','Sindexunit','myIndexUnit2');
            Xobj123=[Xobj1 Xobj2 Xobj3];
            
            myVdata4=rand(4,10);
            myVdata5=rand(4,20);
            myVdata6=rand(4,3);
            Xobj4=Dataseries('Sdescription','myDescription1','Mdata',myVdata4,'Sindexname','myIndexName1','Sindexunit','myIndexUnit1');
            Xobj5=Dataseries('Sdescription','myDescription2','Mdata',myVdata5,'Sindexname','myIndexName2','Sindexunit','myIndexUnit2');
            Xobj6=Dataseries('Sdescription','myDescription2','Mdata',myVdata6,'Sindexname','myIndexName2','Sindexunit','myIndexUnit2');
            Xobj123add=[Xobj4 Xobj5 Xobj6];
            
            % Merge samples
            Xobj123=Xobj123.addSamples('Xdataseries',Xobj123add);
            
            % Add samples
           
            %Check values
            testCase.assertEqual([Xobj123.Nsamples],[12 12 12]);
        end
        
        %         function mergeDataseries(testCase)
        %             myVdata1=rand(2,100);
        %             myVdata2=rand(2,20);
        %             myCoord1=1:100;
        %             myCoord2=1:20;
        %             Xobj1=Dataseries('Sdescription','myDescription1','Mcoord',myCoord1,'Sindexname','Vdata',myVdata1,'myIndexName1','Sindexunit','myIndexUnit1');
        %             Xobj2=Dataseries('Sdescription','myDescription2','Mcoord',myCoord2,'Sindexname','Vdata',myVdata2,'myIndexName2','Sindexunit','myIndexUnit2');
        %
        %             % Add samples
        %             Xobj=Xobj1.merge(Xobj2);
        %             testCase.assertEqual(Xobj.Nsamples,2);
        %
        %         end
        
        
    end
end

