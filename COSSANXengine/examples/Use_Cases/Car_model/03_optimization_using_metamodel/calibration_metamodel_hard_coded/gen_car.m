function Pout = gen_truss(Pinput)

%Pout=Pinput;

%path(path,'/home/lup/workspace2/COSSAN-X/examples/Tutorials/metamodel/fe_02-matlab')
%fe_02
j = 1; jj=0;
for i = 1: size(Pinput,1)
    while intersect(j,[11 13 15 24 30 31 ])==j
        j = j+1;jj=jj+1;
    end
    j
    eval(['load sim_' num2str(j)]);
    eval(['load mass_mat_0']);
    Pout(j-jj).mass = mass_mat_0;
    %Pout(i).stiff = stif_mat;
    % e.g. export for car only the mass of the nominal sytem
    % export the modes using a perl script !perl ...
    % add to Pout modes and lambda
    %[MPhi,Mlambda] = eigs(stif_mat,mass_mat,20,'sm');
    Pout(j-jj).MPhi = eigvc;
    Pout(j-jj).Vlambda = eigvl.^2;
%     Pout(j-jj).Vlambda
%     pause
    j=j+1;
end

% for the length of Pinput, e.g. 150 there should be created 150 @Xo_modes
%Xo=Xo_modes('mass',mass_mat,'stiffness',stif_mat,'Nmodes',5)

% ! : should here the constructor of the Xmodes be called?
% ! : where do I put the Xmodes? newstyle?

%Xmod = Xmodes('mass','mass_mat','stiffness','stif_mat','Nmodes',30,'add_mass','true');

%--> computes 30 modes
%--> stores (1) eigenvectors, (2) eigenvalues and the (3) mass matrix