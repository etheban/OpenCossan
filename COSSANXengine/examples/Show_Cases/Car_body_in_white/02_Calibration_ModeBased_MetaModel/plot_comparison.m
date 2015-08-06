function plot_comparison(Xoutput_fm,Xoutput_mm)

Nsamples_validation = length(Xoutput_mm);
Nmodes = size(Xoutput_mm(1).MPhi,2);
Vf = zeros(size(Xoutput_mm(1).MPhi,1),1);
Vf(231334) = 1.0;
DOF_obs = 61845;
modal_damping_ratio = 0.02;
Vfreqrange = (200:0.5:350)/2/pi;
Tfrf_mm = cell(Nsamples_validation,1);
Tfrf_fm = cell(Nsamples_validation,1);

for isim = 1:Nsamples_validation
    Xm_mm = Xoutput_mm(isim);
    Xm_mm.Vzeta = modal_damping_ratio*ones(Nmodes,1);
    Vforce = Xm_mm.MPhi'*Vf;
    Tfrf_mm{isim} = frf(Xm_mm,'Stype','vel','Vforce',Vforce','Vfreqrange',Vfreqrange,'Vdofs',DOF_obs);
    
    Xm_fm = Xoutput_fm(isim);
    Xm_fm.MPhi = Xm_fm.MPhi(:,1:Nmodes);
    Xm_fm.Vlambda = Xm_fm.Vlambda(1:Nmodes);
    Xm_fm.Vzeta = modal_damping_ratio*ones(Nmodes,1);
    Vforce = Xm_fm.MPhi'*Vf;
    Tfrf_fm{isim} = frf(Xm_fm,'Stype','vel','Vforce',Vforce','Vfreqrange',Vfreqrange,'Vdofs',DOF_obs);
    
    figure
    semilogy(2*pi*Vfreqrange,abs(Tfrf_fm{isim}.FRF_61845),'Linewidth',2); hold on
    semilogy(2*pi*Vfreqrange,abs(Tfrf_mm{isim}.FRF_61845),'Linewidth',2,'Color','r','Linestyle','--');
    xlabel('Excitation [rad/s]')
    ylabel('velocity FRF');
    title(['Comparison of validation sample no. ' num2str(isim)]);
    h1=gca; h2=get(gca,'XLabel'); h3=get(gca,'YLabel'); h4 = get(gca,'Title');
    set([h1 h2 h3 h4],'FontSize',16);
    grid on
    xlim([200 350])
    ylim([10^-3 10^0])
    legend('full model','meta-model');
end


return