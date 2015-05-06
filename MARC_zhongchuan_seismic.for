c	����MARC��UBEAM�ӳ������ֽ��������άģ�� (36����������4���ֽ�)
c	������MARC 2005R2+Visual Fortran 6.6B+
c	last revised: 2005.11.22(22:30)
c	by ½�������廪��ѧ��ľϵ, luxinzheng@sina.com
c	serial number: 00020A
c	Ӧ����ʦҪ��ɾȥ�������������� (20A)
c	�����޸��˸նȾ����㷨���ͻر��ָ���(20)
c	�����޸��˸նȼ��㲿�֣�����˾��ȣ�����debug�ļ��б�����ָ��ն�(18)
c	�����˻������������Ѳ��ִ��� (17)
c     �޸������߸նȾ�����㷨������������õ��նȣ�������ȶ��Ժ;��� (16)
c	�����˸ֽ���Ѳ��ֵĴ��� (16)
c	�汾13~15�����ڷ���ĳЩ������أ�����Ϊͨ�ò����ƹ�
c	�޸Ļ�����ж�غ��ټ��صĲ��ִ��� (12)
c	������������ά�ɶ�ά�����Ϊһά���� (11)
c	�޸�����ά����ʱ�Ĵ��� (10)
c	�����������ĳ�������ͻ������������֣�������������52������Ԫ
c	�����˸ֽ����ѵ��ģ�ͣ�ʹ�������ȷ
c	�޸�����ʾ�߶ȵ�������������������ϵ������ʵ�ʼ��롣
c	�޸��������Բ��λ�Ƶ����
c	ʹ��marc�е�ICODE��������Ԫ�л��ֵ�λ�úͳ���,���������������

c     ��plotv���������ж�truss��Ԫ״̬�ĳ���SQEEN��
c     ��uactive��������ɱ��truss��Ԫ�ĳ���SQEEN��

	module Typ_Fiber
		implicit none

		type :: Typ_Fiber_Concrete        ! �����������ά����
			real*8 :: Ac                  ! ��������ά�����
			real*8 :: x,y                 ! ��������ά���������ĵľ���
			real*8 :: fc                  ! ��������ֵ��ѹǿ��, ��
			real*8 :: eps0                ! ��������ֵѹӦ��, ��
			real*8 :: fu                  ! ���������޿�ѹǿ��, ��
			real*8 :: epsu                ! ���������޿�ѹӦ��, ��
!----------------------
			real*8 :: ft                  ! ���������޿�ѹǿ��, ��
			real*8 :: epsut               ! ���������޿�ѹӦ��, ��		
!----------------------
			real*8 :: e0                  ! ��������ѹ����ģ��
			real*8 :: sigc                ! ��������ά��ǰӦ��
			real*8 :: epsc                ! ��������ά��ǰӦ��
			real*8 :: epsmax              ! ���������������ѹӦ��
!----------------------
			real*8 :: epsmax_t            ! �����������������Ӧ��
!----------------------
			real*8 :: sigc_pre            ! ��������άǰ��Ӧ��
			real*8 :: epsc_pre            ! ��������άǰ��Ӧ��
			real*8 :: epsmax_pre          ! ������������ǰ�����ѹӦ��
!----------------------
			real*8 :: epsmax_t_pre        ! ������������ǰ�������Ӧ��
!-----------------------
			real*8 :: dEPS
			real*8 :: Ect                 ! ��������ά���߸ն�
		end type Typ_Fiber_Concrete

		type :: typ_Fiber_Steel
			real*8 :: as                  ! �ֽ���ά�����
			real*8 :: x,y                 ! �ֽ���ά���������ĵľ���
			real*8 :: fy,SIGP0            ! �ֽ���ά������Ӧ������ʼԤ��Ӧ�����Ǹ���
			real*8 :: es                  ! �ֽ���ά�ĵ���ģ��
			real*8 :: sigs_pre            ! �ϴ���������ʱ�ֽ��Ӧ��
			real*8 :: epss_pre            ! �ϴ���������ʱ�ֽ��Ӧ��
			real*8 :: sigs,epss           ! �ֽ���ά��ǰ��Ӧ����Ӧ��
			real*8 :: epssmax_P           ! �ֽ�������Ӧ�䣬��
              real*8 :: epssmax_P_pre       ! �ֽ����ǰ�����Ӧ�䣬�� 
			real*8 :: epssmax_N           ! �ֽ������СӦ�䣬��
              real*8 :: epssmax_N_pre       ! �ֽ����ǰ����СӦ�䣬�� 
			real*8 :: K1,K2,K3            ! Ӳ����㡢��ֵ�㡢�ƻ���Ӧ��������Ӧ��ı�ֵ 
			real*8 :: K4                  ! ��ֵ��Ӧ��������Ӧ���ı�ֵ 
			real*8 :: P4                  ! ����ѹ����Ӧ����ֵ��ȡ��ֱ�� 
              real*8 :: S_UNLD,E_UNLD       ! �ֽ���ά���ټ���תΪж��ʱ��ת�۵�Ӧ����Ӧ�� 
              real*8 :: S_RELD,E_RELD       ! �ֽ���ά��ж��תΪ�ټ���ʱ��ת�۵�Ӧ����Ӧ��
              real*8 :: SSRL3,ESRL3         ! �ֽ���ά�ټ������������߽���Ӧ����Ӧ��
			real*8 :: EST,sigs_pre1       ! �ֽ���ά���ߵ���ģ�������ϴ���������ʱ�ֽ��Ӧ��         
			real*8 :: deps,deps_pre       ! ��ǰӦ��������ǰһ���ز���Ӧ��Ӧ������         
			integer:: FF,PRF              ! �ƻ���ǡ������������     ��ֵΪ1ʱ���ƻ�������������
		end type typ_Fiber_Steel

		contains

		subroutine Fiber_Steel_Force(F_S,dEPS)                                ! ����ֽ���ά����
			type (Typ_Fiber_Steel) :: F_S                 
			real*8 :: dEPS,x                                                  ! ��ǰӦ����������Ӧ��
			real*8 :: s                                                       ! ����ѹ�ֽ��ЧΪ�����ֽ����ʱ�ı任����
			real*8 :: y
			x=F_s%epss_pre+dEPS                                               ! ��ǰ��Ӧ�� 
              if(F_s%sigs_pre==0.)then
			   s=(F_s%sigs_pre+sign(1.d0,deps)) 
	1		      /abs(F_s%sigs_pre+sign(1.d0,deps))                          ! ��F_s%sigs_pre=0.ʱs��ȡֵ
              else
	           s=F_s%sigs_pre/abs(F_s%sigs_pre)                               ! ��F_s%sigs_pre������0.ʱs��ȡֵ
	        end if
			F_S%K1=F_S%K1*(F_S%P4**((ABS(s)-s)/(2.*abs(s))))                  ! ΪԤӦ����� 
              F_S%K2=F_S%K2*(F_S%P4**((ABS(s)-s)/(2.*abs(s))))                  ! ΪԤӦ�����
              F_S%K3=F_S%K3*(F_S%P4**((ABS(s)-s)/(2.*abs(s))))                  ! ΪԤӦ�����
              F_S%fy=F_S%fy/(F_S%P4**((ABS(s)-s)/(2.*abs(s))))                  ! ΪԤӦ�����
	        y=F_S%fy/F_S%es
			F_S%Est=F_S%es									                  ! �������߸նȳ�ֵ�������ٸ�
              F_S%deps=deps
			if(F_S%FF==1)then
	           F_S%sigs=0.
	           F_S%EST=0.001*F_S%ES
	        else if(abs(x)>=F_S%K3*y)then
	           F_S%sigs=0.
                 F_S%FF=1
	           F_S%EST=0.001*F_S%ES
			else
                 if(abs(F_s%epss_pre)>y.AND.F_s%PRF==0)then
	              if(abs(x)<abs(F_s%epss_pre))F_S%PRF=1
                 end if
	           if(F_s%PRF==0)then                                             ! �ֽ���ά��������
				  call steel_mono(F_S,s,x,deps)
			   else                                                           ! �ֽ���ά��������
	              call steel_recy(F_S,s,x,deps)
	           end if
			end if
              F_S%K1=F_S%K1/(F_S%P4**((ABS(s)-s)/(2.*abs(s))))
              F_S%K2=F_S%K2/(F_S%P4**((ABS(s)-s)/(2.*abs(s))))
              F_S%K3=F_S%K3/(F_S%P4**((ABS(s)-s)/(2.*abs(s))))
              F_S%fy=F_S%fy*(F_S%P4**((ABS(s)-s)/(2.*abs(s))))
			return
		end subroutine Fiber_Steel_Force	     
			
		subroutine steel_mono(F_S,s,x,deps)	                                  ! �ֽ���ά��������
			type (Typ_Fiber_Steel) :: F_S                 
			real*8 :: a
			real*8 :: x,y,s,deps
			y=F_S%fy/F_S%es
              if(abs(x)<y)then
	           F_S%sigs=F_S%sigs_pre+F_S%es*deps
	           F_S%EST=F_S%ES
              else if(y<=abs(x).and.abs(x)<y*F_S%K1)then
			   F_S%sigs=F_S%fy*s
	           F_S%EST=0.001*F_S%ES
			else 
			   a=(1.-F_S%K4)*F_S%es/((F_S%K2-F_S%K1)**2*y)
	           F_S%sigs=a*(abs(x)-F_S%K2*y)**2+F_S%K4*F_S%fy
	           if(F_S%sigs<0.)then
			      F_S%sigs=0.
                    F_S%FF=1			      
			   end if
			   F_S%sigs=F_S%sigs*s
			   F_S%EST=2.*a*(abs(x)-F_S%K2*y)
	           if(F_S%sigs==0.)F_S%EST=0.001*F_S%ES
			end if
	        return
	    end subroutine steel_mono     
			        
		subroutine steel_recy(F_S,s,x,deps)	                                  ! �ֽ���ά��������
			type (Typ_Fiber_Steel) :: F_S                 
			real*8 :: K2
			real*8 :: K                                                       ! ��ЧӲ��ֱ��б��
			real*8 :: x,s,dEPS
              K=(F_S%K4-1.)*F_S%es/(F_S%K2-1.)
			if(F_s%sigs_pre*deps<0.)then                                           
                call steel_ULD(F_S,s,x,deps,K)                                  ! �ֽ���άж��
			ELSE                                                             
                CALL steel_RLD(F_S,s,x,deps,K)                                  ! �ֽ���ά�ټ���
              END IF
			return
	    end subroutine steel_recy     

		subroutine steel_ULD(F_S,s,x,deps,K)	                              ! �ֽ���άж��
			type (Typ_Fiber_Steel) :: F_S                 
			real*8 :: dEPS,sigs
			real*8 :: x,y,s,k
			y=F_S%fy/F_S%es
			if(F_s%dEPS*F_s%dEPS_pre<0.)then
	           F_s%E_UNLD=F_s%epss_pre                                        ! ж�����Ӧ��
	           F_s%S_UNLD=F_s%sigs_pre                                        ! ж�����Ӧ��
			end if
			F_S%sigs=F_S%sigs_pre+F_S%es*deps
              F_S%EST=F_S%ES
			return
	    end subroutine steel_ULD    

		subroutine steel_RLD(F_S,s,x,deps,K)	                              ! �ֽ���ά�ټ���
			type (Typ_Fiber_Steel) :: F_S                 
			real*8 :: dEPS,sigs
			real*8 :: x,k,s,x0,aa,bb,cc
			real*8 :: p,p1,epssmax_pre
			if(F_S%sigs_pre*F_S%sigs_pre1<0..OR.F_s%dEPS*F_s%dEPS_pre<0.)then
	           F_s%E_RELD=F_s%epss_pre*s                                      ! �ټ������Ӧ��
	           F_s%S_RELD=F_s%sigs_pre*s                                      ! �ټ������Ӧ��	           
			   IF(F_S%sigs_pre>0.)THEN			   
			      epssmax_pre=F_S%epssmax_P_pre
			   ELSE
			      epssmax_pre=F_S%epssmax_N_pre
			   END IF
                 call steel_mono(F_S,s,epssmax_pre,deps)
	           F_s%SSRL3=F_S%sigs*s                                           ! �ټ������������߽���Ӧ��	           
			   F_s%ESRL3=epssmax_pre*S                                        ! �ټ������������߽���Ӧ��
			end if
              if(x*s<=F_s%ESRL3)then
			   p1=F_S%es*(F_s%ESRL3-F_s%E_RELD)
			   IF(p1<=F_s%SSRL3-F_s%S_RELD)THEN
			      sigs=((F_s%SSRL3-F_s%S_RELD)/(F_s%ESRL3-F_s%E_RELD))
				  F_S%sigs=min(SIGS*(x*s-F_s%E_RELD)+F_s%S_RELD,F_s%SSRL3)
			      F_S%EST=SIGS
			   else
			      p=p1*(1.-K/F_S%es)/(p1-(F_s%SSRL3-F_s%S_RELD))
			      sigs=((x*s-F_s%E_RELD)/(F_s%ESRL3-F_s%E_RELD))
	1		           **p*(p1-(F_s%SSRL3-F_s%S_RELD))
                    F_S%sigs=F_S%es*(x*s-F_s%E_RELD)+F_s%S_RELD-sigs
			      F_S%EST=F_S%es-(P1-(F_s%SSRL3-F_s%S_RELD))
     1                   *p*((x*s-F_s%E_RELD)/(F_s%ESRL3-F_s%E_RELD))
     2                   **(p-1.)/(F_s%ESRL3-F_s%E_RELD)
			   end if
			ELSE   
                 call steel_mono(F_S,s,x,deps)
		       F_S%sigs=F_S%sigs*s
              end if
		    F_S%sigs=F_S%sigs*s
			return
	    end subroutine steel_RLD    
			

		subroutine Fiber_Concrete_Force(F_C, dEPS)              ! �����������ά����
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: dEPS	                                    ! Ӧ��
			real*8 :: EPS, x
			F_C%Ect=F_C%e0                                      ! �������߸նȳ�ֵ�������ٸ�
			EPS=F_C%epsc_pre+dEPS                               ! ���ز�����ʱ��Ӧ��
			if(EPS>=0. ) then 
				call Fiber_Concrete_Tension(F_C,   dEPS)          ! ��������ģ��
			else
				if( EPS<F_C%epsmax_pre) then                    ! ������ʷ���ѹӦ�䣬�Ǽ��߼���
					call Fiber_Concrete_Load(F_C,  EPS)
				else                                            ! ��������ʷ���ѹӦ�䣬������ж�ػ����ټ���
					if (dEPS>0.) then                           ! ж��
						call Fiber_Concrete_UNLoad(F_C,  EPS)
					else                                        ! �ټ���
						call Fiber_Concrete_ReLoad(F_C,  EPS)
					end if
				end if
			end if
			return
		end subroutine Fiber_Concrete_Force

		subroutine Fiber_Concrete_UNLoad(F_C, EPS)
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: EPS                                       ! Ӧ��
			real*8 :: x,E, ESOFT
			x=F_C%epsmax_pre/F_C%eps0
			if(x<1.) then                                       ! δ�ﵽ��ֵѹӦ��
				E=F_C%fc*(2.*x-x**2)/F_C%epsmax_pre
				F_C%sigc=E*EPS; 
				F_C%Ect=E
			else                                                ! ����������ֵѹӦ��
				ESOFT=(F_C%fu-F_C%fc)/(F_C%epsu-F_C%eps0)
				F_C%sigc=min(F_C%fc+ESOFT*(F_C%epsmax_pre-F_C%eps0),0.d0)
				E=F_C%sigc/F_C%epsmax_pre
				F_C%sigc=E*EPS; 
				F_C%Ect=E
			end if			
			if(F_C%sigc>0.) then								! ��������ά��ѹ��				
				F_C%sigc=0. 
				F_C%Ect=0.0001*F_C%E0
			end if
			return
		end subroutine Fiber_Concrete_UNLoad

		subroutine Fiber_Concrete_ReLoad(F_C, EPS)              ! ��������ά�ټ���
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: EPS                                       ! Ӧ��
			real*8 :: x,E, ESOFT
			x=F_C%epsmax_pre/F_C%eps0
			if(x<1.) then                                       ! δ�ﵽ��ֵѹӦ��
				E=F_C%fc*(2.*x-x**2)/F_C%epsmax_pre
				F_C%sigc=E*EPS; 
				F_C%Ect=E
			else                                                ! ����������ֵѹӦ��
				ESOFT=(F_C%fu-F_C%fc)/(F_C%epsu-F_C%eps0)
				F_C%sigc=min(F_C%fc+ESOFT*(F_C%epsmax_pre-F_C%eps0),0.d0)
				E=F_C%sigc/F_C%epsmax_pre
				F_C%sigc=E*EPS; 
				F_C%Ect=E
			end if			
			if(F_C%sigc>0.) then			                    ! ��������ά��ѹ��				
				F_C%sigc=0. 
				F_C%Ect=0.0001*F_C%E0
			end if

			return
		end subroutine Fiber_Concrete_ReLoad

		subroutine Fiber_Concrete_Load(F_C, EPS)                ! ��������ά�Ǽ��߼���
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: EPS                                       ! Ӧ��
			real*8 :: x, ESOFT
			x=(EPS)/F_C%eps0
			if(x<1..and.x>=0.) then                             ! δ�ﵽ��ֵѹӦ��
				F_C%sigc=F_C%fc*(2.*x-x**2)
				F_C%Ect=F_C%fc*(2.-2.*x)/F_C%eps0
			else                                                ! ������ֵѹӦ��
				ESOFT=(F_C%fu-F_C%fc)/(F_C%epsu-F_C%eps0)
				F_C%sigc=F_C%fc+ESOFT*(x-1)*F_C%eps0
				F_C%Ect=ESOFT
			end if
			if(F_C%sigc>0.) then			                    ! ��������ά��ѹ��				
					F_C%sigc=0. 
					F_C%Ect=0.0001*F_C%E0
			end if
			F_C%epsmax=	EPS                                     ! ��¼���ѹӦ��

			return
		end subroutine Fiber_Concrete_Load

		subroutine Fiber_Concrete_Tension(F_C, dEPS)            ! ��������ά����
			type (Typ_Fiber_Concrete) :: F_C
			real*8 ::  dEPS,EPS                                 ! ������Ӧ�䣬Ӧ������
			EPS=F_C%epsc_pre+dEPS
			if( EPS>F_C%epsmax_t_pre) then                      ! ������ʷ�����Ӧ�䣬�Ǽ��߼���
					call Fiber_Concrete_Load_T(F_C,  EPS)
				else                                            ! ��������ʷ�����Ӧ�䣬������ж�ػ����ټ���
					if (dEPS<0.) then                           ! ж��
						call Fiber_Concrete_UNLoad_T(F_C,  EPS)
					else                                        ! �ټ���
						call Fiber_Concrete_ReLoad_T(F_C,  EPS)
					end if
				end if
			return
		end subroutine Fiber_Concrete_Tension

         subroutine Fiber_Concrete_UNLoad_T(F_C, EPS)
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: EPS                                       ! Ӧ��
			real*8 :: x,E,ESOFT,epst0
              epst0=F_C%ft/F_C%e0
			x=F_C%epsmax_t_pre/epst0
			if(x<1.) then                                       ! δ�ﵽ��ֵ��Ӧ��
				F_C%sigc=F_C%e0*EPS; 
				F_C%Ect=F_C%e0
			else                                                ! ����������ֵ��Ӧ��
				ESOFT=-F_C%ft/(F_C%epsut-epst0)
				F_C%sigc=max(F_C%ft+ESOFT*(F_C%epsmax_t_pre-epst0),0.d0)
				E=F_C%sigc/F_C%epsmax_t_pre
				F_C%sigc=E*EPS; 
				F_C%Ect=E
			end if			
			if(F_C%sigc<0.) then								! ��������ά������				
				F_C%sigc=0. 
				F_C%Ect=F_C%e0*0.0001
			end if
			return
		end subroutine Fiber_Concrete_UNLoad_T

		subroutine Fiber_Concrete_ReLoad_T(F_C, EPS)            ! ��������ά�ټ���
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: EPS                                       ! Ӧ��
			real*8 :: x,E, ESOFT,epst0
	        epst0=F_C%ft/F_C%e0
			x=F_C%epsmax_t_pre/epst0
			if(x<1.) then                                       ! δ�ﵽ��ֵ��Ӧ��
				F_C%sigc=F_C%e0*EPS; 
				F_C%Ect=F_C%e0
			else                                                ! ����������ֵ��Ӧ��
				ESOFT=-F_C%ft/(F_C%epsut-epst0)
				F_C%sigc=max(F_C%ft+ESOFT*(F_C%epsmax_t_pre-epst0),0.d0)
				E=F_C%sigc/F_C%epsmax_t_pre
				F_C%sigc=E*EPS; 
				F_C%Ect=E
			end if			
			if(F_C%sigc<0.) then			                    ! ��������ά������				
				F_C%sigc=0. 
				F_C%Ect=F_C%e0*0.0001
			end if
			return
		end subroutine Fiber_Concrete_ReLoad_T

		subroutine Fiber_Concrete_Load_T(F_C, EPS)                ! ��������ά�Ǽ��߼���
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: EPS                                       ! Ӧ��
			real*8 :: x, ESOFT, epst0
			epst0=F_C%ft/F_C%e0
			x= EPS/epst0
			if(x<1..and.x>=0.) then                             ! δ�ﵽ��ֵ��Ӧ��
				F_C%sigc=F_C%e0*EPS
				F_C%Ect=F_C%e0
			else                                                ! ������ֵ��Ӧ��
				ESOFT=-F_C%ft/(F_C%epsut-epst0)
				F_C%sigc=F_C%ft+ESOFT*(x-1)*epst0
				F_C%Ect=ESOFT
			end if
			if(F_C%sigc<0.) then			                    ! ��������ά������				
				F_C%sigc=0. 
				F_C%Ect=F_C%e0*0.0001
			end if
			F_C%epsmax_t=	EPS                                 ! ��¼�����Ӧ��
			return
		end subroutine Fiber_Concrete_Load_T


		subroutine Mid_Euler_F_C(F_C, N0, EPS0, dEPS0 )         !   ���ֻ�������ά
			type(Typ_Fiber_Concrete) :: F_C
			real*8 :: N0, EPS0, dEPS0
			real*8 :: dEPS1  
			dEPS1=EPS0+dEPS0-F_C%epsc_pre                       ! �õ���ǰӦ������
			F_C%dEPS=dEPS1
			call Fiber_Concrete_Force(F_C,dEPS1)                ! �����������άӦ��
			N0=F_C%sigc*F_C%ac                                  ! �õ���ά����
			F_C%epsc=EPS0+dEPS0                                 ! ����Ӧ��
			F_C%epsmax=min(F_C%epsmax_pre,EPS0+dEPS0)
			F_C%epsmax_t=max(F_C%epsmax_t_pre,EPS0+dEPS0)

			return
		end subroutine Mid_Euler_F_C

		subroutine Mid_Euler_F_S(F_S, N0, EPS0, dEPS0 )                       ! ���ָֽ���ά
			type(Typ_Fiber_Steel) :: F_S
			real*8 :: N0, EPS0, dEPS0, dEPS1
			INTEGER::INC
			dEPS1=EPS0+dEPS0-F_S%epss_pre                                     ! �õ���ǰӦ������
			call Fiber_Steel_Force(F_S, dEPS1 )                               ! ����ֽ���άӦ��
			F_S%epss=EPS0+dEPS0                                               ! ���¸ֽ�Ӧ��
			N0=F_S%sigs*F_S%as                                                ! �õ���ά����
			F_S%epssmax_P=MAX(F_s%epssmax_P_pre,EPS0+dEPS0)                   ! ����������ֽ������ʷӦ��
			F_S%epssmax_N=MIN(F_s%epssmax_N_pre,EPS0+dEPS0)                   ! ���¸�����ֽ������ʷӦ��	           
			return
		end subroutine Mid_Euler_F_S
				
	end module Typ_Fiber

	module Typ_Section
		use Typ_Fiber
		implicit none
		type :: Typ_RC_Rect01                                   ! ������ν��� 
			type(Typ_Fiber_Concrete) :: F_C(36)                 ! һ����36����������ά
			type(Typ_Fiber_Steel)    :: F_S(12)                 ! һ����12���ֽ���ά
			real*8 :: F(3), E(3), dE(3)                         ! ����أ�Ӧ�䣬Ӧ������
			real*8 :: Reaction(3)                               ! ���淴��
			real*8 :: E0, b,H                                   ! ����������������ĵ����������
			real*8 :: D(3,3)                                    ! �������߸ն�
			integer :: yield									! ��ʶ����
			integer :: failure

			real(8) :: Elem_Energy								! ������������%%%

		end type Typ_RC_Rect01

	contains

		subroutine Rect01_Iteration(Rect01)                     ! �Խ���������
			type( Typ_RC_Rect01 ):: Rect01
			call Rect01_Integrate_Fiber(Rect01)                 ! ��dmin�����Ӧ�����������
			
			
			return
		end subroutine

		subroutine Rect01_Integrate_Fiber(Rect01)               ! ������ά��������
			type( Typ_RC_Rect01 ):: Rect01
			integer :: I,J
			real*8  :: COSA, SINA
			real*8  :: x0, y0
			real*8  :: N, EPS, dEPS

			x0=0.; y0=0.;                 ! ��������λ��
			Rect01%Reaction=0.
			Rect01%D=0.

			Rect01.Elem_Energy=0.        ! �����ܹ���


			do I=1, size(Rect01%F_C)
				N=Rect01%F_C(I)%sigc*Rect01%F_C(I)%ac
				EPS=(Rect01%F_C(I)%y-y0)*Rect01%E(2)+
	1    			(-Rect01%F_C(I)%x+x0)*Rect01%E(3)+Rect01%E(1)
				dEPS=(Rect01%F_C(I)%y-y0)*Rect01%dE(2)+
	1    			(-Rect01%F_C(I)%x+x0)*Rect01%dE(3)+Rect01%dE(1)
				call  Mid_Euler_F_C(Rect01%F_C(I), N, EPS, dEPS) !  ���������ά����
				Rect01%Reaction(1)=Rect01%Reaction(1)+N;
				Rect01%Reaction(2)=Rect01%Reaction(2)+
	1						N*Rect01%F_C(I)%y;
				Rect01%Reaction(3)=Rect01%Reaction(3)-
	1						N*Rect01%F_C(I)%x;

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		    Rect01.Elem_Energy=Rect01.Elem_Energy+
	1		0.5*Rect01%F_C(I)%sigc*Rect01%F_C(I)%epsc*Rect01%F_C(I)%ac	    ! ����������Ӧ����%%%
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
				 if(Rect01%F_C(I)%Ect<Rect01%F_C(I)%E0*0.05) then
					Rect01%F_C(I)%Ect=Rect01%F_C(I)%E0*0.05
				 end if
			     Rect01%D(1,1)=Rect01%D(1,1)+Rect01%F_C(I)%Ect
	1				 *Rect01%F_C(I)%Ac
		         Rect01%D(2,2)=Rect01%D(2,2)+Rect01%F_C(I)%Ect
	1				 *Rect01%F_C(I)%Ac*Rect01%F_C(I)%y**2
		         Rect01%D(3,3)=Rect01%D(3,3)+Rect01%F_C(I)%Ect
	1				 *Rect01%F_C(I)%Ac*Rect01%F_C(I)%x**2
		         Rect01%D(1,2)=Rect01%D(1,2)+Rect01%F_C(I)%Ect
	1				 *Rect01%F_C(I)%Ac*Rect01%F_C(I)%Y
		         Rect01%D(1,3)=Rect01%D(1,3)-Rect01%F_C(I)%Ect
	1				 *Rect01%F_C(I)%Ac*Rect01%F_C(I)%X
		         Rect01%D(2,3)=Rect01%D(2,3)-Rect01%F_C(I)%Ect
	1				 *Rect01%F_C(I)%Ac*Rect01%F_C(I)%x*Rect01%F_C(I)%y
			end do


			do I=1, size(Rect01%F_S)
				N=Rect01%F_S(I)%sigs*Rect01%F_S(I)%as
				EPS=(Rect01%F_S(I)%y-y0)*Rect01%E(2)+
	1    			(-Rect01%F_S(I)%x+x0)*Rect01%E(3)+Rect01%E(1)
				dEPS=(Rect01%F_S(I)%y-y0)*Rect01%dE(2)+
	1    			(-Rect01%F_S(I)%x+x0)*Rect01%dE(3)+Rect01%dE(1)
				call  Mid_Euler_F_S(Rect01%F_S(I), N, EPS, dEPS) !  ��ֽ���ά����
				Rect01%Reaction(1)=Rect01%Reaction(1)+N;
				Rect01%Reaction(2)=Rect01%Reaction(2)+
	1					N*Rect01%F_S(I)%y;
				Rect01%Reaction(3)=Rect01%Reaction(3)-
	1					N*Rect01%F_S(I)%x;

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		Rect01.Elem_Energy=Rect01.Elem_Energy+0.5*Rect01%F_S(I)%sigs*	
	1		Rect01%F_S(I)%sigs*Rect01%F_S(I)%as/Rect01%F_S(I)%Es					! �ֽ��Ӧ����%%%
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

			     Rect01%D(1,1)=Rect01%D(1,1)+Rect01%F_S(I)%Est
	1				 *Rect01%F_S(I)%As
		         Rect01%D(2,2)=Rect01%D(2,2)+Rect01%F_S(I)%Est
	1				 *Rect01%F_S(I)%As*Rect01%F_S(I)%y**2
		         Rect01%D(3,3)=Rect01%D(3,3)+Rect01%F_S(I)%Est
	1				 *Rect01%F_S(I)%As*Rect01%F_S(I)%x**2
		         Rect01%D(1,2)=Rect01%D(1,2)+Rect01%F_S(I)%Est
	1				 *Rect01%F_S(I)%As*Rect01%F_S(I)%Y
		         Rect01%D(1,3)=Rect01%D(1,3)-Rect01%F_S(I)%Est
	1				 *Rect01%F_S(I)%As*Rect01%F_S(I)%X
		         Rect01%D(2,3)=Rect01%D(2,3)-Rect01%F_S(I)%Est
	1				 *Rect01%F_S(I)%As*Rect01%F_S(I)%x*Rect01%F_S(I)%y
			end do
			Rect01%D(2,1)=Rect01%D(1,2); Rect01%D(3,1)=Rect01%D(1,3)
			Rect01%D(3,2)=Rect01%D(2,3)

			return
		end subroutine Rect01_Integrate_Fiber
		
		subroutine Rect01_Initial (Rect01,Rect00) ! ��ʼ��������ά
			type( Typ_RC_Rect01 ):: Rect01,Rect00
			Rect01=Rect00
			return
		end subroutine Rect01_Initial
			
	end module Typ_Section


	module My_Var
		use Typ_Section
		real*8 :: time, dtime                   ! ��ǰ����ʱ�̣���ǰ����ʱ������
		integer :: FirstStep                    ! �ж��Ƿ��ǵ�һ��
		integer :: Current_Int_ElemNo           ! ��ǰ�ڲ���Ԫ��
	    integer :: Current_Glb_ElemNo           ! ��ǰ���嵥Ԫ����
		integer :: MatNumber(0:80)              ! ��Ų��ϱ��
		type(Typ_RC_Rect01 ):: Column_Mat(80)   ! ��Ŵ����Ե�Ԫ������Ϣ
		type(Typ_RC_Rect01 ):: Column0(10000,3)	! ���顣������еĵ�Ԫ
		type(Typ_RC_Rect01 ):: Column1(size(Column0,1),size(Column0,2)) ! ��������

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c		real(8) :: G_de1(50000,3),G_f1(50000,3),G_df1(50000,3)    
c		real(8) :: G_de2(50000,3),G_f2(50000,3),G_df2(50000,3)
c		real(8) :: G_de3(50000,3),G_f3(50000,3),G_df3(50000,3)
		integer:: inf(50000) 	      !���ڴ��ubeam��Ԫ�ı�ţ� ��ubeam��Ԫ�ı�Ŷ�Ӧinf()����ֵһ��Ϊ0
          integer:: trussfailure1(50000),trussfailure4(50000)
          integer:: this(50000)          !�����ж���truss��Ԫ����ubeam��Ԫ
c		dimension G_nn(50000,3),E_nn(50000,3)  

c		dimension denergy(50000), energy(50000), energy_pre(50000)
c		dimension dene_E(50000), ene_E(50000)
c		dimension ene_P(50000)      

c		dimension G_fcol_storey(24),E_fcol_storey(24),P_fcol_storey(24)
c		dimension G_fbeam_storey(24),E_fbeam_storey(24),
c	1		      P_fbeam_storey(24),Genergy_T(24)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


	end module My_Var

	subroutine Column_A(d,df,etot,de,gs,ngens,m1,nnn,cptim,timinc,mats)
		use My_Var
		implicit none
		integer :: ngens
		real*8 :: d(ngens,ngens),df(ngens),etot(ngens),de(ngens),gs(ngens)
		real*8 :: cptim, timinc
		integer :: m1, nnn,mats
		integer :: m
		integer :: I , J,ii,jj
c		m=m1
ccccccccccccccccccc�����Ԫ������ң���ô������һ�ο��԰Ѵ洢 ubeam��Ԫ������ ������Сccccccccccccccccc
		if(m1>Current_Glb_ElemNo) then                                    ! �µ�Ԫ��ʼ����
			Current_Glb_ElemNo=m1
			Current_Int_ElemNo=Current_Int_ElemNo+1
		end if
		if(m1<Current_Glb_ElemNo) then                                    ! �������¿�ʼ
			Current_Glb_ElemNo=m1
			Current_Int_ElemNo=1
		end if
		m=Current_Int_ElemNo
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


		inf(m1)=m	 ! ��¼��Ԫm1����Ϣ������������ubeam��Ԫ�еı�ţ�����inf(m1) ����

		if(cptim==0..and.timinc==0.) then                                 ! ��ʼ��
			call Initial_ALL(cptim, timinc,m, nnn,mats)
		end if
		if(cptim>time) then                                               ! �µĺ��ز���ʼ
			call New_Step (d,df,etot,de,gs,ngens,m,nnn,cptim,timinc)
		end if
		if(cptim==time.and. timinc< dtime.and.cptim.ne.0) then            ! �������ز��۰�
			call Cut_Step (d,df,etot,de,gs,ngens,m,nnn,cptim,timinc)
		end if
	
		d=0.;  ! �����ʼ�ն�
		d(1,1)=Column1(m,nnn)%E0*Column1(m,nnn)%b*Column1(m,nnn)%H
		d(2,2)=Column1(m,nnn)%E0*Column1(m,nnn)%b*Column1(m,nnn)%H**3/12.
		d(3,3)=Column1(m,nnn)%E0*Column1(m,nnn)%b**3*Column1(m,nnn)%H/12.
		d(4,4)=d(2,2)+d(3,3)

		Column1(m,nnn)%E=etot(1:3)
		Column1(m,nnn)%dE=de(1:3)
		Column1(m,nnn)%F=gs(1:3)
		call Rect01_Iteration(Column1(m,nnn))

		do I=1,3
			df(I)=Column1(m,nnn)%Reaction(I)-Column1(m,nnn)%F(I)
		end do
		d(1:3,1:3)=Column1(m,nnn)%D
		df(4)=d(4,4)*de(4)    !!!!!!!!
		gs=gs+df

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		!��ÿһ���������������뾲̬����,����plotv�ӳ����е��� %%%%
c		G_de1(m,nnn)=de(1)
c		G_f1(m,nnn)=gs(1)
c		G_df1(m,nnn)=df(1)
c		G_de2(m,nnn)=de(2)
c		G_f2(m,nnn)=gs(2)
c		G_df2(m,nnn)=df(2)
          ! �����˵�����������������Σ����԰����᷽��Ҳ���ǽ�����������άģ�͡�
c		G_de3(m,nnn)=de(3)
c		G_f3(m,nnn)=gs(3)
c		G_df3(m,nnn)=df(3)

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


		return
	end subroutine

	subroutine Cut_Step(d,df,etot,de,gs,ngens,m,nnn,cptim,timinc)
		use My_Var
		implicit none
		integer :: ngens
		real*8 :: d(ngens,ngens),df(ngens),etot(ngens),de(ngens),gs(ngens)
		real*8 :: cptim, timinc
		integer :: m, nnn
		dtime=timinc;
		Column1=Column0
		return
	end subroutine Cut_Step
	
	subroutine New_Step(d,df,etot,de,gs,ngens,m,nnn,cptim,timinc)
		use My_Var
		implicit none
		integer :: ngens
		real*8 :: d(ngens,ngens),df(ngens),etot(ngens),de(ngens),gs(ngens)
		real*8 :: cptim, timinc
		integer :: m, nnn, I,J,K,L
		real*8 :: x
		time=cptim; dtime=timinc;
		do I=1, size(Column1,1)
			do J=1,3

				Column1(I,J)%F_S(:)%epss_pre=Column1(I,J)%F_S(:)%epss             ! �����ڲ�����
				Column1(I,J)%F_S(:)%sigs_pre1=Column1(I,J)%F_S(:)%sigs_pre        ! �����ڲ�����
				Column1(I,J)%F_S(:)%sigs_pre=Column1(I,J)%F_S(:)%sigs             ! �����ڲ�����
				Column1(I,J)%F_S(:)%epssmax_P_pre=Column1(I,J)%F_S(:)%epssmax_P   ! �����ڲ�����
				Column1(I,J)%F_S(:)%epssmax_N_pre=Column1(I,J)%F_S(:)%epssmax_N   ! �����ڲ�����
				Column1(I,J)%F_S(:)%deps_pre=Column1(I,J)%F_S(:)%deps             ! �����ڲ�����


				Column1(I,J)%F_C(:)%epsc_pre=Column1(I,J)%F_C(:)%epsc       ! �����ڲ�����
				Column1(I,J)%F_C(:)%sigc_pre=Column1(I,J)%F_C(:)%sigc       ! �����ڲ�����
				Column1(I,J)%F_C(:)%epsmax_pre=Column1(I,J)%F_C(:)%epsmax   ! �����ڲ�����	

				Column1(I,J)%F_C(:)%epsmax_t_pre=Column1(I,J)%F_C(:)%epsmax_t   ! �����ڲ�����

				do K=1,size(Column1(I,J)%F_C)								 !�¼Ӳ��֣���֤������ѹ��ʧЧ
					if(Column1(I,J)%F_C(K)%epsmax_pre<-0.05) then
						Column1(I,J)%F_C(K)%e0=0.001*Column1(I,J)%F_C(K)%e0
						Column1(I,J)%failure=1
					end if
				end do


				do K=1, size(Column1(I,J)%F_S)								! �����иֽԪѭ��
		
					if((Column1(I,J)%F_S(K)%epss_pre)>0.1) then				
						Column1(I,J)%F_S(K)%es=0.001*Column1(I,J)%F_S(K)%es	 !�¼Ӳ��֣���֤�ֽ�ʧЧ
						Column1(I,J)%failure=1
					end if	

					x=Column1(I,J)%F_S(K)%fy/Column1(I,J)%F_S(K)%Es;		! �õ��ֽ������Ӧ��
					if( abs( Column1(I,J)%F_S(K)%epss_pre ) >=x ) then		! ����ֽ�Ӧ���������Ӧ��
						Column1(I,J)%yield=1								! �ֽ�����
					end if
				end do	
			end do
		end do
		Column0=Column1
		return
	end subroutine New_Step

	subroutine Initial_All(cptim, timinc, m, nnn,mats)
		use My_Var
		implicit none
		real*8 :: cptim, timinc
		integer :: m, nnn,mats
		time=cptim; dtime=timinc
		call Rect01_Initial (Column1(m,nnn),Column_Mat(mats))
		return
	end subroutine Initial_ALl

	subroutine ubeam(d,fcrp,df,dum1,etot,de,dum2,s,dum3,gs,dum4,temp,
     * dtemp,ngens,m,nnn,mat) ! MARC ubeam����ӿ�
		implicit real*8 (a-h,o-z)                                               dp
		common/creeps/cptim,timinc,timinc_p,timinc_s,timincm,
     1	timinc_a,timinc_b,creept(33),icptim,icfte,icfst,
     2	icfeq,icftm,icetem,mcreep,jcreep,icpa,icftmp,icfstr,
     3	icfqcp,icfcpm,icrppr,icrcha,icpb,iicpmt,iicpa
		dimension d(ngens,ngens),fcrp(*),df(ngens),etot(ngens),de(ngens),
     *	s(*),gs(ngens),temp(*),dtemp(*)

			call Column_A(d,df,etot,de,gs,ngens,m,nnn,cptim,timinc,mat)

      return
      end

	subroutine ReadData() ! ��matcode�ļ��ж�����ά����
		use My_Var
		use Typ_Section
		IMPLICIT REAL *8 (A-H, O-Z)
		common/creeps/cptim,timinc,timinc_p,timinc_s,timincm,
     1	timinc_a,timinc_b,creept(33),icptim,icfte,icfst,
     2	icfeq,icftm,icetem,mcreep,jcreep,icpa,icftmp,icfstr,
     3	icfqcp,icfcpm,icrppr,icrcha,icpb,iicpmt,iicpa
		integer :: I,J,II
			open(66,file='debug.txt')
				write(66,*)	
			close(66)
			open (55,file='matcode.txt')	! �����û��Զ�������Ӧ��Щ���ϱ��
			read(55,*) MatNumber(0)			! һ���ж��ٸ�����
			do II=1,MatNumber(0)
				read(55,*) Column_Mat(II)%H,Column_Mat(II)%b,Column_Mat(II)%E0
				Column_Mat(II)%yield=0
				do I=1,size(Column_Mat(II)%F_C)
					read(55,*) Column_Mat(II)%F_C(I)%x,Column_Mat(II)%F_C(I)%y,
	1						Column_Mat(II)%F_C(I)%ac				
					read(55,*)	Column_Mat(II)%F_C(I)%fc,Column_Mat(II)%F_C(I)%eps0
					read(55,*)	Column_Mat(II)%F_C(I)%fu,Column_Mat(II)%F_C(I)%epsu
					read(55,*)	Column_Mat(II)%F_C(I)%e0;

					Column_Mat(II)%F_C(I)%ft=-Column_Mat(II)%F_C(I)%fc/10.
					Column_Mat(II)%F_C(I)%epsut=Column_Mat(II)%F_C(I)%ft/
	1					Column_Mat(II)%F_C(I)%e0*10.

					Column_Mat(II)%F_C(I)%epsmax_t_pre=0.;
					Column_Mat(II)%F_C(I)%epsmax_pre=0.;
					Column_Mat(II)%F_C(I)%epsc_pre=0.;
					Column_Mat(II)%F_C(I)%sigc_pre=0.;
				end do

				do I=1,size(Column_Mat(II)%F_S)
		            read(55,*)Column_Mat(II)%F_S(I)%x,                        ! ����ֽ���ά�����꣨x��y�������
     1                          Column_Mat(II)%F_S(I)%y,
     2				          Column_Mat(II)%F_S(I)%as				
		            read(55,*)Column_Mat(II)%F_S(I)%fy,                       ! ����ֽ���ά�Ŀ�������ǿ�ȣ���ֵ��
     2					      Column_Mat(II)%F_S(I)%es                        ! ����ֽ���ά�ĵ�ģ
					Column_Mat(II)%F_S(I)%SIGP0=0.;
					Column_Mat(II)%F_S(I)%K1=4.
					Column_Mat(II)%F_S(I)%K2=25.
					Column_Mat(II)%F_S(I)%K3=45.
					Column_Mat(II)%F_S(I)%K4=1.2;
					Column_Mat(II)%F_S(I)%P4=1.
					Column_Mat(II)%F_S(I)%deps_pre=0.;
					Column_Mat(II)%F_S(I)%sigs_pre=0.;
					Column_Mat(II)%F_S(I)%sigs_pre1=0.;
					Column_Mat(II)%F_S(I)%epss_pre=0.;
					Column_Mat(II)%F_S(I)%FF=0;
					Column_Mat(II)%F_S(I)%PRF=0;
                      Column_Mat(II)%F_S(I)%S_UNLD=0.;
                      Column_Mat(II)%F_S(I)%E_UNLD=0.;
                      Column_Mat(II)%F_S(I)%S_RELD=0.;
                      Column_Mat(II)%F_S(I)%E_RELD=0.;
                      Column_Mat(II)%F_S(I)%SSRL3=0.;
                      Column_Mat(II)%F_S(I)%ESRL3=0.;
					Column_Mat(II)%F_S(I)%epssmax_P_pre=Column_Mat(II)        ! �Ըֽ���ά�����Ӧ�丳��ֵ
	1				      %F_S(I)%fy/Column_Mat(II)%F_S(I)%es;
					Column_Mat(II)%F_S(I)%epssmax_N_pre=-Column_Mat(II)       ! �Ըֽ���ά���ѹӦ�丳��ֵ
	1				      %F_S(I)%epssmax_P_pre/Column_Mat(II)%F_S(I)%P4;
				end do

			open(66,file='debug.txt',position='append')
				write(66,'(A,I4,A,2G13.5)')	'���ϱ��',II,'   ������',
	1				Column_Mat(II)%b,Column_Mat(II)%H
				write(66,*)
				write(66,*)  
	1	"��������άX,  Y,      Area,      fc,   EPS0,
     2  fu,    EPSU, E0"
				do I=1,size(Column_Mat(II)%F_C)
					write(66,'(I3,10G10.3)') I, Column_Mat(II)%F_C(I)%x,
	1				Column_Mat(II)%F_C(I)%y,
	1				Column_Mat(II)%F_C(I)%ac,Column_Mat(II)%F_C(I)%fc,
     1				Column_Mat(II)%F_C(I)%eps0,
	2				Column_Mat(II)%F_C(I)%fu,Column_Mat(II)%F_C(I)%epsu,
     2				Column_Mat(II)%F_C(I)%e0
				end do
			write(66,*)
			write(66,*) "�ֽ���άX,      Y,        Area,        fy,        Es"
             	do I=1,size(Column_Mat(II)%F_S)
					write(66,'(I3,5G11.3)') I, 
	1						Column_Mat(II)%F_S(I)%x,Column_Mat(II)%F_S(I)%y,
	1						Column_Mat(II)%F_S(I)%as,Column_Mat(II)%F_S(I)%fy,
     2						Column_Mat(II)%F_S(I)%es
			end do
			write(66,*)
			close(66)
			end do
			close(55)
		return
	end subroutine ReadData

	SUBROUTINE UBGINC(INC,INCSUB) ! �����������
		use My_Var
		use Typ_Section
		IMPLICIT REAL *8 (A-H, O-Z)
		common/creeps/cptim,timinc,timinc_p,timinc_s,timincm,
     1	timinc_a,timinc_b,creept(33),icptim,icfte,icfst,
     2	icfeq,icftm,icetem,mcreep,jcreep,icpa,icftmp,icfstr,
     3	icfqcp,icfcpm,icrppr,icrcha,icpb,iicpmt,iicpa
		integer :: I,J,II
		if(FirstStep.ne.123.and.cptim.eq.0.and.timinc.eq.0 ) then ! ��һ�μ���
			FirstStep=123
			Current_Int_ElemNo=0;
			Current_Glb_ElemNo=0;
			call ReadData() ! ��������
		end if
		RETURN
	END
			
      subroutine plotv(v,s,sp,etot,eplas,ecreep,t,m,nn,layer,ndi,
     * nshear,jpltcd)
		use My_Var
	    implicit real*8 (a-h,o-z)                                               dp

		include "../common/concom"
		include "../common/dimen"
		include "../common/blnk"

		dimension s(*),etot(*),eplas(*),ecreep(*),sp(*),m(2)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

		integer :: nn,i,j,k
		real*8 coordY1, coordY2
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


	 if(inf( m(1) ) .ne. 0 .and. jpltcd==1) then
	    k=inf(m(1))
		v=real(Column1(k,nn)%yield)  ! ������Խ�
	end if

	if(inc==1 .and. inf(m(1))==0 .and. ndi==1) then          !���˿ǵ�Ԫ��ubeam��Ԫ��ĵ�Ԫ(�˵�Ԫ��beam��Ԫ��
	   if(nn==2) then
	      this(m(1))=1                                       !beam��Ԫ��thisֵΪ1
	   end if
	end if

	if(inf(m(1))==0 .and. ndi==1 
     1         .and. this(m(1))==0)  then                 !�����truss��Ԫ

	      if(etot(1)>0.002 .or. etot(1)<-0.002 
     1		            .and. trussfailure1(m(1))==0) then
	         trussfailure1(m(1))=1
	      end if

	      if(etot(1)>=0.1 .or. etot(1)<=-0.1
     1		             .and. trussfailure4(m(1))==0) then
	         trussfailure4(m(1))=1
	      end if
	end if 	
	
	if(inf(m(1))==0 .and. jpltcd==1) then
	     v=real(trussfailure1(m(1)))   !���truss��Ԫ���Խ�
      end if  	   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c		if(jpltcd==6) then				! ��� ubeam��Ԫ����%%%
c		if( inf( m(1) ) .ne. 0) then      ! ֻ�� ubeam��Ԫ���ܽ������³���Σ����ܰ����淽����ⵥԪ����

c			CALL ELMVAR(69,m(1),nn,1,VAR_UBEAM)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c			LL_beam=0.400			   	         !�������Ԫ����, ��Ҫ����ʵ�� ��Ԫ�ĳ����޸�
c			LL_beammid=0.270	
c			LL_columndown=0.205				     !�������Ԫ���ȣ�ͬ��
c			LL_columnup=0.4
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c			CALL NODVAR(0,lm(1),VALNO,NQNCOMP,NQDATATYPE)		! ÿ��ubeam ��Ԫ�ĵ�һ���ڵ�����
c			coord_f(1,1)=valno(1)
c			coord_f(1,2)=valno(2)
c			coord_f(1,3)=valno(3)

c			CALL NODVAR(0,lm(2),VALNO,NQNCOMP,NQDATATYPE)		! ÿ��ubeam ��Ԫ�ĵڶ����ڵ�����
c			coord_f(2,1)=valno(1)
c			coord_f(2,2)=valno(2)
c			coord_f(2,3)=valno(3)

c		coord_f_Y(m(1))=( coord_f(1,2)+coord_f(2,2) )/2.0		! ÿ�� ubeam��Ԫ���ڵ��������ƽ��ֵ���������жϵ�Ԫ��λ��ʱʹ��
c		coord_f_X(m(1))=( coord_f(1,1)+coord_f(2,1) )/2.0		! ÿ�� ubeam��Ԫ���ڵ�������ƽ��ֵ��ͬ��

		!�����ÿ�����ز��� ��ubeam��Ԫ�����ӵ��������������� ������˹�����֣�%%%%
		! ������ʱֻ�����˿����������������漰����ʱ������LL_beam��
		! �����Ҫ����������Ԫ��Ӧ�����жϵ�Ԫ�������������������������������Ϣ����������������Ȳ���LL_column

c			if ( nn==1.or.nn==3 ) 	then	! �������Ԫ 1,3 ��˹��
     			
c				G_nn(m(1),nn)= (G_f1(m(1),nn)-0.5*G_df1(m(1),nn))*		! ����������
c	1			VAR_UBEAM*G_de1(m(1),nn)+
c	2			(G_f2(m(1),nn)-0.5*G_df2(m(1),nn))*
c	3			VAR_UBEAM*G_de2(m(1),nn)+
c	4			(G_f3(m(1),nn)-0.5*G_df3(m(1),nn))*
c	5			VAR_UBEAM*G_de3(m(1),nn)
c     		
c			E_nn(m(1),nn)=VAR_UBEAM*Column1(m(1),nn).Elem_Energy     !��������
c		
c			else if ( nn==2 ) then			! �������Ԫ 2 ��˹��
c			
c				G_nn(m(1),nn)=(G_f1(m(1),nn)-0.5*G_df1(m(1),nn))*
c	1			VAR_UBEAM*G_de1(m(1),nn)+
c	2			(G_f2(m(1),nn)-0.5*G_df2(m(1),nn))*
c	3			VAR_UBEAM*G_de2(m(1),nn)+
c	4			(G_f3(m(1),nn)-0.5*G_df3(m(1),nn))*
c	5			VAR_UBEAM*G_de3(m(1),nn)
	
c			E_nn(m(1),nn)=VAR_UBEAM*Column1(m(1),nn).Elem_Energy

c               end if 




        !   ��ÿ��ubeam��Ԫ�ĵ�3�����ֵ�����������һ�κ󣬿��ԶԸõ�Ԫ�ĸ������������л���
c		  if ( nn==3 )  then
 
              !  ���ÿ����Ԫ�� ÿ�����ز�������������������͵������� ��������˹�����������ӣ�
c				denergy(m(1))=0.
c				denergy(m(1))=G_nn(m(1),1)+G_nn(m(1),2)+G_nn(m(1),3)
c				dene_E(m(1))=0.
c				dene_E(m(1))=E_nn(m(1),1)+E_nn(m(1),2)+E_nn(m(1),3)


			! ���ÿ����Ԫ�� ÿ�����ز���������ۻ�������
c			  if(inc>1) then
c				energy(m(1))=energy_pre(m(1))+denergy(m(1))
c	
c			  else if(inc==1) then
c				energy(m(1))=denergy(m(1))
c			  end if
             
		    !  ���õ�Ԫ���º���������洢��energy_pre(:)������һ������������ʱʹ��		
c				energy_pre(m(1))= energy(m(1))    


				! ÿ������Ԫ �� ÿ�����ز�������ĵ���Ӧ����
c				ene_E(m(1)) = dene_E(m(1))

				! ÿ������Ԫ��ÿ�����ز������������Ӧ����
c				ene_P(m(1)) = energy(m(1))-ene_E(m(1))


c		   end if					! if( nn==3 )


ccccccccccccccccccccccccccccccc qiu  ccccccccccccccccccccccccccccccccccccc

		!�ر�ע�⣺��ģ����0-486��ubeam��Ԫ��443-490�ǵ�����������Ԫ��491-1378�Ƿֲ�ǵ�Ԫ������ǰע���޸�
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c		if(m(1)==404 .and. nn==1 )then          !! �������һ��Ubeam��Ԫ���486���� �޸�***
			! ��ģ�����һ��ubeam��Ԫ( ��ģ��Ϊ442�ŵ�Ԫ )�����˱��γ���֮�󣬽��и��㵥Ԫ�������ܼ��� 

c				G_fbeam_storey(1:24)=0.
c				E_fbeam_storey(1:24)=0.
c				P_fbeam_storey(1:24)=0.
c
c				G_fcol_storey(1:24)=0.
c				E_fcol_storey(1:24)=0.
c				P_fcol_storey(1:24)=0.

ccccccccccccccccccccccccccccccc test_qiu �ⲿ���ǵ��Գ���ʱʹ�õ� cccccccccccccccccccccccccccccccccccccccccc
c		if (m(1)==924 .and. nn==3) then
c		open (735,file="test.txt",position='APPEND')
c		write(735,'(25G15.5)') INC,m(1)
c		write(735,'(25G15.5)') INC,G_f1(m(1),nn),G_df1(m(1),nn),G_de1(m(1),nn)
c		close(735)		
c		end if
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c		   do j=1,924	
c		   do j=1,numel	

			! ͳ������ubeam��Ԫ�� ����������������
			! ��������Ԫ���ڵ��������ƽ��ֵcoord_f_Y(j) ��ÿһ��Ĳ��coordY1 �Ĵ�С��ϵ���ж��õ�Ԫ������һ�����
			  
c			   coordY1=0.
c			   do i=1,6
c			   coordY2=coordY1
c			   coordY1=4.200+(i-1)*3.600
c			    if( abs( coord_f_Y(j)-coordY1 )<0.0001 ) then
c			    G_fbeam_storey(i)= G_fbeam_storey(i)+energy(j)
c			    E_fbeam_storey(i)= E_fbeam_storey(i)+ene_E(j)
c			    P_fbeam_storey(i)= P_fbeam_storey(i)+ene_P(j)

c			   elseif(  (coord_f_Y(j)-coordY2 )>0.0001
c	1		         .and.(coord_f_Y(j)-coordY1 )<-0.0001) then
c			    G_fcol_storey(i)= G_fcol_storey(i)+energy(j)
c			    E_fcol_storey(i)= E_fcol_storey(i)+ene_E(j)
c			    P_fcol_storey(i)= P_fcol_storey(i)+ene_P(j)
c			    end if
			  
c			  end do

			! ͬ��������ͳ������ubeam��Ԫ�� ����������������
			! ��������Ԫ���ڵ��������ƽ��ֵcoord_f_Y(j) ��ÿһ��Ĳ��coordY1 �Ĵ�С��ϵ���ж��õ�Ԫ������һ�����

			!	��������
c			end do              ! do j=1,numel
c					Genergy_T=G_fbeam_storey+G_fcol_storey
c
c				open (745,file="Genergy-fbeamstorey.txt",position='APPEND')
c				write(745,'(25G15.5)') INC,G_fbeam_storey(1:6)
c				close(745)
c
c				open (742,file="Genergy-fcolstorey.txt",position='APPEND')
c				write(742,'(25G15.5)') INC,G_fcol_storey(1:6)
c				close(742)

c				open (743,file="Genergy-T.txt",position='APPEND')
c				write(743,'(25G15.5)') INC,Genergy_T(1:6)
c				close(743)


c		  end if           ! if (m(1)==486 .and. nn==3 )		
		
c		end if					! if ( inf( m(1) ) .ne. 0 )	

c	   end if					! if(jpltcd==6)


      return
      end
	


	SUBROUTINE UEDINC(INC,INCSUB)
		IMPLICIT REAL *8 (A-H, O-Z)
		DIMENSION VALNO(3)
		DIMENSION DISPX(20)
          DIMENSION DISPY(20)
		DIMENSION Driftx(20)          ! �ǵ�1 ��x����������²��λ��
          DIMENSION Drifty(20)

		DIMENSION SRC_DISPX(20)
          DIMENSION SRC_DISPY(20)
          DIMENSION SRC_Driftx(20)
          DIMENSION SRC_Drifty(20)

		DIMENSION ALTITUDE(20)
		DIMENSION NODENUM(20)
		DIMENSION FORCE(26)
		DIMENSION Acceler(25)
		DIMENSION ext_force(25)

		integer:: i,j, nodenumber

		Reac_forceY=0.
		Reac_forceX=0.

	!!!!!!!!!!!�����ܽṹ���λ�����!!!!!!!!!

			CALL NODVAR(1,8391,VALNO,NQNCOMP,NQDATATYPE)    !!!��1�Ĳ��λ��
			DISPX(1)=VALNO(1)
	        DiSPY(1)=VALNO(2)

			CALL NODVAR(1,8620,VALNO,NQNCOMP,NQDATATYPE)
			DISPX(2)=VALNO(1)
	        DiSPY(2)=VALNO(2)

			CALL NODVAR(1,9463,VALNO,NQNCOMP,NQDATATYPE)
			DISPX(3)=VALNO(1)
	        DiSPY(3)=VALNO(2)
			
			driftx(1)=(dispx(2)-dispx(1))/4.5
			driftx(2)=(dispx(3)-dispx(2))/3.8
			
			drifty(1)=(dispy(2)-dispy(1))/4.5
			drifty(2)=(dispy(3)-dispy(2))/3.8            
	        
			CALL NODVAR(1,8461,VALNO,NQNCOMP,NQDATATYPE)    !!!��2�Ĳ��λ��
			DISPX(11)=VALNO(1)
	        DiSPY(11)=VALNO(2)

			CALL NODVAR(1,8851,VALNO,NQNCOMP,NQDATATYPE)
			DISPX(12)=VALNO(1)
	        DiSPY(12)=VALNO(2)

			CALL NODVAR(1,10051,VALNO,NQNCOMP,NQDATATYPE)
			DISPX(13)=VALNO(1)
	        DiSPY(13)=VALNO(2)
			
			driftx(11)=(dispx(12)-dispx(11))/4.5
			driftx(12)=(dispx(13)-dispx(12))/3.8
			
			drifty(11)=(dispy(12)-dispy(11))/4.5
			drifty(12)=(dispy(13)-dispy(12))/3.8   
			
		    CALL NODVAR(1,8259,VALNO,NQNCOMP,NQDATATYPE)    !!!��3�Ĳ��λ��
			DISPX(15)=VALNO(1)
	        DiSPY(15)=VALNO(2)

			CALL NODVAR(1,9201,VALNO,NQNCOMP,NQDATATYPE)
			DISPX(16)=VALNO(1)
	        DiSPY(16)=VALNO(2)

			CALL NODVAR(1,10454,VALNO,NQNCOMP,NQDATATYPE)
			DISPX(17)=VALNO(1)
	        DiSPY(17)=VALNO(2)
			
			driftx(15)=(dispx(16)-dispx(15))/4.5
			driftx(16)=(dispx(17)-dispx(16))/3.8
			
			drifty(15)=(dispy(16)-dispy(15))/4.5
			drifty(16)=(dispy(17)-dispy(16))/3.8 
	
		open (721,file="frame_driftx.txt",position='APPEND')
	write(721,'(7G15.5)') INC,driftx(1:2),driftx(11:12),driftx(15:16)      !!!ע���޸�������ݸ���
		close(721)

		open (722,file="frame_drifty.txt",position='APPEND')
	write(722,'(7G15.5)') INC,drifty(1:2),drifty(11:12),drifty(15:16)
		close(722)

	       !!!!!!!!!!!!SRC�����λ�Ƽ��㼰���!!!!!!!!!!!!!!!!!
			
			CALL NODVAR(1,11080,VALNO,NQNCOMP,NQDATATYPE)           !src_1
			SRC_DISPX(1)=VALNO(1)
	        SRC_DiSPY(1)=VALNO(2)

			CALL NODVAR(1,11163,VALNO,NQNCOMP,NQDATATYPE)
			SRC_DISPX(2)=VALNO(1)
	        SRC_DiSPY(2)=VALNO(2)

	             SRC_driftx(1)=(SRC_DISPX(2)-SRC_DISPX(1))/29.68
                   SRC_drifty(1)=(SRC_DISPy(2)-SRC_DISPy(1))/29.68

			CALL NODVAR(1,8352,VALNO,NQNCOMP,NQDATATYPE)           !src_2
			SRC_DISPX(1)=VALNO(1)
	        SRC_DiSPY(1)=VALNO(2)

			CALL NODVAR(1,11161,VALNO,NQNCOMP,NQDATATYPE)
			SRC_DISPX(2)=VALNO(1)
	        SRC_DiSPY(2)=VALNO(2)

	             SRC_driftx(2)=(SRC_DISPX(2)-SRC_DISPX(1))/33.73
                   SRC_drifty(2)=(SRC_DISPy(2)-SRC_DISPy(1))/33.73
	           
 			CALL NODVAR(1,8328,VALNO,NQNCOMP,NQDATATYPE)           !src_3
			SRC_DISPX(1)=VALNO(1)
	        SRC_DiSPY(1)=VALNO(2)

			CALL NODVAR(1,11172,VALNO,NQNCOMP,NQDATATYPE)
			SRC_DISPX(2)=VALNO(1)
	        SRC_DiSPY(2)=VALNO(2)

	             SRC_driftx(3)=(SRC_DISPX(2)-SRC_DISPX(1))/19.46
                   SRC_drifty(3)=(SRC_DISPy(2)-SRC_DISPy(1))/19.46

			CALL NODVAR(1,11083,VALNO,NQNCOMP,NQDATATYPE)           !src_4
			SRC_DISPX(1)=VALNO(1)
	        SRC_DiSPY(1)=VALNO(2)

			CALL NODVAR(1,11203,VALNO,NQNCOMP,NQDATATYPE)
			SRC_DISPX(2)=VALNO(1)
	        SRC_DiSPY(2)=VALNO(2)

	             SRC_driftx(4)=(SRC_DISPX(2)-SRC_DISPX(1))/19.71
                   SRC_drifty(4)=(SRC_DISPy(2)-SRC_DISPy(1))/19.71				        
	        
			CALL NODVAR(1,11079,VALNO,NQNCOMP,NQDATATYPE)           !src_5
			SRC_DISPX(1)=VALNO(1)
	        SRC_DiSPY(1)=VALNO(2)

			CALL NODVAR(1,11169,VALNO,NQNCOMP,NQDATATYPE)
			SRC_DISPX(2)=VALNO(1)
	        SRC_DiSPY(2)=VALNO(2)

	             SRC_driftx(5)=(SRC_DISPX(2)-SRC_DISPX(1))/25.03
                   SRC_drifty(5)=(SRC_DISPy(2)-SRC_DISPy(1))/25.03
	
			CALL NODVAR(1,11078,VALNO,NQNCOMP,NQDATATYPE)           !src_6
			SRC_DISPX(1)=VALNO(1)
	        SRC_DiSPY(1)=VALNO(2)

			CALL NODVAR(1,11200,VALNO,NQNCOMP,NQDATATYPE)
			SRC_DISPX(2)=VALNO(1)
	        SRC_DiSPY(2)=VALNO(2)

	             SRC_driftx(6)=(SRC_DISPX(2)-SRC_DISPX(1))/19.45
                   SRC_drifty(6)=(SRC_DISPy(2)-SRC_DISPy(1))/19.45
			
			CALL NODVAR(1,11077,VALNO,NQNCOMP,NQDATATYPE)           !src_7
			SRC_DISPX(1)=VALNO(1)
	        SRC_DiSPY(1)=VALNO(2)

			CALL NODVAR(1,11269,VALNO,NQNCOMP,NQDATATYPE)
			SRC_DISPX(2)=VALNO(1)
	        SRC_DiSPY(2)=VALNO(2)

	             SRC_driftx(7)=(SRC_DISPX(2)-SRC_DISPX(1))/13.09
                   SRC_drifty(7)=(SRC_DISPy(2)-SRC_DISPy(1))/13.09
				 
			CALL NODVAR(1,11099,VALNO,NQNCOMP,NQDATATYPE)           !src_8
			SRC_DISPX(1)=VALNO(1)
	        SRC_DiSPY(1)=VALNO(2)

			CALL NODVAR(1,11257,VALNO,NQNCOMP,NQDATATYPE)
			SRC_DISPX(2)=VALNO(1)
	        SRC_DiSPY(2)=VALNO(2)

	             SRC_driftx(8)=(SRC_DISPX(2)-SRC_DISPX(1))/16.81
                   SRC_drifty(8)=(SRC_DISPy(2)-SRC_DISPy(1))/16.81

			CALL NODVAR(1,11108,VALNO,NQNCOMP,NQDATATYPE)           !src_9
			SRC_DISPX(1)=VALNO(1)
	        SRC_DiSPY(1)=VALNO(2)

			CALL NODVAR(1,11266,VALNO,NQNCOMP,NQDATATYPE)
			SRC_DISPX(2)=VALNO(1)
	        SRC_DiSPY(2)=VALNO(2)

	             SRC_driftx(9)=(SRC_DISPX(2)-SRC_DISPX(1))/13.28
                   SRC_drifty(9)=(SRC_DISPy(2)-SRC_DISPy(1))/13.28

		open (721,file="SRC_driftx.txt",position='APPEND')
			write(721,'(10G15.5)') INC,SRC_driftx(1:9)
		close(721)

		open (722,file="SRC_drifty.txt",position='APPEND')
			write(722,'(10G15.5)') INC,SRC_drifty(1:9)
		close(722)

c	����ǰ����Ҫ����ģ��ʵ�ʽڵ�����i���е���

c		do i=1, 390                             ! Ҫ����ģ��ʵ���ܽ�������е���
c	        call nodvar(0,i,VALNO,NQNCOMP,NQDATATYPE)

c	����ǰ����Ҫ�Եײ�ڵ�����ֵ�������޸�ȷ��

c			if (abs(valno(2)+0.001)<0.1) then   !ͨ��Y������˳��ײ���5700��Ҫ�޸�
c				call nodvar(5,i,VALNO,NQNCOMP,NQDATATYPE)
c				Reac_forceY=Reac_forceY+VALNO(3)
c				Reac_forceX=Reac_forceX+VALNO(1)

c			end if 
c		end do

c		open (722,file="Reactforce.txt",position='APPEND')
c			write(722,'(3G15.5)') INC,Reac_forceY, Reac_forceX

c		close(722)

c		NQNCOMP=3


c		  do j=2,7
c			Driftx(j)=DISP(j)-DISP(j-1)
c		  end do


c		  do i=4,6
c			nodenumber=45+(i-4)*12
c			CALL NODVAR(1,nodenumber,VALNO,NQNCOMP,NQDATATYPE)
c			DISP(i)=VALNO(2)
c		  end do

c	�����λ�Ƶ����������޸ģ���ֹ���ֺܶ�0�����

c		open (722,file="result-dis.txt",position='APPEND')
c			write(722,'(21G15.5)') INC,DISP(1:7)   
c		close(722)

c		open (721,file="result-driftx.txt",position='APPEND')
c			write(721,'(21G15.5)') INC,DISP(1:7)
c		close(721)
		
		
c		CALL NODVAR(1,12,VALNO,NQNCOMP,NQDATATYPE)
c		ALTITUDE(1)=VALNO(2)
c		NODENUM(1)=12



c		CALL NODVAR(1,25,VALNO,NQNCOMP,NQDATATYPE)
c		ALTITUDE(2)=VALNO(2)
c		NODENUM(2)=25


c		CALL NODVAR(1,63,VALNO,NQNCOMP,NQDATATYPE)
c		ALTITUDE(3)=VALNO(2)
c		NODENUM(3)=63


c		CALL NODVAR(1,50,VALNO,NQNCOMP,NQDATATYPE)
c		ALTITUDE(4)=VALNO(2)
c		NODENUM(4)=50




c		DO Q=1,4
c		   if(abs(ALTITUDE(Q))>1.0) then
c			 open (721,file="debug2.txt",position='APPEND')
c			 write(721,'(21G15.5)') INC,NODENUM(Q)
c		   end if
c		end DO


c		open (721,file="altitude.txt",position='APPEND')
c			write(721,'(21G15.5)') INC,ALTITUDE(1:4)   
c		close(721)


		RETURN
	END


	SUBROUTINE UACTIVE(M,N,MODE,IRSTSTR,IRSTSTN,INC,TIME,TIMINC)
	use My_Var
	IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION M(2)
      integer:: k
          if( inf( m(1) ) .ne. 0) then
              k=inf(m(1))
		    if(Column1(k,1)%failure >0 .and. Column1(k,2)%failure >0
	1	     .and. Column1(k,3)%failure >0)    then 

			MODE=-1
			IRSTSTR=1
			IRSTSTN=1

		open (721,file="killed.txt",position='APPEND')
		write(721,'(2G15.5)') inc, m(1)
		close(721)

		end if
	end if

	   if(inf(m(1))==0 .and. trussfailure4(m(1))==1) then
	        MODE=-1
			IRSTSTR=1
			IRSTSTN=1

		  open (721,file="killed.txt",position='APPEND')
		  write(721,'(2G15.5)') inc, m(1)
		  close(721)
	end if
	       
	RETURN
	END