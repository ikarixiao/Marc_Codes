c	利用MARC的UBEAM子程序加入钢筋混凝土纤维模型 (36根混凝土，4根钢筋)
c	适用于MARC 2005R2+Visual Fortran 6.6B+
c	last revised: 2005.11.22(22:30)
c	by 陆新征，清华大学土木系, luxinzheng@sina.com
c	serial number: 00020A
c	应张老师要求删去混凝土抗拉代码 (20A)
c	重新修改了刚度矩阵算法，滞回表现更好(20)
c	重新修改了刚度计算部分，提高了精度，并在debug文件中报告出现负刚度(18)
c	增加了混凝土受拉开裂部分代码 (17)
c     修改了切线刚度矩阵的算法，引入差分运算得到刚度，提高了稳定性和精度 (16)
c	修正了钢筋断裂部分的错误 (16)
c	版本13~15，用于分析某些特殊荷载，不作为通用部分推广
c	修改混凝土卸载和再加载的部分错误 (12)
c	将混凝土和纤维由二维数组改为一维数组 (11)
c	修改了纤维集成时的错误 (10)
c	增加了能量的初级程序和混凝土受拉部分，本程序适用于52号梁单元
c	增加了钢筋的汪训流模型，使计算更精确
c	修改了显示高度的数组列数。阻尼器的系数按照实际减半。
c	修改添加了相对层间位移的输出
c	使用marc中的ICODE代替梁单元中积分点位置和长度,简化了能量计算程序

c     在plotv中增加了判断truss单元状态的程序（SQEEN）
c     在uactive中增加了杀死truss单元的程序（SQEEN）

	module Typ_Fiber
		implicit none

		type :: Typ_Fiber_Concrete        ! 定义混凝土纤维类型
			real*8 :: Ac                  ! 混凝土纤维的面积
			real*8 :: x,y                 ! 混凝土纤维到截面中心的距离
			real*8 :: fc                  ! 混凝土峰值抗压强度, 负
			real*8 :: eps0                ! 混凝土峰值压应变, 负
			real*8 :: fu                  ! 混凝土极限抗压强度, 负
			real*8 :: epsu                ! 混凝土极限抗压应变, 负
!----------------------
			real*8 :: ft                  ! 混凝土极限抗压强度, 正
			real*8 :: epsut               ! 混凝土极限抗压应变, 正		
!----------------------
			real*8 :: e0                  ! 混凝土抗压弹性模量
			real*8 :: sigc                ! 混凝土纤维当前应力
			real*8 :: epsc                ! 混凝土纤维当前应变
			real*8 :: epsmax              ! 混凝土经历的最大压应变
!----------------------
			real*8 :: epsmax_t            ! 混凝土经历的最大拉应变
!----------------------
			real*8 :: sigc_pre            ! 混凝土纤维前次应力
			real*8 :: epsc_pre            ! 混凝土纤维前次应变
			real*8 :: epsmax_pre          ! 混凝土经历的前次最大压应变
!----------------------
			real*8 :: epsmax_t_pre        ! 混凝土经历的前次最大拉应变
!-----------------------
			real*8 :: dEPS
			real*8 :: Ect                 ! 混凝土纤维切线刚度
		end type Typ_Fiber_Concrete

		type :: typ_Fiber_Steel
			real*8 :: as                  ! 钢筋纤维的面积
			real*8 :: x,y                 ! 钢筋纤维到截面中心的距离
			real*8 :: fy,SIGP0            ! 钢筋纤维的屈服应力、初始预加应力（非负）
			real*8 :: es                  ! 钢筋纤维的弹性模量
			real*8 :: sigs_pre            ! 上次收敛迭代时钢筋的应力
			real*8 :: epss_pre            ! 上次收敛迭代时钢筋的应变
			real*8 :: sigs,epss           ! 钢筋纤维当前的应力、应变
			real*8 :: epssmax_P           ! 钢筋经历的最大应变，正
              real*8 :: epssmax_P_pre       ! 钢筋经历的前次最大应变，正 
			real*8 :: epssmax_N           ! 钢筋经历的最小应变，负
              real*8 :: epssmax_N_pre       ! 钢筋经历的前次最小应变，负 
			real*8 :: K1,K2,K3            ! 硬化起点、峰值点、破坏点应变与屈服应变的比值 
			real*8 :: K4                  ! 峰值点应力与屈服应力的比值 
			real*8 :: P4                  ! 拉、压屈服应力比值（取正直） 
              real*8 :: S_UNLD,E_UNLD       ! 钢筋纤维由再加载转为卸载时的转折点应力、应变 
              real*8 :: S_RELD,E_RELD       ! 钢筋纤维由卸载转为再加载时的转折点应力、应变
              real*8 :: SSRL3,ESRL3         ! 钢筋纤维再加载曲线与框架线交点应力、应变
			real*8 :: EST,sigs_pre1       ! 钢筋纤维切线弹性模量，上上次收敛迭代时钢筋的应力         
			real*8 :: deps,deps_pre       ! 当前应变增量、前一荷载步对应的应变增量         
			integer:: FF,PRF              ! 破坏标记、塑性往复标记     （值为1时则破坏、塑性往复）
		end type typ_Fiber_Steel

		contains

		subroutine Fiber_Steel_Force(F_S,dEPS)                                ! 计算钢筋纤维内力
			type (Typ_Fiber_Steel) :: F_S                 
			real*8 :: dEPS,x                                                  ! 当前应变增量、总应变
			real*8 :: s                                                       ! 将受压钢筋等效为受拉钢筋计算时的变换符号
			real*8 :: y
			x=F_s%epss_pre+dEPS                                               ! 当前总应变 
              if(F_s%sigs_pre==0.)then
			   s=(F_s%sigs_pre+sign(1.d0,deps)) 
	1		      /abs(F_s%sigs_pre+sign(1.d0,deps))                          ! 当F_s%sigs_pre=0.时s的取值
              else
	           s=F_s%sigs_pre/abs(F_s%sigs_pre)                               ! 当F_s%sigs_pre不等于0.时s的取值
	        end if
			F_S%K1=F_S%K1*(F_S%P4**((ABS(s)-s)/(2.*abs(s))))                  ! 为预应力筋考虑 
              F_S%K2=F_S%K2*(F_S%P4**((ABS(s)-s)/(2.*abs(s))))                  ! 为预应力筋考虑
              F_S%K3=F_S%K3*(F_S%P4**((ABS(s)-s)/(2.*abs(s))))                  ! 为预应力筋考虑
              F_S%fy=F_S%fy/(F_S%P4**((ABS(s)-s)/(2.*abs(s))))                  ! 为预应力筋考虑
	        y=F_S%fy/F_S%es
			F_S%Est=F_S%es									                  ! 赋予切线刚度初值，后面再改
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
	           if(F_s%PRF==0)then                                             ! 钢筋纤维单调加载
				  call steel_mono(F_S,s,x,deps)
			   else                                                           ! 钢筋纤维往复加载
	              call steel_recy(F_S,s,x,deps)
	           end if
			end if
              F_S%K1=F_S%K1/(F_S%P4**((ABS(s)-s)/(2.*abs(s))))
              F_S%K2=F_S%K2/(F_S%P4**((ABS(s)-s)/(2.*abs(s))))
              F_S%K3=F_S%K3/(F_S%P4**((ABS(s)-s)/(2.*abs(s))))
              F_S%fy=F_S%fy*(F_S%P4**((ABS(s)-s)/(2.*abs(s))))
			return
		end subroutine Fiber_Steel_Force	     
			
		subroutine steel_mono(F_S,s,x,deps)	                                  ! 钢筋纤维单调加载
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
			        
		subroutine steel_recy(F_S,s,x,deps)	                                  ! 钢筋纤维往复加载
			type (Typ_Fiber_Steel) :: F_S                 
			real*8 :: K2
			real*8 :: K                                                       ! 等效硬化直线斜率
			real*8 :: x,s,dEPS
              K=(F_S%K4-1.)*F_S%es/(F_S%K2-1.)
			if(F_s%sigs_pre*deps<0.)then                                           
                call steel_ULD(F_S,s,x,deps,K)                                  ! 钢筋纤维卸载
			ELSE                                                             
                CALL steel_RLD(F_S,s,x,deps,K)                                  ! 钢筋纤维再加载
              END IF
			return
	    end subroutine steel_recy     

		subroutine steel_ULD(F_S,s,x,deps,K)	                              ! 钢筋纤维卸载
			type (Typ_Fiber_Steel) :: F_S                 
			real*8 :: dEPS,sigs
			real*8 :: x,y,s,k
			y=F_S%fy/F_S%es
			if(F_s%dEPS*F_s%dEPS_pre<0.)then
	           F_s%E_UNLD=F_s%epss_pre                                        ! 卸载起点应变
	           F_s%S_UNLD=F_s%sigs_pre                                        ! 卸载起点应力
			end if
			F_S%sigs=F_S%sigs_pre+F_S%es*deps
              F_S%EST=F_S%ES
			return
	    end subroutine steel_ULD    

		subroutine steel_RLD(F_S,s,x,deps,K)	                              ! 钢筋纤维再加载
			type (Typ_Fiber_Steel) :: F_S                 
			real*8 :: dEPS,sigs
			real*8 :: x,k,s,x0,aa,bb,cc
			real*8 :: p,p1,epssmax_pre
			if(F_S%sigs_pre*F_S%sigs_pre1<0..OR.F_s%dEPS*F_s%dEPS_pre<0.)then
	           F_s%E_RELD=F_s%epss_pre*s                                      ! 再加载起点应变
	           F_s%S_RELD=F_s%sigs_pre*s                                      ! 再加载起点应力	           
			   IF(F_S%sigs_pre>0.)THEN			   
			      epssmax_pre=F_S%epssmax_P_pre
			   ELSE
			      epssmax_pre=F_S%epssmax_N_pre
			   END IF
                 call steel_mono(F_S,s,epssmax_pre,deps)
	           F_s%SSRL3=F_S%sigs*s                                           ! 再加载曲线与框架线交点应力	           
			   F_s%ESRL3=epssmax_pre*S                                        ! 再加载曲线与框架线交点应变
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
			

		subroutine Fiber_Concrete_Force(F_C, dEPS)              ! 计算混凝土纤维内力
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: dEPS	                                    ! 应变
			real*8 :: EPS, x
			F_C%Ect=F_C%e0                                      ! 赋予切线刚度初值，后面再改
			EPS=F_C%epsc_pre+dEPS                               ! 荷载步结束时的应变
			if(EPS>=0. ) then 
				call Fiber_Concrete_Tension(F_C,   dEPS)          ! 调用受拉模块
			else
				if( EPS<F_C%epsmax_pre) then                    ! 大于历史最大压应变，骨架线加载
					call Fiber_Concrete_Load(F_C,  EPS)
				else                                            ! 不大于历史最大压应变，可能是卸载或者再加载
					if (dEPS>0.) then                           ! 卸载
						call Fiber_Concrete_UNLoad(F_C,  EPS)
					else                                        ! 再加载
						call Fiber_Concrete_ReLoad(F_C,  EPS)
					end if
				end if
			end if
			return
		end subroutine Fiber_Concrete_Force

		subroutine Fiber_Concrete_UNLoad(F_C, EPS)
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: EPS                                       ! 应变
			real*8 :: x,E, ESOFT
			x=F_C%epsmax_pre/F_C%eps0
			if(x<1.) then                                       ! 未达到峰值压应变
				E=F_C%fc*(2.*x-x**2)/F_C%epsmax_pre
				F_C%sigc=E*EPS; 
				F_C%Ect=E
			else                                                ! 曾经超过峰值压应变
				ESOFT=(F_C%fu-F_C%fc)/(F_C%epsu-F_C%eps0)
				F_C%sigc=min(F_C%fc+ESOFT*(F_C%epsmax_pre-F_C%eps0),0.d0)
				E=F_C%sigc/F_C%epsmax_pre
				F_C%sigc=E*EPS; 
				F_C%Ect=E
			end if			
			if(F_C%sigc>0.) then								! 混凝土纤维被压坏				
				F_C%sigc=0. 
				F_C%Ect=0.0001*F_C%E0
			end if
			return
		end subroutine Fiber_Concrete_UNLoad

		subroutine Fiber_Concrete_ReLoad(F_C, EPS)              ! 混凝土纤维再加载
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: EPS                                       ! 应变
			real*8 :: x,E, ESOFT
			x=F_C%epsmax_pre/F_C%eps0
			if(x<1.) then                                       ! 未达到峰值压应变
				E=F_C%fc*(2.*x-x**2)/F_C%epsmax_pre
				F_C%sigc=E*EPS; 
				F_C%Ect=E
			else                                                ! 曾经超过峰值压应变
				ESOFT=(F_C%fu-F_C%fc)/(F_C%epsu-F_C%eps0)
				F_C%sigc=min(F_C%fc+ESOFT*(F_C%epsmax_pre-F_C%eps0),0.d0)
				E=F_C%sigc/F_C%epsmax_pre
				F_C%sigc=E*EPS; 
				F_C%Ect=E
			end if			
			if(F_C%sigc>0.) then			                    ! 混凝土纤维被压坏				
				F_C%sigc=0. 
				F_C%Ect=0.0001*F_C%E0
			end if

			return
		end subroutine Fiber_Concrete_ReLoad

		subroutine Fiber_Concrete_Load(F_C, EPS)                ! 混凝土纤维骨架线加载
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: EPS                                       ! 应变
			real*8 :: x, ESOFT
			x=(EPS)/F_C%eps0
			if(x<1..and.x>=0.) then                             ! 未达到峰值压应变
				F_C%sigc=F_C%fc*(2.*x-x**2)
				F_C%Ect=F_C%fc*(2.-2.*x)/F_C%eps0
			else                                                ! 超过峰值压应变
				ESOFT=(F_C%fu-F_C%fc)/(F_C%epsu-F_C%eps0)
				F_C%sigc=F_C%fc+ESOFT*(x-1)*F_C%eps0
				F_C%Ect=ESOFT
			end if
			if(F_C%sigc>0.) then			                    ! 混凝土纤维被压坏				
					F_C%sigc=0. 
					F_C%Ect=0.0001*F_C%E0
			end if
			F_C%epsmax=	EPS                                     ! 记录最大压应变

			return
		end subroutine Fiber_Concrete_Load

		subroutine Fiber_Concrete_Tension(F_C, dEPS)            ! 混凝土纤维受拉
			type (Typ_Fiber_Concrete) :: F_C
			real*8 ::  dEPS,EPS                                 ! 轴力，应变，应变增量
			EPS=F_C%epsc_pre+dEPS
			if( EPS>F_C%epsmax_t_pre) then                      ! 大于历史最大拉应变，骨架线加载
					call Fiber_Concrete_Load_T(F_C,  EPS)
				else                                            ! 不大于历史最大拉应变，可能是卸载或者再加载
					if (dEPS<0.) then                           ! 卸载
						call Fiber_Concrete_UNLoad_T(F_C,  EPS)
					else                                        ! 再加载
						call Fiber_Concrete_ReLoad_T(F_C,  EPS)
					end if
				end if
			return
		end subroutine Fiber_Concrete_Tension

         subroutine Fiber_Concrete_UNLoad_T(F_C, EPS)
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: EPS                                       ! 应变
			real*8 :: x,E,ESOFT,epst0
              epst0=F_C%ft/F_C%e0
			x=F_C%epsmax_t_pre/epst0
			if(x<1.) then                                       ! 未达到峰值拉应变
				F_C%sigc=F_C%e0*EPS; 
				F_C%Ect=F_C%e0
			else                                                ! 曾经超过峰值拉应变
				ESOFT=-F_C%ft/(F_C%epsut-epst0)
				F_C%sigc=max(F_C%ft+ESOFT*(F_C%epsmax_t_pre-epst0),0.d0)
				E=F_C%sigc/F_C%epsmax_t_pre
				F_C%sigc=E*EPS; 
				F_C%Ect=E
			end if			
			if(F_C%sigc<0.) then								! 混凝土纤维被拉断				
				F_C%sigc=0. 
				F_C%Ect=F_C%e0*0.0001
			end if
			return
		end subroutine Fiber_Concrete_UNLoad_T

		subroutine Fiber_Concrete_ReLoad_T(F_C, EPS)            ! 混凝土纤维再加载
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: EPS                                       ! 应变
			real*8 :: x,E, ESOFT,epst0
	        epst0=F_C%ft/F_C%e0
			x=F_C%epsmax_t_pre/epst0
			if(x<1.) then                                       ! 未达到峰值拉应变
				F_C%sigc=F_C%e0*EPS; 
				F_C%Ect=F_C%e0
			else                                                ! 曾经超过峰值拉应变
				ESOFT=-F_C%ft/(F_C%epsut-epst0)
				F_C%sigc=max(F_C%ft+ESOFT*(F_C%epsmax_t_pre-epst0),0.d0)
				E=F_C%sigc/F_C%epsmax_t_pre
				F_C%sigc=E*EPS; 
				F_C%Ect=E
			end if			
			if(F_C%sigc<0.) then			                    ! 混凝土纤维被拉断				
				F_C%sigc=0. 
				F_C%Ect=F_C%e0*0.0001
			end if
			return
		end subroutine Fiber_Concrete_ReLoad_T

		subroutine Fiber_Concrete_Load_T(F_C, EPS)                ! 混凝土纤维骨架线加载
			type (Typ_Fiber_Concrete) :: F_C
			real*8 :: EPS                                       ! 应变
			real*8 :: x, ESOFT, epst0
			epst0=F_C%ft/F_C%e0
			x= EPS/epst0
			if(x<1..and.x>=0.) then                             ! 未达到峰值拉应变
				F_C%sigc=F_C%e0*EPS
				F_C%Ect=F_C%e0
			else                                                ! 超过峰值拉应变
				ESOFT=-F_C%ft/(F_C%epsut-epst0)
				F_C%sigc=F_C%ft+ESOFT*(x-1)*epst0
				F_C%Ect=ESOFT
			end if
			if(F_C%sigc<0.) then			                    ! 混凝土纤维被拉断				
				F_C%sigc=0. 
				F_C%Ect=F_C%e0*0.0001
			end if
			F_C%epsmax_t=	EPS                                 ! 记录最大拉应变
			return
		end subroutine Fiber_Concrete_Load_T


		subroutine Mid_Euler_F_C(F_C, N0, EPS0, dEPS0 )         !   积分混凝土纤维
			type(Typ_Fiber_Concrete) :: F_C
			real*8 :: N0, EPS0, dEPS0
			real*8 :: dEPS1  
			dEPS1=EPS0+dEPS0-F_C%epsc_pre                       ! 得到当前应变增量
			F_C%dEPS=dEPS1
			call Fiber_Concrete_Force(F_C,dEPS1)                ! 计算混凝土纤维应力
			N0=F_C%sigc*F_C%ac                                  ! 得到纤维轴力
			F_C%epsc=EPS0+dEPS0                                 ! 更新应变
			F_C%epsmax=min(F_C%epsmax_pre,EPS0+dEPS0)
			F_C%epsmax_t=max(F_C%epsmax_t_pre,EPS0+dEPS0)

			return
		end subroutine Mid_Euler_F_C

		subroutine Mid_Euler_F_S(F_S, N0, EPS0, dEPS0 )                       ! 积分钢筋纤维
			type(Typ_Fiber_Steel) :: F_S
			real*8 :: N0, EPS0, dEPS0, dEPS1
			INTEGER::INC
			dEPS1=EPS0+dEPS0-F_S%epss_pre                                     ! 得到当前应变增量
			call Fiber_Steel_Force(F_S, dEPS1 )                               ! 计算钢筋纤维应力
			F_S%epss=EPS0+dEPS0                                               ! 更新钢筋应变
			N0=F_S%sigs*F_S%as                                                ! 得到纤维轴力
			F_S%epssmax_P=MAX(F_s%epssmax_P_pre,EPS0+dEPS0)                   ! 更新正方向钢筋最大历史应变
			F_S%epssmax_N=MIN(F_s%epssmax_N_pre,EPS0+dEPS0)                   ! 更新负方向钢筋最大历史应变	           
			return
		end subroutine Mid_Euler_F_S
				
	end module Typ_Fiber

	module Typ_Section
		use Typ_Fiber
		implicit none
		type :: Typ_RC_Rect01                                   ! 定义矩形截面 
			type(Typ_Fiber_Concrete) :: F_C(36)                 ! 一共有36个混凝土纤维
			type(Typ_Fiber_Steel)    :: F_S(12)                 ! 一共有12个钢筋纤维
			real*8 :: F(3), E(3), dE(3)                         ! 外荷载，应变，应变增量
			real*8 :: Reaction(3)                               ! 截面反力
			real*8 :: E0, b,H                                   ! 截面参数，截面中心到中性轴距离
			real*8 :: D(3,3)                                    ! 截面切线刚度
			integer :: yield									! 标识屈服
			integer :: failure

			real(8) :: Elem_Energy								! 弹性能量变量%%%

		end type Typ_RC_Rect01

	contains

		subroutine Rect01_Iteration(Rect01)                     ! 对截面迭代求解
			type( Typ_RC_Rect01 ):: Rect01
			call Rect01_Integrate_Fiber(Rect01)                 ! 用dmin计算对应的轴力和弯矩
			
			
			return
		end subroutine

		subroutine Rect01_Integrate_Fiber(Rect01)               ! 截面纤维内力积分
			type( Typ_RC_Rect01 ):: Rect01
			integer :: I,J
			real*8  :: COSA, SINA
			real*8  :: x0, y0
			real*8  :: N, EPS, dEPS

			x0=0.; y0=0.;                 ! 弯曲中心位置
			Rect01%Reaction=0.
			Rect01%D=0.

			Rect01.Elem_Energy=0.        ! 弹性能归零


			do I=1, size(Rect01%F_C)
				N=Rect01%F_C(I)%sigc*Rect01%F_C(I)%ac
				EPS=(Rect01%F_C(I)%y-y0)*Rect01%E(2)+
	1    			(-Rect01%F_C(I)%x+x0)*Rect01%E(3)+Rect01%E(1)
				dEPS=(Rect01%F_C(I)%y-y0)*Rect01%dE(2)+
	1    			(-Rect01%F_C(I)%x+x0)*Rect01%dE(3)+Rect01%dE(1)
				call  Mid_Euler_F_C(Rect01%F_C(I), N, EPS, dEPS) !  求混凝土纤维内力
				Rect01%Reaction(1)=Rect01%Reaction(1)+N;
				Rect01%Reaction(2)=Rect01%Reaction(2)+
	1						N*Rect01%F_C(I)%y;
				Rect01%Reaction(3)=Rect01%Reaction(3)-
	1						N*Rect01%F_C(I)%x;

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		    Rect01.Elem_Energy=Rect01.Elem_Energy+
	1		0.5*Rect01%F_C(I)%sigc*Rect01%F_C(I)%epsc*Rect01%F_C(I)%ac	    ! 混凝土弹性应变能%%%
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
				call  Mid_Euler_F_S(Rect01%F_S(I), N, EPS, dEPS) !  求钢筋纤维内力
				Rect01%Reaction(1)=Rect01%Reaction(1)+N;
				Rect01%Reaction(2)=Rect01%Reaction(2)+
	1					N*Rect01%F_S(I)%y;
				Rect01%Reaction(3)=Rect01%Reaction(3)-
	1					N*Rect01%F_S(I)%x;

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		Rect01.Elem_Energy=Rect01.Elem_Energy+0.5*Rect01%F_S(I)%sigs*	
	1		Rect01%F_S(I)%sigs*Rect01%F_S(I)%as/Rect01%F_S(I)%Es					! 钢筋弹性应变能%%%
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
		
		subroutine Rect01_Initial (Rect01,Rect00) ! 初始化截面纤维
			type( Typ_RC_Rect01 ):: Rect01,Rect00
			Rect01=Rect00
			return
		end subroutine Rect01_Initial
			
	end module Typ_Section


	module My_Var
		use Typ_Section
		real*8 :: time, dtime                   ! 当前计算时刻，当前计算时间增量
		integer :: FirstStep                    ! 判断是否是第一步
		integer :: Current_Int_ElemNo           ! 当前内部单元号
	    integer :: Current_Glb_ElemNo           ! 当前整体单元号码
		integer :: MatNumber(0:80)              ! 存放材料编号
		type(Typ_RC_Rect01 ):: Column_Mat(80)   ! 存放代表性单元截面信息
		type(Typ_RC_Rect01 ):: Column0(10000,3)	! 数组。存放所有的单元
		type(Typ_RC_Rect01 ):: Column1(size(Column0,1),size(Column0,2)) ! 备份数组

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c		real(8) :: G_de1(50000,3),G_f1(50000,3),G_df1(50000,3)    
c		real(8) :: G_de2(50000,3),G_f2(50000,3),G_df2(50000,3)
c		real(8) :: G_de3(50000,3),G_f3(50000,3),G_df3(50000,3)
		integer:: inf(50000) 	      !用于存放ubeam单元的编号， 非ubeam单元的编号对应inf()数组值一律为0
          integer:: trussfailure1(50000),trussfailure4(50000)
          integer:: this(50000)          !用于判断是truss单元还是ubeam单元
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
ccccccccccccccccccc如果单元排序混乱，那么下面这一段可以把存储 ubeam单元的数组 开销减小ccccccccccccccccc
		if(m1>Current_Glb_ElemNo) then                                    ! 新单元开始计算
			Current_Glb_ElemNo=m1
			Current_Int_ElemNo=Current_Int_ElemNo+1
		end if
		if(m1<Current_Glb_ElemNo) then                                    ! 从零重新开始
			Current_Glb_ElemNo=m1
			Current_Int_ElemNo=1
		end if
		m=Current_Int_ElemNo
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


		inf(m1)=m	 ! 记录单元m1的信息，即其在所有ubeam单元中的编号，存入inf(m1) 数组

		if(cptim==0..and.timinc==0.) then                                 ! 初始化
			call Initial_ALL(cptim, timinc,m, nnn,mats)
		end if
		if(cptim>time) then                                               ! 新的荷载步开始
			call New_Step (d,df,etot,de,gs,ngens,m,nnn,cptim,timinc)
		end if
		if(cptim==time.and. timinc< dtime.and.cptim.ne.0) then            ! 发生荷载步折半
			call Cut_Step (d,df,etot,de,gs,ngens,m,nnn,cptim,timinc)
		end if
	
		d=0.;  ! 计算初始刚度
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
		!将每一步的内力变量存入静态数组,后在plotv子程序中调用 %%%%
c		G_de1(m,nnn)=de(1)
c		G_f1(m,nnn)=gs(1)
c		G_df1(m,nnn)=df(1)
c		G_de2(m,nnn)=de(2)
c		G_f2(m,nnn)=gs(2)
c		G_df2(m,nnn)=df(2)
          ! 增加了第三组截面内力、变形，可以把弱轴方向也考虑进来，用于三维模型。
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

				Column1(I,J)%F_S(:)%epss_pre=Column1(I,J)%F_S(:)%epss             ! 更新内部变量
				Column1(I,J)%F_S(:)%sigs_pre1=Column1(I,J)%F_S(:)%sigs_pre        ! 更新内部变量
				Column1(I,J)%F_S(:)%sigs_pre=Column1(I,J)%F_S(:)%sigs             ! 更新内部变量
				Column1(I,J)%F_S(:)%epssmax_P_pre=Column1(I,J)%F_S(:)%epssmax_P   ! 更新内部变量
				Column1(I,J)%F_S(:)%epssmax_N_pre=Column1(I,J)%F_S(:)%epssmax_N   ! 更新内部变量
				Column1(I,J)%F_S(:)%deps_pre=Column1(I,J)%F_S(:)%deps             ! 更新内部变量


				Column1(I,J)%F_C(:)%epsc_pre=Column1(I,J)%F_C(:)%epsc       ! 更新内部变量
				Column1(I,J)%F_C(:)%sigc_pre=Column1(I,J)%F_C(:)%sigc       ! 更新内部变量
				Column1(I,J)%F_C(:)%epsmax_pre=Column1(I,J)%F_C(:)%epsmax   ! 更新内部变量	

				Column1(I,J)%F_C(:)%epsmax_t_pre=Column1(I,J)%F_C(:)%epsmax_t   ! 更新内部变量

				do K=1,size(Column1(I,J)%F_C)								 !新加部分，保证混凝土压碎失效
					if(Column1(I,J)%F_C(K)%epsmax_pre<-0.05) then
						Column1(I,J)%F_C(K)%e0=0.001*Column1(I,J)%F_C(K)%e0
						Column1(I,J)%failure=1
					end if
				end do


				do K=1, size(Column1(I,J)%F_S)								! 对所有钢筋单元循环
		
					if((Column1(I,J)%F_S(K)%epss_pre)>0.1) then				
						Column1(I,J)%F_S(K)%es=0.001*Column1(I,J)%F_S(K)%es	 !新加部分，保证钢筋失效
						Column1(I,J)%failure=1
					end if	

					x=Column1(I,J)%F_S(K)%fy/Column1(I,J)%F_S(K)%Es;		! 得到钢筋的屈服应变
					if( abs( Column1(I,J)%F_S(K)%epss_pre ) >=x ) then		! 如果钢筋应变大于屈服应变
						Column1(I,J)%yield=1								! 钢筋屈服
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
     * dtemp,ngens,m,nnn,mat) ! MARC ubeam程序接口
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

	subroutine ReadData() ! 从matcode文件中读入纤维数据
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
			open (55,file='matcode.txt')	! 读入用户自定义截面对应哪些材料编号
			read(55,*) MatNumber(0)			! 一共有多少个截面
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
		            read(55,*)Column_Mat(II)%F_S(I)%x,                        ! 读入钢筋纤维的坐标（x，y）和面积
     1                          Column_Mat(II)%F_S(I)%y,
     2				          Column_Mat(II)%F_S(I)%as				
		            read(55,*)Column_Mat(II)%F_S(I)%fy,                       ! 读入钢筋纤维的抗拉屈服强度（正值）
     2					      Column_Mat(II)%F_S(I)%es                        ! 读入钢筋纤维的弹模
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
					Column_Mat(II)%F_S(I)%epssmax_P_pre=Column_Mat(II)        ! 对钢筋纤维最大拉应变赋初值
	1				      %F_S(I)%fy/Column_Mat(II)%F_S(I)%es;
					Column_Mat(II)%F_S(I)%epssmax_N_pre=-Column_Mat(II)       ! 对钢筋纤维最大压应变赋初值
	1				      %F_S(I)%epssmax_P_pre/Column_Mat(II)%F_S(I)%P4;
				end do

			open(66,file='debug.txt',position='append')
				write(66,'(A,I4,A,2G13.5)')	'材料编号',II,'   截面宽高',
	1				Column_Mat(II)%b,Column_Mat(II)%H
				write(66,*)
				write(66,*)  
	1	"混凝土纤维X,  Y,      Area,      fc,   EPS0,
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
			write(66,*) "钢筋纤维X,      Y,        Area,        fy,        Es"
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

	SUBROUTINE UBGINC(INC,INCSUB) ! 赋予总体参数
		use My_Var
		use Typ_Section
		IMPLICIT REAL *8 (A-H, O-Z)
		common/creeps/cptim,timinc,timinc_p,timinc_s,timincm,
     1	timinc_a,timinc_b,creept(33),icptim,icfte,icfst,
     2	icfeq,icftm,icetem,mcreep,jcreep,icpa,icftmp,icfstr,
     3	icfqcp,icfcpm,icrppr,icrcha,icpb,iicpmt,iicpa
		integer :: I,J,II
		if(FirstStep.ne.123.and.cptim.eq.0.and.timinc.eq.0 ) then ! 第一次计算
			FirstStep=123
			Current_Int_ElemNo=0;
			Current_Glb_ElemNo=0;
			call ReadData() ! 读入数据
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
		v=real(Column1(k,nn)%yield)  ! 输出塑性铰
	end if

	if(inc==1 .and. inf(m(1))==0 .and. ndi==1) then          !除了壳单元和ubeam单元外的单元(杆单元和beam单元）
	   if(nn==2) then
	      this(m(1))=1                                       !beam单元的this值为1
	   end if
	end if

	if(inf(m(1))==0 .and. ndi==1 
     1         .and. this(m(1))==0)  then                 !如果是truss单元

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
	     v=real(trussfailure1(m(1)))   !输出truss单元塑性铰
      end if  	   

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c		if(jpltcd==6) then				! 求解 ubeam单元能量%%%
c		if( inf( m(1) ) .ne. 0) then      ! 只有 ubeam单元才能进入以下程序段，才能按下面方法求解单元能量

c			CALL ELMVAR(69,m(1),nn,1,VAR_UBEAM)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c			LL_beam=0.400			   	         !框架梁单元长度, 需要根据实际 单元的长度修改
c			LL_beammid=0.270	
c			LL_columndown=0.205				     !框架柱单元长度，同上
c			LL_columnup=0.4
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c			CALL NODVAR(0,lm(1),VALNO,NQNCOMP,NQDATATYPE)		! 每个ubeam 单元的第一个节点坐标
c			coord_f(1,1)=valno(1)
c			coord_f(1,2)=valno(2)
c			coord_f(1,3)=valno(3)

c			CALL NODVAR(0,lm(2),VALNO,NQNCOMP,NQDATATYPE)		! 每个ubeam 单元的第二个节点坐标
c			coord_f(2,1)=valno(1)
c			coord_f(2,2)=valno(2)
c			coord_f(2,3)=valno(3)

c		coord_f_Y(m(1))=( coord_f(1,2)+coord_f(2,2) )/2.0		! 每个 ubeam单元两节点纵坐标的平均值，供后续判断单元的位置时使用
c		coord_f_X(m(1))=( coord_f(1,1)+coord_f(2,1) )/2.0		! 每个 ubeam单元两节点横坐标的平均值，同上

		!求解在每个加载步后 该ubeam单元所增加的能量及弹性能量 （按高斯点区分）%%%%
		! 以下暂时只计算了框架梁的能量，因此涉及长度时均采用LL_beam；
		! 如果还要计算框架柱单元，应该先判断单元是梁还是柱（可以利用上面的坐标信息），如果是柱，长度采用LL_column

c			if ( nn==1.or.nn==3 ) 	then	! 框架梁单元 1,3 高斯点
     			
c				G_nn(m(1),nn)= (G_f1(m(1),nn)-0.5*G_df1(m(1),nn))*		! 总能量增量
c	1			VAR_UBEAM*G_de1(m(1),nn)+
c	2			(G_f2(m(1),nn)-0.5*G_df2(m(1),nn))*
c	3			VAR_UBEAM*G_de2(m(1),nn)+
c	4			(G_f3(m(1),nn)-0.5*G_df3(m(1),nn))*
c	5			VAR_UBEAM*G_de3(m(1),nn)
c     		
c			E_nn(m(1),nn)=VAR_UBEAM*Column1(m(1),nn).Elem_Energy     !弹性能量
c		
c			else if ( nn==2 ) then			! 框架梁单元 2 高斯点
c			
c				G_nn(m(1),nn)=(G_f1(m(1),nn)-0.5*G_df1(m(1),nn))*
c	1			VAR_UBEAM*G_de1(m(1),nn)+
c	2			(G_f2(m(1),nn)-0.5*G_df2(m(1),nn))*
c	3			VAR_UBEAM*G_de2(m(1),nn)+
c	4			(G_f3(m(1),nn)-0.5*G_df3(m(1),nn))*
c	5			VAR_UBEAM*G_de3(m(1),nn)
	
c			E_nn(m(1),nn)=VAR_UBEAM*Column1(m(1),nn).Elem_Energy

c               end if 




        !   当每个ubeam单元的第3个积分点截面调用完这一段后，可以对该单元的各截面能量进行汇总
c		  if ( nn==3 )  then
 
              !  求解每个单元在 每个加载步结束后的总能量增量和弹性能量 （将各高斯点截面区段相加）
c				denergy(m(1))=0.
c				denergy(m(1))=G_nn(m(1),1)+G_nn(m(1),2)+G_nn(m(1),3)
c				dene_E(m(1))=0.
c				dene_E(m(1))=E_nn(m(1),1)+E_nn(m(1),2)+E_nn(m(1),3)


			! 求解每个单元在 每个加载步结束后的累积总能量
c			  if(inc>1) then
c				energy(m(1))=energy_pre(m(1))+denergy(m(1))
c	
c			  else if(inc==1) then
c				energy(m(1))=denergy(m(1))
c			  end if
             
		    !  将该单元更新后的总能量存储到energy_pre(:)，供下一步计算总能量时使用		
c				energy_pre(m(1))= energy(m(1))    


				! 每个梁单元 在 每个加载步结束后的弹性应变能
c				ene_E(m(1)) = dene_E(m(1))

				! 每个梁单元在每个加载步结束后的塑性应变能
c				ene_P(m(1)) = energy(m(1))-ene_E(m(1))


c		   end if					! if( nn==3 )


ccccccccccccccccccccccccccccccc qiu  ccccccccccccccccccccccccccccccccccccc

		!特别注意：该模型中0-486是ubeam单元，443-490是弹性质量柱单元，491-1378是分层壳单元，计算前注意修改
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c		if(m(1)==404 .and. nn==1 )then          !! 根据最后一个Ubeam单元编号486进行 修改***
			! 当模型最后一个ubeam单元( 本模型为442号单元 )调用了本段程序之后，进行各层单元能量汇总计算 

c				G_fbeam_storey(1:24)=0.
c				E_fbeam_storey(1:24)=0.
c				P_fbeam_storey(1:24)=0.
c
c				G_fcol_storey(1:24)=0.
c				E_fcol_storey(1:24)=0.
c				P_fcol_storey(1:24)=0.

ccccccccccccccccccccccccccccccc test_qiu 这部分是调试程序时使用的 cccccccccccccccccccccccccccccccccccccccccc
c		if (m(1)==924 .and. nn==3) then
c		open (735,file="test.txt",position='APPEND')
c		write(735,'(25G15.5)') INC,m(1)
c		write(735,'(25G15.5)') INC,G_f1(m(1),nn),G_df1(m(1),nn),G_de1(m(1),nn)
c		close(735)		
c		end if
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c		   do j=1,924	
c		   do j=1,numel	

			! 统计所有ubeam单元中 各层框架梁的能量，
			! 根据梁单元两节点的纵坐标平均值coord_f_Y(j) 与每一层的层高coordY1 的大小关系来判定该单元属于哪一层的梁
			  
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

			! 同样，可以统计所有ubeam单元中 各层框架柱的能量，
			! 根据柱单元两节点的纵坐标平均值coord_f_Y(j) 与每一层的层高coordY1 的大小关系来判定该单元属于哪一层的柱

			!	。。。。
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
		DIMENSION Driftx(20)          ! 角点1 在x向地震作用下层间位移
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

	!!!!!!!!!!!两层框架结构层间位移输出!!!!!!!!!

			CALL NODVAR(1,8391,VALNO,NQNCOMP,NQDATATYPE)    !!!柱1的层间位移
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
	        
			CALL NODVAR(1,8461,VALNO,NQNCOMP,NQDATATYPE)    !!!柱2的层间位移
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
			
		    CALL NODVAR(1,8259,VALNO,NQNCOMP,NQDATATYPE)    !!!柱3的层间位移
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
	write(721,'(7G15.5)') INC,driftx(1:2),driftx(11:12),driftx(15:16)      !!!注意修改输出数据个数
		close(721)

		open (722,file="frame_drifty.txt",position='APPEND')
	write(722,'(7G15.5)') INC,drifty(1:2),drifty(11:12),drifty(15:16)
		close(722)

	       !!!!!!!!!!!!SRC柱层间位移计算及输出!!!!!!!!!!!!!!!!!
			
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

c	计算前，需要根据模型实际节点数对i进行调整

c		do i=1, 390                             ! 要根据模型实际总结点数进行调整
c	        call nodvar(0,i,VALNO,NQNCOMP,NQDATATYPE)

c	计算前，需要对底层节点坐标值，进行修改确认

c			if (abs(valno(2)+0.001)<0.1) then   !通过Y坐标过滤出底层结点5700需要修改
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

c	对输出位移的列数进行修改，防止出现很多0的情况

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