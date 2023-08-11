      subroutine mcmc(nstates, states_init, param, nparam, nb,
     $     times, ntimes, t0, dt, trajectory, naccum, iaccum,
     $     parmin, parmax, step, wl,
     $     ind_opt,nopt,obs, gamaObs, wght, nMCMC, ithin, iseed, tab,
     $     imodel)

      implicit none
      integer nstates, nparam, nb, ntimes, naccum, imodel
      integer iaccum(naccum)
      real*8 states_init(nstates)
      real*8 param((nparam + 2*nb))
      real*8 times(ntimes)
      real*8 t0, dt, sum_ts
      real*8 trajectory(ntimes, nstates)
      real*8 wl
      
!     States will be orderes as: sS, sI, sE, sR, sH, Ic, Ih
!     Ic and Ih will be zeroed at the begining of each time step)

      real*8 parmin((nparam+2*nb)), parmax((nparam+2*nb))
      real*8 step((nparam+2*nb))
      
      real*8 obs(ntimes), gamaObs(ntimes), wght(ntimes)
      integer nopt, ind_opt(nopt), nMCMC, ithin, iseed
      
      real tab((nMCMC/ithin), (nparam+2*nb+1))
      
      real*8 scale, range_min, range_max, step_min, step_max
      integer ionep, iadapt
      integer nlines, nparam_tot
      integer ii, jj, iaccept, icount
      integer ind_rho, ind_bsln
      real*8 cases(ntimes), curLLK, obsLLK, curMin, newLLK
      real*8 casesBest(ntimes)
      
      real*8 curpars((nparam + 2*nb)), parupdt((nparam+2*nb))
      real*8 parbest((nparam+2*nb))
      real*8 savepar((nparam+2*nb)), copypar((nparam+2*nb))
      real*8 savestep((nparam+2*nb))
      
      real*8 trajBest(ntimes, nstates)
      real*8 rv, myaccept
      logical accept

      real*8  calcfit
      external calcfit

      real*8 mu
      integer ZBQLPOI
      external ZBQLPOI

!     total number of parameters

      nparam_tot = nparam + 2 * nb
      
! Number of lines in the tab array

      nlines = nMCMC/ithin

! For an adaptive size MCMC - decide if we need to update step size every 1% 
! or 1,000 steps
      scale = 2.0d0
      ionep = int(nMCMC * 0.01)
      ionep = min(1000, ionep)
      ionep = max(ionep, 1)
      range_min = 0.20d0
      range_max = 0.30d0
      step_max = 1.d0
      step_min = 1e-5
      iadapt = 0
      
! ran1 works with -iseed

      iseed = -abs(iseed)

      call srand(iseed)

!     ensure that the last time change does not exceed number of days

      param(nparam_tot) = 0.0d0
      sum_ts = sum(param((nparam+nb+1):nparam_tot))

      if (sum_ts > ntimes) Then
         param((nparam+nb+1):nparam_tot) =
     $        param((nparam+nb+1):nparam_tot) * dble(ntimes)/sum_ts
      endif
      
!     calculate initial profile      
      curpars = param

!     imodel = 1, COVID, SEIRH
!     imodel = 2, INFLUENZA, SIRH

      
      if (imodel == 1) Then
         call detseirh(nstates, states_init, param, nparam, nb, times,
     $        ntimes, t0, dt, naccum, iaccum, trajectory, wl)
         ind_rho = 6
         ind_bsln = 7
      else
         call detsirh(nstates, states_init, param, nparam, nb, times,
     $        ntimes, t0, dt, naccum, iaccum, trajectory, wl)
         ind_rho = 5
         ind_bsln = 6
      endif
      
      
!     cases = Ih * rho + 1e-6
!     order of parameters is  pop, gamma, pH, mu_H1H2, mu_EI, rho, baseline, all beta 
!                              1     2     3     4      5     6      7   
      do ii = 1, ntimes
         mu = trajectory(ii,nstates) * param(ind_rho)  +
     $        1.e-6 + param(ind_bsln)
         cases(ii) = dble(ZBQLPOI(mu))
      enddo

!
! calculate Likelihood of the solution
!

!     This is the best scenario basically
      obsLLK = calcfit(obs,gamaObs,obs+1e-6,wght,ntimes)
!     Initial LLK     
      curLLK = calcfit(obs,gamaObs,cases+1e-6,wght,ntimes)

c$$$      print*, 'Initial LLK',curLLK
      
!
! MCMC loop starts here 
!
      curMIN = curLLK
      casesBest = cases

      iaccept = 0
      icount = 0

      savestep = step
     
      parupdt = curpars      
      savepar = curpars
      copypar = curpars

      do ii = 1, nMCMC
         
         savepar = curpars
         copypar = curpars       
!     Propose a new value for each parameter we are optimizing 
         call fnProposeParamUpdates(nparam_tot,copypar,
     $        parmin,parmax,step,parupdt,iseed,nopt,ind_opt)

         parupdt(nparam_tot) = 0.0d0
         sum_ts = sum(parupdt((nparam+nb+1):nparam_tot))
         
         if (sum_ts > ntimes) Then
            parupdt((nparam+nb+1):nparam_tot) =
     $           parupdt((nparam+nb+1):nparam_tot) * dble(ntimes)/sum_ts
         endif
         
         curpars(ind_opt) = parupdt(ind_opt)

!     calculate a new profile
         if (imodel == 1) Then
            call detseirh(nstates, states_init, curpars, nparam, nb,
     $           times, ntimes, t0, dt, naccum, iaccum, trajectory, wl)
         else
            call detsirh( nstates, states_init, curpars, nparam, nb,
     $           times, ntimes, t0, dt, naccum, iaccum, trajectory, wl)
         endif
         
         do jj = 1, ntimes
            mu = trajectory(jj,nstates) * param(ind_rho)  +
     $           1.e-6 + param(ind_bsln)
            cases(jj) = dble(ZBQLPOI(mu))
         enddo
         
         newLLK = calcfit(obs,gamaObs,cases+1e-6,wght,ntimes)

           rv = rand()

           if (exp((curLLK - newLLK)) .gt. rv ) Then
              accept = .true.
              iaccept = iaccept + 1
              iadapt = iadapt + 1
              curLLK = newLLK
              savepar = curpars
              casesBest = cases
              trajBest  = trajectory
              parbest = curpars
           else
              curpars = savepar
           endif

        if (mod(ii,ithin) .eq. 0) Then
            icount = icount + 1
            tab(icount,1:nparam_tot)   = real(curpars)
            tab(icount,(nparam_tot+1)) = real(curLLK)
           
         endif

         if (mod(ii, 100000) .eq. 0) Then
            print*,int(real(ii)/real(nMCMC) * 100.0),'% MCMC Completed'
         endif
         
c$$$         if (mod(ii, 100000) .eq. 0) then
c$$$            print *, ii, iaccept, real(curLLK), real(curpars(ind_opt))
c$$$         end if
         
         if (mod(ii,ionep) .eq. 0) Then
            
            myaccept = dble(iadapt)/dble(ionep)
            if (myaccept > range_max .and.
     $           all(step(ind_opt)*scale < step_max)) Then
                 step(ind_opt)= step(ind_opt)*scale
               endif
               if (myaccept < range_min .and.
     $         all(step(ind_opt)/scale > step_min)) Then
                 step(ind_opt)= step(ind_opt)/scale
               endif
               iadapt = 0    !reset acceptance number

         endif
         
      enddo

      param = parBest             ! return the best estimate for the parameters
      trajectory = trajBest
      return
      end subroutine mcmc

!--------------------------------------------------------------------------------
        function calcfit(y,gamay,x,wght,ndata)

        implicit none

        integer ndata, i
        real*8 y(ndata),x(ndata), gamay(ndata)
        real*8 wght(ndata)
        real*8 xi, yi,my_sum,val,calcfit


c x is the simulated data
c y is the base profile

C calculate the P(yi,xi)


        my_sum = 0.0
        do i=1,ndata
           
           yi = y(i)
           xi = x(i)
           
           val = yi * log(xi) - xi - gamay(i)           
           my_sum = my_sum + val  * wght(i) 
           
        enddo
! helps get the width we know from daily fits
        calcfit = -my_sum / SUM(wght)
           
        return
        end function calcfit


!----------------------------------------------------------------

      subroutine fnProposeParamUpdates(nparam,curval,
     $     valmin,valmax,step,parupdt,iseed,nopt,ind_opt)

      implicit none
      integer nparam, nopt, ind_opt(nopt)
      real*8 curval(nparam),valmin(nparam),valmax(nparam)
      real*8 parupdt(nparam),step(nparam)
      real*8 x, rv, rtn
      real*8 ran1
      real*8 SR_to_unit, SR_from_unit
      integer iseed
      integer i,j
      external SR_to_unit, SR_from_unit

      
       do j = 1, nopt
          i = ind_opt(j)
          parupdt(i)=curval(i)
          
          rv = rand()

          rv = (rv - 0.50d0)*step(i)

! convert to a zero - one scale

          x = SR_to_unit(curval(i),valmin(i),valmax(i))

          x = x + rv
          
          if (x .lt. 0.0d0) x = 1.0d0 + x
          if (x .gt. 1.0d0) x = x - 1.0d0

! Do not use period boundary conditions here but rather re-smaple
c$$$      if (x .le. 0.0d0 .or. x .ge. 1.0d0) go to 101


! bring value back to original scale
      
         rtn = SR_from_unit(x,valmin(i),valmax(i))

         parupdt(i) = rtn

      enddo

      return
      end subroutine fnProposeParamUpdates


c----------------------------------------------------------------

      function SR_to_unit(y,ymin,ymax)

      implicit none
      real*8 y, ymin,ymax,rtn,SR_to_Unit

 
      rtn = (log(y) - log(ymin)) /
     $     (log(ymax)-log(ymin))
 
      SR_to_unit = rtn
      
      return
      end function SR_to_unit

c----------------------------------------------------------------

      function SR_from_unit(x,ymin,ymax)
      
      implicit none
   
      real*8 x,ymin,ymax,rtn,SR_from_unit


         rtn = ymin * 
     $        exp(x*(log(ymax)-log(ymin)))


      SR_from_unit = rtn

      return
      end function SR_from_unit

C--------------------------------------------------------------------------------
      
