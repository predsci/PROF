      subroutine detsirh(nstates, states_init, param, nparam, nb,
     $     times, ntimes, t0, dt, naccum, iaccum, trajectory, wl)

      implicit none
      real*8 wl
      integer nstates, nparam, nb, ntimes, naccum
      integer iaccum(naccum)
      real*8 states_init(nstates)
      real*8 param((nparam+2*nb))
      real*8 times(ntimes)
      real*8 t0, dt
      real*8 beta_cur
      real*8 trajectory(ntimes, nstates)
      real*8 y(nstates), yt(nstates)
      real*8 dydx(nstates), dyt(nstates), dym(nstates)
      real*8 hh, h6, xh, tiny, myt
      real*8 beta(nb), ts(nb)
      real*8 pop, gamma, pH, mu_H1H2, sI0, time0
      integer icount, k
      real*8 mu
      
!     States will be orderes as: sS, sI, sR, sH, Ic, Ih
!     Ic and Ih will be zeroed at the begining of each time step

!     order of parameters is  pop, gamma, pH, mu_H1H2, rho, baseline, I0, time0, all beta 
!                              1     2     3     4      5     6       7      8      
      pop   = param(1)
      gamma = param(2)
      pH    = param(3)
      mu_H1H2 = param(4)
      sI0     = param(7)
      time0 = param(8)
      
      trajectory = 0.0d0

      yt = 0.0d0
      dydx = 0.0d0
      dyt = 0.0d0
      dym = 0.0d0
      
! initialize      
      y = states_init

      y(1) = pop - sI0
      y(2) = sI0
      y(3) = pop - (y(1) + y(2))
     

      hh = dt * 0.5
      h6 = dt/6.0d0
      xh = t0 + hh


      
      do k = 1, nb
         beta(k) = param(nparam+k)
         ts(k)   = param(nparam+nb +k)
      enddo

! convert from absolute days in each R(t) value to relative number of days

      do k = 2, (nb-1)
         ts(k) = ts((k-1)) + ts(k)
      enddo

      !     propagate states without recording of data until 'time0' which is the start
!     of observations and fitting

      myt = 0.0d0

      do while (myt <= nint(time0))
         beta_cur = beta(1)
         call derivs_sirh(y, nstates, pop, gamma, pH, mu_H1H2,
     $        beta_cur, dydx)
         yt = y + hh *dydx
         call derivs_sirh(yt, nstates, pop, gamma, pH, mu_H1H2,
     $        beta_cur, dyt)
         yt = y + hh * dyt
         call derivs_sirh(yt, nstates, pop, gamma, pH, mu_H1H2,
     $        beta_cur, dym)
         yt = y + dt * dym
         dym = dyt + dym
         call derivs_sirh(yt, nstates, pop, gamma, pH, mu_H1H2,
     $        beta_cur, dyt)
         y = y + h6 *(dydx + dyt + 2.0d0 * dym)
         myt = myt + dt
!     it is OK to simply zero Ic and Ih since time0 is rounded to the nearest integer                 
         y(iaccum) = 0.0d0
             
      enddo

      myt = t0
      tiny = 1.e-12
      icount = 1
      
      do while (icount <= ntimes)

         beta_cur = (beta(1) + beta(nb))
         do k = 2, nb
            beta_cur = beta_cur + (beta(k) - beta((k-1))) *
     $           tanh((myt-ts((k-1)))/wl)
         enddo

         beta_cur = beta_cur * 0.5
         
         call derivs_sirh(y, nstates, pop, gamma, pH, mu_H1H2,
     $        beta_cur, dydx)
         yt = y + hh *dydx
         call derivs_sirh(yt, nstates, pop, gamma, pH, mu_H1H2,
     $        beta_cur, dyt)
         yt = y + hh * dyt
         call derivs_sirh(yt, nstates, pop, gamma, pH, mu_H1H2,
     $        beta_cur, dym)
         yt = y + dt * dym
         dym = dyt + dym
         call derivs_sirh(yt, nstates, pop, gamma, pH, mu_H1H2,
     $        beta_cur, dyt)
         y = y + h6 *(dydx + dyt + 2.0d0 * dym)
         myt = myt + dt
         if ((myt + dt) > times((icount))) Then           
            trajectory(icount,:) = y
            y(iaccum) = 0.0d0
             icount = icount + 1
         endif
            
      enddo


      return
      end subroutine detsirh

      subroutine derivs_sirh(y, nstates, pop, gamma, pH, mu_H1H2,
     $     Beta, dy)

      implicit none
      integer nstates
      real*8 y(nstates)
      real*8 dy(nstates)
      real*8 pop, gamma, pH, Beta, mu_H1H2
      real*8 rate
      
     
      rate = Beta * y(1) * y(2) /pop

      ! S
      dy(1) = -rate
      ! I
      dy(2) = rate - gamma*y(2)
      ! R
      dy(3) = (1.0d0-pH)*gamma*y(2) 
      ! H1
      dy(4) = pH * gamma * y(2) - mu_H1H2 * y(4)      
      ! H2 
      dy(5) =  mu_H1H2 * y(4)
      ! Ic
      dy(6) = rate
      ! Ih
      dy(7) = mu_H1H2 * y(4)

      return

      end subroutine derivs_sirh
      
      

         
      
      
      
      
      
     
