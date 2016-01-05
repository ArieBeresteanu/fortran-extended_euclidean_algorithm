!     main.f95 - Program that calculate the solution of Bezout's identity
!     with extended euclidean algorithm.
!
!     created: 05/01/2015
!
!     author: albert r carnier guedes ( albert@teko.net.br )
!
      PROGRAM EXTENDED_EUCLIDEAN_ALGORITHM

      integer p,q,m,n
      
      read*, p,q

      call EEA(p,q,m,n)

      print*, m,n
      
      END PROGRAM EXTENDED_EUCLIDEAN_ALGORITHM

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FUNCTIONS AND SUBROUTINES     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
!     Subroutine that calculate the solution of Bezout's identity
!     and save result on variables 'm' and 'n' given.
!
      SUBROUTINE EEA(p,q,m,n)

      integer p,q,m,n
      integer t,x,y,z0,z1,w0,w1
      integer temp
      
      x=p
      y=q
      
!     Verify if 'p' is lower than 'q'
      if(p.lt.q) then
         x=q
         y=p
      end if

      z0=1
      z1=0
      
      w0=0
      w1=1
      
!     Iteratively calculates the remainder of the division of 'x' by 'y'.
      do while( y.ne.0 )

         r = modulo(x,y)

!     't' is the module of 'x = r + t*y'
         t = (x-r)/y

!     Renew 'x' and 'y' with new values.
         x = y
         y = r

!     Apply recursive formula for EEA to find 'm'
         temp = z1
         z1 = z0 - t*z1
         z0 = temp

!     Apply recursive formula for EEA to find 'n'         
         temp = w1
         w1 = w0 - t*w1
         w0 = temp
                  
      enddo

      m=z0
      n=w0
      
      END SUBROUTINE EEA
