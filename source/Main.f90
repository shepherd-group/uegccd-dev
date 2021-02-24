
      Program TermedCC
      Use IO
      Use DoCalc
      Implicit None
! Dimensioning variables
      type(UEG_Input) :: UEGInput
 
!==========================================!
!  This code implements RHF-based CCD.     !
!  We give it the option of keeping the    !
!  ring, ladder, crossed-ring, and mosaic  !
!  terms each on a case-by-case basis.     !
!------------------------------------------!
!  The first thing we must do is read the  !
!  basic information for the calculation.  !
!==========================================!
      
      Call ReadInput(UEGInput)
!==========================================!
!  calling a wrapper based on cases to     ! 
!  select the calculation we want to run   !
!  based on the "DoCal" input parameter.   !
!==========================================!
      Select Case (UEGInput%DoCalc)
         Case ('HF')
            Call Do_HF(UEGInput)
            Stop
   
   
         Case ('MP2')
            Call Do_MP2(UEGInput)
            Stop
   
   
         Case ('CCD')
            Call Do_CCD(UEGInput)
            Stop

         Case ('TA-CCD')
            Call Do_TACCD(UEGInput)
            Stop
   
   
         Case ('cTA-CCD')
            Call Do_cTACCD(UEGInput)
            Stop


         Case Default   !  Unknown keyword
            print *, "calculation ",UEGInput%DoCalc," not recognised"
            Stop

      End Select
      Stop
 
      End Program TermedCC

