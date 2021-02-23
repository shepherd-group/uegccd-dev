
      Program TermedCC
      Use IO
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


      Subroutine Do_HF(UEGInput)
            Use IO
            Use HEG
            Use Precision
            Use Constants
            type(UEG_Input) :: UEGInput
   
            print *, "Doing HF calculation"
!!            print *, "Currently not implamented. Exiting"

            Call Init_HEG_dummy(UEGInput%MaxKPoint,UEGInput%NElectron)
            Call change_rs(UEGInput%Rs) 
            UEGInput%NAO = nBasis
            
      End Subroutine Do_HF
 
 
      Subroutine Do_MP2(UEGInput)
   
            Use Precision
            Use IO
            Use MP2
            Use CCD
            Implicit None
      ! Dimensioning variables
            type(UEG_Input) :: UEGInput
      ! Correlated stuff
            Real (Kind=pr) :: ECorr
            Real (Kind=pr), Allocatable :: X2aaaa(:,:,:), X2abab(:,:,:), X2abba(:,:,:)
      ! Error checking variables
            Integer, Parameter :: NAlloc = 6
            Integer :: IAlloc(NAlloc)
            Logical, Parameter :: T = .true., F=.false.

            print *, "Doing MP2 calculation"
           !! print *, "Currently not implamented. Exiting"


            Call Do_HF(UEGInput)
      !==========================================!
      !  Now we can allocate the memory and go!  !
      !==========================================!
      
            IAlloc = 0
            Allocate(X2aaaa(UEGInput%NOcc,UEGInput%NOcc,UEGInput%NOcc+1:UEGInput%NAO),  Stat=IAlloc(4))
            Allocate(X2abab(UEGInput%NOcc,UEGInput%NOcc,UEGInput%NOcc+1:UEGInput%NAO),  Stat=IAlloc(5))
            Allocate(X2abba(UEGInput%NOcc,UEGInput%NOcc,UEGInput%NOcc+1:UEGInput%NAO),  Stat=IAlloc(6))
            If(Any(IAlloc /= 0)) Stop "Could not allocate in main"
            Open(7,File='Output',Status="Replace")
            Close(7)
      
      !=====================================================================!
            
            Call DrvMBPT(Eigen,X2aaaa,X2abab,X2abba,UEGInput%NOcc,UEGInput%NAO,EHF,ECorr)
            
      End Subroutine Do_MP2

 
      Subroutine Do_CCD(UEGInput)

            Use Precision
            Use IO
            Use MP2
            Use CCD
            Implicit None
      ! Dimensioning variables
            type(UEG_Input) :: UEGInput
      ! Correlated stuff
            Real (Kind=pr) :: ECorr
            Real (Kind=pr), Allocatable :: T2aaaa(:,:,:), T2abab(:,:,:), T2abba(:,:,:)
            Real (Kind=pr), Allocatable :: X2aaaa(:,:,:), X2abab(:,:,:), X2abba(:,:,:)
      ! Error checking variables
            Integer, Parameter :: NAlloc = 6
            Integer :: IAlloc(NAlloc)
            Logical, Parameter :: T = .true., F=.false.
      
 
            print *, "Doing CCD calculation"
            !!Call Do_HF(UEGInput)
            Call Do_MP2(UEGInput)

      !==========================================!
      !  Now we can allocate the memory and go!  !
      !==========================================!
      
            IAlloc = 0
            Allocate(T2aaaa(UEGInput%NOcc,UEGInput%NOcc,UEGInput%NOcc+1:UEGInput%NAO),  Stat=IAlloc(1))
            Allocate(T2abab(UEGInput%NOcc,UEGInput%NOcc,UEGInput%NOcc+1:UEGInput%NAO),  Stat=IAlloc(2))
            Allocate(T2abba(UEGInput%NOcc,UEGInput%NOcc,UEGInput%NOcc+1:UEGInput%NAO),  Stat=IAlloc(3))
            Allocate(X2aaaa(UEGInput%NOcc,UEGInput%NOcc,UEGInput%NOcc+1:UEGInput%NAO),  Stat=IAlloc(4))
            Allocate(X2abab(UEGInput%NOcc,UEGInput%NOcc,UEGInput%NOcc+1:UEGInput%NAO),  Stat=IAlloc(5))
            Allocate(X2abba(UEGInput%NOcc,UEGInput%NOcc,UEGInput%NOcc+1:UEGInput%NAO),  Stat=IAlloc(6))
            If(Any(IAlloc /= 0)) Stop "Could not allocate in main"
            Open(7,File='Output',Status="Replace")
            Close(7)
      
      !=====================================================================!
            
            T2aaaa = X2aaaa
            T2abab = X2abab
            T2abba = X2abba
              
            Call DrvCCD(Eigen,T2aaaa,T2abab,T2abba,UEGInput%NOcc,UEGInput%NAO,EHF,ECorr, &
                         UEGInput%DoRing,UEGInput%DoXRing,UEGInput%DoLadder,UEGInput%DoMosaic, &
                         UEGInput%IRangeRing,UEGInput%IRangeXRing,UEGInput%IRangeLadder,UEGInput%IRangeMosaic, &
                         UEGInput%IRangeDriverDirect,UEGInput%IRangeDriverExchange,UEGInput%IRangeEnergy,       &
                         UEGInput%IRangeLinRings,UEGInput%IRangeQuadRings,UEGInput%IRangeDirectRings,  &
                         UEGInput%IRangeExchangeRings, &
                         UEGInput%IRangeLinLadders,UEGInput%IRangeQuadLadders,UEGInput%IRangeDirectLadders,  &
                         UEGInput%IRangeExchangeLadders)     
  
      !==============================================!
      !  Lastly, deallocate memory and exit safely.  !
      !==============================================!
      
            DeAllocate(T2aaaa,    Stat=IAlloc(1))
            DeAllocate(T2abab,    Stat=IAlloc(2))
            DeAllocate(T2abba,    Stat=IAlloc(3))
            DeAllocate(X2aaaa,    Stat=IAlloc(4))
            DeAllocate(X2abab,    Stat=IAlloc(5))
            DeAllocate(X2abba,    Stat=IAlloc(6))
            If(Any(IAlloc /= 0)) Stop "Could not deallocate in main"
 
      End Subroutine Do_CCD
 
  
      Subroutine Do_TACCD(UEGInput)
            Use IO
            type(UEG_Input) :: UEGInput
   
            print *, "Doing TA-CCD calculation"
            print *, "Currently not implamented. Exiting"
            Stop
      End Subroutine Do_TACCD
 
 
      Subroutine Do_cTACCD(UEGInput)
            Use IO
            type(UEG_Input) :: UEGInput
   
            print *, "Doing cTA-CCD calculation"
            print *, "Currently not implamented. Exiting"
            Stop
      End Subroutine Do_cTACCD

