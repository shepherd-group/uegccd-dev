
      Program TermedCC
      Use Precision
      Use IO
      Use MP2
      Use CCD
      Implicit None
! Dimensioning variables
!!      Integer :: NOcc, NAO
!!      Logical :: DoRing, DoXRing, DoLadder, DoMosaic
!!      Integer :: IRangeRing, IRangeXRing, IRangeLadder, IRangeMosaic
!!      Integer :: IRangeDriverDirect, IRangeDriverExchange, IRangeEnergy
!!      Integer :: IRangeLinRings, IRangeQuadRings, IRangeDirectRings, IRangeExchangeRings  
!!      Integer :: IRangeLinLadders, IRangeQuadLadders, IRangeDirectLadders, IRangeExchangeLadders
!!      Real (Kind=pr) :: Rs
! Correlated stuff
      Real (Kind=pr) :: ECorr
      Real (Kind=pr), Allocatable :: T2aaaa(:,:,:), T2abab(:,:,:), T2abba(:,:,:)
      Real (Kind=pr), Allocatable :: X2aaaa(:,:,:), X2abab(:,:,:), X2abba(:,:,:)
! Error checking variables
      Integer, Parameter :: NAlloc = 6
      Integer :: IAlloc(NAlloc)
      Logical, Parameter :: T = .true., F=.false.
 
!==========================================!
!  This code implements RHF-based CCD.     !
!  We give it the option of keeping the    !
!  ring, ladder, crossed-ring, and mosaic  !
!  terms each on a case-by-case basis.     !
!------------------------------------------!
!  The first thing we must do is read the  !
!  basic information for the calculation.  !
!==========================================!
      type UEGInput
          include "UEG_Input.f90"!!type(UEG_Input), Intent(Out) :: UEGInput
      end type UEGInput
      
      Call ReadInput(UEGInput)

!==========================================!
!  Now we can allocate the memory and go!  !
!==========================================!

      IAlloc = 0
      Allocate(T2aaaa(UEG_Inp%NOcc,UEG_Inp%NOcc,UEG_Inp%NOcc+1:UEG_Inp%NAO),  Stat=IAlloc(1))
      Allocate(T2abab(UEG_Inp%NOcc,UEG_Inp%NOcc,UEG_Inp%NOcc+1:UEG_Inp%NAO),  Stat=IAlloc(2))
      Allocate(T2abba(UEG_Inp%NOcc,UEG_Inp%NOcc,UEG_Inp%NOcc+1:UEG_Inp%NAO),  Stat=IAlloc(3))
      Allocate(X2aaaa(UEG_Inp%NOcc,UEG_Inp%NOcc,UEG_Inp%NOcc+1:UEG_Inp%NAO),  Stat=IAlloc(4))
      Allocate(X2abab(UEG_Inp%NOcc,UEG_Inp%NOcc,UEG_Inp%NOcc+1:UEG_Inp%NAO),  Stat=IAlloc(5))
      Allocate(X2abba(UEG_Inp%NOcc,UEG_Inp%NOcc,UEG_Inp%NOcc+1:UEG_Inp%NAO),  Stat=IAlloc(6))
      If(Any(IAlloc /= 0)) Stop "Could not allocate in main"
      Open(7,File='Output',Status="Replace")
      Close(7)

!=====================================================================!
      Call DrvMBPT(Eigen,X2aaaa,X2abab,X2abba,UEG_Inp%NOcc,UEG_Inp%NAO,EHF,ECorr)
!!      If(iRSPoint == 1) Then
      T2aaaa = X2aaaa
      T2abab = X2abab
      T2abba = X2abba
!!       End If
        
       Call DrvCCD(Eigen,T2aaaa,T2abab,T2abba,UEG_Inp)

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

      Stop
      End Program TermedCC

