
   Module IO

   Use Precision
   Use Constants
   Use HEG
   Implicit None
   !!Private
   Public  :: ReadInput
   type UEG_input   !! derived type for the inputs from the file "Input"
        Integer :: NOcc
        Integer :: NAO
        Real (Kind=pr) :: Rs !Density measured in Wigner-Seitz radius, r_s
        Logical :: DoRing   !T/F flag to turn off/on the rings
        Logical :: DoXRing  !T/F flag to turn off/on the cross-rings
        Logical :: DoLadder !T/F flag to turn off/on the ladders
        Logical :: DoMosaic ! T/F flag to turn off/on the mosaic
        Integer :: IRangeRing  
        Integer :: IRangeXRing
        Integer :: IRangeLadder
        Integer :: IRangeMosaic
        Integer :: IRangeDriverDirect
        Integer :: IRangeDriverExchange
        Integer :: IRangeEnergy
        Integer :: IRangeLinRings
        Integer :: IRangeQuadRings
        Integer :: IRangeDirectRings
        Integer :: IRangeExchangeRings
        Integer :: IRangeLinLadders
        Integer :: IRangeQuadLadders
        Integer :: IRangeDirectLadders
        Integer :: IRangeExchangeLadders
        Integer :: NElectron              !Number of electrons in the system
        Integer :: MaxKPoint
        Character(len = 20) :: DoCalc  !Input to choose which calc to run
   end type UEG_input

   Contains
      Subroutine ReadInput(UEG_Inp)
      Implicit None
      !!Integer, Parameteonrams = 22
      Integer, Parameter    :: NParams = 22
      Integer, Parameter    :: LName   = 19
      Integer, Parameter    :: LLine   = 79
      Logical               :: Error, Exists
      Character (len=5)     :: FormatString
      Character (len=LLine) :: Line, KeyWord, Value
      Character (len=LName) :: ParamName(NParams)
      Logical               :: SetOnce(NParams), SetTwice(NParams)
      Integer :: I, ExStatus, LineNumber
      type(UEG_Input), Intent(Inout) :: UEG_Inp  !!calls on the dervied type defined above 
!=====================================================================!
!  This is a pretty complicated subroutine for me, as I know little   !
!  about string-handling.  But here's what everything does.           !
!                                                                     !
!  We open Input and read it one line at a time.  Lines are assumed   !
!  to be either of the general form                                   !
!       Keyword: Value                                                !
!       Keyword:                                                      !
!  We want each of the possible keywords to be set exactly once, so   !
!  we keep track of which are set and of which are set twice.  We     !
!  also check to see if the user tried to set an invalid keyword,     !
!  telling him on which line he tried to do so.                       !
!                                                                     !
!  The parameters NParams, LName, and LLine mean:                     !
!     NParams: The number of keywords we expect to be set.            !
!     LName:   The length of the string naming each keyword.          !
!     LLine:   The length of a line.                                  !
!  Other variables are:                                               !
!     Charge:        The total charge of the system, read in.         !
!     Multiplicity:  The multiplicity of the system, read in.         !
!     CorFunc:       The correlation functional to be used, read in.  !
!     CorPot:        The correlation potential to be used, read in.   !
!     ExFunc:        The exchange functional to be used, read in.     !
!     ExPot:         The exchange potential to be used, read in.      !
!     Exists:        .True. if Input file exists.                     !
!     Error:         .True. if there is an error in the routine.      !
!     FormatString:  If LLine = xyz, FormatString = (Axyz).           !
!     Line:          The line we read in.                             !
!     Keyword:       The keyword we read in.                          !
!     Value:         The value corresponding to that keyword.         !
!     ParamName:     Array of acceptable keyword names.               !
!     SetOnce:       Array, .True. when the right keyword is set.     !
!     SetTwice:      Array, .True. if the keyword is multiply set.    !
!                                                                     !
!  After creating FormatString, we set the defaults for charge,       !
!  multiplicity, and Kohn-Sham stuff.                                 !
!=====================================================================!

      Write(6,*) 'Reading Input...'
      Write(FormatString,'(a2,i2,a1)') '(a',LLine,')'


!============================================================!
!  Initialize the name list, and the Input checking arrays.  !
!============================================================!

      ExStatus = 0
      SetOnce  = .False.
      SetTwice = .False.
      ParamName = (/'# Electrons        ',    &
                    'Momentum Cutoff    ',    &
                    'Rs                 ',    &
                    'Do Calculation     ',    & ! This sets the calculation type
                    'Do Rings           ',    &
                    'Do XRings          ',    &
                    'Do Ladders         ',    &
                    'Do Mosaics         ',    &
                    'Rings Range        ',    &
                    'XRings Range       ',    &
                    'Ladders Range      ',    &
                    'Mosaics Range      ',    &
                    'DriverDir Range    ',    &  
                    'DriverEx Range     ',    &   ! this implicitly sets the driver
                    'Energy Range       ',    &   ! for the energy expression
                    'LinRings Range     ',    &   ! for the linear rings and cross rings
                    'QuadRings Range    ',    &   ! for the quadratic rings and cross rings
                    'DRings Range       ',    &   ! for the dRPA terms
                    'ExRings Range      ',    &   ! for the RPAX terms
                    'LinLadd Range      ',    &   ! for the linear ladders
                    'QuadLadd Range     ',    &   ! for the quadratic ladders
                    'DLadders Range     ',    &   ! by analogy to rings 
                    'ExLadders Range    '/)       ! by analogy to rings


!================================================================!
!  Assuming the Input file exists, open it and get ready to go.  !
!================================================================!

      Inquire(File='Input',Exist=Exists)
      If(.not. Exists) Stop 'No Input file'
      Open(4,File='Input',Status='Old')
      LineNumber = 0

      Do
       LineNumber = LineNumber + 1
       Read(4,FormatString,End=10) Line
       Call ParseLine(Line,KeyWord,Value)

       Do I = 1,NParams        !  Input debugging checks...
        If(Trim(ParamName(I)) == Trim(AdjustL(KeyWord))) Then
         If(SetOnce(I))  SetTwice(I) = .True.
         If(SetTwice(I)) Write(6,1020) Trim(ParamName(I))
         SetOnce(I) = .True.
        End If
       End Do
       
       Select Case (Trim(AdjustL(Keyword)))     !  Setting the variables...
        Case ('SKIP')

        Case ('# Electrons')
         Read(Value,*) UEG_Inp%NElectron
         UEG_Inp%NOcc = UEG_Inp%NElectron/2
         If(UEG_Inp%NElectron < 0)         Stop 'Must have positive number of electrons'
         If(Mod(UEG_Inp%NElectron,2) == 1) Stop 'Must have closed shell'

        Case ('Momentum Cutoff')
         Read(Value,*) UEG_Inp%MaxKPoint

        Case ('Rs')
         Read(Value,*) UEG_Inp%Rs

        Case ('Do Calculation')
         Read(Value,*)UEG_Inp%DoCalc

        Case ('Do Rings')
         Read(Value,*) UEG_Inp%DoRing

        Case ('Do XRings')
         Read(Value,*) UEG_Inp%DoXRing

        Case ('Do Ladders')
         Read(Value,*) UEG_Inp%DoLadder

        Case ('Do Mosaics')
         Read(Value,*) UEG_Inp%DoMosaic

        Case ('Rings Range')
         Read(Value,*) UEG_Inp%IRangeRing

        Case ('XRings Range')
         Read(Value,*) UEG_Inp%IRangeXRing

        Case ('Ladders Range')
         Read(Value,*) UEG_Inp%IRangeLadder

        Case ('Mosaics Range')
         Read(Value,*) UEG_Inp%IRangeMosaic

        Case ('DriverDir Range') ! this implicitly sets the driver
         Read(Value,*) UEG_Inp%IRangeDriverDirect

        Case ('DriverEx Range') ! this implicitly sets the driver
         Read(Value,*) UEG_Inp%IRangeDriverExchange

        Case ('Energy Range') ! for the energy expression
         Read(Value,*) UEG_Inp%IRangeEnergy

        Case ('LinRings Range') ! for the linear rings and cross rings
         Read(Value,*) UEG_Inp%IRangeLinRings

        Case ('QuadRings Range') ! for the quadratic rings and cross rings
         Read(Value,*) UEG_Inp%IRangeQuadRings
        
        Case ('DRings Range') ! for the dRPA terms
         Read(Value,*) UEG_Inp%IRangeDirectRings

        Case ('ExRings Range') ! for the RPAX terms
         Read(Value,*) UEG_Inp%IRangeExchangeRings

        Case ('LinLadd Range') ! for the linear ladders
         Read(Value,*) UEG_Inp%IRangeLinLadders

        Case ('QuadLadd Range') ! for the quadratic ladders
         Read(Value,*) UEG_Inp%IRangeQuadLadders
        
        Case ('DLadders Range') ! by analogy to rings 
         Read(Value,*) UEG_Inp%IRangeDirectLadders

        Case ('ExLadders Range') ! by analogy to rings
         Read(Value,*) UEG_Inp%IRangeExchangeLadders


        Case Default   !  Unknown keyword
         ExStatus=1
         write(6,*) Value
         Exit

       End Select
      End Do
10    Close(4,Status='Keep')


!==================================================================!
!  If the user tried to set an undefined keyword, tell him where.  !
!  Then check if the required keywords are set.                    !
!  Then check if any are multiply set.                             !
!  Finally, if there was an error, we stop.                        !
!                                                                  !
!  Since parameters 1:2 have defaults, we tell the code as much.   !
!==================================================================!

      SetOnce(1:2) = .True.
      If(ExStatus == 1) Write(6,1000) LineNumber
      Do I = 1,NParams
       If(.not. SetOnce(I)) Write(6,1010) Trim(ParamName(I))
      End Do
      Error = ExStatus == 1 .or. Any(SetTwice) .or. .not. All(SetOnce)
      If(Error) Stop 'Error in Input'


!==================!
!  Over to James.  !
!==================!

      Call Init_HEG_dummy(UEG_Inp%MaxKPoint,UEG_Inp%NElectron)
      UEG_Inp%NAO = nBasis

1000  Format('Error: Line number ',I4,' of Input not recognized; ',  &
             'subsequent lines not read.')
1010  Format('Error: Parameter ',A,' not set.')
1020  Format('Error: Parameter ',A,' multiply set.')

      Return
      End Subroutine ReadInput





      Subroutine ParseLine(Line,KeyWord,Value)
      Implicit None
      Character (Len=*),         Intent(In)  :: Line
      Character (Len=Len(Line)), Intent(Out) :: KeyWord, Value
      Character (Len=Len(Line))              :: WorkingLine
      Integer :: I, CommentPos, ColonPos

!========================================================================!
!  Read keywords and values.                                             !
!------------------------------------------------------------------------!
! First, check to see if we have a colon to separate keyword from value  !
! If there's no colon, we do not have a keyword:value pair.              !
!========================================================================!

      ColonPos = Index(Line,':')
      If (ColonPos == 0) Then
        Keyword = "SKIP"
        Return
      End If


!===============================================!
!  Replace all control characters with spaces.  !
!===============================================!

      WorkingLine = Line
      Do I = 1,Len(Line)
       If(IAChar(WorkingLine(I:I)) <= 31) WorkingLine(I:I) = ' '
      End Do


!===========================================!
!  Keyword is everything before the colon,  !
!  and value is everything after it.        !
!===========================================!

      KeyWord = WorkingLine(1:ColonPos-1)
      Value   = WorkingLine(ColonPos+1:)


!==================================================================!
!  For both keyword and value, delete everything after a comment.  !
!==================================================================!

      CommentPos  = Index(Keyword,'!')
      If(CommentPos /= 0) Keyword = Keyword(1:CommentPos-1)
      CommentPos  = Index(Value,'!')
      If(CommentPos /= 0) Value = Value(1:CommentPos-1)


!==================================================!
!  If the keyword or value is blank, we can skip.  !
!==================================================!

      If(Len_Trim(Keyword) == 0 .or. Len_Trim(Value) == 0) Keyword = "SKIP"
      Return
      End Subroutine ParseLine

  End Module IO

