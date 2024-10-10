' COPY EVERYTHING BELOW TO THE TOP OF YOUR TABLE SCRIPT UNDER OPTION EXPLICIT

'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings for this table ********************************

usePUP   = true               ' enable Pinup Player functions for this table
cPuPPack = "SpookyWednesday"    ' name of the PuP-Pack / PuPVideos folder for this table

'//////////////////// PINUP PLAYER: STARTUP & CONTROL SECTION //////////////////////////

' This is used for the startup and control of Pinup Player

Sub PuPStart(cPuPPack)
    If PUPStatus=true then Exit Sub
    If usePUP=true then
        Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay") 
        If PuPlayer is Nothing Then
            usePUP=false
            PUPStatus=false
        Else
            PuPlayer.B2SInit "",cPuPPack 'start the Pup-Pack
            PUPStatus=true
        End If
    End If
End Sub

Sub pupevent(EventNum)
    if (usePUP=false or PUPStatus=false) then Exit Sub
    PuPlayer.B2SData "E"&EventNum,1  'send event to Pup-Pack
End Sub

' ******* How to use PUPEvent to trigger / control a PuP-Pack *******

' Usage: pupevent(EventNum)

' EventNum = PuP Exxx trigger from the PuP-Pack

' Example: pupevent 102

' This will trigger E102 from the table's PuP-Pack

' DO NOT use any Exxx triggers already used for DOF (if used) to avoid any possible confusion

'************ PuP-Pack Startup **************

' PuPStart(cPuPPack) 'Check for PuP - If found, then start Pinup Player / PuP-Pack



'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  Player Options
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	'If you are wanting to use the pup on your FULLDMD Screen go to line 5026 and change the Screen Number 

	Rockmusic = 1 'change this to 1 to switch out the background music with 80s rock/pop songs from the show
	soundtrackvol = 63 'Set the background audio volume to whatever you'd like out of 100
	videovol = 105 'set the volme you'd like for the videos
	calloutvol = 85 ' set this to whatever you're like your callouts to be
	calloutlowermusicvol = 1 'set to 1 if you want music volume lowered during audio callouts


'***********************************
'Intro art options on line 281, 2211 
'***********************************


Randomize

'---------- UltraDMD Unique Table Color preference -------------
Dim DMDColor, DMDColorSelect, UseFullColor
Dim DMDPosition, DMDPosX, DMDPosY, DMDSize, DMDWidth, DMDHeight 


UseFullColor = "True" '                           "True" / "False"
DMDColorSelect = "white"            ' Rightclick on UDMD window to get full list of colours

DMDPosition = False                               ' Use Manual DMD Position, True / False
DMDPosX = 100                                   ' Position in Decimal
DMDPosY = 40                                     ' Position in Decimal

DMDSize = False                                     ' Use Manual DMD Size, True / False
DMDWidth = 512                                    ' Width in Decimal
DMDHeight = 128                                   ' Height in Decimal 

'Note open Ultradmd and right click on window to get the various sizes in decimal 

' GetDMDColor
' Sub GetDMDColor
' Dim WshShell,filecheck,directory
' Set WshShell = CreateObject("WScript.Shell")
' If DMDSize then
' WshShell.RegWrite "HKCU\Software\UltraDMD\w",DMDWidth,"REG_DWORD"
' WshShell.RegWrite "HKCU\Software\UltraDMD\h",DMDHeight,"REG_DWORD"
' End if
' If DMDPosition then
' WshShell.RegWrite "HKCU\Software\UltraDMD\x",DMDPosX,"REG_DWORD"
' WshShell.RegWrite "HKCU\Software\UltraDMD\y",DMDPosY,"REG_DWORD"
' End if
' WshShell.RegWrite "HKCU\Software\UltraDMD\fullcolor",UseFullColor,"REG_SZ"
' WshShell.RegWrite "HKCU\Software\UltraDMD\color",DMDColorSelect,"REG_SZ"
' End Sub
'---------------------------------------------------


' Load the core.vbs for supporting Subs and functions
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
    On Error Goto 0
End Sub

Const cGameName = "SpookyWednesday"
'Const UseFullColor = "True"						'Use Color DMD (True / False)
'Const DMDColorSelect = "Azure"				'Rightclick on UDMD window to get full list of colors

Dim Ballsize,BallMass
Ballsize = 50
BallMass = (Ballsize^3)/125000



' Define any Constants
Const TableName = "SpookyWednesday"
Const myVersion = "0.0.1"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 10 ' in seconds
Const MaxMultiplier = 5 ' limit to 5x in this game
Const BallsPerGame = 3   ' 3 or 5
Const MaxMultiballs = 5  ' max number of balls during multiballs

' Define Global Variables
Dim calloutlowermusicvol
Dim soundtrackvol
Dim toppervideo
Dim useB2SBG
Dim videovol
Dim calloutvol
Dim rockmusic
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot
Dim SuperJackpot
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim SkillshotValue(4)
Dim bAutoPlunger
Dim bAttractMode
Dim ComboValue
Dim bSlimerSuperJackpot
Dim SlimerDefeats

' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLock
Dim BallsInHole

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallWonThisBall
Dim bJustStarted

' core.vbs variables
Dim plungerIM 'used mostly as an autofire plunger
Dim LeftMagnet  'Magic Ball
Dim RightMagnet 'used for magnet
Dim RightMagnet2 'used for magnet
Dim ThingMagnet 'things apron magnet
Dim WizardMagnet 'wizard mode magnet
Dim SingleModeMagnet 'SingleBall mode magnet

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
    Randomize
    PuPStart(cPuPPack) 'Check for PuP - If found, then start Pinup Player / PuP-Pack
    InitPuP
   '****************************************************************************************************************************************
   'EVERYTHING FROM LINE THIS LINE TO LINE 415 IS EVERTHING THING THAT NEEDS TO BE SHUT OFF OR TURNED ON WHEN THE TABEL STARTS UP
   '****************************************************************************************************************************************
   '******************************************************
   ' BasePUP AND ATTRACT_Fog and a few Miscellaneous items
   '******************************************************
    PuPlayer.playlistplayex pMusic,"audioattract","",0,0
    Start_Fog
    PuPlayer.playlistplayex pBackglass,"scene","base.mp4",0,1
    PuPlayer.SetBackground pBackglass,1
    'Start_ThingsApronFog
    AudioAttractTimer.Enabled = True
    DiscoBallLightGlow.State=0
    FireBallDMDTrigger.Enabled = True
    Hyde2Timerattract.Enabled = True
    Hyde2offattractTimer.Enabled = True
    GameVideoStartTrigger.Enabled = True
    SlingFlasherGreen002.State=0
    SlingFlasherGreen001.State=0
    ThingLaunchSkillTrigger.Enabled = True
    MBBlocker.Isdropped = 1
    MBBlocker003.Isdropped = 1
    MBBlocker5.Isdropped = 1
    'SJPKicker.Enabled = 1
    SingleModeBlocker001.Isdropped = 1
    SingleModeBlocker002.Isdropped = 1
    SingleModeBlocker003.Isdropped = 1
    HSResetBlocker.Isdropped = 1
    CRACKSTONE.visible =0
    rampSpot001.visible = 0
    JackpotBlockers001.Isdropped = 1
    JackpotBlockers002.Isdropped = 1
   '**********************************
   '   Hyde Jackpot Apron Lights
   '**********************************
	APGlow.State=1
	HYDEMB2Lit.State=0
    HydeModeDonelit.State=0
    DiscoPartyDone.state = 0
    HydeModeDone003.State=0
    DiscoPartyLit.State=0
    Hyde2DrainTimer.Enabled = False
    Hyde2offTimer.Enabled = False
    APglowFlasher.opacity = 100
    Slimer.visible =0
    '************************************
    ' Grave Stone Drop Targets Off
	'************************************
    MBBlocker.Isdropped = 1
    GraveStoneSuperTarget001.Isdropped = 1
    GraveStoneSuperTarget002.Isdropped = 1
    GraveStoneSuperTarget003.Isdropped = 1
    DiscoBlocker.Isdropped = 1
    MBBlocker001.Isdropped = 1
    MBBlocker002.Isdropped = 1
    GIOFFWIZARDTrigger001.Enabled = False
    GIOFFWIZARDTrigger.Enabled = False
    WizardBlocker.Isdropped = 1
    '***********************************************
    ' Single Ball Mode Modles Lights and Triggers
	'***********************************************
    NightShadeTargetsOff
    EndNightShades
    NightShadeTargetsOff
    NightShadeGIOff
    TheHydesHead.visible = 0
    Book.visible = 0
    JailKey.visible = 0
    Glasses.visible = 0
    discoball.visible = 0
    NMLOGO.visible = 0
    Enid_Sinclair.visible = 0
    FreeGomezFlasher.opacity = 0
    WheresTheHydeMBFlasher.opacity = 0
    SaveEugeneFlasher.opacity = 0
    UnderslingGreen004.State=0
    UnderslingGreen003.State=0
    UnderslingGreen002.State=0
    UnderslingGreen001.State=0
    WizardSaverKicker.Enabled = False
    WizardDrainKicker.Enabled = False
    '****************************

    '****************************
    ' Quote timers and triggers
	'****************************
    quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    quotetrigger002.Enabled = True
    '****************************
    '       SMALL DMD 
	'****************************
    DMDSmall2.visible = 0
    DMDSmall3.visible = 0
    DMDSmall1Timeron.Enabled = 1
    DMDSmallHMBLit.visible = 0
    DMDSmallHMBLok1.visible = 0
    DMDSmallHMBLok2.visible = 0
    '****************************
	introflasherDance1.opacity = 100
    DiscoDanceMBFlasher.opacity = 0
    DiscoDanceMBFlasher2.opacity = 0
   '**********************************
   '        HYDE MBFlashers
   '**********************************
    HMBFlasher22.opacity = 0
    HMBFlasher02.opacity = 0
    HMBFlasherApron.opacity = 0
   '**********************************
   '**********************************
   '      Blood Ramp Flashers
   '**********************************
    BloodRamp1.opacity = 0
	BloodRamp2.opacity = 0
    BloodRamp3.opacity = 0
    BloodRamp4.opacity = 0
    BloodRamp5.opacity = 0
    BloodRamp6.opacity = 0
    BloodRamp7.opacity = 0
    BloodRamp8.opacity = 0
    BloodRamp9.opacity = 0
    BloodRamp10.opacity = 0
    BloodRamp11.opacity = 0
    BloodRamp12.opacity = 0
    BloodRamp13.opacity = 0
    BloodRamp14.opacity = 0
    BloodRamp15.opacity = 0
    BloodRamp16.opacity = 0
    BloodRamp17.opacity = 0
    BloodRamp18.opacity = 0
    BloodRamp19.opacity = 0
    BloodRamp20.opacity = 0
    BloodRamp21.opacity = 0
   '****************************************************************************************************************************************
    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 45 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFX("fx_kicker", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
        .CreateEvents "plungerIM"
    End With

     vpmtimer.AddTimer 1000, "PlaySound ""fx_sys11_bootup"" '"
     vpmtimer.AddTimer 2700, "PlaySound ""fx_sys11_bootup"" '"    

' Left Magnet
    Set LeftMagnet = New cvpmMagnet
    With LeftMagnet
        .InitMagnet Magnet3o, 19
        .GrabCenter = False
        .CreateEvents "LeftMagnet"
    End With

' Right Magnet
    Set RightMagnet = New cvpmMagnet
    With RightMagnet
        .InitMagnet Magnet2o, 19
        .GrabCenter = False
        .CreateEvents "RightMagnet"
    End With

' Right Magnet2
    Set RightMagnet2 = New cvpmMagnet
    With RightMagnet2
        .InitMagnet Magnet4o, 19
        .GrabCenter = False
        .CreateEvents "RightMagnet2"
    End With


' Thing Magnet
    Set ThingMagnet = New cvpmMagnet
    With ThingMagnet
        .InitMagnet thingapronmag, 120
        .GrabCenter = False
        .CreateEvents "ThingMagnet"
    End With

' Wizard Magnet
    Set WizardMagnet = New cvpmMagnet
    With WizardMagnet
        .InitMagnet WizardMagOn, 100
        .GrabCenter = True
        .CreateEvents "WizardMagnet"
    End With


' SingleMode Magnet
    Set SingleModeMagnet = New cvpmMagnet
    With SingleModeMagnet
        .InitMagnet SingleModeMagOn, 100
        .GrabCenter = True
        .CreateEvents "SingleModeMagnet"
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
    Loadhs

    ' Initalise the DMD display
    DMD_Init

    ' freeplay or coins
    bFreePlay = True 'we dont want coins

    ' Init main variables and any other flags
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bMultiBallMode = False
    bGameInPlay = False
    bAutoPlunger = False
    bMusicOn = True
    BallsOnPlayfield = 0
    BallsInLock = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bJustStarted = True
    BHit = 0
    ' set any lights for the attract mode
    GiOff
    vpmtimer.AddTimer 8000, "StartAttractMode '"
   '****************************************************
   ' APRON GLOW OFF PUP_ATTRACT-MODE PRE-INTO AND INTRO
   '****************************************************
    APGlow.State=1
    PuPlayer.playlistplayex pBackglass,"scene","Pre-IntroDMD.mp4",videovol,1
    PuPlayer.SetBackground pBackglass,1
    PuPlayer.playlistplayex pBackglass,"videoattract","",videovol,1
   	PuPlayer.SetBackground pBackglass,1
End Sub


'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        If(Tilted = False) Then
            PlaySound "goldrake voce"
            DMDFlush
            DMD "dmd baseBlank.png", " ", "",  2800
            DMD "dmd baseBlank.png", "_", " CREDITS " &credits, 500
            PlaySound "fx_coin"
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        plungerIM.AutoFire
      If (Bulb30.State = 2) Then
         addscore  50000
         'Playsound "goldrake voce"
         GiEffect 2
         FlashEffect 2
         LightEffect 1
         FireFlash.State = 0
         flashforms  flasher2,2000, 50, 0
         flashforms flasher6, 2000, 50, 0
         Bulb30.State = 0
         DMD "dmd baseBlank.png", " ", "",  3000
         DMDFLush
         DMD "dmd baseBlank.png", "SPECIAL KEY", "PRESSED 50.000" , 200
      End If
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Table specific

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 6:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1
        
        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMDFlush
                    DMD "dmd baseBlank.png", " ", PlayersPlayingGame & " PLAYERS", 500
                    PlaySound "so_fanfare1"
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMDFlush
                        DMD "dmd baseBlank.png", " ", PlayersPlayingGame & " PLAYERS", 500
                        PlaySound "so_fanfare1"
                    Else
                        ' Not Enough Credits to start a game.
                        DOF 140, DOFOff
                        DMDFlush
                        DMD "dmd baseBlank.png", "CREDITS " &credits, "INSERT COIN", 500
                        PlaySound "so_nocredits"
                   End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetForNewGame()
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DOF 140, DOFOff
                        DMDFlush
                        DMD "dmd baseBlank.png", "CREDITS " &credits, "INSERT COIN", 500
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)


    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper 0
           End If
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
           End If
        End Sub
   


'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub table1_Exit
    Savehs
    Controller.Stop
End Sub

'********************
' Special JP Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySound "fx_flipperup", 0, 1, -0.05, 0.15
        LeftFlipper.RotateToEnd
        UpperLFlipper.RotateToEnd
        RotateLaneLightsLeft
        
    Else
        PlaySound "fx_flipperdown", 0, 1, -0.05, 0.15
        LeftFlipper.RotateToStart
        UpperLFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySound "fx_flipperup", 0, 1, 0.05, 0.15
        RightFlipper.RotateToEnd
        UpperRFlipper.RotateToEnd
        RotateLaneLightsRight
        

    Else
        PlaySound "fx_flipperdown", 0, 1, 0.05, 0.15
        RightFlipper.RotateToStart
        UpperRFlipper.RotateToStart
    End If
End Sub



' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, -0.05, 0.25
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, 0.05, 0.25
End Sub



Sub UpdateFlipperLogo_Timer
   LFLogo.RotY = LeftFlipper.CurrentAngle
   LFLogo001.RotY = LeftFlipper.CurrentAngle
   RFlogo.RotY = RightFlipper.CurrentAngle
   RFlogo001.RotY = RightFlipper.CurrentAngle
End Sub


Sub RotateLaneLightsLeft
    Dim TempState
    TempState = SP1.State
    SP1.State = SP2.State
    SP2.State = SP3.State
    SP3.State = SP4.State
    SP4.State = TempState
End Sub

Sub RotateLaneLightsRight
    Dim TempState
    TempState = SP4.State
    SP4.State = SP3.State
    SP3.State = SP2.State
    SP2.State = SP1.State
    SP1.State = TempState
End Sub

'*********
' TILT
'*********
'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <25) Then 'show a warning
        DMDFlush
        DMD "black.jpg", " ", "CAREFUL!", 800
    End if
    If Tilt> 25 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
        DMD "dmd baseBlank.png", " ", "TILT!", 99999
        Playsound "tilt"
        DMDFlush
        DMD "dmd baseBlank.png", " ", "TILT!", 99999
        Playsound "tilt"
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt> 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
		'UpperRFlipper.RotateToStart


        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        'Bumper1.Force = 6
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0

        'clean up the buffer display
        DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0) Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
        Me.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'********************
' Music as wav sounds
'********************


Dim Song
Song = ""

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            If Song = "mu_end" Then
                PlaySound Song, 0, 0.1  'this last number is the volume, from 0 to 1
            Else
                PlaySound Song, -1, 0.1 'this last number is the volume, from 0 to 1
            End If
        End If
    End If
End Sub



'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub GIUpdateTimer_Timer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = -1 Then
            GiOff
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSat"
   
End Sub

Sub GiOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    Table1.ColorGradeImage = "ColorGradeLUT256x16_ConSatDark"
    
End Sub

' GI & light sequence effects

Sub GiEffect(n)
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 4
            LightSeqGi.Play SeqBlinking, , 5, 100
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 5, , 1000
        Case 3 'upon
            LightSeqGi.UpdateInterval = 4
            LightSeqGi.Play SeqUpOn, 5, 1
        Case 4 ' left-right-left
            LightSeqGi.UpdateInterval = 5
            LightSeqGi.Play SeqLeftOn, 10, 1
            LightSeqGi.UpdateInterval = 5
            LightSeqGi.Play SeqRightOn, 10, 1
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'upon
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 10, 1
        Case 4 ' left-right-left
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqLeftOn, 10, 1
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqRightOn, 10, 1
    End Select
End Sub

' Flasher Effects using lights

Dim FEStep, FEffect
FEStep = 0
FEffect = 0

Sub FlashEffect(n)
    Dim ii
    Select case n
        Case 0 ' all off
            LightSeqFlasher.Play SeqAlloff
        Case 1 'all blink
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqBlinking, , 10, 10
        Case 2 'random
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqRandom, 50, , 1000
        Case 3 'upon
            LightSeqFlasher.UpdateInterval = 4
            LightSeqFlasher.Play SeqUpOn, 10, 1
        Case 4 ' left-right-left
            LightSeqFlasher.UpdateInterval = 5
            LightSeqFlasher.Play SeqLeftOn, 10, 1
            LightSeqFlasher.UpdateInterval = 5
            LightSeqFlasher.Play SeqRightOn, 10, 1
    End Select
End Sub






Sub GameTimer_Timer()
	RollingUpdate
	'BallShadowUpdate

   
 'Boomeran
   If boomerD <= 0 Then
      TimerboomerOff.enabled = 0 
      boomerD = 0
   End If

  If boomerD >= 400 Then  
    Timerboomer.enabled = 0
    vpmtimer.addtimer 200, "TimerboomerOff.enabled = 1 '"       
  End If

 
   

 If A6.state = 0 Then
    A6P.image = "plasticYellow"
 End If
 If A6.state = 1 Then
    A6P.image = "PlasticWhite"
 End If
 If A7.state = 0 Then
    A7P.image = "plasticYellow"
 End If
 If A7.state = 1 Then
    A7P.image = "PlasticWhite"
 End If
 If A8.state = 0 Then
    A8P.image = "plasticYellow"
 End If
 If A8.state = 1 Then
    A8P.image = "PlasticWhite"
 End If

End Sub



' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 1500)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp> 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 20 ' total number of balls
Const lob = 0   'number of locked balls
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)
        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) * 50
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b) ), Pan(BOT(b) ), 0, ballpitch, 1, 0
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0
End Sub




'******************************
' Diverse Collection Hit Sounds
'******************************

Sub aMetals_Hit(idx):PlaySound "fx_MetalHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Pins_Hit(idx):PlaySound "fx_postrubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aPlastics_Hit(idx):PlaySound "fx_PlasticHit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aGates_Hit(idx):PlaySound "fx_Gate", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aWoods_Hit(idx):PlaySound "fx_Woodhit", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub

Sub aRubber_Bands_Hit(idx):PlaySound "fx_rubber_band", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub
Sub aRubber_Posts_Hit(idx):PlaySound "fx_rubber", 0, Vol(ActiveBall), pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0:End Sub





' Some quotes from the 2 first movies

Sub PlayQuote_timer() 'one quote each 2 minutes
    Dim Quote
    Quote = "gb_quote" & INT(RND * 56) + 1
    'PlaySound Quote
End Sub

' Ramp Soundss
Sub RHelp_Hit()
    PlaySound "fx_ballrampdrop", 0, 1, pan(ActiveBall)
End Sub

Sub RHelp2_Hit()
    StopSound "fx_metalrolling"
    PlaySound "fx_ballrampdrop", 0, 1, pan(ActiveBall)
End Sub

Sub LHelp1_Hit()
    StopSound "fx_metalrolling"
    PlaySound "fx_ballrampdrop", 0, 1, pan(ActiveBall)
End Sub

Sub Trigger212121_hit()
    StopSound "fx_metalrolling"
    PlaySound "fx_metalrolling", 0, 1, pan(ActiveBall)
End Sub


' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

     AlabardaAnimOff
     diskoAnim
     ActarusTimer360.enabled = 0
    
    bGameInPLay = True
    'resets the score display, and turn off attrack mode
    StopAttractMode
    GiOn

	If B2SOn Then Controller.B2SSetData 1,1

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusHeldPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

    ' initialise any other flags
    Tilt = 0
    ComboM = 0
    ' initialise Game variables
    Game_Init()
    
'you may wish to start some music, play a sound, do whatever at this point

    vpmtimer.addtimer 3500, "FirstBall '"
End Sub
' This is used to delay the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
    'MiniDMD
    'vpmtimer.addtimer 1500, "IntroSmallDMD '"
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    ' make sure the correct display is upto date
    AddScore 0

    ' set the current players bonus multiplier back down to 1X
    SetBonusMultiplier 1

    ' reset any drop targets, lights, game modes etc..
    'LightShootAgain.State = 0
'    Bonus = 0
    bExtraBallWonThisBall = False
    ResetNewBallLights()
    
    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
    'bSkillShotReady = True 'no skillshot in this game

    'Change the music ?

    'Reset any table specific
'    TargetBonus = 0
'    BumperBonus = 0
'    PokemonBonus = 0
'    HoleBonus = 0
'    EggBonus = 0



	' Bumperlanes reset:
FlashForMs bulb17, 100, 10, 0  'off
FlashForMs bulb25, 100, 10, 0  'off



End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedball BallSize / 2
'    UpdateBallImage
    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1
    DropKillTrigger.Enabled = True
    ' kick it out..
    PlaySound "fx_Ballrel", 0, 1, 0.1, 0.1
    BallRelease.Kick 90, 4
    'NightShadeTargetsOff
    'EndNightShades
    'NightShadesDrainTimer.Enabled = True
    DropKillTrigger.Enabled = True

    ' if there is 2 or more balls then set the multibal flag (remember to check for locked balls and other balls used for animations)
	' set the bAutoPlunger flag to kick the ball in play automatically
    If BallsOnPlayfield > 1 Then
        bMultiBallMode = True
		bAutoPlunger = True
        'ChangeSong
       ' PlaySong "Multiball"
    End If
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        CreateNewBall()
        mBalls2Eject = mBalls2Eject -1
        If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
            Me.Enabled = False
        End If
    End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded
'
Sub EndOfBall()
    Dim BonusDelayTime
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)
    If(Tilted = False) Then
        Dim AwardPoints, TotalBonus
        AwardPoints = 0:TotalBonus = 0
 DMDFLush
 DMD "dmd baseBlank.png", " ", " BALL LOST SUCKER ", 25000
'********************************************************************
'                        PUP-DRAIN VIDEO
'********************************************************************
 PuPlayer.playlistplayex pBackglass,"videodrain","",videovol,1
 PuPlayer.playlistplayex pCallouts,"AudioBallLost","",90,1
'********************************************************************

'********************************************************************
'                        MUSIC RESTART
'********************************************************************
 'PuPlayer.playlistplayex pMusic,"audioattract","",0,1
 'PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
 'PuPlayer.SetLoop 4,1
'********************************************************************

'********************************************************************
'	   SHUTS OFF ANY MULTIBALL MODES & All GamePlay Timers
'********************************************************************
 Stop_Fog
 SlimerDrainTimer_Timer()
 EndNightShades
 NightShadeGIOff
 SaveEugeneDrain
 SaveGomezDrain
 StopWedSpin
 StartLOGOAni
 Stophydespin
 ShutOffBloodRampToff
 FreeGomezAllSmallDMDsOff
 WWMBAllSmallDMDsOff
 NightShadeSAllSmallDMDsOff
 RavenDisceAllSmallDMDsOff
 WTHAllSmallDMDsOff
 EVHAllSmallDMDsOff
 SaveEugeneAllSmallDMDsOff
 SkillShotDrainResetTimer_Timer
 ShutOffBloodRampToff
 QON.Enabled = True
 QON2.Enabled = False
 QON3.Enabled = True
 AudioAttractTimer001.Enabled = False
 SingleModeBlocker001.Isdropped = 1
 SingleModeBlocker002.Isdropped = 1
 SingleModeBlocker003.Isdropped = 1
 NMLOGO.visible = 1
 NightShadesDrainTimer.Enabled = True
 NightShadesDrainTimer2.Enabled = True
 NightShadesDrainTimer3.Enabled = True
 NightShadesDrainTimer4.Enabled = True
 NightShadesDrainTimer5.Enabled = True
 SlimerDrainTimer.Enabled = True
 DiscoPartyMBDrain.Enabled = True
 DiscoPartyMBTimeoutTimer.Enabled = False
 ResetHMBGITimer.Enabled = False
 HMBFlasher22.opacity = 0
 HMBFlasher02.opacity = 0
 HMBFlasherApron.opacity = 0
 APGlow.state = 0
 HydeModeDonelit.state = 0
 HYDEmbVideo.Enabled = False
 Hyde2DrainTimer.Enabled = True
 Hyde2offTimer.Enabled = True
 GraveStoneSuperTarget001.Isdropped = 1
 GraveStoneSuperTarget002.Isdropped = 1
 GraveStoneSuperTarget003.Isdropped = 1
 grave001tragertimer002.Enabled = False
 grave001tragertimer001.Enabled = False
 grave001tragertimer.Enabled = False
 DiscoBlocker.Isdropped = 1
 HYDEMB2Lit.state = 0
 DiscoPartyLit.state = 0
 HydeSuperJackpotTrigger.Enabled = False
 EndHydeJackP.Enabled = true
 quotetrigger.Enabled = True
 quotetrigger001.Enabled = True
 quotetrigger002.Enabled = True
 VideoQuoteResteTimer.Enabled = False
 VideoQuoteResteTimer001.Enabled = False
 VideoQuoteResteTimer002.Enabled = False
 VideoQuoteResteTimer003.Enabled = False
 VideoQuoteResteTimer004.Enabled = False
 VideoQuoteResteTimer005.Enabled = False
 MBBlocker.Isdropped = 1
 MBBlocker001.Isdropped = 1
 MBBlocker002.Isdropped = 1
 MBBlocker5.Isdropped = 1
 MBBlockerTimer002.Enabled = False
 MBBlockerTimer003.Enabled = False
 MBBlocker002Target.Enabled = false
 MBBlockerTarget.Enabled = false
 MBBlocker001Target.Enabled = false
 hyde2SJPDrainTimer.Enabled = True
 WizardLightGlowFade003.state = 0
 WizardLightGlowFade004.state = 0
 WizardLightGlowFade005.state = 0
 SkillShotWonTrigger.Enabled = False
 '********************************************************************
 LightEffect 1
 FlashEffect 1
' add in any bonus points (multipled by the bonus multiplier)
AwardPoints = BonusPoints(CurrentPlayer) * BonusMultiplier(CurrentPlayer)
AddScore AwardPoints
debug.print "Bonus Points = " & AwardPoints
'DMD_DisplaySceneEx "DMD1.png", "AWARD: " & BonusPoints(CurrentPlayer), 15, 4, "Bonus", -1, -1, UltraDMD_Animation_None, 1000, UltraDMD_Animation_None  
'DMD_DisplaySceneEx "DMD1.png", "", 15, 4, "", -1, -1, UltraDMD_Animation_None, 5, UltraDMD_Animation_None
'this table uses several bonus

'        DMD CenterLine(0, FormatScore(TotalBonus) ), CenterLine(1, "TOTAL BONUS " & " X" & BonusMultiplier(CurrentPlayer) ), 0, eBlinkFast, eNone, eNone, 1000, True, ""
'        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        BonusDelayTime = 3000
        vpmtimer.addtimer BonusDelayTime, "Addscore TotalBonus '"
    Else
        'no bonus to count so move quickly to the next stage
        BonusDelayTime = 100
    End If
    ' start the end of ball timer which allows you to add a delay at this point
    vpmtimer.addtimer BonusDelayTime, "EndOfBall2 '"
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the currentplayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
        debug.print "Extra Ball"
        DMD "dmd baseBlank.png", " ", "Extra Ball", 100
        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            FlashForMs ShootAgainLightF, 100, 10, 0  'off
            ShootAgainLight.State = 0
            playsound "Shootagain"
        End If

        ' You may wish to do a bit of a song AND dance at this point

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0) Then
            debug.print "No More Balls, High Score Entry"
			If B2SOn Then Controller.B2SSetData 1,0
            ' Submit the currentplayers score to the High Score system
            CheckHighScore()
        ' you may wish to play some music at this point

        Else

            ' not the last ball (for that player)
            ' if multiple players are playing then move onto the next one
            EndOfBallComplete()
        End If
    End If
End Sub

' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame> 1) Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer> PlayersPlayingGame) Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode

        ' set the machine into game over mode
        EndOfGame()
		BHit = 0
    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            PlaySound "vo_player" &CurrentPlayer
            DMD "dmd baseBlank.png", " ", "PLAYER " &CurrentPlayer, 800
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
   ' PuPlayer.playlistplayex pMusic,"audiobgrock","",0, 0
    'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then
    'PlaySong "m_end"
    End If

    BHit = 0

    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0
    
    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball
    'PlayQuote.Enabled = 0
    ' show game over on the DMD
    DMDFLush
    DMD "black.jpg", " ", "GAME OVER", 1500
    '*************************************************************
	' RE-STARTS ATTRACT_FOG AND SHUTS OFF MULTIBALL APRON LIGHTS
    '*************************************************************
    Start_Fog
    SlimerMoveUp
    StopLOGOAni
    NightShadeTargetsOff
    SkillShotLightsOff
    SkillShotTimersOff
    HydeTimeoutTimer.Enabled = False
    Hyde2offTimer.Enabled = False
    Book2.visible = 1
    JailKey2.visible = 1
    Glasses2.visible = 1
    XAVIER.State= 0
    Biannca.State= 0
    AJAX.State= 0
    Eugene.State= 0
    Addams1.State= 0
    Addams2.State= 0
    aquamanLight.State=0
    aquawoman.State=0
    YoKo.State=0
    HydeModeDone.state = 0
    HYDEMB2Done.state = 0
    DiscoPartyDone.state = 0
    quotetrigger.Enabled = False
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False
    QON.Enabled = False
    QON2.Enabled = False
    GameVideoStartTrigger.Enabled = True
    totherampSkill003.Enabled = 0
    GIOFFWIZARDTrigger.Enabled = False
    GIOFFWIZARDTrigger001.Enabled = false
    GIOFFWIZARDTrigger002.Enabled = false
    GIOFFWIZARDTrigger003.Enabled = false
    GIOFFWIZARDTrigger004.Enabled = false
    WizaedGIOffTimer.Enabled = False
    WizaedGIOffTimer001.Enabled = False
    NightShadesFailedTimer.Enabled = False
    SaveGomezModeFailed.Enabled = False
    EugeneModeFailed.Enabled = False
    EugeneTimer001Off.Enabled = False
    JailTimer001OFF.Enabled = False
    NMLogoFlasherTimer.Enabled = 0
    NMLOGO.visible = 0
    RestartLoGoHMB.Enabled = False
    AudioAttractTimer001.Enabled = False
    ResetHMBGITimer.Enabled = False 
    SlimerUpTimer.Enabled = True
    showhydetimer.Enabled = True
    Slimer.visible = 0
    SingleModeKillerKicker.Enabled = 0
    SingleModeKillerKicker001.Enabled = 0
    SingleModeKillerKicker002.Enabled = 0
    SingleModeKillerKicker003.Enabled = 0
    SingleModeKillerKicker004.Enabled = 0
    SingleModeKillerKicker005.Enabled = 0
    SingleModeKillerKicker006.Enabled = 0
    SingleModeKillerKicker007.Enabled = 0
    SingleModeKillerKicker008.Enabled = 0
    SingleModeKillerKicker009.Enabled = 0
    SingleModeKillerKicker010.Enabled = 0
    SingleModeKillerTrigger.Enabled = False
    SingleKillerTimer.Enabled = False
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0, 0
    'GiOff
    '*************************************************************

    ' set any lights for the attract mode
    GiOff
    StartAttractMode
    
   
    ActarusTimer360.enabled = 1
    diskoAnimOff
' you may wish to light any Game Over Light you may have
End Sub

Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function



'*********************************************************************************
' ANIMATION TIMES FOR BLOOD_APRON,BLOOD-RUNNING PLAYFIELD, AND CRAWLING SPIDERS
'*********************************************************************************
Dim ESFX
ESFX=0

Sub ExplsionSFX_Timer
    If ExplsionSFX.enabled=true then ESFX=ESFX+1: End If
    If ESFX > 23 Then
          ExAnimation.Enabled = True
          ExplosionLight.Enabled = True
          ExplsionSFX.enabled=false
          ESFX=0
    End If
End Sub


Dim BRFX
BRFX=0

Sub BloodRampSFX_Timer
    If BloodRampSFX.enabled=true then BRFX=BRFX+1: End If
    If BRFX > 23 Then
          BRAnimation.Enabled = True
          BloodRampLight.Enabled = True
          BloodRampSFX.enabled=false
          BRFX=0
    End If
End Sub


Dim SCFX
SCFX=0

Sub SpiderSFX_Timer
    If SpiderSFX.enabled=true then SCFX=SCFX+1: End If
    If SCFX > 23 Then
          SpiderSFX.Enabled = True
          SCLight.Enabled = True
          SpiderSFX.enabled=false
          SCFX=0
    End If
End Sub


Dim SC001FX
SC001FX=0

Sub Spider001SFX_Timer
    If Spider001SFX.enabled=true then SC001FX=SC001FX+1: End If
    If SC001FX > 23 Then
          Spider001SFX.Enabled = True
          SC001Light.Enabled = True
          Spider001SFX.enabled=false
          SC001FX=0
    End If
End Sub


Dim SC002FX
SC002FX=0

Sub Spider002SFX_Timer
    If Spider002SFX.enabled=true then SC002FX=SC002FX+1: End If
    If SC002FX > 23 Then
          Spider002SFX.Enabled = True
          SC002Light.Enabled = True
          Spider002SFX.enabled=false
          SC002FX=0
    End If
End Sub


Dim SC003FX
SC003FX=0

Sub Spider003SFX_Timer
    If Spider003SFX.enabled=true then SC003FX=SC003FX+1: End If
    If SC003FX > 23 Then
          Spider003SFX.Enabled = True
          SC003Light.Enabled = True
          Spider003SFX.enabled=false
          SC003FX=0
    End If
End Sub


Dim SC004FX
SC004FX=0

Sub Spider004SFX_Timer
    If Spider004SFX.enabled=true then SC004FX=SC004FX+1: End If
    If SC004FX > 23 Then
          Spider004SFX.Enabled = True
          SC004Light.Enabled = True
          Spider004SFX.enabled=false
          SC004FX=0
    End If
End Sub


Dim CStoneEXPSFX
CStoneEXPSFX=0

Sub CStoneSFX_Timer
    If CStoneSFX.enabled=true then CStoneEXPSFX=CStoneEXPSFX+1: End If
    If CStoneEXPSFX > 23 Then
          CStoneSFX.Enabled = True
          CStoneLight.Enabled = True
          CStoneSFX.enabled=false
          CStoneEXPSFX=0
    End If
End Sub


' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
   	'startB2S(5)
    'GolgothRotateTimer.enabled = 0
if ballsonplayfield = 2 and ShootAgainLight.state = 0 then
'DMDframe.set true, 1
J1.state = 0
J2.state = 0
J3.state = 0
J4.state = 0

StopSound Song:Song = ""
ExtraBall.state = 2
end if

if ballsonplayfield = 2 and ShootAgainLight.state = 1  then
'DMDframe.set true, 1
J1.state = 0
J2.state = 0
J3.state = 0
J4.state = 0

StopSound Song:Song = ""
ExtraBall.state = 2
end if

      
    ' Destroy the ball
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1
    ' pretend to knock the ball into the ball storage mech
    PlaySound "fx_drain"





    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) Then

            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
			' we kick the ball with the autoplunger
			bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
 	 '       DMD_DisplaySceneEx "DMD1.png", "", 14, 2, "BALL SAVED", -1, -1, UltraDMD_Animation_ScrollOnUp, 1200, UltraDMD_Animation_ScrollOffDown
            GiEffect 2
            FlashEffect 2 
            DMDFLush  
            DMD "dmd baseBlank.png", " ", " BALL SAVED ", 150
			'********************************************************************************
			'                              BALL SAVE PUPS
			'********************************************************************************
            PuPlayer.playlistplayex pCallouts,"audiocallouts","ballsaved.mp4",100,1
		    'chilloutthemusic
            PuPlayer.playlistplayex pBackglass,"videoballsaved","",videovol,0
		    Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1) Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True) then 
                    ' not in multiball mode any more
                    bMultiBallMode = False
                   ' ChangeGi "white"
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
               '     ResetJackpotLights
                    'ChangeSong
                    ' PlaySong "GamePlay"

                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
                ' handle the end of ball (change player, high score entry etc..)

                EndOfBall()
                ' End Modes and timers
'                If bCatchemMode Then StopCatchem_Timer
'                If bcoinfrenzy Then StopCoinFrenzyTimer_Timer
'                If bPikachuTargetMode Then PikachuTargetTimer_Timer
'                If bCharizardMode Then StopCharizardTimer_Timer
'                If bRampBonus Then StopRampBonusTimer_Timer
'                If bLoopBonus Then StopLoopBonusTimer_Timer
              if kickbacklit.state = 0 Then
                kickbacktargets()
              End If
            End If
        End If
    End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.


Sub ballrelease_UnHit
   'DMDFLush
   'DMD "dmd baseBlank.png", "HIT THE", "FIRE BUTTON", 1500
   ' NightShadeTargetsOff
End Sub




Sub swPlungerRest_Hit()
	'startB2S(6)
    'debug.print "ball in plunger lane"
    ' some sound according to the ball position
    PlaySound "fx_sensor", 0, 1, 0.15, 0.25
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    LaunchLight.State = 2

    'be sure to update the Scoreboard after the animations, if any
    UltraDMDScoreTimer.Enabled = 1

    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        PlungerIM.AutoFire
        bAutoPlunger = False
    End If
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
    End If
    'Start the Selection of the skillshot if ready
    If bSkillShotReady Then
        swPlungerRest.TimerEnabled = 1 ' this is a new ball, so show the launch ball if inactive for 6 seconds
        UpdateSkillshot()
    End If
    ' remember last trigger hit by the ball.
    LastSwitchHit = "swPlungerRest"
End Sub


' The ball is released from the plunger turn off some flags and check for skillshot

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
    swPlungerRest.TimerEnabled = 0 'stop the launch ball timer if active
    If bSkillShotReady Then
        ResetSkillShotTimer.Enabled = 1
    End If
    If NOT bMultiballMode Then
        'PlaySong "mu_GamePlay"
    End If
    'LaserKickP.TransY = 100
    Playsound "bumper_retro"
End Sub


' swPlungerRest timer to show the "launch ball" if the player has not shot the ball during 6 seconds

Sub swPlungerRest_Timer
    DMD "dmd baseBlank.png", "Player "&CurrentPlayer, "let's play", 3000
    swPlungerRest.TimerEnabled = 0
End Sub


Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimerExpired.Interval = 1000 * seconds
    BallSaverTimerExpired.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    ShootAgainLight.BlinkInterval = 160
    SetLightColor ShootAgainLight, green, 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimerExpired_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimerExpired.Enabled = False
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
    ShootAgainLight.State = 0
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    ShootAgainLight.BlinkInterval = 80
    ShootAgainLight.State = 2
End Sub







' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
'
Sub AddScore(points)
    If(Tilted = False) Then
        ' add the points to the current players score variable
        Score(CurrentPlayer) = Score(CurrentPlayer) + points' * BallType
        ' update the score displays
        DMDScore
    End if

' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board
'
Sub AddBonus(points)
    If(Tilted = False) Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
        ' update the score displays
        DMDScore
    End if

' you may wish to check to see if the player has gotten a replay
End Sub

Sub AddCoin(n)
    If(Tilted = False) Then
        ' add the coins to the current players coin variable
        Coins(CurrentPlayer) = Coins(CurrentPlayer) + n
        ' update the score displays
        DMDScore
    End if

    ' check if there is enough coins to enable the update ball
    If Coins(CurrentPlayer) > 249 Then
        BallUpdateLight.State = 2
    Else
        BallUpdateLight.State = 0
    End If
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If(Tilted = False) Then

        If(bMultiBallMode = True) Then
            Jackpot = Jackpot + points
        ' you may wish to limit the jackpot to a upper limit, ie..
        '	If (Jackpot >= 6000) Then
        '		Jackpot = 6000
        ' 	End if
        End if
    End if
End Sub

' Will increment the Bonus Multiplier to the next level
'
Sub IncrementBonusMultiplier()
    Dim NewBonusLevel

    ' if not at the maximum bonus level
    if(BonusMultiplier(CurrentPlayer) < MaxMultiplier) then
        ' then set it the next next one AND set the lights
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + 1
        SetBonusMultiplier(NewBonusLevel)
    End if
End Sub

' Set the Bonus Multiplier to the specified level AND set any lights accordingly
'
Sub SetBonusMultiplier(Level)
    ' Set the multiplier to the specified level
    BonusMultiplier(CurrentPlayer) = Level

    ' If the multiplier is 1 then turn off all the bonus lights
    If(BonusMultiplier(CurrentPlayer) = 1) Then
 '       LightBonus2x.State = 0
 '       LightBonus3x.State = 0
 '       LightBigBonus.State = 0
    Else
        ' there is a bonus, turn on all the lights upto the current level
        If(BonusMultiplier(CurrentPlayer) >= 2) Then
            If(BonusMultiplier(CurrentPlayer) >= 2) Then
  '              LightBonus2x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 3) Then
   '             LightBonus3x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 4) Then
   '             LightBigBonus.state = 1
            End If
        End If
    ' etc..
    End If
End Sub

Sub IncrementBonus(Amount)
    Dim Value
    AddBonus Amount * 1000
    Bonus = Bonus + Amount
End Sub

Sub AwardSpecial()
    DMDblink "dmd baseBlank.png", " ", "EXTRA GAME WON", 100, 10
    Credits = Credits + 1
    GiEffect 1
    LightEffect 1
End Sub


'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 100000 End If

    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If

    x = LoadValue(TableName, "HighScore2")
    If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 100000 End If

    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If

    x = LoadValue(TableName, "HighScore3")
    If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 100000 End If

    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If

    x = LoadValue(TableName, "HighScore4")
    If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 100000 End If

    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If

    x = LoadValue(TableName, "Credits")
    If(x <> "") then Credits = CInt(x) Else Credits = 0 End If

    'x = LoadValue(TableName, "Jackpot")
    'If(x <> "") then Jackpot = CDbl(x) Else Jackpot = 200000 End If
    x = LoadValue(TableName, "TotalGamesPlayed")
    If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If
End Sub

Sub Savehs
    SaveValue TableName, "HighScore1", HighScore(0)
    SaveValue TableName, "HighScore1Name", HighScoreName(0)
    SaveValue TableName, "HighScore2", HighScore(1)
    SaveValue TableName, "HighScore2Name", HighScoreName(1)
    SaveValue TableName, "HighScore3", HighScore(2)
    SaveValue TableName, "HighScore3Name", HighScoreName(2)
    SaveValue TableName, "HighScore4", HighScore(3)
    SaveValue TableName, "HighScore4Name", HighScoreName(3)
    SaveValue TableName, "Credits", Credits
    'SaveValue TableName, "Jackpot", Jackpot
    SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

' ***********************************************************
'  High Score Initals Entry Functions - based on Black's code
' ***********************************************************

Dim hsbModeActive
Dim hsEnteredName
Dim hsEnteredDigits(3)
Dim hsCurrentDigit
Dim hsValidLetters
Dim hsCurrentLetter
Dim hsLetterFlash

Sub CheckHighscore()
    'PuPlayer.playlistplayex pBackglass,"videoattract","HighScore.mp4",videovol,1
    'PuPlayer.playlistplayex pMusic,"audiomultiballs","HighScore.mp3",0,1
    'NoAudioWizardJPTimer001.Enabled = True
    'NoAudioWizardJPTimer.Enabled = True
    Dim tmp
    tmp = Score(1)

    If Score(2)> tmp Then tmp = Score(2)
    If Score(3)> tmp Then tmp = Score(3)
    If Score(4)> tmp Then tmp = Score(4)

    If tmp> HighScore(1) Then 'add 1 credit for beating the highscore
        AwardSpecial
    End If

    If tmp> HighScore(3) Then
        vpmtimer.addtimer 2000, "PlaySound ""goldrake voce"" '"
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    PlaySound "fx_knocker"
    hsLetterFlash = 0

    hsEnteredDigits(0) = "A"
    hsEnteredDigits(1) = "A"
    hsEnteredDigits(2) = "A"
    hsCurrentDigit = 0

    hsValidLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ<+-0123456789" ' < is used to delete the last letter
    hsCurrentLetter = 1
    DMDFlush
    DMDId "hsc", "Enter.png", "YOUR NAME:", " ", 999999
    HighScoreDisplayName()
    PuPlayer.playlistplayex pBackglass,"videoattract","HighScore.mp4",videovol,1
    NoAudioWizardJPTimer001.Enabled = True
    NoAudioWizardJPTimer.Enabled = True
    PuPlayer.playlistplayex pMusic,"audiomodes","HighScore.mp3",0,1
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        Playsound "fx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayName()
    End If

    If keycode = RightFlipperKey Then
        Playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayName()
    End If

    If keycode = StartGameKey OR keycode = PlungerKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) then
                HighScoreCommitName()
            else
                HighScoreDisplayName()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit> 0) then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayName()
        end if
    end if
End Sub

Sub HighScoreDisplayName()
    Dim i, TempStr

    TempStr = " >"
    if(hsCurrentDigit> 0) then TempStr = TempStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2) then TempStr = TempStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempStr = TempStr & "_"
        else
            TempStr = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1) then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) then TempStr = TempStr & hsEnteredDigits(2)

    TempStr = TempStr & "< "
    DMDMod "hsc", "YOUR NAME:", Mid(TempStr, 2, 5), 999999
End Sub

Sub HighScoreCommitName()
    hsbModeActive = False
    'PlaySong "m_end"
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ") then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
    DMDFlush
    EndOfBallComplete()
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) <HighScore(j + 1) Then
                tmp = HighScore(j + 1)
                tmp2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = tmp
                HighScoreName(j) = tmp2
            End If
        Next
    Next
End Sub



'***********************************************************************************
'         	    JPS DMD - very simple DMD routines using UltraDMD
'***********************************************************************************

Dim UltraDMD

' DMD using UltraDMD calls

Sub DMD(background, toptext, bottomtext, duration)
    UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, 14, duration, 14
    UltraDMDScoreTimer.Enabled = 1                               'to show the score after the animation/message
End Sub

Sub DMDBlink(background, toptext, bottomtext, duration, nblinks) 'blinks the lower text nblinks times
    Dim i
    For i = 1 to nblinks
        UltraDMD.DisplayScene00 background, toptext, 15, bottomtext, 15, 14, duration, 14
    Next

    UltraDMDScoreTimer.Enabled = 1 'to show the score after the animation/message
End Sub

Sub DMDScore
    If NOT UltraDMD.IsRendering Then
        UltraDMD.SetScoreboardBackgroundImage "DMD.jpg", 15, 15
        UltraDMD.DisplayScoreboard PlayersPlayingGame, CurrentPlayer, Score(1), Score(2), Score(3), Score(4), "Player " & CurrentPlayer, "Ball " & Balls 
      Else
        UltraDMDScoreTimer.Enabled = 1
      End If

End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMDFLush
    UltraDMDTimer.Enabled = 0
    UltraDMDScoreTimer.Enabled = 0
    UltraDMD.CancelRendering
    UltraDMD.Clear
End Sub

Sub DMDScrollCredits(background, text, duration)
    UltraDMD.ScrollingCredits background, text, 15, 14, duration, 14
End Sub

Sub DMDId(id, background, toptext, bottomtext, duration)
    UltraDMD.DisplayScene00ExwithID id, False, background, toptext, 15, 0, bottomtext, 15, 0, 14, duration, 14
End Sub

Sub DMDMod(id, toptext, bottomtext, duration)
    UltraDMD.ModifyScene00Ex id, toptext, bottomtext, duration
End Sub

Sub UltraDMDTimer_Timer() 'used for repeating the attrack mode.
    If bAttractMode Then
        ShowTableInfo
    End If
End Sub

Sub UltraDMDScoreTimer_Timer()
    If NOT UltraDMD.IsRendering Then
        DMDScoreNow
    End If
End Sub




Sub DMD_Init
    Set UltraDMD = CreateObject("UltraDMD.DMDObject")
    If UltraDMD is Nothing Then
        MsgBox "No UltraDMD found.  This table will NOT run without it."
        Exit Sub
    End If

    UltraDMD.Init
    If Not UltraDMD.GetMajorVersion = 1 Then
        MsgBox "Incompatible Version of UltraDMD found."
        Exit Sub
    End If

    If UltraDMD.GetMinorVersion <1 Then
        MsgBox "Incompatible Version of UltraDMD found. Please update to version 1.1 or newer."
        Exit Sub
    End If

    Dim fso:Set fso = CreateObject("Scripting.FileSystemObject")
    Dim curDir:curDir = fso.GetAbsolutePathName(".")

    Dim DirName
    DirName = curDir& "\" &TableName& ".UltraDMD"

    If Not fso.FolderExists(DirName) Then _
            Msgbox "UltraDMD userfiles directory '" & DirName & "' does not exist." & CHR(13) & "No graphic images will be displayed on the DMD"
    UltraDMD.SetProjectFolder DirName

    ' wait for the animation to end
    While UltraDMD.IsRendering = True
    WEnd

    ' Show ROM version number
    DMDblink "dmd baseBlank.png", " ", " ", 1000,2
    DMD "dmd baseBlank.png", "Spooky Wednesday", "VPX Original " &myVersion, 2000

End Sub





' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    Dim i

    

    'info goes in a loop only stopped by the credits and the startkey
    If Score(1) Then
        DMD "dmd baseBlank.png", "PLAYER 1", Score(1), 3000
    End If
    If Score(2) Then
        DMD "dmd baseBlank.png", "PLAYER 2", Score(2), 3000
    End If
    If Score(3) Then
        DMD "dmd baseBlank.png", "PLAYER 3", Score(3), 3000
    End If
    If Score(4) Then
        DMD "dmd baseBlank.png", "PLAYER 4", Score(4), 3000
    End If

    'coins or freeplay
    If bFreePlay Then
        DMD "dmd baseBlank.png", " ", "FREE PLAY", 2500
        DMD "introCompleta3.png", "", "", 9500
        DMD "dmd baseBlank.png", "SPOOKY WEDNESDAY", "VPX ORIGINAL BY", 4200
        DMD "intro logo.png", "", "", 4000
        'DMD "Thank You 1.png", "", "", 4000
        'DMD "Thank You 2.png", "", "", 4000
        'DMD "Thank You 3.png", "", "", 4000
        DMD "introCompleta3.png", "", "", 9500
    Else
        If Credits> 0 Then
            DMD "dmd baseBlank.png", " CREDITS " &credits, " PRESS START ", 2000
        Else
            DMD "dmd baseBlank.png", " CREDITS " &credits, " INSERT COIN ", 2000
        End If
        DMD "introCompleta3.png", "", "", 9500
        DMD "dmd baseBlank.png", "", "", 2500
    End If

    DMD "dmd baseBlank.png", "HIGHSCORES", "1> " & HighScoreName(0) & " " & FormatNumber(HighScore(0), 0, , , -1), 3000
    DMD "dmd baseBlank.png", "HIGHSCORES", "2> " & HighScoreName(1) & " " & FormatNumber(HighScore(1), 0, , , -1), 3000
    DMD "dmd baseBlank.png", "HIGHSCORES", "3> " & HighScoreName(2) & " " & FormatNumber(HighScore(2), 0, , , -1), 3000
    DMD "dmd baseBlank.png", "HIGHSCORES", "4> " & HighScoreName(3) & " " & FormatNumber(HighScore(3), 0, , , -1), 3000
End Sub


Sub StartAttractMode()
    bAttractMode = True
    UltraDMDTimer.Enabled = 1
   '****************************************************
   '    PUPATTRACT AND DISCOATTRACT FOR ATTRACTMODE
   '****************************************************
    PuPlayer.playlistplayex pBackglass,"videoattract","intro-smaller.mp4",videovol,1
    PuPlayer.SetBackground pBackglass,1
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0, 0
	StartDiscoAttract
    ZombieAttractOnTimer_Timer
    ZombieAttractResetTimer_Timer
    Pyramid2Exit
    ThingShake
    thingapronrainbowtimer.Enabled = True 
   '*************************************
   '   HYDE ANIMATION FOR ATTRACTMODE
   '*************************************
   SlimerMoveUp
   SlimerUpTimer.Enabled = True
   showhydetimer.Enabled = True
   '*************************************
  
   '*************************************
    introflasherDance1.opacity = 100
    '**********************************
   '      TIMER FOR BLOOD_APRON
   '**********************************
    ExplsionSFX.Enabled = True
   '**********************************
    StartLightSeq
    ShowTableInfo
 'Animaciones
    ActarusTimer360.enabled = 1
    AlabardaAnim() 
   'Start SmallDMD
    'vpmtimer.AddTimer 25000, "IntroSmallDMD '"
End Sub

Sub StopAttractMode()
    bAttractMode = False
    DMDScoreNow
    LightSeqAttract.StopPlay
    LightSeqFlasher.StopPlay
 'StopSong
 End Sub




'********************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first myVersion

    If TypeName(MyLight) = "Light" Then

        If FinalState = 2 Then
            FinalState = MyLight.State 'Keep the current light state
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then

        Dim steps

        ' Store all blink information
        steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
        If FinalState = 2 Then                      'Keep the current flasher state
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

        ' Start blink timer and create timer subroutine
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub


'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 10 colors: red, orange, amber, yellow...
'******************************************
' in this table this colors are use to keep track of the progress during the acts and battles

'colors
Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white

red = 10
orange = 9
amber = 8
yellow = 7
darkgreen = 6
green = 5
blue = 4
darkblue = 3
purple = 2
white = 1

Sub SetLightColor(n, col, stat)
    Select Case col
        Case 0
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(255, 0, 0)
        Case red
            n.color = RGB(18, 0, 0)
            n.colorfull = RGB(255, 0, 0)
        Case orange
            n.color = RGB(18, 3, 0)
            n.colorfull = RGB(255, 64, 0)
        Case amber
            n.color = RGB(193, 49, 0)
            n.colorfull = RGB(255, 153, 0)
        Case yellow
            n.color = RGB(18, 18, 0)
            n.colorfull = RGB(255, 255, 0)
        Case darkgreen
            n.color = RGB(0, 8, 0)
            n.colorfull = RGB(0, 64, 0)
        Case green
            n.color = RGB(0, 18, 0)
            n.colorfull = RGB(0, 255, 0)
        Case blue
            n.color = RGB(0, 18, 18)
            n.colorfull = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 8, 8)
            n.colorfull = RGB(0, 64, 64)
        Case purple
            n.color = RGB(128, 0, 128)
            n.colorfull = RGB(255, 0, 255)
        Case white
            n.color = RGB(255, 252, 224)
            n.colorfull = RGB(193, 91, 0)
        Case white
            n.color = RGB(255, 252, 224)
            n.colorfull = RGB(193, 91, 0)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub







Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    ExFlasher001.visible = False
    ExplsionSFX.Enabled = True
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    ExFlasher001.visible = False
    ExplsionSFX.Enabled = True
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    ExFlasher001.visible = False
    ExplsionSFX.Enabled = True
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    ExFlasher001.visible = False
    ExplsionSFX.Enabled = True
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 40, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 40, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqRightOn, 30, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqLeftOn, 30, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqStripe1VertOn, 50, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqSkillshot_PlayDone()
    LightSeqSkillshot.Play SeqAllOff
End Sub


'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************


' tables walls and animations
Sub VPObjects_Init
End Sub


'**************
'Animates Toys
'**************


''Actarus
Sub ActarusTimer360_Timer
    'ActarusP.RotY = ActarusP.RotY  + 5
End Sub


Dim ActarusA, ActarusB, ActarusC, ActarusY


Sub ActarusMoveA_Timer
'ActarusP.RotY = 0
'ActarusA = True
'ActarusY = ActarusY + 5
'ActarusP.RotY = ActarusY
End Sub

Sub ActarusMoveB_Timer
'ActarusP.RotY = 0
'ActarusB = True
'ActarusY = ActarusY + 5
'ActarusP.RotY = ActarusY
End Sub

Sub ActarusMoveC_Timer
'ActarusP.RotY = 0
'ActarusC = True
'ActarusY = ActarusY + 5
'ActarusP.RotY = ActarusY
End Sub

                 

'Alabarda
Sub AlabardaTimer_Timer
    Alabarda.RotY = Alabarda.RotY + 10
 If PlayersPlayingGame Then
    vpmtimer.addtimer 1000, "AlabardaAnimOff '"
 End If
End Sub

Sub AlabardaAnim
    AlabardaTimer.enabled = 1
    ZombieShake
    'playsound "Scream 8"
    playsound "sfx_sword1"
End Sub

Sub AlabardaAnimOff
    AlabardaTimer.enabled = 0
    ZombieShakeTimer.Enabled = False
End Sub


'Boomeran
Dim boomerD
sub Timerboomer_Timer()
   ' boomer.transZ = boomer.transZ - 1
    'boomerD = boomerD + 1
End Sub

Sub TimerboomerOff_Timer
    Timerboomer.enabled = 0
   ' boomerD = boomerD - 1
   ' boomer.transZ = boomer.transZ + 1
End Sub



'disko
Sub diskoTimer_Timer
    'disko.RotY = disko.RotY + 5
End Sub

Sub diskoAnim
    diskoTimer.enabled = 1
End Sub

Sub diskoAnimOff
    diskoTimer.enabled = 0
End Sub


Sub Game_Init()
    StartLOGOAni
    DMDFlush
    DMD "Snap Twice.wmv", "", "", 2800
    ZombieAttractOffTimer_Timer
    SpotlightAttractOff
	SlimerMoveDown
    StopDiscoAttract
    Stop_Fog
    bSlimerUp = False
    SlimerHits = 0
    SlimerDefeats = 0
    bSlimerSuperJackpot = False
    bSlimerSuperJackpot = False
    bExtraBallWonThisBall = False
    TurnOffPlayfieldLights()
    PuPlayer.SetLoop 4,1		
	PuPlayer.playlistplayex pCallouts,"audiocallouts","EvilLauch.mp3",90,1
    PuPlayer.playlistplayex pBackglass,"videosnapstart","snap snap.mp4",videovol,1
    PuPlayer.playlistplayex pBackglass,"scene","base.mp4",0,0
    PuPlayer.SetBackground pBackglass,1
    HydeKillAttractTime.Enabled = True
    NMLOGO.visible = 1
    Book2.visible = 0
    JailKey2.visible = 0
    Glasses2.visible = 0
    discoball.visible = 0
    GreenLightTimeron.Enabled = True
    RedLightTimeron.Enabled = True
    YellowLightTimeron.Enabled = True
    SlimerDrainVideoResetTimer.Enabled = True
    APGlow.state = 0
    killTimer.Enabled = 1
    Hyde2Timer.Enabled = 0
	SCAnimation.Enabled = True
	SC001Animation.Enabled = True
    SC002Animation.Enabled = True
    SC003Animation.Enabled = True
    SC004Animation.Enabled = True
    Hyde2DrainTimer.Enabled = False
    Hyde2offTimer.Enabled = False
    DiscoBlocker.Isdropped = 1
    MBBlocker.Isdropped = 1
    MBBlocker5.Isdropped = 1
    MBBlocker001.Isdropped = 1
    MBBlocker002.Isdropped = 1
    MBBlocker002Target.Enabled = false
    MBBlockerTarget.Enabled = false
    MBBlocker001Target.Enabled = false
    '*****************************
    '    SMALL DMD FLASHERS
    '*****************************
    DMDSmall1Timeroff.Enabled = 1
    DMDSmall2Timeron2.Enabled = 1
    DMDSmall1Timeron.Enabled = 0
    DMDSmall2Timeroff2.Enabled = 1
    DMDSmall3Timeron.Enabled = 1
   '*******************************
   
   '**********************************
    introflasherDance1.opacity = 0
    ExFlasher001.opacity = 0
   '**********************************
    Jackpot = 250000
    Lockdiverter.RotateToStart
    kickback.enabled = 1
    KickBackDiverter.RotateToStart
    
    

A1.state = 2
A2.state = 2
A3.state = 0
A4.state = 0
A5.state = 0
A6.state = 0
'FlashForMs A6F, 100, 10, 0  'off
A7.state = 0
'FlashForMs A7F, 100, 10, 0  'off
A8.state = 0
'FlashForMs A8F, 100, 10, 0  'off
Ak.state = 0
AKL.state = 0
'FlashForMs AKL, 100, 10, 0  'off
Ab.state = 0

B1.state = 2
B2.state = 0
B3.state = 0
B4.state = 0
B5.state = 0

J1.state = 0
J2.state = 0
J3.state = 0
J4.state = 0

ExtraBall.state = 0

kickbacklit.state = 1
ActivateKickbak.state = 0
kickbacktargets1.Isdropped = 1
kickbacktargets2.Isdropped = 1

SP1.state = 0
SP2.state = 0
SP3.state = 0
SP4.state = 0



'**************************************************************************
'				        BALL KICKOUT PUP_VIDEO
'**************************************************************************
 'PuPlayer.playlistplayex pBackglass,"videosnapstart","snap snap.mp4",videovol,1
 
 

''attractseq.stopplay
'flasher2.state = 0
FlashForMs flasher2, 1000, 10, 0  'off
'flasher6.state = 0
FlashForMs flasher6, 1000, 10, 0  'off
End Sub




Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub ResetNewBallLights()
'    LightArrow1.State = 2
'    LightArrow6.State = 2
'    l53.State = 2
End Sub


Sub UpdateSkillShot() 'Updates the skillshot light
    LightSeqSkillshot.Play SeqAllOff
'DMDFlush
End Sub


' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/modes this trigger is a member of
' - set the "LastSwicthHit" variable in case it is needed later
' *********************************************************************

' Slingshots has been hit

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    'startB2S(6)
    PlaySound "fx_slingshot", 0, 1, -0.05, 0.05
    LeftSling4.Visible = 1:LeftSling1.Visible = 0
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add some points
    AddScore 500
    ' add some effect to the table?
    Gi2.State = 0
    ' remember last trigger hit by the ball
    LastSwitchHit = "LeftSlingShot"
    FlashForms SlingFlashL, 1000, 50, 0 
    FlashForms SlingFlashL1, 1000, 50, 0 
    FlashForms SlingFlasherGreen001, 1000, 50, 0 
    FlashForms SlingFlasherGreen003, 1000, 50, 0
    ScorpionLShake 
 End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:LeftSling1.Visible = 1:Lemk.RotX = -10:Gi2.State = 1:LeftSlingShot.TimerEnabled = False
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    'startB2S(1)
    If Tilted Then Exit Sub
    PlaySound "fx_slingshot", 0, 1, 0.05, 0.05
    RightSling4.Visible = 1:RightSling1.Visible = 0
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add some points
    AddScore 500
    ' add some effect to the table?
    Gi1.State = 0
    ' remember last trigger hit by the ball
    LastSwitchHit = "RightSlingShot"
    FlashForms SlingFlashR, 1000, 50, 0
    FlashForms SlingFlashR1, 1000, 50, 0
    FlashForms SlingFlasherGreen002, 1000, 50, 0
    FlashForms SlingFlasherGreen004, 1000, 50, 0
    ScorpionRShake
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:RightSLing1.Visible = 1:Remk.RotX = -10:Gi1.State = 1:RightSlingShot.TimerEnabled = False
    End Select
    RStep = RStep + 1
End Sub




' LANE SWITCH SCRIPT (& Flipper scripting)


dim tempstateherolights 'to rotate lights


Sub LeftInLaneTrigger_Hit()
    'startB2S(2)
	Addscore(1000)
If bMultiBallMode then Exit Sub
if SP1.state = 1 and SP2.state = 0 and SP3.state = 1 and SP4.state = 1 then
EnableBallSaver 10
SP1.state = 0
SP2.state = 0
SP3.state = 0
SP4.state = 0
playsound "soundFX15"
FlashForms SlingFlashL, 1000, 50, 0
FlashForms SlingFlashL1, 1000, 50, 0
FlashForms SlingFlashR, 1000, 50, 0
FlashForms SlingFlashR1, 1000, 50, 0


end if
	If (SP2.State=0) then
		SP2.State=1
	End If

End Sub


Sub RightInLaneTrigger_Hit()
    'startB2S(3)
	Addscore 1000
If bMultiBallMode then Exit Sub
if SP1.state = 1 and SP2.state = 1 and SP3.state = 0 and SP4.state = 1 then

			' set our game flag
'			bBallSaverActive = TRUE
'			' start the timer
'			BallSaverTimer.Enabled = FALSE
'			BallSaverTimer.Interval = constBallSaverTime
'			BallSaverTimer.Enabled = TRUE
'			' if you have a ball saver light you might want to turn it on at this 
'			' point (or make it flash)
'			ShootAgainLight.State = 2
EnableBallSaver 10
SP1.state = 0
SP2.state = 0
SP3.state = 0
SP4.state = 0
playsound "soundFX15"
FlashForms SlingFlashL, 1000, 50, 0
FlashForms SlingFlashL1, 1000, 50, 0
FlashForms SlingFlashR, 1000, 50, 0
FlashForms SlingFlashR1, 1000, 50, 0
end if
	If (SP3.State=0) then
		SP3.State=1
	End If
End Sub



Sub LeftOutLaneTrigger_Hit()
    'startB2S(5)
	AddScore 5000 
If bMultiBallMode then Exit Sub
if SP1.state = 0 and SP2.state = 1 and SP3.state = 1 and SP4.state = 1 then

			' set our game flag
'			bBallSaverActive = TRUE
'			' start the timer
'			BallSaverTimer.Enabled = FALSE
'			BallSaverTimer.Interval = constBallSaverTime
'			BallSaverTimer.Enabled = TRUE
'			' if you have a ball saver light you might want to turn it on at this 
'			' point (or make it flash)
'			ShootAgainLight.State = 2
EnableBallSaver 10
SP1.state = 0
SP2.state = 0
SP3.state = 0
SP4.state = 0
playsound "soundFX15"
FlashForms SlingFlashL, 1000, 50, 0
FlashForms SlingFlashL1, 1000, 50, 0
FlashForms SlingFlashR, 1000, 50, 0
FlashForms SlingFlashR1, 1000, 50, 0
end if
	If (SP1.State=0) Then
		SP1.State=1
	End If
LastSwitchHit = "LeftOutLaneTrigger"
End Sub


Sub RightOutLaneTrigger_Hit()
    'startB2S(5)
	AddScore 5000
If bMultiBallMode then Exit Sub
if SP1.state = 1 and SP2.state = 1 and SP3.state = 1 and SP4.state = 0 then

			' set our game flag
'			bBallSaverActive = TRUE
'			' start the timer
'			BallSaverTimer.Enabled = FALSE
'			BallSaverTimer.Interval = constBallSaverTime
'			BallSaverTimer.Enabled = TRUE
'			' if you have a ball saver light you might want to turn it on at this 
'			' point (or make it flash)
'			ShootAgainLight.State = 2
EnableBallSaver 10
SP1.state = 0
SP2.state = 0
SP3.state = 0
SP4.state = 0
playsound "soundFX15"
FlashForms SlingFlashL, 1000, 50, 0
FlashForms SlingFlashL1, 1000, 50, 0
FlashForms SlingFlashR, 1000, 50, 0
FlashForms SlingFlashR1, 1000, 50, 0
end if
	If (SP4.State=0) then
		SP4.State=1
	End If
LastSwitchHit = "RightOutLaneTrigger"
End Sub








'-----------------------------------------------------
'**************************************************************************
'timed circular 4 flashers:
'integrate this in script somewhere (1000 or 2000 must be VARIATED!!!)
'fltimer1.set true, 1
'fltimer2.set true, 2000


sub fltimer1_Timer()
fltimer1.enabled = false
FlashForms SlingFlashL, 100, 50, 0
FlashForms SlingFlashL1, 100, 50, 0
FlashForms flasher2, 1000, 50, 0
FlashForms flasher6, 1000, 50, 0
FlashForms flasher7, 1000, 50, 0
FlashForms flasher7b, 1000, 50, 0

'flasher1.BlinkInterval = 65
'flasher2.BlinkInterval = 65
'flasher6.BlinkInterval = 65
'flasher7.BlinkInterval = 65

'flasher1.state = bulbblink
'flasher2.state = bulbblink
'flasher6.state = bulbblink
'flasher7.state = bulbblink
end sub

sub fltimer2_Timer()
fltimer2.enabled = false
FlashForms SlingFlashR, 100, 50, 0
FlashForms SlingFlashR1, 100, 50, 0
FlashForms flasher2, 1000, 50, 0
FlashForms flasher6, 1000, 50, 0
FlashForms flasher7, 1000, 50, 0
FlashForms flasher7b, 1000, 50, 0
end sub



'timed circular 8 jackpot flashers:
'integrate this in script somewhere (1000 or 2000 must be VARIATED!!!)
'fltimerA.set true, 1
'fltimerB.set true, 2000


sub fltimerA_Timer()
fltimerA.enabled = false

FlashForms SlingFlashL, 1000, 50, 0
FlashForms SlingFlashL1, 1000, 50, 0
FlashForms flasher2, 1000, 50, 0
FlashForms flasher3, 1000, 50, 0
FlashForms flasher4, 1000, 50, 0.
FlashForms flasher4b, 1000, 50, 0
FlashForms SlingFlashR, 1000, 50, 0
FlashForms SlingFlashR1, 1000, 50, 0
FlashForms flasher6, 1000, 50, 0
FlashForms flasher7, 1000, 50, 0
FlashForms flasher7b, 1000, 50, 0
FlashForms flasher8, 1000, 50, 0
FlashForms flasher8b, 1000, 50, 0
FlashForms flasher9, 1000, 50, 0
FlashForms flasher9b, 1000, 50, 0


FlashForms SlingFlashL, 1000, 50, 0
FlashForms SlingFlashL1, 1000, 50, 0
FlashForms flasher2, 1000, 50, 0
FlashForms flasher3, 1000, 50, 0
FlashForms flasher4, 1000, 50, 0
FlashForms flasher4b, 1000, 50, 0
FlashForms SlingFlashR, 1000, 50, 0
FlashForms SlingFlashR1, 1000, 50, 0
FlashForms flasher6, 1000, 50, 0
FlashForms flasher7, 1000, 50, 0
FlashForms flasher7b, 1000, 50, 0
FlashForms flasher8, 1000, 50, 0
FlashForms flasher8b, 1000, 50, 0
FlashForms flasher9, 1000, 50, 0
FlashForms flasher9b, 1000, 50, 0

FlashForms SlingFlashL, 1000, 50, 0
FlashForms SlingFlashL1, 1000, 50, 0
FlashForms flasher2, 1000, 50, 0
FlashForms flasher3, 1000, 50, 0
FlashForms flasher4, 1000, 50, 0
FlashForms flasher4b, 1000, 50, 0
FlashForms SlingFlashR, 1000, 50, 0
FlashForms SlingFlashR1, 1000, 50, 0
FlashForms flasher6, 1000, 50, 0
FlashForms flasher7, 1000, 50, 0
FlashForms flasher7b, 1000, 50, 0
FlashForms flasher8, 1000, 50, 0
FlashForms flasher8b, 1000, 50, 0
FlashForms flasher9, 1000, 50, 0
FlashForms flasher9b, 1000, 50, 0

FlashForms SlingFlashL, 1000, 50, 0
FlashForms SlingFlashL1, 1000, 50, 0
FlashForms flasher2, 1000, 50, 0
FlashForms flasher3, 1000, 50, 0
FlashForms flasher4, 1000, 50, 0
FlashForms flasher4b, 1000, 50, 0
FlashForms SlingFlashR, 1000, 50, 0
FlashForms SlingFlashR1, 1000, 50, 0
FlashForms flasher6, 1000, 50, 0
FlashForms flasher7, 1000, 50, 0
FlashForms flasher7b, 1000, 50, 0
FlashForms flasher8, 1000, 50, 0
FlashForms flasher8b, 1000, 50, 0
FlashForms flasher9, 1000, 50, 0
FlashForms flasher9b, 1000, 50, 0


'SlingFlashL.blinkpattern = "10001000"
'flasher2.blinkpattern = "01000000"
'flasher7.blinkpattern = "00100100"
'SlingFlashR.blinkpattern = "00010000"
'flasher4.blinkpattern = "10000100"
'flasher3.blinkpattern = "01000000"
'flasher8.blinkpattern = "00101000"
'flasher9.blinkpattern = "00010001"

'SlingFlashL.BlinkInterval = 65
'flasher2.BlinkInterval = 65
'flasher7.BlinkInterval = 65
'SlingFlashR.BlinkInterval = 65
'flasher4.BlinkInterval = 65
'flasher3.BlinkInterval = 65
'flasher8.BlinkInterval = 65
'flasher9.BlinkInterval = 65

'SlingFlashL.state = bulbblink
'flasher2.state = bulbblink
'flasher7.state = bulbblink
'SlingFlashR.state = bulbblink
'flasher4.state = bulbblink
'flasher3.state = bulbblink
'flasher8.state = bulbblink
'flasher9.state = bulbblink
end sub

sub fltimerB_Timer()
fltimerB.enabled = false
'flasher1.state = bulboff
'flasher2.state = bulboff
'flasher7.state = bulboff
'SlingFlashR.state = bulboff
'flasher4.state = bulboff
'flasher3.state = bulboff
'flasher8.state = bulboff
'flasher9.state = bulboff



FlashForms SlingFlashL, 1000, 50, 0
FlashForms SlingFlashL1, 1000, 50, 0
FlashForms flasher2, 1000, 50, 0
FlashForms flasher3, 1000, 50, 0
FlashForms flasher4, 1000, 50, 0
FlashForms flasher4b, 1000, 50, 0
FlashForms SlingFlashR, 1000, 50, 0
FlashForms SlingFlashR1, 1000, 50, 0
FlashForms flasher6, 1000, 50, 0
FlashForms flasher7, 1000, 50, 0
FlashForms flasher7b, 1000, 50, 0
FlashForms flasher8, 1000, 50, 0
FlashForms flasher8b, 1000, 50, 0
FlashForms flasher9, 1000, 50, 0
FlashForms flasher9b, 1000, 50, 0
end sub




'Kick it back:

sub kickbacktargets1_hit()
 'startB2S(5)
If Tilted Then Exit Sub
addscore 750
PlaySound "wham"
kickbacktargets()
LastSwitchHit = "kickbacktargets1"
end sub

sub kickbacktargets2_hit()
    'startB2S(5)
If Tilted Then Exit Sub
addscore 750
PlaySound "wham"
kickbacktargets()
LastSwitchHit = "kickbacktargets2"
end sub


Sub kickbacktargets()
if (kickbacktargets1.isdropped + kickbacktargets2.isdropped = 1) then
addscore 1500
TruckShake
PlaySound "fx_droptarget"
DMDFLush
DMD "dmd baseBlank.png", "", "KICKBACK ACTIVE", 1500
kickback.enabled = 1
playsound "newsfx02"
playsound "laser10"
kickbacklit.state = 1
ActivateKickbak.state = 0
KickBackDiverter.RotateToStart
Else
kickback.enabled = 0
kickbacklit.state = 0
kickbacktargets1.Isdropped = 0
kickbacktargets2.Isdropped = 0
KickBackDiverter.RotateToEnd
end if
end sub


sub kickback_hit()
    'startB2S(6)
if kickbacklit.state = 1 then
kickbacklit.state = 0
FlashForms SlingFlashL, 1000, 50, 0
FlashForms SlingFlashL1, 1000, 50, 0
FlashForms SlingFlashR, 1000, 50, 0
FlashForms SlingFlashR1, 1000, 50, 0
kickback.kick  0, 45
playsound "Ghostly Whisper"
kickbacktargets1.isdropped = 0
kickbacktargets2.isdropped = 0
ActivateKickbak.state = 2
addscore 2500
Timerboomer.enabled = 1
KickBackDiverter.RotateToEnd
Else
Kickback.kick 110, 8
end if
LastSwitchHit = "kickback"
end sub





'**************************************************************************
'-----Small things---------------


sub timeractarus_Timer()
    timeractarus.enabled = false
		DispDmd1.Text = "[f6][edge4][b][y8]PRESS 1 TO PLAY[/b]"
		DispDmd2.Text = "[f6][edge4][b][y8]PRESS 1 TO PLAY[/b]"
end sub


 
dim StopEgo
sub Trigger28_hit  '..out
    'startB2S(6)
	StopEgo=true
end sub
Sub Trigger32_Hit() '..in
    'startB2S(1)
	If BallsOnPlayfield = 1 then
		if StopEgo=false then
			playsound "goldrake raggio ant" '..in
           
            'DMD "dmd baseBlank.png", "_", "  ANTIGRAVITY BEAM  ", 1500
		'   DispDmd1.QueueText "[f6][b][edge4][y4]ANTIGRAVITY[y17]BEAM[/b]",, seScrollRight, 2000, True
		'   DispDmd2.QueueText "[f6][b][edge4][y4]ANTIGRAVITY[y17]BEAM[/b]",, seScrollRight, 2000, True
		end if
		if StopEgo=true then
			StopEgo=false
			PlaySound "'''''''" '..out
		end if

	End If
End Sub


dim StopEgo2
sub Trigger35_hit  '..out
    'startB2S(6)
	StopEgo2=true
end sub
Sub Trigger36_Hit()  '..in
	If BallsOnPlayfield = 1 then
		if StopEgo2=false then
			playsound "goldrake raggi las" '..in
            'DMD "dmd baseBlank.png", "_", "   LASER BEAM   ", 1500
		 '  DispDmd1.QueueText "[f6][edge4][b][y8]LASER BEAM[/b]", seScrollLeft, 2000, True
		 '  DispDmd2.QueueText "[f6][edge4][b][y8]LASER BEAM[/b]", seScrollLeft, 2000, True
		end if
		if StopEgo2=true then
			StopEgo2=false
			PlaySound "" '..out
		end if
	End If
End Sub













sub trigger1_hit()
If Tilted Then Exit Sub
playsound "sfx_sword1"
'playsound "Scream 8"
'alabarta.rotatexz -300, 345
AlabardaAnim
end sub

sub trigger39_hit()
If Tilted Then Exit Sub
AlabardaAnim
end sub


Sub trigger14_hit()
	'startB2S(1)
If Tilted Then Exit Sub
addscore 500
playsound "laser05"
    If LastSwitchHit = "Trigger2" Then 'give combo
        ComboValue = 5000 + Round(Score(CurrentPlayer) / 10, 0)
        DMD "dmd baseBlank.png", "COMBO", ComboValue, 100
        AddScore ComboValue
        ComboM = ComboM + 1
        CheckComboMultiball
		'StopDisco
    End If
LastSwitchHit = "trigger14"
End Sub



Sub trigger2_hit()
	'startB2S(4)
If Tilted Then Exit Sub
addscore 500
playsound "laser05"
    If LastSwitchHit = "Trigger14" Then 'give combo
        ComboValue = 5000 + Round(Score(CurrentPlayer) / 10, 0)
        DMD "dmd baseBlank.png", "COMBO", ComboValue, 100
        AddScore ComboValue
        ComboM = ComboM + 1
        CheckComboMultiball
		'StopDisco
    End If
LastSwitchHit = "Trigger2"
End Sub

Sub Trigger3_hit()
PlaySound "fx_centrifugue2"
End Sub



sub trigger16_hit()
If Tilted Then Exit Sub
addscore 500
end sub

sub trigger15_hit()
	'startB2S(2)
If Tilted Then Exit Sub
addscore 500
playsound "Werewolf"
EletricTimer.Enabled = True
If B4.state = 2 Then
   StopSound "RotorStart2"
   'DMD "SpinSaucer.wmv", "", "", 4500
End If
If B2SOn Then Controller.B2SSetData 3,0:Controller.B2SSetData 2,1
LastSwitchHit = "Trigger15"
end sub


sub trigger18_hit()
	'startB2S(4)
If Tilted Then Exit Sub
If B2SOn Then Controller.B2SSetData 2,0:Controller.B2SSetData 3,1
addscore 500
end sub

sub trigger17_hit()
If Tilted Then Exit Sub
If B2SOn Then Controller.B2SSetData 2,0:Controller.B2SSetData 3,1
StartElectricityLightFXSeq
LighteningFlashTimer.Enabled = True
flashforms  lightening001,2000, 50, 0
flashforms  lightening004,2100, 30, 0
flashforms  lightening005,2300, 20, 0
playsound "sfx_thunder1"
playsound "EvilVoices"
addscore 500
FlashForMs flasher6, 100, 10, 0  'off
'DMD "dmd baseBlank.png", "_", "  HAND BEAM  ", 1500
'GolgothRotateTimer.enabled = 0
'playSound "RotateLoop"
LastSwitchHit = "Trigger17"
end sub

sub GolgothRotatetrigger_hit()
   If B2SOn Then Controller.B2SSetData 2,0:Controller.B2SSetData 3,1
    'playSound "RotateLoop"
    GolgothRotateTimer.enabled = 1       

   If bMultiBallMode Then
      Lockdiverter.RotateToStart
     Else
   if B4.state = 0 And B5.state = 0 Then Lockdiverter.RotateToStart
   if B4.state = 2 And B5.state = 0 Then Lockdiverter.RotateToEnd
   if B4.state = 1 And B5.state = 2 Then Lockdiverter.RotateToEnd
  End If
End Sub



sub trigger27_hit()
If Tilted Then Exit Sub
playsound "goldrake rientro"
'DispDmd1.QueueText "[f6][edge4][b][y8]GOLDRAKE BACK[/b]", deNone , 2000, True
'DispDmd2.QueueText "[f6][edge4][b][y8]GOLDRAKE BACK[/b]", deNone , 2000, True
 'DMD "dmd baseBlank.png", "_", "GOLDRAKE BACK", 1800
end sub

sub trigger40_hit()
If Tilted Then Exit Sub
playsound "goldrake rientro"
'DispDmd1.QueueText "[f6][edge4][b][y8]GOLDRAKE BACK[/b]", deNone , 2000, True
'DispDmd2.QueueText "[f6][edge4][b][y8]GOLDRAKE BACK[/b]", deNone , 2000, True
 'DMD "dmd baseBlank.png", "_", "GOLDRAKE BACK", 1500
end sub


sub trigger42_hit()
If Tilted Then Exit Sub
FlashForms SlingFlashR, 1000, 50, 0
FlashForms SlingFlashR1, 1000, 50, 0
'playmusic 2, "goldrake avanti", false
'DMDframe.set true, 2500
'dmdsec 32, 38, "[f11][edge4]", 150   '------- goldrake go
'dmdsec 32, 38, "[f11][edge4]", 150
end sub



sub trigger12_hit()
If Tilted Then Exit Sub
FlashForms bulb9, 1000, 50, 0
FlashForms bulb10, 1000, 50, 0
end sub

sub Atarget1_hit()
	'startB2S(1)
If Tilted Then Exit Sub
playsound "sfx_electric6"
addscore 1500
LastSwitchHit = "Atarget1"
FlashForms ThingMagicLightning, 1000, 50, 0
end sub

sub Atarget2_hit()
	'startB2S(6)
If Tilted Then Exit Sub
playsound "sfx_electric6"
addscore 1500
LastSwitchHit = "Atarget2"
FlashForms ThingMagicLightning, 1000, 50, 0
end sub


sub trigger7_hit()
	'startB2S(1)
LaserKickP.TransY = 0
If Tilted Then Exit Sub
FlashForms SlingFlashR, 1000, 50, 0
FlashForms SlingFlashR1, 1000, 50, 0
playsound "soundFX13"
' turn off LaunchLight
 LaunchLight.State = 0
end sub

sub trigger7_Unhit()
playsound "ActarusGoldorakGoA"
end sub


sub trigger24_hit()
If Tilted Then Exit Sub
FlashForms UFOflasher, 1000, 50, 0
playsound "soundFX13"
end sub



'--------------------BUMPERS SCRIPT:



Dim BHit                   
Sub Bumper_Hit()
    'startB2S(6)
	 BHit = BHit + 1
   If (Bulb30.State = 2) Then Exit Sub
   '  Add Score, Bonus or Jackpot
   '  Play Sound
	'	DispDmd1.QueueText "[f6][edge4][b][y5]" & BHit & " / 15[y17]HIT FOR ATTACK", deNone, 2000, True
	'	DispDmd2.QueueText "[f6][edge4][b][y5]" & BHit & " / 15[y17]HIT FOR ATTACK", deNone, 2000, True
      '  DMDFLush
        'DMD "dmd baseBlank.png", "_", " " & BHit & " HIT FOR ATTACK", 800
   If (BHit = 15) Then 
      Bulb30.State = 2
      'Timerbutton.set true , 3000
      vpmtimer.addtimer 3000, "Timerbutton.enabled = 1'"
     ' DMDframe.set true, 6200
      BHit = 0
   End If
End Sub




sub Timerbutton_Timer()
'SlimerMoveUp
'SlimerUpTimer.Enabled = True
Timerbutton.enabled = 0
'playsound "hydehit"
'DispDmd1.QueueText "[f6][edge4][b][y4]PRESS SPECIAL[y17]KEY1 FOR ATTACK[/b]", deNone, 5000, True
'DispDmd2.QueueText "[f6][edge4][b][y4]PRESS SPECIAL[y17]KEY1 FOR ATTACK[/b]", deNone, 5000, True
'DMDFLush
'DMD "dmd baseBlank.png", "SMASH THE FIREBUTTON", " TO ATTACK THE HYDE ", 500
'FireFlash.State = 2
'Playsound "gold fx push"
vpmtimer.addtimer 5000, "TimerbuttonOff'"
End Sub



sub TimerbuttonOff
   If (Bulb30.State = 0) Then Exit Sub
      'FireFlash.State = 0
      Timerbutton.enabled = 0
      Bulb30.State = 0
      BHit = 0
End Sub


sub Bumper1_hit()
   ' startB2S(1)
Playsound "fx_bumper"
If Tilted Then Exit Sub
addscore 1000
If Bulb30.State = 2 Then Exit Sub
BHit = BHit + 1
'DMDFLush
'DMD "dmd baseBlank.png", "_", " " & BHit & " HIT FOR ATTACK", 500
FlashForms flasher2, 1000, 50, 0
FlashForms bulb17, 1000, 50, 0
FlashForms BumperLight, 100, 50, 0
   If (BHit = 15) Then 
      Bulb30.State = 2
      Timerbutton.enabled = 1
      BHit = 0
   End If

end sub

sub bumper2_hit()
    'startB2S(4)
Playsound "fx_bumper"
If Tilted Then Exit Sub
addscore 1000
If Bulb30.State = 2 Then Exit Sub
BHit = BHit + 1
'DMDFLush
'DMD "dmd baseBlank.png", "_", " " & BHit & " HIT FOR ATTACK", 500
FlashForms flasher6, 1000, 50, 0
FlashForms bulb25, 100, 50, 0
FlashForms BumperLight, 100, 50, 0
   If (BHit = 15) Then 
      Bulb30.State = 2
      Timerbutton.enabled = 1
      BHit = 0
   End If
end sub

sub bumper3_hit()
   ' startB2S(3)
Playsound "fx_bumper"
addscore 1000
If Bulb30.State = 2 Then Exit Sub
BHit = BHit + 1
'DMDFLush
'DMD "dmd baseBlank.png", "_", " " & BHit & " HIT FOR ATTACK", 500
FlashForms flasher2, 1000, 50, 0
FlashForms BumperLight, 100, 50, 0
   If (BHit = 15) Then 
      Bulb30.State = 2
      Timerbutton.enabled = 1
      BHit = 0
   End If
end sub




'***************************************************************************************
'--------------------Ramp & Golgoth Multiball------------------------------------------


'---Ramp entry (only for effects):
sub Rampentry_hit()

FlashForms flasher3, 1000, 50, 0
playsound "soundFX11", 0.8
end sub

'---initiate Ramp:
sub LeftRampTarget_hit()
	'startB2S(6)
If Tilted Then Exit Sub
wolfShake
playsound "soundFX21b"
playsound "WolfGrawl2"
if A1.state = 2 and A2.state = 1 and ballsonplayfield = 1  then
A1.state = 1
A3.state = 2
fltimer1.enabled = 1
vpmtimer.addtimer 2000, "fltimer2.enabled = 1'"'fltimerB.set true, 2000
'scoreupdate = false
'flushdmdtimer.set true , 2000
'DispDmd1.QueueText "[f6][xc][y5]- SHOOT RAMP -[y18]- 10.000 -", deFlip, 2000, True
'DispDmd2.QueueText "[f6][xc][y5]- SHOOT RAMP -[y18]- 10.000 -", deFlip, 2000, True
 DMD "dmd baseBlank.png", "_", " SHOOT RAMP ", 1500
FlashForms flasher3, 1000, 50, 0
end if
if A1.state = 1 and A2.state = 1 and ballsonplayfield = 1  then
addscore 20000
end if
if A1.state = 2 and A2.state = 2 and ballsonplayfield = 1 then
A1.state = 1
addscore 5000
end if
LastSwitchHit = "LeftRampTarget"
end sub


sub RightRampTarget_hit()
	'startB2S(1)
If Tilted Then Exit Sub
wolfShake
playsound "WolfGrawl"
if A1.state = 1 and A2.state = 2 and ballsonplayfield = 1  then
A2.state = 1
A3.state = 2
fltimer1.enabled = 1
vpmtimer.addtimer 2000, "fltimer2.enabled = 1'"'fltimerB.set true, 2000
'scoreupdate = false
'flushdmdtimer.set true , 2000
'DispDmd1.QueueText "[f6][xc][y5]SHOOT RAMP[y18]10.000", deFlip, 2000, True
'DispDmd2.QueueText "[f6][xc][y5]SHOOT RAMP[y18]10.000", deFlip, 2000, True
 DMD "dmd baseBlank.png", "_", " SHOOT RAMP ", 1500
FlashForms flasher3, 1000, 50, 0
end if
if A1.state = 1 and A2.state = 1 and ballsonplayfield = 1  then
addscore 20000
end if
if A1.state = 2 and A2.state = 2 and ballsonplayfield = 1 then
A1.state = 1
addscore 5000
end if
LastSwitchHit = "RightRampTarget"
end sub

sub RightRampTarget_Unhit
playsound "soundFX02b"
End Sub

'---Ramp trigger to Golgothkicker1 & 2 (initiate only ONE time):

sub ramptrigger_hit()

if ballsonplayfield = 4 then
addscore 250000
fltimerA.enabled = 1
vpmtimer.addtimer 2000, "fltimerB.enabled = 1'"'fltimerB.set true, 2000
playsound "JackpotFX2"
' GOLDORAK JACKPOT:
'scoreupdate = false
'flushdmdtimer.set true , 2100
'DMDframe.set true, 2500
'dmdsec 32, 44, "[f8][edge4]", 150   '------- ramp
'dmdsec 32, 44, "[f8][edge4]", 150
DMDFLush
DMD "dmd baseBlank.png", "", "", 2000
end if
if ballsonplayfield = 3 then
addscore 250000
fltimerA.enabled = 1
vpmtimer.addtimer 2000, "fltimerB.enabled = 1'"'fltimerB.set true, 2000
playsound "JackpotFX2"
' GOLDORAK JACKPOT:
'scoreupdate = false
'flushdmdtimer.set true , 2100
'DMDframe.set true, 2500
'dmdsec 32, 44, "[f8][edge4]", 150   '------- ramp
'dmdsec 32, 44, "[f8][edge4]", 150
DMDFLush
DMD "dmd baseBlank.png", "", "", 2000
end if
if ballsonplayfield = 2 then
addscore 250000
fltimerA.enabled = 1
vpmtimer.addtimer 2000, "fltimerB.enabled = 1'"'fltimerB.set true, 2000
playsound "JackpotFX2"
' GOLDORAK JACKPOT:
'scoreupdate = false
'flushdmdtimer.set true , 2100
'DMDframe.set true, 2500
'dmdsec 32, 44, "[f8][edge4]", 150   '------- ramp
'dmdsec 32, 44, "[f8][edge4]", 150
DMDFLush
DMD "dmd baseBlank.png", "", "", 2000
end if

if Ab.state = 2 and ballsonplayfield = 1 then
addscore 150000
FlashForMs flasher2, 400, 10, 0  'off
FlashForMs flasher6, 400, 10, 0  'off
FlashForMs bulb9, 2000, 10, 0  'off
FlashForMs bulb10, 2000, 10, 0  'off
playsound "soundFX08", 0.6
'scoreupdate = false
'flushdmdtimer.set true , 2600
'DMDframe.set true, 2500
'DispDmd1.QueueText "[f6][edge4][b][y8]LOCK IS LIT[/b]", deNone, 2000, True
'DispDmd2.QueueText "[f6][edge4][b][y8]LOCK IS LIT[/b]", deNone, 2000, True
 DMDFLush
 'DMD "girugiru.gif", "", "", 1500
 DMD "dmd baseBlank.png", "_", " LOCK IS LIT ", 1000
end if

if A3.state = 1 and A4.state = 1 and A5.state = 2 and ballsonplayfield = 1 then
A5.state = 1
A6.state = 2
'FlashForMs A6, 1, 500, 1  'blink
Ab.state = 2
addscore 150000
FlashForMs bulb9, 2000, 10, 0  'off
FlashForMs bulb10, 2000, 10, 0  'off
'scoreupdate = false
'flushdmdtimer.set true , 2600
'DMDframe.set true, 2500
 DMDFLush
 DMD "dmd baseBlank.png", "", "", 1500
'DispDmd1.QueueText "[f6][edge4][b][y8]LOCK IS LIT[/b]", deNone, 2000, True
'DispDmd2.QueueText "[f6][edge4][b][y8]LOCK IS LIT[/b]", deNone, 2000, True
 DMD "dmd baseBlank.png", "_", "LOCK IS LIT", 1000

fltimerA.enabled = 1
vpmtimer.addtimer 2000, "fltimerB.enabled = 1'"'fltimerB.set true, 2000
'effectmusic 1, fadeoutandpause, 0, 1
'musicin.set true, 3000
'playsound "goldrake prof"
end if


if A3.state = 1 and A4.state = 2 and A5.state = 0 and ballsonplayfield = 1 then
A4.state = 1
A5.state = 2
addscore 100000
FlashForMs bulb9, 2000, 10, 0  'off
FlashForMs bulb10, 2000, 10, 0  'off
'scoreupdate = false
'flushdmdtimer.set true , 2600
'DMDframe.set true, 3000
 'DMDFLush
 'DMD "dmd baseBlank.png", "", "", 1000
'dmdsec 32, 39, "[f16][edge4]", 200   '------- goldrake drill
'dmdsec 32, 39, "[f16][edge4]", 200
fltimer1.enabled = 1
vpmtimer.addtimer 2000, "fltimer2.enabled = 1'"'fltimerB.set true, 2000
'effectmusic 1, fadeoutandpause, 0, 1
'musicin.set true, 3000
playsound "goldrake maria"
'StopDisco
end if


if A3.state = 2 and A4.state = 0 and A5.state = 0 and ballsonplayfield = 1 then
A3.state = 1
A4.state = 2
addscore 50000
FlashForMs bulb9, 2000, 10, 0  'off
FlashForMs bulb10, 2000, 10, 0  'off
'DMDSmallSequence 119, 130
'scoreupdate = false
'flushdmdtimer.set true , 2600
'DMDframe.set true, 3000
 'DMDFLush
 'DMD "dmd baseBlank.png", "", "", 1000
'dmdsec 32, 38, "[f15][edge4]", 200   '------- goldrake alcoor
'dmdsec 32, 38, "[f15][edge4]", 200
fltimer1.enabled = 1
vpmtimer.addtimer 2000, "fltimer2.enabled = 1'"'fltimerB.set true, 2000
'effectmusic 1, fadeoutandpause, 0, 1
'musicin.set true, 3000
playsound "goldrake alcoor"
'StopDisco
end if


if A3.state = 0 and ballsonplayfield = 1 then
addscore 25000
FlashForMs flasher2, 400, 10, 0  'off
FlashForMs flasher6, 400, 10, 0  'off
FlashForMs bulb9, 2000, 10, 0  'off
FlashForMs bulb10, 2000, 10, 0  'off
playsound "soundFX08", 0.6
'scoreupdate = false
'flushdmdtimer.set true , 2600
'DMDframe.set true, 2500
'DispDmd1.QueueText "[f6][edge4][b][y4]GOLDRAKE ATLAS[y17]UFO ROBOT[/b]", deNone, 2000, True
'DispDmd2.QueueText "[f6][edge4][b][y4]GOLDRAKE ATLAS[y17]UFO ROBOT[/b]", deNone, 2000, True
 DMDFLush
 DMD "Rave-NMB.wmv", "", "", 2000
end if

    If LastSwitchHit = "ramptrigger" Then 'give combo
        ComboValue = 5000 + Round(Score(CurrentPlayer) / 10, 0)
        DMD "dmd baseBlank.png", "COMBO", ComboValue, 100
        AddScore ComboValue
        ComboM = ComboM + 1
        CheckComboMultiball
    End If

LastSwitchHit = "ramptrigger"
end sub






'---Golgothkicker1 & 2 (initiated by ramp):
sub Golgothkicker1_hit()
	'startB2S(5)
Golgothkicker1Fake.enabled = 0
playsound "rballfall2"
ActarusMoveB.enabled = 1
'playsound "soundfxchair"
if ballsonplayfield = 4 then
'addscore 250000
fltimerA.enabled = 1
vpmtimer.addtimer 1000, "fltimerB.enabled = 1'"'fltimerB.set true, 2000
'playsound "JackpotFX1"
' Golodorak JACKPOT 2:
'scoreupdate = false
'flushdmdtimer.set true , 4100
'DMDframe.set true, 4000
'DispDmd1.QueueText "[il1][sf15][ef50]", deNone, 4000, FALSE
'DispDmd2.QueueText "[il1][sf15][ef50]", deNone, 4000, FALSE
 'DMDFLush
 'DMD "DMDJackpotG.gif", "", "", 3000
Golgothkicker2timer.Interval = 1000
Golgothkicker2timer.enabled = True
'vpmtimer.addtimer 800, "Golgothkicker2timer '"     '     Golgothkicker2timer.Interval =  1000
end if

if ballsonplayfield = 3 then
'addscore 250000
fltimerA.enabled = 1
vpmtimer.addtimer 1000, "fltimerB.enabled = 1'"'fltimerB.set true, 2000
'playsound "JackpotFX1"
' Golodorak JACKPOT 2:
'scoreupdate = false
'flushdmdtimer.set true , 4100
'DMDframe.set true, 4000
'DispDmd1.QueueText "[il1][sf15][ef50]", deNone, 4000, FALSE
'DispDmd2.QueueText "[il1][sf15][ef50]", deNone, 4000, FALSE
 'DMDFLush
 'DMD "DMDJackpotG.gif", "", "", 3000
Golgothkicker2timer.Interval = 1000
Golgothkicker2timer.enabled = True
'vpmtimer.addtimer 800, "Golgothkicker2timer '"     '     Golgothkicker2timer.Interval =  1000
end if

if ballsonplayfield = 2 then
'addscore 250000
fltimerA.enabled = 1
vpmtimer.addtimer 1000, "fltimerB.enabled = 1'"'fltimerB.set true, 2000
'playsound "JackpotFX1"
' Golodorak JACKPOT 2:
'scoreupdate = false
'flushdmdtimer.set true , 4100
'DMDframe.set true, 4000
'DispDmd1.QueueText "[il1][sf15][ef50]", deNone, 4000, FALSE
'DispDmd2.QueueText "[il1][sf15][ef50]", deNone, 4000, FALSE
' DMDFLush
 'DMD "DMDJackpotG.gif", "", "", 3000
'Golgothkicker1.destroyball
'Golgothkicker1.kick 350, 10
Golgothkicker2timer.Interval = 1000
Golgothkicker2timer.enabled = True
'vpmtimer.addtimer 800, "Golgothkicker2timer '"     '     Golgothkicker2timer.Interval =  1000
end if


if A6.state = 1 and A7.state = 1 and A8.state = 2 and ballsonplayfield = 1 then
'Multiball GO
'StopAllMusic
'Song = "bgout_GoldorakThemeInstrumental" & ".mp3"
'PlayMusic Song 
'****************************************************************************************
'			  Where's The HYDE_MB Lock 2 PUP 
'****************************************************************************************
PuPlayer.playlistplayex pCallouts,"audiocallouts","FoundTheHyde.mp3",calloutvol,1
PuPlayer.playlistplayex pBackglass,"FoundTheHyde","FoundTheHyde.mp4",95,1
PuPlayer.playlistplayex pMusic,"audiomodes","FoundTheHyde.mp3",0,1
PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
WheresTheHydeMBFlasher.opacity = 90
AudioAttractTimer001.Enabled = False
quotetrigger.Enabled = false
quotetrigger001.Enabled = False
VideoQuoteResteTimer.Enabled = False
VideoQuoteResteTimer001.Enabled = False
VideoQuoteResteTimer002.Enabled = False
VideoQuoteResteTimer003.Enabled = False
VideoQuoteResteTimer004.Enabled = False
VideoQuoteResteTimer005.Enabled = False
quotetrigger002.Enabled = False
StopLOGOAni
'GiOff
'chilloutthemusic
DMDFLush
DMD "dmd baseBlank.png", "", "", 500
DMD "FoundTheHyde.png", " ", "", 7500
    '*********************************
    'HYDE MULTIBALL 1 START ANIMATION
    '*********************************
    'Start_Fog
	StartWheresHMB.Enabled = True
    HydeShakeTimer.Enabled = 1
    HydeLocationTimer.Enabled = 1
    HydePos = HydePos + HydeDir
    'Hyder is moving up
    If HydePos >= 0 Then
        DOF 127, DOFOff
        'Me.Enabled = 0
        HydePos = 1
        HydeShakeDir = 1
        HydeShakeTimer.Enabled = 1
        HydeLocationTimer.Enabled = 1
        bHydeUp = True
    End If
    'Hyde is moving down
    If HydePos <= -140 Then
        DOF 127, DOFOff
        'Me.Enabled = 0
        HydePos = -140
        HydeTrigger1.Enabled = 1
        HydeTrigger2.Enabled = 1
        HydeTrigger2.Enabled = 1
       'IncreaseGhostLevel
    End If
    Hyde.Transz = HydePos
    'Camera.RotX = - HydePos / 4		
    'HydeMoveUp
'*********************************
bMultiBallMode = True
StopSound Song:Song = ""
'PlaySong "mu_Multiball"
'DMDFLush
'DMD "dmd baseBlank.png", "", "", 4500
'Hyde2Timer.Enabled = 1
'IntroSmallDMD()
A1.state = 2
A2.state = 2
A3.state = 0
A4.state = 0
A5.state = 0
A6.state = 0
'FlashForMs A6, 100, 10, 0  'off
A7.state = 0
'FlashForMs A7, 100, 10, 0  'off
A8.state = 0
'FlashForMs A8, 100, 10, 0  'off
Ak.state = 0
'FlashForMs AKL, 100, 10, 0  'off
Ab.state = 0
addscore 500000
ballsonplayfield = 4
'playsound "MainTheme"
Golgothkicker2timer.Interval = 19000
Golgothkicker2timer.enabled = True
'vpmtimer.addtimer 4000, "Golgothkicker2timer '"     '     Golgothkicker2timer.Interval =  1000
Golgothkicker3timer.Interval = 20000
Golgothkicker3timer.enabled = True
'vpmtimer.addtimer 5000, "Golgothkicker3timer '"     '     Golgothkicker2timer.Interval =  1000
Golgothkicker4timer.Interval = 21000
Golgothkicker4timer.enabled = True
'vpmtimer.addtimer 6000, "Golgothkicker4timer '"     '     Golgothkicker2timer.Interval =  1000
Golgothkicker5timer.Interval = 22000
Golgothkicker5timer.enabled = True
'vpmtimer.addtimer 4000, "Golgothkicker5timer '"     '     Golgothkicker2timer.Interval =  1000
    

'scoreupdate = false
'flushdmdtimer.set true , 2600
'DMDframe.set true, 2500
'DispDmd1.QueueText "[f6][xc][y5]MULTIBALL[y18]500.000", deFlip, 2000, True
'DispDmd2.QueueText "[f6][xc][y5]MULTIBALL[y18]500.000", deFlip, 2000, True
'DMDBlink "black.jpg", " ", "MULTIBALL", 200, 20
fltimerA.enabled = 1
vpmtimer.addtimer 6100, "fltimerB.enabled = 1'"'fltimerB.set true, 2000
'effectmusic 1, fadeoutandpause, 0, 1
'musicin.set true, 4000
playsound "VilainVegaMaitre"
'duringMULTIBALL.play seqalloff
J1.state = 2
J2.state = 2
J3.state = 2
J4.state = 2
'hologram.frame 1, 211, 1
end if


if A6.state = 1 and A7.state = 2 and A8.state = 0 and ballsonplayfield = 1 then
A7.state = 1
'FlashForMs A7, 200, 10, 3   'on
A8.state = 2
'FlashForMs A8, 1, 500, 1  'blink
'Multiball READY
Ab.state = 2
addscore 150000
Golgothkicker2timer.Interval = 1000
Golgothkicker2timer.enabled = True
'****************************************************************************************
'			  Where's The HYDE_MB Lock 2 PUP 
'****************************************************************************************
 AudioAttractTimer001.Enabled = False
 quotetrigger.Enabled = false
 quotetrigger001.Enabled = False
 VideoQuoteResteTimer.Enabled = False
 VideoQuoteResteTimer001.Enabled = False
 VideoQuoteResteTimer002.Enabled = False
 VideoQuoteResteTimer003.Enabled = False
 VideoQuoteResteTimer004.Enabled = True
 VideoQuoteResteTimer005.Enabled = False
 quotetrigger002.Enabled = False
 MBBlockerTimer002.Enabled = True
 MBBlockerTimer003.Enabled = True
 'MBBlockerTimer001.Enabled = True
 'MBBlockerTimer.Enabled = True
 Trigger2.Enabled = False
 MBBlockerTarget.Enabled = True
 MBBlocker001Target.Enabled = True
 MBBlocker002Target.Enabled = True
 MBBlocker.Isdropped = 0
 MBBlocker001.Isdropped = 0
 MBBlocker002.Isdropped = 0
 'MBBlocker5.Isdropped = 0
 MBBlockerlight001.state = 2
 MBBlockerlight002.state = 2
 MBBlockerlight003.state = 2
 MBBlockerlight004.state = 2
 MBBlockerlight005.state = 2
 DiscoBlocker.Isdropped = 0
 PuPlayer.playlistplayex pCallouts,"audiocallouts","OnTheHunt.mp3",110,1
 PuPlayer.playlistplayex pBackglass,"WheresTheHydeLock","WheresTheHydeLock2.mp4",videovol,1
 PuPlayer.playlistplayex pMusic,"audiomodes","WheresTheHydeLock2.mp3",0,1
 PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
 'chilloutthemusic
DMDFLush
DMD "dmd baseBlank.png", "", "", 300
DMD "OnTheHunt.png", " ", "", 4500
fltimerA.enabled = 1
vpmtimer.addtimer 1000, "fltimerB.enabled = 1'"'fltimerB.set true, 2000
'effectmusic 1, fadeoutandpause, 0, 1
'musicin.set true, 2000
playsound "ActarusMetamorphose"
end if


if A6.state = 2 and A7.state = 0 and A8.state = 0 and ballsonplayfield = 1 then
A6.state = 1
'FlashForMs A6, 200, 10, 3   'on
A7.state = 2
'FlashForMs A7, 1, 500, 1  'blink
Ab.state = 2
addscore 150000
'Golgothkicker1.destroyball
'Golgothkicker1.kick 350, 10
Golgothkicker2timer.Interval = 1000
Golgothkicker2timer.enabled = True
'AudioAttractTimer001.Enabled = False
'quotetrigger.Enabled = false 
'quotetrigger001.Enabled = False
'VideoQuoteResteTimer.Enabled = False
'VideoQuoteResteTimer001.Enabled = False
'VideoQuoteResteTimer002.Enabled = False
'VideoQuoteResteTimer003.Enabled = False
'VideoQuoteResteTimer004.Enabled = True
'VideoQuoteResteTimer005.Enabled = False
'quotetrigger002.Enabled = False
'****************************************************************************************
'			  Where's The HYDE_MB Lock 1 PUP 
'****************************************************************************************
 AudioAttractTimer001.Enabled = False
 quotetrigger.Enabled = false
 quotetrigger001.Enabled = False
 VideoQuoteResteTimer.Enabled = False
 VideoQuoteResteTimer001.Enabled = False
 VideoQuoteResteTimer002.Enabled = False
 VideoQuoteResteTimer003.Enabled = False
 VideoQuoteResteTimer004.Enabled = True
 VideoQuoteResteTimer005.Enabled = False
 quotetrigger002.Enabled = False
 'MBBlockerTimer.Enabled = True
 'MBBlockerTimer001.Enabled = True
 Trigger2.Enabled = False
 DiscoBlocker.Isdropped = 0
 MBBlockerTimer002.Enabled = True
 MBBlockerTimer003.Enabled = True
 MBBlockerTarget.Enabled = True
 MBBlocker001Target.Enabled = True
 MBBlocker002Target.Enabled = True
 MBBlocker.Isdropped = 0
 MBBlocker001.Isdropped = 0
 MBBlocker002.Isdropped = 0
 'MBBlocker5.Isdropped = 0
 MBBlockerlight001.state = 2
 MBBlockerlight002.state = 2
 MBBlockerlight003.state = 2
 MBBlockerlight004.state = 2
 MBBlockerlight005.state = 2
 'chilloutthemusic
 PuPlayer.playlistplayex pBackglass,"WheresTheHydeLock","WheresTheHydeLock1.mp4",videovol,1
 PuPlayer.playlistplayex pCallouts,"audiocallouts","WheresTheHyde.mp3",calloutvol,1
 PuPlayer.playlistplayex pMusic,"audiomodes","WheresTheHydeLock1.mp3",0,1
 PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
 'chilloutthemusic
 DMDFLush
 DMD "dmd baseBlank.png", "", "", 300
 DMD "WheresTheHyde.gif", " ", "", 4500
 AudioAttractTimer001.Enabled = False
 quotetrigger.Enabled = false
 quotetrigger001.Enabled = False
 VideoQuoteResteTimer.Enabled = False
 VideoQuoteResteTimer001.Enabled = False
 VideoQuoteResteTimer002.Enabled = False
 VideoQuoteResteTimer003.Enabled = False
 VideoQuoteResteTimer004.Enabled = True
 VideoQuoteResteTimer005.Enabled = False
 quotetrigger002.Enabled = False
fltimerA.enabled = 1
vpmtimer.addtimer 1500, "fltimerB.enabled = 1'"'fltimerB.set true, 2000
end if


if Ab.state = 0 and ballsonplayfield = 1 then
addscore 10000
Ak.state = 1
'FlashForMs AKL, 200, 10, 3   'on
vpmtimer.addtimer 1000, "Golgothkicker1timer '"     'Golgothkicker1timer.Interval =  1000
playsound "soundFXkicker1"
end if

LastSwitchHit = "Golgothkicker1"
end sub



Sub Golgothkicker1_UnHit
    vpmtimer.addtimer 500, "Golgothkicker1Fake.enabled = 1 '" 
    vpmtimer.addtimer 500, "Golgothkicker1.Kick 370, 20 '"
End Sub


sub Golgothkicker1timer()

Golgothkicker1.kick 230, 45, 1.5
FlashForms AKL, 1000, 50, 0
FlashForms flasher8, 1000, 50, 0
FlashForms flasher8b, 1000, 50, 0
FlashForms flasher7, 1000, 50, 0
FlashForms flasher7b, 1000, 50, 0
playsound "WheresHydeKickout"
playsound "evilFemalLaugh"
end sub


sub Golgothkicker2timer_Timer()
Golgothkicker2timer.enabled = False
Golgothkicker1.Kick 370, 20
FlashForms flasher8, 1000, 50, 0
FlashForms flasher8b, 1000, 50, 0
If ballsonplayfield = 1 Then
 DMDBlink "dmd baseBlank.png", " ", "GO FOR MULTIBALL", 100, 20
End If
end sub


sub Golgothkicker3timer_Timer()
Golgothkicker3timer.enabled = False
playsound "WheresHydeKickout"
playsound "evilFemalLaugh"
Golgothkicker2.createball
Golgothkicker2.Kick 0, 50, 1.5
FlashForms flasher8, 1000, 50, 0
FlashForms flasher8b, 1000, 50, 0
end sub

sub Golgothkicker4timer_Timer()
Golgothkicker4timer.enabled = False
playsound "WheresHydeKickout"
playsound "evilFemalLaugh"
Golgothkicker2.createball
Golgothkicker2.Kick 0, 50, 1.5
FlashForms flasher8, 1000, 50, 0
FlashForms flasher8b, 1000, 50, 0
end sub

sub Golgothkicker5timer_Timer()
Golgothkicker5timer.enabled = False
playsound "WheresHydeKickout"
playsound "evilFemalLaugh"
Golgothkicker2.createball
Golgothkicker2.Kick 0, 50, 1.5
FlashForms flasher8, 1000, 50, 0
FlashForms flasher8b, 1000, 50, 0
EnableBallSaver 40
GIOFFWIZARDTrigger.Enabled = True
GIOFFWIZARDTrigger001.Enabled = True
GIOFFWIZARDTrigger002.Enabled = True
GIOFFWIZARDTrigger003.Enabled = True
GIOFFWIZARDTrigger004.Enabled = True
magickicker.Enabled = False
quotetrigger.Enabled = false
quotetrigger001.Enabled = False
VideoQuoteResteTimer.Enabled = False
VideoQuoteResteTimer001.Enabled = False
VideoQuoteResteTimer002.Enabled = False
VideoQuoteResteTimer003.Enabled = False
VideoQuoteResteTimer004.Enabled = False
VideoQuoteResteTimer005.Enabled = False
quotetrigger002.Enabled = False
AudioAttractTimer001.Enabled = False
WTHSmallDMDOn.Enabled = True
DMDSmall3.visible = 0
FoundHydeSDMD.visible = 1
FoundHydeSDMD001.visible = 1
end sub


Sub Golgothkicker2_Hit
Golgothkicker2.Kick 0, 50, 1.5
playsound "WheresHydeKickout"
playsound "evilFemalLaugh"
end sub

Sub Golgothkicker2_UnHit
playsound "fx_vukout_LAH"
AlabardaAnim
end sub


'***************************************************************************************
'-----EXTRA BALL SCRIPT (after multiball drain EXTRA BALL blinks for the target)--------



Sub Target3_hit()
	'startB2S(1)
PlaySound "fx_rubber"
addscore 5000
if ExtraBall.state = 2 then
ExtraBall.state = 0
ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1 ' awards extra ball to current player
FlashForMs ShootAgainLightF, 200, 10, 1   'on
'ShootAgainLight.state = 1
addscore 25000
DMD "dmd baseBlank.png", " ", "EXTRA BALL", 100
FlashEffect 2
end if
LastSwitchHit = "Target3"
End Sub





'***************************************************************************************
'-----ORBIT SCRIPT-----------------------------------------------------------------------
sub Orbit_hit()
' MARIA FREED JACKPOT during Multiball (if ballsonpf = 2 3 4):
if ballsonplayfield = 4 then
addscore 1000000
playsound "Ghostly Whisper"
playsound "Scream 7"
'DMDFLush
'DMD "dmd baseBlank.png", "", "", 2000
end if
if ballsonplayfield = 3 then
addscore 500000
playsound "The Scarecrow"
playsound "Scream 8"
'DMDFLush
'DMD "dmd baseBlank.png", "", "", 2000
end if
if ballsonplayfield = 2 then
addscore 250000
playsound "Ghostly Whisper"
playsound "burp"
'DMDFLush
'DMD "dmd baseBlank.png", "", "", 2000
end if
if ballsonplayfield = 1 then
addscore 15000
RavenSmall1LShake
RavenSmall2RShake
playsound "The Scarecrow"
fltimer1.enabled = true
vpmtimer.addtimer 1000, "fltimer2.enabled = 1 '"     'fltimer2.enabled = true
end if
end sub



'***************************************************************************************
'---ACTARUS 2 BALL SCRIPT-----------------------------------------------------------------------


'---Lockhole script-------

sub lockhole_hit()
playsound "rballfall"
if ballsonplayfield = 4 then
lockhole.Kick 80,5
end if
if ballsonplayfield = 3 then
lockhole.Kick 80,5 
end if
if ballsonplayfield = 2 then
lockhole.Kick 80,5 
end if



if B4.state = 1 and B5.state = 2 and ballsonplayfield = 1 then
B1.state = 2
B2.state = 0
B3.state = 0
B4.state = 0
B5.state = 0
'playsound "goldrake music"
'duringMULTIBALL.play seqalloff
J1.state = 2
J2.state = 2
J3.state = 2
J4.state = 2


lockhole.Kick 80,5
FlashForMs flasher9, 100, 10, 0  'off
FlashForms flasher9b, 100, 10, 0
vpmtimer.addtimer 20000, "ball1out '" 
vpmtimer.addtimer 21000, "ball2out '"
addscore 250000
DMDFLush
DMD "dmd baseBlank.png", "", "", 300
DMD "HydeMB.png", " ", "", 2800
quotetrigger.Enabled = false
quotetrigger001.Enabled = False
VideoQuoteResteTimer.Enabled = False
VideoQuoteResteTimer001.Enabled = False
VideoQuoteResteTimer002.Enabled = False
VideoQuoteResteTimer003.Enabled = False
VideoQuoteResteTimer004.Enabled = False
VideoQuoteResteTimer005.Enabled = False
quotetrigger002.Enabled = False
AudioAttractTimer001.Enabled = False
'****************************************************************************************
'			  HYDE MULTIBALL PUP_VIDEO 2 LOCK 2 AND MB DMDSMALL FLASHERS
'****************************************************************************************
PuPlayer.playlistplayex pBackglass,"videoHydelock","HydeLockTwo.mp4",videovol,1
PuPlayer.playlistplayex pMusic,"audiomodes","HydeLockTwo",0,1
PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
BRAnimation.Enabled = True
bMultiBallMode = True
DMDFLush
DMD "DMD baseBlank.png", "", "", 4500
fltimerA.enabled = 1
vpmtimer.addtimer 20000, "fltimerB.enabled = 1 '"
end if

if B4.state = 2 and B5.state = 0 and ballsonplayfield = 1 then
B4.state = 1
B5.state = 2
lockhole.Kick 80,5
FlashForMs flasher9, 100, 10, 0  'off
FlashForMs flasher9b, 100, 10, 0  'off
ActaruskicktorampTimer.interval = 2000
ActaruskicktorampTimer.enabled = True
'vpmtimer.addtimer 2000, "Actaruskicktoramp '"'Actaruskicktoramp.set true, 2000
addscore 150000
FlashForMs flasher2, 100, 10, 0  'off
FlashForMs flasher6, 100, 10, 0  'off
DMDFLush
DMD "dmd baseBlank.png", "", "", 300
DMD "HydeLock1.png", " ", "", 2800
quotetrigger.Enabled = false
quotetrigger001.Enabled = False
VideoQuoteResteTimer.Enabled = False
VideoQuoteResteTimer001.Enabled = False
VideoQuoteResteTimer002.Enabled = False
VideoQuoteResteTimer003.Enabled = False
VideoQuoteResteTimer004.Enabled = False
VideoQuoteResteTimer005.Enabled = False
quotetrigger002.Enabled = False
AudioAttractTimer001.Enabled = True
'****************************************************************************************
'			  HYDE MULTIBALL PUP_VIDEO 1 LOCK 1 AND MB DMDSMALL FLASHERS
'****************************************************************************************
 PuPlayer.playlistplayex pCallouts,"audiocallouts","B1locked.mp3",calloutvol,1
 PuPlayer.playlistplayex pBackglass,"videoHydelock","HydeLockOne.mp4",videovol,1
 PuPlayer.playlistplayex pMusic,"audiomodes","HydeLockOne.mp3",0,1
 PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
 fltimerA.enabled = 1
vpmtimer.addtimer 2000, "fltimerB.enabled = 1 '"
end if

end sub


Sub LockholeFake_Hit
if ballsonplayfield = 4 then
LockholeFake.Kick 0,5
vpmtimer.addtimer 700, "Actaruskicker1.kick 0, 25, 1.5 '" 
end if
if ballsonplayfield = 3 then
LockholeFake.Kick 80,5
vpmtimer.addtimer 700, "Actaruskicker1.kick 0, 25, 1.5 '"  
end if
if ballsonplayfield = 2 then
LockholeFake.Kick 0,5
vpmtimer.addtimer 700, "Actaruskicker1.kick 0, 25, 1.5 '" 
end if
End Sub

Sub closediverter_hit 
  '  GolgothRotateTimer.enabled = 0
End Sub



'---ACTARUS kicker (initiates Lock in LOCKHOLE)-----



Sub Actaruskicker1_hit()
If B2SOn Then Controller.B2SSetData 5,0:Controller.B2SSetData 5,1
'DMD "DMD meta.gif", "", "", 2800
ActarusKicker1Fake.enabled = 0
playsound "fx_kicker-enter"
'ActarusKicker1.destroyball

if ballsonplayfield = 4 then
FlashForMs flasher9, 1000, 10, 0  'off
FlashForMs flasher9b, 1000, 10, 0  'off
ActarusKicker1.kick 285, 10
ActaruskicktorampTimer.Interval = 2000
ActaruskicktorampTimer.Enabled = True
'vpmtimer.addtimer 1000, "Actaruskicktoramp '"
vpmtimer.addtimer 2100, "ActarusKicker1Fake.enabled = 1 '"
addscore 50000
end if

if ballsonplayfield = 3 then
FlashForMs flasher9, 1000, 10, 0  'off
FlashForMs flasher9b, 1000, 10, 0  'off
ActarusKicker1.kick 285, 10
ActaruskicktorampTimer.Interval = 2000
ActaruskicktorampTimer.Enabled = True
'vpmtimer.addtimer 1000, "Actaruskicktoramp '"
vpmtimer.addtimer 2100, "ActarusKicker1Fake.enabled = 1 '"
addscore 50000
end if

if ballsonplayfield = 2 then
FlashForMs flasher9, 1000, 10, 0  'off
FlashForMs flasher9b, 1000, 10, 0  'off
ActarusKicker1.kick 285, 10
ActaruskicktorampTimer.Interval = 2000
ActaruskicktorampTimer.Enabled = True
'vpmtimer.addtimer 1000, "Actaruskicktoramp '"
vpmtimer.addtimer 2100, "ActarusKicker1Fake.enabled = 1 '"
addscore 50000
end if


if B1.state = 1 and B2.state = 1 and B3.state = 1 and ballsonplayfield = 1 then
playsound "soundfxchair"
addscore 25000
Bulb5.state = 1
Bulb5L.state = 1
ActaruskickstRaight.interval = 1000
ActaruskickstRaight.enabled =  True
playsound "soundFXkicker1"
end if

if B1.state = 1 and B2.state = 1 and B3.state = 2 and ballsonplayfield = 1 then
playsound "soundfxchair"
'****************************************************************************************
'			  HYDE MULTIBALL PUP_VIDEO 0 LIT 1 AND MB DMDSMALL FLASHERS
'****************************************************************************************
 quotetrigger.Enabled = false
 quotetrigger001.Enabled = False
 VideoQuoteResteTimer.Enabled = False
 VideoQuoteResteTimer001.Enabled = False
 VideoQuoteResteTimer002.Enabled = False
 VideoQuoteResteTimer003.Enabled = False
 VideoQuoteResteTimer004.Enabled = False
 VideoQuoteResteTimer005.Enabled = True
 AudioAttractTimer001.Enabled = False
 PuPlayer.playlistplayex pCallouts,"audiocallouts","HydeMBLit.mp3",115,1
 PuPlayer.playlistplayex pBackglass,"videoHydelit","This Kitty is LIT.mp4",videovol,1
 PuPlayer.playlistplayex pMusic,"audiomodes","This Kitty is Lit.mp3",0,1
 PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
 DMDFLush
 DMD "HydeLockLit.png", "", "", 5000
 'chilloutthemusic
B3.state = 1
B4.state = 2
Lockdiverter.RotateToEnd
addscore 2000
FlashForMs flasher9, 1000, 10, 0  'off
FlashForMs flasher9b, 1000, 10, 0  'off
ActarusKicker1.kick 285, 10
ActaruskicktorampTimer.Interval = 2000
ActaruskicktorampTimer.Enabled = True
'vpmtimer.addtimer 1000, "Actaruskicktoramp '"
vpmtimer.addtimer 2100, "ActarusKicker1Fake.enabled = 1 '"
DMDFLush
DMD "dmd baseBlank.png", "", "", 300
DMD "HydeLockLit.png", " ", "", 2800
'fltimerA.set true, 1
'fltimerB.set true, 2000
fltimerA.enabled = 1

end if

if B1.state = 1 and B2.state = 2 and B3.state = 0 and ballsonplayfield = 1 then
playsound "soundfxchair"
B2.state = 1
B3.state = 2
addscore 20000
FlashForMs flasher9, 1000, 10, 0  'off
FlashForMs flasher9b, 1000, 10, 0  'off
ActarusKicker1.kick 285, 10
ActaruskicktorampTimer.Interval = 2000
ActaruskicktorampTimer.Enabled = True
'vpmtimer.addtimer 1000, "Actaruskicktoramp '"
vpmtimer.addtimer 2100, "ActarusKicker1Fake.enabled = 1 '"
end if

if B1.state = 2 and B2.state = 0 and B3.state = 0 and ballsonplayfield = 1 then
playsound "soundfxchair"
B1.state = 1
B2.state = 2
addscore 15000
Bulb5.state = 1
Bulb5L.state = 1
ActaruskickstRaight.interval = 1000
ActaruskickstRaight.enabled =  True
'vpmtimer.addtimer 1000, "Actaruskickstraight '"
playsound "soundFXkicker1"
end if

LastSwitchHit = "ActarusKicker1"
end sub


Sub ActarusKicker1_UnHit
    Actaruskicker2.kick 0, 45, 1.5
End Sub


sub Timerbulb_Timer()
Timerbulb.enabled = 0
gi4.state = 1
gi3.state = 1
gi5.state = 1
gi6.state = 1
End Sub






'---Kickout Timers-------
sub ActaruskickstRaight_Timer()
'---straight if B1 or B3 or B5 blink
ActaruskickstRaight.enabled = False
playsound "fx_kicker"
ActarusMoveA.enabled = 1
Actaruskicker1.Kick 2, 10, 1.5
ThingLittleShake
PyramidExit
Bulb5.state = 0
Bulb5L.state = 0
playsound "soundfx17"
vpmtimer.addtimer 2100, "ActarusKicker1Fake.enabled = 1 '"
'RightMagnet.MagnetOn = True
'vpmTimer.AddTimer 1000, " RightMagnet.MagnetOn = False '"
end sub



sub ActaruskicktorampTimer_Timer()
ActaruskicktorampTimer.enabled = False
ActarusMoveC.enabled = 1
playsound "soundfxchair"
'---if B2 or B4 blink & during Multiball
Actaruskicker2.kick 0, 45, 1.5
FlashForMs flasher9, 100, 10, 0  'off
FlashForMs flasher9b, 100, 10, 0  'off
playsound "soundfx17"
end sub



'---2 Ball Multiball Timers-------
sub ball1out()
playsound "soundfx17"
'Actaruskicker2.createball
Actaruskicker2.kick 0, 45, 1.5
vpmtimer.addtimer 500, "Actaruskicker1.kick 0, 25, 1.5 '" 
FlashForMs flasher9, 100, 10, 0  'off
FlashForMs flasher9b, 100, 10, 0  'off
EnableBallSaver 20
end sub


sub ball2out
playsound "soundfx17"
Actaruskicker2.createball
Actaruskicker2.kick 0, 45, 1.5
'****************************************************************************************
'			  ENID VS THE HYDE MULTIBALL PUP_VIDEO AND MB DMDSMALL FLASHERS
'****************************************************************************************
PuPlayer.playlistplayex pBackglass,"videoHydemb","EnidvsHydeMB.mp4",videovol,1
PuPlayer.playlistplayex pMusic,"audiomodes","hmb.mp3",0,1
PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
DMDFLush
DMD "HydeMB.png", "", "", 5000
FlashForMs flasher9, 100, 10, 0  'off
FlashForMs flasher9b, 100, 10, 0  'off
BallsOnPlayfield = 2
SlimerMoveUp
StartEnidSpin
StartWizSpotLightSeq
DMDSmallHMB.visible = 1
DMDSmallHMB001.visible = 1
DMDSmall3.visible = 0
EVHSmallDMDOn.Enabled = True
HYDEmbVideo.Enabled = True
AudioAttractTimer001.Enabled = False
vpmtimer.addtimer 900, "Actaruskicker1.kick 0, 25, 1.5 '" 
EvHMBGIOFFTrigger.Enabled = True
EvHMBGIOFFTTimer.Enabled = True
totherampSkill003.Enabled = 0
multiballholeblockerTimer.Enabled = True
GIOFFWIZARDTrigger.Enabled = True
GIOFFWIZARDTrigger001.Enabled = True
GIOFFWIZARDTrigger002.Enabled = True
GIOFFWIZARDTrigger003.Enabled = True
GIOFFWIZARDTrigger004.Enabled = True
'GIOFFWIZARDTrigger.Enabled = True
GIOFFWIZARDTrigger002.Enabled = True
RightInLaneTrigger.Enabled = False
LeftInLaneTrigger.Enabled = False
BRAnimation.Enabled = False
FireBallDMDTrigger.Enabled = False
HMBFlasher22.opacity = 100
HMBFlasher02.opacity = 100
HMBFlasherApron.opacity = 100
APGlow.state = 2
SlimerUpTimer.Enabled = True
HSJPTimer1.Enabled = True
GraveStoneSuperTarget001.Isdropped = 0
GraveStoneSuperTarget002.Isdropped = 0
GraveStoneSuperTarget003.Isdropped = 0
SuperTargetsOffTimer.Enabled = True 
SuperTargetsOffTimer001.Enabled = True 
quotetrigger.Enabled = false
quotetrigger001.Enabled = False
VideoQuoteResteTimer.Enabled = False
VideoQuoteResteTimer001.Enabled = False
VideoQuoteResteTimer002.Enabled = False
VideoQuoteResteTimer003.Enabled = False
VideoQuoteResteTimer004.Enabled = False
VideoQuoteResteTimer005.Enabled = False
quotetrigger002.Enabled = False
trigger17.Enabled = False
MBBlockerlight001.state = 2
MBBlockerlight002.state = 2
MBBlockerlight003.state = 2
MBBlockerlight004.state = 2
MBBlockerlight005.state = 2
WizardSaverKicker.Enabled = True
Drain.Enabled = 0
MBBlocker003.Isdropped = 0
magickicker.Enabled = False
NightShadeGIOn
ResetHMBGITimer.Enabled = True
end sub


Dim ComboM
Sub CheckComboMultiball()
If bMultiBallMode then Exit Sub

 if ComboM = 3 Then
     DMD "dmd baseBlank.png", ComboM &" Combos-> 7 More For", "MULTIBALL", 2000
 End If

 If ComboM = 6 Then
     GraveStoneSuperTarget001.Isdropped = 0
     GraveStoneSuperTarget002.Isdropped = 0
     GraveStoneSuperTarget003.Isdropped = 0
     SuperTargetsOffTimer.Enabled = True
     SuperTargetsOffTimer001.Enabled = True  
     DiscoBlocker.Isdropped = 0
     quotetrigger.Enabled = false
     quotetrigger001.Enabled = False
     quotetrigger002.Enabled = False
     VideoQuoteResteTimer001.Enabled = False
     VideoQuoteResteTimer002.Enabled = False
     VideoQuoteResteTimer003.Enabled = False
     VideoQuoteResteTimer004.Enabled = False
     VideoQuoteResteTimer005.Enabled = False
     AudioAttractTimer001.Enabled = True
     Trigger2.Enabled = False
     MBBlocker.Isdropped = 0
     'MBBlocker5.Isdropped = 0
     MBBlocker001.Isdropped = 0
     MBBlocker002.Isdropped = 0
     MBBlockerTimer.Enabled = True
     MBBlockerTimer003.Enabled = True
     MBBlockerTarget.Enabled = True
     MBBlocker001Target.Enabled = True
     MBBlocker002Target.Enabled = True
     MBBlockerlight001.state = 2
     MBBlockerlight002.state = 2
     MBBlockerlight003.state = 2
     MBBlockerlight004.state = 2
     MBBlockerlight005.state = 2
     PuPlayer.playlistplayex pBackglass,"videoDiscoPartyLit","DiscoPartyLit001.mp4",videovol,1
     PuPlayer.playlistplayex pMusic,"audiomodes","DiscoPartyLit001.mp3",0,1
     PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
     DMD "dmd baseBlank.png", ComboM &" Combos-> 4 More For", "MULTIBALL", 2000
 End If

 If ComboM = 9 Then
     GraveStoneSuperTarget001.Isdropped = 0
     GraveStoneSuperTarget002.Isdropped = 0
     GraveStoneSuperTarget003.Isdropped = 0
     SuperTargetsOffTimer.Enabled = True
     SuperTargetsOffTimer001.Enabled = True  
     DiscoBlocker.Isdropped = 0
     quotetrigger.Enabled = false
     quotetrigger001.Enabled = False
     quotetrigger002.Enabled = False
     VideoQuoteResteTimer001.Enabled = False
     VideoQuoteResteTimer002.Enabled = False
     VideoQuoteResteTimer003.Enabled = False
     VideoQuoteResteTimer004.Enabled = True
     VideoQuoteResteTimer005.Enabled = False
     AudioAttractTimer001.Enabled = False
     Trigger2.Enabled = False
     MBBlocker.Isdropped = 0
     'MBBlocker5.Isdropped = 0
     MBBlocker001.Isdropped = 0
     MBBlocker002.Isdropped = 0
     MBBlockerTimer.Enabled = True
     MBBlockerTimer003.Enabled = True
     MBBlockerTarget.Enabled = True
     MBBlocker001Target.Enabled = True
     MBBlocker002Target.Enabled = True
     MBBlockerlight001.state = 2
     MBBlockerlight002.state = 2
     MBBlockerlight003.state = 2
     MBBlockerlight004.state = 2
     MBBlockerlight005.state = 2
     PuPlayer.playlistplayex pBackglass,"videoDiscoPartyLit","DiscoPartyLit002.mp4",videovol,1
     PuPlayer.playlistplayex pMusic,"audiomodes","DiscoPartyLit002.mp3",0,1
     PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
    DMD "dmd baseBlank.png", ComboM &" Combos-> 1 More For", "MULTIBALL", 2000
 End If

 If ComboM = 10 Then
    ComboM = 0
    GraveStoneSuperTarget001.Isdropped = 0
    GraveStoneSuperTarget002.Isdropped = 0
    GraveStoneSuperTarget003.Isdropped = 0
    SuperTargetsOffTimer.Enabled = True 
    SuperTargetsOffTimer001.Enabled = True  
    MBBlocker.Isdropped = 0
    MBBlocker001.Isdropped = 0
    MBBlocker002.Isdropped = 0
    MBBlockerTimer.Enabled = False
    MBBlockerTimer001.Enabled = False
    MBBlockerTimer002.Enabled = False
    MBBlockerlight001.state = 2
    MBBlockerlight002.state = 2
    MBBlockerlight003.state = 2
    MBBlockerlight004.state = 2
    MBBlockerlight005.state = 2
    quotetrigger.Enabled = false
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = True
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False
    DMD "dmd baseBlank.png", ComboM &" Combos Ready For", "MULTIBALL", 2000
    vpmtimer.addtimer 3000, "ComboMultiball '"
 End If
End Sub

Sub ComboMultiball()
    StopSound Song:Song = ""
    AddMultiball 3
    StartDisco
    StartEnidSpotLightSeq
    WizardSaverKicker.Enabled = True
    Drain.Enabled = 0
    GIOFFWIZARDTrigger.Enabled = True
    GIOFFWIZARDTrigger001.Enabled = True
    GIOFFWIZARDTrigger002.Enabled = True
    GIOFFWIZARDTrigger003.Enabled = True
    GIOFFWIZARDTrigger004.Enabled = True
    FireBallDMDTrigger.Enabled = False
    totherampSkill003.Enabled = False
 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 'Start DiscoPartyMB
 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
	PuPlayer.playlistplayex pBackglass,"videoDiscoPartyMB","DiscoPartyMB.mp4",videovol,1
	PuPlayer.playlistplayex pMusic,"audiomodes","AudioDiscoPartyMB.mp3",0,1
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
    DiscoPartyMBTimeoutTimer.Enabled = True 
    MBBlockerTimer.Enabled = False
    MBBlockerTimer001.Enabled = False
    MBBlockerTimer002.Enabled = False
    APGlow.state = 2
    totherampSkill003.Enabled = False
    quotetrigger.Enabled = false
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False 
    DMDSmall3.visible = 0
    RavenDiscoSDMD.visible = 1
    RavenDiscoSDMD001.visible = 1
    RavenDiscemallDMDOn.Enabled = true
End Sub


Sub StopAllMusic:EndMusic:end sub




'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'   PUPDMD - WIP
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

'**************************
'   PinUp Player Config
'   Change HasPuP = True if using PinUp Player Videos
'**************************

	Dim HasPup:HasPuP = True

'	Dim PuPlayer

			Const pTopper=0
			Const pDMD=1
            Const pBackglass=2		'If you are wanting to use the pup on the FULLDMD Set this Number to whatever Number your FULLDMD Screen is		
			Const pPlayfield=3 
			Const pMusic=4
'			Const pBackglassB2S = 2			
			Const pAudio=7
			Const pCallouts=8
Sub InitPuP
	if HasPuP Then
	' on error resume next
	' Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay") 
	' on error goto 0
	if not IsObject(PuPlayer) then HasPuP = False
	end If

	if HasPuP Then

	PuPlayer.Init pBackglass,"SpookyWednesday"
	If useB2SBG = 1 Then
		PuPlayer.Init pBackglassB2S,"SpookyWednesday"		
	End If
	PuPlayer.Init pMusic,"SpookyWednesday"
	PuPlayer.Init pAudio,"SpookyWednesday"
	PuPlayer.Init pCallouts,"SpookyWednesday"
	If toppervideo = 1 Then
	PuPlayer.Init pTopper,"SpookyWednesday"
	If useB2SBG = 1 Then
		PuPlayer.Init pBackglassB2S,"SpookyWednesday"		
	End If
	End If

	PuPlayer.SetScreenex pBackglass,0,0,0,0,0       'Set PuPlayer DMD TO Always ON    <screen number> , xpos, ypos, width, height, POPUP
    PuPlayer.SetScreenex pAudio,0,0,0,0,2
	PuPlayer.hide pAudio
	PuPlayer.SetScreenex pMusic,0,0,0,0,2
	PuPlayer.hide pMusic
	PuPlayer.SetScreenex pCallouts,0,0,0,0,2
	PuPlayer.hide pCallouts
	If useB2SBG = 1 then 
		PuPlayer.SetScreenex pBackglassB2S,0,0,0,0,0
	If useB2SBG = 1 Then
		PuPlayer.SetScreenex pTopper,0,0,0,0,0	
	End If
	End If



PuPlayer.playlistadd pMusic,"audioattract", 1 , 0
PuPlayer.playlistadd pMusic,"audiobg", 1 , 0
PuPlayer.playlistadd pMusic,"audioclear", 1 , 0
PuPlayer.playlistadd pMusic,"audiobgrock", 1 , 0
PuPlayer.playlistadd pAudio,"audioevents", 1 , 0
PuPlayer.playlistadd pAudio,"audiomodes", 1 , 0
PuPlayer.playlistadd pMusic,"audioquotes", 1 , 0
PuPlayer.playlistadd pAudio,"audiomultiballs", 1 , 0
PuPlayer.playlistadd pCallouts,"audiocallouts", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioBallLost", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioJackpots", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioSuperJackpots", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioDiscoMB", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioDiscoMBCleared", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioFoundTheHyde", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioHydeMBCleared", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioHydeMBLock", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioWheresTheHyde", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioOnTheHunt", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioWTHMBCleared", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioWizardMode", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioWizardCleared", 1 , 0
PuPlayer.playlistadd pCallouts,"AudioGameLost", 1 , 0
PuPlayer.playlistadd pCallouts,"audiocheers", 1 , 0
PuPlayer.playlistadd pBackglass,"bgs", 1 , 0
PuPlayer.playlistadd pBackglass,"scene", 1 , 0
PuPlayer.playlistadd pBackglass,"videoattract", 1 , 0
PuPlayer.playlistadd pBackglass,"videosnapstart", 1 , 0
PuPlayer.playlistadd pBackglass,"videoballsaved", 1 , 0
PuPlayer.playlistadd pBackglass,"videodrain", 1 , 0
PuPlayer.playlistadd pBackglass,"videoquotes", 1 , 0
PuPlayer.playlistadd pBackglass,"videoquotes2", 1 , 0
PuPlayer.playlistadd pBackglass,"videoquotes3", 1 , 0
PuPlayer.playlistadd pBackglass,"videoHydelit", 1 , 0
PuPlayer.playlistadd pBackglass,"videoHydelock", 1 , 0
PuPlayer.playlistadd pBackglass,"videoHydemb", 1 , 0
PuPlayer.playlistadd pBackglass,"videoHydeKilled", 1 , 0
PuPlayer.playlistadd pBackglass,"Seceret Libriary", 1 , 2
PuPlayer.playlistadd pBackglass,"WheresTheHydeLock", 1 , 0
PuPlayer.playlistadd pBackglass,"FoundTheHyde", 1 , 0
PuPlayer.playlistadd pBackglass,"videoFoundTheHydeMB", 1 , 0
PuPlayer.playlistadd pBackglass,"FreeGomezMode", 1 , 2
PuPlayer.playlistadd pBackglass,"videoDiscoPartyLit", 1 , 0
PuPlayer.playlistadd pBackglass,"videoDiscoPartyMB", 1 , 0
PuPlayer.playlistadd pBackglass,"SaveEugeneMode", 1 , 2
PuPlayer.playlistadd pBackglass,"WednesdayWizardMode", 1 , 0
PuPlayer.playlistadd pBackglass,"WizardMB", 1 , 0
PuPlayer.playlistadd pBackglass,"EndOfWizardMode", 1 , 0
PuPlayer.playlistadd pBackglass,"LostWizardMode", 1 , 2
PuPlayer.playlistadd pBackglass,"SkillShot", 1 , 0
End If
End Sub


Sub chilloutthemusic
If calloutlowermusicvol = 1 Then
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":40 }"
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":40 }"
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 7, ""FN"":11, ""VL"":40 }"
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 8, ""FN"":11, ""VL"":"&(calloutvol)&" }"
    vpmtimer.addtimer 4500, "turnitbackup'"
End If
End Sub

Sub turnitbackup
If calloutlowermusicvol = 1 Then
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 2, ""FN"":11, ""VL"":"&(videovol)&" }"
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":"&(soundtrackvol)&" }"
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 7, ""FN"":11, ""VL"":"&(soundtrackvol)&" }"
End If
End Sub

'*****************************************************
'		 THINGS MAGNET AND BALL SAVE KICKER
'*****************************************************
' Magic Ball

Sub magickicker_Timer
magickicker.Kick 2, 9
magickicker.TimerEnabled = 0
'vpmTimer.AddTimer 800, " magickicker.TimerEnabled = 1 '"
End Sub

Sub magickicker_Hit()
magickicker.TimerEnabled = 1
flashforms  MagicLightning,700, 70, 0
flashforms  MagicLightning001,800, 70, 0
playsound "sfx_electric6"
LeftMagnet.MagnetOn = True
vpmTimer.AddTimer 850, " LeftMagnet.MagnetOn = False '"
PlaySound ""
end sub


Sub MagicMagOffTrigger_Hit()
LeftMagnet.MagnetOn = False
vpmTimer.AddTimer 1, " LeftMagnet.MagnetOn = False '"
PlaySound ""
end sub



Sub MagicMagOffTrigger001_Hit()
LeftMagnet.MagnetOn = False
vpmTimer.AddTimer 1, " LeftMagnet.MagnetOn = False '"
PlaySound ""
end sub
'*****************************************************
'   	RANDOM PUP_VIDEO TRIGGER AND PLAYLIST
'*****************************************************
'****Quotes****

Sub quotetrigger_Hit()
playsound "sfx_thunder2"
playsound "Werewolf"
PuPlayer.playlistplayex pBackglass,"videoquotes","",videovol,0
PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
PuPlayer.playlistplayex pMusic,"audioquotes","bang.mp3",0,0
quotetrigger.Enabled = False
quotetrigger001.Enabled = False
quotetrigger002.Enabled = False
VideoQuoteResteTimer.Enabled = True
AudioAttractTimer001.Enabled = False
end sub

Sub quotetrigger001_Hit()
playsound "burp"
playsound "EvilMaleLaugh002"
PuPlayer.playlistplayex pBackglass,"videoquotes2","",videovol,0
PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
PuPlayer.playlistplayex pMusic,"audioquotes","GoodieSavesWed.mp3",0,0
quotetrigger.Enabled = False
quotetrigger001.Enabled = False
quotetrigger002.Enabled = False
VideoQuoteResteTimer001.Enabled = True
AudioAttractTimer001.Enabled = False
end sub


Sub quotetrigger002_Hit()
playsound "Ghostly Breath"
playsound "sfx_thunder7"
PuPlayer.playlistplayex pBackglass,"videoquotes3","",videovol,0
PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
PuPlayer.playlistplayex pMusic,"audioquotes","HelloThing.mp3",0,0
quotetrigger.Enabled = False
quotetrigger001.Enabled = False
quotetrigger002.Enabled = False
VideoQuoteResteTimer002.Enabled = True
AudioAttractTimer001.Enabled = False
end sub


Sub VideoQuoteResteTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    VideoQuoteResteTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	VideoQuoteResteTimer003.Enabled = True
End Sub


Sub VideoQuoteResteTimer001_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    VideoQuoteResteTimer001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	VideoQuoteResteTimer003.Enabled = True
End Sub


Sub VideoQuoteResteTimer002_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    VideoQuoteResteTimer002.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	VideoQuoteResteTimer003.Enabled = True
End Sub


Sub VideoQuoteResteTimer003_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    VideoQuoteResteTimer003.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	PuPlayer.playlistplayex pMusic,"audiobgrock","",soundtrackvol,0
    PuPlayer.SetLoop 4,0
    quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    quotetrigger002.Enabled = True
 End Sub


Sub VideoQuoteResteTimer005_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    VideoQuoteResteTimer005.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    quotetrigger002.Enabled = True
    AudioAttractTimer001.Enabled = True 
 End Sub


Sub VideoQuoteResteTimer004_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    VideoQuoteResteTimer004.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    quotetrigger002.Enabled = True
    AudioAttractTimer001.Enabled = True 
 End Sub


Sub QON_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    QON.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	PuPlayer.playlistplayex pMusic,"audiobgrock","",soundtrackvol,0
    PuPlayer.SetLoop 4,0
    quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    quotetrigger002.Enabled = True
End Sub


Sub QON3_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    QON3.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    quotetrigger002.Enabled = True
End Sub


Sub QON2_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    QON2.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	PuPlayer.playlistplayex pMusic,"audiobgrock","",soundtrackvol,0
    PuPlayer.SetLoop 4,0
    quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    quotetrigger002.Enabled = True
End Sub
'*****************************************************


'*****************************************************
'	  THINGS APRON MAGNET AND LAUNCH BALL KICKER
'*****************************************************
'thingsapronkickers**********
Sub thingsKickerapron_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    thingsKickerapron.DestroyBall
    thingapronkicker2.CreateBall:thingapronkicker2.Kick -180,5:PlaySound ""
    thingapronkicker2.Kick -180, a
    Pyramid2Exit
    ThingMagnet.MagnetOn = True
    vpmTimer.AddTimer 3200, " ThingMagnet.MagnetOn = False '"
    PlaySound "zap"
End Sub




'********************************************
'    Apron Launcher and Skillshot Launcher
'********************************************
Sub totherampSkill003_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    totherampSkill003.DestroyBall
    thingsrampkicker.CreateBall:thingsrampkicker.Kick 2, 120:PlaySound "zap"
    thingsrampkicker.Kick 2, 120, a
    StartSwirl
    ThingLaunchShakerOffTimer.Enabled = True
    thingsrampkicker.Enabled = 0
    SkillShotWonTrigger.Enabled = True
    GreenLightTimerReset.Enabled = False
    Spot2Pink001.state = 2
    Spot2Pink002.state = 2
    Spot2Pink003.state = 2
    PlaySound "thing shoots"
End Sub


Sub totheramp_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    totheramp.DestroyBall
    LaunchKicker.CreateBall:LaunchKicker.Kick 2, 22:PlaySound "zap"
    LaunchKicker.Kick 2, 22, a
    PlaySound "thing shoots"
End Sub



'********************************************
'    Skillshot lights amd Timers & Kickers
'********************************************

Sub ThingLaunchSkillTrigger_Hit()
    if Spot2Pink003.state = 1 and Light001.state = 1 and bMultiBallMode = False then
    totherampSkill003.Enabled = True
    ThingLaunchTrigger.Enabled = 0
    quotetrigger.Enabled = False
    quotetrigger001.Enabled = False
    quotetrigger002.Enabled = False
    VideoQuoteResteTimer004.Enabled = True
    ExtraBall.state = 2
    DMDFLush
    DMD "dmd baseBlank.png", "", "", 100
    DMD "SkillShott.png", " ", "", 1500
    DMD "dmd baseBlank.png", "", "", 100
    DMD "SkillShott.png", " ", "", 1500
    DMD "dmd baseBlank.png", "", "", 100
    DMD "SkillShott.png", " ", "", 1500
    DMD "dmd baseBlank.png", "", "", 100
    DMD "SkillShott.png", " ", "", 1500
    PuPlayer.playlistplayex pBackglass,"SkillShot","",115,1
    VideoQuoteResteTimer004.Enabled = True
    SkillShotTimersOff
    End If
    if Spot2Pink003.state = 0 and bMultiBallMode = False then
    totherampSkill003.Enabled = 0
    ThingLaunchTrigger.Enabled = 1 
    SkillShotLightsOff
    SkillShotTimersOff
    End If
    End Sub



Sub ThingLaunchTrigger_hit()
    If bMultiBallMode = False then
    SkillShotLightsOff
    SkillShotTimersOff
    totheramp.Enabled = False 
    thingPFAKicker.Enabled = True
    End If
    If bMultiBallMode = True then
    SkillShotLightsOff
    SkillShotTimersOff
    totheramp.Enabled = True 
    thingPFAKicker.Enabled = False
    End If
  End Sub







Sub thingPFAKicker_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    thingPFAKicker.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	totheramp.Enabled = True
    SkillTimerNLightsKill.Enabled = True
    SkillShotLightsOff
    SkillShotTimersOff
   End Sub


Sub SkillTimerNLightsKill_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SkillTimerNLightsKill.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	SkillShotLightsOff
    SkillShotTimersOff
   End Sub





Sub GreenLightTimeron_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    GreenLightTimeron.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	Spot2Pink003.state = 1
    GreenLightTimeroff.Enabled = True 
   End Sub


Sub GreenLightTimeroff_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    GreenLightTimeroff.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	Spot2Pink003.state = 0
    Spot2Pink002.state = 0
    Spot2Pink001.state = 0
    GreenLightTimerReset.Enabled = True
    GreenLightSkillTrigger.Enabled = False
End Sub

Sub GreenLightTimerReset_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    GreenLightTimerReset.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	GreenLightTimeron.Enabled = True 
    RedLightTimeron.Enabled = True 
    YellowLightTimeron.Enabled = True 
   End Sub


Sub RedLightTimeron_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    RedLightTimeron.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	Spot2Pink001.state = 1
   End Sub

Sub YellowLightTimeron_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    YellowLightTimeron.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	Spot2Pink002.state = 1
   End Sub



Sub SkillWonAPLTrigger_hit()
    AddScore 500
    End Sub


Sub SkillShotWonTrigger_hit()
    SkillShotTimersOff
    SkillShotTriggerOffTimer.Enabled = True
    SkillWonAPLTrigger.Enabled = True
   End Sub


Sub SkillShotWonTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SkillShotWonTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	totherampSkill001.Enabled = True
    SkillKickerWonOffTimer.Enabled = True
End Sub

Sub SkillKickerWonOffTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SkillKickerWonOffTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	totherampSkill001.Enabled = False
    SkillShotLightsOff
    SkillShotTimersOff
    StopSwirl
    End Sub


Sub SkillShotTimersOn()
    GreenLightTimeron.Enabled = True
    RedLightTimeron.Enabled = True
    YellowLightTimeron.Enabled = True
End Sub


Sub SkillShotTimersOff()
    GreenLightTimeron.Enabled = False
    RedLightTimeron.Enabled = False
    YellowLightTimeron.Enabled = False
End Sub


Sub SkillShotLightsOff()
    Spot2Pink001.state = 0
    Spot2Pink002.state = 0
    Spot2Pink003.state = 0
End Sub


Sub SkillLightsFlashing()
    Spot2Pink001.state = 2
    Spot2Pink002.state = 2
    Spot2Pink003.state = 2
End Sub


Sub SkillShotLightsOn()
    Spot2Pink001.state = 1
    Spot2Pink002.state = 1
    Spot2Pink003.state = 1
End Sub



Sub SkillShotDrainResetTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SkillShotDrainResetTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	SkillShotTimersOn
   End Sub


Sub SkillShotTriggerOffTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SkillShotTriggerOffTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	SkillShotWonTrigger.Enabled = False
   End Sub





Sub totherampSkill_Timer
totherampSkill003.Kick 2, 30
totherampSkillTimer.Enabled = 0
End Sub

Sub totherampSkill_Hit()
totherampSkillTimer.Enabled = 1
PlaySound "sfx_thunder7"
end sub


Sub totherampSkill001Timer_Timer
totherampSkill001.Kick 2, 70
totherampSkill001Timer.Enabled = 0
SkillWonAPL.state = 1
End Sub

Sub totherampSkill001_Hit()
totherampSkill001Timer.Enabled = 1
end sub


Sub totherampSkill002Timer_Timer
totherampSkill002.Kick 2, 120
totherampSkill002Timer.Enabled = 0
End Sub

Sub totherampSkill002_Hit()
totherampSkill002Timer.Enabled = 1
SkillShotWonTimer.Enabled = 1
end sub




'*****************************************************
'		 THINGS APRON FLASHERS AND LIGHTS
'*****************************************************
'*****Apron Animation*******

Sub ThingsApronanimation1ON_Hit
	'thingsapronanimation1.opacity = 75
    'thingsapronanimation3.opacity = 25
    'thingsapronanimation6.opacity = 55
    'apronlight1.Duration 2, 2, 2
    'ThingShake
End Sub

Sub ThingsApronanimation1OFF_Hit
    ThingLaunchShakerOnTimer.Enabled = True
End Sub


Sub ThingsApronLightning_Hit
    ThingLaunchShakerOffTimer.Enabled = True
End Sub




'*********************************************************************************
Sub PyramidExit()
    FlashForMs TFL1, 1000, 50, 0
    FlashForMs TFL001, 1000, 50, 0
    FlashForMs TFL002, 1000, 50, 0
    FlashForMs TFL003, 1000, 50, 0
    FlashForMs TFL004, 1000, 50, 0
    FlashForMs TFL005, 1000, 50, 0
    FlashForMs TFL006, 1000, 50, 0
    flashforms  LittleThingZap,2000, 70, 0
    PlaySound "sfx_electric6"
end sub


Sub Pyramid2Exit()
    FlashForMs TFL007, 1000, 50, 0
    FlashForMs TFL008, 1000, 50, 0
    FlashForMs TFL009, 1000, 50, 0
    FlashForMs TFL010, 1000, 50, 0
    FlashForMs TFL011, 1000, 50, 0
    FlashForMs TFL012, 1000, 50, 0
    FlashForMs TFL013, 1000, 50, 0
    flashforms  ThingsApronZap,2000, 60, 0
end sub


Sub thingapronrainbowtimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    thingapronrainbowtimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Pyramid2Exit
 End Sub


'Sub ThingsShakerTrigger_Hit
'	ThingShake
 'End Sub

'******************
'     FOG Intro
'******************

'Dim fr, fh, fopac 'rotation and height

Sub Start_Fog
    fog_flash.visible = 1
    FogSpinTimer.Enabled = 1
 End Sub

Sub Stop_Fog
    'DOF 141, DOFOff
    fog_flash.visible = 0
    FogSpinTimer.Enabled = 0
End Sub

Sub FogSpinTimer_Timer
    fog_flash.rotz = (fog_flash.rotz + 1)mod 360
End Sub


'*****************************************************
'		 BLOOD APRON FLASHER ANIMATION
'*****************************************************

Dim ExCnt
ExCnt = 0
ExAnimation.Interval=17*2  '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub ExAnimation_Timer
	'Initialize the animation
	If ExFlasher.visible = False Then 
		ExFlasher.visible = True
		ExCnt = 0
	End If

	'Select the correct frame
	If ExCnt > 99 Then
		ExFlasher.imageA = "EXP-" & ExCnt
	Elseif ExCnt > 9 Then
		ExFlasher.imageA = "EXP-0" & ExCnt
	Else
		ExFlasher.imageA = "EXP-00" & ExCnt
	End If
	ExCnt = ExCnt + 1
    ExpLight.State=1

	'Finish animation
	If ExCnt > 113 Then    
		ExCnt = 0
        ExFlasher001.visible = True
		ExAnimation.Enabled = False
		ExFlasher.visible = False
    End If
End Sub


'ExplsionSFX.Enabled = True


'*****************************************************
'		 BLOOD RUNNING FROM RAMPS ON PLAYFIELD
'*****************************************************
Dim BRCnt
BRCnt = 0
BRAnimation.Interval=17*2 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub BRAnimation_Timer
	'Initialize the animation
	If BRFlasher.visible = False Then 
		BRFlasher.visible = True
		BRCnt = 0
	End If

	'Select the correct frame
	If BRCnt > 99 Then
		BRFlasher.imageA = "BRS-" & BRCnt
	Elseif BRCnt > 9 Then
		BRFlasher.imageA = "BRS-0" & BRCnt
	Else
		BRFlasher.imageA = "BRS-00" & BRCnt
	End If
	BRCnt = BRCnt + 1
    BRLight.State=1

	'Finish animation
	If BRCnt > 113 Then    
		BRCnt = 0
        'BRFlasher001.visible = True
		BRAnimation.Enabled = False
		BRFlasher.visible = False
    End If
End Sub


Sub BloodRepeatTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    BloodRepeatTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    BRAnimation.Enabled = True
    ExplsionSFX.Enabled = True
    BloodRepeatTimer2.Enabled = true
End Sub

Sub BloodRepeatTimer2_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    BloodRepeatTimer2.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    BRAnimation.Enabled = True
    ExplsionSFX.Enabled = True
    BloodRepeatTimer.Enabled = true
End Sub

'BRAnimation.Enabled = True



'BloodRamp

Sub BloodRampT1_Hit
	BloodRamp1.opacity = 3000
 End Sub

Sub BloodRampT2_Hit
	BloodRamp2.opacity = 3000
 End Sub

Sub BloodRampT3_Hit
	BloodRamp3.opacity = 3000
 End Sub

Sub BloodRampT4_Hit
	BloodRamp4.opacity = 3000
 End Sub

Sub BloodRampT5_Hit
	BloodRamp5.opacity = 3000
 End Sub

Sub BloodRampT6_Hit
	BloodRamp6.opacity = 3000
 End Sub


Sub BloodRampT7_Hit
	BloodRamp7.opacity = 3000
 End Sub

Sub BloodRampT8_Hit
	BloodRamp8.opacity = 3000
 End Sub

Sub BloodRampT9_Hit
	BloodRamp9.opacity = 3000
 End Sub

Sub BloodRampT10_Hit
	BloodRamp10.opacity = 3000
 End Sub

Sub BloodRampT11_Hit
	BloodRamp11.opacity = 3000
 End Sub

Sub BloodRampT12_Hit
	BloodRamp12.opacity = 3000
 End Sub

Sub BloodRampT13_Hit
	BloodRamp13.opacity = 3000
 End Sub

Sub BloodRampT14_Hit
	BloodRamp14.opacity = 3000
 End Sub


Sub BloodRampT15_Hit
	BloodRamp15.opacity = 3000
 End Sub

Sub BloodRampT16_Hit
	BloodRamp16.opacity = 3000
 End Sub

Sub BloodRampT17_Hit
	BloodRamp17.opacity = 3000
 End Sub

Sub BloodRampT18_Hit
	BloodRamp18.opacity = 3000
 End Sub

Sub BloodRampT19_Hit
	BloodRamp19.opacity = 3000
    BRAnimation.Enabled = True
 End Sub

Sub BloodRampT20_Hit
	BloodRamp20.opacity = 3000
    BloodRamp21.opacity = 3000
 End Sub

Sub BloodRampANITRIGGER_Hit
	BRAnimation.Enabled = True
 End Sub


'Bloodrampoff

Sub BloodRampToff_Hit
	BloodRamp1.opacity = 0
    BloodRamp2.opacity = 0
    BloodRamp3.opacity = 0
    BloodRamp4.opacity = 0
    BloodRamp5.opacity = 0
    BloodRamp6.opacity = 0
    BloodRamp7.opacity = 0
    BloodRamp8.opacity = 0
    BloodRamp9.opacity = 0
    BloodRamp10.opacity = 0
    BloodRamp11.opacity = 0
    BloodRamp12.opacity = 0
    BloodRamp13.opacity = 0
    BloodRamp14.opacity = 0
    BloodRamp15.opacity = 0
    BloodRamp16.opacity = 0
    BloodRamp17.opacity = 0
    BloodRamp18.opacity = 0
    BloodRamp19.opacity = 0
    BloodRamp20.opacity = 0
    BloodRamp21.opacity = 0
 End Sub


Sub ShutOffBloodRampToff
	BloodRamp1.opacity = 0
    BloodRamp2.opacity = 0
    BloodRamp3.opacity = 0
    BloodRamp4.opacity = 0
    BloodRamp5.opacity = 0
    BloodRamp6.opacity = 0
    BloodRamp7.opacity = 0
    BloodRamp8.opacity = 0
    BloodRamp9.opacity = 0
    BloodRamp10.opacity = 0
    BloodRamp11.opacity = 0
    BloodRamp12.opacity = 0
    BloodRamp13.opacity = 0
    BloodRamp14.opacity = 0
    BloodRamp15.opacity = 0
    BloodRamp16.opacity = 0
    BloodRamp17.opacity = 0
    BloodRamp18.opacity = 0
    BloodRamp19.opacity = 0
    BloodRamp20.opacity = 0
    BloodRamp21.opacity = 0
 End Sub




'************************************************************************
'			Spider Crawl Animations and Triggers
'************************************************************************
Dim SCCnt
SCCnt = 0
SCAnimation.Interval=17*2 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub SCAnimation_Timer
	'Initialize the animation
	If SpiderFlasher.visible = False Then 
		SpiderFlasher.visible = True
		SCCnt = 0
	End If

	'Select the correct frame
	If SCCnt > 43 Then
		SpiderFlasher.imageA = "SC-" & SCCnt
	Elseif SCCnt > 9 Then
		SpiderFlasher.imageA = "SC-0" & SCCnt
	Else
		SpiderFlasher.imageA = "SC-00" & SCCnt
	End If
	SCCnt = SCCnt + 1
    SCLight.State=1

	'Finish animation
	If SCCnt > 43 Then    
		SCCnt = 0
        'BRFlasher001.visible = True
		SCAnimation.Enabled = False
		SpiderFlasher.visible = False
    End If
End Sub


'SCAnimation.Enabled = True
'*************************************************************************
'		Spider Crawl Animations and Triggers 2		
'*************************************************************************
Dim SC001Cnt
SC001Cnt = 0
SC001Animation.Interval=17*2 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub SC001Animation_Timer
	'Initialize the animation
	If SpiderFlasher001.visible = False Then 
		SpiderFlasher001.visible = True
		SC001Cnt = 0
	End If

	'Select the correct frame
	If SC001Cnt > 40 Then
		SpiderFlasher001.imageA = "SC2-" & SC001Cnt
	Elseif SC001Cnt > 9 Then
		SpiderFlasher001.imageA = "SC2-0" & SC001Cnt
	Else
		SpiderFlasher001.imageA = "SC2-00" & SC001Cnt
	End If
	SC001Cnt = SC001Cnt + 1
    SC001Light.State=1

	'Finish animation
	If SC001Cnt > 40 Then    
		SC001Cnt = 0
        'BRFlasher001.visible = True
		SC001Animation.Enabled = False
		SpiderFlasher001.visible = False
    End If
End Sub


'SC001Animation.Enabled = True


'*************************************************************************
'		Spider Crawl Animations and Triggers 3		
'*************************************************************************
Dim SC002Cnt
SC002Cnt = 0
SC002Animation.Interval=17*2 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub SC002Animation_Timer
	'Initialize the animation
	If SpiderFlasher002.visible = False Then 
		SpiderFlasher002.visible = True
		SC002Cnt = 0
	End If

	'Select the correct frame
	If SC002Cnt > 38 Then
		SpiderFlasher002.imageA = "SC3-" & SC002Cnt
	Elseif SC002Cnt > 9 Then
		SpiderFlasher002.imageA = "SC3-0" & SC002Cnt
	Else
		SpiderFlasher002.imageA = "SC3-00" & SC002Cnt
	End If
	SC002Cnt = SC002Cnt + 1
    SC002Light.State=1

	'Finish animation
	If SC002Cnt > 38 Then    
		SC002Cnt = 0
        'BRFlasher001.visible = True
		SC002Animation.Enabled = False
		SpiderFlasher002.visible = False
    End If
End Sub


'SC002Animation.Enabled = True


'*************************************************************************
'		Spider Crawl Animations and Triggers 4		
'*************************************************************************
Dim SC003Cnt
SC003Cnt = 0
SC003Animation.Interval=17*2 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub SC003Animation_Timer
	'Initialize the animation
	If SpiderFlasher003.visible = False Then 
		SpiderFlasher003.visible = True
		SC003Cnt = 0
	End If

	'Select the correct frame
	If SC003Cnt > 36 Then
		SpiderFlasher003.imageA = "SC4-" & SC003Cnt
	Elseif SC003Cnt > 9 Then
		SpiderFlasher003.imageA = "SC4-0" & SC003Cnt
	Else
		SpiderFlasher003.imageA = "SC4-00" & SC003Cnt
	End If
	SC003Cnt = SC003Cnt + 1
    SC003Light.State=1

	'Finish animation
	If SC003Cnt > 36 Then    
		SC003Cnt = 0
        'BRFlasher001.visible = True
		SC003Animation.Enabled = False
		SpiderFlasher003.visible = False
    End If
End Sub


'SC003Animation.Enabled = True

'*************************************************************************
'		Spider Crawl Animations and Triggers 4		
'*************************************************************************
Dim SC004Cnt
SC004Cnt = 0
SC004Animation.Interval=17*2 '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub SC004Animation_Timer
	'Initialize the animation
	If SpiderFlasher004.visible = False Then 
		SpiderFlasher004.visible = True
		SC004Cnt = 0
	End If

	'Select the correct frame
	If SC004Cnt > 21 Then
		SpiderFlasher004.imageA = "SC5-" & SC004Cnt
	Elseif SC004Cnt > 9 Then
		SpiderFlasher004.imageA = "SC5-0" & SC004Cnt
	Else
		SpiderFlasher004.imageA = "SC5-00" & SC004Cnt
	End If
	SC004Cnt = SC004Cnt + 1
    SC004Light.State=1

	'Finish animation
	If SC004Cnt > 21 Then    
		SC004Cnt = 0
        'BRFlasher001.visible = True
		SC004Animation.Enabled = False
		SpiderFlasher004.visible = False
    End If
End Sub


'SC004Animation.Enabled = True



'*****************************************************
'		Crackstone FLASHER ANIMATION
'*****************************************************

Dim CStoneEXPCnt
CStoneEXPCnt = 0
CStoneAnimation.Interval=17*2  '<-- increase this number to make animation slower. 17 is 60 fps, 17*2 is 30 fps, 17*3 is 20 fps (roughly)

Sub CStoneAnimation_Timer
	'Initialize the animation
	If CStoneEXFlasher.visible = False Then 
		CStoneEXFlasher.visible = True
		CStoneEXPCnt = 0
	End If

	'Select the correct frame
	If CStoneEXPCnt > 67 Then
		CStoneEXFlasher.imageA = "Missile-" & CStoneEXPCnt
	Elseif CStoneEXPCnt > 9 Then
		CStoneEXFlasher.imageA = "Missile-0" & CStoneEXPCnt
	Else
		CStoneEXFlasher.imageA = "Missile-00" & CStoneEXPCnt
	End If
	CStoneEXPCnt = CStoneEXPCnt + 1
    CStoneLight.State=1

	'Finish animation
	If CStoneEXPCnt > 67 Then    
		CStoneEXPCnt = 0
        'ExFlasher001.visible = True
		CStoneAnimation.Enabled = False
		CStoneEXFlasher.visible = False
    End If
End Sub


'CStoneAnimation.Enabled = True




'***************************************************************
'   HYDE MULTIBALL 2 ANIMATION AND JACKPOTS AND SUPER JACKPOT
'***************************************************************
'*************************
'Slimer UP/DOWN Animation
'*************************

Dim SlimerPos, SlimerDir, SlimerShakePos, SlimerShakeDir, SlimerHitPos, SlimerHits
Dim bSlimerUp, bPlayfieldSlimed

SlimerPos = 160
SlimerShakePos = -160

Sub SlimerLocation(param)
    Select Case param
        Case 1:Slimer.X = 253:Slimer.Y = 949.5:SlimerTrigger1.Enabled = 1:SlimerTrigger2.Enabled = 0:SlimerTrigger3.Enabled = 0
        Case 2:Slimer.X = 455:Slimer.Y = 927.:SlimerTrigger2.Enabled = 1:SlimerTrigger1.Enabled = 0:SlimerTrigger3.Enabled = 0
        Case 3:Slimer.X = 717:Slimer.Y = 561:SlimerTrigger3.Enabled = 1:SlimerTrigger1.Enabled = 0:SlimerTrigger2.Enabled = 0
    End Select
End Sub

Sub SlimerAnimTimer_Timer()
    SlimerShakeTimer.Enabled = 0
    SlimerLocationTimer.Enabled = 0
    SlimerPos = SlimerPos + SlimerDir
    'Slimer is moving up
    If SlimerPos >= 160 Then
        DOF 127, DOFOff
        'Me.Enabled = 0
        SlimerPos = 160
        SlimerShakeDir = 1
        SlimerShakeTimer.Enabled = 1
        SlimerLocationTimer.Enabled = 1
        bSlimerUp = True
    End If
    'Slimer is moving down
    If SlimerPos <= -160 Then
        DOF 127, DOFOff
        'Me.Enabled = 0
        SlimerPos = -160
        SlimerTrigger1.Enabled = 0
        SlimerTrigger2.Enabled = 0
        SlimerTrigger3.Enabled = 0
        'IncreaseGhostLevel
    End If
    Slimer.Transz = SlimerPos
    'Camera.RotX = - SlimerPos / 4
End Sub

Sub SlimerShakeTimer_Timer
    SlimerShakePos = SlimerShakePos + SlimerShakeDir
    'Slimer is moving up
    If SlimerShakePos > 10 Then
        SlimerShakeDir = -1
    End If
    'Slimer is moving down
    If SlimerShakePos < 0 Then
        SlimerShakeDir = 1
    End If
    Slimer.Transz = SlimerShakePos
    'Camera.RotX = - SlimerShakePos / 4
End Sub

Sub SlimerMoveUp()
    PlaySound "gb_slimer2"
    PlaySound "vo_slimer1"
    SlimerLocation INT(RND * 3 + 1)
    SlimerDir = 2
    SlimerAnimTimer.Enabled = 1
    DOF 127, DOFOn
End Sub

Sub SlimerMoveDown()
    SlimerDir = -2
    SlimerAnimTimer.Enabled = 1
    bSlimerUp = False
    DOF 127, DOFOn
    Slimer.X = 455
    Slimer.Y = 927
End Sub





Sub SlimerTrigger1_Hit()
        SlimerHitPos = 6:SlimerHitTimer.Enabled = 1
        DOF 139, DOFOn
' Kill Hyde JACKPOT during Multiball (if ballsonpf = 2):
addscore 250000
playsound "Hydehit"
StartAttackMBLightsJP
PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
DMDFLush
DMD "WheresTheHydeJP.wmv", "", "", 3000
end sub

Sub SlimerTrigger2_Hit()
        SlimerHitPos = 6:SlimerHitTimer.Enabled = 1
        DOF 139, DOFOn
  ' Kill Hyde JACKPOT during Multiball (if ballsonpf = 2):
addscore 250000
playsound "Hydehit"
StartAttackMBLightsJP
PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
DMDFLush
DMD "WheresTheHydeJP.wmv", "", "", 3000
end sub

Sub SlimerTrigger3_Hit()
        SlimerHitPos = 6:SlimerHitTimer.Enabled = 1
        DOF 139, DOFOn
        ' Kill Hyde JACKPOT during Multiball (if ballsonpf = 2):
addscore 250000
playsound "Hydehit"
StartAttackMBLightsJP
PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
DMDFLush
DMD "WheresTheHydeJP.wmv", "", "", 3000
end sub

Sub HydeSuperJackpotTrigger_Hit()
    SlimerHitPos = 6:SlimerHitTimer.Enabled = 1
   ' Kill Hyde JACKPOT during Multiball (if ballsonpf = 2):
PuPlayer.playlistplayex pBackglass,"videoHydeKilled","HydeKilled.mp4",videovol,1
PuPlayer.playlistplayex pMusic,"audiomodes","HydeKilled.mp3",0,1
PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
addscore 250000
playsound "Superjackpot"
playsound "Hydehit"
StartAttackMBLightsJP
SpotlightAttractOn
GiOn
StopEnidSpin
StartLOGOAni
AttackMBLightsKillTimer.Enabled = True
MBBlocker.Isdropped = 1
MBBlocker001.Isdropped = 1
MBBlocker002.Isdropped = 1
MBBlockerTarget.Enabled = false
MBBlocker001Target.Enabled = false
MBBlocker002Target.Enabled = false
HydeSuperJackpotTrigger.Enabled = False
ExplsionSFX.Enabled = True
BRAnimation.Enabled = True
Slimer.X = 403:Slimer.Y = 429.5
APGlow.state = 0
HydeModeDonelit.state = 0
HydeModeDone.state = 1
HMBFlasher22.opacity = 0
HMBFlasher02.opacity = 0
HMBFlasherApron.opacity = 0
NightShadesFailedTimer.Enabled = True
StartVideoNightShadesTimer.Enabled = True
'SingleModeKicker.Enabled = 1
'SingleModeMagnet.MagnetOn = True
StartNightShadesTimer.Enabled = True 
'vpmTimer.AddTimer 5, " SingleModeMagnet.MagnetOn = False '"
'vpmTimer.AddTimer 35, " SingleModeMagnet.MagnetOn = False '"
'vpmTimer.AddTimer 40, " SingleModeMagnet.MagnetOn = False '"
'vpmTimer.AddTimer 45, " SingleModeMagnet.MagnetOn = False '"
DMDFLush
DMD "dmd baseBlank.png", "", "", 300
DMD "HydeDefeated.png", "", "", 4000
DMD "dmd baseBlank.png", "", "", 100
playsound "ModeCleared"
SlimerMoveDown
bSlimerUp = False
HydeSuperJackpotTrigger.Enabled = False
trigger17.Enabled = True
WizardLightGlowFade003.state = 0
WizardLightGlowFade004.state = 0
WizardLightGlowFade005.state = 0
MBBlockerlight001.state = 0
MBBlockerlight002.state = 0
MBBlockerlight003.state = 0
MBBlockerlight004.state = 0
MBBlockerlight005.state = 0
'MBBlocker5.Isdropped = 0
'SJPKicker.Enabled = 1
SJPBlockerTimer.Enabled = True
DiscoStrobe.StopPlay
WizardSpotSeq.StopPlay
WizardSaverKicker.Enabled = False
Drain.Enabled = 1
GIOFFWIZARDTrigger.Enabled = False
GIOFFWIZARDTrigger001.Enabled = False
GIOFFWIZARDTrigger002.Enabled = False
GIOFFWIZARDTrigger003.Enabled = False
GIOFFWIZARDTrigger004.Enabled = False
MBBlocker003.Isdropped = 1
GIOFFWIZARDTrigger002.Enabled = False
RightInLaneTrigger.Enabled = True
LeftInLaneTrigger.Enabled = True
NightShadeGIOff
HYDEmbVideo.Enabled = False 
quotetrigger.Enabled = false
quotetrigger001.Enabled = False
VideoQuoteResteTimer.Enabled = False
VideoQuoteResteTimer001.Enabled = False
VideoQuoteResteTimer002.Enabled = False
VideoQuoteResteTimer003.Enabled = False
VideoQuoteResteTimer004.Enabled = False
VideoQuoteResteTimer005.Enabled = False
quotetrigger002.Enabled = False
AudioAttractTimer001.Enabled = False
JackpotBlockers001.Isdropped = 0
JackpotBlockers002.Isdropped = 0
SPJTargetTimer.Enabled = True
EVHAllSmallDMDsOff
NightHsadeSJPLight.state = 0
HYDEmbVideo.Enabled = False
SingleModeKillerTrigger.Enabled = True
SingleModeKillerKicker001.Enabled = 1
SingleModeKillerKicker009.Enabled = 1
end sub

Sub SlimerHitTimer_Timer()
    Slimer.TransX = SlimerHitPos
    If SlimerHitPos <= 0.1 AND SlimerHitPos >= -0.1 Then :Exit Sub
    If SlimerHitPos < 0 Then
        SlimerHitPos = ABS(SlimerHitPos)- 0.1
    Else
        SlimerHitPos = - SlimerHitPos + 0.1
    End If
End Sub

Sub SlimerLocationTimer_Timer
    SlimerLocation INT(RND * 3 + 1)
End Sub

Sub SlimePlayfield() 'lits 2 lights
    If RND < 0.5 Then
        PlaySound "vo_slimeus2"
    Else
        PlaySound "vo_slimeus"
    End If
    bPlayfieldSlimed = True
End Sub

'***************************************
'        HYDE 2 MULTIBALL-TIMERS 
'***************************************                            
Sub showhydetimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    showhydetimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Slimer.visible =1
 End Sub


Sub SPJTargetTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SPJTargetTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    JackpotBlockers001.Isdropped = 1
    JackpotBlockers002.Isdropped = 1
end sub



'****SlimerUpTimer
Sub SlimerUpTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SlimerUpTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SlimerAnimTimer.Enabled = 0
    SlimerShakeTimer.Enabled = 1
    SlimerLocationTimer.Enabled = 1
    
End Sub




'****SlimerDrainVideoResetTimer
Sub SlimerDrainVideoResetTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SlimerDrainVideoResetTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    PuPlayer.playlistplayex pBackglass,"scene","base.mp4",0,1
    PuPlayer.SetBackground pBackglass,1
    FireBallDMDTrigger.Enabled = True  
    Drain.Enabled = 1
End Sub


'****SlimerDrainTimer
Sub SlimerDrainTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SlimerDrainTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SlimerMoveDown
    FireBallDMDTrigger.Enabled = True
    HSJPTimer1.Enabled = False
    trigger17.Enabled = True
    StopEnidSpin
    WizardSaverKicker.Enabled = False
    Drain.Enabled = 1
    GIOFFWIZARDTrigger.Enabled = False
    GIOFFWIZARDTrigger001.Enabled = False
    GIOFFWIZARDTrigger002.Enabled = False
    GIOFFWIZARDTrigger003.Enabled = False
    GIOFFWIZARDTrigger004.Enabled = False
    MBBlocker003.Isdropped = 1
    GIOFFWIZARDTrigger002.Enabled = False
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    
 End Sub





'****HYDE MB VIDEO KILL*****
Sub HYDEmbVideo_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    HYDEmbVideo.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'SlimerKillTimer.Enabled = False
    GiOn
    EVHAllSmallDMDsOff
    HMBFlasher02.opacity = 0
    HMBFlasher22.opacity = 0
    HMBFlasherApron.opacity = 0
    APGlow.state = 0
    HydeSuperJackpotTrigger.Enabled = False
    HydeModeDonelit.state = 0
    NightHsadeSJPLight.state = 0
    HSJPTimer1.Enabled = False
	SlimerMoveDown
    bSlimerUp = False
    FireBallDMDTrigger.Enabled = True
    quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    QON2.Enabled = True
    AudioAttractTimer001.Enabled = True
    MBBlocker.Isdropped = 1
    MBBlocker001.Isdropped = 1
    MBBlocker002.Isdropped = 1
    MBBlockerTarget.Enabled = false
    MBBlocker001Target.Enabled = false
    MBBlocker002Target.Enabled = false
    MBBlockerTimer.Enabled = False 
    MBBlockerTimer001.Enabled = False 
    MBBlockerTimer002.Enabled = False
    EndHydeJackP.Enabled = False
    MBBlockerlight001.state = 0
    MBBlockerlight002.state = 0
    MBBlockerlight003.state = 0
    MBBlockerlight004.state = 0
    MBBlockerlight005.state = 0
    WizardLightGlowFade003.state = 0
    WizardLightGlowFade004.state = 0
    WizardLightGlowFade005.state = 0
    StopEnidSpin
    trigger17.Enabled = True
    WizardSaverKicker.Enabled = False
    Drain.Enabled = 1
    GIOFFWIZARDTrigger.Enabled = False
    GIOFFWIZARDTrigger001.Enabled = False
    GIOFFWIZARDTrigger002.Enabled = False
    GIOFFWIZARDTrigger003.Enabled = False
    GIOFFWIZARDTrigger004.Enabled = False
    MBBlocker003.Isdropped = 1
    magickicker.Enabled = True
    GIOFFWIZARDTrigger002.Enabled = False
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    SingleModeKillerTrigger.Enabled = False   
    NightShadeGIOff
   End Sub


'****HYDESJP1_Timer*****
Sub HSJPTimer1_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    HSJPTimer1.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    PuPlayer.playlistplayex pCallouts,"AudioSuperJackpots","",calloutvol,1
    HydeSuperJackpotTrigger.Enabled = True
	HydeModeDonelit.state = 2
    DMDFLush
    'DMD "dmd baseBlank.png", "", "", 300
    DMD "WheresTheHydeSJP.wmv", "", "", 4000
    playsound "Jackpotlit"
    SlimerTrigger1.Enabled = 0
    SlimerTrigger2.Enabled = 0
    SlimerTrigger3.Enabled = 0
    WizardSaverKicker.Enabled = False
    Drain.Enabled = 1
    GIOFFWIZARDTrigger.Enabled = False
    GIOFFWIZARDTrigger001.Enabled = False
    GIOFFWIZARDTrigger002.Enabled = False
    GIOFFWIZARDTrigger003.Enabled = False
    GIOFFWIZARDTrigger004.Enabled = False
    MBBlocker003.Isdropped = 1
    GIOFFWIZARDTrigger002.Enabled = True
  End Sub


'**** Reset HMB GI *****
Sub ResetHMBGITimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    ResetHMBGITimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    GiOn
    StartLOGOAni
  End Sub


'****EndHydeJP1_Timer*****
Sub EndHydeJackP_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EndHydeJackP.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    HydeSuperJackpotTrigger.Enabled = false
    HSJPTimer1.Enabled = False
	HydeModeDonelit.state = 0
    NightHsadeSJPLight.state = 0
    trigger17.Enabled = True
    MBBlockerlight001.state = 0
    MBBlockerlight002.state = 0
    MBBlockerlight003.state = 0
    MBBlockerlight004.state = 0
    MBBlockerlight005.state = 0
    WizardSaverKicker.Enabled = False
   Drain.Enabled = 1
   GIOFFWIZARDTrigger.Enabled = False
   GIOFFWIZARDTrigger001.Enabled = False
   GIOFFWIZARDTrigger002.Enabled = False
   GIOFFWIZARDTrigger003.Enabled = False
   GIOFFWIZARDTrigger004.Enabled = False
   MBBlocker003.Isdropped = 1
End Sub


'*********************************************************************************
'Enid VS The Hyde MB Small DMD Timers
'*********************************************************************************

Sub EVHSmallDMDOn_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EVHSmallDMDOn.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    DMDSmallHMB.visible = 0
    Shoot4HydeSDMD.visible = 1
    DMDSmallHMB001.visible = 0
    Shoot4HydeSDMD001.visible = 1
    EVHSmallDMDOff.Enabled = True
  End Sub


Sub EVHSmallDMDOff_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EVHSmallDMDOff.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Shoot4HydeSDMD.visible = 0
    SJPWhenLitSDMD.visible = 1
    Shoot4HydeSDMD001.visible = 0
    SJPWhenLitSDMD001.visible = 1
    EVHSmallDMDOff002.Enabled = True
  End Sub


Sub EVHSmallDMDOff002_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EVHSmallDMDOff002.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SJPWhenLitSDMD.visible = 0
    GraveSDMD.visible = 1
    SJPWhenLitSDMD001.visible = 0
    GraveSDMD001.visible = 1
    EVHSmallDMDOn003.Enabled = True
  End Sub


Sub EVHSmallDMDOn003_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EVHSmallDMDOn003.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    GraveSDMD.visible = 0
    DMDSmallHMB.visible = 1
    GraveSDMD001.visible = 0
    DMDSmallHMB001.visible = 1
    EVHSmallDMDOn.Enabled = True
  End Sub


Sub EVHAllSmallDMDsOff() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DMDSmallHMB.visible = 0
    Shoot4HydeSDMD.visible = 0
    SJPWhenLitSDMD.visible = 0
    GraveSDMD.visible = 0
    DMDSmallHMB001.visible = 0
    Shoot4HydeSDMD001.visible = 0
    SJPWhenLitSDMD001.visible = 0
    GraveSDMD001.visible = 0
    DMDSmall3.visible = 1
    EVHSmallDMDOn.Enabled = False
    EVHSmallDMDOff.Enabled = False
    EVHSmallDMDOff002.Enabled = False
    EVHSmallDMDOn003.Enabled = False
    EVHSmallDMDOn004.Enabled = False
  End Sub
'**************************************************************************************************

'***************************
'  SmallDMD Intro Timers
'***************************
Sub DMDSmall1Timeroff_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DMDSmall1Timeroff.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
   'DMDSMALL Is Off
    DMDSmall1.visible = 0
  End Sub



Sub DMDSmall1Timeron_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DMDSmall1Timeron.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
   'DMDSMALL Is On
    DMDSmall1.visible = 1
  End Sub

Sub DMDSmall2Timeroff2_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DMDSmall2Timeroff2.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
   'DMDSMALL 2 Is Off
    DMDSmall2.visible = 0
  End Sub



Sub DMDSmall2Timeron2_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DMDSmall2Timeron2.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
   'DMDSMALL 2 Is On
    DMDSmall2.visible = 1
  End Sub


Sub DMDSmall3Timeroff_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DMDSmall3Timeroff.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
   'DMDSMALL 2 Is Off
    DMDSmall3.visible = 0
  End Sub



Sub DMDSmall3Timeron_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DMDSmall3Timeron.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
   'DMDSMALL 2 Is On
    DMDSmall3.visible = 1
  End Sub



Sub DMDSmallHMBLitTimerOff_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DMDSmallHMBLitTimerOff.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
   'DMDSMALL 2 Is Off
    DMDSmall3.visible = 0
  End Sub



Sub DMDSmallHMBLitTimeron_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DMDSmallHMBLitTimeron.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
   'DMDSMALL 2 Is On
    DMDSmall3.visible = 1
  End Sub


'***************************************************************
'   HYDE MULTIBALL 1 ANIMATION AND JACKPOTS AND SUPER JACKPOT
'***************************************************************

'*************************
'Hyde UP/DOWN Animation
'*************************

Dim HydePos, HydeDir, HydeShakePos, HydeShakeDir, HydeHitPos, HydeHits
Dim bHydeUp, bPlayfieldHyde

HydePos = 160
HydeShakePos = -160

Sub HydeLocation(param)
    Select Case param
        Case 1:Hyde.X = 213.2222:Hyde.Y = 1150:HydeTrigger1.Enabled = 1:HydeTrigger2.Enabled = 0:HydeTrigger3.Enabled = 0
        Case 2:Hyde.X = 586:Hyde.Y = 939.:HydeTrigger2.Enabled = 1:HydeTrigger1.Enabled = 0:HydeTrigger3.Enabled = 0
        Case 3:Hyde.X = 503:Hyde.Y = 569.:HydeTrigger3.Enabled = 1:HydeTrigger1.Enabled = 0:HydeTrigger2.Enabled = 0
    End Select
End Sub

Sub HydeAnimTimer_Timer()
    HydeShakeTimer.Enabled = 0
    HydeLocationTimer.Enabled = 0
    HydePos = HydePos + HydeDir
    'Hyde is moving up
    If HydePos >= 160 Then
        DOF 127, DOFOff
        'Me.Enabled = 0
        HydePos = 160
        HydeShakeDir = 1
        HydeShakeTimer.Enabled = 1
        HydeLocationTimer.Enabled = 1
        bHydeUp = True
    End If
    'Hyde is moving down
    If HydePos <= -160 Then
        DOF 127, DOFOff
        'Me.Enabled = 0
        HydePos = -160
        HydeTrigger1.Enabled = 0
        HydeTrigger2.Enabled = 0
		HydeTrigger3.Enabled = 0
     End If
    Hyde.Transz = HydePos
End Sub

Sub HydeShakeTimer_Timer
    HydeShakePos = HydeShakePos + HydeShakeDir
    'Hyde is moving up
    If HydeShakePos > 10 Then
        HydeShakeDir = -1
    End If
    'Hyde is moving down
    If HydeShakePos < 0 Then
        HydeShakeDir = 1
    End If
    Hyde.Transz = HydeShakePos
End Sub

Sub HydeMoveUp()
    PlaySound "gb_slimer2"
    PlaySound "vo_slimer1"
    HydeLocation INT(RND * 3 + 1)
    HydeDir = 2
    HydeAnimTimer.Enabled = 1
    DOF 127, DOFOn
    Hyde.X = 213.2222
    Hyde.Y = 1150
End Sub

Sub HydeMoveDown()
    HydeDir = -2
    HydeAnimTimer.Enabled = 1
    bHydeUp = False
    DOF 127, DOFOn
    Hyde.X = 213.2222
    Hyde.Y = 1150
End Sub


Sub HydeHitTimer_Timer()
    Hyde.TransX = HydeHitPos
    If HydeHitPos <= 0.1 AND HydeHitPos >= -0.1 Then :Exit Sub
    If HydeHitPos < 0 Then
        HydeHitPos = ABS(HydeHitPos)- 0.1
    Else
        HydeHitPos = - HydeHitPos + 0.1
    End If
End Sub

Sub HydeLocationTimer_Timer
    HydeLocation INT(RND * 3 + 1)
End Sub

Sub HydePlayfield() 'lits 2 lights
    If RND < 0.5 Then
        PlaySound "vo_slimeus2"
    Else
        PlaySound "vo_slimeus"
    End If
    bPlayfieldHyde = True
    'SetLightColor l61, "green", 2
    'SetLightColor l63, "green", 2
End Sub


Sub HydeTrigger1_Hit()
        HydeHitPos = 6:HydeHitTimer.Enabled = 1
        DOF 139, DOFOn
' Kill Hyde 2 JACKPOT during Multiball (if ballsonpf = 2):
if ballsonplayfield = 4 then
addscore 25000
PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
playsound "Grawl001"
StartAttackMBLightsJP
DMDFLush
DMD "WheresTheHydeJP.wmv", "", "", 3000
'Hyde.X = 403:Hyde.Y = 429.5
end if
if ballsonplayfield = 3 then
addscore 25000
PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
playsound "Hydehit"
StartAttackMBLightsJP
DMDFLush
DMD "WheresTheHydeJP.wmv", "", "", 3000
'Hyde.X = 403:Hyde.Y = 429.5
end if
if ballsonplayfield = 2 then
addscore 25000
PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
playsound "Hydehit"
StartAttackMBLightsJP
DMDFLush
DMD "WheresTheHydeJP.wmv", "", "", 3000
'Hyde.X = 403:Hyde.Y = 429.5
end if
end sub



Sub HydeTrigger2_Hit()
        HydeHitPos = 6:HydeHitTimer.Enabled = 1
        DOF 139, DOFOn
' Kill Hyde 2 JACKPOT during Multiball (if ballsonpf = 2):
if ballsonplayfield = 4 then
addscore 25000
PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
playsound "Hydehit"
StartAttackMBLightsJP
DMDFLush
DMD "WheresTheHydeJP.wmv", "", "", 3000
'Hyde.X = 403:Hyde.Y = 429.5
end if
if ballsonplayfield = 3 then
addscore 25000
PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
playsound "Grawl001"
StartAttackMBLightsJP
DMDFLush
DMD "WheresTheHydeJP.wmv", "", "", 3000
'Hyde.X = 403:Hyde.Y = 429.5
end if
if ballsonplayfield = 2 then
addscore 25000
PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
playsound "Grawl002"
StartAttackMBLightsJP
DMDFLush
DMD "WheresTheHydeJP.wmv", "", "", 3000
'Hyde.X = 403:Hyde.Y = 429.5
end if
end sub



Sub HydeTrigger3_Hit()
        HydeHitPos = 6:HydeHitTimer.Enabled = 1
        DOF 139, DOFOn
' Kill Hyde 2 JACKPOT during Multiball (if ballsonpf = 2):
if ballsonplayfield = 4 then
addscore 25000
PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
playsound "Grawl001"
StartAttackMBLightsJP
DMDFLush
DMD "WheresTheHydeJP.wmv", "", "", 3000
'Hyde.X = 403:Hyde.Y = 429.5
end if
if ballsonplayfield = 3 then
addscore 25000
PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
playsound "Hydehit"
StartAttackMBLightsJP
DMDFLush
DMD "WheresTheHydeJP.wmv", "", "", 3000
'Hyde.X = 403:Hyde.Y = 429.5
end if
if ballsonplayfield = 2 then
addscore 25000
PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
playsound "Grawl002"
StartAttackMBLightsJP
DMDFLush
DMD "WheresTheHydeJP.wmv", "", "", 3000
'Hyde.X = 403:Hyde.Y = 429.5
end if
end sub



Sub HydeSuperJackpotTrigger2_Hit()
        HydeHitPos = 6:HydeHitTimer.Enabled = 1
        DOF 139, DOFOn
' Kill Hyde 2 JACKPOT during Multiball (if ballsonpf = ):
'if ballsonplayfield = 3 then
HydeSuperJackpotTrigger2.Enabled = False
HSJPTimer.Enabled = False
addscore 25000
Stophydespin
StartLOGOAni
WTHAllSmallDMDsOff
PuPlayer.playlistplayex pCallouts,"audiocallouts","HydeEscapedModeCleared",calloutvol,1
playsound "Superjackpot"
HYDEMB2Lit.Duration 0, 0, 0
HYDEMB2Done.state = 1
APGlow.state = 0
Hyde2Timer.Enabled = True 
Hyde2offTimer.Enabled = True
MBBlocker.Isdropped = 1
MBBlocker001.Isdropped = 1
MBBlocker002.Isdropped = 1
MBBlockerTarget.Enabled = false
MBBlocker001Target.Enabled = false
MBBlocker002Target.Enabled = false
MBBlockerTimer.Enabled = False 
MBBlockerTimer001.Enabled = False 
MBBlockerTimer002.Enabled = False
MBBlockerlight001.state = 0
MBBlockerlight002.state = 0
MBBlockerlight003.state = 0
MBBlockerlight004.state = 0
MBBlockerlight005.state = 0
GIOFFWIZARDTrigger001.Enabled = False
GIOFFWIZARDTrigger.Enabled = false
GIOFFWIZARDTrigger.Enabled = False
GIOFFWIZARDTrigger001.Enabled = false
GIOFFWIZARDTrigger002.Enabled = false
GIOFFWIZARDTrigger003.Enabled = false
GIOFFWIZARDTrigger004.Enabled = false
NightShadeGIOff
EnidMBLightSeq.StopPlay
trigger17.Enabled = True
playsound "Hydehit"
StartAttackMBLightsJP
DMDFLush
DMD "dmd baseBlank.png", "", "", 300
DMD "HydeEscaped.wmv", "", "", 3800
PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
HydeSuperJackpotTrigger2.Enabled = False
Stop_Fog
GIOFFWIZARDTrigger003.Enabled = False
playsound "ModeCleared"
'RightRampTarget.Enabled = 0
FreeGomezLitVideoTimer.Enabled = True
WheresTheHydeMBFlasher.opacity = 0
'SingleModeKicker.Enabled = 1
'SingleModeMagnet.MagnetOn = True
'vpmTimer.AddTimer 5, " SingleModeMagnet.MagnetOn = False '"
'vpmTimer.AddTimer 35, " SingleModeMagnet.MagnetOn = False '"
'vpmTimer.AddTimer 40, " SingleModeMagnet.MagnetOn = False '"
'vpmTimer.AddTimer 45, " SingleModeMagnet.MagnetOn = False '"
GIOFFWIZARDTrigger.Enabled = False
GIOFFWIZARDTrigger001.Enabled = False
GIOFFWIZARDTrigger002.Enabled = False
GIOFFWIZARDTrigger003.Enabled = False
GIOFFWIZARDTrigger004.Enabled = False
'RightInLaneTrigger.Enabled = True
'LeftInLaneTrigger.Enabled = True
'VideoQuoteResteTimer004.Enabled = True
quotetrigger.Enabled = false
quotetrigger001.Enabled = False
VideoQuoteResteTimer.Enabled = False
VideoQuoteResteTimer001.Enabled = False
VideoQuoteResteTimer002.Enabled = False
VideoQuoteResteTimer003.Enabled = False
VideoQuoteResteTimer004.Enabled = False
VideoQuoteResteTimer005.Enabled = False
quotetrigger002.Enabled = False
AudioAttractTimer001.Enabled = False
JackpotBlockers001.Isdropped = 0
JackpotBlockers002.Isdropped = 0
SPJTargetTimer.Enabled = True
NightHsadeSJPLight.state = 0
HydeTimeoutTimer.Enabled = False
SaveGomezModeFailed.Enabled = True
SingleModeKillerTrigger.Enabled = True
SingleModeKillerKicker001.Enabled = 1
SingleModeKillerKicker009.Enabled = 1
'MBBlocker5.Isdropped = 0
'SJPKicker.Enabled = 1
SJPBlockerTimer.Enabled = True
End Sub



'**********************************************************************************
'                             HYDE 1 MULTIBALL TIMERS
'**********************************************************************************
'Killer
Sub Hyde2Timer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    Hyde2Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	HYDEMB2Lit.state = 0
	HydeSuperJackpotTrigger2.Enabled = False
    HSJPTimer.Enabled = False
    HydeMoveDown
	'Stop_Fog
    HYDEMB2Lit.state = 0
    APGlow.state = 0
    WheresTheHydeMBFlasher.opacity = 0
   End Sub


Sub Hyde2offTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    Hyde2offTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Hyde2Timer.Enabled = False
    HydeSuperJackpotTrigger2.Enabled = False
    HydeAnimTimer.Enabled = 0
    HydeShakeTimer.Enabled = 0
    HydeLocationTimer.Enabled = 0
	HydeSuperJackpotTrigger2.Enabled = False
    bHydeUp = False
    HYDEMB2Lit.state = 0
    FireBallDMDTrigger.Enabled = True
    MBBlocker.Isdropped = 1
    MBBlocker001.Isdropped = 1
    MBBlocker002.Isdropped = 1
    MBBlockerTarget.Enabled = false
    MBBlocker001Target.Enabled = false
    MBBlocker002Target.Enabled = false
    MBBlockerTimer.Enabled = False 
    MBBlockerTimer001.Enabled = False 
    MBBlockerTimer002.Enabled = False
    MBBlockerlight001.state = 0
    MBBlockerlight002.state = 0
    MBBlockerlight003.state = 0
    MBBlockerlight004.state = 0
    MBBlockerlight005.state = 0
    GIOFFWIZARDTrigger.Enabled = False
    GIOFFWIZARDTrigger001.Enabled = false
    GIOFFWIZARDTrigger002.Enabled = false
    GIOFFWIZARDTrigger003.Enabled = false
    GIOFFWIZARDTrigger004.Enabled = false
    NightShadeGIOff
    EnidMBLightSeq.StopPlay
    WheresTheHydeMBFlasher.opacity = 0
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    DiscoStrobe.StopPlay
    Stophydespin
    StartLOGOAni
    SingleModeKillerTrigger.Enabled = False
End Sub

Sub Hyde3Timer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    Hyde3Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Hyde2Timer.Enabled = True
	Hyde2offTimer.Enabled = True
   End Sub



'This Is the main time. This timer goes off first and starts the MB kill Sequ***********
Sub HydeTimeoutTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    HydeTimeoutTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Hyde2Timer.Enabled = True
	Hyde2offTimer.Enabled = True
    APGlow.state = 0
    MBBlocker.Isdropped = 1
    MBBlocker001.Isdropped = 1
    MBBlocker002.Isdropped = 1
    MBBlockerTarget.Enabled = false
    MBBlocker001Target.Enabled = false
    MBBlocker002Target.Enabled = false
    FireBallDMDTrigger.Enabled = True
    MBBlocker.Isdropped = 1
    MBBlocker001.Isdropped = 1
    MBBlocker002.Isdropped = 1
    MBBlockerTarget.Enabled = false
    MBBlocker001Target.Enabled = false
    MBBlocker002Target.Enabled = false
    MBBlockerTimer.Enabled = False 
    MBBlockerTimer001.Enabled = False 
    MBBlockerTimer002.Enabled = False
    HYDEMB2Lit.state = 0
    MBBlockerlight001.state = 2
    MBBlockerlight002.state = 2
    MBBlockerlight003.state = 2
    MBBlockerlight004.state = 2
    MBBlockerlight005.state = 2
    GIOFFWIZARDTrigger001.Enabled = false
    GIOFFWIZARDTrigger002.Enabled = false
    GIOFFWIZARDTrigger003.Enabled = false
    GIOFFWIZARDTrigger004.Enabled = false
    EnidMBLightSeq.StopPlay
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    Stophydespin
    StartLOGOAni
    NightShadeGIOff
    WTHAllSmallDMDsOff
    NightHsadeSJPLight.state = 0
    WheresTheHydeMBFlasher.opacity = 0
    GiOn
    quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = True
    AudioAttractTimer001.Enabled = False
    QON2.Enabled = True
   End Sub


Sub HSJPTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    HSJPTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	HydeSuperJackpotTrigger2.Enabled = True
    HYDEMB2Lit.state = 2
    NightHsadeSJPLight.state = 2
    DMDFLush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "WheresTheHydeSJP.wmv", "", "", 3000
    WizardSaverKicker.Enabled = False
    Drain.Enabled = 1
    playsound "Jackpotlit" 
    PuPlayer.playlistplayex pCallouts,"AudioSuperJackpots","",calloutvol,1
    GIOFFWIZARDTrigger001.Enabled = True
    GIOFFWIZARDTrigger002.Enabled = True
    GIOFFWIZARDTrigger003.Enabled = True
    GIOFFWIZARDTrigger004.Enabled = True
    MBBlocker003.Isdropped = 1
    HydeTrigger1.Enabled = 0
    HydeTrigger2.Enabled = 0
	HydeTrigger3.Enabled = 0
   End Sub


Sub Hyde2DrainTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    Hyde2DrainTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	HydeMoveDown
    magickicker.Enabled = True
    HYDEMB2Lit.state = 0
    WheresTheHydeMBFlasher.opacity = 0
    hyde2shutoff.Enabled = True
    NightShadeGIOff
End Sub


Sub hyde2shutoff_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    hyde2shutoff.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    HydeShakeTimer.Enabled = 0
    HydeLocationTimer.Enabled = 0
	HydeSuperJackpotTrigger2.Enabled = False
    bHydeUp = False
 end sub


Sub StartWheresHMB_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    StartWheresHMB.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	HydeTimeoutTimer.Enabled = True
    HSJPTimer.Enabled = True
    Starthydespin
    GraveStoneSuperTarget001.Isdropped = 1
    GraveStoneSuperTarget002.Isdropped = 1
    GraveStoneSuperTarget003.Isdropped = 1
    grave001tragertimer002.Enabled = False
    grave001tragertimer001.Enabled = False
    grave001tragertimer.Enabled = False
    WizardSaverKicker.Enabled = True
    Drain.Enabled = 0
    PuPlayer.playlistplayex pBackglass,"videoFoundTheHydeMB","FoundTheHydeMB.mp4",90,1
    PuPlayer.playlistplayex pMusic,"audiomodes","FoundTheHydeMB.mp3",0,1
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
    APGlow.state = 2
    FireBallDMDTrigger.Enabled = False
    quotetrigger.Enabled = false
    quotetrigger001.Enabled = False
    quotetrigger002.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    trigger17.Enabled = False
    MBBlockerTarget.Enabled = True
    MBBlocker001Target.Enabled = True
    MBBlocker002Target.Enabled = True
    MBBlocker.Isdropped = 0
    MBBlocker001.Isdropped = 0
    MBBlocker002.Isdropped = 0
    MBBlocker003.Isdropped = 0
    MBBlockerTimer.Enabled = False 
    MBBlockerTimer001.Enabled = False 
    MBBlockerTimer002.Enabled = False
    GIOFFWIZARDTrigger.Enabled = True
    GIOFFWIZARDTrigger001.Enabled = True
    GIOFFWIZARDTrigger002.Enabled = True
    GIOFFWIZARDTrigger003.Enabled = True
    GIOFFWIZARDTrigger004.Enabled = True
    NightShadeGIOn
    magickicker.Enabled = False
    totherampSkill003.Enabled = 0
    RightInLaneTrigger.Enabled = False
    LeftInLaneTrigger.Enabled = False
End Sub


Sub Hyde2Timerattract_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    Hyde2Timerattract.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	HydeMoveDown
End Sub


Sub Hyde2offattractTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    Hyde2offattractTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    HydeAnimTimer.Enabled = 0
    HydeShakeTimer.Enabled = 0
    HydeLocationTimer.Enabled = 0
	bHydeUp = False
    End Sub

Sub hyde2SJPDrainTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    hyde2SJPDrainTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	HydeSuperJackpotTrigger2.Enabled = False
    HSJPTimer.Enabled = False
    StartWheresHMB.Enabled = False
    HYDEMB2Lit.state = 0
    NightHsadeSJPLight.state = 0
   End Sub


'*********************************************************************************
'Where's The Hyde MB Small DMD Timers
'*********************************************************************************

Sub WTHSmallDMDOn_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WTHSmallDMDOn.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    FoundHydeSDMD.visible = 0
    Shoot4HydeSDMD.visible = 1
    FoundHydeSDMD001.visible = 0
    Shoot4HydeSDMD001.visible = 1
    WTHSmallDMDOff.Enabled = True
  End Sub


Sub WTHSmallDMDOff_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WTHSmallDMDOff.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Shoot4HydeSDMD.visible = 0
    SJPWhenLitSDMD.visible = 1
    Shoot4HydeSDMD001.visible = 0
    SJPWhenLitSDMD001.visible = 1
    WTHSmallDMD002.Enabled = True
  End Sub


Sub WTHSmallDMD002_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WTHSmallDMD002.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SJPWhenLitSDMD.visible = 0
    FoundHydeSDMD.visible = 1
    SJPWhenLitSDMD001.visible = 0
    FoundHydeSDMD001.visible = 1
    WTHSmallDMDOn.Enabled = True
  End Sub


Sub WTHAllSmallDMDsOff() 'in timer options look at the interval, its 2500 2 1/2 seconds
    FoundHydeSDMD.visible = 0
    Shoot4HydeSDMD.visible = 0
    SJPWhenLitSDMD.visible = 0
    FoundHydeSDMD001.visible = 0
    Shoot4HydeSDMD001.visible = 0
    SJPWhenLitSDMD001.visible = 0
    DMDSmall3.visible = 1
    WTHSmallDMDOn.Enabled = False
    WTHSmallDMDOff.Enabled = False
    WTHSmallDMD002.Enabled = False
End Sub



'*************************************************************
'     SuperKicker This Kills all SuperJackpot Triggers
'*************************************************************

Sub WizardKickerTimer_Timer
WizardKicker.Kick 2, 30
WizardKickerTimer.Enabled = 0
End Sub

Sub WizardKicker_Hit()
WizardKickerTimer.Enabled = 1
PlaySound "sfx_thunder7"
end sub




'************************
'   DISCO lights & Modes
'************************
Sub StartDisco
    totherampSkill003.Enabled = 0
    discoball.visible = 1
    DiscoDanceMBFlasher.opacity = 95
    DiscoPartyMBFlasherSwitchTimer.Enabled = True
    discolight.visible = 1
    DiscoTimer.Enabled = 1
    StartDiscoLightSeq
    DiscoJackpotTrigger001.Enabled = True
    PuPlayer.playlistplayex pCallouts,"audiocallouts","DiscoMB.mp3",calloutvol,1
    quotetrigger.Enabled = false
    quotetrigger001.Enabled = False
    quotetrigger002.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    MBBlocker003.Isdropped = 0
    MBBlocker.Isdropped = 0
    MBBlocker001.Isdropped = 0
    MBBlocker002.Isdropped = 0
    MBBlockerTarget.Enabled = True
    MBBlocker001Target.Enabled = True
    MBBlocker002Target.Enabled = True
    MBBlockerTimer.Enabled = False
    MBBlockerTimer001.Enabled = False
    MBBlockerTimer002.Enabled = False
    trigger17.Enabled = False
    MBBlockerlight001.state = 2
    MBBlockerlight002.state = 2
    MBBlockerlight003.state = 2
    MBBlockerlight004.state = 2
    MBBlockerlight005.state = 2
    magickicker.Enabled = False
    RightInLaneTrigger.Enabled = False
    LeftInLaneTrigger.Enabled = False
    AudioAttractTimer001.Enabled = False
    StopLOGOAni
  End Sub

Sub StopDisco
    discoball.visible = 0
    discolight.visible = 0
    DiscoTimer.Enabled = 0
    DiscoStrobe.StopPlay
    FireBallDMDTrigger.Enabled = True
    trigger17.Enabled = True
    MBBlocker.Isdropped = 1
    MBBlocker001.Isdropped = 1
    MBBlocker002.Isdropped = 1
    MBBlockerTarget.Enabled = false
    MBBlocker001Target.Enabled = false
    MBBlocker002Target.Enabled = false
    MBBlockerlight001.state = 0
    MBBlockerlight002.state = 0
    MBBlockerlight003.state = 0
    MBBlockerlight004.state = 0
    MBBlockerlight005.state = 0
   DiscoDanceMBFlasher.opacity = 0
   DiscoDanceMBFlasher.opacity = 0
   DiscoDanceMBFlasher2.opacity = 0
   WizardSaverKicker.Enabled = False
   Drain.Enabled = 1
   GIOFFWIZARDTrigger.Enabled = False
   GIOFFWIZARDTrigger001.Enabled = False
   GIOFFWIZARDTrigger002.Enabled = False
   GIOFFWIZARDTrigger003.Enabled = False
   GIOFFWIZARDTrigger004.Enabled = False
   MBBlocker003.Isdropped = 1
   totherampSkill003.Enabled = 0
   NightHsadeSJPLight.state = 0
   RavenDisceAllSmallDMDsOff
   DiscoPartyLit.state = 0
   DiscoSuperJackpotTrigger.Enabled = False  
End Sub

Sub DiscoTimer_Timer
    discoball.rotz = (discoball.rotz + 1)mod 360
    discolight.rotz = (discolight.rotz + 1)mod 360
End Sub


'************************
'   DISCOAttract lights
'************************
Sub StartDiscoAttract
    discoball.visible = 1
    discolight.visible = 1
    DiscoAttractTimer.Enabled = 1
    DiscoBallLightGlow.State=1
    StopLOGOAni
 End Sub

Sub StopDiscoAttract
    discolight.visible = 0
    discoball.visible = 0
    DiscoAttractTimer.Enabled = 0
    DiscoBallLightGlow.State=0
    StartLOGOAni
End Sub

Sub DiscoAttractTimer_Timer
    discoball.rotz = (discoball.rotz + 1)mod 360
    discolight.rotz = (discolight.rotz + 1)mod 360
End Sub



'************************
'DISCO lightsStrobeEff
'************************

Sub StartDiscoLightSeq()
    'Disco lights sequences
    DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 25
	DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 25
	DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 25
	DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 25
	DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 1
    DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 25
	DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 25
	DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 25
	DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 25
	DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 1
    DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 25
	DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 25
	DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 25
	DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 25
	DiscoStrobe.UpdateInterval = 25
    DiscoStrobe.Play SeqBlinking, , 5, 150
    DiscoStrobe.Play SeqRandom, 40, , 4000
    DiscoStrobe.Play SeqAllOff
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqUpOn, 50, 1
    DiscoStrobe.UpdateInterval = 8
    DiscoStrobe.Play SeqDownOn, 25, 1
    DiscoStrobe.UpdateInterval = 8
	DiscoStrobe.Play SeqCircleOutOn, 15, 2
    DiscoStrobe.UpdateInterval = 1
End Sub


'*******************************
'DISCO Ramp Spot lightsStrobeEff
'*******************************

Sub StartEnidSpotLightSeq()
    'Disco lights sequences
    EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 25
	EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 25
	EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 25
	EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 25
	EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 1
    EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 25
	EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 25
	EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 25
	EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 25
	EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 1
    EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 25
	EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 25
	EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 25
	EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 25
	EnidMBLightSeq.UpdateInterval = 25
    EnidMBLightSeq.Play SeqBlinking, , 5, 150
    EnidMBLightSeq.Play SeqRandom, 40, , 4000
    EnidMBLightSeq.Play SeqAllOff
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqUpOn, 50, 1
    EnidMBLightSeq.UpdateInterval = 8
    EnidMBLightSeq.Play SeqDownOn, 25, 1
    EnidMBLightSeq.UpdateInterval = 8
	EnidMBLightSeq.Play SeqCircleOutOn, 15, 2
    EnidMBLightSeq.UpdateInterval = 1
End Sub

'EnidMBLightSeq.StopPlay



Sub DiscoPartyMBFlasherSwitchTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DiscoPartyMBFlasherSwitchTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    DiscoDanceMBFlasher.opacity = 0
    DiscoDanceMBFlasher2.opacity = 95
End Sub

'*************************************************************
'		Dicso Jackpots and Super Jackpot
'*************************************************************
Sub DiscoJackpotTrigger001_Hit
    addscore 25000
	DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "4MoretoGo.wmv", "", "", 2800
    StartAttackMBLightsJP
    PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
    DiscoJackpotTrigger001Timer.Enabled = True
    DiscoJackpotTrigger001.Enabled = False 
 End Sub

Sub DiscoJackpotTrigger001Timer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DiscoJackpotTrigger001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	DiscoJackpotTrigger001.Enabled = False 
    DiscoJackpotTrigger002.Enabled = True
    
 End Sub

Sub DiscoJackpotTrigger002_Hit
    addscore 25000
	DMDFlush
    DMD "3MoretoGo.wmv", "", "", 3800
    StartAttackMBLightsJP
    PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
   DiscoJackpotTrigger002Timer002.Enabled = True
   GraveStoneSuperTarget001.Isdropped = 0
   GraveStoneSuperTarget002.Isdropped = 0
   GraveStoneSuperTarget003.Isdropped = 0
   SuperTargetsOffTimer.Enabled = True 
   SuperTargetsOffTimer001.Enabled = True  
   DiscoJackpotTrigger002.Enabled = False 
End Sub

Sub DiscoJackpotTrigger002Timer002_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DiscoJackpotTrigger002Timer002.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	DiscoJackpotTrigger002.Enabled = False 
    DiscoJackpotTrigger003.Enabled = True
 End Sub

Sub DiscoJackpotTrigger003_Hit
    addscore 25000
	DMDFlush
    DMD "dmd baseBlank.png", "", "", 100
    DMD "2MoretoGo.wmv", "", "", 3800
    StartAttackMBLightsJP
    PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
    DiscoJackpotTrigger003Timer003.Enabled = True 
    DiscoJackpotTrigger003.Enabled = False
End Sub

Sub DiscoJackpotTrigger003Timer003_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DiscoJackpotTrigger003Timer003.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	DiscoJackpotTrigger003.Enabled = False
    DiscoJackpotTrigger004.Enabled = True 
 End Sub

Sub DiscoJackpotTrigger004_Hit
    addscore 25000
	DMDFlush
    DMD "dmd baseBlank.png", "", "", 100
    DMD "1MoretoGo.wmv", "", "", 3800
    StartAttackMBLightsJP
    PuPlayer.playlistplayex pCallouts,"AudioJackpots","",calloutvol,0
    DiscoJackpotTrigger004Timer004.Enabled = True
    GraveStoneSuperTarget001.Isdropped = 0
    GraveStoneSuperTarget002.Isdropped = 0
    GraveStoneSuperTarget003.Isdropped = 0
    SuperTargetsOffTimer.Enabled = True 
    SuperTargetsOffTimer001.Enabled = True 
    DiscoJackpotTrigger004.Enabled = False 
End Sub

Sub DiscoJackpotTrigger004Timer004_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DiscoJackpotTrigger004Timer004.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	DiscoJackpotTrigger004.Enabled = False
    DiscoJackpotTrigger005.Enabled = True 
 End Sub

Sub DiscoJackpotTrigger005_Hit
    addscore 25000
	DMDFlush
    DMD "dmd baseBlank.png", "", "", 100
    DMD "DiscoGetSJP.wmv", "", "", 3800
    StartAttackMBLightsJP
    PuPlayer.playlistplayex pCallouts,"AudioSuperJackpots","",calloutvol,1
    DiscoJackpotTrigger005Timer005.Enabled = True
    DiscoPartyLit.state = 2
    NightHsadeSJPLight.state = 2
    DiscoPartySuperJPTimer.Enabled = True
    DiscoJackpotTrigger005.Enabled = False  
    playsound "Jackpotlit"
End Sub

Sub DiscoJackpotTrigger005Timer005_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DiscoJackpotTrigger005Timer005.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	DiscoJackpotTrigger005.Enabled = False
    DiscoSuperJackpotTrigger.Enabled = True 
End Sub

Sub DiscoSuperJackpotTrigger_Hit
    addscore 25000
    playsound "Superjackpot"
    DMD "dmd baseBlank.png", "", "", 100
	DMD "dmd baseBlank.png", "SUPER JACKPOT", "", 2000
    StartAttackMBLightsJP
    PuPlayer.playlistplayex pCallouts,"audiocallouts","DiscoCleared.mp3",calloutvol,1
    DiscoSuperJackpotTrigger.Enabled = False 
    DiscoJackpotTrigger005.Enabled = False 
    DiscoJackpotTrigger004.Enabled = False 
    DiscoJackpotTrigger003.Enabled = False 
    DiscoJackpotTrigger002.Enabled = False 
    DiscoJackpotTrigger001.Enabled = False 
    DiscoDanceMBFlasher.opacity = 0
    DiscoDanceMBFlasher2.opacity = 0
    DiscoPartyMBFlasherSwitchTimer.Enabled = False
    magickicker.Enabled = False
    DiscoPartyLit.state = 0
    NightHsadeSJPLight.state = 0
    DiscoPartyDone.state = 1
    GiOn
    APGlow.state = 0
    EugeneLitVideoTimer.Enabled = True
    'SingleModeKicker.Enabled = 1
    'SingleModeMagnet.MagnetOn = True
    'vpmTimer.AddTimer 5, " SingleModeMagnet.MagnetOn = False '"
    'vpmTimer.AddTimer 35, " SingleModeMagnet.MagnetOn = False '"
    'vpmTimer.AddTimer 40, " SingleModeMagnet.MagnetOn = False '"
    'vpmTimer.AddTimer 45, " SingleModeMagnet.MagnetOn = False '"
    StopDisco
    EnidMBLightSeq.StopPlay
    MBBlocker003.Isdropped = 1
    PuPlayer.playlistplayex pBackglass,"videoDiscoPartyMB","DiscoPartyOver.mp4",videovol,1
    PuPlayer.playlistplayex pAudio,"audiobgrock","",0,0
    PuPlayer.playlistplayex pMusic,"audiomodes","DiscoPartyOver.mp3",0,1
    playsound "ModeCleared"
    WizardSaverKicker.Enabled = False
    Drain.Enabled = 1
    GIOFFWIZARDTrigger.Enabled = False
    GIOFFWIZARDTrigger001.Enabled = False
    GIOFFWIZARDTrigger002.Enabled = False
    GIOFFWIZARDTrigger003.Enabled = False
    GIOFFWIZARDTrigger004.Enabled = False
    DiscoPartyMBTimeoutTimer.Enabled = False
    StartLOGOAni
    quotetrigger.Enabled = false
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False
    JackpotBlockers001.Isdropped = 0
    JackpotBlockers002.Isdropped = 0
    SPJTargetTimer.Enabled = True
    NightHsadeSJPLight.state = 0
    DiscoPartyMBTimeoutTimer.Enabled = False 
    SingleModeKillerTrigger.Enabled = True
    SingleModeKillerKicker001.Enabled = 1
    SingleModeKillerKicker009.Enabled = 1
    'MBBlocker5.Isdropped = 0
    'SJPKicker.Enabled = 1
    SJPBlockerTimer.Enabled = True
End Sub

Sub DiscoPartySuperJPTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DiscoPartySuperJPTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	DiscoPartyLit.state = 0
    NightHsadeSJPLight.state = 0
    DiscoSuperJackpotTrigger.Enabled = False 
    DiscoJackpotTrigger005.Enabled = False 
    DiscoJackpotTrigger004.Enabled = False 
    DiscoJackpotTrigger003.Enabled = False 
    DiscoJackpotTrigger002.Enabled = False 
    DiscoJackpotTrigger001.Enabled = False
    APGlow.state = 0
    StopDisco
    WizardSaverKicker.Enabled = False
    Drain.Enabled = 1
    GIOFFWIZARDTrigger.Enabled = False
    GIOFFWIZARDTrigger001.Enabled = False
    GIOFFWIZARDTrigger002.Enabled = False
    GIOFFWIZARDTrigger003.Enabled = False
    GIOFFWIZARDTrigger004.Enabled = False
End Sub


Sub DiscoPartyMBTimeoutTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DiscoPartyMBTimeoutTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	DiscoPartyLit.state = 0
    NightHsadeSJPLight.state = 0
    DiscoSuperJackpotTrigger.Enabled = False 
    DiscoJackpotTrigger005.Enabled = False 
    DiscoJackpotTrigger004.Enabled = False 
    DiscoJackpotTrigger003.Enabled = False 
    DiscoJackpotTrigger002.Enabled = False 
    DiscoJackpotTrigger001.Enabled = False
    DiscoDanceMBFlasher.opacity = 0
    DiscoDanceMBFlasher2.opacity = 0
    DiscoPartyMBFlasherSwitchTimer.Enabled = False
    APGlow.state = 0
    GiOn
    StopDisco
    magickicker.Enabled = True
    EnidMBLightSeq.StopPlay
    WizardSaverKicker.Enabled = False
    Drain.Enabled = 1
    GIOFFWIZARDTrigger.Enabled = False
    GIOFFWIZARDTrigger001.Enabled = False
    GIOFFWIZARDTrigger002.Enabled = False
    GIOFFWIZARDTrigger003.Enabled = False
    GIOFFWIZARDTrigger004.Enabled = False
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    ramptrigger.Enabled = True
    Trigger2.Enabled = True
    DiscoJackpotTrigger005.Enabled = True
    quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = True
    AudioAttractTimer001.Enabled = False
    QON2.Enabled = True
    SingleModeKillerTrigger.Enabled = false
End Sub

Sub DiscoPartyMBDrain_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    DiscoPartyMBDrain.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	DiscoPartyLit.state = 0
    NightHsadeSJPLight.state = 0
    DiscoSuperJackpotTrigger.Enabled = False 
    DiscoJackpotTrigger005.Enabled = False 
    DiscoJackpotTrigger004.Enabled = False 
    DiscoJackpotTrigger003.Enabled = False 
    DiscoJackpotTrigger002.Enabled = False 
    DiscoJackpotTrigger001.Enabled = False 
    DiscoDanceMBFlasher.opacity = 0
    DiscoDanceMBFlasher2.opacity = 0
    DiscoPartyMBFlasherSwitchTimer.Enabled = False
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    ramptrigger.Enabled = True
    Trigger2.Enabled = True
    StopDisco
    EnidMBLightSeq.StopPlay
    APGlow.state = 0
End Sub

'*********************************************************************************
'Rav-N Disco MB Small DMD Timers
'*********************************************************************************

Sub RavenDiscemallDMDOn_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    RavenDiscemallDMDOn.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    RavenDiscoSDMD.visible = 0
    RavenOrbitsSDMD.visible = 1
    RavenDiscemallDMDOff.Enabled = True
    RavenDiscoSDMD001.visible = 0
    RavenOrbitsSDMD001.visible = 1
  End Sub


Sub RavenDiscemallDMDOff_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    RavenDiscemallDMDOff.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    RavenOrbitsSDMD.visible = 0
    Raven5JPSDMD.visible = 1
    RavenDiscemallDMD001.Enabled = True
    RavenOrbitsSDMD001.visible = 0
    Raven5JPSDMD001.visible = 1
  End Sub


Sub RavenDiscemallDMD001_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    RavenDiscemallDMD001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    Raven5JPSDMD.visible = 0
    RavenSJPSDMD.visible = 1
    RavenDiscemallDMD002.Enabled = True
    Raven5JPSDMD001.visible = 0
    RavenSJPSDMD001.visible = 1
  End Sub


Sub RavenDiscemallDMD002_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    RavenDiscemallDMD002.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    RavenSJPSDMD.visible = 0
    RavenDiscoSDMD.visible = 1
    RavenDiscemallDMDOn.Enabled = True
    RavenSJPSDMD001.visible = 0
    RavenDiscoSDMD001.visible = 1
  End Sub


Sub RavenDisceAllSmallDMDsOff() 'in timer options look at the interval, its 2500 2 1/2 seconds
    RavenDiscoSDMD.visible = 0
    RavenOrbitsSDMD.visible = 0
    Raven5JPSDMD.visible = 0
    RavenSJPSDMD.visible = 0
    RavenDiscoSDMD001.visible = 0
    RavenOrbitsSDMD001.visible = 0
    Raven5JPSDMD001.visible = 0
    RavenSJPSDMD001.visible = 0
    DMDSmall3.visible = 1
    RavenDiscemallDMDOn.Enabled = False
    RavenDiscemallDMDOff.Enabled = False
    RavenDiscemallDMD001.Enabled = False
    RavenDiscemallDMD002.Enabled = False
 End Sub


'*************************************************************


'*************************************************************
'        Free gomez mode targets and timers
'*************************************************************

Sub JailTrigger001_Hit
    JailTrigger001.Enabled = False
    JailLight001.State = 0
    JailTrigger002.Enabled = True
    JailLight002.State = 2
    PlaySound "The Addams Family"
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "FGM4more.png", "", "", 3800
  End Sub


Sub JailTrigger002_Hit
    JailTrigger002.Enabled = False
    JailLight002.State = 0
    JailTrigger003.Enabled = True
    JailLight003.State = 2
    PlaySound "The Addams Family"
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "FGM3more.png", "", "", 3800
  End Sub


Sub JailTrigger003_Hit
    JailTrigger003.Enabled = False
    JailLight003.State = 0
    JailTrigger004.Enabled = True
    JailLight004.State = 2
    PlaySound "The Addams Family"
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "FGM2more.png", "", "", 3800
  End Sub

Sub JailTrigger004_Hit
    JailTrigger004.Enabled = False
    JailLight004.State = 0
    JailTrigger005.Enabled = True
    JailLight005.State = 2
    PlaySound "The Addams Family"
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "FGM1more.png", "", "", 3800
  End Sub


Sub JailTrigger005_Hit
    PlaySound "The Addams Family"
    SaveGomezModeFailed.Enabled = False
    JailTrigger005.Enabled = False
    JailLight005.State = 0
    ramptrigger.Enabled = True
    Trigger2.Enabled = True
    JailKey2.visible = 1
    SingleModeKicker.Enabled = 1
    SingleModeMagnet.MagnetOn = True
    vpmTimer.AddTimer 30, " SingleModeMagnet.MagnetOn = False '"
    vpmTimer.AddTimer 35, " SingleModeMagnet.MagnetOn = False '"
    vpmTimer.AddTimer 40, " SingleModeMagnet.MagnetOn = False '"
    vpmTimer.AddTimer 45, " SingleModeMagnet.MagnetOn = False '"
    SingleModeBlocker001.Isdropped = 0 
    SingleModeBlocker002.Isdropped = 0
    SingleModeBlocker003.Isdropped = 0
    StopJailKeySpin
    StartLOGOAni
    FreeGomezAllSmallDMDsOff
    JailTimer001OFF.Enabled = True
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    JackpotBlockers001.Isdropped = 0
    JackpotBlockers002.Isdropped = 0
    SPJTargetTimer.Enabled = True
    SingleModeKillerTrigger.Enabled = True
    NightShadeGIOff
    SpotlightAttractOff
    PuPlayer.playlistplayex pBackglass,"FreeGomezMode","GomezFreed.mp4",115,1
    PuPlayer.playlistplayex pMusic,"audiomodes","GomezFreed.mp3",0,1
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "FGMSetFree.png", "", "", 3800
  End Sub


Sub JailTimer001On_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    JailTimer001On.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    JailTrigger001.Enabled = True
    JailLight001.State = 2
    StartJailKeySpin
    PuPlayer.playlistplayex pBackglass,"FreeGomezMode","FreeGomezMode.mp4",115,1
    PuPlayer.playlistplayex pMusic,"audiomodes","FreeGomezMode.mp3",0,1
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "FreeGomezMode.png", "", "", 3800
    FreeGomezFlasher.opacity = 95
    ramptrigger.Enabled = False
    Trigger2.Enabled = False
    SingleModeKicker.Enabled = 0
    SingleModeKicker.Kick 2, 14
    SingleModeBlocker001.Isdropped = 0
    SingleModeBlocker002.Isdropped = 0
    SingleModeBlocker003.Isdropped = 0
    Gioff
    quotetrigger.Enabled = false
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False
    NightShadeGIOn
    SpotlightAttractOn
    magickicker.Enabled = True
    RightInLaneTrigger.Enabled = False
    LeftInLaneTrigger.Enabled = False
    DiscoBlocker.Isdropped = 1
    FreeGomezSDMD.visible = 1
    DMDSmall3.visible = 0
    FreeGomezSmallDMDOn.Enabled = True
End Sub


Sub JailTimer001OFF_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    JailTimer001OFF.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    JailTrigger005.Enabled = False
    JailLight005.State = 0
    FreeGomezFlasher.opacity = 0
    SaveGomezModeFailed.Enabled = False
    'ramptrigger.Enabled = True
    SingleModeKicker.Enabled = 0
    SingleModeKicker.Kick 2, 14
    SingleModeBlocker001.Isdropped = 1
    SingleModeBlocker002.Isdropped = 1
    SingleModeBlocker003.Isdropped = 1
    JailTrigger004.Enabled = False
    JailLight004.State = 0
    JailTrigger003.Enabled = False
    JailLight003.State = 0
    JailTrigger002.Enabled = False
    JailLight002.State = 0
    JailTrigger001.Enabled = False
    JailLight001.State = 0
    JailTrigger005.Enabled = False
    JailLight005.State = 0
   quotetrigger.Enabled = True
   quotetrigger001.Enabled = True
   VideoQuoteResteTimer.Enabled = False
   VideoQuoteResteTimer001.Enabled = False
   VideoQuoteResteTimer002.Enabled = False
   VideoQuoteResteTimer003.Enabled = False
   VideoQuoteResteTimer004.Enabled = False
   VideoQuoteResteTimer005.Enabled = False
   quotetrigger002.Enabled = True
   AudioAttractTimer001.Enabled = True
   DiscoBlocker.Isdropped = 1
   StopJailKeySpin
   GiOn
   FreeGomezAllSmallDMDsOff
   Addams1.State=1
   Addams2.State=1
   SingleModeKillerTrigger.Enabled = False
END Sub


Sub SaveGomezModeFailed_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SaveGomezModeFailed.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    JailTrigger005.Enabled = False
    JailLight005.State = 0
    FreeGomezFlasher.opacity = 0
    ramptrigger.Enabled = True
    SingleModeKicker.Enabled = 0
    SingleModeKicker.Kick 2, 14
    SingleModeBlocker001.Isdropped = 1
    SingleModeBlocker002.Isdropped = 1
    SingleModeBlocker003.Isdropped = 1
    JailTrigger004.Enabled = False
    JailLight004.State = 0
    JailTrigger003.Enabled = False
    JailLight003.State = 0
    JailTrigger002.Enabled = False
    JailLight002.State = 0
    JailTrigger001.Enabled = False
    JailLight001.State = 0
    JailTrigger005.Enabled = False
    JailLight005.State = 0
    quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = True
    AudioAttractTimer001.Enabled = False
    QON2.Enabled = True
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    DiscoBlocker.Isdropped = 1
    NightShadeGIOff
    StopJailKeySpin
    GiOn
    FreeGomezAllSmallDMDsOff
    SpotlightAttractOff
 END Sub



Sub FreeGomezLitVideoTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    FreeGomezLitVideoTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    JailTimer001On.Enabled = True
    PuPlayer.playlistplayex pBackglass,"FreeGomezMode","GomezArrested.mp4",115,1
    PuPlayer.playlistplayex pMusic,"audiomodes","GomezArrested.mp3",0,1
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
    quotetrigger.Enabled = false
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False
End Sub

Sub SaveGomezDrain()
    StopJailKeySpin
    JailTrigger005.Enabled = False
    JailLight005.State = 0
    FreeGomezFlasher.opacity = 0
    ramptrigger.Enabled = True
    SingleModeBlocker001.Isdropped = 1
    SingleModeBlocker002.Isdropped = 1
    SingleModeBlocker003.Isdropped = 1
    JailTrigger004.Enabled = False
    JailLight004.State = 0
    JailTrigger003.Enabled = False
    JailLight003.State = 0
    JailTrigger002.Enabled = False
    JailLight002.State = 0
    JailTrigger001.Enabled = False
    JailLight001.State = 0
    JailTrigger005.Enabled = False
    JailLight005.State = 0
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    DiscoBlocker.Isdropped = 1
End Sub



Sub SaveGomesOff()
JailTrigger004.Enabled = False
JailLight004.State = 0
JailTrigger003.Enabled = False
JailLight003.State = 0
JailTrigger002.Enabled = False
JailLight002.State = 0
JailTrigger001.Enabled = False
JailLight001.State = 0
JailTrigger005.Enabled = False
JailLight005.State = 0
SingleModeKicker.Enabled = 0
SingleModeMagnet.MagnetOn = false
end sub


Sub StartJailKeySpin
    JailKeyTimer.Enabled = 1
    JailKey.visible = 1
    discoball.visible = 0
    StopLOGOAni
    FreeGomezSDMD.visible = 1
    FreeGomezSDMD001.visible = 1
End Sub

Sub StopJailKeySpin
    JailKeyTimer.Enabled = 0
    JailKey.visible = 0
End Sub

Sub JailKeyTimer_Timer
    JailKey.rotz = (JailKey.rotz + 1)mod 360
End Sub


'*********************************************************************************
'Free Gomez Small DMD Timers
'*********************************************************************************

Sub FreeGomezSmallDMDOn_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    FreeGomezSmallDMDOn.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    FreeGomezSDMD.visible = 0
    ShootRedSDMD.visible = 1
    FreeGomezSDMD001.visible = 0
    ShootRedSDMD001.visible = 1
    FreeGomezSmallDMDOff.Enabled = True
  End Sub


Sub FreeGomezSmallDMDOff_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    FreeGomezSmallDMDOff.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    ShootRedSDMD.visible = 0
    KeySDMD.visible = 1
    ShootRedSDMD001.visible = 0
    KeySDMD001.visible = 1
    FreeGomezSmallDMD001.Enabled = True
  End Sub


Sub FreeGomezSmallDMD001_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    FreeGomezSmallDMD001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    KeySDMD.visible = 0
    FreeGomezSDMD.visible = 1
    KeySDMD001.visible = 0
    FreeGomezSDMD001.visible = 1
    FreeGomezSmallDMDOn.Enabled = True
  End Sub


Sub FreeGomezAllSmallDMDsOff() 'in timer options look at the interval, its 2500 2 1/2 seconds
    FreeGomezSDMD.visible = 0
    KeySDMD.visible = 0
    ShootRedSDMD.visible = 0
    FreeGomezSDMD001.visible = 0
    KeySDMD001.visible = 0
    ShootRedSDMD001.visible = 0
    DMDSmall3.visible = 1
    FreeGomezSmallDMDOn.Enabled = False
    FreeGomezSmallDMDOff.Enabled = False
    FreeGomezSmallDMD001.Enabled = False
End Sub
'*************************************************************


'*************************************************************
'         Save Eugene mode triggers and timers
'*************************************************************


Sub EugeneTrigger001_Hit
    EugeneTrigger001.Enabled = False
    EugeneMB001.State = 0
    EugeneTrigger002.Enabled = True
    EugeneMB002.State = 2
    PuPlayer.playlistplayex pCallouts,"audiocallouts","EugeneCallout001.mp3",105,1
    PlaySound "EugeneCallout"
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "SEM4more.png", "", "", 3800
  End Sub


Sub EugeneTrigger002_Hit
    EugeneTrigger002.Enabled = False
    EugeneMB002.State = 0
    EugeneTrigger003.Enabled = True
    EugeneMB003.State = 2
    PuPlayer.playlistplayex pCallouts,"audiocallouts","EugeneCallout002.mp3",105,1
    PlaySound "EugeneCallout"
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "SEM3more.png", "", "", 3800
  End Sub


Sub EugeneTrigger003_Hit
    EugeneTrigger003.Enabled = False
    EugeneMB003.State = 0
    EugeneTrigger004.Enabled = True
    EugeneMB004.State = 2
    PuPlayer.playlistplayex pCallouts,"audiocallouts","EugeneCallout001.mp3",105,1
    PlaySound "EugeneCallout"
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "SEM2more.png", "", "", 3800
  End Sub

Sub EugeneTrigger004_Hit
    EugeneTrigger004.Enabled = False
    EugeneMB004.State = 0
    EugeneTrigger005.Enabled = True
    EugeneMB005.State = 2
    PlaySound "EugeneCallout"
    PuPlayer.playlistplayex pCallouts,"audiocallouts","EugeneCallout002.mp3",105,1
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "SEM1more.png", "", "", 3800
  End Sub


Sub EugeneTrigger005_Hit
    EugeneTrigger005.Enabled = False
    EugeneMB005.State = 0
    ramptrigger.Enabled = True
    Glasses2.visible = 1
    SingleModeKicker.Enabled = 1
    SingleModeMagnet.MagnetOn = True
    vpmTimer.AddTimer 30, " SingleModeMagnet.MagnetOn = False '"
    vpmTimer.AddTimer 35, " SingleModeMagnet.MagnetOn = False '"
    vpmTimer.AddTimer 40, " SingleModeMagnet.MagnetOn = False '"
    vpmTimer.AddTimer 45, " SingleModeMagnet.MagnetOn = False '"
    SingleModeBlocker001.Isdropped = 1
    SingleModeBlocker002.Isdropped = 1
    SingleModeBlocker003.Isdropped = 1
    EugeneTimer001Off.Enabled = True
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    Trigger2.Enabled = True
    ramptrigger.Enabled = True
    EugeneDSMSOffTrigger.Enabled = True
    EugeneModeFailed.Enabled = False 
    StopGlassesSpin
    NightShadeGIOff
    SpotlightAttractOff
    StartLOGOAni
    PuPlayer.playlistplayex pBackglass,"SaveEugeneMode","EugeneSaved.mp4",115,1
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "SEMSaved.png", "", "", 3800
    PlaySound "EugeneCallout"
    PuPlayer.playlistplayex pCallouts,"audiocallouts","EugeneCallout001.mp3",105,1
    JackpotBlockers001.Isdropped = 0
    JackpotBlockers002.Isdropped = 0
    SPJTargetTimer.Enabled = True
    SaveEugeneSDMD.opacity = 0
    GlassesSDMD.opacity = 0
    ShootRedSDMD.opacity = 0
    DMDSmall3.visible = 1
    SaveEugeneSmallDMDOn.Enabled = False
    SaveEugeneSmallDMDOff.Enabled = False
    SaveEugeneSmallDMD001.Enabled = False
    SaveEugeneSmallDMDOn.Enabled = 0
    SaveEugeneSmallDMDOff.Enabled = 0
    SaveEugeneSmallDMD001.Enabled = 0
    SaveEugeneSDMD001.visible = 0
    SingleModeKillerTrigger.Enabled = True
 End Sub




Sub EugeneTimer001On_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EugeneTimer001On.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    EugeneTrigger001.Enabled = True
    EugeneMB001.State = 2
    StartGlassesSpin
    RightInLaneTrigger.Enabled = False
    LeftInLaneTrigger.Enabled = False
    Trigger2.Enabled = False
    ramptrigger.Enabled = False
    SingleModeKicker.Enabled = 0
    SingleModeKicker.Kick 2, 14
    SingleModeBlocker001.Isdropped = 0
    SingleModeBlocker002.Isdropped = 0
    SingleModeBlocker003.Isdropped = 0
    multiballholeblockerTimer.Enabled = True
    quotetrigger.Enabled = false
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False
    SaveEugeneFlasher.opacity = 95
    RightInLaneTrigger.Enabled = False
    LeftInLaneTrigger.Enabled = False
    NightShadeGIOn
    SpotlightAttractOn
    GiOff
    SaveEugeneSmallDMDOn.Enabled = True
    SaveEugeneSDMD.visible = 1
    DMDSmall3.visible = 0
    magickicker.Enabled = True
    EugeneModeFailed.Enabled = True
    DiscoBlocker.Isdropped = 1
    PuPlayer.playlistplayex pBackglass,"SaveEugeneMode","SaveEugeneMode.mp4",75,1
    PuPlayer.playlistplayex pMusic,"audiomodes","SaveEugeneMode.mp3",0,1
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "SaveEugeneMode.png", "", "", 3800
  End Sub


Sub EugeneLitVideoTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EugeneLitVideoTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    PuPlayer.playlistplayex pBackglass,"SaveEugeneMode","DontGoAlone.mp4",115,1
    PuPlayer.playlistplayex pMusic,"audiomodes","DontGoAlone.mp3",0,1
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "SaveEugeneMode.png", "", "", 3800
    EugeneTimer001On.Enabled = True
    quotetrigger.Enabled = false
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False
End Sub



Sub EugeneTimer001Off_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
 EugeneTimer001Off.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
 SingleModeKicker.Enabled = 0
 SingleModeKicker.Kick 2, 14
 Eugene.State=1
 SaveEugeneFlasher.opacity = 0
 StopGlassesSpin
 GiOn
 NightShadeGIOff
 SpotlightAttractOff
 SaveEugeneSmallDMDOn.Enabled = False
 SaveEugeneSmallDMDOff.Enabled = False
 SaveEugeneSmallDMD001.Enabled = False
 SaveEugeneSmallDMDOn.Enabled = 0
 SaveEugeneSmallDMDOff.Enabled = 0
 SaveEugeneSmallDMD001.Enabled = 0
 SaveEugeneSDMD001.visible = 0
 quotetrigger.Enabled = True
 quotetrigger001.Enabled = True
 VideoQuoteResteTimer.Enabled = False
 VideoQuoteResteTimer001.Enabled = False
 VideoQuoteResteTimer002.Enabled = False
 VideoQuoteResteTimer003.Enabled = False
 VideoQuoteResteTimer004.Enabled = False
 VideoQuoteResteTimer005.Enabled = False
 quotetrigger002.Enabled = True
 AudioAttractTimer001.Enabled = True
 DiscoBlocker.Isdropped = 1
 SingleModeKillerTrigger.Enabled = False
End Sub

Sub EugeneModeFailed_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EugeneModeFailed.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    StopGlassesSpin
    GiOn
    NightShadeGIOff
    SpotlightAttractOff
    SaveEugeneAllSmallDMDsOff
    SaveEugeneSDMD001.visible = 0
    SingleModeBlocker001.Isdropped = 1
    SingleModeBlocker002.Isdropped = 1
    SingleModeBlocker003.Isdropped = 1
    SaveEugeneFlasher.opacity = 0
    ramptrigger.Enabled = True
    EugeneDSMSOffTrigger.Enabled = False
    Trigger2.Enabled = True
    EugeneTrigger001.Enabled = False
    EugeneMB001.State = 0
    EugeneTrigger002.Enabled = False
    EugeneMB002.State = 0
    EugeneTrigger003.Enabled = False
    EugeneMB003.State = 0
    EugeneTrigger004.Enabled = False
    EugeneMB004.State = 0
    EugeneTrigger005.Enabled = False
    EugeneMB005.State = 0
    quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = True
    AudioAttractTimer001.Enabled = False
    QON2.Enabled = True
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    Trigger2.Enabled = True
    ramptrigger.Enabled = True
    DiscoBlocker.Isdropped = 1
 End Sub


Sub SaveEugeneDrain()
    StopGlassesSpin
    'GiOn
    NightShadeGIOff
    SpotlightAttractOff
    SaveEugeneFlasher.opacity = 0
    ramptrigger.Enabled = True
    EugeneTrigger001.Enabled = False
    EugeneMB001.State = 0
    EugeneTrigger002.Enabled = False
    EugeneMB002.State = 0
    EugeneTrigger003.Enabled = False
    EugeneMB003.State = 0
    EugeneTrigger004.Enabled = False
    EugeneMB004.State = 0
    EugeneTrigger005.Enabled = False
    EugeneMB005.State = 0
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    Trigger2.Enabled = True
    ramptrigger.Enabled = True
    DiscoBlocker.Isdropped = 1
 End Sub



Sub SaveEugeneOff()
EugeneTrigger001.Enabled = False
EugeneMB001.State = 0
EugeneTrigger002.Enabled = False
EugeneMB002.State = 0
EugeneTrigger003.Enabled = False
EugeneMB003.State = 0
EugeneTrigger004.Enabled = False
EugeneMB004.State = 0
EugeneTrigger005.Enabled = False
EugeneMB005.State = 0
SingleModeKicker.Enabled = 0
SingleModeMagnet.MagnetOn = false
StopGlassesSpin
AudioAttractTimer001.Enabled = True
end sub


Sub StartGlassesSpin
    Glasses.visible = 1
    discoball.visible = 0
    GlassesTimer.Enabled = 1
    StopLOGOAni
    NMLogoFlasherTimer.Enabled = 0
    NMLOGO.visible = 0
    EnableBallSaver -1
    SaveEugeneSDMD.visible = 1
    SaveEugeneSDMD001.visible = 1
End Sub

Sub StopGlassesSpin
    Glasses.visible = 0
    GlassesTimer.Enabled = 0
    SaveEugeneAllSmallDMDsOff
End Sub

Sub GlassesTimer_Timer
    Glasses.rotz = (Glasses.rotz + 1)mod 360
End Sub


'*********************************************************************************
'Save Eugene Small DMD Timers
'*********************************************************************************

Sub SaveEugeneSmallDMDOn_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SaveEugeneSmallDMDOn.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SaveEugeneSDMD.visible = 0
    ShootRedSDMD.visible = 1
    SaveEugeneSDMD001.visible = 0
    ShootRedSDMD001.visible = 1
    SaveEugeneSmallDMDOff.Enabled = True
  End Sub


Sub SaveEugeneSmallDMDOff_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SaveEugeneSmallDMDOff.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    ShootRedSDMD.visible = 0
    GlassesSDMD.visible = 1
    ShootRedSDMD001.visible = 0
    GlassesSDMD001.visible = 1
    SaveEugeneSmallDMD001.Enabled = True
  End Sub


Sub SaveEugeneSmallDMD001_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SaveEugeneSmallDMD001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    GlassesSDMD.visible = 0
    GlassesSDMD001.visible = 0
    SaveEugeneSDMD.visible = 1
    SaveEugeneSDMD001.visible = 1
    SaveEugeneSmallDMDOn.Enabled = True
  End Sub


Sub SaveEugeneAllSmallDMDsOff() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SaveEugeneSDMD.visible = 0
    SaveEugeneSDMD001.visible = 0
    GlassesSDMD.visible = 0
    GlassesSDMD001.visible = 0
    ShootRedSDMD.visible = 0
    ShootRedSDMD001.visible = 0
    DMDSmall3.visible = 1
    SaveEugeneSmallDMDOn.Enabled = False
    SaveEugeneSmallDMDOff.Enabled = False
    SaveEugeneSmallDMD001.Enabled = False
    SaveEugeneSmallDMDOn.Enabled = 0
    SaveEugeneSmallDMDOff.Enabled = 0
    SaveEugeneSmallDMD001.Enabled = 0
End Sub


'*************************************************************
'		NightShade Mode Targets Timers
'*************************************************************

sub NightShadesTarger001_hit()
  addscore 25000
  EnableBallSaver -1
  PlaySound "soundmagnet"
  NLight.state = 1
  NightShadesTarger004.Isdropped = 0
end sub

sub NightShadesTarger002_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  ILight.state = 1
  NightShadesTarger005.Isdropped = 0
end sub

sub NightShadesTarger003_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  GLight.state = 1
  NightShadesTarger006.Isdropped = 0
  DMDFlush
  DMD "dmd baseBlank.png", "", "", 300
  DMD "NSMKeepGoing.png", "", "", 3800
end sub


sub NightShadesTarger004_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  HLight.state = 1
  NightShadesTarger007.Isdropped = 0
end sub


sub NightShadesTarger005_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  TLight.state = 1
  NightShadesTarger008.Isdropped = 0
end sub


sub NightShadesTarger006_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  SLght.state = 1
  NightShadesTarger009.Isdropped = 0
  DMDFlush
  DMD "dmd baseBlank.png", "", "", 300
  DMD "NSMKeepGoing.png", "", "", 3800
end sub


sub NightShadesTarger007_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  HLight2.state = 1
  NightShadesTarger010.Isdropped = 0
end sub


sub NightShadesTarger008_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  ALight.state = 1
  NightShadesTarger011.Isdropped = 0
end sub


sub NightShadesTarger009_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  DLight.state = 1
  NightShadesTarger012.Isdropped = 0
  DMDFlush
  DMD "dmd baseBlank.png", "", "", 300
  DMD "NSMKeepGoing.png", "", "", 3800
end sub


sub NightShadesTarger010_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  ELight.state = 1
  NightShadesTarger013.Isdropped = 0
end sub


sub NightShadesTarger011_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  SLght2.state = 1
  NightShadesTarger014.Isdropped = 0
end sub


sub NightShadesTarger012_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  OLight.state = 1
  NightShadesTarger015.Isdropped = 0
  DMDFlush
  DMD "dmd baseBlank.png", "", "", 300
  DMD "NSMKeepGoing.png", "", "", 3800
end sub


sub NightShadesTarger013_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  CLight.state = 1
  NightShadesTarger016.Isdropped = 0
end sub


sub NightShadesTarger014_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  ILight2.state = 1
  NightShadesTarger017.Isdropped = 0
end sub


sub NightShadesTarger015_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  ELight2.state = 1
  DMDFlush
  DMD "dmd baseBlank.png", "", "", 300
  DMD "NSMKeepGoing.png", "", "", 3800
end sub


sub NightShadesTarger016_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  TLight2.state = 1
end sub


sub NightShadesTarger017_hit()
  EnableBallSaver -1
  addscore 25000
  PlaySound "soundmagnet"
  YLight.state = 1
  NightShadesTarger001timer.Enabled = True
  DMDFlush
  DMD "dmd baseBlank.png", "", "", 300
  DMD "NSMKeepGoing.png", "", "", 3800
end sub


sub NightShadesTarger018_hit()
   if YLight.state = 0 and ELight2.state = 0 and TLight2.state = 0 Then
   NightShadesTarger001timer.Enabled = True
  End If
  if YLight.state = 1 and ELight2.state = 1 and TLight2.state = 1 Then
  addscore 25000
  PlaySound "soundmagnet"
  YLight.state = 1
  NightHsadeSJPLight.state = 2
  NightShadeSJPTrigger.Enabled = True
  NightShadesTarger001timer.Enabled = False
  NightShadesTarger002timer.Enabled = False
  End If
end sub


Sub NightShadesTarger001timer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NightShadesTarger001timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    NightShadesTarger002timer.Enabled = True
	NightShadesTarger018.Isdropped = 0
 End Sub

Sub NightShadesTarger002timer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NightShadesTarger002timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    NightShadesTarger001timer.Enabled = True
	NightShadesTarger018.Isdropped = 0
End Sub


sub NightShadeSJPTrigger_hit()
  ' if YLight.state = 1 and ELight2.state = 1 and TLight2.state = 1 Then
  addscore 25000
  GiOn
  SpotlightAttractOff
  NightShadeGIOff
  StopBookSpin
  NightShadeSAllSmallDMDsOff
  NightShadesFailedTimer.Enabled = False
  Book.visible = 0
  Book2.visible = 1
  EndOfNightShadesTimer.Enabled = True
  NightShadesTarger001timer.Enabled = False
  NightShadesTarger002timer.Enabled = False
  SingleModeKicker.Enabled = 1
  NightHsadeSJPLight.state = 0
  GIOFFWIZARDTrigger001.Enabled = False
  GIOFFWIZARDTrigger.Enabled = false
  GIOFFWIZARDTrigger.Enabled = False
  GIOFFWIZARDTrigger001.Enabled = false
  GIOFFWIZARDTrigger002.Enabled = false
  GIOFFWIZARDTrigger003.Enabled = false
  GIOFFWIZARDTrigger004.Enabled = false
  JackpotBlockers001.Isdropped = 0
  JackpotBlockers002.Isdropped = 0
  SPJTargetTimer.Enabled = True
  MBBlocker.Isdropped = 1
  MBBlocker001.Isdropped = 1
  MBBlocker002.Isdropped = 1
  NMLogoFlasherTimer.Enabled = 1
  NMLOGO.visible = 1
  SingleModeMagnet.MagnetOn = True
  vpmTimer.AddTimer 30, " SingleModeMagnet.MagnetOn = False '"
  vpmTimer.AddTimer 35, " SingleModeMagnet.MagnetOn = False '"
  vpmTimer.AddTimer 40, " SingleModeMagnet.MagnetOn = False '"
  vpmTimer.AddTimer 45, " SingleModeMagnet.MagnetOn = False '"
  RightInLaneTrigger.Enabled = True
  LeftInLaneTrigger.Enabled = True
  Trigger2.Enabled = True
  ramptrigger.Enabled = True
  SingleModeKillerTrigger.Enabled = True
  PuPlayer.playlistplayex pBackglass,"Seceret Libriary","NightShadesForever.mp4",115,1
  DMDFlush
  DMD "dmd baseBlank.png", "", "", 300
  DMD "NSMBookCollected.png", "", "", 3800
end sub


Sub NightShadeLightsOff()
NLight.state = 0
ILight.state = 0
GLight.state = 0
HLight.state = 0
TLight.state = 0
SLght.state = 0
HLight2.state = 0
ALight.state = 0
DLight.state = 0
ELight.state = 0
SLght2.state = 0
OLight.state = 0
CLight.state = 0
ILight2.state = 0
ELight2.state = 0
TLight2.state = 0
YLight.state = 0
End Sub


Sub NightShadeTargetsOff()
NightShadesTarger004.Isdropped = 1
NightShadesTarger005.Isdropped = 1
NightShadesTarger006.Isdropped = 1
NightShadesTarger007.Isdropped = 1
NightShadesTarger008.Isdropped = 1
NightShadesTarger009.Isdropped = 1
NightShadesTarger010.Isdropped = 1
NightShadesTarger011.Isdropped = 1
NightShadesTarger012.Isdropped = 1
NightShadesTarger013.Isdropped = 1
NightShadesTarger014.Isdropped = 1
NightShadesTarger015.Isdropped = 1
NightShadesTarger016.Isdropped = 1
NightShadesTarger017.Isdropped = 1
NightShadesTarger018.Isdropped = 1
End Sub


Sub StartNightShadesMode()
NightShadeGIOn
GiOff
StartBookSpin
SpotlightAttractOn
EnableBallSaver -1
DMDSmall3.visible = 0
NightShadeSDMD.visible = 1
NightShadeSDMD001.visible = 1
NightShadeSmallDMDOn.Enabled = True
DiscoBlocker.Isdropped = 1
KillDropLight.state = 1
DropKillTrigger.Enabled = True
RightInLaneTrigger.Enabled = False
LeftInLaneTrigger.Enabled = False
Trigger2.Enabled = False
ramptrigger.Enabled = False
Book.visible = 1
discoball.visible = 0
NightShadesTarger001.Isdropped = 0
NightShadesTarger002.Isdropped = 0
NightShadesTarger003.Isdropped = 0
NightShadeFlasher.opacity = 95
SingleModeBlocker001.Isdropped = 0
SingleModeBlocker002.Isdropped = 0
SingleModeBlocker003.Isdropped = 0
PuPlayer.playlistplayex pBackglass,"Seceret Libriary","Seceret Libriary.mp4",115,1
PuPlayer.playlistplayex pMusic,"audiomodes","Seceret Libriary.mp3",0,1
PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
DMDFlush
DMD "dmd baseBlank.png", "", "", 300
DMD "NightshadeMode.png", "", "", 3800
End Sub


Sub EndNightShades()
SpotlightAttractOff
NightShadeGIOff
StopBookSpin
DiscoBlocker.Isdropped = 1
Book.visible = 0
NightShadesTarger001.Isdropped = 1
NightShadesTarger002.Isdropped = 1
NightShadesTarger003.Isdropped = 1
NightShadesTarger004.Isdropped = 1
NightShadesTarger005.Isdropped = 1
NightShadesTarger006.Isdropped = 1
NightShadesTarger007.Isdropped = 1
NightShadesTarger008.Isdropped = 1
NightShadesTarger009.Isdropped = 1
NightShadesTarger010.Isdropped = 1
NightShadesTarger011.Isdropped = 1
NightShadesTarger012.Isdropped = 1
NightShadesTarger013.Isdropped = 1
NightShadesTarger014.Isdropped = 1
NightShadesTarger015.Isdropped = 1
NightShadesTarger016.Isdropped = 1
NightShadesTarger017.Isdropped = 1
NightShadesTarger018.Isdropped = 1
NightShadeFlasher.opacity = 0
NightShadesTarger001timer.Enabled = False
NightShadesTarger002timer.Enabled = False
NightShadeSJPTrigger.Enabled = False
NightHsadeSJPLight.state = 0
GIOFFWIZARDTrigger001.Enabled = False
GIOFFWIZARDTrigger.Enabled = false
GIOFFWIZARDTrigger.Enabled = False
GIOFFWIZARDTrigger001.Enabled = false
GIOFFWIZARDTrigger002.Enabled = false
GIOFFWIZARDTrigger003.Enabled = false
GIOFFWIZARDTrigger004.Enabled = false
SingleModeBlocker001.Isdropped = 1
SingleModeBlocker002.Isdropped = 1
SingleModeBlocker003.Isdropped = 1
NLight.state = 0
ILight.state = 0
GLight.state = 0
HLight.state = 0
TLight.state = 0
SLght.state = 0
HLight2.state = 0
ALight.state = 0
DLight.state = 0
ELight.state = 0
SLght2.state = 0
OLight.state = 0
CLight.state = 0
ILight2.state = 0
ELight2.state = 0
TLight2.state = 0
YLight.state = 0
End Sub


Sub StartBookSpin
    Book.visible = 1
    BookTimer.Enabled = 1
    NMLogoFlasherTimer.Enabled = 0
    NMLOGO.visible = 0
 End Sub

Sub StopBookSpin
    Book.visible = 0
    BookTimer.Enabled = 0
End Sub

Sub BookTimer_Timer
    Book.rotz = (book.rotz + 1)mod 360
End Sub


Sub StartVideoNightShadesTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    StartVideoNightShadesTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    PuPlayer.playlistplayex pBackglass,"Seceret Libriary","ComingWithUs.mp4",115,1
    PuPlayer.playlistplayex pMusic,"audiomodes","ComingWithUs.mp3",0,1
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
    DMDFlush
    DMD "dmd baseBlank.png", "", "", 300
    DMD "NightshadeMode.png", "", "", 3800
    quotetrigger.Enabled = false
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False
 End Sub


Sub NightShadeTarger003Timer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NightShadesTarger003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	'quotetrigger.Enabled = True
    'quotetrigger001.Enabled = True
    'quotetrigger002.Enabled = True
    'VideoQuoteResteTimer004.Enabled = True
 End Sub


Sub StartNightShadesTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    StartNightShadesTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    StartNightShadesMode
    SingleModeKicker.Enabled = 0
    SingleModeKicker.Kick 2, 14
 End Sub


Sub StartNightShadesTargetTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    StartNightShadesTargetTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    NightShadesTarger001.Isdropped = 1
    NightShadesTarger002.Isdropped = 1
    NightShadesTarger003.Isdropped = 1
    NightShadesTarger004.Isdropped = 1
    NightShadesTarger005.Isdropped = 1
    NightShadesTarger006.Isdropped = 1
    NightShadesTarger007.Isdropped = 1
    NightShadesTarger008.Isdropped = 1
    NightShadesTarger009.Isdropped = 1
    NightShadesTarger010.Isdropped = 1
    NightShadesTarger011.Isdropped = 1
    NightShadesTarger012.Isdropped = 1
    NightShadesTarger013.Isdropped = 1
    NightShadesTarger014.Isdropped = 1
    NightShadesTarger015.Isdropped = 1
    NightShadesTarger016.Isdropped = 1
    NightShadesTarger017.Isdropped = 1
    NightShadesTarger018.Isdropped = 1
    DiscoBlocker.Isdropped = 1
 End Sub



Sub EndOfNightShadesTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EndOfNightShadesTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    EndNightShades
    Biannca.State=1
    AJAX.State=1
    XAVIER.State=1
    aquamanLight.State=1
    aquawoman.State=1
    YoKo.State=1
    VideoQuoteResteTimer004.Enabled = True
    SingleModeKicker.Enabled = 0
    SingleModeKicker.Kick 2, 14
    quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = True
    AudioAttractTimer001.Enabled = True
    SingleModeKillerTrigger.Enabled = False
End Sub


Sub NightShadesFailedTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NightShadesFailedTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	EndNightShades
    NightShadeGIOff
    GiOn
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    Trigger2.Enabled = True
    ramptrigger.Enabled = True
    DiscoBlocker.Isdropped = 1
    quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = True
    AudioAttractTimer001.Enabled = False
    QON2.Enabled = True
    NightShadeSAllSmallDMDsOff
End Sub


Sub NightShadesDrainTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NightShadesDrainTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	EndNightShades
    NightShadeSAllSmallDMDsOff
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    Trigger2.Enabled = True
    ramptrigger.Enabled = True
    DiscoBlocker.Isdropped = 1
 End Sub


Sub NightShadesDrainTimer2_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NightShadesDrainTimer2.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	EndNightShades
 End Sub


Sub NightShadesDrainTimer3_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NightShadesDrainTimer3.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	EndNightShades
 End Sub


Sub NightShadesDrainTimer4_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NightShadesDrainTimer4.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	EndNightShades
End Sub


Sub NightShadesDrainTimer5_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NightShadesDrainTimer5.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	NightShadesDrainTimer.Enabled = True
    NightShadesDrainTimer2.Enabled = True
    NightShadesDrainTimer3.Enabled = True
    NightShadesDrainTimer4.Enabled = True
 End Sub

Sub NightShadeGIOn()
   giRedSlingNightShade1.State=2
   giRedSlingNightShade2.State=2
   giPurpleNightShade1.State=2
   giPurpleNightShade2.State=2
   GIUnderslingGreenNS1.State=2
   GIUnderslingGreenNS2.State=2
   GIUnderslingGreenNS3.State=2
   GIUnderslingGreenNS4.State=2
End Sub

Sub NightShadeGIOff()
   giRedSlingNightShade1.State=0
   giRedSlingNightShade2.State=0
   giPurpleNightShade1.State=0
   giPurpleNightShade2.State=0
   GIUnderslingGreenNS1.State=0
   GIUnderslingGreenNS2.State=0
   GIUnderslingGreenNS3.State=0
   GIUnderslingGreenNS4.State=0
End Sub


Sub DropKillTrigger_Hit()
    If KillDropLight.state =0 Then
    KillDropLight.state =0
    NightShadesTarger001.Isdropped = 1
    NightShadesTarger002.Isdropped = 1
    NightShadesTarger003.Isdropped = 1
    NightShadesTarger004.Isdropped = 1
    NightShadesTarger005.Isdropped = 1
    NightShadesTarger006.Isdropped = 1
    NightShadesTarger007.Isdropped = 1
    NightShadesTarger008.Isdropped = 1
    NightShadesTarger009.Isdropped = 1
    NightShadesTarger010.Isdropped = 1
    NightShadesTarger011.Isdropped = 1
    NightShadesTarger012.Isdropped = 1
    NightShadesTarger013.Isdropped = 1
    NightShadesTarger014.Isdropped = 1
    NightShadesTarger015.Isdropped = 1
    NightShadesTarger016.Isdropped = 1
    NightShadesTarger017.Isdropped = 1
    NightShadesTarger018.Isdropped = 1
    DiscoBlocker.Isdropped = 1
End If
End Sub


'*********************************************************************************
'Night Sahdes Small DMD Timers
'*********************************************************************************

Sub NightShadeSmallDMDOn_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NightShadeSmallDMDOn.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    NightShadeSDMD.visible = 0
    SpellSDMD.visible = 1
    NightShadeSDMD001.visible = 0
    SpellSDMD001.visible = 1
    NightShadeSmallDMDOff.Enabled = True
  End Sub


Sub NightShadeSmallDMDOff_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NightShadeSmallDMDOff.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SpellSDMD.visible = 0
    ClearModeSDMD.visible = 1
    SpellSDMD001.visible = 0
    ClearModeSDMD001.visible = 1
    NightShadeSmallDMDClear.Enabled = True
  End Sub


Sub NightShadeSmallDMDClear_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NightShadeSmallDMDClear.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    ClearModeSDMD.visible = 0
    NightShadeSDMD.visible = 1
    ClearModeSDMD001.visible = 0
    NightShadeSDMD001.visible = 1
    NightShadeSmallDMDOn.Enabled = True
  End Sub


Sub NightShadeSAllSmallDMDsOff() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NightShadeSDMD.visible = 0
    ClearModeSDMD.visible = 0
    SpellSDMD.visible = 0
    NightShadeSDMD001.visible = 0
    ClearModeSDMD001.visible = 0
    SpellSDMD001.visible = 0
    DMDSmall3.visible = 1
    NightShadeSmallDMDOn.Enabled = False
    NightShadeSmallDMDClear.Enabled = False
    NightShadeSmallDMDOff.Enabled = False
 End Sub



'*************************************************************




'*************************************************************
'		MBBlocker and SuperTarget Timers
'*************************************************************

Sub GraveStoneSuperTarget002_hit()
  addscore 25000
  PlaySound "wack"
  grave001tragertimer.Enabled = True
end sub


Sub GraveStoneSuperTarget001_hit()
  addscore 25000
  PlaySound "wack"
  grave001tragertimer001.Enabled = True
end sub

Sub GraveStoneSuperTarget003_hit()
  addscore 25000
  PlaySound "wack"
  grave001tragertimer002.Enabled = True
end sub

Sub grave001tragertimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    grave001tragertimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	GraveStoneSuperTarget002.Isdropped = 0
 End Sub

Sub grave001tragertimer001_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    grave001tragertimer001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	GraveStoneSuperTarget001.Isdropped = 0
 End Sub

Sub grave001tragertimer002_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    grave001tragertimer002.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	GraveStoneSuperTarget003.Isdropped = 0
 End Sub

Sub SuperTargetsOffTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SuperTargetsOffTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	GraveStoneSuperTarget001.Isdropped = 1
    GraveStoneSuperTarget002.Isdropped = 1
    GraveStoneSuperTarget003.Isdropped = 1
    grave001tragertimer002.Enabled = False
    grave001tragertimer001.Enabled = False
    grave001tragertimer.Enabled = False
    DiscoBlocker.Isdropped = 1
End Sub


Sub SuperTargetsOffTimer001_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SuperTargetsOffTimer001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	GraveStoneSuperTarget001.Isdropped = 1
    GraveStoneSuperTarget002.Isdropped = 1
    GraveStoneSuperTarget003.Isdropped = 1
    grave001tragertimer002.Enabled = False
    grave001tragertimer001.Enabled = False
    grave001tragertimer.Enabled = False
    DiscoBlocker.Isdropped = 1
    DiscoBlocker.Isdropped = 1
    DiscoBlocker.Isdropped = 1
End Sub



Sub MBBlockerTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    MBBlockerTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	MBBlocker.Isdropped = 1
    MBBlocker001.Isdropped = 1
    MBBlockerTarget.Enabled = false
    MBBlocker001Target.Enabled = false
    MBBlockerlight001.state = 0
    MBBlockerlight002.state = 0
    MBBlockerlight003.state = 0
    MBBlockerlight004.state = 0
    MBBlockerlight005.state = 0
    DiscoBlocker.Isdropped = 1
End Sub


Sub MBBlockerTimer001_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    MBBlockerTimer001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	MBBlocker.Isdropped = 1
    MBBlocker001.Isdropped = 1
    MBBlockerTarget.Enabled = false
    MBBlocker001Target.Enabled = false
    MBBlockerlight001.state = 0
    MBBlockerlight002.state = 0
    MBBlockerlight003.state = 0
    MBBlockerlight004.state = 0
    MBBlockerlight005.state = 0
    DiscoBlocker.Isdropped = 1
End Sub


Sub MBBlockerTimer002_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    MBBlockerTimer002.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	MBBlocker.Isdropped = 1
    MBBlocker001.Isdropped = 1
    MBBlocker002.Isdropped = 1
    MBBlockerTarget.Enabled = false
    MBBlocker001Target.Enabled = false
    MBBlocker002Target.Enabled = false
    MBBlockerlight001.state = 0
    MBBlockerlight002.state = 0
    MBBlockerlight003.state = 0
    MBBlockerlight004.state = 0
    MBBlockerlight005.state = 0
    DiscoBlocker.Isdropped = 1
End Sub



Sub MBBlockerTimer003_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    MBBlockerTimer003.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'MBBlocker5.Isdropped = 1
    Trigger2.Enabled = True
End Sub


Sub SJPBlockerTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SJPBlockerTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'MBBlocker5.Isdropped = 1
    'SJPKicker.Enabled = 0
    'SJPKicker.Kick 2, 14
end sub




sub MBBlockerTarget_hit()
  addscore 5000
  PlaySound "EvilMaleLaugh001"
end sub

sub MBBlocker001Target_hit()
  addscore 5000
  PlaySound "evilFemalLaugh"
end sub

sub MBBlocker002Target_hit()
  addscore 5000
  PlaySound "evilFemalLaugh"
end sub


Sub SingleModeBlocker001Timer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SingleModeBlocker001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SingleModeBlocker001.Isdropped = 1
    SingleModeBlocker002.Isdropped = 1
    SingleModeBlocker003.Isdropped = 1
End Sub


Sub SingleModeBlocker002Timer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SingleModeBlocker002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SingleModeBlocker001.Isdropped = 1
    SingleModeBlocker002.Isdropped = 1
    SingleModeBlocker003.Isdropped = 1
End Sub


Sub SingleModeBlocker003Timer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SingleModeBlocker003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SingleModeBlocker001.Isdropped = 1
    SingleModeBlocker002.Isdropped = 1
    SingleModeBlocker003.Isdropped = 1
    
End Sub

'MBBlockerTimer.Enabled = False
'MBBlockerTimer001.Enabled = False
'MBBlockerTimer002.Enabled = False
'MBBlockerTarget.Enabled = false
'MBBlocker001Target.Enabled = false
'MBBlocker002Target.Enabled = false
'MBBlocker.Isdropped = 1
'MBBlocker001.Isdropped = 1
'MBBlocker002.Isdropped = 1
'MBBlockerTimer002.Enabled = False
'MBBlocker002Target.Enabled = false

'*************************************************************
'		Electricity Light Effects
'*************************************************************


Sub StartElectricityLightFXSeq()
   'Eletricity light sequence
    ElectricityLightFX.UpdateInterval = 25
    ElectricityLightFX.Play SeqBlinking, , 5, 150
    ElectricityLightFX.Play SeqRandom, 40, , 4000
    ElectricityLightFX.Play SeqAllOff
    ElectricityLightFX.UpdateInterval = 8
End Sub

sub ElectricityLightFXStopTrigger_hit()
    EletricTimer.Enabled = True
end sub


sub ElectricityLightFXStopTrigge001_hit()
    EletricTimerTimeout.Enabled = True
end sub


Sub EletricTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EletricTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	ElectricityLightFX.StopPlay
    StopSound "sfx_thunder1"
    StopSound "EvilVoices"
 End Sub


Sub EletricTimerTimeout_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EletricTimerTimeout.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	ElectricityLightFX.StopPlay
    StopSound "sfx_thunder1"
    StopSound "EvilVoices"
 End Sub
'*************************************************************




'*****************************************************************************
'   				END OF WEDNESDAY WIZARD MODE TIMER
'*****************************************************************************


Sub WednesdayWizardStartTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WednesdayWizardStartTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    StartWednesdayWizadrMode
    WizardModeKicker.Enabled = 0
    WizardModeKicker.DestroyBall
    WEDWizardMBTimer.Enabled = True
    End Sub



Sub WizaedRestartTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizaedRestartTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    EndOfGame
    BallsOnPlayfield = 0
    HSResetBlocker.Isdropped = 1
    GameVideoStartTrigger.Enabled = True
    HSResettKicker.Enabled = 0
    bAutoPlunger = False 
    MBBlocker003.Isdropped = 1
End Sub


Sub HSResetPause_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    HSResetPause.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    bMultiBallMode = False
    bAutoPlunger = False 
    BallsOnPlayfield = 0
    WizaedRestartTimer.Enabled = True
    HSResettKicker.DestroyBall
    DMDFLush
    DMD "dmd baseBlank.png", "", "PLAY AGAIN", 3500
    DMD "dmd baseBlank.png", "", "GAME CLEARED", 3500
    WizardBallKillerTimer.Enabled = False
    WizardBallKillerResetTimer.Enabled = False
    WizardBallKiller001.Enabled = 0
    WizardBallKiller002.Enabled = 0
    WizardBallKiller003.Enabled = 0
    WizardBallKiller004.Enabled = 0
    WizardBallKiller005.Enabled = 0
    NMLogoFlasherTimer.Enabled = 1
    NMLogo.visible = 1
    StopWedSpin
End Sub


Sub HSResetTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    HSResetTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    HSResettKicker.DestroyBall
    HSResetPause.Enabled = True
End Sub


Sub EndOfWizardGameTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EndOfWizardGameTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    HSResetBlocker.Isdropped = 0
    HSResettKicker.Enabled = 1
    WizardModeKicker.Enabled = 0
    WizardModeKicker.DestroyBall
    WizardSaverKicker.Enabled = False
    LaunchKicker.DestroyBall
    GIOFFWIZARDTrigger.Enabled = False
    GIOFFWIZARDTrigger001.Enabled = false
    GIOFFWIZARDTrigger002.Enabled = false
    GIOFFWIZARDTrigger003.Enabled = false
    GIOFFWIZARDTrigger004.Enabled = false
    WizaedGIOffTimer.Enabled = False
    WizaedGIOffTimer001.Enabled = False
    quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    quotetrigger002.Enabled = True
    DiscoPartyDone.state = 0
    HYDEMB2Done.state = 0
    HydeModeDone.state = 0
    XAVIER.State= 0
    Biannca.State= 0
    AJAX.State= 0
    Eugene.State= 0
    Addams1.State= 0
    aquamanLight.State=0
    aquawoman.State=0
    YoKo.State=0
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    Trigger2.Enabled = True
    ramptrigger.Enabled = True
    DiscoBlocker.Isdropped = 1
    BloodRepeatTimer.Enabled = False
    BloodRepeatTimer2.Enabled = False
    GameVideoStartTrigger.Enabled = True
End Sub


Sub Timer001_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    Timer001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    WizardBallKillerTimer.Enabled = True
End Sub


Sub Timer002_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    Timer002.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    CheckHighscore
    PuPlayer.playlistplayex pBackglass,"videoattract","HighScore.mp4",videovol,1
    WizardClearTimer.Enabled = True
    HSResettKicker.DestroyBall
 End Sub


Sub EndOfWizareModeTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EndOfWizareModeTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    EndWednesdayWizardMode
    WWMBAllSmallDMDsOff
    PuPlayer.playlistplayex pBackglass,"LostWizardMode","LostWizardMode.mp4",videovol,1
    PuPlayer.playlistplayex pMusic,"audiomodes","LostWizardMode.mp3",0,1
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0,2
    WizardBallKiller001.Enabled = 1
    WizardBallKiller002.Enabled = 1
    WizardBallKiller003.Enabled = 1
    WizardBallKiller004.Enabled = 1
    WizardBallKiller005.Enabled = 1
    WizardBallKillerTimer.Enabled = True
    Timer002.Enabled = True
    CRACKSTONEMoveDown
    CStoneBoom002Timer.Enabled = True
    CRACKSTONETrigger1.Enabled = 0
    CRACKSTONETrigger2.Enabled = 0
	CRACKSTONETrigger2.Enabled = 0
    DMDFLush
    DMD "dmd baseBlank.png", "SO SORRY", "BUT YOU LOST", 7500
    DMD "dmd baseBlank.png", "BETTER LUCK", "NEXT TIME SUCKERS", 7500
    PuPlayer.playlistplayex pCallouts,"audiocallouts","BetterLuckNextTime.mp3",calloutvol,1
   ' WizardMagnet.MagnetOn = True
   ' vpmTimer.AddTimer 40, " WizardMagnet.MagnetOn = False '"
   ' vpmTimer.AddTimer 45, " WizardMagnet.MagnetOn = False '"
   ' vpmTimer.AddTimer 50, " WizardMagnet.MagnetOn = False '"
   ' vpmTimer.AddTimer 55, " WizardMagnet.MagnetOn = False '"
    WizardSaverKicker.Enabled = False
    WizardModeKicker.Enabled = 1
    WizardSpotSeq.StopPlay
    playsound "ModeFail"
    XAVIER.State= 0
    Biannca.State= 0
    AJAX.State= 0
    Eugene.State= 0
    Addams1.State= 0
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    Trigger2.Enabled = True
    ramptrigger.Enabled = True
    DiscoBlocker.Isdropped = 1
    BloodRepeatTimer.Enabled = False
    BloodRepeatTimer2.Enabled = False
    GameVideoStartTrigger.Enabled = True
    WizardSaverKicker.Enabled = False
End Sub




Sub WizardSPTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizardSPTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    DMDFLush
    DMD "CStoneSJP.png", "", "", 3000
    DMD "dmd baseBlank.png", "SHOOT THE", "LEFT ORBIT", 4500
    playsound "Jackpotlit"
    CRACKSTONESuperJackpotLight.state = 2
    CRACKSTONESuperJackpotTrigger.Enabled = True
    WizardSPoffTimer.Enabled = True
    magickicker.Enabled = False
    HSResettKicker.Enabled = 1
    HSResetBlocker.Isdropped = 0
    CRACKSTONETrigger1.Enabled = 0
    CRACKSTONETrigger2.Enabled = 0
	CRACKSTONETrigger2.Enabled = 0
End Sub


Sub WizardSPoffTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizardSPoffTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	CRACKSTONESuperJackpotLight.state = 0
    CRACKSTONESuperJackpotTrigger.Enabled = False
End Sub



Sub WizardClearTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizardClearTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'CRACKSTONEMoveDown
    'CStoneBoom002Timer.Enabled = True
    WizardBlocker.Isdropped = 1
    playsound "ModeCleared"
    WizardLightGlowFade.state = 0
    WizardLightGlowFade001.state = 0
    WizardLightGlowFade002.state = 0
    GIOFFWIZARDTrigger.Enabled = False
    GIOFFWIZARDTrigger001.Enabled = false
    GIOFFWIZARDTrigger002.Enabled = false
    GIOFFWIZARDTrigger003.Enabled = false
    GIOFFWIZARDTrigger004.Enabled = false
    WizaedGIOffTimer.Enabled = False
    WizaedGIOffTimer001.Enabled = False
    CRACKSTONETrigger1.Enabled = 0
    CRACKSTONETrigger2.Enabled = 0
    CRACKSTONETrigger2.Enabled = 0
    MBBlocker.Isdropped = 1
    MBBlocker001.Isdropped = 1
    MBBlocker002.Isdropped = 1
    DiscoPartyLit.state = 0
    HydeModeDonelit.state = 0
    HYDEMB2Lit.state = 0
    quotetrigger.Enabled = True
    quotetrigger001.Enabled = True
    quotetrigger002.Enabled = True
    magickicker.Enabled = True
    WizardSaverKicker.Enabled = False
    wizardDrainLight.State= 0
    WizardCoverFlasher.visible = 0
    DiscoPartyDone.state = 0
    HYDEMB2Done.state = 0
    HydeModeDone.state = 0
    XAVIER.State= 0
    Biannca.State= 0
    AJAX.State= 0
    Eugene.State= 0
    Addams1.State= 0
    RightInLaneTrigger.Enabled = True
    LeftInLaneTrigger.Enabled = True
    Trigger2.Enabled = True
    ramptrigger.Enabled = True
    DiscoBlocker.Isdropped = 1
    GameVideoStartTrigger.Enabled = True
    BloodRampANITRIGGER.Enabled = True
    HSResetTimer.Enabled = True
End Sub

Sub WizardBallKillerTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizardBallKillerTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    WizardBallKiller001.Enabled = 1
    WizardBallKiller002.Enabled = 1
    WizardBallKiller003.Enabled = 1
    WizardBallKiller004.Enabled = 1
    WizardBallKiller005.Enabled = 1
    WizardBallKiller001.DestroyBall
    WizardBallKiller002.DestroyBall
    WizardBallKiller003.DestroyBall
    WizardBallKiller004.DestroyBall
    WizardBallKiller005.DestroyBall
    WizardBallKillerResetTimer.Enabled = True
 End Sub



Sub WizardBallKillerResetTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizardBallKillerResetTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    WizardBallKiller001.Enabled = 1
    WizardBallKiller002.Enabled = 1
    WizardBallKiller003.Enabled = 1
    WizardBallKiller004.Enabled = 1
    WizardBallKiller005.Enabled = 1
    WizardBallKiller001.DestroyBall
    WizardBallKiller002.DestroyBall
    WizardBallKiller003.DestroyBall
    WizardBallKiller004.DestroyBall
    WizardBallKiller005.DestroyBall
    WizardBallKillerTimer.Enabled = True
 End Sub


Sub WizardkickerkillerTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizardkickerkillerTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    WizardModeKicker.Enabled = 0
    WizardModeKicker.DestroyBall
    BallsOnPlayfield = 0
    HSResettKicker.DestroyBall
end sub

'*************************************************************
'	            WEDNESDAY WIZARD MODE
'*************************************************************
sub startwizardmodeTrigger_hit()
    if DiscoPartyDone.state = 1 and HYDEMB2Done.state = 1 and HydeModeDone.state = 1 and XAVIER.State= 1 and Eugene.State= 1 and Addams1.State= 1  then
    GiOff
    NightShadeGIOff
    DMDFLush
    DMD "dmd baseBlank.png", "", "", 500
    DMD "WEDWIZARDMODE.png", "", "", 4400
    DMD "dmd baseBlank.png", "", "", 500
    DMD "GetReady.wmv", "", "", 2400
    DMD "dmd baseBlank.png", "", "", 500
    DMD "ToDefeat.wmv", "", "", 2400
    DMD "dmd baseBlank.png", "", "", 500
    DMD "JosphCrackstone.wmv", "", "", 2400
    PuPlayer.playlistplayex pBackglass,"WednesdayWizardMode","WednesdayWizardMode.mp4",91,1
    PuPlayer.playlistplayex pMusic,"audiomodes","WednesdayWizardMode.mp3",0,1
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
    PuPlayer.playlistplayex pCallouts,"audiocallouts","WednesdayWizardMode.mp3",calloutvol,1
    WizardMagnet.MagnetOn = True
    vpmTimer.AddTimer 40, " WizardMagnet.MagnetOn = False '"
    WizardModeKicker.Enabled = 1
    WednesdayWizardStartTimer.Enabled = True
    Wednesday2.visible = 1
    discoball.visible = 0
    ShootAgainLight.state = 0
    WizardLightGlowFade.state = 2
    WizardLightGlowFade001.state = 2
    WizardLightGlowFade002.state = 2
    FireBallDMDTrigger.Enabled = False
    HYDEmbVideo.Enabled = False
    HSJPTimer1.Enabled = False
    EndHydeJackP.Enabled = False 
    SlimerDrainVideoResetTimer.Enabled = False
    SlimerDrainTimer.Enabled = False
    StartWheresHMB.Enabled = False 
    HSJPTimer.Enabled = False
    DiscoPartySuperJPTimer.Enabled = False
    DiscoPartyMBTimeoutTimer.Enabled = False
    quotetrigger.Enabled = False
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False
    QON.Enabled = False
    QON2.Enabled = False
    MBBlocker.Isdropped = 1
    MBBlocker001.Isdropped = 1
    MBBlocker002.Isdropped = 1
    MBBlockerTarget.Enabled = false
    MBBlocker001Target.Enabled = false
    MBBlocker002Target.Enabled = false
    MBBlockerTimer.Enabled = False 
    MBBlockerTimer001.Enabled = False
    MBBlockerTimer002.Enabled = False
    MBBlockerlight001.state = 0
    MBBlockerlight002.state = 0
    MBBlockerlight003.state = 0
    MBBlockerlight004.state = 0
    MBBlockerlight005.state = 0
    wizardDrainLight.State= 1
    totherampSkill003.Enabled = 0
    RightInLaneTrigger.Enabled = False
    LeftInLaneTrigger.Enabled = False
    WizardCoverFlasher.visible = 1
    NMLogoFlasherTimer.Enabled = 0
    NMLOGO.visible = 0
    NightShadesFailedTimer.Enabled = False
    EugeneModeFailed.Enabled = False
    SaveGomezModeFailed.Enabled = False 
    SingleModeKillerKicker.Enabled = 0
    SingleModeKillerKicker001.Enabled = 0
    SingleModeKillerKicker002.Enabled = 0
    SingleModeKillerKicker003.Enabled = 0
    SingleModeKillerKicker004.Enabled = 0
    SingleModeKillerKicker005.Enabled = 0
    SingleModeKillerKicker006.Enabled = 0
    SingleModeKillerKicker007.Enabled = 0
    SingleModeKillerKicker008.Enabled = 0
    SingleModeKillerKicker009.Enabled = 0
    SingleModeKillerKicker010.Enabled = 0
    SingleModeKillerTrigger.Enabled = False
    SingleKillerTimer.Enabled = False
    J2.state = 0
    J3.state = 0    
    A1.state = 0
    A2.state = 0
    ActivateKickbak.state = 0
    B1.state = 0
    B2.state = 0
    B3.state = 0
    B4.state = 0
    ExtraBall.state = 0
    J4.state = 0
    A5.state = 0
    B5.state = 0
    A3.state = 0
    A4.state = 0
    AB.state = 0
    kickbacklit.state = 0
end if 
End Sub



Sub StartWednesdayWizadrMode()
addscore 25000
StartWizSpotLightSeq
StartWedSpin
GiOff
BallsOnPlayfield = 5
DMDFLush
DMD "dmd baseBlank.png", "", "GO GO GO", 4500
PuPlayer.playlistplayex pBackglass,"WizardMB","WednesdayWizardMB.mp4",92,1
PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
PuPlayer.playlistplayex pMusic,"audiomodes","WednesdayWizardMB.mp3",0,1
'AudioAttractTimer001.Enabled = True
CRACKSTONE.visible =1
WWMBSDMD.visible = 1
WWMBSDMD001.visible = 1
DMDSmall3.visible = 0
WWMBSmallDMDOn.Enabled = True
MBBlocker003.Isdropped = 0
WizardBlocker.Isdropped = 0
WizardSaverKicker.Enabled = True
Drain.Enabled = 0
FireBallDMDTrigger.Enabled = False
BloodRampANITRIGGER.Enabled = False
WizardSPTimer.Enabled = True
EndOfWizareModeTimer.Enabled = True
BloodRepeatTimer.Enabled = True
ActivateKickbak.state = 2
Orbit.Enabled = False
ramptrigger.Enabled = False
Trigger2.Enabled = False
Bumper.Enabled = False
Trigger17.Enabled = False
DiscoPartyDone.state = 2
HYDEMB2Done.state = 2
HydeModeDone.state = 2
WizardLightGlowFade.state = 0
WizardLightGlowFade001.state = 0
WizardLightGlowFade002.state = 0
WizardModeLights.state = 2
WizardModeLights001.state = 2
WizardModeLights002.state = 2
WizardModeLights003.state = 2
WizardModeLights004.state = 2
WizardModeLights005.state = 2
WizardModeLights006.state = 2
WizardModeLights007.state = 2
WizardModeLights008.state = 2
WizardModeLights009.state = 2
WizardModeLights010.state = 2
WizardModeLights012.state = 2
WizardModeLights013.state = 2
WizardModeLights014.state = 2
WizardModeLights015.state = 2
WizardModeLights016.state = 2
WizardModeLights017.state = 2
WizardModeLights018.state = 2
WizardModeLights019.state = 2
WizardModeLights020.state = 2
WizardModeLights021.state = 2
WizardModeLights023.state = 2
WizardModeLights025.state = 2
WizardModeLights030.state = 2
WizardGlow.state = 2
APGlow.state = 1
WizardLight001.state = 2
WizardLight002.state = 2
WizardLight003.state = 2
spidertrigger001.Enabled = True
spidertrigger002.Enabled = True
spidertrigger003.Enabled = True
spidertrigger004.Enabled = True
spidertrigger005.Enabled = True
spidertrigger006.Enabled = True
spidertrigger007.Enabled = True
spidertrigger008.Enabled = True
spidertrigger009.Enabled = True
spidertrigger010.Enabled = True
spidertrigger011.Enabled = True
spidertrigger012.Enabled = True
spidertrigger013.Enabled = True
HYDEmbVideo.Enabled = False
HSJPTimer1.Enabled = False
EndHydeJackP.Enabled = False 
SlimerDrainVideoResetTimer.Enabled = False
SlimerDrainTimer.Enabled = False
StartWheresHMB.Enabled = False 
HSJPTimer.Enabled = False
DiscoPartySuperJPTimer.Enabled = False
DiscoPartyMBTimeoutTimer.Enabled = False
quotetrigger.Enabled = false
quotetrigger001.Enabled = False
VideoQuoteResteTimer.Enabled = False
VideoQuoteResteTimer001.Enabled = False
VideoQuoteResteTimer002.Enabled = False
VideoQuoteResteTimer003.Enabled = False
VideoQuoteResteTimer004.Enabled = False
VideoQuoteResteTimer005.Enabled = False
quotetrigger002.Enabled = False
AudioAttractTimer001.Enabled = False
MBBlocker.Isdropped = 1
MBBlocker001.Isdropped = 1
MBBlocker002.Isdropped = 1
MBBlockerTarget.Enabled = false
MBBlocker001Target.Enabled = false
MBBlocker002Target.Enabled = false
MBBlockerTimer.Enabled = False 
MBBlockerTimer001.Enabled = False
MBBlockerTimer002.Enabled = False
MBBlockerlight001.state = 0
MBBlockerlight002.state = 0
MBBlockerlight003.state = 0
MBBlockerlight004.state = 0
MBBlockerlight005.state = 0
kickbacklit.state = 1
MBBlocker.Isdropped = 0
MBBlocker001.Isdropped = 0
MBBlocker002.Isdropped = 0
CRACKSTONEMoveUp
CStoneActiveOnTimer.Enabled = True
if WizardLight001.state = 2 then
    GiOff
    AddMultiball 5
    quotetrigger.Enabled = False
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False
    QON.Enabled = False
    QON2.Enabled = False
    End If
End Sub



'*************************************************************
'	           END OF WEDNESDAY WIZARD MODE
'*************************************************************



Sub EndWednesdayWizardMode()
'CStoneBoomOnTimer.Enabled = True
'CStoneBoomOffTimer.Enabled = True
EndOfWizardGameTimer.Enabled = True
'DiscoPartyLit.state = 2
'HydeModeDonelit.state = 2
'HYDEMB2Lit.state = 2
Orbit.Enabled = True
ramptrigger.Enabled = True
Trigger2.Enabled = True
Bumper.Enabled = True
Trigger17.Enabled = True
WizardLightGlowFade.state = 2
WizardLightGlowFade001.state = 2
WizardLightGlowFade002.state = 2
WizardModeLights.state = 0
WizardModeLights001.state = 0
WizardModeLights002.state = 0
WizardModeLights003.state = 0
WizardModeLights004.state = 0
WizardModeLights005.state = 0
WizardModeLights006.state = 0
WizardModeLights007.state = 0
WizardModeLights008.state = 0
WizardModeLights009.state = 0
WizardModeLights010.state = 0
WizardModeLights012.state = 0
WizardModeLights013.state = 0
WizardModeLights014.state = 0
WizardModeLights015.state = 0
WizardModeLights016.state = 0
WizardModeLights017.state = 0
WizardModeLights018.state = 0
WizardModeLights019.state = 0
WizardModeLights020.state = 0
WizardModeLights021.state = 0
WizardModeLights023.state = 0
WizardModeLights025.state = 0
WizardModeLights030.state = 0
WizardGlow.state = 0
APGlow.state = 0
WizardLight001.state = 0
WizardLight002.state = 0
WizardLight003.state = 0
spidertrigger001.Enabled = False
spidertrigger002.Enabled = False
spidertrigger003.Enabled = False
spidertrigger004.Enabled = False
spidertrigger005.Enabled = False
spidertrigger006.Enabled = False
spidertrigger007.Enabled = False
spidertrigger008.Enabled = False
spidertrigger009.Enabled = False
spidertrigger010.Enabled = False
spidertrigger011.Enabled = False
spidertrigger012.Enabled = False
spidertrigger013.Enabled = False
DiscoStrobe.StopPlay
End Sub



Sub ClearWednesdayWizardMode()
GIOFFWIZARDTrigger.Enabled = False
GIOFFWIZARDTrigger001.Enabled = false
GIOFFWIZARDTrigger002.Enabled = false
GIOFFWIZARDTrigger003.Enabled = false
GIOFFWIZARDTrigger004.Enabled = false
Orbit.Enabled = True
ramptrigger.Enabled = True
Trigger2.Enabled = True
Bumper.Enabled = True
Trigger17.Enabled = True
DiscoPartyDone.state = 0
HYDEMB2Done.state = 2
HydeModeDone.state = 2
DiscoPartyLit.state = 2
HydeModeDonelit.state = 0
HYDEMB2Lit.state = 0
WizardLightGlowFade.state = 0
WizardLightGlowFade001.state = 0
WizardLightGlowFade002.state = 0
WizardModeLights.state = 0
WizardModeLights001.state = 0
WizardModeLights002.state = 0
WizardModeLights003.state = 0
WizardModeLights004.state = 0
WizardModeLights005.state = 0
WizardModeLights006.state = 0
WizardModeLights007.state = 0
WizardModeLights008.state = 0
WizardModeLights009.state = 0
WizardModeLights010.state = 0
WizardModeLights012.state = 0
WizardModeLights013.state = 0
WizardModeLights014.state = 0
WizardModeLights015.state = 0
WizardModeLights016.state = 0
WizardModeLights017.state = 0
WizardModeLights018.state = 0
WizardModeLights019.state = 0
WizardModeLights020.state = 0
WizardModeLights021.state = 0
WizardModeLights023.state = 0
WizardModeLights025.state = 0
WizardModeLights030.state = 0
WizardGlow.state = 0
APGlow.state = 0
WizardLight001.state = 0
WizardLight002.state = 0
WizardLight003.state = 0
spidertrigger001.Enabled = False
spidertrigger002.Enabled = False
spidertrigger003.Enabled = False
spidertrigger004.Enabled = False
spidertrigger005.Enabled = False
spidertrigger006.Enabled = False
spidertrigger007.Enabled = False
spidertrigger008.Enabled = False
spidertrigger009.Enabled = False
spidertrigger010.Enabled = False
spidertrigger011.Enabled = False
spidertrigger012.Enabled = False
spidertrigger013.Enabled = False
FireBallDMDTrigger.Enabled = True
End Sub


   

Sub WizaedGIOffTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizaedGIOffTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	GiOff
End Sub

Sub WizaedGIOffTimer001_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WizaedGIOffTimer001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	GiOff
  End Sub

sub GIOFFWIZARDTrigger001_hit()
     GiOff
  end sub

sub GIOFFWIZARDTrigger_hit()
     GiOff
  end sub

sub GIOFFWIZARDTrigger002_hit()
     GiOff
     WizaedGIOffTimer001.Enabled = True
  end sub

sub GIOFFWIZARDTrigger003_hit()
     GiOff
     GIOFFWIZARDTrigger003.Enabled = False
  end sub

sub GIOFFWIZARDTrigger004_hit()
     GiOff
     WizaedGIOffTimer001.Enabled = True
  end sub

'*************************************************************
'		Spider Animation Triggers On
'*************************************************************



Sub spidertrigger001_Hit
    GiOff
	SCAnimation.Enabled = True 
    GIUpdateTimer.Enabled = False
End Sub

Sub spidertrigger002_Hit
	SCAnimation.Enabled = True
    GiOff 
    GIUpdateTimer.Enabled = False
 End Sub

Sub spidertrigger003_Hit
	SC001Animation.Enabled = True
    GiOff 
    GIUpdateTimer.Enabled = False
 End Sub

Sub spidertrigger004_Hit
	SC001Animation.Enabled = True
    GiOff 
    GIUpdateTimer.Enabled = False
 End Sub

Sub spidertrigger005_Hit
	SC002Animation.Enabled = True
    GiOff 
    GIUpdateTimer.Enabled = False
 End Sub

Sub spidertrigger006_Hit
	SC002Animation.Enabled = True
    GiOff 
    GIUpdateTimer.Enabled = False
 End Sub

Sub spidertrigger007_Hit
	SC002Animation.Enabled = True
    GiOff 
    GIUpdateTimer.Enabled = False
 End Sub


Sub spidertrigger008_Hit
	SC003Animation.Enabled = True
    GiOff 
    GIUpdateTimer.Enabled = False
 End Sub

Sub spidertrigger009_Hit
	SC003Animation.Enabled = True
    GiOff 
    GIUpdateTimer.Enabled = False
 End Sub

Sub spidertrigger010_Hit
	SC003Animation.Enabled = True
    GiOff 
    GIUpdateTimer.Enabled = False
 End Sub

Sub spidertrigger011_Hit
	SC004Animation.Enabled = True
    GiOff 
    GIUpdateTimer.Enabled = False
  End Sub

Sub spidertrigger012_Hit
	SC004Animation.Enabled = True
    GiOff 
    GIUpdateTimer.Enabled = False
  End Sub

Sub spidertrigger013_Hit
	SC004Animation.Enabled = True
    GiOff 
    GIUpdateTimer.Enabled = False
 End Sub

'*************************************************************
'		Wizard Spotlight Flashing FX
'*************************************************************
Sub StartWizSpotLightSeq()
    'Wizard Spotlights sequences
    WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 25
	WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 25
	WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 25
	WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 25
	WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 1
    WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 25
	WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 25
	WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 25
	WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 25
	WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 1
    WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 25
	WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 25
	WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 25
	WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 25
	WizardSpotSeq.UpdateInterval = 25
    WizardSpotSeq.Play SeqBlinking, , 5, 150
    WizardSpotSeq.Play SeqRandom, 40, , 4000
    WizardSpotSeq.Play SeqAllOff
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqUpOn, 50, 1
    WizardSpotSeq.UpdateInterval = 8
    WizardSpotSeq.Play SeqDownOn, 25, 1
    WizardSpotSeq.UpdateInterval = 8
	WizardSpotSeq.Play SeqCircleOutOn, 15, 2
    WizardSpotSeq.UpdateInterval = 1
End Sub



'WizardSpotSeq.StopPlay




'*****************************************************************************
'   CRACKSTONE WIZARD MULTIBALL ANIMATION AND JACKPOTS AND SUPER JACKPOT
'*****************************************************************************

'*************************
'CRACKSTONE UP/DOWN Animation
'*************************

Dim CRACKSTONEPos, CRACKSTONEDir, CRACKSTONEShakePos, CRACKSTONEShakeDir, CRACKSTONEHitPos, CRACKSTONEHits
Dim bCRACKSTONEUp, bPlayfieldCRACKSTONE

CRACKSTONEPos = 160
CRACKSTONEShakePos = -160

Sub CRACKSTONELocation(param)
    Select Case param
        Case 1:CRACKSTONE.X = 217.3334:CRACKSTONE.Y = 1191.167:CRACKSTONETrigger1.Enabled = 1:CRACKSTONETrigger2.Enabled = 0:CRACKSTONETrigger3.Enabled = 0
        Case 2:CRACKSTONE.X = 472:CRACKSTONE.Y = 1115.166:CRACKSTONETrigger2.Enabled = 1:CRACKSTONETrigger1.Enabled = 0:CRACKSTONETrigger3.Enabled = 0
        Case 3:CRACKSTONE.X = 634:CRACKSTONE.Y = 1148:CRACKSTONETrigger3.Enabled = 1:CRACKSTONETrigger1.Enabled = 0:CRACKSTONETrigger2.Enabled = 0
    End Select
End Sub

Sub CRACKSTONEAnimTimer_Timer()
    CRACKSTONEShakeTimer.Enabled = 0
    CRACKSTONELocationTimer.Enabled = 0
    CRACKSTONEPos = CRACKSTONEPos + CRACKSTONEShakeDir
    'Hyde is moving up
    If CRACKSTONEPos >= 160 Then
        DOF 127, DOFOff
        'Me.Enabled = 0
        CRACKSTONEPos = 160
        CRACKSTONEShakeDir = 1
        CRACKSTONEShakeTimer.Enabled = 1
        CRACKSTONELocationTimer.Enabled = 1
        bCRACKSTONEUp = True
    End If
    'CRACKSTONE is moving down
    If CRACKSTONEPos <= -160 Then
        DOF 127, DOFOff
        'Me.Enabled = 0
        CRACKSTONEPos = -160
        CRACKSTONETrigger1.Enabled = 0
        CRACKSTONETrigger2.Enabled = 0
		CRACKSTONETrigger3.Enabled = 0
     End If
    CRACKSTONE.Transz = CRACKSTONEPos
    'Camera.RotX = - SlimerPos / 4
End Sub

Sub CRACKSTONEShakeTimer_Timer
    CRACKSTONEShakePos = CRACKSTONEShakePos + CRACKSTONEShakeDir
    'CRACKSTONE is moving up
    If CRACKSTONEShakePos > 10 Then
        CRACKSTONEShakeDir = -1
    End If
    'CRACKSTONE is moving down
    If CRACKSTONEShakePos < 0 Then
        CRACKSTONEShakeDir = 1
    End If
    CRACKSTONE.Transz = CRACKSTONEShakePos
    'Camera.RotX = - SlimerShakePos / 4
End Sub

Sub CRACKSTONEMoveUp()
    PlaySound "gb_slimer2"
    PlaySound "vo_slimer1"
    CRACKSTONELocation INT(RND * 3 + 1)
    CRACKSTONEDir = 2
    CRACKSTONEAnimTimer.Enabled = 1
    DOF 127, DOFOn
    'CRACKSTONE.X = 472
    'CRACKSTONE.Y = 1115.166
    CRACKSTONE.X = 431
    CRACKSTONE.Y = 1446.333
    showCStonetimer.Enabled = True
End Sub

Sub CRACKSTONEMoveDown()
    CRACKSTONEDir = -2
    CRACKSTONEAnimTimer.Enabled = 1
    bCRACKSTONEUp = False
    DOF 127, DOFOn
    CRACKSTONE.X = 431
    CRACKSTONE.Y = 1446.333
End Sub


Sub CRACKSTONEHitTimer_Timer()
    CRACKSTONE.TransX = CRACKSTONEHitPos
    If CRACKSTONEHitPos <= 0.1 AND CRACKSTONEHitPos >= -0.1 Then :Exit Sub
    If CRACKSTONEHitPos < 0 Then
        CRACKSTONEHitPos = ABS(CRACKSTONEHitPos)- 0.1
    Else
        CRACKSTONEHitPos = - CRACKSTONEHitPos + 0.1
    End If
End Sub

Sub CRACKSTONELocationTimer_Timer
    CRACKSTONELocation INT(RND * 3 + 1)
End Sub

Sub CRACKSTONEPlayfield() 'lits 2 lights
    If RND < 0.5 Then
        PlaySound "vo_slimeus2"
    Else
        PlaySound "vo_slimeus"
    End If
    bPlayfieldCRACKSTONE = True
    'SetLightColor l61, "green", 2
    'SetLightColor l63, "green", 2
End Sub


Sub CRACKSTONETrigger1_Hit()
        CRACKSTONEHitPos = 6:CRACKSTONEHitTimer.Enabled = 1
        DOF 139, DOFOn
' Kill Hyde 2 JACKPOT during Multiball (if ballsonpf = 2):
addscore 1000000
PuPlayer.playlistplayex pCallouts,"AudioJackpots","Jackpot1.mp3",calloutvol,0
playsound "Hydehit"
DMDFLush
DMD "CStoneJP.png", "", "", 2000
'Hyde.X = 403:Hyde.Y = 429.5
end sub



Sub CRACKSTONETrigger2_Hit()
        CRACKSTONEHitPos = 6:CRACKSTONEHitTimer.Enabled = 1
        DOF 139, DOFOn
' Kill Hyde 2 JACKPOT during Multiball (if ballsonpf = 2):
addscore 1000000
'StopWednesdayWizardMode
playsound "Hydehit"
PuPlayer.playlistplayex pCallouts,"AudioJackpots","Jackpot2.mp3",calloutvol,0
DMDFLush
DMD "CStoneJP.png", "", "", 2000
'Hyde.X = 403:Hyde.Y = 429.5
end sub



Sub CRACKSTONETrigger3_Hit()
        CRACKSTONEHitPos = 6:CRACKSTONEHitTimer.Enabled = 1
        DOF 139, DOFOn
' Kill Hyde 2 JACKPOT during Multiball (if ballsonpf = 2):
addscore 1000000
PuPlayer.playlistplayex pCallouts,"AudioJackpots","Jackpot4.mp3",calloutvol,0
playsound "Hydehit"
DMDFLush
DMD "CStoneJP.png", "", "", 2000
'Hyde.X = 403:Hyde.Y = 429.5
end sub


Sub CRACKSTONESuperJackpotTrigger_Hit()
    addscore 2000000
    WizardSpotSeq.StopPlay
    Timer002.Enabled = True
    CStoneBoomOnTimer.Enabled = True
    CStoneBoomOffTimer.Enabled = True
    CRACKSTONETrigger1.Enabled = 0
    CRACKSTONETrigger2.Enabled = 0
	CRACKSTONETrigger2.Enabled = 0
    WWMBAllSmallDMDsOff
    EndWednesdayWizardMode
    PuPlayer.playlistplayex pCallouts,"audiocallouts","YouWonTheGame.mp3",calloutvol,1
    PuPlayer.playlistplayex pBackglass,"EndOfWizardMode","EndWizardMode.mp4",videovol,1
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
    PuPlayer.playlistplayex pMusic,"audiomodes","EndWizardMode.mp3",0,1
    'WizardMagnet.MagnetOn = True
    'vpmTimer.AddTimer 30, " WizardMagnet.MagnetOn = False '"
    'vpmTimer.AddTimer 35, " WizardMagnet.MagnetOn = False '"
    'vpmTimer.AddTimer 40, " WizardMagnet.MagnetOn = False '"
    'vpmTimer.AddTimer 45, " WizardMagnet.MagnetOn = False '"
    DMDFLush
    DMD "dmd baseBlank.png", "", "", 500
    DMD "WizardCleared.wmv", "", "", 6400
    DMD "dmd baseBlank.png", "", "", 500
    DMD "CaughtLG.png", "", "", 6400
    DMD "dmd baseBlank.png", "", "", 500
    DMD "CaughtHyde.png", "", "", 6400
    DMD "dmd baseBlank.png", "", "", 500
    DMD "CaughtJCS.png", "", "", 6400
    DMD "dmd baseBlank.png", "", "", 500
    playsound "Superjackpot"
    quotetrigger.Enabled = False
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False
    QON2.Enabled = False
    JackpotBlockers001.Isdropped = 0
    JackpotBlockers002.Isdropped = 0
    SPJTargetTimer.Enabled = True
    WizardBallKiller001.Enabled = 1
    WizardBallKiller002.Enabled = 1
    WizardBallKiller003.Enabled = 1
    WizardBallKiller004.Enabled = 1
    WizardBallKiller005.Enabled = 1
    WizardBallKillerTimer.Enabled = True
    WizardSaverKicker.Enabled = False
    CRACKSTONESuperJackpotLight.state = 0
    CRACKSTONESuperJackpotTrigger.Enabled = False
    EndOfWizareModeTimer.Enabled = False
    CRACKSTONETrigger1.Enabled = 0
    CRACKSTONETrigger2.Enabled = 0
    CRACKSTONETrigger3.Enabled = 0
    WizardModeKicker.Enabled = 1
    magickicker.Enabled = False
    BloodRepeatTimer.Enabled = False
    BloodRepeatTimer2.Enabled = False
    'MBBlocker5.Isdropped = 0
    'SJPKicker.Enabled = 1
    SJPBlockerTimer.Enabled = True
 end Sub


Sub showCStonetimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    showCStonetimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    CRACKSTONE.visible =1
 End Sub


Sub NoAudioWizardJPTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NoAudioWizardJPTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	PuPlayer.playlistplayex pMusic,"audiobgrock","",0,1
    NoAudioWizardJPTimer001.Enabled = True
End Sub


Sub NoAudioWizardJPTimer001_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    NoAudioWizardJPTimer001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	PuPlayer.playlistplayex pMusic,"audiobgrock","",0,1
End Sub


'Zombie Animation / shake
'***************************

	Dim CStonePos

	Sub CStoneShake()
		CStonePos = 3
		'DOF 128, DOFPulse  'Shaker Motor
		CStoneShakeTimer.Enabled = True
	End Sub

	Sub CStoneShakeTimer_Timer()
		CRACKSTONE.RotY = CStonePos
        If CStonePos <= 0.1 AND CStonePos >= -0.1 Then Me.Enabled = False:Exit Sub
		If CStonePos < 0 Then
			CStonePos = ABS(CStonePos)- 0.1
		Else
			CStonePos = - CStonePos + 0.1
		End If
	End Sub


Sub CStoneBoomOnTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    CStoneBoomOnTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    CStoneBoomResetTimer.Enabled = True
	CStoneShake
 End Sub

Sub CStoneBoomResetTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    CStoneBoomResetTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    CStoneBoomOnTimer.Enabled = True
	CStoneShake
 End Sub

Sub CStoneBoomOffTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    CStoneBoomOffTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'CStoneBoomOnTimer.Enabled = False
	CStoneBoomResetTimer.Enabled = False
    CRACKSTONEMoveDown
    CStoneexppOn.Enabled = True
    CStoneBoom002Timer.Enabled = True
End Sub


Sub CStoneBoom002Timer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    CStoneBoom002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    'CRACKSTONEAnimTimer.Enabled = 0
    CRACKSTONEShakeTimer.Enabled = 0
    CRACKSTONELocationTimer.Enabled = 0
    bCRACKSTONEUp = False
    CRACKSTONE.visible =0
 End Sub



Sub CStoneActiveOnTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    CStoneActiveOnTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    CStoneBoom002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    CRACKSTONEAnimTimer.Enabled = 0
    CRACKSTONEShakeTimer.Enabled = 1
    CRACKSTONELocationTimer.Enabled = 1
    bCRACKSTONEUp = True
    CRACKSTONE.visible =1
 End Sub


Sub CStoneexppOn_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    CStoneexppOn.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    CStoneAnimation.Enabled = True
    CStoneBoomOnTimer.Enabled = False
    CStoneBoomResetTimer.Enabled = False
 End Sub


'CStoneAnimation.Enabled = True

'*****************************************************************************
' wizard saver nd wizard drain
'*****************************************************************************

Sub WizardSaverKicker_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    WizardSaverKicker.DestroyBall
    thingapronkicker2.CreateBall:thingapronkicker2.Kick -180,5:PlaySound ""
    thingapronkicker2.Kick -180, a
    ThingMagnet.MagnetOn = True
    vpmTimer.AddTimer 3200, " ThingMagnet.MagnetOn = False '"
    PlaySound "zap"
    flashforms  MagicLightning,700, 70, 0
    flashforms  MagicLightning001,800, 70, 0
    'thingsKickerapron.Enabled = False
    'LaunchKicker.Enabled = 1
End Sub

'*****************************************************************************


Sub SingleModeKillerKicker_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    SingleModeKillerKicker.DestroyBall
    drain2.CreateBall:drain2.Kick -180,5:PlaySound ""
    drain2.Kick -180, a
End Sub


Sub SingleModeKillerKicker001_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    SingleModeKillerKicker001.DestroyBall
    drain2.CreateBall:drain2.Kick -180,5:PlaySound ""
    drain2.Kick -180, a
End Sub


Sub SingleModeKillerKicker002_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    SingleModeKillerKicker002.DestroyBall
    drain2.CreateBall:drain2.Kick -180,5:PlaySound ""
    drain2.Kick -180, a
End Sub


Sub SingleModeKillerKicker003_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    SingleModeKillerKicker003.DestroyBall
    drain2.CreateBall:drain2.Kick -180,5:PlaySound ""
    drain2.Kick -180, a
End Sub


Sub SingleModeKillerKicker004_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    SingleModeKillerKicker004.DestroyBall
    drain2.CreateBall:drain2.Kick -180,5:PlaySound ""
    drain2.Kick -180, a
End Sub


Sub SingleModeKillerKicker005_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    SingleModeKillerKicker005.DestroyBall
    drain2.CreateBall:drain2.Kick -180,5:PlaySound ""
    drain2.Kick -180, a
End Sub


Sub SingleModeKillerKicker006_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    SingleModeKillerKicker006.DestroyBall
    drain2.CreateBall:drain2.Kick -180,5:PlaySound ""
    drain2.Kick -180, a
End Sub


Sub SingleModeKillerKicker007_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    SingleModeKillerKicker007.DestroyBall
    drain2.CreateBall:drain2.Kick -180,5:PlaySound ""
    drain2.Kick -180, a
End Sub



Sub SingleModeKillerKicker008_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    SingleModeKillerKicker008.DestroyBall
    drain2.CreateBall:drain2.Kick -180,5:PlaySound ""
    drain2.Kick -180, a
End Sub


Sub SingleModeKillerKicker009_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    SingleModeKillerKicker009.DestroyBall
    drain2.CreateBall:drain2.Kick -180,5:PlaySound ""
    drain2.Kick -180, a
End Sub


Sub SingleModeKillerKicker010_Hit
    Dim a
    a = ABS(ActiveBall.VelY) / 2
    SingleModeKillerKicker010.DestroyBall
    drain2.CreateBall:drain2.Kick -180,5:PlaySound ""
    drain2.Kick -180, a
End Sub


Sub SingleModeKillerTrigger_Hit()
    SingleModeKillerKicker.Enabled = 1
    SingleModeKillerKicker001.Enabled = 1
    SingleModeKillerKicker002.Enabled = 1
    SingleModeKillerKicker003.Enabled = 1
    SingleModeKillerKicker004.Enabled = 1
    SingleModeKillerKicker005.Enabled = 1
    SingleModeKillerKicker006.Enabled = 1
    SingleModeKillerKicker007.Enabled = 1
    SingleModeKillerKicker008.Enabled = 1
    SingleModeKillerKicker009.Enabled = 1
    SingleModeKillerKicker010.Enabled = 1
    SingleModeKicker.Enabled = 1
    SingleKillerTimer.Enabled = True
End Sub


Sub SingleKillerTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    SingleKillerTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SingleModeKillerKicker.Enabled = 0
    SingleModeKillerKicker001.Enabled = 0
    SingleModeKillerKicker002.Enabled = 0
    SingleModeKillerKicker003.Enabled = 0
    SingleModeKillerKicker004.Enabled = 0
    SingleModeKillerKicker005.Enabled = 0
    SingleModeKillerKicker006.Enabled = 0
    SingleModeKillerKicker007.Enabled = 0
    SingleModeKillerKicker008.Enabled = 0
    SingleModeKillerKicker009.Enabled = 0
    SingleModeKillerKicker010.Enabled = 0
    SingleModeKillerTrigger.Enabled = False
End Sub


'*****************************************************************************
'               Fire The Ball DMD Trigger
'*****************************************************************************


Sub FireBallDMDTrigger_Hit()
    DMDFLush
    DMD "FireTheBall.wmv", "", "", 2800
 End Sub


Sub GameVideoStartTrigger_Hit()
    GameVideoStartTrigger.Enabled = False
    quotetrigger.Enabled = False
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = False
    VideoQuoteResteTimer005.Enabled = True
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
    PuPlayer.playlistplayex pMusic,"audiomodes","SoItBegins.mp3",0,1
    PuPlayer.playlistplayex pBackglass,"videosnapstart","SoItBegins.mp4",videovol,1
End Sub


Sub StartAttackMBLightsJP
    'AttackMBLights light sequences
FlashForms WizardModeLights, 1000, 50, 0
FlashForms WizardModeLights001, 1000, 50, 0
FlashForms WizardModeLights002, 1000, 50, 0
FlashForms WizardModeLights003, 1000, 50, 0
FlashForms WizardModeLights004, 1000, 50, 0
FlashForms WizardModeLights005, 1000, 50, 0
FlashForms WizardModeLights006, 1000, 50, 0
FlashForms WizardModeLights007, 1000, 50, 0
FlashForms WizardModeLights008, 1000, 50, 0
FlashForms WizardModeLights009, 1000, 50, 0
FlashForms WizardModeLights010, 1000, 50, 0
FlashForms WizardModeLights023, 1000, 50, 0
FlashForms WizardModeLights012, 1000, 50, 0
FlashForms WizardModeLights013, 1000, 50, 0
FlashForms WizardModeLights014, 1000, 50, 0
FlashForms WizardModeLights015, 1000, 50, 0
FlashForms WizardModeLights016, 1000, 50, 0
FlashForms WizardModeLights025, 1000, 50, 0
FlashForms WizardModeLights017, 1000, 50, 0
FlashForms WizardModeLights018, 1000, 50, 0
FlashForms WizardModeLights019, 1000, 50, 0
FlashForms WizardModeLights020, 1000, 50, 0
FlashForms WizardModeLights021, 1000, 50, 0
FlashForms WizardModeLights022, 1000, 50, 0
FlashForms WizardModeLights030, 1000, 50, 0
End Sub

'AttackMBLights.StopPlay


Sub AttackMBLightsKillTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    AttackMBLightsKillTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	AttackMBLights.StopPlay
 End Sub


Sub AudioAttractTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    AudioAttractTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
 End Sub

Sub AudioAttractTimer001_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    AudioAttractTimer001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	PuPlayer.playlistplayex pMusic,"audiobgrock","",soundtrackvol,0
    PuPlayer.SetLoop 4,0
    'QON.Enabled = True
 End Sub


'blackRavenBig  Animation / shake
'***************************

	Dim TruckPos

	Sub TruckShake()
		TruckPos = 8
		TruckShakeTimer.Enabled = True
	End Sub

	Sub TruckShakeTimer_Timer()
		blackscorpion.RotX = TruckPos
		If TruckPos <= 0.1 AND TruckPos >= -0.1 Then Me.Enabled = False:Exit Sub
		If TruckPos < 0 Then
			TruckPos = ABS(TruckPos)- 0.1
		Else
			TruckPos = - TruckPos + 0.1
		End If
	End Sub



'blackRavenSmall 2  Animation / shake
'***************************************

	Dim RavenSmall2Pos

	Sub RavenSmall2RShake()
		RavenSmall2Pos = 3
		Raven002ShakeTimer.Enabled = True
	End Sub

	Sub Raven002ShakeTimer_Timer()
		Raven002.RotY = RavenSmall2Pos
		If RavenSmall2Pos <= 0.1 AND RavenSmall2Pos >= -0.1 Then Me.Enabled = False:Exit Sub
		If RavenSmall2Pos < 0 Then
			RavenSmall2Pos = ABS(RavenSmall2Pos)- 0.1
		Else
			RavenSmall2Pos = - RavenSmall2Pos + 0.1
		End If
	End Sub


'blackRavenSmall 1  Animation / shake
'***************************************

	Dim RavenSmall1Pos

	Sub RavenSmall1LShake()
		RavenSmall1Pos = 3
		Raven001ShakeTimer.Enabled = True
	End Sub

	Sub Raven001ShakeTimer_Timer()
		Raven001.RotY = RavenSmall1Pos
		If RavenSmall1Pos <= 0.1 AND RavenSmall1Pos >= -0.1 Then Me.Enabled = False:Exit Sub
		If RavenSmall1Pos < 0 Then
			RavenSmall1Pos = ABS(RavenSmall1Pos)- 0.1
		Else
			RavenSmall1Pos = - RavenSmall1Pos + 0.1
		End If
	End Sub


Sub BlackScorpinTrigger_Hit()
   SingleModeKillerTrigger.Enabled = True
End Sub



'Wolf Animation / shake
'***************************

	Dim wolfPos

	Sub wolfShake()
		wolfPos = 3
		WolfMouthTimer.Enabled = True
	End Sub

	Sub WolfMouthTimer_Timer()
		wolfmouth.RotX = wolfPos
     	If wolfPos <= 0.1 AND wolfPos >= -0.1 Then Me.Enabled = False:Exit Sub
		If wolfPos < 0 Then
			wolfPos = ABS(wolfPos)- 0.1
		Else
			wolfPos = - wolfPos + 0.1
		End If
	End Sub



'ScorpionSlingL Animation / shake
'***************************

	Dim ScorpionLPos

	Sub ScorpionLShake()
		ScorpionLPos = 3
		ScorpionSlingLShakeTimer.Enabled = True
	End Sub

	Sub ScorpionSlingLShakeTimer_Timer()
		ScorpionSlingL.RotY = ScorpionLPos
        If ScorpionLPos <= 0.1 AND ScorpionLPos >= -0.1 Then Me.Enabled = False:Exit Sub
		If ScorpionLPos < 0 Then
			ScorpionLPos = ABS(ScorpionLPos)- 0.1
		Else
			ScorpionLPos = - ScorpionLPos + 0.1
		End If
	End Sub


'ScorpionSlingR Animation / shake
'***************************

	Dim ScorpionRPos

	Sub ScorpionRShake()
		ScorpionRPos = 3
		ScorpionSlingRShakeTimer.Enabled = True
	End Sub

	Sub ScorpionSlingRShakeTimer_Timer()
		ScorpionSlingR.RotY = ScorpionRPos
        If ScorpionRPos <= 0.1 AND ScorpionRPos >= -0.1 Then Me.Enabled = False:Exit Sub
		If ScorpionRPos < 0 Then
			ScorpionRPos = ABS(ScorpionRPos)- 0.1
		Else
			ScorpionRPos = - ScorpionRPos + 0.1
		End If
	End Sub


'BigThing Animation / shake
'***************************

	Dim ThingPos

	Sub ThingShake()
		ThingPos = 3
		ThingShakeTimer.Enabled = True
	End Sub

	Sub ThingShakeTimer_Timer()
		Thing.RotY = ThingPos
        Thing001.RotY = ThingPos
        If ThingPos <= 0.1 AND ThingPos >= -0.1 Then Me.Enabled = False:Exit Sub
		If ThingPos < 0 Then
			ThingPos = ABS(ThingPos)- 0.1
		Else
			ThingPos = - ThingPos + 0.1
		End If
	End Sub


Sub ThingLaunchShakerOnTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    ThingLaunchShakerOnTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    ThingLaunchShakerResetTimer.Enabled = True
    thingapronrainbowtimer.Enabled = True
	ThingShake
 End Sub

Sub ThingLaunchShakerResetTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    ThingLaunchShakerResetTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    ThingLaunchShakerOnTimer.Enabled = True
	ThingShake
    
 End Sub

Sub ThingLaunchShakerOffTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    ThingLaunchShakerOffTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    ThingLaunchShakerOnTimer.Enabled = False
	ThingLaunchShakerResetTimer.Enabled = False
    thingapronrainbowtimer.Enabled = True
 End Sub




'LittleThing Animation / shake
'***************************

	Dim ThingLittlePos

	Sub ThingLittleShake()
		ThingLittlePos = 3
		ThingLittleShakeTimer.Enabled = True
	End Sub

	Sub ThingLittleShakeTimer_Timer()
		thing2.RotY = ThingLittlePos
        thing003.RotY = ThingLittlePos
        If ThingLittlePos <= 0.1 AND ThingLittlePos >= -0.1 Then Me.Enabled = False:Exit Sub
		If ThingLittlePos < 0 Then
			ThingLittlePos = ABS(ThingLittlePos)- 0.1
		Else
			ThingLittlePos = - ThingLittlePos + 0.1
		End If
	End Sub





'Zombie Animation / shake
'***************************

	Dim ZombiePos

	Sub ZombieShake()
		ZombiePos = 3
		ZombieShakeTimer.Enabled = True
	End Sub

	Sub ZombieShakeTimer_Timer()
		Zombie.RotY = ZombiePos
        If ZombiePos <= 0.1 AND ZombiePos >= -0.1 Then Me.Enabled = False:Exit Sub
		If ZombiePos < 0 Then
			ZombiePos = ABS(ZombiePos)- 0.1
		Else
			ZombiePos = - ZombiePos + 0.1
		End If
	End Sub


Sub ZombieAttractOnTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    ZombieAttractOnTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    ZombieAttractResetTimer.Enabled = True
	ZombieShake
 End Sub

Sub ZombieAttractResetTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    ZombieAttractResetTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    ZombieAttractOnTimer.Enabled = True
	ZombieShake
 End Sub

Sub ZombieAttractOffTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    ZombieAttractOffTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    ZombieAttractOnTimer.Enabled = False
	ZombieAttractResetTimer.Enabled = False
 End Sub


Sub ZombieAxe001Timer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    ZombieAxe001Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    playsound "sfx_sword1"
    ZombieAxe002Timer.Enabled = True
 End Sub

Sub ZombieAxe002Timer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    ZombieAxe002Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    playsound "sfx_sword1"
    ZombieAxe003Timer.Enabled = True
 End Sub

Sub ZombieAxe003Timer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    ZombieAxe003Timer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    playsound "sfx_sword1"
    ZombieAxe001Timer.Enabled = False
    ZombieAxe002Timer.Enabled = False
 End Sub


'ZombieAxe001Timer_Timer
'ZombieAxe002Timer_Timer
'ZombieAxe003Timer_Timer

'ZombieAttractOnTimer_Timer
'ZombieAttractResetTimer_Timer


'*********************************************************************************
'   Spotlight Attract
'*********************************************************************************
Sub SpotlightAttractOn()
   'Turns on Sptolight Atract NeonGlow
   Spot1Pink.State = 2
   Spot2Pink.State = 2
   Spot3Pink.State = 2
   Spot4Pink.State = 2
   Spot6Pink.State = 2
End Sub


Sub SpotlightAttractOff()
   'Turns on Sptolight Atract NeonGlow
   Spot1Pink.State = 0
   Spot2Pink.State = 0
   Spot3Pink.State = 0
   Spot4Pink.State = 0
   Spot6Pink.State = 0
End Sub


'*********************************************************************************
'   Wednesday and Enid Spin
'*********************************************************************************
Sub StartWedSpin
    WednesdaySword.visible = 1
    Wednesday2.visible = 1
    wed2Spin.Enabled = 1
    rampSpot001.visible = 1
    rampSpot.visible = 0
 End Sub

Sub StopWedSpin
    Wednesday2.visible = 0
    wed2Spin.Enabled = 0
    WednesdaySword.visible = 0
    rampSpot001.visible = 0
    rampSpot.visible = 1
End Sub

Sub wed2Spin_Timer
    Wednesday2.rotz = (Wednesday2.rotz + 1)mod 360
    WednesdaySword.rotz = (WednesdaySword.rotz + 1)mod 360
End Sub


Sub StartEnidSpin
    EnidSpinTimer.Enabled = 1
    Enid_Sinclair2.visible = 0
    Enid_Sinclair.visible = 1
    rampSpot001.visible = 1
    rampSpot.visible = 0
    discoball.visible = 0
    StopLOGOAni
End Sub

Sub StopEnidSpin
    EnidSpinTimer.Enabled = 0
    Enid_Sinclair2.visible = 1
    Enid_Sinclair.visible = 0
    rampSpot001.visible = 0
    rampSpot.visible = 1
    WizardSpotSeq.StopPlay
End Sub

Sub EnidSpinTimer_Timer
    Enid_Sinclair.rotz = (Enid_Sinclair.rotz + 1)mod 360
End Sub




Sub StartLOGOAni
    NMLogoFlasherTimer.Enabled = 1
    NMLOGO.visible = 1
End Sub

Sub StopLOGOAni
    NMLogoFlasherTimer.Enabled = 0
    NMLOGO.visible = 0
End Sub

Sub NMLogoFlasherTimer_Timer
    NMLOGO.roty = (NMLOGO.roty + 2)mod 360
End Sub


   
Sub RestartLoGoHMB_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    RestartLoGoHMB.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    GiOn
End Sub

'*********************************************************************************
'   Swril Attract
'*********************************************************************************
Sub StartSwirl
    SkillSwirl.visible = 1
    SwirlTimer.Enabled = 1
 End Sub

Sub StopSwirl
    SkillSwirl.visible = 0
    SwirlTimer.Enabled = 0
End Sub

Sub SwirlTimer_Timer
    SkillSwirl.rotz = (SkillSwirl.rotz + 1)mod 360
End Sub

Sub EvHMBGIOFFTrigger_Hit
    GiOff
 End Sub


Sub EvHMBGIOFFTrigger001_Hit
    GiOff
 End Sub


Sub EvHMBGIOFFTrigger002_Hit
    GiOff
 End Sub


Sub EvHMBGIOFFTrigger003_Hit
    GiOff
 End Sub


Sub EvHMBGIOFFTrigger004_Hit
    GiOff
    EvHMBGIOFFTTimer.Enabled = True
 End Sub


Sub EvHMBGIOFFTTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    EvHMBGIOFFTTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	EvHMBGIOFFTrigger.Enabled = False
    EvHMBGIOFFTrigger001.Enabled = False
    EvHMBGIOFFTrigger002.Enabled = False
    EvHMBGIOFFTrigger003.Enabled = False
    EvHMBGIOFFTrigger004.Enabled = False
 End Sub

'*********************************************************************************
'   Thing2Triggers and Timers
'*********************************************************************************

Sub thing2magtrigger_Hit
    RightMagnet.MagnetOn = True
    thing2magtimer.Enabled = True
 End Sub

Sub thing2magtrigger2_Hit
    RightMagnet2.MagnetOn = True
    thing2magtimer.Enabled = True
 End Sub

Sub thing2magtimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    thing2magtimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	RightMagnet.MagnetOn = False
 End Sub

Sub thing2magtimer2_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    thing2magtimer2.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	RightMagnet2.MagnetOn = False
 End Sub


Sub multiballholeblockerTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    multiballholeblockerTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
	MBBlocker.Isdropped = 0
    MBBlocker001.Isdropped = 0
    MBBlocker002.Isdropped = 0
    MBBlockerTarget.Enabled = True
    MBBlocker001Target.Enabled = True
    MBBlocker002Target.Enabled = True
 End Sub


Sub HydeKillAttractTime_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    HydeKillAttractTime.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    SlimerMoveDown
 End Sub
'*********************************************************************************

'*********************************************************************************
'   HydeHeadSpin
'*********************************************************************************
Sub Starthydespin
    TheHydesHead.visible = 1
    hydehead.Enabled = 1
    rampSpot001.visible = 1
    rampSpot.visible = 0
    StartWizSpotLightSeq
 End Sub

Sub Stophydespin
    TheHydesHead.visible = 0
    hydehead.Enabled = 0
    rampSpot001.visible = 0
    rampSpot.visible = 1
    WizardSpotSeq.StopPlay
End Sub

Sub hydehead_Timer
    TheHydesHead.rotz = (TheHydesHead.rotz + 1)mod 360
End Sub




Sub FireBallLaunchTriggerOn_Hit
    if BallsOnPlayfield = BallsOnPlayfield + 1 then
    quotetrigger.Enabled = False
    quotetrigger001.Enabled = False
    VideoQuoteResteTimer.Enabled = False
    VideoQuoteResteTimer001.Enabled = False
    VideoQuoteResteTimer002.Enabled = False
    VideoQuoteResteTimer003.Enabled = False
    VideoQuoteResteTimer004.Enabled = True
    VideoQuoteResteTimer005.Enabled = False
    quotetrigger002.Enabled = False
    AudioAttractTimer001.Enabled = False
    QON.Enabled = False
    QON2.Enabled = False
    QON3.Enabled = False
    LeftMagnet.MagnetOn = false
    magickicker.Enabled = False
    PuPlayer.playlistplayex pMusic,"audiobgrock","",0,0
    end If
    end sub


Sub LighteningFlashTimer_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    LighteningFlashTimer.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    LighteningFlashTimer001.Enabled = True 
    flashforms  lightening002,2900, 70, 0
    flashforms  lightening004,2500, 80, 0
    End Sub


Sub LighteningFlashTimer001_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    LighteningFlashTimer001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    LighteningFlashTimer002.Enabled = True 
    flashforms  lightening005,2000, 55, 0
    End Sub


Sub LighteningFlashTimer002_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    LighteningFlashTimer002.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    flashforms  lightening001,2500, 70, 0
    flashforms  lightening002,3000, 30, 0
    flashforms  lightening006,2500, 70, 0
    flashforms  lightening007,3000, 30, 0
    End Sub




'*********************************************************************************
'Wednesday Wizard MB Small DMD Timers
'*********************************************************************************

Sub WWMBSmallDMDOn_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WWMBSmallDMDOn.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    WWMBSDMD.visible = 0
    ShootCStoneSDMD.visible = 1
    WWMBSDMD001.visible = 0
    ShootCStoneSDMD001.visible = 1
    WWMBSmallDMDOff.Enabled = True
  End Sub


Sub WWMBSmallDMDOff_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WWMBSmallDMDOff.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    ShootCStoneSDMD.visible = 0
    CStoneSJPSDMD.visible = 1
    ShootCStoneSDMD001.visible = 0
    CStoneSJPSDMD001.visible = 1
    WWMBSmallDMD001.Enabled = True
  End Sub


Sub WWMBSmallDMD001_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WWMBSmallDMD001.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    CStoneSJPSDMD.visible = 0
    WinGameSDMD.visible = 1
    CStoneSJPSDMD001.visible = 0
    WinGameSDMD001.visible = 1
    WWMBSmallDMD002.Enabled = True
  End Sub


Sub WWMBSmallDMD002_Timer() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WWMBSmallDMD002.Enabled = False 'after 2 1/2 seconds Timer1 stops and finishes the Sub
    WinGameSDMD.visible = 0
    WWMBSDMD.visible = 1
    WinGameSDMD001.visible = 0
    WWMBSDMD001.visible = 1
    WWMBSmallDMDOn.Enabled = True
  End Sub


Sub WWMBAllSmallDMDsOff() 'in timer options look at the interval, its 2500 2 1/2 seconds
    WWMBSDMD.visible = 0
    ShootCStoneSDMD.visible = 0
    CStoneSJPSDMD.visible = 0
    WinGameSDMD.visible = 0
    WWMBSDMD001.visible = 0
    ShootCStoneSDMD001.visible = 0
    CStoneSJPSDMD001.visible = 0
    WinGameSDMD001.visible = 0
    DMDSmall3.visible = 1
    WWMBSmallDMDOn.Enabled = False
    WWMBSmallDMDOff.Enabled = False
    WWMBSmallDMD001.Enabled = False
    WWMBSmallDMD002.Enabled = False
End Sub


