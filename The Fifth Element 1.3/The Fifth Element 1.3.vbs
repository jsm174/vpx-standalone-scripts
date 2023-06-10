Option Explicit
Randomize

'#### ##  ###  ##  ### ###           ### ###    ####   ### ###  #### ##  ###  ##           ### ###  ####     ### ###  ##   ##  ### ###  ###  ##  #### ##  
'# ## ##   ##  ##   ##  ##            ##  ##     ##     ##  ##  # ## ##   ##  ##            ##  ##   ##       ##  ##   ## ##    ##  ##    ## ##  # ## ##  
'  ##      ##  ##   ##                ##         ##     ##        ##      ##  ##            ##       ##       ##      # ### #   ##       # ## #    ##     
'  ##      ## ###   ## ##             ## ##      ##     ## ##     ##      ## ###            ## ##    ##       ## ##   ## # ##   ## ##    ## ##     ##     
'  ##      ##  ##   ##                ##         ##     ##        ##      ##  ##            ##       ##       ##      ##   ##   ##       ##  ##    ##     
'  ##      ##  ##   ##  ##            ##         ##     ##        ##      ##  ##            ##  ##   ##  ##   ##  ##  ##   ##   ##  ##   ##  ##    ##     
' ####    ###  ##  ### ###           ####       ####   ####      ####    ###  ##           ### ###  ### ###  ### ###  ##   ##  ### ###  ###  ##   ####    
                                                                                                                                                         

'******************************************************
'******  TABLE SETUP
'******************************************************

'DOF Config by Outhere
'101 Left Flipper
'102 Right Flipper
'103 Left Slingshot
'104 
'105 Right Slingshot
'106 
'107 Bumper Left
'108 Bumper Center
'109 Bumper Right
'110 Ball Release
'111 AutoPlunger
'112 Drop Target Reset
'113 Drop Target Reset
'114 Drop Target Reset
'115 Knocker
'116 Shaker  (Turntable)
'117 Beacon  (Turntable)
'118 Strobe  (Drop Target Reset)

Const BallSize = 50
Const BallMass = 1.7
Const FlexDMDHighQuality = True

'/////////////////////////////  LOAD CORE.VBS  ////////////////////////////

LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
    On Error Goto 0
End Sub

'/////////////////////////////  CONSTANTS AND GLOBAL VARIABLES  ////////////////////////////

Const TNOB = 4 'Total number of balls.
Const MaxVel = 45 'Maximum ball velocity: lower is slower.
Const VolumeDial = 0.8 'Sound volume.
Const SongVolume = 0.8 'Music volume.

Const cGameName = "5th_Element"
Const B2STableName = "The Fifth Element"
Const myVersion = "1.00"

Const MaxPlayers = 4
Const MaxMultiplier = 1 'Maximum playfield score multiplier.
Const BallsPerGame = 3
Const MaxMultiballs = 4
Const MaxBonusMultiplier = 7

Const MaxOrbitLevel = 10 'Multipass letters.
Const MaxOrbitAwardLevel = 9 '9 different Multipass awards.

Const MaxOrbitManiaLevel = 5
Const MaxDTFrenzyLevel = 5
Const MaxSuperSpinnersLevel = 5
Const MaxSuperPopsLevel = 5

Dim TableWidth: TableWidth = Fluffy.Width
Dim TableHeight: TableHeight = Fluffy.Height

Dim EnableBallControl
EnableBallControl = False

'/////////////////////////////  USE FLEXDMD IF IN FS MODE  ////////////////////////////

Dim UseFlexDMD

If Fluffy.ShowDT = True Then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If

'/////////////////////////////  DEFINE GLOBAL VARIABLES  ////////////////////////////

Dim BallSaverTime
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim Bonus
Dim BonusMultiplier(4)
Dim AwardPoints
Dim TotalBonus
Dim PlayfieldMultiplier(4)
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot(4)
Dim SuperJackpot
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim bAutoPlunger
Dim bInstantInfo
Dim bAttractMode
Dim x

'/////////////////////////////  ORBIT AWARDS  ////////////////////////////

Dim OrbitLevel
Dim OrbitAwardLevel(4)
Dim StoreOrbitLevel(4)

'/////////////////////////////  MODES & SHOT VALUES  ////////////////////////////

Dim OrbitValue(4)
Dim OrbitMultiplier(4)
Dim DropTargetValue(4)
Dim SpinnerValue(4)
Dim SpinnerMultiplier(4)
Dim BumperValue(4)
Dim BumperMultiplier(4)

Dim SuperPopsValue(4)
Dim SuperPopsHits
Dim SuperPopsLevel(4)
Dim SuperSpinnersValue(4)
Dim SuperSpinnersFirstHitValue(4)
Dim SuperSpinnersHits
Dim SuperSpinnersLevel(4)
Dim DTFrenzyValue(4)
Dim DTFrenzyHits
Dim DTFrenzyLevel(4)
Dim OrbitManiaValue(4)
Dim OrbitManiaHits
Dim OrbitManiaLevel(4)
Dim MultiballHits

'/////////////////////////////  SKILLSHOT & OTHER VALUES  ////////////////////////////

Dim DoomValue(4)
Dim ScoreBonus

Dim SkillshotValue(4)
Dim LoopSkillshotValue(4)
Dim SuperSkillshotValue(4)
Dim DoomSkillshotValue(4)

'/////////////////////////////  DEFINE GAME CONTROL VARIABLES  ////////////////////////////

Dim LastSwitchHit
Dim OrbitLastDirection
Dim BallsOnPlayfield
Dim EndModeCountdown

'/////////////////////////////  DEFINE GAME FLAGS  ////////////////////////////

Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bMusicOn
Dim bSkillshotReady
Dim bExtraBallIsLit
Dim bExtraBallWonThisBall
Dim bJackpot
Dim bMagnetOn
Dim bDoomFailed
Dim bEternalDone1
Dim bEternalDone2
Dim bModeReady
Dim bModeOn
Dim b2BallMultiball
Dim bSuperPops
Dim bSuperSpinners
Dim bDropTargetFrenzy
Dim bOrbitMania
Dim bOrbitAwardReady
Dim b2BallMultiballFinished(4)
Dim bOrbitManiaFinished(4)
Dim bDTFrenzyFinished(4)
Dim bSuperSpinnersFinished(4)
Dim bSuperPopsFinished(4)
Dim bWizardModeReady(4)
Dim bWizardMode
Dim bJackpotReady
Dim bScoreBonusLit

'/////////////////////////////  CORE VBS VARIABLES  ////////////////////////////

Dim AUTOPLUNGER
Dim MAGNET
Dim TURNTABLE_DISC

'****** PuP Variables ******

Dim usePUP: Dim cPuPPack: Dim PuPlayer: Dim PUPStatus: PUPStatus=false ' dont edit this line!!!

'*************************** PuP Settings for this table ********************************

usePUP   = true               ' enable Pinup Player functions for this table
cPuPPack = "5th_Element"    ' name of the PuP-Pack / PuPVideos folder for this table

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

'************ PuP-Pack Startup **************

PuPStart(cPuPPack) 'Check for PuP - If found, then start Pinup Player / PuP-Pack

'******************************************************
'******  END TABLE SETUP
'******************************************************

'******************************************************
'******  TABLE INIT AND EXIT
'******************************************************

Sub Fluffy_Init

'/////////////////////////////  LAUNCH VPM  ////////////////////////////


    Dim i
    Randomize

	If B2SOn then
        Set Controller = CreateObject("B2S.Server")
        Controller.B2SName = B2STableName
        Controller.Run()
        If Err Then MsgBox "Can't Load B2S.Server."
    End If

'/////////////////////////////  IMPULSE PLUNGER AS AUTOPLUNGER  ////////////////////////////

    Const IMPowerSetting = 45 ' Plunger Power
    Const IMTime = 0.5        ' Time in seconds for Full Plunge
    Set AUTOPLUNGER = New cvpmImpulseP
    With AUTOPLUNGER
        .InitImpulseP T_AUTOPLUNGER, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "AUTOPLUNGER"
    End With

'/////////////////////////////  SETUP MAGNET  ////////////////////////////

    Set MAGNET = New cvpmMagnet
    With MAGNET
        .InitMagnet T_MAGNET, 35
        .GrabCenter = True
        .CreateEvents "MAGNET"
    End With

'/////////////////////////////  TURN OFF BUMPER LIGHTS  ////////////////////////////

	FlBumperFadeTarget(1) = 0 : FlBumperFadeTarget(2) = 0 : FlBumperFadeTarget(3) = 0

'/////////////////////////////  SETUP TURNTABLE  ////////////////////////////

	Set TURNTABLE_DISC = New cvpmTurntable
		TURNTABLE_DISC.InitTurntable TRIGGER_TURNTABLE, 25
		TURNTABLE_DISC.SpinUp = 1000
		TURNTABLE_DISC.SpinDown = 10
		TURNTABLE_DISC.CreateEvents "TURNTABLE_DISC"

'/////////////////////////////  LOAD SAVED VALUES  ////////////////////////////

    Credits = 0
    LoadHighScore

'/////////////////////////////  INIT DMD  ////////////////////////////

    DMD_Init

'/////////////////////////////  FREEPLAY OR COINS  ////////////////////////////

    bFreePlay = False

'/////////////////////////////  INIT MAIN VARIABLES AND OTHER FLAGS  ////////////////////////////

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
    LastSwitchHit = ""
	OrbitLastDirection = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bJackpot = False
    bInstantInfo = False
    bMagnetOn = False
    bDoomFailed = False
    bEternalDone1 = False
    bEternalDone2 = False
    bModeReady = False
	bModeOn = False
	b2BallMultiball = False
	bSuperPops = False
	bSuperSpinners = False
	bDropTargetFrenzy = False
	bOrbitMania = False
    bOrbitAwardReady = False
	bJackpotReady = False
	bScoreBonusLit = False

    GiOff
    StartAttractMode

    RealTime.Enabled = 1
    LoadEM

End Sub

'******************************************************
'******  END TABLE INIT AND EXIT
'******************************************************

'******************************************************
'******  KEYPRESSES
'******************************************************

'/////////////////////////////  KEY DOWN  ////////////////////////////

Sub Fluffy_KeyDown(ByVal Keycode)

'/////////////////////////////  PLUNGER KEY  ////////////////////////////

	If Keycode = PlungerKey Then Plunger.Pullback : SoundPlungerPull

'/////////////////////////////  CHANGE MUSIC WITH MAGNASAVE BUTTONS  ////////////////////////////

    If Keycode = RightMagnaSave Then :pupevent 816
    If Keycode = LeftMagnaSave  Then :pupevent 816

	If Keycode = RightMagnaSave AND PUPStatus=false Then ChangeRandomSong
    If Keycode = LeftMagnaSave AND PUPStatus=false Then ChangeRandomSong
	

'/////////////////////////////  ADD CREDITS  ////////////////////////////

    If Keycode = AddCreditKey or Keycode = AddCreditKey2 Then
        If Credits < 99 Then Credits = Credits + 1
        If(Tilted = False)Then
            DMDFlush
            DMD "", CL("CREDITS " & Credits), "", eNone, eNone, eNone, 500, True, ""
            If NOT bGameInPlay Then ShowTableInfo
        End If
		'PlaySoundAtLevelStatic ("SFX_Coin_1"), SFXSoundLevel, BULB_DS_6
		Select Case Int(rnd*3)
			Case 0: PlaySound ("Coin_In_1"), 0, CoinSoundLevel, 0, 0.25
			Case 1: PlaySound ("Coin_In_2"), 0, CoinSoundLevel, 0, 0.25
			Case 2: PlaySound ("Coin_In_3"), 0, CoinSoundLevel, 0, 0.25
		End Select
    End If

'/////////////////////////////  HIGHSCORE  ////////////////////////////

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

'/////////////////////////////  NUDGING  ////////////////////////////

	If Keycode = LeftTiltKey Then Nudge 90, 8 : SoundNudgeLeft
	If Keycode = RightTiltKey Then Nudge 270, 8 : SoundNudgeRight
	If Keycode = CenterTiltKey Then Nudge 0, 9 : SoundNudgeCenter

'/////////////////////////////  MANUAL BALL CONTROL  ////////////////////////////

	If Keycode = 46 Then
		If EnableBallControl = 1 Then
			EnableBallControl = 0
		Else
			EnableBallControl = 1
		End If
	End If

    If EnableBallControl = 1 Then
		If keycode = 48 Then
			If BCboost = 1 Then
				BCboost = BCboostmulti
			Else
				BCboost = 1
			End If
		End If

		If Keycode = 203 Then BCLeft = 1
		If Keycode = 200 Then BCUp = 1
		If Keycode = 208 Then BCDown = 1
		If Keycode = 205 Then BCRight = 1

	End If

'/////////////////////////////  GAME IN PROGRESS  ////////////////////////////

    If bGameInPlay AND NOT Tilted Then

        If Keycode = LeftTiltKey Then CheckTilt
        If Keycode = RightTiltKey Then CheckTilt
        If Keycode = CenterTiltKey Then CheckTilt

        If Keycode = LeftFlipperKey Then SolLFlipper True : INSTANTINFOTIMER.Enabled = True
        If Keycode = RightFlipperKey Then SolRFlipper True : INSTANTINFOTIMER.Enabled = True

        If Keycode = StartGameKey Then
			If usePUP=false then ChangeSongGameOver
			If usePUP=true then:pupevent 800
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True)) Then
				
                If(bFreePlay = True) Then
					RandomSoundPlayerJoins()
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                Else
                    If(Credits > 0) Then
						RandomSoundPlayerJoins()
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 1000, True, ""
                        Else
                            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, ""
							PlaySoundAtLevelStatic ("VO_Pay"), VoiceOverSoundLevel, BULB_DS_6
							
                    End If
                End If
            End If
        End If

        Else

            If Keycode = StartGameKey Then
				pupevent 800
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetForNewGame()
						RandomSoundPlayerJoins()
                    End If
                Else
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            ResetForNewGame()
							RandomSoundPlayerJoins()
                        End If
                    Else
                        DMDFlush
                        DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 1000, True, ""
						PlaySoundAtLevelStatic ("VO_Pay"), VoiceOverSoundLevel, BULB_DS_6
                        ShowTableInfo
                    End If
                End If
            End If
    End If

End Sub

'/////////////////////////////  END KEY DOWN  ////////////////////////////

'/////////////////////////////  KEY UP  ////////////////////////////

Sub Fluffy_KeyUp(ByVal Keycode)

'/////////////////////////////  PLUNGER KEY  ////////////////////////////

	If KeyCode = PlungerKey Then
		Plunger.Fire
		If bBallInPlungerLane = True Then
			SoundPlungerReleaseBall()
		Else
			SoundPlungerReleaseNoBall()
		End If
	End If

'/////////////////////////////  HIGHSCORE  ////////////////////////////

    If hsbModeActive Then
        Exit Sub
    End If

'/////////////////////////////  GAME IN PROGRESS  ////////////////////////////

    If bGameInPLay AND NOT Tilted Then
        If Keycode = LeftFlipperKey Then
            SolLFlipper False
            INSTANTINFOTIMER.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
        If Keycode = RightFlipperKey Then
            SolRFlipper False
            INSTANTINFOTIMER.Enabled = False
            If bInstantInfo Then
                DMDScoreNow
                bInstantInfo = False
            End If
        End If
    End If

'/////////////////////////////  MANUAL BALL CONTROL  ////////////////////////////

	If EnableBallControl = 1 Then
		If Keycode = 203 Then BCleft = 0
		If Keycode = 200 Then BCup = 0
		If Keycode = 208 Then BCdown = 0
		If Keycode = 205 Then BCright = 0
	End If

End Sub

'/////////////////////////////  END KEY UP  ////////////////////////////

'******************************************************
'******  END KEYPRESSES
'******************************************************

'******************************************************
'******  PAUSE AND EXIT TABLE
'******************************************************

Sub Fluffy_Paused
End Sub

Sub Fluffy_UnPaused
End Sub

Sub Fluffy_Exit
    SaveHighScore
    If UseFlexDMD Then FlexDMD.Run = False
    If B2SOn = True Then Controller.Stop
End Sub

'******************************************************
'******  END PAUSE AND EXIT TABLE
'******************************************************

'******************************************************
'******  TILT
'******************************************************

Sub CheckTilt
    If BallsOnPlayfield = 0 Then Exit Sub
    Tilt = Tilt + TiltSensitivity
    If (Tilt > TiltSensitivity) And (Tilt <= 15) Then
        DMD "_", CL("CAREFUL"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
		RandomSoundTiltWarning()
    End If
    If(NOT Tilted)AND Tilt > 15 Then
        INSTANTINFOTIMER.Enabled = False
        DMDFlush
        DMD CL("YOU"), CL("TILTED"), "", eNone, eNone, eNone, 200, False, ""
        DisableTable True
        TILTRECOVERYTIMER.Enabled = True
        bMultiBallMode = False
		STOPMODE
		STOPMULTIBALL
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        Tilted = True
        GiOff
        LEFTFLIPPER.RotateToStart
        LEFTUPPERFLIPPER.RotateToStart
        RIGHTFLIPPER.RotateToStart
        RIGHTUPPERFLIPPER.RotateToStart
        BUMPER1.Threshold = 100
        BUMPER2.Threshold = 100
        BUMPER3.Threshold = 100
        LEFTSLINGSHOT.Disabled = 1
        RIGHTSLINGSHOT.Disabled = 1
        LIGHTSEQTILT.Play SeqAllOff
    Else
        Tilted = False
        GiOn
        BUMPER1.Threshold = 1.3
        BUMPER2.Threshold = 1.3
        BUMPER3.Threshold = 1.3
        LEFTSLINGSHOT.Disabled = 0
        RIGHTSLINGSHOT.Disabled = 0
        LIGHTSEQTILT.StopPlay
        DMDFlush
    End If
End Sub

Sub TILTRECOVERYTIMER_Timer()
    If(BallsOnPlayfield = 0)Then
        vpmtimer.Addtimer 2000, "EndOfBall() '"
        TILTRECOVERYTIMER.Enabled = False
    End If
End Sub

'******************************************************
'******  END TILT
'******************************************************

'******************************************************
'******  FLIPPERS
'******************************************************

Const ReflipAngle = 20

Sub SolLFlipper(Enabled)
	If Enabled Then
		LEFTFLIPPER.RotateToEnd
		LEFTUPPERFLIPPER.RotateToEnd
        DOF  101, DOFOn
		If LEFTFLIPPER.CurrentAngle < LEFTFLIPPER.EndAngle + ReflipAngle Then 
			RandomSoundReflipUpLeft LEFTFLIPPER
		Else 
			SoundFlipperUpAttackLeft LEFTFLIPPER
			RandomSoundFlipperUpLeft LEFTFLIPPER
		End If		
	Else
		LEFTFLIPPER.RotateToStart
		LEFTUPPERFLIPPER.RotateToStart
        DOF  101, DOFOFF
		If LEFTFLIPPER.CurrentAngle < LEFTFLIPPER.StartAngle - 5 Then
			RandomSoundFlipperDownLeft LEFTFLIPPER
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolRFlipper(Enabled)
	If Enabled Then
		RIGHTFLIPPER.RotateToEnd
		RIGHTUPPERFLIPPER.RotateToEnd
        DOF  102, DOFOn
		If RIGHTFLIPPER.CurrentAngle > RIGHTFLIPPER.EndAngle - ReflipAngle Then
			RandomSoundReflipUpRight RIGHTFLIPPER
		Else 
			SoundFlipperUpAttackRight RIGHTFLIPPER
			RandomSoundFlipperUpRight RIGHTFLIPPER
		End If
	Else
		RIGHTFLIPPER.RotateToStart
		RIGHTUPPERFLIPPER.RotateToStart
        DOF  102, DOFOFF
		If RIGHTFLIPPER.CurrentAngle > RIGHTFLIPPER.StartAngle + 5 Then
			RandomSoundFlipperDownRight RIGHTFLIPPER
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub LEFTFLIPPER_Collide(parm)
	LeftFlipperCollide parm
End Sub

Sub RIGHTFLIPPER_Collide(parm)
	RightFlipperCollide parm
End Sub

'******************************************************
'******  END FLIPPERS
'******************************************************

'******************************************************
'******  GI EFFECTS
'******************************************************

Dim OldGiState
OldGiState = 0

Sub GIUPDATETIMER_Timer
    If BallsOnPlayfield <> OldGiState Then
        OldGiState = BallsOnPlayfield
        If BallsOnPlayfield = 0 Then
            GiOff
        Else
            GiOn
        End If
    End If
End Sub

Sub GiOn
	PlaySoundAtLevelStatic ("Relay_Flashers_On"), FlasherSoundLevel, TRIGGER_TURNTABLE
    Dim Bulb
    For Each Bulb in GI
        Bulb.State = 1
    Next
End Sub

Sub GiOff
	PlaySoundAtLevelStatic ("Relay_Flashers_Off"), FlasherSoundLevel, TRIGGER_TURNTABLE
    Dim Bulb
    For Each Bulb in GI
        Bulb.State = 0
    Next
End Sub

'******************************************************
'******  END GI EFFECTS
'******************************************************

'******************************************************
'******  USER DEFINED SCRIPT EVENTS
'******************************************************

Sub ResetForNewGame()

    Dim i

    bGameInPLay = True

    StopAttractMode
    GiOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        Bonus = 0
        BonusMultiplier(i) = 1
        PlayfieldMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

    Tilt = 0

    Game_Init()

	FirstBall()

End Sub

Sub FirstBall
    ResetForNewPlayerBall()
    CreateNewBall()
End Sub

Sub ResetForNewPlayerBall()

    AddScore 0
    SetBonusMultiplier 1
    SetPlayfieldMultiplier 1
    Bonus = 0
    SuperJackpot = 0
	OrbitLastDirection = ""
	OrbitLevel = StoreOrbitLevel(CurrentPlayer)
	AddOrbit(0)
	ScoreBonus = 0
	bScoreBonusLit = False

    ResetNewBallVariables

    bBallSaverReady = True
    bSkillShotReady = True
	bExtraBallIsLit = False
	bDoomFailed = False
    bEternalDone1 = False
    bEternalDone2 = False
    bModeReady = False
	bModeOn = False
	b2BallMultiball = False
    bSuperPops = False
    bSuperSpinners = False
    bDropTargetFrenzy = False
    bOrbitMania = False
    bOrbitAwardReady = False
	bJackpotReady = False

	If b2BallMultiballFinished(CurrentPlayer) = True Then L001.State = 1
	If bOrbitManiaFinished(CurrentPlayer) = True Then L002.State = 1
	If bDTFrenzyFinished(CurrentPlayer) = True Then L003.State = 1
	If bSuperSpinnersFinished(CurrentPlayer) = True Then L004.State = 1
	If bSuperPopsFinished(CurrentPlayer) = True Then L005.State = 1
	If bWizardModeReady(CurrentPlayer) = True Then L015.State = 2

	If bMagnetOn = True Then TURNMAGNETOFF

	RESET_ALL_DROPS
	SELECTMODE

	'PlaySoundAtLevelStatic ("SFX_Start_Game"), SFXSoundLevel, BULB_DS_6
	'ChangeSongGameStart

End Sub

Sub CreateNewBall()

	BALLRELEASE.CreateBall
    BallsOnPlayfield = BallsOnPlayfield + 1

	RandomSoundBallRelease BALLRELEASE
	BALLRELEASE.Kick 90, 7
  DOF 110, DOFPulse
    If BallsOnPlayfield > 1 Then 
        bMultiBallMode = True
        bAutoPlunger = True
    End If

End Sub

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
End Sub

Sub CreateMultiballTimer_Timer()

    If bBallInPlungerLane Then
		Exit Sub
	Else
        If BallsOnPlayfield < MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then
                CreateMultiballTimer.Enabled = False
            End If
        Else
            mBalls2Eject = 0
            CreateMultiballTimer.Enabled = False
        End If
    End If
End Sub

Sub EndOfBall()

    AwardPoints = 0
    TotalBonus = 0

	TurnOffLightsForBonus
	StoreOrbitLevel(CurrentPlayer) = OrbitLevel
	If StoreOrbitLevel(CurrentPlayer) = MaxOrbitLevel Then StoreOrbitLevel(CurrentPlayer) = 9

    bOnTheFirstBall = False

    If NOT Tilted Then
        BONUSCOUNTTIMER.Enabled = 1
		'ChangeSongBonus
    Else
        vpmtimer.addtimer 200, "EndOfBall2 '"
    End If
End Sub

Sub BONUSCOUNTTIMER_Timer
    If Bonus > 0 Then
		FLASH1 True
        Bonus = Bonus - 1
        UpdateBonusLights
        AwardPoints = AwardPoints + 1000
		DMDFlush
        DMD CL("BONUS"), CL(FormatScore(AwardPoints)), "", eNone, eNone, eNone, 1000, True, ""
    Else
        BONUSCOUNTTIMER.Enabled = 0
        DMD CL("BONUS X MULTIPLIER"), CL(FormatScore(AwardPoints) & " X " & BonusMultiplier(CurrentPlayer)), "", eNone, eNone, eNone, 2000, True, "SFX_Bonus_End_1"
        TotalBonus = AwardPoints * BonusMultiplier(CurrentPlayer)
        DMD CL("TOTAL BONUS"), CL(FormatScore(TotalBonus)), "", eNone, eNone, eNone, 4500, True, "SFX_Bonus_End_1"
        AddScoreNoMultiplier TotalBonus
        vpmtimer.addtimer 6000, "EndOfBall2 '"
    End If
End Sub

Sub UpdateBonusLights
    Select Case Bonus
        Case 0: L024.State = 0 : L025.State = 0 : L026.State = 0 : L027.State = 0 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 0 : L034.State = 0
        Case 1: L024.State = 1 : L025.State = 0 : L026.State = 0 : L027.State = 0 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 0 : L034.State = 0
        Case 2: L024.State = 1 : L025.State = 1 : L026.State = 0 : L027.State = 0 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 0 : L034.State = 0
        Case 3: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 0 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 0 : L034.State = 0
        Case 4: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 0 : L034.State = 0
        Case 5: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 0 : L034.State = 0
        Case 6: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 1 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 0 : L034.State = 0
        Case 7: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 1 : L030.State = 1 : L031.State = 0 : L032.State = 0 : L033.State = 0 : L034.State = 0
        Case 8: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 1 : L030.State = 1 : L031.State = 1 : L032.State = 0 : L033.State = 0 : L034.State = 0
        Case 9: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 1 : L030.State = 1 : L031.State = 1 : L032.State = 1 : L033.State = 0 : L034.State = 0
        Case 10: L024.State = 0 : L025.State = 0 : L026.State = 0 : L027.State = 0 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 0
        Case 11: L024.State = 1 : L025.State = 0 : L026.State = 0 : L027.State = 0 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 0
        Case 12: L024.State = 1 : L025.State = 1 : L026.State = 0 : L027.State = 0 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 0
        Case 13: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 0 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 0
        Case 14: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 0
        Case 15: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 0
        Case 16: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 1 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 0
        Case 17: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 1 : L030.State = 1 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 0
        Case 18: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 1 : L030.State = 1 : L031.State = 1 : L032.State = 0 : L033.State = 1 : L034.State = 0
        Case 19: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 1 : L030.State = 1 : L031.State = 1 : L032.State = 1 : L033.State = 1 : L034.State = 0
        Case 20: L024.State = 0 : L025.State = 0 : L026.State = 0 : L027.State = 0 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 1
        Case 21: L024.State = 1 : L025.State = 0 : L026.State = 0 : L027.State = 0 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 1
        Case 22: L024.State = 1 : L025.State = 1 : L026.State = 0 : L027.State = 0 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 1
        Case 23: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 0 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 1
        Case 24: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 0 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 1
        Case 25: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 0 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 1
        Case 26: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 1 : L030.State = 0 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 1
        Case 27: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 1 : L030.State = 1 : L031.State = 0 : L032.State = 0 : L033.State = 1 : L034.State = 1
        Case 28: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 1 : L030.State = 1 : L031.State = 1 : L032.State = 0 : L033.State = 1 : L034.State = 1
        Case 29: L024.State = 1 : L025.State = 1 : L026.State = 1 : L027.State = 1 : L028.State = 1 : L029.State = 1 : L030.State = 1 : L031.State = 1 : L032.State = 1 : L033.State = 1 : L034.State = 1
    End Select
End Sub

Sub EndOfBall2()
    Tilt = 0
    DisableTable False

    If ExtraBallsAwards(CurrentPlayer) > 0 Then
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            L008.State = 0
        End If

        DMD CL("EXTRA BALL"), CL("SHOOT AGAIN"), "", eNone, eBlink, eNone, 1500, True, ""
		RandomSoundShootAgain()

        ResetForNewPlayerBall()
        CreateNewBall()

    Else

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1
		bExtraBallWonThisBall = False

        If(BallsRemaining(CurrentPlayer) <= 0)Then
            CheckHighScore()

        Else
            EndOfBallComplete()
        End If
    End If
End Sub

Sub EndOfBallComplete()

    Dim NextPlayer

    If(PlayersPlayingGame > 1)Then
        NextPlayer = CurrentPlayer + 1
        If(NextPlayer > PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then

        EndOfGame()

    Else

        CurrentPlayer = NextPlayer
        DMDScoreNow
        ResetForNewPlayerBall()
        CreateNewBall()

        If PlayersPlayingGame > 1 Then
            Select Case CurrentPlayer
                Case 1: DMD "", CL("PLAYER 1"), "", eNone, eNone, eNone, 1000, True, "" : PlaySoundAtLevelStatic ("VO_Player_1"), VoiceOverSoundLevel, BULB_DS_6
                Case 2: DMD "", CL("PLAYER 2"), "", eNone, eNone, eNone, 1000, True, "" : PlaySoundAtLevelStatic ("VO_Player_2"), VoiceOverSoundLevel, BULB_DS_6
                Case 3: DMD "", CL("PLAYER 3"), "", eNone, eNone, eNone, 1000, True, "" : PlaySoundAtLevelStatic ("VO_Player_3"), VoiceOverSoundLevel, BULB_DS_6
                Case 4: DMD "", CL("PLAYER 4"), "", eNone, eNone, eNone, 1000, True, "" : PlaySoundAtLevelStatic ("VO_Player_4"), VoiceOverSoundLevel, BULB_DS_6
            End Select
        Else
            DMD "", CL("PLAYER 1"), "", eNone, eNone, eNone, 1000, True, "" : PlaySoundAtLevelStatic ("VO_Player_1"), VoiceOverSoundLevel, BULB_DS_6
        End If
    End If
End Sub

Sub EndOfGame()

    bGameInPLay = False

    SolLFlipper False
    SolRFlipper False

    GiOff
    StartAttractMode

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

'******************************************************
'******  END USER DEFINED SCRIPT EVENTS
'******************************************************

'******************************************************
'******  TIMERS
'******************************************************

'/////////////////////////////  GAME TIMER  ////////////////////////////

Sub GAMETIMER_Timer()
	COR.Update
	RollingUpdate
End Sub

'/////////////////////////////  INSTANT INFO TIMER  ////////////////////////////

Sub INSTANTINFOTIMER_Timer
    INSTANTINFOTIMER.Enabled = False
    If NOT hsbModeActive Then
        bInstantInfo = True
        DMDFlush
        InstantInfo
    End If
End Sub

'******************************************************
'******  END TIMERS
'******************************************************

'******************************************************
'******  TRACK BALL VELOCITIES
'******************************************************

Dim COR : Set COR = New CoRTracker

Class CoRTracker
	Public BallVel, BallVelX, BallVelY

	Private Sub Class_Initialize : ReDim BallVel(0) : ReDim BallVelX(0): ReDim BallVelY(0) : End Sub 

	Public Sub Update()
		Dim str, b, AllBalls, HighestID : AllBalls = GetBalls

		For Each b In AllBalls
			If b.ID >= HighestID Then HighestID = b.ID
		Next

		If uBound(BallVel) < highestID Then ReDim BallVel(HighestID)
		If uBound(BallVelX) < highestID Then ReDim BallVelX(HighestID)
		If uBound(BallVelY) < highestID Then ReDim BallVelY(HighestID)

		For Each b In AllBalls
			BallVel(b.ID) = BallSpeed(b)
			BallVelX(b.ID) = b.VelX
			BallVelY(b.ID) = b.VelY
		Next
	End Sub
End Class

Function BallSpeed(Ball)
	BallSpeed = SQR(Ball.VelX^2 + Ball.VelY^2 + Ball.VelZ^2)
End Function

'******************************************************
'******  END TRACK BALL VELOCITIES
'******************************************************

'******************************************************
'******  DRAIN / PLUNGER FUNCTIONS
'******************************************************

Sub Drain_Hit()
	DRAIN.DestroyBall
    If bGameInPLay = False Then Exit Sub
    BallsOnPlayfield = BallsOnPlayfield - 1
	RandomSoundDrain DRAIN
	
    If Tilted Then
        StopEndOfBallMode
    End If

    If(bGameInPlay = True)AND(Tilted = False) Then

        If(bBallSaverActive = True) Then
			If bMultiBallMode = False Then BALLSAVERTIMEREXPIRED_Timer
            AddMultiball 1
            bAutoPlunger = True
            If NOT bMultiBallMode Then
                DMD "_", CL("BALL SAVED"), "_", eNone, eBlinkfast, eNone, 2500, True, ""
				RandomSoundBallSaved()  
            End If
        Else
            If(BallsOnPlayfield = 1)Then
                If(bMultiBallMode = True) Then
                    bMultiBallMode = False
					STOPMODE
					STOPMULTIBALL
                End If
            End If

            If(BallsOnPlayfield = 0)Then
				'PlaySoundAtLevelStatic ("SFX_Drain_1"), SFXSoundLevel, BULB_DS_6
				If usePUP=false then  ChangeRandomSong
				':pupevent 800
				:pupevent 801
				RandomSoundBallLost()
                StopEndOfBallMode
                vpmtimer.addtimer 500, "EndOfBall '"
            End If
        End If
    End If
End Sub

Sub PLUNGERLANE_Hit()
    bBallInPlungerLane = True
    If bSkillShotReady Then
        UpdateSkillshot()
    End If
    LastSwitchHit = "PLUNGERLANE"
End Sub

Sub T_AUTOFIRE_Hit
    If bAutoPlunger Then
        vpmtimer.addtimer 1500, "AUTOPLUNGER.AutoFire '"
    End If
End Sub

Sub PLUNGERLANE_UnHit()
    bBallInPlungerLane = False
    bAutoPlunger = False
    'DOF 111, DOFPulse
End Sub

Sub GATE_Hit()
    If bSkillShotReady Then
		'ChangeRandomSong
        RESETSKILLSHOTTIMER.Enabled = 1
		FLASHEFFECT(9)
		'PlaySoundAtLevelStatic ("SFX_Gate_1"), SFXSoundLevel, BULB_DS_6
    End If
    If(bBallSaverReady = True) And (BallSaverTime <> 0) And (bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
    End If
End Sub

Sub EnableBallSaver(Seconds)
    bBallSaverActive = True
    bBallSaverReady = False
    BALLSAVERTIMEREXPIRED.Enabled = False
    BALLSAVERSPEEDUPTIMER.Enabled = False
    BALLSAVERTIMEREXPIRED.Interval = 1000 * Seconds
    BALLSAVERTIMEREXPIRED.Enabled = True
    BALLSAVERSPEEDUPTIMER.Interval = 1000 * Seconds - (1000 * Seconds) / 3
    BALLSAVERSPEEDUPTIMER.Enabled = True
    L008.BlinkInterval = 160
    L008.State = 2
End Sub

Sub BALLSAVERTIMEREXPIRED_Timer()
    BALLSAVERTIMEREXPIRED.Enabled = False
    BALLSAVERSPEEDUPTIMER.Enabled = False
    bBallSaverActive = False
    L008.State = 0
    If ExtraBallsAwards(CurrentPlayer) > 0 Then
        L008.State = 1
    End If
End Sub

Sub BALLSAVERSPEEDUPTIMER_Timer()
    BALLSAVERSPEEDUPTIMER.Enabled = False
    L008.BlinkInterval = 80
    L008.State = 2
End Sub

'******************************************************
'******  END / PLUNGER FUNCTIONS
'******************************************************

'******************************************************
'******  SUPPORTING SCORE FUNCTIONS
'******************************************************

Sub AddScore(points)
    If Tilted Then Exit Sub
    Score(CurrentPlayer) = Score(CurrentPlayer) + points * PlayfieldMultiplier(CurrentPlayer)
End Sub

Sub AddScoreNoMultiplier(points)
    If Tilted Then Exit Sub
    Score(CurrentPlayer) = Score(CurrentPlayer) + points
End Sub

Sub AddOrbitScore
    If Tilted Then Exit Sub
    If bOrbitMania = True Then
		Dim OrbitManiaCurrentValue
		OrbitManiaCurrentValue = OrbitValue(CurrentPlayer) + OrbitManiaValue(CurrentPlayer) * OrbitMultiplier(CurrentPlayer) * PlayfieldMultiplier(CurrentPlayer)
		Score(CurrentPlayer) = Score(CurrentPlayer) + OrbitManiaCurrentValue
		SuperJackpot = SuperJackpot + OrbitManiaCurrentValue
	Else
	    Score(CurrentPlayer) = Score(CurrentPlayer) + OrbitValue(CurrentPlayer) * OrbitMultiplier(CurrentPlayer) * PlayfieldMultiplier(CurrentPlayer)
	End If
End Sub

Sub AddSpinnerScore
    If Tilted Then Exit Sub
    If bSuperSpinners = True Then
		Dim SuperSpinnersCurrentValue
		SuperSpinnersCurrentValue = SpinnerValue(CurrentPlayer) + SuperSpinnersValue(CurrentPlayer) * SpinnerMultiplier(CurrentPlayer) * PlayfieldMultiplier(CurrentPlayer)
		Score(CurrentPlayer) = Score(CurrentPlayer) + SuperSpinnersCurrentValue
		SuperJackpot = SuperJackpot + SuperSpinnersCurrentValue
	Else
		Score(CurrentPlayer) = Score(CurrentPlayer) + SpinnerValue(CurrentPlayer) * SpinnerMultiplier(CurrentPlayer) * PlayfieldMultiplier(CurrentPlayer)
	End If
End Sub

Sub AddBumperScore
    If Tilted Then Exit Sub
    If bSuperPops = True Then
		Dim SuperPopsCurrentValue
		SuperPopsCurrentValue = BumperValue(CurrentPlayer) + SuperPopsValue(CurrentPlayer) * BumperMultiplier(CurrentPlayer) * PlayfieldMultiplier(CurrentPlayer)
		Score(CurrentPlayer) = Score(CurrentPlayer) + SuperPopsCurrentValue
		SuperJackpot = SuperJackpot + SuperPopsCurrentValue
	Else
	    Score(CurrentPlayer) = Score(CurrentPlayer) + BumperValue(CurrentPlayer) * BumperMultiplier(CurrentPlayer) * PlayfieldMultiplier(CurrentPlayer)
	End If
End Sub

Sub AddDropTargetScore
    If Tilted Then Exit Sub
    If bDropTargetFrenzy = True Then
		Dim DTFrenzyCurrentValue
		DTFrenzyCurrentValue = DropTargetValue(CurrentPlayer) + DTFrenzyValue(CurrentPlayer) * PlayfieldMultiplier(CurrentPlayer)
		Score(CurrentPlayer) = Score(CurrentPlayer) + DTFrenzyCurrentValue
		SuperJackpot = SuperJackpot + DTFrenzyCurrentValue
	Else
		Score(CurrentPlayer) = Score(CurrentPlayer) + DropTargetValue(CurrentPlayer) * PlayfieldMultiplier(CurrentPlayer)
	End If
End Sub

Sub AddBonus(BonusPoints)
    If Tilted = False Then
        Bonus = Bonus + BonusPoints
        If Bonus > 29 Then
            Bonus = 29
        End If
        UpdateBonusLights
    End if
End Sub

Sub AddBonusMultiplier(n)
    Dim NewBonusLevel
    If(BonusMultiplier(CurrentPlayer) + n <= MaxBonusMultiplier) Then
        NewBonusLevel = BonusMultiplier(CurrentPlayer) + n
        SetBonusMultiplier(NewBonusLevel)
        DMD "_", CL("BONUS X " &NewBonusLevel), "_", eNone, eBlink, eNone, 2000, True, ""
    Else
        AddScoreNoMultiplier 50000
        DMD CL("BONUS X AT MAX"), CL("50000"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
    End if
End Sub

Sub SetBonusMultiplier(Level)
    BonusMultiplier(CurrentPlayer) = Level
    UpdateBonusXLights(Level)
End Sub

Sub UpdateBonusXLights(Level)
    Select Case Level
            Case 0: L014.State = 0 : L012.State = 0 : L013.State = 0
            Case 1: L014.State = 1 : L012.State = 0 : L013.State = 0
            Case 2: L014.State = 0 : L012.State = 1 : L013.State = 0
            Case 3: L014.State = 1 : L012.State = 1 : L013.State = 0
            Case 4: L014.State = 0 : L012.State = 0 : L013.State = 1
            Case 5: L014.State = 1 : L012.State = 0 : L013.State = 1
            Case 6: L014.State = 0 : L012.State = 1 : L013.State = 1
            Case 7: L014.State = 1 : L012.State = 1 : L013.State = 1
    End Select
End Sub

Sub AddPlayfieldMultiplier(n)
    Dim NewPFLevel
    If(PlayfieldMultiplier(CurrentPlayer) + n <= MaxMultiplier) Then
        NewPFLevel = PlayfieldMultiplier(CurrentPlayer) + n
        SetPlayfieldMultiplier(NewPFLevel)
    End if
End Sub

Sub SetPlayfieldMultiplier(Level)
    PlayfieldMultiplier(CurrentPlayer) = Level
End Sub

Sub AwardExtraBall()
	bExtraBallIsLit = False
	FLASHEFFECT(4)
    If Not bExtraBallWonThisBall Then
		DMD "_", CL("EXTRA BALL"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
		ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
		L008.State = 1
		bExtraBallWonThisBall = True
		RandomSoundExtraBall()
		'PlaySoundAtLevelStatic ("SFX_Extra_Ball_Hit"), SFXSoundLevel, BULB_DS_6
	Else
        AddScoreNoMultiplier 200000
		DMD "_", CL("200000"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
    End If
End Sub

Sub AwardSpecial()
    DMD "_", CL("EXTRA GAME WON"), "_", eNone, eBlink, eNone, 2000, True, SoundFXDOF("fx_Knocker", 115, DOFPulse, DOFKnocker)
    Credits = Credits + 1
    AddScoreNoMultiplier 400000
	FLASHEFFECT(4)
	PlaySoundAtLevelStatic ("VO_Replay"), VoiceOverSoundLevel, BULB_DS_6 :pupevent 822
End Sub

Sub AwardJackpot()
    SuperJackpot = SuperJackpot + Jackpot(CurrentPlayer)
    DMD CL("JACKPOT"), CL(FormatScore(Jackpot(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, ""
    AddScoreNoMultiplier Jackpot(CurrentPlayer)
    Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + 100000
	FLASHEFFECT(4)
	RandomSoundJackpot()
	'PlaySoundAtLevelStatic ("SFX_Jackpot_1"), SFXSoundLevel, BULB_DS_6
End Sub

Sub AwardSuperJackpot()
    DMD CL("SUPER JACKPOT"), CL(FormatScore(SuperJackpot)), "d_border", eNone, eBlink, eNone, 2000, True, ""
    AddScoreNoMultiplier SuperJackpot
	SuperJackpot = 0
	FLASHEFFECT(6)
	RandomSoundSuperJackpot()
End Sub

Sub AwardSkillshot()
    ResetSkillShotTimer_Timer
    DMD CL("SKILLSHOT"), CL(FormatScore(SkillshotValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, ""
    AddScoreNoMultiplier SkillShotValue(CurrentPlayer)
    SkillShotValue(CurrentPlayer) = SkillShotValue(CurrentPlayer) + 10000
	FLASHEFFECT(3)
End Sub

Sub AwardLoopSkillshot()
    ResetSkillShotTimer_Timer
    DMD CL("LOOP SKILLSHOT"), CL(FormatScore(LoopSkillshotValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, ""
    AddScoreNoMultiplier LoopSkillshotValue(CurrentPlayer)
    LoopSkillshotValue(CurrentPlayer) = LoopSkillshotValue(CurrentPlayer) + 25000
	FLASHEFFECT(3)
End Sub

Sub AwardSuperSkillshot()
    ResetSkillShotTimer_Timer
    DMD CL("SUPER SKILLSHOT"), CL(FormatScore(SuperSkillshotValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, ""
    AddScoreNoMultiplier SuperSkillshotValue(CurrentPlayer)
    SuperSkillshotValue(CurrentPlayer) = SuperSkillshotValue(CurrentPlayer) + 100000
	FLASHEFFECT(3)
End Sub

Sub AwardDoomSkillshot()
    ResetSkillShotTimer_Timer
    DMD CL("RUBY RHOD SKILLSHOT"), CL(FormatScore(DoomSkillshotValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, ""
    AddScoreNoMultiplier DoomSkillshotValue(CurrentPlayer)
    DoomSkillshotValue(CurrentPlayer) = DoomSkillshotValue(CurrentPlayer) + 100000
	FLASHEFFECT(3)
End Sub

Sub AwardDoom()
    DMD CL("RUBY AWARD"), CL(FormatScore(DoomValue(CurrentPlayer))), "d_border", eNone, eBlinkFast, eNone, 2000, True, ""
    AddScoreNoMultiplier DoomValue(CurrentPlayer)
    DoomValue(CurrentPlayer) = DoomValue(CurrentPlayer) + 50000
End Sub

'******************************************************
'******  END SUPPORTING SCORE FUNCTIONS
'******************************************************

'******************************************************
'******  LOAD / SAVE / HIGHSCORE
'******************************************************

Sub LoadHighScore
    Dim x
    x = LoadValue(cGameName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 250000 End If
    x = LoadValue(cGameName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "LKK" End If
    x = LoadValue(cGameName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 150000 End If
    x = LoadValue(cGameName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "LPK" End If
    x = LoadValue(cGameName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 100000 End If
    x = LoadValue(cGameName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "ESF" End If
    x = LoadValue(cGameName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 50000 End If
    x = LoadValue(cGameName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "KRP" End If
    x = LoadValue(cGameName, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0 End If
    x = LoadValue(cGameName, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If
End Sub

Sub SaveHighScore
    SaveValue cGameName, "HighScore1", HighScore(0)
    SaveValue cGameName, "HighScore1Name", HighScoreName(0)
    SaveValue cGameName, "HighScore2", HighScore(1)
    SaveValue cGameName, "HighScore2Name", HighScoreName(1)
    SaveValue cGameName, "HighScore3", HighScore(2)
    SaveValue cGameName, "HighScore3Name", HighScoreName(2)
    SaveValue cGameName, "HighScore4", HighScore(3)
    SaveValue cGameName, "HighScore4Name", HighScoreName(3)
    SaveValue cGameName, "Credits", Credits
    SaveValue cGameName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub ResetHighScore
    HighScoreName(0) = "LKK"
    HighScoreName(1) = "LPK"
    HighScoreName(2) = "ESF"
    HighScoreName(3) = "KRP"
    HighScore(0) = 250000
    HighScore(1) = 150000
    HighScore(2) = 100000
    HighScore(3) = 50000
    SaveHighScore
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
    Dim TMP
    TMP = Score(CurrentPlayer)

    If TMP > HighScore(0)Then
        Credits = Credits + 1
    End If

    If TMP > HighScore(3)Then
		KnockerSolenoid()
		RandomSoundHighScore()
        HighScore(3) = TMP
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<"
    hsCurrentLetter = 1
    DMDFlush()
    HighScoreDisplayNameNow()

    HighScoreFlashTimer.Interval = 250
    HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(Keycode)
    If Keycode = LeftFlipperKey Then
        PlaySoundAtLevelStatic ("SFX_Highscore_Select"), SFXSoundLevel, BULB_DS_6
        hsCurrentLetter = hsCurrentLetter - 1
        If (hsCurrentLetter = 0) Then
            hsCurrentLetter = len(hsValidLetters)
        End If
        HighScoreDisplayNameNow()
    End If

    If Keycode = RightFlipperKey Then
        PlaySoundAtLevelStatic ("SFX_Highscore_Select"), SFXSoundLevel, BULB_DS_6
        hsCurrentLetter = hsCurrentLetter + 1
        If (hsCurrentLetter > len(hsValidLetters)) Then
            hsCurrentLetter = 1
        End If
        HighScoreDisplayNameNow()
    End If

    If Keycode = PlungerKey Or Keycode = StartGameKey Then
        If (mid(hsValidLetters, hsCurrentLetter, 1) <> "<") Then
            PlaySoundAtLevelStatic ("SFX_Highscore_Enter"), SFXSoundLevel, BULB_DS_6
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            If (hsCurrentDigit = 3) Then
                HighScoreCommitName()
            Else
                HighScoreDisplayNameNow()
            End If
        Else
            PlaySoundAtLevelStatic ("SFX_Highscore_Exit"), SFXSoundLevel, BULB_DS_6
            hsEnteredDigits(hsCurrentDigit) = " "
            If (hsCurrentDigit > 0) Then
                hsCurrentDigit = hsCurrentDigit - 1
            End If
            HighScoreDisplayNameNow()
        End If
    End If
End Sub

Sub HighScoreDisplayNameNow()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreDisplayName()
    Dim i
    Dim TempTopStr
    Dim TempBotStr

    TempTopStr = "YOUR NAME:"
    dLine(0) = ExpandLine(TempTopStr)
    DMDUpdate 0

    TempBotStr = "    > "
    If (hsCurrentDigit > 0) Then TempBotStr = TempBotStr & hsEnteredDigits(0)
    If (hsCurrentDigit > 1) Then TempBotStr = TempBotStr & hsEnteredDigits(1)
    If (hsCurrentDigit > 2) Then TempBotStr = TempBotStr & hsEnteredDigits(2)

    If (hsCurrentDigit <> 3) Then
        If (hsLetterFlash <> 0) Then
            TempBotStr = TempBotStr & "_"
        Else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        End If
    End If

    If (hsCurrentDigit < 1) Then TempBotStr = TempBotStr & hsEnteredDigits(1)
    If (hsCurrentDigit < 2) Then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <    "
    dLine(1) = ExpandLine(TempBotStr)
    DMDUpdate 1
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    If (hsLetterFlash = 2) Then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False

    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    If (hsEnteredName = "   ") Then
        hsEnteredName = "YOU"
    End If

    HighScoreName(3) = hsEnteredName
    SortHighscore
    EndOfBallComplete()
End Sub

Sub SortHighscore
    Dim TMP, TMP2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) < HighScore(j + 1) Then
                TMP = HighScore(j + 1)
                TMP2 = HighScoreName(j + 1)
                HighScore(j + 1) = HighScore(j)
                HighScoreName(j + 1) = HighScoreName(j)
                HighScore(j) = TMP
                HighScoreName(j) = TMP2
            End If
        Next
    Next
End Sub

'******************************************************
'******  END LOAD / SAVE / HIGHSCORE
'******************************************************

'******************************************************
'******  JP'S REDUCED DISPLAY DRIVER FUNCTIONS
'******************************************************

Const eNone = 0        ' Instantly displayed
Const eScrollLeft = 1  ' scroll on from the right
Const eScrollRight = 2 ' scroll on from the left
Const eBlink = 3       ' Blink (blinks for 'TimeOn')
Const eBlinkFast = 4   ' Blink (blinks for 'TimeOn') at user specified intervals (fast speed)

Const dqSize = 64

Dim dqHead
Dim dqTail
Dim deSpeed
Dim deBlinkSlowRate
Dim deBlinkFastRate

Dim dLine(2)
Dim deCount(2)
Dim deCountEnd(2)
Dim deBlinkCycle(2)

Dim dqText(2, 64)
Dim dqEffect(2, 64)
Dim dqTimeOn(64)
Dim dqbFlush(64)
Dim dqSound(64)

Dim FlexDMD
Dim DMDScene

Sub DMD_Init()
    Dim i, j
    If UseFlexDMD Then
        Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
        If Not FlexDMD is Nothing Then
            If FlexDMDHighQuality Then
                FlexDMD.TableFile = Fluffy.Filename & ".vpx"
                FlexDMD.RenderMode = 2
                FlexDMD.Width = 256
                FlexDMD.Height = 64
                FlexDMD.Clear = True
                FlexDMD.GameName = cGameName
                FlexDMD.Run = True
                Set DMDScene = FlexDMD.NewGroup("Scene")
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.d_border")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
                For i = 0 to 19
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + i * 12, 6, 12, 22
                Next
                For i = 20 to 39
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + (i - 20) * 12, 34, 12, 22
                Next
                FlexDMD.LockRenderThread
                FlexDMD.Stage.AddActor DMDScene
                FlexDMD.UnlockRenderThread
            Else
                FlexDMD.TableFile = Fluffy.Filename & ".vpx"
                FlexDMD.RenderMode = 2
                FlexDMD.Width = 128
                FlexDMD.Height = 32
                FlexDMD.Clear = True
                FlexDMD.GameName = cGameName
                FlexDMD.Run = True
                Set DMDScene = FlexDMD.NewGroup("Scene")
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.d_border")
                DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
                For i = 0 to 40
                    DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.d_empty&dmd=2")
                    Digits(i).Visible = False
                Next
                digitgrid.Visible = False
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3, 6, 11
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 4 + (i - 20) * 6, 17, 6, 11
                Next
                FlexDMD.LockRenderThread
                FlexDMD.Stage.AddActor DMDScene
                FlexDMD.UnlockRenderThread
            End If
        End If
    End If

    DMDFlush()
    deSpeed = 20
    deBlinkSlowRate = 10
    deBlinkFastRate = 5
    For i = 0 to 2
        dLine(i) = Space(20)
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
    dLine(2) = " "
    For i = 0 to 2
        For j = 0 to 64
            dqText(i, j) = ""
            dqEffect(i, j) = eNone
        Next
    Next
    DMD dLine(0), dLine(1), dLine(2), eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDFlush()
    Dim i
    DMDTimer.Enabled = False
    DMDEffectTimer.Enabled = False
    dqHead = 0
    dqTail = 0
    For i = 0 to 2
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
    Next
End Sub

Sub DMDScore()
    Dim tmp, tmp1, tmp1a, tmp1b, tmp2
    if(dqHead = dqTail)Then
        tmp = RL(FormatScore(Score(Currentplayer)))
        tmp1 = FL("PLAYER " &CurrentPlayer, "BALL " & Balls)
        tmp2 = "d_border"
		If bModeOn = True Then tmp2 = "d_mode" Else tmp2 = "d_border" End If
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
    if(dqTail < dqSize)Then
        if(Text0 = "_")Then
            dqEffect(0, dqTail) = eNone
            dqText(0, dqTail) = "_"
        Else
            dqEffect(0, dqTail) = Effect0
            dqText(0, dqTail) = ExpandLine(Text0)
        End If

        if(Text1 = "_")Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1)
        End If

        if(Text2 = "_")Then
            dqEffect(2, dqTail) = eNone
            dqText(2, dqTail) = "_"
        Else
            dqEffect(2, dqTail) = Effect2
            dqText(2, dqTail) = Text2
        End If

        dqTimeOn(dqTail) = TimeOn
        dqbFlush(dqTail) = bFlush
        dqSound(dqTail) = Sound
        dqTail = dqTail + 1
        if(dqTail = 1)Then
            DMDHead()
        End If
    End If
End Sub

Sub DMDHead()
    Dim i
    deCount(0) = 0
    deCount(1) = 0
    deCount(2) = 0
    DMDEffectTimer.Interval = deSpeed

    For i = 0 to 2
        Select Case dqEffect(i, dqHead)
            Case eNone:deCountEnd(i) = 1
            Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead))
            Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead))
            Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
            Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
        End Select
    Next
    if(dqSound(dqHead) <> "")Then
        PlaySound(dqSound(dqHead))
    End If
    DMDEffectTimer.Enabled = True
End Sub

Sub DMDEffectTimer_Timer()
    DMDEffectTimer.Enabled = False
    DMDProcessEffectOn()
End Sub

Sub DMDTimer_Timer()
    Dim Head
    DMDTimer.Enabled = False
    Head = dqHead
    dqHead = dqHead + 1
    if(dqHead = dqTail)Then
        if(dqbFlush(Head) = True)Then
            DMDScoreNow()
        Else
            dqHead = 0
            DMDHead()
        End If
    Else
        DMDHead()
    End If
End Sub

Sub DMDProcessEffectOn()
    Dim i
    Dim BlinkEffect
    Dim Temp

    BlinkEffect = False

    For i = 0 to 2
        if(deCount(i) <> deCountEnd(i))Then
            deCount(i) = deCount(i) + 1

            select case(dqEffect(i, dqHead))
                case eNone:
                    Temp = dqText(i, dqHead)
                case eScrollLeft:
                    Temp = Right(dLine(i), 19)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), 21 - deCount(i), 1)
                    Temp = Temp & Left(dLine(i), 19)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkSlowRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(20)
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i)MOD deBlinkFastRate) = 0)Then
                        deBlinkCycle(i) = deBlinkCycle(i)xor 1
                    End If

                    if(deBlinkCycle(i) = 0)Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(20)
                    End If
            End Select

            if(dqText(i, dqHead) <> "_")Then
                dLine(i) = Temp
                DMDUpdate i
            End If
        End If
    Next

    if(deCount(0) = deCountEnd(0))and(deCount(1) = deCountEnd(1))and(deCount(2) = deCountEnd(2))Then

        if(dqTimeOn(dqHead) = 0)Then
            DMDFlush()
        Else
            if(BlinkEffect = True)Then
                DMDTimer.Interval = 10
            Else
                DMDTimer.Interval = dqTimeOn(dqHead)
            End If

            DMDTimer.Enabled = True
        End If
    Else
        DMDEffectTimer.Enabled = True
    End If
End Sub

Function ExpandLine(TempStr) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(20)
    Else
        if Len(TempStr) > Space(20)Then
            TempStr = Left(TempStr, Space(20))
        Else
            if(Len(TempStr) < 20)Then
                TempStr = TempStr & Space(20 - Len(TempStr))
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString

    NumString = CStr(abs(Num))

    For i = Len(NumString)-3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1))then
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1)) + 48) & right(NumString, Len(NumString)- i)
        end if
    Next
    FormatScore = NumString
End function

Function FL(NumString1, NumString2) 'Fill line
    Dim Temp, TempStr
    Temp = 20 - Len(NumString1)- Len(NumString2)
    TempStr = NumString1 & Space(Temp) & NumString2
    FL = TempStr
End Function

Function CL(NumString) 'center line
    Dim Temp, TempStr
    Temp = (20 - Len(NumString)) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(NumString) 'right line
    Dim Temp, TempStr
    Temp = 20 - Len(NumString)
    TempStr = Space(Temp) & NumString
    RL = TempStr
End Function

'******************************************************
'******  UPDATE DMD
'******************************************************

Sub DMDUpdate(id)
    Dim digit, value
    If UseFlexDMD Then FlexDMD.LockRenderThread
    Select Case id
        Case 0 'top text line
            For digit = 0 to 19
                DMDDisplayChar mid(dLine(0), digit + 1, 1), digit
            Next
        Case 1 'bottom text line
            For digit = 20 to 39
                DMDDisplayChar mid(dLine(1), digit -19, 1), digit
            Next
        Case 2 ' back image - back animations
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "d_border"
            Digits(40).ImageA = dLine(2)
            If UseFlexDMD Then DMDScene.GetImage("Back").Bitmap = FlexDMD.NewImage("", "VPX." & dLine(2) & "&dmd=2").Bitmap
    End Select
    If UseFlexDMD Then FlexDMD.UnlockRenderThread
End Sub

Sub DMDDisplayChar(achar, adigit)
    If achar = "" Then achar = " "
    achar = ASC(achar)
    Digits(adigit).ImageA = Chars(achar)
    If UseFlexDMD Then DMDScene.GetImage("Dig" & adigit).Bitmap = FlexDMD.NewImage("", "VPX." & Chars(achar) & "&dmd=2&add").Bitmap
End Sub

'******************************************************
'******  END JP'S REDUCED DISPLAY DRIVER FUNCTIONS
'******************************************************

'******************************************************
'******  JP'S NEW DMD USING FLASHERS
'******************************************************

Dim Digits, Chars(255), Images(255)

DMDInit

Sub DMDInit
    Dim i
    Digits = Array(digit001, digit002, digit003, digit004, digit005, digit006, digit007, digit008, digit009, digit010, _
        digit011, digit012, digit013, digit014, digit015, digit016, digit017, digit018, digit019, digit020,            _
        digit021, digit022, digit023, digit024, digit025, digit026, digit027, digit028, digit029, digit030,            _
        digit031, digit032, digit033, digit034, digit035, digit036, digit037, digit038, digit039, digit040,            _
        digit041)
    For i = 0 to 255:Chars(i) = "d_empty":Next

    Chars(32) = "d_empty"
    Chars(33) = ""        '!
    Chars(34) = ""        '"
    Chars(35) = ""        '#
    Chars(36) = ""        '$
    Chars(37) = ""        '%
    Chars(38) = ""        '&
    Chars(39) = ""        ''
    Chars(40) = ""        '(
    Chars(41) = ""        ')
    Chars(42) = ""        '*
    Chars(43) = "d_plus"  '+
    Chars(44) = ""        '
    Chars(45) = "d_minus" '-
    Chars(46) = "d_dot"   '.
    Chars(47) = ""        '/
    Chars(48) = "d_0"     '0
    Chars(49) = "d_1"     '1
    Chars(50) = "d_2"     '2
    Chars(51) = "d_3"     '3
    Chars(52) = "d_4"     '4
    Chars(53) = "d_5"     '5
    Chars(54) = "d_6"     '6
    Chars(55) = "d_7"     '7
    Chars(56) = "d_8"     '8
    Chars(57) = "d_9"     '9
    Chars(60) = "d_less"  '<
    Chars(61) = ""        '=
    Chars(62) = "d_more"  '>
    Chars(64) = ""        '@
    Chars(65) = "d_a"     'A
    Chars(66) = "d_b"     'B
    Chars(67) = "d_c"     'C
    Chars(68) = "d_d"     'D
    Chars(69) = "d_e"     'E
    Chars(70) = "d_f"     'F
    Chars(71) = "d_g"     'G
    Chars(72) = "d_h"     'H
    Chars(73) = "d_i"     'I
    Chars(74) = "d_j"     'J
    Chars(75) = "d_k"     'K
    Chars(76) = "d_l"     'L
    Chars(77) = "d_m"     'M
    Chars(78) = "d_n"     'N
    Chars(79) = "d_o"     'O
    Chars(80) = "d_p"     'P
    Chars(81) = "d_q"     'Q
    Chars(82) = "d_r"     'R
    Chars(83) = "d_s"     'S
    Chars(84) = "d_t"     'T
    Chars(85) = "d_u"     'U
    Chars(86) = "d_v"     'V
    Chars(87) = "d_w"     'W
    Chars(88) = "d_x"     'X
    Chars(89) = "d_y"     'Y
    Chars(90) = "d_z"     'Z
    Chars(94) = ""    '^
    '    Chars(95) = '_
    Chars(96) = "d_0a"  '0.
    Chars(97) = "d_1a"  '1. 'a
    Chars(98) = "d_2a"  '2. 'b
    Chars(99) = "d_3a"  '3. 'c
    Chars(100) = "d_4a" '4. 'd
    Chars(101) = "d_5a" '5. 'e
    Chars(102) = "d_6a" '6. 'f
    Chars(103) = "d_7a" '7. 'g
    Chars(104) = "d_8a" '8. 'h
    Chars(105) = "d_9a" '9. 'i
    Chars(106) = ""     'j
    Chars(107) = ""     'k
    Chars(108) = ""     'l
    Chars(109) = ""     'm
    Chars(110) = ""     'n
    Chars(111) = ""     'o
    Chars(112) = ""     'p
    Chars(113) = ""     'q
    Chars(114) = ""     'r
    Chars(115) = ""     's
    Chars(116) = ""     't
    Chars(117) = ""     'u
    Chars(118) = ""     'v
    Chars(119) = ""     'w
    Chars(120) = ""     'x
    Chars(121) = ""     'y
    Chars(122) = ""     'z
    Chars(123) = ""     '{
    Chars(124) = ""     '|
    Chars(125) = ""     '}
    Chars(126) = ""     '~
End Sub

'******************************************************
'******  END JP'S NEW DMD USING FLASHERS
'******************************************************

'******************************************************
'******  TABLE INFO AND ATTRACT MODE
'******************************************************

Sub ShowTableInfo
    Dim ii
    If Score(1)Then
        DMD CL("LAST SCORE"), CL("PLAYER 1 " &FormatScore(Score(1))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2)Then
        DMD CL("LAST SCORE"), CL("PLAYER 2 " &FormatScore(Score(2))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3)Then
        DMD CL("LAST SCORE"), CL("PLAYER 3 " &FormatScore(Score(3))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4)Then
        DMD CL("LAST SCORE"), CL("PLAYER 4 " &FormatScore(Score(4))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    DMD "", CL("GAME OVER"), "", eNone, eBlink, eNone, 2000, False, ""
    If bFreePlay Then
        DMD "", CL("FREE PLAY"), "", eNone, eBlink, eNone, 2000, False, ""
    Else
        If Credits > 0 Then
            DMD CL("CREDITS " & Credits), CL("PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "", "", "d_pixelfipresents", eNone, eNone, eNone, 2000, False, ""
    DMD "", "", "d_title", eNone, eNone, eNone, 4000, False, ""
    DMD "", CL("ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("HIGHSCORES"), Space(20), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL("HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL("HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

'/////////////////////////////  ATTRACT MODE LIGHT SEQUENCE  ////////////////////////////

Sub StartAttractMode
	If usePUP=false then ChangeSongGameOver
    StartLightSeq
    DMDFlush
    ShowTableInfo
End Sub

Sub StopAttractMode
    DMDScoreNow
    LIGHTSEQATTRACT.StopPlay
End Sub

Sub StartLightSeq()
    LIGHTSEQATTRACT.UpdateInterval = 25
    LIGHTSEQATTRACT.Play SeqBlinking, , 5, 150
	LIGHTSEQATTRACT.UpdateInterval = 7
	LIGHTSEQATTRACT.Play SeqUpOn,35,1
	LIGHTSEQATTRACT.UpdateInterval = 7
	LIGHTSEQATTRACT.Play SeqDownOn,35,1
	LIGHTSEQATTRACT.UpdateInterval = 7
	LIGHTSEQATTRACT.Play SeqUpOn,35,1
	LIGHTSEQATTRACT.UpdateInterval = 7
	LIGHTSEQATTRACT.Play SeqDownOn,35,1
    LIGHTSEQATTRACT.UpdateInterval = 25
    LIGHTSEQATTRACT.Play SeqBlinking, , 5, 150
    LIGHTSEQATTRACT.UpdateInterval = 10
    LIGHTSEQATTRACT.Play SeqRightOn, 35, 1
    LIGHTSEQATTRACT.UpdateInterval = 10
    LIGHTSEQATTRACT.Play SeqLeftOn, 35, 1
    LIGHTSEQATTRACT.UpdateInterval = 10
    LIGHTSEQATTRACT.Play SeqRightOn, 35, 1
    LIGHTSEQATTRACT.UpdateInterval = 10
    LIGHTSEQATTRACT.Play SeqLeftOn, 35, 1
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

'******************************************************
'******  END TABLE INFO AND ATTRACT MODE
'******************************************************

'******************************************************
'******  LIGHTSHOWS
'******************************************************

Dim f, FESTEP, FEFFECT, BSTEP

FESTEP = 0
FEFFECT = 0
BSTEP = 0

'///////////////////////////// LIGHTSHOWS  ////////////////////////////

Sub FLASHEFFECT(f)
    Select Case f
        Case 1: FESTEP = 0 : FEFFECT = 1 : FLASHEFFECTTIMER.Enabled = 1
        Case 2: FESTEP = 0 : FEFFECT = 2 : FLASHEFFECTTIMER.Enabled = 1
        Case 3: FESTEP = 0 : FEFFECT = 3 : FLASHEFFECTTIMER.Enabled = 1
        Case 4: FESTEP = 0 : FEFFECT = 4 : FLASHEFFECTTIMER.Enabled = 1
        Case 5: FESTEP = 0 : FEFFECT = 5 : FLASHEFFECTTIMER.Enabled = 1
        Case 6: FESTEP = 0 : FEFFECT = 6 : FLASHEFFECTTIMER.Enabled = 1
        Case 7: FESTEP = 0 : FEFFECT = 7 : FLASHEFFECTTIMER.Enabled = 1
        Case 8: FESTEP = 0 : FEFFECT = 8 : FLASHEFFECTTIMER.Enabled = 1
        Case 9: FESTEP = 0 : FEFFECT = 9 : FLASHEFFECTTIMER.Enabled = 1
    End Select
End Sub

Sub FLASHEFFECTTIMER_Timer()
    Select Case FEFFECT
        Case 1
            Select Case FESTEP
				Case 0: FLASH1 True
				Case 1: FLASH2 True
				Case 2: FLASH3 True
				Case 3: FLASH4 True
				Case 4: FLASH1 True
				Case 5: FLASHEFFECTTIMER.Enabled = 0
            End Select
        Case 2
            Select Case FESTEP
				Case 0: FLASH5 True : FLASH6 True : FLASH4 True
				Case 1: FLASH5 True : FLASH6 True : FLASH4 True
				Case 2: FLASH5 True : FLASH6 True : FLASH4 True
				Case 3: FLASHEFFECTTIMER.Enabled = 0
            End Select
        Case 3
            Select Case FESTEP
				Case 0: FLASH2 True : FLASH3 True
				Case 1: FLASH1 True
				Case 2: FLASH4 True
				Case 3: FLASH5 True : FLASH6 True
				Case 4: FLASH5 True : FLASH6 True
				Case 5: FLASH5 True : FLASH6 True
				Case 6: FLASHEFFECTTIMER.Enabled = 0
            End Select
        Case 4
            Select Case FESTEP
				Case 0: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 1: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 2: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 3: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 4: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 5: FLASHEFFECTTIMER.Enabled = 0
            End Select
        Case 5
            Select Case FESTEP
				Case 0: FLASH5 True : FLASH6 True : FLASH4 True
				Case 1: FLASH5 True : FLASH6 True : FLASH1 True
				Case 2: FLASH5 True : FLASH6 True : FLASH3 True
				Case 3: FLASH5 True : FLASH6 True : FLASH2 True
				Case 4: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 5: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 6: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 7: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 8: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 9: FLASHEFFECTTIMER.Enabled = 0
            End Select
        Case 6
            Select Case FESTEP
				Case 0: FLASH2 True
				Case 1: FLASH4 True
				Case 2: FLASH3 True
				Case 3: FLASH1 True
				Case 4: FLASH4 True
				Case 5: FLASH3 True
				Case 6: FLASH2 True
				Case 7: FLASH1 True
				Case 8: FLASH2 True
				Case 9: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 10: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 11: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 12: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 13: FLASH1 True : FLASH2 True : FLASH3 True : FLASH4 True : FLASH5 True : FLASH6 True
				Case 14: FLASHEFFECTTIMER.Enabled = 0
            End Select
        Case 7
            Select Case FESTEP
				Case 0: FLASH2 True
				Case 1: FLASH2 True
				Case 2: FLASH2 True
				Case 3: FLASH2 True
				Case 4: FLASHEFFECTTIMER.Enabled = 0
            End Select
        Case 8
            Select Case FESTEP
				Case 0: FLASH2 True : FLASH3 True
				Case 1: FLASH2 True : FLASH3 True
				Case 2: FLASH1 True : FLASH4 True
				Case 3: FLASH1 True : FLASH4 True
				Case 4: FLASH5 True : FLASH6 True
				Case 5: FLASH5 True : FLASH6 True
				Case 6: FLASH5 True : FLASH6 True
				Case 7: FLASH5 True : FLASH6 True
				Case 8: FLASHEFFECTTIMER.Enabled = 0
            End Select
        Case 9
            Select Case FESTEP
				Case 0: FLASH1 True
				Case 1: FLASH1 True
				Case 2: FLASH1 True
				Case 3: FLASH1 True
				Case 4: FLASH1 True
				Case 5: FLASHEFFECTTIMER.Enabled = 0
            End Select
    End Select
    FESTEP = FESTEP + 1
End Sub

Sub BUMPEREFFECTTIMER_Timer()
	Select Case BSTEP
		Case 0: FlBumperFadeTarget(1) = 1 : BUMPER1.TimerEnabled = True : BSTEP = 1
		Case 1: FlBumperFadeTarget(2) = 1 : BUMPER2.TimerEnabled = True : BSTEP = 2
		Case 2: FlBumperFadeTarget(3) = 1 : BUMPER3.TimerEnabled = True : BSTEP = 0
    End Select
End Sub

'******************************************************
'******  END LIGHTSHOWS
'******************************************************

'******************************************************
'******  SLINGSHOT ANIMATIONS
'******************************************************

Dim RSTEP, LSTEP

Sub LEFTSLINGSHOT_Slingshot
    DOF 103, DOFPulse
	AddScore(75)
	RandomSoundSlingshotLeft SLINGL
	RandomSoundSlings
    LSLING4.Visible = 1
    SLINGL.TransX = -17.5
    LSTEP = 0
    LEFTSLINGSHOT.TimerEnabled = 1
	FLASH2 True
    If bExtraBallIsLit = True Then
		If L016.State = 1 Then
			L016.State = 0
			L017.State = 1
		Else
			L016.State = 1
			L017.State = 0
		End If
	End If
    If L006.State = 2 Or L007.State = 2 Then
		If L006.State = 2 Then
			L006.State = 0
			L007.State = 2
		Else
			L006.State = 2
			L007.State = 0
		End If
	End If
End Sub

Sub LEFTSLINGSHOT_Timer
    Select Case LSTEP
        Case 1:LSLING4.Visible = 0 : LSLING3.Visible = 1 : SLINGL.TransX = -9.5
        Case 2:LSLING3.Visible = 0 : LSLING2.Visible = 1 : SLINGL.TransX = -1.5
        Case 3:LSLING2.Visible = 0 : SLINGL.TransX = 6.5 : LEFTSLINGSHOT.TimerEnabled = 0
    End Select
    LSTEP = LSTEP + 1
End Sub

Sub RIGHTSLINGSHOT_Slingshot
    DOF 105, DOFPulse
	AddScore(75)
	RandomSoundSlingshotRight SLINGR
	RandomSoundSlings
    RSLING4.Visible = 1
    SLINGR.TransX = -17.5
    RSTEP = 0
    RIGHTSLINGSHOT.TimerEnabled = 1
	FLASH3 True
    If bExtraBallIsLit = True Then
		If L016.State = 1 Then
			L016.State = 0
			L017.State = 1
		Else
			L016.State = 1
			L017.State = 0
		End If
	End If
    If L006.State = 2 Or L007.State = 2 Then
		If L006.State = 2 Then
			L006.State = 0
			L007.State = 2
		Else
			L006.State = 2
			L007.State = 0
		End If
	End If
End Sub

Sub RIGHTSLINGSHOT_Timer
    Select Case RSTEP
        Case 1:RSLING4.Visible = 0 : RSLING3.Visible = 1 : SLINGR.TransX = -9.5
        Case 2:RSLING3.Visible = 0 : RSLING2.Visible = 1 : SLINGR.TransX = -1.5
        Case 3:RSLING2.Visible = 0 : SLINGR.TransX = 6.5 : RIGHTSLINGSHOT.TimerEnabled = 0
    End Select
    RSTEP = RSTEP + 1
End Sub

'******************************************************
'******  END SLINGSHOT ANIMATIONS
'******************************************************

'******************************************************
'******  MANUAL BALL CONTROL
'******************************************************

Dim BCUp, BCDown, BCLeft, BCRight
Dim ControlBallInPlay, ControlActiveBall
Dim BCvel, BCyveloffset, BCboostmulti, BCboost

BCboost = 1				'Do Not Change - default setting
BCvel = 4				'Controls the speed of the ball movement
BCyveloffset = -0.01 	'Offsets the force of gravity to keep the ball from drifting vertically on the table, should be negative
BCboostmulti = 3		'Boost multiplier to ball veloctiy (toggled with the B key) 

ControlBallInPlay = false

Sub StartBallControl_Hit()
	Set ControlActiveBall = ActiveBall
	ControlBallInPlay = true
End Sub

Sub StopBallControl_Hit()
	ControlBallInPlay = false
End Sub	

Sub BallControlTimer_Timer()
	If EnableBallControl and ControlBallInPlay then
		If BCright = 1 Then
			ControlActiveBall.velx =  BCvel*BCboost
		ElseIf BCleft = 1 Then
			ControlActiveBall.velx = -BCvel*BCboost
		Else
			ControlActiveBall.velx = 0
		End If

		If BCup = 1 Then
			ControlActiveBall.vely = -BCvel*BCboost
		ElseIf BCdown = 1 Then
			ControlActiveBall.vely =  BCvel*BCboost
		Else
			ControlActiveBall.vely = bcyveloffset
		End If
	End If
End Sub

'******************************************************
'******  END MANUAL BALL CONTROL
'******************************************************

'******************************************************
'****  BALL ROLLING AND DROP SOUNDS
'******************************************************

ReDim Rolling(TNOB)
InitRolling

Dim DropCount
ReDim DropCount(TNOB)

Sub InitRolling
	Dim i
	For i = 0 to TNOB
		Rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim BOT, b,  SpeedFactorX, SpeedFactorY
	BOT = GetBalls

'/////////////////////////////  STOP THE SOUND OF DELETED BALLS  ////////////////////////////

	For b = UBound(BOT) + 1 to TNOB
		Rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

'/////////////////////////////  EXIT THE SUB IF THERE ARE NO BALLS ON THE TABLE  ////////////////////////////

	If UBound(BOT) = -1 Then Exit Sub

'/////////////////////////////  PLAY THE ROLLING SOUND FOR EACH BALL  ////////////////////////////

	For b = 0 to UBound(BOT)
		If BallVel(BOT(b)) > 1 AND BOT(b).z < 30 Then
			Rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(BOT(b)) * 1.1 * VolumeDial, AudioPan(BOT(b)), 0, PitchPlayfieldRoll(BOT(b)), 1, 0, AudioFade(BOT(b))

		Else
			If Rolling(b) = True Then
				StopSound("BallRoll_" & b)
				Rolling(b) = False
			End If
		End If

'/////////////////////////////  PLAY BALL DROP SOUNDS  ////////////////////////////

		If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If BOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft BOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard BOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If

'/////////////////////////////  BALL SPEED CONTROL  ////////////////////////////

        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            SpeedFactorX = ABS(MaxVel / BOT(b).VelX)
            SpeedFactorY = ABS(MaxVel / BOT(b).VelY)
            If speedfactorx <1 Then
                BOT(b).VelX = BOT(b).VelX * SpeedFactorX
                BOT(b).VelY = BOT(b).VelY * SpeedFactorX
            End If
            If SpeedFactorY <1 Then
                BOT(b).VelX = BOT(b).VelX * SpeedFactorY
                BOT(b).VelY = BOT(b).VelY * SpeedFactorY
            End If
        End If
	Next
End Sub

'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************

'******************************************************
'******  FLIPPER SHADOWS
'******************************************************

Sub FLIPPERTIMER_Timer()
	FLIPPERLSH.RotZ = LEFTFLIPPER.CurrentAngle
	FLIPPERRSH.RotZ = RIGHTFLIPPER.CurrentAngle
	FLIPPERURSH.RotZ = RIGHTUPPERFLIPPER.CurrentAngle
	FLIPPERULSH.RotZ = LEFTUPPERFLIPPER.CurrentAngle
End Sub

'******************************************************
'******  END FLIPPER SHADOWS
'******************************************************

'******************************************************
'******  BALL SHADOWS
'******************************************************

Dim BallShadow
BallShadow = Array (BallShadow1,BallShadow2,BallShadow3,BallShadow4,BallShadow5)

Sub BallShadowUpdate_Timer()
    Dim BOT, b
    BOT = GetBalls

'/////////////////////////////  HIDE THE SHADOW OF DELETED BALLS  ////////////////////////////

    If UBound(BOT)<(tnob-1) Then
        For b = (UBound(BOT) + 1) to (tnob-1)
            BallShadow(b).visible = 0
        Next
    End If

'/////////////////////////////  EXIT THE SUB IF THERE ARE NO BALLS ON THE TABLE  ////////////////////////////

    If UBound(BOT) = -1 Then Exit Sub

'/////////////////////////////  RENDER THE SHADOW FOR EACH BALL  ////////////////////////////

    For b = 0 to UBound(BOT)
        If BOT(b).X < Fluffy.Width/2 Then
            BallShadow(b).X = ((BOT(b).X) - (Ballsize/6) + ((BOT(b).X - (Fluffy.Width/2))/21)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize/6) + ((BOT(b).X - (Fluffy.Width/2))/21)) - 6
        End If
        BallShadow(b).Y = BOT(b).Y + 4
        If BOT(b).Z > 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub

'******************************************************
'******  END BALL SHADOWS
'******************************************************

'******************************************************
'******  FLUPPER BUMPERS
'******************************************************

Dim DayNightAdjust , DNA30, DNA45, DNA90

If NightDay < 10 Then
	DNA30 = 0 : DNA45 = (NightDay-10)/20 : DNA90 = 0 : DayNightAdjust = 0.4
Else
	DNA30 = (NightDay-10)/30 : DNA45 = (NightDay-10)/45 : DNA90 = (NightDay-10)/90 : DayNightAdjust = NightDay/25
End If

Dim FlBumperFadeActual(3), FlBumperFadeTarget(3), FlBumperColor(3), FlBumperTop(3), FlBumperSmallLight(3), FlBumperBigLight(3)
Dim FlBumperDisk(3), FlBumperBase(3), FlBumperBulb(3), FlBumperscrews(3), FlBumperActive(3), FlBumperHighlight(3)
Dim cnt : For cnt = 1 to 3 : FlBumperActive(cnt) = False : Next

FlInitBumper 1, "yo" : FlInitBumper 2, "yo" : FlInitBumper 3, "yo" 

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	FlBumperFadeActual(nr) = 1 : FlBumperFadeTarget(nr) = 1.1: FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("BUMPERTOP" & nr) : FlBumperTop(nr).material = "BUMPERTOPMAT" & nr
	Set FlBumperSmallLight(nr) = Eval("BUMPERSMALLLIGHT" & nr) : Set Flbumperbiglight(nr) = Eval("BUMPERBIGLIGHT" & nr)
	Set FlBumperDisk(nr) = Eval("BUMPERDISK" & nr) : Set FlBumperBase(nr) = Eval("BUMPERBASE" & nr)
	Set FlBumperBulb(nr) = Eval("BUMPERBULB" & nr) : FlBumperBulb(nr).material = "BUMPERBULBMAT" & nr
	Set FlBumperscrews(nr) = Eval("BUMPERSCREWS" & nr): FlBumperscrews(nr).material = "BUMPERSCREW" & col
	Set FlBumperHighlight(nr) = Eval("BUMPERHIGHLIGHT" & nr)
	Select Case col
		Case "RED"
			FlBumperSmallLight(nr).color = RGB(255,4,0) : FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0) : FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "BLUE"
			FlBumperBigLight(nr).color = RGB(32,80,255) : FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255) : FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : MaterialColor "BUMPERTOPMAT" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "GREEN"
			FlBumperSmallLight(nr).color = RGB(8,255,8) : FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32) : FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255) : MaterialColor "BUMPERTOPMAT" & nr, RGB(16,255,16) 
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "ORANGE"
			FlBumperHighlight(nr).color = RGB(255,130,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(255,130,0) : FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(255,190,8) : FlBumperBigLight(nr).colorfull = RGB(255,190,8)
		Case "WHITE"
			FlBumperBigLight(nr).color = RGB(255,180,100) : FlBumperBigLight(nr).colorfull = RGB(255,180,100)
			FlBumperHighlight(nr).color = RGB(255,180,100) : FlBumperSmallLight(nr).colorfull = RGB(255,180,100)
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99

		Case "yo"
			FlBumperBigLight(nr).color = RGB(255,230,190) : FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
		Case "BLACKLIGHT"
			FlBumperBigLight(nr).color = RGB(32,32,255) : FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "YELLOW"
			FlBumperSmallLight(nr).color = RGB(255,230,4) : FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50) : FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "PURPLE"
			FlBumperBigLight(nr).color = RGB(119,22,52) : FlBumperBigLight(nr).colorfull = RGB(119,22,52)
			FlBumperSmallLight(nr).color = RGB(119,22,52) : FlBumperSmallLight(nr).colorfull = RGB(119,22,52)
			FlBumperSmallLight(nr).TransmissionScale = 0 : 
			FlBumperHighlight(nr).color = RGB(168,47,85)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	End Select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 )* DayNightAdjust	

	Select Case FlBumperColor(nr)

		Case "BLUE" :
			UpdateMaterial "BUMPERBULBMAT" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(38-24*Z,130 - 98*Z,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20  + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 45 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z^3) / (0.5 + DNA90)

		Case "GREEN"	
			UpdateMaterial "BUMPERBULBMAT" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(16 + 16 * sin(Z*3.14),255,16 + 16 * sin(Z*3.14)), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z^3) / (1 + DNA90)
		
		Case "RED" 
			UpdateMaterial "BUMPERBULBMAT" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 16 - 11*Z + 16 * sin(Z*3.14),0), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			MaterialColor "BUMPERTOPMAT" & nr, RGB(255,20 + Z*4,8-Z*8)
		
		Case "ORANGE"
			UpdateMaterial "BUMPERBULBMAT" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 100 - 22*z  + 16 * sin(Z*3.14),Z*32), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (1 + DNA90)
			MaterialColor "BUMPERTOPMAT" & nr, RGB(255,100 + Z*50, 0)

		Case "WHITE"
			UpdateMaterial "BUMPERBULBMAT" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 12 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 14 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20*Z,255-65*Z) : FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20*Z,255-65*Z)
			MaterialColor "BUMPERTOPMAT" & nr, RGB(255,235 - z*36,220 - Z*90)

		Case "yo"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 12 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 11 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20*Z,255-65*Z) : FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20*Z,255-65*Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z*36,220 - Z*90)

		Case "BLACKLIGHT"
			UpdateMaterial "BUMPERBULBMAT" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 1, RGB(30-27*Z^0.03,30-28*Z^0.01, 255), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z^3
			Flbumperbiglight(nr).intensity = 40 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255-240*(Z^0.1),255 - 240*(Z^0.1),255) : FlBumperSmallLight(nr).colorfull = RGB(255-200*z,255 - 200*Z,255)
			MaterialColor "BUMPERTOPMAT" & nr, RGB(255-190*Z,235 - z*180,220 + 35*Z)

		Case "YELLOW"
			UpdateMaterial "BUMPERBULBMAT" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 180 + 40*z, 48* Z), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			MaterialColor "BUMPERTOPMAT" & nr, RGB(255,200, 24 - 24 * z)

		Case "PURPLE" :
			UpdateMaterial "BUMPERBULBMAT" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(128-118*Z - 32 * sin(Z*3.14), 32-26*Z ,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15  + 200 * Z / (0.5 + DNA30) 
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 50 * Z / (1 + DNA45) 
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (0.5 + DNA90)
			MaterialColor "BUMPERTOPMAT" & nr, RGB(128-60*Z,32,255)

	End Select
End Sub

Sub BUMPERTIMER_Timer
	Dim nr
	For nr = 1 to 3
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	Next
End Sub

'******************************************************
'******  END FLUPPER BUMPERS
'******************************************************

'******************************************************
'*****   FLUPPER DOMES 
'******************************************************

Sub FLASH1(Enabled)
	If Enabled Then
		Objlevel(1) = 1 : FlasherFlash1_Timer
		PlaySoundAtLevelStatic ("Relay_Flashers_On"), FlasherSoundLevel, FLASHERLIGHT1
	End If
End Sub

Sub FLASH2(Enabled)
	If Enabled Then
		Objlevel(2) = 1 : FlasherFlash2_Timer
		PlaySoundAtLevelStatic ("Relay_Flashers_On"), FlasherSoundLevel, FLASHERLIGHT2
	End If
End Sub

Sub FLASH3(Enabled)
	If Enabled Then
		Objlevel(3) = 1 : FlasherFlash3_Timer
		PlaySoundAtLevelStatic ("Relay_Flashers_On"), FlasherSoundLevel, FLASHERLIGHT3
	End If
End Sub

Sub FLASH4(Enabled)
	If Enabled Then
		Objlevel(4) = 1 : FlasherFlash4_Timer
		PlaySoundAtLevelStatic ("Relay_Flashers_On"), FlasherSoundLevel, FLASHERLIGHT4
	End If
End Sub

Sub FLASH5(Enabled)
	If Enabled Then
		Objlevel(5) = 1 : FlasherFlash5_Timer
		PlaySoundAtLevelStatic ("Relay_Flashers_On"), FlasherSoundLevel, FLASHERLIGHT5
	End If
End Sub

Sub FLASH6(Enabled)
	If Enabled Then
		Objlevel(6) = 1 : FlasherFlash6_Timer
		PlaySoundAtLevelStatic ("Relay_Flashers_On"), FlasherSoundLevel, FLASHERLIGHT6
	End If
End Sub

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherBloomIntensity, FlasherOffBrightness

TestFlashers = 0
Set TableRef = Fluffy
FlasherLightIntensity = 0.1
FlasherFlareIntensity = 0.3
FlasherBloomIntensity = 0.2
FlasherOffBrightness = 0.5

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objbloom(20), objlight(20)

InitFlasher 1, "white" : InitFlasher 2, "white" : InitFlasher 3, "white" : InitFlasher 4, "white" : InitFlasher 5, "red" : InitFlasher 6, "red"
RotateFlasher 1,0 : RotateFlasher 2,-15 : RotateFlasher 3,15 : RotateFlasher 4,-45 : RotateFlasher 5,0 : RotateFlasher 6,0

Sub InitFlasher(nr, col)
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	Set objbloom(nr) = Eval("Flasherbloom" & nr)
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 60
	End If
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objbloom(nr).color = RGB(4,120,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4) : objbloom(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4) : objbloom(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) : objbloom(nr).color = RGB(230,49,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50) : objbloom(nr).color = RGB(200,173,25)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59) : objbloom(nr).color = RGB(255,240,150)
		Case "orange" :  objlight(nr).color = RGB(255,70,0) : objflasher(nr).color = RGB(255,70,0) : objbloom(nr).color = RGB(255,70,0)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objbloom(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objbloom(nr).opacity = 100 *  FlasherBloomIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objbloom(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 

'******************************************************
'******  END FLUPPER DOMES
'******************************************************

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************

'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1														'volume level; range [0, 1]
NudgeLeftSoundLevel = 1													'volume level; range [0, 1]
NudgeRightSoundLevel = 1												'volume level; range [0, 1]
NudgeCenterSoundLevel = 1												'volume level; range [0, 1]
StartButtonSoundLevel = 0.1												'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8											'volume level; range [0, 1]
PlungerPullSoundLevel = 1												'volume level; range [0, 1]
RollingSoundFactor = 1.1/5		

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel, TurntableSoundLevel, FlasherSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010           						'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635								'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0                        						'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45                      						'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel								'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel								'sound helper; not configurable
SlingshotSoundLevel = 0.95												'volume level; range [0, 1]
BumperSoundFactor = 4.25												'volume multiplier; must not be zero
KnockerSoundLevel = 1 													'volume level; range [0, 1]
TurntableSoundLevel = 0.2 												'volume level; range [0, 1]
FlasherSoundLevel = 0.25 												'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2									'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055/5										'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075/5											'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075/5										'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025									'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025									'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8								'volume level; range [0, 1]
WallImpactSoundFactor = 0.075											'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075/3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////
Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5/5													'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10											'volume multiplier; must not be zero
DTSoundLevel = 0.25														'volume multiplier; must not be zero
RolloverSoundLevel = 0.25                              					'volume level; range [0, 1]
SpinnerSoundLevel = 0.5                              					'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor 

DrainSoundLevel = 0.8													'volume level; range [0, 1]
BallReleaseSoundLevel = 1												'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2									'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015										'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim LaneSoundFactor, LaneEnterSoundFactor
LaneSoundFactor = 0.15													'volume multiplier; must not be zero
LaneEnterSoundFactor = 0.3												'volume multiplier; must not be zero

'///////////////////////-----Voce Overs and SFX-----///////////////////////
Dim VoiceOverSoundLevel, SFXSoundLevel
VoiceOverSoundLevel = 1													'volume level; range [0, 1]
SFXSoundLevel = 0.8														'volume level; range [0, 1]


'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////

'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////

Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, aVol * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, -1, aVol * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAt(soundname, tableobj)
	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, aVol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
	PlaySoundAt soundname, ActiveBall
End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	Playsound soundname, 1,aVol * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'******************************************************
'****  SUPPORTING BALL AND SOUND FUNCTIONS
'******************************************************

Function AudioFade(tableobj)
  Dim tmp
    tmp = tableobj.y * 2 / tableheight-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
		AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10) )
    End If
End Function

Function AudioPan(tableobj)
    Dim tmp
    tmp = tableobj.x * 2 / tablewidth-1

	if tmp > 7000 Then
		tmp = 7000
	elseif tmp < -7000 Then
		tmp = -7000
	end if

    If tmp > 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10) )
    End If
End Function

Function Vol(ball)
	Vol = Csng(BallVel(ball) ^2)
End Function

Function Volz(ball)
	Volz = Csng((ball.velz) ^2)
End Function

Function Pitch(ball)
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball)
	BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2) ) )
End Function

Function VolPlayfieldRoll(ball)
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * Csng(BallVel(ball) ^3)
End Function

Function PitchPlayfieldRoll(ball)
	PitchPlayfieldRoll = BallVel(ball) ^2 * 15
End Function

Function RndInt(min, max)
	RndInt = Int(Rnd() * (max-min + 1) + min)
End Function

Function RndNum(min, max)
	RndNum = Rnd() * (max-min) + min
End Function

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'/////////////////////////////////////////////////////////////////
'					Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////

Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeLeftSoundLevel * VolumeDial, -0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd*2)+1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub

Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger	
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////

Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, FLASHERBASE5
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////

Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd*11)+1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd*7)+1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd*10)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd*8)+1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd*5)+1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////

Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////

'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////

Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////

Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd*9)+1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd*9)+1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd*3)+1,DOFFlippers), (RndNum(0.8, 1))*FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd*7)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd*8)+1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm/10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm/10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd*7)+1), parm  * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////

Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd*4)+1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////

Sub Rubbers_Hit(idx)
	Dim FinalSpeed
	FinalSpeed=SQR(ActiveBall.VelX * ActiveBall.VelX + ActiveBall.VelY * ActiveBall.VelY)
	If FinalSpeed > 5 Then		
		RandomSoundRubberStrong 1
	End If
	If FinalSpeed <= 5 Then
		RandomSoundRubberWeak()
	End If	
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////

Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd*10)+1
		Case 1 : PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 2 : PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 3 : PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 4 : PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 5 : PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 6 : PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 7 : PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 8 : PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 9 : PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor*voladj
		Case 10 : PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6*voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////

Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd*9)+1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////

Sub Walls_Hit(idx)
	RandomSoundWall()      
End Sub

Sub RandomSoundWall()
	Dim FinalSpeed
	FinalSpeed=SQR(ActiveBall.VelX * ActiveBall.VelX + ActiveBall.VelY * ActiveBall.VelY)
	If FinalSpeed > 16 Then 
		Select Case Int(Rnd*5)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If FinalSpeed >= 6 AND FinalSpeed <= 16 Then
		Select Case Int(Rnd*4)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If FinalSpeed < 6 Then
		Select Case Int(Rnd*3)+1
			Case 1 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3 : PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End if
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////

Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd*13)+1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////

Sub RandomSoundBottomArchBallGuide()
	Dim FinalSpeed
	FinalSpeed=SQR(ActiveBall.VelX * ActiveBall.VelX + ActiveBall.VelY * ActiveBall.VelY)
	If FinalSpeed > 16 Then 
		PlaySoundAtLevelActiveBall ("Apron_Bounce_"& Int(Rnd*2)+1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End If
	If FinalSpeed >= 6 AND FinalSpeed <= 16 Then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If FinalSpeed < 6 Then
		Select Case Int(Rnd*2)+1
			Case 1 : PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2 : PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
End Sub

Sub Apron_Hit (idx)
	RandomSoundBottomArchBallGuide
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////

Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()		
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd*4)+1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	Dim FinalSpeed
	FinalSpeed=SQR(ActiveBall.VelX * ActiveBall.VelX + ActiveBall.VelY * ActiveBall.VelY)
	If FinalSpeed > 10 Then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft Activeball
	Else 
		RandomSoundTargetHitWeak()
	End If	
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound	
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////

Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd*9)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9 : PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd*7)+1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////

Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5 : PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd*2)+1), GateSoundLevel, ActiveBall
End Sub

Sub Gates_Hit(idx)
	SoundPlayfieldGate
End Sub

'/////////////////////////////  BALL COLLISION SOUND  ////////////////////////////

Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select
	PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'/////////////////////////////////////////////////////////////////
'					Voice Over & Special Effects Sounds
'/////////////////////////////////////////////////////////////////

'/////////////////////////////  BALL SAVED  ////////////////////////////

Sub RandomSoundBallSaved()
	Select Case Int(Rnd*1)+1
		Case 1 : PlaySoundAtLevelStatic ("VO_Ball_Saved_1"), VoiceOverSoundLevel, BULB_DS_6
		:pupevent 810
		'Case 2 : PlaySoundAtLevelStatic ("VO_Ball_Saved_2"), VoiceOverSoundLevel, BULB_DS_6
		'Case 3 : PlaySoundAtLevelStatic ("VO_Ball_Saved_3"), VoiceOverSoundLevel, BULB_DS_6
		'Case 4 : PlaySoundAtLevelStatic ("VO_Ball_Saved_4"), VoiceOverSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  SHOOT AGAIN  ////////////////////////////

Sub RandomSoundShootAgain()
	Select Case Int(Rnd*2)+1
		'Case 1 : PlaySoundAtLevelStatic ("VO_Shoot_Again_1"), VoiceOverSoundLevel, BULB_DS_6
		'Case 2 : PlaySoundAtLevelStatic ("VO_Shoot_Again_2"), VoiceOverSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  EXTRA BALL  ////////////////////////////

Sub RandomSoundExtraBall()
	Select Case Int(Rnd*1)+1
		Case 1 : PlaySoundAtLevelStatic ("VO_Extra_Ball_1"), VoiceOverSoundLevel, BULB_DS_6
		:pupevent 802
		'Case 2 : PlaySoundAtLevelStatic ("VO_Extra_Ball_2"), VoiceOverSoundLevel, BULB_DS_6
	End Select
End Sub

Sub RandomSoundExtraBallLit()
	Select Case Int(Rnd*1)+1
		Case 1 : PlaySoundAtLevelStatic ("VO_Extra_Ball_Lit_1"), VoiceOverSoundLevel, BULB_DS_6
		:pupevent 820
		'Case 2 : PlaySoundAtLevelStatic ("VO_Extra_Ball_Lit_2"), VoiceOverSoundLevel, BULB_DS_6
		'Case 3 : PlaySoundAtLevelStatic ("VO_Extra_Ball_Lit_3"), VoiceOverSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  HIGH SCORE  ////////////////////////////

Sub RandomSoundHighScore()
	Select Case Int(Rnd*3)+1
		'Case 1 : PlaySoundAtLevelStatic ("VO_Highscore_1"), VoiceOverSoundLevel, BULB_DS_6
		'Case 2 : PlaySoundAtLevelStatic ("VO_Highscore_2"), VoiceOverSoundLevel, BULB_DS_6
		'Case 3 : PlaySoundAtLevelStatic ("VO_Highscore_3"), VoiceOverSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  JACKPOT  ////////////////////////////

Sub RandomSoundJackpot()
	Select Case Int(Rnd*1)+1
		Case 1 : PlaySoundAtLevelStatic ("VO_Jackpot_1"), VoiceOverSoundLevel, BULB_DS_6 :pupevent 820
		'Case 2 : PlaySoundAtLevelStatic ("VO_Jackpot_2"), VoiceOverSoundLevel, BULB_DS_6
		'Case 3 : PlaySoundAtLevelStatic ("VO_Jackpot_3"), VoiceOverSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  MODE IS LIT  ////////////////////////////

Sub RandomSoundModeLit()
	Select Case Int(Rnd*5)+1
		Case 1 : PlaySoundAtLevelStatic ("VO_Mode_Lit_1"), VoiceOverSoundLevel, BULB_DS_6
		Case 2 : PlaySoundAtLevelStatic ("VO_Mode_Lit_2"), VoiceOverSoundLevel, BULB_DS_6
		Case 3 : PlaySoundAtLevelStatic ("VO_Mode_Lit_3"), VoiceOverSoundLevel, BULB_DS_6
		Case 4 : PlaySoundAtLevelStatic ("VO_Mode_Lit_4"), VoiceOverSoundLevel, BULB_DS_6
		Case 5 : PlaySoundAtLevelStatic ("VO_Mode_Lit_5"), VoiceOverSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  MULTIBALL  ////////////////////////////

Sub RandomSoundMultiball()
	Select Case Int(Rnd*1)+1
		Case 1 : PlaySoundAtLevelStatic ("VO_Multiball_1"), VoiceOverSoundLevel, BULB_DS_6 :pupevent 818
		'Case 2 : PlaySoundAtLevelStatic ("VO_Multiball_2"), VoiceOverSoundLevel, BULB_DS_6
		'Case 3 : PlaySoundAtLevelStatic ("VO_Multiball_3"), VoiceOverSoundLevel, BULB_DS_6
		'Case 4 : PlaySoundAtLevelStatic ("VO_Multiball_4"), VoiceOverSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  SUPER JACKPOT  ////////////////////////////

Sub RandomSoundSuperJackpot()
	Select Case Int(Rnd*1)+1
		Case 1 : PlaySoundAtLevelStatic ("VO_Super_Jackpot_1"), VoiceOverSoundLevel, BULB_DS_6 :pupevent 821
		'Case 2 : PlaySoundAtLevelStatic ("VO_Super_Jackpot_2"), VoiceOverSoundLevel, BULB_DS_6
		'Case 3 : PlaySoundAtLevelStatic ("VO_Super_Jackpot_3"), VoiceOverSoundLevel, BULB_DS_6
		'Case 4 : PlaySoundAtLevelStatic ("VO_Super_Jackpot_4"), VoiceOverSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  TILT WARNING  ////////////////////////////

Sub RandomSoundTiltWarning()
	Select Case Int(Rnd*6)+1
		'Case 1 : PlaySoundAtLevelStatic ("VO_Tilt_Warning_1"), VoiceOverSoundLevel, BULB_DS_6
		'Case 2 : PlaySoundAtLevelStatic ("VO_Tilt_Warning_2"), VoiceOverSoundLevel, BULB_DS_6
		'Case 3 : PlaySoundAtLevelStatic ("VO_Tilt_Warning_3"), VoiceOverSoundLevel, BULB_DS_6
		'Case 4 : PlaySoundAtLevelStatic ("VO_Tilt_Warning_4"), VoiceOverSoundLevel, BULB_DS_6
		'Case 5 : PlaySoundAtLevelStatic ("VO_Tilt_Warning_5"), VoiceOverSoundLevel, BULB_DS_6
		'Case 6 : PlaySoundAtLevelStatic ("VO_Tilt_Warning_6"), VoiceOverSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  BALL LOST  ////////////////////////////

Sub RandomSoundBallLost()
	Select Case Int(Rnd*11)+1
		'Case 1 : PlaySoundAtLevelStatic ("VO_Ball_Lost_1"), VoiceOverSoundLevel, BULB_DS_6
		'Case 2 : PlaySoundAtLevelStatic ("VO_Ball_Lost_2"), VoiceOverSoundLevel, BULB_DS_6
		'Case 3 : PlaySoundAtLevelStatic ("VO_Ball_Lost_3"), VoiceOverSoundLevel, BULB_DS_6
		'Case 4 : PlaySoundAtLevelStatic ("VO_Ball_Lost_4"), VoiceOverSoundLevel, BULB_DS_6
		'Case 5 : PlaySoundAtLevelStatic ("VO_Ball_Lost_5"), VoiceOverSoundLevel, BULB_DS_6
		'Case 6 : PlaySoundAtLevelStatic ("VO_Ball_Lost_6"), VoiceOverSoundLevel, BULB_DS_6
		'Case 7 : PlaySoundAtLevelStatic ("VO_Ball_Lost_7"), VoiceOverSoundLevel, BULB_DS_6
		'Case 8 : PlaySoundAtLevelStatic ("VO_Ball_Lost_8"), VoiceOverSoundLevel, BULB_DS_6
		'Case 9 : PlaySoundAtLevelStatic ("VO_Ball_Lost_9"), VoiceOverSoundLevel, BULB_DS_6
		'Case 10 : PlaySoundAtLevelStatic ("VO_Ball_Lost_10"), VoiceOverSoundLevel, BULB_DS_6
		'Case 11 : PlaySoundAtLevelStatic ("VO_Ball_Lost_11"), VoiceOverSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  PLAYER JOINS  ////////////////////////////

Sub RandomSoundPlayerJoins()
	Select Case Int(Rnd*3)+1
		Case 1 : PlaySoundAtLevelStatic ("VO_Player_Joins_1"), VoiceOverSoundLevel, BULB_DS_6
		Case 2 : PlaySoundAtLevelStatic ("VO_Player_Joins_2"), VoiceOverSoundLevel, BULB_DS_6
		Case 3 : PlaySoundAtLevelStatic ("VO_Player_Joins_3"), VoiceOverSoundLevel, BULB_DS_6
		
	End Select
End Sub

'/////////////////////////////  BUMPERS  ////////////////////////////

Sub RandomSoundBumpers()
	Select Case Int(Rnd*6)+1
		'Case 1 : PlaySoundAtLevelStatic ("SFX_Bumper_1"), SFXSoundLevel, BULB_DS_6
		'Case 2 : PlaySoundAtLevelStatic ("SFX_Bumper_2"), SFXSoundLevel, BULB_DS_6
		'Case 3 : PlaySoundAtLevelStatic ("SFX_Bumper_3"), SFXSoundLevel, BULB_DS_6
		'Case 4 : PlaySoundAtLevelStatic ("SFX_Bumper_4"), SFXSoundLevel, BULB_DS_6
		'Case 5 : PlaySoundAtLevelStatic ("SFX_Bumper_5"), SFXSoundLevel, BULB_DS_6
		'Case 6 : PlaySoundAtLevelStatic ("SFX_Bumper_6"), SFXSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  DROPTARGETS  ////////////////////////////

Sub RandomSoundDroptargets()
	Select Case Int(Rnd*6)+1
		'Case 1 : PlaySoundAtLevelStatic ("SFX_Droptarget_1"), SFXSoundLevel, BULB_DS_6
		'Case 2 : PlaySoundAtLevelStatic ("SFX_Droptarget_2"), SFXSoundLevel, BULB_DS_6
		'Case 3 : PlaySoundAtLevelStatic ("SFX_Droptarget_3"), SFXSoundLevel, BULB_DS_6
		'Case 4 : PlaySoundAtLevelStatic ("SFX_Droptarget_4"), SFXSoundLevel, BULB_DS_6
		'Case 5 : PlaySoundAtLevelStatic ("SFX_Droptarget_5"), SFXSoundLevel, BULB_DS_6
		'Case 6 : PlaySoundAtLevelStatic ("SFX_Droptarget_6"), SFXSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  FANFARE  ////////////////////////////

Sub RandomSoundFanfare()
	Select Case Int(Rnd*9)+1
		'Case 1 : PlaySoundAtLevelStatic ("SFX_Fanfare_1"), SFXSoundLevel, BULB_DS_6
		'Case 2 : PlaySoundAtLevelStatic ("SFX_Fanfare_2"), SFXSoundLevel, BULB_DS_6
		'Case 3 : PlaySoundAtLevelStatic ("SFX_Fanfare_3"), SFXSoundLevel, BULB_DS_6
		'Case 4 : PlaySoundAtLevelStatic ("SFX_Fanfare_4"), SFXSoundLevel, BULB_DS_6
		'Case 5 : PlaySoundAtLevelStatic ("SFX_Fanfare_5"), SFXSoundLevel, BULB_DS_6
		'Case 6 : PlaySoundAtLevelStatic ("SFX_Fanfare_6"), SFXSoundLevel, BULB_DS_6
		'Case 7 : PlaySoundAtLevelStatic ("SFX_Fanfare_7"), SFXSoundLevel, BULB_DS_6
		'Case 8 : PlaySoundAtLevelStatic ("SFX_Fanfare_8"), SFXSoundLevel, BULB_DS_6
		'Case 9 : PlaySoundAtLevelStatic ("SFX_Fanfare_9"), SFXSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  TARGETS  ////////////////////////////

Sub RandomSoundTargets()
	Select Case Int(Rnd*3)+1
		'Case 1 : PlaySoundAtLevelStatic ("SFX_Target_1"), SFXSoundLevel, BULB_DS_6
		'Case 2 : PlaySoundAtLevelStatic ("SFX_Target_2"), SFXSoundLevel, BULB_DS_6
		'Case 3 : PlaySoundAtLevelStatic ("SFX_Target_3"), SFXSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////  SLINGS  ////////////////////////////

Sub RandomSoundSlings()
	Select Case Int(Rnd*3)+1
		'Case 1 : PlaySoundAtLevelStatic ("SFX_Slings_1"), SFXSoundLevel, BULB_DS_6
		'Case 2 : PlaySoundAtLevelStatic ("SFX_Slings_2"), SFXSoundLevel, BULB_DS_6
		'Case 3 : PlaySoundAtLevelStatic ("SFX_Slings_3"), SFXSoundLevel, BULB_DS_6
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Voice Over & Special Effects Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  END FLEEP MECHANICAL SOUNDS
'******************************************************

'******************************************************
'****  MUSIC
'******************************************************

Dim Song
Song = ""

Sub PlaySong(Name)
    If bMusicOn Then
        If Song <> Name Then
            StopSound Song
            Song = Name
            PlaySound Song, -1, SongVolume
        End If
    End If
End Sub

Sub ChangeSongGameOver
	StopSound Song
	PlaySong "MU_GAMEOVER"
	Song = "MU_GAMEOVER"
End Sub

Sub ChangeSongGameStart
	'StopSound Song
	'PlaySong "MU_GAMESTART"
	'Song = "MU_GAMESTART"
End Sub

Sub ChangeSongBonus
	StopSound Song
	PlaySong "MU_BONUS"
	Song = "MU_BONUS"
End Sub

Sub ChangeRandomSong
	Select Case Int(Rnd*9)+1
		Case 1:
			If Song = "0" Then
				ChangeRandomSong
			Else
				PlaySong "0"
			End If
		Case 2:
			If Song = "1" Then
				ChangeRandomSong
			Else
				PlaySong "1"
			End If
		Case 3:
			If Song = "2" Then
				ChangeRandomSong
			Else
				PlaySong "2"
			End If
		Case 4:
			If Song = "3" Then
				ChangeRandomSong
			Else
				PlaySong "3"
			End If
		Case 5:
			If Song = "4" Then
				ChangeRandomSong
			Else
				PlaySong "4"
			End If
		Case 6:
			If Song = "5" Then
				ChangeRandomSong
			Else
				PlaySong "5"
			End If
		Case 7:
			If Song = "6" Then
				ChangeRandomSong
			Else
				PlaySong "6"
			End If
		Case 8:
			If Song = "7" Then
				ChangeRandomSong
			Else
				PlaySong "7"
			End If
		Case 9:
			If Song = "8" Then
				ChangeRandomSong
			Else
				PlaySong "8"
			End If
	End Select
End Sub

Sub StopSong
    StopSound Song
End Sub

'******************************************************
'****  END MUSIC
'******************************************************

'******************************************************
'****  TABLE SPECIFIC SCRIPT STARTS HERE
'******************************************************

Sub Game_Init()

    Dim i

    BallSaverTime = 20
    bExtraBallWonThisBall = False
	bExtraBallIsLit = False
	SuperJackpot = 0
	ScoreBonus = 0
	bJackpotReady = False
	bWizardMode = False
	bScoreBonusLit = False

    For i = 0 to 4
        SkillshotValue(i) = 10000
        LoopSkillshotValue(i) = 25000
        SuperSkillshotValue(i) = 100000
        DoomSkillshotValue(i) = 100000
        Jackpot(i) = 200000
        OrbitValue(i) = 6250
        OrbitMultiplier(i) = 1
		SpinnerValue(i) = 75
		SpinnerMultiplier(i) = 1
		BumperValue(i) = 1250
		BumperMultiplier(i) = 1
		DropTargetValue(i) = 1875
		OrbitAwardLevel(i) = 0
		SuperPopsValue(i) = 0
		SuperPopsLevel(i) = 0
		SuperSpinnersValue(i) = 0
		SuperSpinnersFirstHitValue(i) = 0
		SuperSpinnersLevel(i) = 0
		DTFrenzyValue(i) = 0
		DTFrenzyLevel(i) = 0
		OrbitManiaValue(i) = 0
		OrbitManiaLevel(i) = 0
		b2BallMultiballFinished(i) = False
		bOrbitManiaFinished(i) = False
		bDTFrenzyFinished(i) = False
		bSuperSpinnersFinished(i) = False
		bSuperPopsFinished(i) = False
		bWizardModeReady(i) = False
		DoomValue(i) = 100000
		StoreOrbitLevel(i) = 0
    Next

    TurnOffPlayfieldLights()
	SetOrbitLevel(0)

End Sub

Sub InstantInfo
    Dim TMP
    DMD CL("INSTANT INFO"), "", "", eNone, eNone, eNone, 1000, True, ""
    DMD CL("YOUR SCORE"), CL(FormatScore(Score(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("EXTRA BALLS"), CL(ExtraBallsAwards(CurrentPlayer)), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("BONUS MULTIPLIER"), CL(FormatScore(BonusMultiplier(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("ORBIT VALUE"), CL(FormatScore(OrbitValue(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("ORBIT MULTIPLIER"), CL(FormatScore(OrbitMultiplier(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("BUMPER VALUE"), CL(FormatScore(BumperValue(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("BUMPER MULTIPLIER"), CL(FormatScore(BumperMultiplier(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SPINNER VALUE"), CL(FormatScore(SpinnerValue(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("SPINNER MULTIPLIER"), CL(FormatScore(SpinnerMultiplier(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    DMD CL("DROPTARGET VALUE"), CL(FormatScore(DropTargetValue(CurrentPlayer))), "", eNone, eNone, eNone, 2000, False, ""
    If Score(1)Then
        DMD CL("PLAYER 1 SCORE"), CL(FormatScore(Score(1))), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(2)Then
        DMD CL("PLAYER 2 SCORE"), CL(FormatScore(Score(2))), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(3)Then
        DMD CL("PLAYER 3 SCORE"), CL(FormatScore(Score(3))), "", eNone, eNone, eNone, 2000, False, ""
    End If
    If Score(4)Then
        DMD CL("PLAYER 4 SCORE"), CL(FormatScore(Score(4))), "", eNone, eNone, eNone, 2000, False, ""
    End If
End Sub

Sub StopEndOfBallMode()
	StopMode
End Sub

Sub ResetNewBallVariables()
    TurnOffPlayfieldLights
    BonusMultiplier(CurrentPlayer) = 1
    PlayfieldMultiplier(CurrentPlayer) = 1
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For Each a in Inserts
        a.State = 0
    Next
End Sub

Sub TurnOffLightsForBonus()
    Dim a
    For Each a in Inserts_Bonus
        a.State = 0
    Next
End Sub

Sub UPDATESKILLSHOT()
    L075.State = 2
    L021.State = 2
	STARTSKILLSHOTLIGHTS
    L048.State = 2
    L049.State = 2
    L050.State = 2
    L051.State = 2
	UpdateBonusXLights(1)
End Sub

Sub RESETSKILLSHOTTIMER_Timer
    RESETSKILLSHOTTIMER.Enabled = 0
    bSkillShotReady = False
    DMDScoreNow
    L075.State = 0
    L021.State = 0
    STOPSKILLSHOTLIGHTS
    L048.State = 0
    L049.State = 0
    L050.State = 0
    L051.State = 0
    L035.State = 2
    L036.State = 2
    L037.State = 2
    L041.State = 2
    L042.State = 2
    L043.State = 2
    L044.State = 2
    L046.State = 1
End Sub

Sub CHECKSKILLSHOT
    If bSkillShotReady Then
        If LastSwitchHit = "T_SKILLSHOT" Then
            AwardSkillshot
			RandomSoundFanfare
			PlaySoundAtLevelStatic ("VO_Skillshot"), VoiceOverSoundLevel, BULB_DS_6
			:pupevent 823
        End If
    End If
End Sub

Sub CHECKLOOPSKILLSHOT
    If bSkillShotReady Then
		AwardLoopSkillshot
		RandomSoundFanfare
		PlaySoundAtLevelStatic ("VO_Skillshot"), VoiceOverSoundLevel, BULB_DS_6
		:pupevent 823
    End If
End Sub

Sub CHECKSUPERSKILLSHOT
    If bSkillShotReady Then
        If LastSwitchHit = "T_TOPLANE" Then
            AwardSuperSkillshot
			RandomSoundFanfare
			PlaySoundAtLevelStatic ("VO_Skillshot"), VoiceOverSoundLevel, BULB_DS_6
			:pupevent 823
        End If
    End If
End Sub

Sub CHECKDOOMSKILLSHOT
    If bSkillShotReady Then
        If LastSwitchHit = "DT_DOOM_1" Or LastSwitchHit = "DT_DOOM_2" Or LastSwitchHit = "DT_DOOM_3" Or LastSwitchHit = "DT_DOOM_4" Then
            AwardDoomSkillshot
			RandomSoundFanfare
			PlaySoundAtLevelStatic ("VO_Skillshot"), VoiceOverSoundLevel, BULB_DS_6
			:pupevent 823
        End If
    End If
End Sub

Sub SKILLSHOT_Hit(idx)
    If bSkillshotReady Then RESETSKILLSHOTTIMER_Timer
End Sub

'/////////////////////////////  SKILLSHOT LIGHT SEQUENCE  ////////////////////////////

Sub STARTSKILLSHOTLIGHTS
	LIGHTSEQSKILLSHOT.UpdateInterval = 10
	LIGHTSEQSKILLSHOT.Play SeqUpOn,75,1
End Sub

Sub LIGHTSEQSKILLSHOT_PlayDone()
    STARTSKILLSHOTLIGHTS
End Sub

Sub STOPSKILLSHOTLIGHTS
    LIGHTSEQSKILLSHOT.StopPlay
End Sub

'/////////////////////////////  CHECK FOR EXTRA BALL  ////////////////////////////

Sub CHECKEXTRABALL
    If L009.State = 1 And L010.State = 1 And L011.State = 1 And bExtraBallIsLit = False And bExtraBallWonThisBall = False Then
        DMD "_", CL("EXTRA BALL IS LIT"), "_", eNone, eBlink, eNone, 2500, True, ""
		RandomSoundExtraBallLit()
		'PlaySoundAtLevelStatic ("SFX_Extra_Ball_Lit"), SFXSoundLevel, BULB_DS_6
		bExtraBallIsLit = True
		L016.State = 1
    End If
End Sub

'/////////////////////////////  CHECK FOR ORBIT BONUS  ////////////////////////////

Sub CHECKORBITBONUS
    If L038.State = 1 And L045.State = 1 And L052.State = 1 Then
		OrbitValue(CurrentPlayer) = OrbitValue(CurrentPlayer) + 3125
		DMD CL("ORBIT VALUE"), CL(FormatScore(OrbitValue(CurrentPlayer))), "d_border", eNone, eBlink, eNone, 2000, True, ""
		'PlaySoundAtLevelStatic ("SFX_Boost_1"), SFXSoundLevel, BULB_DS_6
		L038.State = 0
		L045.State = 0
		L052.State = 0
    End If
End Sub

'/////////////////////////////  CHECK IF ETERNAL IS COMPLETE  ////////////////////////////

Sub CHECKETERNAL
    If bEternalDone1 = True And bEternalDone2 = True Then
		bEternalDone1 = False
		bEternalDone2 = False
		bModeReady = True
        DMD CL("MODE IS READY"), CL("SHOOT THE ORBITS"), "_", eNone, eBlink, eNone, 1000, True, ""
		RandomSoundModeLit()
		'PlaySoundAtLevelStatic ("SFX_Mode_Lit_1"), SFXSoundLevel, BULB_DS_6
        TURNMAGNETON
		L065.State = 2 : L066.State = 2 : L067.State = 2 : L068.State = 2 : L069.State = 2 : L070.State = 2 : L071.State = 2 : L072.State = 2 : L073.State = 2 : L074.State = 2 : L022.State = 2 : L023.State = 2
    End If
End Sub

'/////////////////////////////  SCORE BONUS  ////////////////////////////

Sub CHECKSCOREBONUS
	ScoreBonus = ScoreBonus + 1
	If ScoreBonus >= 3 Then ScoreBonus = 3
	If ScoreBonus = 3 And bScoreBonusLit = False Then
		bScoreBonusLit = True
		Select Case RndNbr(3)
			Case 1, 0
				L040.State = 1
			Case 2, 1
				L047.State = 1
			Case 3, 2
				L054.State = 1
		End Select
	End If
End Sub

Sub ADDSCOREBONUS
	Dim ScoreBonusValue
	ScoreBonus = -1
	ScoreBonusValue = Bonus * 1000
	AddScoreNoMultiplier(ScoreBonusValue)
	DMD CL("SCORE BONUS"), CL(FormatScore(ScoreBonusValue)), "_", eNone, eBlinkFast, eNone, 1500, True, ""
	bScoreBonusLit = False
	L040.State = 0
	L047.State = 0
	L054.State = 0
End Sub

'******************************************************
'******  TARGETS & TRIGGERS HIT
'******************************************************

'/////////////////////////////  TURNTABLE  ////////////////////////////

Dim TURNTABLEMOTOROFF, TURNTABLESTEP, SS

Sub TURNONTURNTABLE
	TURNTABLE_DISC.MotorOn = True
	TURNTABLESTEP = 10
	TURNTABLEMOTOROFF = False
	TURNTABLETIMER.Interval = 10
	TURNTABLETIMER.Enabled = True
	PlaySoundAtLevelStaticLoop ("Turntable_Loop"), TurntableSoundLevel, TRIGGER_TURNTABLE
	PlaySoundAtLevelStatic ("Turntable_Start"), TurntableSoundLevel, TRIGGER_TURNTABLE
    DOF 116, DOFOn
    DOF 117, DOFOn
End Sub

Sub TURNOFFTURNTABLE
	TURNTABLEMOTOROFF = True
	TURNTABLE_DISC.MotorOn = False
	PlaySoundAtLevelStatic ("Turntable_Stop"), TurntableSoundLevel, TRIGGER_TURNTABLE
	StopSound("Turntable_Loop")
    DOF 116, DOFOff
    DOF 117, DOFOff
End Sub

Sub TURNTABLETIMER_Timer()
	If Not(TURNTABLEMOTOROFF) Then
		TURNTABLE.ObjRotZ = SS
		SS = SS + TURNTABLESTEP
	Else
		If TURNTABLESTEP < 0 Then
			TURNTABLETIMER.Enabled = False
		Else
			TURNTABLESTEP = TURNTABLESTEP - 0.05
			TURNTABLE.ObjRotZ  = SS
			SS = SS + TURNTABLESTEP
		End If
	End If
	If SS > 360 Then SS = SS - 360
End Sub

'/////////////////////////////  MAGNET  ////////////////////////////

Sub TURNMAGNETON
    MAGNET.MagnetOn = True
	bMagnetOn = True
End Sub

Sub TURNMAGNETOFF
	bMagnetOn = False
    MAGNET.MagnetOn = False
End Sub

'/////////////////////////////  BUMPERS  ////////////////////////////

Sub BUMPER1_Hit
    DOF 107, DOFPulse
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
	If bModeOn = False Then SELECTMODE
	FlBumperFadeTarget(1) = 1
	BUMPER1.TimerEnabled = True
	RandomSoundBumperTop BUMPER1
	RandomSoundBumpers
	:pupevent 805
	FLASH4 True
	AddBumperScore
	If bSuperPops = True And bWizardMode = False Then
		SuperPopsHits = SuperPopsHits + 1
		If SuperPopsHits = 10 Then
			SuperPopsHits = 0
			AddSuperPopsLevel(1)
		End If
	End If
	LastSwitchHit = "BUMPER1"
End Sub

Sub BUMPER1_Timer : FlBumperFadeTarget(1) = 0 : End Sub

Sub BUMPER2_Hit
    DOF 108, DOFPulse
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
	If bModeOn = False Then SELECTMODE
	FlBumperFadeTarget(2) = 1
	BUMPER2.TimerEnabled = True
	RandomSoundBumperMiddle BUMPER2
	RandomSoundBumpers
	:pupevent 805
	FLASH4 True
	AddBumperScore
	If bSuperPops = True And bWizardMode = False Then
		SuperPopsHits = SuperPopsHits + 1
		If SuperPopsHits = 10 Then
			SuperPopsHits = 0
			AddSuperPopsLevel(1)
		End If
	End If
	LastSwitchHit = "BUMPER2"
End Sub

Sub BUMPER2_Timer : FlBumperFadeTarget(2) = 0 : End Sub

Sub BUMPER3_Hit
    DOF 109, DOFPulse
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
	If bModeOn = False Then SELECTMODE
	FlBumperFadeTarget(3) = 1
	BUMPER3.TimerEnabled = True
	RandomSoundBumperBottom BUMPER3
	RandomSoundBumpers
	:pupevent 805
	FLASH4 True
	AddBumperScore
	If bSuperPops = True And bWizardMode = False Then
		SuperPopsHits = SuperPopsHits + 1
		If SuperPopsHits = 10 Then
			SuperPopsHits = 0
			AddSuperPopsLevel(1)
		End If
	End If
	LastSwitchHit = "BUMPER3"
End Sub

Sub BUMPER3_Timer : FlBumperFadeTarget(3) = 0 : End Sub

'/////////////////////////////  SPINNERS  ////////////////////////////

Sub SPINNER_LEFT_Spin
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
	SoundSpinner SPINNER_LEFT
	AddSpinnerScore
	If bSuperSpinners = True And bWizardMode = False Then
		FLASH4 True
		SuperSpinnersHits = SuperSpinnersHits + 1
		If SuperSpinnersHits = 20 Then
			SuperSpinnersHits = 0
			AddSuperSpinnersLevel(1)
		End If
	End If
End Sub

Sub SPINNER_RIGHT_Spin
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
	SoundSpinner SPINNER_RIGHT
	AddSpinnerScore
	If bSuperSpinners = True And bWizardMode = False Then
		FLASH1 True
		SuperSpinnersHits = SuperSpinnersHits + 1
		If SuperSpinnersHits = 20 Then
			SuperSpinnersHits = 0
			AddSuperSpinnersLevel(1)
		End If
	End If
End Sub

Sub SUPERSPINNERS_LEFT_Hit
	If bSuperSpinners = True Then
		Score(CurrentPlayer) = Score(CurrentPlayer) + SuperSpinnersFirstHitValue(CurrentPlayer)
		SuperJackpot = SuperJackpot + SuperSpinnersFirstHitValue(CurrentPlayer)
	End If
End Sub

Sub SUPERSPINNERS_RIGHT_Hit
	If bSuperSpinners = True Then
		Score(CurrentPlayer) = Score(CurrentPlayer) + SuperSpinnersFirstHitValue(CurrentPlayer)
		SuperJackpot = SuperJackpot + SuperSpinnersFirstHitValue(CurrentPlayer)
	End If
End Sub

'/////////////////////////////  RESET ALL DROP TARGETS  ////////////////////////////

Sub RESET_ALL_DROPS
	DT_ETERNAL_1.IsDropped = 0
	DT_ETERNAL_2.IsDropped = 0
	DT_ETERNAL_3.IsDropped = 0
	DT_ETERNAL_4.IsDropped = 0
	DT_ETERNAL_5.IsDropped = 0
	DT_ETERNAL_6.IsDropped = 0
	DT_ETERNAL_7.IsDropped = 0
	DT_DOOM_1.IsDropped = 0
	DT_DOOM_2.IsDropped = 0
	DT_DOOM_3.IsDropped = 0
	DT_DOOM_4.IsDropped = 0
	PlaySoundAtLevelStatic ("DropTarget_Up"), DTSoundLevel, DT_ETERNAL_2
	PlaySoundAtLevelStatic ("DropTarget_Up"), DTSoundLevel, DT_ETERNAL_6
	PlaySoundAtLevelStatic ("DropTarget_Up"), DTSoundLevel, DT_DOOM_3
End Sub

'/////////////////////////////  DROPTARGETS ETE BANK  ////////////////////////////

Sub DT_ETERNAL_1_Dropped
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
	RESET_DROPTARGETS_ETE
	PlaySoundAtLevelStatic ("DropTarget_Down"), DTSoundLevel, DT_ETERNAL_1
	RandomSoundDroptargets
	AddDropTargetScore
	If bDropTargetFrenzy = True And bWizardMode = False Then
		DTFrenzyHits = DTFrenzyHits + 1
		If DTFrenzyHits = 5 Then
			DTFrenzyHits = 0
			AddDTFrenzyLevel(1)
		End If
	End If
	AddBonus 1
    LastSwitchHit = "DT_ETERNAL_1"
	L035.State = 1
End Sub

Sub DT_ETERNAL_2_Dropped
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
	RESET_DROPTARGETS_ETE
	PlaySoundAtLevelStatic ("DropTarget_Down"), DTSoundLevel, DT_ETERNAL_2
	RandomSoundDroptargets
	AddDropTargetScore
	If bDropTargetFrenzy = True And bWizardMode = False Then
		DTFrenzyHits = DTFrenzyHits + 1
		If DTFrenzyHits = 5 Then
			DTFrenzyHits = 0
			AddDTFrenzyLevel(1)
		End If
	End If
	AddBonus 1
    LastSwitchHit = "DT_ETERNAL_2"
	L036.State = 1
End Sub

Sub DT_ETERNAL_3_Dropped
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
	RESET_DROPTARGETS_ETE
	PlaySoundAtLevelStatic ("DropTarget_Down"), DTSoundLevel, DT_ETERNAL_3
	RandomSoundDroptargets
	If bDropTargetFrenzy = True And bWizardMode = False Then
		DTFrenzyHits = DTFrenzyHits + 1
		If DTFrenzyHits = 5 Then
			DTFrenzyHits = 0
			AddDTFrenzyLevel(1)
		End If
	End If
	AddDropTargetScore
	AddBonus 1
    LastSwitchHit = "DT_ETERNAL_3"
	L037.State = 1
End Sub

Sub RESET_DROPTARGETS_ETE
    If DT_ETERNAL_1.IsDropped And DT_ETERNAL_2.IsDropped And DT_ETERNAL_3.IsDropped Then
        DT_ETERNAL_1.IsDropped = 0
        DT_ETERNAL_2.IsDropped = 0
        DT_ETERNAL_3.IsDropped = 0
		If bModeReady = False Then
			If bModeOn = False Then
				bEternalDone1 = True
			End If
		End If
        FLASHEFFECT(1)
        AddScore 5625
		PlaySoundAtLevelStatic SoundFXDOF("DropTarget_Up", 114, DOFPulse, DOFContactors), DTSoundLevel, DT_ETERNAL_2
        DOF 118, DOFPulse
		PlaySoundAtLevelStatic ("SFX_Droptargets_Finished_1"), SFXSoundLevel, BULB_DS_6
		If L039.State = 1 Then
			L039.State = 0
			L046.State = 1
			AddBonusMultiplier 1
		End If
		L038.State = 1
		L011.State = 1
		If bScoreBonusLit = True And L040.State = 1 Then ADDSCOREBONUS
		CHECKEXTRABALL
		CHECKORBITBONUS
		CHECKETERNAL
		CHECKSCOREBONUS
    End If
End Sub

'/////////////////////////////  DROPTARGETS RNAL BANK  ////////////////////////////

Sub DT_ETERNAL_4_Dropped
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
	RESET_DROPTARGETS_RNAL
	PlaySoundAtLevelStatic ("DropTarget_Down"), DTSoundLevel, DT_ETERNAL_4
	RandomSoundDroptargets
	If bDropTargetFrenzy = True And bWizardMode = False Then
		DTFrenzyHits = DTFrenzyHits + 1
		If DTFrenzyHits = 5 Then
			DTFrenzyHits = 0
			AddDTFrenzyLevel(1)
		End If
	End If
	AddDropTargetScore
	AddBonus 1
    LastSwitchHit = "DT_ETERNAL_4"
	L041.State = 1
End Sub

Sub DT_ETERNAL_5_Dropped
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
	RESET_DROPTARGETS_RNAL
	PlaySoundAtLevelStatic ("DropTarget_Down"), DTSoundLevel, DT_ETERNAL_5
	RandomSoundDroptargets
	If bDropTargetFrenzy = True And bWizardMode = False Then
		DTFrenzyHits = DTFrenzyHits + 1
		If DTFrenzyHits = 5 Then
			DTFrenzyHits = 0
			AddDTFrenzyLevel(1)
		End If
	End If
	AddDropTargetScore
	AddBonus 1
    LastSwitchHit = "DT_ETERNAL_5"
	L042.State = 1
End Sub

Sub DT_ETERNAL_6_Dropped
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
	RESET_DROPTARGETS_RNAL
	PlaySoundAtLevelStatic ("DropTarget_Down"), DTSoundLevel, DT_ETERNAL_6
	RandomSoundDroptargets
	If bDropTargetFrenzy = True And bWizardMode = False Then
		DTFrenzyHits = DTFrenzyHits + 1
		If DTFrenzyHits = 5 Then
			DTFrenzyHits = 0
			AddDTFrenzyLevel(1)
		End If
	End If
	AddDropTargetScore
	AddBonus 1
    LastSwitchHit = "DT_ETERNAL_6"
	L043.State = 1
End Sub

Sub DT_ETERNAL_7_Dropped
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
	RESET_DROPTARGETS_RNAL
	PlaySoundAtLevelStatic ("DropTarget_Down"), DTSoundLevel, DT_ETERNAL_7
	RandomSoundDroptargets
	If bDropTargetFrenzy = True And bWizardMode = False Then
		DTFrenzyHits = DTFrenzyHits + 1
		If DTFrenzyHits = 5 Then
			DTFrenzyHits = 0
			AddDTFrenzyLevel(1)
		End If
	End If
	AddDropTargetScore
	AddBonus 1
    LastSwitchHit = "DT_ETERNAL_7"
	L044.State = 1
End Sub

Sub RESET_DROPTARGETS_RNAL
    If DT_ETERNAL_4.IsDropped And DT_ETERNAL_5.IsDropped And DT_ETERNAL_6.IsDropped And DT_ETERNAL_7.IsDropped  Then
        DT_ETERNAL_4.IsDropped = 0
        DT_ETERNAL_5.IsDropped = 0
        DT_ETERNAL_6.IsDropped = 0
        DT_ETERNAL_7.IsDropped = 0
		If bModeReady = False Then
			If bModeOn = False Then
				bEternalDone2 = True
			End If
		End If
        FLASHEFFECT(1)
        AddScore 7500
		PlaySoundAtLevelStatic SoundFXDOF("DropTarget_Up", 113, DOFPulse, DOFContactors), DTSoundLevel, DT_ETERNAL_6
        DOF 118, DOFPulse
		PlaySoundAtLevelStatic ("SFX_Droptargets_Finished_1"), SFXSoundLevel, BULB_DS_6
		If L046.State = 1 Then
			L046.State = 0
			L053.State = 1
			AddBonusMultiplier 1
		End If
		L045.State = 1
		L009.State = 1
		If bScoreBonusLit = True And L047.State = 1 Then ADDSCOREBONUS
		CHECKEXTRABALL
		CHECKORBITBONUS
		CHECKETERNAL
		CHECKSCOREBONUS
    End If
End Sub

'/////////////////////////////  DROPTARGETS DOOM BANK  ////////////////////////////

Sub DT_DOOM_1_Dropped
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
    If DT_DOOM_1.IsDropped = 1 And DT_DOOM_2.IsDropped = 0 And DT_DOOM_3.IsDropped = 0 And DT_DOOM_4.IsDropped = 0 And bDoomFailed = False Then
		bDoomFailed = False
	Else
		bDoomFailed = True
	End If
	PlaySoundAtLevelStatic ("DropTarget_Down"), DTSoundLevel, DT_DOOM_1
	RandomSoundDroptargets
	AddDropTargetScore
	If bDropTargetFrenzy = True And bWizardMode = False Then
		DTFrenzyHits = DTFrenzyHits + 1
		If DTFrenzyHits = 5 Then
			DTFrenzyHits = 0
			AddDTFrenzyLevel(1)
		End If
	End If
	AddBonus 1
    LastSwitchHit = "DT_DOOM_1"
	CHECKDOOMSKILLSHOT
	L048.State = 1
	RESET_DROPTARGETS_DOOM
End Sub

Sub DT_DOOM_2_Dropped
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
    If DT_DOOM_1.IsDropped = 1 And DT_DOOM_2.IsDropped = 1 And DT_DOOM_3.IsDropped = 0 And DT_DOOM_4.IsDropped = 0 And bDoomFailed = False Then
		bDoomFailed = False
	Else
		bDoomFailed = True
	End If
	PlaySoundAtLevelStatic ("DropTarget_Down"), DTSoundLevel, DT_DOOM_2
	RandomSoundDroptargets
	AddDropTargetScore
	If bDropTargetFrenzy = True And bWizardMode = False Then
		DTFrenzyHits = DTFrenzyHits + 1
		If DTFrenzyHits = 5 Then
			DTFrenzyHits = 0
			AddDTFrenzyLevel(1)
		End If
	End If
	AddBonus 1
    LastSwitchHit = "DT_DOOM_2"
	CHECKDOOMSKILLSHOT
	L049.State = 1
	RESET_DROPTARGETS_DOOM
End Sub

Sub DT_DOOM_3_Dropped
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
    If DT_DOOM_1.IsDropped = 1 And DT_DOOM_2.IsDropped = 1 And DT_DOOM_3.IsDropped = 1 And DT_DOOM_4.IsDropped = 0 And bDoomFailed = False Then
		bDoomFailed = False
	Else
		bDoomFailed = True
	End If
	PlaySoundAtLevelStatic ("DropTarget_Down"), DTSoundLevel, DT_DOOM_3
	RandomSoundDroptargets
	If bDropTargetFrenzy = True And bWizardMode = False Then
		DTFrenzyHits = DTFrenzyHits + 1
		If DTFrenzyHits = 5 Then
			DTFrenzyHits = 0
			AddDTFrenzyLevel(1)
		End If
	End If
	AddDropTargetScore
	AddBonus 1
    LastSwitchHit = "DT_DOOM_3"
	CHECKDOOMSKILLSHOT
	L050.State = 1
	RESET_DROPTARGETS_DOOM
End Sub

Sub DT_DOOM_4_Dropped
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
    If DT_DOOM_1.IsDropped = 1 And DT_DOOM_2.IsDropped = 1 And DT_DOOM_3.IsDropped = 1 And DT_DOOM_4.IsDropped = 1 And bDoomFailed = False Then
		bDoomFailed = False
	Else
		bDoomFailed = True
	End If
	PlaySoundAtLevelStatic ("DropTarget_Down"), DTSoundLevel, DT_DOOM_4
	RandomSoundDroptargets
	If bDropTargetFrenzy = True And bWizardMode = False Then
		DTFrenzyHits = DTFrenzyHits + 1
		If DTFrenzyHits = 5 Then
			DTFrenzyHits = 0
			AddDTFrenzyLevel(1)
		End If
	End If
	AddDropTargetScore
	AddBonus 1
    LastSwitchHit = "DT_DOOM_4"
	CHECKDOOMSKILLSHOT
	L051.State = 1
	RESET_DROPTARGETS_DOOM
End Sub

Sub RESET_DROPTARGETS_DOOM
    If DT_DOOM_1.IsDropped And DT_DOOM_2.IsDropped And DT_DOOM_3.IsDropped And DT_DOOM_4.IsDropped Then
        DT_DOOM_1.IsDropped = 0
        DT_DOOM_2.IsDropped = 0
        DT_DOOM_3.IsDropped = 0
        DT_DOOM_4.IsDropped = 0
        FLASHEFFECT(1)
		If bDoomFailed = False Then
			AwardDoom
		Else
			AddScore 7500
			bDoomFailed = False
		End If
		PlaySoundAtLevelStatic SoundFXDOF("DropTarget_Up", 112, DOFPulse, DOFContactors), DTSoundLevel, DT_DOOM_3
        DOF 118, DOFPulse
		PlaySoundAtLevelStatic ("SFX_Droptargets_Finished_1"), SFXSoundLevel, BULB_DS_6
		If L053.State = 1 Then
			L053.State = 0
			L039.State = 1
			AddBonusMultiplier 1
		End If
		L052.State = 1
		L010.State = 1
		L048.State = 0
		L049.State = 0
		L050.State = 0
		L051.State = 0
		If bScoreBonusLit = True And L054.State = 1 Then ADDSCOREBONUS
		CHECKEXTRABALL
		CHECKORBITBONUS
		CHECKSCOREBONUS
    End If
End Sub

'/////////////////////////////  SKILLSHOT  ////////////////////////////

Sub T_SKILLSHOT_Hit
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
    LastSwitchHit = "T_SKILLSHOT"
    CHECKSKILLSHOT
End Sub

'/////////////////////////////  ORBITS  ////////////////////////////

Sub T_LEFTORBIT_Hit
    If Tilted Then Exit Sub
    If LastSwitchHit = "T_RIGHTORBIT" And Not OrbitLastDirection = "LEFT" Then
		FLASHEFFECT(2)
		'PlaySoundAtLevelStatic ("SFX_Orbit_1"), SFXSoundLevel, BULB_DS_6
		AddOrbitScore
		If bOrbitMania = True And bWizardMode = False Then
			OrbitManiaHits = OrbitManiaHits + 1
			If OrbitManiaHits = 3 Then
				OrbitManiaHits = 0
				AddOrbitManiaLevel(1)
			End If
		End If 
		AddOrbit(1)
		OrbitLastDirection = "RIGHT"
	Else
		CHECKLOOPSKILLSHOT
		If L021.State = 2 Then
			bJackpotReady = True
			TURNMAGNETON
		End If
		OrbitLastDirection = "LEFT"
	End If
	LastSwitchHit = "T_LEFTORBIT"
End Sub

Sub T_RIGHTORBIT_Hit
    If Tilted Then Exit Sub
    If LastSwitchHit = "T_LEFTORBIT" And Not OrbitLastDirection = "RIGHT" Then
		FLASHEFFECT(2)
		'PlaySoundAtLevelStatic ("SFX_Orbit_1"), SFXSoundLevel, BULB_DS_6
		AddOrbitScore
		If bOrbitMania = True And bWizardMode = False Then
			OrbitManiaHits = OrbitManiaHits + 1
			If OrbitManiaHits = 3 Then
				OrbitManiaHits = 0
				AddOrbitManiaLevel(1)
			End If
		End If
		AddOrbit(1)
		OrbitLastDirection = "LEFT"
	Else
		OrbitLastDirection = "RIGHT"
	End If
	LastSwitchHit = "T_RIGHTORBIT"
End Sub

Sub T_MAGNETGRAB_Hit
    If Tilted Then Exit Sub
    If bMagnetOn = True Then
		If bOrbitAwardReady = True Then
			FLASHEFFECT(8)
			bOrbitAwardReady = False
			OrbitLevel = 0
			UpdateOrbitLights(0)
			AddOrbitAwardLevel(1)
			If bModeReady = True Then
				vpmtimer.AddTimer 3000, "CHECKIFMODEISREADY '"
			Else
				vpmtimer.AddTimer 3000, "TURNMAGNETOFF '"
			End If
		Else
			CHECKIFMODEISREADY
		End If
	End If
End Sub

'/////////////////////////////  ETERNAL MODES  ////////////////////////////

Sub CHECKIFMODEISREADY
	If bSuperPops = False Then
		If bJackpotReady = True Then
			vpmtimer.AddTimer 1000, "TURNMAGNETOFF '"
			AwardJackpot
			L021.State = 0
		Else
			vpmtimer.AddTimer 3000, "TURNMAGNETOFF '"
		End If
	Else
		vpmtimer.AddTimer 1000, "TURNMAGNETOFF '"
		vpmtimer.AddTimer 3500, "TURNMAGNETON '"
	End If
	If bModeReady = True Then
		bModeReady = False
		bModeOn = True
		UpdateOrbitLights(OrbitLevel)
		TURNONTURNTABLE
		STARTMODE
	End If
End Sub

Dim ModeSelected : ModeSelected = 0

Sub SELECTMODE
	If bWizardModeReady(CurrentPlayer) = False Then
		ModeSelected = (ModeSelected + 1)MOD 5
		UPDATEMODE
	End If
End Sub

Sub UPDATEMODE
	If L001.State = 2 Then L001.State = 0
	If L002.State = 2 Then L002.State = 0
	If L003.State = 2 Then L003.State = 0
	If L004.State = 2 Then L004.State = 0
	If L005.State = 2 Then L005.State = 0
    Select Case ModeSelected
        Case 0:
			If L001.State = 0 Then
				L001.State = 2
			Else
				SELECTMODE
			End If
        Case 1:
			If L002.State = 0 Then
				L002.State = 2
			Else
				SELECTMODE
			End If
        Case 2:
			If L003.State = 0 Then
				L003.State = 2
			Else
				SELECTMODE
			End If
        Case 3:
			If L004.State = 0 Then
				L004.State = 2
			Else
				SELECTMODE
			End If
        Case 4:
			If L005.State = 0 Then
				L005.State = 2
			Else
				SELECTMODE
			End If
    End Select
End Sub

Sub STARTMODE
	FLASHEFFECT(5)
	'ChangeRandomSong
	'PlaySoundAtLevelStatic ("SFX_Mode_Start_1"), SFXSoundLevel, BULB_DS_6
	If bWizardModeReady(CurrentPlayer) = False Then
		SuperJackpot = SuperJackpot + 100000
		Select Case ModeSelected
				Case 0:
					b2BallMultiball = True
					MultiballHits = 0
					AddMultiball 1
					EnableBallSaver 25
					AddPlayfieldMultiplier(1)
					DMD CL("MULTIBALL"), CL("2 X SCORING"), "_", eNone, eBlink, eNone, 3000, True, ""
					RandomSoundMultiball()
				Case 1:
					bOrbitMania = True
					OrbitManiaHits = 0
					AddOrbitManiaLevel(1)
					L022.State = 2
					L023.State = 2
					EndModeCountdown = 50
					ENDMODETIMER.Enabled = 1
					PlaySoundAtLevelStatic ("VO_Super_Orbits"), VoiceOverSoundLevel, BULB_DS_6
				Case 2:
					bDropTargetFrenzy = True
					DTFrenzyHits = 0
					AddDTFrenzyLevel(1)
					EndModeCountdown = 50
					ENDMODETIMER.Enabled = 1
					STARTDTFRENZYLIGHTS
					PlaySoundAtLevelStatic ("VO_Super_Drops"), VoiceOverSoundLevel, BULB_DS_6
				Case 3:
					bSuperSpinners = True
					SuperSpinnersHits = 0
					AddSuperSpinnersLevel(1)
					EndModeCountdown = 50
					ENDMODETIMER.Enabled = 1
					PlaySoundAtLevelStatic ("VO_Super_Spinners"), VoiceOverSoundLevel, BULB_DS_6
				Case 4:
					bSuperPops = True
					L055.State = 2
					SuperPopsHits = 0
					AddSuperPopsLevel(1)
					BUMPEREFFECTTIMER.Enabled = 1
					vpmtimer.AddTimer 3500, "TURNMAGNETON '"
					EndModeCountdown = 50
					ENDMODETIMER.Enabled = 1
					PlaySoundAtLevelStatic ("VO_Super_Bumpers"), VoiceOverSoundLevel, BULB_DS_6
		End Select
	Else
		bWizardModeReady(CurrentPlayer) = False
		bWizardMode = True
		b2BallMultiball = True
		bOrbitMania = True
		bDropTargetFrenzy = True
		bSuperSpinners = True
		bSuperPops = True
		AddMultiball 2
		EnableBallSaver 25
		AddPlayfieldMultiplier(1)
		MultiballHits = 0
		DMD CL("WIZARD MODE"), CL("2 X SCORING"), "_", eNone, eBlink, eNone, 1000, True, ""
		RandomSoundMultiball()
		AddOrbitManiaLevel(0)
		AddDTFrenzyLevel(0)
		AddSuperSpinnersLevel(0)
		AddSuperPopsLevel(0)
	End If
End Sub

Sub ENDMODETIMER_Timer
    EndModeCountdown = EndModeCountdown - 1
    Select Case EndModeCountdown
        Case 10: DMD "_", CL("10"), "_", eNone, eNone, eNone, 500, True, "" : PlaySoundAtLevelStatic ("VO_Countdown_10"), VoiceOverSoundLevel, BULB_DS_6
        Case 9: DMD "_", CL("9"), "_", eNone, eNone, eNone, 500, True, "" : PlaySoundAtLevelStatic ("VO_Countdown_9"), VoiceOverSoundLevel, BULB_DS_6
        Case 8: DMD "_", CL("8"), "_", eNone, eNone, eNone, 500, True, "" : PlaySoundAtLevelStatic ("VO_Countdown_8"), VoiceOverSoundLevel, BULB_DS_6
        Case 7: DMD "_", CL("7"), "_", eNone, eNone, eNone, 500, True, "" : PlaySoundAtLevelStatic ("VO_Countdown_7"), VoiceOverSoundLevel, BULB_DS_6
        Case 6: DMD "_", CL("6"), "_", eNone, eNone, eNone, 500, True, "" : PlaySoundAtLevelStatic ("VO_Countdown_6"), VoiceOverSoundLevel, BULB_DS_6
        Case 5: DMD "_", CL("5"), "_", eNone, eNone, eNone, 500, True, "" : PlaySoundAtLevelStatic ("VO_Countdown_5"), VoiceOverSoundLevel, BULB_DS_6
        Case 4: DMD "_", CL("4"), "_", eNone, eNone, eNone, 500, True, "" : PlaySoundAtLevelStatic ("VO_Countdown_4"), VoiceOverSoundLevel, BULB_DS_6
        Case 3: DMD "_", CL("3"), "_", eNone, eNone, eNone, 500, True, "" : PlaySoundAtLevelStatic ("VO_Countdown_3"), VoiceOverSoundLevel, BULB_DS_6
        Case 2: DMD "_", CL("2"), "_", eNone, eNone, eNone, 500, True, "" : PlaySoundAtLevelStatic ("VO_Countdown_2"), VoiceOverSoundLevel, BULB_DS_6
        Case 1: DMD "_", CL("1"), "_", eNone, eNone, eNone, 500, True, "" : PlaySoundAtLevelStatic ("VO_Countdown_1"), VoiceOverSoundLevel, BULB_DS_6
        Case 0:
			DMD CL("SUPER JACKPOT IS LIT"), CL(FormatScore(SuperJackpot)), "_", eNone, eBlink, eNone, 3500, True, ""
			PlaySoundAtLevelStatic ("VO_Super_Jackpot_Lit"), VoiceOverSoundLevel, BULB_DS_6
			RandomSoundFanfare
			FLASHEFFECT(7)
			L064.State = 2
			L035.State = 2
			L036.State = 2
			L037.State = 2
			L041.State = 2
			L042.State = 2
			L043.State = 2
			L044.State = 2
			L048.State = 0
			L049.State = 0
			L050.State = 0
			L051.State = 0
			RESET_ALL_DROPS
			STOPMODE
    End Select
End Sub

Sub STOPMODE
    ENDMODETIMER.Enabled = 0
	BUMPEREFFECTTIMER.Enabled = 0
	If b2BallMultiball = True Then L001.State = 1 : b2BallMultiballFinished(CurrentPlayer) = True : b2BallMultiball = False
	If bOrbitMania = True Then L002.State = 1 : bOrbitManiaFinished(CurrentPlayer) = True : bOrbitMania = False
	If bDropTargetFrenzy = True Then L003.State = 1 : bDTFrenzyFinished(CurrentPlayer) = True : STOPDTFRENZYLIGHTS : bDropTargetFrenzy = False
	If bSuperSpinners = True Then L004.State = 1 : bSuperSpinnersFinished(CurrentPlayer) = True : bSuperSpinners = False
	If bSuperPops = True Then L005.State = 1 : bSuperPopsFinished(CurrentPlayer) = True : bSuperPops = False
	If bWizardMode = True Then
		L001.State = 0
		L002.State = 0
		L003.State = 0
		L004.State = 0
		L005.State = 0
		L015.State = 0
		b2BallMultiballFinished(CurrentPlayer) = False
		bOrbitManiaFinished(CurrentPlayer) = False
		bDTFrenzyFinished(CurrentPlayer) = False
		bSuperSpinnersFinished(CurrentPlayer) = False
		bSuperPopsFinished(CurrentPlayer) = False
		bWizardMode = False
		OrbitManiaLevel(CurrentPlayer) = 0
		DTFrenzyLevel(CurrentPlayer) = 0
		SuperSpinnersLevel(CurrentPlayer) = 0
		SuperPopsLevel(CurrentPlayer) = 0
	End If
    bModeReady = False
	bModeOn = False
    If bOrbitAwardReady = False And bModeReady = False Then TURNMAGNETOFF
	L055.State = 0
	L022.State = 0
	L023.State = 0
	If TURNTABLEMOTOROFF = False Then TURNOFFTURNTABLE
	If b2BallMultiballFinished(CurrentPlayer) = True And bOrbitManiaFinished(CurrentPlayer) = True And bDTFrenzyFinished(CurrentPlayer) = True And bSuperSpinnersFinished(CurrentPlayer) = True And bSuperPopsFinished(CurrentPlayer) = True And bWizardModeReady(CurrentPlayer) = False Then
		L015.State = 2
		bWizardModeReady(CurrentPlayer) = True
	Else
		SELECTMODE
	End If
End Sub

'/////////////////////////////  MULTIBALL  ////////////////////////////

Sub CHECK2BALLMULTIBALLHITS
	If b2BallMultiball = True And L021.State = 0 Then
		MultiballHits = MultiballHits + 1
		If MultiballHits = 20 Then
			MultiballHits = 0
			L021.State = 2
			DMD CL("JACKPOT IS LIT"), CL(FormatScore(Jackpot(CurrentPlayer))), "_", eNone, eBlinkFast, eNone, 1000, True, ""
		End If
	End If
End Sub

Sub STOPMULTIBALL
	bJackpotReady = False
	PlayfieldMultiplier(CurrentPlayer) = 0
	DMD CL("SUPER JACKPOT IS LIT"), CL(FormatScore(SuperJackpot)), "_", eNone, eBlink, eNone, 3500, True, ""
	RandomSoundFanfare
	PlaySoundAtLevelStatic ("VO_Super_Jackpot_Lit"), VoiceOverSoundLevel, BULB_DS_6
	L021.State = 0
	L064.State = 2
	L035.State = 2
	L036.State = 2
	L037.State = 2
	L041.State = 2
	L042.State = 2
	L043.State = 2
	L044.State = 2
	L048.State = 0
	L049.State = 0
	L050.State = 0
	L051.State = 0
	RESET_ALL_DROPS
End Sub

'/////////////////////////////  SUPER BUMPERS  ////////////////////////////

Sub AddSuperPopsLevel(c)
    Dim NewSuperPopsLevel
    If(SuperPopsLevel(CurrentPlayer) + c <= MaxSuperPopsLevel) Then
        NewSuperPopsLevel = SuperPopsLevel(CurrentPlayer) + c
        SetSuperPopsLevel(NewSuperPopsLevel)
    End If
End Sub

Sub SetSuperPopsLevel(Level)
    SuperPopsLevel(CurrentPlayer) = Level
    AddSuperPops(Level)
End Sub

Sub AddSuperPops(Level)
	Dim SuperPopsCurrentValue
	If bWizardMode = False Then SuperPopsValue(CurrentPlayer) = SuperPopsValue(CurrentPlayer) + 5000
	SuperPopsCurrentValue = BumperValue(CurrentPlayer) + SuperPopsValue(CurrentPlayer) * BumperMultiplier(CurrentPlayer)
    Select Case Level
            Case 1: DMD CL("SUPER POP BUMPERS"), CL("LEVEL 1 " &SuperPopsCurrentValue), "_", eNone, eBlink, eNone, 3000, True, ""
            Case 2: DMD CL("SUPER POP BUMPERS"), CL("LEVEL 2 " &SuperPopsCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
            Case 3: DMD CL("SUPER POP BUMPERS"), CL("LEVEL 3 " &SuperPopsCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
            Case 4: DMD CL("SUPER POP BUMPERS"), CL("LEVEL 4 " &SuperPopsCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
            Case 5: DMD CL("SUPER POP BUMPERS"), CL("LEVEL 5 " &SuperPopsCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
    End Select
End Sub

'/////////////////////////////  SUPER SPINNERS  ////////////////////////////

Sub AddSuperSpinnersLevel(c)
    Dim NewSuperSpinnersLevel
    If(SuperSpinnersLevel(CurrentPlayer) + c <= MaxSuperSpinnersLevel) Then
        NewSuperSpinnersLevel = SuperSpinnersLevel(CurrentPlayer) + c
        SetSuperSpinnersLevel(NewSuperSpinnersLevel)
    End If
End Sub

Sub SetSuperSpinnersLevel(Level)
    SuperSpinnersLevel(CurrentPlayer) = Level
    AddSuperSpinners(Level)
End Sub

Sub AddSuperSpinners(Level)
	Dim SuperSpinnersCurrentValue
	If bWizardMode = False Then SuperSpinnersValue(CurrentPlayer) = SuperSpinnersValue(CurrentPlayer) + 300
	If bWizardMode = False Then SuperSpinnersFirstHitValue(CurrentPlayer) = SuperSpinnersFirstHitValue(CurrentPlayer) + 10000
	SuperSpinnersCurrentValue = SpinnerValue(CurrentPlayer) + SuperSpinnersValue(CurrentPlayer) * SpinnerMultiplier(CurrentPlayer)
    Select Case Level
            Case 1: DMD CL("SUPER SPINNERS"), CL("LEVEL 1 " &SuperSpinnersCurrentValue), "_", eNone, eBlink, eNone, 3000, True, ""
            Case 2: DMD CL("SUPER SPINNERS"), CL("LEVEL 2 " &SuperSpinnersCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
            Case 3: DMD CL("SUPER SPINNERS"), CL("LEVEL 3 " &SuperSpinnersCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
            Case 4: DMD CL("SUPER SPINNERS"), CL("LEVEL 4 " &SuperSpinnersCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
            Case 5: DMD CL("SUPER SPINNERS"), CL("LEVEL 5 " &SuperSpinnersCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
    End Select
End Sub

'/////////////////////////////  DROP TARGET FRENZY  ////////////////////////////

Sub STARTDTFRENZYLIGHTS
    LIGHTSEQDROPS.Play SeqBlinking, , 5, 150
End Sub

Sub LIGHTSEQDROPS_PlayDone()
    STARTDTFRENZYLIGHTS
End Sub

Sub STOPDTFRENZYLIGHTS
    LIGHTSEQDROPS.StopPlay
End Sub

Sub AddDTFrenzyLevel(c)
    Dim NewDTFrenzyLevel
    If(DTFrenzyLevel(CurrentPlayer) + c <= MaxDTFrenzyLevel) Then
        NewDTFrenzyLevel = DTFrenzyLevel(CurrentPlayer) + c
        SetDTFrenzyLevel(NewDTFrenzyLevel)
    End If
End Sub

Sub SetDTFrenzyLevel(Level)
    DTFrenzyLevel(CurrentPlayer) = Level
    AddDTFrenzy(Level)
End Sub

Sub AddDTFrenzy(Level)
	Dim DTFrenzyCurrentValue
	If bWizardMode = False Then DTFrenzyValue(CurrentPlayer) = DTFrenzyValue(CurrentPlayer) + 7500
	DTFrenzyCurrentValue = DropTargetValue(CurrentPlayer) + DTFrenzyValue(CurrentPlayer)
    Select Case Level
            Case 1: DMD CL("DROP TARGET FRENZY"), CL("LEVEL 1 " &DTFrenzyCurrentValue), "_", eNone, eBlink, eNone, 3000, True, ""
            Case 2: DMD CL("DROP TARGET FRENZY"), CL("LEVEL 2 " &DTFrenzyCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
            Case 3: DMD CL("DROP TARGET FRENZY"), CL("LEVEL 3 " &DTFrenzyCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
            Case 4: DMD CL("DROP TARGET FRENZY"), CL("LEVEL 4 " &DTFrenzyCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
            Case 5: DMD CL("DROP TARGET FRENZY"), CL("LEVEL 5 " &DTFrenzyCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
    End Select
End Sub

'/////////////////////////////  ORBIT MANIA  ////////////////////////////

Sub AddOrbitManiaLevel(c)
    Dim NewOrbitManiaLevel
    If(OrbitManiaLevel(CurrentPlayer) + c <= MaxOrbitManiaLevel) Then
        NewOrbitManiaLevel = OrbitManiaLevel(CurrentPlayer) + c
        SetOrbitManiaLevel(NewOrbitManiaLevel)
    End If
End Sub

Sub SetOrbitManiaLevel(Level)
    OrbitManiaLevel(CurrentPlayer) = Level
    AddOrbitMania(Level)
End Sub

Sub AddOrbitMania(Level)
	Dim OrbitManiaCurrentValue
	If bWizardMode = False Then OrbitManiaValue(CurrentPlayer) = OrbitManiaValue(CurrentPlayer) + 25000
	OrbitManiaCurrentValue = OrbitValue(CurrentPlayer) + OrbitManiaValue(CurrentPlayer) * OrbitMultiplier(CurrentPlayer)
    Select Case Level
            Case 1: DMD CL("ORBIT MANIA"), CL("LEVEL 1 " &OrbitManiaCurrentValue), "_", eNone, eBlink, eNone, 3000, True, ""
            Case 2: DMD CL("ORBIT MANIA"), CL("LEVEL 2 " &OrbitManiaCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
            Case 3: DMD CL("ORBIT MANIA"), CL("LEVEL 3 " &OrbitManiaCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
            Case 4: DMD CL("ORBIT MANIA"), CL("LEVEL 4 " &OrbitManiaCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
            Case 5: DMD CL("ORBIT MANIA"), CL("LEVEL 5 " &OrbitManiaCurrentValue), "_", eNone, eBlink, eNone, 3000, True, "" : RandomSoundFanfare
    End Select
End Sub

'/////////////////////////////  ORBIT MODES  ////////////////////////////

Sub AddOrbitAwardLevel(q)
    Dim NewOrbitAwardLevel
	'PlaySoundAtLevelStatic ("SFX_Doomslayer_Hit_1"), SFXSoundLevel, BULB_DS_6
    If(OrbitAwardLevel(CurrentPlayer) + q <= MaxOrbitAwardLevel) Then
        NewOrbitAwardLevel = OrbitAwardLevel(CurrentPlayer) + q
        SetOrbitAwardLevel(NewOrbitAwardLevel)
	Else
		AddScoreNoMultiplier 250000
        DMD CL("MULTIPASS AWARD"), CL("250000"), "_", eNone, eBlink, eNone, 3000, True, ""
    End If
End Sub

Sub SetOrbitAwardLevel(Level)
    OrbitAwardLevel(CurrentPlayer) = Level
    AddOrbitAward(Level)
End Sub

Sub AddOrbitAward(Level)
    Select Case Level
            Case 1: DMD CL("MULTIPASS AWARD"), CL("2 X SPINNERS"), "_", eNone, eBlink, eNone, 3000, True, "" : SpinnerMultiplier(CurrentPlayer) = 2
            Case 2: DMD CL("MULTIPASS AWARD"), CL("2 X ORBITS"), "_", eNone, eBlink, eNone, 3000, True, "" : OrbitMultiplier(CurrentPlayer) = 2
            Case 3: DMD CL("MULTIPASS AWARD"), CL("2 X BUMPERS"), "_", eNone, eBlink, eNone, 3000, True, "" : BumperMultiplier(CurrentPlayer) = 2
            Case 4: DMD CL("MULTIPASS AWARD"), CL("3 X SPINNERS"), "_", eNone, eBlink, eNone, 3000, True, "" : SpinnerMultiplier(CurrentPlayer) = 3
            Case 5: DMD CL("MULTIPASS AWARD"), CL("3 X ORBITS"), "_", eNone, eBlink, eNone, 3000, True, "" : OrbitMultiplier(CurrentPlayer) = 3
            Case 6: DMD CL("MULTIPASS AWARD"), CL("3 X BUMPERS"), "_", eNone, eBlink, eNone, 3000, True, "" : BumperMultiplier(CurrentPlayer) = 3
            Case 7: DMD CL("MULTIPASS AWARD"), CL("4 X SPINNERS"), "_", eNone, eBlink, eNone, 3000, True, "" : SpinnerMultiplier(CurrentPlayer) = 4
            Case 8: DMD CL("MULTIPASS AWARD"), CL("4 X ORBITS"), "_", eNone, eBlink, eNone, 3000, True, "" : OrbitMultiplier(CurrentPlayer) = 4
            Case 9: DMD CL("MULTIPASS AWARD"), CL("4 X BUMPERS"), "_", eNone, eBlink, eNone, 3000, True, "" : BumperMultiplier(CurrentPlayer) = 4
    End Select
End Sub

Sub AddOrbit(p)
    Dim NewOrbitLevel
    If(OrbitLevel + p <= MaxOrbitLevel) Then
        NewOrbitLevel = OrbitLevel + p
        SetOrbitLevel(NewOrbitLevel)
		If OrbitLevel = MaxOrbitLevel Then
			'PlaySoundAtLevelStatic ("SFX_Doomslayer_Lit_1"), SFXSoundLevel, BULB_DS_6
			TURNMAGNETON
			bOrbitAwardReady = True
		End If
    End if
End Sub

Sub SetOrbitLevel(Level)
    OrbitLevel = Level
    UpdateOrbitLights(Level)
End Sub

Sub UpdateOrbitLights(Level)
    Select Case Level
            Case 0: L065.State = 0 : L066.State = 0 : L067.State = 0 : L068.State = 0 : L069.State = 0 : L070.State = 0 : L071.State = 0 : L072.State = 0 : L073.State = 0 : L074.State = 0 : L022.State = 0 : L023.State = 0 : FB_DS_1.Visible = 0 : FB_DS_2.Visible = 0 : FB_DS_3.Visible = 0 : FB_DS_4.Visible = 0 : FB_DS_5.Visible = 0 : FB_DS_6.Visible = 0 : FB_DS_7.Visible = 0 : FB_DS_8.Visible = 0 : FB_DS_9.Visible = 0 : FB_DS_10.Visible = 0
            Case 1: L065.State = 1 : L066.State = 0 : L067.State = 0 : L068.State = 0 : L069.State = 0 : L070.State = 0 : L071.State = 0 : L072.State = 0 : L073.State = 0 : L074.State = 0 : L022.State = 0 : L023.State = 0 : FB_DS_1.Visible = 1 : FB_DS_2.Visible = 0 : FB_DS_3.Visible = 0 : FB_DS_4.Visible = 0 : FB_DS_5.Visible = 0 : FB_DS_6.Visible = 0 : FB_DS_7.Visible = 0 : FB_DS_8.Visible = 0 : FB_DS_9.Visible = 0 : FB_DS_10.Visible = 0
            Case 2: L065.State = 1 : L066.State = 1 : L067.State = 0 : L068.State = 0 : L069.State = 0 : L070.State = 0 : L071.State = 0 : L072.State = 0 : L073.State = 0 : L074.State = 0 : L022.State = 0 : L023.State = 0 : FB_DS_1.Visible = 1 : FB_DS_2.Visible = 1 : FB_DS_3.Visible = 0 : FB_DS_4.Visible = 0 : FB_DS_5.Visible = 0 : FB_DS_6.Visible = 0 : FB_DS_7.Visible = 0 : FB_DS_8.Visible = 0 : FB_DS_9.Visible = 0 : FB_DS_10.Visible = 0
            Case 3: L065.State = 1 : L066.State = 1 : L067.State = 1 : L068.State = 0 : L069.State = 0 : L070.State = 0 : L071.State = 0 : L072.State = 0 : L073.State = 0 : L074.State = 0 : L022.State = 0 : L023.State = 0 : FB_DS_1.Visible = 1 : FB_DS_2.Visible = 1 : FB_DS_3.Visible = 1 : FB_DS_4.Visible = 0 : FB_DS_5.Visible = 0 : FB_DS_6.Visible = 0 : FB_DS_7.Visible = 0 : FB_DS_8.Visible = 0 : FB_DS_9.Visible = 0 : FB_DS_10.Visible = 0
            Case 4: L065.State = 1 : L066.State = 1 : L067.State = 1 : L068.State = 1 : L069.State = 0 : L070.State = 0 : L071.State = 0 : L072.State = 0 : L073.State = 0 : L074.State = 0 : L022.State = 0 : L023.State = 0 : FB_DS_1.Visible = 1 : FB_DS_2.Visible = 1 : FB_DS_3.Visible = 1 : FB_DS_4.Visible = 1 : FB_DS_5.Visible = 0 : FB_DS_6.Visible = 0 : FB_DS_7.Visible = 0 : FB_DS_8.Visible = 0 : FB_DS_9.Visible = 0 : FB_DS_10.Visible = 0
            Case 5: L065.State = 1 : L066.State = 1 : L067.State = 1 : L068.State = 1 : L069.State = 1 : L070.State = 0 : L071.State = 0 : L072.State = 0 : L073.State = 0 : L074.State = 0 : L022.State = 0 : L023.State = 0 : FB_DS_1.Visible = 1 : FB_DS_2.Visible = 1 : FB_DS_3.Visible = 1 : FB_DS_4.Visible = 1 : FB_DS_5.Visible = 1 : FB_DS_6.Visible = 0 : FB_DS_7.Visible = 0 : FB_DS_8.Visible = 0 : FB_DS_9.Visible = 0 : FB_DS_10.Visible = 0
            Case 6: L065.State = 1 : L066.State = 1 : L067.State = 1 : L068.State = 1 : L069.State = 1 : L070.State = 1 : L071.State = 0 : L072.State = 0 : L073.State = 0 : L074.State = 0 : L022.State = 0 : L023.State = 0 : FB_DS_1.Visible = 1 : FB_DS_2.Visible = 1 : FB_DS_3.Visible = 1 : FB_DS_4.Visible = 1 : FB_DS_5.Visible = 1 : FB_DS_6.Visible = 1 : FB_DS_7.Visible = 0 : FB_DS_8.Visible = 0 : FB_DS_9.Visible = 0 : FB_DS_10.Visible = 0
            Case 7: L065.State = 1 : L066.State = 1 : L067.State = 1 : L068.State = 1 : L069.State = 1 : L070.State = 1 : L071.State = 1 : L072.State = 0 : L073.State = 0 : L074.State = 0 : L022.State = 0 : L023.State = 0 : FB_DS_1.Visible = 1 : FB_DS_2.Visible = 1 : FB_DS_3.Visible = 1 : FB_DS_4.Visible = 1 : FB_DS_5.Visible = 1 : FB_DS_6.Visible = 1 : FB_DS_7.Visible = 1 : FB_DS_8.Visible = 0 : FB_DS_9.Visible = 0 : FB_DS_10.Visible = 0
            Case 8: L065.State = 1 : L066.State = 1 : L067.State = 1 : L068.State = 1 : L069.State = 1 : L070.State = 1 : L071.State = 1 : L072.State = 1 : L073.State = 0 : L074.State = 0 : L022.State = 0 : L023.State = 0 : FB_DS_1.Visible = 1 : FB_DS_2.Visible = 1 : FB_DS_3.Visible = 1 : FB_DS_4.Visible = 1 : FB_DS_5.Visible = 1 : FB_DS_6.Visible = 1 : FB_DS_7.Visible = 1 : FB_DS_8.Visible = 1 : FB_DS_9.Visible = 0 : FB_DS_10.Visible = 0
            Case 9: L065.State = 1 : L066.State = 1 : L067.State = 1 : L068.State = 1 : L069.State = 1 : L070.State = 1 : L071.State = 1 : L072.State = 1 : L073.State = 1 : L074.State = 0 : L022.State = 0 : L023.State = 0 : FB_DS_1.Visible = 1 : FB_DS_2.Visible = 1 : FB_DS_3.Visible = 1 : FB_DS_4.Visible = 1 : FB_DS_5.Visible = 1 : FB_DS_6.Visible = 1 : FB_DS_7.Visible = 1 : FB_DS_8.Visible = 1 : FB_DS_9.Visible = 1 : FB_DS_10.Visible = 0
            Case 10: L065.State = 2 : L066.State = 2 : L067.State = 2 : L068.State = 2 : L069.State = 2 : L070.State = 2 : L071.State = 2 : L072.State = 2 : L073.State = 2 : L074.State = 2 : L022.State = 2 : L023.State = 2 : FB_DS_1.Visible = 1 : FB_DS_2.Visible = 1 : FB_DS_3.Visible = 1 : FB_DS_4.Visible = 1 : FB_DS_5.Visible = 1 : FB_DS_6.Visible = 1 : FB_DS_7.Visible = 1 : FB_DS_8.Visible = 1 : FB_DS_9.Visible = 1 : FB_DS_10.Visible = 1 : DMD CL("MULTIPASS"), CL("SHOOT THE ORBITS"), "_", eNone, eBlink, eNone, 2500, True, ""
    End Select
	If bOrbitMania = True Then L022.State = 2 : L023.State = 2 End If
End Sub

'/////////////////////////////  TOP MIDDLE LANE  ////////////////////////////

Sub T_TOPLANE_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "T_TOPLANE"
    CHECKSUPERSKILLSHOT
End Sub

'/////////////////////////////  INLANES  ////////////////////////////

Sub T_INLANE_LEFT_Hit
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
    LastSwitchHit = "T_INLANE_LEFT"
	AddScore(150)
    If L016.State = 1 Then
        AwardExtraBall
		L016.State = 0
    End If
End Sub

Sub T_INLANE_RIGHT_Hit
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
    LastSwitchHit = "T_INLANE_RIGHT"
	AddScore(150)
    If L017.State = 1 Then
        AwardExtraBall
		L017.State = 0
    End If
End Sub

'/////////////////////////////  OUTLANES  ////////////////////////////

Sub T_OUTLANE_LEFT_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "T_OUTLANE_LEFT"
	AddScore(450)
	If L006.State = 2 Then
		AwardSpecial
		L006.State = 0
	Else
		'PlaySoundAtLevelStatic ("SFX_Outlane_1"), SFXSoundLevel, BULB_DS_6
	End If
End Sub

Sub T_OUTLANE_RIGHT_Hit
    If Tilted Then Exit Sub
    LastSwitchHit = "T_OUTLANE_RIGHT"
	AddScore(450)
	If L007.State = 2 Then
		AwardSpecial
		L007.State = 0
	Else
		'PlaySoundAtLevelStatic ("SFX_Outlane_1"), SFXSoundLevel, BULB_DS_6
	End If
End Sub

'/////////////////////////////  MYSTERY AWARD  ////////////////////////////

Sub T_MYSTERY_LEFT_Hit
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
    LastSwitchHit = "T_MYSTERY_LEFT"
	AddScore(225)
	If L019.State = 0 Then
		L019.State = 1
		If L019.State = 1 And L020.State = 1 Then
			L018.State = 2
			DMD "_", CL("MYSTERY IS LIT"), "", eNone, eBlink, eNone, 1500, True, ""
			PlaySoundAtLevelStatic ("SFX_Mystery_Lit_1"), SFXSoundLevel, BULB_DS_6
		Else
			'PlaySoundAtLevelStatic ("SFX_Mystery_1"), SFXSoundLevel, BULB_DS_6
		End If
	End If
End Sub

Sub T_MYSTERY_RIGHT_Hit
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
    LastSwitchHit = "T_MYSTERY_RIGHT"
	AddScore(225)
	If L020.State = 0 Then
		L020.State = 1
		If L019.State = 1 And L020.State = 1 Then
			L018.State = 2
			DMD "_", CL("MYSTERY IS LIT"), "", eNone, eBlink, eNone, 1500, True, ""
			'PlaySoundAtLevelStatic ("SFX_Mystery_Lit_1"), SFXSoundLevel, BULB_DS_6
		Else
			'PlaySoundAtLevelStatic ("SFX_Mystery_1"), SFXSoundLevel, BULB_DS_6
		End If
	End If
End Sub

Sub T_MYSTERY_Hit
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
    LastSwitchHit = "T_MYSTERY"
	AddScore(450)
	If L018.State = 2 Then
		L019.State = 0
		L020.State = 0
		L018.State = 0
		MYSTERYAWARD
		FLASHEFFECT(7)
		'PlaySoundAtLevelStatic ("SFX_Mystery_Hit_1"), SFXSoundLevel, BULB_DS_6
	End If
End Sub

Sub MYSTERYAWARD
    DMDFlush
    Dim TMP
    Select Case RndNbr(9)
        Case 1, 0 'Add 1 Doomslayer letter.
			DMD CL("MYSTERY AWARD"), CL("1 DOOMSLAYER LETTER"), "_", eNone, eBlink, eNone, 2500, True, ""
            AddOrbit(1)
        Case 2, 1 'Light special.
            If L006.State = 0 And L007.State = 0 Then
                DMD CL("MYSTERY AWARD"), CL("SPECIAL IS LIT"), "_", eNone, eBlink, eNone, 2500, True, ""
                L006.State = 2
				PlaySoundAtLevelStatic ("VO_Special_Lit"), VoiceOverSoundLevel, BULB_DS_6
            Else
                DMD CL("MYSTERY AWARD"), CL(FormatScore(125000)), "_", eNone, eBlink, eNone, 2000, True, ""
                AddScoreNoMultiplier 125000
            End If
        Case 3, 2 'Big points (10,000-200,000).
            TMP = 10000 * RndNbr(20)
            DMD CL("MYSTERY AWARD"), CL(FormatScore(TMP)), "_", eNone, eBlink, eNone, 2000, True, ""
            AddScoreNoMultiplier TMP
        Case 4, 3 'Small points (1,000-20,000).
            TMP = 1000 * RndNbr(20)
            DMD CL("MYSTERY AWARD"), CL(FormatScore(TMP)), "_", eNone, eBlink, eNone, 2000, True, ""
            AddScoreNoMultiplier TMP
        Case 5, 4 'Add bonus multiplier.
            AddBonusMultiplier 1
            DMD CL("MYSTERY AWARD"), "_", "_", eNone, eNone, eNone, 2000, True, ""
        Case 6, 5 'Boost orbit value.
			OrbitValue(CurrentPlayer) = OrbitValue(CurrentPlayer) + 3125
			DMD CL("ORBIT VALUE"), CL(FormatScore(OrbitValue(CurrentPlayer))), "_", eNone, eBlink, eNone, 2000, True, ""
        Case 7, 6 'Boost spinner value.
			SpinnerValue(CurrentPlayer) = SpinnerValue(CurrentPlayer) + 50
			DMD CL("SPINNER VALUE"), CL(FormatScore(SpinnerValue(CurrentPlayer))), "_", eNone, eBlink, eNone, 2000, True, ""
        Case 8, 7 'Boost bumper value.
			BumperValue(CurrentPlayer) = BumperValue(CurrentPlayer) + 625
			DMD CL("BUMPER VALUE"), CL(FormatScore(BumperValue(CurrentPlayer))), "_", eNone, eBlink, eNone, 2000, True, ""
        Case 9, 8 'Boost droptarget value.
			BumperValue(CurrentPlayer) = BumperValue(CurrentPlayer) + 935
			DMD CL("DROPTARGET VALUE"), CL(FormatScore(DropTargetValue(CurrentPlayer))), "_", eNone, eBlink, eNone, 2000, True, ""
        Case 10, 9 'Ball saver.
			If bBallSaverActive = False Then
				DMD CL("BALL SAVER"), CL("FOR " &BallSaverTime& " SECONDS"), "_", eNone, eNone, eNone, 2000, True, ""
				EnableBallSaver BallSaverTime
			Else
                DMD CL("MYSTERY AWARD"), CL(FormatScore(75000)), "_", eNone, eBlink, eNone, 2000, True, ""
                AddScoreNoMultiplier 75000
			End If
    End Select
End Sub

'/////////////////////////////  GUN TARGETS  ////////////////////////////

Sub T_GUN1_Hit
    If Tilted Then Exit Sub
	If L060.State = 0 Then RandomSoundTargets
	CHECK2BALLMULTIBALLHITS
    LastSwitchHit = "T_GUN1"
	AddScore(445)
	L060.State = 1
	L056.State = 1
	CHECKGUNTARGETS
End Sub

Sub T_GUN2_Hit
    If Tilted Then Exit Sub
	If L061.State = 0 Then RandomSoundTargets
	CHECK2BALLMULTIBALLHITS
    LastSwitchHit = "T_GUN2"
	AddScore(445)
	L061.State = 1
	L057.State = 1
	CHECKGUNTARGETS
End Sub

Sub T_GUN3_Hit
    If Tilted Then Exit Sub
	If L062.State = 0 Then RandomSoundTargets
	CHECK2BALLMULTIBALLHITS
    LastSwitchHit = "T_GUN3"
	AddScore(445)
	L062.State = 1
	L058.State = 1
	CHECKGUNTARGETS
End Sub

Sub T_GUN4_Hit
    If Tilted Then Exit Sub
	If L063.State = 0 Then RandomSoundTargets
	CHECK2BALLMULTIBALLHITS
    LastSwitchHit = "T_GUN3"
	AddScore(445)
	L063.State = 1
	L059.State = 1
	CHECKGUNTARGETS
End Sub

Sub CHECKGUNTARGETS
    If L056.State = 1 And L057.State = 1 And L058.State = 1 And L059.State = 1 Then
        L056.State = 0
        L057.State = 0
        L058.State = 0
        L059.State = 0
        L060.State = 0
        L061.State = 0
        L062.State = 0
        L063.State = 0
        FLASHEFFECT(3)
        AddScore 5500
		SpinnerValue(CurrentPlayer) = SpinnerValue(CurrentPlayer) + 50
		DMD CL("SPINNER VALUE"), CL(FormatScore(SpinnerValue(CurrentPlayer))), "d_border", eNone, eBlink, eNone, 2000, True, ""
		'PlaySoundAtLevelStatic ("SFX_Boost_1"), SFXSoundLevel, BULB_DS_6
    End If
End Sub

'/////////////////////////////  SUPER JACKPOT  ////////////////////////////

Sub T_SUPERJACKPOT_Hit
    If Tilted Then Exit Sub
	CHECK2BALLMULTIBALLHITS
    LastSwitchHit = "T_SUPERJACKPOT"
	If L064.State = 2 Then
		L064.State = 0
		AwardSuperJackpot
		'PlaySoundAtLevelStatic ("SFX_Super_Jackpot_1"), SFXSoundLevel, BULB_DS_6
	Else
		AddScore(550)
	End If
End Sub

'******************************************************
'******  END TARGETS & TRIGGERS HIT
'******************************************************

Sub Table1_Exit

    If B2SOn Then Controller.Stop

End Sub