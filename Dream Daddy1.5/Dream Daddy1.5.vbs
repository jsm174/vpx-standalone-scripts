
Option Explicit
Randomize

' *********************************************************************
' *********************************************************************
'
' LOAD CORE SCRIPTS
'
' *********************************************************************
' *********************************************************************

Const cGameName = "DreamDaddy"

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

' *********************************************************************
' *********************************************************************
'
' OPTIONS
'
' *********************************************************************
' *********************************************************************

Const MaxPlayers = 4    	' from 1 to 4											(4 Default)
Const BallSaverTime = 10	' in seconds											(10 Default)
Const MaxMultiplier = 6 	' limit to 6x in this game								(6 Default)
Const BallsPerGame = 3  	' 3 or 5												(3 Default)
Const MaxMultiballs = 6 	' max number of balls during multiballs					(6 Default)
Const FreePlay = 2      	' Free play								(1) ON (2) OFF	(2 Default)
Const turnoffrules = 1 		' Rules on backglass DURING game 		(1) ON (2) OFF	(1 Default)
Const AttractSong = 1		' Plays a song on at table start 		(1) ON (2) OFF	(1 Default)
Const LaneDifficulty = 1	' Lower lane completion difficulty 		(3) EASIER 3 for Pickup the Kid!, 10 for Extra ball - (2) HARDER 5  for Pickup the Kid!, 16 for Extra ball	(1 Default)
Const ExtraBallsPerBall = 2	' Limit the # of extra balls per ball	(1) Unlimited (2) 1 Per Ball


' Ball Volume
Const VolDiv = 8000      	' Lower number, louder ballrolling/collision sound
Const VolCol = 10        	' Ball collision divider ( voldiv/volcol )
Const Metalpitch = 50000	' Change to Increase/Decrease pitch of ball on metal rails

' The rest of the values are multipliers
'  .5 = lower volume
' 1.5 = higher volume

Const VolBump   = 1.5  	' Bumpers volume.
Const VolGates  = 1   	' Gates volume.
Const VolGuides = 1.5   ' Guides volume.
Const VolMetal  = 1   	' Metals volume.
Const VolRB     = 1   	' Rubber bands volume.
Const VolRH     = 2.5  	' Rubber hits volume.
Const VolPo     = 2.5  	' Rubber posts volume.
Const VolPi     = 2.5  	' Rubber pins volume.
Const VolTarg   = 1    	' Targets volume.
Const VolKick   = 1    	' Kicker volume.
Const VolSpin   = 2.5  	' Spinners volume.
Const VolFlip   = 1   	' Flipper volume.
Const VolSens   = 1   	' Sensor volume.
Const VolSling  = 1    	' Slingshot volume.
Const VolClock  = 1.5   	' Clock volume.

Const typefont = "BebasNeue-Regular"



' *********************************************************************
' *********************************************************************
'
' SETUP & INITIALIZE PUP DMD
'
' *********************************************************************

''''''''''''Table is setup for 3 screen with full dmd by default. See option for full dmd on backglass below. MUST SWITCH BOTH!
' *********************************************************************
'   PinUp Player Config

Const HasPuP = True

Dim PuPlayer

Const pBackglass = 2		'(2)Default)    optional Fulldmd on BG- set to (0)  -!!!!must also set pFullDMD to 2 if using this setting!!!! MUST SWITCH BOTH!
Const pDMD = 1
Const pFullDMD = 5   		'(5)Default)    optional Fulldmd on BG- set to (2)  -!!!!must also set pBackglass to 0 if using this setting!!!! MUST SWITCH BOTH!
Const pPlayfield = 3
Const pMusic = 4
Const pAudio=7
Const pCallouts=8

Sub InitPuP()
if HasPuP Then
    Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")

    'This Initiates PinUp Player for each PuP screen.

    PuPlayer.Init pFullDMD, "DreamDaddy"
	PuPlayer.Init pBackglass,"DreamDaddy"
	
    'Init TextOverlay on PUP Screen

    PuPlayer.LabelInit pFullDMD

    'Set PuPlayer 'Screens TO Always ON

	'<screen number> , xpos, ypos, width, height, POPUP
    PuPlayer.SetScreenex pFullDMD, 0, 0, 0, 0, 0
	PuPlayer.SetScreenex pBackglass, 0, 0, 0, 0, 0

    'Add Media folders

    PuPlayer.playlistadd pFullDMD, "PupOverlays", 0, 0
    PuPlayer.playlistadd pFullDMD, "Backglass", 1, 0
    PuPlayer.playlistadd pFullDMD, "Attract", 1, 0
    PuPlayer.playlistadd pFullDMD, "Jokes", 1, 90
    PuPlayer.playlistadd pFullDMD, "Drains", 0, 5
    PuPlayer.playlistadd pFullDMD, "Random", 0, 180
    PuPlayer.playlistadd pFullDMD, "Multiball", 0, 60
    PuPlayer.playlistadd pFullDMD, "Victim", 1, 5
    PuPlayer.playlistadd pFullDMD, "Weapons", 1, 5
    PuPlayer.playlistadd pFullDMD, "Kill", 1, 5
    PuPlayer.playlistadd pFullDMD, "Pop", 0, 5
    PuPlayer.playlistadd pFullDMD, "Key", 1, 10
	PuPlayer.playlistadd pBackglass, "BG", 1, 0
	PuPlayer.playlistadd pBackglass, "Search", 0, 5
End if
End Sub

' *********************************************************************
' *********************************************************************
'
' Load Table INitialization & MAIN DIMS
'
' *********************************************************************
' *********************************************************************

' Define any Constants
Const TableName = "DreamDaddy"
Const myVersion = "2.0.0"

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusHeldPoints(4)
Dim BonusMultiplier(4)
Dim bBonusHeld
Dim TotalBonus
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim ExtraBallIsLit(4)
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
Dim SkillshotValue(4)
Dim bAutoPlunger
Dim bInstantInfo
Dim bromconfig
Dim bAttractMode



' Define Game Control Variables
Dim LastSwitchHit
Dim BallsOnPlayfield
Dim BallsInLock(4)

' there are three holes in this table
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
Dim bExtraBallWonThisBall 'limit to one extra ball per ball
Dim bJustStarted

Dim plungerIM             'used mostly as an autofire plunger
Dim x                     'used as a counter in timers

Sub Table1_Init()
    LoadEM
    InitPuP
    SetupPuP
    Dim i
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

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    'load saved values, highscore, names, jackpot
    Loadhs

    ' Remove the cabinet rails if in FS mode
    If Table1.ShowDT = False then
        lrail.Visible = False
		rrail.Visible = False
   End If

    ' freeplay or coins
	If FreePlay = 1 then
		bFreePlay = True 'we dont want coins
	Else
		bFreePlay = False 'we want coins
	End If


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
    For i = 0 to 4
        BallsInLock(i) = 0
    Next
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bBonusHeld = False
    bJustStarted = True
    bInstantInfo = False
    bromconfig = False
    ' set any lights for the attract mode
    GiOff
    StartAttractMode
    If AttractSong = 1 Then PlaySong "Sh-Boom" End If
	
	
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
    controller.stop
End Sub


' *********************************************************************
' *********************************************************************

' Keys

' *********************************************************************
' *********************************************************************
Sub Table1_KeyDown(ByVal Keycode)

	If keycode = LeftFlipperKey Then
		FlipperActivate LeftFlipper, LFPress
		SolLFlipper True						'This would be called by the solenoid callbacks if using a ROM	
	End If

	If keycode = RightFlipperKey Then
		FlipperActivate RightFlipper, RFPress
		SolRFlipper True						'This would be called by the solenoid callbacks if using a ROM
	End If

    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        DOF 114, DOFOn
        If(Tilted = False)Then
            pMsg "CREDITS " &credits, 1, "", 0
            PlaySoundAtVol "fx_coin", Drain, 1
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Table specific actions

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 6:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 6:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 7:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1:InstantInfoTimer.Enabled = True
        If keycode = RightFlipperKey Then SolRFlipper 1:InstantInfoTimer.Enabled = True

        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    pMsg PlayersPlayingGame & " DADDY'S", 1, "", 0
                    PlaySound "fx_fanfare1"
					pUpdateScores
                Else
                    If(Credits> 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        pMsg PlayersPlayingGame & " DADDY'S", 1, "", 0
                        PlaySound "fx_fanfare1"
					pUpdateScores
                    Else
                        ' Not Enough Credits to start a game.
                        DOF 119, DOFOff
                        pMsg2Lines "NO CREDITS", "INSERT COIN", 1.5, "", 1
                        'PlaySound "so_nocredits"
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True)Then
                    If(BallsOnPlayfield = 0)Then
                        ResetForNewGame()
                    End If
                Else
                    If(Credits> 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            ResetForNewGame()
                        End If
                    Else
                        ' Not enough Credits to start a game.
                        DOF 119, DOFOff
                        pMsg2Lines "NO CREDITS", "INSERT COIN", 1.5, "", 1
                    End If
                End If
            End If
    End If ' If (GameInPlay)

End Sub


Sub Table1_KeyUp(ByVal keycode)

If keycode = LeftFlipperKey Then
		FlipperDeActivate LeftFlipper, LFPress
		SolLFlipper False						'This would be called by the solenoid callbacks if using a ROM
	End If

	If keycode = RightFlipperKey Then
		FlipperDeActivate RightFlipper, RFPress
		SolRFlipper False						'This would be called by the solenoid callbacks if using a ROM
	End If

    If keycode = PlungerKey Then
        Plunger.Fire
    End If

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific
    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper 0
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                bInstantInfo = False
                InstantInfoTimer2.Enabled = False
            End If
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
            InstantInfoTimer.Enabled = False
            If bInstantInfo Then
                bInstantInfo = False
                InstantInfoTimer2.Enabled = False
            End If
        End If
    End If
End Sub

Dim InfoStep
Sub InstantInfoTimer_Timer
    InstantInfoTimer.Enabled = 0
    If NOT hsbModeActive Then
        bInstantInfo = True
        InfoStep = 0
        InstantInfoTimer2.Enabled = 1
    End If
End Sub

Sub InstantInfoTimer2_Timer
    Select Case InfoStep
        Case 0:pMsg2Lines "Movie Quotes", "" & GSecretsCount(CurrentPlayer), 2, "", 0
        Case 1:pMsg2Lines "Tools", "" & BribesCount(CurrentPlayer), 2, "", 0
        Case 2:pMsg2Lines "Dad Naps", "" & PhotoCount(CurrentPlayer), 2, "", 0
        Case 3:pMsg2Lines "Dad Jokes", "" & CluesCount(CurrentPlayer), 2, "", 0
        Case 4:pMsg2Lines "Bonus Multiplier", "" & BonusMultiplier(CurrentPlayer), 2, "", 0
        Case 5:pMsg2Lines "Total Bonus", "" & FormatNumber(BonusMultiplier(CurrentPlayer) * (GSecretsCount(CurrentPlayer) * 500 + BribesCount(CurrentPlayer) * 500 + PhotoCount(CurrentPlayer) * 500 + CluesCount(CurrentPlayer) * 50), 0), 2, "", 0
        Case 6:pMsg2Lines "Balls in Lock", "" & BallsInLock(CurrentPlayer), 2, "", 0
        Case 7:pMsg2Lines "Dates finished", ModesFinished(CurrentPlayer) & "/7", 2, "", 0
        Case 8:pMsg2Lines "Jackpot base value", "" & Jackpot(CurrentPlayer), 2, "", 0
        Case 9:pMsg2Lines "Bumper value", "" & BumperValue(CurrentPlayer), 2, "", 0
    End Select
    InfoStep = InfoStep + 1
    If InfoStep = 10 Then InfoStep = 0
End Sub


'******************************************************
' Flippers Polarity (dec 1992) 
'******************************************************

dim LF : Set LF = New FlipperPolarity
dim RF : Set RF = New FlipperPolarity

InitPolarity

Sub InitPolarity()
        dim x, a : a = Array(LF, RF)
        for each x in a
                x.AddPoint "Ycoef", 0, RightFlipper.Y-65, 1        'disabled
                x.AddPoint "Ycoef", 1, RightFlipper.Y-11, 1
                x.enabled = True
                x.TimeDelay = 60
        Next

        AddPt "Polarity", 0, 0, 0
        AddPt "Polarity", 1, 0.05, -5.5
        AddPt "Polarity", 2, 0.4, -5.5
        AddPt "Polarity", 3, 0.6, -5.0
        AddPt "Polarity", 4, 0.65, -4.5
        AddPt "Polarity", 5, 0.7, -4.0
        AddPt "Polarity", 6, 0.75, -3.5
        AddPt "Polarity", 7, 0.8, -3.0
        AddPt "Polarity", 8, 0.85, -2.5
        AddPt "Polarity", 9, 0.9,-2.0
        AddPt "Polarity", 10, 0.95, -1.5
        AddPt "Polarity", 11, 1, -1.0
        AddPt "Polarity", 12, 1.05, -0.5
        AddPt "Polarity", 13, 1.1, 0
        AddPt "Polarity", 14, 1.3, 0

        addpt "Velocity", 0, 0,         1
        addpt "Velocity", 1, 0.16, 1.06
        addpt "Velocity", 2, 0.41,         1.05
        addpt "Velocity", 3, 0.53,         1'0.982
        addpt "Velocity", 4, 0.702, 0.968
        addpt "Velocity", 5, 0.95,  0.968
        addpt "Velocity", 6, 1.03,         0.945

        LF.Object = LeftFlipper        
        LF.EndPoint = EndPointLp
        RF.Object = RightFlipper
        RF.EndPoint = EndPointRp
End Sub


' Flipper trigger hit subs
Sub TriggerLF_Hit() : LF.Addball activeball : End Sub
Sub TriggerLF_UnHit() : LF.PolarityCorrect activeball : End Sub
Sub TriggerRF_Hit() : RF.Addball activeball : End Sub
Sub TriggerRF_UnHit() : RF.PolarityCorrect activeball : End Sub


'
' *********************************************************************
' *********************************************************************
'
' Scoring and High Scores
'
' *********************************************************************
' *********************************************************************

' Add points to the score AND update the score board

Sub AddScore(points)
    If(Tilted = False)Then
        ' add the points to the current players score variable
        ' the BumperFrenzyMultiplier can be 2 when each time the Dad Jokes count reaches 100
        Score(CurrentPlayer) = Score(CurrentPlayer) + points * BumperFrenzyMultiplier
        ' update the score displays
        pUpdateScores
    End if

End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If(Tilted = False)Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    ' update the score displays if the bonus is displayed there
    ' pUpdateScores
    End if

' you may wish to check to see if the player has gotten a replay
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If(Tilted = False)Then

        ' If(bMultiBallMode = True) Then
        Jackpot(CurrentPlayer) = Jackpot(CurrentPlayer) + points
    ' you may wish to limit the jackpot to a upper limit, ie..
    '	If (Jackpot >= 6000) Then
    '		Jackpot = 6000
    ' 	End if
    'End if
    End if
End Sub

Sub IncreaseBonusMultiplier 'special for this table
    LightEffect 2
    'DOF Strobe
    Select Case BonusMultiplier(CurrentPlayer)
        Case 1:BonusMultiplier(CurrentPlayer) = 2:pMsg "Bonus Multiplier 2x", 2, "", 1
        Case 2:BonusMultiplier(CurrentPlayer) = 4:pMsg "Bonus Multiplier 4x", 2, "", 1
        Case 4:BonusMultiplier(CurrentPlayer) = 6:pMsg "Bonus Multiplier 6x", 2, "", 1
        Case 6:l15.State = 2
            pMsg "Extra ball is lit", 1, "", 1 'lit the extra ball
            PlaySound "vo_extraballislit"
            ExtraBallIsLit(CurrentPlayer) = 1
    End Select
    UpdateBonusXLights
End Sub

Sub UpdateBonusXLights 'special for this table
    Select Case BonusMultiplier(CurrentPlayer)
        Case 1:l6.State = 0:l5.State = 0:l7.State = 0
        Case 2:l6.State = 1:l5.State = 0:l7.State = 0
        Case 4:l6.State = 0:l5.State = 1:l7.State = 0
        Case 6:l6.State = 0:l5.State = 0:l7.State = 1
    End Select
End Sub

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
    PlaySound "vo_extraball"
    pMsg "DADDY'S GOT AN EXTRA BALL", 2, "", 1
    l15.State = 0
    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
    If bExtraBallWonThisBall = 2 Then
	bExtraBallWonThisBall = True
	End If
    GiEffect 1
    LightEffect 2
Else
	AddScore 100000
	pMsg "Points instead... 100,000", 3, "", 0
    l15.State = 0
	End If
End Sub

Sub AwardSpecial()
    PlaySound SoundFXDOF("fx_knocker",113,DOFPulse,DOFKnocker)
	DOF 114, DOFPulse
    pMsg "EXTRA GAME WON", 1, "", 1
    Credits = Credits + 1
    DOF 119, DOFOn
    GiEffect 1
    LightEffect 1
End Sub

'in this table the jackpot starts at 50000 points

Sub AwardJackpot() 'award a normal jackpot
    Dim tmp
    tmp = Jackpot(CurrentPlayer) + (GSecretsCount(CurrentPlayer) + BribesCount(CurrentPlayer) + PhotoCount(CurrentPlayer)) * 2500
    pMsg2Lines "JACKPOT", tmp, 1.5, "", 1
    PlayJackpot
    AddScore tmp
    GiEffect 1
    DOF 135, DOFPulse
    LightEffect 2
    FlashEffect 2
'DOF 154, DOFPulse
End Sub

Sub AwardSuperJackpot() 'the superjackpot counts a 5 jackpots!
    Dim tmp
    tmp = 5 * (Jackpot(CurrentPlayer) + (GSecretsCount(CurrentPlayer) + BribesCount(CurrentPlayer) + PhotoCount(CurrentPlayer)) * 2500)
    pMsg2Lines "SUPER JACKPOT", tmp, 1.5, "", 1
    PlaySound "vo_superjackpot"
    AddScore tmp
    AddJackpot Jackpot(CurrentPlayer) / 2 'and increases the jackpot base value by 50%
    GiEffect 1
    DOF 137, DOFPulse
    LightEffect 2
    FlashEffect 2
'DOF 154, DOFPulse
End Sub

Sub AwardSkillshot()
    Dim i
    ResetSkillShotTimer_Timer
    'show dmd animation
    pMsg "Skillshot", 2, "", 1
	PlaySound "vo_skillshot"
    Addscore SkillShotValue(CurrentPLayer)
    ' increment the skillshot value with 5000
    SkillShotValue(CurrentPLayer) = SkillShotValue(CurrentPLayer) + 50000
    'do some light show
    GiEffect 1
    DOF 137, DOFPulse
    LightEffect 2
End Sub

Sub Congratulation()
    Dim tmp
    tmp = "vo_congrat" & INT(RND * 21 + 1)
    PlaySound tmp
End Sub

' *********************************************************************
'    Load / Save / Highscore
' *********************************************************************

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 5000000 End If
    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "DAD" End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 4000000 End If
    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "POP" End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 3000000 End If
    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "DDY" End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 2000000 End If
    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "DRD" End If
    x = LoadValue(TableName, "Credits")
    If(x <> "")then Credits = CInt(x):DOF 119, DOFOn:Else Credits = 0 End If
    x = LoadValue(TableName, "TotalGamesPlayed")
    If(x <> "")then TotalGamesPlayed = CInt(x)Else TotalGamesPlayed = 0 End If
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
    Dim tmp
    tmp = Score(1)

    If Score(2)> tmp Then tmp = Score(2)
    If Score(3)> tmp Then tmp = Score(3)
    If Score(4)> tmp Then tmp = Score(4)

    If tmp> HighScore(1)Then 'add 1 credit for beating the highscore
        If NOT bFreePlay Then
            AwardSpecial
        End If
    End If

    If tmp> HighScore(3)Then
        vpmtimer.addtimer 2000, "PlaySound ""fx_clapping"" '"
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    PlaySound "vo_amazing"
    hsLetterFlash = 0

    hsEnteredDigits(0) = "A"
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ<" ' < is used to delete the last letter
    hsCurrentLetter = 1
    pMsg2Lines "YOUR NAME:", " <A  > ", 9999, "", 0
    HighScoreDisplayName()
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        Playsound "fx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayName()
    End If

    If keycode = RightFlipperKey Then
        Playsound "fx_Next2"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayName()
    End If

    If keycode = StartGameKey OR keycode = PlungerKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3)then
                HighScoreCommitName()
            else
                HighScoreDisplayName()
            end if
        else
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = "A"
            if(hsCurrentDigit> 0)then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayName()
        end if
    end if
End Sub

Sub HighScoreDisplayName()
    Dim i, TempStr

    TempStr = " >"
    if(hsCurrentDigit> 0)then TempStr = TempStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1)then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2)then TempStr = TempStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3)then
        if(hsLetterFlash <> 0)then
            TempStr = TempStr & "_"
        else
            TempStr = TempStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1)then TempStr = TempStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2)then TempStr = TempStr & hsEnteredDigits(2)

    TempStr = TempStr & "< "
    pMsg2Lines "YOUR NAME:", Mid(TempStr, 2, 5), 9999, "", 0
End Sub

Sub HighScoreCommitName()
    hsbModeActive = False
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ")then
        hsEnteredName = "YOU"
    end if

    HighScoreName(3) = hsEnteredName
    SortHighscore
    EndOfBallComplete()
End Sub

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 0 to 3
        For j = 0 to 2
            If HighScore(j) <HighScore(j + 1)Then
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

' *********************************************************************
' *********************************************************************
'
' ATTRACT MODE
'
' *********************************************************************
' *********************************************************************
Dim introposition
introposition = 0

Sub dmdattract_timer
    introposition = introposition + 1
    Select Case introposition	
		Case 2
            'coins or freeplay
            If bFreePlay Then
                pMsgA "Free Play", 8, "", 1
		PuPlayer.LabelSet pFullDMD,"RulesTitleA","Skillshot",1,""
		PuPlayer.LabelSet pFullDMD,"RulesTextA","Hit flashing P-O-P target.",1,""
		PuPlayer.playlistplayex pFullDMD, "attract", "",0,1
		PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1
            Else
                If Credits> 0 Then
                    pMsg2LinesA "Credits " &credits, "Press Start", 8, "", 1
		PuPlayer.playlistplayex pFullDMD, "attract", "",0,1
		PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1

                Else
                    pMsg2LinesA "Credits " &credits, "Insert Coins", 8, "", 1
		PuPlayer.playlistplayex pFullDMD, "attract", "",0,1
		PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1
                End If
            End If
        Case 8
            'Highscores
            pMsg3Lines "Worlds Greatest Dad", "" & HighScoreName(0), "" & FormatNumber(HighScore(0), 0), 8, "", 0
		PuPlayer.LabelSet pFullDMD,"RulesTitleA","JOKE Targets",1,""
		PuPlayer.LabelSet pFullDMD,"RulesTextA","Complete all 4 standup targets to light MYSTERY",1,""
		PuPlayer.playlistplayex pFullDMD, "attract", "",0,1
		PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1
        Case 14
            pMsg3Lines "Stepdad - 2nd", "" & HighScoreName(1), "" & FormatNumber(HighScore(1), 0), 8, "", 0
		PuPlayer.LabelSet pFullDMD,"RulesTitleA","Bonus Multiplier",1,""
		PuPlayer.LabelSet pFullDMD,"RulesTextA","P-O-P lanes increases BONUS X (2X, 4X, 6X, Light EXTRA BALL)",1,""
		PuPlayer.playlistplayex pFullDMD, "attract", "",0,1
        PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1
		Case 20
            pMsg3Lines "Father - 3rd", "" & HighScoreName(2), "" & FormatNumber(HighScore(2), 0), 8, "", 0
		PuPlayer.LabelSet pFullDMD,"RulesTitleA","Date Lanes",1,""
		PuPlayer.LabelSet pFullDMD,"RulesTextA","Complete 3x to light PICK UP KID / 10x to light EXTRA BALL.",1,""
		PuPlayer.playlistplayex pFullDMD, "attract", "",0,1
		PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1
        Case 25
            pMsg3Lines "Youre not my real Dad!", "" & HighScoreName(3), "" & FormatNumber(HighScore(3), 0), 8, "", 0
		PuPlayer.LabelSet pFullDMD,"RulesTitleA","Dad Joke Frenzy",1,""
		PuPlayer.LabelSet pFullDMD,"RulesTextA","Double scoring after 100 Dad Jokes hits.",1,""
		PuPlayer.playlistplayex pFullDMD, "attract", "",0,1
		PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1
        Case 30
            pMsgA "Game Over", 8, "", 1
		PuPlayer.LabelSet pFullDMD,"RulesTitleA","Fun Fact",1,""
		PuPlayer.LabelSet pFullDMD,"RulesTextA","Real Dads love Pinball!",1,""
		PuPlayer.playlistplayex pFullDMD, "attract", "",0,1
		PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1
		Case 35
            introposition = 0
    End Select
End Sub

Sub ShowScores
			If PlayersPlayingGame = 1 Then
			PuPlayer.LabelSet pFullDMD,"Play1scoreA","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play1A","Daddy 1",1,"{'mt':2,'color':1604786}"
		PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1
		End If
			If PlayersPlayingGame = 2 Then
			PuPlayer.LabelSet pFullDMD,"Play1scoreA","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play1A","Daddy 1",1,"{'mt':2,'color':1604786}"
			PuPlayer.LabelSet pFullDMD,"Play2scoreA","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play2A","Daddy 2",1,"{'mt':2,'color':1604786}"
		PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1
		End If
		If PlayersPlayingGame = 3 Then
			PuPlayer.LabelSet pFullDMD,"Play1scoreA","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play1A","Daddy 1",1,"{'mt':2,'color':1604786}"
			PuPlayer.LabelSet pFullDMD,"Play2scoreA","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play2A","Player 2",1,"{'mt':2,'color':1604786}"
			PuPlayer.LabelSet pFullDMD,"Play3scoreA","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play3A","Daddy 3",1,"{'mt':2,'color':1604786}"
		PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1
		End If
		If PlayersPlayingGame = 4 Then
			PuPlayer.LabelSet pFullDMD,"Play1scoreA","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play1A","Daddy 1",1,"{'mt':2,'color':1604786}"
			PuPlayer.LabelSet pFullDMD,"Play2scoreA","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play2A","Player 2",1,"{'mt':2,'color':1604786}"
			PuPlayer.LabelSet pFullDMD,"Play3scoreA","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play3A","Daddy 3",1,"{'mt':2,'color':1604786}"
			PuPlayer.LabelSet pFullDMD,"Play4scoreA","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play4A","Daddy 4",1,"{'mt':2,'color':1604786}"
		PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1
		End If
End Sub

Sub StartAttractMode()
    'DOF - Attract Mode ON
    bAttractMode = True
    StartLightSeq
    dmdattract.Enabled = 1
	PuPlayer.LabelShowPage pFullDMD,2,0,""
	Showscores

End Sub

Sub StopAttractMode()
    'DOF - Attract Mode Off
    bAttractMode = False
    'pUpdateScores
    LightSeqAttract.StopPlay
    LightSeqFlasher.StopPlay
    dmdattract.Enabled = 0
    dmdattract.Interval = 1000
End Sub

Sub nextpage
	introposition = introposition + 1
End Sub

Sub prevpage
	introposition = introposition - 1
End Sub

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
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

' *********************************************************************
' *********************************************************************
'
' TILT
'
' *********************************************************************
' *********************************************************************

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                  'Called when table is nudged
    Tilt = Tilt + TiltSensitivity              'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity)AND(Tilt <15)Then 'show a warning
        pMsg "CAREFUL DAD!", 1, "", 1
    'DOF 131, DOFPulse
    'DOF 311, DOFPulse 'DOF - Tilt Warning
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        pMsg "TILT", 5, "", 1
        StopSong
        PlaySound "Theme_Tilt"
        'DOF 310, DOFPulse                'DOF  - TILT
        'DOF 127, DOFOff                  'DOF - Beacon - OFF
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
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LF.fire 'LeftFlipper.RotateToStart
        RF.fire'RightFlipper.RotateToStart
        Bumper1.Force = 0
        Bumper2.Force = 0
        Bumper3.Force = 0

        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        Bumper1.Force = 8
        Bumper2.Force = 8
        Bumper3.Force = 8
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
    'clean up the buffer display
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

' *********************************************************************
' *********************************************************************
'
' MUSIC - Using Sound WAV
'
' *********************************************************************
' *********************************************************************

' Use just the command PlaySong when you want to change the song
' all the modes should be written here with their corresponding songs

Dim Song
Song = ""

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            PlaySound Song, -1, 1 '-1 means repeat the song when it ends
        End If
    End If
End Sub

Sub StopSong
    StopSound Song
    Song = ""
End Sub

Sub PauseSong
    PlaySound Song, -1, 0, 0, 0, 0, 1, 0, 0
End Sub

Sub ResumeSong
    PlaySound Song, -1, 1, 0, 0, 0, 1, 0, 0
End Sub

' *********************************************************************
' *********************************************************************
'
' General Illumination Effects
'
' *********************************************************************
' *********************************************************************

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color and intensity
    Dim bulb
	Dim i
	for i = 1 to 11
		if i = col Then
			DOF 150 + i, DOFOn
		Else
			DOF 150 + i, DOFOff
		end If
	Next
    For each bulb in ColorChangeLights
        SetLightColor bulb, col, -1
    Next
    For each bulb in ColorChangeLights
        bulb.falloffpower = 2
    Next
End Sub

Sub ResetGi(col) 'resets the gi color and intensity
    Dim bulb
	Dim i
	for i = 1 to 11
		if i = col Then
			DOF 150 + i, DOFOn
		Else
			DOF 150 + i, DOFOff
		end If
	Next
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
    For each bulb in ColorChangeLights
        SetLightColor bulb, col, -1
    Next
    For each bulb in aGiLights
        bulb.falloffpower = 5
    Next
    For each bulb in ColorChangeLights
        bulb.falloffpower = 5
    Next
End Sub

Sub GiOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in ColorChangeLights
        bulb.State = 1
    Next
End Sub

Sub GiOff
    Dim bulb	
	Dim i
	for i = 1 to 11
		DOF 150 + i, DOFOff
	Next
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in ColorChangeLights
        bulb.State = 0
    Next
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
        Case 10 'all blink
            LightSeqGi.UpdateInterval = 4
            LightSeqGi.Play SeqBlinking, , 1, 1
    End Select
End Sub

Sub GiEffect2(n)
    Select Case n
        Case 0 'all off
            lightseqPlastics.Play SeqAlloff
        Case 1 'all blink
            lightseqPlastics.UpdateInterval = 4
            lightseqPlastics.Play SeqBlinking, , 8, 40
        Case 2 'random
            lightseqPlastics.UpdateInterval = 10
            lightseqPlastics.Play SeqRandom, 5, , 1000
        Case 3 'upon
            lightseqPlastics.UpdateInterval = 4
            lightseqPlastics.Play SeqUpOn, 5, 1
        Case 4 ' left-right-left
            lightseqPlastics.UpdateInterval = 5
            lightseqPlastics.Play SeqLeftOn, 10, 1
            lightseqPlastics.UpdateInterval = 5
            lightseqPlastics.Play SeqRightOn, 10, 1
        Case 5 'all blink
            lightseqPlastics.UpdateInterval = 4
            lightseqPlastics.Play SeqBlinking, , 1, 1
		Case 6
			lightseqPlastics.UpdateInterval = 4
			lightseqPlastics.Play SeqRightOn, 3, 75 
		Case 7
			lightseqPlastics.UpdateInterval = 4
			lightseqPlastics.Play SeqLeftOn, 3, 75
		Case 8
			 lightseqPlastics.UpdateInterval = 4
			 lightseqPlastics.Play SeqCircleOutOn, 5, 10
    End Select
End Sub

'first number is amount of times, second number is speed

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqBlinking, , 5, 100
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 5, , 1000
        Case 3 'upon
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 10, 1
        Case 4 ' left-right-left
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqLeftOn, 10, 1
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqRightOn, 10, 1
        Case 5 'random
            LightSeqbumper.UpdateInterval = 4
            LightSeqbumper.Play SeqBlinking, , 5, 10
        Case 6 'random
            LightSeqRSling.UpdateInterval = 4
            LightSeqRSling.Play SeqBlinking, , 5, 6
        Case 7 'random
            LightSeqLSling.UpdateInterval = 4
            LightSeqLSling.Play SeqBlinking, , 5, 6
        Case 8 'random
            LightSeqlr.UpdateInterval = 4
            LightSeqlr.Play SeqBlinking, , 5, 10
        Case 9 'random
            LightSeqrr.UpdateInterval = 4
            LightSeqrr.Play SeqBlinking, , 5, 10
        Case 10 'all blink
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqBlinking, , 1, 1
    End Select
End Sub

' Flasher Effects using lights

Dim FEStep, FEffect
FEStep = 0
FEffect = 0

Sub FlashEffect(n)
    Select case n
        Case 0 ' all off
            LightSeqFlasher.Play SeqAlloff
        Case 1 'all blink
            LightSeqFlasher.UpdateInterval = 4
            LightSeqFlasher.Play SeqBlinking, , 5, 100
        Case 2 'random
            LightSeqFlasher.UpdateInterval = 10
            LightSeqFlasher.Play SeqRandom, 5, , 1000
        Case 3 'upon
            LightSeqFlasher.UpdateInterval = 4
            LightSeqFlasher.Play SeqUpOn, 10, 1
        Case 4 ' left-right-left
            LightSeqFlasher.UpdateInterval = 5
            LightSeqFlasher.Play SeqLeftOn, 10, 1
            LightSeqFlasher.UpdateInterval = 5
            LightSeqFlasher.Play SeqRightOn, 10, 1
        Case 5 ' top flashers blink fast
            FlashForms f5, 1000, 50, 0
            FlashForms f6, 1000, 50, 0
            FlashForms f7, 1000, 50, 0
            FlashForms f8, 1000, 50, 0
            FlashForms f9, 1000, 50, 0
    End Select
End Sub

Sub PowerOff
    GiEffect 0
    LightEffect 0
End Sub

Sub PowerOn
    GiEffect 10
    LightEffect 10
End Sub

Sub JackpotFlash
		DOF 130, DOFPulse	'RR
		DOF 133, DOFPulse	'LR	
        FlashForms FlasherRRGreen, 600, 50, 0
        FlashForms RRGreen, 600, 50, 0
        FlashForms RRGreenUp, 600, 50, 0
        FlashForms FlasherLRGreen, 600, 50, 0
        FlashForms LRGreen, 600, 50, 0
        FlashForms LRGreenUp, 600, 50, 0
		FlashForms FlasherVUKGreen, 600, 50, 0
		FlashForms VUKGreen, 600, 600, 0
        FlashForms FlasherModeHoleGreen, 600, 50, 0
        FlashForms ModeGreen, 600, 50, 0
End Sub


'****************************************
' Real Time updates using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
    'GIUpdate
    RollingUpdate
    BallShadowUpdate
    ' add any other real time update subs, like gates or diverters
    VerticalDoor.RotZ = Spinner1.Currentangle
End Sub

'********************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

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

' *********************************************************************
' *********************************************************************
'
' STARTING AND ENDING GAMES & DRAINS ETC
'
' *********************************************************************

' Initialise the Table for a new Game

Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

    'resets the score display, and turn off attract mode
    StopAttractMode
    GiOn
	resetbackglass
	ruleshelperon
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
    Tilt = 0
    Game_Init()
    vpmtimer.addtimer 1500, "FirstBall'"

End Sub

Sub FirstBall
    ResetForNewPlayerBall()
    CreateNewBall()
End Sub

Sub ResetForNewPlayerBall()
    AddScore 0
    BonusMultiplier(CurrentPlayer) = 1:UpdateBonusXLights
    BonusPoints(CurrentPlayer) = 0
    bBonusHeld = False
    bExtraBallWonThisBall = False

    'Reset any table specific
    ResetNewBallLights()
    ResetNewBallVariables
	'currentplayerbackglass

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
    bSkillShotReady = True

	'Change the music ?
End Sub

Sub CreateNewBall()
    'debug.print "ball created"
    BallRelease.CreateBall.image = MyBallImage
    BallsOnPlayfield = BallsOnPlayfield + 1
    GiOn
    ResetGi white
    PlaySoundAtVol SoundFXDOF("fx_Ballrel", 116, DOFPulse, DOFContactors), BallRelease, VolKick
    BallRelease.Kick 90, 4
    If BallsOnPlayfield> 1 Then
        bMultiBallMode = True
        DOF 127, DOFOn 'Beacon ON
        bAutoPlunger = True
    End If
End Sub

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
        If BallsOnPlayfield <MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
                Me.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            Me.Enabled = False
        End If
    End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()

    TotalBonus = 0
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

    If NOT Tilted Then

        'Count the bonus. This table uses several bonus
        x = 0 'use a simple counter variable
        CountBonus.Enabled = 1

        ' calculate the totalbonus
        TotalBonus = formatnumber(GSecretsCount(CurrentPlayer) * 5000 + BribesCount(CurrentPlayer) * 5000 + PhotoCount(CurrentPlayer) * 5000 + CluesCount(CurrentPlayer) * 500, 0)
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)

        ' handle the bonus held
        BonusHeldPoints(CurrentPlayer) = 0

        ' the player has won the bonus held award so do something with it 
        If bBonusHeld Then
            If Balls = BallsPerGame Then ' this is the last ball, so if bonus held has been awarded then double the bonus
                TotalBonus = TotalBonus * 2
            End If
        Else ' this is not the last ball so save the bonus for the next ball
            BonusHeldPoints(CurrentPlayer) = TotalBonus
        End If
        ' reset the bonus held value since it has been already added to the bonus
        bBonusHeld = False

        ' the bonus will be added to the score in the bonus count timer.

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 7000, "EndOfBall2 '"
    Else 'if tilted then only add a short delay
        vpmtimer.addtimer 1000, "EndOfBall2 '"
    End If
End Sub

Sub CountBonus_Timer
Dim tmp
   Select Case x
		Case 0:tmp= GSecretsCount(CurrentPlayer) * 5000: pMsg2Lines GSecretsCount(CurrentPlayer) & " Movies Quoted ", FormatNumber(tmp,0), 2, "", 0:Playsound "fx_Next"
		Case 1:tmp= BribesCount(CurrentPlayer) * 5000:pMsg2Lines BribesCount(CurrentPlayer) & " Tools Returned ", FormatNumber(tmp,0), 2, "", 0:Playsound "fx_Next"
		Case 2:tmp= PhotoCount(CurrentPlayer) * 5000: pMsg2Lines PhotoCount(CurrentPlayer) & " Dad Naps Taken ", FormatNumber(tmp,0), 2, "", 0:Playsound "fx_Next"
		Case 3:tmp= CluesCount(CurrentPlayer) * 500: pMsg2Lines CluesCount(CurrentPlayer) & " Dad Jokes Collected ", FormatNumber(tmp,0), 2, "", 0:Playsound "fx_Next"
        Case 4:pMsg2Lines "Total Bonus X " & BonusMultiplier(CurrentPlayer), formatnumber(TotalBonus, 0), 3, "", 1:Playsound "fx_Next" 'i changed line end to 1 from 0 to time out text
        Case 5:AddScore TotalBonus
    End Select
    x = x + 1
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
    If(ExtraBallsAwards(CurrentPlayer) <> 0)Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        pMsg2Lines "Daddy " &CurrentPlayer, " Shoot Again", 2, "", 1
		PlaySound "vo_shootagain" &CurrentPlayer

        ' In this table an extra ball will have the skillshot and ball saver, so we reset the playfield for the new ball
        ResetForNewPlayerBall()

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            'debug.print "No More Balls, High Score Entry"

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
    If(PlayersPlayingGame> 1)Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from Daddy 4 back to Daddy 1)
        If(NextPlayer> PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode

        ' set the machine into game over mode
        EndOfGame()
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
            pMsg "Daddy " &CurrentPlayer, 2, "", 0
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then
        PlaySong ""
    End If
    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball
    ' show game over on the DMD
    pMsg "Game Over Daddy", 2, "", 1
    ' set any lights for the attract mode
    GiOff
    StartAttractMode
' you may wish to light any Game Over Light you may have
End Sub

Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp> BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
    ' Destroy the ball
    Drain.DestroyBall
    ' Exit Sub ' only for debugging - this way you can add balls from the debug window

    BallsOnPlayfield = BallsOnPlayfield - 1

    ' pretend to knock the ball into the ball storage mech
    PlaySoundAtVol "drain2", Drain, 1
    'DOF 113, DOFPulse
    'if Tilted the end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' is the ball saver active,
        If(bBallSaverActive = True)Then

            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
        ' you may wish to put something on a display or play a sound at this point
		PlaySound "vo_ballsave"
		pMsg "Ball Saved", 2, "", 1
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
					DOF 127, DOFPulse
                    bAutoPlunger = False
                    bSearchMultiball = False
                    bMurderMultiball = False
                    ' you may wish to change any music over at this point and
                    PlaySong "Theme_1"
                    ' turn off any multiball specific lights
                    StopSearchLights
                    MurderMultiballTimer.Enabled = 0
                    SuperJackpotTimer.Enabled = 0
                    'and turn back on any non-multiball mode
                    If Mode(CurrentPlayer, 0) = 0 Then
                        StartCollectables
                        l38.State = 1 'RED enable: make sure it is turned on
                        l21.State = 1 'Dad Jokes enabled at the bumpers
                        UpdateLockLights
                    End If
                    If ModeIsReady(CurrentPlayer)Then l17.State = 2 'lit back the mode ready light
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then
                ' End Mode and timers
                StopSong
                PuPlayer.playlistplayex pFullDMD, "Drains", "", 100, 1
				PuPlayer.playlistplayex pBackglass,"Backglass","Backglass2.mp4",0,1
                'ResetGi white
                GiOff
                ' Show the end of ball animation
                ' and continue with the end of ball
                vpmtimer.addtimer 6000, "EndOfBall '"

                StopEndOfBallMode
            End If
        End If
    End If
	PuPlayer.playlistplayex pBackglass,"Backglass","Backglass3.mp4",0,1
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub swPlungerRest_Hit()
    'debug.print "ball in plunger lane"
    ' some sound according to the ball position
    ' PlaySound "fx_sensor", 0, 1, 0.15, 0.25
    PlaySoundAtVol "fx_sensor", swPlungerRest, VolSens
    bBallInPlungerLane = True
    ' turn on Launch light is there is one
    'LaunchLight.State = 2

    ' kick the ball in play if the bAutoPlunger flag is on
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        PlungerIM.AutoFire
		DOF 140, DOFPulse
		DOF 114, DOFPulse
    End If

    'Start the Selection of the skillshot if ready
    If bSkillShotReady Then
        swPlungerRest.TimerEnabled = 1 ' this is a new ball, so show the "Hurry Up" clips if inactive for 10 seconds
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
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is ready, and it is currently not running, else it will reset the time period
    If(bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False)Then
        EnableBallSaver BallSaverTime
    End If
    If NOT bMultiballMode Then
        PlaySong "Theme_1"
    End If
' turn off LaunchLight
' LaunchLight.State = 0
	DOF 136, DOFPulse
End Sub

' swPlungerRest timer to show the "Hurry Up" clips if the player has not shot the ball during 10 seconds

Sub swPlungerRest_Timer
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
    LightShootAgain.BlinkInterval = 160
    SetLightColor LightShootAgain, white, 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimerExpired_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimerExpired.Enabled = False
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
    LightShootAgain.State = 0
    ' Stop the autoplunger
    bAutoPlunger = False
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
End Sub

'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 10 colors: red, orange, amber, yellow...
'******************************************

'colors
Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white, grey, white2

White2 = 12
Grey = 11
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
        Case Grey
            n.color = RGB(0, 0, 0)
            n.colorfull = RGB(0, 0, 0)
        Case red
            n.color = RGB(45, 0, 0)
            n.colorfull = RGB(121, 0, 0)
        Case orange
            n.color = RGB(18, 3, 0)
            n.colorfull = RGB(255, 64, 0)
        Case amber
            n.color = RGB(193, 49, 0)
            n.colorfull = RGB(187, 113, 0)
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
            n.color = RGB(255, 190, 132)
            n.colorfull = RGB(255, 255, 236)
        Case white2
            n.color = RGB(255, 255, 255)
            n.colorfull = RGB(255, 255, 255)

    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

Sub ResetAllLightsColor ' Called at a new game
End Sub

Sub UpdateBonusColors
End Sub

'***************************************************************************************
'Pinup Helper Subs
'***************************************************************************************

'Page 1 (Gameplay)  ********************************************************************

Sub pFullDMDLabelHide(labName)
    PuPlayer.LabelSet pFullDMD, labName, "", 0, ""
end sub

'most popups
Sub pMsg(msgText, timeSec, mColor, Blink)
		PuPlayer.LabelShowPage pFullDMD, 1, timeSec, ""
	If Blink = 1 then
		PuPlayer.LabelSet pFullDMD, "Message", msgText, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
	Else
	    PuPlayer.LabelSet pFullDMD, "Message", msgText, 1, ""
	End If
End Sub

Sub pMsg1Lines(msgText, timeSec, mColor, Blink)
		PuPlayer.LabelShowPage pFullDMD, 1, timeSec, ""
	If Blink = 1 then
		PuPlayer.LabelSet pFullDMD, "Line1a", msgText, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
	Else
		PuPlayer.LabelSet pFullDMD, "Line1a", msgText, 1, ""
	End If
end sub

Sub pMsg2Lines(msgText, msgText2, timeSec, mColor, Blink)
		PuPlayer.LabelShowPage pFullDMD, 1, timeSec, ""
	If Blink = 1 then
		PuPlayer.LabelSet pFullDMD, "Line1b", msgText, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
		PuPlayer.LabelSet pFullDMD, "Line2b", msgText2, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
	Else
		PuPlayer.LabelSet pFullDMD, "Line1b", msgText, 1, ""
		PuPlayer.LabelSet pFullDMD, "Line2b", msgText2, 1, ""
	End If
end Sub

Sub pMsgM(msgText, timeSec, mColor, Blink)
		PuPlayer.LabelShowPage pFullDMD, 1, timeSec, ""
	If Blink = 1 then
		PuPlayer.LabelSet pFullDMD, "MessageM", msgText, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
	Else
	    PuPlayer.LabelSet pFullDMD, "MessageM", msgText, 1, ""
	End If
End Sub

'Page 2 (Attract Mode)  ****************************************************************

'Attract mode player scores text (page 2)
Sub pScore2Lines(msgText, msgText2, timeSec, mColor, Blink)
		PuPlayer.LabelShowPage pFullDMD, 2, timeSec, ""
	If Blink = 1 then
		PuPlayer.LabelSet pFullDMD, "Score1", msgText, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
		PuPlayer.LabelSet pFullDMD, "Score2", msgText2, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
	Else
		PuPlayer.LabelSet pFullDMD, "Score1", msgText, 1, ""
		PuPlayer.LabelSet pFullDMD, "Score2", msgText2, 1, ""
	End If
end Sub

'attract msg (page 2)
Sub pMsgA(msgText, timeSec, mColor, Blink)
		PuPlayer.LabelShowPage pFullDMD, 2, timeSec, ""
	If Blink = 1 then
		PuPlayer.LabelSet pFullDMD, "Message2", msgText, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
	Else
	    PuPlayer.LabelSet pFullDMD, "Message2", msgText, 1, ""
	End If
End Sub

'2 lines attract msg (page 2)
Sub pMsg2LinesA(msgText, msgText2, timeSec, mColor, Blink)
		PuPlayer.LabelShowPage pFullDMD, 2, timeSec, ""
	If Blink = 1 then
		PuPlayer.LabelSet pFullDMD, "Line1bA", msgText, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
		PuPlayer.LabelSet pFullDMD, "Line2bA", msgText2, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
	Else
		PuPlayer.LabelSet pFullDMD, "Line1bA", msgText, 1, ""
		PuPlayer.LabelSet pFullDMD, "Line2bA", msgText2, 1, ""
	End If
end Sub

'high scores (page 2)
Sub pMsg3Lines(msgText, msgText2, msgText3, timeSec, mColor, Blink)
		PuPlayer.LabelShowPage pFullDMD, 2, timeSec, ""
	If Blink = 1 then
		PuPlayer.LabelSet pFullDMD, "Line1c", msgText, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
		PuPlayer.LabelSet pFullDMD, "Line2c", msgText2, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
		PuPlayer.LabelSet pFullDMD, "Line3c", msgText3, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
	Else
		PuPlayer.LabelSet pFullDMD, "Line1c", msgText, 1, ""
		PuPlayer.LabelSet pFullDMD, "Line2c", msgText2, 1, ""
		PuPlayer.LabelSet pFullDMD, "Line3c", msgText3, 1, ""
	End If
end Sub

'Thanks to (page2)
Sub pMsgTLines(msgText, msgText2, msgText3, timeSec, mColor, Blink)
		PuPlayer.LabelShowPage pFullDMD, 2, timeSec, ""
	If Blink = 1 then
		PuPlayer.LabelSet pFullDMD, "Line1c", msgText, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
		PuPlayer.LabelSet pFullDMD, "Line2c", msgText2, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
		PuPlayer.LabelSet pFullDMD, "Line3t", msgText3, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
	Else
		PuPlayer.LabelSet pFullDMD, "Line1c", msgText, 1, ""
		PuPlayer.LabelSet pFullDMD, "Line2c", msgText2, 1, ""
		PuPlayer.LabelSet pFullDMD, "Line3t", msgText3, 1, ""
	End If
end Sub

'Attract rules (page 2)
Sub pRules(msgText, msgText2, timeSec, mColor, Blink)
		PuPlayer.LabelShowPage pFullDMD, 2, timeSec, ""
	If Blink = 1 then
		PuPlayer.LabelSet pFullDMD, "RulesTitleA", msgText, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
		PuPlayer.LabelSet pFullDMD, "RulesTextA", msgText2, 0, "{'mt':1,'at':" & Blink & ",'fq':150,'len':" & (timeSec * 1000) & ",'fc':" & mColor & "}"
	Else
		PuPlayer.LabelSet pFullDMD, "RulesTitleA", msgText, 1, ""
		PuPlayer.LabelSet pFullDMD, "RulesTextA", msgText2, 1, ""
	End If
end Sub


Sub pAttractscores(msgText, msgText2, timeSec, mColor)
		PuPlayer.LabelShowPage pFullDMD, 2, timeSec, ""
		PuPlayer.LabelSet pFullDMD, "Score1", msgText, 1, ""
		PuPlayer.LabelSet pFullDMD, "Score2", msgText2, 1, ""
End Sub


Sub SetupPuP()
'***************************************************************************************
'Backglass Labelsets
'***************************************************************************************

	PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1
	PuPlayer.playlistplayex pFullDMD,"PuPOverlays","newframe.png",0,1
	PuPlayer.playlistplayex pFullDMD,"Backglass","Backglass.mp4",0,1
	

'Font colors:
	Const cWhite = 	16777215
	Const cRed = 	397512
	Const cGold = 	1604786
	Const cGrey = 	8421504

'<screen#>,<Labelname>,<fontName>,<size%>,<colour>,<rotation>,<xAlign>,<yAlign>,<xpos>,<ypos>,<PageNum>,<visible>

'<screen#>, in standard we'd set this to pDMD ( or 1)
'<Labelname>, your name of the label. When setting the label you will use this labelname to access the label.
'<fontName> Windows font name, this must be exact match of OS front name. if you are using custom TTF fonts then double check the name of font names.
'<size%>, Height as a percent of display height. 20=20% of screen height.
'<colour>, integer value of windows color.
'<rotation>, not used yet (future)
'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but if you want to 'force' a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'<PageNum> IMPORTANT... this will assign this label to this 'page' or group.
'<visible> initial state of label. visible=1 show, 0 = off.

' Page 1  ***********************************************************************************************	
						
	PuPlayer.LabelNew pFullDMD,"Play1",typefont,				4,cWhite	,0,0,1,	5,49,	1,1
	PuPlayer.LabelNew pFullDMD,"Play1score",typefont,			4,cWhite	,0,0,1,	5,53,	1,1
	PuPlayer.LabelNew pFullDMD,"Play2",typefont,				4,cWhite  	,0,0,1,	5,57,	1,1
	PuPlayer.LabelNew pFullDMD,"Play2score",typefont,			4,cWhite  	,0,0,1,	5,61,	1,1
	PuPlayer.LabelNew pFullDMD,"Play3",typefont,				4,cWhite	,0,0,1,	5,65,	1,1
	PuPlayer.LabelNew pFullDMD,"Play3score",typefont,			4,cWhite  	,0,0,1,	5,69,	1,1
	PuPlayer.LabelNew pFullDMD,"Play4",typefont,				4,cWhite  	,0,0,1,	5,73,	1,1
	PuPlayer.LabelNew pFullDMD,"Play4score",typefont,			4,cWhite  	,0,0,1,	5,77,	1,1

	PuPlayer.LabelNew pFullDMD,"curscore",typefont,			10,cWhite	,0,1,1,	50,85,	1,1
	PuPlayer.LabelNew pFullDMD,"curplayer",typefont,			4,cGold		,0,1,1,	50,91,	1,1
	PuPlayer.LabelNew pFullDMD,"Ball",typefont,				3,cWhite 	,0,1,1,	50,95,	1,1

	PuPlayer.LabelNew pFullDMD,"hstitle1",typefont,			4,cGrey 	,0,1,1,	91,12,	1,1
	PuPlayer.LabelNew pFullDMD,"hstitle2",typefont,			4,cGrey 	,0,1,1,	91,16,	1,1
	PuPlayer.LabelNew pFullDMD,"hsname",typefont,				7,cGold 	,0,1,1,	91,21,	1,1
	PuPlayer.LabelNew pFullDMD,"hs",typefont,					5,cWhite 	,0,1,1,	91,26,	1,1
	
	PuPlayer.LabelNew pFullDMD,"gptitle1",typefont,			4,cGrey 	,0,1,1,	91,35,	1,1
	PuPlayer.LabelNew pFullDMD,"gptitle2",typefont,			4,cGrey 	,0,1,1,	91,38,	1,1
	PuPlayer.LabelNew pFullDMD,"gp",typefont,					5,cWhite 	,0,1,1,	91,43,	1,1

	PuPlayer.LabelNew pFullDMD,"RulesTitle",typefont,			4,cGold		,0,1,1,	50,4,	1,1
	PuPlayer.LabelNew pFullDMD,"RulesText",typefont,			3,cWhite 	,0,1,1,	50,8,	1,1

	PuPlayer.LabelNew pFullDMD,"SecretsTitle",typefont,		4,cGold  	,0,0,1,	5,12,	1,1
	PuPlayer.LabelNew pFullDMD,"SecretsTotal",typefont,		4,cWhite  	,0,0,1,	5,16,	1,1
	PuPlayer.LabelNew pFullDMD,"PhotosTitle",typefont,		4,cGold  	,0,0,1,	5,20,	1,1
	PuPlayer.LabelNew pFullDMD,"PhotosTotal",typefont,		4,cWhite	,0,0,1,	5,24,	1,1
	PuPlayer.LabelNew pFullDMD,"BribesTitle",typefont,		4,cGold		,0,0,1,	5,28,	1,1
	PuPlayer.LabelNew pFullDMD,"BribesTotal",typefont,		4,cWhite	,0,0,1,	5,32,	1,1
	PuPlayer.LabelNew pFullDMD,"CluesTitle",typefont,			4,cGold		,0,0,1,	5,36,	1,1
	PuPlayer.LabelNew pFullDMD,"CluesTotal",typefont,			4,cWhite	,0,0,1,	5,40,	1,1

	PuPlayer.LabelNew pFullDMD,"Message", typefont, 			12,cWhite	,0,1,1,	50,50,	1,0
	PuPlayer.LabelNew pFullDMD,"MessageM", typefont, 			12,cGold	,0,1,1,	50,40,	1,0

'(1 Line Text)
	PuPlayer.LabelNew pFullDMD, "Line1a",typefont, 			10,cWhite	,0,1,1,	50,50,	1,0
	
'(2 Line text)
	PuPlayer.LabelNew pFullDMD, "Line1b",typefont,			10,cGold	,0,1,1, 50,60,	1,0
	PuPlayer.LabelNew pFullDMD, "Line2b",typefont, 			10,cWhite	,0,1,1, 50,70,	1,0

' Page 2  ***********************************************************************************************

	PuPlayer.LabelNew pFullDMD,"Message2", typefont, 			12,cWhite	,0,1,1,	50,50,	2,0
	
'(2 Line text)
	PuPlayer.LabelNew pFullDMD, "Line1bA",typefont,			10,cGold	,0,1,1, 50,60,	2,0
	PuPlayer.LabelNew pFullDMD, "Line2bA",typefont, 			10,cWhite	,0,1,1, 50,70,	2,0
	
'(3 Line Text)
	PuPlayer.LabelNew pFullDMD, "Line1c",typefont, 			10,cWhite	,0,1,1, 50,60,	2,0
	PuPlayer.LabelNew pFullDMD, "Line2c",typefont,			20,cGold	,0,1,1, 50,71,	2,0
	PuPlayer.LabelNew pFullDMD, "Line3c",typefont,			10,cWhite	,0,1,1, 50,84,	2,0

	PuPlayer.LabelNew pFullDMD, "Line3t",typefont,			6,cWhite	,0,1,1, 50,83,	2,0

'(2 Line Attract Score Text)
	PuPlayer.LabelNew pFullDMD, "Score1",typefont,			20,cGold	,0,1,1, 50,71,	2,0
	PuPlayer.LabelNew pFullDMD, "Score2",typefont, 			10,cWhite	,0,1,1, 50,84,	2,0

'(attract mode rules)
	PuPlayer.LabelNew pFullDMD,"RulesTitleA",typefont,		4,cGold		,0,1,1,	50,4,	2,1
	PuPlayer.LabelNew pFullDMD,"RulesTextA",typefont,			3,cWhite 	,0,1,1,	50,8,	2,1

	PuPlayer.LabelNew pFullDMD,"Play1A",typefont,				4,cGold		,0,0,1,	5,49,	2,1
	PuPlayer.LabelNew pFullDMD,"Play1scoreA",typefont,		4,cWhite	,0,0,1,	5,53,	2,1
	PuPlayer.LabelNew pFullDMD,"Play2A",typefont,				4,cGold  	,0,0,1,	5,57,	2,1
	PuPlayer.LabelNew pFullDMD,"Play2scoreA",typefont,		4,cWhite  	,0,0,1,	5,61,	2,1
	PuPlayer.LabelNew pFullDMD,"Play3A",typefont,				4,cGold		,0,0,1,	5,65,	2,1
	PuPlayer.LabelNew pFullDMD,"Play3scoreA",typefont,		4,cWhite  	,0,0,1,	5,69,	2,1
	PuPlayer.LabelNew pFullDMD,"Play4A",typefont,				4,cGold  	,0,0,1,	5,73,	2,1
	PuPlayer.LabelNew pFullDMD,"Play4scoreA",typefont,		4,cWhite  	,0,0,1,	5,77,	2,1

'********************************************************************************************************
End Sub

Sub resetbackglass
	PuPlayer.LabelShowPage pFullDMD,1,0,""
	PuPlayer.SetBackground pFullDMD,1
'right side
	PuPlayer.LabelSet pFullDMD,"hstitle1","WORLDS",1,""
	PuPlayer.LabelSet pFullDMD,"hstitle2","GREATEST DAD",1,""
	PuPlayer.LabelSet pFullDMD,"hs","" & FormatNumber(HighScore(0),0),1,""
	PuPlayer.LabelSet pFullDMD,"hsname","" & (HighScorename(0)),1,""
	PuPlayer.LabelSet pFullDMD,"gptitle1","GAMES",1,""
	PuPlayer.LabelSet pFullDMD,"gptitle2","PLAYED",1,""
	PuPlayer.LabelSet pFullDMD,"gp","" & FormatNumber(TotalGamesPlayed,0),1,""
'center
	PuPlayer.LabelSet pFullDMD,"Ball","",1,""
'left side
	PuPlayer.LabelSet pFullDMD,"SecretsTitle","Movie Quotes",1,""
	PuPlayer.LabelSet pFullDMD,"SecretsTotal","" & FormatNumber(GSecretsCount(CurrentPlayer),0),1,""
	PuPlayer.LabelSet pFullDMD,"PhotosTitle","Dad Naps",1,""
	PuPlayer.LabelSet pFullDMD,"PhotosTotal","" & FormatNumber(PhotoCount(CurrentPlayer),0),1,""
	PuPlayer.LabelSet pFullDMD,"BribesTitle","Tools",1,""
	PuPlayer.LabelSet pFullDMD,"BribesTotal","" & FormatNumber(BribesCount(CurrentPlayer),0),1,""
	PuPlayer.LabelSet pFullDMD,"CluesTitle","Dad Jokes",1,""
	PuPlayer.LabelSet pFullDMD,"CluesTotal","" & FormatNumber(CluesCount(CurrentPlayer),0),1,""
'left scores
    PuPlayer.LabelSet pFullDMD,"Play1score","",1,"{'mt':2,'color':8421504}"
    PuPlayer.LabelSet pFullDMD,"Play1","",1,"{'mt':2,'color':8421504}"
    PuPlayer.LabelSet pFullDMD,"Play2score","",1,"{'mt':2,'color':8421504}"
    PuPlayer.LabelSet pFullDMD,"Play2","",1,"{'mt':2,'color':8421504}"
    PuPlayer.LabelSet pFullDMD,"Play3score","",1,"{'mt':2,'color':8421504}"
    PuPlayer.LabelSet pFullDMD,"Play3","",1,"{'mt':2,'color':8421504}"
    PuPlayer.LabelSet pFullDMD,"Play4score","",1,"{'mt':2,'color':8421504}"
    PuPlayer.LabelSet pFullDMD,"Play4","",1,"{'mt':2,'color':8421504}"
End Sub

Sub pUpdateScores
	PuPlayer.LabelSet pFullDMD,"curscore",FormatNumber(Score(CurrentPlayer),0),1,""
	PuPlayer.LabelSet pFullDMD,"curplayer","Daddy " & CurrentPlayer,1,""
	PuPlayer.LabelSet pFullDMD,"Ball","Ball "  &  BallsPerGame - BallsRemaining(CurrentPlayer) + 1 ,1,""
	PuPlayer.LabelSet pFullDMD,"SecretsTitle","Movie Quotes",1,""
	PuPlayer.LabelSet pFullDMD,"SecretsTotal","" & FormatNumber(GSecretsCount(CurrentPlayer),0),1,""
	PuPlayer.LabelSet pFullDMD,"PhotosTitle","Dad Naps",1,""
	PuPlayer.LabelSet pFullDMD,"PhotosTotal","" & FormatNumber(PhotoCount(CurrentPlayer),0),1,""
	PuPlayer.LabelSet pFullDMD,"BribesTitle","Tools",1,""
	PuPlayer.LabelSet pFullDMD,"BribesTotal","" & FormatNumber(BribesCount(CurrentPlayer),0),1,""
	PuPlayer.LabelSet pFullDMD,"CluesTitle","Dad Jokes",1,""
	PuPlayer.LabelSet pFullDMD,"CluesTotal","" & FormatNumber(CluesCount(CurrentPlayer),0),1,""

'Do not show scores on left if only 1 player is playing
	'If PlayersPlayingGame = 1 then exit sub end if 

	If CurrentPlayer = 1 Then
		PuPlayer.LabelSet pFullDMD,"Play1score","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':16777215}"
		PuPlayer.LabelSet pFullDMD,"Play1","Daddy 1",1,"{'mt':2,'color':1604786 }"
	'make other scores grey (inactive)
		If PlayersPlayingGame = 2 Then
			PuPlayer.LabelSet pFullDMD,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play2","Daddy 2",1,"{'mt':2,'color':8421504}"
		End If
		If PlayersPlayingGame = 3 Then
			PuPlayer.LabelSet pFullDMD,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play2","Daddy 2",1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play3","Daddy 3",1,"{'mt':2,'color':8421504}"
		End If
		If PlayersPlayingGame = 4 Then
			PuPlayer.LabelSet pFullDMD,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play2","Daddy 2",1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play3","Daddy 3",1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play4score","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play4","Daddy 4",1,"{'mt':2,'color':8421504}"
		End If
	End If
	If CurrentPlayer = 2 Then
		PuPlayer.LabelSet pFullDMD,"Play2score","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':16777215}"
		PuPlayer.LabelSet pFullDMD,"Play2","Daddy 2",1,"{'mt':2,'color':1604786 }"
		If PlayersPlayingGame = 2 Then
			PuPlayer.LabelSet pFullDMD,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play1","Daddy 1",1,"{'mt':2,'color':8421504}"
		End If
		If PlayersPlayingGame = 3 Then
			PuPlayer.LabelSet pFullDMD,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play1","Daddy 1",1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play3","Daddy 3",1,"{'mt':2,'color':8421504}"
		End If
		If PlayersPlayingGame = 4 Then
			PuPlayer.LabelSet pFullDMD,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play1","Daddy 1",1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play3","Daddy 3",1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play4score","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play4","Daddy 4",1,"{'mt':2,'color':8421504}"
		End If
	End If
	If CurrentPlayer = 3 Then
		PuPlayer.LabelSet pFullDMD,"Play3score","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':16777215}"
		PuPlayer.LabelSet pFullDMD,"Play3","Daddy 3",1,"{'mt':2,'color':1604786 }"
		If PlayersPlayingGame = 3 Then
			PuPlayer.LabelSet pFullDMD,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play1","Daddy 1",1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play2","Daddy 2",1,"{'mt':2,'color':8421504}"
		End If
		If PlayersPlayingGame = 4 Then
			PuPlayer.LabelSet pFullDMD,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play1","Daddy 1",1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play2","Daddy 2",1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play4score","" & FormatNumber(Score(4),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play4","Daddy 4",1,"{'mt':2,'color':8421504}"
		End If
	End If
	If CurrentPlayer = 4 Then
		PuPlayer.LabelSet pFullDMD,"Play4score","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2,'color':16777215}"
		PuPlayer.LabelSet pFullDMD,"Play4","Daddy 4",1,"{'mt':2,'color':1604786 }"
		If PlayersPlayingGame = 4 Then
			PuPlayer.LabelSet pFullDMD,"Play1score","" & FormatNumber(Score(1),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play1","Daddy 1",1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play2score","" & FormatNumber(Score(2),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play2","Daddy 2",1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play3score","" & FormatNumber(Score(3),0),1,"{'mt':2,'color':8421504}"
			PuPlayer.LabelSet pFullDMD,"Play3","Daddy 3",1,"{'mt':2,'color':8421504}"
		End If
	End If
End Sub

Sub ruleshelperon
	rulestime.enabled = 1
End Sub

Sub ruleshelperoff
	rulestime.enabled = 0
End Sub

Dim rulesposition
	rulesposition = 0

Sub rulestime_timer
	If turnoffrules = 2 then exit sub end if
	rulesposition = rulesposition + 1
	Select Case rulesposition
	Case 1
		PuPlayer.LabelSet pFullDMD,"RulesTitle","Skillshot",1,""
		PuPlayer.LabelSet pFullDMD,"RulesText","Hit flashing P-O-P target. Skillshot value increases every hit.",1,""
	Case 8
		PuPlayer.LabelSet pFullDMD,"RulesTitle","Joke Targets",1,""
		PuPlayer.LabelSet pFullDMD,"RulesText","Light up all J-O-K-E targets to light MYSTERY",1,""
	Case 15
		PuPlayer.LabelSet pFullDMD,"RulesTitle","Bonus Multiplier",1,""
		PuPlayer.LabelSet pFullDMD,"RulesText","Completing top P-O-P lanes increases BONUS X (2X, 4X, 6X, Light EXTRA BALL)",1,""
	Case 24
		PuPlayer.LabelSet pFullDMD,"RulesTitle","Date Lanes",1,""
		PuPlayer.LabelSet pFullDMD,"RulesText","D-A-T-E Lanes 3x lights PICK UP KID, 10x to light EXTRA BALL.",1,""
	Case 32
		PuPlayer.LabelSet pFullDMD,"RulesTitle","Dad Joke Frenzy",1,""
		PuPlayer.LabelSet pFullDMD,"RulesText","2X Scoring after 100 Dad Jokes Collected.",1,""
	Case 40
		PuPlayer.LabelSet pFullDMD,"RulesTitle","Collectables",1,""
		PuPlayer.LabelSet pFullDMD,"RulesText","Hit flashing BLUE targets to increase end of ball bonus",1,""
	Case 48
		PuPlayer.LabelSet pFullDMD,"RulesTitle","Dates",1,""
		PuPlayer.LabelSet pFullDMD,"RulesText","Collect any 3 BLUE targets to light START DATE. Follow lit YELLOW targets to complete DATE",1,""
	Case 56
		PuPlayer.LabelSet pFullDMD,"RulesTitle","Margarita Multi-ball",1,""
		PuPlayer.LabelSet pFullDMD,"RulesText","Lock 3 balls to start. Drink Margaritas to collect JACKPOTS. Collect ALL JACKPOTS to light SUPER JACKPOT",1,""
	Case 64
		PuPlayer.LabelSet pFullDMD,"RulesTitle","Dream Daddy Multiball",1,""
		PuPlayer.LabelSet pFullDMD,"RulesText","Starts after 3 and 6 Dates complete",1,""
	Case 72
		PuPlayer.LabelSet pFullDMD,"RulesTitle","WIZARD MODE",1,""
		PuPlayer.LabelSet pFullDMD,"RulesText","Who Will you Choose? Starts after All 7 Dates are complete",1,""
	Case 80
		rulesposition = 1
	End Select
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAtVol SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper, VolFlip
        LF.fire ' LeftFlipper.RotateToEnd      
        If bSkillShotReady = False Then
            RotateLaneLightsLeft
        End If    
    Else
        PlaySoundAtVol SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers),  LeftFlipper, VolFlip
        LeftFlipper.RotateToStart
		lfpress=1
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAtVol SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper, VolFlip
        RF.fire ' RightFlipper.RotateToEnd
		rfpress=1    
        If bSkillShotReady = False Then
            RotateLaneLightsRight
        End If
              
    Else
        PlaySoundAtVol SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper, VolFlip
        RightFlipper.RotateToStart
    End If
End Sub

'*****************************************************

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt        'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay        'delay before trigger turns off and polarity is disabled TODO set time!
	private Flipper, FlipperStart,FlipperEnd, FlipperEndY, LR, PartialFlipCoef
	Private Balls(20), balldata(20)

	dim PolarityIn, PolarityOut
	dim VelocityIn, VelocityOut
	dim YcoefIn, YcoefOut
	Public Sub Class_Initialize 
		redim PolarityIn(0) : redim PolarityOut(0) : redim VelocityIn(0) : redim VelocityOut(0) : redim YcoefIn(0) : redim YcoefOut(0)
		Enabled = True : TimeDelay = 50 : LR = 1:  dim x : for x = 0 to uBound(balls) : balls(x) = Empty : set Balldata(x) = new SpoofBall : next 
	End Sub

	Public Property let Object(aInput) : Set Flipper = aInput : StartPoint = Flipper.x : End Property
	Public Property Let StartPoint(aInput) : if IsObject(aInput) then FlipperStart = aInput.x else FlipperStart = aInput : end if : End Property
	Public Property Get StartPoint : StartPoint = FlipperStart : End Property
	Public Property Let EndPoint(aInput) : FlipperEnd = aInput.x: FlipperEndY = aInput.y: End Property
	Public Property Get EndPoint : EndPoint = FlipperEnd : End Property        
	Public Property Get EndPointY: EndPointY = FlipperEndY : End Property

	Public Sub AddPoint(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out) 
		Select Case aChooseArray
			case "Polarity" : ShuffleArrays PolarityIn, PolarityOut, 1 : PolarityIn(aIDX) = aX : PolarityOut(aIDX) = aY : ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity" : ShuffleArrays VelocityIn, VelocityOut, 1 :VelocityIn(aIDX) = aX : VelocityOut(aIDX) = aY : ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef" : ShuffleArrays YcoefIn, YcoefOut, 1 :YcoefIn(aIDX) = aX : YcoefOut(aIDX) = aY : ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
		if gametime > 100 then Report aChooseArray
	End Sub 

	Public Sub Report(aChooseArray)         'debug, reports all coords in tbPL.text
		if not DebugOn then exit sub
		dim a1, a2 : Select Case aChooseArray
			case "Polarity" : a1 = PolarityIn : a2 = PolarityOut
			Case "Velocity" : a1 = VelocityIn : a2 = VelocityOut
			Case "Ycoef" : a1 = YcoefIn : a2 = YcoefOut 
				case else :tbpl.text = "wrong string" : exit sub
		End Select
		dim str, x : for x = 0 to uBound(a1) : str = str & aChooseArray & " x: " & round(a1(x),4) & ", " & round(a2(x),4) & vbnewline : next
		tbpl.text = str
	End Sub

	Public Sub AddBall(aBall) : dim x : for x = 0 to uBound(balls) : if IsEmpty(balls(x)) then set balls(x) = aBall : exit sub :end if : Next  : End Sub

	Private Sub RemoveBall(aBall)
		dim x : for x = 0 to uBound(balls)
			if TypeName(balls(x) ) = "IBall" then 
				if aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub

	Public Sub Fire() 
		Flipper.RotateToEnd
		processballs
	End Sub

	Public Property Get Pos 'returns % position a ball. For debug stuff.
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next                
	End Property

	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		dim x : for x = 0 to uBound(balls)
			if not IsEmpty(balls(x) ) then
				balldata(x).Data = balls(x)
			End If
		Next
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub
	Private Function FlipperOn() : if gameTime < FlipAt+TimeDelay then FlipperOn = True : End If : End Function        'Timer shutoff for polaritycorrect

	Public Sub PolarityCorrect(aBall)
		if FlipperOn() then 
			dim tmp, BallPos, x, IDX, Ycoef : Ycoef = 1

			'y safety Exit
			if aBall.VelY > -8 then 'ball going down
				RemoveBall aBall
				exit Sub
			end if

			'Find balldata. BallPos = % on Flipper
			for x = 0 to uBound(Balls)
				if aBall.id = BallData(x).id AND not isempty(BallData(x).id) then 
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					if ballpos > 0.65 then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)                                'find safety coefficient 'ycoef' data
				end if
			Next

			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				if ballpos > 0.65 then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)                                                'find safety coefficient 'ycoef' data
			End If

			'Velocity correction
			if not IsEmpty(VelocityIn(0) ) then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)

				if partialflipcoef < 1 then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)

				if Enabled then aBall.Velx = aBall.Velx*VelCoef
				if Enabled then aBall.Vely = aBall.Vely*VelCoef
			End If

			'Polarity Correction (optional now)
			if not IsEmpty(PolarityIn(0) ) then
				If StartPoint > EndPoint then LR = -1        'Reverse polarity if left flipper
				dim AddX : AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR

				if Enabled then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef)
			End If
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY. RUBBER DAMPENER, AND SLINGSHOT CORRECTION SUPPORTING FUNCTIONS 
'******************************************************


Sub AddPt(aStr, idx, aX, aY)        'debugger wrapper for adjusting flipper script in-game
	dim a : a = Array(LF, RF)
	dim x : for each x in a
		x.addpoint aStr, idx, aX, aY
	Next
End Sub


' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	dim x, aCount : aCount = 0
	redim a(uBound(aArray) )
	for x = 0 to uBound(aArray)        'Shuffle objects in a temp array
		if not IsEmpty(aArray(x) ) Then
			if IsObject(aArray(x)) then 
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	if offset < 0 then offset = 0
	redim aArray(aCount-1+offset)        'Resize original array
	for x = 0 to aCount-1                'set objects back into original array
		if IsObject(a(x)) then 
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = SQR(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)        'Set up line via two points, no clamping. Input X, output Y
	dim x, y, b, m : x = input : m = (Y2 - Y1) / (X2 - X1) : b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball 
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius 
	Public Property Let Data(aBall)
		With aBall
			x = .x : y = .y : z = .z : velx = .velx : vely = .vely : velz = .velz
			id = .ID : mass = .mass : radius = .radius
		end with
	End Property
	Public Sub Reset()
		x = Empty : y = Empty : z = Empty  : velx = Empty : vely = Empty : velz = Empty 
		id = Empty : mass = Empty : radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	dim y 'Y output
	dim L 'Line
	dim ii : for ii = 1 to uBound(xKeyFrame)        'find active line
		if xInput <= xKeyFrame(ii) then L = ii : exit for : end if
	Next
	if xInput > xKeyFrame(uBound(xKeyFrame) ) then L = uBound(xKeyFrame)        'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )

	if xInput <= xKeyFrame(lBound(xKeyFrame) ) then Y = yLvl(lBound(xKeyFrame) )         'Clamp lower
	if xInput >= xKeyFrame(uBound(xKeyFrame) ) then Y = yLvl(uBound(xKeyFrame) )        'Clamp upper

	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS 
'******************************************************

RightFlipper.timerinterval=1
Rightflipper.timerenabled=True

sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
end sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim BOT, b
	BOT = GetBalls

	If Flipper1.currentangle = Endangle1 and EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then 
			For b = 0 to Ubound(bot)
				If FlipperTrigger(bot(b).x, bot(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					exit Sub
				end If
			Next
			For b = 0 to Ubound(bot)
				If FlipperTrigger(bot(b).x, bot(b).y, Flipper2) Then
					bot(b).velx = bot(b).velx / 1.3
					bot(b).vely = bot(b).vely - 0.5
				end If
			Next
		End If
	Else 
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 then EOSNudge1 = 0
	End If
End Sub

'*****************
' Maths
'*****************
Dim PI: PI = 4*Atn(1)

Function dSin(degrees)
	dsin = sin(degrees * Pi/180)
End Function

Function dCos(degrees)
	dcos = cos(degrees * Pi/180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then 
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		end if
	ElseIf dx = 0 Then
		if dy = 0 Then
			Atn2 = 0
		else
			Atn2 = Sgn(dy) * pi / 2
		end if
	End If
End Function

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = SQR((ax - bx)^2 + (ay - by)^2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) ' Distance between a point and a line where point is px,py
	DistancePL = ABS((by - ay)*px - (bx - ax) * py + bx*ay - by*ax)/Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI /180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax))*180/PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle+90))+Flipper.x, Sin(Radians(Flipper.currentangle+90))+Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle  = ABS(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360

	If DistanceFromFlipper(ballx,bally,Flipper) < 48 and DiffAngle <= 90 and Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If        
End Function


'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

dim LFPress, RFPress, LFCount, RFCount
dim LFState, RFState
dim EOST, EOSA,Frampup, FElasticity,FReturn
dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0   	'0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1 'EM's to late 80's
Const EOSTnew = 0.8 '90's and later
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode 
	Case 0:
		SOSRampup = 2.5
	Case 1:
		SOSRampup = 6
	Case 2:
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'Const EOSReturn = 0.055  'EM's
'Const EOSReturn = 0.045  'late 70's to mid 80's
Const EOSReturn = 0.035  'mid 80's to early 90's
'Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity

	Flipper.eostorque = EOST         
	Flipper.eostorqueangle = EOSA         
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST*EOSReturn/FReturn


	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim BOT, b
		BOT = GetBalls

		For b = 0 to UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= -0.4 Then BOT(b).vely = -0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState) 
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)        '-1 for Right Flipper

	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup 
			Flipper.endangle = FEndAngle - 3*Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0 
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) and FlipperPress = 1 then
		if FCount = 0 Then FCount = GameTime

		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup                        
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	Elseif Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 and FlipperPress = 1 Then 
		If FState <> 3 Then
			Flipper.eostorque = EOST        
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If

	End If
End Sub

Const LiveDistanceMin = 30  'minimum distance in vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114  'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
	Dim Dir
	Dir = Flipper.startangle/Abs(Flipper.startangle)    '-1 for Right Flipper
	Dim LiveCatchBounce                                                                                                                        'If live catch is not perfect, it won't freeze ball totally
	Dim CatchTime : CatchTime = GameTime - FCount

	if CatchTime <= LiveCatch and parm > 6 and ABS(Flipper.x - ball.x) > LiveDistanceMin and ABS(Flipper.x - ball.x) < LiveDistanceMax Then
		if CatchTime <= LiveCatch*0.5 Then                                                'Perfect catch only when catch time happens in the beginning of the window
			LiveCatchBounce = 0
		else
			LiveCatchBounce = Abs((LiveCatch/2) - CatchTime)        'Partial catch when catch happens a bit late
		end If

		If LiveCatchBounce = 0 and ball.velx * Dir > 0 Then ball.velx = 0
		ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
		ball.angmomx= 0
		ball.angmomy= 0
		ball.angmomz= 0
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf Activeball, parm
	End If
End Sub


'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************


' *********************************************************************
' Lane Change
' *********************************************************************

Sub RotateLaneLightsLeft
	Dim TopTempState
	Dim TempState
    'top lanes
    TopTempState= l41.State
    l41.State = l42.State
    l42.State = l43.State
    l43.State = TopTempState
    ' flipper lanes
    TempState = l1.State
    l1.State = l2.State
    l2.State = l3.State
    l3.State = l4.State
    l4.State = TempState
End Sub

Sub RotateLaneLightsRight
    Dim TempState
	Dim TopTempState
    'top lanes
    TopTempState= l43.State
    l43.State = l42.State
    l42.State = l41.State
    l41.State = TopTempState
    ' flipper lanes
    TempState = l4.State
    l4.State = l3.State
    l3.State = l2.State
    l2.State = l1.State
    l1.State = TempState
End Sub



' *********************************************************************
' *********************************************************************

' Sling Shot Animations

' *********************************************************************
' *********************************************************************

' Rstep and Lstep  are the variables that increment the animation

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    ' PlaySound SoundFXDOF("right_slingshot", 104, DOFPulse, DOFContactors), 0, 1, 0.05, 0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    PlaySoundAtVol SoundFXDOF("right_slingshot", 104, DOFPulse, DOFContactors), sling1, VolSling
  	DOF 109, DOFPulse
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.TransZ = -20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    'gi1.State = 0:Gi2.State = 0
    Primitive2.RotY = 10
    AddScore 500
	RotateJokeLights
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 2:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.TransZ = -10
        Case 3:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.TransZ = 0:Primitive2.RotY = 0:RightSlingShot.TimerEnabled = 0:gi1.State = 1:Gi2.State = 1
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
   ' PlaySound SoundFXDOF("left_slingshot", 103, DOFPulse, DOFContactors), 0, 1, -0.05, 0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    PlaySoundAtVol SoundFXDOF("left_slingshot", 103, DOFPulse, DOFContactors), sling2, VolSling
  	DOF 108, DOFPulse
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.TransZ = -20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    'gi3.State = 0:Gi4.State = 0
    Primitive12.RotY = -10
    AddScore 500
	RotateJokeLights
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 2:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.TransZ = -10
        Case 3:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.TransZ = 0:Primitive12.RotY = 0:LeftSlingShot.TimerEnabled = 0:gi3.State = 1:Gi4.State = 1
    End Select
    LStep = LStep + 1
End Sub

' *********************************************************************
' Positional Sound Playback Functions
' *********************************************************************

' Play a sound, depending on the X,Y position of the table element (especially cool for surround speaker setups, otherwise stereo panning only)
' parameters (defaults): loopcount (1), volume (1), randompitch (0), pitch (0), useexisting (0), restart (1))
' Note that this will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position

Sub PlayXYSound(soundname, tableobj, loopcount, volume, randompitch, pitch, useexisting, restart)
    PlaySound soundname, loopcount, volume, AudioPan(tableobj), randompitch, pitch, useexisting, restart, AudioFade(tableobj)
End Sub

' Similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
Sub PlaySoundAt(soundname, tableobj)
    PlaySound soundname, 1, 1, AudioPan(tableobj), 0, 0, 0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtVol(soundname, tableobj, Vol)
  PlaySound soundname, 1, Vol, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname)
    PlaySoundAt soundname, ActiveBall
End Sub

' *********************************************************************
' Supporting Ball & Sound Functions
' *********************************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.y * 2 / table1.height-1
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = tableobj.x * 2 / table1.width-1
    If tmp> 0 Then
        AudioPan = Csng(tmp ^10)
    Else
        AudioPan = Csng(-((- tmp) ^10))
    End If
End Function

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / VolDiv)
	'Debug.Print Vol
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = INT(SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

'*****************************************
'      JP's VP10 Rolling Sounds
'*****************************************

Const tnob = 6 ' total number of balls
Const lob = 0  'number of locked balls
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
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)
        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + MetalPitch 'increase the pitch on a ramp
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, Vol(BOT(b)), AudioPan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
    Next
End Sub

' *********************************************************************
' Ball Collision Sound
' *********************************************************************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / (VolDiv/VolCol), AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

' *********************************************************************
' BALL SHADOWS
' *********************************************************************

Dim BallShadow
BallShadow = Array(BallShadow1, BallShadow2, BallShadow3, BallShadow4, BallShadow5, BallShadow6)

Sub BallShadowUpdate()
    Dim BOT, b
    BOT = GetBalls
    ' hide shadow of deleted balls
    If UBound(BOT) <(tnob-1)Then
        For b = (UBound(BOT) + 1)to(tnob-1)
            BallShadow(b).visible = 0
        Next
    End If
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub
    ' render the shadow for each ball
    For b = 0 to UBound(BOT)
        If BOT(b).X <Table1.Width / 2 Then
            BallShadow(b).X = ((BOT(b).X)-(Ballsize / 6) + ((BOT(b).X -(Table1.Width / 2)) / 7)) + 6
        Else
            BallShadow(b).X = ((BOT(b).X) + (Ballsize / 6) + ((BOT(b).X -(Table1.Width / 2)) / 7))- 6
        End If
        ballShadow(b).Y = BOT(b).Y + 12
        If BOT(b).Z> 20 Then
            BallShadow(b).visible = 1
        Else
            BallShadow(b).visible = 0
        End If
    Next
End Sub


' *********************************************************************
' Index
' *********************************************************************

Sub aRubberPins_Hit(idx)
    PlaySound "fx_pinhit_low", 0, Vol(ActiveBall)*VolPi, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Targets_Hit(idx)
    PlaySound "target2", 0, Vol(ActiveBall)*VolTarg, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub Guides_Hit(idx)
    PlaySound "fx_wood", 0, Vol(ActiveBall)*VolGuides, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Thin_Hit(idx)
    PlaySound "metalhit_thin", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals_Medium_Hit(idx)
    PlaySound "metalhit_medium", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Metals2_Hit(idx)
    PlaySound "metalhit2", 0, Vol(ActiveBall)*VolMetal, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Gates_Hit(idx)
    PlaySound "gate4", 0, Vol(ActiveBall)*VolGates, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
End Sub

Sub Spinner_Spin
    ' PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
    PlaySoundAtVol "fx_spinner", Spinner, VolSpin
End Sub

Sub aRubberBands_Hit(idx)
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed> 20 then
        PlaySound "fx_rubber_band", 0, Vol(ActiveBall)*VolRB, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End if
    If finalspeed >= 6 AND finalspeed <= 20 then
        RandomSoundRubber()
    End If
End Sub

Sub aRubberPosts_Hit(idx)
    dim finalspeed
    finalspeed = SQR(activeball.velx * activeball.velx + activeball.vely * activeball.vely)
    If finalspeed> 16 then
        PlaySound "fx_postrubber", 0, Vol(ActiveBall)*VolPo, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End if
    If finalspeed >= 6 AND finalspeed <= 16 then
        RandomSoundRubber()
    End If
End Sub

Sub RandomSoundRubber()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "fx_rubber_hit_1", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 2:PlaySound "fx_rubber_hit_2", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 3:PlaySound "fx_rubber_hit_3", 0, Vol(ActiveBall)*VolRH, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End Select
End Sub

Sub LeftFlipper_Collide(parm)
    RandomSoundFlipper()
End Sub

Sub RightFlipper_Collide(parm)
    RandomSoundFlipper()
End Sub

Sub RandomSoundFlipper()
    Select Case Int(Rnd * 3) + 1
        Case 1:PlaySound "fx_flip_hit_1", 0, Vol(ActiveBall)*VolFlip, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 2:PlaySound "fx_flip_hit_2", 0, Vol(ActiveBall)*VolFlip, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
        Case 3:PlaySound "fx_flip_hit_3", 0, Vol(ActiveBall)*VolFlip, AudioPan(ActiveBall), 0, Pitch(ActiveBall), 1, 0, AudioFade(ActiveBall)
    End Select
End Sub

' *********************************************************************
'
'                 Random sound effects
'
' *********************************************************************

'Random Jackpot callout

Sub PlayJackpot
	Dim a
	a=INT(RND * 3)
	Select Case a
		Case 0: PlaySound"vo_jackpot1"
		Case 1: PlaySound"vo_jackpot2"
		Case 2: PlaySound"vo_jackpot3"
		Case 3: PlaySound"vo_jackpot4"
		Case 4: PlaySound"vo_jackpot5"
		Case 5: PlaySound"vo_jackpot6"
	End Select
End Sub

'Random Super Jackpot callout

Sub PlaySuperJackpot
	Dim a
	a=INT(RND * 3)
	Select Case a
		Case 0: PlaySound"vo_superjackpot1"
		Case 1: PlaySound"vo_superjackpot2"
	End Select
End Sub

' *********************************************************************
' *********************************************************************
'
'                 Table Specific Script Starts Here
'
' *********************************************************************
' *********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

' Clue Game variables

Dim GSecretsCount(4)
Dim BribesCount(4)
Dim PhotoCount(4)
Dim CluesCount(4)
Dim LeftRampHits
Dim ModesFinished(4)
Dim MyBallImage
Dim ClueLanes(4) 'number of times the Date lanes has been completed
Dim Lock1Hits(4)
Dim Lock2Hits(4)
Dim LockIsReady(4) 'to open or close the ramp at the start of the ball
Dim BumperFrenzyMultiplier
Dim gClock (24)

Dim Mode(4, 7)
Dim ModeIsReady(4)
Dim ModeHitsNeeded(4)
Dim bSearchMultiball 'this variable is True when the Margarita multiball is on.
Dim bMurderMultiball 'this variable is True when the DreamDaddy multiball is on.
Dim bCollectables    'this variable is True when the collectables are lit
Dim MurderMultiballHits
Dim FeedTheDogsHits(4)
Dim SecondsCounter
Dim BumperValue(4)
Dim CounterName

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    TurnOffPlayfieldLights()
	PuPlayer.playlistplayex pBackglass,"BG","Backglass.mp4",0,1
	PuPlayer.SetBackground pBackglass,1
    PuPlayer.LabelShowPage pFullDMD, 1, 0, ""
	PuPlayer.playlistplayex pFullDMD,"Backglass","Backglass.mp4",0,1
	PuPlayer.SetBackground pFullDMD,1
	resetbackglass
    pUpdateScores
    For i = 0 to 4
        SkillshotValue(i) = 10000 ' increases by 5000 each time it is collected
        BallsInLock(i) = 0
        ClueLanes(i) = 0
        Lock1Hits(i) = 0
        Lock2Hits(i) = 0
        ModesFinished(i) = 0
        LockIsReady(i) = 0
        ModeIsReady(i) = False
        ExtraBallIsLit(i) = 0
        ModeHitsNeeded(i) = 0
        Jackpot(i) = 5000
        GSecretsCount(i) = 0
        BribesCount(i) = 0
        PhotoCount(i) = 0
        CluesCount(i) = 0
        FeedTheDogsHits(i) = 0
        BumperValue(i) = 1000
		gClock(i)= 0
    Next
    ' reset Modes
    For i = 0 to 4
        For j = 0 to 7
            Mode(i, j) = 0
        Next
    Next
    MyBallImage = "Chrome_Ball"
    bSearchMultiball = False
    bMurderMultiball = False
	bCollectables = False
    BumperFrenzyMultiplier = 1
    SecondsCounter = 0
    MurderMultiballHits = 0
	CounterName = ""

End Sub

Sub StopEndOfBallMode()                   'this sub is called after the last ball is drained
    ResetSkillShotTimer_Timer
    If Mode(CurrentPlayer, 0)Then EndMode 'a mode is active so stop it
    RampDoorMoveDown
    SuperJackpotTimer_Timer               'stop the timers in case they were on
    FeedTheDogsTimer_Timer
	BumperFrenzyTimer_Timer
    CounterTimer.Enabled = 0              'stop the counter in case it was on
End Sub

Sub ResetNewBallVariables()               'reset variables for a new ball or player, usually shared/global variables
    BumperFrenzyMultiplier = 1
    LeftRampHits = 0
    MyBallImage = "Chrome_Ball"
    If LockIsReady(CurrentPlayer)Then
        l22.State = 1
        l26.State = 1
        l18.State = 2
        RampDoorMoveUp
    End If
End Sub

Sub ResetNewBallLights()                               'turn on or off the needed lights before a new ball is released
    TurnOffPlayfieldLights                             'turn off all the playfield lights
    StartCollectables                                  'turn on collectable lights
    UpdateModeLights                                   'turn on the completed mode lights
    l38.State = 1                                      'Enabled RED, aka. bonus multiplier lights, it is on during all the game
    l21.State = 1                                      'Enables also during the whole game
    UpdateLockLights
    If ModeIsReady(CurrentPlayer)Then l17.State = 2    'lit the mode ready light
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub UpdateSkillShot() 'Updates the skillshot light
    LightSeqSkillshot.Play SeqAllOff
    l41.State = 0
    l42.State = 2
    l43.State = 0
    CycleSkillShotTimer.Enabled = 1
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    CycleSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    LightSeqSkillshot.StopPlay
    If l41.State = 2 Then l41.State = 0
    If l42.State = 2 Then l42.State = 0
    If l43.State = 2 Then l43.State = 0
    pUpdateScores
End Sub

Sub CycleSkillShotTimer_Timer 'cycle the skillshot lights
    Dim TempState
    'top lanes
    TempState = l43.State
    l43.State = l42.State
    l42.State = l41.State
    l41.State = TempState
End Sub

' *********************************************************************
' Collectables
' *********************************************************************

Sub StartCollectables 'lights
	bCollectables = True
    l20.State = 2
    l28.State = 2
    l36.State = 2
End Sub

Sub StopCollectables
	bCollectables = False
    l20.State = 0
    l28.State = 0
    l36.State = 0
End Sub

Sub CheckCollectables                         'after 3 collectables activate the mode light, the Dad Jokes are not counted here
    ModeHitsNeeded(CurrentPlayer) = ModeHitsNeeded(CurrentPlayer) + 1
    If ModeHitsNeeded(CurrentPlayer) = 3 then 'this means the value is 5, 10, 15, etc
        If Mode(CurrentPlayer, 0) = 0 then    'only if no mode is active
            pMsg "Dad Date is lit", 3, "", 1
            l17.State = 2
            ModeIsReady(CurrentPlayer) = True
    PlaySound "vo_modelit"
        End If
    End If
End Sub

' *********************************************************************
' VUK
' *********************************************************************

Sub VUK_Hit()
    ' PlaySound "Kicker_Hole"
    PlaySoundAtVol "Kicker_Hole", VUK, VolKick
    ' flash before kick out ball
    FlashForms FlasherVUK, 1000, 50, 0
    FlashForms VUKOrange, 1000, 50, 0
	DOF 125, DOFPulse
    VUK.TimerEnabled = 1
	Playsound "fx_warning"
End Sub

Sub VUK_Timer()
    VUK.Kick 0, 40, 1.56
    ' PlaySound SoundFXDOF("Vuk_Kick",117,DOFPulse,DOFContactors)
    PlaySoundAtVol SoundFXDOF("Vuk_Kick",117,DOFPulse,DOFContactors), VUK, VolKick
	  DOF 114, DOFPulse
    PlaySound "fx_metalrolling2", -1 ' TODO
    VUK.TimerEnabled = 0
End Sub

' *********************************************************************
' Cabinet Kicker
' *********************************************************************

Sub CabinetKicker_Hit
    Dim delay
	Dim tmp
    delay = 2000
    ' PlaySound "Kicker_Hole"
    PlaySoundAtVol "Kicker_Hole", CabinetKicker, VolKick
    BallsInHole = BallsInHole + 1
    CabinetKicker.DestroyBall
    'PauseSong
    If Tilted Then vpmtimer.addTimer 200, "ModeHoleKick '":Exit Sub
    If l36.state = 2 then
        PhotoCount(CurrentPlayer) = PhotoCount(CurrentPlayer) + 1
        pMsg PhotoCount(CurrentPlayer) & " Dad Naps", 2, "", 1
        Addscore 2500
        CheckCollectables
    End If
    If l35.State = 2 Then
        AwardJackpot
'Play Random margarita clips
        PuPlayer.playlistplayex pBackglass, "Search", "", 300, 1
        l35.State = 0:CheckSearchSuperJackpot:CheckMurderMultiball
    End If
    If l34.State = 2 Then 'give the superjackpot and start the normal jackpot lights
        AwardSuperJackpot
        l34.State = 0
        CounterTimer.Enabled = 0 'stop the counter at the dmd
        StartSearchLights
    End If
    If l37.State = 2 Then ' collect weapon
        AddScore 60000
        l37.State = 0
        l29.State = 2 'motive light
        Select Case Mode(CurrentPlayer, 0)
            'show a video and set the delay according to the duration of the video.
            Case 1:delay = 9000:PuPlayer.playlistplayex pFullDMD, "Weapons", "Candlestick.mp4", 100, 9
            Case 2:delay = 9000:PuPlayer.playlistplayex pFullDMD, "Weapons", "Knife.mp4", 100, 9
            Case 3:delay = 9000:PuPlayer.playlistplayex pFullDMD, "Weapons", "Wrench.mp4", 100, 9
            Case 4:delay = 9000:PuPlayer.playlistplayex pFullDMD, "Weapons", "Rope.mp4", 100, 9
            Case 5:delay = 9000:PuPlayer.playlistplayex pFullDMD, "Weapons", "Pipe.mp4", 100, 9
            Case 6:delay = 9000:PuPlayer.playlistplayex pFullDMD, "Weapons", "Revolver.mp4", 100, 9
            Case 7:delay = 9000:PuPlayer.playlistplayex pFullDMD, "Weapons", "Revolver.mp4", 100, 9
        End Select
    End If
    If l51.State = 2 Then 'Pickup the Kid! light
        tmp = 100000 + formatnumber(Jackpot(CurrentPlayer),0)
        pMsg2Lines "Great! You Remembered your KID!",  "" & formatnumber((tmp),0), 3, "", 1
        l51.State = 0
        CounterTimer.Enabled = 0 'stop the count at the dmd
        ' now maybe change the delay and show a video at this point
        delay = 4000
    End If
    vpmtimer.addTimer delay, "ModeHoleKick '"
    'vpmtimer.addTimer delay - 1000, "ResumeSong '"
End Sub

' *********************************************************************
' Mode Kicker
' *********************************************************************

Sub ModeKicker_Hit
    Dim delay
    delay = 500
    ' PlaySound "Kicker_Hole"
    PlaySoundAtVol "Kicker_Hole", ModeKicker, VolKick
    BallsInHole = BallsInHole + 1
    ModeKicker.DestroyBall
    If Tilted Then vpmtimer.addTimer 200, "ModeHoleKick '":Exit Sub 'if tilted just kick out the ball fast

    If l15.State = 2 Then                                           'award the extra ball
        AwardExtraBall
    End If
    If l16.State = 2 Then
        AwardJackpot
        FlashForms FlasherModeHoleGreen, 1000, 50, 0
        FlashForms ModeGreen, 1000, 50, 0
		DOF 126, DOFPulse
	'Play Random margarita clips
        PuPlayer.playlistplayex pBackglass, "Search", "", 300, 1
        l16.State = 0:CheckSearchSuperJackpot:CheckMurderMultiball
    End If
    If l52.State = 2 Then 'mystery light is lit
        l52.State = 0
        GiveRandomAward
        Delay = 7000 'kick out the ball after showing the random award
    End If
    ' Start mode if light is blinking and no mode is active
    If(ModeIsReady(CurrentPlayer))AND(Mode(CurrentPlayer, 0) = 0)and bMultiballMode = FALSE Then
        ModeIsReady(CurrentPLayer) = False
        l17.State = 0 ' stop the mode light and select a mode
        SelectMode    'the start of the mode will kick out the ball after a delay
    Else
        vpmtimer.addTimer delay, "ModeHoleKick '"
    End If
End Sub

Sub ModeHoleKick
    ' flash before kick out ball
    FlashForms FlasherModeHole, 1000, 50, 0
    FlashForms ModeOrange, 1000, 50, 0
	DOF 128, DOFPulse
    vpmtimer.addTimer 1000, "ModeHoleKick2 '"
	Playsound "fx_warning"

End Sub

Sub ModeHoleKick2 'part 2: ejecting the balls
    If BallsInHole> 0 Then
        ModeKicker.CreateBall.image = MyBallImage
        ' PlaySound SoundFXDOF("popper_ball",118,DOFPulse,DOFContactors)
        PlaySoundAtVol SoundFXDOF("popper_ball",118,DOFPulse,DOFContactors), ModeKicker, VolKick
		DOF 114, DOFPulse
        ModeKicker.kick 150, 10
        BallsInHole = BallsInHole - 1
    End If
    If BallsInHole> 0 Then
        vpmtimer.addTimer 500, "ModeHoleKick '"
    End If
End Sub

'****************
' Mystery award
'****************
Sub GiveRandomAward()
Dim tmp, tmp2
playsound "fx_count"
' show some random values on the dmd
vpmtimer.addtimer 1200, "pMsg2Lines ""MYSTERY AWARD"", ""BIG POINTS"", 1, """", 0 '"
vpmtimer.addtimer 1300, "pMsg2Lines ""MYSTERY AWARD"", ""LOCK IS LIT"", 1, """", 0 '"
vpmtimer.addtimer 1400, "pMsg2Lines ""MYSTERY AWARD"", ""INCREASE BUMPER VALUE"", 1, """", 0 '"
vpmtimer.addtimer 1500, "pMsg2Lines ""MYSTERY AWARD"", ""SMALL POINTS"", 1, """", 0 '"
vpmtimer.addtimer 1600, "pMsg2Lines ""MYSTERY AWARD"", ""EXTRA BALL IS LIT"", 1, """", 0 '"
vpmtimer.addtimer 1700, "pMsg2Lines ""MYSTERY AWARD"", ""INCREASE BONUS X"", 1, """", 0 '"
vpmtimer.addtimer 1800, "pMsg2Lines ""MYSTERY AWARD"", ""LOCK IS LIT"", 1, """", 0 '"
vpmtimer.addtimer 1900, "pMsg2Lines ""MYSTERY AWARD"", ""BIG POINTS"", 1, """", 0 '"
vpmtimer.addtimer 2000, "pMsg2Lines ""MYSTERY AWARD"", ""LOCK IS LIT"", 1, """", 0 '"
vpmtimer.addtimer 2100, "pMsg2Lines ""MYSTERY AWARD"", ""INCREASE BUMPER VALUE"", 1, """", 0 '"
vpmtimer.addtimer 2200, "pMsg2Lines ""MYSTERY AWARD"", ""SMALL POINTS"", 1, """", 0 '"
vpmtimer.addtimer 2300, "pMsg2Lines ""MYSTERY AWARD"", ""EXTRA BALL IS LIT"", 1, """", 0 '"
vpmtimer.addtimer 2400, "pMsg2Lines ""MYSTERY AWARD"", ""BONUS X"", 1, """", 0 '"
vpmtimer.addtimer 2500, "pMsg2Lines ""MYSTERY AWARD"", ""LOCK IS LIT"", 1, """", 0 '"
vpmtimer.addtimer 2600, "pMsg2Lines ""MYSTERY AWARD"", ""BALL SAVE"", 1, """", 0 '"
vpmtimer.addtimer 2700, "pMsg2Lines ""MYSTERY AWARD"", ""ADD A BALL"", 1, """", 0 '"
vpmtimer.addtimer 2800, "pMsg2Lines ""MYSTERY AWARD"", "" "", 1, """", 0 '"

'vpmtimer.addtimer 3000, "PlaySound ""fx_fanfare1"" '"

tmp = INT(RND(1) * 80)
Select Case tmp
Case 0, 1, 2, 3, 4, 5, 6
vpmtimer.addtimer 3000, "pMsg2Lines ""MYSTERY AWARD"", ""EXTRA BALL IS LIT"", 4, """", 1 '"
l15.State = 2
ExtraBallIsLit(CurrentPlayer) = 1
Case 10, 11, 12, 13, 14
vpmtimer.addtimer 3000, "pMsg2Lines ""MYSTERY AWARD"", ""BIG POINTS 300,000"", 4, """", 1 '"
AddScore 300000
Case 21, 22, 23
vpmtimer.addtimer 3000, "pMsg2Lines ""MYSTERY AWARD"", ""INCREASE BONUS X"", 4, """", 1 '"
IncreaseBonusMultiplier
Case 30, 31, 32
vpmtimer.addtimer 3000, "pMsg2Lines ""MYSTERY AWARD"", ""LOCK IS LIT"", 4, """", 1 '"
l18.State = 2
RampDoorMoveUp
LockIsReady(CurrentPlayer) = 1
Case 40, 41, 42
vpmtimer.addtimer 3000, "pMsg2Lines ""MYSTERY AWARD"", ""INCREASE BUMPER VALUE"", 4, """", 1 '"
BumperValue(CurrentPlayer) = BumperValue(CurrentPlayer) + 50
'Case 50, 51, 52
'vpmtimer.addtimer 3000, "pMsg2Lines ""MYSTERY AWARD"", ""ADD A BALL"", 4, """", 1 '"
'vpmtimer.addTimer 3000, "AddMultiball 1 '"
Case 60, 61, 62, 63, 64
vpmtimer.addtimer 3000, "pMsg2Lines ""MYSTERY AWARD"", ""BALL SAVE"", 4, """", 1 '"
EnableBallSaver 26
Case ELSE 'small points 1000 to 5000
tmp2 = INT((RND) * 5) * 1000 + 1000
vpmtimer.addtimer 3000, "pMsg2Lines ""MYSTERY AWARD"", ""SMALL POINTS"", 4, """", 1 '"
AddScore tmp2
End Select
End Sub

' *********************************************************************
' Secret Passage (Fireplace)
' *********************************************************************

'VerticalDoor animates, the animation is done in the "game timer"

Sub Spinner1_Spin
    ' PlaySound "Spinner2"
    PlaySoundAtVol "Spinner2", Spinner1, VolSpin
	DOF 121, DOFPulse
End Sub

Sub SecretPassageKicker_Hit
    BallsInHole = BallsInHole + 1
    SecretPassageKicker.DestroyBall
    SecretPassageKicker.Enabled = 0
    ' PlaySound "Kicker_Hole"
	pMsg "", 1, "", 0
    PlaySoundAtVol "Kicker_Hole", SecretPassageKicker, VolKick
    l40.State = 0
    If Tilted Then vpmtimer.addTimer 200, "ModeHoleKick '":Exit Sub

    ' Win the mode
    WinMode
'the Winmode routine will kick the ball in play
End Sub

Sub SecretPassageTrigger_Hit
    If Tilted Then Exit Sub
    If l39.State = 2 Then
        AwardJackpot
	'Play margarita clips
        PuPlayer.playlistplayex pBackglass, "Search", "", 300, 1
        l39.State = 0:CheckSearchSuperJackpot:CheckMurderMultiball
    End If
End Sub

' *********************************************************************
' Grandfather clock
' *********************************************************************

Sub Clock_hit
	Dim delay, i

	' PlaySound "Clock"
	PlaySoundAtVol "Clock", Clock, VolClock
	Addscore 1000
	gClock(CurrentPlayer) = gClock(CurrentPlayer) + 1
        ' check to see if player can get the extra ball
        If gClock(CurrentPlayer) = 24 Then
			Addscore 12000 
            pMsg "Extra ball is lit", 3, "", 1
            PlaySound "vo_extraballislit"
			PlaySound "clockrock"
			PlaySound "12chime"
            l15.State = 2
            ExtraBallIsLit(CurrentPlayer) = 1
        End If
		
	
'
'	If gClock(CurrentPlayer) = 24 Then
	
'	 gClock(CurrentPlayer) = 0

'		EndMode

'		StopSong
	
'	PlaySound "clockrock"		
	
'	PuPlayer.playlistplayex pFullDMD, "Multiball", "Murder.mp4", 100, 3
	
'			delay = 10000
	
'			StartMurderMultiball delay
	
'	End If


End Sub

' *********************************************************************
' Left Ramp Animation
' *********************************************************************

Dim RampStep, RampDir
RampStep = 0

Sub RampDoorMoveUp
    If RampDoor.HeightBottom = 0 Then 'only start the animation if the ramp is down
        RampDir = 3
        RampDoorTimer.Enabled = 1
        ' PlaySound SoundFXDOF("trap_door",138,DOFOn,DOFGear)
        PlaySoundAtVol SoundFXDOF("trap_door",138,DOFOn,DOFGear), RampKicker, 1
        RampDoorInVisible.Collidable = 0
        light1.state = 2
		l20.State = 0 'turn off the govt secrets lights
    End If
End Sub

Sub RampDoorMoveDown
    If RampDoor.HeightBottom = 120 Then 'only start the animation if the ramp is up
        RampDir = -3
        RampDoorTimer.Enabled = 1
        ' PlaySound SoundFXDOF("trap_door",138,DOFOn,DOFGear)
        PlaySoundAtVol SoundFXDOF("trap_door",138,DOFOn,DOFGear), RampKicker, 1
        RampDoorInVisible.Collidable = 1
        light1.state = 0
		' restore the govt secrets lights
		If bCollectables Then
			l20.State = 2
		End If
    End If
End Sub

Sub RampDoorTimer_Timer
    RampStep = RampStep + RampDir
    If RampStep <0 Then
        RampStep = 0
        RampDoorTimer.Enabled = 0
		DOF 138, DOFOff
    End If
    If RampStep> 120 Then
        RampStep = 120
        RampDoorTimer.Enabled = 0
		DOF 138, DOFOff
    End If
    RampDoor.HeightBottom = RampStep
End Sub

'***********************************
'The Ramp lock
'***********************************

Sub RampKicker_Hit 'LOCK hole
    Dim delay
    delay = 2000   'standard lock delay after locking a ball
    BallsInHole = BallsInHole + 1
    RampKicker.DestroyBall
    ' PlaySound "Kicker_Hole"
    PlaySoundAtVol "Kicker_Hole", RampKicker, VolKick
    If Tilted Then vpmtimer.addTimer 200, "ModeHoleKick '":Exit Sub
    RampDoorMoveDown
    If Mode(CurrentPlayer, 0)Then 'if a mode is active means the KEY is completed then turn on the collect weapon light and turn off the KEY lights
        l37.State = 2
        l23.State = 0
        l24.State = 0
        l25.State = 0
        pMsg "DADDY DATE TIME!", 6, "", 1
    Else ' normal ball lock
        BallsInLock(CurrentPLayer) = BallsInLock(CurrentPLayer) + 1
        Select Case BallsInLock(CurrentPlayer)
            Case 1:Playsound "vo_ball1lock":pMsg "Ball 1 locked", 2, "", 1
            Case 2:Playsound "vo_ball2lock":pMsg "Ball 2 locked", 2, "", 1
            Case 3:StopSong:Playsound "vo_ball3lock":pMsg "Ball 3 locked", 2, "", 1
                PuPlayer.playlistplayex pFullDMD, "Multiball", "SearchTheHouse.mp4", 100, 3
                delay = 9000 'delay for clip above
                vpmtimer.addTimer delay, "StartLockMultiball '"
        End Select
        ResetLockLights
    End If
    'kick the ball in play
    vpmtimer.addTimer delay, "ModeHoleKick '"
End Sub

'***********************************
'Margarita Multiball (3 Ball)
'***********************************
' all jackpots are lit during this multiball

Sub StartLockMultiball
    'play some sound/ change music
    PlaySong "Theme_3"
    pMsg "Soul Searching w/ Margaritas!", 8, "", 1
    'setup lights ++
    StartSearchLights
    StopCollectables
    'stop the mode light if it was lit
    l17.State = 0
    ' Reset variables
    BallsInLock(CurrentPLayer) = 0
    ' start ball saver
    EnableBallSaver 25
    'kick out 2 multiballs
    AddMultiball 2
	DOF 127, DOFPulse
    bSearchMultiball = True
End Sub

Sub StartSearchLights
    Gate4.Open = True
    l16.State = 2
    l19.State = 2
    l35.State = 2
    l39.State = 2
    l27.State = 2
End Sub

Sub StopSearchLights
    Gate4.Open = False
    l16.State = 0
    l19.State = 0
    l35.State = 0
    l39.State = 0
    l27.State = 0
End Sub

Sub StartRandomSearchLights
    Dim tmp
    tmp = INT(RND * 5)
    StopSearchLights
    Select Case tmp
        Case 1:l16.State = 2
        Case 1:l19.State = 2
        Case 1:l35.State = 2
        Case 1:l39.State = 2
        Case 1:l27.State = 2
    End Select
End Sub

Sub CheckSearchSuperJackpot
    ' check for the superjackpot if running the Seach Multiballs
    If bSearchMultiball Then
        If(l16.State + l19.State + l35.State + l39.State + l27.State) = 0 Then
            pMsg "Super Jackpot is lit", 3, "", 1
            l34.State = 2
            SuperJackpotTimer.Enabled = 1
			CounterName="Super Jackpot - HURRY UP! "
			PlaySound "vo_superjackpotlit"
            vpmtimer.addtimer 3000, "StartCounter 20 '"
        End If
    End If
End Sub

Sub SuperJackpotTimer_Timer
    ' the superjackpot timer has ended so turn off the light and resume
    SuperJackpotTimer.Enabled = 0
    l34.State = 0
    If bSearchMultiball Then ' if still in the mode then turn on the search lights
        StartSearchLights
    End If
    If bMurderMultiball Then
        MurderMultiballTimer_Timer       'turn on one light
        MurderMultiballTimer.Enabled = 1 'enabled to change the light after 10 seconds
    End If
End Sub

'****************************************************
' DMD counter, can be used whenever we need a counter
' use it like this: StartCounter seconds
'****************************************************

Sub StartCounter(seconds)
    SecondsCounter = Seconds
    CounterTimer.Enabled = 1
End Sub

Sub CounterTimer_Timer
    pMsg CounterName & SecondsCounter, 1, "", 0
    SecondsCounter = SecondsCounter -1
    If SecondsCounter = 0 Then
        CounterTimer.Enabled = 0
    End if
End Sub

' *********************************************************************
' Joke Trigger
' *********************************************************************

Sub ClueTrigger_hit
LastSwitchHit = "cluetrigger"
End Sub

Sub ClueTrigger2_hit
if LastSwitchHit = "cluetrigger" Then
pMsg2Lines "EPIC DAD JOKE", "10,000 pts", 3, "", 1
AddScore 10000
Else
end If
end sub

' *********************************************************************
' Bumpers
' *********************************************************************

Sub Bumper1_Hit
    If Tilted Then Exit Sub
    ' PlaySound SoundFXDOF("fx_bumper4",106,DOFPulse,DOFContactors)
    PlaySoundAtVol SoundFXDOF("fx_bumper4",106,DOFPulse,DOFContactors), Bumper1, VolBump
	DOF 111, DOFPulse
    bumperlight1.Duration 1, 100, 0
    AddScore BumperValue(CurrentPlayer)
	LastSwitchHit = "Bumper1"
    If l21.state = 1 then
        CheckClues
    End If
End Sub

Sub Bumper2_Hit
    If Tilted Then Exit Sub
    ' PlaySound SoundFXDOF("fx_bumper2",105,DOFPulse,DOFContactors)
    PlaySoundAtVol SoundFXDOF("fx_bumper2",105,DOFPulse,DOFContactors), Bumper2, VolBump
    DOF 110, DOFPulse
    bumperlight2.Duration 1, 100, 0
    AddScore BumperValue(CurrentPlayer)
	LastSwitchHit = "Bumper2"
    If l21.state = 1 then
        CheckClues
    End If
End Sub

Sub Bumper3_Hit
    If Tilted Then Exit Sub
    ' PlaySound SoundFXDOF("fx_bumper4",107,DOFPulse,DOFContactors)
    PlaySoundAtVol SoundFXDOF("fx_bumper4",107,DOFPulse,DOFContactors), Bumper3, VolBump
	DOF 112, DOFPulse
    bumperlight3.Duration 1, 100, 0
    AddScore BumperValue(CurrentPlayer)
	LastSwitchHit = "Bumper3"
    If l21.state = 1 then
        CheckClues
    End If
End Sub

' bumper frenzy after 100 hits
' increases the joke and start the bumperfrenzy mode for 20 seconds with all the shots scoring double
' increases the bumper value by 50 points after each 50 hits
Sub CheckClues
    CluesCount(CurrentPlayer) = CluesCount(CurrentPlayer) + 1
	pMsg CluesCount(CurrentPlayer) & " Dad Jokes Collected", 1, "", 1
    If CluesCount(CurrentPlayer)MOD 100 = 0 Then 'this means if the CluesCount(CurrentPlayer) = 100 or 200 or 300...
        BumperFrenzyMultiplier = 2
        BumperFrenzyTimer.Enabled = 1
		CounterName = "EPIC JOKE FRENZY - 2X Scoring "
		PlaySound "gun_shot"
		StartCounter 30
        LightSeqClueFrenzy.UpdateInterval = 4
		LightSeqClueFrenzy.Play SeqRandom, 8, , 1000
    End If
    If CluesCount(CurrentPlayer)MOD 50 = 0 Then 'this means if the CluesCount(CurrentPlayer) = 50, 100, 150...
        BumperValue(CurrentPlayer) = BumperValue(CurrentPlayer) + 500
    End If
End Sub

'turns off the BumperFrenzy Multiplier

Sub BumperFrenzyTimer_Timer
    BumperFrenzyTimer.Enabled = 0
    BumperFrenzyMultiplier = 1
	LightSeqClueFrenzy.StopPlay
End Sub

' Restart the bumper sequence if it is finished

Sub LightSeqClueFrenzy_PlayDone()
    LightSeqClueFrenzy.Play SeqRandom, 8, , 1000
End Sub


' *********************************************************************
' Right 4 Standup Targets
' *********************************************************************

Sub sw1_Hit
    PlaySoundAtVol SoundFXDOF("fx_target",115,DOFPulse,DOFTargets), sw1, VolTarg
    If Tilted Then Exit Sub
    sw1.TimerEnabled = 1
    PlaySoundAtVol "gun_shot", sw1, VolTarg
    AddScore 5000
    Primitive3.ObjRotz = 220
    DOF 136, DOFPulse
    If l33.State = 0 then
        l33.State = 1
        CheckJoke
    end if
End Sub

Sub sw1_timer
    Primitive3.ObjRotz = 240
End Sub

Sub sw2_Hit
    PlaySoundAtVol SoundFXDOF("fx_target",115,DOFPulse,DOFTargets), sw2, VolTarg
    If Tilted Then Exit Sub
    sw2.TimerEnabled = 1
    PlaySoundAtVol "gun_shot", sw2, VolTarg
    AddScore 5000
    Primitive3.ObjRotz = 220
    DOF 136, DOFPulse
    If l32.State = 0 then
        l32.State = 1
        CheckJoke
    end if
End Sub

Sub sw2_timer
    Primitive3.ObjRotz = 240
End Sub

Sub sw3_Hit
    PlaySoundAtVol SoundFXDOF("fx_target",115,DOFPulse,DOFTargets), sw3, VolTarg
    If Tilted Then Exit Sub
    sw3.TimerEnabled = 1
    PlaySoundAtVol "gun_shot", sw3, VolTarg
    AddScore 5000
    Primitive3.ObjRotz = 220
    DOF 136, DOFPulse
    If l31.State = 0 then
        l31.State = 1
        CheckJoke
    end if
End Sub

Sub sw3_timer
    Primitive3.ObjRotz = 240
End Sub

Sub sw4_Hit
    PlaySoundAtVol SoundFXDOF("fx_target",115,DOFPulse,DOFTargets), sw4, VolTarg
    If Tilted Then Exit Sub
    sw4.TimerEnabled = 1
    PlaySoundAtVol "gun_shot", sw4, VolTarg
    AddScore 5000
    Primitive3.ObjRotz = 220
    DOF 136, DOFPulse
    If l30.State = 0 then
        l30.State = 1
        CheckJoke
    end if
End Sub

Sub sw4_timer
    Primitive3.ObjRotz = 240
End Sub

Sub CheckJoke
    If(l30.State + l31.State + l32.State + l33.State = 4)Then
        AddScore 25000
		PuPlayer.playlistplayex pFullDMD, "Jokes", "", 300, 1
        pMsg "MYSTERY IS LIT", 4, "", 1
		l52.State = 2
		LightEffect 2
        ResetJoke
    End If
End Sub

Sub ResetJoke
    l33.State = 0
    l32.State = 0
    l31.State = 0
    l30.State = 0
End Sub

Sub RotateJokeLights
    Dim TempState
    TempState = l30.State
    l30.State = l31.State
    l31.State = l32.State
    l32.State = l33.State
    l33.State = TempState
End Sub


' *********************************************************************
' Lock Targets
' *********************************************************************

Sub LockTarget1_Hit
    PlaySoundAtVol SoundFXDOF("fx_target",141,DOFPulse,DOFTargets), LockTarget1, VolTarg
    If Tilted Then Exit Sub
    Addscore 2000
    If bMultiBallMode OR Mode(CurrentPlayer, 0)Then Exit Sub 'do nothing if multiball mode is on or a Mode is on
    Lock1Hits(CurrentPlayer) = Lock1Hits(CurrentPlayer) + 1
    Select Case BallsInLock(CurrentPLayer)
        Case 0
            l22.State = 1:CheckLockLights
        Case 1
            Select Case Lock1Hits(CurrentPlayer)
                Case 1:l22.BlinkInterval = 200:l22.State = 2
                Case 2:l22.State = 1:CheckLockLights
            End Select
        Case 2
            Select Case Lock1Hits(CurrentPlayer)
                Case 1:l22.BlinkInterval = 200:l22.State = 2
                Case 2:l22.BlinkInterval = 100:l22.State = 2
                Case 3:l22.State = 1:CheckLockLights
            End Select
    End Select
End Sub

Sub LockTarget2_Hit
    PlaySoundAtVol SoundFXDOF("fx_target",141,DOFPulse,DOFTargets), LockTarget2, VolTarg
    If Tilted Then Exit Sub
    Addscore 2000
    If bMultiBallMode OR Mode(CurrentPlayer, 0)Then Exit Sub 'do nothing if multiball mode is on
    Lock2Hits(CurrentPlayer) = Lock2Hits(CurrentPlayer) + 1
    Select Case BallsInLock(CurrentPLayer)
        Case 0
            l26.State = 1:CheckLockLights
        Case 1
            Select Case Lock2Hits(CurrentPlayer)
                Case 1:l26.BlinkInterval = 200:l26.State = 2
                Case 2:l26.State = 1:CheckLockLights
            End Select
        Case 2
            Select Case Lock2Hits(CurrentPlayer)
                Case 1:l26.BlinkInterval = 200:l26.State = 2
                Case 2:l26.BlinkInterval = 100:l26.State = 2
                Case 3:l26.State = 1:CheckLockLights
            End Select
    End Select
End Sub

Sub CheckLockLights
    If(l22.State = 1)AND(l26.State = 1)Then
        AddScore 10000
        PlaySound "vo_lockislit" 
        l18.State = 2
        RampDoorMoveUp
        LockIsReady(CurrentPlayer) = 1
    End If
End Sub

Sub ResetLockLights
    l22.State = 0
    l26.State = 0
    l18.State = 0
    Lock1Hits(CurrentPlayer) = 0
    Lock2Hits(CurrentPlayer) = 0
    LockIsReady(CurrentPlayer) = 0
End Sub

Sub UpdateLockLights
    l22.State = 0
    l26.State = 0
    Select Case BallsInLock(CurrentPLayer)
        Case 1
            Select Case Lock1Hits(CurrentPlayer)
                Case 1:l22.BlinkInterval = 200:l22.State = 2
                Case Else:l22.State = 1
            End Select
        Case 2
            Select Case Lock1Hits(CurrentPlayer)
                Case 1:l22.BlinkInterval = 200:l22.State = 2
                Case 2:l22.BlinkInterval = 100:l22.State = 2
                Case Else:l22.State = 1
            End Select
    End Select
    Select Case BallsInLock(CurrentPLayer)
        Case 1
            Select Case Lock2Hits(CurrentPlayer)
                Case 1:l26.BlinkInterval = 200:l26.State = 2
                Case Else:l26.State = 1
            End Select
        Case 2
            Select Case Lock2Hits(CurrentPlayer)
                Case 1:l26.BlinkInterval = 200:l26.State = 2
                Case 2:l26.BlinkInterval = 100:l26.State = 2
                Case Else:l26.State = 1
            End Select
    End Select
End Sub

' *********************************************************************
' Wire Ramp Sounds
' *********************************************************************

Sub RightRampTrigger_Hit
    PlaySound "fx_metalrolling2", -1 ' TODO
    AddScore 15000
End Sub

Sub LeftRampTrigger_Hit
    PlaySound "fx_metalrolling2", -1 ' TODO
    AddScore 15000
End Sub

Sub RightRampKicker_Hit
    StopSound "fx_metalrolling2"
    PlaySound "fx_ballrampdrop"
End Sub

Sub LeftRampKicker_Hit
    StopSound "fx_metalrolling2"
    PlaySound "fx_ballrampdrop"
End Sub

' *********************************************************************
' Top Lane Rollovers
' *********************************************************************

Sub swR_Hit
    ' PlaySound "fx_sensor"
    PlaySoundAtVol "fx_sensor", swR, VolSens
    If Tilted Then Exit Sub
    DOF 122, DOFPulse
    If bskillshotReady Then
        If l41.state = 2 Then
            AwardSkillShot
        Else
            ResetSkillShotTimer_Timer
        End If
    End If

    If l38.state = 1 then
        l41.State = 1
        CheckRed
        AddScore 3000
    End If
End Sub

Sub swE_Hit
    ' PlaySound "fx_sensor"
    PlaySoundAtVol "fx_sensor", swE, VolSens
    If Tilted Then Exit Sub
    DOF 123, DOFPulse
    If bskillshotReady Then
        If l42.state = 2 Then
            AwardSkillShot
        Else
            ResetSkillShotTimer_Timer
        End If
    End If

    If l38.state = 1 then
        l42.State = 1
        CheckRed
        AddScore 3000
    End If
End Sub

Sub swD_Hit
    ' PlaySound "fx_sensor"
    PlaySoundAtVol "fx_sensor", swD, VolSens
    If Tilted Then Exit Sub
    DOF 124, DOFPulse
    If bskillshotReady Then
        If l43.state = 2 Then
            AwardSkillShot
        Else
            ResetSkillShotTimer_Timer
        End If
    End If

    If l38.state = 1 then
        l43.State = 1
        CheckRed
        AddScore 3000
    End If
End Sub

Sub CheckRed
    If(l41.State + l42.State + l43.State = 3)Then
        PuPlayer.playlistplayex pFullDMD, "Pop", "", 100, 1
        AddScore 12000
        ResetRed
        'Increase the bonus multiplier
        IncreaseBonusmultiplier
    End If
End Sub

Sub ResetRed
    l41.State = 0
    l42.State = 0
    l43.State = 0
End Sub

' *********************************************************************
' Lower Lane Rollovers: date letters
' *********************************************************************

Sub LeftOutlane_Hit
	DOF 142, DOFPulse
    ' PlaySound "fx_sensor"
    PlaySoundAtVol "fx_sensor", LeftOutLane, VolSens
    If Tilted Then Exit Sub
    If l1.State = 0 then
        l1.State = 1
        CheckLanes
        AddScore 1000
    end if
End Sub

Sub LeftInlane_Hit
	DOF 143, DOFPulse
    ' PlaySound "fx_sensor"
    PlaySoundAtVol "fx_sensor", LeftInLane, VolSens
    If Tilted Then Exit Sub
    If l2.State = 0 then
        l2.State = 1
        CheckLanes
        AddScore 1000
    end if
End Sub

Sub RightOutlane_Hit
	DOF 145, DOFPulse
    ' PlaySound "fx_sensor"
    PlaySoundAtVol "fx_sensor", RightOutlane, VolSens
    If Tilted Then Exit Sub
    If l4.State = 0 then
        l4.State = 1
        CheckLanes
        AddScore 3000
    end if
End Sub

Sub RightInlane_Hit
	DOF 144, DOFPulse
    ' PlaySound "fx_sensor"
    PlaySoundAtVol "fx_sensor", RightInlane, VolSens
    If Tilted Then Exit Sub
    If l3.State = 0 then
        l3.State = 1
        CheckLanes
        AddScore 3000
    end if
End Sub

Sub CheckLanes
Dim ClueDifficulty, DogDifficulty
	If LaneDifficulty = 1 Then
	ClueDifficulty = 10
	DogDifficulty = 3
	End If
	If LaneDifficulty = 2 Then
	ClueDifficulty = 16
	DogDifficulty = 5
	End If
    If(l1.State + l2.State + l3.State + l4.State = 4)Then
        PuPlayer.playlistplayex pFullDMD, "Random", "", 100, 1
        ResetLanes
        AddScore 50000
        LightEffect 2
        ClueLanes(CurrentPlayer) = ClueLanes(CurrentPlayer) + 1
        ' check to see if player can get the extra ball
        If ClueLanes(CurrentPlayer) = ClueDifficulty Then
            ClueLanes(CurrentPlayer) = 0
            pMsg "Extra ball is lit", 3, "", 1
            PlaySound "vo_extraballislit"
            l15.State = 2
            ExtraBallIsLit(CurrentPlayer) = 1
        End If
        FeedTheDogsHits(CurrentPlayer) = FeedTheDogsHits(CurrentPlayer) + 1
        If FeedTheDogsHits(CurrentPlayer) = DogDifficulty Then
            
            l51.State = 2
            FeedTheDogsHits(CurrentPlayer) = 0
            FeedTheDogsTimer.Enabled = 1
			CounterName = "Pickup KID!"
            vpmtimer.addtimer 3000, "StartCounter 30 '"
        End If
    End If
End Sub

Sub FeedTheDogsTimer_Timer 'turn off the light when the time is up
    FeedTheDogsTimer.Enabled = 0
    l51.State = 0
End Sub

Sub ResetLanes
    l1.State = 0
    l2.State = 0
    l3.State = 0
    l4.State = 0
End Sub

' *********************************************************************
' Left Ramp Actions
' *********************************************************************

Sub LeftRampDone_Hit
    If Tilted Then Exit Sub
    LeftRampHits = LeftRampHits + 1
    LightEffect 8
    If l20.state = 2 then 'Blue
		DOF 129, DOFPulse
        FlashForms FlasherLRBlue, 600, 50, 0
        FlashForms LRBlue, 600, 50, 0
        FlashForms LRBlueUp, 600, 50, 0
        GSecretsCount(CurrentPlayer) = GSecretsCount(CurrentPlayer) + 1
        pMsg GSecretsCount(CurrentPlayer) & " Movie Lines Quoted", 2, "", 1
        CheckCollectables
    End If
    If l19.State = 2 Then 'Green
        AwardJackpot
		DOF 130, DOFPulse
        FlashForms FlasherLRGreen, 600, 50, 0
        FlashForms LRGreenUp, 600, 50, 0
        'Play Random margarita clips
        PuPlayer.playlistplayex pBackglass, "Search", "", 300, 1
        l19.State = 0:CheckSearchSuperJackpot:CheckMurderMultiball
    End If
    If l25.State = 2 Then 'Third key is collected "Y"
        PuPlayer.playlistplayex pFullDMD, "Key", "3Key.mp4", 100, 3
        pMsg "LEAVE THE HOUSE!", 6, "", 1
        AddScore 45000
		DOF 131, DOFPulse
        FlashForms FlasherLRYellow, 600, 50, 0
        FlashForms LRYellowUp, 600, 50, 0
        l23.State = 1
        l24.State = 1
        l25.State = 1
        RampDoorMoveUp
    End If
    If l24.State = 2 Then 'second key is collected "E"
        PuPlayer.playlistplayex pFullDMD, "Key", "2Key.mp4", 100, 3
        AddScore 30000
		DOF 131, DOFPulse
        FlashForms FlasherLRYellow, 600, 50, 0
        FlashForms LRYellowUp, 600, 50, 0
        l24.State = 1
        l25.State = 2     'enable next key
    End If
    If l23.State = 2 Then 'first key is collected "K"
        PuPlayer.playlistplayex pFullDMD, "Key", "1Key.mp4", 100, 3
        AddScore 15000
		DOF 131, DOFPulse
        FlashForms FlasherLRYellow, 600, 50, 0
        FlashForms LRYellowUp, 600, 50, 0
        l23.State = 1
        l24.State = 2 'enable next key
    End If
End Sub

' *********************************************************************
' Right Ramp Actions
' *********************************************************************

Sub RightRampDone_Hit
    If Tilted Then Exit Sub
    'FlashForms FlasherRightWhite, 1000, 50, 0
    If l28.state = 2 then
		DOF 132, DOFPulse
        FlashForms FlasherRRBlue, 600, 50, 0
        FlashForms RRBlue, 600, 50, 0
        FlashForms RRBlueUp, 600, 50, 0
        BribesCount(CurrentPlayer) = BribesCount(CurrentPlayer) + 1
        pMsg BribesCount(CurrentPlayer) & " Tools Returned", 2, "", 1
        CheckCollectables
    End If
    If l27.State = 2 Then
        AwardJackpot
		DOF 133, DOFPulse
        FlashForms FlasherRRGreen, 600, 50, 0
        FlashForms RRGreen, 600, 50, 0
        FlashForms RRGreenUp, 600, 50, 0
'Play Random margarita clips
        PuPlayer.playlistplayex pBackglass, "Search", "", 300, 1
        l27.State = 0:CheckSearchSuperJackpot:CheckMurderMultiball
    End If
    If l29.State = 2 Then ' collect Beer
        pMsg "Daddy Time!", 6, "", 1
        AddScore 75000
		DOF 134, DOFPulse
        FlashForms FlasherRRYellow, 600, 50, 0
        FlashForms RRYellow, 600, 50, 0
        FlashForms RRYellowUp, 600, 50, 0
        l29.State = 0
        l40.State = 2                   'kill victim light
        SecretPassageKicker.Enabled = 1 'and enable the hole
    'show clip ?
    End If
End Sub

' *********************************************************************
' MODES
' *********************************************************************

' Mode(CurrentPlayer,0) will have the active mode number
' when a mode is not active Mode(CurrentPlayer,n) = 0
' when a mode is active Mode(CurrentPlayer,n) = 2
' when a mode is completed Mode(CurrentPlayer,n) = 1
' this is to be consistent with the light states

Sub SelectMode 'Select a random mode
    Dim tmp
    LightSeqModeLights.Play SeqRandom, 7, , 2000
    If ModesFinished(CurrentPlayer) <6 Then
        tmp = 1 + INT(RND(1) * 6)
        SelectMode2(tmp)
    Else
        Mode(CurrentPlayer, 0) = 7
    End If
    vpmtimer.AddTimer 2000, "StartMode '"
End Sub

Sub SelectMode2(tmp)
    IF Mode(CurrentPlayer, tmp) = 0 Then 'not started
        Mode(CurrentPlayer, 0) = tmp
    Else
        tmp = tmp + 1
        If tmp> 6 Then
            tmp = 1
        End If
        SelectMode2(tmp)
    End If
End Sub

Sub StartMode 'starts the mode selected
    Dim delay:delay = 2000
    StopSong
    Select Case Mode(CurrentPlayer, 0)
        Case 1 'DOF Purple color
            Mode(CurrentPlayer, 1) = 2
            'pMsg "MATT DATE ", 3, "", 1
		GiEffect2 1
            PuPlayer.playlistplayex pFullDMD, "Victim", "IntroBoddy.mp4", 100, 3
            delay = 11000
            MyBallImage = "Plum_Ball"
            ChangeGi purple
        Case 2 'DOF Blue color
            Mode(CurrentPlayer, 2) = 2
            'pMsg "ROBERT DATE ", 3, "", 1
		GiEffect2 1
            PuPlayer.playlistplayex pFullDMD, "Victim", "IntroCook.mp4", 100, 3
            delay = 11000
            MyBallImage = "Peacock_Ball"
            ChangeGi Blue
        Case 3 'DOF Orange Color
            Mode(CurrentPlayer, 3) = 2
            'pMsg "CRAIG DATE COMPLETED", 3, "", 1
		GiEffect2 1
            PuPlayer.playlistplayex pFullDMD, "Victim", "IntroMotorist.mp4", 100, 3
            delay = 11000
            MyBallImage = "Mustard_Ball"
            ChangeGi Amber
        Case 4 'DOF White color
            Mode(CurrentPlayer, 4) = 2
            'pMsg "JOSEPH DATE COMPLETED", 3, "", 1
		GiEffect2 1
            PuPlayer.playlistplayex pFullDMD, "Victim", "IntroMaid.mp4", 100, 3
            delay = 11000
            MyBallImage = "White_Ball"
            ChangeGi White2
        Case 5 'DOF Red color
            Mode(CurrentPlayer, 5) = 2
            'pMsg "BRIAN DATE COMPLETED", 3, "", 1
		GiEffect2 1
            PuPlayer.playlistplayex pFullDMD, "Victim", "IntroBrian.mp4", 100, 3
            delay = 11000
            MyBallImage = "Scarlet_Ball"
            ChangeGi Red
        Case 6 'DOF Black (dark grey?) color
            Mode(CurrentPlayer, 6) = 2
            'pMsg "DAMIEN DATE COMPLETED", 3, "", 1
		GiEffect2 1
            PuPlayer.playlistplayex pFullDMD, "Victim", "IntroTelegram.mp4", 100, 3
            delay = 11000
            MyBallImage = "Wadsworth_Ball"
            ChangeGi Grey
        Case 7 'DOF Green Color
            Mode(CurrentPlayer, 7) = 2
            'pMsg "HUGO DATE COMPLETED", 3, "", 1
		GiEffect2 1
            PuPlayer.playlistplayex pFullDMD, "Victim", "IntroWadsworth.mp4", 100, 3
            delay = 11000
            MyBallImage = "Green_Ball"
            ChangeGi Green
    End Select
    vpmtimer.addTimer delay -100, "PlaySong ""Theme_2"" '"
    UpdateModeLights
    RampDoorMoveDown 'close the ramp door if open
    'turn off the lock lights but do not reset the lock status/hits
    l22.State = 0
    l26.State = 0
    l18.State = 0
    StopCollectables
	'PuPlayer.playlistplayex pFullDMD,"BG","Backglass2.mp4",0,1
    ModeHitsNeeded(CurrentPlayer) = 0
    l23.State = 2 'Start the first key
    vpmtimer.addTimer delay, "ModeHoleKick '"
End Sub

Sub EndMode 'mostly at the end of the ball
    Select Case Mode(CurrentPlayer, 0)
        Case 1:Mode(CurrentPlayer, 1) = 0
        Case 2:Mode(CurrentPlayer, 2) = 0
        Case 3:Mode(CurrentPlayer, 3) = 0
        Case 4:Mode(CurrentPlayer, 4) = 0
        Case 5:Mode(CurrentPlayer, 5) = 0
        Case 6:Mode(CurrentPlayer, 6) = 0
        Case 7:Mode(CurrentPlayer, 7) = 0
    End Select
    Mode(CurrentPlayer, 0) = 0
    UpdateModeLights
    'turn off all other Mode lights
    l23.State = 0
    l24.State = 0
    l25.State = 0
    l37.State = 0
    l40.State = 0
    l29.State = 0
    'disable the kicker in case it was enabled
    SecretPassageKicker.Enabled = 0
End Sub

Sub WinMode
    Dim delay
    delay = 2000
    LightEffect 2
    ResetGi white
    'DOF Strobe
    StopSong
    'disable the kicker in case it was enabled
    SecretPassageKicker.Enabled = 0
    Select Case Mode(CurrentPlayer, 0)
        Case 1
            Mode(CurrentPlayer, 1) = 1
            pMsg "MATT DATE COMPLETED", 3, "", 1
            'show a video and set the delay
            PuPlayer.playlistplayex pFullDMD, "Kill", "KillBoddy.mp4", 100, 3
            delay = 9000
			AddScore 100000
        Case 2
            Mode(CurrentPlayer, 2) = 1
            pMsg "ROBERT DATE COMPLETED", 3, "", 1
            'show a video and set the delay
            PuPlayer.playlistplayex pFullDMD, "Kill", "KillCook.mp4", 100, 3
            delay = 13000
			AddScore 100000
        Case 3
            Mode(CurrentPlayer, 3) = 1
            pMsg "CRAIG DATE COMPLETED", 3, "", 1
            'show a video and set the delay
            PuPlayer.playlistplayex pFullDMD, "Kill", "KillMotorist.mp4", 100, 3
            delay = 14000
			AddScore 100000
        Case 4
            Mode(CurrentPlayer, 4) = 1
            pMsg "JOSEPH DATE COMPLETED", 3, "", 1
            'show a video and set the delay
            PuPlayer.playlistplayex pFullDMD, "Kill", "KillMaid.mp4", 100, 3
            delay = 9000
			AddScore 100000
        Case 5
            Mode(CurrentPlayer, 5) = 1
            pMsg "BRIAN DATE COMPLETED", 3, "", 1
            'show a video and set the delay
            PuPlayer.playlistplayex pFullDMD, "Kill", "KillBrian.mp4", 100, 3
            delay = 10000
			AddScore 100000
        Case 6
            Mode(CurrentPlayer, 6) = 1
            pMsg "DAMIEN DATE COMPLETED", 3, "", 1
            'show a video and set the delay
            PuPlayer.playlistplayex pFullDMD, "Kill", "KillTelegram.mp4", 100, 3
            delay = 11000
			AddScore 100000
        Case 7
            Mode(CurrentPlayer, 7) = 1
            pMsg "HUGO DATE COMPLETED", 3, "", 1
            'show a video and set the delay
            PuPlayer.playlistplayex pFullDMD, "Kill", "KillWadsworth.mp4", 100, 3
            delay = 12000
			AddScore 1000000
    End Select
    Mode(CurrentPlayer, 0) = 0
    ModesFinished(CurrentPlayer) = ModesFinished(CurrentPlayer) + 1
    UpdateModeLights
    'turn off all other Mode lights if anyone was still on
    l23.State = 0
    l24.State = 0
    l25.State = 0
    l37.State = 0
    l40.State = 0
    l29.State = 0
    'check if the lock is ready
    If LockIsReady(CurrentPlayer)Then
        l22.State = 1
        l26.State = 1
        l18.State = 2
        RampDoorMoveUp
    End If
    ' wait fot the video to end and go to the step 2
    vpmtimer.addTimer delay, "WinMode2 '"
End Sub

Sub WinMode2 'Starts after video delay
    Dim delay, i
    delay = 2000
    StartCollectables
    'multiball 3 balls after 3 completed modes
    If ModesFinished(CurrentPlayer) = 3 Then
        PuPlayer.playlistplayex pFullDMD, "Multiball", "Murder.mp4", 100, 3
        delay = 7000
        StartMurderMultiball delay
    Else
        ' multiball 6 balls ater the last mode,  and reset of modes
        If ModesFinished(CurrentPlayer) = 7 Then
            For i = 0 to 7
                Mode(CurrentPlayer, i) = 0
            Next
            ModesFinished(CurrentPlayer) = 0
            PuPlayer.playlistplayex pFullDMD, "Multiball", "TakeThemAway.mp4", 100, 3
            Delay = 22000
            StartTakeemawayMultiball delay
        Else
            ' Play the main theme song
            PlaySong "Theme_1"
            UpdateLockLights
        End If
    End If
    ' continue and kick out the ball
    MyBallImage = "Chrome_Ball"
    vpmtimer.addTimer delay, "ModeHoleKick '"
End Sub

Sub UpdateModeLights
    l12.State = Mode(CurrentPlayer, 1)
    l8.State = Mode(CurrentPlayer, 2)
    l10.State = Mode(CurrentPlayer, 3)
    l13.State = Mode(CurrentPlayer, 4)
    l11.State = Mode(CurrentPlayer, 5)
    l14.State = Mode(CurrentPlayer, 6)
    l9.State = Mode(CurrentPlayer, 7)
End Sub

Sub StartMurderMultiball(delay)
    pMsg "Hit Lit Targets", 8, "", 1
    StopCollectables 'they will be started after the multiball
	DOF 127, DOFPulse
    bMurderMultiball = True
    MurderMultiballHits = 0
    EnableBallSaver 15 + (delay / 1000)
    vpmtimer.addTimer delay + 1000, "AddMultiball 2 '"
    MurderMultiballTimer_Timer
    MurderMultiballTimer.Enabled = 1
	'PuPlayer.playlistplayex pFullDMD,"BG","Backglass2.mp4",0,1
End Sub

Sub MurderMultiballTimer_Timer
    Dim tmp
    StopSearchLights
    tmp = INT(RND * 5)
    Select Case tmp
        Case 0:l16.State = 2
        Case 1:l19.State = 2
        Case 2:l35.State = 2
        Case 3:l39.State = 2
        Case 4:l27.State = 2
    End Select
End Sub

Sub CheckMurderMultiball
    ' check for the superjackpot if running the Murder Multiball
    If bMurderMultiball Then
        MurderMultiballHits = MurderMultiballHits + 1
        If MurderMultiballHits = 5 Then
            pMsg "Super Jackpot is lit", 3, "", 1
            l34.State = 2
            SuperJackpotTimer.Enabled = 1
			CounterName = "Super Jackpot - HURRY UP! "
			PlaySound "vo_superjackpotlit"
            vpmtimer.addtimer 3000, "StartCounter 20 '"
            MurderMultiballTimer.Enabled = 0
            StopSearchLights 'in case the timer had lit another jackpot
        End If
    End If
End Sub

Sub StartTakeemawayMultiball(delay) 'similar to the lock multiball but with 6 balls
    pMsg "Hit Lit Targets", 8, "", 1
	DOF 127, DOFPulse
    bSearchMultiball = True         'same rules as with the search multiball
    EnableBallSaver 15 + (delay / 1000)
    StartSearchLights
    StopCollectables 'they will be started after the multiball
    vpmtimer.addTimer delay + 1000, "AddMultiball 5 '"
End Sub

