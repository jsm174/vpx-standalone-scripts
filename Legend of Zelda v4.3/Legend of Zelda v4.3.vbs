' ****************************************************************
'                     VISUAL PINBALL X 7.2
'                  The Legend of Zelda v4.3.1
' ****************************************************************

Option Explicit
Randomize

Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1     ' standard ball mass in JP's VPX Physics 3.0.1
Const SongVolume = 0.1 ' 1 is full volume, but I set it quite low to listen better the other sounds since I use headphones, adjust to your setup :)

'FlexDMD in high or normal quality
'change it to True if you have an LCD screen, 256x64
'or keep it False if you have a real DMD at 128x32 in size
Const FlexDMDHighQuality = False

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

' Define any Constants

Const cGameName = "zof10"
' Define any Constants
Const TableName = "ZELDA"
Const myVersion = "4.3.1"
Const MaxPlayers = 4
Const BallSaverTime = 15 'in seconds
Const MaxMultiplier = 7  '7x is the max in this game
Const BallsPerGame = 3
Const MaxMultiballs = 6

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If

' Define Global Variables
Dim AttractMode
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusMultiplier(4)
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Jackpot
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim mBalls2Eject
Dim x

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
Dim bAutoPlunger
Dim bSkillshotReady

'Define This Table objects and variables

Dim plungerIM, plungerIM2, cbLeft
Dim CBonus

Dim bExtraBallWonThisBall
Dim MusicChannelInUse
Dim CurrentMusicTunePlaying

Dim NCount
Dim R1Count
Dim R2Count
Dim K1Count
Dim K2Count
Dim cbCount
Dim K6Count
Dim P4Count
Dim P2Count

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
    Randomize

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 42 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swPlunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFX("fx_solenoid", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
        .CreateEvents "plungerIM"
    End With

    'Impulse Plunger as autoplunger
    Const IMPowerSetting2 = 42 ' Plunger Power
    Const IMTime2 = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM2 = New cvpmImpulseP
    With plungerIM2
        .InitImpulseP swPlunger2, IMPowerSetting2, IMTime2
        .Random 1.5
        .InitExitSnd SoundFX("fx_solenoid", DOFContactors), SoundFX("fx_solenoid", DOFContactors)
        .CreateEvents "plungerIM2"
    End With

    ' Captive Balls
    Set cbLeft = New cvpmCaptiveBall
    With cbLeft
        .InitCaptive CapTrigger, CapWall, Array(CapKicker1, CapKicker2), 10
        .NailedBalls = 1
        .ForceTrans = .95
        .MinForce = 3.5
        '.CreateEvents "cbLeft" 'the events are done later in the script to add the hit sound
        .Start
    End With
    CapKicker1.CreateBall

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    'load saved values, highscore, names, jackpot
    Credits = 1
    Loadhs

    'Init main variables
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

    ' Initalise the DMD display
    DMD_Init

    ' freeplay or coins
    bFreePlay = FALSE 'we want coins

    ' Setup the lightning according to the nightday slider
    'if table1.nightday <50 Then
    '    for each i in aGiTopLights:i.intensity = i.intensity + (100 - table1.nightday) / 10:next
    '    for each i in aGiBottomLights:i.intensity = i.intensity + (100 - table1.nightday) / 10:next
    '    for each i in aFlashers:i.intensity = i.intensity + (100 - table1.nightday) / 10:next
    'End If

    ' initialse any other flags
    bOnTheFirstBall = FALSE
    bBallInPlungerLane = FALSE
    bBallSaverActive = FALSE
    bBallSaverReady = FALSE
    bMultiBallMode = FALSE
    bGameInPlay = FALSE
    bMusicOn = TRUE 'TRUE
    bAutoPlunger = FALSE
    BallsOnPlayfield = 0
    BallsInLock = 0
    BallsInHole = 0
    LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = FALSE
    ChangeGi "Normal"
    EndOfGame()
    Realtime.Enabled = 1
    LoadLUT
End Sub

'********************
' Real Time updates
'********************
'used for all the real time updates

Sub Realtime_Timer
    RollingUpdate
' add any other real time update subs, like gates or diverters, flippers
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    If hsbModeActive Then EnterHighScoreKey(keycode):Exit Sub

    If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT:End If

    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        If(Tilted = FALSE)Then
            PlaySoundAt "fx_coin", drain
            DMDFlush
            DMD "", CL("CREDITS " & Credits), "_", eNone, eNone, eNone, 500, TRUE, ""
            If NOT bGameInPlay Then ShowTableInfo
        End If
    End If

    If keycode = PlungerKey Then
        PlaySoundAt "fx_plungerpull", Plunger
        Plunger.Pullback
    End If

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 1.6:PlaySound SoundFX("fx_nudge", 0), 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 1.6:PlaySound SoundFX("fx_nudge", 0), 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 2.8:PlaySound SoundFX("fx_nudge", 0), 0, 1, 1, 0.25:CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = TRUE))Then

                If(bFreePlay = TRUE)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush
                        DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 500, TRUE, ""
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = TRUE)Then
                    If(BallsOnPlayfield = 0)Then
                        ResetForNewGame()
                    End If
                Else
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            ResetForNewGame()
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMDFlush
                        DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 500, TRUE, ""
                        ShowTableInfo
                    End If
                End If
            End If
    End If ' If (GameInPlay)

' Table specific

' test keys

End Sub

Sub Table1_KeyUp(ByVal keycode)
    If hsbModeActive Then EnterHighScoreKey(keycode):Exit Sub

    If keycode = LeftMagnaSave Then bLutActive = False: HideLUT

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then SolLFlipper 0
        If keycode = RightFlipperKey Then SolRFlipper 0
    End If

    If keycode = PlungerKey Then
        PlaySoundAt "fx_plunger", Plunger
        Plunger.Fire
    End If

' test keys
End Sub

'************************************
'       LUT - Darkness control
' 10 normal level & 10 warmer levels 
'************************************

Dim bLutActive, LUTImage

Sub LoadLUT
    bLutActive = False
    x = LoadValue(cGameName, "LUTImage")
    If(x <> "")Then LUTImage = x Else LUTImage = 0
    UpdateLUT
End Sub

Sub SaveLUT
    SaveValue cGameName, "LUTImage", LUTImage
End Sub

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 22:UpdateLUT:SaveLUT:SetLUTLine "Color LUT image " & table1.ColorGradeImage:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0"
        Case 1:table1.ColorGradeImage = "LUT1"
        Case 2:table1.ColorGradeImage = "LUT2"
        Case 3:table1.ColorGradeImage = "LUT3"
        Case 4:table1.ColorGradeImage = "LUT4"
        Case 5:table1.ColorGradeImage = "LUT5"
        Case 6:table1.ColorGradeImage = "LUT6"
        Case 7:table1.ColorGradeImage = "LUT7"
        Case 8:table1.ColorGradeImage = "LUT8"
        Case 9:table1.ColorGradeImage = "LUT9"
        Case 10:table1.ColorGradeImage = "LUT10"
        Case 11:table1.ColorGradeImage = "LUT Warm 0"
        Case 12:table1.ColorGradeImage = "LUT Warm 1"
        Case 13:table1.ColorGradeImage = "LUT Warm 2"
        Case 14:table1.ColorGradeImage = "LUT Warm 3"
        Case 15:table1.ColorGradeImage = "LUT Warm 4"
        Case 16:table1.ColorGradeImage = "LUT Warm 5"
        Case 17:table1.ColorGradeImage = "LUT Warm 6"
        Case 18:table1.ColorGradeImage = "LUT Warm 7"
        Case 19:table1.ColorGradeImage = "LUT Warm 8"
        Case 20:table1.ColorGradeImage = "LUT Warm 9"
        Case 21:table1.ColorGradeImage = "LUT Warm 10"
    End Select
End Sub

Dim GiIntensity
GiIntensity = 1   'can be used by the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

' New LUT postit
Function GetHSChar(String, Index)
    Dim ThisChar
    Dim FileName
    ThisChar = Mid(String, Index, 1)
    FileName = "PostIt"
    If ThisChar = " " or ThisChar = "" then
        FileName = FileName & "BL"
    ElseIf ThisChar = "<" then
        FileName = FileName & "LT"
    ElseIf ThisChar = "_" then
        FileName = FileName & "SP"
    Else
        FileName = FileName & ThisChar
    End If
    GetHSChar = FileName
End Function

Sub SetLUTLine(String)
    Dim Index
    Dim xFor
    Index = 1
    LUBack.imagea="PostItNote"
    For xFor = 1 to 40
        Eval("LU" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub HideLUT
SetLUTLine ""
LUBack.imagea="PostitBL"
End Sub

'*************
' Pause Table
'*************

Sub table1_Paused
    If Isobject(Controller)Then Controller.Pause = True
End Sub

Sub table1_unPaused
    If Isobject(Controller)Then Controller.Pause = False
End Sub

Sub table1_Exit
    If Isobject(Controller)Then Controller.Pause = False:Controller.Stop
    Savehs
End Sub

'********************
' Special JP Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipper1.RotateToEnd
        LeftFlipperOn = 1
        DOF dFlipperLeft, 1
        RotateLaneLightsLeft
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
        LeftFlipper1.RotateToStart
        LeftFlipperOn = 0
        DOF dFlipperLeft, 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFX("fx_flipperup", DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipper1.RotateToEnd
        RightFlipperOn = 1
        DOF dFlipperRight, 1
        RotateLaneLightsRight
    Else
        PlaySoundAt SoundFX("fx_flipperdown", DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipper1.RotateToStart
        RightFlipperOn = 0
        DOF dFlipperRight, 0
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub LeftFlipper1_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper1_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 60, pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'*********************************************************
' Real Time Flipper adjustments - by JLouLouLou & JPSalas
'        (to enable flipper tricks) 
'*********************************************************

Dim FlipperPower
Dim FlipperElasticity
Dim SOSTorque, SOSAngle
Dim FullStrokeEOS_Torque, LiveStrokeEOS_Torque
Dim LeftFlipperOn
Dim RightFlipperOn

Dim LLiveCatchTimer
Dim RLiveCatchTimer
Dim LiveCatchSensivity

FlipperPower = 5000
FlipperElasticity = 0.8
FullStrokeEOS_Torque = 0.3 	' EOS Torque when flipper hold up ( EOS Coil is fully charged. Ampere increase due to flipper can't move or when it pushed back when "On". EOS Coil have more power )
LiveStrokeEOS_Torque = 0.2	' EOS Torque when flipper rotate to end ( When flipper move, EOS coil have less Ampere due to flipper can freely move. EOS Coil have less power )

LeftFlipper.EOSTorqueAngle = 10
RightFlipper.EOSTorqueAngle = 10

SOSTorque = 0.1
SOSAngle = 6

LiveCatchSensivity = 10

LLiveCatchTimer = 0
RLiveCatchTimer = 0

LeftFlipper.TimerInterval = 1
LeftFlipper.TimerEnabled = 1

Sub LeftFlipper_Timer 'flipper's tricks timer
'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If LeftFlipper.CurrentAngle >= LeftFlipper.StartAngle - SOSAngle Then LeftFlipper.Strength = FlipperPower * SOSTorque else LeftFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
	If LeftFlipperOn = 1 Then
		If LeftFlipper.CurrentAngle = LeftFlipper.EndAngle then
			LeftFlipper.EOSTorque = FullStrokeEOS_Torque
			LLiveCatchTimer = LLiveCatchTimer + 1
			If LLiveCatchTimer < LiveCatchSensivity Then
				LeftFlipper.Elasticity = 0
			Else
				LeftFlipper.Elasticity = FlipperElasticity
				LLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		LeftFlipper.Elasticity = FlipperElasticity
		LeftFlipper.EOSTorque = LiveStrokeEOS_Torque
		LLiveCatchTimer = 0
	End If
	

'Start Of Stroke Flipper Stroke Routine : Start of Stroke for Tap pass and Tap shoot
    If RightFlipper.CurrentAngle <= RightFlipper.StartAngle + SOSAngle Then RightFlipper.Strength = FlipperPower * SOSTorque else RightFlipper.Strength = FlipperPower : End If
 
'End Of Stroke Routine : Livecatch and Emply/Full-Charged EOS
 	If RightFlipperOn = 1 Then
		If RightFlipper.CurrentAngle = RightFlipper.EndAngle Then
			RightFlipper.EOSTorque = FullStrokeEOS_Torque
			RLiveCatchTimer = RLiveCatchTimer + 1
			If RLiveCatchTimer < LiveCatchSensivity Then
				RightFlipper.Elasticity = 0
			Else
				RightFlipper.Elasticity = FlipperElasticity
				RLiveCatchTimer = LiveCatchSensivity
			End If
		End If
	Else
		RightFlipper.Elasticity = FlipperElasticity
		RightFlipper.EOSTorque = LiveStrokeEOS_Torque
		RLiveCatchTimer = 0
	End If
End Sub

Sub RotateLaneLightsLeft
    Dim TempState
    TempState = LightLeftOutlane.State
    LightLeftOutlane.State = LightLeftInlane1.State
    LightLeftInlane1.State = LightLeftInlane.State
    LightLeftInlane.State = LightRightInlane.State
    LightRightInlane.State = LightRightOutlane.State
    LightRightOutlane.State = TempState
End Sub

Sub RotateLaneLightsRight
    Dim TempState
    TempState = LightRightOutlane.State
    LightRightOutlane.State = LightRightInlane.State
    LightRightInlane.State = LightLeftInlane.State
    LightLeftInlane.State = LightLeftInlane1.State
    LightLeftInlane1.State = LightLeftOutlane.State
    LightLeftOutlane.State = TempState
End SUb

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt 'Called when table is nudged
    Dim BOT
    BOT = GetBalls
    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub
    Tilt = Tilt + TiltSensitivity                 'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt > TiltSensitivity)AND(Tilt <= 15)Then 'show a warning
        DMD "", CL("CAREFUL!"), "_", eNone, eBlinkFast, eNone, 1000, True, ""
    End if
    If(NOT Tilted)AND Tilt > 15 Then 'If more that 15 then TILT the table
        'display Tilt
        DMDFlush
        PlaySound "z_tilt"
        DMD CL("TILT!"), "", "", eNone, eNone, eNone, 200, FALSE, ""
        DMD "", CL("TILT!"), "", eNone, eNone, eNone, 200, FALSE, ""
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        Tilted = True
        'turn off GI and turn off all the lights
        GiOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        LeftFlipper1.RotateToStart
        RightFlipper1.RotateToStart
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
        BallSaverTimerExpired_Timer
        TimerA1.Enabled = 0
        CastleDiverter.TimerEnabled = 0
    Else
        'turn back on GI and the lights
        Tilted = False
        GiOn
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0)Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        vpmtimer.Addtimer 2000, "EndOfBall() '"
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'********************
' Music using sounds
'********************

Dim Song
Song = ""

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            PlaySound Song, -1, SongVolume
        End If
    End If
End Sub

Sub StopSong
    StopSound Song
    Song = ""
End Sub

'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************

Dim OldGiState
OldGiState = -1  'start witht the Gi off

Sub ChangeGi(Gi) 'changes the gi color
    Dim gilight
    Select Case Gi
        Case "Normal" 'orange warm color
            For each giLight in aGITopLights
                giLight.color = RGB(255, 197, 143)
                giLight.colorfull = RGB(193, 91, 0)
            Next
            For each giLight in aGIBottomLights
                giLight.color = RGB(25, 19, 14)
                giLight.colorfull = RGB(255, 197, 143)
            Next
            ChangeGIIntensity 1
        Case "Green"
            For each giLight in aGITopLights
                giLight.color = RGB(0, 18, 0)
                giLight.colorfull = RGB(0, 180, 0)
            Next
            For each giLight in aGIBottomLights
                giLight.color = RGB(0, 18, 0)
                giLight.colorfull = RGB(0, 180, 0)
            Next
            ChangeGIIntensity 1.5
        Case "Red"
            For each giLight in aGITopLights
                giLight.color = RGB(18, 0, 0)
                giLight.colorfull = RGB(180, 0, 0)
            Next
            For each giLight in aGIBottomLights
                giLight.color = RGB(18, 0, 0)
                giLight.colorfull = RGB(180, 0, 0)
            Next
            ChangeGIIntensity 1.6
    End Select
End Sub

Sub GIUpdate 'called from the gametimer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = 1 Then ' 1: since we have 2 captive balls and 1 for the car animation, then Ubound will show 2, so no balls on the table then turn off gi
            GiOff
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    Dim bulb
    PlaySound "fx_gion"
    For each bulb in aGiTopLights
        bulb.State = 1
    Next
    For each bulb in aGiBottomLights
        bulb.State = 1
    Next
    Bulb1.State = 1:Bulb2.State = 1
End Sub

Sub GiOff
    Dim bulb
    PlaySound "fx_gioff"
    For each bulb in aGiTopLights
        bulb.State = 0
    Next
    For each bulb in aGiBottomLights
        bulb.State = 0
    Next
    Bulb1.State = 0:Bulb2.State = 0
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 40
            LightSeqGi.Play SeqBlinking, , 15, 25
        Case 2 'random
            LightSeqGi.UpdateInterval = 25
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 20
            LightSeqGi.Play SeqBlinking, , 10, 20
        Case 4 'seq up
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqUpOn, 25, 3
        Case 5 'seq down
            LightSeqGi.UpdateInterval = 3
            LightSeqGi.Play SeqDownOn, 25, 3
    End Select
End Sub

'***************************************************************
'             Supporting Ball & Sound Functions v4.0
'***************************************************************

Dim TableWidth, TableHeight

TableWidth = Table1.width
TableHeight = Table1.height

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / TableWidth-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
    Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
    BallVel = (SQR((ball.VelX ^2) + (ball.VelY ^2)))
End Function

Function AudioFade(ball) 'only on VPX 10.4 and newer
    Dim tmp
    tmp = ball.y * 2 / TableHeight-1
    If tmp > 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.2, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, Pitch(ActiveBall) * 10, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function

'***********************************************
'   JP's VP10 Rolling Sounds + Ballshadow v4.0
'   uses a collection of shadows, aBallShadow
'***********************************************

Const tnob = 19   'total number of balls
Const lob = 2     'number of locked balls
Const maxvel = 34 'max ball velocity
ReDim rolling(tnob)
InitRolling

Sub InitRolling
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate()
    Dim BOT, b, ballpitch, ballvol, speedfactorx, speedfactory
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
        aBallShadow(b).Y = 3000
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y
        aBallShadow(b).Height = BOT(b).Z - Ballsize / 2

        If BallVel(BOT(b)) > 1 Then
            If BOT(b).z < 30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b)), 0, ballpitch, 1, 0, AudioFade(BOT(b))
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If

        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ < -1 and BOT(b).z < 55 and BOT(b).z > 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If

        ' jps ball speed control
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory < 1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactory
                BOT(b).VelY = BOT(b).VelY * speedfactory
            End If
        End If
    Next
End Sub

'*****************************
' Ball 2 Ball Collision Sound
'*****************************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound("fx_collide"), 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

'*********************************
' Diverse Collection Hit Sounds
'*********************************

Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
Sub aMetalWires_Hit(idx):PlaySoundAtBall "fx_MetalWire":End Sub
Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
Sub aRubber_LongBands_Hit(idx):PlaySoundAtBall "fx_rubber_longband":End Sub
Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
Sub aRubber_Pegs_Hit(idx):PlaySoundAtBall "fx_rubber_peg":End Sub
Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = TRUE

    'resets the score display, and turn off attrack mode
    StopAttractMode
    GiOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = TRUE
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
    Next

    ' initialise any other flags
    bMultiBallMode = FALSE
    Tilt = 0

    ' initialise Game variables
    zelda_Init()

    ' you may wish to start some music, play a sound, do whatever at this point

    ' set up the start delay to handle any Start of Game Attract Sequence
    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to   the start of a game to allow any attract sequence to
' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))
'
Sub ResetForNewPlayerBall()
    ' make sure the correct display is upto date
    DMDScoreNow

    ' set the current players bonus multiplier back down to 1X
    SetBonusMultiplier 1

    ' reset any drop targets, lights, game modes etc..
    LightShootAgain.State = 0
    CBonus = 0
    bExtraBallWonThisBall = FALSE
    TurnOffPlayfieldLights()

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'and the skillshot
    bSkillShotReady = True
End Sub

' Create a new ball on the Playfield
'
Sub CreateNewBall()
    ' create a ball in the plunger lane kicker.
    BallRelease.Createball

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    PlaySoundAt SoundFX("fx_Ballrel", DOFContactors), Ballrelease
    BallRelease.Kick 90, 4
    DOF dBallRelease, 2

    Popup1.Isdropped = 0
    Popup2.Isdropped = 0
    Popup3.Isdropped = 0
    towerrightlight.State = 0
    towerleftlight.State = 0

    DOF dCastleGate, 2
    If(LightG1.State = 1)Then
        LightA1.State = 0
        LightA2.State = 0
        LightA3.State = 0
        LightA4.State = 0
        LightA5.State = 0
        LightG1.State = 0
        TimerA1.Enabled = TRUE
    Else
        TimerA1.Enabled = TRUE
    End If
    ' if there is 2 or more balls then set the multibal flag
    If BallsOnPlayfield > 1 Then
        bMultiBallMode = TRUE
        bAutoPlunger = True
        DOF dMultiBall, 2
        ChangeGi "Green"
    Else
        ChangeGi "Normal"
        LightSeqGI.StopPlay
    End If
End Sub

' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table
Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
    'and eject the first ball
    CreateMultiballTimer_Timer
End Sub

Sub CreateMultiballTimer_Timer()
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        If BallsOnPlayfield < MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
                CreateMultiballTimer.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            CreateMultiballTimer.Enabled = False
        End If
    End If
End Sub

' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded
'
Sub EndOfBall()
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = FALSE
    PlaySong "m_attract"
    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)
    If(Tilted = FALSE)Then
        Dim AwardPoints

        ' add in any bonus points (multipled by the bunus multiplier)
        AwardPoints = BonusPoints(CurrentPlayer) * BonusMultiplier(CurrentPlayer)
        AddScore AwardPoints
        'AddDebugText "Bonus Points = " & AwardPoints

        DMD "_", CL("BONUS " & BonusPoints(CurrentPlayer) & " X" & BonusMultiplier(CurrentPlayer)), "", eNone, eBlink, eNone, 3000, TRUE, ""

        ' add a bit of a delay to allow for the bonus points to be added up and start the end of ball part 2
        vpmtimer.addtimer 3000, "EndOfBall2 '"
    Else
        ' we were tilted, reset the internal tilted flag and warnings
        Tilted = FALSE
        Tilt = 0
        DisableTable FALSE 'enable again bumpers and slingshots
        vpmtimer.addtimer 100, "EndOfBall2 '"
    End If
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the currentplayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    Tilted = FALSE:DisableTable FALSE

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0)Then

        'AddDebugText "Extra Ball"
        DMD "_", CL("EXTRA BALL"), "_", eNone, eBlink, eNone, 1000, TRUE, ""

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0)Then

            'AddDebugText "No More Balls, High Score Entry"

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

    'AddDebugText "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame > 1)Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer > PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'AddDebugText "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode

        ' set the machine into game over mode
        EndOfGame()

    ' you may wish to put a Game Over message on the

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is upto date
        AddScore 0

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    ' just ended your game then play the end of game tune
    If bGameInPlay Then
        StopSong
        DMD "", CL("GAME OVER"), "", eNone, eBlink, eNone, 3000, True, "z_gameover"
        vpmtimer.addtimer 3000, "StartAttractMode '"
    Else
        StartAttractMode
    End If
    'AddDebugText "End Of Game"
    bGameInPLay = FALSE

    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all modes - eject locked balls
    TimerA1.Enabled = 0

    ' set any lights for the attract mode
    GiOff

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

    If bGameInPLay = False Then Exit Sub 'don't do anything, just delete the ball

    BallsOnPlayfield = BallsOnPlayfield - 1

    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "fx_drain", Drain

    ' if there is a game in progress AND
    If(bGameInPLay = TRUE)AND(Tilted = FALSE)Then

        ' is the ball saver active,
        If(bBallSaverActive = TRUE)Then

            ' yep, create a new ball in the shooters lane
            '   CreateNewBall
            ' we use the Addmultiball in case the multiballs are being ejected
            AddMultiball 1
            ' we kick the ball with the autoplunger
            bAutoPlunger = True
            ' you may wish to put something on a display or play a sound at this point
            If NOT bMultiBallMode Then
                DMD "_", CL("BALL SAVED"), "_", eNone, eBlinkfast, eNone, 2500, True, ""
                BallSaverTimerExpired_Timer
            End If
        Else
            ' cancel any multiball if on last ball (ie. lost all other balls)
            '
            If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = TRUE)then
                    ' not in multiball mode any more
                    bMultiBallMode = FALSE
                    ChangeGi "Normal"
                    If DoubleScore = 2 Then 'restore the double scoring multiball
                        ResetDoubleScore
                    End If
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
                    PlaySong "m_main"
                End If
            End If

            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then
                ' handle the end of ball (change player, high score entry etc..)
                EndOfBall()
                ' End Modes and timers
                ChangeGi "Normal"
                CastleDiverter.TimerEnabled = 0
            End If
        End If
    End If
End Sub

' A ball is pressing down the trigger in the shooters lane
'
Sub swPlungerRest_Hit()
    ' some sound according to the ball position
    PlaySoundAt "fx_sensor", swPlungerRest
    bBallInPlungerLane = TRUE
    ' turn on LaunchLight
    'LaunchLight.State = 2
    ' if the ball goes into the plunger lane during a multiball then activate the autoplunger
    If bMultiBallMode Then bAutoPlunger = True
    ' remember last trigger hit by the ball.
    If bAutoPlunger Then
        plungerIM.AutoFire
        DOF dAutoPlunger, 2
    End If
    LastSwitchHit = "swPlungerRest"
End Sub

' The Ball has rolled out of the Plunger Lane.  Check to see if a ball saver mechanism
' is needed AND if so fire it up.
'
Sub swPlungerRest_UnHit()
    bBallInPlungerLane = FALSE
    bAutoPlunger = FALSE
    ' turn off LaunchLight
    'LaunchLight.State = 0
    ' if there is a need for a ball saver, then start off a timer
    ' only start if it is currently not running and not in multiball, else it will reset the time period
    If(bBallSaverReady = TRUE)AND(bBallSaverActive = False)AND(bMultiBallMode = FALSE)Then
        ' AND only if the last trigger hit was the plunger wire.
        ' (ball in the shooters lane)
        If(LastSwitchHit = "swPlungerRest")Then
            'Start the skillshot if ready
            If bSkillShotReady Then
                PlaySong "m_main"
                ResetSkillShotTimer.Enabled = 1
                LightSeqTable.Play SeqUpOn, 20
                PlaySound "z_eponaride"
            End If
            EnableBallSaver BallSaverTime
        End If
    End If
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag

Sub EnableBallSaver(seconds)
    ' set our game flag
    bBallSaverActive = TRUE
    bBallSaverReady = FALSE
    ' start the timer
    ' start the timer
    BallSaverTimerExpired.Interval = 1000 * seconds
    BallSaverTimerExpired.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
End Sub

Sub BallSaverTimerExpired_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimerExpired.Enabled = False
    BallSaverSpeedUpTimer.Enabled = False 'ensure this timer is also stopped
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
    LightShootAgain.State = 0
    ' if the table uses the same lights for the extra ball or replay then turn them on if needed
    If ExtraBallsAwards(CurrentPlayer) > 0 Then
        LightShootAgain.State = 1
    End If
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
'
' Add points to the score AND update the score board

Sub AddScore(points) 'normal score routine; points x playfieldmultiplier
    If Tilted Then Exit Sub
    If bSkillshotReady Then ResetSkillShotTimer_Timer
    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points
' you may wish to check to see if the player has gotten a replay
End Sub

' Add bonus to the bonuspoints AND update the score board

Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If Tilted Then Exit Sub
    ' add the bonus to the current players bonus variable
    BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
End Sub

' Add some points to the current Jackpot.
'
Sub AddJackpot(points)
    ' Jackpots only generally increment in multiball mode AND not tilted
    ' but this doesn't have to be the case
    If(Tilted = FALSE)Then

        If(bMultiBallMode = TRUE)Then
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
    if(BonusMultiplier(CurrentPlayer) < MaxMultiplier)then
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
    If(BonusMultiplier(CurrentPlayer) = 1)Then
        LightBonus2x.State = 0
        LightBonus3x.State = 0
        LightBonus4x.State = 0
        LightBonus5x.State = 0
        LightBonus6x.State = 0
        LightBonus7x.State = 0
    Else
        ' there is a bonus, turn on all the lights upto the current level
        If(BonusMultiplier(CurrentPlayer) >= 2)Then
            If(BonusMultiplier(CurrentPlayer) >= 2)Then
                LightBonus2x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 3)Then
                LightBonus3x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 4)Then
                LightBonus4x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 5)Then
                LightBonus5x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 6)Then
                LightBonus6x.state = 1
            End If
            If(BonusMultiplier(CurrentPlayer) >= 7)Then
                LightBonus7x.state = 1
            End If
        End If
    ' etc..
    End If
End Sub

Sub IncrementCBonus(Amount)
    Dim Value
    AddBonus Amount * 1000
    CBonus = CBonus + Amount
    If(CBonus > 20)Then
        CBonus = 20
    End If
    If(Amount >= 0)Then
    End If
    LightC1.State = 0
    LightC2.State = 0
    LightC3.State = 0
    LightC4.State = 0
    LightC5.State = 0
    LightC6.State = 0
    LightC7.State = 0
    LightC8.State = 0
    LightC9.State = 0
    LightC10.State = 0
    LightC20.State = 0
    Value = CBonus
    If(Value >= 20)Then
        LightC20.BlinkPattern = "10":LightC20.BlinkInterval = 150:LightC20.State = 2
        Value = Value - 20
        LightC1.BlinkPattern = "100000100000":LightC1.BlinkInterval = 75:LightC1.State = 2
        LightC2.BlinkPattern = "010000010000":LightC2.BlinkInterval = 75:LightC2.State = 2
        LightC3.BlinkPattern = "001000001000":LightC3.BlinkInterval = 75:LightC3.State = 2
        LightC4.BlinkPattern = "000100000100":LightC4.BlinkInterval = 75:LightC4.State = 2
        LightC5.BlinkPattern = "000010000010":LightC5.BlinkInterval = 75:LightC5.State = 2
        LightC6.BlinkPattern = "000001000001":LightC6.BlinkInterval = 75:LightC6.State = 2
        LightC7.BlinkPattern = "100000100000":LightC7.BlinkInterval = 75:LightC7.State = 2
        LightC8.BlinkPattern = "010000010000":LightC8.BlinkInterval = 75:LightC8.State = 2
        LightC9.BlinkPattern = "001000001000":LightC9.BlinkInterval = 75:LightC9.State = 2
        LightC10.BlinkPattern = "000100000100":LightC10.BlinkInterval = 75:LightC10.State = 2
    End If
    If(Value >= 10)Then
        LightC10.State = 1
        Value = Value - 10
    End If
    if(Value >= 9)Then LightC9.State = 1 End If
    if(Value >= 8)Then LightC8.State = 1 End If
    if(Value >= 7)Then LightC7.State = 1 End If
    if(Value >= 6)Then LightC6.State = 1 End If
    if(Value >= 5)Then LightC5.State = 1 End If
    if(Value >= 4)Then LightC4.State = 1 End If
    if(Value >= 3)Then LightC3.State = 1 End If
    if(Value >= 2)Then LightC2.State = 1 End If
    if(Value >= 1)Then LightC1.State = 1 End If
End Sub

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        'DMD "_", CL(("EXTRA BALL WON")), "_", eNone, eBlink, eNone, 1000, TRUE, "fx_Knocker"
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = TRUE
    END If
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 2800000 End If
    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 2700000 End If
    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 2600000 End If
    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 2500000 End If
    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(TableName, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0 End If
    x = LoadValue(TableName, "Jackpot")
    If(x <> "")then Jackpot = CInt(x)Else Jackpot = 0 End If
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
    SaveValue TableName, "Jackpot", Jackpot
    SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
End Sub

Sub Reseths
    HighScore(0) = 2800000
    HighScoreName(0) = "AAA"
    HighScore(1) = 2700000
    HighScoreName(1) = "BBB"
    HighScore(2) = 2600000
    HighScoreName(2) = "CCC"
    HighScore(3) = 2500000
    HighScoreName(3) = "DDD"
    Savehs
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
    tmp = Score(CurrentPlayer)

    If tmp > HighScore(0)Then 'add 1 credit for beating the highscore
        Credits = Credits + 1
        DOF 125, DOFOn
    End If

    If tmp > HighScore(3)Then
        PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
        DOF 121, DOFPulse
        HighScore(3) = tmp
        'enter player's name
        HighScoreEntryInit()
    Else
        EndOfBallComplete()
    End If
End Sub

Sub HighScoreEntryInit()
    hsbModeActive = True
    'PlaySound "vo_greatscore" &RndNbr(6)
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' < is back arrow
    hsCurrentLetter = 1
    DMDFlush()
    HighScoreDisplayNameNow()

    HighScoreFlashTimer.Interval = 250
    HighScoreFlashTimer.Enabled = True
End Sub

Sub EnterHighScoreKey(keycode)
    If keycode = LeftFlipperKey Then
        playsound "fx_Previous"
        hsCurrentLetter = hsCurrentLetter - 1
        if(hsCurrentLetter = 0)then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter > len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey OR keycode = StartGameKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<")then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3)then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit > 0)then
                hsCurrentDigit = hsCurrentDigit - 1
            end if
            HighScoreDisplayNameNow()
        end if
    end if
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
    if(hsCurrentDigit > 0)then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit > 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit > 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3)then
        if(hsLetterFlash <> 0)then
            TempBotStr = TempBotStr & "_"
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit < 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit < 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <    "
    dLine(1) = ExpandLine(TempBotStr)
    DMDUpdate 1
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2)then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
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
            If HighScore(j) < HighScore(j + 1)Then
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

' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text.
' 1st and 2nd lines are 20 characters long
' 3rd line is just 1 character
' Example format:
' DMD "text1","text2","backpicture", eNone, eNone, eNone, 250, True, "sound"
' Short names:
' dq = display queue
' de = display effect
' *************************************************************************

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

Sub DMD_Init() 'default/startup values
    If UseFlexDMD Then
        Set FlexDMD = CreateObject("FlexDMD.FlexDMD")
        If Not FlexDMD is Nothing Then
            If FlexDMDHighQuality Then
                FlexDMD.TableFile = Table1.Filename & ".vpx"
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
                For i = 0 to 19 ' Top
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + i * 12, 6, 12, 22
                Next
                For i = 20 to 39 ' Bottom
                    DMDScene.GetImage("Dig" & i).SetBounds 8 + (i - 20) * 12, 34, 12, 22
                Next
                FlexDMD.LockRenderThread
                FlexDMD.Stage.AddActor DMDScene
                FlexDMD.UnlockRenderThread
            Else
                FlexDMD.TableFile = Table1.Filename & ".vpx"
                FlexDMD.RenderMode = 2
                FlexDMD.Width = 128
                FlexDMD.Height = 32
                FlexDMD.Clear = True
                FlexDMD.GameName = cGameName
                FlexDMD.Run = True
                Set DMDScene = FlexDMD.NewGroup("Scene")
                DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.d_bkempty")
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

    Dim i, j
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
    Dim tmp, tmp1, tmp2
    if(dqHead = dqTail)Then
        ' default when no modes are active
        tmp = RL(FormatScore(Score(Currentplayer)))
        tmp1 = FL("PLAYER " &CurrentPlayer, "BALL " & Balls)
        tmp2 = "d_bkempty"
    'info on the second line: tmp1
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 10, True, ""
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
            dqText(2, dqTail) = Text2 'it is always 1 letter in this table
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
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1)) + 128) & right(NumString, Len(NumString)- i)
        end if
    Next
    FormatScore = NumString
End function

Function FL(NumString1, NumString2) 'Fill line
    Dim Temp, TempStr
    If Len(NumString1) + Len(NumString2) < 20 Then
        Temp = 20 - Len(NumString1)- Len(NumString2)
        TempStr = NumString1 & Space(Temp) & NumString2
        FL = TempStr
    End If
End Function

Function CL(NumString) 'center line
    Dim Temp, TempStr
    If Len(NumString) > 20 Then NumString = Left(NumString, 20)
    Temp = (20 - Len(NumString)) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(NumString) 'right line
    Dim Temp, TempStr
    If Len(NumString) > 20 Then NumString = Left(NumString, 20)
    Temp = 20 - Len(NumString)
    TempStr = Space(Temp) & NumString
    RL = TempStr
End Function

'**************
' Update DMD
'**************

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
            If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "d_bkempty"
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

'************************************
'    JP's new DMD using flashers
' two text lines and 1 backdrop image
'************************************

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
    Chars(33) = "d_excl"  '!
    Chars(34) = ""        '"
    Chars(35) = ""        '#
    Chars(36) = ""        '$
    Chars(37) = ""        '%
    Chars(38) = ""        '&
    Chars(39) = ""        ''
    Chars(40) = ""        '(
    Chars(41) = ""        ')
    Chars(42) = ""        '*
    Chars(43) = ""        '+
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
    Chars(94) = "d_up"    '^
    '    Chars(95) = '_
    Chars(96) = ""
    Chars(97) = ""  'a
    Chars(98) = ""  'b
    Chars(99) = ""  'c
    Chars(100) = "" 'd
    Chars(101) = "" 'e
    Chars(102) = "" 'f
    Chars(103) = "" 'g
    Chars(104) = "" 'h
    Chars(105) = "" 'i
    Chars(106) = "" 'j
    Chars(107) = "" 'k
    Chars(108) = "" 'l
    Chars(109) = "" 'm
    Chars(110) = "" 'n
    Chars(111) = "" 'o
    Chars(112) = "" 'p
    Chars(113) = "" 'q
    Chars(114) = "" 'r
    Chars(115) = "" 's
    Chars(116) = "" 't
    Chars(117) = "" 'u
    Chars(118) = "" 'v
    Chars(119) = "" 'w
    Chars(120) = "" 'x
    Chars(121) = "" 'y
    Chars(122) = "" 'z
    Chars(123) = "" '{
    Chars(124) = "" '|
    Chars(125) = "" '}
    Chars(126) = "" '~
    'used in the FormatScore function
    Chars(176) = "d_0a" '0.
    Chars(177) = "d_1a" '1.
    Chars(178) = "d_2a" '2.
    Chars(179) = "d_3a" '3.
    Chars(180) = "d_4a" '4.
    Chars(181) = "d_5a" '5.
    Chars(182) = "d_6a" '6.
    Chars(183) = "d_7a" '7.
    Chars(184) = "d_8a" '8.
    Chars(185) = "d_9a" '9.
End Sub

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    'info goes in a loop only stopped by the credits and the startkey

    DMD "", CL("GAME OVER"), "", eNone, eBlink, eNone, 2000, False, ""
    If Credits > 0 Then
        DMD CL("CREDITS " & Credits), CL("PRESS START"), "", eNone, eBlink, eNone, 2000, FALSE, ""
    Else
        DMD CL("CREDITS " & Credits), CL("INSERT COIN"), "", eNone, eBlink, eNone, 2000, FALSE, ""
    End If
    DMD CL("LOADEDWEAPON"), CL("AND"), "", eNone, eNone, eNone, 2000, False, ""
    DMD "        JPSALAS", "          PRESENTS", "d_jppresents", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "d_title", eNone, eNone, eNone, 3000, False, ""
    DMD "", CL("ROM VERSION " &myversion), "", eNone, eNone, eNone, 2000, False, ""
    DMD "HIGHSCORES", Space(20), "", eScrollLeft, eScrollLeft, eNone, 20, FALSE, ""
    DMD "HIGHSCORES", "", "", eNone, eNone, eNone, 1000, FALSE, ""
    DMD "_", "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, FALSE, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, FALSE, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, FALSE, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, FALSE, ""
    DMD Space(20), Space(20), "", eScrollLeft, eScrollLeft, eNone, 500, FALSE, ""
End Sub

Sub StartAttractMode
If NOT bGameInPlay Then
    PlaySong "m_attract"
    StartLightSeq
    DMDFlush
    ShowTableInfo
    AttractMode = True
End If
End Sub

Sub StopAttractMode
    DMDFlush
    AttractMode = False
    LightSeqAttract.StopPlay
    LightSeqFlasher.StopPlay
'StopSong
End Sub

Sub StartLightSeq()
    Real_StartLightSeq()
End Sub

Sub Real_StartLightSeq()
    'flasher sequences
    LightSeqFlasher.Play SeqBlinking, , 5, 90
    LightSeqFlasher.Play SeqBlinking, , 5, 80
    LightSeqFlasher.Play SeqBlinking, , 6, 70
    LightSeqFlasher.Play SeqBlinking, , 6, 60
    LightSeqFlasher.Play SeqBlinking, , 7, 50
    LightSeqFlasher.Play SeqBlinking, , 8, 40
    LightSeqFlasher.Play SeqBlinking, , 8, 30
    LightSeqFlasher.Play SeqRandom, 4, , 4000
    LightSeqFlasher.Play SeqRightOn, 50, 1
    LightSeqFlasher.Play SeqLeftOn, 50, 1
    LightSeqFlasher.Play SeqRightOn, 50, 1
    LightSeqFlasher.Play SeqLeftOn, 50, 1
    LightSeqFlasher.Play SeqRightOn, 50, 1
    LightSeqFlasher.Play SeqLeftOn, 50, 1
    LightSeqFlasher.Play SeqBlinking, , 9, 30
    LightSeqFlasher.Play SeqBlinking, , 8, 40
    LightSeqFlasher.Play SeqBlinking, , 7, 50
    LightSeqFlasher.Play SeqBlinking, , 6, 60
    LightSeqFlasher.Play SeqBlinking, , 6, 70
    LightSeqFlasher.Play SeqBlinking, , 5, 80
    LightSeqFlasher.Play SeqBlinking, , 5, 90
    LightSeqFlasher.Play SeqBlinking, , 5, 100
    LightSeqFlasher.Play SeqRandom, 4, , 4000
    LightSeqFlasher.Play SeqRightOn, 50, 1
    LightSeqFlasher.Play SeqLeftOn, 50, 1
    LightSeqFlasher.Play SeqRightOn, 50, 1
    LightSeqFlasher.Play SeqLeftOn, 50, 1
    LightSeqFlasher.Play SeqRightOn, 50, 1
    LightSeqFlasher.Play SeqLeftOn, 50, 1
    LightSeqFlasher.Play SeqBlinking, , 5, 90
    LightSeqFlasher.Play SeqBlinking, , 5, 80
    LightSeqFlasher.Play SeqBlinking, , 5, 70
    LightSeqFlasher.Play SeqBlinking, , 5, 60
    LightSeqFlasher.Play SeqBlinking, , 5, 50
    LightSeqFlasher.Play SeqBlinking, , 5, 40
    LightSeqFlasher.Play SeqBlinking, , 5, 30
    LightSeqFlasher.Play SeqBlinking, , 15, 20
    LightSeqFlasher.Play SeqBlinking, , 5, 30
    LightSeqFlasher.Play SeqBlinking, , 5, 40
    LightSeqFlasher.Play SeqBlinking, , 5, 50
    LightSeqFlasher.Play SeqBlinking, , 5, 60
    LightSeqFlasher.Play SeqBlinking, , 5, 70
    LightSeqFlasher.Play SeqBlinking, , 5, 80
    LightSeqFlasher.Play SeqBlinking, , 5, 90
    LightSeqFlasher.Play SeqBlinking, , 5, 200

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

Sub LightSeqSkillshot_PlayDone()
    LightSeqSkillshot.Play SeqAllOff
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

Sub LightSeqDouble_PlayDone()
    LightSeqDouble.Play SeqRandom, 10, , 4000
End Sub

'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 11 colors: red, orange, amber, yellow...
'******************************************

'colors
Const red = 5
Const orange = 4
Const amber = 6
Const yellow = 3
Const darkgreen = 7
Const green = 2
Const blue = 1
Const darkblue = 8
Const purple = 9
Const white = 11
Const teal = 10

Sub SetLightColor(n, col, stat) 'stat 0 = off, 1 = on, 2 = blink, -1= no change
    Select Case col
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
            n.color = RGB(0, 16, 0)
            n.colorfull = RGB(0, 128, 0)
        Case blue
            n.color = RGB(0, 18, 18)
            n.colorfull = RGB(0, 255, 255)
        Case darkblue
            n.color = RGB(0, 8, 8)
            n.colorfull = RGB(0, 64, 64)
        Case purple
            n.color = RGB(64, 0, 96)
            n.colorfull = RGB(128, 0, 192)
        Case white
            n.color = RGB(193, 91, 0)
            n.colorfull = RGB(255, 197, 143)
        Case teal
            n.color = RGB(1, 64, 62)
            n.colorfull = RGB(2, 128, 126)
    End Select
    If stat <> -1 Then
        n.State = 0
        n.State = stat
    End If
End Sub

' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************

' tables walls and animations
Sub VPObjects_Init

End Sub

' tables variables and modes init

Dim Mode

Sub zelda_Init()
    bExtraBallWonThisBall = FALSE
    TurnOffPlayfieldLights()
    ChangeGi "Normal"
    'PlayMusicForMode(1)

    PlaySoundAt "fx_droptarget", DropA1
    DropA1.IsDropped = 1
    DropA2.IsDropped = 1

    '	TimerG1.Set TRUE, 200
    PlaySoundAt "fx_resetdrop", TargetG1 'closes primitive
    Popup1.Isdropped = 0
    Popup2.Isdropped = 0
    Popup3.Isdropped = 0

    LightM1.State = 0

    P2Count = 1
    P4Count = 1
    K1Count = 1
    K2Count = 1
    cbCount = 1
    K6Count = 1
    NCount = 1
    R1Count = 1
    R2Count = 1
    PlaySoundAt "fx_resetdrop", TargetR2
    TargetR1.IsDropped = 0
    TargetR2.IsDropped = 0
    TargetR3.IsDropped = 0
    EndLights
    TargetG1.IsDropped = 1
    TimerA1.Enabled = 1 'droptargets animation
    CastleDiverter.TimerEnabled = 0
'InitDelays
'SkillshotInit
'MainModesInit()
End Sub

Sub ResetSkillShot
    bSkillshotReady = FALSE
End Sub

Sub TurnOffPlayfieldLights()
    LightShootAgain.State = 0

    LightBonus2x.State = 0
    LightBonus3x.State = 0
    LightBonus4x.State = 0
    LightBonus5x.State = 0
    LightBonus6x.State = 0
    LightBonus7x.State = 0

    LightLeftInlane1.State = 1
    LightLeftOutlane.State = 0
    LightRightOutlane.State = 0
    LightLeftInlane.State = 0
    LightRightInlane.State = 0

    LightC1.State = 0
    LightC2.State = 0
    LightC3.State = 0
    LightC4.State = 0
    LightC5.State = 0
    LightC6.State = 0
    LightC7.State = 0
    LightC8.State = 0
    LightC9.State = 0
    LightC10.State = 0
    LightC20.State = 0
End Sub

Sub EndLights()
    LightSD1.State = 0
    LightSD2.State = 0
    LightSD3.State = 0
    LightSD4.State = 0
    LightSD5.State = 0
    LightSD6.State = 0

    LightMult1.State = 0
    LightMult2.State = 0
    LightMult3.State = 0
    LightMult4.State = 0

    LightG1.State = 0

    LightR1.State = 0
    LightR2.State = 0
    LightR3.State = 0

    LightN1.State = 0
    LightN2.State = 0

    LightExtra2.State = 0
    LightPC4.State = 0

    LightExtra3.State = 0
    LightJackpot1.State = 0

    LightPD1.State = 0
    LightPD2.State = 0
    LightPD3.State = 0
    LightPD4.State = 0

    LightPC1.State = 0
    LightPC2.State = 0
    LightPC3.State = 0

    LightMystery1.State = 0
    LightM1.State = 0
    LightSpinner1.State = 0

    LightK5A.State = 0
    LightK5B.State = 0
    LightK5C.State = 0
    LightSpecial1.State = 0
    LightExtra1.State = 0

    LightV1.State = 0
    LightV2.State = 0
    LightV3.State = 0
    LightV4.State = 0
    LightV5.State = 0
    LightV6.State = 0
    LightV7.State = 0

    LightBonus2x.State = 0
    LightBonus3x.State = 0
    LightBonus4x.State = 0
    LightBonus5x.State = 0
    LightBonus6x.State = 0
    LightBonus7x.State = 0

    LightA1.State = 0
    LightA2.State = 0
    LightA3.State = 0
    LightA4.State = 0
    LightA5.State = 0

    LightP1.State = 0
    LightP2.State = 0
    LightP3.State = 0
    LightP4.State = 0
End Sub

Sub ResetSkillShotTimer_Timer 'timer to reset the skillshot lights & variables
    ResetSkillShotTimer.Enabled = 0
    bSkillShotReady = False
    DMDScoreNow
End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit sub will follow this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/modes this trigger is a member of
' - set the "LastSwicthHit" variable in case it is needed later
' *********************************************************************

' Slingshots has been hit

Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If NOT Tilted Then
        PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Lemk
        LeftSling4.Visible = 1
        Lemk.RotX = 26
        LStep = 0
        LeftSlingShot.TimerEnabled = 1
        ' add some points
        AddScore 150
        ' remember last trigger hit by the ball
        LastSwitchHit = "LeftSlingShot"
        DOF dSlingLeft, 2
    End If
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing4.Visible = 0:LeftSLing3.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing3.Visible = 0:LeftSLing2.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing2.Visible = 0:Lemk.RotX = -10:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Not Tilted Then
        PlaySoundAt SoundFX("fx_slingshot", DOFContactors), Remk
        RightSling4.Visible = 1
        Remk.RotX = 26
        RStep = 0
        RightSlingShot.TimerEnabled = 1
        ' add some points
        AddScore 150
        ' remember last trigger hit by the ball
        LastSwitchHit = "RightSlingShot"
        DOF dSlingRight, 2
    End If
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing4.Visible = 0:RightSLing3.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing3.Visible = 0:RightSLing2.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing2.Visible = 0:Remk.RotX = -10:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'*********************
' Inlanes - Outlanes
'*********************

Sub TriggerLeftInLane_Hit()
    PlaySoundAt "fx_sensor", TriggerLeftInLane
    If Tilted Then Exit Sub
    AddScore 1000
    IncrementCBonus 1
    LightLeftInlane.State = 1
    LastSwitchHit = "TriggerLeftInLane"
    PlaySound "z_Whip1"
    DOF dLeftInlane, 2
    CheckTriggerLights()
End Sub

Sub TriggerLeftInLane1_Hit()
    PlaySoundAt "fx_sensor", TriggerLeftInLane1
    If Tilted Then Exit Sub
    AddScore 1000
    IncrementCBonus 1
    LightLeftInlane1.State = 1
    LastSwitchHit = "TriggerLeftInLane1"
    PlaySound "z_Whip2"
    DOF dLeftInlane1, 2
    CheckTriggerLights()
End Sub

Sub TriggerLeftOutLane_Hit()
    PlaySoundAt "fx_sensor", TriggerLeftOutLane
    If Tilted Then Exit Sub
    AddScore 2000
    IncrementCBonus 1
    LightLeftOutlane.State = 1
    If(LightSaveBall.State = 1)Then
        'blink the light and turn it off
        PlungerIM2.AutoFire
        DOF dAutoPlunger2, 2
    End if
    LastSwitchHit = "TriggerLeftOutLane"
    PlaySound "z_fall1"
    DOF dLeftOutlane, 2
    CheckTriggerLights()
End Sub

Sub TriggerRightInLane_Hit()
    PlaySoundAt "fx_sensor", TriggerRightInLane
    If Tilted Then Exit Sub
    AddScore 1000
    IncrementCBonus 1
    LightRightInlane.State = 1
    LastSwitchHit = "TriggerRightInLane"
    PlaySound "z_Whip2"
    DOF dRightInlane, 2
    CheckTriggerLights()
End Sub

Sub TriggerRightOutLane_Hit()
    PlaySoundAt "fx_sensor", TriggerRightOutLane
    If Tilted Then Exit Sub
    AddScore 2000
    IncrementCBonus 1
    LightRightOutlane.State = 1
    LastSwitchHit = "TriggerRightOutLane"
    PlaySound "z_fall2"
    PlaySound "z_ganonlaugh"
    DOF dRightOutlane, 2
    CheckTriggerLights()
End Sub

Sub CheckTriggerLights()
    DMD "_", CL(FormatScore(Bonuspoints(Currentplayer)) & " X" &BonusMultiplier(Currentplayer)), "_", eNone, eNone, eNone, 500, TRUE, ""
    If(LightRightInlane.State = 1)And(LightLeftInlane.State = 1)And(LightLeftInlane1.State = 1)And(LightRightOutlane.State = 1)And(LightLeftOutlane.State = 1)Then
        AddScore 10000
        LightSeqLanes.Play SeqRandom, 4, , 3000
        LightSeqFlasher.Play SeqRandom, 4, , 3000
        IncrementBonusMultiplier()
        CheckTextoMult()
        LightRightInlane.State = 0
        LightLeftInlane.State = 0
        LightLeftInlane1.State = 0
        LightRightOutlane.State = 0
        LightLeftOutlane.State = 0
        PlaySound "z_fall2"
    End If
End Sub

Sub CheckTextoMult()
    If(LightBonus2x.State = 1)And(LightBonus3x.State = 0)And(LightBonus4x.State = 0)And(LightBonus5x.State = 0)And(LightBonus6x.State = 0)And(LightBonus7x.State = 0)Then
        DMD "_", CL("2 X"), "d_crown", eNone, eScrollLeft, eNone, 1000, FALSE, "z_zeldascream"
        DMD "_", "SAVE PRINCESS ZELDA", "d_crown", eNone, eBlinkFast, eNone, 1000, TRUE, ""
    End If
    If(LightBonus2x.State = 1)And(LightBonus3x.State = 1)And(LightBonus4x.State = 0)And(LightBonus5x.State = 0)And(LightBonus6x.State = 0)And(LightBonus7x.State = 0)Then
        DMD "_", CL("3 X"), "d_crown", eNone, eScrollLeft, eNone, 1000, FALSE, "z_zeldascream"
        DMD "_", "SAVE PRINCESS ZELDA", "d_crown", eNone, eBlinkFast, eNone, 1000, TRUE, ""
    End If
    If(LightBonus2x.State = 1)And(LightBonus3x.State = 1)And(LightBonus4x.State = 1)And(LightBonus5x.State = 0)And(LightBonus6x.State = 0)And(LightBonus7x.State = 0)Then
        DMD "_", CL("4 X"), "d_crown", eNone, eScrollLeft, eNone, 1000, FALSE, "z_zeldascream"
        DMD "_", "SAVE PRINCESS ZELDA", "d_crown", eNone, eBlinkFast, eNone, 1000, TRUE, ""
    End If
    If(LightBonus2x.State = 1)And(LightBonus3x.State = 1)And(LightBonus4x.State = 1)And(LightBonus5x.State = 1)And(LightBonus6x.State = 0)And(LightBonus7x.State = 0)Then
        DMD "_", CL("5 X"), "d_crown", eNone, eScrollLeft, eNone, 1000, FALSE, "z_zeldascream"
        DMD "_", "SAVE PRINCESS ZELDA", "d_crown", eNone, eBlinkFast, eNone, 1000, TRUE, ""
    End If
    If(LightBonus2x.State = 1)And(LightBonus3x.State = 1)And(LightBonus4x.State = 1)And(LightBonus5x.State = 1)And(LightBonus6x.State = 1)And(LightBonus7x.State = 0)Then
        DMD "_", CL("6 X"), "d_crown", eNone, eScrollLeft, eNone, 1000, FALSE, "z_zeldascream"
        DMD "_", "SAVE PRINCESS ZELDA", "d_crown", eNone, eBlinkFast, eNone, 1000, TRUE, ""
    End If
    If(LightBonus2x.State = 1)And(LightBonus3x.State = 1)And(LightBonus4x.State = 1)And(LightBonus5x.State = 1)And(LightBonus6x.State = 1)And(LightBonus7x.State = 1)Then
        DMD "_", CL("7 X"), "d_crown", eNone, eScrollLeft, eNone, 1000, FALSE, "z_zeldascream"
        DMD "_", "SAVE PRINCESS ZELDA", "d_crown", eNone, eBlinkFast, eNone, 1000, TRUE, ""
    End If
End Sub

'***********
' A targets
'***********

Sub DropA1_Hit()
    If Not Tilted Then
        PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), DropA1
        If(LightExtra3.State = 2)Then
            AwardExtraBall()
            DMD CL("EXTRA"), CL("BALL"), "d_dekuscrub", eBlink, eBlink, eNone, 1000, TRUE, "fx_Knocker"
            DMD CL("EXTRA"), CL("BALL"), "d_dekuscrub", eBlink, eBlink, eNone, 1000, TRUE, "z_ExtraBall"
            AddScore 1000
            LightSeqFlasher.Play SeqRandom, 4, , 3000
            LightSeqGi.Play SeqRandom, 4, , 3000
            LightExtra3.State = 0
        Else
            DMD " YOU HIT DEKU SCRUB", CL("1000"), "d_dekuscrub", eNone, eBlink, eNone, 1500, True, "z_DekuScrub"
        End If
        DropA1.IsDropped = 1
        DOF dDropTargetA1, 2
        Addscore 1000
    End If
End Sub

Sub DropA2_Hit()
    If Not Tilted Then
        PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), DropA2
        DropA2.IsDropped = 1
        DOF dDropTargetA1, 2
        If(LightJackpot1.State = 2)Then
            DMD "BIG DEKU BABA JACKPOT", CL(FormatScore("100000")), "d_dekubaba", eNone, eBlinkFast, eNone, 1500, TRUE, "z_BigDekuBaba"
            AddScore 100000
            LightSeqFlasher.Play SeqRandom, 4, , 3000
            DOF dJackpot, 2
            LightSeqGi.Play SeqRandom, 4, , 3000
            LightJackpot1.State = 0
        Else
            DMD CL("YOU HIT DEKU BABA"), CL("1000"), "d_dekubaba", eNone, eBlink, eNone, 1500, True, "z_BigDekuBaba"
            Addscore 1000
        End If
    End If
End Sub

'************
' N targets
'************

Sub TargetN1_Hit
    If Not Tilted Then
        PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), TargetN1
        LightSeqRed.Play SeqBlinking, , 8, 30
        Addscore 1000
        LightN1.State = 1
        LightSD1.State = 1
        CheckSD
        DMD CL("1000"), CL("SHIELD UP"), "d_shield", eBlinkFast, eScrollLeft, eNone, 1000, TRUE, "z_Clang2"
        DOF dTargetN1, 2
    End If
End Sub

Sub TargetN2_Hit
    If Not Tilted Then
        PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), TargetN2
        LightSeqRed.Play SeqBlinking, , 8, 30

        Addscore 1000
        LightN2.State = 1
        LightSD5.State = 1
        CheckSD
        DMD CL("1000"), CL("SHIELD UP"), "d_shield", eBlinkFast, eScrollLeft, eNone, 1000, TRUE, "z_Clang2"
        DOF dTargetN2, 2
    End If
End Sub

'************
' R Targets
'************

Sub TargetR1_Hit
    If Not Tilted Then
        PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), TargetR1
        LightSeqGreen.Play SeqBlinking, , 8, 30
        TargetR1.IsDropped = 1
        LightR1.State = 1
        DMD CL("5000"), CL("BOW"), "", eScrollRight, eScrollLeft, eNone, 1000, TRUE, "z_bow"
        AddScore 5000
        LightSD2.State = 1
        CheckSD
        CheckRTargets
        DOF dDropTargetR1, 2
    End If
End Sub

Sub TargetR2_Hit
    If Not Tilted Then
        PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), TargetR2
        LightSeqGreen.Play SeqBlinking, , 8, 30
        TargetR2.IsDropped = 1
        LightR2.State = 1
        PlaySound "z_sword"
        DMD CL("5000"), CL("SWORD"), "", eScrollRight, eScrollLeft, eNone, 1000, TRUE, "z_sword"
        AddScore 5000
        LightSD3.State = 1
        CheckSD
        CheckRTargets
        DOF dDropTargetR2, 2
    End If
End Sub

Sub TargetR3_Hit
    If Not Tilted Then
        PlaySoundAt SoundFX("fx_droptarget", DOFDropTargets), TargetR3
        LightSeqGreen.Play SeqBlinking, , 8, 30
        TargetR3.IsDropped = 1
        LightR3.State = 1
        DMD CL("5000"), CL("SHIELD"), "d_shield", eScrollRight, eScrollLeft, eNone, 1000, TRUE, "z_shield"
        AddScore 5000
        LightSD4.State = 1
        CheckSD
        CheckRTargets
        DOF dDropTargetR3, 2
    End If
End Sub

Sub CheckRTargets
    If(LightR1.State + LightR2.State + LightR3.State) = 3 Then
        AddScore 10000
        LightSeqFlasher.Play SeqRandom, 4, , 3000
        ' a little delay before reseting the targets and lights
        vpmTimer.AddTimer 1000, "ResetRTargets"
    End If
End Sub

Sub ResetRTargets(d)
    PlaySoundAt SoundFX("fx_resetdrop", DOFContactors), TargetR2
    TargetR1.IsDropped = 0
    TargetR2.IsDropped = 0
    TargetR3.IsDropped = 0
    LightR1.State = 0
    LightR2.State = 0
    LightR3.State = 0
    DOF dDropTargetR_Reset, 2
End Sub

' Shield Lights - Turn on the Jackpot or the Extra game lights on the right hole

Dim SDCount:SDCount = 0
Sub CheckSD
    If(LightSD1.State = 1)And(LightSD2.State = 1)And(LightSD3.State = 1)And(LightSD4.State = 1)And(LightSD5.State = 1)And(LightSD6.State = 1)Then
        LightSeqShield.Play SeqRandom, 6, , 3000
        LightSeqFlasher.Play SeqRandom, 6, , 3000
        LightN1.State = 0
        LightN2.State = 0
        LightSD1.State = 0
        LightSD2.State = 0
        LightSD3.State = 0
        LightSD4.State = 0
        LightSD5.State = 0
        LightSD6.State = 0
        If SDCount Then 'prepare extra life
            LightExtra2.State = 2
            SDCount = 0
        Else 'prepare right jackpot
            LightPC4.State = 2
            SDCount = 1
        End If
    End If
End Sub

'************
' P Triggers
'************

Sub TriggerP1_Hit
    PlaySoundAt "fx_sensor", TriggerP1
    If NOT Tilted Then
        DOF dLeftCastleTrigger, 2
        LightP1.State = 1
        AddScore 1500
        CheckRollovers
    End If
    LastSwitchHit = "TriggerP1"
    PlaySound "z_link"
End Sub

Sub TriggerP2_Hit
    PlaySoundAt "fx_sensor", TriggerP2
    DOF dBehindCastleTrigger, 2
    If ActiveBall.VelX < 0 Then bSkillShotReady = FALSE 'turn of the skill if the ball goes to the left and down.
    If NOT Tilted Then
        If ActiveBall.VelX > 0 Then
            LightP2.State = 1
            AddScore 1500
            CheckRollovers
            Select Case P2Count
                Case 1
                    Addscore 1000
                    LightPC1.State = 1
                    P2Count = 2
                Case 2
                    Addscore 2000
                    LightPC2.State = 1
                    P2Count = 3
                Case 3
                    Addscore 3000
                    LightPC3.State = 1
                    P2Count = 4
                Case 4
                    Addscore 6000
                    LightPC4.State = 1
                    DMD "_", CL("GET THE JACKPOT"), "", eNone, eScrollLeft, eNone, 1000, TRUE, ""
                    LightSpinner1.State = 1
                    P2Count = 1
                    LightPC1.State = 0
                    LightPC2.State = 0
                    LightPC3.State = 0
            End Select
            PlaySound "z_watchout"
            PlaySound "z_Out"
        End If
    End If
    LastSwitchHit = "TriggerP2"
End Sub

Sub TriggerP3_Hit
    PlaySoundAt "fx_sensor", TriggerP3
    DOF dShooterLaneTrigger, 2
    If ActiveBall.VelY > 0 Then bSkillShotReady = FALSE 'turn of the skill if the ball goes down.
    If NOT Tilted Then
        LightP3.State = 1
        AddScore 1500
        CheckRollovers
    End If
    LastSwitchHit = "TriggerP3"
    PlaySound "z_Dismount"
End Sub

Sub TriggerP4_Hit
    PlaySoundAt "fx_sensor", TriggerP4
    If NOT Tilted Then
        LightP4.State = 1
        DOF dTreasureRampTrigger, 2
        CheckRollovers
        Select Case P4Count
            Case 1
                Addscore 1000
                LightPD1.State = 1
                DMD CL("1000"), CL("EPONA"), "d_epona", eScrollRight, eScrollLeft, eNone, 1000, TRUE, ""
                P4Count = 2
            Case 2
                Addscore 2000
                LightPD2.State = 1
                DMD CL("2000"), CL("EPONA"), "d_epona", eScrollRight, eScrollLeft, eNone, 1000, TRUE, ""
                P4Count = 3
            Case 3
                Addscore 3000
                LightPD3.State = 1
                DMD CL("3000"), CL("EPONA"), "d_epona", eScrollRight, eScrollLeft, eNone, 1000, TRUE, ""
                P4Count = 4
            Case 4
                Addscore 3000
                LightPD4.State = 1
                DMD "_", CL("PREPARE ATTACK"), "d_arrow", eNone, eScrollLeft, eNone, 1000, FALSE, ""
                DMD "_", CL("ATTACK BONUS"), "d_arrow", eNone, eScrollLeft, eNone, 1000, TRUE, ""
                LightExtra3.State = 2
                LightSpinner1.State = 1
                P4Count = 1
        End Select
    End If
    LastSwitchHit = "TriggerP4"
    PlaySound "z_eponaride"
End Sub

'*********
' spinner
'*********

Sub Spinner1_Spin()
    PlaySoundAt "fx_spinner", Spinner1
    If NOT Tilted Then
        If(LightSpinner1.State = 1)Then
            AddScore 1000
            DMD CL("1000"), "_", "", eBlinkFast, eNone, eNone, 500, TRUE, ""
        Else
            AddScore 100
        End If
        DOF dSpinner, 2
        PlaySound "z_swordspin"
        LastSwitchHit = "Spinner1"
    End If
End Sub

'************
' V triggers
'************

Sub TriggerV1_Hit()
    PlaySoundAt "fx_sensor", TriggerV1
    If ActiveBall.VelY > 0 Then bSkillShotReady = FALSE 'turn of the skill if the ball goes down.
    If NOT Tilted Then
        DOF dRightCastleTrigger, 2
        LightV1.State = 1
        AddScore 1500
        CheckRollovers
    End If
    LastSwitchHit = "TriggerV1"
End Sub

Sub TriggerV2_Hit()
    PlaySoundAt "fx_sensor", TriggerV2
    If NOT Tilted Then
        DOF dBlueTrigger1, 2
        LightV2.State = 1
        AddScore 1500
        CheckRollovers
    End If
    LastSwitchHit = "TriggerV2"
End Sub

Sub TriggerV3_Hit()
    PlaySoundAt "fx_sensor", TriggerV3
    If NOT Tilted Then
        DOF dBlueTrigger2, 2
        LightV3.State = 1
        AddScore 1500
        CheckRollovers
    End If
    LastSwitchHit = "TriggerV3"
End Sub

Sub TriggerV4_Hit()
    PlaySoundAt "fx_sensor", TriggerV4
    If NOT Tilted Then
        DOF dBlueTrigger3, 2
        LightV4.State = 1
        AddScore 1500
        CheckRollovers
    End If
    LastSwitchHit = "TriggerV4"
End Sub

Sub TriggerV5_Hit()
    PlaySoundAt "fx_sensor", TriggerV5
    If NOT Tilted Then
        DOF dBlueTrigger4, 2
        LightV5.State = 1
        AddScore 1500
        CheckRollovers
    End If
    LastSwitchHit = "TriggerV5"
End Sub

Sub TriggerV6_Hit()
    PlaySoundAt "fx_sensor", TriggerV6
    If NOT Tilted Then
        DOF dBlueTrigger5, 2
        LightV6.State = 1
        AddScore 1500
        CheckRollovers
    End If
    LastSwitchHit = "TriggerV6"
End Sub

Sub TriggerV7_Hit()
    PlaySoundAt "fx_sensor", TriggerV7
    If NOT Tilted Then
        DOF dBlueTrigger6, 2
        LightV7.State = 1
        AddScore 1500
        CheckRollovers
    End If
    LastSwitchHit = "TriggerV7"
End Sub

Dim DoubleScore
DoubleScore = 1

Sub CheckRollovers
    If DoubleScore = 1 Then
        If LightP1.State + LightP2.State + LightP3.State + LightP4.State + LightV1.State + LightV2.State + LightV3.State + LightV4.State + LightV5.State + LightV6.State + LightV7.State = 11 Then
            AddMultiball 1
            EnableBallSaver 20
            DoubleScore = 2 'this is a score multiplier
            DMD "_", CL("DOUBLE SCORING!"), "", eNone, eBlinkFast, eNone, 1000, FALSE, ""
            DMD "_", CL("MULTIBALL!"), "", eNone, eBlinkFast, eNone, 1000, TRUE, ""
            LightSeqFlasher.Play SeqRandom, 4, , 3000
            LightSeqDouble.Play SeqRandom, 10, , 4000 'blink the GI lights during this mode
        End If
    End If
End Sub

Sub ResetDoubleScore
    DoubleScore = 1
    LightP1.State = 0
    LightP2.State = 0
    LightP3.State = 0
    LightP4.State = 0
    LightV1.State = 0
    LightV2.State = 0
    LightV3.State = 0
    LightV4.State = 0
    LightV5.State = 0
    LightV6.State = 0
    LightV7.State = 0
    LightSeqDouble.StopPlay
End Sub

' A Targets

Sub TargetA1_Hit()
    PlaySoundAtBall "fx_target"
    If NOT Tilted Then
        LightSeq2Flasher.Play SeqRandom, 2, , 2000
        LightA1.State = 1
        CheckA1
        AddScore 1000
        PlaySound "z_Volvagia"
        DOF dDTargetA1, 2
    End If
End Sub

Sub TargetA2_Hit()
    PlaySoundAtBall "fx_target"
    If NOT Tilted Then
        LightSeq2Flasher.Play SeqRandom, 2, , 2000
        LightA2.State = 1
        CheckA1
        AddScore 1000
        PlaySound "z_Gohma"
        DOF dDTargetA2, 2
    End If
End Sub

Sub TargetA3_Hit()
    PlaySoundAtBall "fx_target"
    If NOT Tilted Then
        LightSeq2Flasher.Play SeqRandom, 2, , 2000
        DOF dDTargetA3, 2
        LightA3.State = 1
        CheckA1
        AddScore 1000
        PlaySound "z_BongoBongo"
    End If
End Sub

Sub TargetA4_Hit()
    PlaySoundAtBall "fx_target"
    If NOT Tilted Then
        LightSeq2Flasher.Play SeqRandom, 2, , 2000
        DOF dDTargetA4, 2
        LightA4.State = 1
        CheckA1
        AddScore 1000
        PlaySound "z_Barinade"
    End If
End Sub

Sub TargetA5_Hit()
    PlaySoundAtBall "fx_target"
    If NOT Tilted Then
        LightSeq2Flasher.Play SeqRandom, 2, , 2000
        DOF dDTargetA5, 2
        LightA5.State = 1
        CheckA1
        AddScore 1000
        PlaySound "z_KingDodongo"
    End If
End Sub

Dim TimerA1Step:TimerA1Step = 0

Sub TimerA1_Timer
    Select case TimerA1Step
        Case 0:PlaySoundAt "fx_gion", TargetA3:TargetA2.IsDropped = 0:TargetA4.IsDropped = 0:TargetA1.IsDropped = 1:TargetA3.IsDropped = 1:TargetA5.IsDropped = 1:TimerA1Step = 1
        Case 1:PlaySoundAt "fx_gioff", TargetA3:TargetA2.IsDropped = 1:TargetA4.IsDropped = 1:TargetA1.IsDropped = 0:TargetA3.IsDropped = 0:TargetA5.IsDropped = 0:TimerA1Step = 0
    End Select
End Sub

Sub CheckA1()
    If(LightA1.State + LightA2.State + LightA3.State + LightA4.State + LightA5.State) = 5 Then
        TimerA1.Enabled = FALSE
        TargetA1.IsDropped = 1
        TargetA2.IsDropped = 1
        TargetA3.IsDropped = 1
        TargetA4.IsDropped = 1
        TargetA5.IsDropped = 1
        DMD "_", CL("GUARDIANS ARE DOWN"), "", eNone, eBlinkFast, eNone, 1000, FALSE, ""
        DMD "_", CL("KILL GANONDORF!"), "", eNone, eBlinkFast, eNone, 1000, TRUE, ""
        PlaySong "m_ganondorf"
        TargetG1.IsDropped = 0
        DOF dDTargetA_Change, 2
        ChangeGi "Red"
        LightG1.State = 2
    End If
End Sub

Sub TargetG1_Hit
    If NOT Tilted Then
        TargetG1.IsDropped = 1
        LightG1.State = 1
        LightSeqFlasher.Play SeqRandom, 4, , 5000
        DMD "_", CL("GANONDORF IS DEAD"), "", eNone, eBlink, eNone, 1000, FALSE, "z_link"
        DMD "_", CL("GET THE TRIFORCE"), "", eNone, eBlink, eNone, 1000, FALSE, "z_attack"
        DMD "_", CL("RESCUE ZELDA"), "", eNone, eBlinkFast, eNone, 1000, TRUE, "z_ganondie"
        'Open the gates
        vpmTimer.AddTimer 1000, "OpenGates"
        AddScore 5000
        DOF dDTargetG1, 2
    End If
End Sub

Sub TargetG1_Timer
    If TargetG1p.Z < -65 Then TargetG1p.Z = -60:TargetG1.isdropped = 1:Me.TimerEnabled = 0
    If TargetG1p.Z > 10 Then TargetG1p.Z = -25:TargetG1.isdropped = 0:Me.TimerEnabled = 0
    TargetG1p.Z = TargetG1p.Z + TargetG1.UserValue
End Sub

Sub OpenGates(dummy)
    Popup1.IsDropped = 1
    Popup2.IsDropped = 1
    Popup3.IsDropped = 1
    DOF dCastleGate, 2
    PlaySound "z_doorclose"
    CastleDiverter.TimerEnabled = 1
    SetLightColor towerrightlight, green, 2
    SetLightColor towerleftlight, red, 1
End Sub

Dim cdivpos:cdivPos = 0

Sub CastleDiverter_Timer
    Select Case cdivpos
        Case 0:CastleDiverter.RotateToEnd:cdivpos = 1
        Case 1:CastleDiverter.RotateToStart:cdivPos = 0
    End Select
    DOF dCastleDiverter, 2
End Sub

Sub CastleDiverter_Collide(parm)
    PlaySound "fx_plastic", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

' Captive Ball target

' Captive Ball Right - done here to add the ball hit sound.
Sub CapTrigger_Hit:cbLeft.TrigHit ActiveBall:DOF dCaptiveBall, 2:End Sub
Sub CapTrigger_UnHit:cbLeft.TrigHit 0:End Sub
Sub CapWall_Hit:PlaySoundAtBall "fx_collide":cbLeft.BallHit ActiveBall:End Sub
Sub CapKicker2_Hit:cbLeft.BallReturn Me:End Sub

Sub cbTarget_Hit()
    PlaySoundAtBall SoundFX("fx_target", DOFTargets)
    'animate the target
    DOF dcbTarget, 2
    If Not Tilted Then
        PlaySound "z_switch"
        Select Case cbCount
            Case 1
                LightK5A.State = 1
                DMD CL(FormatScore("1000")), CL("FLAG 1"), "", eBlink, eScrollLeft, eNone, 1000, TRUE, ""
                AddScore(1000)
                LightSeqFlasher.Play SeqRandom, 4, , 3000
                cbCount = 2
            Case 2
                LightK5B.State = 1
                DMD CL(FormatScore("1000")), CL("FLAG 2"), "", eBlink, eScrollLeft, eNone, 1000, TRUE, ""
                AddScore 1000
                LightSeqFlasher.Play SeqRandom, 4, , 3000
                cbCount = 3
            Case 3
                LightK5C.State = 1
                DMD CL(FormatScore("1000")), CL("FLAG 3"), "", eBlink, eScrollLeft, eNone, 1000, TRUE, ""
                AddScore 1000
                LightSeqFlasher.Play SeqRandom, 4, , 3000
                cbCount = 4
            Case 4 'prepare left extra ball
                LightSpecial1.State = 1
                DropA1.IsDropped = 0
                DropA2.IsDropped = 0
                DOF dDropTargetA_Reset, 2
                LightJackpot1.State = 2
                DMD "_", CL("JACKPOT PREPARED"), "", eNone, eBlinkFast, eNone, 1000, TRUE, ""
                AddScore 1000
                LightSeqFlasher.Play SeqRandom, 4, , 3000
                cbCount = 5
            Case 5 'give extra life or extra ball
                LightExtra1.State = 1
                If(LightMystery1.State = 1)Then
                    DMD CL("MYSTERY!!!"), "", "", eNone, eNone, eNone, 1000, FALSE, ""
                    Credits = Credits + 1
                    DMD CL("30000"), CL("CREDITS: " & Credits), "_", eBlinkFast, eNone, eNone, 1000, True, "fx_knocker"
                    DOF dKnocker, 2
                    Addscore 30000
                    LightSpecial1.State = 0
                    DMD CL("INCREDIBLE!!!"), CL("FREE GAME"), "_", eNone, eBlink, eNone, 1000, TRUE, "z_YAY"
                    LightSeqFlasher.Play SeqRandom, 4, , 5000
                    'LightSeqEffect.UpdateInterval = 1
                    'LightSeqEffect.Play SeqUpOn, 20
                    LightMystery1.State = 0
                Else
                    AwardExtraBall()
                    DMD CL("MYSTERY!!!"), "", "", eNone, eNone, eNone, 1000, TRUE, "z_YAY"
                    DMD CL("MYSTERY!!!"), "", "", eNone, eNone, eNone, 1000, TRUE, "fx_knocker"
                    DMD CL("INCREDIBLE!!!"), CL("EXTRA BALL"), "_", eNone, eBlink, eNone, 1000, TRUE, "z_ExtraBall"
                    LightSeqFlasher.Play SeqRandom, 4, , 3000
                    LightSeqGi.Play SeqRandom, 4, , 3000
                    Addscore 10000
                End If
                cbCount = 5
                vpmTimer.AddTimer 1000, "ResetcbLights"
        End select
    End If
End Sub

Sub ResetcbLights(d)
    LightK5A.State = 0
    LightK5B.State = 0
    LightK5C.State = 0
    LightSpecial1.State = 0
    LightExtra1.State = 0
    cbCount = 1
End Sub

'**********************
' Left Ramp Castle Hole
'**********************

Sub Kicker1_Hit()
    Dim tmp
    PlaySoundAt "fx_kicker_enter", kicker1
    If NOT Tilted Then
        LightSeqFlasher.Play SeqRandom, 4, , 3000
        LightSeqGI.Play SeqRandom, 4, , 3000

        ' Give multiball if all 4 triforce lights are on.
        If((LightMult1.State + LightMult2.State + LightMult3.State + LightMult4.State) = 4)And(bMultiBallMode = FALSE)Then
            DMD CL("MULTIBALL"), CL("YOU HAVE THE POWER"), "d_triforce", eBlinkFast, eBlinkFast, eNone, 2000, TRUE, ""
            DMD "_", CL("ZELDA IS SAFE"), "d_crown", eNone, eScrollLeft, eNone, 2000, True, "z_ZeldaLaugh"
            LightMult4.State = 0
            LightMult3.State = 0
            LightMult2.State = 0
            LightMult1.State = 0
            EnableBallSaver 20 '20 seconds ballsaver
            AddMultiball 4     ' add 4 multiballs
            PlaySong "m_multiball"
            Addscore 8000
            PlaySound "z_link"
            PlaySound "z_attack"
            PlaySound "z_ganondie"

        'otherwise give Jackpot during multiballs
        ElseIf bMultiBallMode Then
            DMD "", "", "d_sword", eNone, eNone, eNone, 1000, FALSE, ""
            DMD "", CL(FormatScore("30000")), "d_sword", eNone, eBlinkFast, eNone, 1000, TRUE, "z_attack"
            AddScore 30000
            DOF dJackpot, 2
        ElseIf LightMystery1.State = 1 Then
            DMD "", "", "d_sword", eNone, eNone, eNone, 1000, FALSE, "z_LOOT"
            tmp = 5000 + 5000 * INT(RND(1) * 10)
            DMD CL(FormatScore(tmp)), CL("MYSTERY SCORE"), "d_sword", eBlink, eBlinkFast, eNone, 1000, TRUE, "z_attack"
            Addscore tmp
            LightMystery1.State = 0
        Else
            ' otherwise give some points
            DMD CL("GET THE TRIFORCE"), CL("TO RESCUE ZELDA"), "d_triforce", eNone, eBlinkFast, eNone, 2000, TRUE, "z_zeldascream"
            Addscore 2500
        End If
    End If
    Kicker1.TimerEnabled = 1
End Sub

Sub Kicker1_Timer
    Kicker1.TimerEnabled = 0
    LightSeqFlasher.Play SeqBlinking, , 8, 30
    kicker1.destroyball
    kicker1a.createball
    Kicker1a.kick 270, 6
    DOF dCastleKickerLeft, 2
    LightMystery1.State = 0
    PlaySoundAt "fx_kicker", Kicker1
End Sub

'*****************
' Right Ramp hole
'*****************

' kicker2a is the exit kicker for kicker2, kicker3 and kicker6
' so we create a ballstack so we don't loose balls during multiball

Dim rBalls2Eject
rBalls2Eject = 0

Sub AddRampBall(nballs)
    rBalls2Eject = rBalls2Eject + nballs
    vpmTimer.AddTimer 2000, "EjectRampBall '"
End Sub

' Eject the ball/balls
Sub EjectRampBall()
    If rBalls2Eject Then
        rBalls2Eject = rBalls2Eject -1
        ' create a ball in the plunger lane kicker.
        kicker2a.CreateBall
        ' kick it out..
        Kicker2a.Kick 170, 6
        DOF dCastleKickerRight, 2
        PlaySoundAt SoundFX("fx_popper", DOFContactors), Kicker2a
    End If
End Sub

Sub Kicker2_Hit()
    PlaySoundAt "fx_hole_enter", Kicker2
    Kicker2.Destroyball:AddRampBall 1
If Not Tilted Then
    PlaySound "z_ocarina"
    LightSeqGI.Play SeqRandom, 4, , 3000
    If(LightPD4.State = 1)Then
        LightPD1.State = 0
        LightPD2.State = 0
        LightPD3.State = 0
        LightPD4.State = 0
        P4Count = 1
        DMD CL(FormatScore("25000")), CL("OCARINA SCORE"), "d_flute", eBlink, eBlink, eNone, 1000, FALSE, ""
        DMD "_", CL("MULTIBALL"), "d_flute", eNone, eBlink, eNone, 1000, TRUE, ""
        AddScore 25000
        AddMultiball 1
    End If

    Select Case K2Count
        Case 1
            DMD "_", CL("CASTLE 1 RETURN"), "d_castle", eNone, eBlink, eNone, 1000, TRUE, ""
            AddScore 1000
            LightSeqFlasher.Play SeqRandom, 4, , 4000
            K2Count = 2
        Case 2
            DMD "_", CL("CASTLE 2 RETURN"), "d_castle", eNone, eBlink, eNone, 1000, TRUE, ""
            AddScore 1000
            LightSeqFlasher.Play SeqRandom, 4, , 4000
            K2Count = 3
        Case 3
            DMD "_", CL("CASTLE 3 RETURN"), "d_castle", eNone, eBlink, eNone, 1000, TRUE, ""
            AddScore 1000
            LightSeqFlasher.Play SeqRandom, 4, , 4000
            K2Count = 4
        Case 4
            DMD "_", CL("CASTLE 4 RETURN"), "d_castle", eNone, eBlink, eNone, 1000, TRUE, ""
            AddScore 1000
            LightSeqFlasher.Play SeqRandom, 4, , 4000
            K2Count = 5
        Case 5
            DMD "_", CL("CASTLE 5 RETURN"), "d_castle", eNone, eBlink, eNone, 1000, TRUE, ""
            AddScore 1000
            LightSeqFlasher.Play SeqRandom, 4, , 4000
            LightSeqGi.Play SeqRandom, 4, , 4000
            EnableBallSaver 20
            K2Count = 6
        Case 6
            DMD "_", CL("CASTLE 6 RETURN"), "d_castle", eNone, eBlink, eNone, 1000, TRUE, ""
            AddScore 1000
            LightSeqFlasher.Play SeqRandom, 4, , 4000
            K2Count = 7
        Case 7
            DMD "_", CL("CASTLE 7 RETURN"), "d_castle", eNone, eBlink, eNone, 1000, TRUE, ""
            AddScore 1000
            LightSeqFlasher.Play SeqRandom, 4, , 4000
            K2Count = 8
        Case 8
            DMD "_", CL("CASTLE 8 RETURN"), "d_castle", eNone, eBlink, eNone, 1000, TRUE, ""
            AddScore 1000
            LightSeqFlasher.Play SeqRandom, 4, , 4000
            K2Count = 9
        Case 9
            DMD "_", CL("CASTLE 9 RETURN"), "d_castle", eNone, eBlink, eNone, 1000, TRUE, ""
            AddScore 1000
            LightSeqFlasher.Play SeqRandom, 4, , 4000
            K2Count = 10
        Case 10
            DMD "_", CL("CASTLE 10 RETURN"), "d_castle", eNone, eBlink, eNone, 1000, FALSE, ""
            DMD "_", CL("JACKPOT PREPARED"), "d_castle", eNone, eBlink, eNone, 1000, TRUE, ""
            AddScore 1000
            LightSeqFlasher.Play SeqRandom, 4, , 4000
            DropA1.IsDropped = 0
            DOF dDropTargetA1, 2
            DropA2.IsDropped = 0
            DOF dDropTargetA2, 2
            LightJackpot1.State = 2
            K2Count = 1
    End Select
End If
End Sub

'************************************************
' Right castle hole - the triforce multiball hole
'************************************************

Sub Kicker3_Hit()
    PlaySoundAt "fx_hole_enter", Kicker3
    Kicker3.Destroyball:AddRampBall 1
If NOT Tilted Then
    If(LightMult1.State = 0)And(LightMult2.State = 0)And(LightMult3.State = 0)And(LightMult4.State = 0)And(bMultiBallMode = FALSE)Then
        LightMult1.State = 1
        DMD "_", CL("1ST TRIFORCE PIECE"), "d_triforce", eNone, eScrollLeft, eNone, 1000, True, "z_triforce1"
        DMD "_", CL("ASSEMBLE THE TRIFORCE"), "d_triforce", eNone, eScrollLeft, eNone, 1000, TRUE, ""
        Addscore 1000
        Kicker3.TimerEnabled = 1
        Exit Sub
    End If
    If(LightMult1.State = 1)And(lightMult2.State = 0)And(LightMult3.State = 0)And(LightMult4.State = 0)And(bMultiBallMode = FALSE)Then
        LightMult2.State = 1
        DMD "_", CL("2ND TRIFORCE PIECE"), "d_triforce", eNone, eScrollLeft, eNone, 1000, True, "z_triforce1"
        DMD "_", CL("ASSEMBLE THE TRIFORCE"), "d_triforce", eNone, eScrollLeft, eNone, 1000, TRUE, ""
        Addscore 2000
        Kicker3.TimerEnabled = 1
        Exit Sub
    End If
    If(LightMult1.State = 1)And(LightMult2.State = 1)And(LightMult3.State = 0)And(LightMult4.State = 0)And(bMultiBallMode = FALSE)Then
        LightMult3.State = 1
        DMD "_", CL("3RD TRIFORCE PIECE"), "d_triforce", eNone, eScrollLeft, eNone, 1000, True, "z_triforce2"
        DMD "_", CL("ASSEMBLE THE TRIFORCE"), "d_triforce", eNone, eScrollLeft, eNone, 1000, TRUE, ""
        Addscore 3000
        Kicker3.TimerEnabled = 1
        Exit Sub
    End If
    If(LightMult1.State = 1)And(LightMult2.State = 1)And(LightMult3.State = 1)And(LightMult4.State = 0)And(bMultiBallMode = FALSE)Then
        LightMult4.State = 1
        DMD "_", CL("TRIFORCE ASSEMBLED"), "d_triforce", eNone, eScrollLeft, eNone, 1000, True, "z_triforce3"
        DMD "_", CL("FREE ZELDA NOW!"), "d_triforce", eNone, eScrollLeft, eNone, 1000, TRUE, ""
        Addscore 4000
        SetLightColor towerrightlight, red, 1
        SetLightColor towerleftlight, green, 2
        Kicker3.TimerEnabled = 1
        Exit Sub
    End If
    If(LightMult1.State = 1)And(LightMult2.State = 1)And(LightMult3.State = 1)And(LightMult4.State = 1)Then 'triforce score
        DMD CL(FormatScore("5000")), CL("TRIFORCE SCORE"), "d_triforce", eBlink, eBlinkFast, eNone, 1000, TRUE, "z_LOOT"
        Addscore 5000
    End If
End If
End Sub

'*****************
' Right Ramp hole
'*****************

Sub Kicker4_Hit()
    Dim tmp
    PlaySoundAt "fx_Kicker_enter", Kicker4
    If NOT Tilted Then
        If LightExtra2.State = 2 Then
            AwardExtraBall()
            DMD CL("EXTRA"), CL("BALL"), "d_treasure", eBlink, eBlink, eNone, 1000, FALSE, "fx_knocker"
            DMD CL(FormatScore("10000")), CL("EXTRA-LIFE"), "d_treasure", eBlink, eBlinkFast, eNone, 2000, TRUE, "z_ExtraBall"
            Addscore 10000
            LightExtra2.State = 0
        ElseIf LightPC4.State = 2 Then
            DMD "YOU FOUND A TREASURE", "", "d_treasure", eNone, eNone, eNone, 1000, FALSE, "z_LOOT"
            DMD "YOU FOUND A TREASURE", CL(FormatScore("50000")), "d_treasure", eBlink, eBlinkFast, eNone, 2000, TRUE, ""
            AddScore 50000
            LightPC4.State = 0
        ElseIf LightMystery1.State = 1 Then
            DMD CL("MYSTERY SCORE"), "", "d_treasure", eNone, eNone, eNone, 1000, FALSE, "z_LOOT"
            tmp = 5000 + 5000 * RndNbr(10)
            DMD CL("MYSTERY SCORE"), CL(FormatScore(tmp)), "d_treasure", eNone, eBlinkFast, eNone, 2000, TRUE, ""
            Addscore tmp
            LightMystery1.State = 0
        Else
            LightSD8.State = 1
            CheckSD
            PlaySound "z_chest"
            AddScore 5000
        End If
    End If
    Flasher5.State = 2
    DOF dLowerRFlasher, 1
    Kicker4.TimerEnabled = 1
End Sub

Sub Kicker4_Timer()
    Me.TimerEnabled = 0
    Flasher5.State = 0
    DOF dLowerRFlasher, 0
    'LightSeqFlasher.Play SeqBlinking, , 6, 60
    Kicker4.kick 230, 16
    PlaySoundAt "fx_Kicker", Kicker4
    DOF dKickerRight, 2
    PlaySound "z_heart"
End Sub

'*****************************
' Hidden castle back entrance
'*****************************

Sub Kicker6_Hit()
    PlaySoundAt "fx_Kicker-Enter", Kicker6
    DOF dSecretPassageFound, 2
    Kicker6.Destroyball:AddRampBall 1
If NOT Tilted Then
    DMD "_", "FOUND SECRET PASSAGE", "d_castle", eNone, eBlink, eNone, 2000, TRUE, "z_secret"
    If bSkillshotReady Then ' give skillshot or superskillshot!!!!
        LightSeqFlasher.Play SeqRandom, 4, , 4000
        bSkillshotReady = FALSE
        If LastSwitchHit = "TriggerV1" Then
            DMD CL("SKILLSHOT"), CL("50000"), "d_castle", eBlink, eBlinkFast, eNone, 2000, TRUE, "vo_skillshot"
            AddScore 50000
        End If
        If LastSwitchHit = "TriggerP2" Then
            DMD CL("SUPERSKILLSHOT"), CL("100000"), "d_castle", eBlink, eBlinkFast, eNone, 2000, TRUE, "vo_superskillshot"
            AddScore 100000
        End If
    Else
        LightSeqFlasher.Play SeqRandom, 4, , 2000
        AddScore 5000
        K6Count = K6Count + 1
        If K6Count = 5 Then
            DropA1.IsDropped = 0
            DOF dDropTargetA1, 2
            DropA2.IsDropped = 0
            DOF dDropTargetA2, 2
            LightJackpot1.State = 2
            DMD "_", CL("JACKPOT PREPARED"), "d_castle", eNone, eBlinkFast, eNone, 2000, TRUE, ""
            K6Count = 0
        End If
    End If
End If
End Sub

'*****************
' castle targets
'*****************

Sub TargetA_Hit()
    PlaySoundAt SoundFX("fx_target", DOFTargets), TargetA
    If NOT Tilted Then
        LightSeqRed.Play SeqBlinking, , 8, 30
        LightSD6.State = 1
        CheckSD
        LightSaveBall.State = 1
        LightMystery1.State = 0
        Addscore 1000
        PlaySound "z_pound"
        DOF dTargetA, 2
    End If
End Sub

Sub TargetB_Hit()
    PlaySoundAt SoundFX("fx_target", DOFTargets), TargetB
    If NOT Tilted Then
        LightSeqRed.Play SeqBlinking, , 8, 30
        DOF dTargetB, 2
        LightSD7.State = 1
        CheckSD
        LightSaveBall.State = 0
        LightMystery1.State = 1
        Addscore 1000
        PlaySound "z_pound"
    End If
End Sub

'***********************************************************************************
'****            				 	DOF add-on									****
'***********************************************************************************

Sub swShooterLane_Hit:DOF dBallInShooterLane, 1:End Sub
Sub swShooterLane_UnHit:DOF dBallInShooterLane, 0:End Sub
Sub Gate1_Hit():DOF dShooterLaneGate, 2:End Sub
Sub Gate3_Hit():DOF dTreasureRampGate, 2:End Sub

'***********************************************************************************
'****				       		DOF reference	 	     				    	****
'***********************************************************************************
Const dBallRelease = 101         ' ball release
Const dFlipperLeft = 102         ' flipper left
Const dFlipperRight = 103        ' flipper right
Const dSlingLeft = 104           ' slingshot left hit
Const dSlingRight = 105          ' slingshot right hit
Const dKnocker = 106             ' knocker fired
Const dBallInShooterLane = 111   ' ball in shooterlane
Const dShooterLaneGate = 112     ' shooterlane gate hit
Const dLeftInlane = 113          ' left inlane right
Const dLeftInlane1 = 114         ' left inlane left
Const dLeftOutlane = 115         ' reft outlane
Const dRightInlane = 116         ' right inlane
Const dRightOutlane = 117        ' right outlane
Const dAutoPlunger = 118         ' autoplunger in shooterlane
Const dAutoPlunger2 = 119        ' autoplunger in left outlane
Const dKickerRight = 120         ' right lower kicker
Const dDropTargetR1 = 121        ' lower left droptarget 1 (left)
Const dDropTargetR2 = 122        ' lower left droptarget 2 (center)
Const dDropTargetR3 = 123        ' lower left droptarget 3 (right)
Const dDropTargetR_Reset = 124   ' reset (raise) lower left droptargets
Const dDropTargetA1 = 125        ' upper left droptarget 1 down (in treasure ramp)
Const dDropTargetA2 = 125        ' upper left droptarget 2 down (in treasure ramp)
Const dDropTargetA_Reset = 126   ' reset (raise) upper left droptargets
Const dTargetN1 = 127            ' right red target 1
Const dTargetN2 = 128            ' right red target 2
Const dTreasureRampGate = 129    ' gate at the end of the treasure ramp
Const dSpinner = 130             ' left spinner
Const dTargetA = 131             ' top target left (near castle)
Const dTargetB = 132             ' top target right (near castle)
Const dDTargetA1 = 133           ' top droptarget Voloagia (attention -> only hit event)
Const dDTargetA2 = 134           ' top droptarget Ghoma (attention -> only hit event)
Const dDTargetA3 = 135           ' top droptarget Bongo (attention -> only hit event)
Const dDTargetA4 = 136           ' top droptarget Barinade (attention -> only hit event)
Const dDTargetA5 = 137           ' top droptarget Dodongo (attention -> only hit event)
Const dDTargetG1 = 138           ' top droptarget Ganondorf (attention -> only hit event)
Const dDTargetA_Change = 139     ' any status change (drop or raise) on the dDTargetA1-dDTargetA5 and dDTargetG1 drop targets
Const dTreasureRampTrigger = 140 ' star trigger in treasure ramp (purple) hit
Const dShooterLaneTrigger = 141  ' star trigger in shooter lane (green) hit
Const dRightCastleTrigger = 142  ' star trigger in right castle outlane (red) hit
Const dLeftCastleTrigger = 143   ' star trigger in left castle outlane (red) hit
Const dBehindCastleTrigger = 144 ' star trigger behind the castle (hidden - not visible) hit
Const dBlueTrigger1 = 145        ' most left blue star trigger in fron of catsle hit
Const dBlueTrigger2 = 146        ' blue star trigger in fron of catsle hit
Const dBlueTrigger3 = 147        ' blue star trigger in fron of catsle hit
Const dBlueTrigger4 = 148        ' blue star trigger in fron of catsle hit
Const dBlueTrigger5 = 149        ' blue star trigger in fron of catsle hit
Const dBlueTrigger6 = 150        ' most right blue star trigger in fron of catsle hit
Const dCastleGate = 161          ' popup gate at castle entry (fires on any status change)
Const dCastleDiverter = 162      ' diverter inside the castle (fires on any status change)
Const dCaptiveBall = 163         ' captive ball hit
Const dcbTarget = 164            ' target at the end of the captive ball lane hit
Const dCastleKickerRight = 165   ' Kicker right to the castle (kicks ball into right wire track)
Const dCastleKickerLeft = 166    ' Kicker left inside the castle (kicks ball into left wire track)
Const dSecretPassageFound = 170  ' kicker 6 hit -> displays "Secret Passage Found" on the DMD
Const dMultiBall = 171           ' multiball mode has started
Const dCreditButton = 172        ' credit buttons needs to be switched on
Const dCreditButtonBlink = 173   ' credit Button needs to blink
Const dStartButton = 174         ' start Button needs to be switched on
Const dStartButtonBlink = 175    ' start Button needs to blink
Const dJackpot = 176             ' Jackpot is payed
Const dUpperLLFlasher = 201      ' upper outer left flasher (red)
Const dUpperLFlasher = 202       ' upper center left flasher (green)
Const dUpperRFlasher = 203       ' upper center right flasher (red)
Const dUpperRRFlasher = 204      ' upper outer right flasher (green)
Const dLowerRFlasher = 205       ' lower right flasher (red)
Const dLowerLFlasher = 206       ' lower left flasher (green)