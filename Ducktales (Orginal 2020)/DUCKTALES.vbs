' ****************************************************************
'                       VISUAL PINBALL X
'                		Ducktales by remdwaas
'                       Version 1.1.0
'						pinking83 door last multiball fix
'						JPsalas fix moneybin/ catchhole
'						started 21-05-2020
'						Finished 11-6-2020
' ****************************************************************

'DOF Config by Outhere
'101 Left Flipper
'102 Right Flipper
'103 Left Slingshot
'104 
'105 Right Slingshot
'106 
'107 Bumper Left
'108 
'109 Bumper Right
'110 
'111 AutoPlunger
'112 hole 1 Kicker
'113 hole 2 Kicker
'114 hole 3 Kicker
'115 hole 4 Kicker
'116 Reset Drop 1,2,3
'117 Reset Drop 4,5,6
'118 See Table Script
'119 Spinner1
'120 Spinner2
'121 See Table Script
'122 Knocker
'123 Ball Release
'124 
'125 See Table Script
'126 Reset Monsters Drop Gates
'127 Piggybank hit  (Beacon)
'128 Treasure hit  (Strobe)
'129 Hit the Moneybin  (Beacon)
'130 Hole 4 Kicker
'131 Hole 5 Kicker

Option Explicit
Randomize

Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1
Const SongVolume = 0.1 ' 1 is full volume. Value is from 0 to 1

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
Const cGameName = "DUCKTALES"
Const TableName = "DUCKTALES"
Const myVersion = "1.0.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 3  ' limit to 3x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame =  5   ' usually 3 or 5
Const MaxMultiballs = 4  ' max number of balls during multiballs

Const Special1 = 1000000 ' High score to obtain an extra ball/game
Const Special2 = 3000000
Const Special3 = 5000000

' Use FlexDMD if in FS mode
Dim UseFlexDMD
If Table1.ShowDT = True then
    UseFlexDMD = False
Else
    UseFlexDMD = True
End If

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusMultiplier(4)
Dim bBonusHeld
Dim BallsRemaining(4)
Dim ExtraBallsAwards(4)
Dim Special1Awarded(4)
Dim Special2Awarded(4)
Dim Special3Awarded(4)
Dim Score(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim bAttractMode
Dim mBalls2Eject
Dim bAutoPlunger

' Define Game Control Variables
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
'Dim Multiball
Dim bMusicOn
Dim bJustStarted
Dim bJackpot
Dim plungerIM
dim targetsleft
dim targetsright1

' core.vbs variables

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
    Dim i
    'Randomize

'Reseths

    door1.IsDropped = True
    door2.IsDropped = True
    door3.IsDropped = True
    door4.IsDropped = True
    m1.IsDropped = True
    m2.IsDropped = True
    m3.IsDropped = True
    m4.IsDropped = True

    'Impulse Plunger as autoplunger
    Const IMPowerSetting = 36 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("fx_kicker", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

    ' load saved values, highscore, names, jackpot
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
    bFreePlay = False 'we want coins

    'if bFreePlay = false Then DOF 125, DOFOn

    ' Init main variables and any other flags
    bAttractMode = False
    bOnTheFirstBall = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bGameInPlay = False
    bMusicOn = True
    BallsOnPlayfield = 0
    bMultiBallMode = False
    'Multiball=false
    bAutoPlunger = False
    BallsInLock = 0
    BallsInHole = 0
	targetsleft = 0
	targetsright1 = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bJustStarted = True
    ' set any lights for the attract mode
    GiOff
    StartAttractMode
'EndOfGame()
End Sub

'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
    RollingUpdate
    ' add any other real time update subs, like gates or diverters
    FlipperLSh.Rotz = LeftFlipper.CurrentAngle
    FlipperRSh.Rotz = RightFlipper.CurrentAngle
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
    If Keycode = AddCreditKey Then
        Credits = Credits + 1
        if bFreePlay = False Then
            DOF 125, DOFOn
            If(Tilted = False)Then
                DMDFlush
                DMD "_", CL(1, "CREDITS: " & Credits), "", eNone, eNone, eNone, 500, True, "fx_coin"
            End If
        End If
    End If

    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
        PlaySoundAt "fx_reload", plunger
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "so_fanfare1"
                Else
                    If(Credits> 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
                        Credits = Credits - 1
                        DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "so_fanfare1"
                        If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True)Then
                    If(BallsOnPlayfield = 0)Then
                        ResetForNewGame()
                        UpdateMusicNow
                    End If
                Else
                    If(Credits> 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
                            UpdateMusicNow
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
                    End If
                End If
            End If
    End If ' If (GameInPlay)

'table keys
'If keycode = RightMagnaSave or keycode = LeftMagnasave Then ShowPost
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
        If bBallInPlungerLane Then PlaySoundAt "fx_fire", plunger
    End If

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific

    If bGameInPLay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then
            SolLFlipper 0
        End If
        If keycode = RightFlipperKey Then
            SolRFlipper 0
        End If
    End If
End Sub

'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub Table1_Exit
    Savehs
    If B2SOn = true Then Controller.Stop
End Sub

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        '		Flipper1.RotateToEnd 'Adds To End Movement for Flipper1
        RotateLaneLightsLeft
    'RotateLaneLightsLeft2
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
    '		Flipper1.RotateToStart 'Adds To End Movement for Flipper1
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RotateLaneLightsRight
    'RotateLaneLightsRight2
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, parm / 10, pan(ActiveBall), 0, Pitch(ActiveBall), 0, 0, AudioFade(ActiveBall)
End Sub

Sub RotateLaneLightsLeft
    Dim TempState
    TempState = LeftOutlane.State
    LeftOutlane.State = LeftInlane.State
    LeftInlane.State = RightInlane.State
    RightInlane.State = RightOutlane.State
    RightOutlane.State = TempState
End Sub

Sub RotateLaneLightsRight
    Dim TempState
    TempState = RightOutlane.State
    RightOutlane.State = RightInlane.State
    RightInlane.State = LeftInlane.State
    LeftInlane.State = LeftOutlane.State
    LeftOutlane.State = TempState
End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                  'Called when table is nudged
    Tilt = Tilt + TiltSensitivity              'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity)AND(Tilt <15)Then 'show a warning
        DMD "_", CL(1, "CAREFUL!"), "", eNone, eBlinkFast, eNone, 500, True, ""
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
        DMD "", "", "dmdtilt", eNone, eNone, eBlink, 200, False, ""
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
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        '       Bumper1.Force = 0
        '       Bumper2.Force = 0
        '		Bumper3.Force = 0
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
        LightSeqTilt.StopPlay
        '        Bumper1.Force = 8
        '        Bumper2.Force = 8
        '		Bumper3.Force = 8
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        DMDFlush
    End If
End Sub

' GI light sequence effects

Sub GiEffect(n)
    Select Case n
        Case 0 'all blink
            LightSeqGi.UpdateInterval = 8
            LightSeqGi.Play SeqBlinking, , 5, 50
        Case 1 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 5, , 1000
        Case 2 'upon
            LightSeqGi.UpdateInterval = 4
            LightSeqGi.Play SeqUpOn, 5, 1
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 'all blink
            LightSeqInserts.UpdateInterval = 8
            LightSeqInserts.Play SeqBlinking, , 5, 50
        Case 1 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 5, , 1000
        Case 2 'upon
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 10, 1
        Case 3 ' left-right-left
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqLeftOn, 10, 1
            LightSeqInserts.UpdateInterval = 5
            LightSeqInserts.Play SeqRightOn, 10, 1
    End Select
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

'********************
' Music as wav sounds
'********************

Dim Song, UpdateMusic
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
    If bMusicOn Then
        StopSound Song
        Song = ""
    End If
End Sub

Sub ChangeSong
    If(BallsOnPlayfield = 0)Then
        PlaySong "M_end"
        Exit Sub
    End If

    If bAttractMode Then
        PlaySong "M_end"
        Exit Sub
    End If
    If bMultiBallMode Then
        PlaySong ""
    Else
        UpdateMusicNow
    end if
End Sub

Sub UpdateMusicNow
    Select Case UpdateMusic
        Case 0:PlaySong "m_main"
        Case 1:PlaySong "Amazon"
        Case 2:PlaySong "egypt"
        Case 3:PlaySong "Africa"
        Case 4:PlaySong "Himalayas"
        Case 5:PlaySong "M_end"
    End Select
end sub

'********************
' Play random quotes
'********************

Sub PlayQuote1
    Dim tmp
    tmp = INT(RND * 50) + 1
    PlaySound "dh3_" &tmp
End Sub

Sub PlayQuote2
    Dim tmp
    tmp = INT(RND * 25) + 1
    PlaySound "bb_" &tmp
End Sub

Sub PlayQuote3
    Dim tmp
    tmp = INT(RND * 13) + 1
    PlaySound "ww_" &tmp
End Sub

Sub PlayQuote4
    Dim tmp
    tmp = INT(RND * 35) + 1
    PlaySound "mga_" &tmp
End Sub

Sub PlayQuote5
    Dim tmp
    tmp = INT(RND * 61) + 1
    PlaySound "scr_" &tmp
End Sub

Sub PlayQuote6
    Dim tmp
    tmp = INT(RND * 9) + 1
    PlaySound "rmp_" &tmp
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
        If UBound(tmp) = -1 Then '-1 means no balls, 0 is the first ball, 1 is the second...
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    DOF 118, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aBumperLights
        bulb.State = 1
    Next
' table1.ColorGradeImage = "ColorGradeLUT256x16_HalfSat"
End Sub

Sub GiOff
    DOF 118, DOFOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aBumperLights
        bulb.State = 0
    Next
' table1.ColorGradeImage = "ColorGradeLUT256x16_HalfSat-dark"
End Sub

' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 10, 10
        Case 4 'all blink once
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 4, 1
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 4 'up 1 time
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 8, 1
        Case 5 'up 2 times
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 8, 2
        Case 6 'down 1 time
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 8, 1
        Case 7 'down 2 times
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 8, 2
    End Select
End Sub

' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Vol(ball) ' Calculates the Volume of the sound based on the ball speed
    Vol = Csng(BallVel(ball) ^2 / 2000)
End Function

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp> 0 Then
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
    tmp = ball.y * 2 / Table1.height-1
    If tmp> 0 Then
        AudioFade = Csng(tmp ^10)
    Else
        AudioFade = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.06, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'********************************************
'   JP's VP10 Rolling Sounds + Ballshadow
' uses a collection of shadows, aBallShadow
'********************************************

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
    Dim BOT, b, ballpitch, ballvol
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball and draw the shadow
    For b = lob to UBound(BOT)
        aBallShadow(b).X = BOT(b).X
        aBallShadow(b).Y = BOT(b).Y

        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 25000 'increase the pitch on a ramp
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
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b)), 0, Pitch(BOT(b)), 1, 0, AudioFade(BOT(b))
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

    'resets the score display, and turn off attract mode
    StopAttractMode
    GiOn

    'reset Targets
    Target1.IsDropped = False
    Target2.IsDropped = False
    Target3.IsDropped = False
    Target4.IsDropped = False
    Target5.IsDropped = False
    Target6.IsDropped = False
	targetsleft = 0
	targetsright1 = 0

    'reset money
    money1.z = - 120
    money2.z = - 120
    PlaySoundAt SoundFXDOF("fx_resetdrop",116,DOFPulse,DOFDropTargets), Target3

    ' reset variables to 0
    DoorPos1 = 0
    DoorPos2 = 0
    DoorPos3 = 0
    DoorPos4 = 0
    MonsPos1 = 0
    MonsPos2 = 0
    MonsPos3 = 0
    MonsPos4 = 0
    MoneyPos = 0
    prun = 0
    pron = 0
    MagicL1 = 0
    MagicL2 = 0

    'dropes doors and monsters
    door1.IsDropped = True
    door2.IsDropped = True
    door3.IsDropped = True
    door4.IsDropped = True
    m1.IsDropped = True
    m2.IsDropped = True
    m3.IsDropped = True
    m4.IsDropped = True
    kluisdown.Enabled = 1

    'update music
    UpdateMusic = 0

	' reset lights
    TurnOffPlayfieldLights

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
    bOnTheFirstBall = True
    'Multiball=false
    For i = 1 To MaxPlayers
        Score(i) = 0
        BonusPoints(i) = 0
        'BonusHeldPoints(i) = 0
        BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
        ExtraBallsAwards(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        Special3Awarded(i) = False
    Next

    ' initialise any other flags
    Tilt = 0

    ' initialise Game variables
    Game_Init()

    ' you may wish to start some music, play a sound, do whatever at this point
    StopSong
    PlaySound ""

    vpmtimer.addtimer 100, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to

' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    ' make sure the correct display is upto date
    AddScore 0

    ' set the current players bonus multiplier back down to 1X
    BonusMultiplier(CurrentPlayer) = 1
    'UpdateBonusXLights

    ' reset any drop targets, lights, game Mode etc..

    'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'Reset any table specific
    Pigbonus = 0
    KluisbonusPoint = 0
    Beaglebonus = 0
    Nephewbonus = 0
    Moneybonus = 0
    Magicbonus = 0
    BumperBonus = 0
    HoleBonus = 0
    ResetNewBallVariables
    ResetNewBallLights()
'Multiball=false
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()

    'LightSeqAttract.StopPlay

    ' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4

    'only this tableDrain / Plunger Functions
    'ChangeBallImage

    If BallsOnPlayfield> 1 Then
        bMultiBallMode = True
        bAutoPlunger = True
    'ChangeSong
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

Sub EndOfBall()
    Dim BonusDelayTime
    ' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

    kluisdown.Enabled = 1

    kluiscontrolleTimer.enabled = True

    StopSong
    bonuscheckie
    'LightSeqAttract.Play SeqBlinking, , 5, 150

    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0

    if li2.state = 1 and li3.state = 1 and li4.state = 1 and li5.state = 1 Then
        'diamond ligths down
        li2.state = 0
        li3.state = 0
        li4.state = 0
        li5.state = 0

        'set light off doors
        Light47.state = 0
    end if

    'If NOT Tilted Then
    If(Tilted = False)Then
        PlaySound "totalbonus"
        DMD "", "", "dmdballlost", eNone, eNone, eNone, 1000, True, ""
        'Number of Target hits
        AwardPoints = TargetBonus * 2000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = BumperBonus * 100000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = Pigbonus * 25000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = KluisbonusPoint * 100000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = Beaglebonus * 25000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = Nephewbonus * 25000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = Moneybonus * 50000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = Magicbonus * 25000
        TotalBonus = TotalBonus + AwardPoints

        DMD CL(0, FormatScore(TotalBonus)), CL(1, "TOTAL BONUS " & " VIII" & BonusMultiplier(CurrentPlayer)), "", eBlinkFast, eNone, eNone, 1000, True, "po_bonus7"
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)

        AddScore TotalBonus

        ' add a bit of a delay to allow for the bonus points to be shown & added up
        vpmtimer.addtimer 3000, "EndOfBall2 '"
    Else 'Si hay falta simplemente espera un momento y va directo a la segunta parte después de perder la bola
        BonusDelayTime = 100
        EndOfBall2
    End If
'vpmtimer.addtimer BonusDelayTime, "EndOfBall2 '"
End Sub

sub kluiscontrolleTimer_timer()
    if Light1.state = 2 and Light49.state = 1 Then
        'Kluis.z = 0
        kluisbonusdone
        Light49.state = 0
    end if
end sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    UpdateMusicNow
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    kluiscontrolleTimer.enabled = False

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
        DMD "", "", "dmdextra", eNone, eNone, eNone, 1000, True, ""
        'DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, "vo_extraball"

        ' reset the playfield for the new ball
        ResetForNewPlayerBall()

        ' set the dropped wall for bonus

        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            'debug.print "No More Balls, High Score Entry"

            ' Submit the CurrentPlayers score to the High Score system
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
        ' (ie say from player 4 back to player 1)
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
        StopSong
        'DMD CL(0, "GAME OVER") "", eNone, 13000, True, ""
        DMD "", "", "dmdgameover", eNone, eNone, eNone, 7000, True, ""
        'DMD "", CL(1, "GAME OVER"), "", eNone, eNone, eNone, 7000, False, ""
        PlaySound "GAMEOVER"
        ' set the machine into game over mode
        vpmtimer.addtimer 7000, "EndOfGame() '"

    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        DMDScoreNow

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            PlaySound "vo_player" &CurrentPlayer
            DMD "_", CL(1, "PLAYER " &CurrentPlayer), "", eNone, eNone, eNone, 800, True, ""
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    LightSeqAttract.StopPlay
    'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then
        ChangeSong
    End If

    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball
    UpdateMusic = 0
    UpdateMusic = UpdateMusic + 5
    UpdateMusicNow
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
    BallsOnPlayfield = BallsOnPlayfield - 1

    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "fx_drain", Drain
    'if Tilted then end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' is the ball saver active,
        If(bBallSaverActive = True)Then
            AddMultiball 1
            bAutoPlunger = True
            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
            DMD CL(0, "BALL SAVED"), CL(1, "SHOOT AGAIN"), "", eBlink, eBlink, eNone, 800, True, ""
        'vpmtimer.addtimer 1250, "CreateNewBall() '"
'  DOF 111, DOFPulse
        ' you may wish to put something on a display or play a sound at this point

        Else

            If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
                    ChangeSong
                End If
            End If
            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0)Then

                ' End Mode and timers
                'StopSong
                'PlaySound "Death3"
                'vpmtimer.addtimer 3000, "ChangeSong '"
                ' Show the end of ball animation
                ' and continue with the end of ball
                ' DMD something?
                StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, since there is no animation then move to the end of ball
            End If
        End If
    End If
End Sub

' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub Trigger1_Hit()
    If bAutoPlunger Then
        'debug.print "autofire the ball"
        PlungerIM.AutoFire
        DOF 111, DOFPulse
        DOF 121, DOFPulse
        PlaySoundAt "fx_fire", Trigger1
        bAutoPlunger = False
    End If
    'StopSong
    DMDScoreNow
    bBallInPlungerLane = True
    DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eBlink, eNone, 1000, True, ""
    If(bBallSaverReady = True)AND(BallSaverTime <> 0)And(bBallSaverActive = False)Then
        EnableBallSaver BallSaverTime
    Else
        ' show the message to shoot the ball in case the player has fallen sleep
        Trigger1.TimerEnabled = 1
    End If
End Sub

' The ball is released from the plunger

Sub Trigger1_UnHit()
    bBallInPlungerLane = False
'LightEffect 4
'ChangeSong
End Sub

Sub Trigger1_Timer
    DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eNone, eNone, 800, True, ""
    trigger1.TimerEnabled = 0
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimer.Interval = 1000 * seconds
    BallSaverTimer.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimer_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimer.Enabled = False
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
    LightShootAgain.State = 0
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
Sub AddScore(points)
    If Tilted Then Exit Sub

    ' add the points to the current players score variable
    Score(CurrentPlayer) = Score(CurrentPlayer) + points

    ' play a sound for each score
    PlaySound "tone" &points

    ' you may wish to check to see if the player has gotten an extra ball by a high score
    If Score(CurrentPlayer) >= Special1 AND Special1Awarded(CurrentPlayer) = False Then
        AwardExtraBall
        Special1Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
        AwardExtraBall
        Special2Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
        AwardExtraBall
        Special3Awarded(CurrentPlayer) = True
    End If
End Sub

' Add bonus to the bonuspoints AND update the score board
Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If(Tilted = False)Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    End if
End Sub

Sub AwardExtraBall()
    DMD "", "", "dmdextra", eNone, eNone, eNone, 1000, True, SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
    'DMD "_", CL(1, ("EXTRA BALL WON") ), "", eNone, eBlink, eNone, 1000, True,
    DOF 121, DOFPulse
    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
    LightShootAgain.State = 1
    LightEffect 2
End Sub

'*****************************
'    Load / Save / Highscore
'*****************************

Sub Loadhs
    Dim x
    x = LoadValue(TableName, "HighScore1")
    If(x <> "")Then HighScore(0) = CDbl(x)Else HighScore(0) = 100000 End If
    x = LoadValue(TableName, "HighScore1Name")
    If(x <> "")Then HighScoreName(0) = x Else HighScoreName(0) = "AAA" End If
    x = LoadValue(TableName, "HighScore2")
    If(x <> "")then HighScore(1) = CDbl(x)Else HighScore(1) = 100000 End If
    x = LoadValue(TableName, "HighScore2Name")
    If(x <> "")then HighScoreName(1) = x Else HighScoreName(1) = "BBB" End If
    x = LoadValue(TableName, "HighScore3")
    If(x <> "")then HighScore(2) = CDbl(x)Else HighScore(2) = 100000 End If
    x = LoadValue(TableName, "HighScore3Name")
    If(x <> "")then HighScoreName(2) = x Else HighScoreName(2) = "CCC" End If
    x = LoadValue(TableName, "HighScore4")
    If(x <> "")then HighScore(3) = CDbl(x)Else HighScore(3) = 100000 End If
    x = LoadValue(TableName, "HighScore4Name")
    If(x <> "")then HighScoreName(3) = x Else HighScoreName(3) = "DDD" End If
    x = LoadValue(TableName, "Credits")
    If(x <> "")then Credits = CInt(x)Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
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

Sub Reseths
    HighScoreName(0) = "AAA"
    HighScoreName(1) = "BBB"
    HighScoreName(2) = "CCC"
    HighScoreName(3) = "DDD"
    HighScore(0) = 100000
    HighScore(1) = 110000
    HighScore(2) = 120000
    HighScore(3) = 130000
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
    tmp = Score(1)
    If Score(2)> tmp Then tmp = Score(2)
    If Score(3)> tmp Then tmp = Score(3)
    If Score(4)> tmp Then tmp = Score(4)

    'If tmp > HighScore(1)Then 'add 1 credit for beating the highscore
    '    Credits = Credits + 1
    '    DOF 125, DOFOn
    'End If

    If tmp> HighScore(3)Then
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
    ChangeSong
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

'    hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ'<>*+-/=\^0123456789`" ' ` is back arrow
	hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789<" ' < is back arrow JP FLEX FIX
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
        if(hsCurrentLetter> len(hsValidLetters))then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "`")then
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
            if(hsCurrentDigit> 0)then
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
    dLine(0) = ExpandLine(TempTopStr, 0)
    DMDUpdate 0

    TempBotStr = "    > "
    if(hsCurrentDigit> 0)then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3)then
        if(hsLetterFlash <> 0)then
            TempBotStr = TempBotStr & "_"
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1)then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2)then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <    "
    dLine(1) = ExpandLine(TempBotStr, 1)
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
    ChangeSong
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

' *************************************************************************
'   JP's Reduced Display Driver Functions (based on script by Black)
' only 5 effects: none, scroll left, scroll right, blink and blinkfast
' 3 Lines, treats all 3 lines as text. 3rd line is just 1 character
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

Dim dCharsPerLine(2)
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
            FlexDMD.TableFile = Table1.Filename & ".vpx"
            FlexDMD.RenderMode = 2
            FlexDMD.Width = 128
            FlexDMD.Height = 32
            FlexDMD.Clear = True
            FlexDMD.GameName = cGameName
            FlexDMD.Run = True
            Set DMDScene = FlexDMD.NewGroup("Scene")
            DMDScene.AddActor FlexDMD.NewImage("Back", "VPX.bkempty")
            DMDScene.GetImage("Back").SetSize FlexDMD.Width, FlexDMD.Height
            For i = 0 to 35
                DMDScene.AddActor FlexDMD.NewImage("Dig" & i, "VPX.dempty&dmd=2")
                Digits(i).Visible = False
            Next
            'digitgrid.Visible = False
            For i = 0 to 19 ' Top
                DMDScene.GetImage("Dig" & i).SetBounds 4 + i * 6, 3 + 16 + 2, 8, 8
            Next
            For i = 20 to 35 ' Bottom
                DMDScene.GetImage("Dig" & i).SetBounds ((i - 20) * 8), 3, 8, 16
            Next
            FlexDMD.LockRenderThread
            FlexDMD.Stage.AddActor DMDScene
            FlexDMD.UnlockRenderThread
        End If
    End If


'Sub DMD_Init() 'default/startup values
    Dim i, j
    DMDFlush()
    deSpeed = 20
    deBlinkSlowRate = 5
    deBlinkFastRate = 2
    dCharsPerLine(0) = 16 'characters lower line
    dCharsPerLine(1) = 20 'characters top line
    dCharsPerLine(2) = 1  'characters back line
    For i = 0 to 2
        dLine(i) = Space(dCharsPerLine(i) )
        deCount(i) = 0
        deCountEnd(i) = 0
        deBlinkCycle(i) = 0
        dqTimeOn(i) = 0
        dqbFlush(i) = True
        dqSound(i) = ""
    Next
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
    if(dqHead = dqTail) Then
        tmp = RL(0, FormatScore(Score(Currentplayer) ) )
        tmp1 = CL(1, "PLAYER " & CurrentPlayer & "  BALL " & Balls)
        tmp2 = "bkborder"
    End If
    DMD tmp, tmp1, tmp2, eNone, eNone, eNone, 25, True, ""
End Sub

Sub DMDScoreNow
    DMDFlush
    DMDScore
End Sub

Sub DMD(Text0, Text1, Text2, Effect0, Effect1, Effect2, TimeOn, bFlush, Sound)
    if(dqTail <dqSize) Then
        if(Text0 = "_") Then
            dqEffect(0, dqTail) = eNone
            dqText(0, dqTail) = "_"
        Else
            dqEffect(0, dqTail) = Effect0
            dqText(0, dqTail) = ExpandLine(Text0, 0)
        End If

        if(Text1 = "_") Then
            dqEffect(1, dqTail) = eNone
            dqText(1, dqTail) = "_"
        Else
            dqEffect(1, dqTail) = Effect1
            dqText(1, dqTail) = ExpandLine(Text1, 1)
        End If

        if(Text2 = "_") Then
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
        if(dqTail = 1) Then
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
            Case eScrollLeft:deCountEnd(i) = Len(dqText(i, dqHead) )
            Case eScrollRight:deCountEnd(i) = Len(dqText(i, dqHead) )
            Case eBlink:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
            Case eBlinkFast:deCountEnd(i) = int(dqTimeOn(dqHead) / deSpeed)
                deBlinkCycle(i) = 0
        End Select
    Next
    if(dqSound(dqHead) <> "") Then
        PlaySound(dqSound(dqHead) )
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
    if(dqHead = dqTail) Then
        if(dqbFlush(Head) = True) Then
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
        if(deCount(i) <> deCountEnd(i) ) Then
            deCount(i) = deCount(i) + 1

            select case(dqEffect(i, dqHead) )
                case eNone:
                    Temp = dqText(i, dqHead)
                case eScrollLeft:
                    Temp = Right(dLine(i), dCharsPerLine(i) - 1)
                    Temp = Temp & Mid(dqText(i, dqHead), deCount(i), 1)
                case eScrollRight:
                    Temp = Mid(dqText(i, dqHead), (dCharsPerLine(i) + 1) - deCount(i), 1)
                    Temp = Temp & Left(dLine(i), dCharsPerLine(i) - 1)
                case eBlink:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkSlowRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If

                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i) )
                    End If
                case eBlinkFast:
                    BlinkEffect = True
                    if((deCount(i) MOD deBlinkFastRate) = 0) Then
                        deBlinkCycle(i) = deBlinkCycle(i) xor 1
                    End If

                    if(deBlinkCycle(i) = 0) Then
                        Temp = dqText(i, dqHead)
                    Else
                        Temp = Space(dCharsPerLine(i) )
                    End If
            End Select

            if(dqText(i, dqHead) <> "_") Then
                dLine(i) = Temp
                DMDUpdate i
            End If
        End If
    Next

    if(deCount(0) = deCountEnd(0) ) and(deCount(1) = deCountEnd(1) ) and(deCount(2) = deCountEnd(2) ) Then

        if(dqTimeOn(dqHead) = 0) Then
            DMDFlush()
        Else
            if(BlinkEffect = True) Then
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

Function ExpandLine(TempStr, id) 'id is the number of the dmd line
    If TempStr = "" Then
        TempStr = Space(dCharsPerLine(id) )
    Else
        if(Len(TempStr)> Space(dCharsPerLine(id) ) ) Then
            TempStr = Left(TempStr, Space(dCharsPerLine(id) ) )
        Else
            if(Len(TempStr) <dCharsPerLine(id) ) Then
                TempStr = TempStr & Space(dCharsPerLine(id) - Len(TempStr) )
            End If
        End If
    End If
    ExpandLine = TempStr
End Function

Function FormatScore(ByVal Num) 'it returns a string with commas (as in Black's original font)
    dim i
    dim NumString

    NumString = CStr(abs(Num) )

    For i = Len(NumString) -3 to 1 step -3
        if IsNumeric(mid(NumString, i, 1) ) then
            NumString = left(NumString, i-1) & chr(asc(mid(NumString, i, 1) ) + 48) & right(NumString, Len(NumString) - i)
        end if
    Next
    FormatScore = NumString
End function

Function CL(id, NumString)
    Dim Temp, TempStr
    Temp = (dCharsPerLine(id) - Len(NumString) ) \ 2
    TempStr = Space(Temp) & NumString & Space(Temp)
    CL = TempStr
End Function

Function RL(id, NumString)
    Dim Temp, TempStr
    Temp = dCharsPerLine(id) - Len(NumString)
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
			For digit = 20 to 35
				DMDDisplayChar mid(dLine(0), digit-19, 1), digit
			Next
		Case 1 'bottom text line
			For digit = 0 to 19
				DMDDisplayChar mid(dLine(1), digit + 1, 1), digit
			Next
		Case 2 ' back image - back animations
			If dLine(2) = "" OR dLine(2) = " " Then dLine(2) = "bkempty"
			DigitsBack(0).ImageA = dLine(2)
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

'****************************
' JP's new DMD using flashers
'****************************

Dim Digits, DigitsBack, Chars(255), Images(255)

DMDInit

Sub DMDInit
    Dim i
    'If Table1.ShowDT = true then
        Digits = Array(digit0, digit1, digit2, digit3, digit4, digit5, digit6, digit7, digit8, digit9, digit10, digit11,                  _
            digit12, digit13, digit14, digit15, digit16, digit17, digit18, digit19, digit20, digit21, digit22, digit23, digit24, digit25, _
            digit26, digit27, digit28, digit29, digit30, digit31, digit32, digit33, digit34, digit35)
        DigitsBack = Array(digit36)

    For i = 0 to 255:Chars(i)  = "dempty":Next '= "dempty":Images(i) = "dempty":Next

    Chars(32) = "dempty"
    '    Chars(34) = '"
    '    Chars(36) = '$
    '    Chars(39) = ''
    '    Chars(42) = '*
    '    Chars(43) = '+
    '    Chars(45) = '-
    '    Chars(47) = '/
    Chars(48) = "d0"       '0
    Chars(49) = "d1"       '1
    Chars(50) = "d2"       '2
    Chars(51) = "d3"       '3
    Chars(52) = "d4"       '4
    Chars(53) = "d5"       '5
    Chars(54) = "d6"       '6
    Chars(55) = "d7"       '7
    Chars(56) = "d8"       '8
    Chars(57) = "d9"       '9
    Chars(60) = "dless"    '<
    Chars(61) = "dequal"   '=
    Chars(62) = "dgreater" '>
    '   Chars(64) = '@
    Chars(65) = "da" 'A
    Chars(66) = "db" 'B
    Chars(67) = "dc" 'C
    Chars(68) = "dd" 'D
    Chars(69) = "de" 'E
    Chars(70) = "df" 'F
    Chars(71) = "dg" 'G
    Chars(72) = "dh" 'H
    Chars(73) = "di" 'I
    Chars(74) = "dj" 'J
    Chars(75) = "dk" 'K
    Chars(76) = "dl" 'L
    Chars(77) = "dm" 'M
    Chars(78) = "dn" 'N
    Chars(79) = "do" 'O
    Chars(80) = "dp" 'P
    Chars(81) = "dq" 'Q
    Chars(82) = "dr" 'R
    Chars(83) = "ds" 'S
    Chars(84) = "dt" 'T
    Chars(85) = "du" 'U
    Chars(86) = "dv" 'V
    Chars(87) = "dw" 'W
    Chars(88) = "dx" 'X
    Chars(89) = "dy" 'Y
    Chars(90) = "dz" 'Z
    'Chars(91) = "dball" '[
    'Chars(92) = "dcoin" '|
    'Chars(93) = "dpika" ']
    '    Chars(94) = '^
    '    Chars(95) = '_
    Chars(96) = "d0a"  '0.
    Chars(97) = "d1a"  '1.
    Chars(98) = "d2a"  '2.
    Chars(99) = "d3a"  '3.
    Chars(100) = "d4a" '4.
    Chars(101) = "d5a" '5.
    Chars(102) = "d6a" '6.
    Chars(103) = "d7a" '7.
    Chars(104) = "d8a" '8.
    Chars(105) = "d9a" '9
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

' ********************************
'   Table info & Attract Mode
' ********************************

Sub ShowTableInfo
    Dim ii
    'info goes in a loop only stopped by the credits and the startkey
    If Score(1)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 1" &FormatScore(Score(1))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 2 " &FormatScore(Score(2))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 3 " &FormatScore(Score(3))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4)Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 4 " &FormatScore(Score(4))), "", eNone, eNone, eNone, 3000, False, ""
    End If
    DMD CL(0, "GAME OVER"), CL(1, "TRY AGAIN"), "", eNone, eBlink, eNone, 2000, True, ""
    If bFreePlay Then
        DMD "", CL(1, "FREE PLAY"), "", eNone, eNone, eNone, 2000, False, ""
    Else
        If Credits> 0 Then
            DMD CL(0, "CREDITS " & Credits), CL(1, "PRESS START"), "", eNone, eBlink, eNone, 2000, False, ""
        Else
            DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 2000, False, ""
        End If
    End If
    DMD "", "", "dmdintro1", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "dmdintro2", eNone, eNone, eNone, 4000, False, ""
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3)), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0)), Space(dCharsPerLine(1)), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
    ChangeSong
    StartLightSeq
    DMDFlush
    ShowTableInfo
End Sub

Sub StopAttractMode
    LightSeqAttract.StopPlay
    DMDScoreNow
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

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

' tables variables and Mode init
Dim HoleBonus, BumperBonus, ALLRampBonus, RampBonus1, RampBonus2, RampBonus3, MulitballBonus, TargetBonus, Pigbonus, KluisbonusPoint, Beaglebonus, Nephewbonus, Moneybonus, Magicbonus

Sub Game_Init() 'called at the start of a new game
    Dim i, j

    UpdateMusicNow
    Pigbonus = 0
    KluisbonusPoint = 0
    Beaglebonus = 0
    Nephewbonus = 0
    Moneybonus = 0
    Magicbonus = 0
    TargetBonus = 0
    bumperHits = 100
    BumperBonus = 0
    TurnOffPlayfieldLights()
End Sub

Sub StopEndOfBallMode()     'this sub is called after the last ball is drained
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
    Dim i
    TargetBonus = 0
    bBallSaverReady = True
End Sub

Sub ResetNewBallLights() 'turn on or off the needed lights before a new ball is released
    li13.State = 1
    li14.State = 1
    li15.State = 1
    li16.State = 1
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub should do something like this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in case it is needed later
' *********************************************************************

'************
' Slingshots
'************

Dim RStep, Lstep

Sub RightSlingShot_Slingshot
    '    If li001.State=1 then
    '	AddScore 210
    '	end if
    '	If li002.State=1 then
    '	AddScore 420
    '	end if
    PlaySound SoundFXDOF("right_slingshot", 105, DOFPulse, DOFContactors), 0, 1, 0.05, 0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
    'gi1.State = 0:Gi2.State = 0
    AddScore 210
    D2Shaker
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0 ':gi1.State = 1:Gi2.State = 1
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
    '	If li001.State=1 then
    '	AddScore 210
    '	end if
    '	If li002.State=1 then
    '	AddScore 420
    '	end if
    PlaySound SoundFXDOF("left_slingshot", 103, DOFPulse, DOFContactors), 0, 1, -0.05, 0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
    'gi3.State = 0:Gi4.State = 0
    AddScore 210
    D1Shaker
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0 'gi3.State = 1:Gi4.State = 1
    End Select
    LStep = LStep + 1
End Sub

'*****************
'triggers
'*****************

Sub TRIGL_Hit()
    PlaySound SoundFX("fx_metalrolling", DOFContactors), 0, 1, -0.05, 0.05
'PlaySound "fx_metalrolling"
end sub

Sub TRIGR_Hit()
    PlaySound SoundFX("fx_metalrolling", DOFContactors), 0, 1, 0.05, 0.05
'PlaySound "fx_metalrolling"
end sub

Sub RHelp1_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAt "fx_ballrampdrop", RHelp1
End Sub

Sub RHelp2_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAt "fx_ballrampdrop", RHelp2
End Sub

'for blinking lights
Sub TRL1_Hit
    li16.state = 2
    vpmtimer.addtimer 3000, "li16.state = 1 ' "
End Sub
Sub TRL2_Hit
    li13.state = 2
    vpmtimer.addtimer 3000, "li13.state = 1 ' "
End Sub
Sub TRL3_Hit
    li14.state = 2
    vpmtimer.addtimer 3000, "li14.state = 1 ' "
End Sub
Sub TRL4_Hit
    li15.state = 2
    vpmtimer.addtimer 3000, "li15.state = 1 ' "
End Sub
Sub TRL5_Hit
    PlayQuote6
End Sub
Sub TRL6_Hit
    PlayQuote5
End Sub

Sub TLeftInlane_Hit
    If li001.State = 1 then
        AddScore 5000
    end if
    If li002.State = 1 then
        AddScore 10000
    end if
    If LeftInlane.state = 0 then
        PlaySound "fx_sensor"
        AddScore 5000
        LeftInlane.state = 1
    Elseif LeftInlane.state = 0 then
        PlaySound "fx_sensor"
        AddScore 5000
    end if
    PlaySound "fx_sensor"
    CheckbBoys
End Sub

Sub TLeftOutlane_Hit
    If li001.State = 1 then
        AddScore 50000
    end if
    If li002.State = 1 then
        AddScore 100000
    end if
    If LeftOutlane.state = 0 then
        PlaySound "fx_sensor"
        AddScore 50000
        LeftOutlane.state = 1
    Elseif LeftOutlane.state = 0 then
        PlaySound "fx_sensor"
        AddScore 50000
    end if
    FlashForMs Flasher2, 1000, 50, 0
    PlaySound "Death3"
    CheckbBoys
End Sub

Sub TRightInlane_Hit
    If li001.State = 1 then
        AddScore 5000
    end if
    If li002.State = 1 then
        AddScore 10000
    end if
    If RightInlane.state = 0 then
        PlaySound "fx_sensor"
        AddScore 5000
        RightInlane.state = 1
    Elseif RightInlane.state = 0 then
        PlaySound "fx_sensor"
        AddScore 5000
    end if
    PlaySound "fx_sensor"
    CheckbBoys
End Sub

Sub TRightOutlane_Hit
    If li001.State = 1 then
        AddScore 50000
    end if
    If li002.State = 1 then
        AddScore 100000
    end if
    If RightOutlane.state = 0 then
        PlaySound "fx_sensor"
        AddScore 50000
        RightOutlane.state = 1
    Elseif RightOutlane.state = 0 then
        PlaySound "fx_sensor"
        AddScore 50000
    end if
    FlashForMs Flasher1, 1000, 50, 0
    PlaySound "Death3"
    CheckbBoys
End Sub

Sub CheckbBoys
    If(LeftInlane.State = 1)And(LeftOutlane.State = 1)And(RightInlane.State = 1)And(RightOutlane.State = 1)Then
        DMD "", "", "dmdboys", eNone, eNone, eNone, 1000, True, "2x"
        AddScore 50000
        LeftInlane.State = 0
        LeftOutlane.State = 0
        RightInlane.State = 0
        RightOutlane.State = 0
    End If
End Sub

Sub Bonuschecker_Hit
    FlashForMs Flasher1, 1000, 50, 0
    FlashForMs Flasher2, 1000, 50, 0
end sub

Sub bonuscheckie
    if li17.state = 1 then
        Pigbonus = 1
        li17.state = 0
    else
        Pigbonus = 0
    end if

    if li6.state = 1 then
        KluisbonusPoint = 1
        li6.state = 0
    else
        KluisbonusPoint = 0
    end if

    if li7.state = 1 then
        Beaglebonus = 1
        li7.state = 0
    else
        Beaglebonus = 0
    end if

    if Light48.state = 1 then
        Nephewbonus = 1
        Light48.state = 0
    else
        Nephewbonus = 0
    end if

    if li1.state = 1 then
        Moneybonus = 1
        li1.state = 0
    else
        Moneybonus = 0
    end if

    if Light46.state = 1 then
        Magicbonus = 1
        Light46.state = 0
    else
        Magicbonus = 0
    end if
End Sub

'**************************
'Bumpers
'**************************
Dim bumperHits, MagicL1, MagicL2

Sub Bumper1_hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 500
    end if
    If li002.State = 1 then
        AddScore 1000
    end if
    addscore 500
    bumperHits = bumperHits - 1
DOF  107,DOFPulse
    playsound "Jolt"
    MagicL1 = MagicL1 + 1
    UpdateMagicLights1
    checkMagicLights1
    cplas.Enabled = true
    CheckBumpers
end Sub

Sub Bumper2_hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 500
    end if
    If li002.State = 1 then
        AddScore 1000
    end if
    addscore 500
    bumperHits = bumperHits - 1
DOF  109,DOFPulse
    playsound "Jolt"
    MagicL2 = MagicL2 + 1
    UpdateMagicLights2
    checkMagicLights2
    cples.Enabled = true
    CheckBumpers
end Sub

Sub UpdateMagicLights1
    Select Case MagicL1
        Case 0:li022.State = 0:li021.State = 0:li020.State = 0:li019.State = 0:li018.State = 0
        Case 1:li022.State = 1:li021.State = 0:li020.State = 0:li019.State = 0:li018.State = 0
        Case 2:li022.State = 1:li021.State = 1:li020.State = 0:li019.State = 0:li018.State = 0
        Case 3:li022.State = 1:li021.State = 1:li020.State = 1:li019.State = 0:li018.State = 0
        Case 4:li022.State = 1:li021.State = 1:li020.State = 1:li019.State = 1:li018.State = 0
        Case 5:li022.State = 1:li021.State = 1:li020.State = 1:li019.State = 1:li018.State = 1
    End Select
end sub

Sub UpdateMagicLights2
    Select Case MagicL2
        Case 0:li023.State = 0:li024.State = 0:li025.State = 0:li026.State = 0:li027.State = 0
        Case 1:li023.State = 1:li024.State = 0:li025.State = 0:li026.State = 0:li027.State = 0
        Case 2:li023.State = 1:li024.State = 1:li025.State = 0:li026.State = 0:li027.State = 0
        Case 3:li023.State = 1:li024.State = 1:li025.State = 1:li026.State = 0:li027.State = 0
        Case 4:li023.State = 1:li024.State = 1:li025.State = 1:li026.State = 1:li027.State = 0
        Case 5:li023.State = 1:li024.State = 1:li025.State = 1:li026.State = 1:li027.State = 1
    End Select
end sub

Sub checkMagicLights1
    if(li022.state = 1)And(li021.state = 1)And(li020.state = 1)And(li019.state = 1)And(li018.state = 1)then
        DMD "", "", "dmdmagic", eNone, eBlink, eNone, 1000, True, ""
        AddScore 10000
        PlayQuote4
        Light46.state = 1
        li022.state = 0
        li021.state = 0
        li020.state = 0
        li019.state = 0
        li018.state = 0
        MagicL1 = 0
    end if
end sub

Sub checkMagicLights2
    if(li023.state = 1)And(li024.state = 1)And(li025.state = 1)And(li026.state = 1)And(li027.state = 1)then
        DMD "", "", "dmdmagic", eNone, eBlink, eNone, 1000, True, ""
        AddScore 10000
        PlayQuote4
        Light46.state = 1
        li023.state = 0
        li024.state = 0
        li025.state = 0
        li026.state = 0
        li027.state = 0
        MagicL2 = 0
    end if
end sub

' Bumper Bonus
' 100000 i bonus after each 100 hits

Sub CheckBumpers()
    If bumperHits <= 0 Then
        BumperBonus = BumperBonus + 1
        DMD "_", CL(1, "BUMPERS BONUS " & BumperBonus), "_", eNone, eBlink, eNone, 500, True, ""
        bumperHits = 100
    ' do something more
    End If
End Sub

Sub ResetBumpers()
    bumperHits = 100
End Sub

Dim pstep, prun
pstep = 1

Sub cplas_timer()
    prun = prun + 1
    If prun = 2500 Then
        cstub.enabled = True
        prun = 0
        me.enabled = 0
    End If
    Select Case pstep
        Case 1:DiscP.image = "plasma1":Light2.state = 1:pstep = 2
        Case 2:DiscP.image = "plasma2":Light2.state = 0:pstep = 3
        Case 3:DiscP.image = "plasma3":Light2.state = 1:pstep = 4
        Case 4:DiscP.image = "plasma4":Light2.state = 0:pstep = 5
        Case 5:DiscP.image = "plasma5":Light2.state = 1:pstep = 6
        Case 6:DiscP.image = "plasma6":Light2.state = 0:pstep = 7
        Case 7:DiscP.image = "plasma7":Light2.state = 1:pstep = 8
        Case 8:DiscP.image = "plasma8":Light2.state = 0:pstep = 9
        Case 9:DiscP.image = "plasma9":Light2.state = 1:pstep = 10
        Case 10:DiscP.image = "plasma10":Light2.state = 0:pstep = 1
    End Select
End Sub

Sub cstub_timer()
    cplas.enabled = False
    DiscP.image = "plasma_off_dark2"
    Light2.State = 0
    prun = 0
    vpmtimer.addtimer 100, "cstub.enabled = False '"
    me.enabled = 0
End Sub

Dim pstap, pron
pstap = 1

Sub cples_timer()
    pron = pron + 1
    If pron = 2500 Then
        cstob.enabled = True
        'vpmtimer.addtimer 2000, "cstob.enabled = True '"
        pron = 0
        me.enabled = 0
    End If
    Select Case pstap
        Case 1:DiscP1.image = "plasma1":Light3.state = 1:pstap = 2
        Case 2:DiscP1.image = "plasma2":Light3.state = 0:pstap = 3
        Case 3:DiscP1.image = "plasma3":Light3.state = 1:pstap = 4
        Case 4:DiscP1.image = "plasma4":Light3.state = 0:pstap = 5
        Case 5:DiscP1.image = "plasma5":Light3.state = 1:pstap = 6
        Case 6:DiscP1.image = "plasma6":Light3.state = 0:pstap = 7
        Case 7:DiscP1.image = "plasma7":Light3.state = 1:pstap = 8
        Case 8:DiscP1.image = "plasma8":Light3.state = 0:pstap = 9
        Case 9:DiscP1.image = "plasma9":Light3.state = 1:pstap = 10
        Case 10:DiscP1.image = "plasma10":Light3.state = 0:pstap = 1
    End Select
End Sub

Sub cstob_timer()
    cples.enabled = False
    DiscP1.image = "plasma_off_dark2"
    Light3.State = 0
    pron = 0
    vpmtimer.addtimer 100, "cstob.enabled = False '"
'me.enabled = 0
End Sub

'*****************
'Targets
'*****************
Sub Target7_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 and li23.State = 0 then
        li23.State = 1
        AddScore 2000
        PlaySound "fx_droptarget"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li001.State = 1 and li23.State = 1 then
        AddScore 2000
        PlaySound "fx_droptarget"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li002.State = 1 then
        AddScore 3000
        PlaySound "fx_droptarget"
        TargetBonus = TargetBonus + 1
        Exit Sub
    end if

    If li23.State = 1 Then
        PlaySound "fx_droptarget"
    else
        PlaySound "fx_droptarget"
        li23.State = 1
    end if
    AddScore 1000
    TargetBonus = TargetBonus + 1
    PlaySound "fx_droptarget"
    Checkb2x
    Checkb3x
End Sub

Sub Target8_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 and li22.State = 0 then
        li22.State = 1
        AddScore 2000
        PlaySound "fx_droptarget"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li001.State = 1 and li22.State = 1 then
        AddScore 2000
        PlaySound "fx_droptarget"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li002.State = 1 then
        AddScore 3000
        PlaySound "fx_droptarget"
        TargetBonus = TargetBonus + 1
        Exit Sub
    end if

    If li22.State = 1 Then
        PlaySound "fx_droptarget"
    else
        PlaySound "fx_droptarget"
        li22.State = 1
    end if
    AddScore 1000
    TargetBonus = TargetBonus + 1
    PlaySound "fx_droptarget"
    Checkb2x
    Checkb3x
End Sub

Sub Checkb2x
    If(li22.State = 1)Or(li23.State = 1)Then
        DMD "", "", "dmdbonus2", eNone, eNone, eNone, 1000, True, "2x"
        li001.State = 1
        vpmtimer.addtimer 60000, "li001.state = 0 ' "
        vpmtimer.addtimer 60000, "li23.State = 0 ' "
        vpmtimer.addtimer 60000, "li22.State = 0 ' "
    End If
End Sub

Sub Checkb3x
    If(li22.State = 1)And(li23.State = 1)Then
        DMD "", "", "dmdbonus3", eNone, eNone, eNone, 1000, True, "3x"
        li002.State = 1
        li001.State = 0
        vpmtimer.addtimer 60000, "li002.state = 0 ' "
        vpmtimer.addtimer 60000, "li23.State = 0 ' "
        vpmtimer.addtimer 60000, "li22.State = 0 ' "
    End If
End Sub

dim MoneyPos

sub TP1_Hit()
    If Tilted Then Exit Sub
'DOF here piggybank hit
DOF 127, DOFPulse
    If li001.State = 1 then
        AddScore 10000
    end if
    If li002.State = 1 then
        AddScore 20000
    end if
    PlaySoundAt "cash", Target3
    MoneyPos = MoneyPos + 1
	If MoneyPos > 5 Then MoneyPos = 5
    money1.z = -120 + 24 * MoneyPos
    money2.z = -120 + 24 * MoneyPos
    li17.state = 1
    Addscore 5000
    UpdateMoneyLights
    'playquote3
    TargetBonus = TargetBonus + 1
    checkmoney
    UpdateMoneyLights
End Sub

Sub UpdateMoneyLights
    Select Case MoneyPos
        Case 0:Light36.State = 0:Light37.State = 0:Light38.State = 0:Light39.State = 0:Light40.State = 0:Light41.State = 0:Light42.State = 0:Light43.State = 0:Light44.State = 0:Light45.State = 0:li8.state = 0:li9.state = 0:li10.state = 0:li11.state = 0:li12.state = 0
        Case 1:Light36.State = 1:Light37.State = 0:Light38.State = 0:Light39.State = 0:Light40.State = 0:Light41.State = 1:Light42.State = 0:Light43.State = 0:Light44.State = 0:Light45.State = 0:li8.state = 2:li9.state = 0:li10.state = 0:li11.state = 0:li12.state = 0
        Case 2:Light36.State = 1:Light37.State = 1:Light38.State = 0:Light39.State = 0:Light40.State = 0:Light41.State = 1:Light42.State = 1:Light43.State = 0:Light44.State = 0:Light45.State = 0:li8.state = 2:li9.state = 2:li10.state = 0:li11.state = 0:li12.state = 0
        Case 3:Light36.State = 1:Light37.State = 1:Light38.State = 1:Light39.State = 0:Light40.State = 0:Light41.State = 1:Light42.State = 1:Light43.State = 1:Light44.State = 0:Light45.State = 0:li8.state = 2:li9.state = 2:li10.state = 2:li11.state = 0:li12.state = 0
        Case 4:Light36.State = 1:Light37.State = 1:Light38.State = 1:Light39.State = 1:Light40.State = 0:Light41.State = 1:Light42.State = 1:Light43.State = 1:Light44.State = 1:Light45.State = 0:li8.state = 2:li9.state = 2:li10.state = 2:li11.state = 2:li12.state = 0
        Case 5:Light36.State = 1:Light37.State = 1:Light38.State = 1:Light39.State = 1:Light40.State = 1:Light41.State = 1:Light42.State = 1:Light43.State = 1:Light44.State = 1:Light45.State = 1:li8.state = 2:li9.state = 2:li10.state = 2:li11.state = 2:li12.state = 2
    End Select
end sub

Sub checkmoney
    If money1.z = 0 then
        li1.state = 1
        addscore 10000
        vpmtimer.addtimer 1000, "resetmoney '"
        Playsound "alarm"
        vpmtimer.addtimer 2000, "kluisbonus '"
    End if
'GiEffect 1
End sub

Sub kluisbonus
    if Light1.state = 2 Then
        exit Sub
    else
        DMD "", "", "dmdsafe", eNone, eNone, eNone, 2000, True, ""
        addscore 25000
        Playsound "ground"
        li6.state = 1
        kluisup.enabled = 1
        Light1.state = 2
        vpmtimer.addtimer 30000, "kluisbonusdone '"
    end if
End sub

sub kluisup_Timer
If Kluis.z < 100 Then
    Kluis.z = Kluis.z + 5
Else
    Light49.state = 1
    kluismuur.IsDropped = False
	kluisup.Enabled = 0
End If
end sub

'reset money
    money1.z = - 120
    money2.z = - 120
    PlaySoundAt SoundFXDOF("fx_resetdrop",116,DOFPulse,DOFDropTargets), Target3

sub kluismuur_hit()
    If Tilted Then Exit Sub
'DOF here hit the moneybin
DOF 128, DOFPulse
    If li001.State = 1 then
        DMD "", "", "dmdsafehit2", eNone, eNone, eNone, 1000, True, ""
        PlaySound "fx_droptarget"
        AddScore 20000
        vpmtimer.addtimer 500, "PlayQuote5 '"
    ElseIf li002.State = 1 then
        DMD "", "", "dmdsafehit3", eNone, eNone, eNone, 1000, True, ""
        PlaySound "fx_droptarget"
        AddScore 30000
        vpmtimer.addtimer 500, "PlayQuote5 '"
    else
        DMD "", "", "dmdsafehit", eNone, eNone, eNone, 1000, True, ""
        PlaySound "fx_droptarget"
        addscore 10000
        vpmtimer.addtimer 500, "PlayQuote5 '"
    end if
    FlashForMs Flasher1, 1000, 50, 0
    FlashForMs Flasher2, 1000, 50, 0
    Kluis.image = "rood"
    vpmtimer.addtimer 900, "kluisgold '"
end sub

Sub kluisgold
    Kluis.image = "gold3"
End Sub

Sub kluisbonusdone
    if Light1.state = 0 Then
        exit sub
    else
        Playsound "ground"
        kluisdown.enabled = 1
    end if
End sub

sub kluisdown_Timer
kluisup.enabled = 0
If Kluis.z > -150 then 
    Kluis.z = Kluis.z -5
Else
    Light49.state = 0
    Light1.state = 0
    kluismuur.IsDropped = True
	kluisdown.Enabled = 0
End If
end sub

Sub resetmoney
    MoneyPos = 0
    Light36.State = 0
    Light37.State = 0
    Light38.State = 0
    Light39.State = 0
    Light40.State = 0
    Light41.State = 0
    Light42.State = 0
    Light43.State = 0
    Light44.State = 0
    Light45.State = 0
    li8.state = 0
    li9.state = 0
    li10.state = 0
    li11.state = 0
    li12.state = 0
    money1.z = - 120
    money2.z = - 120
    PlaySoundAt SoundFXDOF("fx_resetdrop",116,DOFPulse,DOFDropTargets), Target3
end sub

Sub Target1_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    PlaySoundAt "fx_droptarget", Target1
    li012.state = 1
	targetsleft = targetsleft + 1
    Addscore 1000
    playquote1
    TargetBonus = TargetBonus + 1
    checkT123
End Sub

Sub Target2_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    PlaySoundAt "fx_droptarget", Target2
    li013.state = 1
	targetsleft = targetsleft + 1
    Addscore 1000
    playquote1
    TargetBonus = TargetBonus + 1
    checkT123
End Sub

Sub Target3_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    PlaySoundAt "fx_droptarget", Target3
    li014.state = 1
	targetsleft = targetsleft + 1    
	Addscore 1000
    playquote1
    TargetBonus = TargetBonus + 1
    checkT123
End Sub

Sub Target4_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    PlaySoundAt "fx_droptarget", Target4
    li015.state = 1
	targetsright1 = targetsright1 + 1
    Addscore 1000
    playquote2
    TargetBonus = TargetBonus + 1
    checkT456
End Sub

Sub Target5_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    PlaySoundAt "fx_droptarget", Target5
    li016.state = 1
	targetsright1 = targetsright1 + 1
    Addscore 1000
    playquote2
    TargetBonus = TargetBonus + 1
    checkT456
End Sub

Sub Target6_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    PlaySoundAt "fx_droptarget", Target6
    li017.state = 1
	targetsright1 = targetsright1 + 1
    Addscore 1000
    playquote2
    TargetBonus = TargetBonus + 1
    checkT456
End Sub

Sub checkT123
    if targetsleft = 3 then
        DMD "", "", "dmdnephew", eNone, eNone, eNone, 1000, True, "2x"
        'DMD "_", CL(0, "NEPHEW BONUS"), "_", eNone, eBlink, eNone, 3000, True, ""
        Light48.State = 1
        AddScore 10000
        li012.state = 0
        li013.state = 0
        li014.state = 0
        vpmtimer.addtimer 1000, "resett123 '"
    End If
End Sub

Sub resett123
    Target1.IsDropped = False
    Target2.IsDropped = False
    Target3.IsDropped = False
    PlaySoundAt SoundFXDOF("fx_resetdrop",116,DOFPulse,DOFDropTargets), Target3
	targetsleft = 0
end sub

Sub checkT456
    if targetsright1 = 3then
        DMD "", "", "dmdbeagle", eNone, eNone, eNone, 1000, True, "2x"
        'DMD "_", CL(0, "BEAGLE BONUS"), "_", eNone, eBlink, eNone, 3000, True, ""
        li7.State = 1
        AddScore 10000
        li015.state = 0
        li016.state = 0
        li017.state = 0
        vpmtimer.addtimer 1000, "resett456 '"
    End If
'GiEffect 1
End Sub

Sub resett456
    Target4.IsDropped = False
    Target5.IsDropped = False
    Target6.IsDropped = False
    PlaySoundAt SoundFXDOF("fx_resetdrop",117,DOFPulse,DOFDropTargets), Target6
	targetsright1 = 0
end sub

'*****************
'Kickers
'*****************
Sub hole1_Hit()
    If Tilted Then
        hole1.Kick 180, 1
    end if
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    addScore 1000
    PlaySound "fx_popper"
    vpmtimer.addtimer 100, "hole1.Kick 180, 1 ' "
End Sub

Sub hole2_Hit()
    If Tilted Then
        hole2.Kick 180, 1
    end if
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    addScore 1000
    PlaySound "fx_popper"
    vpmtimer.addtimer 100, "hole2.Kick 180, 1 ' "
End Sub

Sub hole4_Hit()
DOF 130, DOFPulse  '  DOF Kicker  (Time Delay added to DOF Website)
    If Tilted Then
        hole4.Kick 270, 5, 1
    end if
    If li001.State = 1 then
        AddScore 25000
    end if
    If li002.State = 1 then
        AddScore 50000
    end if
    addScore 25000
    PlaySound "fx_popper"
    DMD "", "", "dmdreturn", eNone, eNone, eNone, 1000, True, ""
    playquote3
    vpmtimer.addtimer 1005, "hole4.Kick 270, 5, 1 ' "
End Sub
Sub hole5_Hit()
DOF 131, DOFPulse  '  DOF Kicker  (Time Delay added to DOF Website)
    If Tilted Then
        hole5.Kick 90, 5, 1
    end if
    If li001.State = 1 then
        AddScore 25000
    end if
    If li002.State = 1 then
        AddScore 50000
    end if
    addScore 25000
    PlaySound "fx_popper"
    DMD "", "", "dmdrente", eNone, eNone, eNone, 1000, True, ""
    playquote3
    vpmtimer.addtimer 1005, "hole5.Kick 90, 5, 1 ' "
End Sub

Sub Kicker1_Hit()
    If Tilted Then
        Kicker1.Kick 195, 5
    end if
    If li001.State = 1 then
        AddScore 20000
    end if
    If li002.State = 1 then
        AddScore 30000
    end if
    if li5.state = 1 Then
        DMD "", "", "dmdtaken", eNone, eNone, eNone, 1000, True, ""
        vpmtimer.addtimer 500, "Kicker1.Kick 195,5 ' "
        PlaySound "flt_1"
        addScore 10000
    end if
    if li5.state = 0 Then
'DOF HERE treasure hit
DOF 127, DOFPulse
        addScore 100000
        DMDFlush
        StopSong
        li5.state = 1
        li18.state = 1
        PlaySound "treasure3"
        DMD "", "", "dmdfly", eNone, eNone, eNone, 1000, True, ""
        vpmtimer.addtimer 1100, "vliegtuig '"
        vpmtimer.addtimer 5600, "purple ' "
        vpmtimer.addtimer 9000, "Kicker1.Kick 195,5 ' "
        vpmtimer.addtimer 9005, "UpdateMusicNow ' "
        vpmtimer.addtimer 9005, "AddMultiball 3 ' "
        vpmtimer.addtimer 9605, "checkjackpot ' "
    end if
    PlaySound "fx_popper":
End Sub

Sub Kicker2_Hit()
    If Tilted Then
        Kicker2.Kick 15, 14
    end if
    If li001.State = 1 then
        AddScore 20000
    end if
    If li002.State = 1 then
        AddScore 30000
    end if
    addScore 10000
    if li4.state = 1 Then
        DMD "", "", "dmdtaken", eNone, eNone, eNone, 1000, True, ""
        vpmtimer.addtimer 500, "Kicker2.Kick 15,14 ' "
        PlaySound "flt_1"
        addScore 10000
    end if
    if li4.state = 0 Then
'DOF HERE treasure hit
        addScore 100000
        DMDFlush
        StopSong
        li4.state = 1
        li19.state = 1
        PlaySound "treasure3"
        DMD "", "", "dmdfly", eNone, eNone, eNone, 1000, True, ""
        vpmtimer.addtimer 1100, "vliegtuig '"
        vpmtimer.addtimer 5600, "green ' "
        vpmtimer.addtimer 9000, "Kicker2.Kick 15,14 ' "
        vpmtimer.addtimer 9005, "UpdateMusicNow ' "
        vpmtimer.addtimer 9005, "AddMultiball 3 ' "
        vpmtimer.addtimer 9605, "checkjackpot ' "
    end if
    PlaySound "fx_popper"
End Sub

Sub Kicker3_Hit()
    If Tilted Then
        Kicker3.Kick 95, 5
    end if
    If li001.State = 1 then
        AddScore 20000
    end if
    If li002.State = 1 then
        AddScore 30000
    end if
    addScore 10000
    if li3.state = 1 Then
        DMD "", "", "dmdtaken", eNone, eNone, eNone, 1000, True, ""
        vpmtimer.addtimer 500, "Kicker3.Kick 95,5 ' "
        PlaySound "flt_1"
        addScore 10000
    end if
    if li3.state = 0 Then
'DOF HERE treasure hit
        addScore 100000
        DMDFlush
        StopSong
        li3.state = 1
        li21.state = 1
        PlaySound "treasure3"
        DMD "", "", "dmdfly", eNone, eNone, eNone, 1000, True, ""
        vpmtimer.addtimer 1100, "vliegtuig '"
        vpmtimer.addtimer 5600, "red ' "
        vpmtimer.addtimer 9000, "Kicker3.Kick 95,5 ' "
        vpmtimer.addtimer 9005, "UpdateMusicNow ' "
        vpmtimer.addtimer 9005, "AddMultiball 3 ' "
        vpmtimer.addtimer 9605, "checkjackpot ' "
    end if
    PlaySound "fx_popper"
End Sub

Sub Kicker4_Hit()
    If Tilted Then
        Kicker4.Kick 270, 14
    end if
    If li001.State = 1 then
        AddScore 20000
    end if
    If li002.State = 1 then
        AddScore 30000
    end if
    addScore 10000
    if li2.state = 1 Then
        DMD "", "", "dmdtaken", eNone, eNone, eNone, 1000, True, ""
        PlaySound "flt_1"
        addScore 10000
        vpmtimer.addtimer 500, "Kicker4.Kick 270,14 ' "
    end if
    if li2.state = 0 Then
'DOF HERE treasure hit
        addScore 100000
        DMDFlush
        StopSong
        li2.state = 1
        li20.state = 1
        PlaySound "treasure3"
        DMD "", "", "dmdfly", eNone, eNone, eNone, 1000, True, ""
        vpmtimer.addtimer 1100, "vliegtuig '"
        vpmtimer.addtimer 5600, "yellow ' "
        vpmtimer.addtimer 9000, "Kicker4.Kick 270,14 ' "
        vpmtimer.addtimer 9005, "UpdateMusicNow ' "
        vpmtimer.addtimer 9005, "AddMultiball 3 ' "
        vpmtimer.addtimer 9605, "checkjackpot ' "
    end if
    PlaySound "fx_popper"
End Sub

sub checkjackpot
    if li2.state = 1 and li3.state = 1 and li4.state = 1 and li5.state = 1 Then
        playsound ""
        Jackpot
        vpmtimer.addtimer 1905, "jackpotreset ' "
    end if
end sub

sub vliegtuig
    DMD "", "", "fl1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl5", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl6", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl7", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl8", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl9", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl10", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl11", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl12", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl13", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl14", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl15", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl16", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl17", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl18", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl19", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl20", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl21", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl22", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl23", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl24", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl25", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl26", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl27", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl28", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl29", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl30", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl31", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl32", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl33", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl34", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl35", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl36", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl37", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl38", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl39", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl40", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "fl41", eNone, eNone, eNone, 100, True, ""
end sub

Sub yellow
    DMD "", "", "dmdyellow", eNone, eNone, eNone, 2700, True, ""
end sub

Sub green
    DMD "", "", "dmdgreen", eNone, eNone, eNone, 2700, True, ""
end sub

Sub red
    DMD "", "", "dmdred", eNone, eNone, eNone, 2700, True, ""
end sub

Sub purple
    DMD "", "", "dmdpurple", eNone, eNone, eNone, 2700, True, ""
end sub

'***************************
' Catch Hole
'***************************

Dim BallInHole ', HolePos

Dim aBall

Sub CatchHole_Timer
    Do While aBall.Z> 0
        aBall.Z = aBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
End Sub

Sub CatchHole_hit()
    If Light47.state = 0 Then
        PlaySound "Ddown2":DOF 126, DOFPulse  '  DOF Reset Monsters Drop Gates
        Light47.state = 1
        door1.IsDropped = False
        door2.IsDropped = False
        door3.IsDropped = False
        door4.IsDropped = False
    end if
    'm2.IsDropped = True

    'GI1.state = 0
    'GI2.state = 0
    'FlashForMs Flasher5, 1000, 50, 0
    PlaySoundAt "fx_hole_enter", CatchHole
    BallInHole = BallInHole + 1
    Set aBall = ActiveBall:Me.TimerEnabled = 1
    StartSlotmachine
End Sub

Sub CatchHoleExit()
    If BallInHole> 0 Then
        BallInHole = BallInHole - 1
        Select Case SlotValue
            Case 0
                hole1.CreateSizedball BallSize / 2
                PlaySoundAt SoundFXDOF("fx_popper", 112, DOFPulse, DOFContactors), hole1
                DOF 121, DOFPulse
                hole1.Kick 180, 1
                vpmtimer.AddTimer 1000, "li041.state = 0 '"
            Case 1
                hole2.CreateSizedball BallSize / 2
                PlaySoundAt SoundFXDOF("fx_popper", 113, DOFPulse, DOFContactors), hole2
                DOF 121, DOFPulse
                hole2.Kick 180, 1
                vpmtimer.AddTimer 1000, "li042.state = 0 '"
            Case 2
                hole4.CreateSizedball BallSize / 2
                PlaySoundAt SoundFXDOF("fx_popper", 114, DOFPulse, DOFContactors), hole4
                DOF 121, DOFPulse
                hole4.Kick 270, 5, 1
                vpmtimer.AddTimer 1000, "li044.state = 0 '"
            case 3
                hole5.CreateSizedball BallSize / 2
                PlaySoundAt SoundFXDOF("fx_popper", 115, DOFPulse, DOFContactors), hole5
                DOF 121, DOFPulse
                hole5.Kick 90, 5, 1
                vpmtimer.AddTimer 1000, "li045.state = 0 '"
        End Select
        vpmtimer.addtimer 1000, "CatchHoleExit '" 'repeat until all the balls are kicked out
    End If
End Sub

'**************
' SlotMachine
'**************

Dim SlotAward, SlotValue

SlotAward = Array("a-hole1", "a-hole2", "a-hole4", "a-hole5")
SlotValue = 0

Sub StartSlotmachine() ' uses the HolePos variable
    Dim i
    DMDFlush
    For i = 0 to 3
        DMD "", "", SlotAward(i), eNone, eNone, eNone, 50, False, "fx_spinner"
    Next
    'DOF 142, DOFPulse
    vpmtimer.AddTimer 500, "GiveSlotAward '"
End Sub

Sub GiveSlotAward()
    DMDFlush
    SlotValue = INT(RND * 4)
    DMD "", "", SlotAward(SlotValue), eNone, eNone, eNone, 500, True, "takeoff2"

    Select Case SlotValue
        Case 0:li041.state = 2 'lights hole1
        Case 1:li042.state = 2 'lights hole2
        Case 2:li044.state = 2 'lights hole4
        Case 3:li045.state = 2 'lights hole5
    End Select
    'GiEffect 1
    vpmtimer.addtimer 500, "CatchHoleExit '"

End Sub

'**************
' Doors
'**************

Dim DoorPos1, DoorPos2, DoorPos3, DoorPos4

Sub door1_hit()
    If Tilted Then Exit Sub
'DOF HERE (an x in the middle or lines that goes from middle to the sides)
    If Light6.state = 1 Then
        playsound "boing"
        exit Sub
    end if
    If Light20.state = 1 Then
        playsound "boing"
        exit Sub
    end if
    If Light27.state = 1 Then
        playsound "boing"
        exit Sub
    end if
    DoorPos1 = DoorPos1 + 1
    UpdateDoorLights1
    playsound "Gate_slam"
    if Light11.state = 1 and Light12.state = 1 Then
        UpdateMusic = 0
        Light13.state = 1
        playsound "Unlock"
        door1.IsDropped = True
        m1.IsDropped = False
        StopSong
        UpdateMusic = UpdateMusic + 1
        UpdateMusicNow
    end if
    AddScore 500
    FlashForMs Flasher2, 1000, 50, 0
end sub

Sub UpdateDoorLights1
    Select Case DoorPos1
        Case 0:Light11.State = 0:Light12.State = 0
        Case 1:Light11.State = 1:Light12.State = 0
        Case 2:Light11.State = 1:Light12.State = 1
    End Select
end sub

Sub door2_hit()
    If Tilted Then Exit Sub
'DOF HERE (an x in the middle or lines that goes from middle to the sides)
    If Light13.state = 1 Then
        playsound "boing"
        exit Sub
    end if
    If Light20.state = 1 Then
        playsound "boing"
        exit Sub
    end if
    If Light27.state = 1 Then
        playsound "boing"
        exit Sub
    end if
    DoorPos2 = DoorPos2 + 1
    UpdateDoorLights2
    playsound "Gate_slam"
    if Light4.state = 1 and Light5.state = 1 Then
        UpdateMusic = 0
        Light6.state = 1
        playsound "Unlock"
        door2.IsDropped = True
        m2.IsDropped = False
        StopSong
        UpdateMusic = UpdateMusic + 2
        UpdateMusicNow
    end if
    AddScore 500
    FlashForMs Flasher2, 1000, 50, 0
end sub

Sub UpdateDoorLights2
    Select Case DoorPos2
        Case 0:Light4.State = 0:Light5.State = 0
        Case 1:Light4.State = 1:Light5.State = 0
        Case 2:Light4.State = 1:Light5.State = 1
    End Select
end sub

Sub door3_hit()
    If Tilted Then Exit Sub
'DOF HERE (an x in the middle or lines that goes from middle to the sides)
    If Light13.state = 1 Then
        playsound "boing"
        exit Sub
    end if
    If Light6.state = 1 Then
        playsound "boing"
        exit Sub
    end if
    If Light27.state = 1 Then
        playsound "boing"
        exit Sub
    end if
    DoorPos3 = DoorPos3 + 1
    UpdateDoorLights3
    playsound "Gate_slam"
    if Light18.state = 1 and Light19.state = 1 Then
        UpdateMusic = 0
        Light20.state = 1
        playsound "Unlock"
        door3.IsDropped = True
        m3.IsDropped = False
        StopSong
        UpdateMusic = UpdateMusic + 3
        UpdateMusicNow
    end if
    AddScore 500
    FlashForMs Flasher1, 1000, 50, 0
end sub

Sub UpdateDoorLights3
    Select Case DoorPos3
        Case 0:Light18.State = 0:Light19.State = 0
        Case 1:Light18.State = 1:Light19.State = 0
        Case 2:Light18.State = 1:Light19.State = 1
    End Select
end sub

Sub door4_hit()
    If Tilted Then Exit Sub
'DOF HERE (an x in the middle or lines that goes from middle to the sides)
    If Light13.state = 1 Then
        playsound "boing"
        exit Sub
    end if
    If Light6.state = 1 Then
        playsound "boing"
        exit Sub
    end if
    If Light20.state = 1 Then
        playsound "boing"
        exit Sub
    end if
    DoorPos4 = DoorPos4 + 1
    UpdateDoorLights4
    playsound "Gate_slam"

    if Light25.state = 1 and Light26.state = 1 Then
        Light27.state = 1
        UpdateMusic = 0
        playsound "Unlock"
        door4.IsDropped = True
        m4.IsDropped = False
        StopSong
        UpdateMusic = UpdateMusic + 4
        UpdateMusicNow
    end if
    AddScore 500
    FlashForMs Flasher1, 1000, 50, 0
end sub

Sub UpdateDoorLights4
    Select Case DoorPos4
        Case 0:Light25.State = 0:Light26.State = 0
        Case 1:Light25.State = 1:Light26.State = 0
        Case 2:Light25.State = 1:Light26.State = 1

    End Select
end sub

'**************
' Monsters
'**************

Dim MonsPos1, MonsPos2, MonsPos3, MonsPos4

Sub m1_hit()
    If Tilted Then Exit Sub
'DOF HERE (something special an moster gets hit)
    MonsPos1 = MonsPos1 + 1
    UpdateMonsLights1
    if Light14.state = 1 and Light15.state = 1 and Light16.state = 1 Then
        Light17.state = 1
        DMD "", "", "dmdm1", eNone, eNone, eNone, 1000, True, ""
        addscore 50000
        playsound "Unlock"
        m1.IsDropped = True
        Light32.state = 1
        li20.state = 2
        StopSong
        UpdateMusic = 0
        UpdateMusicNow
    end if
    playsound "Hit"
    FlashForMs Flasher2, 1000, 50, 0
end sub

Sub UpdateMonsLights1
    Select Case MonsPos1
        Case 0:Light14.State = 0:Light15.State = 0:Light16.State = 0
        Case 1:Light14.State = 1:Light15.State = 0:Light16.State = 0
        Case 2:Light14.State = 1:Light15.State = 1:Light16.State = 0
        Case 3:Light14.State = 1:Light15.State = 1:Light16.State = 1:Light13.State = 0
    End Select
end sub

Sub m2_hit()
    If Tilted Then Exit Sub
'DOF HERE (something special an moster gets hit)
    MonsPos2 = MonsPos2 + 1
    UpdateMonsLights2
    if Light7.state = 1 and Light8.state = 1 and Light9.state = 1 Then
        Light10.state = 1
        DMD "", "", "dmdm2", eNone, eNone, eNone, 1000, True, ""
        addscore 50000
        playsound "Unlock"
        m2.IsDropped = True
        Light33.state = 1
        li18.state = 2
        StopSong
        UpdateMusic = 0
        UpdateMusicNow
    end if
    playsound "Hit"
    FlashForMs Flasher2, 1000, 50, 0
end sub

Sub UpdateMonsLights2
    Select Case MonsPos2
        Case 0:Light7.State = 0:Light8.State = 0:Light9.State = 0
        Case 1:Light7.State = 1:Light8.State = 0:Light9.State = 0
        Case 2:Light7.State = 1:Light8.State = 1:Light9.State = 0
        Case 3:Light7.State = 1:Light8.State = 1:Light9.State = 1:Light6.State = 0
    End Select
end sub

Sub m3_hit()
    If Tilted Then Exit Sub
'DOF HERE (something special an moster gets hit)
    MonsPos3 = MonsPos3 + 1
    UpdateMonsLights3
    if Light21.state = 1 and Light22.state = 1 and Light23.state = 1 Then
        Light24.state = 1
        DMD "", "", "dmdm3", eNone, eNone, eNone, 1000, True, ""
        addscore 50000
        playsound "Unlock"
        m3.IsDropped = True
        Light34.state = 1
        li19.state = 2
        StopSong
        UpdateMusic = 0
        UpdateMusicNow
    end if
    playsound "Hit"
    FlashForMs Flasher1, 1000, 50, 0
end sub

Sub UpdateMonsLights3
    Select Case MonsPos3
        Case 0:Light21.State = 0:Light22.State = 0:Light23.State = 0
        Case 1:Light21.State = 1:Light22.State = 0:Light23.State = 0
        Case 2:Light21.State = 1:Light22.State = 1:Light23.State = 0
        Case 3:Light21.State = 1:Light22.State = 1:Light23.State = 1:Light20.State = 0
    End Select
end sub

Sub m4_hit()
    If Tilted Then Exit Sub
'DOF HERE (something special an moster gets hit)
    MonsPos4 = MonsPos4 + 1
    UpdateMonsLights4
    if Light28.state = 1 and Light29.state = 1 and Light30.state = 1 Then
        Light31.state = 1
        DMD "", "", "dmdm4", eNone, eNone, eNone, 1000, True, ""
        addscore 50000
        playsound "Unlock"
        m4.IsDropped = True
        Light35.state = 1
        li21.state = 2
        StopSong
        UpdateMusic = 0
        UpdateMusicNow
    end if
    playsound "Hit"
    FlashForMs Flasher1, 1000, 50, 0
end sub

Sub UpdateMonsLights4
    Select Case MonsPos4
        Case 0:Light28.State = 0:Light29.State = 0:Light30.State = 0
        Case 1:Light28.State = 1:Light29.State = 0:Light30.State = 0
        Case 2:Light28.State = 1:Light29.State = 1:Light30.State = 0
        Case 3:Light28.State = 1:Light29.State = 1:Light30.State = 1:Light27.State = 0
    End Select
end sub

'*****************
' object1  shaking
'*****************

Dim D1Shake, D2Shake

Sub D1Shaker()
    D1Shake = 6
    D1Timer.Enabled = True
End Sub

Sub D1Timer_Timer()
    D1.Transz = D1Shake / 2
    If D1Shake = 0 Then Me.Enabled = False:Exit Sub
    If D1Shake <0 Then
        D1Shake = ABS(D1Shake)- 0.1
    Else
        D1Shake = - D1Shake + 0.1
    End If
End Sub

Sub D2Shaker()
    D2Shake = 6
    D2Timer.Enabled = True
End Sub

Sub D2Timer_Timer()
    D2.Transz = D2Shake / 2
    If D2Shake = 0 Then Me.Enabled = False:Exit Sub
    If D2Shake <0 Then
        D2Shake = ABS(D2Shake)- 0.1
    Else
        D2Shake = - D2Shake + 0.1
    End If
End Sub

'**************************
'Spinners
'**************************
Sub Spinner1_Spin() 'Inside this Sub is what the spinner1 will do
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    PlaySound "swish"
     DOF 119, DOFPulse
    Addscore 1000
End Sub

Sub Spinner2_Spin() 'Inside this Sub is what the spinner1 will do
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    PlaySound "swish"
     DOF 120, DOFPulse
    Addscore 1000
End Sub

'**************************
'reset jackpot
'**************************

Sub jackpotreset
    ' reset lights doors and monsters
    Light4.State = 0
    Light5.State = 0
    Light6.State = 0
    Light7.State = 0
    Light8.State = 0
    Light9.State = 0
    Light10.State = 0
    Light11.State = 0
    Light12.State = 0
    Light13.State = 0
    Light14.State = 0
    Light15.State = 0
    Light16.State = 0
    Light17.State = 0
    Light18.State = 0
    Light19.State = 0
    Light20.State = 0
    Light21.State = 0
    Light22.State = 0
    Light23.State = 0
    Light24.State = 0
    Light25.State = 0
    Light26.State = 0
    Light27.State = 0
    Light28.State = 0
    Light29.State = 0
    Light30.State = 0
    Light31.State = 0

    'diamond ligths up
    li18.state = 0
    li20.state = 0
    li21.state = 0
    li19.state = 0

    'm lights down
    Light32.state = 0
    Light33.state = 0
    Light34.state = 0
    Light35.state = 0

    'reset doors and monsters
    DoorPos1 = 0
    DoorPos2 = 0
    DoorPos3 = 0
    DoorPos4 = 0
    MonsPos1 = 0
    MonsPos2 = 0
    MonsPos3 = 0
    MonsPos4 = 0
end sub

sub Jackpot
    DMDFlush
    playsound "jackpot"
    AddScore 1000000
    DMD "", "", "dmdj1", eNone, eNone, eNone, 300, True, ""
    DMD "", "", "dmdj2", eNone, eNone, eNone, 300, True, ""
    DMD "", "", "dmdj3", eNone, eNone, eNone, 300, True, ""
    DMD "", "", "dmdj4", eNone, eNone, eNone, 300, True, ""
    DMD "", "", "dmdj5", eNone, eNone, eNone, 400, True, ""
    DMD "", "", "dmdj6", eNone, eNone, eNone, 400, True, ""
end sub