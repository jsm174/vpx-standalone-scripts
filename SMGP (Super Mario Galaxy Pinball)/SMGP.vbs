' ****************************************************************
'                       VISUAL PINBALL X
'                		Super Mario Galaxy Pinball by remdwaas
'                       Version 1.0.0
'						started 13-6-2020
'						Finished
' ****************************************************************

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
Const cGameName = "SMGP"
Const TableName = "SMGP"
Const myVersion = "1.0.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 3  ' limit to 3x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 5   ' usually 3 or 5
Const MaxMultiballs = 4  ' max number of balls during multiballs

Const Special1 = 3000000  ' High score to obtain an extra ball/game
Const Special2 = 6000000
Const Special3 = 9000000

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
Dim Mario
Dim Luigi
Dim Peach
Dim Wario
Dim PlayerSelectActive
'Dim SuperVuk

' core.vbs variables
Dim plungerIM
Dim mMagnaSave

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
	Dim i
	'Randomize
'Reseths
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

' Magnet/turntable
Set mMagnaSave = New cvpmTurntable
With mMagnaSave
        .InitTurnTable Magna, 80
        .spinCW = False
        .MotorOn = True
        .CreateEvents "mMagnaSave"
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
            If(Tilted = False) Then
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
	
    If PlayerSelectActive Then
        SelectPlayerStart(Keycode)
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
            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall = True) ) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                    DMD "_", CL(1, PlayersPlayingGame & " PLAYERS"), "", eNone, eBlink, eNone, 500, True, "so_fanfare1"
                Else
                    If(Credits> 0) then
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
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetForNewGame()
						UpdateMusicNow
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
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
        Playsound "fire"
		'PlaySoundAt "fx_plunger", plunger
		'If bBallInPlungerLane Then PlayQuoteWstart
        If bBallInPlungerLane Then PlaySoundAt "fire", plunger
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
		Flipper2.RotateToEnd 'Adds To End Movement for Flipper2
		RotateLaneLightsLeft
		'RotateLaneLightsLeft2
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
		Flipper2.RotateToStart 'Adds To End Movement for Flipper2
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
		Flipper1.RotateToEnd 'Adds To End Movement for Flipper1
		RotateLaneLightsRight
		'RotateLaneLightsRight2
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
		Flipper1.RotateToStart 'Adds To End Movement for Flipper1
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

'Sub RotateLaneLightsLeft2
'    Dim TempState
'    TempState = li016.State
'    li016.State = li017.State
'    li017.State = li018.State
'    li018.State = li019.State
'	li019.State = li020.state
'    li020.state = TempState
'End Sub

'Sub RotateLaneLightsRight2
'    Dim TempState
'    TempState = li020.State
'    li020.State = li019.State
'    li019.State = li018.State
'    li018.State = li017.State
'	li017.State = li016.state
'    li016.state = TempState
'End Sub

'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
        DMD "_", CL(1, "CAREFUL!"), "", eNone, eBlinkFast, eNone, 500, True, ""
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        DMDFlush
        DMD "", "", "tilt", eNone, eNone, eBlink, 200, False, ""
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
    If(BallsOnPlayfield = 0) Then
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
        PlaySong "MULT_1" '"MULTIBALL"
    Else
        UpdateMusicNow
    end if
End Sub

Sub UpdateMusicNow
    Select Case UpdateMusic
        Case 0:PlaySong "BALL1"
        Case 1:PlaySong "BALL2"
        Case 2:PlaySong "BALL3"
        Case 3:PlaySong "BALL4"
        Case 4:PlaySong "BALL5"
        Case 5:PlaySong "M_end"
        Case 6:PlaySong "chooseplayer2"
    End Select
end sub

'********************
' Play random quotes
'********************

Sub PlayMultiball
    Dim tmp
    tmp = INT(RND * 2) + 1
    PlaySong "MULT_" &tmp
End Sub

Sub PlayQuote_timer() 'one quote each 2 minutes
Dim tmp

If Mario = 1 then
    tmp = INT(RND * 67) + 1
    PlaySound "MM_" &tmp
end if
If Luigi = 1 then
    tmp = INT(RND * 57) + 1
    PlaySound "LUI_" &tmp
end if
If Peach = 1 then
    tmp = INT(RND * 60) + 1
    PlaySound "PP_" &tmp
end if
If Wario = 1 then
    tmp = INT(RND * 62) + 1
    PlaySound "WA_" &tmp
end if
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
        If UBound(tmp) = 1 Then 'we have 2 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    DOF 118, DOFOn
	'Table1.ColorGradeImage = "JPS_EnvTT7"'"-30"
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
	'Table1.ColorGradeImage = "-70"
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
    tmp = ball.y * 2 / Table1.height-1
    If tmp > 0 Then
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

        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        End If
    Next
End Sub

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub Spinner_Spin
	PlaySound "fx_spinner", 0, .25, AudioPan(Spinner), 0.25, 0, 0, 1, AudioFade(Spinner)
End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

'Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
'Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
'Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
'Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
'Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
'Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
'Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

Sub RHelp1_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAtBall "fx_ballrampdrop"
End Sub

Sub RHelp2_Hit()
    StopSound "fx_metalrolling"
    PlaySoundAtBall"fx_ballrampdrop"
End Sub

' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Dim SlotAward, SelectCounter, SlotValue

Sub ResetForNewGame()
    'ResetForNewGame2()
    'exit sub
    'SlotAward = 0
    SlotValue = 0
    SelectCounter = 0
    SlotAward = Array("p1", "p2", "p3", "p4")
    'bOnTheFirstBall = True
    'bGameInPLay = True
	BowPos1 = 0
	EggPos = 0
	KoopaPos = 0
	SpikePos = 0
	GoombaPos = 0
	BooPosy = 0
	BombPos = 0
	Starlvl = 0
	Superjackpot = 0
    UpdateMusic = 0
    UpdateMusic = UpdateMusic + 6
    UpdateMusicNow
	
	'reset characters Object
	W1.z = -200
	W2.z = -200
	P1.z = -200
	P2.z = -200
	M1.z = -200
	M2.z = -200
	L1.z = -200
	L2.z = -200
	
	'reset lights characters
	li053.state = 0
	li054.state = 0
	li055.state = 0
	li056.state = 0

	'reset Towb
	TowbTimer.Enabled = 0
	Towb.z = 100

	'reset variable
    Mario = 0
    Luigi = 0
    Peach = 0
    Wario = 0
    StopAttractMode
    LightSeqAttract.Play SeqBlinking, , 5, 150

    DMDFlush
    DMD "", "", "p0", eNone, eNone, eNone, 10000, False, "char"
    PlayerSelectActive = True
end sub

Sub SelectPlayerStart(Keycode)
    DMD "", "", SlotAward(SelectCounter), eNone, eNone, eNone, 500, False, ""
    If keycode = LeftFlipperKey then
        SelectCounter = SelectCounter - 1
        If SelectCounter = -1 Then SelectCounter = 3
    end If
    If keycode = RightFlipperKey then
        SelectCounter = SelectCounter + 1
        If SelectCounter = 4 Then SelectCounter = 0
    end If
    Select Case SelectCounter
        Case 0
            DMDFlush
            DMD "", "", "p1", eNone, eNone, eNone, 200000, False, "mario"
            SlotValue = 0
        Case 1
            DMDFlush
            DMD "", "", "p2", eNone, eNone, eNone, 200000, False, "luigi"
            SlotValue = 1
        Case 2
            DMDFlush
            DMD "", "", "p3", eNone, eNone, eNone, 200000, False, "peach"
            SlotValue = 2
        Case 3
            DMDFlush
            DMD "", "", "p4", eNone, eNone, eNone, 200000, False, "wario"
            SlotValue = 3
    End Select

    If keycode = StartGameKey Then
        Select Case SlotValue
            Case 0
            'DMDFlush
				PlayerSelectActive = False
				Mario = 1
				M1.z = 53
				M2.z = 53
			    UpdateMusic = 0
				li054.state = 1
				PlaySound ""
				vpmtimer.addtimer 1000, "MarioStart '"
                vpmtimer.addtimer 3000, "ResetForNewGame2() '"
            Case 1
            'DMDFlush
				PlayerSelectActive = False
                Luigi = 1
				L1.z = 53
				L2.z = 53
				UpdateMusic = 0
				li053.state = 1
                PlaySound ""
				vpmtimer.addtimer 1000, "LuigiStart '"
                vpmtimer.addtimer 4000, "ResetForNewGame2() '"
            Case 2
            'DMDFlush
				PlayerSelectActive = False 
				Peach = 1
				P1.z = 53
				P2.z = 53
				UpdateMusic = 0
				li056.state = 1
                PlaySound ""
				vpmtimer.addtimer 1000, "PeachStart '"
                vpmtimer.addtimer 2000, "ResetForNewGame2() '"
            Case 3
            'DMDFlush
				PlayerSelectActive = False
                Wario = 1
				W1.z = 53
				W2.z = 53
				UpdateMusic = 0
				li055.state = 1
                PlaySound ""
				vpmtimer.addtimer 1000, "WarioStart '"
                vpmtimer.addtimer 5000, "ResetForNewGame2() '"
        End Select
        'vpmtimer.addtimer 1000, "ResetForNewGame2() '"
    end if
end sub

sub MarioStart
Playsound "ms"
end sub

sub LuigiStart
Playsound "ls"
end sub

sub PeachStart
Playsound "ps"
end sub

sub WarioStart
Playsound "ws"
end sub

Sub ResetForNewGame2()
    Dim i

    bGameInPLay = True

    'resets the score display, and turn off attract mode
    StopAttractMode
    GiOn

    'update music
    UpdateMusic = 0
	'UpdateMusic = UpdateMusic
	'UpdateMusicNow

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
'StopSong
PlaySound ""


    vpmtimer.addtimer 500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to

' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
	UpdateMusicNow
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
	BumperBonus = 0
	HoleBonus = 0
	spikebonus = 0
	boobonus = 0
	bombbonus = 0
	goombabonus = 0
	koopabonus = 0
	bowsersbonus = 0
    ResetNewBallVariables
    ResetNewBallLights()
	'Multiball=false
	'UpdateMusicNow	
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    
	LightSeqAttract.StopPlay

	' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    PlaySoundAt SoundFXDOF("fx_Ballrel", 123, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4

	'only this tableDrain / Plunger Functions
	ChangeBallImage

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

Sub EndOfBall()
	Dim BonusDelayTime
	' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

'	LightSeqAttract.Play SeqBlinking, , 5, 150

StopSong
bonuscheckie

    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0

    'If NOT Tilted Then
	If(Tilted = False) Then
If Mario = 1 then
DMD "", "", "dmdballlostM", eNone, eNone, eNone, 1000, True, ""
end if
If Luigi = 1 then
DMD "", "", "dmdballlostL", eNone, eNone, eNone, 1000, True, ""
end if
If Peach = 1 then
DMD "", "", "dmdballlostP", eNone, eNone, eNone, 1000, True, ""
end if
If Wario = 1 then
DMD "", "", "dmdballlostW", eNone, eNone, eNone, 1000, True, ""
end if
		PlaySound "Death"

		'Number of Target hits
        AwardPoints = TargetBonus * 10000
        TotalBonus = TotalBonus + AwardPoints

		AwardPoints = BooHits * 50000
		TotalBonus = TotalBonus + AwardPoints

        AwardPoints = BumperBonus * 100000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = koopabonus * 100000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = goombabonus * 100000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = bombbonus * 100000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = boobonus * 100000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = spikebonus * 100000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = bowsersbonus * 100000
        TotalBonus = TotalBonus + AwardPoints

        DMD CL(0, FormatScore(TotalBonus)), CL(1, "TOTAL BONUS " & " VIII" & BonusMultiplier(CurrentPlayer)), "", eBlinkFast, eNone, eNone, 1000, True, ""
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        
		AddScore TotalBonus

		' add a bit of a delay to allow for the bonus points to be shown & added up
		vpmtimer.addtimer 3300, "EndOfBall2 '"
    Else 'Si hay falta simplemente espera un momento y va directo a la segunta parte después de perder la bola
		BonusDelayTime = 100
		EndOfBall2
    End If
	'vpmtimer.addtimer BonusDelayTime, "EndOfBall2 '"
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    UpdateMusic = UpdateMusic + 1
	UpdateMusicNow
	Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
            LightShootAgain.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        DMD "", "", "dmdextraball2", eNone, eNone, eNone, 1000, True, ""
		
		UpdateMusic = UpdateMusic - 1
		UpdateMusicNow
        ' reset the playfield for the new ball
        ResetForNewPlayerBall()
		
		' set the dropped wall for bonus

		
        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0) Then
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
		PlayQuote.Enabled = 0
		StopSong
		'DMD CL(0, "GAME OVER") "", eNone, 13000, True, ""
		DMD "", "", "dmdgameover", eNone, eNone, eNone, 5200, True, ""
	'DMD "", CL(1, "GAME OVER"), "", eNone, eNone, eNone, 13000, False, ""
		PlaySound "GAMEOVER"
        ' set the machine into game over mode
        vpmtimer.addtimer 5200, "EndOfGame() '"

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
	
	'PlayQuote.Enabled = 0

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

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
	'If BallsOnPlayfield<2 Then
	'Multiball=false
	'end if
	
    ' pretend to knock the ball into the ball storage mech
    PlaySoundAt "fx_drain", Drain
    'if Tilted then end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) Then
	AddMultiball 1
	bAutoPlunger = True
            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
	DMD CL(0, "BALL SAVED"), CL(1, "SHOOT AGAIN"), "", eBlink, eBlink, eNone, 800, True, ""
    'vpmtimer.addtimer 1250, "CreateNewBall() '"


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
            If(BallsOnPlayfield = 0) Then

                ' End Mode and timers
				StopSong
				PlaySound ""
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
        DOF 121, DOFPulse
        PlaySoundAt "fx_fire", Trigger1
        bAutoPlunger = False
    End If	
'StopSong
    DMDScoreNow
    bBallInPlungerLane = True
    'DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eBlink, eNone, 1000, True, ""
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
        EnableBallSaver BallSaverTime
		'PlayQuoteWstart
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
	'PlayQuoteWstart
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
	PlaySound "tone"&points

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
    If(Tilted = False) Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    End if
End Sub

Sub AwardExtraBall()
	DMD "", "", "dmdextraball2", eNone, eNone, eNone, 1000, True, "EXTRABALL"
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
    If(x <> "") then Credits = CInt(x) Else Credits = 0:If bFreePlay = False Then DOF 125, DOFOff:End If
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

    If tmp> HighScore(3) Then
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
        if(hsCurrentLetter = 0) then
            hsCurrentLetter = len(hsValidLetters)
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = RightFlipperKey Then
        playsound "fx_Next"
        hsCurrentLetter = hsCurrentLetter + 1
        if(hsCurrentLetter> len(hsValidLetters) ) then
            hsCurrentLetter = 1
        end if
        HighScoreDisplayNameNow()
    End If

    If keycode = PlungerKey Then
        if(mid(hsValidLetters, hsCurrentLetter, 1) <> "`") then
            playsound "fx_Enter"
            hsEnteredDigits(hsCurrentDigit) = mid(hsValidLetters, hsCurrentLetter, 1)
            hsCurrentDigit = hsCurrentDigit + 1
            if(hsCurrentDigit = 3) then
                HighScoreCommitName()
            else
                HighScoreDisplayNameNow()
            end if
        else
            playsound "fx_Esc"
            hsEnteredDigits(hsCurrentDigit) = " "
            if(hsCurrentDigit> 0) then
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
    if(hsCurrentDigit> 0) then TempBotStr = TempBotStr & hsEnteredDigits(0)
    if(hsCurrentDigit> 1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit> 2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

    if(hsCurrentDigit <> 3) then
        if(hsLetterFlash <> 0) then
            TempBotStr = TempBotStr & "_"
        else
            TempBotStr = TempBotStr & mid(hsValidLetters, hsCurrentLetter, 1)
        end if
    end if

    if(hsCurrentDigit <1) then TempBotStr = TempBotStr & hsEnteredDigits(1)
    if(hsCurrentDigit <2) then TempBotStr = TempBotStr & hsEnteredDigits(2)

    TempBotStr = TempBotStr & " <    "
    dLine(1) = ExpandLine(TempBotStr, 1)
    DMDUpdate 1
End Sub

Sub HighScoreFlashTimer_Timer()
    HighScoreFlashTimer.Enabled = False
    hsLetterFlash = hsLetterFlash + 1
    if(hsLetterFlash = 2) then hsLetterFlash = 0
    HighScoreDisplayName()
    HighScoreFlashTimer.Enabled = True
End Sub

Sub HighScoreCommitName()
    HighScoreFlashTimer.Enabled = False
    hsbModeActive = False
    ChangeSong
    hsEnteredName = hsEnteredDigits(0) & hsEnteredDigits(1) & hsEnteredDigits(2)
    if(hsEnteredName = "   ") then
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
                DMDScene.GetImage("Dig" & i).SetBounds 8 * (i - 20), 3, 8, 16
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
    If Score(1) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 1" &FormatScore(Score(1) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(2) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 2 " &FormatScore(Score(2) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(3) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 3 " &FormatScore(Score(3) ) ), "", eNone, eNone, eNone, 3000, False, ""
    End If
    If Score(4) Then
        DMD CL(0, "LAST SCORE"), CL(1, "PLAYER 4 " &FormatScore(Score(4) ) ), "", eNone, eNone, eNone, 3000, False, ""
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
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0) ), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
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
Dim bRedBall

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

' tables variables and Mode init
Dim HoleBonus, BumperBonus, TargetBonus, bowsersbonus, koopabonus, goombabonus, bombbonus, boobonus, spikebonus    

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    'ChangeSong
	bRedBall = False
	TargetBonus = 0
	bumperHits = 100
	BumperBonus = 0
	spikebonus = 0
	boobonus = 0
	bombbonus = 0
	goombabonus = 0
	koopabonus = 0
	bowsersbonus = 0
	PlayQuote.Enabled = 1
	'BallInHole = 0
    TurnOffPlayfieldLights()
End Sub

Sub StopEndOfBallMode()     'this sub is called after the last ball is drained
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
Dim i
TargetBonus = 0
bBallSaverReady = True
End Sub

Sub ResetNewBallLights()    'turn on or off the needed lights before a new ball is released
    'TurnOffPlayfieldLights
    save.state = 2
	save2.state = 2
	If Mario = 1 then
            li054.state = 1
        Elseif Luigi = 1 then
            li053.state = 1
        Elseif Peach = 1 then
            li056.state = 1
        Elseif Wario = 1 then
            li055.state = 1
        End If
	li070.state = 1
	li071.state = 1
	li072.state = 1
	li073.state = 1
	li074.state = 1
	li075.state = 1
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub


Sub ChangeBallImage()
    Dim BOT, b
    BOT = GetBalls
    ' exit the Sub if no balls on the table
    If UBound(BOT) = -1 Then Exit Sub

	' change the ball color
	bRedBall = NOT bRedBall
    ' change the image for each ball
    For b = 0 to UBound(BOT)
        If Mario = 1 then
            BOT(b).FrontDecal = "Bmario"
        Elseif Luigi = 1 then
            BOT(b).FrontDecal = "Bluigi"
        Elseif Peach = 1 then
            BOT(b).FrontDecal = "Bpeach"
        Elseif Wario = 1 then
            BOT(b).FrontDecal = "Bwario"
        End If
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
	FlashForMs Flasher7, 1000, 50, 0
	if Mario = 1 then
	M2Shaker
	end if
	if Luigi = 1 then
	L2Shaker
	end if
	if Peach = 1 then
	P2Shaker
	end if
	if Wario = 1 then
	W2Shaker
	end if
    If li001.State=1 then 
	AddScore 210
	end if
	If li002.State=1 then 
	AddScore 420
	end if
	PlaySound SoundFX("Rsling",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	'gi1.State = 0:Gi2.State = 0
	AddScore 210
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0':gi1.State = 1:Gi2.State = 1
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	FlashForMs Flasher8, 1000, 50, 0
	if Mario = 1 then
	M1Shaker
	end if
	if Luigi = 1 then
	L1Shaker
	end if
	if Peach = 1 then
	P1Shaker
	end if
	if Wario = 1 then
	W1Shaker
	end if
	If li001.State=1 then 
	AddScore 210
	end if
	If li002.State=1 then 
	AddScore 420
	end if
    PlaySound SoundFX("Lsling",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	'gi3.State = 0:Gi4.State = 0
	AddScore 210
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0'gi3.State = 1:Gi4.State = 1
    End Select
    LStep = LStep + 1
End Sub

'*****************
'triggers
'*****************
Sub TLeftInlane_Hit
    If li001.State = 1 then
        AddScore 5000
    end if
    If li002.State = 1 then
        AddScore 10000
    end if
    If LeftInlane.state = 0 then
        'PlaySound "fx_sensor"
        AddScore 5000
        LeftInlane.state = 1
    Elseif LeftInlane.state = 0 then
        'PlaySound "fx_sensor"
        AddScore 5000
    end if
	DMD "", "", "DMDBONUS11", eNone, eNone, eNone, 500, True, ""
    PlaySound "INNERLANE"
    CheckbBOWIE
End Sub

Sub TLeftOutlane_Hit
    If li001.State = 1 then
        AddScore 10000
    end if
    If li002.State = 1 then
        AddScore 20000
    end if
	If save.state = 2 Then
	KickbackPulse.enabled = 1
    kickbacktimer.interval = 800
	kickbacktimer.enabled = true
	end If
	If save.state = 0 Then
	KickbackPulse.enabled = 0
	kickbacktimer.enabled = False
	end If
    If LeftOutlane.state = 0 then
        PlaySound "fx_sensor"
        AddScore 10000
        LeftOutlane.state = 1
    Elseif LeftOutlane.state = 0 then
        PlaySound "fx_sensor"
        AddScore 10000
    end if
    FlashForMs Flasher2, 1000, 50, 0
	DMD "", "", "DMDBONUS6", eNone, eNone, eNone, 500, True, ""
    'PlaySound "Death3"
    CheckbBOWIE
End Sub

Sub TRightInlane_Hit
    If li001.State = 1 then
        AddScore 5000
    end if
    If li002.State = 1 then
        AddScore 10000
    end if
    If RightInlane.state = 0 then
        'PlaySound "fx_sensor"
        AddScore 5000
        RightInlane.state = 1
    Elseif RightInlane.state = 0 then
        'PlaySound "fx_sensor"
        AddScore 5000
    end if
	DMD "", "", "DMDBONUS11", eNone, eNone, eNone, 500, True, ""
    PlaySound "INNERLANE"
    CheckbBOWIE
End Sub

Sub TRightOutlane_Hit
    If li001.State = 1 then
        AddScore 10000
    end if
    If li002.State = 1 then
        AddScore 20000
    end if
	If save2.state = 2 Then
	KickbackPulse2.enabled = 1
    kickbacktimer2.interval = 800
	kickbacktimer2.enabled = true
	end If
	If save2.state = 0 Then
	KickbackPulse2.enabled = 0
	kickbacktimer2.enabled = False
	end If
    If RightOutlane.state = 0 then
        PlaySound "fx_sensor"
        AddScore 10000
        RightOutlane.state = 1
    Elseif RightOutlane.state = 0 then
        PlaySound "fx_sensor"
        AddScore 10000
    end if
    FlashForMs Flasher1, 1000, 50, 0
	DMD "", "", "DMDBONUS6", eNone, eNone, eNone, 500, True, ""
    'PlaySound "Death3"
    CheckbBOWIE
End Sub

Sub CheckbBOWIE
    If(LeftInlane.State = 1) And(LeftOutlane.State = 1) And(RightInlane.State = 1) And(RightOutlane.State = 1) Then
    'DMD "_", CL(0, "BOWSER JR. BONUS"), "_", eNone, eBlink, eNone, 3000, True, "BONUS"
	DMD "", "", "dmdbowjrbonus", eNone, eNone, eNone, 1000, True, "BONUS"
	AddScore 50000
	LeftInlane.State=0
	LeftOutlane.State=0
	RightInlane.State=0
	RightOutlane.State=0      
    End If
End Sub

Sub Trigger001_Hit()
playsound "PIPE2"
end sub

Sub TRIGL_Hit()
PlaySoundAt "fx_metalrolling", TRIGR
end sub

Sub TRIGR_Hit()
PlaySoundAt "fx_metalrolling", TRIGL
end sub

'*************************Bonus checker*************************

Sub Bonuschecker_Hit
FlashForMs Flasher4, 1000, 50, 0
FlashForMs Flasher5, 1000, 50, 0
FlashForMs Flasher6, 1000, 50, 0
FlashForMs Flasher7, 1000, 50, 0
FlashForMs Flasher8, 1000, 50, 0
FlashForMs Flasher9, 1000, 50, 0
FlashForMs Flasher10, 1000, 50, 0
end sub

Sub bonuscheckie
'spikebonus
if li040.State = 2 then
li040.State = 0
li051.state = 0
spikebonus = 1
else
spikebonus = 0
end if

'boobonus
if li038.State = 2 then 
li038.State = 0
li049.state = 0
boobonus = 1
else
boobonus = 0
end if

'bombbonus
if li037.State = 2 then
li037.State = 0
li050.state = 0
bombbonus = 1
else
bombbonus = 0
end if

'goombabonus
if li039.State = 2 then
li039.State = 0
li048.state = 0
goombabonus = 1
else
goombabonus = 0
end if

'koopabonus
if li041.State = 2 then
li041.State = 0
li052.state = 0
koopabonus = 1
else
koopabonus = 0
end if

'bowserbonus
If li008.state = 1 Then
bankup.enabled = 1
	sw9.IsDropped=False
	sw10.IsDropped=False
	sw11.IsDropped=False
PlaySound "bankDOWN"
li008.state = 0
bowsersbonus = 1
Else
bowsersbonus = 0
end if
End Sub

'************************** 
'Bumpers 
'************************** 
Dim bumperHits, Starlvl

Sub Bumper1_hit()
If li001.State=1 then 
	AddScore 500
	end if
	If li002.State=1 then 
	AddScore 1000
	end if
PlaySound "STAR1"
addscore 500
FlashForMs Flasher1, 1000, 50, 0
Starlvl = Starlvl + 1
	bumperHits = bumperHits - 1
    CheckBumpers
	UpdateStarLights
end sub

Sub Bumper2_hit()
If li001.State=1 then 
	AddScore 500
	end if
	If li002.State=1 then 
	AddScore 1000
	end if
PlaySound "STAR2"
addscore 500
FlashForMs Flasher2, 1000, 50, 0
Starlvl = Starlvl + 1
	bumperHits = bumperHits - 1
    CheckBumpers
	UpdateStarLights
end sub

Sub Bumper3_hit()
If li001.State=1 then 
	AddScore 500
	end if
	If li002.State=1 then 
	AddScore 1000
	end if
PlaySound "STAR3"
addscore 500
FlashForMs Flasher3, 1000, 50, 0
Starlvl = Starlvl + 1
	bumperHits = bumperHits - 1
    CheckBumpers
	UpdateStarLights
end sub

' Bumper Bonus
' 100000 i bonus after each 100 hits

Sub CheckBumpers()
    If bumperHits <= 0 Then
        BumperBonus = BumperBonus + 1
        'DMD "_", CL(1, "BUMPERS BONUS " & BumperBonus), "_", eNone, eBlink, eNone, 500, True, ""
        bumperHits = 100
    ' do something more
    End If
	if Starlvl = 10 Then
	Starlvl = 0
	UpdateStarLights
	End if
End Sub

Sub ResetBumpers()
    bumperHits = 100
End Sub

Sub UpdateStarLights
    Select Case Starlvl
Case 0 li043.state = 0:li044.state = 0:li045.state = 0:li046.state = 0:li047.state = 0
Case 1 li043.state = 1:li044.state = 0:li045.state = 0:li046.state = 0:li047.state = 0
Case 2 li043.state = 1:li044.state = 1:li045.state = 0:li046.state = 0:li047.state = 0
Case 3 li043.state = 1:li044.state = 1:li045.state = 1:li046.state = 0:li047.state = 0
Case 4 li043.state = 1:li044.state = 1:li045.state = 1:li046.state = 1:li047.state = 0
Case 5 li043.state = 1:li044.state = 1:li045.state = 1:li046.state = 1:li047.state = 1
Case 6 li043.state = 2:li044.state = 1:li045.state = 1:li046.state = 1:li047.state = 1
Case 7 li043.state = 2:li044.state = 2:li045.state = 1:li046.state = 1:li047.state = 1
Case 8 li043.state = 2:li044.state = 2:li045.state = 2:li046.state = 1:li047.state = 1
Case 9 li043.state = 2:li044.state = 2:li045.state = 2:li046.state = 2:li047.state = 1
Case 10 li043.state = 2:li044.state = 2:li045.state = 2:li046.state = 2:li047.state = 2
    End Select
end sub

'*****************
'Targets
'*****************
Dim BowPos1, KoopaPos, BooPosy, GoombaPos, SpikePos, BombPos, Superjackpot

'*************************target Boo king*************************

Sub Target001_Hit()
	If Tilted Then Exit Sub
	if li042.state = 2 Then
	Playsound "TARGET1"
	exit Sub
	end if
    PlaySound "TARGETBOO"
    AddScore 5500
	vpmtimer.addtimer 60000, "BooMoveDown ' "
    BooMoveUp
End Sub

'*************************target savers(left/right below)*************************

Sub Tsave_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 5000
    end if
    If li002.State = 1 then
        AddScore 10000
    end if
	DMD "", "", "DMDBONUS11", eNone, eNone, eNone, 500, True, ""
	Save.state = 2
	Save2.state = 2
    PlaySound "MESSAGE"
    Addscore 5000
	TowbTimer.Enabled = 1
    TargetBonus = TargetBonus + 1
End Sub

'*************************target flagpole*************************

Sub TFLAG_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 25000
    end if
    If li002.State = 1 then
        AddScore 50000
    end if
	DMD "", "", "DMDBONUS5", eNone, eNone, eNone, 500, True, ""
	FlagDir = 5 'set the direction to up
	FlagTimer.Enabled = 1
    PlaySound "flagpole"
    Addscore 25000
    TargetBonus = TargetBonus + 1
End Sub


'*************************target 2x/3x*************************

Sub Target1_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 and li22.State = 0 then
        li22.State = 1
        AddScore 2000
        PlaySound "TARGET1"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li001.State = 1 and li22.State = 1 then
        AddScore 2000
        PlaySound "TARGET1"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li002.State = 1 then
        AddScore 3000
        PlaySound "TARGET1"
        TargetBonus = TargetBonus + 1
        Exit Sub
    end if

    If li22.State = 1 Then
        PlaySound "TARGET1"
    else
        PlaySound "TARGET1"
        li22.State = 1
    end if
    AddScore 1000
    TargetBonus = TargetBonus + 1
    'PlaySound "TARGET1"
    Checkb2x
    Checkb3x
End Sub

Sub Target2_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 and li23.State = 0 then
        li23.State = 1
        AddScore 2000
        PlaySound "TARGET1"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li001.State = 1 and li23.State = 1 then
        AddScore 2000
        PlaySound "TARGET1"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li002.State = 1 then
        AddScore 3000
        PlaySound "TARGET1"
        TargetBonus = TargetBonus + 1
        Exit Sub
    end if

    If li23.State = 1 Then
        PlaySound "TARGET1"
    else
        PlaySound "TARGET1"
        li23.State = 1
    end if
    AddScore 1000
    TargetBonus = TargetBonus + 1
    'PlaySound "TARGET1"
    Checkb2x
    Checkb3x
End Sub

Sub Checkb2x
    If(li22.State = 1)Or(li23.State = 1)Then
        DMD "", "", "DMDBONUS1", eNone, eNone, eNone, 1000, True, "2x"
        li001.State = 1
        vpmtimer.addtimer 60000, "li001.state = 0 ' "
        vpmtimer.addtimer 60000, "li23.State = 0 ' "
        vpmtimer.addtimer 60000, "li22.State = 0 ' "
    End If
End Sub

Sub Checkb3x
    If(li22.State = 1)And(li23.State = 1)Then
        DMD "", "", "DMDBONUS2", eNone, eNone, eNone, 1000, True, "3x"
        li002.State = 1
        li001.State = 0
        vpmtimer.addtimer 60000, "li002.state = 0 ' "
        vpmtimer.addtimer 60000, "li23.State = 0 ' "
        vpmtimer.addtimer 60000, "li22.State = 0 ' "
    End If
End Sub


'*************************target koopa*************************

Sub T1_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
	KoopaPos = KoopaPos + 1
    PlaySound "KOOPA"
    Addscore 1000
    TargetBonus = TargetBonus + 1
	UpdateKoopaLights
	checkKoopa
End Sub

Sub UpdateKoopaLights
    Select Case KoopaPos
        Case 0:li012.State = 0:li013.State = 0:li014.State = 0:li015.State = 0:li016.State = 0
        Case 1:li012.State = 1:li013.State = 0:li014.State = 0:li015.State = 0:li016.State = 0:Koopadmd1
        Case 2:li012.State = 1:li013.State = 1:li014.State = 0:li015.State = 0:li016.State = 0:Koopadmd2
        Case 3:li012.State = 1:li013.State = 1:li014.State = 1:li015.State = 0:li016.State = 0:Koopadmd3
        Case 4:li012.State = 1:li013.State = 1:li014.State = 1:li015.State = 1:li016.State = 0:Koopadmd4
        Case 5:li012.State = 1:li013.State = 1:li014.State = 1:li015.State = 1:li016.State = 1:Koopadmd5
    End Select
end sub

Sub Koopadmd1
        DMD "", "", "dmdkoop1", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Koopadmd2
        DMD "", "", "dmdkoop2", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Koopadmd3
        DMD "", "", "dmdkoop3", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Koopadmd4
        DMD "", "", "dmdkoop4", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Koopadmd5
        DMD "", "", "dmdkoop5", eNone, eNone, eNone, 1000, True, ""
end sub

Sub checkKoopa
    if(li012.state = 1)And(li013.state = 1)And(li014.state = 1)And(li015.state = 1)And(li016.state = 1)And(li041.State = 2) Then
		KoopaPos = 0
        li041.State = 2
        AddScore 20000
        li012.state = 0
        li013.state = 0
        li014.state = 0
        li015.state = 0
        li016.state = 0
		li052.state = 1
	exit sub
	end if

If bMultiBallMode = False Then
	if(li012.state = 1)And(li013.state = 1)And(li014.state = 1)And(li015.state = 1)And(li016.state = 1)then
        DMD "", "", "dmdkoop6", eNone, eNone, eNone, 1000, True, ""
		KoopaPos = 0
        li041.State = 2
        AddScore 10000
        li012.state = 0
        li013.state = 0
        li014.state = 0
        li015.state = 0
        li016.state = 0
		li052.state = 1
		AddMultiball 1
		ChangeSong	
    End If
End If
End Sub

'*************************target goomba*************************

Sub T2_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
	GoombaPos = GoombaPos + 1
    PlaySound "GOOMBA"
    Addscore 1000
    TargetBonus = TargetBonus + 1
	UpdateGoombaLights
	checkGoomba
End Sub

Sub UpdateGoombaLights
    Select Case GoombaPos
        Case 0:li022.State = 0:li023.State = 0:li024.State = 0:li025.State = 0:li026.State = 0
        Case 1:li022.State = 1:li023.State = 0:li024.State = 0:li025.State = 0:li026.State = 0:Goombadmd1
        Case 2:li022.State = 1:li023.State = 1:li024.State = 0:li025.State = 0:li026.State = 0:Goombadmd2
        Case 3:li022.State = 1:li023.State = 1:li024.State = 1:li025.State = 0:li026.State = 0:Goombadmd3
        Case 4:li022.State = 1:li023.State = 1:li024.State = 1:li025.State = 1:li026.State = 0:Goombadmd4
        Case 5:li022.State = 1:li023.State = 1:li024.State = 1:li025.State = 1:li026.State = 1:Goombadmd5
    End Select
end sub

Sub Goombadmd1
        DMD "", "", "dmdgoom1", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Goombadmd2
        DMD "", "", "dmdgoom2", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Goombadmd3
        DMD "", "", "dmdgoom3", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Goombadmd4
        DMD "", "", "dmdgoom4", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Goombadmd5
        DMD "", "", "dmdgoom5", eNone, eNone, eNone, 1000, True, ""
end sub

Sub checkGoomba
    if(li022.state = 1)And(li023.state = 1)And(li024.state = 1)And(li025.state = 1)And(li026.state = 1)And(li039.State = 2)then
		GoombaPos = 0
        AddScore 20000
        li022.state = 0
        li023.state = 0
        li024.state = 0
        li025.state = 0
        li026.state = 0
	exit sub
    End If

If bMultiBallMode = False Then
    if(li022.state = 1)And(li023.state = 1)And(li024.state = 1)And(li025.state = 1)And(li026.state = 1)then
        DMD "", "", "dmdgoom6", eNone, eNone, eNone, 1000, True, ""
		GoombaPos = 0
        li039.State = 2
        AddScore 10000
        li022.state = 0
        li023.state = 0
        li024.state = 0
        li025.state = 0
        li026.state = 0
		li048.state = 1
		AddMultiball 1
		ChangeSong	
    End If
End If
End Sub

'*************************target bomb*************************

Sub T3_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
	BombPos = BombPos + 1
    PlaySound "BOMB"
    Addscore 1000
    TargetBonus = TargetBonus + 1
	UpdateBombLights
	checkBomb
End Sub

Sub UpdateBombLights
    Select Case BombPos
        Case 0:li032.State = 0:li033.State = 0:li034.State = 0:li035.State = 0:li036.State = 0
        Case 1:li032.State = 1:li033.State = 0:li034.State = 0:li035.State = 0:li036.State = 0:Bombdmd1
        Case 2:li032.State = 1:li033.State = 1:li034.State = 0:li035.State = 0:li036.State = 0:Bombdmd2
        Case 3:li032.State = 1:li033.State = 1:li034.State = 1:li035.State = 0:li036.State = 0:Bombdmd3
        Case 4:li032.State = 1:li033.State = 1:li034.State = 1:li035.State = 1:li036.State = 0:Bombdmd4
        Case 5:li032.State = 1:li033.State = 1:li034.State = 1:li035.State = 1:li036.State = 1:Bombdmd5
    End Select
end sub

Sub Bombdmd1
        DMD "", "", "dmdbomb1", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Bombdmd2
        DMD "", "", "dmdbomb2", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Bombdmd3
        DMD "", "", "dmdbomb3", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Bombdmd4
        DMD "", "", "dmdbomb4", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Bombdmd5
        DMD "", "", "dmdbomb5", eNone, eNone, eNone, 1000, True, ""
end sub

Sub checkBomb
    if(li032.state = 1)And(li033.state = 1)And(li034.state = 1)And(li035.state = 1)And(li036.state = 1)and(li037.State = 2)then
		BombPos = 0
        AddScore 20000
        li032.state = 0
        li033.state = 0
        li034.state = 0
        li035.state = 0
        li036.state = 0
		Exit sub	
    End If

If bMultiBallMode = False Then
    if(li032.state = 1)And(li033.state = 1)And(li034.state = 1)And(li035.state = 1)And(li036.state = 1)then
        DMD "", "", "dmdbomb6", eNone, eNone, eNone, 1000, True, ""
		BombPos = 0
        li037.State = 2
        AddScore 10000
        li032.state = 0
        li033.state = 0
        li034.state = 0
        li035.state = 0
        li036.state = 0
		li050.state = 1
		AddMultiball 1
		ChangeSong	
    End If
End If
End Sub

'*************************target boo*************************

Sub T4_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
	BooPosy = BooPosy + 1
    PlaySound "boo"
    Addscore 1000
    TargetBonus = TargetBonus + 1
	UpdateBooLights
	checkBoo
End Sub

Sub UpdateBooLights
    Select Case BooPosy
        Case 0:li027.State = 0:li028.State = 0:li029.State = 0:li030.State = 0:li031.State = 0
        Case 1:li027.State = 1:li028.State = 0:li029.State = 0:li030.State = 0:li031.State = 0:Boodmd1
        Case 2:li027.State = 1:li028.State = 1:li029.State = 0:li030.State = 0:li031.State = 0:Boodmd2
        Case 3:li027.State = 1:li028.State = 1:li029.State = 1:li030.State = 0:li031.State = 0:Boodmd3
        Case 4:li027.State = 1:li028.State = 1:li029.State = 1:li030.State = 1:li031.State = 0:Boodmd4
        Case 5:li027.State = 1:li028.State = 1:li029.State = 1:li030.State = 1:li031.State = 1:Boodmd5
    End Select
end sub

Sub Boodmd1
        DMD "", "", "dmdboo1", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Boodmd2
        DMD "", "", "dmdboo2", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Boodmd3
        DMD "", "", "dmdboo3", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Boodmd4
        DMD "", "", "dmdboo4", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Boodmd5
        DMD "", "", "dmdboo5", eNone, eNone, eNone, 1000, True, ""
end sub

Sub checkBoo
    if(li027.state = 1)And(li028.state = 1)And(li029.state = 1)And(li030.state = 1)And(li031.state = 1)And(li038.State = 2)then
		BooPosy = 0 
        AddScore 20000
        li027.state = 0
        li028.state = 0
        li029.state = 0
        li030.state = 0
        li031.state = 0
		Exit sub	
    End If

If bMultiBallMode = False Then
    if(li027.state = 1)And(li028.state = 1)And(li029.state = 1)And(li030.state = 1)And(li031.state = 1)then
        DMD "", "", "dmdboo6", eNone, eNone, eNone, 1000, True, ""
	BooPosy = 0 
        li038.State = 2
        AddScore 10000
        li027.state = 0
        li028.state = 0
        li029.state = 0
        li030.state = 0
        li031.state = 0
		li049.state = 1
		AddMultiball 1
		ChangeSong	
    End If
End If
End Sub

'*************************target spike*************************

Sub T5_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
	SpikePos = SpikePos + 1
    PlaySound "SPIKE"
    Addscore 1000
    TargetBonus = TargetBonus + 1
	UpdateSpikeLights
	checkSpike
End Sub

Sub UpdateSpikeLights
    Select Case SpikePos
        Case 0:li017.State = 0:li018.State = 0:li019.State = 0:li020.State = 0:li021.State = 0
        Case 1:li017.State = 1:li018.State = 0:li019.State = 0:li020.State = 0:li021.State = 0:Spikedmd1
        Case 2:li017.State = 1:li018.State = 1:li019.State = 0:li020.State = 0:li021.State = 0:Spikedmd2
        Case 3:li017.State = 1:li018.State = 1:li019.State = 1:li020.State = 0:li021.State = 0:Spikedmd3
        Case 4:li017.State = 1:li018.State = 1:li019.State = 1:li020.State = 1:li021.State = 0:Spikedmd4
        Case 5:li017.State = 1:li018.State = 1:li019.State = 1:li020.State = 1:li021.State = 1:Spikedmd5
    End Select
end sub

Sub Spikedmd1
        DMD "", "", "dmdspike1", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Spikedmd2
        DMD "", "", "dmdspike2", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Spikedmd3
        DMD "", "", "dmdspike3", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Spikedmd4
        DMD "", "", "dmdspike4", eNone, eNone, eNone, 1000, True, ""
end sub

Sub Spikedmd5
        DMD "", "", "dmdspike5", eNone, eNone, eNone, 1000, True, ""
end sub

Sub checkSpike
	if(li017.state = 1)And(li018.state = 1)And(li019.state = 1)And(li020.state = 1)And(li021.state = 1)And(li040.State = 2)then
		SpikePos = 0
        AddScore 20000
        li017.state = 0
        li018.state = 0
        li019.state = 0
        li020.state = 0
        li021.state = 0
		Exit sub
    End If
If bMultiBallMode = False Then
    if(li017.state = 1)And(li018.state = 1)And(li019.state = 1)And(li020.state = 1)And(li021.state = 1)then
        DMD "", "", "dmdspike6", eNone, eNone, eNone, 1000, True, ""
		SpikePos = 0
        li040.State = 2
        AddScore 10000
        li017.state = 0
        li018.state = 0
        li019.state = 0
        li020.state = 0
        li021.state = 0
		li051.state = 1
		AddMultiball 1
		ChangeSong	
    End If
End If
End Sub

'*************************target bowser*************************

Sub Target3_Hit()
    If Tilted Then Exit Sub
	if li008.State = 1 then
		FlashForMs Flasher9, 1000, 50, 0
		FlashForMs Flasher10, 1000, 50, 0
		FlashForMs Flasher7, 1000, 50, 0
		FlashForMs Flasher8, 1000, 50, 0
        AddScore 10000
		Playsound "Target1"
	exit Sub
	end if
    If li001.State = 1 then
        AddScore 5000
    end if
    If li002.State = 1 then
        AddScore 10000
    end if
	FlashForMs Flasher9, 1000, 50, 0
	FlashForMs Flasher10, 1000, 50, 0
	BowPos1 = BowPos1 + 1
    Addscore 5000
    TargetBonus = TargetBonus + 1
	UpdateBowLights
    checkBow2
End Sub

Sub UpdateBowLights
    Select Case BowPos1
        Case 0:li003.State = 0:li004.State = 0:li005.State = 0:li006.State = 0:li007.State = 0
        Case 1:li003.State = 1:li004.State = 0:li005.State = 0:li006.State = 0:li007.State = 0:BOW.ObjRotX = 0 - 8:Bowdmd1
        Case 2:li003.State = 1:li004.State = 1:li005.State = 0:li006.State = 0:li007.State = 0:BOW.ObjRotX = -8 - 8:Bowdmd2
        Case 3:li003.State = 1:li004.State = 1:li005.State = 1:li006.State = 0:li007.State = 0:BOW.ObjRotX = -16 - 8:Bowdmd3
        Case 4:li003.State = 1:li004.State = 1:li005.State = 1:li006.State = 1:li007.State = 0:BOW.ObjRotX = -24 - 8:Bowdmd4
        Case 5:li003.State = 1:li004.State = 1:li005.State = 1:li006.State = 1:li007.State = 1:BOW.ObjRotX = -32 - 8:Bowdmd5
    End Select
end sub

Sub Bowdmd1
        DMD "", "", "dmdbow1", eNone, eNone, eNone, 1000, True, "BowHIT1"
end sub

Sub Bowdmd2
        DMD "", "", "dmdbow2", eNone, eNone, eNone, 1000, True, "BowHIT2"
end sub

Sub Bowdmd3
        DMD "", "", "dmdbow3", eNone, eNone, eNone, 1000, True, "BowHIT3"
end sub

Sub Bowdmd4
        DMD "", "", "dmdbow4", eNone, eNone, eNone, 1000, True, "BowHIT4"
end sub

Sub Bowdmd5
        DMD "", "", "dmdbow5", eNone, eNone, eNone, 1000, True, "BowHIT5"
end sub

Sub checkBOW2
 if(li003.state = 1)And(li004.state = 1)And(li005.state = 1)And(li006.state = 1)And(li007.state = 1)then
        DMD "", "", "DMDBONUS9", eNone, eNone, eNone, 1300, True, ""
		Superjackpot = Superjackpot + 1
        li008.State = 1
        AddScore 1000000
		BowPos1 = 0
		BOW.ObjRotX = 0
        li003.state = 0
        li004.state = 0
        li005.state = 0
        li006.state = 0
        li007.state = 0
		PlaySoundAt "BowserLaugh", Target3
		UpdateSuperjackpot
		checkSuperjackpot
    End If
End sub

Sub UpdateSuperjackpot
    Select Case Superjackpot
        Case 0:bulb1.image = "red":bulb2.image = "red":bulb3.image = "red":bulb4.image = "red":bulb5.image = "red"
        Case 1:bulb1.image = "green":bulb2.image = "red":bulb3.image = "red":bulb4.image = "red":bulb5.image = "red"
        Case 2:bulb1.image = "green":bulb2.image = "green":bulb3.image = "red":bulb4.image = "red":bulb5.image = "red"
        Case 3:bulb1.image = "green":bulb2.image = "green":bulb3.image = "green":bulb4.image = "red":bulb5.image = "red"
        Case 4:bulb1.image = "green":bulb2.image = "green":bulb3.image = "green":bulb4.image = "green":bulb5.image = "red"
        Case 5:bulb1.image = "green":bulb2.image = "green":bulb3.image = "green":bulb4.image = "green":bulb5.image = "green"
    End Select
end sub

sub bulbchangeall
bulb1.image = "red"
bulb2.image = "red"
bulb3.image = "red"
bulb4.image = "red"
bulb5.image = "red"
end sub

Sub checkSuperjackpot
if(bulb1.image = "green")And(bulb2.image = "green")And(bulb3.image = "green")And(bulb4.image = "green")And(bulb5.image = "green")then
DMD "", "", "DMDBONUS10", eNone, eNone, eNone, 1000, True, ""
AddScore 10000000
Superjackpot = 0
bulbchangeall
    End If
end Sub

'*************************
'Boo UP/DOWN Animation
'*************************

Dim BooPos, BooDir, BooShakePos, BooShakeDir, BooHitPos, BooHits
Dim bBooUp
BooPos = -105
BooShakePos = 0

Sub BooLocation(param)
    Select Case param
        Case 1:Boo.X = 92:Boo.Y = 706:BooGhost1.Enabled = 1:BooGhost2.Enabled = 0:BooGhost3.Enabled = 0
        Case 2:Boo.X = 820:Boo.Y = 581:BooGhost2.Enabled = 1:BooGhost1.Enabled = 0:BooGhost3.Enabled = 0
        Case 3:Boo.X = 501:Boo.Y = 156:BooGhost3.Enabled = 1:BooGhost1.Enabled = 0:BooGhost2.Enabled = 0
    End Select
End Sub

Sub BooAnimTimer_Timer()
    BooShakeTimer.Enabled = 0
    BooLocationTimer.Enabled = 0
    BooPos = BooPos + BooDir
    'Boo is moving up
    If BooPos >= 0 Then
        DOF 127, DOFOff
        Me.Enabled = 0
        BooPos = 0
        BooShakeDir = 1
        BooShakeTimer.Enabled = 1
        BooLocationTimer.Enabled = 1
        bBooUp = True
    End If
    'Boo is moving down
    If BooPos <= -105 Then
        DOF 127, DOFOff
        Me.Enabled = 0
        BooPos = -105
        BooGhost1.Enabled = 0
        BooGhost2.Enabled = 0
        BooGhost3.Enabled = 0
        'IncreaseGhostLevel
    End If
    Boo.Transz = BooPos
End Sub

Sub BooShakeTimer_Timer
    BooShakePos = BooShakePos + BooShakeDir
    'Boo is moving up
    If BooShakePos > 10 Then
        BooShakeDir = -1
    End If
    'Boo is moving down
    If BooShakePos < 0 Then
        BooShakeDir = 1
    End If
    Boo.Transz = BooShakePos
End Sub

Sub BooMoveUp()
	li042.state = 2
    BooLocation INT(RND * 3 + 1)
    BooDir = 2
    BooAnimTimer.Enabled = 1
    DOF 127, DOFOn
End Sub

Sub BooMoveDown()
	li042.state = 0	
    BooDir = -2
    BooAnimTimer.Enabled = 1
    bBooUp = False
    DOF 127, DOFOn
End Sub

Sub BooGhost1_Hit()
        BooHitPos = 6:BooHitTimer.Enabled = 1
        DOF 139, DOFOn
        BooHits = BooHits + 1
        BooHitCheck
   ' End If
End Sub

Sub BooGhost2_Hit()
        BooHitPos = 6:BooHitTimer.Enabled = 1
        DOF 139, DOFOn
        BooHits = BooHits + 1
        BooHitCheck
    'End If
End Sub

Sub BooGhost3_Hit()
        BooHitPos = 6:BooHitTimer.Enabled = 1
        DOF 139, DOFOn
        BooHits = BooHits + 1
        BooHitCheck
   ' End If
End Sub

Sub BooHitTimer_Timer()
    Boo.TransX = BooHitPos
    If BooHitPos <= 0.1 AND BooHitPos >= -0.1 Then Me.Enabled = False:DOF 139, DOFOff:Exit Sub
    If BooHitPos < 0 Then
        BooHitPos = ABS(BooHitPos)- 0.1
    Else
        BooHitPos = - BooHitPos + 0.1
    End If
End Sub

Sub BooLocationTimer_Timer
    BooLocation INT(RND * 3 + 1)
End Sub

Sub BooHitCheck
PlaySound "BOOHIT"
DMD "", "", "dmdbooHIT", eNone, eNone, eNone, 1000, True, ""
End Sub

'************************** 
'Spinners
'************************** 
Sub Spinner1_Spin()'Inside this Sub is what the spinner1 will do
	If Tilted Then Exit Sub
	If li001.State=1 then 
	AddScore 1000
	end if
	If li002.State=1 then 
	AddScore 2000
	end if
	PlaySound "swish"
	Addscore 1000
	FlashForMs Flasher6, 1000, 50, 0
End Sub

Sub Spinner2_Spin()'Inside this Sub is what the spinner1 will do
	If Tilted Then Exit Sub
	If li001.State=1 then 
	AddScore 1000
	end if
	If li002.State=1 then 
	AddScore 2000
	end if
	PlaySound "Mario_spinning"
	Addscore 1000
	FlashForMs Flasher9, 1000, 50, 0
End Sub

Sub Spinner3_Spin()'Inside this Sub is what the spinner1 will do
	If Tilted Then Exit Sub
	If li001.State=1 then 
	AddScore 1000
	end if
	If li002.State=1 then 
	AddScore 2000
	end if
	PlaySound "swish"
	Addscore 1000
	FlashForMs Flasher4, 1000, 50, 0
End Sub

'*****************
'Kickers
'*****************

Sub KickbackPulse_Hit()
If save.state = 0 Then
	KickbackPulse.enabled = 0
	kickbacktimer.enabled = False
end If
   	KickbackPulse.enabled = 0
    'DMDUpdate.interval = 2000
    'DMDUpdate.Enabled = 1
    KickbackPulse.kick 0, 30
    LaserKickP1.TransY = 90
    vpmtimer.addtimer 800, "LaserKickP1.TransY = 0 '"
    Playsound "LoutSaved"
	vpmtimer.addtimer 4000, "save.state = 0 '"
End sub

Sub KickbackPulse2_Hit()
If save2.state = 0 Then
	KickbackPulse.enabled = 0
	kickbacktimer.enabled = False
end If
   	KickbackPulse2.enabled = 0
    'DMDUpdate.interval = 2000
    'DMDUpdate.Enabled = 1
    KickbackPulse2.kick 0, 30
    LaserKickP2.TransY = 90
    vpmtimer.addtimer 800, "LaserKickP2.TransY = 0 '"
    Playsound "RoutSaved"
	vpmtimer.addtimer 4000, "save2.state = 0 '"
End sub

'*****************
'Flames
'*****************

'Flashers (Flames) from JP Salas table "Diablo"

Dim Fire1Pos, Fire2Pos, Flames
Flames = Array("fire01", "fire02", "fire03", "fire04", "fire05", "fire06", "fire07", "fire08", "fire09", "fire10", "fire11", "fire12", "fire13")

Sub StartFire
    Fire1Pos = 0
    Fire2Pos = 6
    FireTimer.Enabled = 1
End Sub

Sub FireTimer_Timer
    'debug.print fire1pos
    Fire1.ImageA = Flames(Fire1Pos)
    Fire2.ImageA = Flames(Fire2Pos)
    Fire1Pos = (Fire1Pos + 1) MOD 13
    Fire2Pos = (Fire2Pos + 1) MOD 13
End Sub

'*****************
'Spinning Planets
'*****************

Sub EarthTimer_Timer
   EARTH.ObjRotz = EARTH.ObjRotz + 1
   if EARTH.ObjRotz > 360 then
       EARTH.ObjRotz = 1
   end if
end sub


Sub MoonTimer_Timer
   MOON.ObjRotz = MOON.ObjRotz + 1
   if MOON.ObjRotz > 360 then
       MOON.ObjRotz = 1
   end if
end sub

'*****************
'gate
'*****************
Sub Gate_Hit
	If li001.State=1 then 
	AddScore 250
	end if
	If li002.State=1 then 
	AddScore 500
	end if
PlaySoundAtBall "TOADSOUND"
AddScore 250
End sub

'*********
'Yoshi bucket
'*********
Dim BallInHole
Dim cBall
Dim SlotAward3
Dim EggPos

Sub Kicker001_Hit()
	BallInHole = BallInHole + 1
	Playsound "fx_ballrampdrop"
	Set cBall = ActiveBall:Me.TimerEnabled = 1
     Me.Enabled = 0
	StartSlotmachine2
End Sub

Sub SuperVukAddBall()
	If BallInHole> 0 Then
     BallInHole = BallInHole - 1
	sw45.CreateSizedball BallSize / 2
	ChangeBallImage
	Playsound "fx_popper"
	sw45.Kick 0,35,1.56
    vpmtimer.addtimer 1000, "SuperVukAddBall '" 'to kick other balls if more than 1 ball in the hole
End If
End Sub

Sub Kicker001_Timer
    Do While cBall.Z > 0
        cBall.Z = cBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
    Me.Enabled = 1
End Sub

SlotAward3 = Array("dmdyoshi1", "dmdyoshi2", "dmdyoshi3", "dmdyoshi4")

Sub StartSlotmachine2() ' uses the HolePos variable
    Dim i
	DMDFlush
For i = 0 to 3
	'Playsound "itemreel"
    DMD "", "", SlotAward3(i), eNone, eNone, eNone, 50, False, "fx_spinner"
Next
    'DOF 142, DOFPulse
    vpmtimer.AddTimer 1500, "GiveSlotAward2 '"
End Sub

Sub GiveSlotAward2()
    Dim tmp
    DMDFlush
    tmp = INT(RND * 4)
    DMD "", "", SlotAward3(tmp),  eNone, eNone, eBlinkFast, 1000, True, ""

    Select Case tmp
        Case 0:AddScore INT(100000 * RND * 9):Playsound "YOSHI1"
		Case 1:eggupdate:Playsound "YOSHI2"
        Case 2:EnableBallSaver 30:Playsound "YOSHI3"
		Case 3:AddScore INT(10000 * RND * 9):Playsound "YOSHI4"
	End Select
	UpdateEggLights
	CheckEggLights
	GiEffect 1
	vpmtimer.addtimer 1500, "SuperVukAddBall '"
End Sub

sub eggupdate
EggPos = EggPos + 1
end sub

Sub UpdateEggLights
    Select Case EggPos
        Case 0:li065.State = 0:li066.State = 0:li067.State = 0:li068.State = 0:li069.State = 0
        Case 1:li065.State = 1:li066.State = 0:li067.State = 0:li068.State = 0:li069.State = 0
        Case 2:li065.State = 1:li066.State = 1:li067.State = 0:li068.State = 0:li069.State = 0
        Case 3:li065.State = 1:li066.State = 1:li067.State = 1:li068.State = 0:li069.State = 0
        Case 4:li065.State = 1:li066.State = 1:li067.State = 1:li068.State = 1:li069.State = 0
        Case 5:li065.State = 1:li066.State = 1:li067.State = 1:li068.State = 1:li069.State = 1
    End Select
end sub

sub CheckEggLights
 if(li065.state = 1)And(li066.state = 1)And(li067.state = 1)And(li068.state = 1)And(li069.state = 1)then
        DMD "", "", "DMDBONUS9", eNone, eNone, eNone, 1300, True, ""
        'li008.State = 1
		EggPos = 0
        AddScore 100000
		li065.state = 0
		li066.state = 0
		li067.state = 0
		li068.state = 0
		li069.state = 0
		AddMultiball 3
		ChangeSong	
    End If
end sub

'*********
'castle change
'*********
Dim BallInHole1
Dim dBall
Dim SlotAward4

Sub Kicker002_Hit()
	'If bMultiBallMode Then
	FlashForMs Flasher5, 1000, 50, 0
	BallInHole1 = BallInHole1 + 1
    Set dBall = ActiveBall:Me.TimerEnabled = 1
	Playsound "BUISS"
    'vpmTimer.AddTimer 1200, "SuperVukAddBall1'"
    Me.Enabled = 0
	startSlotmachine4
End Sub

Sub SuperVukAddBall1()
	If BallInHole1> 0 Then
        BallInHole1 = BallInHole1 - 1
	sw35.CreateSizedball BallSize / 2
	ChangeBallImage
	Playsound "fx_popper"
	sw35.Kick 0,35,1.56
 vpmtimer.addtimer 1000, "SuperVukAddBall1 '" 
end If
End Sub

Sub Kicker002_Timer
    Do While dBall.Z > 0
        dBall.Z = dBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
	Me.Enabled = 1
End Sub

SlotAward4 = Array("s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "s12", "s13", "s14", "s15")

Sub StartSlotmachine4() ' uses the HolePos variable
    Dim i
	DMDFlush
For i = 0 to 14
	Playsound "slot4"
    DMD "", "", SlotAward4(i), eNone, eNone, eNone, 50, False, ""
Next
    'DOF 142, DOFPulse
    vpmtimer.AddTimer 4200, "GiveSlotAward4 '"
End Sub

Sub GiveSlotAward4()
    Dim tmp
    DMDFlush
    tmp = INT(RND * 14)
    DMD "", "", SlotAward4(tmp),  eNone, eNone, eBlinkFast, 1000, True, "CARDS"

    Select Case tmp
        Case 0:AddScore 5000:score1
		Case 1:AddScore 5000:score1
        Case 2:AddScore 50000:score2
		Case 3:AddScore 50000:score2
		Case 4:AddScore 500000:score3
        Case 5:AddScore 5000:score1
        Case 6:AddScore 5000:score1
        Case 7:AddScore 50000:score2
		Case 8:AddScore 50000:score2
		Case 9:AddScore 500000:score3
		Case 10:AddScore 5000:score1
		Case 11:AddScore 5000:score1
		Case 12:AddScore 50000:score2
		Case 13:AddScore 50000:score2
		Case 14:AddScore 500000:score3
    End Select
	'checkgalaxy
	GiEffect 1
	vpmTimer.AddTimer 1200, "SuperVukAddBall1'"
End Sub

Sub score1
        DMD "", "", "DMDBONUS11", eNone, eNone, eNone, 1000, True, ""
end sub

Sub score2
        DMD "", "", "DMDBONUS7", eNone, eNone, eNone, 1000, True, ""
end sub

Sub score3
        DMD "", "", "DMDBONUS3", eNone, eNone, eNone, 1000, True, ""
end sub

'************************************************
'**************Bank Animation*****************
'************************************************
Dim BankDir, BankPos
	
Sub sw9_Hit
	PlaySound "BLOCKHIT"
	li009.state = 1
	CheckbBank
End Sub

Sub sw10_Hit
	PlaySound "BLOCKHIT"
	li010.state = 1
	CheckbBank

End Sub

Sub sw11_Hit 
	PlaySound "BLOCKHIT"
	li011.state = 1
	CheckbBank
End Sub

Sub CheckbBank
    If(li009.State = 1) And(li010.State = 1) And(li011.State = 1) Then
    'DMD "_", CL(0, "BANK BONUS"), "_", eNone, eBlink, eNone, 3000, True, "BONUS"
	AddScore 50000
	li009.State=0
	li010.State=0
	li011.State=0
	'Attendi=0
	sw9.IsDropped=true
	sw10.IsDropped=true
	sw11.IsDropped=true
	BankDir = 1
	bankdown.enabled = 1
	PlaySound "bankDOWN"
    End If
End Sub

Sub BankTimer_Timer
	TargetBank.TransY = -BankPos   	
	BankPos = BankPos + BankDir 
	If BankPos < 0 Then BankPos = 0: Me.Enabled = 0
End Sub

sub bankdown_Timer
bankup.enabled = 0
If TargetBank.z > -25 then 
    TargetBank.z = TargetBank.z -5
Else
	bankdown.Enabled = 0
End If
'li008.state = 1
end sub

sub bankup_Timer
bankdown.enabled = 0
If TargetBank.z < 30 Then
    TargetBank.z = TargetBank.z + 5
Else
	bankup.Enabled = 0
End If
end sub

'*****************
' objects shaking
'*****************

Dim M1Shake, M2Shake, L1Shake, L2Shake, W1Shake, W2Shake, P1Shake, P2Shake 

Sub M1Shaker()
    M1Shake = 6
    M1Timer.Enabled = True
End Sub

Sub M1Timer_Timer()
    M1.Transz = M1Shake / 2
    If M1Shake = 0 Then Me.Enabled = False:Exit Sub
    If M1Shake <0 Then
        M1Shake = ABS(M1Shake)- 0.1
    Else
        M1Shake = - M1Shake + 0.1
    End If
End Sub

Sub M2Shaker()
    M2Shake = 6
    M2Timer.Enabled = True
End Sub

Sub M2Timer_Timer()
    M2.Transz = M2Shake / 2
    If M2Shake = 0 Then Me.Enabled = False:Exit Sub
    If M2Shake <0 Then
        M2Shake = ABS(M2Shake)- 0.1
    Else
        M2Shake = - M2Shake + 0.1
    End If
End Sub

'*****************

Sub L1Shaker()
    L1Shake = 6
    L1Timer.Enabled = True
End Sub

Sub L1Timer_Timer()
    L1.Transz = L1Shake / 2
    If L1Shake = 0 Then Me.Enabled = False:Exit Sub
    If L1Shake <0 Then
        L1Shake = ABS(L1Shake)- 0.1
    Else
        L1Shake = - L1Shake + 0.1
    End If
End Sub

Sub L2Shaker()
    L2Shake = 6
    L2Timer.Enabled = True
End Sub

Sub L2Timer_Timer()
    L2.Transz = L2Shake / 2
    If L2Shake = 0 Then Me.Enabled = False:Exit Sub
    If L2Shake <0 Then
        L2Shake = ABS(L2Shake)- 0.1
    Else
        L2Shake = - L2Shake + 0.1
    End If
End Sub

'*****************

Sub P1Shaker()
    P1Shake = 6
    P1Timer.Enabled = True
End Sub

Sub P1Timer_Timer()
    P1.Transz = P1Shake / 2
    If P1Shake = 0 Then Me.Enabled = False:Exit Sub
    If P1Shake <0 Then
        P1Shake = ABS(P1Shake)- 0.1
    Else
        P1Shake = - P1Shake + 0.1
    End If
End Sub

Sub P2Shaker()
    P2Shake = 6
    P2Timer.Enabled = True
End Sub

Sub P2Timer_Timer()
    P2.Transz = P2Shake / 2
    If P2Shake = 0 Then Me.Enabled = False:Exit Sub
    If P2Shake <0 Then
        P2Shake = ABS(P2Shake)- 0.1
    Else
        P2Shake = - P2Shake + 0.1
    End If
End Sub

'*****************

Sub W1Shaker()
    W1Shake = 6
    W1Timer.Enabled = True
End Sub

Sub W1Timer_Timer()
    W1.Transz = W1Shake / 2
    If W1Shake = 0 Then Me.Enabled = False:Exit Sub
    If W1Shake <0 Then
        W1Shake = ABS(W1Shake)- 0.1
    Else
        W1Shake = - W1Shake + 0.1
    End If
End Sub

Sub W2Shaker()
    W2Shake = 6
    W2Timer.Enabled = True
End Sub

Sub W2Timer_Timer()

    W2.Transz = W2Shake / 2
    If W2Shake = 0 Then Me.Enabled = False:Exit Sub
    If W2Shake <0 Then
        W2Shake = ABS(W2Shake)- 0.1
    Else
        W2Shake = - W2Shake + 0.1
    End If
End Sub

'*****************
' Thowmp move
'*****************

Dim TowDir
TowDir = -7 'this is both the direction and the speed

Sub TowbTimer_Timer
Towb.z = Towb.z + TowDir
If Towb.z < 2 Then TowDir = 2
If Towb.z > 95 Then TowDir = -7
End Sub


'*****************
' galaxys
'*****************

Dim SlotAward2

Sub Kicker003_Hit()
	FlashForMs Flasher9, 1000, 50, 0
	FlashForMs Flasher10, 1000, 50, 0
	If li001.State=1 then 
	AddScore 1000
	end if
	If li002.State=1 then 
	AddScore 2000
	end if
StartSlotmachine
end sub

SlotAward2 = Array("dmdplaneet1", "dmdplaneet2", "dmdplaneet3", "dmdplaneet4", "dmdplaneet5", "dmdplaneet6", "dmdplaneet7", "dmdplaneet8")

Sub StartSlotmachine() ' uses the HolePos variable
    Dim i
	DMDFlush
For i = 0 to 7
	Playsound "itemreel4"
    DMD "", "", SlotAward2(i), eNone, eNone, eNone, 50, False, ""
Next
    'DOF 142, DOFPulse
    vpmtimer.AddTimer 3500, "GiveSlotAward '"
End Sub

Sub GiveSlotAward()
    Dim tmp
    DMDFlush
    tmp = INT(RND * 8)
    DMD "", "", SlotAward2(tmp),  eNone, eNone, eBlinkFast, 1000, True, "gotitem"

    Select Case tmp
        Case 0:AddScore 5000:li057.state = 1
		Case 1:AddScore 5000:li058.state = 1
        Case 2:AddScore 5000:li059.state = 1
		Case 3:AddScore 5000:li060.state = 1
		Case 4:AddScore 5000:li061.state = 1
        Case 5:AddScore 5000:li062.state = 1
        Case 6:AddScore 5000:li063.state = 1
        Case 7:AddScore 5000:li064.state = 1
    End Select
	checkgalaxy
	GiEffect 1
	vpmtimer.addtimer 1500, "ejectkicker '"
End Sub

sub checkgalaxy
 if(li057.state = 1)And(li058.state = 1)And(li059.state = 1)And(li060.state = 1)And(li061.state = 1)And(li062.state = 1)And(li063.state = 1)And(li064.state = 1)then
        DMD "", "", "dmdplaneet9", eNone, eNone, eNone, 1000, True, ""
        AddScore 200000
		li057.state = 0
		li058.state = 0
		li059.state = 0
		li060.state = 0
		li061.state = 0
		li062.state = 0
		li063.state = 0
		li064.state = 0
		AddMultiball 3
		ChangeSong	
    End If
end sub

sub ejectkicker
PlaySound "fx_popper"
Kicker003.Kick 270,10
end sub

'*****************
' Flag
'*****************

Dim FlagDir
FlagDir = 5 'this is both the direction, if + goes up, if - goes down, and also the speed

Sub FlagTimer_Timer
    Flag.z = Flag.z + FlagDir
    If Flag.z > 235 Then FlagDir = -5 'goes down
    If Flag.z < 115 Then FlagTimer.Enabled = 0
End Sub
