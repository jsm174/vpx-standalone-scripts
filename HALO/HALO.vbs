' ****************************************************************
'                       VISUAL PINBALL X
'                		Halo
'                       Version 1.0.0
'						started 11-10-2020
'						Finished 05-05-2021 
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
Const cGameName = "halo"
Const TableName = "halo"
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
Dim plungerIM
dim mode1TimerCount
dim mode2TimerCount
dim rampl1
dim rampl2
dim rampr1
dim rampr2
dim rampm1
dim rampm2
dim bonusyscorechecker
dim tempbonusaf
dim cBall
dim waveattack
dim missionmodes
dim mission1
dim mission2
dim mission3
dim mission4
dim mission5
dim mission6
dim mission7
dim mission8
dim mission9
dim mission10
dim mission11
dim mission12
dim mission13
dim mission14
dim mission15
dim Pslot
dim countr
dim countr2
dim countr3
dim countr4
dim countr5
dim countr6
dim countr7
dim countr8
dim countr9
dim countr10
dim countr11
dim countr12
dim countr13
dim countr14
dim countr15
dim countr16
dim countr17
dim countr18
dim countr19
dim countr20
dim countr21
dim countr22
dim countr23
dim countr24
dim countr25
dim countr26
dim countr27
dim countr28
dim countr29
dim countr30
dim countr31
dim countr32
dim countr33
dim countr34
dim countr35
dim countr36
dim countr37
dim countr38
dim countr39
dim countr40
dim countr41
dim countr50
dim countr51
dim countr52
dim countr53
dim countr54
dim cletter
dim mgrund
dim mskilhigh
dim mskillow  
dim melite
dim mjackal
dim mfloodc
dim mfloode
dim mlogo
dim mgrenade
dim mskull
dim wavy
dim kildys
dim mcomplete
dim LowerFlippersActive
dim bonustime
dim bonus1hits
dim bonus2hits
dim bonus3hits
dim bonus4hits
dim bonus5hits
dim mreactor
dim mreactor2
dim halovara
dim halovara1
dim halovarb
dim halovarb1
dim enemylft
' core.vbs variables

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
	Dim i
	'Randomize

'reset HighScore
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
bonusyscore
    'BigUfoUpdate
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
        playsound "pullfire"
		'PlaySoundAt "fx_plungerpull", plunger
        'PlaySoundAt "fx_reload", plunger
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
						'UpdateMusicNow
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
						'UpdateMusicNow
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                        DMD CL(0, "CREDITS " & Credits), CL(1, "INSERT COIN"), "", eNone, eBlink, eNone, 500, True, "so_nocredits"
                    End If
                End If
            End If
    End If ' If (GameInPlay)

    If LowerFlippersActive Then
		If keycode = RightFlipperkey then
		    PlaySound "fx_flipperup", 0, 1, 0, 0.25
            RightFlipper001.RotateToEnd
		end If
			If keycode = LeftFlipperkey then
		    PlaySound "fx_flipperup", 0, 1, 0, 0.25
            LeftFlipper001.RotateToEnd
		end If
		Else
        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1
    End If

'table keys
'If keycode = RightMagnaSave or keycode = LeftMagnasave Then ShowPost 
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = PlungerKey Then
        Plunger.Fire
		PlaySound "fire"
        'PlaySoundAt "pullfire", plunger
        'If bBallInPlungerLane Then PlaySoundAt "fire", plunger
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

    If LowerFlippersActive Then
		If keycode = RightFlipperkey then
		    PlaySound "fx_flipperup", 0, 1, 0, 0.25
            RightFlipper001.RotatetoStart
		end If
			If keycode = LeftFlipperkey then
		    PlaySound "fx_flipperup", 0, 1, 0, 0.25
            LeftFlipper001.RotatetoStart
		end If
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

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
        DMD "_", CL(1, "CAREFUL!"), "", eNone, eBlinkFast, eNone, 500, True, "careful"
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
		playSound "tilty2"
        DMDFlush
        DMD "", "", "TILT", eNone, eNone, eBlink, 200, False, ""
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
        Case 0:PlaySong "1"
        Case 1:PlaySong "2"
        Case 2:PlaySong "3"
        Case 3:PlaySong "4"
        Case 4:PlaySong "5"
        Case 5:PlaySong "M_end"
        'Case 6:PlaySong "chooseplayer2"
    End Select
end sub

'********************
' Play random quotes
'********************

Sub PlayQuoteC
    Dim tmp
    tmp = INT(RND * 84) + 1
    PlaySound "cor_" &tmp
End Sub


Sub PlayQuoteJ
    Dim tmp
    tmp = INT(RND * 93) + 1
    PlaySound "jon_" &tmp
End Sub

Sub PlayQuoteDie
    Dim tmp
    tmp = INT(RND * 12) + 1
    PlaySound "TR_" &tmp
End Sub

Sub PlayQuoteCovenant
    Dim tmp
    tmp = INT(RND * 3) + 1
    PlaySound "COV_" &tmp
End Sub

Sub PlayQuoteGrundy
    Dim tmp
    tmp = INT(RND * 67) + 1
    PlaySound "gru_" &tmp
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
	
		If BOT(b).Z < 0 Then aBallShadow(b).Height = BOT(b).Z -70

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

Sub TRIGL_Hit()
PlaySound "fx_metalrolling"
'PlaySoundAt "fx_metalrolling", TRIGL
end sub

Sub TRIGR_Hit()
PlaySound "fx_metalrolling"
'PlaySoundAt "fx_metalrolling", TRIGR
end sub

Sub RHelp1_Hit()
	RampBonus2 = RampBonus2 + 1
    StopSound "fx_metalrolling"
    PlaySound "fx_ballrampdrop"
	'PlaySoundAtBall "fx_ballrampdrop"
End Sub

Sub RHelp2_Hit()
	RampBonus2 = RampBonus2 + 1
    StopSound "fx_metalrolling"
    PlaySound "fx_ballrampdrop"
'    PlaySoundAtBall"fx_ballrampdrop"
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

    'update music
    UpdateMusic = 0

'reset mission, bonus and Keys
keysback
disablebonusgames
stopmissions

'resetship
covenantship.transx = 0
covenantship.transy = 0
covenantship.roty = 0
raket.Visible = False
raket.z = 113
raket.TransX = 10
raket.TransY = 10
raket.TransZ = 10
raket.TransY = 0
shipshakertimer.enabled = false
Target012.IsDropped = False
bringbankback
explosion1.visible = false
explosion2.visible = false
explosion3.visible = false
explosion4.visible = false
explosion5.visible = false
explosion6.visible = false
explosion001.visible = false
explosion002.visible = false
explosion003.visible = false
explosion004.visible = false
explosion005.visible = false
explosion006.visible = false
explosion007.visible = false
explosion008.visible = false
covenantship.transy = -601
haloLstop.IsDropped=true
haloRstop.IsDropped=true
reactoryA.Image = "Generatorkopie"
reactoryB.Image = "Generatorkopie"

    'Reset any table specific variables
	waveattack = 0
	rampr1 = 0
	rampr2 = 0
	rampl1 = 0
	rampl2 = 0
	rampm1 = 0
	rampm2 = 0
	missionmodes = 0
	mission1 = 0
	mission2 = 0
	mission3 = 0
	mission4 = 0
	mission5 = 0
	mission6 = 0
	mission7 = 0	
	mission8 = 1
	mission9 = 1
	mission10 = 1
	mission11 = 0
	mission12 = 0
	mission13 = 0
	mission14 = 0
	mission15 = 0
countr = 0
countr2 = 0
countr4 = 0
countr3 = 0
countr5 = 0
countr6 = 0
countr7 = 0
countr8 = 0
countr9 = 0
countr10 = 0
countr11 = 0
countr12 = 0
countr13 = 0
countr14 = 0
countr15 = 0
countr16 = 0
countr17 = 0
countr18 = 0
countr19 = 0
countr20 = 0
countr21 = 0
countr22 = 0
countr23 = 0
countr24 = 0
countr25 = 0
countr26 = 0
countr27 = 0
countr28 = 0
countr29 = 0
countr30 = 0
countr31 = 0
countr32 = 0
countr33 = 0
countr34 = 0
countr35 = 0
countr36 = 0
countr37 = 0
countr38 = 0
countr39 = 0
countr40 = 0
countr41 = 1
countr50 = 0
countr51 = 0
countr52 = 0
countr53 = 0
countr54 = 0
mskilhigh = 0 
mskillow = 0  
cletter = 0
mgrund = 0
melite = 0
mjackal = 0
mfloodc =0
mfloode = 0
mlogo = 0
mgrenade = 0
mskull = 0
wavy = 0
kildys = 0
mcomplete = 0
bumperHits = 100
bonustime = 0
bonus1hits = 0
bonus2hits = 0
bonus3hits = 0
bonus4hits = 0
bonus5hits = 0
mreactor = 0
mreactor2 = 0
halovara = 0
halovara1 = 0
halovarb = 0
halovarb1 = 0
enemylft = 0

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
PlaySound "johnsen"
DMD "", "", "JS070", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS071", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS072", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS073", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS074", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS075", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS076", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS077", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS078", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS079", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS080", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS081", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS082", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS083", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS084", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS085", eNone, eNone, eNone, 100, True, ""
DMD "", "", "JS086", eNone, eNone, eNone, 100, True, ""

    vpmtimer.addtimer 1700, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to

' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
	'LightSeq001.Play SeqUpOn, 25, 1000
	UpdateMusicNow
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    ' make sure the correct display is upto date
    AddScore 0
	bonusyscorechecker = 0
		LightSeq001.Play SeqUpOn, 25, 1000

    ' set the current players bonus multiplier back down to 1X
    BonusMultiplier(CurrentPlayer) = 1
    'UpdateBonusXLights
	
' reset any drop targets, lights, game Mode etc..
    
   'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'Reset any table specific
	'bonusyscore 0
	BumperBonus = 0
	HoleBonus = 0
	RampBonus2 = 0
    ResetNewBallVariables
    ResetNewBallLights()
	'Multiball=false	
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
	'Dim BonusDelayTime
	' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

	'LightSeqAttract.Play SeqBlinking, , 5, 150
StopmodeEndofBall
stopmissions
PlaySound "ballosty3"
'StopSong
'bonuscheckie
tempbonusaf = 33
GameTimer.enabled = false
lichtTimer.enabled = True
stopidlchiefke
chblRtimer.enabled = 1
chblLtimer.enabled = 1

		vpmtimer.addtimer 3500, "endofballcontinue '"
end sub

sub endofballcontinue
	
	Dim BonusDelayTime

    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0

    'If NOT Tilted Then
	If(Tilted = False) Then


'tempbonusaf = 64
'GameTimer.enabled = false
'lichtTimer.enabled = True


        'Number of Target hits
        AwardPoints = TargetBonus * 2000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints)), CL(1, "TARGET BONUS " & TargetBonus), "", eBlink, eNone, eNone, 500, False, "bonusaff"

        AwardPoints = RampBonus2 * 25000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints) ), CL(1, "RAMP BONUS: " & RampBonus2), "", eBlink, eNone, eNone, 500, False, "bonusaff"

        AwardPoints = kildys * 5000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints) ), CL(1, "ENEMIE BONUS: " & kildys), "", eBlink, eNone, eNone, 500, False, "bonusaff"

        AwardPoints = BumperBonus * 100000
        TotalBonus = TotalBonus + AwardPoints
        DMD CL(0, FormatScore(AwardPoints) ), CL(1, "NEEDLER BONUS: " & BumperBonus), "", eBlink, eNone, eNone, 500, False, "bonusaff"
        
		DMD CL(0, FormatScore(TotalBonus) ), CL(1, "TOTAL BONUS " & " XII" & BonusMultiplier(CurrentPlayer) ), "", eBlinkFast, eNone, eNone, 1100, True, "bonusaff"
        TotalBonus = TotalBonus * BonusMultiplier(CurrentPlayer)
        
		AddScore TotalBonus

		' add a bit of a delay to allow for the bonus points to be shown & added up
		vpmtimer.addtimer 3200, "EndOfBall2 '"
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
	GameTimer.enabled = True
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
        'DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, "vo_extraball"
		DMD "", "", "dmdshootagain", eNone, eNone, eNone, 1000, True, ""

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

stopidlchiefke
chdyLtimer.enabled = 1
chdyRtimer.enabled = 1

        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode
		StopSong
		'DMD CL(0, "GAME OVER") "", eNone, 13000, True, ""
'DMD "", CL(1, "GAME OVER"), "", eNone, eNone, eNone, 6000, False, ""
		PlaySound "GAMEOVER"
DMD CL(0, "WAVES"), CL(1, "COMPLETED " &wavy), "", eNone, eNone, eNone, 1000, True, ""
DMD CL(0, "ENEMIES"), CL(1, "KILLED " &kildys), "", eNone, eNone, eNone, 1000, True, ""
DMD CL(0, "MISSIONS"), CL(1, "COMPLETED " &mcomplete), "", eNone, eNone, eNone, 1000, True, ""
DMD "", "", "dmdgameover1", eNone, eNone, eNone, 500, True, ""
DMD "", "", "dmdgameover2", eNone, eNone, eNone, 1000, True, ""
DMD "", "", "dmdgameover3", eNone, eNone, eNone, 1500, True, ""
        ' set the machine into game over mode
        vpmtimer.addtimer 6000, "EndOfGame() '"

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
	DMD "", "", "dmdextraball", eNone, eNone, eNone, 1000, True, ""
    'vpmtimer.addtimer 1250, "CreateNewBall() '"


           ' you may wish to put something on a display or play a sound at this point

            
        Else

			If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
                    bMultiBallMode = False
					'multitimer.enabled = 0
					timerquotemulti.enabled = 0
					stopmultigrundy
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
        PlaySound "fire"
        bAutoPlunger = False
    End If	
'StopSong
    DMDScoreNow
    bBallInPlungerLane = True
	flashplaat1.ImageA = "flsshootball"
   ' DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eBlink, eNone, 1000, True, ""
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
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
  '  DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eNone, eNone, 800, True, ""
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
    'DMD "_", CL(1, ("EXTRA BALL WON") ), "", eNone, eBlink, eNone, 1000, True, SoundFXDOF("lifeextraball", 122, DOFPulse, DOFKnocker)
    DMD "", "", "dmdextraball", eNone, eNone, eNone, 1000, True, "lifeextraball"
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
    'ChangeSong
	PlaySound "highscoree"
    hsLetterFlash = 0

    hsEnteredDigits(0) = " "
    hsEnteredDigits(1) = " "
    hsEnteredDigits(2) = " "
    hsCurrentDigit = 0

    'hsValidLetters = " ABCDEFGHIJKLMNOPQRSTUVWXYZ'<>*+-/=\^0123456789`" ' ` is back arrow
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
        'if(mid(hsValidLetters, hsCurrentLetter, 1) <> "`") then
		 if(mid(hsValidLetters, hsCurrentLetter, 1) <> "<") then
            'playsound "fx_Enter3"
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
            FlexDMD.Height = 36
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
StartElectric1
StartElectric2
    Dim i, j
    DMDFlush()
    deSpeed = 30
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

' #####################################
' ###### Flashers flupper #####
' #####################################

Dim TestFlashers, TableRef, FlasherLightIntensity, FlasherFlareIntensity, FlasherOffBrightness

								' *********************************************************************
TestFlashers = 0				' *** set this to 1 to check position of flasher object 			***
Set TableRef = Table1   		' *** change this, if your table has another name       			***
FlasherLightIntensity = 1		' *** lower this, if the VPX lights are too bright (i.e. 0.1)		***
FlasherFlareIntensity = 1		' *** lower this, if the flares are too bright (i.e. 0.1)			***
FlasherOffBrightness = 0.5		' *** brightness of the flasher dome when switched off (range 0-2)	***
								' *********************************************************************

Dim ObjLevel(20), objbase(20), objlit(20), objflasher(20), objlight(20)
Dim tablewidth, tableheight : tablewidth = TableRef.width : tableheight = TableRef.height
'initialise the flasher color, you can only choose from "green", "red", "purple", "blue", "white" and "yellow"
'InitFlasher 1, "green" : 
InitFlasher 1, "blue" : InitFlasher 9, "blue" : InitFlasher 3, "red" : InitFlasher 4, "red" : InitFlasher 5, "red" : 
InitFlasher 6, "red" : InitFlasher 7, "red" ': InitFlasher 8, "red"
'InitFlasher 9, "red" ': InitFlasher 10, "red" : InitFlasher 11, "white" 
' rotate the flasher with the command below (first argument = flasher nr, second argument = angle in degrees)
'RotateFlasher 4,17 : RotateFlasher 5,0 : RotateFlasher 6,90
'RotateFlasher 7,0 : RotateFlasher 8,0 
'RotateFlasher 9,-45 : RotateFlasher 10,90 : RotateFlasher 11,90

Sub InitFlasher(nr, col)
	' store all objects in an array for use in FlashFlasher subroutine
	Set objbase(nr) = Eval("Flasherbase" & nr) : Set objlit(nr) = Eval("Flasherlit" & nr)
	Set objflasher(nr) = Eval("Flasherflash" & nr) : Set objlight(nr) = Eval("Flasherlight" & nr)
	' If the flasher is parallel to the playfield, rotate the VPX flasher object for POV and place it at the correct height
	If objbase(nr).RotY = 0 Then
		objbase(nr).ObjRotZ =  atn( (tablewidth/2 - objbase(nr).x) / (objbase(nr).y - tableheight*1.1)) * 180 / 3.14159
		objflasher(nr).RotZ = objbase(nr).ObjRotZ : objflasher(nr).height = objbase(nr).z + 35
	End If
	' set all effects to invisible and move the lit primitive at the same position and rotation as the base primitive
	objlight(nr).IntensityScale = 0 : objlit(nr).visible = 0 : objlit(nr).material = "Flashermaterial" & nr
	objlit(nr).RotX = objbase(nr).RotX : objlit(nr).RotY = objbase(nr).RotY : objlit(nr).RotZ = objbase(nr).RotZ
	objlit(nr).ObjRotX = objbase(nr).ObjRotX : objlit(nr).ObjRotY = objbase(nr).ObjRotY : objlit(nr).ObjRotZ = objbase(nr).ObjRotZ
	objlit(nr).x = objbase(nr).x : objlit(nr).y = objbase(nr).y : objlit(nr).z = objbase(nr).z
	objbase(nr).BlendDisableLighting = FlasherOffBrightness
	' set the texture and color of all objects
	select case objbase(nr).image
		Case "dome2basewhite" : objbase(nr).image = "dome2base" & col : objlit(nr).image = "dome2lit" & col : 
		Case "ronddomebasewhite" : objbase(nr).image = "ronddomebase" & col : objlit(nr).image = "ronddomelit" & col
		Case "domeearbasewhite" : objbase(nr).image = "domeearbase" & col : objlit(nr).image = "domeearlit" & col
	end select
	If TestFlashers = 0 Then objflasher(nr).imageA = "domeflashwhite" : objflasher(nr).visible = 0 : End If
	select case col
		Case "blue" :   objlight(nr).color = RGB(4,120,255) : objflasher(nr).color = RGB(200,255,255) : objlight(nr).intensity = 5000
		Case "green" :  objlight(nr).color = RGB(12,255,4) : objflasher(nr).color = RGB(12,255,4)
		Case "red" :    objlight(nr).color = RGB(255,32,4) : objflasher(nr).color = RGB(255,32,4)
		Case "purple" : objlight(nr).color = RGB(230,49,255) : objflasher(nr).color = RGB(255,64,255) 
		Case "yellow" : objlight(nr).color = RGB(200,173,25) : objflasher(nr).color = RGB(255,200,50)
		Case "white" :  objlight(nr).color = RGB(255,240,150) : objflasher(nr).color = RGB(100,86,59)
	end select
	objlight(nr).colorfull = objlight(nr).color
	If TableRef.ShowDT and ObjFlasher(nr).RotX = -45 Then 
		objflasher(nr).height = objflasher(nr).height - 20 * ObjFlasher(nr).y / tableheight
		ObjFlasher(nr).y = ObjFlasher(nr).y + 10
	End If
End Sub

Sub RotateFlasher(nr, angle) : angle = ((angle + 360 - objbase(nr).ObjRotZ) mod 180)/30 : objbase(nr).showframe(angle) : objlit(nr).showframe(angle) : End Sub

Sub FlashFlasher(nr)
	If not objflasher(nr).TimerEnabled Then objflasher(nr).TimerEnabled = True : objflasher(nr).visible = 1 : objlit(nr).visible = 1 : End If
	objflasher(nr).opacity = 1000 *  FlasherFlareIntensity * ObjLevel(nr)^2.5
	objlight(nr).IntensityScale = 0.5 * FlasherLightIntensity * ObjLevel(nr)^3
	objbase(nr).BlendDisableLighting =  FlasherOffBrightness + 10 * ObjLevel(nr)^3	
	objlit(nr).BlendDisableLighting = 10 * ObjLevel(nr)^2
	UpdateMaterial "Flashermaterial" & nr,0,0,0,0,0,0,ObjLevel(nr),RGB(255,255,255),0,0,False,True,0,0,0,0 
	ObjLevel(nr) = ObjLevel(nr) * 0.9 - 0.01
	If ObjLevel(nr) < 0 Then objflasher(nr).TimerEnabled = False : objflasher(nr).visible = 0 : objlit(nr).visible = 0 : End If
End Sub

Sub FlasherFlash1_Timer() : FlashFlasher(1) : End Sub 
Sub FlasherFlash2_Timer() : FlashFlasher(2) : End Sub 
Sub FlasherFlash3_Timer() : FlashFlasher(3) : End Sub 
Sub FlasherFlash4_Timer() : FlashFlasher(4) : End Sub 
Sub FlasherFlash5_Timer() : FlashFlasher(5) : End Sub 
Sub FlasherFlash6_Timer() : FlashFlasher(6) : End Sub 
Sub FlasherFlash7_Timer() : FlashFlasher(7) : End Sub
Sub FlasherFlash8_Timer() : FlashFlasher(8) : End Sub
Sub FlasherFlash9_Timer() : FlashFlasher(9) : End Sub
Sub FlasherFlash10_Timer() : FlashFlasher(10) : End Sub
Sub FlasherFlash11_Timer() : FlashFlasher(11) : End Sub


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
	DMD "", "", "dmdintro1", eNone, eNone, eNone, 3000, True, ""
    DMD "", "", "introh00", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh01", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh02", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh03", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh04", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh05", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh06", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh07", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh08", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh09", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh10", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh11", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh12", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh13", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh14", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh15", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh16", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh17", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh18", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh19", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh20", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh21", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh22", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh23", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh24", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh25", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh26", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh27", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh28", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh29", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh30", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh31", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh32", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh33", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh34", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh35", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh36", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh37", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh38", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "introh39", eNone, eNone, eNone, 2000, True, ""
    DMD CL(0, "HIGHSCORES"), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 20, False, ""
    DMD CL(0, "HIGHSCORES"), "", "", eBlinkFast, eNone, eNone, 1000, False, ""
    DMD CL(0, "HIGHSCORES"), "1> " &HighScoreName(0) & " " &FormatScore(HighScore(0) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "2> " &HighScoreName(1) & " " &FormatScore(HighScore(1) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "3> " &HighScoreName(2) & " " &FormatScore(HighScore(2) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD "_", "4> " &HighScoreName(3) & " " &FormatScore(HighScore(3) ), "", eNone, eScrollLeft, eNone, 2000, False, ""
    DMD Space(dCharsPerLine(0) ), Space(dCharsPerLine(1) ), "", eScrollLeft, eScrollLeft, eNone, 500, False, ""
End Sub

Sub StartAttractMode
stopdyaniL
stopdyanir
    ChangeSong
    StartLightSeq
    DMDFlush
    ShowTableInfo
idatlLtimer.Enabled = 1
idatlLtimer001.Enabled = 1
AttractTimer.Enabled = 1
HaloTimer.enabled = 1
End Sub

Sub AttractTimer_Timer
    Dim tmp
    tmp = INT(RND * 5)
    Select Case tmp
        Case 0:PlaySound "attract_1a"
        Case 1:PlaySound "attract_2"
        Case 2:PlaySound "attract_3"
        Case 3:PlaySound "attract_4"
        Case 4:PlaySound "attract_5"
    End Select
End Sub

Sub StopAttractMode
	idatlLtimer.Enabled = 0
	idatlLtimer001.Enabled = 0
	stopattractchief1
	stopattractchief2
	AttractTimer.Enabled = 0
    LightSeqAttract.StopPlay
    DMDScoreNow
	salutetimer.enabled = 1
	salutetimer001.enabled = 1
	flashplaat1.ImageA = "flsshootball"
	HaloTimer.enabled = 0
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
Dim HoleBonus, BumperBonus, RampBonus2, TargetBonus    

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    ChangeSong
	TargetBonus = 0
	bumperHits = 100
	BumperBonus = 0
	RampBonus2 = 0
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
li066.state = 1
li065.state = 1
li064.state = 1
li063.state = 1
li062.state = 1
li061.state = 1
if halovara = 1 then 
li065.state = 2
playsong "missionmusic"
missionmodes = 1
changelights1
end if
if halovarb = 1 then 
li062.state = 2
playsong "missionmusic"
missionmodes = 1
changelights1
end if

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
    If li001.State=1 then 
	AddScore 210
	bonusyscorechecker = bonusyscorechecker + 210
	end if
	If li002.State=1 then 
	AddScore 420
	bonusyscorechecker = bonusyscorechecker + 410
	end if
	PlaySound SoundFX("sling_left",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	'gi1.State = 0:Gi2.State = 0
	AddScore 210
	bonusyscorechecker = bonusyscorechecker + 210
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 3:RSLing1.Visible = 0:RSLing2.Visible = 1:sling1.rotx = 10
        Case 4:RSLing2.Visible = 0:RSLing.Visible = 1:sling1.rotx = 0:RightSlingShot.TimerEnabled = 0':gi1.State = 1:Gi2.State = 1
    End Select
    RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	If li001.State=1 then 
	AddScore 210
	bonusyscorechecker = bonusyscorechecker + 210
	end if
	If li002.State=1 then 
	AddScore 420
	bonusyscorechecker = bonusyscorechecker + 410
	end if
    PlaySound SoundFX("sling_right",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	'gi3.State = 0:Gi4.State = 0
	AddScore 210
	bonusyscorechecker = bonusyscorechecker + 210
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

'********************************triggers boven************************************
sub Trigger001_hit
playsound "trigger_up2"
'ObjLevel(9) = 1 : FlasherFlash9_Timer
'ObjLevel(1) = 1 : FlasherFlash1_Timer
 If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
	li067.state = 2
	li068.state = 2
	DMD "", "", "1k", eNone, eNone, eNone, 500, True, ""
    vpmtimer.addtimer 1500, "disablelightsT1 ' "
end sub

sub Trigger002_hit
playsound "trigger_up2"
'ObjLevel(9) = 1 : FlasherFlash9_Timer
'ObjLevel(1) = 1 : FlasherFlash1_Timer
 If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
	li069.state = 2
	li070.state = 2
	DMD "", "", "1k", eNone, eNone, eNone, 500, True, ""
    vpmtimer.addtimer 1500, "disablelightsT2 ' "
end sub

sub disablelightsT2
li069.state = 0
li070.state = 0
end sub

sub disablelightsT1
li067.state = 0
li068.state = 0
end sub

'**********************inner/outerlane*********************

Sub TLeftInlane_Hit
ObjLevel(6) = 1 : FlasherFlash6_Timer
    PlaySound "trigger_up3"
    If li001.State = 1 then
        AddScore 5000
	bonusyscorechecker = bonusyscorechecker + 5000
    end if
    If li002.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If LeftInlane.state = 0 then
        AddScore 5000
	bonusyscorechecker = bonusyscorechecker + 5000
        LeftInlane.state = 1
    Elseif LeftInlane.state = 0 then
        AddScore 5000
	bonusyscorechecker = bonusyscorechecker + 5000
    end if
	DMD "", "", "5k", eNone, eNone, eNone, 500, True, ""
    CheckbHalo
End Sub

Sub TLeftOutlane_Hit
ObjLevel(6) = 1 : FlasherFlash6_Timer
    PlaySound "fx_sensor"
    If li001.State = 1 then
        AddScore 5000
	bonusyscorechecker = bonusyscorechecker + 5000
    end if
    If li002.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If LeftOutlane.state = 0 then
        AddScore 5000
	bonusyscorechecker = bonusyscorechecker + 5000
        LeftOutlane.state = 1
    Elseif LeftOutlane.state = 0 then
        AddScore 5000
	bonusyscorechecker = bonusyscorechecker + 5000
    end if
	DMD "", "", "5k", eNone, eNone, eNone, 500, True, ""
    CheckbHalo
End Sub

Sub TRightInlane_Hit
ObjLevel(7) = 1 : FlasherFlash7_Timer
    PlaySound "trigger_up3"
    If li001.State = 1 then
        AddScore 5000
	bonusyscorechecker = bonusyscorechecker + 5000
    end if
    If li002.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If RightInlane.state = 0 then
        AddScore 5000
	bonusyscorechecker = bonusyscorechecker + 5000
        RightInlane.state = 1
    Elseif RightInlane.state = 0 then
        AddScore 5000
	bonusyscorechecker = bonusyscorechecker + 5000
    end if
	DMD "", "", "5k", eNone, eNone, eNone, 500, True, ""
    CheckbHalo
End Sub

Sub TRightOutlane_Hit
ObjLevel(7) = 1 : FlasherFlash7_Timer
    PlayQuoteDie
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
    If RightOutlane.state = 0 then
        PlaySound "trigger_right"
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
        RightOutlane.state = 1
    Elseif RightOutlane.state = 0 then
        PlaySound "trigger_right"
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    'FlashForMs Flasher1, 1000, 50, 0
	DMD "", "", "10k", eNone, eNone, eNone, 500, True, ""
    CheckbHalo
End Sub

Sub CheckbHalo
    If(LeftInlane.State = 1) And(LeftOutlane.State = 1) And(RightInlane.State = 1) And(RightOutlane.State = 1) Then
	DMD "", "", "dmdhalo", eNone, eNone, eNone, 1000, True, "halobon"
	AddScore 50000
	bonusyscorechecker = bonusyscorechecker + 50000
	LeftInlane.State=0
	LeftOutlane.State=0
	RightInlane.State=0
	RightOutlane.State=0      
    End If
End Sub

'**********************ramp left*********************

sub Trigger006_hit
 If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	bonusyscorechecker = bonusyscorechecker + 1000
        AddScore 1000
If li034.state = 1 then exit sub
rampl2 = rampl2 + 1
UpdateRampL2Lights
end sub

Sub UpdateRampL2Lights
    Select Case rampl2
Case 0 li035.state = 0:li036.state = 0:li037.state = 0:li034.state = 0
Case 1 li035.state = 1:li036.state = 0:li037.state = 0:li034.state = 0:PlaySound "30procent":showcor
Case 2 li035.state = 1:li036.state = 1:li037.state = 0:li034.state = 0:PlaySound "50procent":showcor
Case 3 li035.state = 1:li036.state = 1:li037.state = 1:li034.state = 2:PlaySound "70procent":showcor
Case 4 li035.state = 1:li036.state = 1:li037.state = 1:li034.state = 1::Jackpot1:CheckSuperJackpot
    End Select
end sub

sub Jackpot1
	DMD "", "", "dmdj", eNone, eNone, eNone, 100, True, "jackpot"
	DMD "", "", "50k", eNone, eNone, eNone, 500, True, ""
	AddScore 50000
	bonusyscorechecker = bonusyscorechecker + 50000
end sub

'**********************ramp left jackpot*********************

sub Trigger003_hit
 If Tilted Then Exit Sub
 if halovara1 = 1 then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	bonusyscorechecker = bonusyscorechecker + 1000
        AddScore 1000
If li033.state = 1 then exit sub
rampl1 = rampl1 + 1
UpdateRampLLights
end sub

Sub UpdateRampLLights
    Select Case rampl1
Case 0 li038.state = 0:li039.state = 0:li040.state = 0:li033.state = 0
Case 1 li038.state = 1:li039.state = 0:li040.state = 0:li033.state = 0:PlaySound "30procent":showcor
Case 2 li038.state = 1:li039.state = 1:li040.state = 0:li033.state = 0:PlaySound "50procent":showcor
Case 3 li038.state = 1:li039.state = 1:li040.state = 1:li033.state = 2:PlaySound "70procent":showcor
Case 4 li038.state = 1:li039.state = 1:li040.state = 1:li033.state = 1:Jackpot2:CheckSuperJackpot
    End Select
end sub

sub Jackpot2
	DMD "", "", "dmdj", eNone, eNone, eNone, 100, True, "jackpot"
	DMD "", "", "50k", eNone, eNone, eNone, 500, True, ""
	AddScore 50000
	bonusyscorechecker = bonusyscorechecker + 50000
end sub


'**********************ramp right jackpot*********************

sub Trigger004_hit
 If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	bonusyscorechecker = bonusyscorechecker + 1000
        AddScore 1000
If li029.state = 1 then exit sub
rampr1 = rampr1 + 1
UpdateRampRLights
end sub

Sub UpdateRampRLights
    Select Case rampr1
Case 0 li047.state = 0:li048.state = 0:li049.state = 0:li029.state = 0
Case 1 li047.state = 1:li048.state = 0:li049.state = 0:li029.state = 0:PlaySound "30procent":showcor
Case 2 li047.state = 1:li048.state = 1:li049.state = 0:li029.state = 0:PlaySound "50procent":showcor
Case 3 li047.state = 1:li048.state = 1:li049.state = 1:li029.state = 2:PlaySound "70procent":showcor
Case 4 li047.state = 1:li048.state = 1:li049.state = 1:li029.state = 1:Jackpot3:CheckSuperJackpot
    End Select
end sub

sub Jackpot3
	DMD "", "", "dmdj", eNone, eNone, eNone, 100, True, "jackpot"
	DMD "", "", "50k", eNone, eNone, eNone, 500, True, ""
	AddScore 50000
	bonusyscorechecker = bonusyscorechecker + 50000
end sub

'**********************ramp right*********************

sub Trigger007_hit
 If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	bonusyscorechecker = bonusyscorechecker + 1000
        AddScore 1000
If li030.state = 1 then exit sub
rampr2 = rampr2 + 1
UpdateRampR2Lights
end sub

Sub UpdateRampR2Lights
    Select Case rampr2
Case 0 li050.state = 0:li051.state = 0:li052.state = 0:li030.state = 0
Case 1 li050.state = 1:li051.state = 0:li052.state = 0:li030.state = 0:PlaySound "30procent":showcor
Case 2 li050.state = 1:li051.state = 1:li052.state = 0:li030.state = 0:PlaySound "50procent":showcor
Case 3 li050.state = 1:li051.state = 1:li052.state = 1:li030.state = 2:PlaySound "70procent":showcor
Case 4 li050.state = 1:li051.state = 1:li052.state = 1:li030.state = 1:Jackpot4:CheckSuperJackpot
    End Select
end sub

sub Jackpot4
	DMD "", "", "dmdj", eNone, eNone, eNone, 500, True, "jackpot"
	DMD "", "", "50k", eNone, eNone, eNone, 500, True, ""
	AddScore 50000
	bonusyscorechecker = bonusyscorechecker + 50000
end sub

'**********************ramp right*********************

sub Trigger008_hit
 If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	bonusyscorechecker = bonusyscorechecker + 1000
        AddScore 1000
If li032.state = 1 then exit sub
rampm1 = rampm1 + 1
UpdateRampM1Lights
end sub

Sub UpdateRampM1Lights
    Select Case rampm1
Case 0 li041.state = 0:li042.state = 0:li043.state = 0:li032.state = 0
Case 1 li041.state = 1:li042.state = 0:li043.state = 0:li032.state = 0:PlaySound "30procent":showcor
Case 2 li041.state = 1:li042.state = 1:li043.state = 0:li032.state = 0:PlaySound "50procent":showcor
Case 3 li041.state = 1:li042.state = 1:li043.state = 1:li032.state = 2:PlaySound "70procent":showcor
Case 4 li041.state = 1:li042.state = 1:li043.state = 1:li032.state = 1:Jackpot5:CheckSuperJackpot
    End Select
end sub

sub showcor
cortana.z = 70
vpmtimer.addtimer 2000, "resetcortana ' "
end sub

sub resetcortana
cortana.z = -100
end sub

sub Jackpot5
	DMD "", "", "dmdj", eNone, eNone, eNone, 500, True, "jackpot"
	DMD "", "", "50k", eNone, eNone, eNone, 500, True, ""
	AddScore 50000
	bonusyscorechecker = bonusyscorechecker + 50000
end sub

sub CheckSuperJackpot
If (li034.state = 1)and (li033.state = 1)and (li029.state = 1)and (li030.state = 1)and (li032.state = 1)and (li031.state = 1) then
	DMD "", "", "dmdsj", eNone, eNone, eNone, 100, True, "unbelief"
'	DMD "", "", "500k", eNone, eNone, eNone, 500, True, "
rampl1 = 0
rampr1 = 0
rampl2 = 0
rampr2 = 0
rampm1 = 0
rampm2 = 0
UpdateRampM2Lights
UpdateRampM1Lights
UpdateRampR2Lights
UpdateRampL2Lights
UpdateRampRLights
UpdateRampLLights
addscore 500000
bonusyscorechecker = bonusyscorechecker + 500000
li085.state = 1
end if
end sub

'**********************

Sub Bonuschecker_Hit
'FlashForMs Flasher4, 1000, 50, 0
'FlashForMs Flasher5, 1000, 50, 0
'FlashForMs Flasher6, 1000, 50, 0
'FlashForMs Flasher7, 1000, 50, 0
'FlashForMs Flasher8, 1000, 50, 0
'FlashForMs Flasher9, 1000, 50, 0
'If li003.state=1 then
'DoraBonus = 1
'Else
'DoraBonus = 0
'end if
'If li004.state=1 then
'EmonBonus = 1
'Else
'EmonBonus = 0
'end if
'If li033.state=1 then
'DoraEmonBonus = 1
'Else
'DoraEmonBonus = 0
'end if
'If li015.state=1 then
'CatBonus = 1
'Else
'CatBonus = 0
'end if
'If li016.state=1 then
'HomeBonus = 1
'Else
'HomeBonus = 0
'end if
'If li034.state=1 then
'AnywhereBonus = 1
'Else
'AnywhereBonus = 0
'end if
'If li005.state=1 then
'RampBonus1 = 1
'Else
'RampBonus1 = 0
'end if
'If li006.state=1 then
'RampBonus2 = 1
'Else
'RampBonus2 = 0
'end if
'If li007.state=1 then
'RampBonus3 = 1
'Else
'RampBonus3 = 0
'end if
'If li017.state=1 then
'ALLRampBonus = 1
'Else
'ALLRampBonus = 0
'end if
'If li018.state=1 then
'MulitballBonus = 1
'Else
'MulitballBonus = 0
'end if
End Sub
'************************** 
'Spinners
'************************** 
Sub Spinner001_Spin()'Inside this Sub is what the spinner1 will do
	FlashForMs Flasher012, 1000, 50, 0
	FlashForMs Flasher011, 1000, 50, 0
'FlashForMs Flasher010, 1000, 50, 0
'FlashForMs Flasher009, 1000, 50, 0
	If Tilted Then Exit Sub
	If li001.State=1 then 
	AddScore 1000
		bonusyscorechecker = bonusyscorechecker + 1000
	end if
	If li002.State=1 then 
	AddScore 2000
		bonusyscorechecker = bonusyscorechecker + 2000
	end if
	PlaySound "banshee3c"
	If Tilted Then Exit Sub
	Addscore 1000
		bonusyscorechecker = bonusyscorechecker + 1000
End Sub

Sub Spinner002_Spin()'Inside this Sub is what the spinner1 will do
	FlashForMs Flasher012, 1000, 50, 0
	FlashForMs Flasher011, 1000, 50, 0
'FlashForMs Flasher010, 1000, 50, 0
'FlashForMs Flasher009, 1000, 50, 0
	If Tilted Then Exit Sub
	If li001.State=1 then 
	AddScore 1000
		bonusyscorechecker = bonusyscorechecker + 1000
	end if
	If li002.State=1 then 
	AddScore 2000
		bonusyscorechecker = bonusyscorechecker + 2000
	end if
	PlaySound "banshee2c"
	Addscore 1000
		bonusyscorechecker = bonusyscorechecker + 1000
End Sub

Sub Spinner003_Spin()'Inside this Sub is what the spinner1 will do
	FlashForMs Flasher014, 1000, 50, 0
	FlashForMs Flasher013, 1000, 50, 0
'FlashForMs Flasher010, 1000, 50, 0
'FlashForMs Flasher009, 1000, 50, 0
	If Tilted Then Exit Sub
	If li001.State=1 then 
	AddScore 1000
		bonusyscorechecker = bonusyscorechecker + 1000
	end if
	If li002.State=1 then 
	AddScore 2000
		bonusyscorechecker = bonusyscorechecker + 2000
	end if
	PlaySound "banshee1c"
	Addscore 1000
		bonusyscorechecker = bonusyscorechecker + 1000
End Sub

'************************** 
'Bumpers 
'************************** 
Dim bumperHits

Sub Bumper003_hit()
ObjLevel(7) = 1 : FlasherFlash7_Timer
ObjLevel(6) = 1 : FlasherFlash6_Timer
ObjLevel(3) = 0.5 : FlasherFlash3_Timer
ObjLevel(9) = 1 : FlasherFlash9_Timer
ObjLevel(1) = 1 : FlasherFlash1_Timer
If li001.State=1 then 
	AddScore 500
	bonusyscorechecker = bonusyscorechecker + 500
	end if
	If li002.State=1 then 
	AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
	end if
PlaySound "bumb1"
addscore 500
	bonusyscorechecker = bonusyscorechecker + 500
'FlashForMs Flasher1, 1000, 50, 0
'Starlvl = Starlvl + 1
	bumperHits = bumperHits - 1
    CheckBumpers
'	UpdateStarLights
end sub

Sub Bumper002_hit()
ObjLevel(7) = 1 : FlasherFlash7_Timer
ObjLevel(6) = 1 : FlasherFlash6_Timer
ObjLevel(4) = 0.5 : FlasherFlash4_Timer
ObjLevel(9) = 1 : FlasherFlash9_Timer
ObjLevel(1) = 1 : FlasherFlash1_Timer
If li001.State=1 then 
	AddScore 500
	bonusyscorechecker = bonusyscorechecker + 500
	end if
	If li002.State=1 then 
	AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
	end if
PlaySound "bumb2"
addscore 500
	bonusyscorechecker = bonusyscorechecker + 500
'FlashForMs Flasher2, 1000, 50, 0
'Starlvl = Starlvl + 1
	bumperHits = bumperHits - 1
    CheckBumpers
'	UpdateStarLights
end sub

Sub Bumper004_hit()
ObjLevel(7) = 1 : FlasherFlash7_Timer
ObjLevel(6) = 1 : FlasherFlash6_Timer
ObjLevel(5) = 0.5 : FlasherFlash5_Timer
ObjLevel(9) = 1 : FlasherFlash9_Timer
ObjLevel(1) = 1 : FlasherFlash1_Timer
If li001.State=1 then 
	AddScore 500
	bonusyscorechecker = bonusyscorechecker + 500
	end if
	If li002.State=1 then 
	AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
	end if
PlaySound "bumb3"
addscore 500
	bonusyscorechecker = bonusyscorechecker + 500
'FlashForMs Flasher3, 1000, 50, 0
'Starlvl = Starlvl + 1
	bumperHits = bumperHits - 1
    CheckBumpers
'	UpdateStarLights
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
End Sub

Sub ResetBumpers()
    bumperHits = 100
End Sub

'*****************
'Targets
'*****************

'*************************target 2x/3x*************************

Sub Target1_Hit()

    If Tilted Then Exit Sub
    If li001.State = 1 and li059.State = 0 then
        li059.State = 1
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 1000
        PlaySound "target_teleport"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li001.State = 1 and li059.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
        PlaySound "target_teleport"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li002.State = 1 then
        AddScore 3000
	bonusyscorechecker = bonusyscorechecker + 3000
        PlaySound "target_teleport"
        TargetBonus = TargetBonus + 1
        Exit Sub
    end if

    If li059.State = 1 Then
        PlaySound "target_teleport"
    else
        PlaySound "target_teleport"
        li059.State = 1
    end if
    AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
BigUfoShake
    'PlaySound "TARGET1"
    Checkb2x
    Checkb3x
End Sub

Sub Target2_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 and li060.State = 0 then
        li060.State = 1
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
        PlaySound "target_teleport"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li001.State = 1 and li060.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
        PlaySound "target_teleport"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li002.State = 1 then
        AddScore 3000
	bonusyscorechecker = bonusyscorechecker + 3000
        PlaySound "target_teleport"
        TargetBonus = TargetBonus + 1
        Exit Sub
    end if

    If li060.State = 1 Then
        PlaySound "target_teleport"
    else
        PlaySound "target_teleport"
        li060.State = 1
    end if
    AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
    'PlaySound "TARGET1"
    Checkb2x
    Checkb3x
End Sub

Sub Checkb2x
    If(li059.State = 1)Or(li060.State = 1)Then
        DMD "", "", "dmd2x", eNone, eNone, eNone, 1000, True, "2x3x"
		LightSeq002.Play SeqBlinking, , 1, 300
        li001.State = 1
		stopidlchiefke
		chbsLtimer.enabled = 1
		chbsRtimer.enabled = 1
		'LightSeqAttract.StopPlay
        vpmtimer.addtimer 60000, "resetlightandvar1 ' "
    End If
End Sub

Sub Checkb3x
    If(li059.State = 1)And(li060.State = 1)Then
        DMD "", "", "dmd3x", eNone, eNone, eNone, 1000, True, "2x3x"
		LightSeq002.Play SeqBlinking, , 1, 300
        li002.State = 1
        li001.State = 0
		stopidlchiefke
		chbsLtimer.enabled = 1
		chbsRtimer.enabled = 1
		'LightSeqAttract.StopPlay
        vpmtimer.addtimer 60000, "resetlightandvar2 ' "
    End If
End Sub

sub resetlightandvar1
if li002. state = 1 then exit sub
li001.state = 0
li059.State = 0
li060.State = 0
end sub

sub resetlightandvar2
li002.state = 0
li059.State = 0
li060.State = 0
end sub



'*********************targets covenant******************************************************
sub Target005_hit()
	PlaySound "target_hit"
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	li003.state = 1
    Addscore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
	checkConveant
End sub

sub Target006_hit()
	PlaySound "target_hit"
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	li004.state = 1
    Addscore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
	checkConveant
End sub

sub Target007_hit()
	PlaySound "target_hit"
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	li005.state = 1
    Addscore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
	checkConveant
End sub

sub Target015_hit()
	PlaySound "target_hit"
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	li006.state = 1
    Addscore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
	checkConveant
End sub

sub Target008_hit()
	PlaySound "target_hit"
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	li007.state = 1
    Addscore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
	checkConveant
End sub

sub Target009_hit()
	PlaySound "target_hit"
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	li008.state = 1
    Addscore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
	checkConveant
End sub

sub Target010_hit()
	PlaySound "target_hit"
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	li012.state = 1
    Addscore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
	checkConveant
End sub

sub Target011_hit()
	PlaySound "target_hit"
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	li013.state = 1
    Addscore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
	checkConveant
End sub


Sub checkConveant
if(li003.state = 1)And(li004.state = 1)And(li005.state = 1)And(li006.state = 1)And(li007.state = 1)And(li008.state = 1)And(li012.state = 1)And(li013.state = 1) Then
        DMD "", "", "dmdcovenant", eNone, eNone, eNone, 1000, True, "covenantbon"
		AddScore 500000
		bonusyscorechecker = bonusyscorechecker + 500000
        li012.state = 0
        li013.state = 0
        li003.state = 0
        li004.state = 0
        li005.state = 0
		li006.state = 0
		li007.state = 0
		li008.state = 0
end if	
end sub

'laagste
sub Target018_hit()
playsound "hitlow"
if mskillow = 1 Then
endskillow
end if
End sub

'hoogte
sub Target016_hit()
playsound "hithigh"
if mskilhigh = 1 Then
endskilhigh
end if
End sub

'*********************droptargets doors******************************************************

Sub Target013_Hit()
	PlaySound "dooropen2"
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
    Addscore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
	vpmtimer.addtimer 1000, "resettarget13 '"
End Sub

sub resettarget13
	PlaySound "doorcloseee2"
	Target013.IsDropped = False
end sub

Sub Target014_Hit()
	PlaySound "dooropen2"
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
    Addscore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
	vpmtimer.addtimer 1000, "resettarget14 '"
End Sub

sub resettarget14
	PlaySound "doorcloseee2"
	Target014.IsDropped = False
end sub

'*********************droptarget bunkerdoor******************************************************
Sub Target012_Hit()
	PlaySound "dooropen2"
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	Addscore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
End Sub

sub resettarget12
	PlaySound "doorcloseee2"
	Target012.IsDropped = False
end sub

'*********************targets bunker*********************
Sub Target003_Hit()
	BigUfoShake
	PlaySound "target_bunker"	
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	Addscore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
    If bMultiBallMode = true or (waveattack=1)or (waveattack=4)or (waveattack=8)or (waveattack=13)or (waveattack=19)or (waveattack=26) then exit sub
	PlayQuoteJ
end sub

Sub Target004_Hit()
	BigUfoShake
	PlaySound "target_bunker"
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    end if
    If li002.State = 1 then
        AddScore 2000
	bonusyscorechecker = bonusyscorechecker + 2000
    end if
	Addscore 1000
	bonusyscorechecker = bonusyscorechecker + 1000
    TargetBonus = TargetBonus + 1
end sub


'*****************
'Gates
'*****************

sub Gate_hit()
playsound "door"
end sub

sub Gate001_hit()
playsound "door"
end sub

sub Gate002_hit()
LightSeq001.StopPlay
playsound "door"
flashplaat1.ImageA = "flshalopin"
if halovara = 1 then 
flashplaat1.ImageA = "m8"
end if
if halovarb = 1 then 
flashplaat1.ImageA = "m10"
end if
end sub

sub Gate003_hit()
playsound "door"
FlashForMs Flasher019, 1000, 50, 0
end sub

sub Gate004_hit()
playsound "door"
FlashForMs Flasher020, 1000, 50, 0
end sub


'*****************
'Kickers
'*****************

'***************** Cortana *****************
sub Kicker001_hit()
cortanadmd
showcor
PlayQuoteC
vpmTimer.AddTimer 2050, "kickcortana'"
end sub

sub kickcortana
playsound "fx_popper"
Kicker001.Kick 0,28
end sub

sub cortanadmd
DMD "", "", "cort00", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort01", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort02", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort03", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort04", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort05", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort06", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort07", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort08", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort09", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort10", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort11", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort12", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort13", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort14", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort15", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort16", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort17", eNone, eNone, eNone, 100, True, ""
DMD "", "", "cort18", eNone, eNone, eNone, 100, True, ""
end sub

'***************** teleport *****************
Dim BallInHole1
dim BallInHole3
Dim dBall
Dim fBall

sub Kicker004_hit()
	'FlashForMs Flasher5, 1000, 50, 0
	'LightSeq001.Play SeqUpOn, 25, 1000
	'LightSeq001.Play SeqCircleInOn,50
	BallInHole1 = BallInHole1 + 1
    Set dBall = ActiveBall:Me.TimerEnabled = 1
	Playsound "portalsound2"
    Me.Enabled = 0
	If bMultiBallMode = true Then
	SuperVukAddBall1
	exit Sub
	end if
    vpmTimer.AddTimer 500, "StartSlotmachine'"
end sub

Sub SuperVukAddBall1()
	If BallInHole1> 0 Then
        BallInHole1 = BallInHole1 - 1
	Kicker005.CreateSizedball BallSize / 2
	'ChangeBallImage
	kickteleport
 vpmtimer.addtimer 1000, "SuperVukAddBall1 '" 
end If
End Sub

Sub SuperVukAddBall10()
	If BallInHole3> 0 Then
        BallInHole3 = BallInHole1 - 3
	Kicker005.CreateSizedball BallSize / 2
	'ChangeBallImage
	kickteleport
 vpmtimer.addtimer 1000, "SuperVukAddBall10 '" 
end If
End Sub

Sub Kicker004_Timer
    Do While dBall.Z > 0
        dBall.Z = dBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
	Me.Enabled = 1
End Sub

Sub SuperVukAddBall4()
	If BallInHole1> 0 Then
        BallInHole1 = BallInHole1 - 1
	Kicker006.CreateSizedball BallSize / 2
	'ChangeBallImage
	kickteleport2
	vpmtimer.addtimer 1100, "resettarget13 '"
 vpmtimer.addtimer 1000, "SuperVukAddBall4 '" 
end If
End Sub

Sub SuperVukAddBall40()
	If BallInHole3> 0 Then
        BallInHole3 = BallInHole3 - 1
	Kicker006.CreateSizedball BallSize / 2
	'ChangeBallImage
	kickteleport2
	vpmtimer.addtimer 1100, "resettarget13 '"
 vpmtimer.addtimer 1000, "SuperVukAddBall40 '" 
end If
End Sub

sub kickteleport2
Playsound "fx_popperteleport"
Kicker006.Kick 280, 7, 14
end sub

sub Kicker006_hit()
PlaySound "dooropen2"
Target013.IsDropped = True
vpmtimer.addtimer 500, "kickteleport2 '"
vpmtimer.addtimer 1600, "resettarget13 '"
end sub

sub openydoor
FlashForMs Flasher010, 1000, 50, 0
PlaySound "dooropen2"
Target013.IsDropped = True
vpmTimer.AddTimer 1000, "SuperVukAddBall4'"
end Sub

sub openydoor3
FlashForMs Flasher010, 1000, 50, 0
PlaySound "dooropen2"
Target013.IsDropped = True
vpmTimer.AddTimer 1000, "SuperVukAddBall40'"
end Sub

Sub SuperVukAddBall5()
	If BallInHole1> 0 Then
        BallInHole1 = BallInHole1 - 1
	'openydoor2
	Kicker007.CreateSizedball BallSize / 2
	'ChangeBallImage
	kickteleport3
	vpmtimer.addtimer 1100, "resettarget14 '"
 vpmtimer.addtimer 1000, "SuperVukAddBall5 '" 
end If
End Sub

Sub SuperVukAddBall50()
	If BallInHole3> 0 Then
        BallInHole3 = BallInHole3 - 1
	'openydoor2
	Kicker007.CreateSizedball BallSize / 2
	'ChangeBallImage
	kickteleport3
	vpmtimer.addtimer 1100, "resettarget14 '"
 vpmtimer.addtimer 1000, "SuperVukAddBall50 '" 
end If
End Sub

sub kickteleport3
Playsound "fx_popperteleport"
Kicker007.Kick 80, 7, 14
end sub

sub Kicker007_hit()
PlaySound "dooropen2"
Target013.IsDropped = True
vpmtimer.addtimer 500, "kickteleport3 '"
vpmtimer.addtimer 1600, "resettarget14 '"
end sub

sub openydoor2
FlashForMs Flasher009, 1000, 50, 0
PlaySound "dooropen2"
Target014.IsDropped = True
vpmTimer.AddTimer 1000, "SuperVukAddBall5'"
end Sub

sub openydoor4
FlashForMs Flasher009, 1000, 50, 0
PlaySound "dooropen2"
Target014.IsDropped = True
vpmTimer.AddTimer 1000, "SuperVukAddBall50'"
end Sub

sub Kicker005_hit()
	Playsound "portalsound2"
	rampm2 = rampm2 + 1
	UpdateRampM2Lights	
	if missionmodes = 1 or bMultiBallMode = true then 
	vpmTimer.AddTimer 1000, "kickteleport'"
	exit Sub
	end if
		BallInHole3 = BallInHole3 + 1
    Set fBall = ActiveBall:Me.TimerEnabled = 1
    Me.Enabled = 0
	StartChoisemachine
end sub

Sub UpdateRampM2Lights
    Select Case rampm2  
Case 0 li044.state = 0:li045.state = 0:li046.state = 0:li031.state = 0
Case 1 li044.state = 1:li045.state = 0:li046.state = 0:li031.state = 0:'PlaySound "30procent":showcor
Case 2 li044.state = 1:li045.state = 1:li046.state = 0:li031.state = 0:'PlaySound "50procent":showcor
Case 3 li044.state = 1:li045.state = 1:li046.state = 1:li031.state = 2:'PlaySound "70procent":showcor
Case 4 li044.state = 1:li045.state = 1:li046.state = 1:li031.state = 1:Jackpot6:CheckSuperJackpot
    End Select
end sub

sub Jackpot6
	DMD "", "", "dmdj", eNone, eNone, eNone, 100, True, "jackpot"
	DMD "", "", "50k", eNone, eNone, eNone, 500, True, ""
	AddScore 50000
	bonusyscorechecker = bonusyscorechecker + 50000
end sub

Sub SuperVukAddBall3()
	If BallInHole3> 0 Then
        BallInHole3 = BallInHole3 - 1
	Kicker005.CreateSizedball BallSize / 2
	kickteleport
 vpmtimer.addtimer 1000, "SuperVukAddBall3 '" 
end If
End Sub

sub kickteleport
Playsound "fx_popperteleport"
Kicker005.Kick 190, 7, 1
end sub

Sub Kicker005_Timer
    Do While fBall.Z> 0
        fBall.Z = fBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
	Me.Enabled = 1
End Sub

sub dmdkeys
DMD "", "", "keysmis00", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis01", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis02", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis03", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis04", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis05", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis06", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis07", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis08", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis09", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis10", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis11", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis12", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis13", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis14", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis15", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis16", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis17", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis18", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis19", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis20", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis21", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis22", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis23", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis24", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis25", eNone, eNone, eNone, 100, True, ""
DMD "", "", "keysmis26", eNone, eNone, eNone, 100, True, ""
end sub

'**************
' choiseMachine
'**************
Dim SlotAward7, SlotValue7

SlotAward7 = Array("chs1", "chs2", "chs3")
SlotValue7 = 0

Sub StartChoisemachine()
    Dim i
    DMDFlush
    For i = 0 to 2
        DMD "", "", SlotAward7(i), eNone, eNone, eNone, 100, False, ""
    Next
    vpmtimer.AddTimer 1500, "GiveSlotAward7 '"
End Sub

Sub GiveSlotAward7()
    DMDFlush
    SlotValue7 = INT(RND * 3)
    DMD "", "", SlotAward7(SlotValue7), eNone, eNone, eNone, 50, True, ""

    Select Case SlotValue7
        Case 0:StartSlotmachine8
        Case 1:StartSlotmachine4
        Case 2:bonusSlotmachine
    End Select
End Sub

'**************
' SlotMachine
'**************

Dim SlotAward1, SlotValue1

SlotAward1 = Array("dmdteleport1", "dmdteleport2", "dmdteleport3")
SlotValue1 = 0

Sub StartSlotmachine()
    Dim i
    DMDFlush
    For i = 0 to 2
        DMD "", "", SlotAward1(i), eNone, eNone, eNone, 50, False, ""
    Next
    vpmtimer.AddTimer 500, "GiveSlotAward1 '"
End Sub

Sub GiveSlotAward1()
	'LightSeq001.StopPlay
    DMDFlush
    SlotValue1 = INT(RND * 3)
    DMD "", "", SlotAward1(SlotValue1), eNone, eNone, eNone, 500, True, ""

    Select Case SlotValue1
        Case 0:openydoor2
        Case 1:SuperVukAddBall1
        Case 2:openydoor
    End Select
End Sub


'**************
' SlotMachine2
'**************

Dim SlotAward8, SlotValue8

SlotAward8 = Array("dmdteleport1", "dmdteleport2", "dmdteleport3")
SlotValue8 = 0

Sub StartSlotmachine8()
    Dim i
    DMDFlush
    For i = 0 to 2
        DMD "", "", SlotAward8(i), eNone, eNone, eNone, 50, False, ""
    Next
    vpmtimer.AddTimer 500, "GiveSlotAward8 '"
End Sub

Sub GiveSlotAward8()
    DMDFlush
    SlotValue1 = INT(RND * 3)
    DMD "", "", SlotAward8(SlotValue8), eNone, eNone, eNone, 500, True, ""

    Select Case SlotValue8
        Case 0:openydoor4
        Case 1:SuperVukAddBall10
        Case 2:openydoor3
    End Select
End Sub


'**************
' missions
'**************

Sub StartSlotmachine4() 
	SelectMissionTimer.Enabled = true
    vpmtimer.AddTimer 1200, "SlotAward2 '"
End Sub

Dim Holostep:Holostep = INT(RND(1)*80)
Sub SelectMissionTimer_Timer()
Playsound "fx_spinner"
Holostep = (Holostep + 1) MOD 14
Select Case Holostep
case 0: 	if mission1 = 0 then 
			flashplaat1.ImageA = "m1"
			Pslot = 0
			Holostep = 0
			end if
case 1: 	if mission2= 0 then 
			flashplaat1.ImageA = "m2" 
			Pslot = 1
			Holostep = 1
			end if
case 2: 	if mission3= 0 then 
			flashplaat1.ImageA = "m3" 
			Pslot = 2
			Holostep = 2
			end if
case 3: 	if mission4= 0 then 
			flashplaat1.ImageA = "m4"
			Pslot = 3 
			Holostep = 3
			end if
case 4: 	if mission5= 0 then 
			flashplaat1.ImageA = "m5" 
			Pslot = 4
			Holostep = 4
			end if
case 5: 	if mission6= 0 then 
			flashplaat1.ImageA = "m6" 
			Pslot = 5
			Holostep = 5
			end if
case 6: 	if mission7= 0 then 
			flashplaat1.ImageA = "m7"
			Pslot = 6 
			Holostep = 6
			end if
case 7: 	if mission8= 0 then 
			flashplaat1.ImageA = "m8" 
			Pslot = 7
			Holostep = 7
			end if
case 8: 	if mission9= 0 then 
			flashplaat1.ImageA = "m9" 
			Pslot = 8
			Holostep = 8
			end if
case 9: 	if mission10= 0 then 
			flashplaat1.ImageA = "m10" 
			Pslot = 9
			Holostep = 9
			end if
case 10: 	if mission11= 0 then 
			flashplaat1.ImageA = "m11" 
			Pslot = 10
			Holostep = 10
			end if
case 11: 	if mission12= 0 then 
			flashplaat1.ImageA = "m12" 
			Pslot = 11
			Holostep = 11
			end if
case 12: 	if mission13= 0 then 
			flashplaat1.ImageA = "m13" 
			Pslot = 12
			Holostep = 12
			end if
case 13: 	if mission14= 0 then 
			flashplaat1.ImageA = "m14" 
			Pslot = 13
			Holostep = 13
			end if
case 14: 	if mission15= 0 then 
			flashplaat1.ImageA = "m15" 
			Pslot = 14
			Holostep = 14
			end if
End Select
End Sub

Sub choiseaward
Select Case Pslot
case 0
startmode2
case 1
startmode5
case 2
startmode3
case 3
startmode8
case 4
startmode7
case 5
startmode10
case 6
startmode12
case 7
startmode13
case 8
startmode14
case 9
startmode15
case 10
startmode1
case 11
startmode11
case 12
startmode9
case 13
startmode4
case 14
startmode6
End Select
changelights1
end sub

sub slotaward2
SelectMissionTimer.Enabled = false
dmdkeys
playsound"newmission2"
'AbumperTimer.Enabled = True
'BumperToCaught = 1
'WeepDir = 1
'EnableBallSaver 60
vpmTimer.AddTimer 2700, "choiseaward'"
vpmTimer.AddTimer 2700, "SuperVukAddBall3'"
End Sub

Sub UpdateEnemiesLeft
    Select Case enemylft  
Case 0 dmd5left
Case 1 dmd4left
Case 2 dmd3left
Case 3 dmd2left
Case 4 dmd1left
    End Select
end sub

sub dmd5left
DMD "", "", "dmd5e", eNone, eNone, eNone, 1000, True, ""
end sub

sub dmd4left
DMD "", "", "dmd4e", eNone, eNone, eNone, 1000, True, ""
end sub

sub dmd3left
DMD "", "", "dmd3e", eNone, eNone, eNone, 1000, True, ""
end sub

sub dmd2left
DMD "", "", "dmd2e", eNone, eNone, eNone, 1000, True, ""
end sub

sub dmd1left
DMD "", "", "dmd1e", eNone, eNone, eNone, 1000, True, ""
end sub

sub changelights1
TargetBank.Image = "targetT1Square_Green"
Target003.Image = "a1_target-t1-green_squared"
Target004.Image = "a1_target-t1-green_squared"
Target1.Image = "a1_target-t1-green_squared"
Target2.Image = "a1_target-t1-green_squared"
Target005.Image = "a1_target-t1-green_squared"
Target006.Image = "a1_target-t1-green_squared"
Target007.Image = "a1_target-t1-green_squared"
Target008.Image = "a1_target-t1-green_squared"
Target009.Image = "a1_target-t1-green_squared"
Target010.Image = "a1_target-t1-green_squared"
Target011.Image = "a1_target-t1-green_squared"
Target015.Image = "a1_target-t1-green_squared"
Target016.Image = "a1_target-t1-green_squared"
Target018.Image = "a1_target-t1-green_squared"
Flasher005.Visible = False
Flasher003.Visible = True
Flasher001.Visible = False
Flasher006.Visible = True 
Flasher002.Visible = False
Flasher007.Visible = True
Flasher004.Visible = False
Flasher008.Visible = True
end sub

sub changelights2
TargetBank.Image = "targetT1Square_Red"
Target003.Image = "a1_target-t1-red_squared"
Target004.Image = "a1_target-t1-red_squared"
Target1.Image = "a1_target-t1-red_squared"
Target2.Image = "a1_target-t1-red_squared"
Target005.Image = "a1_target-t1-red_squared"
Target006.Image = "a1_target-t1-red_squared"
Target007.Image = "a1_target-t1-red_squared"
Target008.Image = "a1_target-t1-red_squared"
Target009.Image = "a1_target-t1-red_squared"
Target010.Image = "a1_target-t1-red_squared"
Target011.Image = "a1_target-t1-red_squared"
Target015.Image = "a1_target-t1-red_squared"
Target016.Image = "a1_target-t1-red_squared"
Target018.Image = "a1_target-t1-red_squared"
Flasher003.Visible = False
Flasher005.Visible = True
Flasher006.Visible = False 
Flasher001.Visible = True
Flasher007.Visible = False
Flasher002.Visible = True
Flasher008.Visible = False
Flasher004.Visible = True
Flasher015.Visible = False
Flasher016.Visible = False
Flasher017.Visible = False
Flasher018.Visible = False 
end sub

sub changelights3
TargetBank.Image = "targetT1Square_yellow"
Target003.Image = "a1_target-t1-yellow_squared"
Target004.Image = "a1_target-t1-yellow_squared"
Target1.Image = "a1_target-t1-yellow_squared"
Target2.Image = "a1_target-t1-yellow_squared"
Target005.Image = "a1_target-t1-yellow_squared"
Target006.Image = "a1_target-t1-yellow_squared"
Target007.Image = "a1_target-t1-yellow_squared"
Target008.Image = "a1_target-t1-yellow_squared"
Target009.Image = "a1_target-t1-yellow_squared"
Target010.Image = "a1_target-t1-yellow_squared"
Target011.Image = "a1_target-t1-yellow_squared"
Target015.Image = "a1_target-t1-yellow_squared"
Target016.Image = "a1_target-t1-yellow_squared"
Target018.Image = "a1_target-t1-yellow_squared"
Flasher005.Visible = False
Flasher015.Visible = True
Flasher001.Visible = False
Flasher016.Visible = True 
Flasher002.Visible = False
Flasher017.Visible = True
Flasher004.Visible = False
Flasher018.Visible = True
end sub


'********mode 1***************

sub startmode1
StopSong
missionmodes = 1
PlaySong "missionmusic" 
mode1TimerCount = 60
mode1timer.Enabled = 1
enablefloodcs
end sub

sub enablefloodcs
tim001.Enabled = 1
tim002.Enabled = 1
tim003.Enabled = 1
tim004.Enabled = 1
tim005.Enabled = 1
tim006.Enabled = 1 
tfloodc001.enabled = 1
floodc1timer.enabled = true
UpdateEnemiesLeft 
end sub

Sub tfloodc001_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tfloodc001.enabled = 0
    floodc1timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "floodchurt"
mfloodc = mfloodc + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusfloodc
countr25 = 0
movefloodc1down
tfloodc002.enabled = 1
floodc2timer.enabled = true 
end sub

sub movefloodc1down
floodc001.z = -200
floodc002.z = -200
floodc003.z = -200
floodc004.z = -200
floodc005.z = -200
floodc006.z = -200
floodc007.z = -200
floodc008.z = -200
end sub

Sub tfloodc002_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tfloodc002.enabled = 0
    floodc2timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "floodchurt"
mfloodc = mfloodc + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusfloodc
countr26 = 0
movefloodc2down
tfloodc003.enabled = 1
floodc3timer.enabled = true 
end sub

sub movefloodc2down
floodc009.z = -200
floodc010.z = -200
floodc011.z = -200
floodc012.z = -200
floodc013.z = -200
floodc014.z = -200
floodc015.z = -200
floodc016.z = -200
end sub

Sub tfloodc003_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tfloodc003.enabled = 0
    floodc3timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "floodchurt"
mfloodc = mfloodc + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusfloodc
countr27 = 0
movefloodc3down
tfloodc004.enabled = 1
floodc4timer.enabled = true 
end sub

sub movefloodc3down
floodc017.z = -200
floodc018.z = -200
floodc019.z = -200
floodc020.z = -200
floodc021.z = -200
floodc022.z = -200
floodc023.z = -200
floodc024.z = -200
end sub

Sub tfloodc004_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tfloodc004.enabled = 0
    floodc4timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "floodchurt"
mfloodc = mfloodc + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusfloodc
countr28 = 0
movefloodc4down
tfloodc005.enabled = 1
floodc5timer.enabled = true 
end sub

sub movefloodc4down
floodc025.z = -200
floodc026.z = -200
floodc027.z = -200
floodc028.z = -200
floodc029.z = -200
floodc030.z = -200
floodc031.z = -200
floodc032.z = -200
end sub

Sub tfloodc005_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tfloodc005.enabled = 0
    floodc5timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "floodchurt"
mfloodc = mfloodc + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusfloodc
countr29 = 0
movefloodc5down
end sub

sub movefloodc5down
floodc033.z = -200
floodc034.z = -200
floodc035.z = -200
floodc036.z = -200
floodc037.z = -200
floodc038.z = -200
floodc039.z = -200
floodc040.z = -200
end sub

sub checkbonusfloodc
If mfloodc = 5 then
flashplaat1.ImageA = "flshalopin"
DMD "", "", "dmdmc", eNone, eNone, eNone, 1000, True, ""
playsound "obj_complete"
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
StopmodeEndofBall
li083.state = 1
mfloodc = 0
enemylft = 0
mission11 = 1
mcomplete = mcomplete + 1
'UpdateMusicNow
AddMultiball 1
ChangeSong
end if
end sub

'********mode 2***************

sub startmode2
StopSong
missionmodes = 1
PlaySong "missionmusic" 
mode1TimerCount = 60
mode1timer.Enabled = 1
enablelogomode
end sub

sub enablelogomode
tim001.Enabled = 1
tim002.Enabled = 1
tim003.Enabled = 1
tim004.Enabled = 1
tim005.Enabled = 1
tim006.Enabled = 1 
logooTimer.enabled = true
tlogo001.enabled = 1
logo001.z = 50
end sub

Sub tlogo001_Hit()
	tlogo001.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
logo001.z = -200
Playsound "pickingup"
mlogo = mlogo + 1 
mode1TimerCount = mode1TimerCount + 5
logo002.z = 50
tlogo002.enabled = 1
checkbonuslogoo
end sub

Sub tlogo002_Hit()
	tlogo002.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
logo002.z = -200
Playsound "pickingup"
mlogo = mlogo + 1
mode1TimerCount = mode1TimerCount + 5 
logo003.z = 50
tlogo003.enabled = 1
checkbonuslogoo
end sub

Sub tlogo003_Hit()
	tlogo003.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
logo003.z = -200
Playsound "pickingup"
mlogo = mlogo + 1
mode1TimerCount = mode1TimerCount + 5
logo004.z = 50
tlogo004.enabled = 1 
checkbonuslogoo
end sub

Sub tlogo004_Hit()
	tlogo004.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
logo004.z = -200
Playsound "pickingup"
mlogo = mlogo + 1 
mode1TimerCount = mode1TimerCount + 5
logo005.z = 50
tlogo005.enabled = 1 
checkbonuslogoo
end sub

Sub tlogo005_Hit()
	tlogo005.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
logo005.z = -200
Playsound "pickingup"
mlogo = mlogo + 1
mode1TimerCount = mode1TimerCount + 5
checkbonuslogoo
end sub

sub checkbonuslogoo
If mlogo = 5 then
flashplaat1.ImageA = "flshalopin"
DMD "", "", "dmdmc", eNone, eNone, eNone, 1000, True, ""
logooTimer.enabled = false
playsound "obj_complete"
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
li026.state = 1
StopmodeEndofBall
mlogo = 0
mission1 = 1
mcomplete = mcomplete + 1
'UpdateMusicNow
AddMultiball 1
ChangeSong
'multitimer.enabled = 1
timerquotemulti.enabled = 1
end if
end sub

Sub logooTimer_Timer
   logo001.ObjRotz = logo001.ObjRotz + 1
   if logo001.ObjRotz > 360 then
       logo001.ObjRotz = 1
   end if
   logo002.ObjRotz = logo002.ObjRotz + 1
   if logo002.ObjRotz > 360 then
       logo002.ObjRotz = 1
   end if
   logo003.ObjRotz = logo003.ObjRotz + 1
   if logo003.ObjRotz > 360 then
       logo003.ObjRotz = 1
   end if
   logo004.ObjRotz = logo004.ObjRotz + 1
   if logo004.ObjRotz > 360 then
       logo004.ObjRotz = 1
   end if
   logo005.ObjRotz = logo005.ObjRotz + 1
   if logo005.ObjRotz > 360 then
       logo005.ObjRotz = 1
   end if
end sub

'********mode 3***************

sub startmode3
StopSong
missionmodes = 1
PlaySong "missionmusic" 
mode1TimerCount = 60
mode1timer.Enabled = 1
bridgeDir = 1
bridgeup.enabled = 1
PlaySound "LIFTY2"
mskillow = 1
Ramp005.Collidable = 1
li064.state = 2
end sub

sub endskillow
flashplaat1.ImageA = "flshalopin"
Ramp005.Collidable = 0
DMD "", "", "dmdmc", eNone, eNone, eNone, 1000, True, ""
playsound "obj_complete"
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
StopmodeEndofBall
li028.state = 1
mission3 = 1
mcomplete = mcomplete + 1
bridgeDir = 1
bridgedown.enabled = 1
mskillow = 0
li028.state = 1
'UpdateMusicNow
li064.state = 1
AddMultiball 1
ChangeSong
'multitimer.enabled = 1
timerquotemulti.enabled = 1
end sub

'********mode 4***************

sub startmode4
StopSong
missionmodes = 1
PlaySong "missionmusic" 
mode1TimerCount = 60
mode1timer.Enabled = 1
enablegrunds
end sub

sub enablegrunds
tim001.Enabled = 1
tim002.Enabled = 1
tim003.Enabled = 1
tim004.Enabled = 1
tim005.Enabled = 1
tim006.Enabled = 1 
tenem001.enabled = 1
grund1timer.enabled = true
UpdateEnemiesLeft 
end sub

Sub tenem001_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tenem001.enabled = 0
    grund1timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "grundhurt"
mgrund = mgrund + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusgrund
countr5 = 0
movegrund1down
tenem002.enabled = 1
grund2timer.enabled = true 
end sub

sub movegrund1down
grund001.z = -200
grund002.z = -200
grund003.z = -200
grund004.z = -200
grund005.z = -200
grund006.z = -200
grund007.z = -200
grund008.z = -200
end sub

Sub tenem002_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tenem002.enabled = 0
    grund2timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "grundhurt"
mgrund = mgrund + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusgrund
countr6 = 0
movegrund2down
tenem003.enabled = 1
grund3timer.enabled = true 
end sub

sub movegrund2down
grund009.z = -200
grund010.z = -200
grund011.z = -200
grund012.z = -200
grund013.z = -200
grund014.z = -200
grund015.z = -200
grund016.z = -200
end sub

Sub tenem003_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tenem003.enabled = 0
    grund3timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "grundhurt"
mgrund = mgrund + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusgrund
countr7 = 0
movegrund3down
tenem004.enabled = 1
grund4timer.enabled = true 
end sub

sub movegrund3down
grund017.z = -200
grund018.z = -200
grund019.z = -200
grund020.z = -200
grund021.z = -200
grund022.z = -200
grund023.z = -200
grund024.z = -200
end sub

Sub tenem004_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tenem004.enabled = 0
    grund4timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "grundhurt"
mgrund = mgrund + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusgrund
countr8 = 0
movegrund4down
tenem005.enabled = 1
grund5timer.enabled = true 
end sub

sub movegrund4down
grund025.z = -200
grund026.z = -200
grund027.z = -200
grund028.z = -200
grund029.z = -200
grund030.z = -200
grund031.z = -200
grund032.z = -200
end sub

Sub tenem005_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tenem005.enabled = 0
    grund5timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "grundhurt"
mgrund = mgrund + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusgrund
countr9 = 0
movegrund5down
end sub

sub movegrund5down
grund033.z = -200
grund034.z = -200
grund035.z = -200
grund036.z = -200
grund037.z = -200
grund038.z = -200
grund039.z = -200
grund040.z = -200
end sub

sub checkbonusgrund
If mgrund = 5 then
flashplaat1.ImageA = "flshalopin"
DMD "", "", "dmdmc", eNone, eNone, eNone, 1000, True, ""
playsound "obj_complete"
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
StopmodeEndofBall
mgrund = 0
enemylft = 0
li084.state = 1
mission14 = 1
mcomplete = mcomplete + 1
'UpdateMusicNow
AddMultiball 1
ChangeSong
'multitimer.enabled = 1
timerquotemulti.enabled = 1
end if
end sub

'********mode 5***************

sub startmode5
StopSong
missionmodes = 1
PlaySong "missionmusic" 
mode1TimerCount = 60
mode1timer.Enabled = 1
enablechiefletters
end sub

sub enablechiefletters
tim001.Enabled = 1
tim002.Enabled = 1
tim003.Enabled = 1
tim004.Enabled = 1
tim005.Enabled = 1
tim006.Enabled = 1 
chiefTimer.enabled = true
cz.Enabled = 1
hz.Enabled = 1
iz.Enabled = 1
ez.Enabled = 1
fz.Enabled = 1
c.z = 50
h.z = 50
i.z = 50
e.z = 50
f.z = 50
end sub

Sub cz_Hit()
	cz.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
c.z = -200
Playsound "pickingup"
cletter = cletter + 1 
mode1TimerCount = mode1TimerCount + 5
checkbonusletters
end sub

Sub hz_Hit()
	hz.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
h.z = -200
Playsound "pickingup"
cletter = cletter + 1
mode1TimerCount = mode1TimerCount + 5 
checkbonusletters
end sub

Sub iz_Hit()
	iz.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
i.z = -200
Playsound "pickingup"
cletter = cletter + 1
mode1TimerCount = mode1TimerCount + 5 
checkbonusletters
end sub

Sub ez_Hit()
	ez.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
e.z = -200
Playsound "pickingup"
cletter = cletter + 1 
mode1TimerCount = mode1TimerCount + 5
checkbonusletters
end sub

Sub fz_Hit()
	fz.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
f.z = -200
Playsound "pickingup"
cletter = cletter + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusletters
end sub

sub checkbonusletters
If cletter = 5 then
flashplaat1.ImageA = "flshalopin"
DMD "", "", "dmdmc", eNone, eNone, eNone, 1000, True, ""
chiefTimer.enabled = false
playsound "obj_complete"
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
StopmodeEndofBall
li027.state = 1
cletter = 0
mission2 = 1
mcomplete = mcomplete + 1
'UpdateMusicNow
AddMultiball 1
ChangeSong
'multitimer.enabled = 1
timerquotemulti.enabled = 1
end if
end sub

Sub chiefTimer_Timer
   c.ObjRotz = c.ObjRotz + 1
   if c.ObjRotz > 360 then
       C.ObjRotz = 1
   end if
   h.ObjRotz = h.ObjRotz + 1
   if h.ObjRotz > 360 then
       h.ObjRotz = 1
   end if
   i.ObjRotz = i.ObjRotz + 1
   if i.ObjRotz > 360 then
       i.ObjRotz = 1
   end if
   e.ObjRotz = e.ObjRotz + 1
   if e.ObjRotz > 360 then
       e.ObjRotz = 1
   end if
   f.ObjRotz = f.ObjRotz + 1
   if f.ObjRotz > 360 then
       f.ObjRotz = 1
   end if
end sub

'********mode 6***************

sub startmode6
StopSong
missionmodes = 1
PlaySong "missionmusic" 
mode1TimerCount = 60
mode1timer.Enabled = 1
enableelites
end sub

sub enableelites
tim001.Enabled = 1
tim002.Enabled = 1
tim003.Enabled = 1
tim004.Enabled = 1
tim005.Enabled = 1
tim006.Enabled = 1 
teli001.enabled = 1
elite1timer.enabled = true
UpdateEnemiesLeft
end sub

Sub teli001_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    teli001.enabled = 0
    elite1timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "elitehurt"
melite = melite + 1
mode1TimerCount = mode1TimerCount + 5
checkbonuselite
countr10 = 0
moveelite1down
teli002.enabled = 1
elite2timer.enabled = true 
end sub

sub moveelite1down
elite001.z = -200
elite002.z = -200
elite003.z = -200
elite004.z = -200
elite005.z = -200
elite006.z = -200
elite007.z = -200
end sub

Sub teli002_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    teli002.enabled = 0
    elite2timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "elitehurt"
melite = melite + 1
mode1TimerCount = mode1TimerCount + 5
checkbonuselite
countr11 = 0
moveelite2down
teli003.enabled = 1
elite3timer.enabled = true 
end sub

sub moveelite2down
elite008.z = -200
elite009.z = -200
elite010.z = -200
elite011.z = -200
elite012.z = -200
elite013.z = -200
elite014.z = -200
end sub

Sub teli003_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    teli003.enabled = 0
    elite3timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "elitehurt"
melite = melite + 1
mode1TimerCount = mode1TimerCount + 5
checkbonuselite
countr12 = 0
moveelite3down
teli004.enabled = 1
elite4timer.enabled = true 
end sub

sub moveelite3down
elite015.z = -200
elite016.z = -200
elite017.z = -200
elite018.z = -200
elite019.z = -200
elite020.z = -200
elite021.z = -200
end sub

Sub teli004_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    teli004.enabled = 0
    elite4timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "elitehurt"
melite = melite + 1
mode1TimerCount = mode1TimerCount + 5
checkbonuselite
countr13 = 0
moveelite4down
teli005.enabled = 1
elite5timer.enabled = true 
end sub

sub moveelite4down
elite022.z = -200
elite023.z = -200
elite024.z = -200
elite025.z = -200
elite026.z = -200
elite027.z = -200
elite028.z = -200
end sub

Sub teli005_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    teli005.enabled = 0
    elite5timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "elitehurt"
melite = melite + 1
mode1TimerCount = mode1TimerCount + 5
checkbonuselite
countr14 = 0
moveelite5down
end sub

sub moveelite5down
elite029.z = -200
elite030.z = -200
elite031.z = -200
elite032.z = -200
elite033.z = -200
elite034.z = -200
elite035.z = -200
end sub

sub checkbonuselite
If melite = 5 then
flashplaat1.ImageA = "flshalopin"
DMD "", "", "dmdmc", eNone, eNone, eNone, 1000, True, ""
playsound "obj_complete"
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
StopmodeEndofBall
li093.state = 1
melite = 0
enemylft = 0
mission15 = 1
mcomplete = mcomplete + 1
'UpdateMusicNow
AddMultiball 1
ChangeSong
'multitimer.enabled = 1
timerquotemulti.enabled = 1
end if
end sub

'********mode 7***************

sub startmode7
StopSong
missionmodes = 1
PlaySong "missionmusic" 
mode1TimerCount = 60
mode1timer.Enabled = 1
bridgeDir = 1
bridgeup.enabled = 1
PlaySound "LIFTY2"
mskilhigh = 1
Ramp005.Collidable = 1
li064.state = 2
end sub

sub endskilhigh
flashplaat1.ImageA = "flshalopin"
Ramp005.Collidable = 0
DMD "", "", "dmdmc", eNone, eNone, eNone, 1000, True, ""
playsound "obj_complete"
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
StopmodeEndofBall
mission7 = 1
bridgeDir = 1
bridgedown.enabled = 1
li072.state = 1
mission5 = 1
mcomplete = mcomplete + 1
mskilhigh = 0
'UpdateMusicNow
li064.state = 1
AddMultiball 1
ChangeSong
'multitimer.enabled = 1
timerquotemulti.enabled = 1
end sub

'********mode 8***************

sub startmode8
StopSong
missionmodes = 1
PlaySong "missionmusic" 
mode1TimerCount = 60
mode1timer.Enabled = 1
enablegrenademode
end sub

sub enablegrenademode
tim001.Enabled = 1
tim002.Enabled = 1
tim003.Enabled = 1
tim004.Enabled = 1
tim005.Enabled = 1
tim006.Enabled = 1 
fraggyTimer.enabled = true
tgrenade001.enabled = 1
grenade001.z = 50
end sub

Sub tgrenade001_Hit()
	tgrenade001.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
grenade001.z = -200
Playsound "pickingup"
mgrenade = mgrenade + 1 
mode1TimerCount = mode1TimerCount + 5
grenade002.z = 50
tgrenade002.enabled = 1
checkbonusgrenadeo
end sub

Sub tgrenade002_Hit()
	tgrenade002.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
grenade002.z = -200
Playsound "pickingup"
mgrenade = mgrenade + 1
mode1TimerCount = mode1TimerCount + 5 
grenade003.z = 50
tgrenade003.enabled = 1
checkbonusgrenadeo
end sub

Sub tgrenade003_Hit()
	tgrenade003.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
grenade003.z = -200
Playsound "pickingup"
mgrenade = mgrenade + 1
mode1TimerCount = mode1TimerCount + 5
grenade004.z = 50
tgrenade004.enabled = 1 
checkbonusgrenadeo
end sub

Sub tgrenade004_Hit()
	tgrenade004.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
grenade004.z = -200
Playsound "pickingup"
mgrenade = mgrenade + 1 
mode1TimerCount = mode1TimerCount + 5
grenade005.z = 50
tgrenade005.enabled = 1 
checkbonusgrenadeo
end sub

Sub tgrenade005_Hit()
	tgrenade005.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
grenade005.z = -200
Playsound "pickingup"
mgrenade = mgrenade + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusgrenadeo
end sub

sub checkbonusgrenadeo
If mgrenade = 5 then
flashplaat1.ImageA = "flshalopin"
fraggyTimer.enabled = false
DMD "", "", "dmdmc", eNone, eNone, eNone, 1000, True, ""
playsound "obj_complete"
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
li071.state = 1
StopmodeEndofBall
mgrenade = 0
mission4 = 1
mcomplete = mcomplete + 1
'UpdateMusicNow
AddMultiball 1
ChangeSong
'multitimer.enabled = 1
timerquotemulti.enabled = 1
end if
end sub

Sub fraggyTimer_Timer
   grenade001.ObjRotz = grenade001.ObjRotz + 1
   if grenade001.ObjRotz > 360 then
       grenade001.ObjRotz = 1
   end if
   grenade002.ObjRotz = grenade002.ObjRotz + 1
   if grenade002.ObjRotz > 360 then
       grenade002.ObjRotz = 1
   end if
   grenade003.ObjRotz = grenade003.ObjRotz + 1
   if grenade003.ObjRotz > 360 then
       grenade003.ObjRotz = 1
   end if
   grenade004.ObjRotz = grenade004.ObjRotz + 1
   if grenade004.ObjRotz > 360 then
       grenade004.ObjRotz = 1
   end if
   grenade005.ObjRotz = grenade005.ObjRotz + 1
   if grenade005.ObjRotz > 360 then
       grenade005.ObjRotz = 1
   end if
end sub

'********mode 9***************

sub startmode9
StopSong
missionmodes = 1
PlaySong "missionmusic" 
mode1TimerCount = 60
mode1timer.Enabled = 1
enablejackals
UpdateEnemiesLeft
end sub

sub enablejackals
tim001.Enabled = 1
tim002.Enabled = 1
tim003.Enabled = 1
tim004.Enabled = 1
tim005.Enabled = 1
tim006.Enabled = 1 
tjac001.enabled = 1
jackal1timer.enabled = true 
end sub

Sub tjac001_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tjac001.enabled = 0
    jackal1timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "jackalhurt"
mjackal = mjackal + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusjackal
countr15 = 0
movejackal1down
tjac002.enabled = 1
jackal2timer.enabled = true 
end sub

sub movejackal1down
jackal001.z = -200
jackal002.z = -200
jackal003.z = -200
jackal004.z = -200
jackal005.z = -200
jackal006.z = -200
jackal007.z = -200
end sub

Sub tjac002_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tjac002.enabled = 0
    jackal2timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "jackalhurt"
mjackal = mjackal + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusjackal
countr16 = 0
movejackal2down
tjac003.enabled = 1
jackal3timer.enabled = true 
end sub

sub movejackal2down
jackal008.z = -200
jackal009.z = -200
jackal010.z = -200
jackal011.z = -200
jackal012.z = -200
jackal013.z = -200
jackal014.z = -200
end sub

Sub tjac003_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tjac003.enabled = 0
    jackal3timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "jackalhurt"
mjackal = mjackal + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusjackal
countr17 = 0
movejackal3down
tjac004.enabled = 1
jackal4timer.enabled = true 
end sub

sub movejackal3down
jackal015.z = -200
jackal016.z = -200
jackal017.z = -200
jackal018.z = -200
jackal019.z = -200
jackal020.z = -200
jackal021.z = -200
end sub

Sub tjac004_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tjac004.enabled = 0
    jackal4timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "jackalhurt"
mjackal = mjackal + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusjackal
countr18 = 0
movejackal4down
tjac005.enabled = 1
jackal5timer.enabled = true 
end sub

sub movejackal4down
jackal022.z = -200
jackal023.z = -200
jackal024.z = -200
jackal025.z = -200
jackal026.z = -200
jackal027.z = -200
jackal028.z = -200
end sub

Sub tjac005_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tjac005.enabled = 0
    jackal5timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "jackalhurt"
mjackal = mjackal + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusjackal
countr19 = 0
movejackal5down
end sub

sub movejackal5down
jackal029.z = -200
jackal030.z = -200
jackal031.z = -200
jackal032.z = -200
jackal033.z = -200
jackal034.z = -200
jackal035.z = -200
end sub

sub checkbonusjackal
If mjackal = 5 then
flashplaat1.ImageA = "flshalopin"
DMD "", "", "dmdmc", eNone, eNone, eNone, 1000, True, ""
playsound "obj_complete"
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
StopmodeEndofBall
li092.state = 1
mjackal = 0
enemylft = 0
mission13 = 1
mcomplete = mcomplete + 1
'UpdateMusicNow
AddMultiball 1
ChangeSong
'multitimer.enabled = 1
timerquotemulti.enabled = 1
end if
end sub

'********mode 10***************

sub startmode10
StopSong
missionmodes = 1
PlaySong "missionmusic" 
mode1TimerCount = 60
mode1timer.Enabled = 1
enableskullz
end sub

sub enableskullz
tim001.Enabled = 1
tim002.Enabled = 1
tim003.Enabled = 1
tim004.Enabled = 1
tim005.Enabled = 1
tim006.Enabled = 1 
skullyTimer.enabled = true
tskull001.Enabled = 1
tskull002.Enabled = 1
tskull003.Enabled = 1
tskull004.Enabled = 1
tskull005.Enabled = 1
skull001.z = 50
skull002.z = 50
skull003.z = 50
skull004.z = 50
skull005.z = 50
end sub

Sub tskull001_Hit()
	tskull001.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
skull001.z = -200
Playsound "pickingup"
mskull = mskull + 1 
mode1TimerCount = mode1TimerCount + 5
checkbonuskullz
end sub

Sub tskull002_Hit()
	tskull002.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
skull002.z = -200
Playsound "pickingup"
mskull = mskull + 1
mode1TimerCount = mode1TimerCount + 5 
checkbonuskullz
end sub

Sub tskull003_Hit()
	tskull003.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
skull003.z = -200
Playsound "pickingup"
mskull = mskull + 1
mode1TimerCount = mode1TimerCount + 5 
checkbonuskullz
end sub

Sub tskull004_Hit()
	tskull004.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
skull004.z = -200
Playsound "pickingup"
mskull = mskull + 1 
mode1TimerCount = mode1TimerCount + 5
checkbonuskullz
end sub

Sub tskull005_Hit()
	tskull005.Enabled = 0
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
skull005.z = -200
Playsound "pickingup"
mskull = mskull + 1
mode1TimerCount = mode1TimerCount + 5
checkbonuskullz
end sub

sub checkbonuskullz
If mskull = 5 then
flashplaat1.ImageA = "flshalopin"
DMD "", "", "dmdmc", eNone, eNone, eNone, 1000, True, ""
chiefTimer.enabled = false
playsound "obj_complete"
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
StopmodeEndofBall
mskull = 0
mission6 = 1
mcomplete = mcomplete + 1
li073.state = 1
'UpdateMusicNow
AddMultiball 1
ChangeSong
'multitimer.enabled = 1
timerquotemulti.enabled = 1
end if
end sub

Sub skullyTimer_Timer
   skull001.ObjRotz = skull001.ObjRotz + 1
   if skull001.ObjRotz > 360 then
       skull001.ObjRotz = 1
   end if
   skull002.ObjRotz = skull002.ObjRotz + 1
   if skull002.ObjRotz > 360 then
       skull002.ObjRotz = 1
   end if
   skull003.ObjRotz = skull003.ObjRotz + 1
   if skull003.ObjRotz > 360 then
       skull003.ObjRotz = 1
   end if
   skull004.ObjRotz = skull004.ObjRotz + 1
   if skull004.ObjRotz > 360 then
       skull004.ObjRotz = 1
   end if
   skull005.ObjRotz = skull005.ObjRotz + 1
   if skull005.ObjRotz > 360 then
       skull005.ObjRotz = 1
   end if
end sub

'********mode 11***************

sub startmode11
StopSong
missionmodes = 1
PlaySong "missionmusic" 
mode1TimerCount = 60
mode1timer.Enabled = 1
enablefloodes
end sub

sub enablefloodes
tim001.Enabled = 1
tim002.Enabled = 1
tim003.Enabled = 1
tim004.Enabled = 1
tim005.Enabled = 1
tim006.Enabled = 1 
tfloode001.enabled = 1
floode1timer.enabled = true
UpdateEnemiesLeft 
end sub

Sub tfloode001_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tfloode001.enabled = 0
    floode1timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "floodehurt"
mfloode = mfloode + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusfloode
countr25 = 0
movefloode1down
tfloode002.enabled = 1
floode2timer.enabled = true 
end sub

sub movefloode1down
floode001.z = -200
floode002.z = -200
floode003.z = -200
floode004.z = -200
floode005.z = -200
floode006.z = -200
floode007.z = -200
floode008.z = -200
end sub

Sub tfloode002_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tfloode002.enabled = 0
    floode2timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "floodehurt"
mfloode = mfloode + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusfloode
countr24 = 0
movefloode2down
tfloode003.enabled = 1
floode3timer.enabled = true 
end sub

sub movefloode2down
floode009.z = -200
floode010.z = -200
floode011.z = -200
floode012.z = -200
floode013.z = -200
floode014.z = -200
floode015.z = -200
floode016.z = -200
end sub

Sub tfloode003_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tfloode003.enabled = 0
    floode3timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "floodehurt"
mfloode = mfloode + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusfloode
countr25 = 0
movefloode3down
tfloode004.enabled = 1
floode4timer.enabled = true 
end sub

sub movefloode3down
floode017.z = -200
floode018.z = -200
floode019.z = -200
floode020.z = -200
floode021.z = -200
floode022.z = -200
floode023.z = -200
floode024.z = -200
end sub

Sub tfloode004_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tfloode004.enabled = 0
    floode4timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "floodehurt"
mfloode = mfloode + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusfloode
countr23 = 0
movefloode4down
tfloode005.enabled = 1
floode5timer.enabled = true 
end sub

sub movefloode4down
floode025.z = -200
floode026.z = -200
floode027.z = -200
floode028.z = -200
floode029.z = -200
floode030.z = -200
floode031.z = -200
floode032.z = -200
end sub

Sub tfloode005_Hit()
enemylft = enemylft + 1
UpdateEnemiesLeft
    tfloode005.enabled = 0
    floode5timer.enabled = false
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
kildys = kildys + 1
Playsound "floodehurt"
mfloode = mfloode + 1
mode1TimerCount = mode1TimerCount + 5
checkbonusfloode
countr24 = 0
movefloode5down
end sub

sub movefloode5down
floode033.z = -200
floode034.z = -200
floode035.z = -200
floode036.z = -200
floode037.z = -200
floode038.z = -200
floode039.z = -200
floode040.z = -200
end sub

sub checkbonusfloode
If mfloode = 5 then
flashplaat1.ImageA = "flshalopin"
DMD "", "", "dmdmc", eNone, eNone, eNone, 1000, True, ""
playsound "obj_complete"
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
StopmodeEndofBall
enemylft = 0
li094.state = 1
mfloode = 0
mission12 = 1
mcomplete = mcomplete + 1
'UpdateMusicNow
AddMultiball 1
ChangeSong
'multitimer.enabled = 1
timerquotemulti.enabled = 1
end if
end sub

'********mode 12***************

sub startmode12
StopSong
missionmodes = 1
halovara1 = 1 
PlaySong "missionmusic" 
mode1TimerCount = 90
mode1timer.Enabled = 1
li065.state = 2
enableHaloA
end sub

sub enableHaloA
tim001.Enabled = 1
tim002.Enabled = 1
tim003.Enabled = 1
tim004.Enabled = 1
tim005.Enabled = 1
tim006.Enabled = 1 
ThaloA.enabled = 1
end sub

Sub ThaloA_Hit()
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
mreactor = mreactor + 1
mode1TimerCount = mode1TimerCount + 5
Updatereactor
checkhaloAactivated
end sub

Sub Updatereactor
    Select Case mreactor  
Case 0 li089.state = 0:li090.state = 0:li091.state = 0
Case 1 li089.state = 1:li090.state = 0:li091.state = 0:PlaySound "reactor1"
Case 2 li089.state = 1:li090.state = 1:li091.state = 0:PlaySound "reactor2"
Case 3 li089.state = 1:li090.state = 1:li091.state = 1:PlaySound "reactor3"
'Case 4 li089.state = 0:li090.state = 0:li091.state = 0:DestroyhaloA'PlaySound ""
    End Select
end sub

sub checkhaloAactivated
If (li089.state = 1)and (li090.state =1)and (li091.state = 1) then
flashplaat1.ImageA = "m8"
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
halovara = 1
halovara1 = 0 
HaloTimer.enabled = 1
li089.state = 2
li090.state = 2
li091.state = 2
mission7 = 1
mcomplete = mcomplete + 1
li074.state = 1
mode1timer.Enabled = 0
TurnOffClock
tim001.Enabled = 0
tim002.Enabled = 0
tim003.Enabled = 0
tim004.Enabled = 0
tim005.Enabled = 0
tim006.Enabled = 0
vpmTimer.AddTimer 1100, "enablemuur1'" 
end if
end sub

sub enablemuur1
haloLstop.IsDropped=false
ThaloA001.enabled = 1
end sub


sub ThaloA001_Hit()
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
DestroyhaloA
end sub

sub DestroyhaloA
stopsong
PlaySound "halodestroyed2"
vpmTimer.AddTimer 1400, "explosie1'"
vpmTimer.AddTimer 4000, "explosie2'"
vpmTimer.AddTimer 6000, "explosie3'"
vpmTimer.AddTimer 7800, "explosie4'"
vpmTimer.AddTimer 11000, "HaloAkapot'"
end sub

sub explosie1
explosion001.visible = True
end sub

sub explosie2
dmdhaloboom
reactoryA.Image = "Generatorburned"
explosion003.visible = True
end sub

sub explosie3
explosion004.visible = True
explosion001.visible = false
end sub

sub explosie4
HaloTimer.enabled = 0
explosion005.visible = True
end sub

sub HaloAkapot
halo.visible = False
explosion003.visible = False
explosion004.visible = False
explosion005.visible = False
haloLstop.IsDropped=true
vpmTimer.AddTimer 3000, "newhalo'"
UpdateMusicNow
ThaloA001.enabled = 0
mcomplete = mcomplete + 1
li075.state = 1
mission8 = 1
li065.state = 1
end sub

sub newhalo
playsound "afterhaloA"
halo2.visible = True
	mission9 = 0
halovara = 0
missionmodes = 0
flashplaat1.ImageA = "flshalopin"
end sub

sub dmdhaloboom
DMD "", "", "haloboom00", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom01", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom02", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom03", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom04", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom05", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom06", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom07", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom08", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom09", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom10", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom11", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom12", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom13", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom14", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom15", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom16", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom17", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom18", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom19", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom20", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom21", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom22", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom23", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom24", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom25", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom26", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom27", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom28", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom29", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom30", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom31", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom32", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom33", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom34", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom35", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom36", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom37", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom38", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom39", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom40", eNone, eNone, eNone, 100, True, ""
DMD "", "", "haloboom41", eNone, eNone, eNone, 100, True, ""
end sub

'********mode 13***************

sub startmode13
StopSong
missionmodes = 1
PlaySong "missionmusic" 
mode1TimerCount = 60
mode1timer.Enabled = 1
end sub

'********mode 14***************

sub startmode14
StopSong
missionmodes = 1
halovarb1 = 1 
PlaySong "missionmusic" 
mode1TimerCount = 90
mode1timer.Enabled = 1
li062.state = 2
enableHaloB
end sub

sub enableHaloB
tim001.Enabled = 1
tim002.Enabled = 1
tim003.Enabled = 1
tim004.Enabled = 1
tim005.Enabled = 1
tim006.Enabled = 1 
ThaloB.enabled = 1
end sub

Sub ThaloB_Hit()
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
mreactor2 = mreactor2 + 1
mode1TimerCount = mode1TimerCount + 5
Updatereactor2
checkhaloAactivated2
end sub

Sub Updatereactor2
    Select Case mreactor2  
Case 0 li086.state = 0:li087.state = 0:li088.state = 0
Case 1 li086.state = 1:li087.state = 0:li088.state = 0:PlaySound "reactor1"
Case 2 li086.state = 1:li087.state = 1:li088.state = 0:PlaySound "reactor2"
Case 3 li086.state = 1:li087.state = 1:li088.state = 1:PlaySound "reactor3"
'Case 4 li086.state = 0:li087.state = 0:li088.state = 0:DestroyhaloA'PlaySound ""
    End Select
end sub

sub checkhaloAactivated2
If (li086.state = 1)and (li087.state =1)and (li088.state = 1) then
flashplaat1.ImageA = "m10"
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
halovarb = 1
halovarb1 = 0 
HaloTimer.enabled = 1
li086.state = 2
li087.state = 2
li088.state = 2
mission9 = 1
mcomplete = mcomplete + 1
li076.state = 1
mode1timer.Enabled = 0
TurnOffClock
tim001.Enabled = 0
tim002.Enabled = 0
tim003.Enabled = 0
tim004.Enabled = 0
tim005.Enabled = 0
tim006.Enabled = 0
vpmTimer.AddTimer 1100, "enablemuur2'" 
end if
end sub

sub enablemuur2
haloRstop.IsDropped=false
ThaloB001.enabled = 1
end sub


sub ThaloB001_Hit()
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
DestroyhaloB
end sub

sub DestroyhaloB
stopsong
PlaySound "halodestroyed2"
vpmTimer.AddTimer 1400, "explosie1a'"
vpmTimer.AddTimer 4000, "explosie2a'"
vpmTimer.AddTimer 6000, "explosie3a'"
vpmTimer.AddTimer 7800, "explosie4a'"
vpmTimer.AddTimer 11000, "HaloBkapot'"
end sub

sub explosie1a
explosion002.visible = True
end sub

sub explosie2a
dmdhaloboom
reactoryB.Image = "Generatorburned"
explosion006.visible = True
end sub

sub explosie3a
explosion007.visible = True
explosion002.visible = false
end sub

sub explosie4a
HaloTimer.enabled = 0
explosion008.visible = True
end sub

sub HaloBkapot
halo2.visible = False
halovarb = 0
missionmodes = 0
flashplaat1.ImageA = "flshalopin"
explosion006.visible = False
explosion007.visible = False
explosion008.visible = False
haloRstop.IsDropped=true
UpdateMusicNow
ThaloB001.enabled = 0
mcomplete = mcomplete + 1
li077.state = 1
mission10 = 1
li062.state = 1
end sub

'********mode 15***************

sub startmode15
StopSong
missionmodes = 1
PlaySong "missionmusic" 
mode1TimerCount = 60
mode1timer.Enabled = 1
end sub


'********end of ball stop missions + add time ***************
sub stopmissions
enemylft = 0
'li065.state = 1
'li064.state = 1
flashplaat1.ImageA = "flshalopin"
bridgeDir = 1
halovara1 = 0 
bridgedown.enabled = 1
Ramp005.Collidable = 0
ThaloA.enabled = 0
tim001.Enabled = 0
tim002.Enabled = 0
tim003.Enabled = 0
tim004.Enabled = 0
tim005.Enabled = 0
tim006.Enabled = 0
tenem001.enabled = 0
tenem002.enabled = 0
tenem003.enabled = 0
tenem004.enabled = 0
tenem005.enabled = 0
teli001.enabled = 0
teli002.enabled = 0
teli003.enabled = 0
teli004.enabled = 0
teli005.enabled = 0
tjac001.enabled = 0
tjac002.enabled = 0
tjac003.enabled = 0
tjac004.enabled = 0
tjac005.enabled = 0
tfloode001.enabled = 0
tfloode002.enabled = 0
tfloode003.enabled = 0
tfloode004.enabled = 0
tfloode005.enabled = 0
tfloodc001.enabled = 0
tfloodc002.enabled = 0
tfloodc003.enabled = 0
tfloodc004.enabled = 0
tfloodc005.enabled = 0
tlogo001.enabled = 0
tlogo002.enabled = 0
tlogo003.enabled = 0
tlogo004.enabled = 0
tlogo005.enabled = 0
tskull001.enabled = 0
tskull002.enabled = 0
tskull003.enabled = 0
tskull004.enabled = 0
tskull005.enabled = 0
tgrenade001.enabled=0
tgrenade002.enabled=0
tgrenade003.enabled=0
tgrenade004.enabled=0
tgrenade005.enabled=0
chiefTimer.enabled = false
grund1timer.enabled = false
grund2timer.enabled = false
grund3timer.enabled = false
grund4timer.enabled = false
grund5timer.enabled = false
elite1timer.enabled = false
elite2timer.enabled = false
elite3timer.enabled = false
elite4timer.enabled = false
elite5timer.enabled = false
jackal1timer.enabled = false
jackal2timer.enabled = false
jackal3timer.enabled = false
jackal4timer.enabled = false
jackal5timer.enabled = false
floodc1timer.enabled = false
floodc2timer.enabled = false
floodc3timer.enabled = false
floodc4timer.enabled = false
floodc5timer.enabled = false
floode1timer.enabled = false
floode2timer.enabled = false
floode3timer.enabled = false
floode4timer.enabled = false
floode5timer.enabled = false
mskillow = 0
mskilhigh = 0
cz.Enabled = 0
hz.Enabled = 0
iz.Enabled = 0
ez.Enabled = 0
fz.Enabled = 0
c.z = -200
h.z = -200
i.z = -200
e.z = -200
f.z = -200
cletter = 0
mgrund = 0
melite = 0
mjackal = 0
mfloodc =0
mfloode = 0
mlogo = 0
mgrenade = 0
mskull = 0
countr5 = 0
countr6 = 0
countr7 = 0
countr8 = 0
countr9 = 0
countr10 = 0
countr11 = 0
countr12 = 0
countr13 = 0
countr14 = 0
countr15 = 0
countr16 = 0
countr17 = 0
countr18 = 0
countr19 = 0
countr20 = 0
countr21 = 0
countr22 = 0
countr23 = 0
countr24 = 0
countr25 = 0
countr26 = 0
countr27 = 0
countr28 = 0
countr29 = 0
grund001.z = -200
grund002.z = -200
grund003.z = -200
grund004.z = -200
grund005.z = -200
grund006.z = -200
grund007.z = -200
grund008.z = -200
grund009.z = -200
grund010.z = -200
grund011.z = -200
grund012.z = -200
grund013.z = -200
grund014.z = -200
grund015.z = -200
grund016.z = -200
grund017.z = -200
grund018.z = -200
grund019.z = -200
grund020.z = -200
grund021.z = -200
grund022.z = -200
grund023.z = -200
grund024.z = -200
grund025.z = -200
grund026.z = -200
grund027.z = -200
grund028.z = -200
grund029.z = -200
grund030.z = -200
grund030.z = -200
grund031.z = -200
grund032.z = -200
grund033.z = -200
grund034.z = -200
grund035.z = -200
grund036.z = -200
grund037.z = -200
grund038.z = -200
grund039.z = -200
grund040.z = -200
elite001.z=-200
elite002.z=-200
elite003.z=-200
elite004.z=-200
elite005.z=-200
elite006.z=-200
elite007.z=-200
elite008.z=-200
elite009.z=-200
elite010.z=-200
elite011.z=-200
elite012.z=-200
elite013.z=-200
elite014.z=-200
elite015.z=-200
elite016.z=-200
elite017.z=-200
elite018.z=-200
elite019.z=-200
elite020.z=-200
elite021.z=-200
elite022.z=-200
elite023.z=-200
elite024.z=-200
elite025.z=-200
elite026.z=-200
elite027.z=-200
elite028.z=-200
elite029.z=-200
elite030.z=-200
elite031.z=-200
elite032.z=-200
elite033.z=-200
elite034.z=-200
elite035.z=-200
jackal001.z=-200
jackal002.z=-200
jackal003.z=-200
jackal004.z=-200
jackal005.z=-200
jackal006.z=-200
jackal007.z=-200
jackal008.z=-200
jackal009.z=-200
jackal010.z=-200
jackal011.z=-200
jackal012.z=-200
jackal013.z=-200
jackal014.z=-200
jackal015.z=-200
jackal016.z=-200
jackal017.z=-200
jackal018.z=-200
jackal019.z=-200
jackal020.z=-200
jackal021.z=-200
jackal022.z=-200
jackal023.z=-200
jackal024.z=-200
jackal025.z=-200
jackal026.z=-200
jackal027.z=-200
jackal028.z=-200
jackal029.z=-200
jackal030.z=-200
jackal031.z=-200
jackal032.z=-200
jackal033.z=-200
jackal034.z=-200
jackal035.z=-200
floode001.z=-200
floode002.z=-200
floode003.z=-200
floode004.z=-200
floode005.z=-200
floode006.z=-200
floode007.z=-200
floode008.z=-200
floode009.z=-200
floode010.z=-200
floode011.z=-200
floode012.z=-200
floode013.z=-200
floode014.z=-200
floode015.z=-200
floode016.z=-200
floode017.z=-200
floode018.z=-200
floode019.z=-200
floode020.z=-200
floode021.z=-200
floode022.z=-200
floode023.z=-200
floode024.z=-200
floode025.z=-200
floode026.z=-200
floode027.z=-200
floode028.z=-200
floode029.z=-200
floode030.z=-200
floode031.z=-200
floode032.z=-200
floode033.z=-200
floode034.z=-200
floode035.z=-200
floode036.z=-200
floode037.z=-200
floode038.z=-200
floode039.z=-200
floode040.z=-200
floodc001.z=-200
floodc002.z=-200
floodc003.z=-200
floodc004.z=-200
floodc005.z=-200
floodc006.z=-200
floodc007.z=-200
floodc008.z=-200
floodc009.z=-200
floodc010.z=-200
floodc011.z=-200
floodc012.z=-200
floodc013.z=-200
floodc014.z=-200
floodc015.z=-200
floodc016.z=-200
floodc017.z=-200
floodc018.z=-200
floodc019.z=-200
floodc020.z=-200
floodc021.z=-200
floodc022.z=-200
floodc023.z=-200
floodc024.z=-200
floodc025.z=-200
floodc026.z=-200
floodc027.z=-200
floodc028.z=-200
floodc029.z=-200
floodc030.z=-200
floodc031.z=-200
floodc032.z=-200
floodc033.z=-200
floodc034.z=-200
floodc035.z=-200
floodc036.z=-200
floodc037.z=-200
floodc038.z=-200
floodc039.z=-200
floodc040.z=-200
logo001.z=-200
logo002.z=-200
logo003.z=-200
logo004.z=-200
logo005.z=-200
skull001.z=-200
skull002.z=-200
skull003.z=-200
skull004.z=-200
skull005.z=-200
grenade001.z=-200
grenade002.z=-200
grenade003.z=-200
grenade004.z=-200
grenade005.z=-200
end sub

Sub tim001_Hit()
if mode1TimerCount > 90 then exit sub
mode1TimerCount = mode1TimerCount + 2
end sub

Sub tim002_Hit()
if mode1TimerCount > 90 then exit sub
mode1TimerCount = mode1TimerCount + 2
end sub

Sub tim003_Hit()
if mode1TimerCount > 90 then exit sub
mode1TimerCount = mode1TimerCount + 2
end sub

Sub tim004_Hit()
if mode1TimerCount > 90 then exit sub
mode1TimerCount = mode1TimerCount + 2
end sub

Sub tim005_Hit()
if mode1TimerCount > 90 then exit sub
mode1TimerCount = mode1TimerCount + 2
end sub

Sub tim006_Hit()
if mode1TimerCount > 90 then exit sub 
mode1TimerCount = mode1TimerCount + 2
end sub

'**************
'*********bonus games*********
'**************

Dim BonusyAward
dim bonusslot
Dim BallInHole8
Dim rball

BonusyAward = Array("bon1", "bon2", "bon3", "bon4", "bon5")

Sub bonusSlotmachine() ' uses the HolePos variable
    Dim i
	changelights3
	flashplaat1.ImageA = "flsbonus"
	DMDFlush
For i = 0 to 4
    DMD "", "", BonusyAward(i), eNone, eNone, eNone, 50, False, "fx_spinner"
Next
    'DOF 142, DOFPulse
    vpmtimer.AddTimer 2200, "GiveBonusyAward '"
End Sub

Sub GiveBonusyAward()
    Dim bonusslot
    DMDFlush
    bonusslot = INT(RND * 5)
    DMD "", "", BonusyAward(bonusslot),  eNone, eNone, eBlinkFast, 1000, True, "brs"

    Select Case bonusslot
case 0: StartBonus1
case 1: StartBonus2
case 2: StartBonus3
case 3: StartBonus4
case 4: StartBonus5
    End Select
End Sub

sub Kicker010_hit()
	if bonustime = 1 then
	continuebonus
	exit Sub
	end if
	'StopSong
	BallInHole8 = BallInHole8 + 1
    Set rBall = ActiveBall:Me.TimerEnabled = 1
	Playsound "fx_ballrampdrop"
    Me.Enabled = 0
		duuus
	HelmDir = 1
	helmupTimer.enabled = 1
	vpmtimer.addtimer 2100, "SuperVukAddBall8 '" 
end sub

sub continuebonus
Playsound "fx_popper"
Kicker010.Kick 8, 28, 1
end sub

Sub SuperVukAddBall8()
	'UpdateMusicNow
	If BallInHole8> 0 Then
        BallInHole8 = BallInHole8 - 1
	Kicker005.CreateSizedball BallSize / 2
	LowerFlippersActive = False
	disablebonusgames
	flashplaat1.ImageA = "flshalopin"
	check4bonus
	Playsound "fx_popper"
	Kicker005.Kick 190, 7, 1

 vpmtimer.addtimer 1000, "SuperVukAddBall8 '" 
end If
End Sub

Sub Kicker010_Timer
    Do While rBall.Z > 0
        rBall.Z = rBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
	Me.Enabled = 1
End Sub

sub StartBonus1
Ramp003.image = "bonuss1"
bringhelmdown
StopSong
PlaySong "bonusmusic" 
bonus1Timer.enabled = 1
vpmtimer.addtimer 2100, "kickbonus '"
UpdateEnemiesLeft
end sub

sub StartBonus2 
Ramp003.image = "bonuss2"
bringhelmdown
StopSong
PlaySong "bonusmusic" 
bonus2Timer.enabled = 1
vpmtimer.addtimer 2100, "kickbonus '"
UpdateEnemiesLeft
end sub

sub StartBonus3
 Ramp003.image = "bonuss3"
bringhelmdown
StopSong
PlaySong "bonusmusic" 
bonus3Timer.enabled = 1
vpmtimer.addtimer 2100, "kickbonus '"
UpdateEnemiesLeft
end sub

sub StartBonus4
Ramp003.image = "bonuss4"
bringhelmdown
StopSong
PlaySong "bonusmusic" 
bonus4Timer.enabled = 1
vpmtimer.addtimer 2100, "kickbonus '"
UpdateEnemiesLeft
end sub

sub StartBonus5
 Ramp003.image = "bonuss5"
bringhelmdown
StopSong
PlaySong "bonusmusic" 
bonus5Timer.enabled = 1
vpmtimer.addtimer 2100, "kickbonus '"
UpdateEnemiesLeft
end sub

Sub kickbonus
	If BallInHole3> 0 Then
        BallInHole3 = BallInHole3 - 1
	Kicker010.CreateSizedball BallSize / 2
	LowerFlippersActive = True
	Playsound "fx_popper"
	bonustime = 1
	Kicker010.Kick 8, 28, 1
mode2TimerCount = 30
mode2timer.Enabled = 1
 vpmtimer.addtimer 1000, "kickbonus '"
end if
end sub

sub bringhelmdown
	duuus
	HelmDir = 1
	helmdownTimer.enabled = 1
end sub

sub check4bonus
if (li080.state = 1)and (li079.state = 1)and (li078.state = 1)and (li081.state = 1) and (li082.state = 1)Then
'DMD "", "", "bonusjackpot", eNone, eNone, eNone, 500, True, ""
addscore 500000
li080.state = 0
li079.state = 0
li078.state = 0
li081.state = 0
li082.state = 0
end if
end sub

Sub disablebonusgames
bt1.Enabled = 0
bt2.Enabled = 0
ct1.Enabled = 0
ct2.Enabled = 0
dt1.Enabled = 0
dt2.Enabled = 0
et1.Enabled = 0
et2.Enabled = 0
ft1.Enabled = 0
ft2.Enabled = 0
bonus1Timer.enabled = 0
bonus2Timer.enabled = 0
bonus3Timer.enabled = 0
bonus4Timer.enabled = 0
bonus5Timer.enabled = 0
bonustime = 0
bonus1hits = 0
bonus2hits = 0
bonus3hits = 0
bonus4hits = 0
bonus5hits = 0
b1grund001.Z=-200
b1grund002.Z=-200
b2elite001.Z=-200
b2elite002.Z=-200
b3jacky001.Z=-200
b3jacky002.Z=-200
b4floodelite001.Z=-200
b4floodelite002.Z=-200
b5floodcarrier001.Z=-200
b5floodcarrier002.Z=-200
end sub

'************************grund bonusgame**************************************
Sub bonus1Timer_Timer
		countr50 = countr50 + 1 : If Countr50 > 2 then Countr50 = 1 : end If
			select case countr50
				case 1 : bt1.Enabled = 1:bt2.Enabled = 0:b1grund001.Z=-71:b1grund002.Z=-200
				case 2 : bt1.Enabled = 0:bt2.Enabled = 1:b1grund001.Z=-200:b1grund002.Z=-71
			end Select
end Sub

sub bt1_hit
enemylft = enemylft + 1
UpdateEnemiesLeft
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
bonus1hits = bonus1hits + 1
checkbonus1complete
end sub

sub bt2_hit
enemylft = enemylft + 1
UpdateEnemiesLeft
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
bonus1hits = bonus1hits + 1
checkbonus1complete
end sub

sub checkbonus1complete
if bonus1hits = 5 or bonus1hits > 5 Then
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
flashplaat1.ImageA = "flshalopin"
playsound "brc"
'disablebonusgames
addscore 50000
enemylft = 0
dmdbonus1complete
bonusyscorechecker = bonusyscorechecker + 50000
Stopmode2
li080.state = 1
end if
end sub

Sub dmdbonus1complete
DMD "", "", "50k", eNone, eNone, eNone, 1000, True, ""
end sub


'************************elite bonusgame**************************************
Sub bonus2Timer_Timer
		countr51 = countr51 + 1 : If Countr51 > 2 then Countr51 = 1 : end If
			select case countr51
				case 1 : ct1.Enabled = 1:ct2.Enabled = 0:b2elite001.Z=-71:b2elite002.Z=-200
				case 2 : ct1.Enabled = 0:ct2.Enabled = 1:b2elite001.Z=-200:b2elite002.Z=-71
			end Select
end Sub

sub ct1_hit
enemylft = enemylft + 1
UpdateEnemiesLeft
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "elitehurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
bonus2hits = bonus2hits + 1
checkbonus2complete
end sub

sub ct2_hit
enemylft = enemylft + 1
UpdateEnemiesLeft
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "elitehurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
bonus2hits = bonus2hits + 1
checkbonus1complete
end sub

sub checkbonus2complete
if bonus2hits = 5 or bonus2hits > 5 Then
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
flashplaat1.ImageA = "flshalopin"
playsound "brc"
'disablebonusgames
addscore 50000
enemylft = 0
dmdbonus2complete
bonusyscorechecker = bonusyscorechecker + 50000
Stopmode2
li079.state = 1
end if
end sub

Sub dmdbonus2complete
DMD "", "", "50k", eNone, eNone, eNone, 1000, True, ""
end sub

'************************jackal bonusgame**************************************
Sub bonus3Timer_Timer
		countr52 = countr52 + 1 : If Countr52 > 2 then Countr52 = 1 : end If
			select case countr52
				case 1 : dt1.Enabled = 1:dt2.Enabled = 0:b3jacky001.Z=-71:b3jacky002.Z=-200
				case 2 : dt1.Enabled = 0:dt2.Enabled = 1:b3jacky001.Z=-200:b3jacky002.Z=-71
			end Select
end Sub

sub dt1_hit
enemylft = enemylft + 1
UpdateEnemiesLeft
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "jackalhurt"
bonus3hits = bonus3hits + 1
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
checkbonus3complete
end sub

sub dt2_hit
enemylft = enemylft + 1
UpdateEnemiesLeft
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "jackalhurt"
bonus3hits = bonus3hits + 1
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
checkbonus1complete
end sub

sub checkbonus3complete
if bonus3hits = 5 or bonus3hits > 5 Then
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
flashplaat1.ImageA = "flshalopin"
playsound "brc"
'disablebonusgames
addscore 50000
dmdbonus3complete
enemylft = 0
bonusyscorechecker = bonusyscorechecker + 50000
Stopmode2
li078.state = 1
end if
end sub

Sub dmdbonus3complete
DMD "", "", "50k", eNone, eNone, eNone, 1000, True, ""
end sub

'************************flood elite bonusgame**************************************
Sub bonus4Timer_Timer
		countr53 = countr53 + 1 : If Countr53 > 2 then Countr53 = 1 : end If
			select case countr53
				case 1 : et1.Enabled = 1:et2.Enabled = 0:b4floodelite001.Z=-71:b4floodelite002.Z=-200
				case 2 : et1.Enabled = 0:et2.Enabled = 1:b4floodelite001.Z=-200:b4floodelite002.Z=-71
			end Select
end Sub

sub et1_hit
enemylft = enemylft + 1
UpdateEnemiesLeft
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "floodehurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
bonus4hits = bonus4hits + 1
checkbonus4complete
end sub

sub et2_hit
enemylft = enemylft + 1
UpdateEnemiesLeft
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "floodehurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
bonus4hits = bonus4hits + 1
checkbonus4complete
end sub

sub checkbonus4complete
if bonus4hits = 5 or bonus4hits > 5 Then
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
flashplaat1.ImageA = "flshalopin"
playsound "brc"
'disablebonusgames
addscore 50000
enemylft = 0
dmdbonus4complete
bonusyscorechecker = bonusyscorechecker + 50000
Stopmode2
li081.state = 1
end if
end sub

Sub dmdbonus4complete
DMD "", "", "50k", eNone, eNone, eNone, 1000, True, ""
end sub

'************************flood carrier bonusgame**************************************
Sub bonus5Timer_Timer
		countr54 = countr54 + 1 : If Countr54 > 2 then Countr54 = 1 : end If
			select case countr54
				case 1 : ft1.Enabled = 1:ft2.Enabled = 0:b5floodcarrier001.Z=-71:b5floodcarrier002.Z=-200
				case 2 : ft1.Enabled = 0:ft2.Enabled = 1:b5floodcarrier001.Z=-200:b5floodcarrier002.Z=-71
			end Select
end Sub

sub ft1_hit
enemylft = enemylft + 1
UpdateEnemiesLeft
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "floodchurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
bonus5hits = bonus5hits + 1
checkbonus5complete
end sub

sub ft2_hit
enemylft = enemylft + 1
UpdateEnemiesLeft
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "floodchurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
bonus5hits = bonus5hits + 1
checkbonus5complete
end sub

sub checkbonus5complete
if bonus5hits = 5 or bonus5hits > 5 Then
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
flashplaat1.ImageA = "flshalopin"
playsound "brc"
'disablebonusgames
addscore 50000
enemylft = 0
dmdbonus5complete
bonusyscorechecker = bonusyscorechecker + 50000
Stopmode2
li082.state = 1
end if
end sub

Sub dmdbonus5complete
DMD "", "", "50k", eNone, eNone, eNone, 1000, True, ""
end sub

'**************************************************************************************
'**************
'*********Extraball modus*********
'**************
Sub timerquotemulti_Timer
PlayQuoteGrundy
updatemultigrundy
end sub

Sub updatemultigrundy
select case countr41
				case 1 : mbgrund001.z=0:tmball001.enabled=1:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 2 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=0:tmball002.enabled=1:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 3 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=0:tmball003.enabled=1:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 4 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=0:tmball004.enabled=1:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 5 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=0:tmball005.enabled=1:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 6 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=0:tmball006.enabled=1:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 7 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=0:tmball007.enabled=1:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 8 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=0:tmball008.enabled=1:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 9 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=0:tmball009.enabled=1:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 10 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=0:tmball010.enabled=1:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 11 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=0:tmball011.enabled=1:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 12 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=0:tmball012.enabled=1:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 13 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=1:mbgrund013.z=0:tmball013.enabled=1:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 14 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=0:tmball014.enabled=1:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 15 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=0:tmball015.enabled=1:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 16 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=0:tmball016.enabled=1:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 17 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=0:tmball017.enabled=1:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 18 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=0:tmball018.enabled=1:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=-200:tmball020.enabled=0
				case 19 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=0:tmball019.enabled=1:mbgrund020.z=-200:tmball020.enabled=0
				case 20 : mbgrund001.z=-200:tmball001.enabled=0:mbgrund002.z=-200:tmball002.enabled=0:mbgrund003.z=-200:tmball003.enabled=0:mbgrund004.z=-200:tmball004.enabled=0:mbgrund005.z=-200:tmball005.enabled=0:mbgrund006.z=-200:tmball006.enabled=0:mbgrund007.z=-200:tmball007.enabled=0:mbgrund008.z=-200:tmball008.enabled=0:mbgrund009.z=-200:tmball009.enabled=0:mbgrund010.z=-200:tmball010.enabled=0:mbgrund011.z=-200:tmball011.enabled=0:mbgrund012.z=-200:tmball012.enabled=0:mbgrund013.z=-200:tmball013.enabled=0:mbgrund014.z=-200:tmball014.enabled=0:mbgrund015.z=-200:tmball015.enabled=0:mbgrund016.z=-200:tmball016.enabled=0:mbgrund017.z=-200:tmball017.enabled=0:mbgrund018.z=-200:tmball018.enabled=0:mbgrund019.z=-200:tmball019.enabled=0:mbgrund020.z=0:tmball020.enabled=1
			end Select
End Sub


sub tmball001_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball002_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball003_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball004_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball005_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball006_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball007_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball008_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball009_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball010_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball011_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball012_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball013_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball014_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball015_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball016_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball017_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball018_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball019_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub tmball020_hit
    If li001.State = 1 then
        AddScore 10000
	bonusyscorechecker = bonusyscorechecker + 10000
    end if
    If li002.State = 1 then
        AddScore 20000
	bonusyscorechecker = bonusyscorechecker + 20000
    end if
kildys = kildys + 1
Playsound "grundhurt"
addscore 10000
bonusyscorechecker = bonusyscorechecker + 10000
countr41 = countr41 + 1
updatemultigrundy
end sub

sub stopmultigrundy
mbgrund001.z=-200
mbgrund002.z=-200
mbgrund003.z=-200
mbgrund004.z=-200
mbgrund005.z=-200
mbgrund006.z=-200
mbgrund007.z=-200
mbgrund008.z=-200
mbgrund009.z=-200
mbgrund010.z=-200
mbgrund011.z=-200
mbgrund012.z=-200
mbgrund013.z=-200
mbgrund014.z=-200
mbgrund015.z=-200
mbgrund016.z=-200
mbgrund017.z=-200
mbgrund018.z=-200
mbgrund019.z=-200
mbgrund020.z=-200
tmball001.enabled=0
tmball002.enabled=0
tmball003.enabled=0
tmball004.enabled=0
tmball005.enabled=0
tmball006.enabled=0
tmball007.enabled=0
tmball008.enabled=0
tmball009.enabled=0
tmball010.enabled=0
tmball011.enabled=0
tmball012.enabled=0
tmball013.enabled=0
tmball014.enabled=0
tmball015.enabled=0
tmball016.enabled=0
tmball017.enabled=0
tmball018.enabled=0
tmball019.enabled=0
tmball020.enabled=0
countr41 = 1
end sub 

'**************
'*********covenantship movement*********
'**************

Dim shipDir
shipDir = 50 'this is both the direction, if + goes up, if - goes down, and also the speed

Sub schipgo_Timer
    covenantship.TransY = covenantship.TransY - shipDir
    If covenantship.TransY < -300 Then shipDir = -10 'goes down
    If covenantship.TransY > -1 Then schipgo.Enabled = 0
End Sub

'**************
'***************** covenant wave *****************
'**************

Dim BallInHole2
Dim eBall

sub shipshakertimer_Timer
BigUfoUpdate
end sub

Sub Kicker003_hit()
If Tilted Then Exit Sub
If bMultiBallMode = True Then
mbalykick
Exit Sub
end if
BallInHole2 = BallInHole2 + 1
Set eBall = ActiveBall:Me.TimerEnabled = 1
Me.Enabled = 0
if (waveattack=0)Then
'wave1 starts
AddScore 5000
bonusyscorechecker = bonusyscorechecker + 5000
resettarget12
covenantincomdmd
	flashplaat1.ImageA = "flswaveattack"
vpmTimer.AddTimer 500, "enableshipWave'"
vpmTimer.AddTimer 3600, "wavestarty'"
vpmTimer.AddTimer 3500, "shiptimerupdate'"
vpmTimer.AddTimer 3500, "keysturny'"
li053.state = 2
waveattack = waveattack + 1
Exit Sub
end if
if (waveattack=1)Then
flashplaat1.ImageA = "flshalopin"
addscore 100000
bonusyscorechecker = bonusyscorechecker + 100000
keysback
Playsound "keystakeout3"
destroyshipdmd
li053.state = 1
waveattack = waveattack + 1
wavy = wavy + 1
LightSeq003.Play SeqCircleOutOn, 25, 1
vpmTimer.AddTimer 2500, "shootcanon'"
vpmTimer.AddTimer 5400, "bigexplosion'"
vpmTimer.AddTimer 5400, "BigUfoShake'"
vpmTimer.AddTimer 6800, "BigUfoShake'"
vpmTimer.AddTimer 8150, "BigUfoShake'"
vpmTimer.AddTimer 9650, "shiptimerupdate2'"
vpmTimer.AddTimer 9650, "dmd100k'"
Exit Sub
end if
if (waveattack=2)Then
'wave2 starts
AddScore 5000
bonusyscorechecker = bonusyscorechecker + 5000
resettarget12
covenantincomdmd
	flashplaat1.ImageA = "flswaveattack"
vpmTimer.AddTimer 500, "enableshipWave'"
vpmTimer.AddTimer 3600, "wavestarty'"
vpmTimer.AddTimer 3500, "shiptimerupdate'"
li054.state = 2
waveattack = waveattack + 1
Exit Sub
end if
if (waveattack=3)Then
AddScore 10000
bonusyscorechecker = bonusyscorechecker + 10000
KeysturnbackTimer.enabled = true
BigUfoShake
explosion2.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp1'"
DMD "", "", "dmd1t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=4)Then
flashplaat1.ImageA = "flshalopin"
addscore 200000
bonusyscorechecker = bonusyscorechecker + 200000
keysback
Playsound "keystakeout3"
destroyshipdmd
li054.state = 1
waveattack = waveattack + 1
wavy = wavy + 1
LightSeq003.Play SeqCircleOutOn, 25, 2
vpmTimer.AddTimer 2500, "shootcanon'"
vpmTimer.AddTimer 5400, "bigexplosion'"
vpmTimer.AddTimer 5400, "BigUfoShake'"
vpmTimer.AddTimer 6800, "BigUfoShake'"
vpmTimer.AddTimer 8150, "BigUfoShake'"
vpmTimer.AddTimer 9650, "shiptimerupdate2'"
vpmTimer.AddTimer 9650, "dmd200k'"
Exit Sub
end if
if (waveattack=5)Then
'wave3 starts
AddScore 5000
bonusyscorechecker = bonusyscorechecker + 5000
resettarget12
covenantincomdmd
	flashplaat1.ImageA = "flswaveattack"
vpmTimer.AddTimer 500, "enableshipWave'"
vpmTimer.AddTimer 3600, "wavestarty'"
vpmTimer.AddTimer 3500, "shiptimerupdate'"
li055.state = 2
waveattack = waveattack + 1
Exit Sub
end if
if (waveattack=6)Then
AddScore 10000
bonusyscorechecker = bonusyscorechecker + 10000
BigUfoShake
explosion2.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp1'"
DMD "", "", "dmd2t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=7)Then
AddScore 15000
bonusyscorechecker = bonusyscorechecker + 15000
KeysturnbackTimer.enabled = true
BigUfoShake
explosion3.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp2'"
DMD "", "", "dmd1t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=8)Then
flashplaat1.ImageA = "flshalopin"
addscore 300000
bonusyscorechecker = bonusyscorechecker + 300000
keysback
Playsound "keystakeout3"
destroyshipdmd
li055.state = 1
waveattack = waveattack + 1
wavy = wavy + 1
LightSeq003.Play SeqCircleOutOn, 25, 3
vpmTimer.AddTimer 2500, "shootcanon'"
vpmTimer.AddTimer 5400, "bigexplosion'"
vpmTimer.AddTimer 5400, "BigUfoShake'"
vpmTimer.AddTimer 6800, "BigUfoShake'"
vpmTimer.AddTimer 8150, "BigUfoShake'"
vpmTimer.AddTimer 9650, "shiptimerupdate2'"
vpmTimer.AddTimer 9650, "dmd300k'"
Exit Sub
end if
if (waveattack=9)Then
'wave4 starts
AddScore 5000
bonusyscorechecker = bonusyscorechecker + 5000
resettarget12
covenantincomdmd
	flashplaat1.ImageA = "flswaveattack"
vpmTimer.AddTimer 500, "enableshipWave'"
vpmTimer.AddTimer 3600, "wavestarty'"
vpmTimer.AddTimer 3500, "shiptimerupdate'"
li056.state = 2
waveattack = waveattack + 1
Exit Sub
end if
if (waveattack=10)Then
AddScore 10000
bonusyscorechecker = bonusyscorechecker + 10000
BigUfoShake
explosion2.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp1'"
DMD "", "", "dmd3t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=11)Then
AddScore 15000
bonusyscorechecker = bonusyscorechecker + 15000
BigUfoShake
explosion3.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp2'"
DMD "", "", "dmd2t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=12)Then
AddScore 20000
bonusyscorechecker = bonusyscorechecker + 20000
KeysturnbackTimer.enabled = true
BigUfoShake
explosion4.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp3'"
DMD "", "", "dmd1t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=13)Then
flashplaat1.ImageA = "flshalopin"
addscore 400000
bonusyscorechecker = bonusyscorechecker + 400000
keysback
Playsound "keystakeout3"
destroyshipdmd
li056.state = 1
waveattack = waveattack + 1
wavy = wavy + 1
LightSeq003.Play SeqCircleOutOn, 25, 4
vpmTimer.AddTimer 2500, "shootcanon'"
vpmTimer.AddTimer 5400, "bigexplosion'"
vpmTimer.AddTimer 5400, "BigUfoShake'"
vpmTimer.AddTimer 6800, "BigUfoShake'"
vpmTimer.AddTimer 8150, "BigUfoShake'"
vpmTimer.AddTimer 9650, "shiptimerupdate2'"
vpmTimer.AddTimer 9650, "dmd400k'"
Exit Sub
end if
if (waveattack=14)Then
'wave5 starts
AddScore 5000
bonusyscorechecker = bonusyscorechecker + 5000
resettarget12
covenantincomdmd
	flashplaat1.ImageA = "flswaveattack"
vpmTimer.AddTimer 500, "enableshipWave'"
vpmTimer.AddTimer 3600, "wavestarty'"
vpmTimer.AddTimer 3500, "shiptimerupdate'"
li057.state = 2
waveattack = waveattack + 1
Exit Sub
end if
if (waveattack=15)Then
AddScore 10000
bonusyscorechecker = bonusyscorechecker + 10000
BigUfoShake
explosion2.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp1'"
DMD "", "", "dmd4t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=16)Then
AddScore 15000
bonusyscorechecker = bonusyscorechecker + 15000
BigUfoShake
explosion3.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp2'"
DMD "", "", "dmd3t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=17)Then
AddScore 20000
bonusyscorechecker = bonusyscorechecker + 20000
BigUfoShake
explosion4.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp3'"
DMD "", "", "dmd2t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=18)Then
AddScore 25000
bonusyscorechecker = bonusyscorechecker + 25000
KeysturnbackTimer.enabled = true
BigUfoShake
explosion5.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp4'"
DMD "", "", "dmd1t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=19)Then
flashplaat1.ImageA = "flshalopin"
addscore 500000
bonusyscorechecker = bonusyscorechecker + 500000
keysback
Playsound "keystakeout3"
destroyshipdmd
li057.state = 1
waveattack = waveattack + 1
wavy = wavy + 1
LightSeq003.Play SeqCircleOutOn, 25, 5
vpmTimer.AddTimer 2500, "shootcanon'"
vpmTimer.AddTimer 5400, "bigexplosion'"
vpmTimer.AddTimer 5400, "BigUfoShake'"
vpmTimer.AddTimer 6800, "BigUfoShake'"
vpmTimer.AddTimer 8150, "BigUfoShake'"
vpmTimer.AddTimer 9650, "shiptimerupdate2'"
vpmTimer.AddTimer 9650, "dmd500k'"
Exit Sub
end if
if (waveattack=20)Then
'wave6 starts
AddScore 5000
bonusyscorechecker = bonusyscorechecker + 5000
resettarget12
covenantincomdmd
	flashplaat1.ImageA = "flswaveattack"
vpmTimer.AddTimer 500, "enableshipWave'"
vpmTimer.AddTimer 3600, "wavestarty'"
vpmTimer.AddTimer 3500, "shiptimerupdate'"
li058.state = 2
waveattack = waveattack + 1
Exit Sub
end if
if (waveattack=21)Then
AddScore 10000
bonusyscorechecker = bonusyscorechecker + 10000
BigUfoShake
explosion2.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp1'"
DMD "", "", "dmd5t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=22)Then
AddScore 15000
bonusyscorechecker = bonusyscorechecker + 15000
BigUfoShake
explosion3.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp2'"
DMD "", "", "dmd4t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=23)Then
AddScore 20000
bonusyscorechecker = bonusyscorechecker + 20000
BigUfoShake
explosion4.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp3'"
DMD "", "", "dmd3t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=24)Then
AddScore 25000
bonusyscorechecker = bonusyscorechecker + 25000
BigUfoShake
explosion5.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp4'"
DMD "", "", "dmd2t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=25)Then
AddScore 30000
bonusyscorechecker = bonusyscorechecker + 30000
KeysturnbackTimer.enabled = true
BigUfoShake
explosion6.visible = True
PlaySound "hitship"
waveattack = waveattack + 1
vpmTimer.AddTimer 1500, "waveexp5'"
DMD "", "", "dmd1t", eNone, eNone, eNone, 1000, True, ""
Exit Sub
end if
if (waveattack=26)Then
flashplaat1.ImageA = "flshalopin"
addscore 600000
bonusyscorechecker = bonusyscorechecker + 600000
keysback
Playsound "keystakeout3"
destroyshipdmd
li058.state = 1
waveattack = 0
wavy = wavy + 1
LightSeq003.Play SeqCircleOutOn, 25, 6
vpmTimer.AddTimer 2500, "shootcanon'"
vpmTimer.AddTimer 5400, "bigexplosion'"
vpmTimer.AddTimer 5400, "BigUfoShake'"
vpmTimer.AddTimer 6800, "BigUfoShake'"
vpmTimer.AddTimer 8150, "BigUfoShake'"
vpmTimer.AddTimer 9650, "resetwavers'"
vpmTimer.AddTimer 9650, "shiptimerupdate2'"
vpmTimer.AddTimer 9650, "dmd600k'"
Exit Sub
end if
end sub

sub mbalykick
Playsound "fx_popper"
Kicker003.Kick 190, 7, 14
end sub

Sub resetwavers
li058.state = 0
li057.state = 0
li056.state = 0
li055.state = 0
li054.state = 0
li053.state = 0
end sub

sub dmd100k
	DMD "", "", "100k", eNone, eNone, eNone, 500, True, ""
end sub

sub dmd200k
	DMD "", "", "200k", eNone, eNone, eNone, 500, True, ""
end sub

sub dmd300k
	DMD "", "", "300k", eNone, eNone, eNone, 500, True, ""
end sub

sub dmd400k
	DMD "", "", "400k", eNone, eNone, eNone, 500, True, ""
end sub

sub dmd500k
	DMD "", "", "500k", eNone, eNone, eNone, 500, True, ""
end sub

sub dmd600k
	DMD "", "", "600k", eNone, eNone, eNone, 500, True, ""
end sub

Sub SuperVukAddBall2()
	If BallInHole2> 0 Then
        BallInHole2 = BallInHole2 - 1
	Kicker002.CreateSizedball BallSize / 2
	'ChangeBallImage
	'kickteleport
	Playsound "fx_popper"
	Kicker002.Kick 180, 1, 1
	Kicker002.enabled = false
 vpmtimer.addtimer 1000, "SuperVukAddBall2 '" 
end If
End Sub

Sub Kicker003_Timer
    Do While eBall.Z > 0
        eBall.Z = eBall.Z -5
        Exit Sub
    Loop
    Me.DestroyBall
    Me.TimerEnabled = 0
	Me.Enabled = 1
End Sub

sub enableshipWave
Playsound "dropshipsound7"
keysc.z = 100
KeysturnTimer.enabled = true
shipDir = 50
schipgo.Enabled = 1
vpmTimer.AddTimer 1250, "callcovenant'"
end sub

sub shiptimerupdate
shipshakertimer.enabled = true
end sub

sub shiptimerupdate2
stopidlchiefke
chchLtimer.enabled = 1
chchRtimer.enabled = 1
covenantship.transx = 0
covenantship.transy = 0
covenantship.roty = 0
raket.Visible = False
raket.z = 113
raket.TransX = 10
raket.TransY = 10
raket.TransZ = 10
raket.TransY = 0
shipshakertimer.enabled = false
resettarget12
bringbankback
Playsound "Waveend"
explosion1.visible = false
explosion2.visible = false
explosion3.visible = false
explosion4.visible = false
explosion5.visible = false
explosion6.visible = false
covenantship.transy = -601
vpmTimer.AddTimer 500, "SuperVukAddBall2 '" 
end sub

sub covenantincomdmd
DMD "", "", "dropshipp003", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp004", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp005", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp006", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp007", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp008", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp009", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp010", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp011", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp012", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp013", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp014", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp015", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp016", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp017", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp018", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp019", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp020", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp021", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp022", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp023", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp024", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp025", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp026", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp027", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp028", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp029", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp030", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp031", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dropshipp032", eNone, eNone, eNone, 100, True, ""
end sub

sub destroyshipdmd
DMD "", "", "tankshoot060", eNone, eNone, eNone, 100, True, ""
DMD "", "", "tankshoot061", eNone, eNone, eNone, 100, True, ""
DMD "", "", "tankshoot062", eNone, eNone, eNone, 100, True, ""
DMD "", "", "tankshoot063", eNone, eNone, eNone, 100, True, ""
DMD "", "", "tankshoot064", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot065", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot066", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot067", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot068", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot069", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot070", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot071", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot072", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot073", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot074", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot075", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot076", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot077", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot078", eNone, eNone, eNone, 200, True, ""
DMD "", "", "tankshoot079", eNone, eNone, eNone, 300, True, ""
DMD "", "", "tankshoot080", eNone, eNone, eNone, 300, True, ""
DMD "", "", "tankshoot081", eNone, eNone, eNone, 300, True, ""
DMD "", "", "tankshoot082", eNone, eNone, eNone, 300, True, ""
DMD "", "", "explus00", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus01", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus02", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus03", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus04", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus05", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus06", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus07", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus08", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus09", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus10", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus11", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus12", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus13", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus14", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus15", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus16", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus17", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus18", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus19", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus20", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus21", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus22", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus23", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus24", eNone, eNone, eNone, 100, True, ""
DMD "", "", "explus25", eNone, eNone, eNone, 100, True, ""
DMD "", "", "dmddestroyed", eNone, eNone, eNone, 2000, True, ""
end sub

sub callcovenant
PlayQuoteCovenant
'Playsound "covenant"
end sub

sub wavestarty
Playsound "Wavestart"
SuperVukAddBall2
end sub

sub waveexp1
Playsound "keys_1"
SuperVukAddBall2
end sub

sub waveexp2
Playsound "keys_2"
SuperVukAddBall2
end sub

sub waveexp3
Playsound "keys_3"
SuperVukAddBall2
end sub

sub waveexp4
Playsound "keys_4"
SuperVukAddBall2
end sub

sub waveexp5
Playsound "keys_5"
SuperVukAddBall2
end sub

sub bigexplosion
explosion1.visible = true
end sub

sub shootcanon
tanky.TransX = -20
vpmTimer.AddTimer 500, "tanknormal '" 
end sub

sub keysback
keysc.z = -400
keysc.RotY = 230
end sub

sub keysturny
KeysturnbackTimer.enabled = true
end sub

sub tanknormal
tanky.TransX = 0
raket.Visible = true
RaketupTimer.enabled = 1
RaketmoveTimer.enabled = 1
end sub

sub bringbankback
sw9.IsDropped=false
sw10.IsDropped=false
sw11.IsDropped=false
sw001.IsDropped=false
Bankup.Enabled= True
end sub

'***************** movement timers keys and raket *****************
Dim Raketup, raketmove, keysturn, keysturnback
Raketup = 3 'this is both the direction, if + goes up, if - goes down, and also the speed
raketmove = 15
keysturn = 5
keysturnback = 5

Sub RaketupTimer_Timer
    raket.z = raket.z + Raketup
    If raket.z < 175 Then Raketup = + 3 'goes up
    If raket.z > 175 Then RaketupTimer.Enabled = 0
End Sub

Sub RaketmoveTimer_Timer
    raket.TransY = raket.TransY + raketmove
    If raket.TransY < 675 Then raketmove = + 15 'move
    If raket.TransY > 675 Then RaketmoveTimer.Enabled = 0
End Sub

Sub KeysturnTimer_Timer
    keysc.RotY = keysc.RotY - keysturn
    If keysc.RotY > 229  Then keysturn = -5 'move
    If keysc.RotY < 115 Then KeysturnTimer.Enabled = 0
End Sub

Sub KeysturnbackTimer_Timer
    keysc.RotY = keysc.RotY + keysturnback
    If keysc.RotY < 116  Then keysturnback = + 5 'move
    If keysc.RotY > 229 Then KeysturnbackTimer.Enabled = 0
End Sub

'************************************************
'**************shake covenantship*****************
'************************************************

BigUfoInit

Sub BigUfoInit
    Set cBall = ckicker.createball
    ckicker.Kick 0, 0
End Sub

Sub BigUfoUpdate
    Dim a, b
    a = ckicker.y - cball.y
    b = cball.x - ckicker.x
    covenantship.rotx = - a
    covenantship.transy = a
    covenantship.roty = b
End Sub

Sub BigUfoShake
    cball.velx = -4 + 4 * 1'RND(1)
    cball.vely = 8 + 4 * 1'RND(1)
End Sub

Sub BigUfoShake2
    cball.velx = -2 + 2 * 1'RND(1)
    cball.vely = -10 + 2 * 1'RND(1)
End Sub

Sub BigUfoShake3
    cball.velx = -10 + 2 * 1'RND(1)
    cball.vely = -2 + 2 * 1'RND(1)
End Sub
'*****************
'explosions ship
'*****************

Dim Fire1Pos,Flames
Flames = Array("exp1", "exp2", "exp3", "exp4")

Sub StartFire
    Fire1Pos = 0
    FireTimer.Enabled = 1
End Sub

Sub FireTimer_Timer
    explosion2.ImageA = Flames(Fire1Pos)
	explosion1.ImageA = Flames(Fire1Pos)
	explosion3.ImageA = Flames(Fire1Pos)
	explosion4.ImageA = Flames(Fire1Pos)
	explosion5.ImageA = Flames(Fire1Pos)
	explosion6.ImageA = Flames(Fire1Pos)
	explosion001.ImageA = Flames(Fire1Pos)
	explosion002.ImageA = Flames(Fire1Pos)
	explosion003.ImageA = Flames(Fire1Pos)
	explosion004.ImageA = Flames(Fire1Pos)
	explosion005.ImageA = Flames(Fire1Pos)
	explosion006.ImageA = Flames(Fire1Pos)
	explosion007.ImageA = Flames(Fire1Pos)
	explosion008.ImageA = Flames(Fire1Pos)
    Fire1Pos = (Fire1Pos + 1) MOD 4
End Sub

'*****************
'cortanabase animation
'*****************

Dim cbaseePos, cbasee
cbasee = Array("Cor16", "Cor15", "Cor14")

Sub Startcbasee
    cbaseePos = 0
    cbaseetimer.Enabled = 1
End Sub

Sub cbaseetimer_Timer
    'debug.print fire1pos
    cbase.image = cbasee(cbaseePos)
    cbaseePos = (cbaseePos + 1) MOD 3
End Sub

'*****************
'slingshot animation
'*****************
'Flashers (Flames) from JP Salas table "Diablo"

Dim Electric1Pos, Electric1
Electric1 = Array("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10")

Sub StartElectric1
    Electric1Pos = 0
    sling1timer.Enabled = 1
End Sub

Sub sling1timer_Timer
    'debug.print fire1pos
    Flasher001.ImageA = Electric1(Electric1Pos)
    Flasher006.ImageA = Electric1(Electric1Pos)
    Flasher016.ImageA = Electric1(Electric1Pos)
    Electric1Pos = (Electric1Pos + 1) MOD 10
End Sub

Dim Electric2Pos, Electric2
Electric2 = Array("a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "a10")

Sub StartElectric2
    Electric2Pos = 0
    sling2timer.Enabled = 1
End Sub

Sub sling2timer_Timer
    'debug.print fire1pos
    Flasher002.ImageA = Electric2(Electric1Pos)
    Flasher007.ImageA = Electric2(Electric1Pos)
    Flasher017.ImageA = Electric2(Electric1Pos)
    Electric2Pos = (Electric2Pos + 1) MOD 10
End Sub

'*****************
'side table animation
'*****************
Dim side1Pos, side1a
side1a = Array("frm1", "frm2", "frm3", "frm4", "frm5", "frm6", "frm7", "frm8", "frm9", "frm10", "frm11", "frm12", "frm13", "frm14", "frm15", "frm16", "frm17", "frm18", "frm19", "frm20", "frm21", "frm22", "frm23", "frm24", "frm25", "frm26", "frm27", "frm28", "frm29", "frm30", "frm31", "frm32", "frm33")

Sub Startside1
    side1Pos = 0
    side1timer.Enabled = 1
End Sub

Sub side1timer_Timer
    'debug.print fire1pos
    Flasher004.ImageA = side1a(side1Pos)
    Flasher008.ImageA = side1a(side1Pos)
    Flasher018.ImageA = side1a(side1Pos)
    side1Pos = (side1Pos + 1) MOD 33
End Sub

Dim side2Pos, side1b
side1b = Array("frm1", "frm2", "frm3", "frm4", "frm5", "frm6", "frm7", "frm8", "frm9", "frm10", "frm11", "frm12", "frm13", "frm14", "frm15", "frm16", "frm17", "frm18", "frm19", "frm20", "frm21", "frm22", "frm23", "frm24", "frm25", "frm26", "frm27", "frm28", "frm29", "frm30", "frm31", "frm32", "frm33")

Sub Startside2
    side2Pos = 0
    side2timer.Enabled = 1
End Sub

Sub side2timer_Timer
    'debug.print fire1pos
    Flasher005.ImageA = side1b(side2Pos)
	Flasher003.ImageA = side1b(side2Pos)
	Flasher015.ImageA = side1b(side2Pos)
    side2Pos = (side2Pos + 1) MOD 33
End Sub


'*****************
'activate halo
'*****************

Sub HaloTimer_Timer
	   halo2.Roty = halo2.Roty + 1
   halo.Roty = halo.Roty + 1
   if halo.Roty > 360 then
       halo.Roty = 1
   end if
   if halo2.Roty > 360 then
       halo2.Roty = 1
   end if
end sub

'********************************
'        Digital clock
'********************************

Dim ClockDigits(4), ClockChars(10)

ClockDigits(0) = Array(a00, a02, a05, a06, a04, a01, a03) 'clock left digit
ClockDigits(1) = Array(a10, a12, a15, a16, a14, a11, a13)
ClockChars(0) = Array(1, 1, 1, 1, 1, 1, 0)                '0
ClockChars(1) = Array(0, 1, 1, 0, 0, 0, 0)                '1
ClockChars(2) = Array(1, 1, 0, 1, 1, 0, 1)                '2
ClockChars(3) = Array(1, 1, 1, 1, 0, 0, 1)                '3
ClockChars(4) = Array(0, 1, 1, 0, 0, 1, 1)                '4
ClockChars(5) = Array(1, 0, 1, 1, 0, 1, 1)                '5
ClockChars(6) = Array(1, 0, 1, 1, 1, 1, 1)                '6
ClockChars(7) = Array(1, 1, 1, 0, 0, 0, 0)                '7
ClockChars(8) = Array(1, 1, 1, 1, 1, 1, 1)                '8
ClockChars(9) = Array(1, 1, 1, 1, 0, 1, 1)                '9

Sub UpdateClock(myTime)
    Dim a, b, i
    a = myTime \ 10
    b = myTime MOD 10
    For i = 0 to 6
        ClockDigits(0)(i).State = ClockChars(a)(i)
        ClockDigits(1)(i).State = ClockChars(b)(i)
    Next
End Sub

Sub TurnOffClock
    Dim i
    For i = 0 to 6
        ClockDigits(0)(i).State = 0
        ClockDigits(1)(i).State = 0
    Next
End Sub

'clocktimer
Sub mode1timer_Timer
	PlaySong "missionmusic"
	missionmodes = 1
    mode1TimerCount = mode1TimerCount - 1
    UpdateClock mode1TimerCount
    If mode1TimerCount = 30 Then PlaySound "30s2"
    If mode1TimerCount = 10 Then PlaySound "10s"
    If mode1TimerCount = 0 Then
	DMD "", "", "dmdmf", eNone, eNone, eNone, 1000, True, ""
        PlaySound "obj_failed"
		stopidlchiefke
		chmfRtimer.enabled = 1
		chmfLtimer.enabled = 1
		stopmissions
        Stopmode1
    End If
End Sub

Sub mode2timer_Timer
	'PlaySong ""
    mode2TimerCount = mode2TimerCount - 1
    UpdateClock mode2TimerCount
    'If mode2TimerCount = 30 Then PlaySound "30s2"
    'If mode1TimerCount = 10 Then PlaySound "10s"
    If mode2TimerCount = 0 Then
	DMD "", "", "dmdbf", eNone, eNone, eNone, 1000, True, ""
        PlaySound "brf"
		stopidlchiefke
		chmfRtimer.enabled = 1
		chmfLtimer.enabled = 1
        Stopmode2
		disablebonusgames
    End If
End Sub

Sub Stopmode1
flashplaat1.ImageA = "flshalopin"
mode1timer.Enabled = 0
changelights2
StopSong
UpdateMusicNow
TurnOffClock
missionmodes = 0
enemylft = 0
End Sub

Sub Stopmode2
flashplaat1.ImageA = "flshalopin"
changelights2
RightFlipper001.RotatetoStart
LeftFlipper001.RotatetoStart
LowerFlippersActive = False
mode2timer.Enabled = 0
'changelights3
StopSong
UpdateMusicNow
TurnOffClock
disablebonusgames
enemylft = 0
End Sub

Sub StopmodeEndofBall
changelights2
mode1timer.Enabled = 0
StopSong
'UpdateMusicNow
TurnOffClock
missionmodes = 0
End Sub

'************************************************
'**************Bank Animation*****************
'************************************************
Dim BankDir, BankPos
	
Sub sw9_Hit
	PlaySound "target_bank"
	li009.state = 1
	CheckbBank
End Sub

Sub sw10_Hit
	PlaySound "target_bank"
	li010.state = 1
	CheckbBank
End Sub

Sub sw11_Hit 
	PlaySound "target_bank"
	li011.state = 1
	CheckbBank
End Sub

Sub CheckbBank
    If(li009.State = 1) And(li010.State = 1) And(li011.State = 1) Then
    DMD "", "", "dmdbank", eNone, eNone, eNone, 1000, True, "bank"
	AddScore 50000
	bonusyscorechecker = bonusyscorechecker + 50000
	li009.State=0
	li010.State=0
	li011.State=0
	'Attendi=0
	sw9.IsDropped=true
	sw10.IsDropped=true
	sw11.IsDropped=true
	sw001.IsDropped=true
	BankDir = 1
	Bankdown.Enabled= True
	PlaySound "LIFTY2"
    End If
End Sub

'Sub BankTimer_Timer
'	TargetBank.TransY = -BankPos   	
'	BankPos = BankPos + BankDir 
'	If BankPos < 0 Then BankPos = 0: Me.Enabled = 0
'End Sub

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

sub resetswitchdropped
	'bankup.enabled = 1
	sw9.IsDropped=False
	sw10.IsDropped=False
	sw11.IsDropped=False
	sw001.IsDropped=False
end sub

'************************************************
'**************bridge*****************
'************************************************
Dim bridgeDir, bridgePos

sub bridgedown_Timer
bridgeup.enabled = 0
If bridge.z > -35 then 
    bridge.z = bridge.z -5
Else
	bridgedown.Enabled = 0
End If
end sub

sub bridgeup_Timer
bridgedown.enabled = 0
If bridge.z < 35 Then
    bridge.z = bridge.z + 5
Else
	bridgeup.Enabled = 0
End If
end sub

'************************************************
'**************helm Animation*****************
'************************************************
Dim HelmDir, HelmPos

sub helmdownTimer_Timer
bankup.enabled = 0
If helmet.z > -85 then 
    helmet.z = helmet.z -2
Else
	helmdownTimer.Enabled = 0
End If
end sub

sub helmupTimer_Timer
bankdown.enabled = 0
If helmet.z < -1 Then
    helmet.z = helmet.z + 2
Else
	helmupTimer.Enabled = 0
End If
vpmTimer.AddTimer 1500, "helmetright '" 
end sub

sub helmetright
helmet.z = "-0,5"
end sub

sub duuus
playsound "LIFT2"
end sub


'************************************************
'**************3d animations*****************
'************************************************

Sub boxingtimer_Timer
countr = countr + 1 : If Countr > 5 then Countr = 1 : end If
select case countr
				case 1 : box1.z=53:box2.z=-200:box3.z=-200:box4.z=-200:box5.z=-200
				case 2 : box1.z=-200:box2.z=53:box3.z=-200:box4.z=-200:box5.z=-200
				case 3 : box1.z=-200:box2.z=-200:box3.z=53:box4.z=-200:box5.z=-200
				case 4 : box1.z=-200:box2.z=-200:box3.z=-200:box4.z=53:box5.z=-200
				case 5 : box1.z=-200:box2.z=-200:box3.z=-200:box4.z=-200:box5.z=53
			end Select
End Sub

Sub boxingRtimer_Timer
countr36 = countr36 + 1 : If Countr36 > 5 then Countr36 = 1 : end If
select case countr36
				case 1 : box001.z=53:box002.z=-200:box003.z=-200:box004.z=-200:box005.z=-200
				case 2 : box001.z=-200:box002.z=53:box003.z=-200:box004.z=-200:box005.z=-200
				case 3 : box001.z=-200:box002.z=-200:box003.z=53:box004.z=-200:box005.z=-200
				case 4 : box001.z=-200:box002.z=-200:box003.z=-200:box004.z=53:box005.z=-200
				case 5 : box001.z=-200:box002.z=-200:box003.z=-200:box004.z=-200:box005.z=53
			end Select
End Sub

Sub salutetimer_Timer
countr2 = countr2 + 1 : If Countr2 > 19 then stopanimation1 : end If 'then Countr2 = 1 : end If 'then stopanimation1 : end If
select case countr2
				case 1 : cfsal001.z=53:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				case 2 :  cfsal001.z=-200:cfsal002.z=53:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				case 3 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=53:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				case 4 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=53:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				case 5 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=53:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				case 6 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=53:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				case 7 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=53:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				case 8 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=53:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				case 9 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=53:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				case 10 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=53:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				case 11 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=53:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				case 12 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=53:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				case 13 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=53:cfsal014.z=-200:cfsal015.z=-200
				case 14 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=53:cfsal015.z=-200
				case 15 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=53
				case 16 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=53:cfsal015.z=-200
				case 17 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=53:cfsal014.z=-200:cfsal015.z=-200
				case 18 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=53:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				case 19 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=53:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
				'case 20 :  cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-53:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
			end Select
End Sub

Sub salutetimer001_Timer
countr31 = countr31 + 1 : If Countr31 > 19 then stopanimation2 : end If 'then Countr2 = 1 : end If 'then stopanimation1 : end If
select case countr31
				case 1 : cfsalb001.z=53:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				case 2 :  cfsalb001.z=-200:cfsalb002.z=53:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				case 3 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=53:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				case 4 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=53:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				case 5 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=53:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				case 6 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=53:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				case 7 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=53:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				case 8 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=53:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				case 9 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=53:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				case 10 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=53:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				case 11 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=53:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				case 12 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=53:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				case 13 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=53:cfsalb014.z=-200:cfsalb015.z=-200
				case 14 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=53:cfsalb015.z=-200
				case 15 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=53
				case 16 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=53:cfsalb015.z=-200
				case 17 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=53:cfsalb014.z=-200:cfsalb015.z=-200
				case 18 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=53:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				case 19 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=53:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
				'case 20 :  cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-53:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
			end Select
End Sub

Sub idatlLtimer001_Timer
countr30 = countr30 + 1 : If Countr30 > 18 then Countr30 = 1 : end If 
select case countr30
				case 1 : cfidl1atract001.z=53:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 2 : cfidl1atract001.z=-200:cfidl1atract002.z=53:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 3 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=53:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 4 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=53:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 5 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=53:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 6 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=53:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 7 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=53:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 8 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=53:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 9 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=53:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 10 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=53:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 11 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=53:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 12 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=53:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 13 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=53:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 14 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=53:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 15 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=53:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 16 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=53:cfidl1atract017.z=-200:cfidl1atract018.z=-200
				case 17 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=53:cfidl1atract018.z=-200
				case 18 : cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=53
			end Select
End Sub

Sub idlLtimer_Timer
countr3 = countr3 + 1 : If Countr3 > 18 then Countr3 = 1 : end If 
select case countr3
				case 1 : cfidl001.z=53:cfidl002.z=-200:cfidl003.z=-200:cfidl004.z=-200:cfidl005.z=-200:cfidl006.z=-200
				case 2 : cfidl001.z=-200:cfidl002.z=53:cfidl003.z=-200:cfidl004.z=-200:cfidl005.z=-200:cfidl006.z=-200
				case 3 : cfidl001.z=-200:cfidl002.z=-200:cfidl003.z=53:cfidl004.z=-200:cfidl005.z=-200:cfidl006.z=-200
				case 4 : cfidl001.z=-200:cfidl002.z=-200:cfidl003.z=-200:cfidl004.z=53:cfidl005.z=-200:cfidl006.z=-200
				case 5 : cfidl001.z=-200:cfidl002.z=-200:cfidl003.z=-200:cfidl004.z=-200:cfidl005.z=53:cfidl006.z=-200
				case 6 : cfidl001.z=-200:cfidl002.z=-200:cfidl003.z=-200:cfidl004.z=-200:cfidl005.z=-200:cfidl006.z=53
				case 7 : cfidl001.z=53:cfidl002.z=-200:cfidl003.z=-200:cfidl004.z=-200:cfidl005.z=-200:cfidl006.z=-200
				case 8 : cfidl001.z=-200:cfidl002.z=53:cfidl003.z=-200:cfidl004.z=-200:cfidl005.z=-200:cfidl006.z=-200
				case 9 : cfidl001.z=-200:cfidl002.z=-200:cfidl003.z=53:cfidl004.z=-200:cfidl005.z=-200:cfidl006.z=-200
				case 10 : cfidl001.z=-200:cfidl002.z=-200:cfidl003.z=-200:cfidl004.z=53:cfidl005.z=-200:cfidl006.z=-200
				case 11 : cfidl001.z=-200:cfidl002.z=-200:cfidl003.z=-200:cfidl004.z=-200:cfidl005.z=53:cfidl006.z=-200
				case 12 : cfidl001.z=-200:cfidl002.z=-200:cfidl003.z=-200:cfidl004.z=-200:cfidl005.z=-200:cfidl006.z=53
				case 13 : cfidl001.z=53:cfidl002.z=-200:cfidl003.z=-200:cfidl004.z=-200:cfidl005.z=-200:cfidl006.z=-200
				case 14 : cfidl001.z=-200:cfidl002.z=53:cfidl003.z=-200:cfidl004.z=-200:cfidl005.z=-200:cfidl006.z=-200
				case 15 : cfidl001.z=-200:cfidl002.z=-200:cfidl003.z=53:cfidl004.z=-200:cfidl005.z=-200:cfidl006.z=-200
				case 16 : cfidl001.z=-200:cfidl002.z=-200:cfidl003.z=-200:cfidl004.z=53:cfidl005.z=-200:cfidl006.z=-200
				case 17 : cfidl001.z=-200:cfidl002.z=-200:cfidl003.z=-200:cfidl004.z=-200:cfidl005.z=53:cfidl006.z=-200
				case 18 : cfidl001.z=-200:cfidl002.z=-200:cfidl003.z=-200:cfidl004.z=-200:cfidl005.z=-200:cfidl006.z=53
			end Select
End Sub

Sub idlLtimer001_Timer
countr32 = countr32 + 1 : If Countr32 > 18 then Countr32 = 1 : end If 
select case countr32
				case 1 : cfidla001.z=53:cfidla002.z=-200:cfidla003.z=-200:cfidla004.z=-200:cfidla005.z=-200:cfidla006.z=-200
				case 2 : cfidla001.z=-200:cfidla002.z=53:cfidla003.z=-200:cfidla004.z=-200:cfidla005.z=-200:cfidla006.z=-200
				case 3 : cfidla001.z=-200:cfidla002.z=-200:cfidla003.z=53:cfidla004.z=-200:cfidla005.z=-200:cfidla006.z=-200
				case 4 : cfidla001.z=-200:cfidla002.z=-200:cfidla003.z=-200:cfidla004.z=53:cfidla005.z=-200:cfidla006.z=-200
				case 5 : cfidla001.z=-200:cfidla002.z=-200:cfidla003.z=-200:cfidla004.z=-200:cfidla005.z=53:cfidla006.z=-200
				case 6 : cfidla001.z=-200:cfidla002.z=-200:cfidla003.z=-200:cfidla004.z=-200:cfidla005.z=-200:cfidla006.z=53
				case 7 : cfidla001.z=53:cfidla002.z=-200:cfidla003.z=-200:cfidla004.z=-200:cfidla005.z=-200:cfidla006.z=-200
				case 8 : cfidla001.z=-200:cfidla002.z=53:cfidla003.z=-200:cfidla004.z=-200:cfidla005.z=-200:cfidla006.z=-200
				case 9 : cfidla001.z=-200:cfidla002.z=-200:cfidla003.z=53:cfidla004.z=-200:cfidla005.z=-200:cfidla006.z=-200
				case 10 : cfidla001.z=-200:cfidla002.z=-200:cfidla003.z=-200:cfidla004.z=53:cfidla005.z=-200:cfidla006.z=-200
				case 11 : cfidla001.z=-200:cfidla002.z=-200:cfidla003.z=-200:cfidla004.z=-200:cfidla005.z=53:cfidla006.z=-200
				case 12 : cfidla001.z=-200:cfidla002.z=-200:cfidla003.z=-200:cfidla004.z=-200:cfidla005.z=-200:cfidla006.z=53
				case 13 : cfidla001.z=53:cfidla002.z=-200:cfidla003.z=-200:cfidla004.z=-200:cfidla005.z=-200:cfidla006.z=-200
				case 14 : cfidla001.z=-200:cfidla002.z=53:cfidla003.z=-200:cfidla004.z=-200:cfidla005.z=-200:cfidla006.z=-200
				case 15 : cfidla001.z=-200:cfidla002.z=-200:cfidla003.z=53:cfidla004.z=-200:cfidla005.z=-200:cfidla006.z=-200
				case 16 : cfidla001.z=-200:cfidla002.z=-200:cfidla003.z=-200:cfidla004.z=53:cfidla005.z=-200:cfidla006.z=-200
				case 17 : cfidla001.z=-200:cfidla002.z=-200:cfidla003.z=-200:cfidla004.z=-200:cfidla005.z=53:cfidla006.z=-200
				case 18 : cfidla001.z=-200:cfidla002.z=-200:cfidla003.z=-200:cfidla004.z=-200:cfidla005.z=-200:cfidla006.z=53
			end Select
End Sub

Sub idatlLtimer_Timer
countr4 = countr4 + 1 : If Countr4 > 18 then Countr4 = 1 : end If 
select case countr4
				case 1 : cfidlatract001.z=53:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 2 : cfidlatract001.z=-200:cfidlatract002.z=53:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 3 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=53:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 4 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=53:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 5 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=53:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 6 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=53:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 7 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=53:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 8 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=53:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 9 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=53:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 10 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=53:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 11 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=53:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 12 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=53:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 13 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=53:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 14 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=53:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 15 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=53:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 16 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=53:cfidlatract017.z=-200:cfidlatract018.z=-200
				case 17 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=53:cfidlatract018.z=-200
				case 18 : cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=53
			end Select
End Sub

Sub grund1timer_Timer
countr5 = countr5 + 1 : If Countr5 > 8 then Countr5 = 1 : end If 
select case countr5
				case 1 : grund001.z=0:grund002.z=-200:grund003.z=-200:grund004.z=-200:grund005.z=-200:grund006.z=-200:grund007.z=-200:grund008.z=-200
				case 2 : grund001.z=-200:grund002.z=0:grund003.z=-200:grund004.z=-200:grund005.z=-200:grund006.z=-200:grund007.z=-200:grund008.z=-200
				case 3 : grund001.z=-200:grund002.z=-200:grund003.z=0:grund004.z=-200:grund005.z=-200:grund006.z=-200:grund007.z=-200:grund008.z=-200
				case 4 : grund001.z=-200:grund002.z=-200:grund003.z=-200:grund004.z=0:grund005.z=-200:grund006.z=-200:grund007.z=-200:grund008.z=-200
				case 5 : grund001.z=-200:grund002.z=-200:grund003.z=-200:grund004.z=-200:grund005.z=0:grund006.z=-200:grund007.z=-200:grund008.z=-200
				case 6 : grund001.z=-200:grund002.z=-200:grund003.z=-200:grund004.z=-200:grund005.z=-200:grund006.z=0:grund007.z=-200:grund008.z=-200
				case 7 : grund001.z=-200:grund002.z=-200:grund003.z=-200:grund004.z=-200:grund005.z=-200:grund006.z=-200:grund007.z=0:grund008.z=-200
				case 8 : grund001.z=-200:grund002.z=-200:grund003.z=-200:grund004.z=-200:grund005.z=-200:grund006.z=-200:grund007.z=-200:grund008.z=0
			end Select
End Sub

Sub grund2timer_Timer
countr6 = countr6 + 1 : If Countr6 > 8 then Countr6 = 1 : end If 
select case countr6
				case 1 : grund009.z=0:grund010.z=-200:grund011.z=-200:grund012.z=-200:grund013.z=-200:grund014.z=-200:grund015.z=-200:grund016.z=-200
				case 2 : grund009.z=-200:grund010.z=0:grund011.z=-200:grund012.z=-200:grund013.z=-200:grund014.z=-200:grund015.z=-200:grund016.z=-200
				case 3 : grund009.z=-200:grund010.z=-200:grund011.z=0:grund012.z=-200:grund013.z=-200:grund014.z=-200:grund015.z=-200:grund016.z=-200
				case 4 : grund009.z=-200:grund010.z=-200:grund011.z=-200:grund012.z=0:grund013.z=-200:grund014.z=-200:grund015.z=-200:grund016.z=-200
				case 5 : grund009.z=-200:grund010.z=-200:grund011.z=-200:grund012.z=-200:grund013.z=0:grund014.z=-200:grund015.z=-200:grund016.z=-200
				case 6 : grund009.z=-200:grund010.z=-200:grund011.z=-200:grund012.z=-200:grund013.z=-200:grund014.z=0:grund015.z=-200:grund016.z=-200
				case 7 : grund009.z=-200:grund010.z=-200:grund011.z=-200:grund012.z=-200:grund013.z=-200:grund014.z=-200:grund015.z=0:grund016.z=-200
				case 8 : grund009.z=-200:grund010.z=-200:grund011.z=-200:grund012.z=-200:grund013.z=-200:grund014.z=-200:grund015.z=-200:grund016.z=0
			end Select
End Sub

Sub grund3timer_Timer
countr7 = countr7 + 1 : If Countr7 > 8 then Countr7 = 1 : end If 
select case countr7
				case 1 : grund017.z=0:grund018.z=-200:grund019.z=-200:grund020.z=-200:grund021.z=-200:grund022.z=-200:grund023.z=-200:grund024.z=-200
				case 2 : grund017.z=-200:grund018.z=0:grund019.z=-200:grund020.z=-200:grund021.z=-200:grund022.z=-200:grund023.z=-200:grund024.z=-200
				case 3 : grund017.z=-200:grund018.z=-200:grund019.z=0:grund020.z=-200:grund021.z=-200:grund022.z=-200:grund023.z=-200:grund024.z=-200
				case 4 : grund017.z=-200:grund018.z=-200:grund019.z=-200:grund020.z=0:grund021.z=-200:grund022.z=-200:grund023.z=-200:grund024.z=-200
				case 5 : grund017.z=-200:grund018.z=-200:grund019.z=-200:grund020.z=-200:grund021.z=0:grund022.z=-200:grund023.z=-200:grund024.z=-200
				case 6 : grund017.z=-200:grund018.z=-200:grund019.z=-200:grund020.z=-200:grund021.z=-200:grund022.z=0:grund023.z=-200:grund024.z=-200
				case 7 : grund017.z=-200:grund018.z=-200:grund019.z=-200:grund020.z=-200:grund021.z=-200:grund022.z=-200:grund023.z=0:grund024.z=-200
				case 8 : grund017.z=-200:grund018.z=-200:grund019.z=-200:grund020.z=-200:grund021.z=-200:grund022.z=-200:grund023.z=-200:grund024.z=0
			end Select
End Sub

Sub grund4timer_Timer
countr8 = countr8 + 1 : If Countr8 > 8 then Countr8 = 1 : end If 
select case countr8
				case 1 : grund025.z=0:grund026.z=-200:grund027.z=-200:grund028.z=-200:grund029.z=-200:grund030.z=-200:grund031.z=-200:grund032.z=-200
				case 2 : grund025.z=-200:grund026.z=0:grund027.z=-200:grund028.z=-200:grund029.z=-200:grund030.z=-200:grund031.z=-200:grund032.z=-200
				case 3 : grund025.z=-200:grund026.z=-200:grund027.z=0:grund028.z=-200:grund029.z=-200:grund030.z=-200:grund031.z=-200:grund032.z=-200
				case 4 : grund025.z=-200:grund026.z=-200:grund027.z=-200:grund028.z=0:grund029.z=-200:grund030.z=-200:grund031.z=-200:grund032.z=-200
				case 5 : grund025.z=-200:grund026.z=-200:grund027.z=-200:grund028.z=-200:grund029.z=0:grund030.z=-200:grund031.z=-200:grund032.z=-200
				case 6 : grund025.z=-200:grund026.z=-200:grund027.z=-200:grund028.z=-200:grund029.z=-200:grund030.z=0:grund031.z=-200:grund032.z=-200
				case 7 : grund025.z=-200:grund026.z=-200:grund027.z=-200:grund028.z=-200:grund029.z=-200:grund030.z=-200:grund031.z=0:grund032.z=-200
				case 8 : grund025.z=-200:grund026.z=-200:grund027.z=-200:grund028.z=-200:grund029.z=-200:grund030.z=-200:grund031.z=-200:grund032.z=0
			end Select
End Sub

Sub grund5timer_Timer
countr9 = countr9 + 1 : If Countr9 > 8 then Countr9 = 1 : end If 
select case countr9
				case 1 : grund033.z=0:grund034.z=-200:grund035.z=-200:grund036.z=-200:grund037.z=-200:grund038.z=-200:grund039.z=-200:grund040.z=-200
				case 2 : grund033.z=-200:grund034.z=0:grund035.z=-200:grund036.z=-200:grund037.z=-200:grund038.z=-200:grund039.z=-200:grund040.z=-200
				case 3 : grund033.z=-200:grund034.z=-200:grund035.z=0:grund036.z=-200:grund037.z=-200:grund038.z=-200:grund039.z=-200:grund040.z=-200
				case 4 : grund033.z=-200:grund034.z=-200:grund035.z=-200:grund036.z=0:grund037.z=-200:grund038.z=-200:grund039.z=-200:grund040.z=-200
				case 5 : grund033.z=-200:grund034.z=-200:grund035.z=-200:grund036.z=-200:grund037.z=0:grund038.z=-200:grund039.z=-200:grund040.z=-200
				case 6 : grund033.z=-200:grund034.z=-200:grund035.z=-200:grund036.z=-200:grund037.z=-200:grund038.z=0:grund039.z=-200:grund040.z=-200
				case 7 : grund033.z=-200:grund034.z=-200:grund035.z=-200:grund036.z=-200:grund037.z=-200:grund038.z=-200:grund039.z=0:grund040.z=-200
				case 8 : grund033.z=-200:grund034.z=-200:grund035.z=-200:grund036.z=-200:grund037.z=-200:grund038.z=-200:grund039.z=-200:grund040.z=0
			end Select
End Sub

Sub elite1timer_Timer
countr10 = countr10 + 1 : If Countr10 > 7 then Countr10 = 1 : end If 
select case countr10
				case 1 : elite001.z=0:elite002.z=-200:elite003.z=-200:elite004.z=-200:elite005.z=-200:elite006.z=-200:elite007.z=-200
				case 2 : elite001.z=-200:elite002.z=0:elite003.z=-200:elite004.z=-200:elite005.z=-200:elite006.z=-200:elite007.z=-200
				case 3 : elite001.z=-200:elite002.z=-200:elite003.z=0:elite004.z=-200:elite005.z=-200:elite006.z=-200:elite007.z=-200
				case 4 : elite001.z=-200:elite002.z=-200:elite003.z=-200:elite004.z=0:elite005.z=-200:elite006.z=-200:elite007.z=-200
				case 5 : elite001.z=-200:elite002.z=-200:elite003.z=-200:elite004.z=-200:elite005.z=0:elite006.z=-200:elite007.z=-200
				case 6 : elite001.z=-200:elite002.z=-200:elite003.z=-200:elite004.z=-200:elite005.z=-200:elite006.z=0:elite007.z=-200
				case 7 : elite001.z=-200:elite002.z=-200:elite003.z=-200:elite004.z=-200:elite005.z=-200:elite006.z=-200:elite007.z=0
			end Select
End Sub

Sub elite2timer_Timer
countr11 = countr11 + 1 : If Countr11 > 7 then Countr11 = 1 : end If 
select case countr11
				case 1 : elite008.z=0:elite009.z=-200:elite010.z=-200:elite011.z=-200:elite012.z=-200:elite013.z=-200:elite014.z=-200
				case 2 : elite008.z=-200:elite009.z=0:elite010.z=-200:elite011.z=-200:elite012.z=-200:elite013.z=-200:elite014.z=-200
				case 3 : elite008.z=-200:elite009.z=-200:elite010.z=0:elite011.z=-200:elite012.z=-200:elite013.z=-200:elite014.z=-200
				case 4 : elite008.z=-200:elite009.z=-200:elite010.z=-200:elite011.z=0:elite012.z=-200:elite013.z=-200:elite014.z=-200
				case 5 : elite008.z=-200:elite009.z=-200:elite010.z=-200:elite011.z=-200:elite012.z=0:elite013.z=-200:elite014.z=-200
				case 6 : elite008.z=-200:elite009.z=-200:elite010.z=-200:elite011.z=-200:elite012.z=-200:elite013.z=0:elite014.z=-200
				case 7 : elite008.z=-200:elite009.z=-200:elite010.z=-200:elite011.z=-200:elite012.z=-200:elite013.z=-200:elite014.z=0
			end Select
End Sub

Sub elite3timer_Timer
countr12 = countr12 + 1 : If Countr12 > 7 then Countr12 = 1 : end If 
select case countr12
				case 1 : elite015.z=0:elite016.z=-200:elite017.z=-200:elite018.z=-200:elite019.z=-200:elite020.z=-200:elite021.z=-200
				case 2 : elite015.z=-200:elite016.z=0:elite017.z=-200:elite018.z=-200:elite019.z=-200:elite020.z=-200:elite021.z=-200
				case 3 : elite015.z=-200:elite016.z=-200:elite017.z=0:elite018.z=-200:elite019.z=-200:elite020.z=-200:elite021.z=-200
				case 4 : elite015.z=-200:elite016.z=-200:elite017.z=-200:elite018.z=0:elite019.z=-200:elite020.z=-200:elite021.z=-200
				case 5 : elite015.z=-200:elite016.z=-200:elite017.z=-200:elite018.z=-200:elite019.z=0:elite020.z=-200:elite021.z=-200
				case 6 : elite015.z=-200:elite016.z=-200:elite017.z=-200:elite018.z=-200:elite019.z=-200:elite020.z=0:elite021.z=-200
				case 7 : elite015.z=-200:elite016.z=-200:elite017.z=-200:elite018.z=-200:elite019.z=-200:elite020.z=-200:elite021.z=0
			end Select
End Sub

Sub elite4timer_Timer
countr13 = countr13 + 1 : If Countr13 > 7 then Countr13 = 1 : end If 
select case countr13
				case 1 : elite022.z=0:elite023.z=-200:elite024.z=-200:elite025.z=-200:elite026.z=-200:elite027.z=-200:elite028.z=-200
				case 2 : elite022.z=-200:elite023.z=0:elite024.z=-200:elite025.z=-200:elite026.z=-200:elite027.z=-200:elite028.z=-200
				case 3 : elite022.z=-200:elite023.z=-200:elite024.z=0:elite025.z=-200:elite026.z=-200:elite027.z=-200:elite028.z=-200
				case 4 : elite022.z=-200:elite023.z=-200:elite024.z=-200:elite025.z=0:elite026.z=-200:elite027.z=-200:elite028.z=-200
				case 5 : elite022.z=-200:elite023.z=-200:elite024.z=-200:elite025.z=-200:elite026.z=0:elite027.z=-200:elite028.z=-200
				case 6 : elite022.z=-200:elite023.z=-200:elite024.z=-200:elite025.z=-200:elite026.z=-200:elite027.z=0:elite028.z=-200
				case 7 : elite022.z=-200:elite023.z=-200:elite024.z=-200:elite025.z=-200:elite026.z=-200:elite027.z=-200:elite028.z=0
			end Select
End Sub

Sub elite5timer_Timer
countr14 = countr14 + 1 : If Countr14 > 7 then Countr14 = 1 : end If 
select case countr14
				case 1 : elite029.z=0:elite030.z=-200:elite031.z=-200:elite032.z=-200:elite033.z=-200:elite034.z=-200:elite035.z=-200
				case 2 : elite029.z=-200:elite030.z=0:elite031.z=-200:elite032.z=-200:elite033.z=-200:elite034.z=-200:elite035.z=-200
				case 3 : elite029.z=-200:elite030.z=-200:elite031.z=0:elite032.z=-200:elite033.z=-200:elite034.z=-200:elite035.z=-200
				case 4 : elite029.z=-200:elite030.z=-200:elite031.z=-200:elite032.z=0:elite033.z=-200:elite034.z=-200:elite035.z=-200
				case 5 : elite029.z=-200:elite030.z=-200:elite031.z=-200:elite032.z=-200:elite033.z=0:elite034.z=-200:elite035.z=-200
				case 6 : elite029.z=-200:elite030.z=-200:elite031.z=-200:elite032.z=-200:elite033.z=-200:elite034.z=0:elite035.z=-200
				case 7 : elite029.z=-200:elite030.z=-200:elite031.z=-200:elite032.z=-200:elite033.z=-200:elite034.z=-200:elite035.z=0
			end Select
End Sub

Sub jackal1timer_Timer
countr15 = countr15 + 1 : If Countr15 > 7 then Countr15 = 1 : end If 
select case countr15
				case 1 : jackal001.z=0:jackal002.z=-200:jackal003.z=-200:jackal004.z=-200:jackal005.z=-200:jackal006.z=-200:jackal007.z=-200
				case 2 : jackal001.z=-200:jackal002.z=0:jackal003.z=-200:jackal004.z=-200:jackal005.z=-200:jackal006.z=-200:jackal007.z=-200
				case 3 : jackal001.z=-200:jackal002.z=-200:jackal003.z=0:jackal004.z=-200:jackal005.z=-200:jackal006.z=-200:jackal007.z=-200
				case 4 : jackal001.z=-200:jackal002.z=-200:jackal003.z=-200:jackal004.z=0:jackal005.z=-200:jackal006.z=-200:jackal007.z=-200
				case 5 : jackal001.z=-200:jackal002.z=-200:jackal003.z=-200:jackal004.z=-200:jackal005.z=0:jackal006.z=-200:jackal007.z=-200
				case 6 : jackal001.z=-200:jackal002.z=-200:jackal003.z=-200:jackal004.z=-200:jackal005.z=-200:jackal006.z=0:jackal007.z=-200
				case 7 : jackal001.z=-200:jackal002.z=-200:jackal003.z=-200:jackal004.z=-200:jackal005.z=-200:jackal006.z=-200:jackal007.z=0
			end Select
End Sub

Sub jackal2timer_Timer
countr16 = countr16 + 1 : If Countr16 > 7 then Countr16 = 1 : end If 
select case countr16
				case 1 : jackal008.z=0:jackal009.z=-200:jackal010.z=-200:jackal011.z=-200:jackal012.z=-200:jackal013.z=-200:jackal014.z=-200
				case 2 : jackal008.z=-200:jackal009.z=0:jackal010.z=-200:jackal011.z=-200:jackal012.z=-200:jackal013.z=-200:jackal014.z=-200
				case 3 : jackal008.z=-200:jackal009.z=-200:jackal010.z=0:jackal011.z=-200:jackal012.z=-200:jackal013.z=-200:jackal014.z=-200
				case 4 : jackal008.z=-200:jackal009.z=-200:jackal010.z=-200:jackal011.z=0:jackal012.z=-200:jackal013.z=-200:jackal014.z=-200
				case 5 : jackal008.z=-200:jackal009.z=-200:jackal010.z=-200:jackal011.z=-200:jackal012.z=0:jackal013.z=-200:jackal014.z=-200
				case 6 : jackal008.z=-200:jackal009.z=-200:jackal010.z=-200:jackal011.z=-200:jackal012.z=-200:jackal013.z=0:jackal014.z=-200
				case 7 : jackal008.z=-200:jackal009.z=-200:jackal010.z=-200:jackal011.z=-200:jackal012.z=-200:jackal013.z=-200:jackal014.z=0
			end Select
End Sub

Sub jackal3timer_Timer
countr17 = countr17 + 1 : If Countr17 > 7 then Countr17 = 1 : end If 
select case countr17
				case 1 : jackal015.z=0:jackal016.z=-200:jackal017.z=-200:jackal018.z=-200:jackal019.z=-200:jackal020.z=-200:jackal021.z=-200
				case 2 : jackal015.z=-200:jackal016.z=0:jackal017.z=-200:jackal018.z=-200:jackal019.z=-200:jackal020.z=-200:jackal021.z=-200
				case 3 : jackal015.z=-200:jackal016.z=-200:jackal017.z=0:jackal018.z=-200:jackal019.z=-200:jackal020.z=-200:jackal021.z=-200
				case 4 : jackal015.z=-200:jackal016.z=-200:jackal017.z=-200:jackal018.z=0:jackal019.z=-200:jackal020.z=-200:jackal021.z=-200
				case 5 : jackal015.z=-200:jackal016.z=-200:jackal017.z=-200:jackal018.z=-200:jackal019.z=0:jackal020.z=-200:jackal021.z=-200
				case 6 : jackal015.z=-200:jackal016.z=-200:jackal017.z=-200:jackal018.z=-200:jackal019.z=-200:jackal020.z=0:jackal021.z=-200
				case 7 : jackal015.z=-200:jackal016.z=-200:jackal017.z=-200:jackal018.z=-200:jackal019.z=-200:jackal020.z=-200:jackal021.z=0
			end Select
End Sub

Sub jackal4timer_Timer
countr18 = countr18 + 1 : If Countr18 > 7 then Countr18 = 1 : end If 
select case countr18
				case 1 : jackal022.z=0:jackal023.z=-200:jackal024.z=-200:jackal025.z=-200:jackal026.z=-200:jackal027.z=-200:jackal028.z=-200
				case 2 : jackal022.z=-200:jackal023.z=0:jackal024.z=-200:jackal025.z=-200:jackal026.z=-200:jackal027.z=-200:jackal028.z=-200
				case 3 : jackal022.z=-200:jackal023.z=-200:jackal024.z=0:jackal025.z=-200:jackal026.z=-200:jackal027.z=-200:jackal028.z=-200
				case 4 : jackal022.z=-200:jackal023.z=-200:jackal024.z=-200:jackal025.z=0:jackal026.z=-200:jackal027.z=-200:jackal028.z=-200
				case 5 : jackal022.z=-200:jackal023.z=-200:jackal024.z=-200:jackal025.z=-200:jackal026.z=0:jackal027.z=-200:jackal028.z=-200
				case 6 : jackal022.z=-200:jackal023.z=-200:jackal024.z=-200:jackal025.z=-200:jackal026.z=-200:jackal027.z=0:jackal028.z=-200
				case 7 : jackal022.z=-200:jackal023.z=-200:jackal024.z=-200:jackal025.z=-200:jackal026.z=-200:jackal027.z=-200:jackal028.z=0
			end Select
End Sub

Sub jackal5timer_Timer
countr19 = countr19 + 1 : If Countr19 > 7 then Countr19 = 1 : end If 
select case countr19
				case 1 : jackal029.z=0:jackal030.z=-200:jackal031.z=-200:jackal032.z=-200:jackal033.z=-200:jackal034.z=-200:jackal035.z=-200
				case 2 : jackal029.z=-200:jackal030.z=0:jackal031.z=-200:jackal032.z=-200:jackal033.z=-200:jackal034.z=-200:jackal035.z=-200
				case 3 : jackal029.z=-200:jackal030.z=-200:jackal031.z=0:jackal032.z=-200:jackal033.z=-200:jackal034.z=-200:jackal035.z=-200
				case 4 : jackal029.z=-200:jackal030.z=-200:jackal031.z=-200:jackal032.z=0:jackal033.z=-200:jackal034.z=-200:jackal035.z=-200
				case 5 : jackal029.z=-200:jackal030.z=-200:jackal031.z=-200:jackal032.z=-200:jackal033.z=0:jackal034.z=-200:jackal035.z=-200
				case 6 : jackal029.z=-200:jackal030.z=-200:jackal031.z=-200:jackal032.z=-200:jackal033.z=-200:jackal034.z=0:jackal035.z=-200
				case 7 : jackal029.z=-200:jackal030.z=-200:jackal031.z=-200:jackal032.z=-200:jackal033.z=-200:jackal034.z=-200:jackal035.z=0
			end Select
End Sub


Sub floode1timer_Timer
countr20 = countr20 + 1 : If Countr20 > 8 then Countr20 = 1 : end If 
select case countr20
				case 1 : floode001.z=0:floode002.z=-200:floode003.z=-200:floode004.z=-200:floode005.z=-200:floode006.z=-200:floode007.z=-200:floode008.z=-200
				case 2 : floode001.z=-200:floode002.z=0:floode003.z=-200:floode004.z=-200:floode005.z=-200:floode006.z=-200:floode007.z=-200:floode008.z=-200
				case 3 : floode001.z=-200:floode002.z=-200:floode003.z=0:floode004.z=-200:floode005.z=-200:floode006.z=-200:floode007.z=-200:floode008.z=-200
				case 4 : floode001.z=-200:floode002.z=-200:floode003.z=-200:floode004.z=0:floode005.z=-200:floode006.z=-200:floode007.z=-200:floode008.z=-200
				case 5 : floode001.z=-200:floode002.z=-200:floode003.z=-200:floode004.z=-200:floode005.z=0:floode006.z=-200:floode007.z=-200:floode008.z=-200
				case 6 : floode001.z=-200:floode002.z=-200:floode003.z=-200:floode004.z=-200:floode005.z=-200:floode006.z=0:floode007.z=-200:floode008.z=-200
				case 7 : floode001.z=-200:floode002.z=-200:floode003.z=-200:floode004.z=-200:floode005.z=-200:floode006.z=-200:floode007.z=0:floode008.z=-200
				case 8 : floode001.z=-200:floode002.z=-200:floode003.z=-200:floode004.z=-200:floode005.z=-200:floode006.z=-200:floode007.z=-200:floode008.z=0
			end Select
End Sub

Sub floode2timer_Timer
countr21 = countr21 + 1 : If Countr21 > 8 then Countr21 = 1 : end If 
select case countr21
				case 1 : floode009.z=0:floode010.z=-200:floode011.z=-200:floode012.z=-200:floode013.z=-200:floode014.z=-200:floode015.z=-200:floode016.z=-200
				case 2 : floode009.z=-200:floode010.z=0:floode011.z=-200:floode012.z=-200:floode013.z=-200:floode014.z=-200:floode015.z=-200:floode016.z=-200
				case 3 : floode009.z=-200:floode010.z=-200:floode011.z=0:floode012.z=-200:floode013.z=-200:floode014.z=-200:floode015.z=-200:floode016.z=-200
				case 4 : floode009.z=-200:floode010.z=-200:floode011.z=-200:floode012.z=0:floode013.z=-200:floode014.z=-200:floode015.z=-200:floode016.z=-200
				case 5 : floode009.z=-200:floode010.z=-200:floode011.z=-200:floode012.z=-200:floode013.z=0:floode014.z=-200:floode015.z=-200:floode016.z=-200
				case 6 : floode009.z=-200:floode010.z=-200:floode011.z=-200:floode012.z=-200:floode013.z=-200:floode014.z=0:floode015.z=-200:floode016.z=-200
				case 7 : floode009.z=-200:floode010.z=-200:floode011.z=-200:floode012.z=-200:floode013.z=-200:floode014.z=-200:floode015.z=0:floode016.z=-200
				case 8 : floode009.z=-200:floode010.z=-200:floode011.z=-200:floode012.z=-200:floode013.z=-200:floode014.z=-200:floode015.z=-200:floode016.z=0
			end Select
End Sub

Sub floode3timer_Timer
countr22 = countr22 + 1 : If Countr22 > 8 then Countr22 = 1 : end If 
select case countr22
				case 1 : floode017.z=0:floode018.z=-200:floode019.z=-200:floode020.z=-200:floode021.z=-200:floode022.z=-200:floode023.z=-200:floode024.z=-200
				case 2 : floode017.z=-200:floode018.z=0:floode019.z=-200:floode020.z=-200:floode021.z=-200:floode022.z=-200:floode023.z=-200:floode024.z=-200
				case 3 : floode017.z=-200:floode018.z=-200:floode019.z=0:floode020.z=-200:floode021.z=-200:floode022.z=-200:floode023.z=-200:floode024.z=-200
				case 4 : floode017.z=-200:floode018.z=-200:floode019.z=-200:floode020.z=0:floode021.z=-200:floode022.z=-200:floode023.z=-200:floode024.z=-200
				case 5 : floode017.z=-200:floode018.z=-200:floode019.z=-200:floode020.z=-200:floode021.z=0:floode022.z=-200:floode023.z=-200:floode024.z=-200
				case 6 : floode017.z=-200:floode018.z=-200:floode019.z=-200:floode020.z=-200:floode021.z=-200:floode022.z=0:floode023.z=-200:floode024.z=-200
				case 7 : floode017.z=-200:floode018.z=-200:floode019.z=-200:floode020.z=-200:floode021.z=-200:floode022.z=-200:floode023.z=0:floode024.z=-200
				case 8 : floode017.z=-200:floode018.z=-200:floode019.z=-200:floode020.z=-200:floode021.z=-200:floode022.z=-200:floode023.z=-200:floode024.z=0
			end Select
End Sub

Sub floode4timer_Timer
countr23 = countr23 + 1 : If Countr23 > 8 then Countr23 = 1 : end If 
select case countr23
				case 1 : floode025.z=0:floode026.z=-200:floode027.z=-200:floode028.z=-200:floode029.z=-200:floode030.z=-200:floode031.z=-200:floode032.z=-200
				case 2 : floode025.z=-200:floode026.z=0:floode027.z=-200:floode028.z=-200:floode029.z=-200:floode030.z=-200:floode031.z=-200:floode032.z=-200
				case 3 : floode025.z=-200:floode026.z=-200:floode027.z=0:floode028.z=-200:floode029.z=-200:floode030.z=-200:floode031.z=-200:floode032.z=-200
				case 4 : floode025.z=-200:floode026.z=-200:floode027.z=-200:floode028.z=0:floode029.z=-200:floode030.z=-200:floode031.z=-200:floode032.z=-200
				case 5 : floode025.z=-200:floode026.z=-200:floode027.z=-200:floode028.z=-200:floode029.z=0:floode030.z=-200:floode031.z=-200:floode032.z=-200
				case 6 : floode025.z=-200:floode026.z=-200:floode027.z=-200:floode028.z=-200:floode029.z=-200:floode030.z=0:floode031.z=-200:floode032.z=-200
				case 7 : floode025.z=-200:floode026.z=-200:floode027.z=-200:floode028.z=-200:floode029.z=-200:floode030.z=-200:floode031.z=0:floode032.z=-200
				case 8 : floode025.z=-200:floode026.z=-200:floode027.z=-200:floode028.z=-200:floode029.z=-200:floode030.z=-200:floode031.z=-200:floode032.z=0
			end Select
End Sub

Sub floode5timer_Timer
countr24 = countr24 + 1 : If Countr24 > 8 then Countr24 = 1 : end If 
select case countr24
				case 1 : floode033.z=0:floode034.z=-200:floode035.z=-200:floode036.z=-200:floode037.z=-200:floode038.z=-200:floode039.z=-200:floode040.z=-200
				case 2 : floode033.z=-200:floode034.z=0:floode035.z=-200:floode036.z=-200:floode037.z=-200:floode038.z=-200:floode039.z=-200:floode040.z=-200
				case 3 : floode033.z=-200:floode034.z=-200:floode035.z=0:floode036.z=-200:floode037.z=-200:floode038.z=-200:floode039.z=-200:floode040.z=-200
				case 4 : floode033.z=-200:floode034.z=-200:floode035.z=-200:floode036.z=0:floode037.z=-200:floode038.z=-200:floode039.z=-200:floode040.z=-200
				case 5 : floode033.z=-200:floode034.z=-200:floode035.z=-200:floode036.z=-200:floode037.z=0:floode038.z=-200:floode039.z=-200:floode040.z=-200
				case 6 : floode033.z=-200:floode034.z=-200:floode035.z=-200:floode036.z=-200:floode037.z=-200:floode038.z=0:floode039.z=-200:floode040.z=-200
				case 7 : floode033.z=-200:floode034.z=-200:floode035.z=-200:floode036.z=-200:floode037.z=-200:floode038.z=-200:floode039.z=0:floode040.z=-200
				case 8 : floode033.z=-200:floode034.z=-200:floode035.z=-200:floode036.z=-200:floode037.z=-200:floode038.z=-200:floode039.z=-200:floode040.z=0
			end Select
End Sub


Sub floodc1timer_Timer
countr25 = countr25 + 1 : If Countr25 > 8 then Countr25 = 1 : end If 
select case countr25
				case 1 : floodc001.z=0:floodc002.z=-200:floodc003.z=-200:floodc004.z=-200:floodc005.z=-200:floodc006.z=-200:floodc007.z=-200:floodc008.z=-200
				case 2 : floodc001.z=-200:floodc002.z=0:floodc003.z=-200:floodc004.z=-200:floodc005.z=-200:floodc006.z=-200:floodc007.z=-200:floodc008.z=-200
				case 3 : floodc001.z=-200:floodc002.z=-200:floodc003.z=0:floodc004.z=-200:floodc005.z=-200:floodc006.z=-200:floodc007.z=-200:floodc008.z=-200
				case 4 : floodc001.z=-200:floodc002.z=-200:floodc003.z=-200:floodc004.z=0:floodc005.z=-200:floodc006.z=-200:floodc007.z=-200:floodc008.z=-200
				case 5 : floodc001.z=-200:floodc002.z=-200:floodc003.z=-200:floodc004.z=-200:floodc005.z=0:floodc006.z=-200:floodc007.z=-200:floodc008.z=-200
				case 6 : floodc001.z=-200:floodc002.z=-200:floodc003.z=-200:floodc004.z=-200:floodc005.z=-200:floodc006.z=0:floodc007.z=-200:floodc008.z=-200
				case 7 : floodc001.z=-200:floodc002.z=-200:floodc003.z=-200:floodc004.z=-200:floodc005.z=-200:floodc006.z=-200:floodc007.z=0:floodc008.z=-200
				case 8 : floodc001.z=-200:floodc002.z=-200:floodc003.z=-200:floodc004.z=-200:floodc005.z=-200:floodc006.z=-200:floodc007.z=-200:floodc008.z=0
			end Select
End Sub

Sub floodc2timer_Timer
countr26 = countr26 + 1 : If Countr26 > 8 then Countr26 = 1 : end If 
select case countr26
				case 1 : floodc009.z=0:floodc010.z=-200:floodc011.z=-200:floodc012.z=-200:floodc013.z=-200:floodc014.z=-200:floodc015.z=-200:floodc016.z=-200
				case 2 : floodc009.z=-200:floodc010.z=0:floodc011.z=-200:floodc012.z=-200:floodc013.z=-200:floodc014.z=-200:floodc015.z=-200:floodc016.z=-200
				case 3 : floodc009.z=-200:floodc010.z=-200:floodc011.z=0:floodc012.z=-200:floodc013.z=-200:floodc014.z=-200:floodc015.z=-200:floodc016.z=-200
				case 4 : floodc009.z=-200:floodc010.z=-200:floodc011.z=-200:floodc012.z=0:floodc013.z=-200:floodc014.z=-200:floodc015.z=-200:floodc016.z=-200
				case 5 : floodc009.z=-200:floodc010.z=-200:floodc011.z=-200:floodc012.z=-200:floodc013.z=0:floodc014.z=-200:floodc015.z=-200:floodc016.z=-200
				case 6 : floodc009.z=-200:floodc010.z=-200:floodc011.z=-200:floodc012.z=-200:floodc013.z=-200:floodc014.z=0:floodc015.z=-200:floodc016.z=-200
				case 7 : floodc009.z=-200:floodc010.z=-200:floodc011.z=-200:floodc012.z=-200:floodc013.z=-200:floodc014.z=-200:floodc015.z=0:floodc016.z=-200
				case 8 : floodc009.z=-200:floodc010.z=-200:floodc011.z=-200:floodc012.z=-200:floodc013.z=-200:floodc014.z=-200:floodc015.z=-200:floodc016.z=0
			end Select
End Sub

Sub floodc3timer_Timer
countr27 = countr27 + 1 : If Countr27 > 8 then Countr27 = 1 : end If 
select case countr27
				case 1 : floodc017.z=0:floodc018.z=-200:floodc019.z=-200:floodc020.z=-200:floodc021.z=-200:floodc022.z=-200:floodc023.z=-200:floodc024.z=-200
				case 2 : floodc017.z=-200:floodc018.z=0:floodc019.z=-200:floodc020.z=-200:floodc021.z=-200:floodc022.z=-200:floodc023.z=-200:floodc024.z=-200
				case 3 : floodc017.z=-200:floodc018.z=-200:floodc019.z=0:floodc020.z=-200:floodc021.z=-200:floodc022.z=-200:floodc023.z=-200:floodc024.z=-200
				case 4 : floodc017.z=-200:floodc018.z=-200:floodc019.z=-200:floodc020.z=0:floodc021.z=-200:floodc022.z=-200:floodc023.z=-200:floodc024.z=-200
				case 5 : floodc017.z=-200:floodc018.z=-200:floodc019.z=-200:floodc020.z=-200:floodc021.z=0:floodc022.z=-200:floodc023.z=-200:floodc024.z=-200
				case 6 : floodc017.z=-200:floodc018.z=-200:floodc019.z=-200:floodc020.z=-200:floodc021.z=-200:floodc022.z=0:floodc023.z=-200:floodc024.z=-200
				case 7 : floodc017.z=-200:floodc018.z=-200:floodc019.z=-200:floodc020.z=-200:floodc021.z=-200:floodc022.z=-200:floodc023.z=0:floodc024.z=-200
				case 8 : floodc017.z=-200:floodc018.z=-200:floodc019.z=-200:floodc020.z=-200:floodc021.z=-200:floodc022.z=-200:floodc023.z=-200:floodc024.z=0
			end Select
End Sub

Sub floodc4timer_Timer
countr28 = countr28 + 1 : If Countr28 > 8 then Countr28 = 1 : end If 
select case countr28
				case 1 : floodc025.z=0:floodc026.z=-200:floodc027.z=-200:floodc028.z=-200:floodc029.z=-200:floodc030.z=-200:floodc031.z=-200:floodc032.z=-200
				case 2 : floodc025.z=-200:floodc026.z=0:floodc027.z=-200:floodc028.z=-200:floodc029.z=-200:floodc030.z=-200:floodc031.z=-200:floodc032.z=-200
				case 3 : floodc025.z=-200:floodc026.z=-200:floodc027.z=0:floodc028.z=-200:floodc029.z=-200:floodc030.z=-200:floodc031.z=-200:floodc032.z=-200
				case 4 : floodc025.z=-200:floodc026.z=-200:floodc027.z=-200:floodc028.z=0:floodc029.z=-200:floodc030.z=-200:floodc031.z=-200:floodc032.z=-200
				case 5 : floodc025.z=-200:floodc026.z=-200:floodc027.z=-200:floodc028.z=-200:floodc029.z=0:floodc030.z=-200:floodc031.z=-200:floodc032.z=-200
				case 6 : floodc025.z=-200:floodc026.z=-200:floodc027.z=-200:floodc028.z=-200:floodc029.z=-200:floodc030.z=0:floodc031.z=-200:floodc032.z=-200
				case 7 : floodc025.z=-200:floodc026.z=-200:floodc027.z=-200:floodc028.z=-200:floodc029.z=-200:floodc030.z=-200:floodc031.z=0:floodc032.z=-200
				case 8 : floodc025.z=-200:floodc026.z=-200:floodc027.z=-200:floodc028.z=-200:floodc029.z=-200:floodc030.z=-200:floodc031.z=-200:floodc032.z=0
			end Select
End Sub

Sub floodc5timer_Timer
countr29 = countr29 + 1 : If Countr29 > 8 then Countr29 = 1 : end If 
select case countr29
				case 1 : floodc033.z=0:floodc034.z=-200:floodc035.z=-200:floodc036.z=-200:floodc037.z=-200:floodc038.z=-200:floodc039.z=-200:floodc040.z=-200
				case 2 : floodc033.z=-200:floodc034.z=0:floodc035.z=-200:floodc036.z=-200:floodc037.z=-200:floodc038.z=-200:floodc039.z=-200:floodc040.z=-200
				case 3 : floodc033.z=-200:floodc034.z=-200:floodc035.z=0:floodc036.z=-200:floodc037.z=-200:floodc038.z=-200:floodc039.z=-200:floodc040.z=-200
				case 4 : floodc033.z=-200:floodc034.z=-200:floodc035.z=-200:floodc036.z=0:floodc037.z=-200:floodc038.z=-200:floodc039.z=-200:floodc040.z=-200
				case 5 : floodc033.z=-200:floodc034.z=-200:floodc035.z=-200:floodc036.z=-200:floodc037.z=0:floodc038.z=-200:floodc039.z=-200:floodc040.z=-200
				case 6 : floodc033.z=-200:floodc034.z=-200:floodc035.z=-200:floodc036.z=-200:floodc037.z=-200:floodc038.z=0:floodc039.z=-200:floodc040.z=-200
				case 7 : floodc033.z=-200:floodc034.z=-200:floodc035.z=-200:floodc036.z=-200:floodc037.z=-200:floodc038.z=-200:floodc039.z=0:floodc040.z=-200
				case 8 : floodc033.z=-200:floodc034.z=-200:floodc035.z=-200:floodc036.z=-200:floodc037.z=-200:floodc038.z=-200:floodc039.z=-200:floodc040.z=0
			end Select
End Sub

Sub chblLtimer_Timer
countr30 = countr30 + 1 : If Countr30 > 8 then stopblostaniL'Countr30 = 1 : end If 
select case countr30
				case 1 : cfballlostL001.z=53:cfballlostL002.z=-200:cfballlostL003.z=-200:cfballlostL004.z=-200:cfballlostL005.z=-200:cfballlostL006.z=-200:cfballlostL007.z=-200:cfballlostL008.z=-200
				case 2 : cfballlostL001.z=-200:cfballlostL002.z=53:cfballlostL003.z=-200:cfballlostL004.z=-200:cfballlostL005.z=-200:cfballlostL006.z=-200:cfballlostL007.z=-200:cfballlostL008.z=-200
				case 3 : cfballlostL001.z=-200:cfballlostL002.z=-200:cfballlostL003.z=53:cfballlostL004.z=-200:cfballlostL005.z=-200:cfballlostL006.z=-200:cfballlostL007.z=-200:cfballlostL008.z=-200
				case 4 : cfballlostL001.z=-200:cfballlostL002.z=-200:cfballlostL003.z=-200:cfballlostL004.z=53:cfballlostL005.z=-200:cfballlostL006.z=-200:cfballlostL007.z=-200:cfballlostL008.z=-200
				case 5 : cfballlostL001.z=-200:cfballlostL002.z=-200:cfballlostL003.z=-200:cfballlostL004.z=-200:cfballlostL005.z=53:cfballlostL006.z=-200:cfballlostL007.z=-200:cfballlostL008.z=-200
				case 6 : cfballlostL001.z=-200:cfballlostL002.z=-200:cfballlostL003.z=-200:cfballlostL004.z=-200:cfballlostL005.z=-200:cfballlostL006.z=53:cfballlostL007.z=-200:cfballlostL008.z=-200
				case 7 : cfballlostL001.z=-200:cfballlostL002.z=-200:cfballlostL003.z=-200:cfballlostL004.z=-200:cfballlostL005.z=-200:cfballlostL006.z=-200:cfballlostL007.z=53:cfballlostL008.z=-200
				case 8 : cfballlostL001.z=-200:cfballlostL002.z=-200:cfballlostL003.z=-200:cfballlostL004.z=-200:cfballlostL005.z=-200:cfballlostL006.z=-200:cfballlostL007.z=-200:cfballlostL008.z=53
			end Select
End Sub

Sub chblRtimer_Timer
countr31 = countr31 + 1 : If Countr31 > 8 then stopblostanir'Countr31 = 1 : end If 
select case countr31
				case 1 : cfballlostR001.z=53:cfballlostR002.z=-200:cfballlostR003.z=-200:cfballlostR004.z=-200:cfballlostR005.z=-200:cfballlostR006.z=-200:cfballlostR007.z=-200:cfballlostR008.z=-200
				case 2 : cfballlostR001.z=-200:cfballlostR002.z=53:cfballlostR003.z=-200:cfballlostR004.z=-200:cfballlostR005.z=-200:cfballlostR006.z=-200:cfballlostR007.z=-200:cfballlostR008.z=-200
				case 3 : cfballlostR001.z=-200:cfballlostR002.z=-200:cfballlostR003.z=53:cfballlostR004.z=-200:cfballlostR005.z=-200:cfballlostR006.z=-200:cfballlostR007.z=-200:cfballlostR008.z=-200
				case 4 : cfballlostR001.z=-200:cfballlostR002.z=-200:cfballlostR003.z=-200:cfballlostR004.z=53:cfballlostR005.z=-200:cfballlostR006.z=-200:cfballlostR007.z=-200:cfballlostR008.z=-200
				case 5 : cfballlostR001.z=-200:cfballlostR002.z=-200:cfballlostR003.z=-200:cfballlostR004.z=-200:cfballlostR005.z=53:cfballlostR006.z=-200:cfballlostR007.z=-200:cfballlostR008.z=-200
				case 6 : cfballlostR001.z=-200:cfballlostR002.z=-200:cfballlostR003.z=-200:cfballlostR004.z=-200:cfballlostR005.z=-200:cfballlostR006.z=53:cfballlostR007.z=-200:cfballlostR008.z=-200
				case 7 : cfballlostR001.z=-200:cfballlostR002.z=-200:cfballlostR003.z=-200:cfballlostR004.z=-200:cfballlostR005.z=-200:cfballlostR006.z=-200:cfballlostR007.z=53:cfballlostR008.z=-200
				case 8 : cfballlostR001.z=-200:cfballlostR002.z=-200:cfballlostR003.z=-200:cfballlostR004.z=-200:cfballlostR005.z=-200:cfballlostR006.z=-200:cfballlostR007.z=-200:cfballlostR008.z=53
			end Select
End Sub

Sub chmfLtimer_Timer
countr32 = countr32 + 1 : If Countr32 > 8 then stopmfailedaniL'Countr32 = 1 : end If 
select case countr32
				case 1 : cfmisfailedL001.z=53:cfmisfailedL002.z=-200:cfmisfailedL003.z=-200:cfmisfailedL004.z=-200:cfmisfailedL005.z=-200:cfmisfailedL006.z=-200:cfmisfailedL007.z=-200:cfmisfailedL008.z=-200
				case 2 : cfmisfailedL001.z=-200:cfmisfailedL002.z=53:cfmisfailedL003.z=-200:cfmisfailedL004.z=-200:cfmisfailedL005.z=-200:cfmisfailedL006.z=-200:cfmisfailedL007.z=-200:cfmisfailedL008.z=-200
				case 3 : cfmisfailedL001.z=-200:cfmisfailedL002.z=-200:cfmisfailedL003.z=53:cfmisfailedL004.z=-200:cfmisfailedL005.z=-200:cfmisfailedL006.z=-200:cfmisfailedL007.z=-200:cfmisfailedL008.z=-200
				case 4 : cfmisfailedL001.z=-200:cfmisfailedL002.z=-200:cfmisfailedL003.z=-200:cfmisfailedL004.z=53:cfmisfailedL005.z=-200:cfmisfailedL006.z=-200:cfmisfailedL007.z=-200:cfmisfailedL008.z=-200
				case 5 : cfmisfailedL001.z=-200:cfmisfailedL002.z=-200:cfmisfailedL003.z=-200:cfmisfailedL004.z=-200:cfmisfailedL005.z=53:cfmisfailedL006.z=-200:cfmisfailedL007.z=-200:cfmisfailedL008.z=-200
				case 6 : cfmisfailedL001.z=-200:cfmisfailedL002.z=-200:cfmisfailedL003.z=-200:cfmisfailedL004.z=-200:cfmisfailedL005.z=-200:cfmisfailedL006.z=53:cfmisfailedL007.z=-200:cfmisfailedL008.z=-200
				case 7 : cfmisfailedL001.z=-200:cfmisfailedL002.z=-200:cfmisfailedL003.z=-200:cfmisfailedL004.z=-200:cfmisfailedL005.z=-200:cfmisfailedL006.z=-200:cfmisfailedL007.z=53:cfmisfailedL008.z=-200
				case 8 : cfmisfailedL001.z=-200:cfmisfailedL002.z=-200:cfmisfailedL003.z=-200:cfmisfailedL004.z=-200:cfmisfailedL005.z=-200:cfmisfailedL006.z=-200:cfmisfailedL007.z=-200:cfmisfailedL008.z=53
			end Select
End Sub

Sub chmfRtimer_Timer
countr33 = countr33 + 1 : If Countr33 > 8 then stopmfailedanir'Countr33 = 1 : end If 
select case countr33
				case 1 : cfmisfailedR001.z=53:cfmisfailedR002.z=-200:cfmisfailedR003.z=-200:cfmisfailedR004.z=-200:cfmisfailedR005.z=-200:cfmisfailedR006.z=-200:cfmisfailedR007.z=-200:cfmisfailedR008.z=-200
				case 2 : cfmisfailedR001.z=-200:cfmisfailedR002.z=53:cfmisfailedR003.z=-200:cfmisfailedR004.z=-200:cfmisfailedR005.z=-200:cfmisfailedR006.z=-200:cfmisfailedR007.z=-200:cfmisfailedR008.z=-200
				case 3 : cfmisfailedR001.z=-200:cfmisfailedR002.z=-200:cfmisfailedR003.z=53:cfmisfailedR004.z=-200:cfmisfailedR005.z=-200:cfmisfailedR006.z=-200:cfmisfailedR007.z=-200:cfmisfailedR008.z=-200
				case 4 : cfmisfailedR001.z=-200:cfmisfailedR002.z=-200:cfmisfailedR003.z=-200:cfmisfailedR004.z=53:cfmisfailedR005.z=-200:cfmisfailedR006.z=-200:cfmisfailedR007.z=-200:cfmisfailedR008.z=-200
				case 5 : cfmisfailedR001.z=-200:cfmisfailedR002.z=-200:cfmisfailedR003.z=-200:cfmisfailedR004.z=-200:cfmisfailedR005.z=53:cfmisfailedR006.z=-200:cfmisfailedR007.z=-200:cfmisfailedR008.z=-200
				case 6 : cfmisfailedR001.z=-200:cfmisfailedR002.z=-200:cfmisfailedR003.z=-200:cfmisfailedR004.z=-200:cfmisfailedR005.z=-200:cfmisfailedR006.z=53:cfmisfailedR007.z=-200:cfmisfailedR008.z=-200
				case 7 : cfmisfailedR001.z=-200:cfmisfailedR002.z=-200:cfmisfailedR003.z=-200:cfmisfailedR004.z=-200:cfmisfailedR005.z=-200:cfmisfailedR006.z=-200:cfmisfailedR007.z=53:cfmisfailedR008.z=-200
				case 8 : cfmisfailedR001.z=-200:cfmisfailedR002.z=-200:cfmisfailedR003.z=-200:cfmisfailedR004.z=-200:cfmisfailedR005.z=-200:cfmisfailedR006.z=-200:cfmisfailedR007.z=-200:cfmisfailedR008.z=53
			end Select
End Sub

Sub chbsLtimer_Timer
countr34 = countr34 + 1 : If Countr34 > 9 then stopbonusaniL 'Countr34 = 1 : end If 
select case countr34
				case 1 : cfbonusL001.z=53:cfbonusL002.z=-200:cfbonusL003.z=-200:cfbonusL004.z=-200:cfbonusL005.z=-200:cfbonusL006.z=-200:cfbonusL007.z=-200:cfbonusL008.z=-200:cfbonusL009.z=-200
				case 2 : cfbonusL001.z=-200:cfbonusL002.z=53:cfbonusL003.z=-200:cfbonusL004.z=-200:cfbonusL005.z=-200:cfbonusL006.z=-200:cfbonusL007.z=-200:cfbonusL008.z=-200:cfbonusL009.z=-200
				case 3 : cfbonusL001.z=-200:cfbonusL002.z=-200:cfbonusL003.z=53:cfbonusL004.z=-200:cfbonusL005.z=-200:cfbonusL006.z=-200:cfbonusL007.z=-200:cfbonusL008.z=-200:cfbonusL009.z=-200
				case 4 : cfbonusL001.z=-200:cfbonusL002.z=-200:cfbonusL003.z=-200:cfbonusL004.z=53:cfbonusL005.z=-200:cfbonusL006.z=-200:cfbonusL007.z=-200:cfbonusL008.z=-200:cfbonusL009.z=-200
				case 5 : cfbonusL001.z=-200:cfbonusL002.z=-200:cfbonusL003.z=-200:cfbonusL004.z=-200:cfbonusL005.z=53:cfbonusL006.z=-200:cfbonusL007.z=-200:cfbonusL008.z=-200:cfbonusL009.z=-200
				case 6 : cfbonusL001.z=-200:cfbonusL002.z=-200:cfbonusL003.z=-200:cfbonusL004.z=-200:cfbonusL005.z=-200:cfbonusL006.z=53:cfbonusL007.z=-200:cfbonusL008.z=-200:cfbonusL009.z=-200
				case 7 : cfbonusL001.z=-200:cfbonusL002.z=-200:cfbonusL003.z=-200:cfbonusL004.z=-200:cfbonusL005.z=-200:cfbonusL006.z=-200:cfbonusL007.z=53:cfbonusL008.z=-200:cfbonusL009.z=-200
				case 8 : cfbonusL001.z=-200:cfbonusL002.z=-200:cfbonusL003.z=-200:cfbonusL004.z=-200:cfbonusL005.z=-200:cfbonusL006.z=-200:cfbonusL007.z=-200:cfbonusL008.z=53:cfbonusL009.z=-200
				case 9 : cfbonusL001.z=-200:cfbonusL002.z=-200:cfbonusL003.z=-200:cfbonusL004.z=-200:cfbonusL005.z=-200:cfbonusL006.z=-200:cfbonusL007.z=-200:cfbonusL008.z=-200:cfbonusL009.z=53
			end Select
End Sub

Sub chbsRtimer_Timer
countr35 = countr35 + 1 : If Countr35 > 9 then stopbonusanir 'Countr35 = 1 : end If 
select case countr35
				case 1 : cfbonusR001.z=53:cfbonusR002.z=-200:cfbonusR003.z=-200:cfbonusR004.z=-200:cfbonusR005.z=-200:cfbonusR006.z=-200:cfbonusR007.z=-200:cfbonusR008.z=-200:cfbonusR009.z=-200
				case 2 : cfbonusR001.z=-200:cfbonusR002.z=53:cfbonusR003.z=-200:cfbonusR004.z=-200:cfbonusR005.z=-200:cfbonusR006.z=-200:cfbonusR007.z=-200:cfbonusR008.z=-200:cfbonusR009.z=-200
				case 3 : cfbonusR001.z=-200:cfbonusR002.z=-200:cfbonusR003.z=53:cfbonusR004.z=-200:cfbonusR005.z=-200:cfbonusR006.z=-200:cfbonusR007.z=-200:cfbonusR008.z=-200:cfbonusR009.z=-200
				case 4 : cfbonusR001.z=-200:cfbonusR002.z=-200:cfbonusR003.z=-200:cfbonusR004.z=53:cfbonusR005.z=-200:cfbonusR006.z=-200:cfbonusR007.z=-200:cfbonusR008.z=-200:cfbonusR009.z=-200
				case 5 : cfbonusR001.z=-200:cfbonusR002.z=-200:cfbonusR003.z=-200:cfbonusR004.z=-200:cfbonusR005.z=53:cfbonusR006.z=-200:cfbonusR007.z=-200:cfbonusR008.z=-200:cfbonusR009.z=-200
				case 6 : cfbonusR001.z=-200:cfbonusR002.z=-200:cfbonusR003.z=-200:cfbonusR004.z=-200:cfbonusR005.z=-200:cfbonusR006.z=53:cfbonusR007.z=-200:cfbonusR008.z=-200:cfbonusR009.z=-200
				case 7 : cfbonusR001.z=-200:cfbonusR002.z=-200:cfbonusR003.z=-200:cfbonusR004.z=-200:cfbonusR005.z=-200:cfbonusR006.z=-200:cfbonusR007.z=53:cfbonusR008.z=-200:cfbonusR009.z=-200
				case 8 : cfbonusR001.z=-200:cfbonusR002.z=-200:cfbonusR003.z=-200:cfbonusR004.z=-200:cfbonusR005.z=-200:cfbonusR006.z=-200:cfbonusR007.z=-200:cfbonusR008.z=53:cfbonusR009.z=-200
				case 9 : cfbonusR001.z=-200:cfbonusR002.z=-200:cfbonusR003.z=-200:cfbonusR004.z=-200:cfbonusR005.z=-200:cfbonusR006.z=-200:cfbonusR007.z=-200:cfbonusR008.z=-200:cfbonusR009.z=53
			end Select
End Sub

Sub chchLtimer_Timer
countr37 = countr37 + 1 : If Countr37 > 12 then stopcheeraniL 'Countr37 = 1 : end If 
select case countr37
				case 1 : cfcheerL001.z=53:cfcheerL002.z=-200:cfcheerL003.z=-200:cfcheerL004.z=-200:cfcheerL005.z=-200:cfcheerL006.z=-200:cfcheerL007.z=-200:cfcheerL008.z=-200:cfcheerL009.z=-200:cfcheerL010.z=-200:cfcheerL011.z=-200:cfcheerL012.z=-200
				case 2 : cfcheerL001.z=-200:cfcheerL002.z=53:cfcheerL003.z=-200:cfcheerL004.z=-200:cfcheerL005.z=-200:cfcheerL006.z=-200:cfcheerL007.z=-200:cfcheerL008.z=-200:cfcheerL009.z=-200:cfcheerL010.z=-200:cfcheerL011.z=-200:cfcheerL012.z=-200
				case 3 : cfcheerL001.z=-200:cfcheerL002.z=-200:cfcheerL003.z=53:cfcheerL004.z=-200:cfcheerL005.z=-200:cfcheerL006.z=-200:cfcheerL007.z=-200:cfcheerL008.z=-200:cfcheerL009.z=-200:cfcheerL010.z=-200:cfcheerL011.z=-200:cfcheerL012.z=-200
				case 4 : cfcheerL001.z=-200:cfcheerL002.z=-200:cfcheerL003.z=-200:cfcheerL004.z=53:cfcheerL005.z=-200:cfcheerL006.z=-200:cfcheerL007.z=-200:cfcheerL008.z=-200:cfcheerL009.z=-200:cfcheerL010.z=-200:cfcheerL011.z=-200:cfcheerL012.z=-200
				case 5 : cfcheerL001.z=-200:cfcheerL002.z=-200:cfcheerL003.z=-200:cfcheerL004.z=-200:cfcheerL005.z=53:cfcheerL006.z=-200:cfcheerL007.z=-200:cfcheerL008.z=-200:cfcheerL009.z=-200:cfcheerL010.z=-200:cfcheerL011.z=-200:cfcheerL012.z=-200
				case 6 : cfcheerL001.z=-200:cfcheerL002.z=-200:cfcheerL003.z=-200:cfcheerL004.z=-200:cfcheerL005.z=-200:cfcheerL006.z=53:cfcheerL007.z=-200:cfcheerL008.z=-200:cfcheerL009.z=-200:cfcheerL010.z=-200:cfcheerL011.z=-200:cfcheerL012.z=-200
				case 7 : cfcheerL001.z=-200:cfcheerL002.z=-200:cfcheerL003.z=-200:cfcheerL004.z=-200:cfcheerL005.z=-200:cfcheerL006.z=-200:cfcheerL007.z=53:cfcheerL008.z=-200:cfcheerL009.z=-200:cfcheerL010.z=-200:cfcheerL011.z=-200:cfcheerL012.z=-200
				case 8 : cfcheerL001.z=-200:cfcheerL002.z=-200:cfcheerL003.z=-200:cfcheerL004.z=-200:cfcheerL005.z=-200:cfcheerL006.z=-200:cfcheerL007.z=-200:cfcheerL008.z=53:cfcheerL009.z=-200:cfcheerL010.z=-200:cfcheerL011.z=-200:cfcheerL012.z=-200
				case 9 : cfcheerL001.z=-200:cfcheerL002.z=-200:cfcheerL003.z=-200:cfcheerL004.z=-200:cfcheerL005.z=-200:cfcheerL006.z=-200:cfcheerL007.z=-200:cfcheerL008.z=-200:cfcheerL009.z=53:cfcheerL010.z=-200:cfcheerL011.z=-200:cfcheerL012.z=-200
				case 10 : cfcheerL001.z=-200:cfcheerL002.z=-200:cfcheerL003.z=-200:cfcheerL004.z=-200:cfcheerL005.z=-200:cfcheerL006.z=-200:cfcheerL007.z=-200:cfcheerL008.z=-200:cfcheerL009.z=-200:cfcheerL010.z=53:cfcheerL011.z=-200:cfcheerL012.z=-200
				case 11 : cfcheerL001.z=-200:cfcheerL002.z=-200:cfcheerL003.z=-200:cfcheerL004.z=-200:cfcheerL005.z=-200:cfcheerL006.z=-200:cfcheerL007.z=-200:cfcheerL008.z=-200:cfcheerL009.z=-200:cfcheerL010.z=-200:cfcheerL011.z=53:cfcheerL012.z=-200
				case 12 : cfcheerL001.z=-200:cfcheerL002.z=-200:cfcheerL003.z=-200:cfcheerL004.z=-200:cfcheerL005.z=-200:cfcheerL006.z=-200:cfcheerL007.z=-200:cfcheerL008.z=-200:cfcheerL009.z=-200:cfcheerL010.z=-200:cfcheerL011.z=-200:cfcheerL012.z=53
			end Select
End Sub

Sub chchRtimer_Timer
countr38 = countr38 + 1 : If Countr38 > 12 then stopcheeranir'Countr38 = 1 : end If 
select case countr38
				case 1 : cfcheerR001.z=53:cfcheerR002.z=-200:cfcheerR003.z=-200:cfcheerR004.z=-200:cfcheerR005.z=-200:cfcheerR006.z=-200:cfcheerR007.z=-200:cfcheerR008.z=-200:cfcheerR009.z=-200:cfcheerR010.z=-200:cfcheerR011.z=-200:cfcheerR012.z=-200
				case 2 : cfcheerR001.z=-200:cfcheerR002.z=53:cfcheerR003.z=-200:cfcheerR004.z=-200:cfcheerR005.z=-200:cfcheerR006.z=-200:cfcheerR007.z=-200:cfcheerR008.z=-200:cfcheerR009.z=-200:cfcheerR010.z=-200:cfcheerR011.z=-200:cfcheerR012.z=-200
				case 3 : cfcheerR001.z=-200:cfcheerR002.z=-200:cfcheerR003.z=53:cfcheerR004.z=-200:cfcheerR005.z=-200:cfcheerR006.z=-200:cfcheerR007.z=-200:cfcheerR008.z=-200:cfcheerR009.z=-200:cfcheerR010.z=-200:cfcheerR011.z=-200:cfcheerR012.z=-200
				case 4 : cfcheerR001.z=-200:cfcheerR002.z=-200:cfcheerR003.z=-200:cfcheerR004.z=53:cfcheerR005.z=-200:cfcheerR006.z=-200:cfcheerR007.z=-200:cfcheerR008.z=-200:cfcheerR009.z=-200:cfcheerR010.z=-200:cfcheerR011.z=-200:cfcheerR012.z=-200
				case 5 : cfcheerR001.z=-200:cfcheerR002.z=-200:cfcheerR003.z=-200:cfcheerR004.z=-200:cfcheerR005.z=53:cfcheerR006.z=-200:cfcheerR007.z=-200:cfcheerR008.z=-200:cfcheerR009.z=-200:cfcheerR010.z=-200:cfcheerR011.z=-200:cfcheerR012.z=-200
				case 6 : cfcheerR001.z=-200:cfcheerR002.z=-200:cfcheerR003.z=-200:cfcheerR004.z=-200:cfcheerR005.z=-200:cfcheerR006.z=53:cfcheerR007.z=-200:cfcheerR008.z=-200:cfcheerR009.z=-200:cfcheerR010.z=-200:cfcheerR011.z=-200:cfcheerR012.z=-200
				case 7 : cfcheerR001.z=-200:cfcheerR002.z=-200:cfcheerR003.z=-200:cfcheerR004.z=-200:cfcheerR005.z=-200:cfcheerR006.z=-200:cfcheerR007.z=53:cfcheerR008.z=-200:cfcheerR009.z=-200:cfcheerR010.z=-200:cfcheerR011.z=-200:cfcheerR012.z=-200
				case 8 : cfcheerR001.z=-200:cfcheerR002.z=-200:cfcheerR003.z=-200:cfcheerR004.z=-200:cfcheerR005.z=-200:cfcheerR006.z=-200:cfcheerR007.z=-200:cfcheerR008.z=53:cfcheerR009.z=-200:cfcheerR010.z=-200:cfcheerR011.z=-200:cfcheerR012.z=-200
				case 9 : cfcheerR001.z=-200:cfcheerR002.z=-200:cfcheerR003.z=-200:cfcheerR004.z=-200:cfcheerR005.z=-200:cfcheerR006.z=-200:cfcheerR007.z=-200:cfcheerR008.z=-200:cfcheerR009.z=53:cfcheerR010.z=-200:cfcheerR011.z=-200:cfcheerR012.z=-200
				case 10 : cfcheerR001.z=-200:cfcheerR002.z=-200:cfcheerR003.z=-200:cfcheerR004.z=-200:cfcheerR005.z=-200:cfcheerR006.z=-200:cfcheerR007.z=-200:cfcheerR008.z=-200:cfcheerR009.z=-200:cfcheerR010.z=53:cfcheerR011.z=-200:cfcheerR012.z=-200
				case 11 : cfcheerR001.z=-200:cfcheerR002.z=-200:cfcheerR003.z=-200:cfcheerR004.z=-200:cfcheerR005.z=-200:cfcheerR006.z=-200:cfcheerR007.z=-200:cfcheerR008.z=-200:cfcheerR009.z=-200:cfcheerR010.z=-200:cfcheerR011.z=53:cfcheerR012.z=-200
				case 12 : cfcheerR001.z=-200:cfcheerR002.z=-200:cfcheerR003.z=-200:cfcheerR004.z=-200:cfcheerR005.z=-200:cfcheerR006.z=-200:cfcheerR007.z=-200:cfcheerR008.z=-200:cfcheerR009.z=-200:cfcheerR010.z=-200:cfcheerR011.z=-200:cfcheerR012.z=53
			end Select
End Sub

Sub chdyLtimer_Timer
countr39 = countr39 + 1 : If Countr39 > 13 then Countr39 = 13 : end If 
select case countr39
				case 1 : cfdyL001.z=53:cfdyL002.z=-200:cfdyL003.z=-200:cfdyL004.z=-200:cfdyL005.z=-200:cfdyL006.z=-200:cfdyL007.z=-200:cfdyL008.z=-200:cfdyL009.z=-200:cfdyL010.z=-200:cfdyL011.z=-200:cfdyL012.z=-200:cfdyL012.z=-200
				case 2 : cfdyL001.z=-200:cfdyL002.z=53:cfdyL003.z=-200:cfdyL004.z=-200:cfdyL005.z=-200:cfdyL006.z=-200:cfdyL007.z=-200:cfdyL008.z=-200:cfdyL009.z=-200:cfdyL010.z=-200:cfdyL011.z=-200:cfdyL012.z=-200:cfdyL012.z=-200
				case 3 : cfdyL001.z=-200:cfdyL002.z=-200:cfdyL003.z=53:cfdyL004.z=-200:cfdyL005.z=-200:cfdyL006.z=-200:cfdyL007.z=-200:cfdyL008.z=-200:cfdyL009.z=-200:cfdyL010.z=-200:cfdyL011.z=-200:cfdyL012.z=-200:cfdyL012.z=-200
				case 4 : cfdyL001.z=-200:cfdyL002.z=-200:cfdyL003.z=-200:cfdyL004.z=53:cfdyL005.z=-200:cfdyL006.z=-200:cfdyL007.z=-200:cfdyL008.z=-200:cfdyL009.z=-200:cfdyL010.z=-200:cfdyL011.z=-200:cfdyL012.z=-200:cfdyL012.z=-200
				case 5 : cfdyL001.z=-200:cfdyL002.z=-200:cfdyL003.z=-200:cfdyL004.z=-200:cfdyL005.z=53:cfdyL006.z=-200:cfdyL007.z=-200:cfdyL008.z=-200:cfdyL009.z=-200:cfdyL010.z=-200:cfdyL011.z=-200:cfdyL012.z=-200:cfdyL012.z=-200
				case 6 : cfdyL001.z=-200:cfdyL002.z=-200:cfdyL003.z=-200:cfdyL004.z=-200:cfdyL005.z=-200:cfdyL006.z=53:cfdyL007.z=-200:cfdyL008.z=-200:cfdyL009.z=-200:cfdyL010.z=-200:cfdyL011.z=-200:cfdyL012.z=-200:cfdyL012.z=-200
				case 7 : cfdyL001.z=-200:cfdyL002.z=-200:cfdyL003.z=-200:cfdyL004.z=-200:cfdyL005.z=-200:cfdyL006.z=-200:cfdyL007.z=53:cfdyL008.z=-200:cfdyL009.z=-200:cfdyL010.z=-200:cfdyL011.z=-200:cfdyL012.z=-200:cfdyL012.z=-200
				case 8 : cfdyL001.z=-200:cfdyL002.z=-200:cfdyL003.z=-200:cfdyL004.z=-200:cfdyL005.z=-200:cfdyL006.z=-200:cfdyL007.z=-200:cfdyL008.z=53:cfdyL009.z=-200:cfdyL010.z=-200:cfdyL011.z=-200:cfdyL012.z=-200:cfdyL012.z=-200
				case 9 : cfdyL001.z=-200:cfdyL002.z=-200:cfdyL003.z=-200:cfdyL004.z=-200:cfdyL005.z=-200:cfdyL006.z=-200:cfdyL007.z=-200:cfdyL008.z=-200:cfdyL009.z=53:cfdyL010.z=-200:cfdyL011.z=-200:cfdyL012.z=-200:cfdyL012.z=-200
				case 10 : cfdyL001.z=-200:cfdyL002.z=-200:cfdyL003.z=-200:cfdyL004.z=-200:cfdyL005.z=-200:cfdyL006.z=-200:cfdyL007.z=-200:cfdyL008.z=-200:cfdyL009.z=-200:cfdyL010.z=53:cfdyL011.z=-200:cfdyL012.z=-200:cfdyL012.z=-200
				case 11 : cfdyL001.z=-200:cfdyL002.z=-200:cfdyL003.z=-200:cfdyL004.z=-200:cfdyL005.z=-200:cfdyL006.z=-200:cfdyL007.z=-200:cfdyL008.z=-200:cfdyL009.z=-200:cfdyL010.z=-200:cfdyL011.z=53:cfdyL012.z=-200:cfdyL012.z=-200
				case 12 : cfdyL001.z=-200:cfdyL002.z=-200:cfdyL003.z=-200:cfdyL004.z=-200:cfdyL005.z=-200:cfdyL006.z=-200:cfdyL007.z=-200:cfdyL008.z=-200:cfdyL009.z=-200:cfdyL010.z=-200:cfdyL011.z=-200:cfdyL012.z=53:cfdyL012.z=-200
				case 13 : cfdyL001.z=-200:cfdyL002.z=-200:cfdyL003.z=-200:cfdyL004.z=-200:cfdyL005.z=-200:cfdyL006.z=-200:cfdyL007.z=-200:cfdyL008.z=-200:cfdyL009.z=-200:cfdyL010.z=-200:cfdyL011.z=-200:cfdyL012.z=-200:cfdyL012.z=53
			end Select
End Sub

Sub chdyRtimer_Timer
countr40 = countr40 + 1 : If Countr40 > 13 then Countr40 = 13 : end If 
select case countr40
				case 1 : cfdyR001.z=53:cfdyR002.z=-200:cfdyR003.z=-200:cfdyR004.z=-200:cfdyR005.z=-200:cfdyR006.z=-200:cfdyR007.z=-200:cfdyR008.z=-200:cfdyR009.z=-200:cfdyR010.z=-200:cfdyR011.z=-200:cfdyR012.z=-200:cfdyR012.z=-200
				case 2 : cfdyR001.z=-200:cfdyR002.z=53:cfdyR003.z=-200:cfdyR004.z=-200:cfdyR005.z=-200:cfdyR006.z=-200:cfdyR007.z=-200:cfdyR008.z=-200:cfdyR009.z=-200:cfdyR010.z=-200:cfdyR011.z=-200:cfdyR012.z=-200:cfdyR012.z=-200
				case 3 : cfdyR001.z=-200:cfdyR002.z=-200:cfdyR003.z=53:cfdyR004.z=-200:cfdyR005.z=-200:cfdyR006.z=-200:cfdyR007.z=-200:cfdyR008.z=-200:cfdyR009.z=-200:cfdyR010.z=-200:cfdyR011.z=-200:cfdyR012.z=-200:cfdyR012.z=-200
				case 4 : cfdyR001.z=-200:cfdyR002.z=-200:cfdyR003.z=-200:cfdyR004.z=53:cfdyR005.z=-200:cfdyR006.z=-200:cfdyR007.z=-200:cfdyR008.z=-200:cfdyR009.z=-200:cfdyR010.z=-200:cfdyR011.z=-200:cfdyR012.z=-200:cfdyR012.z=-200
				case 5 : cfdyR001.z=-200:cfdyR002.z=-200:cfdyR003.z=-200:cfdyR004.z=-200:cfdyR005.z=53:cfdyR006.z=-200:cfdyR007.z=-200:cfdyR008.z=-200:cfdyR009.z=-200:cfdyR010.z=-200:cfdyR011.z=-200:cfdyR012.z=-200:cfdyR012.z=-200
				case 6 : cfdyR001.z=-200:cfdyR002.z=-200:cfdyR003.z=-200:cfdyR004.z=-200:cfdyR005.z=-200:cfdyR006.z=53:cfdyR007.z=-200:cfdyR008.z=-200:cfdyR009.z=-200:cfdyR010.z=-200:cfdyR011.z=-200:cfdyR012.z=-200:cfdyR012.z=-200
				case 7 : cfdyR001.z=-200:cfdyR002.z=-200:cfdyR003.z=-200:cfdyR004.z=-200:cfdyR005.z=-200:cfdyR006.z=-200:cfdyR007.z=53:cfdyR008.z=-200:cfdyR009.z=-200:cfdyR010.z=-200:cfdyR011.z=-200:cfdyR012.z=-200:cfdyR012.z=-200
				case 8 : cfdyR001.z=-200:cfdyR002.z=-200:cfdyR003.z=-200:cfdyR004.z=-200:cfdyR005.z=-200:cfdyR006.z=-200:cfdyR007.z=-200:cfdyR008.z=53:cfdyR009.z=-200:cfdyR010.z=-200:cfdyR011.z=-200:cfdyR012.z=-200:cfdyR012.z=-200
				case 9 : cfdyR001.z=-200:cfdyR002.z=-200:cfdyR003.z=-200:cfdyR004.z=-200:cfdyR005.z=-200:cfdyR006.z=-200:cfdyR007.z=-200:cfdyR008.z=-200:cfdyR009.z=53:cfdyR010.z=-200:cfdyR011.z=-200:cfdyR012.z=-200:cfdyR012.z=-200
				case 10 : cfdyR001.z=-200:cfdyR002.z=-200:cfdyR003.z=-200:cfdyR004.z=-200:cfdyR005.z=-200:cfdyR006.z=-200:cfdyR007.z=-200:cfdyR008.z=-200:cfdyR009.z=-200:cfdyR010.z=53:cfdyR011.z=-200:cfdyR012.z=-200:cfdyR012.z=-200
				case 11 : cfdyR001.z=-200:cfdyR002.z=-200:cfdyR003.z=-200:cfdyR004.z=-200:cfdyR005.z=-200:cfdyR006.z=-200:cfdyR007.z=-200:cfdyR008.z=-200:cfdyR009.z=-200:cfdyR010.z=-200:cfdyR011.z=53:cfdyR012.z=-200:cfdyR012.z=-200
				case 12 : cfdyR001.z=-200:cfdyR002.z=-200:cfdyR003.z=-200:cfdyR004.z=-200:cfdyR005.z=-200:cfdyR006.z=-200:cfdyR007.z=-200:cfdyR008.z=-200:cfdyR009.z=-200:cfdyR010.z=-200:cfdyR011.z=-200:cfdyR012.z=53:cfdyR012.z=-200
				case 13 : cfdyR001.z=-200:cfdyR002.z=-200:cfdyR003.z=-200:cfdyR004.z=-200:cfdyR005.z=-200:cfdyR006.z=-200:cfdyR007.z=-200:cfdyR008.z=-200:cfdyR009.z=-200:cfdyR010.z=-200:cfdyR011.z=-200:cfdyR012.z=-200:cfdyR012.z=53
			end Select
End Sub



sub stopdyaniL
chdyLtimer.enabled = 0
cfdyL001.z=-200:cfdyL002.z=-200:cfdyL003.z=-200:cfdyL004.z=-200:cfdyL005.z=-200:cfdyL006.z=-200:cfdyL007.z=-200:cfdyL008.z=-200:cfdyL009.z=-200:cfdyL010.z=-200:cfdyL011.z=-200:cfdyL012.z=-200:cfdyL012.z=-200
Countr39 = 0
end sub

sub stopdyanir
chdyRtimer.enabled = 0
cfdyR001.z=-200:cfdyR002.z=-200:cfdyR003.z=-200:cfdyR004.z=-200:cfdyR005.z=-200:cfdyR006.z=-200:cfdyR007.z=-200:cfdyR008.z=-200:cfdyR009.z=-200:cfdyR010.z=-200:cfdyR011.z=-200:cfdyR012.z=-200:cfdyR012.z=-200
Countr40 = 0
end sub

sub stopanimation1
salutetimer.enabled = 0
cfsal001.z=-200:cfsal002.z=-200:cfsal003.z=-200:cfsal004.z=-200:cfsal005.z=-200:cfsal006.z=-200:cfsal007.z=-200:cfsal008.z=-200:cfsal009.z=-200:cfsal010.z=-200:cfsal011.z=-200:cfsal012.z=-200:cfsal013.z=-200:cfsal014.z=-200:cfsal015.z=-200
'countr2 = 0
idlLtimer.enabled = 1
end sub 

sub stopanimation2
salutetimer001.enabled = 0
cfsalb001.z=-200:cfsalb002.z=-200:cfsalb003.z=-200:cfsalb004.z=-200:cfsalb005.z=-200:cfsalb006.z=-200:cfsalb007.z=-200:cfsalb008.z=-200:cfsalb009.z=-200:cfsalb010.z=-200:cfsalb011.z=-200:cfsalb012.z=-200:cfsalb013.z=-200:cfsalb014.z=-200:cfsalb015.z=-200
idlLtimer001.enabled = 1
end sub 

sub stopattractchief1
cfidlatract001.z=-200:cfidlatract002.z=-200:cfidlatract003.z=-200:cfidlatract004.z=-200:cfidlatract005.z=-200:cfidlatract006.z=-200:cfidlatract007.z=-200:cfidlatract008.z=-200:cfidlatract009.z=-200:cfidlatract010.z=-200:cfidlatract011.z=-200:cfidlatract012.z=-200:cfidlatract013.z=-200:cfidlatract014.z=-200:cfidlatract015.z=-200:cfidlatract016.z=-200:cfidlatract017.z=-200:cfidlatract018.z=-200
end sub

sub stopattractchief2
cfidl1atract001.z=-200:cfidl1atract002.z=-200:cfidl1atract003.z=-200:cfidl1atract004.z=-200:cfidl1atract005.z=-200:cfidl1atract006.z=-200:cfidl1atract007.z=-200:cfidl1atract008.z=-200:cfidl1atract009.z=-200:cfidl1atract010.z=-200:cfidl1atract011.z=-200:cfidl1atract012.z=-200:cfidl1atract013.z=-200:cfidl1atract014.z=-200:cfidl1atract015.z=-200:cfidl1atract016.z=-200:cfidl1atract017.z=-200:cfidl1atract018.z=-200
end sub

sub stopbonusaniL
chbsLtimer.enabled = 0
cfbonusL001.z=-200:cfbonusL002.z=-200:cfbonusL003.z=-200:cfbonusL004.z=-200:cfbonusL005.z=-200:cfbonusL006.z=-200:cfbonusL007.z=-200:cfbonusL008.z=-200:cfbonusL009.z=-200
Countr34 = 0
idlLtimer.enabled = 1
end sub

sub stopbonusanir
chbsRtimer.enabled = 0
cfbonusR001.z=-200:cfbonusR002.z=-200:cfbonusR003.z=-200:cfbonusR004.z=-200:cfbonusR005.z=-200:cfbonusR006.z=-200:cfbonusR007.z=-200:cfbonusR008.z=-200:cfbonusR009.z=-200
Countr35 = 0
idlLtimer001.enabled = 1
end sub

sub stopcheeraniL
chchLtimer.enabled = 0
cfcheerL001.z=-200:cfcheerL002.z=-200:cfcheerL003.z=-200:cfcheerL004.z=-200:cfcheerL005.z=-200:cfcheerL006.z=-200:cfcheerL007.z=-200:cfcheerL008.z=-200:cfcheerL009.z=-200:cfcheerL010.z=-200:cfcheerL011.z=-200:cfcheerL012.z=-200
Countr37 = 0
idlLtimer.enabled = 1
end sub

sub stopcheeranir
chchRtimer.enabled = 0
cfcheerR001.z=-200:cfcheerR002.z=-200:cfcheerR003.z=-200:cfcheerR004.z=-200:cfcheerR005.z=-200:cfcheerR006.z=-200:cfcheerR007.z=-200:cfcheerR008.z=-200:cfcheerR009.z=-200:cfcheerR010.z=-200:cfcheerR011.z=-200:cfcheerR012.z=-200
Countr38 = 0
idlLtimer001.enabled = 1
end sub

sub stopblostaniL
chblLtimer.enabled = 0
cfballlostL001.z=-200:cfballlostL002.z=-200:cfballlostL003.z=-200:cfballlostL004.z=-200:cfballlostL005.z=-200:cfballlostL006.z=-200:cfballlostL007.z=-200:cfballlostL008.z=-200
Countr31 = 0
idlLtimer.enabled = 1
end sub

sub stopblostanir
chblRtimer.enabled = 0
cfballlostR001.z=-200:cfballlostR002.z=-200:cfballlostR003.z=-200:cfballlostR004.z=-200:cfballlostR005.z=-200:cfballlostR006.z=-200:cfballlostR007.z=-200:cfballlostR008.z=-200
Countr31 = 0
idlLtimer001.enabled = 1
end sub

sub stopmfailedaniL
chmfLtimer.enabled = 0
cfmisfailedL001.z=-200:cfmisfailedL002.z=-200:cfmisfailedL003.z=-200:cfmisfailedL004.z=-200:cfmisfailedL005.z=-200:cfmisfailedL006.z=-200:cfmisfailedL007.z=-200:cfmisfailedL008.z=-200
Countr32 = 0
idlLtimer.enabled = 1
end sub

sub stopmfailedanir
chmfRtimer.enabled = 0
cfmisfailedR001.z=-200:cfmisfailedR002.z=-200:cfmisfailedR003.z=-200:cfmisfailedR004.z=-200:cfmisfailedR005.z=-200:cfmisfailedR006.z=-200:cfmisfailedR007.z=-200:cfmisfailedR008.z=-200
Countr33 = 0
idlLtimer001.enabled = 1
end sub

sub stopidlchiefke
idlLtimer001.enabled = 0
cfidl001.z=-200
cfidl002.z=-200
cfidl003.z=-200
cfidl004.z=-200
cfidl005.z=-200
cfidl006.z=-200
idlLtimer.enabled = 0
cfidla001.z=-200
cfidla002.z=-200
cfidla003.z=-200
cfidla004.z=-200
cfidla005.z=-200
cfidla006.z=-200
end sub

'************************************************
'**************Bonusscore lights*****************
'************************************************

Sub bonusyscore
if bonusyscorechecker > 100000 then li023.state = 1 end if
if bonusyscorechecker > 200000 then li022.state = 1 end if
if bonusyscorechecker > 300000 then li021.state = 1 end if
if bonusyscorechecker > 400000 then li020.state = 1 end if
if bonusyscorechecker > 500000 then li019.state = 1 end if
if bonusyscorechecker > 600000 then li018.state = 1 end if
if bonusyscorechecker > 700000 then li017.state = 1 end if
if bonusyscorechecker > 800000 then li016.state = 1 end if
if bonusyscorechecker > 900000 then li015.state = 1 end if
if bonusyscorechecker > 1000000 then li014.state = 1:li024.state = 1 end if
if bonusyscorechecker > 1100000 then li023.state = 1:li014.state = 0:li015.state = 0:li016.state = 0:li017.state = 0:li018.state = 0:li019.state = 0:li020.state = 0:li021.state = 0:li022.state = 0 end if
if bonusyscorechecker > 1200000 then li022.state = 1 end if
if bonusyscorechecker > 1300000 then li021.state = 1 end if
if bonusyscorechecker > 1400000 then li020.state = 1 end if
if bonusyscorechecker > 1500000 then li019.state = 1 end if
if bonusyscorechecker > 1600000 then li018.state = 1 end if
if bonusyscorechecker > 1700000 then li017.state = 1 end if
if bonusyscorechecker > 1800000 then li016.state = 1 end if
if bonusyscorechecker > 1900000 then li015.state = 1 end if
if bonusyscorechecker > 2000000 then li014.state = 1:li025.state = 1 end if
if bonusyscorechecker > 2100000 then li023.state = 1:li014.state = 0:li015.state = 0:li016.state = 0:li017.state = 0:li018.state = 0:li019.state = 0:li020.state = 0:li021.state = 0:li022.state = 0 end if
if bonusyscorechecker > 2200000 then li022.state = 1 end if
if bonusyscorechecker > 2300000 then li021.state = 1 end if
if bonusyscorechecker > 2400000 then li020.state = 1 end if
if bonusyscorechecker > 2500000 then li019.state = 1 end if
if bonusyscorechecker > 2600000 then li018.state = 1 end if
if bonusyscorechecker > 2700000 then li017.state = 1 end if
if bonusyscorechecker > 2800000 then li016.state = 1 end if
if bonusyscorechecker > 2900000 then li015.state = 1 end if
if bonusyscorechecker > 3000000 then li014.state = 1 end if
end sub

'************************************************
'**************Bonusscore lights drainen case*****************
'************************************************

sub blicht1
if  li023.state = 1 then li023.state = 0: playsound "bonusaff":AddScore 10000 end if
end sub

sub blicht2
if  li022.state = 1 then li022.state = 0: playsound "bonusaff":AddScore 10000 end if
end sub

sub blicht3
if  li021.state = 1 then li021.state = 0: playsound "bonusaff":AddScore 10000 end if
end sub

sub blicht4
if  li020.state = 1 then li020.state = 0: playsound "bonusaff":AddScore 10000 end if
end sub

sub blicht5
if  li019.state = 1 then li019.state = 0: playsound "bonusaff":AddScore 10000 end if
end sub

sub blicht6
if  li018.state = 1 then li018.state = 0: playsound "bonusaff":AddScore 10000 end if
end sub

sub blicht7
if  li017.state = 1 then li017.state = 0: playsound "bonusaff":AddScore 10000 end if
end sub

sub blicht8
if  li016.state = 1 then li016.state = 0: playsound "bonusaff":AddScore 10000 end if
end sub

sub blicht9
if  li015.state = 1 then li015.state = 0: playsound "bonusaff":AddScore 10000 end if
end sub

sub blicht10
if  li014.state = 1 then li014.state = 0: playsound "bonusaff":AddScore 10000 end if
end sub

sub blicht14
if  li025.state = 1 then li025.state = 0::li014.state = 1:li015.state = 1:li016.state = 1:li017.state = 1:li018.state = 1:li019.state = 1:li020.state = 1:li021.state = 1:li022.state = 1:li023.state = 1: playsound "bonusaff":AddScore 100000 end if
end sub

sub blicht15
if  li024.state = 1 then li024.state = 0::li014.state = 1:li015.state = 1:li016.state = 1:li017.state = 1:li018.state = 1:li019.state = 1:li020.state = 1:li021.state = 1:li022.state = 1:li023.state = 1: playsound "bonusaff":AddScore 100000 end if
end sub

sub resettempbonus
lichtTimer.enabled = False
tempbonusaf = 1
end sub

sub drainlights
    Select Case tempbonusaf
	Case 1: blicht1: resettempbonus
	Case 2: blicht2
	Case 3: blicht3
	Case 4: blicht4
	Case 5: blicht5
	Case 6: blicht6
	Case 7: blicht7
	Case 8: blicht8
	Case 9: blicht9
	Case 10: blicht10
	Case 11: blicht14
	Case 12: blicht1
	Case 13: blicht2
	Case 14: blicht3
	Case 15: blicht4
	Case 16: blicht5
	Case 17: blicht6
	Case 18: blicht7
	Case 19: blicht8
	Case 20: blicht9
	Case 21: blicht10
	Case 22: blicht15
	Case 23: blicht1
	Case 24: blicht2
	Case 25: blicht3
	Case 26: blicht4
	Case 27: blicht5
	Case 28: blicht6
	Case 29: blicht7
	Case 30: blicht8
	Case 31: blicht9
	Case 32: blicht10
    End Select
End Sub

Sub lichtTimer_Timer
If tempbonusaf > 1 then
tempbonusaf = tempbonusaf -1
drainlights
end if
end sub
