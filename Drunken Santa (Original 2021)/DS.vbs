' ****************************************************************
'                       VISUAL PINBALL X
'                		Drunken Santa
'                       Version 1.0.0
'						started 10-11-2020
'						Finished 19-11-2020
'						Made by Remdwaas 
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
Const cGameName = "DS"
Const TableName = "DS"
Const myVersion = "1.0.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 20 ' in seconds
Const MaxMultiplier = 3  ' limit to 3x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 5   ' usually 3 or 5
Const MaxMultiballs = 4  ' max number of balls during multiballs

Const Special1 = 1000000  ' High score to obtain an extra ball/game
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
Dim Bumperbonusy
Dim ReindeerLeftBonus
Dim ReindeerRightBonus
Dim Beer1Bonus
Dim Beer2Bonus
Dim Santabonus
Dim Sleighbonus
Dim Treebonus

' core.vbs variables

' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
	Dim i
	'Randomize


'reset hiscore
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
		Flipper001.RotateToEnd 'Adds To End Movement for Flipper001
		RotateLaneLightsLeft
		RotateLaneLightsLeft2
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
		Flipper001.RotateToStart 'Adds To End Movement for Flipper001
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
		Flipper002.RotateToEnd 'Adds To End Movement for Flipper002
		RotateLaneLightsRight
		RotateLaneLightsRight2
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
		Flipper002.RotateToStart 'Adds To End Movement for Flipper002
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


Sub RotateLaneLightsLeft2
    Dim TempState2
    TempState2 = li018.State
    li018.State = li019.State
    li019.State = li020.State
    li020.state = TempState2
End Sub

Sub RotateLaneLightsRight2
    Dim TempState2
    TempState2 = li020.State
    li020.State = li019.State
    li019.State = li018.State
    li018.state = TempState2
End Sub


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
        PlaySong "MULTI"
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

Sub PlayQuote
    Dim tmp
    tmp = INT(RND * 30) + 1
    PlaySound "BRP_" &tmp
End Sub

Sub PlayQuote1
    Dim tmp
    tmp = INT(RND * 19) + 1
    PlaySound "FRT_" &tmp
End Sub

Sub PlayQuote2
    Dim tmp
    tmp = INT(RND * 18) + 1
    PlaySound "drunk_" &tmp
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
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

	' Reset beer bottles
	Bottle1.z = - 100
	Bottle2.z = - 100
	PlaySound "fx_resetdrop"

	' reset Santa and sleigh
	Santa.Z = 120
	sleigh.ObjRotY = -30

	'reset variable
	BottlePos = 0
	BottlePos2 = 0
	XmasPos = 0
	SantaPos = 0
	SleighPos = 0

    'resets the score display, and turn off attract mode
    StopAttractMode
    GiOn

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

    UpdateMusic = 0
    'UpdateMusic = UpdateMusic + 6
    UpdateMusicNow

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
	Bumperbonusy = 0
	ReindeerLeftBonus = 0
	ReindeerRightBonus = 0
	Beer1Bonus = 0
	Beer2Bonus = 0
	Santabonus = 0
	Sleighbonus = 0
	Treebonus = 0
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
	Dim BonusDelayTime
	' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

	'LightSeqAttract.Play SeqBlinking, , 5, 150

StopSong
bonuscheckie

    Dim AwardPoints, TotalBonus, ii
    AwardPoints = 0
    TotalBonus = 0

    'If NOT Tilted Then
	If(Tilted = False) Then
		
		DMD "", "", "blost1", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost2", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost3", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost4", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost5", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost6", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost7", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost8", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost9", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost10", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost1", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost2", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost3", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost4", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost5", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost6", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost7", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost8", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost9", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "blost10", eNone, eNone, eNone, 100, True, ""

		PlaySound "BALLLOST"

        'Number of Target hits
        AwardPoints = TargetBonus * 10000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = Bumperbonusy * 100000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = ReindeerLeftBonus * 100000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = ReindeerRightBonus * 100000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = Beer1Bonus * 50000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = Beer2Bonus * 50000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = Santabonus * 100000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = Sleighbonus * 100000
        TotalBonus = TotalBonus + AwardPoints

        AwardPoints = Treebonus * 50000
        TotalBonus = TotalBonus + AwardPoints
        
		DMD CL(0, FormatScore(TotalBonus) ), CL(1, "TOTAL BONUS " & " XII" & BonusMultiplier(CurrentPlayer) ), "", eBlinkFast, eNone, eNone, 1000, True, "po_bonus7"
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
        DMD "", "", "extraballl", eNone, eNone, eNone, 1000, True, "nice"

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
		StopSong
		'DMD CL(0, "GAME OVER") "", eNone, 13000, True, ""
		''DMD "", CL(1, "GAME OVER"), "", eNone, eNone, eNone, 13000, False, ""
		DMD "", "", "go1", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go2", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go3", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go4", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go5", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go6", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go7", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go8", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go9", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go10", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go1", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go2", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go3", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go4", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go5", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go6", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go7", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go8", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go9", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go10", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go1", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go2", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go3", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go4", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go5", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go6", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go7", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go8", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go9", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go10", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go1", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go2", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go3", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go4", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go5", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go6", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go7", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go8", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go9", eNone, eNone, eNone, 100, True, ""
		DMD "", "", "go10", eNone, eNone, eNone, 100, True, ""
		PlaySound "GAMEOVER"
        ' set the machine into game over mode
        vpmtimer.addtimer 6300, "EndOfGame() '"

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
    DMD "_", CL(1, "SHOOT THE BALL"), "", eNone, eBlink, eNone, 1000, True, ""
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
    DMD "_", CL(1, ("EXTRA BALL WON") ), "", eNone, eBlink, eNone, 1000, True, SoundFXDOF("EXTRABALL", 122, DOFPulse, DOFKnocker)
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
	DMD "", "", "intro1", eNone, eNone, eNone, 3000, False, ""
    DMD "", "", "ints1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "ints2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "ints3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "ints4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "ints5", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "ints6", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "ints7", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "ints8", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "ints9", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "ints10", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int5", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int5", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int5", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int5", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int5", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int5", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int5", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "int5", eNone, eNone, eNone, 100, True, ""
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

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

' tables variables and Mode init
Dim HoleBonus, BumperBonus, ALLRampBonus, RampBonus1, RampBonus2, RampBonus3, MulitballBonus, TargetBonus    

Sub Game_Init() 'called at the start of a new game
    Dim i, j
    'ChangeSong
	TargetBonus = 0
	bumperHits = 100
	BumperBonus = 0
	ALLRampBonus = 0
	RampBonus1 = 0
	RampBonus2 = 0
	RampBonus3 =0
	MulitballBonus = 0
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
	save.state = 1
	save2.state = 1
    li021.State = 1
    li022.State = 1
    li017.State = 1
    li016.State = 1
	li015.state = 1
	li014.state = 1
	li013.state = 1
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
	end if
	If li002.State=1 then 
	AddScore 420
	end if
	PlaySound SoundFX("slingshot",DOFContactors), 0,1, 0.05,0.05 '0,1, AudioPan(RightSlingShot), 0.05,0,0,1,AudioFade(RightSlingShot)
    RSling.Visible = 0
    RSling1.Visible = 1
    sling1.rotx = 20
    RStep = 0
    RightSlingShot.TimerEnabled = 1
	'gi1.State = 0:Gi2.State = 0
	AddScore 210
	li044.state = 2
	li045.state = 2
	vpmtimer.addtimer 1000, "turnofRight ' "
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
	end if
	If li002.State=1 then 
	AddScore 420
	end if
    PlaySound SoundFX("slingshot",DOFContactors), 0,1, -0.05,0.05 '0,1, AudioPan(LeftSlingShot), 0.05,0,0,1,AudioFade(LeftSlingShot)
    LSling.Visible = 0
    LSling1.Visible = 1
    sling2.rotx = 20
    LStep = 0
    LeftSlingShot.TimerEnabled = 1
	'gi3.State = 0:Gi4.State = 0
	AddScore 210
	li042.state = 2
	li043.state = 2
	vpmtimer.addtimer 1000, "turnofLeft ' "
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 3:LSLing1.Visible = 0:LSLing2.Visible = 1:sling2.rotx = 10
        Case 4:LSLing2.Visible = 0:LSLing.Visible = 1:sling2.rotx = 0:LeftSlingShot.TimerEnabled = 0'gi3.State = 1:Gi4.State = 1
    End Select
    LStep = LStep + 1
End Sub

sub turnofLeft
	li042.state = 0
	li043.state = 0
end sub

sub turnofRight
	li044.state = 0
	li045.state = 0
end sub
'*****************
'triggers
'*****************

Sub Bonuschecker_Hit
FlashForMs Flasher001, 1000, 50, 0
FlashForMs Flasher002, 1000, 50, 0
end sub

Sub bonuscheckie
if li023.state = 1 then
ReindeerLeftBonus = 1
li023.state = 0
Else
ReindeerLeftBonus = 0
End if

if li024.state = 1 Then
ReindeerRightBonus = 1
li024.state = 0
Else
ReindeerRightBonus = 0
End if

if li025.state = 1 Then
Beer1Bonus = 1
li025.state = 0
Else
Beer1Bonus = 0
End if

if li026.state = 1 Then
Beer2Bonus = 1
li026.state = 0
Else
Beer2Bonus = 0
End if

if li027.state = 1  Then
Santabonus = 1
li027.state = 0
Else
Santabonus = 0
End if

if li028.state = 1  Then
Sleighbonus = 1
li028.state = 0
Else
Sleighbonus = 0
End if

if li029.state = 1  Then
Treebonus = 1
li029.state = 0
Else
Treebonus = 0
End if

if li030.state = 1  Then
Bumperbonusy = 1
li030.state = 0
Else
Bumperbonusy = 0
End if
End Sub

Sub TLeftInlane_Hit
    If li001.State = 1 then
        AddScore 5000
    end if
    If li002.State = 1 then
        AddScore 10000
    end if
    If LeftInlane.state = 0 then
        PlaySound "fx_POP"
        AddScore 5000
        LeftInlane.state = 1
    Elseif LeftInlane.state = 0 then
        PlaySound "fx_POP"
        AddScore 5000
    end if
    PlaySound "fx_POP"
    CheckbBEER
End Sub

Sub TLeftOutlane_Hit
    If li001.State = 1 then
        AddScore 50000
    end if
    If li002.State = 1 then
        AddScore 100000
    end if
	If save.state = 1 Then
	KickbackPulse.enabled = 1
    kickbacktimer.interval = 800
	kickbacktimer.enabled = true
	end If
	If save.state = 0 Then
	KickbackPulse.enabled = 0
	kickbacktimer.enabled = False
	end If
    If LeftOutlane.state = 0 then
        PlaySound "fx_POP"
        AddScore 50000
        LeftOutlane.state = 1
    Elseif LeftOutlane.state = 0 then
        PlaySound "fx_POP"
        AddScore 50000
    end if
FlashForMs Flasher001, 1000, 50, 0
    PlaySound "fx_POP"
    CheckbBEER
End Sub

Sub TRightInlane_Hit
    If li001.State = 1 then
        AddScore 5000
    end if
    If li002.State = 1 then
        AddScore 10000
    end if
    If RightInlane.state = 0 then
        PlaySound "fx_POP"
        AddScore 5000
        RightInlane.state = 1
    Elseif RightInlane.state = 0 then
        PlaySound "fx_POP"
        AddScore 5000
    end if
    PlaySound "fx_POP"
    CheckbBEER
End Sub

Sub TRightOutlane_Hit
    If li001.State = 1 then
        AddScore 50000
    end if
    If li002.State = 1 then
        AddScore 100000
    end if
	If save2.state = 1 Then
	KickbackPulse2.enabled = 1
    kickbacktimer2.interval = 800
	kickbacktimer2.enabled = true
	end If
	If save2.state = 0 Then
	KickbackPulse2.enabled = 0
	kickbacktimer2.enabled = False
	end If
    If RightOutlane.state = 0 then
        PlaySound "fx_POP"
        AddScore 50000
        RightOutlane.state = 1
    Elseif RightOutlane.state = 0 then
        PlaySound "fx_POP"
        AddScore 50000
    end if
FlashForMs Flasher002, 1000, 50, 0
    PlaySound "fx_POP"
    CheckbBEER
End Sub

Sub CheckbBEER
    If(LeftInlane.State = 1)And(LeftOutlane.State = 1)And(RightInlane.State = 1)And(RightOutlane.State = 1)Then
        DMD "", "", "beerdmd", eNone, eNone, eNone, 1000, True, "nice"
        AddScore 50000
        LeftInlane.State = 0
        LeftOutlane.State = 0
        RightInlane.State = 0
        RightOutlane.State = 0
    End If
End Sub

Sub Trigger001_Hit()
	If li001.State= 1 then 
	AddScore 1000
	end if
	If li002.State= 1 then 
	AddScore 2000
	end if
	addscore 1000
	playsound "fx_sensor"
	li018.state= 1
	CheckbTOY
End Sub

Sub Trigger002_Hit()
	If li001.State= 1 then 
	AddScore 1000
	end if
	If li002.State= 1 then 
	AddScore 2000
	end if
	addscore 1000
	playsound "fx_sensor"
	li019.state= 1
	CheckbTOY
End Sub

Sub Trigger003_Hit()
	If li001.State= 1 then 
	AddScore 1000
	end if
	If li002.State= 1 then 
	AddScore 2000
	end if
	addscore 1000
	playsound "fx_sensor"
	li020.state= 1
	CheckbTOY
End Sub

Sub CheckbTOY
    If(li018.State = 1) And(li019.State = 1) And(li020.State = 1) Then
    DMD "", "", "toydmd", eNone, eNone, eNone, 1000, True, "nice"
    AddScore 25000
	li018.state=0
	li019.state=0
	li020.state=0
	End If
End Sub

Sub Trigger004_Hit()
Playsound "fx_ballrampdrop"
End sub

Sub Trigger005_Hit()
Playsound "fx_ballrampdrop"
End sub

'************************** 
'Bumpers 
'************************** 
Dim bumperHits

Sub Bumper001_hit()
If li001.State=1 then 
	AddScore 500
	end if
	If li002.State=1 then 
	AddScore 1000
	end if
PlaySound "JINGLES"
addscore 500
FlashForMs Flashbump1, 1000, 50, 0
FlashForMs Flasher001, 1000, 50, 0
FlashForMs Flasher002, 1000, 50, 0
	bumperHits = bumperHits - 1
    DMDFlush
    DMD CL(0, FormatScore(Score(Currentplayer) ) ), CL(1, bumperHits& " TOYS LEFT"), "_", eNone, eNone, eNone, 300, True, ""
    CheckBumpers
end sub

Sub Bumper002_hit()
If li001.State=1 then 
	AddScore 500
	end if
	If li002.State=1 then 
	AddScore 1000
	end if
PlaySound "JINGLES"
addscore 500
FlashForMs Flashbump2, 1000, 50, 0
FlashForMs Flasher001, 1000, 50, 0
FlashForMs Flasher002, 1000, 50, 0
	bumperHits = bumperHits - 1
    DMDFlush
    DMD CL(0, FormatScore(Score(Currentplayer) ) ), CL(1, bumperHits& " TOYS LEFT"), "_", eNone, eNone, eNone, 300, True, ""
    CheckBumpers
end sub

Sub Bumper003_hit()
If li001.State=1 then 
	AddScore 500
	end if
	If li002.State=1 then 
	AddScore 1000
	end if
PlaySound "JINGLES"
addscore 500
FlashForMs Flashbump3, 1000, 50, 0
FlashForMs Flasher001, 1000, 50, 0
FlashForMs Flasher002, 1000, 50, 0
	bumperHits = bumperHits - 1
    DMDFlush
    DMD CL(0, FormatScore(Score(Currentplayer) ) ), CL(1, bumperHits& " TOYS LEFT"), "_", eNone, eNone, eNone, 300, True, ""
    CheckBumpers
end sub


' Bumper Bonus
' 100000 i bonus after each 100 hits

Sub CheckBumpers()
    If bumperHits <= 0 Then
		li030.state = 1
        DMD "_", CL(1, "BUMPERS BONUS " & BumperBonus), "_", eNone, eBlink, eNone, 500, True, ""
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

'*************************targets Reindeer left *************************

Sub Target001_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    DMD "", "", "rudolph", eNone, eNone, eNone, 1000, True, ""
    PlaySound "rudolf10"
	li003.state = 1
    Addscore 1000
    TargetBonus = TargetBonus + 1
	CheckreindeerLEFT
End Sub

Sub Target002_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    DMD "", "", "blitzen", eNone, eNone, eNone, 1000, True, ""
    PlaySound "blitzen9"
	li004.state = 1
    Addscore 1000
    TargetBonus = TargetBonus + 1
	CheckreindeerLEFT
End Sub

Sub Target003_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    DMD "", "", "blixsem", eNone, eNone, eNone, 1000, True, ""
    PlaySound "blixsem8"
	li005.state = 1
    Addscore 1000
    TargetBonus = TargetBonus + 1
	CheckreindeerLEFT
End Sub

Sub Target004_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    DMD "", "", "comet", eNone, eNone, eNone, 1000, True, ""
    PlaySound "comet5"
	li006.state = 1
    Addscore 1000
    TargetBonus = TargetBonus + 1
	CheckreindeerLEFT
End Sub

Sub Target005_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    DMD "", "", "cupit", eNone, eNone, eNone, 1000, True, ""
    PlaySound "cupit6"
	li007.state = 1
    Addscore 1000
    TargetBonus = TargetBonus + 1
	CheckreindeerLEFT
End Sub


Sub CheckreindeerLEFT
	'If li023.State = 1 Then Exit Sub
    If(li003.State = 1) And(li004.State = 1) And(li005.State = 1) And(li006.State = 1) And(li007.State = 1) Then
        Playsound "nice"
		DMD "", "", "rdb1", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb2", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb3", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb4", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb5", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb6", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb1", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb2", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb3", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb4", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb5", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb6", eNone, eNone, eNone, 100, True, ""
        AddScore 100000
        li023.State = 1
		li003.state = 0
		li004.state = 0
		li005.state = 0
		li006.state = 0
		li007.state = 0
    End If
End Sub

'*************************targets Reindeer right *************************

Sub Target006_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    DMD "", "", "dancer", eNone, eNone, eNone, 1000, True, ""
    PlaySound "dancer2"
	li008.state = 1
    Addscore 1000
    TargetBonus = TargetBonus + 1
	CheckreindeerRIGHT
End Sub

Sub Target007_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    DMD "", "", "dasher", eNone, eNone, eNone, 1000, True, ""
    PlaySound "dasher1"
	li009.state = 1
    Addscore 1000
    TargetBonus = TargetBonus + 1
	CheckreindeerRIGHT
End Sub

Sub Target008_Hit()
    If Tilted Then Exit Sub
    If li010.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    DMD "", "", "donder", eNone, eNone, eNone, 1000, True, ""
    PlaySound "donder7"
	li010.state = 1
    Addscore 1000
    TargetBonus = TargetBonus + 1
	CheckreindeerRIGHT
End Sub

Sub Target009_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    DMD "", "", "prancer", eNone, eNone, eNone, 1000, True, ""
    PlaySound "prancer3"
	li011.state = 1
    Addscore 1000
    TargetBonus = TargetBonus + 1
	CheckreindeerRIGHT
End Sub

Sub Target010_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 1000
    end if
    If li002.State = 1 then
        AddScore 2000
    end if
    DMD "", "", "vixen", eNone, eNone, eNone, 1000, True, ""
    PlaySound "vixen4"
	li012.state = 1
    Addscore 1000
    TargetBonus = TargetBonus + 1
	CheckreindeerRIGHT
End Sub

Sub CheckreindeerRIGHT
	'If li023.State = 1 Then Exit Sub
    If(li008.State = 1) And(li009.State = 1) And(li010.State = 1) And(li011.State = 1) And(li012.State = 1) Then
        Playsound "nice"
		DMD "", "", "rdb1", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb2", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb3", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb4", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb5", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb6", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb1", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb2", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb3", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb4", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb5", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "rdb6", eNone, eNone, eNone, 100, True, ""
        AddScore 100000
        li024.State = 1
		li008.state = 0
		li009.state = 0
		li010.state = 0
		li011.state = 0
		li012.state = 0
    End If
End Sub

'*************************target 2x/3x*************************

Sub Target012_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 and li034.State = 0 then
        li034.State = 1
        AddScore 2000
        PlaySound "BONUS"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li001.State = 1 and li034.State = 1 then
        AddScore 2000
        PlaySound "BONUS"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li002.State = 1 then
        AddScore 3000
        PlaySound "BONUS"
        TargetBonus = TargetBonus + 1
        Exit Sub
    end if

    If li034.State = 1 Then
        PlaySound "BONUS"
    else
        PlaySound "BONUS"
        li034.State = 1
    end if
    AddScore 1000
    TargetBonus = TargetBonus + 1
    'PlaySound "TARGET1"
    Checkb2x
    Checkb3x
	save.state = 1
	save2.state = 1
End Sub

Sub Target013_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 and li035.State = 0 then
        li035.State = 1
        AddScore 2000
        PlaySound "BONUS"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li001.State = 1 and li035.State = 1 then
        AddScore 2000
        PlaySound "BONUS"
        TargetBonus = TargetBonus + 1
        Checkb3x
        Exit Sub
    end if

    If li002.State = 1 then
        AddScore 3000
        PlaySound "BONUS"
        TargetBonus = TargetBonus + 1
        Exit Sub
    end if

    If li035.State = 1 Then
        PlaySound "BONUS"
    else
        PlaySound "BONUS"
        li035.State = 1
    end if
    AddScore 1000
    TargetBonus = TargetBonus + 1
    'PlaySound "TARGET1"
    Checkb2x
    Checkb3x
	save.state = 1
	save2.state = 1
End Sub

Sub Checkb2x
    If(li034.State = 1)Or(li035.State = 1)Then
        DMD "", "", "2x1", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "2x2", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "2x3", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "2x4", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "2x5", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "2x6", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "2x1", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "2x2", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "2x3", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "2x4", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "2x5", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "2x6", eNone, eNone, eNone, 100, True, ""
        li001.State = 1
        vpmtimer.addtimer 60000, "li001.state = 0 ' "
        vpmtimer.addtimer 60000, "li034.State = 0 ' "
        vpmtimer.addtimer 60000, "li035.State = 0 ' "
    End If
End Sub

Sub Checkb3x
    If(li034.State = 1)And(li035.State = 1)Then
        DMD "", "", "3x1", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "3x2", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "3x3", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "3x4", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "3x5", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "3x6", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "3x1", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "3x2", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "3x3", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "3x4", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "3x5", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "3x6", eNone, eNone, eNone, 100, True, ""
        li002.State = 1
        li001.State = 0
        vpmtimer.addtimer 60000, "li002.state = 0 ' "
        vpmtimer.addtimer 60000, "li034.State = 0 ' "
        vpmtimer.addtimer 60000, "li035.State = 0 ' "
    End If
End Sub


'*************************Beer bottle1*************************
Dim BottlePos

sub Target014_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 10000
    end if
    If li002.State = 1 then
        AddScore 20000
    end if
    'PlaySoundAt "cash", Target3
    BottlePos = BottlePos + 1
	If BottlePos > 3 Then BottlePos = 3
    Bottle1.z = -100 + 50 * BottlePos
    'li17.state = 1
    Addscore 5000
    UpdateBottleLights
    'playquote3
    TargetBonus = TargetBonus + 1
    checkBottle
End Sub

Sub UpdateBottleLights
    Select Case BottlePos
        Case 0:Li036.State = 0:Li037.State = 0:Li038.State = 0
        Case 1:Li036.State = 1:Li037.State = 0:Li038.State = 0:PlayQuote:bottle1b
        Case 2:Li036.State = 1:Li037.State = 1:Li038.State = 0:PlayQuote1:bottle1a
        Case 3:Li036.State = 1:Li037.State = 1:Li038.State = 1:bottle1c
    End Select
end sub

sub bottle1a
        DMD "", "", "bt11", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt12", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt13", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt14", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt15", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt11", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt12", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt13", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt14", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt15", eNone, eNone, eNone, 100, True, ""
end Sub

sub bottle1b
        DMD "", "", "bt21", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt22", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt23", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt24", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt25", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt21", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt22", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt23", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt24", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt25", eNone, eNone, eNone, 100, True, ""
end Sub

sub bottle1c
		Playsound "beer1"
        DMD "", "", "B27", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B28", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B29", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B30", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B31", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B32", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B33", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B27", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B28", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B29", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B30", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B31", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B32", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B33", eNone, eNone, eNone, 100, True, ""
end Sub

Sub checkBottle
    If Bottle1.z = 50 then
        li025.state = 1
        addscore 75000
        vpmtimer.addtimer 3000, "resetBottle1 '"
    End if
'GiEffect 1
End sub

Sub resetBottle1
    BottlePos = 0
    Li036.State = 0
    Li037.State = 0
    Li038.State = 0
    Bottle1.z = - 100
    'PlaySoundAt "fx_resetdrop", Target3
end sub

'*************************Beer bottle2*************************
Dim BottlePos2

sub Target015_Hit()
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 10000
    end if
    If li002.State = 1 then
        AddScore 20000
    end if
    'PlaySoundAt "cash", Target3
    BottlePos2 = BottlePos2 + 1
	If BottlePos2 > 3 Then BottlePos2 = 3
    Bottle2.z = -100 + 50 * BottlePos2
    'li17.state = 1
    Addscore 5000
    UpdateBottleLights2
    'playquote3
    TargetBonus = TargetBonus + 1
    checkBottle2
End Sub


Sub UpdateBottleLights2
    Select Case BottlePos2
        Case 0:Li039.State = 0:Li040.State = 0:Li041.State = 0
        Case 1:Li039.State = 1:Li040.State = 0:Li041.State = 0:PlayQuote:bottle2b
        Case 2:Li039.State = 1:Li040.State = 1:Li041.State = 0:PlayQuote1:bottle2a
        Case 3:Li039.State = 1:Li040.State = 1:Li041.State = 1:bottle2c
    End Select
end sub

sub bottle2a
        DMD "", "", "bt11", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt12", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt13", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt14", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt15", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt11", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt12", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt13", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt14", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt15", eNone, eNone, eNone, 100, True, ""
end Sub

sub bottle2b
        DMD "", "", "bt21", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt22", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt23", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt24", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt25", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt21", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt22", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt23", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt24", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "bt25", eNone, eNone, eNone, 100, True, ""
end Sub

sub bottle2c
		Playsound "beer1"
        DMD "", "", "B27", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B28", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B29", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B30", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B31", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B32", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B33", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B27", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B28", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B29", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B30", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B31", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B32", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "B33", eNone, eNone, eNone, 100, True, ""
end Sub

Sub checkBottle2
    If Bottle2.z = 50 then
        li026.state = 1
        addscore 75000
        vpmtimer.addtimer 3000, "resetBottle2 '"
    End if
'GiEffect 1
End sub

Sub resetBottle2
    BottlePos2 = 0
    Li041.State = 0
    Li040.State = 0
    Li039.State = 0
    Bottle2.z = - 100
    'PlaySoundAt "fx_resetdrop", Target3
end sub


'*************************Xmastree*************************
Dim XmasPos

Sub Target011_Hit()	
    If Tilted Then Exit Sub
    If li001.State = 1 then
        AddScore 10000
    end if
    If li002.State = 1 then
        AddScore 20000
    end if
    XmasPos = XmasPos + 1
	If XmasPos > 3 Then XmasPos = 3
	Addscore 5000
	UpdateXmasLights
	TargetBonus = TargetBonus + 1
	checkXmastree
	D1Shaker
	save.state = 1
	save2.state = 1
end sub

Dim D1Shake

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

Sub UpdateXmasLights
    Select Case XmasPos
        Case 0:Li031.State = 0:Li032.State = 0:Li033.State = 0
        Case 1:Li031.State = 1:Li032.State = 0:Li033.State = 0:PlayQuote2:tree2
        Case 2:Li031.State = 1:Li032.State = 1:Li033.State = 0:PlayQuote2:tree1
        Case 3:Li031.State = 1:Li032.State = 1:Li033.State = 1:tree3
    End Select
end sub

Sub tree1
        DMD "", "", "xmast11", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast12", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast11", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast12", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast11", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast12", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast11", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast12", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast11", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast12", eNone, eNone, eNone, 100, True, ""
end sub

Sub tree2
        DMD "", "", "xmast21", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast22", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast21", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast22", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast21", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast22", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast21", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast22", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast21", eNone, eNone, eNone, 100, True, ""
        DMD "", "", "xmast22", eNone, eNone, eNone, 100, True, ""
end sub

Sub tree3
Playsound "xmastreeHIT"
end sub

Sub checkXmastree
If bMultiBallMode = False Then
    If(li031.State = 1) And(li032.State = 1) And(li033.State = 1) Then
		DMD "", "", "xmastreedmd1", eNone, eNone, eNone, 1000, True, ""
	    AddScore 25000
		XmasPos = 0
		li031.state = 0
		li032.state = 0
		li033.state = 0
		li029.State = 1
		AddMultiball 1
		ChangeSong	
    End If
End If
    If(li031.State = 1) And(li032.State = 1) And(li033.State = 1) And(li029.State = 1) Then
        AddScore 25000
		XmasPos = 0
		li031.state = 0
		li032.state = 0
		li033.state = 0
	End If
End Sub

'*****************
'Gates
'*****************
Sub Gate_Hit
	If li001.State=1 then 
	AddScore 250
	end if
	If li002.State=1 then 
	AddScore 500
	end if
FlashForMs Flasher002, 1000, 50, 0
PlaySound "Enter"
AddScore 250
End sub

Sub Gate001_Hit
	If li001.State=1 then 
	AddScore 250
	end if
	If li002.State=1 then 
	AddScore 500
	end if
PlaySound "Enter"
AddScore 250
End sub

Sub Gate002_Hit
	If li001.State=1 then 
	AddScore 250
	end if
	If li002.State=1 then 
	AddScore 500
	end if
FlashForMs Flasher001, 1000, 50, 0
PlaySound "Enter"
AddScore 250
End sub

'************************** 
'Spinners
'************************** 
Sub Spinner001_Spin()'Inside this Sub is what the spinner1 will do
	If Tilted Then Exit Sub
	If li001.State=1 then 
	AddScore 1000
	end if
	If li002.State=1 then 
	AddScore 2000
	end if
	PlaySound "swish"
	Addscore 1000
End Sub

Sub Spinner002_Spin()'Inside this Sub is what the spinner1 will do
	If Tilted Then Exit Sub
	If li001.State=1 then 
	AddScore 1000
	end if
	If li002.State=1 then 
	AddScore 2000
	end if
	PlaySound "swish"
	Addscore 1000
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
    DMD "", "", "bs1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs5", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs5", eNone, eNone, eNone, 100, True, ""
    KickbackPulse.kick 0, 15
    LaserKickP1.TransY = 90
    vpmtimer.addtimer 800, "LaserKickP1.TransY = 0 '"
    Playsound "holeft"
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
    DMD "", "", "bs1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs5", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs1", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs2", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs3", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs4", eNone, eNone, eNone, 100, True, ""
    DMD "", "", "bs5", eNone, eNone, eNone, 100, True, ""
    KickbackPulse2.kick 0, 15
    LaserKickP2.TransY = 90
    vpmtimer.addtimer 800, "LaserKickP2.TransY = 0 '"
    Playsound "horight"
	vpmtimer.addtimer 4000, "save2.state = 0 '"
End sub

'*****************
' sleigh
'*****************
Dim SleighPos

Sub Kicker002_Hit()
	FlashForMs Flasher002, 1000, 50, 0
	If li001.State=1 then 
	AddScore 50000
	end if
	If li002.State=1 then 
	AddScore 100000
	end if
	SleighPos = SleighPos + 1
	If SleighPos > 3 Then SleighPos = 3
        DMDFlush
        StopSong
		UpdateSleighPos
        vpmtimer.addtimer 6010, "ejectkicker2 ' "
End Sub

Sub UpdateSleighPos
    Select Case SleighPos
        Case 0:sleigh.ObjRotY = -30
        Case 1:sleigh.ObjRotY = -30 + 10:sleigh1:PlaySound "sleigh1"
        Case 2:sleigh.ObjRotY = -20 + 10:sleigh2:PlaySound "sleigh1"
        Case 3:sleigh.ObjRotY = -10 + 10:sleigh3:PlaySound "sleigh2"
    End Select
end sub

Sub sleigh1
        DMD "", "", "slt2", eNone, eNone, eNone, 6000, True, ""
		addScore 50000
end sub

Sub sleigh2
        DMD "", "", "slt1", eNone, eNone, eNone, 6000, True, ""
		addScore 50000
end sub

Sub sleigh3
        DMD "", "", "slB1", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB2", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB3", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB4", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB5", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB6", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB1", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB2", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB3", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB4", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB5", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB6", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB1", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB2", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB3", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB4", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB5", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB6", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB1", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB2", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB3", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB4", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB5", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB6", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB1", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB2", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB3", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB4", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB5", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB6", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB1", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB2", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB3", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB4", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB5", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "slB6", eNone, eNone, eNone, 200, True, ""
		vpmtimer.addtimer 6000, "AddMultiball 3 ' "
		SleighPos = 0
		vpmtimer.addtimer 6000, "UpdateSleighPos ' "
		li028.state = 1
		addScore 500000
end sub

sub ejectkicker2
PlaySound "fx_popper"
Kicker002.Kick 160,10
ChangeSong
end sub

'*****************
' Santa
'*****************

Dim SantaPos

Sub Kicker001_Hit()
	FlashForMs Flasher001, 1000, 50, 0
	If li001.State=1 then 
	AddScore 50000
	end if
	If li002.State=1 then 
	AddScore 100000
	end if
	SantaPos = SantaPos + 1
	If SantaPos > 3 Then SantaPos = 3
        DMDFlush
        StopSong
		UpdateSantaPos
        vpmtimer.addtimer 6010, "ejectkicker ' "

End Sub

Sub UpdateSantaPos
    Select Case SantaPos
        Case 0:Santa.Z = 120
        Case 1:Santa.Z = 120 - 30:santa1:PlaySound "chim1"
        Case 2:Santa.Z = 90 - 20:santa2:PlaySound "chim1"
        Case 3:Santa.Z = 70 - 50:santa3:PlaySound "chim2"
    End Select
end sub

Sub santa1
        DMD "", "", "chimt2", eNone, eNone, eNone, 6000, True, ""
		AddScore 50000
end sub

Sub santa2
        DMD "", "", "chimt1", eNone, eNone, eNone, 6000, True, ""
		AddScore 50000
end sub

Sub santa3
        DMD "", "", "chimB1", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB2", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB3", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB4", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB5", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB1", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB2", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB3", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB4", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB5", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB1", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB2", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB3", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB4", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB5", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB1", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB2", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB3", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB4", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB5", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB1", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB2", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB3", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB4", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB5", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB1", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB2", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB3", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB4", eNone, eNone, eNone, 200, True, ""
        DMD "", "", "chimB5", eNone, eNone, eNone, 200, True, ""
		vpmtimer.addtimer 6000, "AddMultiball 3 ' "
		SantaPos = 0
		vpmtimer.addtimer 6000, "UpdateSantaPos ' "
		li027.state = 1
		AddScore 250000
end sub

sub ejectkicker
PlaySound "fx_popper"
Kicker001.Kick 180,10
ChangeSong
end sub



