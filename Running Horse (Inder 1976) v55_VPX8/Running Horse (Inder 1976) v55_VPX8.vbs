' ***********************************************************************
'        Running Horse / IPD No. 4414 / Inder 1976 / 1 Player
'  VPX8 table by jpsalas, akiles50000 & pedator 2024, version 5.5.1
' ***********************************************************************

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
'111 kicker  Left Hole
'112 kicker  Center Hole
'113 kicker  Right Hole
'114 Kicker  Bonus
'115 Chimes
'116 Chimes
'117 Chimes
'118 Chimes
'119 Bell

Option Explicit
Randomize

' core.vbs constants
Const BallSize = 50 ' 50 is the normal size
Const BallMass = 1  ' 1 is the normal ball mass.

' load extra vbs files
LoadCoreFiles

Sub LoadCoreFiles
    On Error Resume Next
    ExecuteGlobal GetTextFile("core.vbs")
    If Err Then MsgBox "Can't open core.vbs"
    On Error Resume Next
    ExecuteGlobal GetTextFile("controller.vbs")
    If Err Then MsgBox "Can't open controller.vbs"
End Sub

' Valores Constants
Const cGameName = "runninghorse" ' B2S name
Const MaxPlayers = 1             ' 1 to 4 can play
Const MaxMultiplier = 3          ' limit bonus multiplier
Const BallsPerGame = 5           ' 3 or 5
Const FreePlay = False           ' Free play or coins
Const Special1 = 570000          ' extra ball or credit
Const Special2 = 690000
'Const Special3 = 920000

' Global variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim Bonus
Dim BallsRemaining(4)
Dim BonusMultiplier
Dim ExtraBallsAwards(4)
Dim Special1Awarded(4)
Dim Special2Awarded(4)
'Dim Special3Awarded(4)
Dim Score(4)
Dim HighScore
Dim Match
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim Add10
Dim Add100
Dim Add1000
Dim Add10000
Dim x

' Control variables
Dim BallsOnPlayfield
Dim ballnr

' Boolean variables
Dim bAttractMode
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall
Dim bExtraBallWonThisBall
Dim bJustStarted
Dim bBallInPlungerLane
Dim bBallSaverActive

' core.vbs variables

' *********************************************************************
'                Common rutines to all the tables
' *********************************************************************

Sub Table1_Init()
    Dim x

    ' Init som objects, like walls, targets
    VPObjects_Init
    LoadEM

    ' load highscore
    Loadhs
    ScoreReel1.SetValue HSScore(1)
    If B2SOn then
        Controller.B2SSetScorePlayer 1, HSScore(1)
    End If
    UpdateCredits

    ' init all the global variables
    bFreePlay = FreePlay
    bAttractMode = False
    bOnTheFirstBall = False
    bGameInPlay = False
    bBallInPlungerLane = False
    BallsOnPlayfield = 0
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    Match = 0
    bJustStarted = True
    Add10 = 0
    Add100 = 0
    Add1000 = 0
    ballnr = 0
    Bonus = 0

    ' setup table in game over mode
    EndOfGame

    'turn on GI lights
    vpmtimer.addtimer 1000, "GiOn '"

    ' Remove desktop items in FS mode
    If Table1.ShowDT then
        For each x in aReels
            x.Visible = 1
        Next
    Else
        For each x in aReels
            x.Visible = 0
        Next
    End If

    ' Start the RealTime timer
    RealTime.Enabled = 1

    ' Load table color
    LoadLut
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    If EnteringInitials then
        CollectInitials(keycode)
        Exit Sub
    End If

    If keycode = LeftMagnaSave Then bLutActive = True: SetLUTLine "Color LUT image " & table1.ColorGradeImage
    If keycode = RightMagnaSave AND bLutActive Then NextLUT

    ' add coins
    If Keycode = AddCreditKey OR Keycode = AddCreditKey2 Then
        If(Tilted = False)Then
            AddCredits 1
            PlaySound "fx_coin"
        End If
    End If

    ' plunger
    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' tilt keys
    If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25
    If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25
    If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25

    ' keys during game

    If bGameInPlay AND NOT Tilted Then
        If keycode = LeftTiltKey Then CheckTilt
        If keycode = RightTiltKey Then CheckTilt
        If keycode = CenterTiltKey Then CheckTilt
        If keycode = MechanicalTilt Then CheckTilt

        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        If keycode = StartGameKey Then
            If((PlayersPlayingGame < MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    UpdatePlayersPlaying
                'PlaySound "so_fanfare1"
                Else
                    If(Credits > 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        Credits = Credits - 1
                        UpdateCredits
                        UpdateBallInPlay
                        UpdatePlayersPlaying
                    Else
                    ' Not Enough Credits to start a game.
                    'PlaySound "so_nocredits"
                    End If
                End If
            End If
        End If
        Else

            If keycode = StartGameKey Then
                If(bFreePlay = True)Then
                    If(BallsOnPlayfield = 0)Then
                        ResetScores
                        ResetForNewGame()
                    End If
                Else
                    If(Credits > 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            UpdateCredits
                            ResetScores
                            ResetForNewGame()
                        End If
                    Else
                    ' Not Enough Credits to start a game.
                    'PlaySound "so_nocredits"
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)
    If EnteringInitials then
        Exit Sub
    End If

    If keycode = LeftMagnaSave Then bLutActive = False: HideLUT

    If bGameInPlay AND NOT Tilted Then
        ' teclas de los flipers
        If keycode = LeftFlipperKey Then SolLFlipper 0
        If keycode = RightFlipperKey Then SolRFlipper 0
    End If

    If keycode = PlungerKey Then
        Plunger.Fire
        If bBallInPlungerLane Then
            PlaySoundAt "fx_plunger", plunger
        Else
            PlaySoundAt "fx_plunger_empty", plunger
        End If
    End If
End Sub

'******************
' Table stop/pause
'******************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub table1_Exit
    Savehs
'Controller.Stop
End Sub

'*******************
' Flipper Subs Rev3
'*******************

SolCallback(sLRFlipper) = "SolRFlipper"
SolCallback(sLLFlipper) = "SolLFlipper"

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToEnd
        LeftFlipperOn = 1
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.RotateToStart
        LeftFlipperOn = 0
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.RotateToEnd
        RightFlipperOn = 1
    Else
        PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.RotateToStart
        RightFlipperOn = 0
    End If
End Sub

' flippers top animations

Sub LeftFlipper_Animate: LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle: End Sub
Sub RightFlipper_Animate: RightFlipperTop.RotZ = RightFlipper.CurrentAngle: End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.1, 0, 0, 0, AudioFade(ActiveBall)
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

'***********
' GI lights
'***********

Sub GiOn 'enciende las luces GI
	PlaySound"fx_gion"
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff 'apaga las luces GI
	PlaySound"fx_gioff"
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
End Sub

'**************
'    TILT
'**************

Sub CheckTilt
    Tilt = Tilt + TiltSensitivity
    TiltDecreaseTimer.Enabled = True
    If Tilt > 15 Then
        Tilted = True
        TiltReel.SetValue 1
        If B2SOn then
            Controller.B2SSetTilt 1
        end if
        DisableTable True
        ' BallsRemaining(CurrentPlayer) = 0 'player looses the game
        TiltRecoveryTimer.Enabled = True 'wait for all the balls to drain
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    If Tilt > 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        GiOff
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        Bumper001.Threshold = 100
        Bumper002.Threshold = 100
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        GiOn
        Bumper001.Threshold = 1
        Bumper002.Threshold = 1
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' all the balls have drained
    If(BallsOnPlayfield = 0)Then
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' otherwise repeat
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
Const lob = 0     'number of locked balls
Const maxvel = 32 'max ball velocity
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
        aBallShadow(b).Height = BOT(b).Z -Ballsize/2

        If BallVel(BOT(b))> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b))
                ballvol = Vol(BOT(b))
            Else
                ballpitch = Pitch(BOT(b)) + 50000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b)) * 3
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

        ' jps ball speed & spin control
            BOT(b).AngMomZ = BOT(b).AngMomZ * 0.95
        If BOT(b).VelX AND BOT(b).VelY <> 0 Then
            speedfactorx = ABS(maxvel / BOT(b).VelX)
            speedfactory = ABS(maxvel / BOT(b).VelY)
            If speedfactorx <1 Then
                BOT(b).VelX = BOT(b).VelX * speedfactorx
                BOT(b).VelY = BOT(b).VelY * speedfactorx
            End If
            If speedfactory <1 Then
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

'************************************
' Diverse Collection Hit Sounds v3.0
'************************************

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

'************************************************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'************************************************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState)

    If TypeName(MyLight) = "Light" Then
        If FinalState = 2 Then
            FinalState = MyLight.State
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then
        Dim steps
        steps = Int(TotalPeriod / BlinkPeriod + .5)
        If FinalState = 2 Then
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub

'****************************************
' Init table for a new game
'****************************************

Sub ResetForNewGame()
    'debug.print "ResetForNewGame"
    Dim i

    bGameInPLay = True
    bBallSaverActive = False

    StopAttractMode
    If B2SOn then
        Controller.B2SSetGameOver 0
    end if

    GiOn

    CurrentPlayer = 1
    PlayersPlayingGame = 1
    UpdatePlayersPlaying
    bOnTheFirstBall = True
    For i = 1 To MaxPlayers
        Score(i) = 0
        ExtraBallsAwards(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        'Special3Awarded(i) = False
        BallsRemaining(i) = BallsPerGame
    Next
    BonusMultiplier = 1
    Bonus = 0
    UpdateBallInPlay

    'Clear_Match

    ' init other variables
    Tilt = 0

    ' init game variables
    Game_Init()

    ' start a music?
    ' first ball
    vpmtimer.addtimer 2000, "FirstBall '"
End Sub

Sub FirstBall
    'debug.print "FirstBall"
    ' reset table for a new ball, rise droptargets ++
    ResetForNewPlayerBall()
    CreateNewBall()
End Sub

' (Re-)init table for a new ball or player

Sub ResetForNewPlayerBall()
    'debug.print "ResetForNewPlayerBall"
    AddScore 0

    ' reset multiplier to 1x

    ' turn on lights, and variables
    bExtraBallWonThisBall = False
    ResetNewBallLights
    ResetNewBallVariables
End Sub

' Crete new ball

Sub CreateNewBall()
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass
    BallsOnPlayfield = BallsOnPlayfield + 1
    UpdateBallInPlay
    UpdateCurrentPlayer
    PlaySoundAt SoundFXDOF("fx_Ballrel", 110, DOFPulse, DOFContactors), BallRelease
    BallRelease.Kick 90, 4
End Sub

' player lost the ball

Sub EndOfBall()
    'debug.print "EndOfBall"
    ' Lost the first ball, now it cannot accept more players
    bOnTheFirstBall = False

    'No bonus count in this table
    ' StartBonusCount

    vpmtimer.addtimer 400, "EndOfBall2 '"
End Sub

' After bonus count go to the next step
'
Sub EndOfBall2()
    'debug.print "EndOfBall2"

    Tilted = False
    Tilt = 0
    TiltReel.SetValue 0
    If B2SOn then
        Controller.B2SSetTilt 0
    end if
    DisableTable False

    ' win extra ball?
    If(ExtraBallsAwards(CurrentPlayer) > 0)Then
        'debug.print "Extra Ball"

        ' if so then give it
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' turn off light if no more extra balls
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            'XtraBall.SetValue 0
            If B2SOn then
                'Controller.B2SSetShootAgain 0
            End If
        End If

        ' extra ball sound?

        ' reset as in a new ball
        ResetForNewPlayerBall()
        CreateNewBall()
    Else ' no extra ball

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer)- 1

        ' last ball?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            CheckHighScore()
        End If

        ' this is not the last ball, chack for new player
        EndOfBallComplete()
    End If
End Sub

Sub EndOfBallComplete()
    'debug.print "EndOfBallComplete"
    Dim NextPlayer

    ' other players?
    If(PlayersPlayingGame > 1)Then
        NextPlayer = CurrentPlayer + 1
        ' if it is the last player then go to the first one
        If(NextPlayer > PlayersPlayingGame)Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' end of game?
    If((BallsRemaining(CurrentPlayer) <= 0)AND(BallsRemaining(NextPlayer) <= 0))Then

        ' match if playing with coins
        If bFreePlay = False Then
            'Verification_Match
        End If

        ' end of game
        EndOfGame()
    Else
        ' next player
        CurrentPlayer = NextPlayer

        ' update score
        AddScore 0

        ' reset table for new player
        ResetForNewPlayerBall()
        CreateNewBall()
    End If
End Sub

' Called at the end of the game

Sub EndOfGame()
    'debug.print "EndOfGame"
    bGameInPLay = False
    bJustStarted = False
    GameOverR.SetValue 1
    LastBallR. SetValue 0
    If B2SOn then
        Controller.B2SSetGameOver 1
        'Controller.B2SSetBallInPlay 0
        Controller.B2SSetPlayerUp 0
        Controller.B2SSetCanPlay 0
        Controller.B2SSetData 120, 0 'Lastball
    end if
    ' turn off flippers
    SolLFlipper 0
    SolRFlipper 0

    ' start the attract mode
    StartAttractMode
End Sub

' Fuction to calculate the balls left
Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp > BallsPerGame Then
        Balls = BallsPerGame
    Else
        Balls = tmp
    End If
End Function

'******************
'     Match
'******************

Sub Verification_Match()
    PlaySound "fx_match"
    Match = INT(RND(1) * 10) * 10 ' random between 10 and 90
    Display_Match
    If(Score(CurrentPlayer)MOD 100) = Match Then
        PlaySound "fx_knocker"
        AddCredits 1
    End If
End Sub

Sub Clear_Match()
    MatchReel.SetValue 10
    'MatchReel2.SetValue 10
    If B2SOn then
        Controller.B2SSetMatch 10
    end if
End Sub

Sub Display_Match()
    MatchReel.SetValue 1 + (Match \ 11)
    'MatchReel2.SetValue(Match \ 11) + 1
    If B2SOn then
        If Match = 0 then
            Controller.B2SSetMatch 100
        else
            Controller.B2SSetMatch Match
        end if
    end if
End Sub

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

Sub Drain_Hit()
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1
    PlaySoundAt "fx_drain", Drain

    'tilted?
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if still playing and not tilted
    If(bGameInPLay = True)AND(Tilted = False)Then

        ' ballsaver?
        If(bBallSaverActive = True)Then
            CreateNewBall()
        Else
            ' last ball?
            If(BallsOnPlayfield = 0)Then
                StopEndOfBallMode
                vpmtimer.addtimer 500, "EndOfBall '"
                Exit Sub
            End If
        End If
    End If
End Sub

Sub swPlungerRest_Hit()
    bBallInPlungerLane = True
End Sub

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
End Sub

' ****************************************
'             Score functions
' ****************************************

Sub AddScore(Points)
    If Tilted Then Exit Sub
    Select Case Points
        Case 10, 100, 1000, 10000
            Score(CurrentPlayer) = Score(CurrentPlayer) + points
            UpdateScore points
            ' sounds
            If Points = 1000 AND(Score(CurrentPlayer)MOD 10000) \ 1000 = 0 Then  'new reel 10000
                PlaySound SoundFXDOF("bell10000", 115, DOFPulse, DOFChimes)
            ElseIf Points = 100 AND(Score(CurrentPlayer)MOD 1000) \ 100 = 0 Then 'new reel 1000
                PlaySound SoundFXDOF("bell1000", 116, DOFPulse, DOFChimes)
            ElseIf Points = 10 AND(Score(CurrentPlayer)MOD 100) \ 10 = 0 Then    'new reel 100
                PlaySound SoundFXDOF("bell100", 117, DOFPulse, DOFChimes)
            Else
                PlaySound SoundFXDOF("bell", 118, DOFPulse, DOFChimes)&Points
            End If
        Case 20, 30, 40, 50, 60, 70, 80, 90
            Add10 = Add10 + Points \ 10
            AddScore10Timer.Enabled = TRUE
        Case 200, 300, 400, 500, 600, 700, 800, 900
            Add100 = Add100 + Points \ 100
            AddScore100Timer.Enabled = TRUE
        Case 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000
            Add1000 = Add1000 + Points \ 1000
            AddScore1000Timer.Enabled = TRUE
        Case 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000
            Add10000 = Add10000 + Points \ 10000
            AddScore10000Timer.Enabled = TRUE
    End Select

    ' check for higher score and specials
    If Score(CurrentPlayer) >= Special1 AND Special1Awarded(CurrentPlayer) = False Then
        AwardSpecial
        Special1Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
        AwardSpecial
        Special2Awarded(CurrentPlayer) = True
    End If
'    If Score(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
'        AwardSpecial
'        Special3Awarded(CurrentPlayer) = True
'    End If
End Sub

'************************************
'       Score sound Timers
'************************************

Sub AddScore10Timer_Timer()
    if Add10 > 0 then
        AddScore 10
        Add10 = Add10 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore100Timer_Timer()
    if Add100 > 0 then
        AddScore 100
        Add100 = Add100 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore1000Timer_Timer()
    if Add1000 > 0 then
        AddScore 1000
        Add1000 = Add1000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore10000Timer_Timer()
    if Add10000 > 0 then
        AddScore 10000
        Add10000 = Add10000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

'*******************
'     BONUS
'*******************

Sub AddBonus(bonuspoints)
    If(Tilted = False)Then
        Bonus = Bonus + bonuspoints
        'PlaySound "Bell2"
        'If Bonus> 10 Then
        '    Bonus = 10
        'End If
        UpdateBonusLights
    End if
End Sub

Sub AddBonusMultiplier(multi)
    If(Tilted = False)Then
        BonusMultiplier = BonusMultiplier + multi
        If BonusMultiplier > MaxMultiplier Then
            BonusMultiplier = MaxMultiplier
        End If
        UpdateMultiplierLights
    End if
End Sub

Sub UpdateMultiplierLights
    Select Case BonusMultiplier
    'Case 1:li2.State = 1:li3.State = 0:li4.State = 0
    'Case 2:li2.State = 0:li3.State = 1:li4.State = 0
    'Case 3:li2.State = 0:li3.State = 0:li4.State = 1
    End Select
End Sub

Sub StartBonusCount
    ' prevent the ball from draining
    Post2.IsDropped = 0
    Select Case BonusMultiplier
        Case 1:BonusCountTimer.Interval = 250
        Case 2:BonusCountTimer.Interval = 400
        Case 3:BonusCountTimer.Interval = 650
    End Select
    BonusCountTimer.Enabled = 1
End Sub

Sub BonusCountTimer_Timer
    'debug.print "BonusCount_Timer"
    If Bonus > 0 Then
        Bonus = Bonus -1
        AddScore 10000 * BonusMultiplier
        UpdateBonusLights
    Else
        BonusCountTimer.Enabled = 0
        'Enable the drain again
        Post2.IsDropped = 1
        'set bonus to 1
        Addbonus 1
        'and reset multiplier
        BonusMultiplier = 1
        li009.State = 1
        li010.State = 0
        li004.State = 1
        BumpersOff
        If BallinBonusKicker Then
            vpmtimer.addtimer 1000, "BonusKickOut '"
        End If
    'vpmtimer.addtimer 1000, "EndOfBall2 '"
    End If
End Sub

Sub UpdateBonusLights
    Select Case Bonus
        Case 0:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 1:bl1.State = 1:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0:PlaySound "Advance"
        Case 2:bl1.State = 0:bl2.State = 1:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0:PlaySound "Advance"
        Case 3:bl1.State = 0:bl2.State = 0:bl3.State = 1:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0:PlaySound "Advance"
        Case 4:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 1:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0:PlaySound "Advance"
        Case 5:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 1:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0:PlaySound "Advance"
        Case 6:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 1:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0:PlaySound "Advance"
        Case 7:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 1:bl8.State = 0:bl9.State = 0:bl10.State = 0:PlaySound "Advance"
        Case 8:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 1:bl9.State = 0:bl10.State = 0:PlaySound "Advance"
        Case 9:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 1:bl10.State = 0:PlaySound "Advance"
        Case 10:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 1:PlaySound "Advance"
        Case 11:Bonus = 10
    End Select
End Sub

'**********************************
'        Score EM reels
'**********************************

Sub UpdateScore(playerpoints)
    Select Case CurrentPlayer
        Case 1:ScoreReel1.Addvalue playerpoints
    'Case 2:ScoreReel2.Addvalue playerpoints
    'Case 3:ScoreReel3.Addvalue playerpoints
    'Case 4:ScoreReel4.Addvalue playerpoints

    End Select
    If B2SOn then
        Controller.B2SSetScorePlayer CurrentPlayer, Score(CurrentPlayer)
    end if
End Sub

Sub ResetScores
    ScoreReel1.ResetToZero
    'ScoreReel2.ResetToZero
    'ScoreReel3.ResetToZero
    'ScoreReel4.ResetToZero
    If B2SOn then
        Controller.B2SSetScorePlayer1 0
        Controller.B2SSetScoreRolloverPlayer1 0
    'Controller.B2SSetScorePlayer2 0
    'Controller.B2SSetScoreRolloverPlayer2 0
    'Controller.B2SSetScorePlayer3 0
    'Controller.B2SSetScoreRolloverPlayer3 0
    'Controller.B2SSetScorePlayer4 0
    'Controller.B2SSetScoreRolloverPlayer4 0
    end if
End Sub

Sub AddCredits(value) 'limit to 15 credits
    If Credits < 9 Then
        Credits = Credits + value
        UpdateCredits
    end if
End Sub

Sub UpdateCredits
    PlaySound "fx_relay"
    CreditsReel.SetValue credits
    If B2SOn then
        Controller.B2SSetCredits Credits
    end if
End Sub

Sub UpdateBallInPlay
    ballnr = 1 + BallsPerGame - balls + ExtraBallsAwards(CurrentPlayer)
    Select Case balls
        Case 0:BallDisplay.ImageA = "Ballnr0"
        Case 1:BallDisplay.ImageA = "Ballnr1"
        Case 2:BallDisplay.ImageA = "Ballnr2"
        Case 3:BallDisplay.ImageA = "Ballnr3"
        Case 4:BallDisplay.ImageA = "Ballnr4"
        Case 5:BallDisplay.ImageA = "Ballnr5"
    End Select
    If ballnr = 1 Then
        LastBallR.SetValue 1
        If B2SOn then
            Controller.B2SSetData 120, 1 'Lastball
        end if
    Else
        LastBallR.SetValue 0
        If B2SOn then
            Controller.B2SSetData 120, 0 'Lastball
        end if
    End If
'If B2SOn then
'    Controller.B2SSetBallInPlay ballnr
'end if
End Sub

Sub UpdatePlayersPlaying
    Select Case PlayersPlayingGame
    'Case 0:cp1.State = 0:cp2.State = 0:cp3.State = 0:cp4.State = 0
    'Case 1:cp1.State = 1:cp2.State = 0:cp3.State = 0:cp4.State = 0
    'Case 2:cp1.State = 1:cp2.State = 1:cp3.State = 0:cp4.State = 0
    'Case 3:cp1.State = 1:cp2.State = 1:cp3.State = 1:cp4.State = 0
    'Case 4:cp1.State = 1:cp2.State = 1:cp3.State = 1:cp4.State = 1
    End Select
    If B2SOn then
    'Controller.B2SSetPlayersPlaying PlayersPlayingGame
    end if
End Sub

Sub UpdateCurrentPlayer
    Select Case CurrentPlayer
    'Case 0:pl1.State = 0:pl2.State = 0:pl3.State = 0:pl4.State = 0
    'Case 1:pl1.State = 1:pl2.State = 0:pl3.State = 0:pl4.State = 0
    'Case 2:pl1.State = 0:pl2.State = 1:pl3.State = 0:pl4.State = 0
    'Case 3:pl1.State = 0:pl2.State = 0:pl3.State = 1:pl4.State = 0
    'Case 4:pl1.State = 0:pl2.State = 0:pl3.State = 0:pl4.State = 1
    End Select
    If B2SOn then
    'Controller.B2SSetCurrentPlayer CurrentPlayer
    end if
End Sub

'*************************
'        Specials
'*************************

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        PlaySound "fx_knocker"
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
    Else
        Addscore 5000
    END If
End Sub

Sub AwardSpecial()
    PlaySound "fx_knocker"
    AddCredits 1
End Sub

Sub AwardAddaBall()
    If BallsRemaining(CurrentPlayer) < 11 Then
        PlaySound "fx_knocker"
        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) + 1
        UpdateBallInPlay
    End If
End Sub

' ********************************
'        Attract Mode
' ********************************
' use the"Blink Pattern" of each light

Sub StartAttractMode()
    Dim x
    bAttractMode = True
    For each x in aLights
        x.State = 2
    Next
    GameOverR.SetValue 1
    'update current player and balls
    BallDisplay.ImageA = "Ballnr0"
End Sub

Sub StopAttractMode()
    Dim x
    bAttractMode = False
    TurnOffPlayfieldLights
    ResetScores
    GameOverR.SetValue 0
End Sub

'*************************************
'       Highscore Routines
' Based og Black's and GNMOD's code
'*************************************

Dim EnteringInitials
EnteringInitials = False

Dim SelectedChar   ' character under the "cursor" when entering initials
Dim HSTimerCount:HSTimerCount = 0
Dim InitialString  ' the string holding the player's initials as they're entered
Dim AlphaString:AlphaString = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_<"
Dim AlphaStringPos ' pointer to AlphaString, move Forward and backward with flipper keys
Dim HSNewHigh      ' The new score to be recorded
Dim HSScore(5)     ' High Scores read in from config file
Dim HSName(5)      ' High Score Initials read in from config file

' default high scores
HSScore(1) = 250000
HSScore(2) = 240000
HSScore(3) = 230000
HSScore(4) = 220000
HSScore(5) = 210000

HSName(1) = "AAA"
HSName(2) = "ZZZ"
HSName(3) = "XXX"
HSName(4) = "ABC"
HSName(5) = "BBB"

' Load Highscores

Sub Loadhs
    Dim FileObj
    Dim ScoreFile, TextStr
    Dim check0, x

    Set FileObj = CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory)then
        Credits = 0
        Exit Sub
    End If
    If Not FileObj.FileExists(UserDirectory & cGameName& ".txt")then
        Credits = 0
        Exit Sub
    End If
    Set ScoreFile = FileObj.GetFile(UserDirectory & cGameName& ".txt")
    Set TextStr = ScoreFile.OpenAsTextStream(1, 0)
    If(TextStr.AtEndOfStream = True)then
        Exit Sub
    End If
    check0 = Int(textStr.readLine)
    If check0 = 0 then
        Credits = Int(textstr.readline)
        For x = 1 to 5
            HSScore(x) = Int(textstr.readline)
        Next
        For x = 1 to 5
            HSName(x) = textstr.readline
        Next
    End If
    TextStr.Close
    Set ScoreFile = Nothing
    Set FileObj = Nothing
    SortHighscore 'added to fix a previous error
End Sub

' Save Highscores

Sub Savehs
    Dim FileObj
    Dim ScoreFile
    Dim x
    Set FileObj = CreateObject("Scripting.FileSystemObject")
    If Not FileObj.FolderExists(UserDirectory)then
        Exit Sub
    End If
    Set ScoreFile = FileObj.CreateTextFile(UserDirectory & cGameName& ".txt", True)
    ScoreFile.WriteLine 0
    ScoreFile.WriteLine Credits
    For x = 1 to 5
        scorefile.writeline HSScore(x)
    Next
    For x = 1 to 5
        scorefile.writeline HSName(x)
    Next
    ScoreFile.Close
    Set ScoreFile = Nothing
    Set FileObj = Nothing
End Sub

' Sort Highscores

Sub SortHighscore
    Dim tmp, tmp2, i, j
    For i = 1 to 5
        For j = 1 to 4
            If HSScore(j) < HSScore(j + 1)Then
                tmp = HSScore(j + 1)
                tmp2 = HSName(j + 1)
                HSScore(j + 1) = HSScore(j)
                HSName(j + 1) = HSName(j)
                HSScore(j) = tmp
                HSName(j) = tmp2
            End If
        Next
    Next
End Sub

' check for new highscore

Sub CheckHighscore
    HighScoreTimer.Enabled = 1
    'If Score(CurrentPlayer)> HSSCore(1)Then
    '   AwardSpecial
    'End I
    If Score(CurrentPlayer) > HSSCore(5)Then
        NewHighScore Score(CurrentPlayer), CurrentPlayer
    End If
End Sub

' Highscore Timer

Sub HighScoreTimer_Timer
    If EnteringInitials then
        postitlight.State = 1
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
    ElseIf bGameInPlay then
        SetHSLine 1, "HIGH SCORE"
        SetHSLine 2, HSScore(1)
        SetHSLine 3, HSName(1)
        Me.Enabled = 0
    Else
        ' cycle through high scores
        SetHSLine 1, "HIGH SCORE" & (HSTimerCount + 1)
        SetHSLine 2, HSScore(HSTimerCount + 1)
        SetHSLine 3, HSName(HSTimerCount + 1)
        HSTimerCount = (HSTimerCount + 1)MOD 5
        postitlight.State = 0
    End If
End Sub

' Enter Highscore initials

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

Sub SetHsLine(LineNo, String)
    Dim StrLen
    Dim Index
    Dim StartHSArray
    Dim EndHSArray
    Dim xFor
    StartHSArray = array(0, 1, 12, 22)
    EndHSArray = array(0, 11, 21, 31)
    StrLen = len(string)
    Index = 1
    For xFor = StartHSArray(LineNo)to EndHSArray(LineNo)
        Eval("HS" &xFor).imageA = GetHSChar(String, Index)
        Index = Index + 1
    Next
End Sub

Sub NewHighScore(NewScore, PlayNum)
    If NewScore > HSScore(5)then
        HighScoreTimer.interval = 500
        AlphaStringPos = 1      ' start with first character "A"
        EnteringInitials = true ' intercept the control keys while entering initials
        InitialString = ""      ' initials entered so far, initialize to empty
        SetHSLine 1, "PLAYER " + FormatNumber(PlayNum, 0)
        SetHSLine 2, "ENTER NAME"
        SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
        HSNewHigh = NewScore
    End If
End Sub

Sub CollectInitials(keycode)
    Dim i
    If keycode = LeftFlipperKey Then
        ' back up to previous character
        AlphaStringPos = AlphaStringPos - 1
        If AlphaStringPos < 1 then
            AlphaStringPos = len(AlphaString) ' handle wrap from beginning to End
            If InitialString = "" then
                ' Skip the backspace If there are no characters to backspace over
                AlphaStringPos = AlphaStringPos - 1
            End If
        End If
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
        PlaySound "fx_Previous"
    ElseIf keycode = RightFlipperKey Then
        ' advance to Next character
        AlphaStringPos = AlphaStringPos + 1
        If AlphaStringPos > len(AlphaString)or(AlphaStringPos = len(AlphaString)and InitialString = "")then
            ' Skip the backspace If there are no characters to backspace over
            AlphaStringPos = 1
        End If
        SetHSLine 3, InitialString & MID(AlphaString, AlphaStringPos, 1)
        PlaySound "fx_Next"
    ElseIf keycode = StartGameKey or keycode = PlungerKey Then
        SelectedChar = MID(AlphaString, AlphaStringPos, 1)
        If SelectedChar = "_" then
            InitialString = InitialString & " "
            PlaySound("fx_Esc")
        ElseIf SelectedChar = "<" then
            InitialString = MID(InitialString, 1, len(InitialString)- 1)
            If len(InitialString) = 0 then
                ' If there are no more characters to back over, don't leave the < displayed
                AlphaStringPos = 1
            End If
            PlaySound("fx_Esc")
        Else
            InitialString = InitialString & SelectedChar
            PlaySound("fx_Enter")
        End If
        If len(InitialString) < 3 then
            SetHSLine 3, InitialString & SelectedChar
        End If
    End If
    If len(InitialString) = 3 then
        EnteringInitials = False
        HSScore(5) = HSNewHigh
        HSName(5) = InitialString
        SortHighScore
        HighScoreTimer.interval = 2000
    End If
End Sub

'*******************
' Realtime updates
'*******************

Sub RealTime_Timer
    RollingUpdate
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

'***********************************************************************
' *********************************************************************
'  *********     G A M E  C O D E  S T A R T S  H E R E      *********
' *********************************************************************
'***********************************************************************

Sub VPObjects_Init 'init objects
    TurnOffPlayfieldLights()
    Post2.IsDropped = 1
End Sub

' Dim all the variables

Sub Game_Init() 'called at the start of a new game
    'Start music?
    'Init variables?
    'Start or init timers
    'Init lights?
    TurnOffPlayfieldLights
    TurnOffHorseLights
    li016.State = 1
    li017.State = 1
    li018.State = 1
    li019.State = 1
    li020.State = 1
    li005.State = 0
    li006.State = 0
    li015.State = 0
    Bonus = 0
    AddBonus 1
End Sub

Sub StopEndOfBallMode()     'called when the last ball is drained
End Sub

Sub ResetNewBallVariables() 'init variables new ball/player
    BonusMultiplier = 1
End Sub

Sub ResetNewBallLights() 'init lights for new ball/player
    TurnOffPlayfieldLights()
    li004.State = 1
    li009.State = 1
    li012.State = 1
    li013.State = 1
    li003.State = 1
    li001.State = 1
    li032.State = 1
    li033.State = 1
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

Sub TurnOffHorseLights()
    Dim a
    For each a in aHorseLights
        a.State = 0
    Next
    If B2SOn then
        Controller.B2SSetData 121, 0 'turn off the db2s lights too
        Controller.B2SSetData 122, 0
        Controller.B2SSetData 123, 0
        Controller.B2SSetData 124, 0
        Controller.B2SSetData 125, 0
    End If
End Sub

' *********************************************************************
'                        Table Object Hit Events
' *********************************************************************
' Any target hit Sub will do this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/modes this trigger is a member of
' - set the "LastSwicthHit" variable in case it is needed later
' *********************************************************************

' Slingshots
Dim LStep, RStep

Sub LeftSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 103, DOFPulse, DOFContactors), Lemk
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add points
    AddScore 10
    ' some effect?
End Sub

Sub LeftSlingShot_Timer
    Select Case LStep
        Case 1:LeftSLing004.Visible = 0:LeftSLing003.Visible = 1:Lemk.RotX = 14
        Case 2:LeftSLing003.Visible = 0:LeftSLing002.Visible = 1:Lemk.RotX = 2
        Case 3:LeftSLing002.Visible = 0:Lemk.RotX = -20:LeftSlingShot.TimerEnabled = 0
    End Select
    LStep = LStep + 1
End Sub

Sub RightSlingShot_Slingshot
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_slingshot", 105, DOFPulse, DOFContactors), Remk
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add points
    AddScore 10
    ' add effect?
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing002.Visible = 0:Remk.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

'**************
'   Rubbers
'**************

Sub rsband001_Hit
    If Tilted then Exit Sub
    AddScore 10
End Sub

Sub rsband002_Hit
    If Tilted then Exit Sub
    AddScore 10
End Sub

'*********
' Bumpers
'*********

Sub Bumper001_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_Bumper", 107, DOFPulse, DOFContactors), bumper001
    AddScore 1000 + 9000 * LBumper1a.State
End Sub

Sub Bumper002_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_Bumper", 108, DOFPulse, DOFContactors), bumper002
    AddBonus 1
End Sub

Sub Bumper003_Hit
    If Tilted Then Exit Sub
    PlaySoundAt SoundFXDOF("fx_Bumper", 109, DOFPulse, DOFContactors), bumper003
    AddScore 1000 + 9000 * LBumper3a.State
End Sub

Sub BumpersOn
    LBumper1a.State = 1
    LBumper1b.State = 1
    LBumper3a.State = 1
    LBumper3b.State = 1
End Sub

Sub BumpersOff
    LBumper1a.State = 0
    LBumper1b.State = 0
    LBumper3a.State = 0
    LBumper3b.State = 0
End Sub

'*****************
'     Lanes
'*****************

' inlanes & outlanes

Sub Trigger001_Hit
    PlaySoundAt "fx_sensor", Trigger001
    If Tilted Then Exit Sub
    AddScore 5000
    li020.State = 0
    li025.State = 1
    If B2SOn then
        Controller.B2SSetData 125, 1
    End If
    Check12345
    If li005.State Then AwardSpecial
End Sub

Sub Trigger002_Hit
    PlaySoundAt "fx_sensor", Trigger002
    If Tilted Then Exit Sub
    AddBonus 1
End Sub

Sub Trigger003_Hit
    PlaySoundAt "fx_sensor", Trigger003
    If Tilted Then Exit Sub
    AddBonus 1
End Sub

Sub Trigger004_Hit
    PlaySoundAt "fx_sensor", Trigger004
    If Tilted Then Exit Sub
    Post2.IsDropped = 0
    StartBonusCount
End Sub

' top lanes

Sub Trigger005_Hit
    PlaySoundAt "fx_sensor", Trigger005
    If Tilted Then Exit Sub
    AddScore 1000
    li016.State = 0
    li015.State = 1
    li021.State = 1
    If B2SOn then
        Controller.B2SSetData 121, 1
    End If
    Check12345
End Sub

Sub Trigger006_Hit
    PlaySoundAt "fx_sensor", Trigger006
    If Tilted Then Exit Sub
    AddScore 5000 + 45000 * li015.State
End Sub

Sub Trigger007_Hit
    PlaySoundAt "fx_sensor", Trigger007
    If Tilted Then Exit Sub
    AddScore 1000
    li017.State = 0
    li022.State = 1
    If B2SOn then
        Controller.B2SSetData 122, 1
    End If
    Check12345
End Sub

' 1 2 3 4 5 Horses

Sub Check12345
    If li021.State + li022.State + li023.State + li024.State + li025.State = 5 Then 'turn on lower Specials
    If li006.State = 0 Then
        AwardSpecial
        li006.State = 1
        li005.State = 1
    End If
    End If
End Sub

'************************
'       Targets
'************************

Sub Target001_hit
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    AddBonus 1
    li018.State = 0
    li023.State = 1
    If B2SOn then
        Controller.B2SSetData 123, 1
    End If
    Check12345
End Sub

Sub Target002_hit
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    AddBonus 1
    li019.State = 0
    li024.State = 1
    If B2SOn then
        Controller.B2SSetData 124, 1
    End If
    Check12345
End Sub

Sub Target003_hit
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    AddScore 5000
    If li004.State Then
        BonusMultiplier = 3
        BumpersOn
        li009.State = 0
        li010.State = 1
        li004.State = 0
    End If
End Sub

Sub Target004_hit
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    AddScore 5000
    If li006.State Then AwardSpecial
End Sub

Sub Target005_hit
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    AddScore 5000 + 45000 * li007.State
End Sub

Sub Target006_hit
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    AddBonus 1
End Sub

Sub Target007_hit
    PlaySoundAtBall "fx_target"
    If Tilted Then Exit Sub
    AddScore 5000 + 45000 * li008.State
End Sub

'*****************
'    kickers
'*****************

' left yellow hole
Sub LeftHole_Hit
    PlaySoundAt "fx_kicker_enter", LeftHole
    If NOT Tilted Then
        Addscore 5000
        li003.State = 0
        li012.State = 0
        li008.State = 1
        CheckCenterSpecial
    End If
    vpmtimer.addtimer 1000, "ExitLeftHole '"
End Sub

Sub ExitLeftHole
    PlaySoundAt SoundFXDOF("fx_kicker", 111, DOFPulse, DOFContactors), LeftHole
    LeftHole.kick 150, 14
End Sub

Sub CheckCenterSpecial
    If li003.State + li001.State = 0 Then
        li011.State = 1
        li014.State = 1
    End If
End Sub

' center red hole
Sub CenterHole_Hit
    PlaySoundAt "fx_kicker_enter", CenterHole
    If NOT Tilted Then
        If li014.State Then
            AwardSpecial
        Else
            Addscore 50000
        End If
    End If
    vpmtimer.addtimer 1000, "ExitCenterHole '"
End Sub

Sub ExitCenterHole
    PlaySoundAt SoundFXDOF("fx_kicker", 112, DOFPulse, DOFContactors), LeftHole
    CenterHole.kick 168, 14
    ResetCenterLights
End Sub

Sub ResetCenterLights
If li014.State Then
li011.State = 0
li014.State = 0
li001.State = 1
li003.State = 1
li012.State = 1
li013.State = 1
li008.State = 0
li007.State = 0
End If
End Sub

' right hole
Sub RightHole_Hit
    PlaySoundAt "fx_kicker_enter", RightHole
    If NOT Tilted Then
        Addscore 5000
        li001.State = 0
        li013.State = 0
        li007.State = 1
        CheckCenterSpecial
    End If
    vpmtimer.addtimer 1000, "ExitRightHole '"
End Sub

Sub ExitRightHole
    PlaySoundAt SoundFXDOF("fx_kicker", 113, DOFPulse, DOFContactors), RightHole
    RightHole.kick 206, 14
End Sub

' Bonus kicker hole

Dim BallinBonusKicker
BallinBonusKicker = False

Sub BonusKicker_Hit
    PlaySoundAt "fx_kicker_enter", BonusKicker
    DOF 119, DOFPulse
    If NOT Tilted Then
        BallinBonusKicker = True
        StartBonusCount
    Else
        BonusKickOut
    End If
End Sub

Sub BonusKickOut
    PlaySoundAt SoundFXDOF("fx_kicker", 114, DOFPulse, DOFContactors), BonusKicker
    BallinBonusKicker = False
    BonusKicker.kick 145, 8
End Sub

'**************
'  Buttons
'**************

Sub Button001_Hit
    PlaySoundAt "fx_sensor", Button001
    Addscore 5000
End Sub

Sub Button002_Hit
    PlaySoundAt "fx_sensor", Button002
    Addscore 5000
End Sub