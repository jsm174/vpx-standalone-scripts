' ***********************************************************************
'               VISUAL PINBALL X EM Script by JPSalas
'                  Basic EM script up to 4 players
'                 uses core.vbs for extra functions
'         Black & Reed / IPD No. 4413 / INDER 1975 / 1 Player
' ***********************************************************************

Option Explicit
Randomize
'
' DOF config - outhere - leeoneil
'
' Option for more lights effects with DOF (Undercab effects on bumpers and slingshots) 
' "True" to activate (False by default)
Const Epileptikdof = False
'
' Flippers L/R - 101/102
' Slingshot L/R - 103/105
' Bumpers L/R - 107/109
' Targets L/R - 111/113/115/120
' Kickers - 114
'
' LED backboard
' Flasher Outside Left - 203/210/212/213/217/240/250
' Flasher left - 207/215/218/221/227/240/250
' Flasher center - 214/222/225/227/240/250
' Flasher right - 208/215/219/223/227/240/250
' Flasher Outside - 205/209/211/216/220/224/226/240/250
' Flashers MX L/R - 204/206
' Start Button - 200
' Undercab - 201/202
' Strobe - 230
' Knocker - 112
' Chimes - 116/117/118/119

' core.vbs constants
Const BallSize = 50  ' 50 is the normal size
Const BallMass = 1   ' 1 is the normal ball mass.

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
Const cGameName = "TheGrinch" ' B2S name
Const MaxPlayers = 1          ' Only 1 player for this table
Const MaxMultiplier = 3       ' limit bonus multiplier
Const BallsPerGame = 5     ' 5 ball game for the Grinch
Const FreePlay = True        ' Free play or coins
Const Special1 = 79000        ' extra ball or credit
Const Special2 = 91000
'Const Special3 = 92000

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
Dim PupBonuscount
Dim PupBonusadd
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
Dim cbLeft
Dim cbRight

' *********************************************************************
'                Common rutines to all the tables
' *********************************************************************

Sub Table1_Init()
    Dim x

    ' Init som objects, like walls, targets
    VPObjects_Init

    LoadEM

    Set cbLeft = New cvpmCaptiveBall
    With cbLeft
        .InitCaptive CapTrigger1, CapWall1, CapKicker1, 0
        .ForceTrans = .9
        .MinForce = 3.5
        .CreateEvents "cbLeft"
        .Start
    End With

    Set cbRight = New cvpmCaptiveBall
    With cbRight
        .InitCaptive CapTrigger2, CapWall2, CapKicker2, 0
        .ForceTrans = .9
        .MinForce = 3.5
        .CreateEvents "cbRight"
        .Start
    End With

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
PUPInit
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)

    If keycode = LeftMagnaSave Then bLutActive = True: Lutbox.text = "level of darkness " & LUTImage + 1
    If keycode = RightMagnaSave Then
        If bLutActive Then NextLUT:End If
    End If

    If EnteringInitials then
        CollectInitials(keycode)
        Exit Sub
    End If

    ' add coins
    If Keycode = AddCreditKey Then
        If(Tilted = False)Then
            AddCredits 1
            PlaySound "fx_coin"
            DOF 200, DOFOn
        End If
    End If

    ' plunger
    If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
    End If

    ' tilt keys
    If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
    If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
    If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt

    ' keys during game

    If bGameInPlay AND NOT Tilted Then
        If keycode = LeftFlipperKey Then SolLFlipper 1
        If keycode = RightFlipperKey Then SolRFlipper 1

        If keycode = StartGameKey Then
            If((PlayersPlayingGame <MaxPlayers)AND(bOnTheFirstBall = True))Then

                If(bFreePlay = True)Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    UpdatePlayersPlaying
                'PlaySound "so_fanfare1"
                Else
                    If(Credits> 0)then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        Credits = Credits - 1
                        UpdateCredits
                        UpdateBallInPlay
                        UpdatePlayersPlaying
                    Else
                    DOF 200, DOFOff
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
                    If(Credits> 0)Then
                        If(BallsOnPlayfield = 0)Then
                            Credits = Credits - 1
                            UpdateCredits
                            ResetScores
                            ResetForNewGame()
                        End If
                    Else
                    DOF 200, DOFOff
                    ' Not Enough Credits to start a game.
                    'PlaySound "so_nocredits"
                    End If
                End If
            End If
    End If ' If (GameInPlay)
End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = LeftMagnaSave Then bLutActive = False: LutBox.text = ""

    If EnteringInitials then
        Exit Sub
    End If

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

'********************
'     Flippers
'********************

Sub SolLFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF ("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.65:LeftFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF ("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper
        LeftFlipper.EOSTorque = 0.15:LeftFlipper.RotateToStart
    End If
End Sub

Sub SolRFlipper(Enabled)
    If Enabled Then
        PlaySoundAt SoundFXDOF ("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.65:RightFlipper.RotateToEnd
    Else
        PlaySoundAt SoundFXDOF ("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper
        RightFlipper.EOSTorque = 0.15:RightFlipper.RotateToStart
    End If
End Sub

' flippers hit Sound

Sub LeftFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub RightFlipper_Collide(parm)
    PlaySound "fx_rubber_flipper", 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

'***********
' GI lights
'***********

Sub GiOn 'enciende las luces GI
    DOF 201, DOFOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
End Sub

Sub GiOff 'apaga las luces GI
    DOF 201, DOFOff
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
    If Tilt> 15 Then
        Tilted = True
		PuPEvent(44)
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
    If Tilt> 0 Then
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
        DOF 101, DOFOff
        DOF 102, DOFOff
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
'  includes random pitch in PlaySoundAt and PlaySoundAtBall
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
Const maxvel = 26 'max ball velocity
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

        ' jps ball speed control
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

'**********************
' Ball Collision Sound
'**********************

Sub OnBallBallCollision(ball1, ball2, velocity)
    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
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
		PupBonuscount= 1000
        ExtraBallsAwards(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        'Special3Awarded(i) = False
        BallsRemaining(i) = BallsPerGame
    Next
    BonusMultiplier = 1
    Bonus = 0

    Clear_Match

    ' init other variables
    Tilt = 0

    ' init game variables
    Game_Init()

    ' start a music?
    ' first ball
	pDMDStartGame
    vpmtimer.addtimer 2000, "FirstBall '"

End Sub

Sub FirstBall
    'debug.print "FirstBall"
    ' reset table for a new ball, rise droptargets +
    ResetForNewPlayerBall()
    CreateNewBall()

    DOF 250, DOFPulse
    If Credits = 0 Then DOF 200, DOFOff End If
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
    UpdateCurrentPlayer
    PlaySoundAt SoundFXDOF ("fx_Ballrel", 105, DOFPulse, DOFContactors), BallRelease
    If Epileptikdof = True Then DOF 226, DOFPulse End If
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
    If(ExtraBallsAwards(CurrentPlayer)> 0)Then
        'debug.print "Extra Ball"

        ' if so then give it
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer)- 1

        ' turn off light if no more extra balls
        If(ExtraBallsAwards(CurrentPlayer) = 0)Then
            XtraBall.SetValue 0
            If B2SOn then
                Controller.B2SSetShootAgain 0
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
		Clearscreen
            
           'CheckHighScore()
        Else

            ' this is not the last ball, chack for new player
            EndOfBallComplete()
        End If
    End If
End Sub

Sub EndOfBallComplete()
    'debug.print "EndOfBallComplete"

    Dim NextPlayer
	
    ' other players?
    If(PlayersPlayingGame> 1)Then
        NextPlayer = CurrentPlayer + 1
        ' if it is the last player then go to the first one
        If(NextPlayer> PlayersPlayingGame)Then
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
            Verification_Match
        End If

        ' end of game
        TheEndPup
		PuPEvent(100)
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


Sub TheEndPup
	PuPEvent(43)
	puPlayer.LabelSet pDMD, "Boncount","" & FormatNumber(PupBonuscount,0),0,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }"
	pDMDSetPage(9) 
	puPlayer.LabelSet pDMD,"GOScore","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }"
	puPlayer.LabelSet pDMD,"GOtext","FINAL SCORE"  ,1,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }"
	PuPEvent(42)
	gotoattract.Enabled= True
	gotoattract.Interval=4950
End Sub


Sub gotoattract_timer
	gotoattract.Enabled= False
	pDMDSetPage(pDMDBlank)
	pDMDmode="GO"
	pDMDGameOver
	vpmtimer.addtimer 1, "EndOfGame'"
End Sub

' Called at the end of the game

Sub EndOfGame()
    'debug.print "EndOfGame"
    bGameInPLay = False
    bJustStarted = False
    GameOverR.SetValue 1
    If B2SOn then
        Controller.B2SSetGameOver 1
        Controller.B2SSetBallInPlay 0
        Controller.B2SSetPlayerUp 0
        Controller.B2SSetCanPlay 0
    end if
    ' turn off flippers
    SolLFlipper 0
    SolRFlipper 0
    DOF 250, DOFPulse

    ' start the attract mode
    StartAttractMode
End Sub

' Fuction to calculate the balls left
Function Balls
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp> BallsPerGame Then
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
    Match = INT(RND(1) * 10) * 10 ' random between 0 and 90
    Display_Match
    If(Score(CurrentPlayer)MOD 100) = Match Then
        PlaySound SoundFXDOF ("fx_knocker",112,DOFPulse,DOFKnocker)
        DOF 230, DOFPulse
        DOF 200, DOFOn
        AddCredits 1
    End If
End Sub

Sub Clear_Match()
    MatchReel.SetValue 0
    If B2SOn then
        Controller.B2SSetMatch 0
    end if
End Sub

Sub Display_Match()
    MatchReel.SetValue 1 + (Match \ 10)
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
	PuPEvent(51)
	PuPEvent(50)
    DOF 227, DOFPulse

    'tilted?
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if still playing and not tilted
    If(bGameInPLay = True)AND(Tilted = False)Then
 
        ' ballsaver?
        If(bBallSaverActive = True)Then
            vpmtimer.addtimer 5500, "CreateNewBall() '"
        Else
            ' last ball?
            If(BallsOnPlayfield = 0)Then
				
                StopEndOfBallMode
                vpmtimer.addtimer 2000, "EndOfBall '"
                Exit Sub
            End If
        End If
    End If
End Sub

Sub Clearscreen
	PuPEvent(40)
	StartBonusCount
End Sub


Sub swPlungerRest_Hit()
    bBallInPlungerLane = True
End Sub

Sub swPlungerRest_UnHit()
    bBallInPlungerLane = False
End Sub

'******************************
'     DOF lights ball entrance
'******************************
'
Sub Trigger013_Hit
    DOF 240, DOFPulse
End sub

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
                PlaySound SoundFXDOF ("bell1000",119,DOFPulse,DOFChimes)
            ElseIf Points = 100 AND(Score(CurrentPlayer)MOD 1000) \ 100 = 0 Then 'new reel 1000
                PlaySound SoundFXDOF ("bell1000",116,DOFPulse,DOFChimes)
            ElseIf Points = 10 AND(Score(CurrentPlayer)MOD 100) \ 10 = 0 Then    'new reel 100
                PlaySound SoundFXDOF ("bell100",117,DOFPulse,DOFChimes)
            Else
                PlaySound SoundFXDOF ("bell",118,DOFPulse,DOFChimes) &Points
            End If
        Case 20, 30, 40, 50, 60, 70, 80, 90
            Add10 = Add10 + + Points \ 10
            AddScore10Timer.Enabled = TRUE
        Case 200, 300, 400, 500, 600, 700, 800, 900
            Add100 = Add100 + + Points \ 100
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
 
        Special1Awarded(CurrentPlayer) = True
    End If
    If Score(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
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
    if Add10> 0 then
        AddScore 10
        Add10 = Add10 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore100Timer_Timer()
    if Add100> 0 then
        AddScore 100
        Add100 = Add100 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore1000Timer_Timer()
    if Add1000> 0 then
        AddScore 1000
        Add1000 = Add1000 - 1
    Else
        Me.Enabled = FALSE
    End If
End Sub

Sub AddScore10000Timer_Timer()
    if Add10000> 0 then
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
        If Bonus> 10 Then
            Bonus = 10
        End If
        UpdateBonusLights
    End if
End Sub

Sub AddBonusMultiplier(multi)
    If(Tilted = False)Then
        BonusMultiplier = BonusMultiplier + multi
        If BonusMultiplier> MaxMultiplier Then
            BonusMultiplier = MaxMultiplier
        End If
        UpdateMultiplierLights
    End if
End Sub

Sub UpdateMultiplierLights
    Select Case BonusMultiplier
        Case 1:bl002.State = 1:bl001.State = 0:bl003.State = 0
        Case 2:bl002.State = 0:bl001.State = 1:bl003.State = 0
        Case 3:bl002.State = 0:bl001.State = 0:bl003.State = 1
    End Select
End Sub

Sub StartBonusCount
	PuPEvent(25)
    Select Case BonusMultiplier
        Case 1:BonusCountTimer.Interval = 250
        Case 2:BonusCountTimer.Interval = 500
        Case 3:BonusCountTimer.Interval = 750
    End Select
    BonusCountTimer.Enabled = 1
End Sub

Sub BonusCountTimer_Timer
    'debug.print "BonusCount_Timer"
    If Bonus> 0 Then
        Bonus = Bonus -1
        AddScore 1000 * BonusMultiplier
		Pupcountingbonus
		pDMDSetPage(9) 
		puPlayer.LabelSet pDMD, "Boncount","" & FormatNumber(PupBonuscount,0),1,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }"
        UpdateBonusLights
		PupBonusLights
    Else
        BonusCountTimer.Enabled = 0
        'set bonus to 1
        Addbonus 1
        If BallinBonusKicker Then
			
            vpmtimer.addtimer 2000, "BonusKickOut '"
        End If
        ' last ball?
        If(BallsRemaining(CurrentPlayer) <= 0)Then
            CheckHighScore()
 
End If
    End If
End Sub

Sub PupBonusLights
    Select Case Bonus
        Case 0:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0:PupBonuscount=PupBonuscount+1000 * (BonusMultiplier)
        Case 1:bl1.State = 1:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0:Minusthousand:Minustwothousand:PupBonuscount=PupBonuscount+1000 * (BonusMultiplier)
        Case 2:bl1.State = 0:bl2.State = 1:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0:Minusthousand:Minustwothousand:PupBonuscount=PupBonuscount+1000 * (BonusMultiplier)
        Case 3:bl1.State = 0:bl2.State = 0:bl3.State = 1:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0:Minusthousand:Minustwothousand:PupBonuscount=PupBonuscount+1000 * (BonusMultiplier)
        Case 4:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 1:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0:Minusthousand:Minustwothousand:PupBonuscount=PupBonuscount+1000 * (BonusMultiplier)
        Case 5:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 1:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0:Minusthousand:Minustwothousand:PupBonuscount=PupBonuscount+1000 * (BonusMultiplier)
        Case 6:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 1:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0:Minusthousand:Minustwothousand:PupBonuscount=PupBonuscount+1000 * (BonusMultiplier)
        Case 7:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 1:bl8.State = 0:bl9.State = 0:bl10.State = 0:Minusthousand:Minustwothousand:PupBonuscount=PupBonuscount+1000 * (BonusMultiplier)
        Case 8:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 1:bl9.State = 0:bl10.State = 0:Minusthousand:Minustwothousand:PupBonuscount=PupBonuscount+1000 * (BonusMultiplier)
        Case 9:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 1:bl10.State = 0:Minusthousand:Minustwothousand:PupBonuscount=PupBonuscount+1000 * (BonusMultiplier)
        Case 10:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 1:Minusthousand:Minustwothousand:PupBonuscount=PupBonuscount+1000 * (BonusMultiplier)
    End Select
End Sub



Sub UpdateBonusLights
    Select Case Bonus
        Case 0:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 1:bl1.State = 1:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 2:bl1.State = 0:bl2.State = 1:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 3:bl1.State = 0:bl2.State = 0:bl3.State = 1:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 4:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 1:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 5:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 1:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 6:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 1:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 7:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 1:bl8.State = 0:bl9.State = 0:bl10.State = 0
        Case 8:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 1:bl9.State = 0:bl10.State = 0
        Case 9:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 1:bl10.State = 0
        Case 10:bl1.State = 0:bl2.State = 0:bl3.State = 0:bl4.State = 0:bl5.State = 0:bl6.State = 0:bl7.State = 0:bl8.State = 0:bl9.State = 0:bl10.State = 1
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
        Controller.B2SSetScorePlayer2 0
        Controller.B2SSetScoreRolloverPlayer2 0
        Controller.B2SSetScorePlayer3 0
        Controller.B2SSetScoreRolloverPlayer3 0
        Controller.B2SSetScorePlayer4 0
        Controller.B2SSetScoreRolloverPlayer4 0
    end if
End Sub

Sub AddCredits(value) 'limit to 15 credits
    If Credits <15 Then
        Credits = Credits + value
        CreditsReel.SetValue Credits
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



Sub UpdatePlayersPlaying
'    Select Case PlayersPlayingGame
'        Case 0:cp1.State = 0:cp2.State = 0:cp3.State = 0:cp4.State = 0
'        Case 1:cp1.State = 1:cp2.State = 0:cp3.State = 0:cp4.State = 0
'        Case 2:cp1.State = 1:cp2.State = 1:cp3.State = 0:cp4.State = 0
'        Case 3:cp1.State = 1:cp2.State = 1:cp3.State = 1:cp4.State = 0
'        Case 4:cp1.State = 1:cp2.State = 1:cp3.State = 1:cp4.State = 1
'    End Select
'    If B2SOn then
'        Controller.B2SSetPlayersPlaying PlayersPlayingGame
'    end if
End Sub

Sub UpdateCurrentPlayer
'    Select Case CurrentPlayer
'        Case 0:pl1.State = 0:pl2.State = 0:pl3.State = 0:pl4.State = 0
'        Case 1:pl1.State = 1:pl2.State = 0:pl3.State = 0:pl4.State = 0
'        Case 2:pl1.State = 0:pl2.State = 1:pl3.State = 0:pl4.State = 0
'        Case 3:pl1.State = 0:pl2.State = 0:pl3.State = 1:pl4.State = 0
'        Case 4:pl1.State = 0:pl2.State = 0:pl3.State = 0:pl4.State = 1
'    End Select
'    If B2SOn then
'        Controller.B2SSetCurrentPlayer CurrentPlayer
'    end if
End Sub

'*************************
'        Specials
'*************************

Sub AwardExtraBall()
    If NOT bExtraBallWonThisBall Then
        PlaySound SoundFXDOF ("fx_knocker",112,DOFPulse,DOFKnocker)
        DOF 230, DOFPulse
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
        bExtraBallWonThisBall = True
        XtraBall.SetValue 1
    Else
        Addscore 5000
    END If
End Sub

Sub AwardSpecial()
	AddScore 50000
    PlaySound SoundFXDOF ("fx_knocker",112,DOFPulse,DOFKnocker)
    DOF 230, DOFPulse
    DOF 200, DOFOn
End Sub

Sub AwardAddaBall()
    If BallsRemaining(CurrentPlayer) <11 Then
        PlaySound SoundFXDOF ("fx_knocker",112,DOFPulse,DOFKnocker)
        DOF 230, DOFPulse
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
    For each x in aBonusLights
        x.State = 2
    Next
    GameOverR.SetValue 1
    'update current player and balls
    'start the highscore timer
    HighScoreTimer.Enabled = 1
    If Credits > 0 Then DOF 200, DOFOn End If
    If Credits = 0 Then DOF 200, DOFOff End If
    DOF 201, DOFOff
End Sub

Sub StopAttractMode()
    Dim x
    bAttractMode = False
    TurnOffPlayfieldLights
    For each x in aBonusLights
        x.State = 0
    Next
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
HSScore(1) = 51250
HSScore(2) = 43000
HSScore(3) = 35000
HSScore(4) = 28500
HSScore(5) = 16000

HSName(1) = "AAA"
HSName(2) = "BBB"
HSName(3) = "CCC"
HSName(4) = "DDD"
HSName(5) = "EEE"

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
        Credits = Int("0"&textstr.readline)
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
            If HSScore(j) <HSScore(j + 1)Then
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


Sub CheckHighscore
    If Score(CurrentPlayer)> HSSCore(5)Then
		puPlayer.LabelSet pDMD, "Boncount","" & FormatNumber(PupBonuscount,0),0,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }"
		PuPEvent(37)
		PuPEvent(49)
        NewHighScore Score(CurrentPlayer), CurrentPlayer
	End If
	If Score(CurrentPlayer)< HSSCore(5)Then
        EndOfBallComplete()
    End If
End Sub

' Highscore Timer

Sub HighScoreTimer_Timer
    If EnteringInitials then
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

	if EnteringInitials=True then pDMDSetPage(9):PuPlayer.LabelSet pDMD, "EnterHS" & LineNo , String ,1,""
	for xfor = StartHSArray(LineNo) to EndHSArray(LineNo)
		Eval("HS"&xfor).imageA = GetHSChar(String, Index)
		Index = Index + 1
	next
End Sub

Sub NewHighScore(NewScore, PlayNum)
    If NewScore> HSScore(5)then
        HighScoreTimer.interval = 500
        HighScoreTimer.Enabled = 1
        AlphaStringPos = 1      ' start with first character "A"
        EnteringInitials = true ' intercept the control keys while entering initials
        InitialString = ""      ' initials entered so far, initialize to empty
        SetHSLine 1, "PLAYER " + FormatNumber(PlayNum, 0)
        SetHSLine 2, "ENTER NAME"
        SetHSLine 3, MID(AlphaString, AlphaStringPos, 1)
        HSNewHigh = NewScore
	puPlayer.LabelSet pDMD,"GOScore","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }"
    End If	
End Sub

Sub CollectInitials(keycode)
    Dim i
    If keycode = LeftFlipperKey Then
        ' back up to previous character
        AlphaStringPos = AlphaStringPos - 1
        If AlphaStringPos <1 then
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
        If AlphaStringPos> len(AlphaString)or(AlphaStringPos = len(AlphaString)and InitialString = "")then
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
        If len(InitialString) <3 then
            SetHSLine 3, InitialString & SelectedChar
        End If
    End If
    If len(InitialString) = 3 then
        EnteringInitials = False
        HSScore(5) = HSNewHigh
        HSName(5) = InitialString
        SortHighScore
        HighScoreTimer.interval = 2000
	PuPEvent(60)
	pDMDSetPage(pDMDBlank)
	TheEndPup
    End If
End Sub

'*******************
' Realtime updates
'*******************

Sub RealTime_Timer
    RollingUpdate
    LeftFlipperTop.RotZ = LeftFlipper.CurrentAngle
    RightFlipperTop.RotZ = RightFlipper.CurrentAngle
End Sub

'***************************
'   LUT - Darkness control 
'***************************

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

Sub NextLUT:LUTImage = (LUTImage + 1)MOD 15:UpdateLUT:SaveLUT:Lutbox.text = "level of darkness " & LUTImage + 1:End Sub

Sub UpdateLUT
    Select Case LutImage
        Case 0:table1.ColorGradeImage = "LUT0":GiIntensity = 1:ChangeGIIntensity 1
        Case 1:table1.ColorGradeImage = "LUT1":GiIntensity = 1.05:ChangeGIIntensity 1
        Case 2:table1.ColorGradeImage = "LUT2":GiIntensity = 1.1:ChangeGIIntensity 1
        Case 3:table1.ColorGradeImage = "LUT3":GiIntensity = 1.15:ChangeGIIntensity 1
        Case 4:table1.ColorGradeImage = "LUT4":GiIntensity = 1.2:ChangeGIIntensity 1
        Case 5:table1.ColorGradeImage = "LUT5":GiIntensity = 1.25:ChangeGIIntensity 1
        Case 6:table1.ColorGradeImage = "LUT6":GiIntensity = 1.3:ChangeGIIntensity 1
        Case 7:table1.ColorGradeImage = "LUT7":GiIntensity = 1.35:ChangeGIIntensity 1
        Case 8:table1.ColorGradeImage = "LUT8":GiIntensity = 1.4:ChangeGIIntensity 1
        Case 9:table1.ColorGradeImage = "LUT9":GiIntensity = 1.45:ChangeGIIntensity 1
        Case 10:table1.ColorGradeImage = "LUT10":GiIntensity = 1.5:ChangeGIIntensity 1
        Case 11:table1.ColorGradeImage = "LUT11":GiIntensity = 1.55:ChangeGIIntensity 1
        Case 12:table1.ColorGradeImage = "LUT12":GiIntensity = 1.6:ChangeGIIntensity 1
        Case 13:table1.ColorGradeImage = "LUT13":GiIntensity = 1.65:ChangeGIIntensity 1
        Case 14:table1.ColorGradeImage = "LUT14":GiIntensity = 1.7:ChangeGIIntensity 1
    End Select
End Sub

Dim GiIntensity
GiIntensity = 1   'used by the LUT changing to increase the GI lights when the table is darker

Sub ChangeGiIntensity(factor) 'changes the intensity scale
    Dim bulb
    For each bulb in aGiLights
        bulb.IntensityScale = GiIntensity * factor
    Next
End Sub

'***********************************************************************
' *********************************************************************
'  *********     G A M E  C O D E  S T A R T S  H E R E      *********
' *********************************************************************
'***********************************************************************

Sub VPObjects_Init 'init objects
    TurnOffPlayfieldLights()
    Kicker001.CreateSizedBallWithMass BallSize / 2, BallMass:kicker001.kick 180, 0
    Kicker002.CreateSizedBallWithMass BallSize / 2, BallMass:kicker002.kick 180, 0
End Sub

' Dim all the variables
Dim SpecialActivated
Dim BallinBonusKicker
Dim LeftSpec
Dim RightSpec

Sub Game_Init() 'called at the start of a new game
    'Start music? Init variables? Start or init timers Init lights?
    TurnOffPlayfieldLights()
    bl020.State = 1
    bl021.State = 1
    bl013.State = 1
    bl012.State = 1
    bl008.State = 1
    bl009.State = 1
    Bonus = 0
    AddBonus 1
    BonusMultiplier = 1
    UpdateMultiplierLights
    SpecialActivated = False
    BallinBonusKicker = False
    CheckCenterSpecial
End Sub

Sub StopEndOfBallMode()     'called when the last ball is drained
End Sub

Sub ResetNewBallVariables() 'init variables new ball/player

End Sub

Sub ResetNewBallLights() 'init lights for new ball/player
'TurnOffPlayfieldLights()
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
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
    PlaySoundAt SoundFXDOF ("fx_slingshot",103,DOFPulse,DOFcontactors), Lemk
    DOF 203, DOFPulse
    If Epileptikdof = True Then DOF 204, DOFPulse End If
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    LeftSling004.Visible = 1
    Lemk.RotX = 26
    LStep = 0
    LeftSlingShot.TimerEnabled = True
    ' add points
    AddScore 10
    ' some effect?
    RotateSpecialLights
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
    PlaySoundAt SoundFXDOF ("fx_slingshot",105,DOFPulse,DOFcontactors), Remk
    DOF 205, DOFPulse
    If Epileptikdof = True Then DOF 206, DOFPulse End If
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    RightSling004.Visible = 1
    Remk.RotX = 26
    RStep = 0
    RightSlingShot.TimerEnabled = True
    ' add points
    AddScore 10
    ' add effect?
    RotateSpecialLights
End Sub

Sub RightSlingShot_Timer
    Select Case RStep
        Case 1:RightSLing004.Visible = 0:RightSLing003.Visible = 1:Remk.RotX = 14
        Case 2:RightSLing003.Visible = 0:RightSLing002.Visible = 1:Remk.RotX = 2
        Case 3:RightSLing002.Visible = 0:Remk.RotX = -20:RightSlingShot.TimerEnabled = 0
    End Select
    RStep = RStep + 1
End Sub

Sub RotateSpecialLights
    Dim tmp
    tmp = bl022.State
    bl022.State = bl023.State
    bl023.State = tmp
    tmp = bl007.State
    bl007.State = bl006.State
    bl006.State = tmp
End Sub

'**************
'   Rubbers
'**************
Dim Rub1, Rub2, Rub3

Sub sw63_Hit:vpmTimer.PulseSw 63:Rub1 = 1:sw63_Timer:End Sub

Sub sw63_Timer
    Select Case Rub1
        Case 1:r1.Visible = 0:r13.Visible = 1:sw63.TimerEnabled = 1
        Case 2:r13.Visible = 0:r14.Visible = 1
        Case 3:r14.Visible = 0:r1.Visible = 1:sw63.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Sub rsband010_Hit
    If Tilted then Exit Sub
    AddScore 10
    Rub1 = 0
    rsband010_Timer
End Sub

Sub rsband010_Timer
    Select Case Rub1
        Case 1:Rubber005.Visible = 0:Rubber025.Visible = 1:rsband010.TimerEnabled = 1
        Case 2:Rubber025.Visible = 0:Rubber026.Visible = 1
        Case 3:Rubber026.Visible = 0:Rubber005.Visible = 1:rsband010.TimerEnabled = 0
    End Select
    Rub1 = Rub1 + 1
End Sub

Sub rsband002_Hit
    If Tilted then Exit Sub
    AddScore 10
    Rub2 = 0
    rsband002_Timer
End Sub

Sub rsband002_Timer
    Select Case Rub2
        Case 1:Rubber014.Visible = 0:Rubber013.Visible = 1:rsband002.TimerEnabled = 1
        Case 2:Rubber013.Visible = 0:Rubber024.Visible = 1
        Case 3:Rubber024.Visible = 0:Rubber014.Visible = 1:rsband002.TimerEnabled = 0
    End Select
    Rub2 = Rub2 + 1
End Sub

Sub rsband008_Hit
    Rub3 = 0
    rsband008_Timer
End Sub

Sub rsband008_Timer
    Select Case Rub3
        Case 1:Rubber002.Visible = 0:Rubber027.Visible = 1:rsband008.TimerEnabled = 1
        Case 2:Rubber027.Visible = 0:Rubber028.Visible = 1
        Case 3:Rubber028.Visible = 0:Rubber002.Visible = 1:rsband008.TimerEnabled = 0
    End Select
    Rub3 = Rub3 + 1
End Sub

'*********
' Bumpers
'*********

Sub Bumper001_Hit
    If Tilted Then Exit Sub
	LeftBgif
    PlaySoundAt SoundFXDOF ("fx_Bumper",107,DOFPulse,DOFContactors), bumper001
    DOF 207, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    AddScore 10 + 90 * LBumper1a.State
End Sub

Sub Bumper002_Hit
    If Tilted Then Exit Sub
	RightBgif
    PlaySoundAt SoundFXDOF ("fx_Bumper",109,DOFPulse,DOFContactors), bumper002
    DOF 208, DOFPulse
    If Epileptikdof = True Then DOF 202, DOFPulse End If
    AddScore 10 + 90 * LBumper2a.State
End Sub


'*****************
'     Lanes
'*****************

' inlanes & outlanes

Sub Trigger001_Hit
    PlaySoundAt "fx_sensor", Trigger001
    If Tilted Then Exit Sub
    DOF 217, DOfPulse
    AddScore 500
	If bl020.State = 1 Then
	PuPEvent(20)
	End if
    bl020.State = 0
    bl015.State = 1
    If bl022.State Then AwardSpecial:PuPEvent(39)
    CheckLights
End Sub

Sub Trigger002_Hit
    PlaySoundAt "fx_sensor", Trigger002
	PuPEvent(11)
    If Tilted Then Exit Sub
    DOF 218, DOfPulse
    AddScore 100
    AddBonus 1
End Sub

Sub Trigger003_Hit
    PlaySoundAt "fx_sensor", Trigger003
	PuPEvent(12)
    If Tilted Then Exit Sub
    DOF 219, DOfPulse
    AddScore 100
    AddBonus 1
End Sub

Sub Trigger004_Hit
    PlaySoundAt "fx_sensor", Trigger004
    If Tilted Then Exit Sub
    DOF 220, DOfPulse
	If bl021.State = 1 Then
	PuPEvent(21)
	End if
    AddScore 500
    bl021.State = 0
    bl014.State = 1
    If bl023.State Then AwardSpecial:PuPEvent(39)
    CheckLights
End Sub

' top lanes

Sub Trigger005_Hit
    PlaySoundAt "fx_sensor", Trigger005
    If Tilted Then Exit Sub
    DOF 221, DOfPulse
    AddScore 100
    If bl010.State Then
        BonusMultiplier = 2
        UpdateMultiplierLights
        bl010.State = 0
		PuPEvent(22)
    Else
        Lbumper1a.State = 1
        Lbumper1b.State = 1
        bl010.State = 1
    End If
End Sub

Sub Trigger006_Hit
    PlaySoundAt "fx_sensor", Trigger006
    If Tilted Then Exit Sub
    DOF 222, DOfPulse
    AddScore 100
    If bl011.State Then
        BonusMultiplier = 3
        UpdateMultiplierLights
        bl011.State = 0
		PuPEvent(23)
    Else
        Lbumper2a.State = 1
        Lbumper2b.State = 1
        bl011.State = 1
    End If
End Sub

Sub Trigger007_Hit
    PlaySoundAt "fx_sensor", Trigger007
    If Tilted Then Exit Sub
	PuPEvent(15)
    DOF 223, DOfPulse
    AddScore 500
End Sub

' sides lanes

Sub Trigger012_Hit
    PlaySoundAt "fx_sensor", Trigger012
    If Tilted Then Exit Sub
	PuPEvent(16)
    AddScore 30
    AddBonus 3
End Sub

Sub Trigger011_Hit
    PlaySoundAt "fx_sensor", Trigger011
	PuPEvent(10)
    If Tilted Then Exit Sub
    DOF 224, DOfPulse
    AddScore 500
End Sub

' center triggers

Sub Trigger008_Hit 'center
    PlaySoundAt "fx_sensor", Trigger008
    If Tilted Then Exit Sub
    DOF 225, DOfPulse
    AddBonus 1
End Sub

Sub Trigger009_Hit 'right
    PlaySoundAt "fx_sensor", Trigger009
    If Tilted Then Exit Sub
    RightSpec = 1
    CheckCenterSpecial
End Sub

Sub Trigger009_UnHit 'right
    RightSpec = 0
    CheckCenterSpecial
End Sub

Sub Trigger010_Hit 'left
    PlaySoundAt "fx_sensor", Trigger010
    If Tilted Then Exit Sub
    LeftSpec = 1
    CheckCenterSpecial
End Sub

Sub Trigger010_UnHit 'left
    LeftSpec = 0
    CheckCenterSpecial
End Sub

Sub CheckCenterSpecial
    If LeftSpec + RightSpec = 2 Then
        bl005.State = 1
        bl004.State = 0
    Else
        bl005.State = 0
        bl004.State = 1
    End If
End Sub

'************************
'       Targets
'************************

Sub Target001_hit
    PlaySoundAtBall SoundFXDOF ("fx_target", 115, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 210, DOFPulse
    AddScore 100 + 900 * bl008.State
	If bl008.State=1 Then	
	PuPEvent(17)
	End if
    bl008.State = 0
    bl018.State = 1
    CheckLights
End Sub

Sub Target002_hit
    PlaySoundAtBall SoundFXDOF ("fx_target", 115, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 211, DOFPulse
    AddScore 100 + 900 * bl009.State
	If bl009.State=1 Then	
	PuPEvent(18)
	End if
    bl009.State = 0
    bl019.State = 1
    CheckLights
End Sub

Sub Target003_hit
    PlaySoundAtBall SoundFXDOF ("fx_target", 111, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 212, DOFPulse
    AddScore 100 + 400 * bl012.State
	If bl012.State=1 Then	
	PuPEvent(13)
	End if
    bl012.State = 0
    bl017.State = 1
    If bl006.State Then AwardSpecial:  PuPEvent(39)    
    CheckLights
End Sub

Sub Target005_hit
    PlaySoundAtBall SoundFXDOF ("fx_target", 111, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 213, DOFPulse
    AddScore 100 + 400 * bl013.State
	If bl013.State=1 Then	
	PuPEvent(14)
	End if
    bl013.State = 0
    bl016.State = 1
    If bl007.State Then AwardSpecial: PuPEvent(39)     
    CheckLights
End Sub

Sub Target004_hit 'center
    PlaySoundAtBall SoundFXDOF ("fx_target", 120, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
	If bl005.State=1 Then
	PuPEvent(38)
	End if
	If bl005.State=0 Then
	PuPEvent(19)
	End if	
    DOF 214, DOFPulse
    If Epileptikdof = True Then DOF 215, DOFPulse End If
    AddScore 500 + 4500 * bl004.State
    If bl005.State Then AwardSpecial
End Sub

Sub seta001_hit 'at right outlane
    PlaySoundAtBall SoundFXDOF ("fx_target", 113, DOFPulse, DOFTargets)
    If Tilted Then Exit Sub
    DOF 216, DOFPulse
    AddScore 500
    setap1.TransY = -3: Me.TimerEnabled = 1
End Sub

Sub Seta001_Timer:setap1.TransY = 0: Me.TimerEnabled = 0: End Sub

Sub CheckLights
    If bl014.State + bl015.State + bl016.State + bl017.State + bl018.State + bl019.State = 6 Then
        If NOT SpecialActivated Then
            AwardSpecial
            bl023.State = 1
            bl006.State = 1
            SpecialActivated = true
        End If
    End If
End Sub

' Bonus kicker hole

Sub BonusKicker_Hit
    PlaySoundAt "fx_kicker_enter", BonusKicker
	PuPEvent(24)
	PuPEvent(35)
	PuPEvent(52)
	pDMDSetPage(pDMDBlank)
    If NOT Tilted Then
        BallinBonusKicker = True
        vpmtimer.addtimer 1000, "StartBonusCount '"
    Else
		
        BonusKickOut
    End If
End Sub

Sub BonusKickOut
	PupKickout
    PlaySoundAt SoundFXDOF ("fx_kicker",114,DOFPulse,DOFContactors), BonusKicker
    DOF 209, DOFPulse
    DOF 230, DOFPulse
	PupBonuscount= 1000
    BallinBonusKicker = False
	Kickbonus1.enabled=True
	Kickbonus1.interval= 1500
End Sub

Sub Kickbonus1_timer
	Kickbonus1.enabled=false
	BonusKicker.kick 190, 15
End Sub


Sub PupKickout
	If BallinBonusKicker = True Then
	PuPEvent(26)
	PuPEvent(36)
	PuPEvent(37)
	pDMDSetPage(pScores)
	End if
End Sub


Sub Pupcountingbonus
	If BonusMultiplier=2 Then
	PupBonuscount= PupBonuscount+1000
	End If
	If BonusMultiplier=3 Then
	PupBonuscount= PupBonuscount+2000
	End If
End Sub	

Sub Minusthousand
	If BonusMultiplier=2 Then
	PupBonuscount= PupBonuscount-1000
	End If
End Sub

Sub Minustwothousand
	If BonusMultiplier=3 Then
	PupBonuscount= PupBonuscount-2000
	End If
End Sub

'**************************
'   PinUp Player USER Config
'**************************

dim PuPDMDDriverType: PuPDMDDriverType=2   ' 0=LCD DMD, 1=RealDMD 2=FULLDMD (large/High LCD)
dim useRealDMDScale : useRealDMDScale=1    ' 0 or 1 for RealDMD scaling.  Choose which one you prefer.
dim useDMDVideos    : useDMDVideos=true   ' true or false to use DMD splash videos.
Dim pGameName       : pGameName="TheGrinch"  'pupvideos foldername, probably set to cGameName in realworld







'********************* START OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF BELOW   THIS LINE!!!! ***************
'******************************************************************************
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********
'******************************************************************************
'
'
'  Quick Steps:
'      1>  create a folder in PUPVideos with Starter_PuPPack.zip and call the folder "yourgame"
'      2>  above set global variable pGameName="yourgame"
'      3>  copy paste the settings section above to top of table script for user changes.
'      4>  on Table you need to create ONE timer only called pupDMDUpdate and set it to 250 ms enabled on startup.
'      5>  go to your table1_init or table first startup function and call PUPINIT function
'      6>  Go to bottom on framework here and setup game to call the appropriate events like pStartGame (call that in your game code where needed)...etc
'      7>  attractmodenext at bottom is setup for you already,  just go to each case and add/remove as many as you want and setup the messages to show.  
'      8>  Have fun and use pDMDDisplay(xxxx)  sub all over where needed.  remember its best to make a bunch of mp4 with text animations... looks the best for sure!
'
'
'Note:  for *Future Pinball* "pupDMDupdate_Timer()" timer needs to be renamed to "pupDMDupdate_expired()"  and then all is good.
'       and for future pinball you need to add the follow lines near top
'Need to use BAM and have com idll enabled.
'				Dim icom : Set icom = xBAM.Get("icom") ' "icom" is name of "icom.dll" in BAM\Plugins dir
'				if icom is Nothing then MSGBOX "Error cannot run without icom.dll plugin"
'				Function CreateObject(className)       
'   					Set CreateObject = icom.CreateObject(className)   
'				End Function


Const HasPuP = True   'dont set to false as it will break pup

Const pTopper=0
Const pDMD=1
Const pBackglass=2
Const pPlayfield=3
Const pMusic=4
Const pMusic2=5
Const pCallouts=6
Const pBackglass2=7
Const pTopper2=8
Const pPopUP=9
Const pPopUP2=10


'pages
Const pDMDBlank=0
Const pScores=1
Const pBigLine=2
Const pThreeLines=3
Const pTwoLines=4
Const pTargerLetters=5

'dmdType
Const pDMDTypeLCD=0
Const pDMDTypeReal=1
Const pDMDTypeFULL=2






Dim PuPlayer
dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pInAttract : pInAttract=false   'pAttract mode




'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit

Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
PuPlayer.B2SInit "", pGameName

if (PuPDMDDriverType=pDMDTypeReal) and (useRealDMDScale=1) Then 
       PuPlayer.setScreenEx pDMD,0,0,128,32,0  'if hardware set the dmd to 128,32
End if

PuPlayer.LabelInit pDMD


if PuPDMDDriverType=pDMDTypeReal then
Set PUPDMDObject = CreateObject("PUPDMDControl.DMD") 
PUPDMDObject.DMDOpen
PUPDMDObject.DMDPuPMirror
PUPDMDObject.DMDPuPTextMirror
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":33 }"             'set pupdmd for mirror and hide behind other pups
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 1, ""FN"":32, ""FQ"":3 }"   'set no antialias on font render if real
END IF


pSetPageLayouts

pDMDSetPage(pDMDBlank)   'set blank text overlay page.
pDMDStartUP				 ' firsttime running for like an startup video..


End Sub 'end PUPINIT



'PinUP Player DMD Helper Functions

Sub pDMDLabelHide(labName)
PuPlayer.LabelSet pDMD,labName,"",0,""   
end sub




Sub pDMDScrollBig(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub

Sub pDMDScrollBigV(msgText,timeSec,mColor)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec*1000000) & ",'mlen':" & (timeSec*1000) & ",'tt':0,'fc':" & mColor & "}"
end sub


Sub pDMDSplashScore(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':1,'fq':250,'len':"& (timeSec*1000) &",'fc':" & mColor & "}"
end Sub

Sub pDMDSplashScoreScroll(msgText,timeSec,mColor)
PuPlayer.LabelSet pDMD,"MsgScore",msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':"& (timeSec*1000) &", 'mlen':"& (timeSec*1000) &",'tt':0, 'fc':" & mColor & "}"
end Sub

Sub pDMDZoomBig(msgText,timeSec,mColor)  'new Zoom
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':3,'hstart':5,'hend':80,'len':" & (timeSec*1000) & ",'mlen':" & (timeSec*500) & ",'tt':5,'fc':" & mColor & "}"
end sub

Sub pDMDTargetLettersInfo(msgText,msgInfo, timeSec)  'msgInfo = '0211'  0= layer 1, 1=layer 2, 2=top layer3.
'this function is when you want to hilite spelled words.  Like B O N U S but have O S hilited as already hit markers... see example.
PuPlayer.LabelShowPage pDMD,5,timeSec,""  'show page 5
Dim backText
Dim middleText
Dim flashText
Dim curChar
Dim i
Dim offchars:offchars=0
Dim spaces:spaces=" "  'set this to 1 or more depends on font space width.  only works with certain fonts
                          'if using a fixed font width then set spaces to just one space.

For i=1 To Len(msgInfo)
    curChar="" & Mid(msgInfo,i,1)
    if curChar="0" Then
            backText=backText & Mid(msgText,i,1)
            middleText=middleText & spaces
            flashText=flashText & spaces          
            offchars=offchars+1
    End If
    if curChar="1" Then
            backText=backText & spaces
            middleText=middleText & Mid(msgText,i,1)
            flashText=flashText & spaces
    End If
    if curChar="2" Then
            backText=backText & spaces
            middleText=middleText & spaces
            flashText=flashText & Mid(msgText,i,1)
    End If   
Next 

if offchars=0 Then 'all litup!... flash entire string
   backText=""
   middleText=""
   FlashText=msgText
end if  

PuPlayer.LabelSet pDMD,"Back5"  ,backText  ,1,""
PuPlayer.LabelSet pDMD,"Middle5",middleText,1,""
PuPlayer.LabelSet pDMD,"Flash5" ,flashText ,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & "}"   
end Sub


Sub pDMDSetPage(pagenum)    
    PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page if want off
    PDMDCurPage=pagenum
end Sub

Sub pHideOverlayText(pDisp)
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& pDisp &", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
end Sub



Sub pDMDShowLines3(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,3,timeSec,""
PuPlayer.LabelSet pDMD,"Splash3a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash3b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash3c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowLines2(msgText,msgText2,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,4,timeSec,""
PuPlayer.LabelSet pDMD,"Splash4a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash4b",msgText2,vis,pLine2Ani
end Sub

Sub pDMDShowCounter(msgText,msgText2,msgText3,timeSec)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,6,timeSec,""
PuPlayer.LabelSet pDMD,"Splash6a",msgText,vis, pLine1Ani
PuPlayer.LabelSet pDMD,"Splash6b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash6c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDShowBig(msgText,timeSec, mColor)
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,vis,pLine1Ani
end sub


Sub pDMDShowHS(msgText,msgText2,msgText3,timeSec) 'High Score
Dim vis:vis=1
if pLine1Ani<>"" Then vis=0
PuPlayer.LabelShowPage pDMD,7,timeSec,""
PuPlayer.LabelSet pDMD,"Splash7a",msgText,vis,pLine1Ani
PuPlayer.LabelSet pDMD,"Splash7b",msgText2,vis,pLine2Ani
PuPlayer.LabelSet pDMD,"Splash7c",msgText3,vis,pLine3Ani
end Sub


Sub pDMDSetBackFrame(fname)
  PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
end Sub

Sub pDMDStartBackLoop(fPlayList,fname)
  PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
  PuPlayer.SetBackGround pDMD,1
end Sub

Sub pDMDStopBackLoop
  PuPlayer.SetBackGround pDMD,0
  PuPlayer.playstop pDMD
end Sub


Dim pNumLines

'Theme Colors for Text (not used currenlty,  use the |<colornum> in text labels for colouring.
Dim SpecialInfo
Dim pLine1Color : pLine1Color=8454143  
Dim pLine2Color : pLine2Color=8454143
Dim pLine3Color :  pLine3Color=8454143
Dim curLine1Color: curLine1Color=pLine1Color  'can change later
Dim curLine2Color: curLine2Color=pLine2Color  'can change later
Dim curLine3Color: curLine3Color=pLine3Color  'can change later


Dim pDMDCurPriority: pDMDCurPriority =-1
Dim pDMDDefVolume: pDMDDefVolume = 0   'default no audio on pDMD

Dim pLine1
Dim pLine2
Dim pLine3
Dim pLine1Ani
Dim pLine2Ani
Dim pLine3Ani

Dim PriorityReset:PriorityReset=-1
DIM pAttractReset:pAttractReset=-1
DIM pAttractBetween: pAttractBetween=2000 '1 second between calls to next attract page
DIM pDMDVideoPlaying: pDMDVideoPlaying=false


'************************ where all the MAGIC goes,  pretty much call this everywhere  ****************************************
'*************************                see docs for examples                ************************************************
'****************************************   DONT TOUCH THIS CODE   ************************************************************

Sub pupDMDDisplay(pEventID, pText, VideoName,TimeSec, pAni,pPriority)
' pEventID = reference if application,  
' pText = "text to show" separate lines by ^ in same string
' VideoName "gameover.mp4" will play in background  "@gameover.mp4" will play and disable text during gameplay.
' also global variable useDMDVideos=true/false if user wishes only TEXT
' TimeSec how long to display msg in Seconds
' animation if any 0=none 1=Flasher
' also,  now can specify color of each line (when no animation).  "sometext|12345"  will set label to "sometext" and set color to 12345

DIM curPos
if pDMDCurPriority>pPriority then Exit Sub  'if something is being displayed that we don't want interrupted.  same level will interrupt.
pDMDCurPriority=pPriority
if timeSec=0 then timeSec=1 'don't allow page default page by accident


pLine1=""
pLine2=""
pLine3=""
pLine1Ani=""
pLine2Ani=""
pLine3Ani=""


if pAni=1 Then  'we flashy now aren't we
pLine1Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
pLine2Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
pLine3Ani="{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) &  "}"  
end If

curPos=InStr(pText,"^")   'Lets break apart the string if needed
if curPos>0 Then 
   pLine1=Left(pText,curPos-1) 
   pText=Right(pText,Len(pText) - curPos)
   
   curPos=InStr(pText,"^")   'Lets break apart the string
   if curPOS>0 Then
      pLine2=Left(pText,curPos-1) 
      pText=Right(pText,Len(pText) - curPos)

      curPos=InStr("^",pText)   'Lets break apart the string   
      if curPos>0 Then
         pline3=Left(pText,curPos-1) 
      Else 
        if pText<>"" Then pline3=pText 
      End if 
   Else 
      if pText<>"" Then pLine2=pText
   End if    
Else 
  pLine1=pText  'just one line with no break 
End if


'lets see how many lines to Show
pNumLines=0
if pLine1<>"" then pNumLines=pNumlines+1
if pLine2<>"" then pNumLines=pNumlines+1
if pLine3<>"" then pNumLines=pNumlines+1

if pDMDVideoPlaying and (VideoName="") Then 
			PuPlayer.playstop pDMD
			pDMDVideoPlaying=False

End if


if (VideoName<>"") and (useDMDVideos) Then  'we are showing a splash video instead of the text.
    
    PuPlayer.playlistplayex pDMD,"DMDSplash",VideoName,pDMDDefVolume,pPriority  'should be an attract background (no text is displayed)
    pDMDVideoPlaying=true
end if 'if showing a splash video with no text




if StrComp(pEventID,"shownum",1)=0 Then              'check eventIDs
    pDMDShowCounter pLine1,pLine2,pLine3,timeSec
Elseif StrComp(pEventID,"target",1)=0 Then              'check eventIDs
    pDMDTargetLettersInfo pLine1,pLine2,timeSec
Elseif StrComp(pEventID,"highscore",1)=0 Then              'check eventIDs
    pDMDShowHS pLine1,pLine2,pline3,timeSec
Elseif (pNumLines=3) Then                'depends on # of lines which one to use.  pAni=1 will flash.
    pDMDShowLines3 pLine1,pLine2,pLine3,TimeSec
Elseif (pNumLines=2) Then
    pDMDShowLines2 pLine1,pLine2,TimeSec
Elseif (pNumLines=1) Then
    pDMDShowBig pLine1,timeSec, curLine1Color
Else
    pDMDShowBig pLine1,timeSec, curLine1Color
End if

PriorityReset=TimeSec*1000
End Sub 'pupDMDDisplay message

Sub pupDMDupdate_Timer()
	pUpdateScores    

    if PriorityReset>0 Then  'for splashes we need to reset current prioirty on timer
       PriorityReset=PriorityReset-pupDMDUpdate.interval
       if PriorityReset<=0 Then 
            pDMDCurPriority=-1            
            if pInAttract then pAttractReset=pAttractBetween ' pAttractNext  call attract next after 1 second
			pDMDVideoPlaying=false			
			End if
    End if

    if pAttractReset>0 Then  'for splashes we need to reset current prioirty on timer
       pAttractReset=pAttractReset-pupDMDUpdate.interval
       if pAttractReset<=0 Then 
            pAttractReset=-1            
            if pInAttract then pAttractNext
			End if
    end if 
End Sub


'********************* END OF PUPDMD FRAMEWORK v1.0 *************************
'******************** DO NOT MODIFY STUFF ABOVE THIS LINE!!!! ***************
'****************************************************************************

'*****************************************************************
'   **********  PUPDMD  MODIFY THIS SECTION!!!  ***************
'PUPDMD Layout for each Table1
'Setup Pages.  Note if you use fonts they must be in FONTS folder of the pupVideos\tablename\FONTS  "case sensitive exact naming fonts!"
'*****************************************************************

Sub pSetPageLayouts

DIM dmddef
DIM dmdalt
DIM dmdscr
DIM dmdfixed

'labelNew <screen#>, <Labelname>, <fontName>,<size%>,<colour>,<rotation>,<xalign>,<yalign>,<xpos>,<ypos>,<PageNum>,<visible>
'***********************************************************************'
'<screen#>, in standard we’d set this to pDMD ( or 1)
'<Labelname>, your name of the label. keep it short no spaces (like 8 chars) although you can call it anything really. When setting the label you will use this labelname to access the label.
'<fontName> Windows font name, this must be exact match of OS front name. if you are using custom TTF fonts then double check the name of font names.
'<size%>, Height as a percent of display height. 20=20% of screen height.
'<colour>, integer value of windows color.
'<rotation>, degrees in tenths   (900=90 degrees)
'<xAlign>, 0= horizontal left align, 1 = center horizontal, 2= right horizontal
'<yAlign>, 0 = top, 1 = center, 2=bottom vertical alignment
'<xpos>, this should be 0, but if you want to ‘force’ a position you can set this. it is a % of horizontal width. 20=20% of screen width.
'<ypos> same as xpos.
'<PageNum> IMPORTANT… this will assign this label to this ‘page’ or group.
'<visible> initial state of label. visible=1 show, 0 = off.


if PuPDMDDriverType=pDMDTypeFULL THEN  'Using FULL BIG LCD PuPDMD  ************ lcd **************

	'dmddef="Grinched"
	dmdalt="Grinched"    
    dmdfixed="Grinched"
	dmdscr="Grinched"  'main score font
	dmddef="Grinched"

	'Page 1 (default score display)
		PuPlayer.LabelNew pDMD,"Credits" ,dmddef,20,33023   ,0,2,2,95,0,1,0
		PuPlayer.LabelNew pDMD,"Player"  ,dmdalt,8,16777215  ,1,0,0,14,91,1,0
		PuPlayer.LabelNew pDMD,"Ball"    ,dmdalt,8,16777215   ,1,2,0,55,1,1,0
		PuPlayer.LabelNew pDMD,"HS1"     ,dmdalt,6,16777215   ,0,1,0,90,94,1,0
		PuPlayer.LabelNew pDMD,"CurScore",dmdscr,14,16777215  ,0,1,1, 50,94,1,0	
		PuPlayer.LabelNew pDMD,"LeftBumper"  ,dmdalt,8,16777215  ,1,0,0,0,11,1,0	
		PuPlayer.LabelNew pDMD,"LeftBumperB"  ,dmdalt,8,16777215  ,1,0,0,10,35,1,0	
		PuPlayer.LabelNew pDMD,"LeftBumperC"  ,dmdalt,8,16777215  ,1,0,0,0,65,1,0	
		PuPlayer.LabelNew pDMD,"RightBumper"  ,dmdalt,8,16777215  ,1,0,0,70,11,1,0	
		PuPlayer.LabelNew pDMD,"RightBumperB"  ,dmdalt,8,16777215  ,1,0,0,60,35,1,0	
		PuPlayer.LabelNew pDMD,"RightBumperC"  ,dmdalt,8,16777215  ,1,0,0,70,65,1,0	

	'Page 2 (default Text Splash 1 Big Line)
		PuPlayer.LabelNew pDMD,"Splash"  ,dmdalt,40,33023,0,1,1,0,0,2,0

	'Page 3 (default Text 3 Lines)
		PuPlayer.LabelNew pDMD,"Splash3a",dmddef,30,8454143,0,1,0,0,2,3,0
		PuPlayer.LabelNew pDMD,"Splash3b",dmdalt,30,33023,0,1,0,0,30,3,0
		PuPlayer.LabelNew pDMD,"Splash3c",dmdalt,25,33023,0,1,0,0,57,3,0


	'Page 4 (default Text 2 Line)
		PuPlayer.LabelNew pDMD,"Splash4a",dmddef,40,8454143,0,1,0,0,0,4,0
		PuPlayer.LabelNew pDMD,"Splash4b",dmddef,30,33023,0,1,2,0,75,4,0

	'Page 5 (3 layer large text for overlay targets function,  must you fixed width font!
		PuPlayer.LabelNew pDMD,"Back5"    ,dmdfixed,80,8421504,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Middle5"  ,dmdfixed,80,65535  ,0,1,1,0,0,5,0
		PuPlayer.LabelNew pDMD,"Flash5"   ,dmdfixed,80,65535  ,0,1,1,0,0,5,0

	'Page 6 (3 Lines for big # with two lines,  "19^Orbits^Count")
		PuPlayer.LabelNew pDMD,"Splash6a",dmddef,90,65280,0,0,0,15,1,6,0
		PuPlayer.LabelNew pDMD,"Splash6b",dmddef,50,33023,0,1,0,60,0,6,0
		PuPlayer.LabelNew pDMD,"Splash6c",dmddef,40,33023,0,1,0,60,50,6,0

	'Page 7 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"Splash7a",dmddef,20,8454143,0,1,0,0,2,7,0
		PuPlayer.LabelNew pDMD,"Splash7b",dmdfixed,40,33023,0,1,0,0,20,7,0
		PuPlayer.LabelNew pDMD,"Splash7c",dmdfixed,40,33023,0,1,0,0,50,7,0

	'Page 9 (Show High Scores Fixed Fonts)
		PuPlayer.LabelNew pDMD,"HSNUMB",dmddef,15,1769471,0,1,1,50,58,9,0
		PuPlayer.LabelNew pDMD,"GRANDCHAMP",dmddef,15,1769471,0,1,1,50,40,9,0
		PuPlayer.LabelNew pDMD,"HSINIT",dmddef,18,1769471,0,1,1,50,75,9,0
		PuPlayer.LabelNew pDMD,"Boncount",dmddef,18,1769471,0,1,1,50,85,9,0
		PuPlayer.LabelNew pDMD,"GOtext",dmdscr,17,1769471,0,1,1, 50,72,9,0
		PuPlayer.LabelNew pDMD,"GOScore",dmdscr,17,1769471,0,1,1, 50,90,9,0
		PuPlayer.LabelNew pDMD,"FinalScore",dmdscr,20,1769471,0,1,1, 50,85,9,0	
		PuPlayer.LabelNew pDMD,"LastScore",dmdscr,20,1769471,0,1,1, 50,65,9,0	
		PuPlayer.LabelNew pDMD,"HSScore",dmdscr,15,1769471,0,1,1, 50,20,9,0	
		PuPlayer.LabelNew pDMD,"eintials",dmdscr,15,1769471,0,1,1, 50,70,9,0	
		PuPlayer.LabelNew pDMD,"initials",dmdscr,15,1769471,0,1,1, 50,85,9,0
		puPlayer.LabelNew pDMD,"EnterHS1",dmddef,		10,   62207 	,0,1,0 ,0,30   ,9,0				
		puPlayer.LabelNew pDMD,"EnterHS2",dmdfixed,		10,  16777215  	,0,1,0 ,0,60   ,9,0				' HS Entry
		puPlayer.LabelNew pDMD,"EnterHS3",dmdfixed,		10,  62207		,0,1,0 ,0,70   ,9,0				' HS Entry


END IF  ' use PuPDMDDriver




end Sub 'page Layouts


'*****************************************************************
'        PUPDMD Custom SUBS/Events for each Table1
'     **********    MODIFY THIS SECTION!!!  ***************
'*****************************************************************
'
'
'  we need to somewhere in code if applicable
'
'   call pDMDStartGame,pDMDStartBall,pGameOver,pAttractStart
'
'
'
'
'

Sub pDMDStartGame	
If HasPuP=False then exit Sub
	pInAttract=false
	PuPEvent(7)
	PuPEvent(500)
	PuPEvent(6)
    PuPEvent(5)
	pDMDSetPage(pScores)
	pDMDmode="default"
	pDMD_CurSequencePos=0
	pDMD_Sequence.Enabled=false
end Sub


Sub pDMDGameOver
If HasPuP=False then exit Sub
	pDMDSetPage(9)
	pDMD_CurSequencePos=0
	pDMD_Sequence.Interval = 500
	pDMD_Sequence.Enabled=true
end Sub

Sub pAttractStart
If HasPuP=False then exit Sub
	pDMDSetPage(0)   'set blank text overlay page.
	pDMDmode="attract"
	pDMD_CurSequencePos=0
	pDMD_Sequence.Interval = 500
	pDMD_Sequence.Enabled=true
end Sub

Sub pDMDStartUP
If HasPuP=False then exit Sub
	pDMDmode="attract"
	pDMD_CurSequencePos=0
	pDMD_Sequence.Interval = 50
	pDMD_Sequence.Enabled=true
end Sub



'********************** DMD *************************************************************

Dim pDMD_CurSequencePos:pDMD_CurSequencePos=0

Dim pDMDmode: pDMDmode="default"

Sub pDMD_Sequence_Timer
If HasPuP=False then exit Sub	
	pDMD_CurSequencePos=pDMD_CurSequencePos+1
	if pDMDmode="attract" then
		Select Case pDMD_CurSequencePos
		Case 1 PuPEvent(1):pDMD_Sequence.Interval =2 
		Case 2 pDMD_Sequence.Interval = 39800
		Case 3 PuPEvent(2) :pDMD_Sequence.Interval = 8000
		Case 4 pDMDSetPage(9):puPlayer.LabelSet pDMD,"HSNUMB",""&  FormatNumber(HSScore(1),0),1,"{'mt':2,'shadowcolor':0, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"GRANDCHAMP","GRAND CHAMPION",1,"{'mt':2,'shadowcolor':0, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"HSINIT",""& HSName(1),1,"{'mt':2,'shadowcolor':0, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMD_Sequence.Interval = 5000
		Case 5 pDMD_Sequence.Interval = 1000
		Case 6 puPlayer.LabelSet pDMD,"HSNUMB",""& FormatNumber(HSScore(2),0),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"GRANDCHAMP","HIGH SCORE: 1",1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"HSINIT",""& HSName(2),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMD_Sequence.Interval = 5000
		Case 7 pDMD_Sequence.Interval = 1000
		Case 8 puPlayer.LabelSet pDMD,"HSNUMB",""& FormatNumber(HSScore(3),0),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"GRANDCHAMP","HIGH SCORE: 2",1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"HSINIT",""& HSName(3),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMD_Sequence.Interval = 5000
		Case 9 pDMD_Sequence.Interval = 1000
		Case 10 puPlayer.LabelSet pDMD,"HSNUMB",""& FormatNumber(HSScore(4),0),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"GRANDCHAMP","HIGH SCORE: 3",1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"HSINIT",""& HSName(4),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMD_Sequence.Interval = 5000
		Case 11 pDMD_Sequence.Interval = 1000
		Case 12 puPlayer.LabelSet pDMD,"HSNUMB",""& FormatNumber(HSScore(5),0),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"GRANDCHAMP","HIGH SCORE: 4",1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"HSINIT",""& HSName(5),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMD_Sequence.Interval = 5000
		Case 13 pDMD_Sequence.Interval = 1000
		Case 14 pDMDSetPage(0):pDMD_Sequence.Interval =2500 

		Case Else
		pDMDSetPage(9)
		pDMD_CurSequencePos=2
		end Select
	end if

	 if pDMDmode="GO" then	
		Select Case pDMD_CurSequencePos
		Case 1 PuPEvent(2) :pDMD_Sequence.Interval = 5000
		Case 2 pDMDSetPage(9):puPlayer.LabelSet pDMD,"FinalScore","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }":puPlayer.LabelSet pDMD,"LastScore","LAST SCORE",1,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }":pDMD_Sequence.Interval = 3000
		Case 3 pDMDSetPage(9):puPlayer.LabelSet pDMD,"FinalScore","" & FormatNumber(Score(CurrentPlayer),0),0,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }":puPlayer.LabelSet pDMD,"LastScore","LAST SCORE",0,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }":pDMD_Sequence.Interval = 1000
		Case 4 puPlayer.LabelSet pDMD,"HSNUMB",""&  FormatNumber(HSScore(1),0),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"GRANDCHAMP","GRAND CHAMPION",1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"HSINIT",""& HSName(1),1,"{'mt':2,'shadowcolor':0, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMD_Sequence.Interval = 5000
		Case 5 pDMD_Sequence.Interval = 1000
		Case 6 puPlayer.LabelSet pDMD,"HSNUMB",""& FormatNumber(HSScore(2),0),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"GRANDCHAMP","HIGH SCORE: 2",1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"HSINIT",""& HSName(2),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMD_Sequence.Interval = 5000
		Case 7 pDMD_Sequence.Interval = 1000
		Case 8 puPlayer.LabelSet pDMD,"HSNUMB",""& FormatNumber(HSScore(3),0),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"GRANDCHAMP","HIGH SCORE: 3",1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"HSINIT",""& HSName(3),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMD_Sequence.Interval = 5000
		Case 9 pDMD_Sequence.Interval = 1000
		Case 10 puPlayer.LabelSet pDMD,"HSNUMB",""& FormatNumber(HSScore(4),0),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"GRANDCHAMP","HIGH SCORE: 4",1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"HSINIT",""& HSName(4),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMD_Sequence.Interval = 5000
		Case 11 pDMD_Sequence.Interval = 1000
		Case 12 puPlayer.LabelSet pDMD,"HSNUMB",""& FormatNumber(HSScore(5),0),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"GRANDCHAMP","HIGH SCORE: 5",1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMDSetPage(9):puPlayer.LabelSet pDMD,"HSINIT",""& HSName(5),1,"{'mt':2,'shadowcolor':2949120, 'shadowstate':2,'xoffset':1, 'yoffset':8, 'bold':1, 'outline':2 }":pDMD_Sequence.Interval = 5000
		Case 13 pDMD_Sequence.Interval = 1000
		Case 14 pDMDSetPage(0):pDMD_Sequence.Interval =2000 

		Case Else
		pDMDSetPage(9)
		pDMD_CurSequencePos=0
		end Select
End if
End Sub


'************************ called during gameplay to update Scores ***************************
Dim Gifleftcount: Gifleftcount=0
Dim pDMDBBonus: pDMDBBonus="default"

Sub pUpdateScores  'call this ONLY on timer 300ms is good enough
if pDMDCurPage <> pScores then Exit Sub
'puPlayer.LabelSet pDMD,"Credits","CREDITS " & ""& Credits ,1,""
'puPlayer.LabelSet pDMD,"Play1","Player 1",1,""
'puPlayer.LabelSet pDMD,"Ball"," "&pDMDCurPriority ,1,""

puPlayer.LabelSet pDMD,"CurScore","" & FormatNumber(Score(CurrentPlayer),0),1,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }"
puPlayer.LabelSet pDMD,"Player","" & CurrentPlayer,1,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }"
puPlayer.LabelSet pDMD,"Ball",""& BallsPerGame - BallsRemaining(CurrentPlayer) + 1 ,1,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }"
puPlayer.LabelSet pDMD,"HS1","" & FormatNumber(HSScore(1),0),1,"{'mt':2, 'shadowcolor':2949120, 'shadowstate':2,'xoffset':2, 'yoffset':8, 'bold':1, 'outline':2 }"
end Sub

Sub LeftBgif
If HasPuP=False then exit Sub	
	Gifleftcount=Gifleftcount+1
	If Lbumper1a.State = 1 Then
	Select Case Gifleftcount
	Case 1 puPlayer.LabelSet pDMD,"LeftBumper","Gif\\snowballlefta.gif",1,"{'mt':2,'color':111111, 'width': 30, 'height': 25., 'anigif': 130 ,}":LeftGifBumperT1.Enabled=True:LeftGifBumperT1.interval=900
	Case 2 puPlayer.LabelSet pDMD,"LeftBumperB","Gif\\snowballleftb.gif",1,"{'mt':2,'color':111111, 'width': 30, 'height': 25., 'anigif': 130 ,}":LeftGifBumperT2.Enabled=True:LeftGifBumperT2.interval=900
	Case 3 puPlayer.LabelSet pDMD,"LeftBumperC","Gif\\snowballlefta.gif",1,"{'mt':2,'color':111111, 'width': 30, 'height': 25., 'anigif': 130 ,}":LeftGifBumperT3.Enabled=True:LeftGifBumperT3.interval=900	
		Case Else
		puPlayer.LabelSet pDMD,"LeftBumper","Gif\\snowballleftc.gif",1,"{'mt':2,'color':111111, 'width': 30, 'height': 25., 'anigif': 130 ,}":LeftGifBumperT1.Enabled=True:LeftGifBumperT1.interval=900
		Gifleftcount=0
		end Select
	End if
End Sub


Sub RightBgif
If HasPuP=False then exit Sub	
	Gifleftcount=Gifleftcount+1
	If Lbumper2a.State = 1 Then
	Select Case Gifleftcount
	Case 1 puPlayer.LabelSet pDMD,"RightBumper","Gif\\snowballlefta.gif",1,"{'mt':2,'color':111111, 'width': 30, 'height': 25., 'anigif': 130 ,}":RightGifBumperT1.Enabled=True:RightGifBumperT1.interval=900
	Case 2 puPlayer.LabelSet pDMD,"RightBumperB","Gif\\snowballleftb.gif",1,"{'mt':2,'color':111111, 'width': 30, 'height': 25., 'anigif': 130 ,}":RightGifBumperT2.Enabled=True:RightGifBumperT2.interval=900
	Case 3 puPlayer.LabelSet pDMD,"RightBumperC","Gif\\snowballlefta.gif",1,"{'mt':2,'color':111111, 'width': 30, 'height': 25., 'anigif': 130 ,}":RightGifBumperT3.Enabled=True:RightGifBumperT3.interval=900	
		Case Else
		puPlayer.LabelSet pDMD,"RightBumper","Gif\\snowballleftc.gif",1,"{'mt':2,'color':111111, 'width': 30, 'height': 25., 'anigif': 130 ,}":RightGifBumperT1.Enabled=True:RightGifBumperT1.interval=900
		Gifleftcount=0
		end Select
	End if
End Sub



Sub LeftGifBumperT1_Timer
	LeftGifBumperT1.Enabled=False
	puPlayer.LabelSet pDMD,"LeftBumper","Gif\\lblank1.gif",1,"{'mt':2,'color':111111, 'width': 25, 'height': 25., 'anigif': 130 ,}"
 End Sub

Sub LeftGifBumperT2_Timer
	LeftGifBumperT2.Enabled=False
	puPlayer.LabelSet pDMD,"LeftBumperB","Gif\\lblank2.gif",1,"{'mt':2,'color':111111, 'width': 25, 'height': 25., 'anigif': 130 ,}"
 End Sub

Sub LeftGifBumperT3_Timer
	LeftGifBumperT3.Enabled=False
	puPlayer.LabelSet pDMD,"LeftBumperC","Gif\\lblank3.gif",1,"{'mt':2,'color':111111, 'width': 25, 'height': 25., 'anigif': 130 ,}"
 End Sub

Sub RightGifBumperT1_Timer
	RightGifBumperT1.Enabled=False
	puPlayer.LabelSet pDMD,"RightBumper","Gif\\lblank1.gif",1,"{'mt':2,'color':111111, 'width': 25, 'height': 25., 'anigif': 130 ,}"
 End Sub

Sub RightGifBumperT2_Timer
	RightGifBumperT2.Enabled=False
	puPlayer.LabelSet pDMD,"RightBumperB","Gif\\lblank2.gif",1,"{'mt':2,'color':111111, 'width': 25, 'height': 25., 'anigif': 130 ,}"
 End Sub

Sub RightGifBumperT3_Timer
	RightGifBumperT3.Enabled=False
	puPlayer.LabelSet pDMD,"RightBumperC","Gif\\lblank3.gif",1,"{'mt':2,'color':111111, 'width': 25, 'height': 25., 'anigif': 130 ,}"
 End Sub



'**************************
'PinUPPlayer
'**************************


Sub PinUPInit
Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")
PuPlayer.B2SInit "",CGameName
end Sub

Sub PuPEvent(EventNum)
if hasPUP=false then Exit Sub

PuPlayer.B2SData "D"&EventNum,1  'send event to puppack driver
   
End Sub
